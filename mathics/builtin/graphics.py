# -*- coding: utf8 -*-

"""
Graphics
"""

import re
from math import floor, ceil, log10, fmod

from django.utils import simplejson

from mathics.builtin.base import (Builtin, InstancableBuiltin, BoxConstruct,
                                  BoxConstructError)
from mathics.builtin.options import options_to_rules
from mathics.core.expression import (Expression, Integer, Real, NumberError,
                                     Symbol)


class CoordinatesError(BoxConstructError):
    pass


class ColorError(BoxConstructError):
    pass

element_heads = ('Rectangle', 'Disk', 'Line', 'Point',
                 'Circle', 'Polygon', 'Inset', 'Text', 'Sphere')
color_heads = ('RGBColor', 'CMYKColor', 'Hue', 'GrayLevel')
thickness_heads = ('Thickness', 'AbsoluteThickness', 'Thick', 'Thin')

GRAPHICS_SYMBOLS = set(['List', 'Rule', 'VertexColors'] + list(element_heads) +
                       [element + 'Box' for element in element_heads] +
                       list(color_heads) + list(thickness_heads))


def get_class(name):
    from mathics.builtin.graphics3d import GLOBALS3D

    c = GLOBALS.get(name)
    if c is None:
        return GLOBALS3D.get(name)
    else:
        return c

    # globals() does not work with Cython, otherwise one could use something
    # like return globals().get(name)


def coords(value):
    if value.has_form('List', 2):
        x, y = value.leaves[0].to_number(), value.leaves[1].to_number()
        if x is None or y is None:
            raise CoordinatesError
        return (x, y)
    raise CoordinatesError


class Coords(object):
    def __init__(self, graphics, expr=None, pos=None, d=None):
        self.graphics = graphics
        self.p = pos
        self.d = d
        if expr is not None:
            if expr.has_form('Offset', 1, 2):
                self.d = coords(expr.leaves[0])
                if len(expr.leaves) > 1:
                    self.p = coords(expr.leaves[1])
                else:
                    self.p = None
            else:
                self.p = coords(expr)

    def pos(self):
        p = self.graphics.translate(self.p)
        p = (cut(p[0]), cut(p[1]))
        if self.d is not None:
            d = self.graphics.translate_absolute(self.d)
            return (p[0] + d[0], p[1] + d[1])
        return p

    def add(self, x, y):
        p = (self.p[0] + x, self.p[1] + y)
        return Coords(self.graphics, pos=p, d=self.d)


def cut(value):
    "Cut values in graphics primitives (not displayed otherwise in SVG)"
    border = 10 ** 8
    if value < -border:
        value = -border
    elif value > border:
        value = border
    return value


def create_css(edge_color=None, face_color=None, stroke_width=None,
               font_color=None):
    css = []
    if edge_color is not None:
        color, opacity = edge_color.to_css()
        css.append('stroke: %s' % color)
        css.append('stroke-opacity: %s' % opacity)
    else:
        css.append('stroke: none')
    if stroke_width is not None:
        css.append('stroke-width: %fpx' % stroke_width)
    if face_color is not None:
        color, opacity = face_color.to_css()
        css.append('fill: %s' % color)
        css.append('fill-opacity: %s' % opacity)
    else:
        css.append('fill: none')
    if font_color is not None:
        color, opacity = font_color.to_css()
        css.append('color: %s' % color)
    return '; '.join(css)


def asy_number(value):
    return '%s' % value


def create_pens(edge_color=None, face_color=None, stroke_width=None,
                is_face_element=False):
    result = []
    if face_color is not None:
        brush, opacity = face_color.to_asy()
        if opacity != 1:
            brush += '+opacity(%s)' % asy_number(opacity)
        result.append(brush)
    elif is_face_element:
        result.append('nullpen')
    if edge_color is not None:
        pen, opacity = edge_color.to_asy()
        if opacity != 1:
            pen += '+opacity(%s)' % asy_number(opacity)
        if stroke_width is not None:
            pen += '+linewidth(%s)' % asy_number(stroke_width)
        result.append(pen)
    elif is_face_element:
        result.append('nullpen')
    return ', '.join(result)


class Graphics(Builtin):
    r"""
    <dl>
    <dt>'Graphics[$primitives$, $options$]'
        <dd>represents a graphic.
    </dl>

    >> Graphics[{Blue, Line[{{0,0}, {1,1}}]}]
     = -Graphics-

    'Graphics' supports 'PlotRange':
    >> Graphics[{Rectangle[{1, 1}]}, Axes -> True, PlotRange -> {{-2, 1.5}, {-1, 1.5}}]
     = -Graphics-

    'Graphics' produces 'GraphicsBox' boxes:
    >> Graphics[Rectangle[]] // ToBoxes // Head
     = GraphicsBox

    In 'TeXForm', 'Graphics' produces Asymptote figures:
    >> Graphics[Circle[]] // TeXForm
     = 
     . \begin{asy}
     . size(5.85559796438cm, 5cm);
     . draw(ellipse((175.0,175.0),175.0,175.0), rgb(0, 0, 0)+linewidth(0.666666666667));
     . clip(box((-0.333333333333,0.333333333333), (350.333333333,349.666666667)));
     . \end{asy}

    Invalid graphics directives yield invalid box structures:
    >> Graphics[Circle[{a, b}]]
     : GraphicsBox[CircleBox[List[a, b]], Rule[AspectRatio, Automatic], Rule[Axes, False], Rule[AxesStyle, List[]], Rule[ImageSize, Automatic], Rule[LabelStyle, List[]], Rule[PlotRange, Automatic], Rule[PlotRangePadding, Automatic], Rule[TicksStyle, List[]]] is not a valid box structure.
    """

    options = {
        'Axes': 'False',
        'TicksStyle': '{}',
        'AxesStyle': '{}',
        'LabelStyle': '{}',
        'AspectRatio': 'Automatic',
        'PlotRange': 'Automatic',
        'PlotRangePadding': 'Automatic',
        'ImageSize': 'Automatic',
    }

    box_suffix = 'Box'

    def apply_makeboxes(self, content, evaluation, options):
        '''MakeBoxes[%(name)s[content_, OptionsPattern[%(name)s]],
                StandardForm|TraditionalForm|OutputForm]'''

        def convert(content):
            if content.has_form('List', None):
                return Expression('List',
                                  *[convert(item) for item in content.leaves])
            head = content.get_head_name()
            if head in element_heads:
                if head == 'Text':
                    head = 'Inset'
                atoms = content.get_atoms(include_heads=False)
                if any(not isinstance(atom, (Integer, Real)) and
                       not atom.get_name() in GRAPHICS_SYMBOLS
                       for atom in atoms):
                    if head == 'Inset':
                        n_leaves = [content.leaves[0]] + [
                            Expression('N', leaf).evaluate(evaluation)
                            for leaf in content.leaves[1:]]
                    else:
                        n_leaves = (Expression('N', leaf).evaluate(
                            evaluation) for leaf in content.leaves)
                else:
                    n_leaves = content.leaves
                return Expression(head + self.box_suffix, *n_leaves)
            return content

        for option in options:
            options[option] = Expression(
                'N', options[option]).evaluate(evaluation)
        box_name = 'Graphics' + self.box_suffix
        return Expression(box_name, convert(content),
                          *options_to_rules(options))


class _GraphicsElement(InstancableBuiltin):
    def init(self, graphics, item=None, style=None):
        if item is not None and not item.has_form(self.get_name(), None):
            raise BoxConstructError
        self.graphics = graphics
        self.style = style
        self.is_completely_visible = False  # True for axis elements


class _Color(_GraphicsElement):
    components_sizes = []
    default_components = []

    def init(self, item=None, components=None):
        super(_Color, self).init(None, item)
        if item is not None:
            if len(item.leaves) == 1 and item.leaves[0].has_form('List', None):
                leaves = item.leaves[0].leaves
            else:
                leaves = item.leaves
            if len(leaves) in self.components_sizes:
                try:
                    components = [value.to_number(
                        min=0, max=1) for value in leaves]
                except NumberError:
                    raise ColorError
                if len(components) < len(self.default_components):
                    components.extend(self.default_components[
                                      len(components):])
                self.components = components
            else:
                raise ColorError
        elif components is not None:
            self.components = components

    @staticmethod
    def create(expr):
        head = expr.get_head_name()
        cls = get_class(head)
        if cls is None:
            raise ColorError
        return cls(expr)

    def to_css(self):
        rgba = self.to_rgba()
        return (r'rgb(%f%%, %f%%, %f%%)' % (
            rgba[0] * 100, rgba[1] * 100, rgba[2] * 100), rgba[3])

    def to_asy(self):
        rgba = self.to_rgba()
        return (r'rgb(%s, %s, %s)' % (
            asy_number(rgba[0]), asy_number(rgba[1]), asy_number(rgba[2])),
            rgba[3])

    def to_js(self):
        return self.to_rgba()

    def to_expr(self):
        return Expression(self.get_name(), *self.components)


class RGBColor(_Color):
    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]

    def to_rgba(self):
        return self.components


class CMYKColor(_Color):
    components_sizes = [3, 4, 5]
    default_components = [0, 0, 0, 0, 1]

    def to_rgba(self):
        k = self.components[3]
        k_ = 1 - k
        c = self.components
        cmy = [c[0] * k_ + k, c[1] * k_ + k, c[2] * k_ + k]
        rgb = (1 - cmy[0], 1 - cmy[1], 1 - cmy[2])
        return rgb + (c[4],)


class Hue(_Color):
    """
    >> Graphics[Table[{EdgeForm[Gray], Hue[h, s], Disk[{12h, 8s}]}, {h, 0, 1, 1/6}, {s, 0, 1, 1/4}]]
     = -Graphics-

    >> Graphics[Table[{EdgeForm[{GrayLevel[0, 0.5]}], Hue[(-11+q+10r)/72, 1, 1, 0.6], Disk[(8-r) {Cos[2Pi q/12], Sin[2Pi q/12]}, (8-r)/3]}, {r, 6}, {q, 12}]]
     = -Graphics-
    """

    components_sizes = [1, 2, 3, 4]
    default_components = [0, 1, 1, 1]

    def to_rgba(self):
        h, s, v = self.components[:3]
        i = floor(6 * h)
        f = 6 * h - i
        i = i % 6
        p = v * (1 - s)
        q = v * (1 - f * s)
        t = v * (1 - (1 - f) * s)

        rgb = {
            0: (v, t, p),
            1: (q, v, p),
            2: (p, v, t),
            3: (p, q, v),
            4: (t, p, v),
            5: (v, p, q),
        }[i]
        return rgb + (self.components[3],)

    def hsl_to_rgba(self):
        h, s, l = self.components[:3]
        if l < 0.5:
            q = l * (1 + s)
        else:
            q = l + s - l * s
        p = 2 * l - q

        rgb = (h + 1 / 3, h, h - 1 / 3)

        def map(value):
            if value < 0:
                value += 1
            if value > 1:
                value -= 1
            return value

        def trans(t):
            if t < 1 / 6:
                return p + ((q - p) * 6 * t)
            elif t < 1 / 2:
                return q
            elif t < 2 / 3:
                return p + ((q - p) * 6 * (2 / 3 - t))
            else:
                return p

        result = tuple([trans(map(t)) for t in rgb]) + (self.components[3],)
        print result
        return result


class GrayLevel(_Color):
    components_sizes = [1, 2]
    default_components = [0, 1]

    def to_rgba(self):
        g = self.components[0]
        return (g, g, g, self.components[1])


class _Thickness(_GraphicsElement):
    def init(self, graphics, item=None, value=None):
        super(_Thickness, self).init(graphics, item)
        if item is not None:
            self.value = item.leaves[0].to_number()
        elif value is not None:
            self.value = value
        else:
            raise BoxConstructError
        if self.value < 0:
            raise BoxConstructError


class AbsoluteThickness(_Thickness):
    def get_thickness(self):
        return self.graphics.translate_absolute((self.value, 0))[0]


class Thickness(_Thickness):
    def get_thickness(self):
        return self.graphics.translate_relative(self.value)


class Thin(Builtin):
    rules = {
        'Thin': 'AbsoluteThickness[0.5]',
    }


class Thick(Builtin):
    rules = {
        'Thick': 'AbsoluteThickness[2]',
    }


class Offset(Builtin):
    pass


class Rectangle(Builtin):
    """
    <dl>
    <dt>'Rectangle[{$xmin$, $ymin$}]'
        <dd>represents a unit square with bottom-left corner at {$xmin$, $ymin$}.
    <dt>'Rectangle[{$xmin$, $ymin$}, {$xmax$, $ymax$}]
        <dd>is a rectange extending from {$xmin$, $ymin$} to {$xmax$, $ymax$}.
    </dl>

    >> Graphics[Rectangle[]]
     = -Graphics-

    >> Graphics[{Blue, Rectangle[{0.5, 0}], Orange, Rectangle[{0, 0.5}]}]
     = -Graphics-
    """

    rules = {
        'Rectangle[]': 'Rectangle[{0, 0}]',
    }


class Disk(Builtin):
    """
    <dl>
    <dt>'Disk[{$cx$, $cy$}, $r$]'
        <dd>fills a circle with center '($cx$, $cy$)' and radius $r$.
    <dt>'Disk[{$cx$, $cy$}, {$rx$, $ry$}]'
        <dd>fills an ellipse.
    <dt>'Disk[{$cx$, $cy$}]'
        <dd>chooses radius 1.
    <dt>'Disk[]'
        <dd>chooses center '(0, 0)' and radius 1.
    </dl>

    >> Graphics[{Blue, Disk[{0, 0}, {2, 1}]}]
     = -Graphics-
    The outer border can be drawn using 'EdgeForm':
    >> Graphics[{EdgeForm[Black], Red, Disk[]}]
     = -Graphics-
    """

    rules = {
        'Disk[]': 'Disk[{0, 0}]',
    }


class Circle(Builtin):
    """
    <dl>
    <dt>'Circle[{$cx$, $cy$}, $r$]'
        <dd>draws a circle with center '($cx$, $cy$)' and radius $r$.
    <dt>'Circle[{$cx$, $cy$}, {$rx$, $ry$}]'
        <dd>draws an ellipse.
    <dt>'Circle[{$cx$, $cy$}]'
        <dd>chooses radius 1.
    <dt>'Circle[]'
        <dd>chooses center '(0, 0)' and radius 1.
    </dl>

    >> Graphics[{Red, Circle[{0, 0}, {2, 1}]}]
     = -Graphics-
    """

    rules = {
        'Circle[]': 'Circle[{0, 0}]',
    }


class Inset(Builtin):
    pass


class Text(Inset):
    """
    #> Graphics[{Text[x, {0,0}]}]
     = -Graphics-
    """


class RectangleBox(_GraphicsElement):
    def init(self, graphics, style, item):
        super(RectangleBox, self).init(graphics, item, style)
        if len(item.leaves) not in (1, 2):
            raise BoxConstructError
        self.edge_color, self.face_color = style.get_style(
            _Color, face_element=True)
        self.p1 = Coords(graphics, item.leaves[0])
        if len(item.leaves) == 1:
            self.p2 = self.p1.add(1, 1)
        elif len(item.leaves) == 2:
            self.p2 = Coords(graphics, item.leaves[1])

    def extent(self):
        l = self.style.get_line_width(face_element=True) / 2
        result = []
        for p in [self.p1, self.p2]:
            x, y = p.pos()
            result.extend([(x - l, y - l), (
                x - l, y + l), (x + l, y - l), (x + l, y + l)])
        return result

    def to_svg(self):
        l = self.style.get_line_width(face_element=True)
        x1, y1 = self.p1.pos()
        x2, y2 = self.p2.pos()
        xmin = min(x1, x2)
        ymin = min(y1, y2)
        w = max(x1, x2) - xmin
        h = max(y1, y2) - ymin
        style = create_css(self.edge_color, self.face_color, l)
        return '<rect x="%f" y="%f" width="%f" height="%f" style="%s" />' % (
            xmin, ymin, w, h, style)

    def to_asy(self):
        l = self.style.get_line_width(face_element=True)
        x1, y1 = self.p1.pos()
        x2, y2 = self.p2.pos()
        pens = create_pens(
            self.edge_color, self.face_color, l, is_face_element=True)
        x1, x2, y1, y2 = asy_number(x1), asy_number(
            x2), asy_number(y1), asy_number(y2)
        return 'filldraw((%s,%s)--(%s,%s)--(%s,%s)--(%s,%s)--cycle, %s);' % (
            x1, y1, x2, y1, x2, y2, x1, y2, pens)


class _RoundBox(_GraphicsElement):
    face_element = None

    def init(self, graphics, style, item):
        super(_RoundBox, self).init(graphics, item, style)
        if len(item.leaves) not in (1, 2):
            raise BoxConstructError
        self.edge_color, self.face_color = style.get_style(
            _Color, face_element=self.face_element)
        self.c = Coords(graphics, item.leaves[0])
        if len(item.leaves) == 1:
            rx = ry = 1
        elif len(item.leaves) == 2:
            r = item.leaves[1]
            if r.has_form('List', 2):
                rx = r.leaves[0].to_number()
                ry = r.leaves[1].to_number()
            else:
                rx = ry = r.to_number()
        self.r = self.c.add(rx, ry)

    def extent(self):
        l = self.style.get_line_width(face_element=self.face_element) / 2
        x, y = self.c.pos()
        rx, ry = self.r.pos()
        rx -= x
        ry = y - ry
        rx += l
        ry += l
        return [(x - rx, y - ry), (x - rx, y + ry),
                (x + rx, y - ry), (x + rx, y + ry)]

    def to_svg(self):
        x, y = self.c.pos()
        rx, ry = self.r.pos()
        rx -= x
        ry = y - ry
        l = self.style.get_line_width(face_element=self.face_element)
        style = create_css(self.edge_color, self.face_color, stroke_width=l)
        return '<ellipse cx="%f" cy="%f" rx="%f" ry="%f" style="%s" />' % (
            x, y, rx, ry, style)

    def to_asy(self):
        x, y = self.c.pos()
        rx, ry = self.r.pos()
        rx -= x
        ry -= y
        l = self.style.get_line_width(face_element=self.face_element)
        pen = create_pens(edge_color=self.edge_color,
                          face_color=self.face_color, stroke_width=l,
                          is_face_element=self.face_element)
        cmd = 'filldraw' if self.face_element else 'draw'
        return '%s(ellipse((%s,%s),%s,%s), %s);' % (
            cmd, asy_number(x), asy_number(y), asy_number(rx), asy_number(ry),
            pen)


class DiskBox(_RoundBox):
    face_element = True


class CircleBox(_RoundBox):
    face_element = False


class _Polyline(_GraphicsElement):
    def do_init(self, graphics, points):
        if not points.has_form('List', None):
            raise BoxConstructError
        if (points.leaves and points.leaves[0].has_form('List', None) and
            all(leaf.has_form('List', None)
                for leaf in points.leaves[0].leaves)):
            leaves = points.leaves
            self.multi_parts = True
        else:
            leaves = [Expression('List', *points.leaves)]
            self.multi_parts = False
        lines = []
        for leaf in leaves:
            if leaf.has_form('List', None):
                lines.append(leaf.leaves)
            else:
                raise BoxConstructError
        self.lines = [[graphics.coords(
            graphics, point) for point in line] for line in lines]

    def extent(self):
        l = self.style.get_line_width(face_element=False)
        result = []
        for line in self.lines:
            for c in line:
                x, y = c.pos()
                result.extend([(x - l, y - l), (
                    x - l, y + l), (x + l, y - l), (x + l, y + l)])
        return result


class Point(Builtin):
    """
    <dl>
    <dt>'Line[{$point_1$, $point_2$ ...}]'
        <dd>represents the point primitive.
    <dt>'Line[{{$p_11$, $p_12$, ...}, {$p_21$, $p_22$, ...}, ...}]'
        <dd>represents a number of point primitives.
    </dl>

    >> Graphics[Point[{0,0}]]
    = -Graphics-

    >> Graphics[Point[Table[{Sin[t], Cos[t]}, {t, 0, 2. Pi, Pi / 15.}]]]
    = -Graphics-

    >> Graphics3D[Point[Table[{Sin[t], Cos[t], 0}, {t, 0, 2. Pi, Pi / 15.}]]]
    = -Graphics3D-
    """
    pass


class PointBox(_Polyline):
    def init(self, graphics, style, item=None):
        super(PointBox, self).init(graphics, item, style)
        self.edge_color, self.face_color = style.get_style(
            _Color, face_element=True)
        if item is not None:
            if len(item.leaves) != 1:
                raise BoxConstructError
            points = item.leaves[0]
            if points.has_form('List', None) and len(points.leaves) != 0:
                if all(not leaf.has_form('List', None)
                       for leaf in points.leaves):
                    points = Expression('List', points)
            self.do_init(graphics, points)
        else:
            raise BoxConstructError

    def to_svg(self):
        l = self.style.get_line_width(face_element=False)
        style = create_css(edge_color=self.edge_color,
                           stroke_width=l, face_color=self.face_color)
        svg = ''
        for line in self.lines:
            for coords in line:
                svg += '<circle cx="%f" cy="%f" r="1" style="%s" />' % (
                    coords.pos()[0], coords.pos()[1], style)
        return svg

    def to_asy(self):
        pen = create_pens(face_color=self.face_color, is_face_element=False)

        asy = ''
        for line in self.lines:
            for coords in line:
                asy += 'dot(%s, %s);' % (coords.pos(), pen)

        return asy


class Line(Builtin):
    """
    <dl>
    <dt>'Line[{$point_1$, $point_2$ ...}]'
        <dd>represents the line primitive.
    <dt>'Line[{{$p_11$, $p_12$, ...}, {$p_21$, $p_22$, ...}, ...}]'
        <dd>represents a number of line primitives.
    </dl>

    >> Graphics[Line[{{0,1},{0,0},{1,0},{1,1}}]]
    = -Graphics-

    >> Graphics3D[Line[{{0,0,0},{0,1,1},{1,0,0}}]]
    = -Graphics3D-
    """
    pass


class LineBox(_Polyline):
    def init(self, graphics, style, item=None, lines=None):
        super(LineBox, self).init(graphics, item, style)
        self.edge_color, _ = style.get_style(_Color, face_element=False)
        if item is not None:
            if len(item.leaves) != 1:
                raise BoxConstructError
            points = item.leaves[0]
            self.do_init(graphics, points)
        elif lines is not None:
            self.lines = lines
        else:
            raise BoxConstructError

    def to_svg(self):
        l = self.style.get_line_width(face_element=False)
        style = create_css(edge_color=self.edge_color, stroke_width=l)
        svg = ''
        for line in self.lines:
            svg += '<polyline points="%s" style="%s" />' % (
                ' '.join(['%f,%f' % coords.pos() for coords in line]), style)
        return svg

    def to_asy(self):
        l = self.style.get_line_width(face_element=False)
        pen = create_pens(edge_color=self.edge_color, stroke_width=l)
        asy = ''
        for line in self.lines:
            path = '--'.join(['(%s,%s)' % coords.pos() for coords in line])
            asy += 'draw(%s, %s);' % (path, pen)
        return asy


class Polygon(Builtin):
    """
    <dl>
    <dt>'Polygon[{$point_1$, $point_2$ ...}]'
        <dd>represents the filled polygon primitive.
    <dt>'Polygon[{{$p_11$, $p_12$, ...}, {$p_21$, $p_22$, ...}, ...}]'
        <dd>represents a number of filled polygon primitives.
    </dl>

    >> Graphics[Polygon[{{1,0},{0,0},{0,1}}]]
    = -Graphics-

    >> Graphics3D[Polygon[{{0,0,0},{0,1,1},{1,0,0}}]]
    = -Graphics3D-
    """
    pass


class PolygonBox(_Polyline):
    def init(self, graphics, style, item=None):
        super(PolygonBox, self).init(graphics, item, style)
        self.edge_color, self.face_color = style.get_style(
            _Color, face_element=True)
        if item is not None:
            if len(item.leaves) not in (1, 2):
                raise BoxConstructError
            points = item.leaves[0]
            self.do_init(graphics, points)
            self.vertex_colors = None
            for leaf in item.leaves[1:]:
                if not leaf.has_form('Rule', 2):
                    raise BoxConstructError
                name = leaf.leaves[0].get_name()
                self.process_option(name, leaf.leaves[1])
        else:
            raise BoxConstructError

    def process_option(self, name, value):
        if name == 'VertexColors':
            if not value.has_form('List', None):
                raise BoxConstructError
            black = RGBColor(components=[0, 0, 0, 1])
            self.vertex_colors = [[black] * len(line) for line in self.lines]
            colors = value.leaves
            if not self.multi_parts:
                colors = [Expression('List', *colors)]
            for line_index, line in enumerate(self.lines):
                if line_index >= len(colors):
                    break
                line_colors = colors[line_index]
                if not line_colors.has_form('List', None):
                    continue
                for index, color in enumerate(line_colors.leaves):
                    if index >= len(self.vertex_colors[line_index]):
                        break
                    try:
                        self.vertex_colors[line_index][
                            index] = _Color.create(color)
                    except ColorError:
                        continue
        else:
            raise BoxConstructError

    def to_svg(self):
        l = self.style.get_line_width(face_element=True)
        if self.vertex_colors is None:
            face_color = self.face_color
        else:
            face_color = None
        style = create_css(
            edge_color=self.edge_color, face_color=face_color, stroke_width=l)
        svg = ''
        if self.vertex_colors is not None:
            mesh = []
            for index, line in enumerate(self.lines):
                data = [[coords.pos(), color.to_js()] for coords, color in zip(
                    line, self.vertex_colors[index])]
                mesh.append(data)
            svg += '<meshgradient data="%s" />' % simplejson.dumps(mesh)
        for line in self.lines:
            svg += '<polygon points="%s" style="%s" />' % (
                ' '.join('%f,%f' % coords.pos() for coords in line), style)
        return svg

    def to_asy(self):
        l = self.style.get_line_width(face_element=True)
        if self.vertex_colors is None:
            face_color = self.face_color
        else:
            face_color = None
        pens = create_pens(edge_color=self.edge_color, face_color=face_color,
                           stroke_width=l, is_face_element=True)
        asy = ''
        if self.vertex_colors is not None:
            paths = []
            colors = []
            edges = []
            for index, line in enumerate(self.lines):
                paths.append('--'.join([
                    '(%s,%s)' % coords.pos() for coords in line]) + '--cycle')

                # ignore opacity
                colors.append(','.join([
                    color.to_asy()[0] for color in self.vertex_colors[index]]))

                edges.append(','.join(['0'] + ['1'] * (
                    len(self.vertex_colors[index]) - 1)))

            asy += 'gouraudshade(%s, new pen[] {%s}, new int[] {%s});' % (
                '^^'.join(paths), ','.join(colors), ','.join(edges))
        if pens and pens != 'nullpen':
            for line in self.lines:
                path = '--'.join(
                    ['(%s,%s)' % coords.pos() for coords in line]) + '--cycle'
                asy += 'filldraw(%s, %s);' % (path, pens)
        return asy


class InsetBox(_GraphicsElement):
    def init(self, graphics, style, item=None, content=None, pos=None,
             opos=(0, 0)):
        super(InsetBox, self).init(graphics, item, style)
        self.color, _ = style.get_style(_Color, face_element=False)
        if item is not None:
            if len(item.leaves) not in (1, 2, 3):
                raise BoxConstructError
            content = item.leaves[0]
            self.content = content.format(
                graphics.evaluation, 'TraditionalForm')
            if len(item.leaves) > 1:
                self.pos = Coords(graphics, item.leaves[1])
            else:
                self.pos = Coords(graphics, pos=(0, 0))
            if len(item.leaves) > 2:
                self.opos = coords(item.leaves[2])
            else:
                self.opos = (0, 0)
        else:
            self.content = content
            self.pos = pos
            self.opos = opos
        self.content_text = self.content.boxes_to_text(
            evaluation=self.graphics.evaluation)

    def extent(self):
        p = self.pos.pos()
        h = 25
        w = len(self.content_text) * \
            7  # rough approximation by numbers of characters
        opos = self.opos
        x = p[0] - w / 2.0 - opos[0] * w / 2.0
        y = p[1] - h / 2.0 + opos[1] * h / 2.0
        return [(x, y), (x + w, y + h)]

    def to_svg(self):
        x, y = self.pos.pos()
        content = self.content.boxes_to_xml(
            evaluation=self.graphics.evaluation)
        style = create_css(font_color=self.color)
        svg = ('<foreignObject x="%f" y="%f" ox="%f" oy="%f" style="%s">'
               '<math>%s</math></foreignObject>') % (
                    x, y, self.opos[0], self.opos[1], style, content)  # nopep8
        return svg

    def to_asy(self):
        x, y = self.pos.pos()
        content = self.content.boxes_to_tex(
            evaluation=self.graphics.evaluation)
        pen = create_pens(edge_color=self.color)
        asy = 'label("$%s$", (%s,%s), (%s,%s), %s);' % (
            content, x, y, -self.opos[0], -self.opos[1], pen)
        return asy


def total_extent(extents):
    xmin = xmax = ymin = ymax = None
    for extent in extents:
        for x, y in extent:
            if xmin is None or x < xmin:
                xmin = x
            if xmax is None or x > xmax:
                xmax = x
            if ymin is None or y < ymin:
                ymin = y
            if ymax is None or y > ymax:
                ymax = y
    return xmin, xmax, ymin, ymax


class EdgeForm(Builtin):
    pass


class FaceForm(Builtin):
    pass


class Style(object):
    def __init__(self, graphics, edge=False, face=False):
        self.styles = []
        self.graphics = graphics
        self.edge = edge
        self.face = face
        self.klass = graphics.get_style_class()

    def append(self, item, allow_forms=True):
        head = item.get_head_name()
        if head in color_heads:
            style = get_class(head)(item)
        elif head in thickness_heads:
            style = get_class(head)(self.graphics, item)
        elif head in ('EdgeForm', 'FaceForm'):
            style = self.klass(self.graphics, edge=head == 'EdgeForm',
                               face=head == 'FaceForm')
            if len(item.leaves) > 1:
                raise BoxConstructError
            if item.leaves:
                if item.leaves[0].has_form('List', None):
                    for dir in item.leaves[0].leaves:
                        style.append(dir, allow_forms=False)
                else:
                    style.append(item.leaves[0], allow_forms=False)
        else:
            raise BoxConstructError
        self.styles.append(style)

    def extend(self, style, pre=True):
        if pre:
            self.styles = style.styles + self.styles
        else:
            self.styles.extend(style.styles)

    def clone(self):
        result = self.klass(self.graphics, edge=self.edge, face=self.face)
        result.styles = self.styles[:]
        return result

    def get_default_face_color(self):
        return RGBColor(components=(0, 0, 0, 1))

    def get_default_edge_color(self):
        return RGBColor(components=(0, 0, 0, 1))

    def get_style(self, style_class, face_element=None, default_to_faces=True,
                  consider_forms=True):
        if face_element is not None:
            default_to_faces = consider_forms = face_element
        edge_style = face_style = None
        if style_class == _Color:
            if default_to_faces:
                face_style = self.get_default_face_color()
            else:
                edge_style = self.get_default_edge_color()
        elif style_class == _Thickness:
            if not default_to_faces:
                edge_style = AbsoluteThickness(self.graphics, value=0.5)
        for item in self.styles:
            if isinstance(item, style_class):
                if default_to_faces:
                    face_style = item
                else:
                    edge_style = item
            elif isinstance(item, Style):
                if consider_forms:
                    if item.edge:
                        edge_style, _ = item.get_style(
                            style_class, default_to_faces=False,
                            consider_forms=False)
                    elif item.face:
                        _, face_style = item.get_style(
                            style_class, default_to_faces=True,
                            consider_forms=False)

        return edge_style, face_style

    def get_line_width(self, face_element=True):
        if self.graphics.pixel_width is None:
            return 0
        edge_style, _ = self.get_style(
            _Thickness, default_to_faces=face_element,
            consider_forms=face_element)
        if edge_style is None:
            return 0
        return edge_style.get_thickness()


class _GraphicsElements(object):
    def __init__(self, content, evaluation):
        self.evaluation = evaluation
        self.elements = []

        def convert(content, style):
            if content.has_form('List', None):
                items = content.leaves
            else:
                items = [content]
            style = style.clone()
            for item in items:
                if item.get_name() == 'Null':
                    continue
                head = item.get_head_name()
                if (head in color_heads or head in thickness_heads or   # noqa
                    head in ('EdgeForm', 'FaceForm')):
                    style.append(item)
                elif head[-3:] == 'Box':  # and head[:-3] in element_heads:
                    element_class = get_class(head)
                    if element_class is not None:
                        element = get_class(head)(self, style, item)
                        self.elements.append(element)
                    else:
                        raise BoxConstructError
                elif head == 'List':
                    convert(item, style)
                else:
                    raise BoxConstructError

        convert(content, self.get_style_class()(self))

    def create_style(self, expr):
        style = self.get_style_class()(self)

        def convert(expr):
            if expr.has_form(('List', 'Directive'), None):
                for item in expr.leaves:
                    convert(item)
            else:
                style.append(expr)

        convert(expr)
        return style

    def get_style_class(self):
        return Style


class GraphicsElements(_GraphicsElements):
    coords = Coords

    def __init__(self, content, evaluation, neg_y=False):
        super(GraphicsElements, self).__init__(content, evaluation)
        self.neg_y = neg_y
        self.xmin = self.ymin = self.pixel_width = None
        self.pixel_height = self.extent_width = self.extent_height = None

    def translate(self, coords):
        if self.pixel_width is not None:
            w = self.extent_width if self.extent_width > 0 else 1
            h = self.extent_height if self.extent_height > 0 else 1
            result = [(coords[0] - self.xmin) * self.pixel_width / w,
                      (coords[1] - self.ymin) * self.pixel_height / h]
            if self.neg_y:
                result[1] = self.pixel_height - result[1]
            return tuple(result)
        else:
            return (coords[0], coords[1])

    def translate_absolute(self, d):
        if self.pixel_width is None:
            return (0, 0)
        else:
            l = 96.0 / 72
            return (d[0] * l, (-1 if self.neg_y else 1) * d[1] * l)

    def translate_relative(self, x):
        if self.pixel_width is None:
            return 0
        else:
            return x * self.pixel_width

    def extent(self, completely_visible_only=False):
        if completely_visible_only:
            ext = total_extent([element.extent() for element in self.elements
                                if element.is_completely_visible])
        else:
            ext = total_extent([element.extent() for element in self.elements])
        xmin, xmax, ymin, ymax = ext
        if xmin == xmax:
            xmin = 0
            xmax *= 2
        if ymin == ymax:
            ymin = 0
            ymax *= 2
        return xmin, xmax, ymin, ymax

    def to_svg(self):
        return '\n'.join(element.to_svg() for element in self.elements)

    def to_asy(self):
        return '\n'.join(element.to_asy() for element in self.elements)

    def set_size(self, xmin, ymin, extent_width, extent_height, pixel_width,
                 pixel_height):

        self.xmin, self.ymin = xmin, ymin
        self.extent_width, self.extent_height = extent_width, extent_height
        self.pixel_width, self.pixel_height = pixel_width, pixel_height


class GraphicsBox(BoxConstruct):
    options = Graphics.options

    attributes = ('HoldAll', 'ReadProtected')

    def boxes_to_text(self, leaves, **options):
        self._prepare_elements(leaves, options)  # to test for Box errors
        return '-Graphics-'

    def _get_image_size(self, options, graphics_options, max_width):
        inside_row = options.pop('inside_row', False)
        inside_list = options.pop('inside_list', False)
        image_size_multipliers = options.pop('image_size_multipliers', None)

        aspect_ratio = graphics_options['AspectRatio']

        if image_size_multipliers is None:
            image_size_multipliers = (0.5, 0.25)

        if aspect_ratio == Symbol('Automatic'):
            aspect = None
        else:
            try:
                aspect = aspect_ratio.to_number()
            except NumberError:
                # TODO: Custom message - MMA uses a tooltip over red graphics
                aspect = None

        image_size = graphics_options['ImageSize']
        image_size = image_size.get_name()
        base_width, base_height = {
            'Automatic': (400, 350),
            'Tiny': (100, 100),
            'Small': (200, 200),
            'Medium': (400, 350),
            'Large': (600, 500),
        }.get(image_size, (None, None))
        if base_width is None:
            raise BoxConstructError
        if max_width is not None and base_width > max_width:
            base_width = max_width

        if inside_row:
            multi = image_size_multipliers[1]
        elif inside_list:
            multi = image_size_multipliers[0]
        else:
            multi = 1

        return base_width, base_height, multi, aspect

    def _prepare_elements(self, leaves, options, neg_y=False, max_width=None):
        if not leaves:
            raise BoxConstructError

        graphics_options = self.get_option_values(leaves[1:], **options)

        base_width, base_height, size_multiplier, size_aspect = \
            self._get_image_size(options, graphics_options, max_width)

        plot_range = graphics_options['PlotRange'].to_python()
        if plot_range == 'Automatic':
            plot_range = ['Automatic', 'Automatic']
        if not isinstance(plot_range, list) or len(plot_range) != 2:
            raise BoxConstructError

        try:
            elements = GraphicsElements(leaves[
                                        0], options['evaluation'], neg_y)
        except NumberError:
            raise BoxConstructError

        def calc_dimensions(final_pass=True):
            """
            calc_dimensions gets called twice: In the first run
            (final_pass = False, called inside _prepare_elements), the extent
            of all user-defined graphics is determined.
            Axes are created accordingly.
            In the second run (final_pass = True, called from outside),
            the dimensions of these axes are taken into account as well.
            This is also important to size absolutely sized objects correctly
            (e.g. values using AbsoluteThickness).
            """

            if 'Automatic' in plot_range:
                xmin, xmax, ymin, ymax = elements.extent()
            else:
                xmin = xmax = ymin = ymax = None
            if final_pass and plot_range != ['Automatic', 'Automatic']:
                # Take into account the dimensiosn of axes and axes labels
                # (they should be displayed completely even when a specific
                # PlotRange is given).
                exmin, exmax, eymin, eymax = elements.extent(
                    completely_visible_only=True)
            else:
                exmin = exmax = eymin = eymax = None

            def get_range(min, max):
                if max < min:
                    min, max = max, min
                elif min == max:
                    if min < 0:
                        min, max = 2 * min, 0
                    elif min > 0:
                        min, max = 0, 2 * min
                    else:
                        min, max = -1, 1
                return min, max

            try:
                if plot_range[0] == 'Automatic':
                    if xmin is None and xmax is None:
                        xmin = 0
                        xmax = 1
                    elif xmin == xmax:
                        xmin -= 1
                        xmax += 1
                elif (isinstance(plot_range[0], list) and
                      len(plot_range[0]) == 2):
                    xmin, xmax = map(float, plot_range[0])
                    xmin, xmax = get_range(xmin, xmax)
                    xmin = elements.translate((xmin, 0))[0]
                    xmax = elements.translate((xmax, 0))[0]
                    if exmin is not None and exmin < xmin:
                        xmin = exmin
                    if exmax is not None and exmax > xmax:
                        xmax = exmax
                else:
                    raise BoxConstructError

                if plot_range[1] == 'Automatic':
                    if ymin is None and ymax is None:
                        ymin = 0
                        ymax = 1
                    elif ymin == ymax:
                        ymin -= 1
                        ymax += 1
                elif (isinstance(plot_range[1], list) and
                      len(plot_range[1]) == 2):
                    ymin, ymax = map(float, plot_range[1])
                    ymin, ymax = get_range(ymin, ymax)
                    ymin = elements.translate((0, ymin))[1]
                    ymax = elements.translate((0, ymax))[1]
                    if eymin is not None and eymin < ymin:
                        ymin = eymin
                    if eymax is not None and eymax > ymax:
                        ymax = eymax
                else:
                    raise BoxConstructError
            except (ValueError, TypeError):
                raise BoxConstructError

            w = xmax - xmin
            h = ymax - ymin

            if size_aspect is None:
                aspect = h / w
            else:
                aspect = size_aspect

            height = base_height
            width = height / aspect
            if width > base_width:
                width = base_width
                height = width * aspect
            height = height

            width *= size_multiplier
            height *= size_multiplier

            return xmin, xmax, ymin, ymax, w, h, width, height

        xmin, xmax, ymin, ymax, w, h, width, height = calc_dimensions(
            final_pass=False)

        elements.set_size(xmin, ymin, w, h, width, height)

        xmin -= w * 0.02
        xmax += w * 0.02
        ymin -= h * 0.02
        ymax += h * 0.02

        self.create_axes(elements, graphics_options, xmin, xmax, ymin, ymax)

        return elements, calc_dimensions

    def boxes_to_tex(self, leaves, **options):
        elements, calc_dimensions = self._prepare_elements(
            leaves, options, max_width=450)

        asy_completely_visible = '\n'.join(
            element.to_asy() for element in elements.elements
            if element.is_completely_visible)

        asy_regular = '\n'.join(
            element.to_asy() for element in elements.elements
            if not element.is_completely_visible)

        xmin, xmax, ymin, ymax, w, h, width, height = calc_dimensions()

        tex = r"""
\begin{asy}
size(%scm, %scm);
%s
clip(box((%s,%s), (%s,%s)));
%s
\end{asy}
""" % (asy_number(width / 60), asy_number(height / 60),
       asy_regular,
       asy_number(xmin), asy_number(ymin), asy_number(xmax), asy_number(ymax),
       asy_completely_visible)

        return tex

    def boxes_to_xml(self, leaves, **options):
        elements, calc_dimensions = self._prepare_elements(
            leaves, options, neg_y=True)

        svg = elements.to_svg()

        xmin, xmax, ymin, ymax, w, h, width, height = calc_dimensions()

        xmin -= 1
        ymin -= 1
        w += 2
        h += 2

        xml = ('<svg xmlns:svg="http://www.w3.org/2000/svg" '
               'xmlns="http://www.w3.org/2000/svg"\nversion="1.0" width="%f" '
               'height="%f" viewBox="%f %f %f %f">%s</svg>') % (
                    width, height, xmin, ymin, w, h, svg)   # nopep8

        xml = """<mtable><mtr><mtd>%s</mtd></mtr></mtable>""" % xml
        return xml

    def axis_ticks(self, xmin, xmax):
        def round(value):
            if value >= 0:
                return int(value + 0.5)
            else:
                return int(value - 0.5)

        def round_to_zero(value):
            if value == 0:
                return 0
            elif value < 0:
                return ceil(value)
            else:
                return floor(value)

        def round_step(value):
            if not value:
                return 1, 1
            sub_steps = 5
            try:
                shift = 10.0 ** floor(log10(value))
            except ValueError:
                print value
                return 1, 1
            value = value / shift
            if value < 1.5:
                value = 1
            elif value < 3:
                value = 2
                sub_steps = 4
            elif value < 8:
                value = 5
            else:
                value = 10
            return value * shift, sub_steps

        step_x, sub_x = round_step((xmax - xmin) / 5.0)
        step_x_small = 1.0 * step_x / sub_x
        steps_x = int(floor((xmax - xmin) / step_x))
        steps_x_small = int(floor((xmax - xmin) / step_x_small))

        start_k_x = int(ceil(xmin / step_x))
        start_k_x_small = int(ceil(xmin / step_x_small))

        start_x = step_x * round_to_zero((xmax - xmin) / step_x)
        start_x_small = step_x_small * \
            round_to_zero((xmax - xmin) / step_x_small)

        zero_tolerance = 0.01
        if xmax > min:
            if xmin > 0 and xmin / (xmax - xmin) < zero_tolerance:
                xmin = 0
            if xmax < 0 and xmax / (xmax - xmin) < zero_tolerance:
                xmax = 0
        if xmin <= 0 <= xmax:
            origin_k_x = 0
        else:
            origin_k_x = start_k_x
        origin_x = origin_k_x * step_x

        ticks = []
        ticks_small = []
        for k in range(start_k_x, start_k_x + steps_x + 1):
            if k != origin_k_x:
                x = k * step_x
                if x > xmax:
                    break
                ticks.append(x)
        for k in range(start_k_x_small, start_k_x_small + steps_x_small + 1):
            if k % sub_x != 0:
                x = k * step_x_small
                if x > xmax:
                    break
                ticks_small.append(x)

        return ticks, ticks_small, origin_x

    def create_axes(self, elements, graphics_options, xmin, xmax, ymin, ymax):
        axes = graphics_options.get('Axes')
        if axes.is_true():
            axes = (True, True)
        elif axes.has_form('List', 2):
            axes = (axes.leaves[0].is_true(), axes.leaves[1].is_true())
        else:
            axes = (False, False)
        ticks_style = graphics_options.get('TicksStyle')
        axes_style = graphics_options.get('AxesStyle')
        label_style = graphics_options.get('LabelStyle')
        if ticks_style.has_form('List', 2):
            ticks_style = ticks_style.leaves
        else:
            ticks_style = [ticks_style] * 2
        if axes_style.has_form('List', 2):
            axes_style = axes_style.leaves
        else:
            axes_style = [axes_style] * 2

        ticks_style = [elements.create_style(s) for s in ticks_style]
        axes_style = [elements.create_style(s) for s in axes_style]
        label_style = elements.create_style(label_style)
        ticks_style[0].extend(axes_style[0])
        ticks_style[1].extend(axes_style[1])

        def add_element(element):
            element.is_completely_visible = True
            elements.elements.append(element)

        ticks_x, ticks_x_small, origin_x = self.axis_ticks(xmin, xmax)
        ticks_y, ticks_y_small, origin_y = self.axis_ticks(ymin, ymax)

        axes_extra = 6
        tick_small_size = 3
        tick_large_size = 5
        tick_label_d = 2

        for index, (
            min, max, p_self0, p_other0, p_origin,
            ticks, ticks_small) in enumerate([
                (xmin, xmax, lambda y: (0, y), lambda x: (x, 0),
                 lambda x: (x, origin_y), ticks_x, ticks_x_small),
                (ymin, ymax, lambda x: (x, 0), lambda y: (0, y),
                 lambda y: (origin_x, y), ticks_y, ticks_y_small)]):
            if axes[index]:
                add_element(LineBox(
                    elements, axes_style[index],
                    lines=[[Coords(elements, pos=p_origin(min),
                                   d=p_other0(-axes_extra)),
                            Coords(elements, pos=p_origin(max),
                                   d=p_other0(axes_extra))]]))
                ticks_lines = []
                tick_label_style = ticks_style[index].clone()
                tick_label_style.extend(label_style)
                for x in ticks:
                    ticks_lines.append([Coords(elements, pos=p_origin(x)),
                                        Coords(elements, pos=p_origin(x),
                                               d=p_self0(tick_large_size))])
                    add_element(InsetBox(
                        elements, tick_label_style, content=Real(x),
                        pos=Coords(elements, pos=p_origin(x),
                                   d=p_self0(-tick_label_d)), opos=p_self0(1)))
                for x in ticks_small:
                    pos = p_origin(x)
                    ticks_lines.append([Coords(elements, pos=pos),
                                        Coords(elements, pos=pos,
                                               d=p_self0(tick_small_size))])
                add_element(LineBox(elements, axes_style[0],
                                    lines=ticks_lines))

        """if axes[1]:
            add_element(LineBox(elements, axes_style[1], lines=[[Coords(elements, pos=(origin_x,ymin), d=(0,-axes_extra)),
                Coords(elements, pos=(origin_x,ymax), d=(0,axes_extra))]]))
            ticks = []
            tick_label_style = ticks_style[1].clone()
            tick_label_style.extend(label_style)
            for k in range(start_k_y, start_k_y+steps_y+1):
                if k != origin_k_y:
                    y = k * step_y
                    if y > ymax:
                        break
                    pos = (origin_x,y)
                    ticks.append([Coords(elements, pos=pos),
                        Coords(elements, pos=pos, d=(tick_large_size,0))])
                    add_element(InsetBox(elements, tick_label_style, content=Real(y), pos=Coords(elements, pos=pos,
                        d=(-tick_label_d,0)), opos=(1,0)))
            for k in range(start_k_y_small, start_k_y_small+steps_y_small+1):
                if k % sub_y != 0:
                    y = k * step_y_small
                    if y > ymax:
                        break
                    pos = (origin_x,y)
                    ticks.append([Coords(elements, pos=pos),
                        Coords(elements, pos=pos, d=(tick_small_size,0))])
            add_element(LineBox(elements, axes_style[1], lines=ticks))"""


class Directive(Builtin):
    attributes = ('ReadProtected',)


class Blend(Builtin):
    """
    >> Blend[{Red, Blue}]
     = RGBColor[0.5, 0., 0.5, 1.]
    >> Blend[{Red, Blue}, 0.3]
     = RGBColor[0.7, 0., 0.3, 1.]
    >> Blend[{Red, Blue, Green}, 0.75]
     = RGBColor[0., 0.5, 0.5, 1.]

    >> Graphics[Table[{Blend[{Red, Green, Blue}, x], Rectangle[{10 x, 0}]}, {x, 0, 1, 1/10}]]
     = -Graphics-

    >> Graphics[Table[{Blend[{RGBColor[1, 0.5, 0, 0.5], RGBColor[0, 0, 1, 0.5]}, x], Disk[{5x, 0}]}, {x, 0, 1, 1/10}]]
     = -Graphics-

    #> Blend[{Red, Green, Blue}, {1, 0.5}]
     : {1, 0.5} should be a real number or a list of non-negative numbers, which has the same length as {RGBColor[1, 0, 0], RGBColor[0, 1, 0], RGBColor[0, 0, 1]}.
     = Blend[{RGBColor[1, 0, 0], RGBColor[0, 1, 0], RGBColor[0, 0, 1]}, {1, 0.5}]
    """

    messages = {
        'arg': ("`1` is not a valid list of color or gray-level directives, "
                "or pairs of a real number and a directive."),
        'argl': ("`1` should be a real number or a list of non-negative "
                 "numbers, which has the same length as `2`."),
    }

    rules = {
        'Blend[colors_]': 'Blend[colors, ConstantArray[1, Length[colors]]]',
    }

    def do_blend(self, colors, values):
        type = None
        homogenous = True
        for color in colors:
            if type is None:
                type = color.__class__
            else:
                if color.__class__ != type:
                    homogenous = False
                    break
        if not homogenous:
            colors = [RGBColor(components=color.to_rgba()) for color in colors]
            type = RGBColor
        total = sum(values)
        result = None
        for color, value in zip(colors, values):
            frac = value / total
            part = [component * frac for component in color.components]
            if result is None:
                result = part
            else:
                result = [r + p for r, p in zip(result, part)]
        return type(components=result)

    def apply(self, colors, u, evaluation):
        'Blend[{colors___}, u_]'

        colors_orig = colors
        try:
            colors = [_Color.create(color) for color in colors.get_sequence()]
            if not colors:
                raise ColorError
        except ColorError:
            evaluation.message('Blend', 'arg', Expression('List', colors_orig))
            return
        try:
            if u.has_form('List', None):
                values = [value.to_number() for value in u.leaves]
                if len(u.leaves) != len(colors):
                    raise NumberError(None)  # pseudo NumberError caught below
                use_list = True
            else:
                values = u.to_number(min=0, max=1)
                use_list = False
        except NumberError:
            evaluation.message('Blend', 'argl', u, Expression(
                'List', colors_orig))
            return

        if use_list:
            return self.do_blend(colors, values).to_expr()
        else:
            x = values
            pos = int(floor(x * (len(colors) - 1)))
            x = (x - pos * 1.0 / (len(colors) - 1)) * (len(colors) - 1)
            if pos == len(colors) - 1:
                return colors[-1].to_expr()
            else:
                return self.do_blend(colors[pos:(pos + 2)],
                                     [1 - x, x]).to_expr()


class Lighter(Builtin):
    """
    <dl>
    <dt>'Lighter[$c$, $f$]'
        <dd>is equivalent to 'Blend[{$c$, White}, $f$]'.
    <dt>'Lighter[$c$]'
        <dd>is equivalent to 'Lighter[$c$, 1/3]'.
    </dl>

    >> Lighter[Orange, 1/4]
     = RGBColor[1., 0.625, 0.25, 1.]
    >> Graphics[{Lighter[Orange, 1/4], Disk[]}]
     = -Graphics-
    >> Graphics[Table[{Lighter[Orange, x], Disk[{12x, 0}]}, {x, 0, 1, 1/6}]]
     = -Graphics-
    """

    rules = {
        'Lighter[c_, f_]': 'Blend[{c, White}, f]',
        'Lighter[c_]': 'Lighter[c, 1/3]',
    }


class Darker(Builtin):
    """
    <dl>
    <dt>'Darker[$c$, $f$]'
        <dd>is equivalent to 'Blend[{$c$, Black}, $f$]'.
    <dt>'Darker[$c$]'
        <dd>is equivalent to 'Darker[$c$, 1/3]'.
    </dl>

    >> Graphics[Table[{Darker[Yellow, x], Disk[{12x, 0}]}, {x, 0, 1, 1/6}]]
     = -Graphics-
    """

    rules = {
        'Darker[c_, f_]': 'Blend[{c, Black}, f]',
        'Darker[c_]': 'Darker[c, 1/3]',
    }


class _ColorObject(Builtin):
    text_name = None

    def __init__(self, *args, **kwargs):
        super(_ColorObject, self).__init__(*args, **kwargs)

        if self.text_name is None:
            text_name = self.get_name().lower()
        else:
            text_name = self.text_name
        doc = """
            <dl>
            <dt>'%(name)s'
            <dd>represents the color %(text_name)s in graphics.
            </dl>

            >> Graphics[{%(name)s, Disk[]}, ImageSize->Small]
             = -Graphics-
        """ % {'name': self.get_name(), 'text_name': text_name}
        if self.__doc__ is None:
            self.__doc__ = doc
        else:
            self.__doc__ = doc + self.__doc__


class Black(_ColorObject):
    """
    >> Black
     = GrayLevel[0]
    """

    rules = {
        'Black': 'GrayLevel[0]',
    }


class White(_ColorObject):
    """
    >> White
     = GrayLevel[1]
    """

    rules = {
        'White': 'GrayLevel[1]',
    }


class Gray(_ColorObject):
    """
    >> Gray
     = GrayLevel[0.5]
    """

    rules = {
        'Gray': 'GrayLevel[0.5]',
    }


class Red(_ColorObject):
    """
    >> Red
     = RGBColor[1, 0, 0]
    """

    rules = {
        'Red': 'RGBColor[1, 0, 0]',
    }


class Green(_ColorObject):
    """
    >> Green
     = RGBColor[0, 1, 0]
    """

    rules = {
        'Green': 'RGBColor[0, 1, 0]',
    }


class Blue(_ColorObject):
    """
    >> Blue
     = RGBColor[0, 0, 1]
    """

    rules = {
        'Blue': 'RGBColor[0, 0, 1]',
    }


class Cyan(_ColorObject):
    """
    >> Cyan
     = RGBColor[0, 1, 1]
    """

    rules = {
        'Cyan': 'RGBColor[0, 1, 1]',
    }


class Magenta(_ColorObject):
    """
    >> Magenta
     = RGBColor[1, 0, 1]
    """

    rules = {
        'Magenta': 'RGBColor[1, 0, 1]',
    }


class Yellow(_ColorObject):
    """
    >> Yellow
     = RGBColor[1, 1, 0]
    """

    rules = {
        'Yellow': 'RGBColor[1, 1, 0]',
    }


class Purple(_ColorObject):
    rules = {
        'Purple': 'RGBColor[0.5, 0, 0.5]',
    }


class LightRed(_ColorObject):
    text_name = 'light red'

    rules = {
        'LightRed': 'Lighter[Red, 0.85]',
    }


class Orange(_ColorObject):
    rules = {
        'Orange': 'RGBColor[1, 0.5, 0]',
    }

GLOBALS = {
    'Rectangle': Rectangle,
    'Disk': Disk,
    'Circle': Circle,
    'Polygon': Polygon,
    'Inset': Inset,
    'Text': Text,
    'RectangleBox': RectangleBox,
    'DiskBox': DiskBox,
    'LineBox': LineBox,
    'CircleBox': CircleBox,
    'PolygonBox': PolygonBox,
    'PointBox': PointBox,
    'InsetBox': InsetBox,

    'RGBColor': RGBColor,
    'CMYKColor': CMYKColor,
    'Hue': Hue,
    'GrayLevel': GrayLevel,

    'Thickness': Thickness,
    'AbsoluteThickness': AbsoluteThickness,
    'Thick': Thick,
    'Thin': Thin,
}
