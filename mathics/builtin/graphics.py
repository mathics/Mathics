#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Graphics
"""

from __future__ import unicode_literals
from __future__ import absolute_import
from __future__ import division

from math import floor, ceil, log10, sin, cos, pi, sqrt, atan2, degrees, radians, exp
import json
import base64
from six.moves import map
from six.moves import range
from six.moves import zip
from itertools import chain
from math import sin, cos, pi
from sympy.matrices import Matrix

from mathics.builtin.base import (
    Builtin, InstancableBuiltin, BoxConstruct, BoxConstructError)
from mathics.builtin.options import options_to_rules
from mathics.core.expression import (
    Expression, Integer, Rational, Real, String, Symbol, strip_context,
    system_symbols, system_symbols_dict, from_python)
from mathics.builtin.colors import convert as convert_color
from mathics.core.numbers import machine_epsilon


class CoordinatesError(BoxConstructError):
    pass


class ColorError(BoxConstructError):
    pass


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
        x, y = value.leaves[0].round_to_float(), value.leaves[1].round_to_float()
        if x is None or y is None:
            raise CoordinatesError
        return (x, y)
    raise CoordinatesError


class Coords(object):
    def __init__(self, graphics, expr=None, pos=None):
        self.graphics = graphics
        self.p = pos
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
        p = self.p
        p = (cut(p[0]), cut(p[1]))
        return p

    def add(self, x, y):
        p = (self.p[0] + x, self.p[1] + y)
        return Coords(self.graphics, pos=p)

    def is_absolute(self):
        return False


class AxisCoords(Coords):
    def __init__(self, graphics, expr=None, pos=None, d=None):
        super(AxisCoords, self).__init__(graphics, expr=expr, pos=pos)
        self.d = d

    def pos(self):
        p = self.p
        p = self.graphics.translate(p)
        p = (cut(p[0]), cut(p[1]))
        if self.d is not None:
            d = self.graphics.translate_absolute_in_pixels(self.d)
            return p[0] + d[0], p[1] + d[1]
        else:
            return p

    def add(self, x, y):
        raise NotImplementedError

    def is_absolute(self):
        return True


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
    return '%.5g' % value


def _to_float(x):
    x = x.round_to_float()
    if x is None:
        raise BoxConstructError
    return x


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


def _data_and_options(leaves, defined_options):
    data = []
    options = defined_options.copy()
    for leaf in leaves:
        if leaf.get_head_name() == 'System`Rule':
            if len(leaf.leaves) != 2:
                raise BoxConstructError
            name, value = leaf.leaves
            name_head = name.get_head_name()
            if name_head == 'System`Symbol':
                py_name = name.get_name()
            elif name_head == 'System`String':
                py_name = 'System`' + name.get_string_value()
            else:  # unsupported name type
                raise BoxConstructError
            options[py_name] = value
        else:
            data.append(leaf)
    return data, options


def _euclidean_distance(a, b):
    return sqrt(sum((x1 - x2) * (x1 - x2) for x1, x2 in zip(a, b)))


def _component_distance(a, b, i):
    return abs(a[i] - b[i])


def _cie2000_distance(lab1, lab2):
    #reference: https://en.wikipedia.org/wiki/Color_difference#CIEDE2000
    e = machine_epsilon
    kL = kC = kH = 1 #common values

    L1, L2 = lab1[0], lab2[0]
    a1, a2 = lab1[1], lab2[1]
    b1, b2 = lab1[2], lab2[2]

    dL = L2 - L1
    Lm = (L1 + L2)/2
    C1 = sqrt(a1**2 + b1**2)
    C2 = sqrt(a2**2 + b2**2)
    Cm = (C1 + C2)/2;

    a1 = a1 * (1 + (1 - sqrt(Cm**7/(Cm**7 + 25**7)))/2)
    a2 = a2 * (1 + (1 - sqrt(Cm**7/(Cm**7 + 25**7)))/2)

    C1 = sqrt(a1**2 + b1**2)
    C2 = sqrt(a2**2 + b2**2)
    Cm = (C1 + C2)/2
    dC = C2 - C1

    h1 = (180 * atan2(b1, a1 + e))/pi % 360
    h2 = (180 * atan2(b2, a2 + e))/pi % 360
    if abs(h2 - h1) <= 180:
        dh = h2 - h1
    elif abs(h2 - h1) > 180 and h2 <= h1:
        dh = h2 - h1 + 360
    elif abs(h2 - h1) > 180 and h2 > h1:
        dh = h2 - h1 - 360

    dH = 2*sqrt(C1*C2)*sin(radians(dh)/2)

    Hm = (h1 + h2)/2 if abs(h2 - h1) <= 180 else (h1 + h2 + 360)/2
    T = 1 - 0.17*cos(radians(Hm - 30)) + 0.24*cos(radians(2*Hm)) + 0.32*cos(radians(3*Hm + 6)) - 0.2*cos(radians(4*Hm - 63))

    SL = 1 + (0.015*(Lm - 50)**2)/sqrt(20 + (Lm - 50)**2)
    SC = 1 + 0.045*Cm
    SH = 1 + 0.015*Cm*T

    rT = -2 * sqrt(Cm**7/(Cm**7 + 25**7))*sin(radians(60*exp(-((Hm - 275)**2 / 25**2))))
    return sqrt((dL/(SL*kL))**2 + (dC/(SC*kC))**2 + (dH/(SH*kH))**2 + rT*(dC/(SC*kC))*(dH/(SH*kH)))


def _CMC_distance(lab1, lab2, l, c):
    #reference https://en.wikipedia.org/wiki/Color_difference#CMC_l:c_.281984.29
    L1, L2 = lab1[0], lab2[0]
    a1, a2 = lab1[1], lab2[1]
    b1, b2 = lab1[2], lab2[2]

    dL, da, db = L2-L1, a2-a1, b2-b1
    e = machine_epsilon

    C1 = sqrt(a1**2 + b1**2);
    C2 = sqrt(a2**2 + b2**2);

    h1 = (180 * atan2(b1, a1 + e))/pi % 360;
    dC = C2 - C1;
    dH2 = da**2 + db**2 - dC**2;
    F = C1**2/sqrt(C1**4 + 1900);
    T = 0.56 + abs(0.2*cos(radians(h1 + 168))) if (164 <= h1 and h1 <= 345) else 0.36 + abs(0.4*cos(radians(h1 + 35)));

    SL = 0.511 if L1 < 16 else (0.040975*L1)/(1 + 0.01765*L1);
    SC = (0.0638*C1)/(1 + 0.0131*C1) + 0.638;
    SH = SC*(F*T + 1 - F);
    return sqrt((dL/(l*SL))**2 + (dC/(c*SC))**2 + dH2/SH**2)


def _extract_graphics(graphics, format, evaluation):
    graphics_box = Expression('MakeBoxes', graphics).evaluate(evaluation)
    builtin = GraphicsBox(expression=False)

    elements, calc_dimensions = builtin._prepare_elements(
        graphics_box.leaves, {'evaluation': evaluation}, neg_y=True)

    if not isinstance(elements.elements[0], GeometricTransformationBox):
        raise ValueError('expected GeometricTransformationBox')

    contents = elements.elements[0].contents

    # generate code for svg or asy.

    if format == 'asy':
        code = '\n'.join(element.to_asy() for element in contents)
    elif format == 'svg':
        code = ''.join(element.to_svg() for element in contents)
    else:
        raise NotImplementedError

    return code


def _to_float(x):
    if isinstance(x, Integer):
        return x.get_int_value()
    else:
        y = x.round_to_float()
        if y is None:
            raise BoxConstructError
        return y


class _Transform():
    def __init__(self, f):
        if not isinstance(f, Expression):
            self.matrix = f
            return

        if f.get_head_name() != 'System`TransformationFunction':
            raise BoxConstructError

        if len(f.leaves) != 1 or f.leaves[0].get_head_name() != 'System`List':
            raise BoxConstructError

        rows = f.leaves[0].leaves
        if len(rows) != 3:
            raise BoxConstructError
        if any(row.get_head_name() != 'System`List' for row in rows):
            raise BoxConstructError
        if any(len(row.leaves) != 3 for row in rows):
            raise BoxConstructError

        self.matrix = [[_to_float(x) for x in row.leaves] for row in rows]

    def inverse(self):
        return _Transform(Matrix(self.matrix).inv().tolist())

    def multiply(self, other):
        a = self.matrix
        b = other.matrix
        return _Transform([[sum(a[i][k] * b[k][j] for k in range(3)) for j in range(3)] for i in range(3)])

    def transform(self, p):
        m = self.matrix

        m11 = m[0][0]
        m12 = m[0][1]
        m13 = m[0][2]

        m21 = m[1][0]
        m22 = m[1][1]
        m23 = m[1][2]

        for x, y in p:
            yield m11 * x + m12 * y + m13, m21 * x + m22 * y + m23

    def to_svg(self, svg):
        m = self.matrix

        a = m[0][0]
        b = m[1][0]
        c = m[0][1]
        d = m[1][1]
        e = m[0][2]
        f = m[1][2]

        if m[2][0] != 0. or m[2][1] != 0. or m[2][2] != 1.:
            raise BoxConstructError

        # a c e
        # b d f
        # 0 0 1

        t = 'matrix(%f, %f, %f, %f, %f, %f)' % (a, b, c, d, e, f)
        return '<g transform="%s">%s</g>' % (t, svg)

    def to_asy(self, asy):
        m = self.matrix

        a = m[0][0]
        b = m[1][0]
        c = m[0][1]
        d = m[1][1]
        e = m[0][2]
        f = m[1][2]

        if m[2][0] != 0. or m[2][1] != 0. or m[2][2] != 1.:
            raise BoxConstructError

        # a c e
        # b d f
        # 0 0 1
        # see http://asymptote.sourceforge.net/doc/Transforms.html#Transforms
        t = ','.join(map(asy_number, (e, f, a, c, b, d)))

        return ''.join(("add((", t, ")*(new picture(){",
                        "picture s=currentpicture,t=new picture;currentpicture=t;", asy,
                        "currentpicture=s;return t;})());"))


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

    >> Graphics[{Rectangle[],Red,Disk[{1,0}]},PlotRange->{{0,1},{0,1}}]
     = -Graphics-

    'Graphics' produces 'GraphicsBox' boxes:
    >> Graphics[Rectangle[]] // ToBoxes // Head
     = GraphicsBox

    In 'TeXForm', 'Graphics' produces Asymptote figures:
    >> Graphics[Circle[]] // TeXForm
     = 
     . \begin{asy}
     . size(5.8556cm, 5.8333cm);
     . add((175,175,175,0,0,175)*(new picture(){picture s=currentpicture,t=new picture;currentpicture=t;draw(ellipse((0,0),1,1), rgb(0, 0, 0)+linewidth(0.0038095));currentpicture=s;return t;})());
     . clip(box((-0.33333,0.33333), (350.33,349.67)));
     . \end{asy}

    Invalid graphics directives yield invalid box structures:
    >> Graphics[Circle[{a, b}]]
     : GraphicsBox[CircleBox[List[a, b]], Rule[AspectRatio, Automatic], Rule[Axes, False], Rule[AxesStyle, List[]], Rule[Background, Automatic], Rule[ImageSize, Automatic], Rule[LabelStyle, List[]], Rule[PlotRange, Automatic], Rule[PlotRangePadding, Automatic], Rule[TicksStyle, List[]]] is not a valid box structure.
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
        'Background': 'Automatic',
    }

    box_suffix = 'Box'

    def apply_makeboxes(self, content, evaluation, options):
        '''MakeBoxes[%(name)s[content_, OptionsPattern[%(name)s]],
                StandardForm|TraditionalForm|OutputForm]'''

        def convert(content):
            head = content.get_head_name()

            if head == 'System`List':
                return Expression('List', *[convert(item) for item in content.leaves])
            elif head == 'System`Style':
                return Expression('StyleBox', *[convert(item) for item in content.leaves])
            elif head == 'System`GeometricTransformation' and len(content.leaves) == 2:
                return Expression('GeometricTransformationBox', convert(content.leaves[0]), content.leaves[1])

            if head in element_heads:
                if head == 'System`Text':
                    head = 'System`Inset'
                atoms = content.get_atoms(include_heads=False)
                if any(not isinstance(atom, (Integer, Real)) and
                       not atom.get_name() in GRAPHICS_SYMBOLS
                       for atom in atoms):
                    if head == 'System`Inset':
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
            if option not in ('System`ImageSize',):
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

    @staticmethod
    def create_as_style(klass, graphics, item):
        return klass(graphics, item)


class _Color(_GraphicsElement):
    formats = {
        # we are adding ImageSizeMultipliers in the rule below, because we do _not_ want color boxes to
        # diminish in size when they appear in lists or rows. we only want the display of colors this
        # way in the notebook, so we restrict the rule to StandardForm.

        (('StandardForm', ), '%(name)s[x__?(NumericQ[#] && 0 <= # <= 1&)]'):
            'Style[Graphics[{EdgeForm[Black], %(name)s[x], Rectangle[]}, ImageSize -> 16], ' +
            'ImageSizeMultipliers -> {1, 1}]'
    }

    rules = {
        '%(name)s[x_List]': 'Apply[%(name)s, x]',
    }

    components_sizes = []
    default_components = []

    def init(self, item=None, components=None):
        super(_Color, self).init(None, item)
        if item is not None:
            leaves = item.leaves
            if len(leaves) in self.components_sizes:
                # we must not clip here; we copy the components, without clipping,
                # e.g. RGBColor[-1, 0, 0] stays RGBColor[-1, 0, 0]. this is especially
                # important for color spaces like LAB that have negative components.

                components = [value.round_to_float() for value in leaves]
                if None in components:
                    raise ColorError

                # the following lines always extend to the maximum available
                # default_components, so RGBColor[0, 0, 0] will _always_
                # become RGBColor[0, 0, 0, 1]. does not seem the right thing
                # to do in this general context. poke1024

                if len(components) < 3:
                   components.extend(self.default_components[len(components):])

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

    @staticmethod
    def create_as_style(klass, graphics, item):
        return klass(item)

    def to_css(self):
        rgba = self.to_rgba()
        alpha = rgba[3] if len(rgba) > 3 else 1.
        return (r'rgb(%f%%, %f%%, %f%%)' % (
            rgba[0] * 100, rgba[1] * 100, rgba[2] * 100), alpha)

    def to_asy(self):
        rgba = self.to_rgba()
        alpha = rgba[3] if len(rgba) > 3 else 1.
        return (r'rgb(%s, %s, %s)' % (
            asy_number(rgba[0]), asy_number(rgba[1]), asy_number(rgba[2])),
                alpha)

    def to_js(self):
        return self.to_rgba()

    def to_expr(self):
        return Expression(self.get_name(), *self.components)

    def to_rgba(self):
        return self.to_color_space("RGB")

    def to_color_space(self, color_space):
        components = convert_color(self.components, self.color_space, color_space)
        if components is None:
            raise ValueError('cannot convert from color space %s to %s.' % (self.color_space, color_space))
        return components


class RGBColor(_Color):
    """
    <dl>
    <dt>'RGBColor[$r$, $g$, $b$]'
        <dd>represents a color with the specified red, green and blue
        components.
    </dl>

    >> Graphics[MapIndexed[{RGBColor @@ #1, Disk[2*#2 ~Join~ {0}]} &, IdentityMatrix[3]], ImageSize->Small]
     = -Graphics-

    >> RGBColor[0, 1, 0]
     = RGBColor[0, 1, 0]

    >> RGBColor[0, 1, 0] // ToBoxes
     = StyleBox[GraphicsBox[...], ...]
    """

    color_space = 'RGB'
    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]

    def to_rgba(self):
        return self.components


class LABColor(_Color):
    """
    <dl>
    <dt>'LABColor[$l$, $a$, $b$]'
        <dd>represents a color with the specified lightness, red/green and yellow/blue
        components in the CIE 1976 L*a*b* (CIELAB) color space.
    </dl>
    """

    color_space = 'LAB'
    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]


class LCHColor(_Color):
    """
    <dl>
    <dt>'LCHColor[$l$, $c$, $h$]'
        <dd>represents a color with the specified lightness, chroma and hue
        components in the CIELCh CIELab cube color space.
    </dl>
    """

    color_space = 'LCH'
    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]


class LUVColor(_Color):
    """
    <dl>
    <dt>'LCHColor[$l$, $u$, $v$]'
        <dd>represents a color with the specified components in the CIE 1976 L*u*v* (CIELUV) color space.
    </dl>
    """

    color_space = 'LUV'
    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]


class XYZColor(_Color):
    """
    <dl>
    <dt>'XYZColor[$x$, $y$, $z$]'
        <dd>represents a color with the specified components in the CIE 1931 XYZ color space.
    </dl>
    """

    color_space = 'XYZ'
    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]


class CMYKColor(_Color):
    """
    <dl>
    <dt>'CMYKColor[$c$, $m$, $y$, $k$]'
        <dd>represents a color with the specified cyan, magenta,
        yellow and black components.
    </dl>

    >> Graphics[MapIndexed[{CMYKColor @@ #1, Disk[2*#2 ~Join~ {0}]} &, IdentityMatrix[4]], ImageSize->Small]
     = -Graphics-
    """

    color_space = 'CMYK'
    components_sizes = [3, 4, 5]
    default_components = [0, 0, 0, 0, 1]


class Hue(_Color):
    """
    <dl>
    <dt>'Hue[$h$, $s$, $l$, $a$]'
        <dd>represents the color with hue $h$, saturation $s$,
        lightness $l$ and opacity $a$.
    <dt>'Hue[$h$, $s$, $l$]'
        <dd>is equivalent to 'Hue[$h$, $s$, $l$, 1]'.
    <dt>'Hue[$h$, $s$]'
        <dd>is equivalent to 'Hue[$h$, $s$, 1, 1]'.
    <dt>'Hue[$h$]'
        <dd>is equivalent to 'Hue[$h$, 1, 1, 1]'.
    </dl>
    >> Graphics[Table[{EdgeForm[Gray], Hue[h, s], Disk[{12h, 8s}]}, {h, 0, 1, 1/6}, {s, 0, 1, 1/4}]]
     = -Graphics-

    >> Graphics[Table[{EdgeForm[{GrayLevel[0, 0.5]}], Hue[(-11+q+10r)/72, 1, 1, 0.6], Disk[(8-r) {Cos[2Pi q/12], Sin[2Pi q/12]}, (8-r)/3]}, {r, 6}, {q, 12}]]
     = -Graphics-
    """

    color_space = 'HSB'
    components_sizes = [1, 2, 3, 4]
    default_components = [0, 1, 1, 1]

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

        result = tuple([trans(list(map(t))) for t in rgb]) + (self.components[3],)
        return result


class GrayLevel(_Color):
    """
    <dl>
    <dt>'GrayLevel[$g$]'
        <dd>represents a shade of gray specified by $g$, ranging from
        0 (black) to 1 (white).
    <dt>'GrayLevel[$g$, $a$]'
        <dd>represents a shade of gray specified by $g$ with opacity $a$.
    </dl>
    """

    color_space = 'Grayscale'
    components_sizes = [1, 2]
    default_components = [0, 1]


def expression_to_color(color):
    try:
        return _Color.create(color)
    except ColorError:
        return None


def color_to_expression(components, colorspace):
    if colorspace == 'Grayscale':
        converted_color_name = 'GrayLevel'
    elif colorspace == 'HSB':
        converted_color_name = 'Hue'
    else:
        converted_color_name = colorspace + 'Color'

    return Expression(converted_color_name, *components)


class ColorDistance(Builtin):
    """
    <dl>
    <dt>'ColorDistance[$c1$, $c2$]'
        <dd>returns a measure of color distance between the colors $c1$ and $c2$.
    <dt>'ColorDistance[$list$, $c2$]'
        <dd>returns a list of color distances between the colors in $list$ and $c2$.
    </dl>

    The option DistanceFunction specifies the method used to measure the color
    distance. Available options are:

    CIE76: euclidean distance in the LABColor space
    CIE94: euclidean distance in the LCHColor space
    CIE2000 or CIEDE2000: CIE94 distance with corrections
    CMC: Colour Measurement Committee metric (1984)
    DeltaL: difference in the L component of LCHColor
    DeltaC: difference in the C component of LCHColor
    DeltaH: difference in the H component of LCHColor

    It is also possible to specify a custom distance

    >> ColorDistance[Magenta, Green]
     = 2.2507
    >> ColorDistance[{Red, Blue}, {Green, Yellow}, DistanceFunction -> {"CMC", "Perceptibility"}]
     = {1.0495, 1.27455}
    #> ColorDistance[Blue, Red, DistanceFunction -> "CIE2000"]
     = 0.557976
    #> ColorDistance[Red, Black, DistanceFunction -> (Abs[#1[[1]] - #2[[1]]] &)]
     = 0.542917

    """

    options = {
        'DistanceFunction': 'Automatic',
    }

    messages = {
        'invdist': '`1` is not Automatic or a valid distance specification.',
        'invarg': '`1` and `2` should be two colors or a color and a lists of colors or ' +
                  'two lists of colors of the same length.'

    }

    # the docs say LABColor's colorspace corresponds to the CIE 1976 L^* a^* b^* color space
    # with {l,a,b}={L^*,a^*,b^*}/100. Corrections factors are put accordingly.

    _distances = {
        "CIE76": lambda c1, c2: _euclidean_distance(c1.to_color_space('LAB')[:3], c2.to_color_space('LAB')[:3]),
        "CIE94": lambda c1, c2: _euclidean_distance(c1.to_color_space('LCH')[:3], c2.to_color_space('LCH')[:3]),
        "CIE2000": lambda c1, c2: _cie2000_distance(100*c1.to_color_space('LAB')[:3], 100*c2.to_color_space('LAB')[:3])/100,
        "CIEDE2000": lambda c1, c2: _cie2000_distance(100*c1.to_color_space('LAB')[:3], 100*c2.to_color_space('LAB')[:3])/100,
        "DeltaL": lambda c1, c2: _component_distance(c1.to_color_space('LCH'), c2.to_color_space('LCH'), 0),
        "DeltaC": lambda c1, c2: _component_distance(c1.to_color_space('LCH'), c2.to_color_space('LCH'), 1),
        "DeltaH": lambda c1, c2: _component_distance(c1.to_color_space('LCH'), c2.to_color_space('LCH'), 2),
        "CMC": lambda c1, c2: _CMC_distance(100*c1.to_color_space('LAB')[:3], 100*c2.to_color_space('LAB')[:3], 1, 1)/100

    }


    def apply(self, c1, c2, evaluation, options):
        'ColorDistance[c1_, c2_, OptionsPattern[ColorDistance]]'
        distance_function = options.get('System`DistanceFunction')
        compute = None
        if isinstance(distance_function, String):
            compute = ColorDistance._distances.get(distance_function.get_string_value())
            if not compute:
                evaluation.message('ColorDistance', 'invdist', distance_function)
                return
        elif distance_function.has_form('List', 2):
            if distance_function.leaves[0].get_string_value() == 'CMC':
                if distance_function.leaves[1].get_string_value() == 'Acceptability':
                    compute = lambda c1, c2: _CMC_distance(100*c1.to_color_space('LAB')[:3],
                                                            100*c2.to_color_space('LAB')[:3], 2, 1)/100
                elif distance_function.leaves[1].get_string_value() == 'Perceptibility':
                    compute = ColorDistance._distances.get("CMC")

                elif distance_function.leaves[1].has_form('List', 2):
                    if (isinstance(distance_function.leaves[1].leaves[0], Integer)
                    and isinstance(distance_function.leaves[1].leaves[1], Integer)):
                        if (distance_function.leaves[1].leaves[0].get_int_value() > 0
                        and distance_function.leaves[1].leaves[1].get_int_value() > 0):
                            lightness = distance_function.leaves[1].leaves[0].get_int_value()
                            chroma = distance_function.leaves[1].leaves[1].get_int_value()
                            compute = lambda c1, c2: _CMC_distance(100*c1.to_color_space('LAB')[:3],
                                                                    100*c2.to_color_space('LAB')[:3], lightness, chroma)/100

        elif isinstance(distance_function, Symbol) and distance_function.get_name() == 'System`Automatic':
            compute = ColorDistance._distances.get("CIE76")
        else:
            def compute(a, b):
                return Expression('Apply',
                                    distance_function,
                                    Expression('List',
                                                Expression('List', *[Real(val) for val in a.to_color_space('LAB')]),
                                                Expression('List', *[Real(val) for val in b.to_color_space('LAB')])
                                              )
                                    )
        if compute == None:
            evaluation.message('ColorDistance', 'invdist', distance_function)
            return

        def distance(a, b):
            try:
                py_a = _Color.create(a)
                py_b = _Color.create(b)
            except ColorError:
                evaluation.message('ColorDistance', 'invarg', a, b)
                raise
            result = from_python(compute(py_a, py_b))
            return result

        try:
            if c1.get_head_name() == 'System`List':
                if c2.get_head_name() == 'System`List':
                    if len(c1.leaves) != len(c2.leaves):
                        evaluation.message('ColorDistance', 'invarg', c1, c2)
                        return
                    else:
                        return Expression('List', *[distance(a, b) for a, b in zip(c1.leaves, c2.leaves)])
                else:
                    return Expression('List', *[distance(c, c2) for c in c1.leaves])
            elif c2.get_head_name() == 'System`List':
                return Expression('List', *[distance(c1, c) for c in c2.leaves])
            else:
                return distance(c1, c2)
        except ColorError:
            return
        except NotImplementedError:
            evaluation.message('ColorDistance', 'invdist', distance_function)
            return


class _Size(_GraphicsElement):
    def init(self, graphics, item=None, value=None):
        super(_Size, self).init(graphics, item)
        if item is not None:
            self.value = item.leaves[0].round_to_float()
        elif value is not None:
            self.value = value
        else:
            raise BoxConstructError
        if self.value < 0:
            raise BoxConstructError


class _Thickness(_Size):
    pass


class AbsoluteThickness(_Thickness):
    """
    <dl>
    <dt>'AbsoluteThickness[$p$]'
        <dd>sets the line thickness for subsequent graphics primitives
        to $p$ points.
    </dl>

    >> Graphics[Table[{AbsoluteThickness[t], Line[{{20 t, 10}, {20 t, 80}}], Text[ToString[t]<>"pt", {20 t, 0}]}, {t, 0, 10}]]
     = -Graphics-
    """
    def get_thickness(self):
        return self.graphics.translate_absolute((self.value, 0))[0]


class Thickness(_Thickness):
    """
    <dl>
    <dt>'Thickness[$t$]'
        <dd>sets the line thickness for subsequent graphics primitives
        to $t$ times the size of the plot area.
    </dl>

    >> Graphics[{Thickness[0.2], Line[{{0, 0}, {0, 5}}]}, Axes->True, PlotRange->{{-5, 5}, {-5, 5}}]
     = -Graphics-
    """
    def get_thickness(self):
        return self.graphics.translate_relative(self.value)


class Thin(Builtin):
    """
    <dl>
    <dt>'Thin'
        <dd>sets the line width for subsequent graphics primitives to 0.5pt.
    </dl>
    """
    rules = {
        'Thin': 'AbsoluteThickness[0.5]',
    }


class Thick(Builtin):
    """
    <dl>
    <dt>'Thick'
        <dd>sets the line width for subsequent graphics primitives to 2pt.
    </dl>
    """
    rules = {
        'Thick': 'AbsoluteThickness[2]',
    }


class PointSize(_Size):
    """
    <dl>
    <dt>'PointSize[$t$]'
        <dd>sets the diameter of points to $t$, which is relative to the overall width.
    </dl>
    """
    def get_size(self):
        return self.graphics.extent_width * self.value


class FontColor(Builtin):
    """
    <dl>
    <dt>'FontColor'
        <dd>is an option for Style to set the font color.
    </dl>
    """
    pass


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
    <dt>'Disk[{$x$, $y$}, ..., {$t1$, $t2$}]'
        <dd>is a sector from angle $t1$ to $t2$.
    </dl>

    >> Graphics[{Blue, Disk[{0, 0}, {2, 1}]}]
     = -Graphics-
    The outer border can be drawn using 'EdgeForm':
    >> Graphics[{EdgeForm[Black], Red, Disk[]}]
     = -Graphics-

    Disk can also draw sectors of circles and ellipses
    >> Graphics[Disk[{0, 0}, 1, {Pi / 3, 2 Pi / 3}]]
     = -Graphics-
    >> Graphics[{Blue, Disk[{0, 0}, {1, 2}, {Pi / 3, 5 Pi / 3}]}]
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
    >> Graphics[{Circle[], Disk[{0, 0}, {1, 1}, {0, 2.1}]}]
     = -Graphics-
    """

    rules = {
        'Circle[]': 'Circle[{0, 0}]',
    }


class Inset(Builtin):
    pass


class Text(Inset):
    """
    <dl>
    <dt>'Text["$text$", {$x$, $y$}]'
        <dd>draws $text$ centered on position '{$x$, $y$}'.
    </dl>

    >> Graphics[{Text["First", {0, 0}], Text["Second", {1, 1}]}, Axes->True, PlotRange->{{-2, 2}, {-2, 2}}]
     = -Graphics-

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

        tx1, ty1 = self.p1.pos()
        tx2, ty2 = self.p2.pos()

        x1 = min(tx1, tx2) - l
        x2 = max(tx1, tx2) + l
        y1 = min(ty1, ty2) - l
        y2 = max(ty1, ty2) + l

        result.extend([(x1, y1), (x1, y2), (x2, y1), (x2, y2)])

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
                rx = r.leaves[0].round_to_float()
                ry = r.leaves[1].round_to_float()
            else:
                rx = ry = r.round_to_float()
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
        ry = abs(y - ry)
        l = self.style.get_line_width(face_element=self.face_element)
        style = create_css(self.edge_color, self.face_color, stroke_width=l)
        return '<ellipse cx="%f" cy="%f" rx="%f" ry="%f" style="%s" />' % (
            x, y, rx, ry, style)

    def to_asy(self):
        x, y = self.c.pos()
        rx, ry = self.r.pos()
        rx -= x
        ry = abs(ry - y)
        l = self.style.get_line_width(face_element=self.face_element)
        pen = create_pens(edge_color=self.edge_color,
                          face_color=self.face_color, stroke_width=l,
                          is_face_element=self.face_element)
        cmd = 'filldraw' if self.face_element else 'draw'
        return '%s(ellipse((%s,%s),%s,%s), %s);' % (
            cmd, asy_number(x), asy_number(y), asy_number(rx), asy_number(ry),
            pen)


class _ArcBox(_RoundBox):
    def init(self, graphics, style, item):
        if len(item.leaves) == 3:
            arc_expr = item.leaves[2]
            if arc_expr.get_head_name() != 'System`List':
                raise BoxConstructError
            arc = arc_expr.leaves
            pi2 = 2 * pi

            start_angle = arc[0].round_to_float()
            end_angle = arc[1].round_to_float()

            if start_angle is None or end_angle is None:
                raise BoxConstructError
            elif end_angle >= start_angle + pi2:  # full circle?
                self.arc = None
            else:
                if end_angle <= start_angle:
                    self.arc = (end_angle, start_angle)
                else:
                    self.arc = (start_angle, end_angle)

            item = Expression(item.get_head_name(), *item.leaves[:2])
        else:
            self.arc = None
        super(_ArcBox, self).init(graphics, style, item)

    def _arc_params(self):
        x, y = self.c.pos()
        rx, ry = self.r.pos()

        rx -= x
        ry -= y

        start_angle, end_angle = self.arc

        if end_angle - start_angle <= pi:
            large_arc = 0
        else:
            large_arc = 1

        sx = x + rx * cos(start_angle)
        sy = y + ry * sin(start_angle)

        ex = x + rx * cos(end_angle)
        ey = y + ry * sin(end_angle)

        return x, y, abs(rx), abs(ry), sx, sy, ex, ey, large_arc

    def to_svg(self):
        if self.arc is None:
            return super(_ArcBox, self).to_svg()

        x, y, rx, ry, sx, sy, ex, ey, large_arc = self._arc_params()

        def path(closed):
            if closed:
                yield 'M %f,%f' % (x, y)
                yield 'L %f,%f' % (sx, sy)
            else:
                yield 'M %f,%f' % (sx, sy)

            yield 'A %f,%f,0,%d,0,%f,%f' % (rx, ry, large_arc, ex, ey)

            if closed:
                yield 'Z'

        l = self.style.get_line_width(face_element=self.face_element)
        style = create_css(self.edge_color, self.face_color, stroke_width=l)
        return '<path d="%s" style="%s" />' % (' '.join(path(self.face_element)), style)

    def to_asy(self):
        if self.arc is None:
            return super(_ArcBox, self).to_asy()

        x, y, rx, ry, sx, sy, ex, ey, large_arc = self._arc_params()

        def path(closed):
            if closed:
                yield '(%s,%s)--(%s,%s)--' % tuple(
                    asy_number(t) for t in (x, y, sx, sy))

            yield 'arc((%s,%s), (%s, %s), (%s, %s))' % tuple(
                asy_number(t) for t in (x, y, sx, sy, ex, ey))

            if closed:
                yield '--cycle'

        l = self.style.get_line_width(face_element=self.face_element)
        pen = create_pens(edge_color=self.edge_color,
                          face_color=self.face_color, stroke_width=l,
                          is_face_element=self.face_element)
        command = 'filldraw' if self.face_element else 'draw'
        return '%s(%s, %s);' % (command, ''.join(path(self.face_element)), pen)


class DiskBox(_ArcBox):
    face_element = True


class CircleBox(_ArcBox):
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
    <dt>'Point[{$point_1$, $point_2$ ...}]'
        <dd>represents the point primitive.
    <dt>'Point[{{$p_11$, $p_12$, ...}, {$p_21$, $p_22$, ...}, ...}]'
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
        point_size, _ = self.style.get_style(PointSize, face_element=False)
        if point_size is None:
            point_size = PointSize(self.graphics, value=0.005)
        size = point_size.get_size()

        style = create_css(edge_color=self.edge_color,
                           stroke_width=0, face_color=self.face_color)
        svg = ''
        for line in self.lines:
            for coords in line:
                svg += '<circle cx="%f" cy="%f" r="%f" style="%s" />' % (
                    coords.pos()[0], coords.pos()[1], size, style)
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
            path = '--'.join(['(%.5g,%5g)' % coords.pos() for coords in line])
            asy += 'draw(%s, %s);' % (path, pen)
        return asy


def _svg_bezier(*segments):
    # see https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands
    # see https://docs.webplatform.org/wiki/svg/tutorials/smarter_svg_shapes

    while segments and not segments[0][1]:
        segments = segments[1:]

    if not segments:
        return

    forms = 'LQC'  # SVG commands for line, quadratic bezier, cubic bezier

    def path(max_degree, p):
        max_degree = min(max_degree, len(forms))
        while p:
            n = min(max_degree, len(p))  # 1, 2, or 3
            if n < 1:
                raise BoxConstructError
            yield forms[n - 1] + ' '.join('%f,%f' % xy for xy in p[:n])
            p = p[n:]

    k, p = segments[0]
    yield 'M%f,%f' % p[0]

    for s in path(k, p[1:]):
        yield s

    for k, p in segments[1:]:
        for s in path(k, p):
            yield s


def _asy_bezier(*segments):
    # see http://asymptote.sourceforge.net/doc/Bezier-curves.html#Bezier-curves

    while segments and not segments[0][1]:
        segments = segments[1:]

    if not segments:
        return

    def cubic(p0, p1, p2, p3):
        return '..controls(%.5g,%.5g) and (%.5g,%.5g)..(%.5g,%.5g)' % tuple(list(chain(p1, p2, p3)))

    def quadratric(qp0, qp1, qp2):
        # asymptote only supports cubic beziers, so we convert this quadratic
        # bezier to a cubic bezier, see http://fontforge.github.io/bezier.html

        # CP0 = QP0
        # CP3 = QP2
        # CP1 = QP0 + 2 / 3 * (QP1 - QP0)
        # CP2 = QP2 + 2 / 3 * (QP1 - QP2)

        qp0x, qp0y = qp0
        qp1x, qp1y = qp1
        qp2x, qp2y = qp2

        t = 2. / 3.
        cp0 = qp0
        cp1 = (qp0x + t * (qp1x - qp0x), qp0y + t * (qp1y - qp0y))
        cp2 = (qp2x + t * (qp1x - qp2x), qp2y + t * (qp1y - qp2y))
        cp3 = qp2

        return cubic(cp0, cp1, cp2, cp3)

    def linear(p0, p1):
        return '--(%.5g,%.5g)' % p1

    forms = (linear, quadratric, cubic)

    def path(max_degree, p):
        max_degree = min(max_degree, len(forms))
        while p:
            n = min(max_degree, len(p) - 1)  # 1, 2, or 3
            if n < 1:
                break
            yield forms[n - 1](*p[:n + 1])
            p = p[n:]

    k, p = segments[0]
    yield '(%.5g,%.5g)' % p[0]

    connect = []
    for k, p in segments:
        for s in path(k, list(chain(connect, p))):
            yield s
        connect = p[-1:]


class BernsteinBasis(Builtin):
    rules = {
        'BernsteinBasis[d_, n_, x_]': 'Piecewise[{{Binomial[d, n] * x ^ n * (1 - x) ^ (d - n), 0 < x < 1}}, 0]',
    }


class BezierFunction(Builtin):
    rules = {
        'BezierFunction[p_]': 'Function[x, Total[p * BernsteinBasis[Length[p] - 1, Range[0, Length[p] - 1], x]]]',
    }

class BezierCurve(Builtin):
    """
    <dl>
    <dt>'BezierCurve[{$p1$, $p2$ ...}]'
        <dd>represents a bezier curve with $p1$, $p2$ as control points.
    </dl>

    >> Graphics[BezierCurve[{{0, 0},{1, 1},{2, -1},{3, 0}}]]
     = -Graphics-

    >> Module[{p={{0, 0},{1, 1},{2, -1},{4, 0}}}, Graphics[{BezierCurve[p], Red, Point[Table[BezierFunction[p][x], {x, 0, 1, 0.1}]]}]]
     = -Graphics-
    """

    options = {
        'SplineDegree': '3',
    }


class BezierCurveBox(_Polyline):
    def init(self, graphics, style, item, options):
        super(BezierCurveBox, self).init(graphics, item, style)
        if len(item.leaves) != 1 or item.leaves[0].get_head_name() != 'System`List':
            raise BoxConstructError
        self.edge_color, _ = style.get_style(_Color, face_element=False)
        points = item.leaves[0]
        self.do_init(graphics, points)
        spline_degree = options.get('System`SplineDegree')
        if not isinstance(spline_degree, Integer):
            raise BoxConstructError
        self.spline_degree = spline_degree.get_int_value()

    def to_svg(self):
        l = self.style.get_line_width(face_element=False)
        style = create_css(edge_color=self.edge_color, stroke_width=l)

        svg = ''
        for line in self.lines:
            s = ' '.join(_svg_bezier((self.spline_degree, [xy.pos() for xy in line])))
            svg += '<path d="%s" style="%s"/>' % (s, style)
        return svg

    def to_asy(self):
        l = self.style.get_line_width(face_element=False)
        pen = create_pens(edge_color=self.edge_color, stroke_width=l)

        asy = ''
        for line in self.lines:
            for path in _asy_bezier((self.spline_degree, [xy.pos() for xy in line])):
                asy += 'draw(%s, %s);' % (path, pen)
        return asy


class FilledCurve(Builtin):
    """
    <dl>
    <dt>'FilledCurve[{$segment1$, $segment2$ ...}]'
        <dd>represents a filled curve.
    </dl>

    >> Graphics[FilledCurve[{Line[{{0, 0}, {1, 1}, {2, 0}}]}]]
    = -Graphics-

    >> Graphics[FilledCurve[{BezierCurve[{{0, 0}, {1, 1}, {2, 0}}], Line[{{3, 0}, {0, 2}}]}]]
    = -Graphics-
    """
    pass


class FilledCurveBox(_GraphicsElement):
    def init(self, graphics, style, item=None):
        super(FilledCurveBox, self).init(graphics, item, style)
        self.edge_color, self.face_color = style.get_style(_Color, face_element=True)

        if item is not None and item.leaves and item.leaves[0].has_form('List', None):
            if len(item.leaves) != 1:
                raise BoxConstructError
            leaves = item.leaves[0].leaves

            def parse_component(segments):
                for segment in segments:
                    head = segment.get_head_name()

                    if head == 'System`Line':
                        k = 1
                        parts = segment.leaves
                    elif head == 'System`BezierCurve':
                        parts, options = _data_and_options(segment.leaves, {})
                        spline_degree = options.get('SplineDegree', Integer(3))
                        if not isinstance(spline_degree, Integer):
                            raise BoxConstructError
                        k = spline_degree.get_int_value()
                    elif head == 'System`BSplineCurve':
                        raise NotImplementedError  # FIXME convert bspline to bezier here
                        parts = segment.leaves
                    else:
                        raise BoxConstructError

                    coords = []

                    for part in parts:
                        if part.get_head_name() != 'System`List':
                            raise BoxConstructError
                        coords.extend([graphics.coords(graphics, xy) for xy in part.leaves])

                    yield k, coords

            if all(x.get_head_name() == 'System`List' for x in leaves):
                self.components = [list(parse_component(x)) for x in leaves]
            else:
                self.components = [list(parse_component(leaves))]
        else:
            raise BoxConstructError

    def to_svg(self):
        l = self.style.get_line_width(face_element=False)
        style = create_css(edge_color=self.edge_color, face_color=self.face_color, stroke_width=l)

        def components():
            for component in self.components:
                transformed = [(k, [xy.pos() for xy in p]) for k, p in component]
                yield ' '.join(_svg_bezier(*transformed)) + ' Z'

        return '<path d="%s" style="%s" fill-rule="evenodd"/>' % (' '.join(components()), style)

    def to_asy(self):
        l = self.style.get_line_width(face_element=False)
        pen = create_pens(edge_color=self.edge_color, stroke_width=l)

        if not pen:
            pen = 'currentpen'

        def components():
            for component in self.components:
                transformed = [(k, [xy.pos() for xy in p]) for k, p in component]
                yield 'fill(%s--cycle, %s);' % (''.join(_asy_bezier(*transformed)), pen)

        return ''.join(components())

    def extent(self):
        l = self.style.get_line_width(face_element=False)
        result = []
        for component in self.components:
            for _, points in component:
                for p in points:
                    x, y = p.pos()
                    result.extend([(x - l, y - l), (x - l, y + l), (x + l, y - l), (x + l, y + l)])
        return result


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
        if name == 'System`VertexColors':
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
            svg += '<meshgradient data="%s" />' % json.dumps(mesh)
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
                    '(%.5g,%.5g)' % coords.pos() for coords in line]) + '--cycle')

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
                    ['(%.5g,%.5g)' % coords.pos() for coords in line]) + '--cycle'
                asy += 'filldraw(%s, %s);' % (path, pens)
        return asy


class RegularPolygon(Builtin):
    """
    <dl>
    <dt>'RegularPolygon[$n$]'
        <dd>gives the regular polygon with $n$ edges.
    <dt>'RegularPolygon[$r$, $n$]'
        <dd>gives the regular polygon with $n$ edges and radius $r$.
    <dt>'RegularPolygon[{$r$, $phi$}, $n$]'
        <dd>gives the regular polygon with radius $r$ with one vertex drawn at angle $phi$.
    <dt>'RegularPolygon[{$x, $y}, $r$, $n$]'
        <dd>gives the regular polygon centered at the position {$x, $y}.
    </dl>

    >> Graphics[RegularPolygon[5]]
    = -Graphics-

    >> Graphics[{Yellow, Rectangle[], Orange, RegularPolygon[{1, 1}, {0.25, 0}, 3]}]
    = -Graphics-
    """


class RegularPolygonBox(PolygonBox):
    def init(self, graphics, style, item):
        if len(item.leaves) in (1, 2, 3) and isinstance(item.leaves[-1], Integer):
            r = 1.
            phi0 = None

            if len(item.leaves) >= 2:
                rspec = item.leaves[-2]
                if rspec.get_head_name() == 'System`List':
                    if len(rspec.leaves) != 2:
                        raise BoxConstructError
                    r = rspec.leaves[0].round_to_float()
                    phi0 = rspec.leaves[1].round_to_float()
                else:
                    r = rspec.round_to_float()

            x = 0.
            y = 0.
            if len(item.leaves) == 3:
                pos = item.leaves[0]
                if not pos.has_form('List', 2):
                    raise BoxConstructError
                x = pos.leaves[0].round_to_float()
                y = pos.leaves[1].round_to_float()

            n = item.leaves[-1].get_int_value()

            if any(t is None for t in (x, y, r)) or n < 0:
                raise BoxConstructError

            if phi0 is None:
                phi0 = -pi / 2.
                if n % 1 == 0 and n > 0:
                    phi0 += pi / n

            pi2 = pi * 2.

            def vertices():
                for i in range(n):
                    phi = phi0 + pi2 * i / float(n)
                    yield Expression('List', Real(x + r * cos(phi)), Real(y + r * sin(phi)))

            new_item = Expression('RegularPolygonBox', Expression('List', *list(vertices())))
        else:
            raise BoxConstructError

        super(RegularPolygonBox, self).init(graphics, style, new_item)


class Arrow(Builtin):
    """
    <dl>
    <dt>'Arrow[{$p1$, $p2$}]'
        <dd>represents a line from $p1$ to $p2$ that ends with an arrow at $p2$.
    <dt>'Arrow[{$p1$, $p2$}, $s$]'
        <dd>represents a line with arrow that keeps a distance of $s$ from $p1$
        and $p2$.
    <dt>'Arrow[{$point_1$, $point_2$}, {$s1$, $s2$}]'
        <dd>represents a line with arrow that keeps a distance of $s1$ from $p1$
        and a distance of $s2$ from $p2$.
    </dl>

    >> Graphics[Arrow[{{0,0}, {1,1}}]]
    = -Graphics-

    >> Graphics[{Circle[], Arrow[{{2, 1}, {0, 0}}, 1]}]
    = -Graphics-

    Keeping distances may happen across multiple segments:

    >> Table[Graphics[{Circle[], Arrow[Table[{Cos[phi],Sin[phi]},{phi,0,2*Pi,Pi/2}],{d, d}]}],{d,0,2,0.5}]
     = {-Graphics-, -Graphics-, -Graphics-, -Graphics-, -Graphics-}
    """

    pass


class Arrowheads(_GraphicsElement):
    """
    <dl>
    <dt>'Arrowheads[$s$]'
        <dd>specifies that Arrow[] draws one arrow of size $s$ (relative to width of image, defaults to 0.04).
    <dt>'Arrowheads[{$spec1$, $spec2$, ..., $specn$}]'
        <dd>specifies that Arrow[] draws n arrows as defined by $spec1$, $spec2$, ... $specn$.
    <dt>'Arrowheads[{{$s$}}]'
        <dd>specifies that one arrow of size $s$ should be drawn.
    <dt>'Arrowheads[{{$s$, $pos$}}]'
        <dd>specifies that one arrow of size $s$ should be drawn at position $pos$ (for the arrow to
        be on the line, $pos$ has to be between 0, i.e. the start for the line, and 1, i.e. the end
        of the line).
    <dt>'Arrowheads[{{$s$, $pos$, $g$}}]'
        <dd>specifies that one arrow of size $s$ should be drawn at position $pos$ using Graphics $g$.
    </dl>

    Arrows on both ends can be achieved using negative sizes:

    >> Graphics[{Circle[],Arrowheads[{-0.04, 0.04}], Arrow[{{0, 0}, {2, 2}}, {1,1}]}]
     = -Graphics-

    You may also specify our own arrow shapes:

    >> Graphics[{Circle[], Arrowheads[{{0.04, 1, Graphics[{Red, Disk[]}]}}], Arrow[{{0, 0}, {Cos[Pi/3],Sin[Pi/3]}}]}]
     = -Graphics-

    >> Graphics[{Arrowheads[Table[{0.04, i/10, Graphics[Disk[]]},{i,1,10}]], Arrow[{{0, 0}, {6, 5}, {1, -3}, {-2, 2}}]}]
     = -Graphics-
    """

    default_size = 0.04

    symbolic_sizes = {
        'System`Tiny': 3,
        'System`Small': 5,
        'System`Medium': 9,
        'System`Large': 18,
    }

    def init(self, graphics, item=None):
        super(Arrowheads, self).init(graphics, item)
        if len(item.leaves) != 1:
            raise BoxConstructError
        self.spec = item.leaves[0]

    def _arrow_size(self, s, extent):
        if isinstance(s, Symbol):
            size = self.symbolic_sizes.get(s.get_name(), 0)
            return self.graphics.translate_absolute((size, 0))[0]
        else:
            return _to_float(s) * extent

    def heads(self, extent, default_arrow, custom_arrow):
        # see https://reference.wolfram.com/language/ref/Arrowheads.html

        if self.spec.get_head_name() == 'System`List':
            leaves = self.spec.leaves
            if all(x.get_head_name() == 'System`List' for x in leaves):
                for head in leaves:
                    spec = head.leaves
                    if len(spec) not in (2, 3):
                        raise BoxConstructError
                    size_spec = spec[0]
                    if isinstance(size_spec, Symbol) and size_spec.get_name() == 'System`Automatic':
                        s = self.default_size * extent
                    elif size_spec.is_numeric():
                        s = self._arrow_size(size_spec, extent)
                    else:
                        raise BoxConstructError

                    if len(spec) == 3 and custom_arrow:
                        graphics = spec[2]
                        if graphics.get_head_name() != 'System`Graphics':
                            raise BoxConstructError
                        arrow = custom_arrow(graphics)
                    else:
                        arrow = default_arrow

                    if not isinstance(spec[1], (Real, Rational, Integer)):
                        raise BoxConstructError

                    yield s, _to_float(spec[1]), arrow
            else:
                n = max(1., len(leaves) - 1.)
                for i, head in enumerate(leaves):
                    yield self._arrow_size(head, extent), i / n, default_arrow
        else:
            yield self._arrow_size(self.spec, extent), 1, default_arrow


class ArrowBox(_Polyline):
    def init(self, graphics, style, item=None):
        super(ArrowBox, self).init(graphics, item, style)

        if not item:
            raise BoxConstructError

        leaves = item.leaves
        if len(leaves) == 2:
            setback = self._setback_spec(leaves[1])
        elif len(leaves) == 1:
            setback = (0, 0)
        else:
            raise BoxConstructError

        self.setback = setback
        self.do_init(graphics, leaves[0])
        self.graphics = graphics
        self.edge_color, _ = style.get_style(_Color, face_element=False)
        self.heads, _ = style.get_style(Arrowheads, face_element=False)

    @staticmethod
    def _setback_spec(expr):
        if expr.get_head_name() == 'System`List':
            leaves = expr.leaves
            if len(leaves) != 2:
                raise BoxConstructError
            return tuple(max(_to_float(l), 0.) for l in leaves)
        else:
            s = max(_to_float(expr), 0.)
            return s, s

    @staticmethod
    def _default_arrow(polygon):
        # the default arrow drawn by draw() below looks looks like this:
        #
        #       H
        #      .:.
        #     . : .
        #    .  :  .
        #   .  .B.  .
        #  . .  :  . .
        # S.    E    .S
        #       :
        #       :
        #       :
        #
        # the head H is where the arrow's point is. at base B, the arrow spreads out at right angles from the line
        # it attaches to. the arrow size 's' given in the Arrowheads specification always specifies the length H-B.
        #
        # the spread out points S are defined via two constants: arrow_edge (which defines the factor to get from
        # H-B to H-E) and arrow_spread (which defines the factor to get from H-B to E-S).

        arrow_spread = 0.3
        arrow_edge = 1.1

        def draw(px, py, vx, vy, t1, s):
            hx = px + t1 * vx  # compute H
            hy = py + t1 * vy

            t0 = t1 - s
            bx = px + t0 * vx  # compute B
            by = py + t0 * vy

            te = t1 - arrow_edge * s
            ex = px + te * vx  # compute E
            ey = py + te * vy

            ts = arrow_spread * s
            sx = -vy * ts
            sy = vx * ts

            head_points = ((hx, hy),
                           (ex + sx, ey + sy),
                           (bx, by),
                           (ex - sx, ey - sy))

            for shape in polygon(head_points):
                yield shape

        return draw

    def _draw(self, polyline, default_arrow, custom_arrow, extent):
        if self.heads:
            heads = list(self.heads.heads(extent, default_arrow, custom_arrow))
            heads = sorted(heads, key=lambda spec: spec[1])  # sort by pos
        else:
            heads = ((extent * Arrowheads.default_size, 1, default_arrow),)

        def norm(p, q):
            px, py = p
            qx, qy = q

            dx = qx - px
            dy = qy - py

            length = sqrt(dx * dx + dy * dy)
            return dx, dy, length

        def segments(points):
            for i in range(len(points) - 1):
                px, py = points[i]
                dx, dy, dl = norm((px, py), points[i + 1])
                yield dl, px, py, dx, dy

        def setback(p, q, d):
            dx, dy, length = norm(p, q)
            if d >= length:
                return None, length
            else:
                s = d / length
                return (s * dx, s * dy), d

        def shrink_one_end(line, s):
            while s > 0.:
                if len(line) < 2:
                    return []
                xy, length = setback(line[0].p, line[1].p, s)
                if xy is not None:
                    line[0] = line[0].add(*xy)
                else:
                    line = line[1:]
                s -= length
            return line

        def shrink(line, s1, s2):
            return list(reversed(shrink_one_end(
                list(reversed(shrink_one_end(line[:], s1))), s2)))

        def render(points, heads):  # heads has to be sorted by pos
            seg = list(segments(points))

            if not seg:
                return

            i = 0
            t0 = 0.
            n = len(seg)
            dl, px, py, dx, dy = seg[i]
            total = sum(segment[0] for segment in seg)

            for s, t, draw in ((s, pos * total - t0, draw) for s, pos, draw in heads):
                if s == 0.:  # ignore zero-sized arrows
                    continue

                if i < n:  # not yet past last segment?
                    while t > dl:  # position past current segment?
                        t -= dl
                        t0 += dl
                        i += 1
                        if i == n:
                            px += dx  # move to last segment's end
                            py += dy
                            break
                        else:
                            dl, px, py, dx, dy = seg[i]

                for shape in draw(px, py, dx / dl, dy / dl, t, s):
                    yield shape

        for line in self.lines:
            if len(line) < 2:
                continue

            # note that shrinking needs to happen in the Graphics[] coordinate space, whereas the
            # subsequent position calculation needs to happen in pixel space.

            transformed_points = [xy.pos() for xy in shrink(line, *self.setback)]

            for s in polyline(transformed_points):
                yield s

            for s in render(transformed_points, heads):
                yield s

    def _custom_arrow(self, format, transform):
        def make(graphics):
            code = _extract_graphics(
                graphics, format, self.graphics.evaluation)

            half_pi = pi / 2.

            def draw(px, py, vx, vy, t1, s):
                t0 = t1

                tx = px + t0 * vx
                ty = py + t0 * vy

                r = half_pi + atan2(vy, vx)

                s = -s

                cos_r = cos(r)
                sin_r = sin(r)

                # see TranslationTransform[{tx,ty}].ScalingTransform[{s,s}].RotationTransform[r]
                yield transform([[s * cos_r, -s * sin_r, tx], [s * sin_r, s * cos_r, ty], [0, 0, 1]], code)

            return draw

        return make

    def to_svg(self):
        width = self.style.get_line_width(face_element=False)
        style = create_css(edge_color=self.edge_color, stroke_width=width)
        arrow_style = create_css(face_color=self.edge_color, stroke_width=width)

        def polyline(points):
            yield '<polyline points="'
            yield ' '.join('%f,%f' % xy for xy in points)
            yield '" style="%s" />' % style

        def polygon(points):
            yield '<polygon points="'
            yield ' '.join('%f,%f' % xy for xy in points)
            yield '" style="%s" />' % arrow_style

        def svg_transform(m, code):
            return _Transform(m).to_svg(code)

        extent = self.graphics.extent_width or 0
        default_arrow = self._default_arrow(polygon)
        custom_arrow = self._custom_arrow('svg', svg_transform)
        return ''.join(self._draw(polyline, default_arrow, custom_arrow, extent))

    def to_asy(self):
        width = self.style.get_line_width(face_element=False)
        pen = create_pens(edge_color=self.edge_color, stroke_width=width)
        arrow_pen = create_pens(face_color=self.edge_color, stroke_width=width)

        def polyline(points):
            yield 'draw('
            yield '--'.join(['(%.5g,%5g)' % xy for xy in points])
            yield ', % s);' % pen

        def polygon(points):
            yield 'filldraw('
            yield '--'.join(['(%.5g,%5g)' % xy for xy in points])
            yield '--cycle, % s);' % arrow_pen

        def asy_transform(m, code):
            return _Transform(m).to_asy(code)

        extent = self.graphics.extent_width or 0
        default_arrow = self._default_arrow(polygon)
        custom_arrow = self._custom_arrow('asy', asy_transform)
        return ''.join(self._draw(polyline, default_arrow, custom_arrow, extent))

    def extent(self):
        width = self.style.get_line_width(face_element=False)

        def polyline(points):
            for p in points:
                x, y = p
                yield x - width, y - width
                yield x - width, y + width
                yield x + width, y - width
                yield x + width, y + width

        def polygon(points):
            for p in points:
                yield p

        def default_arrow(px, py, vx, vy, t1, s):
            yield px, py

        return list(self._draw(polyline, default_arrow, None, 0))


class TransformationFunction(Builtin):
    """
    >> RotationTransform[Pi].TranslationTransform[{1, -1}]
     = TransformationFunction[{{-1, 0, -1}, {0, -1, 1}, {0, 0, 1}}]

    >> TranslationTransform[{1, -1}].RotationTransform[Pi]
     = TransformationFunction[{{-1, 0, 1}, {0, -1, -1}, {0, 0, 1}}]
    """

    rules = {
        'Dot[TransformationFunction[a_], TransformationFunction[b_]]': 'TransformationFunction[a . b]',
        'TransformationFunction[m_][v_]': 'Take[m . Join[v, {0}], Length[v]]',
    }


class TranslationTransform(Builtin):
    """
    <dl>
    <dt>'TranslationTransform[v]'
        <dd>gives the translation by the vector $v$.
    </dl>

    >> TranslationTransform[{1, 2}]
     = TransformationFunction[{{1, 0, 1}, {0, 1, 2}, {0, 0, 1}}]
    """

    rules = {
        'TranslationTransform[v_]':
            'TransformationFunction[IdentityMatrix[Length[v] + 1] + '
            '(Join[ConstantArray[0, Length[v]], {#}]& /@ Join[v, {0}])]',
    }


class RotationTransform(Builtin):
    rules = {
        'RotationTransform[phi_]':
            'TransformationFunction[{{Cos[phi], -Sin[phi], 0}, {Sin[phi], Cos[phi], 0}, {0, 0, 1}}]',
        'RotationTransform[phi_, p_]':
            'TranslationTransform[-p] . RotationTransform[phi] . TranslationTransform[p]',
    }


class ScalingTransform(Builtin):
    rules = {
        'ScalingTransform[v_]':
            'TransformationFunction[DiagonalMatrix[Join[v, {1}]]]',
        'ScalingTransform[v_, p_]':
            'TranslationTransform[-p] . ScalingTransform[v] . TranslationTransform[p]',
    }


class Translate(Builtin):
    """
    <dl>
    <dt>'Translate[g, {x, y}]'
        <dd>translates an object by the specified amount.
    <dt>'Translate[g, {{x1, y1}, {x2, y2}, ...}]'
        <dd>creates multiple instances of object translated by the specified amounts.
    </dl>

    >> Graphics[{Circle[], Translate[Circle[], {1, 0}]}]
     = -Graphics-
    """

    rules = {
        'Translate[g_, v_?(Depth[#] > 2&)]': 'GeometricTransformation[g, TranslationTransform /@ v]',
        'Translate[g_, v_?(Depth[#] == 2&)]': 'GeometricTransformation[g, TranslationTransform[v]]',
    }


class Rotate(Builtin):
    """
    <dl>
    <dt>'Rotate[g, phi]'
        <dd>rotates an object by the specified amount.
    </dl>

    >> Graphics[Rotate[Rectangle[], Pi / 3]]
     = -Graphics-

    >> Graphics[{Rotate[Rectangle[{0, 0}, {0.2, 0.2}], 1.2, {0.1, 0.1}], Red, Disk[{0.1, 0.1}, 0.05]}]
     = -Graphics-

    >> Graphics[Table[Rotate[Scale[{RGBColor[i,1-i,1],Rectangle[],Black,Text["ABC",{0.5,0.5}]},1-i],Pi*i], {i,0,1,0.2}]]
     = -Graphics-
    """

    rules = {
        'Rotate[g_, phi_]': 'GeometricTransformation[g, RotationTransform[phi]]',
        'Rotate[g_, phi_, p_]': 'GeometricTransformation[g, RotationTransform[phi, p]]',
    }


class Scale(Builtin):
    """
    <dl>
    <dt>'Scale[g, phi]'
        <dd>scales an object by the specified amount.
    </dl>

    >> Graphics[Rotate[Rectangle[], Pi / 3]]
     = -Graphics-

    >> Graphics[{Scale[Rectangle[{0, 0}, {0.2, 0.2}], 3, {0.1, 0.1}], Red, Disk[{0.1, 0.1}, 0.05]}]
     = -Graphics-
    """

    rules = {
        'Scale[g_, s_?ListQ]': 'GeometricTransformation[g, ScalingTransform[s]]',
        'Scale[g_, s_]': 'GeometricTransformation[g, ScalingTransform[{s, s}]]',
        'Scale[g_, s_?ListQ, p_]': 'GeometricTransformation[g, ScalingTransform[s, p]]',
        'Scale[g_, s_, p_]': 'GeometricTransformation[g, ScalingTransform[{s, s}, p]]',
    }


class GeometricTransformation(Builtin):
    """
    <dl>
    <dt>'GeometricTransformation[$g$, $tfm$]'
        <dd>transforms an object $g$ with the transformation $tfm$.
    </dl>
    """
    pass


class GeometricTransformationBox(_GraphicsElement):
    def init(self, graphics, style, contents, transform):
        super(GeometricTransformationBox, self).init(graphics, None, style)
        self.contents = contents
        if transform.get_head_name() == 'System`List':
            functions = transform.leaves
        else:
            functions = [transform]
        evaluation = graphics.evaluation
        self.transforms = [_Transform(Expression('N', f).evaluate(evaluation)) for f in functions]

    def patch_transforms(self, transforms):
        self.transforms = transforms

    def extent(self):
        def points():
            for content in self.contents:
                for transform in self.transforms:
                    p = content.extent()
                    for q in transform.transform(p):
                        yield q
        return list(points())

    def to_svg(self):
        def instances():
            for content in self.contents:
                content_svg = content.to_svg()
                for transform in self.transforms:
                    yield transform.to_svg(content_svg)
        return ''.join(instances())

    def to_asy(self):
        def instances():
            # graphics = self.graphics
            for content in self.contents:
                content_asy = content.to_asy()
                for transform in self.transforms:
                    yield transform.to_asy(content_asy)
        return ''.join(instances())


class InsetBox(_GraphicsElement):
    def init(self, graphics, style, item=None, content=None, pos=None, opos=(0, 0)):
        super(InsetBox, self).init(graphics, item, style)

        self.color = self.style.get_option('System`FontColor')
        if self.color is None:
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
        s = 0.01  # .1 / (self.graphics.pixel_width or 1.)
        h = s * 25
        w = len(self.content_text) * \
            s * 7  # rough approximation by numbers of characters
        opos = self.opos
        x = p[0] - w / 2.0 - opos[0] * w / 2.0
        y = p[1] - h / 2.0 + opos[1] * h / 2.0
        return [(x, y), (x + w, y + h)]

    def to_svg(self):
        x, y = self.pos.pos()
        absolute = self.pos.is_absolute()

        content = self.content.boxes_to_xml(
            evaluation=self.graphics.evaluation)
        style = create_css(font_color=self.color)

        if not absolute:
            x, y = list(self.graphics.local_to_screen.transform([(x, y)]))[0]

        svg = (
            '<foreignObject x="%f" y="%f" ox="%f" oy="%f" style="%s">'
            '<math>%s</math></foreignObject>') % (
                x, y, self.opos[0], self.opos[1], style, content)

        if not absolute:
            svg = self.graphics.inverse_local_to_screen.to_svg(svg)

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
    """
    >> Graphics[{EdgeForm[{Thick, Green}], Disk[]}]
     = -Graphics-

    >> Graphics[{Style[Disk[],EdgeForm[{Thick,Red}]], Circle[{1,1}]}]
     = -Graphics-
    """
    pass


class FaceForm(Builtin):
    pass


def _style(graphics, item):
    head = item.get_head_name()
    if head in style_heads:
        klass = get_class(head)
        style = klass.create_as_style(klass, graphics, item)
    elif head in ('System`EdgeForm', 'System`FaceForm'):
        style = graphics.get_style_class()(graphics, edge=head == 'System`EdgeForm',
                           face=head == 'System`FaceForm')
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
    return style


class Style(object):
    def __init__(self, graphics, edge=False, face=False):
        self.styles = []
        self.options = {}
        self.graphics = graphics
        self.edge = edge
        self.face = face
        self.klass = graphics.get_style_class()

    def append(self, item, allow_forms=True):
        self.styles.append(_style(self.graphics, item))

    def set_option(self, name, value):
        self.options[name] = value

    def extend(self, style, pre=True):
        if pre:
            self.styles = style.styles + self.styles
        else:
            self.styles.extend(style.styles)

    def clone(self):
        result = self.klass(self.graphics, edge=self.edge, face=self.face)
        result.styles = self.styles[:]
        result.options = self.options.copy()
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

    def get_option(self, name):
        return self.options.get(name, None)

    def get_line_width(self, face_element=True):
        if self.graphics.local_to_screen is None:
            return 0
        edge_style, _ = self.get_style(
            _Thickness, default_to_faces=face_element,
            consider_forms=face_element)
        if edge_style is None:
            return 0
        return edge_style.get_thickness()

    def to_axis_style(self):
        return AxisStyle(self)


class AxisStyle(object):
    # used exclusively for graphics generated inside GraphicsBox.create_axes().
    # wraps a Style instance for graphics and does not operate on local but on
    # screen space, i.e. has to apply "local_to_screen" to all widths, sizes, ...

    def __init__(self, style):
        self.base = Style(style.graphics)
        self.base.extend(style)
        self.sx = style.graphics.local_to_screen.matrix[0][0]

    def extend(self, style, pre=True):
        self.base.extend(style.base, pre)

    def clone(self):
        return AxisStyle(self.base.clone())

    def get_style(self, *args, **kwargs):
        return self.base.get_style(*args, **kwargs)

    def get_option(self, name):
        return self.base.get_option(name)

    def get_line_width(self, face_element=True):
        return self.base.get_line_width(face_element) * self.sx


def _flatten(leaves):
    for leaf in leaves:
        if leaf.get_head_name() == 'System`List':
            flattened = leaf.flatten(Symbol('List'))
            if flattened.get_head_name() == 'System`List':
                for x in flattened.leaves:
                    yield x
            else:
                yield flattened
        else:
            yield leaf


class _GraphicsElements(object):
    def __init__(self, content, evaluation):
        self.evaluation = evaluation

        builtins = evaluation.definitions.builtin
        def get_options(name):
            builtin = builtins.get(name)
            if builtin is None:
                return None
            return builtin.options

        def stylebox_style(style, specs):
            new_style = style.clone()
            for spec in _flatten(specs):
                head_name = spec.get_head_name()
                if head_name in style_and_form_heads:
                    new_style.append(spec)
                elif head_name == 'System`Rule' and len(spec.leaves) == 2:
                    option, expr = spec.leaves
                    if not isinstance(option, Symbol):
                        raise BoxConstructError

                    name = option.get_name()
                    create = style_options.get(name, None)
                    if create is None:
                        raise BoxConstructError

                    new_style.set_option(name, create(style.graphics, expr))
                else:
                    raise BoxConstructError
            return new_style

        def convert(content, style):
            if content.has_form('List', None):
                items = content.leaves
            else:
                items = [content]
            style = style.clone()
            for item in items:
                if item.get_name() == 'System`Null':
                    continue
                head = item.get_head_name()
                if head in style_and_form_heads:
                    style.append(item)
                elif head == 'System`StyleBox':
                    if len(item.leaves) < 1:
                        raise BoxConstructError
                    for element in convert(item.leaves[0], stylebox_style(style, item.leaves[1:])):
                        yield element
                elif head == 'System`GeometricTransformationBox':
                    yield GeometricTransformationBox(self, style, list(convert(item.leaves[0], style)), item.leaves[1])
                elif head[-3:] == 'Box':  # and head[:-3] in element_heads:
                    element_class = get_class(head)
                    if element_class is not None:
                        options = get_options(head[:-3])
                        if options:
                            data, options = _data_and_options(item.leaves, options)
                            new_item = Expression(head, *data)
                            element = get_class(head)(self, style, new_item, options)
                        else:
                            element = get_class(head)(self, style, item)
                        yield element
                    else:
                        raise BoxConstructError
                elif head == 'System`List':
                    for element in convert(item, style):
                        yield element
                else:
                    raise BoxConstructError

        self.elements = list(convert(content, self.get_style_class()(self)))

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
        self.pixel_width = None
        self.extent_width = self.extent_height = None
        self.local_to_screen = None

    def set_size(self, xmin, ymin, extent_width, extent_height, pixel_width, pixel_height):
        self.pixel_width = pixel_width
        self.extent_width = extent_width
        self.extent_height = extent_height

        tx = -xmin
        ty = -ymin

        w = extent_width if extent_width > 0 else 1
        h = extent_height if extent_height > 0 else 1

        sx = pixel_width / w
        sy = pixel_height / h

        qx = 0
        if self.neg_y:
            sy = -sy
            qy = pixel_height
        else:
            qy = 0

        # now build a transform matrix that mimics what used to happen in GraphicsElements.translate().
        # m = TranslationTransform[{qx, qy}].ScalingTransform[{sx, sy}].TranslationTransform[{tx, ty}]

        m = [[sx, 0, sx * tx + qx], [0, sy, sy * ty + qy], [0, 0, 1]]
        transform = _Transform(m)

        # update the GeometricTransformationBox, that always has to be the root element.

        self.elements[0].patch_transforms([transform])
        self.local_to_screen = transform
        self.inverse_local_to_screen = transform.inverse()

    def add_axis_element(self, e):
        # axis elements are added after the GeometricTransformationBox and are thus not
        # subject to the transformation from local to pixel space.
        self.elements.append(e)

    def translate(self, coords):
        if self.local_to_screen:
            return list(self.local_to_screen.transform([coords]))[0]
        else:
            return coords[0], coords[1]

    def translate_absolute(self, d):
        s = self.extent_width / self.pixel_width
        x, y = self.translate_absolute_in_pixels(d)
        return x * s, y * s

    def translate_absolute_in_pixels(self, d):
        if self.local_to_screen is None:
            return 0, 0
        else:
            l = 96.0 / 72  # d is measured in printer's points
            return d[0] * l, (-1 if self.neg_y else 1) * d[1] * l

    def translate_relative(self, x):
        if self.local_to_screen is None:
            return 0
        else:
            return x * self.extent_width

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

        aspect_ratio = graphics_options['System`AspectRatio']

        if image_size_multipliers is None:
            image_size_multipliers = (0.5, 0.25)

        if aspect_ratio == Symbol('Automatic'):
            aspect = None
        else:
            aspect = aspect_ratio.round_to_float()

        image_size = graphics_options['System`ImageSize']
        if isinstance(image_size, Integer):
            base_width = image_size.get_int_value()
            base_height = None  # will be computed later in calc_dimensions
        elif image_size.has_form('System`List', 2):
            base_width, base_height = ([x.round_to_float() for x in image_size.leaves] + [0, 0])[:2]
            if base_width is None or base_height is None:
                raise BoxConstructError
            aspect = base_height / base_width
        else:
            image_size = image_size.get_name()
            base_width, base_height = {
                'System`Automatic': (400, 350),
                'System`Tiny': (100, 100),
                'System`Small': (200, 200),
                'System`Medium': (400, 350),
                'System`Large': (600, 500),
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

        background = graphics_options['System`Background']
        if isinstance(background, Symbol) and background.get_name() == 'System`Automatic':
            self.background_color = None
        else:
            self.background_color =_Color.create(background)

        base_width, base_height, size_multiplier, size_aspect = \
            self._get_image_size(options, graphics_options, max_width)

        plot_range = graphics_options['System`PlotRange'].to_python()
        if plot_range == 'System`Automatic':
            plot_range = ['System`Automatic', 'System`Automatic']

        if not isinstance(plot_range, list) or len(plot_range) != 2:
            raise BoxConstructError

        transformation = Expression('System`TransformationFunction', [[1, 0, 0], [0, 1, 0], [0, 0, 1]])

        elements = GraphicsElements(
            Expression('System`GeometricTransformationBox', leaves[0], transformation),
            options['evaluation'], neg_y)

        axes = []  # to be filled further down

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

            # always need to compute extent if size aspect is automatic
            if 'System`Automatic' in plot_range or size_aspect is None:
                xmin, xmax, ymin, ymax = elements.extent()
            else:
                xmin = xmax = ymin = ymax = None

            if final_pass and any(x for x in axes) and plot_range != ['System`Automatic', 'System`Automatic']:
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
                if plot_range[0] == 'System`Automatic':
                    if xmin is None and xmax is None:
                        xmin = 0
                        xmax = 1
                    elif xmin == xmax:
                        xmin -= 1
                        xmax += 1
                elif (isinstance(plot_range[0], list) and
                      len(plot_range[0]) == 2):
                    xmin, xmax = list(map(float, plot_range[0]))
                    xmin, xmax = get_range(xmin, xmax)
                    xmin = elements.translate((xmin, 0))[0]
                    xmax = elements.translate((xmax, 0))[0]
                    if exmin is not None and exmin < xmin:
                        xmin = exmin
                    if exmax is not None and exmax > xmax:
                        xmax = exmax
                else:
                    raise BoxConstructError

                if plot_range[1] == 'System`Automatic':
                    if ymin is None and ymax is None:
                        ymin = 0
                        ymax = 1
                    elif ymin == ymax:
                        ymin -= 1
                        ymax += 1
                elif (isinstance(plot_range[1], list) and
                      len(plot_range[1]) == 2):
                    ymin, ymax = list(map(float, plot_range[1]))
                    ymin, ymax = get_range(ymin, ymax)
                    ymin = elements.translate((0, ymin))[1]
                    ymax = elements.translate((0, ymax))[1]
                    if ymin > ymax:
                        ymin, ymax = ymax, ymin
                    if eymin is not None and eymin < ymin:
                        ymin = eymin
                    if eymax is not None and eymax > ymax:
                        ymax = eymax
                else:
                    raise BoxConstructError
            except (ValueError, TypeError):
                raise BoxConstructError

            w = 0 if (xmin is None or xmax is None) else xmax - xmin
            h = 0 if (ymin is None or ymax is None) else ymax - ymin

            if size_aspect is None:
                aspect = h / w
            else:
                aspect = size_aspect

            height = base_height
            if height is None:
                height = base_width * aspect
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

        axes.extend(self.create_axes(elements, graphics_options, xmin, xmax, ymin, ymax))

        return elements, calc_dimensions

    def boxes_to_tex(self, leaves, **options):
        elements, calc_dimensions = self._prepare_elements(
            leaves, options, max_width=450)

        xmin, xmax, ymin, ymax, w, h, width, height = calc_dimensions()

        asy_completely_visible = '\n'.join(
            element.to_asy() for element in elements.elements
            if element.is_completely_visible)

        asy_regular = '\n'.join(
            element.to_asy() for element in elements.elements
            if not element.is_completely_visible)

        asy_box = 'box((%s,%s), (%s,%s))' % (asy_number(xmin), asy_number(ymin), asy_number(xmax), asy_number(ymax))

        if self.background_color is not None:
            color, opacity = self.background_color.to_asy()
            asy_background = 'filldraw(%s, %s);' % (asy_box, color)
        else:
            asy_background = ''

        tex = r"""
\begin{asy}
size(%scm, %scm);
%s
%s
clip(%s);
%s
\end{asy}
""" % (
            asy_number(width / 60), asy_number(height / 60),
            asy_background,
            asy_regular,
            asy_box,
            asy_completely_visible)

        return tex

    def boxes_to_xml(self, leaves, **options):
        elements, calc_dimensions = self._prepare_elements(
            leaves, options, neg_y=True)

        xmin, xmax, ymin, ymax, w, h, width, height = calc_dimensions()

        svg = elements.to_svg()

        if self.background_color is not None:
            svg = '<rect x="%f" y="%f" width="%f" height="%f" style="fill:%s"/>%s' % (
                xmin, ymin, w, h, self.background_color.to_css()[0], svg)

        xmin -= 1
        ymin -= 1
        w += 2
        h += 2

        svg_xml = '''
            <svg xmlns:svg="http://www.w3.org/2000/svg"
                xmlns="http://www.w3.org/2000/svg"
                version="1.1"
                viewBox="%s">
                %s
            </svg>
        ''' % (' '.join('%f' % t for t in (xmin, ymin, w, h)), svg)

        return '<mglyph width="%dpx" height="%dpx" src="data:image/svg+xml;base64,%s"/>' % (
            int(width),
            int(height),
            base64.b64encode(svg_xml.encode('utf8')).decode('utf8'))

    def axis_ticks(self, xmin, xmax):
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
        step_x_small = step_x / sub_x
        steps_x = int(floor((xmax - xmin) / step_x))
        steps_x_small = int(floor((xmax - xmin) / step_x_small))

        start_k_x = int(ceil(xmin / step_x))
        start_k_x_small = int(ceil(xmin / step_x_small))

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
        axes = graphics_options.get('System`Axes')
        if axes.is_true():
            axes = (True, True)
        elif axes.has_form('List', 2):
            axes = (axes.leaves[0].is_true(), axes.leaves[1].is_true())
        else:
            axes = (False, False)
        ticks_style = graphics_options.get('System`TicksStyle')
        axes_style = graphics_options.get('System`AxesStyle')
        label_style = graphics_options.get('System`LabelStyle')
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

        ticks_style = [s.to_axis_style() for s in ticks_style]
        axes_style = [s.to_axis_style() for s in axes_style]
        label_style = label_style.to_axis_style()

        ticks_style[0].extend(axes_style[0])
        ticks_style[1].extend(axes_style[1])

        def add_element(element):
            element.is_completely_visible = True
            elements.add_axis_element(element)

        ticks_x, ticks_x_small, origin_x = self.axis_ticks(xmin, xmax)
        ticks_y, ticks_y_small, origin_y = self.axis_ticks(ymin, ymax)

        axes_extra = 6
        tick_small_size = 3
        tick_large_size = 5
        tick_label_d = 2

        ticks_x_int = all(floor(x) == x for x in ticks_x)
        ticks_y_int = all(floor(x) == x for x in ticks_y)

        for index, (
            min, max, p_self0, p_other0, p_origin,
            ticks, ticks_small, ticks_int) in enumerate([
                (xmin, xmax, lambda y: (0, y), lambda x: (x, 0),
                 lambda x: (x, origin_y), ticks_x, ticks_x_small, ticks_x_int),
                (ymin, ymax, lambda x: (x, 0), lambda y: (0, y),
                 lambda y: (origin_x, y), ticks_y, ticks_y_small, ticks_y_int)]):
            if axes[index]:
                add_element(LineBox(
                    elements, axes_style[index],
                    lines=[[AxisCoords(elements, pos=p_origin(min), d=p_other0(-axes_extra)),
                            AxisCoords(elements, pos=p_origin(max), d=p_other0(axes_extra))]]))
                ticks_lines = []
                tick_label_style = ticks_style[index].clone()
                tick_label_style.extend(label_style)
                for x in ticks:
                    ticks_lines.append([AxisCoords(elements, pos=p_origin(x)),
                                        AxisCoords(elements, pos=p_origin(x),
                                               d=p_self0(tick_large_size))])
                    if ticks_int:
                        content = String(str(int(x)))
                    elif x == floor(x):
                        content = String('%.1f' % x)  # e.g. 1.0 (instead of 1.)
                    else:
                        content = String('%g' % x)  # fix e.g. 0.6000000000000001
                    add_element(InsetBox(
                        elements, tick_label_style,
                        content=content,
                        pos=AxisCoords(elements, pos=p_origin(x),
                        d=p_self0(-tick_label_d)), opos=p_self0(1)))
                for x in ticks_small:
                    pos = p_origin(x)
                    ticks_lines.append([AxisCoords(elements, pos=pos),
                                        AxisCoords(elements, pos=pos,
                                               d=p_self0(tick_small_size))])
                add_element(LineBox(elements, axes_style[0],
                                    lines=ticks_lines))
        return axes

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
    <dl>
    <dt>'Blend[{$c1$, $c2$}]'
        <dd>represents the color between $c1$ and $c2$.
    <dt>'Blend[{$c1$, $c2$}, $x$]'
        <dd>represents the color formed by blending $c1$ and $c2$ with
        factors 1 - $x$ and $x$ respectively.
    <dt>'Blend[{$c1$, $c2$, ..., $cn$}, $x$]'
        <dd>blends between the colors $c1$ to $cn$ according to the
        factor $x$.
    </dl>

    >> Blend[{Red, Blue}]
     = RGBColor[0.5, 0., 0.5]
    >> Blend[{Red, Blue}, 0.3]
     = RGBColor[0.7, 0., 0.3]
    >> Blend[{Red, Blue, Green}, 0.75]
     = RGBColor[0., 0.5, 0.5]

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

        if u.has_form('List', None):
            values = [value.round_to_float(evaluation) for value in u.leaves]
            if None in values:
                values = None
            if len(u.leaves) != len(colors):
                values = None
            use_list = True
        else:
            values = u.round_to_float(evaluation)
            if values is None:
                pass
            elif values > 1:
                values = 1.0
            elif values < 0:
                values = 0.0
            use_list = False
        if values is None:
            return evaluation.message('Blend', 'argl', u, Expression(
                'List', colors_orig))

        if use_list:
            return self.do_blend(colors, values).to_expr()
        else:
            x = values
            pos = int(floor(x * (len(colors) - 1)))
            x = (x - pos * 1.0 / (len(colors) - 1)) * (len(colors) - 1)
            if pos == len(colors) - 1:
                return colors[-1].to_expr()
            else:
                return self.do_blend(
                    colors[pos:(pos + 2)], [1 - x, x]).to_expr()


class Lighter(Builtin):
    """
    <dl>
    <dt>'Lighter[$c$, $f$]'
        <dd>is equivalent to 'Blend[{$c$, White}, $f$]'.
    <dt>'Lighter[$c$]'
        <dd>is equivalent to 'Lighter[$c$, 1/3]'.
    </dl>

    >> Lighter[Orange, 1/4]
     = RGBColor[1., 0.625, 0.25]
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
            text_name = strip_context(self.get_name()).lower()
        else:
            text_name = self.text_name
        doc = """
            <dl>
            <dt>'%(name)s'
            <dd>represents the color %(text_name)s in graphics.
            </dl>

            >> Graphics[{EdgeForm[Black], %(name)s, Disk[]}, ImageSize->Small]
             = -Graphics-

            >> %(name)s // ToBoxes
             = StyleBox[GraphicsBox[...], ...]
        """ % {'name': strip_context(self.get_name()), 'text_name': text_name}
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


class Automatic(Builtin):
    '''
    <dl>
    <dt>'Automatic'
        <dd>is used to specify an automatically computed option value.
    </dl>

    'Automatic' is the default for 'PlotRange', 'ImageSize', and other
    graphical options:

    >> Cases[Options[Plot], HoldPattern[_ :> Automatic]]
     = {Background :> Automatic, Exclusions :> Automatic, ImageSize :> Automatic, MaxRecursion :> Automatic, PlotRange :> Automatic, PlotRangePadding :> Automatic}
    '''


class Tiny(Builtin):
    '''
    <dl>
    <dt>'ImageSize' -> 'Tiny'
        <dd>produces a tiny image.
    </dl>
    '''


class Small(Builtin):
    '''
    <dl>
    <dt>'ImageSize' -> 'Small'
        <dd>produces a small image.
    </dl>
    '''


class Medium(Builtin):
    '''
    <dl>
    <dt>'ImageSize' -> 'Medium'
        <dd>produces a medium-sized image.
    </dl>
    '''


class Large(Builtin):
    '''
    <dl>
    <dt>'ImageSize' -> 'Large'
        <dd>produces a large image.
    </dl>
    '''


element_heads = frozenset(system_symbols(
    'Rectangle', 'Disk', 'Line', 'Arrow', 'FilledCurve', 'BezierCurve',
    'Point', 'Circle', 'Polygon', 'RegularPolygon',
    'Inset', 'Text', 'Sphere', 'Style'))

styles = system_symbols_dict({
    'RGBColor': RGBColor,
    'XYZColor': XYZColor,
    'LABColor': LABColor,
    'LCHColor': LCHColor,
    'LUVColor': LUVColor,
    'CMYKColor': CMYKColor,
    'Hue': Hue,
    'GrayLevel': GrayLevel,

    'Thickness': Thickness,
    'AbsoluteThickness': AbsoluteThickness,
    'Thick': Thick,
    'Thin': Thin,
    'PointSize': PointSize,
    'Arrowheads': Arrowheads,
})

style_options = system_symbols_dict({
    'FontColor': _style,
})

style_heads = frozenset(styles.keys())

style_and_form_heads = frozenset(style_heads.union(set(['System`EdgeForm', 'System`FaceForm'])))

GLOBALS = system_symbols_dict({
    'Rectangle': Rectangle,
    'Disk': Disk,
    'Circle': Circle,
    'Polygon': Polygon,
    'RegularPolygon': RegularPolygon,
    'Inset': Inset,
    'Text': Text,
    'RectangleBox': RectangleBox,
    'DiskBox': DiskBox,
    'LineBox': LineBox,
    'BezierCurveBox': BezierCurveBox,
    'FilledCurveBox': FilledCurveBox,
    'ArrowBox': ArrowBox,
    'CircleBox': CircleBox,
    'PolygonBox': PolygonBox,
    'RegularPolygonBox': RegularPolygonBox,
    'PointBox': PointBox,
    'InsetBox': InsetBox,
})

GLOBALS.update(styles)

GRAPHICS_SYMBOLS = frozenset(
    ['System`List', 'System`Rule', 'System`VertexColors'] +
    list(element_heads) +
    [element + 'Box' for element in element_heads] +
    list(style_heads))

