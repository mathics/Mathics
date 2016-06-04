#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Graphics
"""

from __future__ import unicode_literals
from __future__ import absolute_import
from __future__ import division

from math import floor, ceil, log10
import math
import json
import base64
from six.moves import map
from six.moves import range
from six.moves import zip
from itertools import chain

from mathics.builtin.base import (
    Builtin, InstancableBuiltin, BoxConstruct, BoxConstructError)
from mathics.builtin.options import options_to_rules
from mathics.core.expression import (
    Expression, Integer, Real, String, Symbol, strip_context,
    system_symbols, system_symbols_dict, from_python)


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
    return '%.5g' % value


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


class _PerfectReflectingDiffuser:
    def __init__(self, x1, x2):
        scale = 1.0 / x2
        self.xyz = (x1 * scale, 1.0, (1.0 - x1 - x2) * scale)

        q_r = self.xyz[0] + 15. * self.xyz[1] + 3. * self.xyz[2]
        self.u_r = 4. * self.xyz[0] / q_r
        self.v_r = 9. * self.xyz[1] / q_r


# MMA's reference white is a # D50, 2 degrees diffuser; for the
# values, see https://en.wikipedia.org/wiki/Standard_illuminant

_ref_white = _PerfectReflectingDiffuser(0.34567, 0.35850)


def _clip(*components):  # clip to [0, 1]
    return tuple(max(0, min(1, c)) for c in components)


def _euclidean_distance(a, b):
    return math.sqrt(sum((x1 - x2) * (x1 - x2) for x1, x2 in zip(a, b)))


def _component_distance(a, b, i):
    return abs(a[i] - b[i])


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
     . size(5.8556cm, 5.8333cm);
     . draw(ellipse((175,175),175,175), rgb(0, 0, 0)+linewidth(0.66667));
     . clip(box((-0.33333,0.33333), (350.33,349.67)));
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
            head = content.get_head_name()

            if head == 'System`List':
                return Expression('List', *[convert(item) for item in content.leaves])
            elif head == 'System`Style':
                return Expression('StyleBox', *[convert(item) for item in content.leaves])

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

                # if len(components) < len(self.default_components):
                #    components.extend(self.default_components[
                #                      len(components):])

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

    def to_cmyka(self):
        rgba = self.to_rgba()
        r, g, b = rgba[:3]
        k = 1 - max(r, g, b)
        k_ = 1 - k
        return ((1 - r - k) / k_, (1 - g - k) / k_, (1 - b - k) / k_, k) + tuple(rgba[3:])

    def to_ga(self):
        # see https://en.wikipedia.org/wiki/Grayscale
        rgba = self.to_rgba()
        r, g, b = rgba[:3]
        y = 0.299 * r + 0.587 * g + 0.114 * b  # Y of Y'UV
        return (y,) + tuple(rgba[3:])

    def to_hsba(self):
        return RGBColor(components=self.to_rgba()).to_hsba()

    def to_laba(self):
        return XYZColor(components=self.to_xyza()).to_laba()

    def to_lcha(self):
        # see http://www.brucelindbloom.com/Eqn_Lab_to_LCH.html
        laba = self.to_laba()
        l, a, b = laba[:3]
        h = math.atan2(b, a)
        if h < 0:
            h += 2 * math.pi
        h /= 2 * math.pi  # MMA specific
        return (l, math.sqrt(a * a + b * b), h) + tuple(laba[3:])

    def to_luva(self):
        return XYZColor(components=self.to_xyza()).to_luva()

    def to_rgba(self):
        return XYZColor(components=self.to_xyza()).to_rgba()

    def to_xyza(self):
        raise NotImplementedError


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

    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]

    def to_rgba(self):
        return self.components

    def to_xyza(self):
        components = _clip(*self.components)

        # inverse sRGB companding. see http://www.brucelindbloom.com/Eqn_RGB_to_XYZ.html
        r, g, b = (math.pow(((c + 0.055) / 1.055), 2.4)
                   if c > 0.04045 else c / 12.92 for c in components[:3])

        # use rRGB D50 conversion like MMA. see http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html
        return _clip(0.4360747 * r + 0.3850649 * g + 0.1430804 * b,
            0.2225045 * r + 0.7168786 * g + 0.0606169 * b,
            0.0139322 * r + 0.0971045 * g + 0.7141733 * b) + tuple(components[3:])

    def to_hsba(self):
        # see https://en.wikipedia.org/wiki/HSB_color_space. HSB is also known as HSV.

        components = _clip(*self.components)

        r, g, b = components[:3]
        m1 = max(r, g, b)
        m0 = min(r, g, b)
        c = m1 - m0

        if c < 1e-15:
            h = 0
        elif m1 == r:
            h = ((g - b) / c) % 6
        elif m1 == g:
            h = (b - r) / c + 2
        else:  # m1 == b
            h = (r - g) / c + 4
        h = (h * 60.) / 360.
        if h < 0.:
            h += 1.

        b = m1
        s = c / b

        return (h, s, b) + tuple(components[3:])


class LABColor(_Color):
    """
    <dl>
    <dt>'LABColor[$l$, $a$, $b$]'
        <dd>represents a color with the specified lightness, red/green and yellow/blue
        components in the CIE 1976 L*a*b* (CIELAB) color space.
    </dl>
    """

    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]

    def to_xyza(self):
        components = self.components

        # see https://en.wikipedia.org/wiki/Lab_color_space
        def inv_f(t):
            if t > (6. / 29.):
                return math.pow(t, 3)
            else:
                return 3. * (36. / 841.) * (t - (4. / 29.))

        l, a, b = components[:3]
        l0 = (l + 0.16) / 1.16
        xyz = [inv_f(l0 + a / 5.), inv_f(l0), inv_f(l0 - b / 2.)]

        return _clip(*[c * w for c, w in zip(xyz, _ref_white.xyz)]) + tuple(components[3:])

    def to_laba(self):
        return self.components


class LCHColor(_Color):
    """
    <dl>
    <dt>'LCHColor[$l$, $c$, $h$]'
        <dd>represents a color with the specified lightness, chroma and hue
        components in the CIELCh CIELab cube color space.
    </dl>
    """

    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]

    def to_xyza(self):
        return LABColor(components=self.to_laba()).to_xyza()

    def to_laba(self):
        lcha = self.to_lcha()
        l, c, h = lcha[:3]
        h *= 2 * math.pi  # MMA specific
        a = c * math.cos(h)
        b = c * math.sin(h)
        return (l, a, b) + tuple(lcha[3:])

    def to_lcha(self):
        return self.components


class LUVColor(_Color):
    """
    <dl>
    <dt>'LCHColor[$l$, $u$, $v$]'
        <dd>represents a color with the specified components in the CIE 1976 L*u*v* (CIELUV) color space.
    </dl>
    """

    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]

    def to_xyza(self):
        lum, u, v = self.components[:3]

        u_0 = u / (13. * lum) + _ref_white.u_r
        v_0 = v / (13. * lum) + _ref_white.v_r

        lum *= 100.0  # MMA specific

        if lum <= 8.:
            y = _ref_white.xyz[1] * lum * math.pow(3. / 29., 3)
        else:
            y = _ref_white.xyz[1] * math.pow((lum + 16.) / 116., 3)

        x = y * (9. * u_0) / (4. * v_0)
        z = y * (12. - 3. * u_0 - 20. * v_0) / (4. * v_0)

        return _clip(x, y, z) + tuple(self.components[3:])


class XYZColor(_Color):
    """
    <dl>
    <dt>'XYZColor[$x$, $y$, $z$]'
        <dd>represents a color with the specified components in the CIE 1931 XYZ color space.
    </dl>
    """

    components_sizes = [3, 4]
    default_components = [0, 0, 0, 1]

    _lab_t0 = math.pow(6. / 29., 3)
    _lab_a = math.pow(29. / 6., 2)

    def to_rgba(self):
        components = _clip(*self.components)

        x, y, z = components[:3]
        # multiply with the inverse matrix of the one in RGBColor.to_xyza()
        r, g, b = [x * 3.13386 + y * -1.61687 + z * -0.490615,
                   x * -0.978769 + y * 1.91614 + z * 0.0334541,
                   x * 0.0719452 + y * -0.228991 + z * 1.40524]

        return _clip(*[1.055 * math.pow(c, 1. / 2.4) - 0.055 if c > 0.0031308
                       else c * 12.92 for c in (r, g, b)]) + tuple(components[3:])

    def to_xyza(self):
        return self.components

    def to_laba(self):
        components = _clip(*self.components)

        # computation of x, y, z; see https://en.wikipedia.org/wiki/Lab_color_space
        components = [c / w for c, w in zip(components, _ref_white.xyz)]

        x, y, z = (math.pow(t, 1. / 3.) if t > XYZColor._lab_t0
                   else XYZColor._lab_a * t + (4. / 29.) for t in components)

        # MMA scales by 1/100
        return ((1.16 * y) - 0.16, 5. * (x - y), 2. * (y - z)) + tuple(components[3:])

    def to_luva(self):
        # see http://www.brucelindbloom.com/Eqn_XYZ_to_Luv.html
        # and https://en.wikipedia.org/wiki/CIELUV

        components = _clip(*self.components)

        x_orig, y_orig, z_orig = components[:3]
        y = y_orig / _ref_white.xyz[1]

        lum = 116. * math.pow(y, 1. / 3.) - 16. if y > 0.008856 else 903.3 * y

        q_0 = x_orig + 15. * y_orig + 3. * z_orig
        u_0 = 4. * x_orig / q_0
        v_0 = 9. * y_orig / q_0

        lum /= 100.0  # MMA specific
        u = 13. * lum * (u_0 - _ref_white.u_r)
        v = 13. * lum * (v_0 - _ref_white.v_r)

        return (lum, u, v) + tuple(components[3:])


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

    components_sizes = [3, 4, 5]
    default_components = [0, 0, 0, 0, 1]

    def to_cmyka(self):
        return self.components

    def to_rgba(self):
        k = self.components[3] if len(self.components) >= 4 else 0
        k_ = 1 - k
        c = self.components
        cmy = [c[0] * k_ + k, c[1] * k_ + k, c[2] * k_ + k]
        rgb = (1 - cmy[0], 1 - cmy[1], 1 - cmy[2])
        return rgb + tuple(c[4:])

    def to_xyza(self):
        return RGBColor(components=self.to_rgba()).to_xyza()


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
        return rgb + tuple(self.components[3:])

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

    def to_xyza(self):
        return RGBColor(components=self.to_rgba()).to_xyza()

    def to_hsba(self):
        return self.components


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
    components_sizes = [1, 2]
    default_components = [0, 1]

    def to_rgba(self):
        g = self.components[0]
        return (g, g, g, self.components[1])

    def to_ga(self):
        return self.components

    def to_xyza(self):
        return RGBColor(components=self.to_rgba()).to_xyza()


class ColorConvert(Builtin):
    """
    <dl>
    <dt>'ColorConvert[$c$, $colspace$]'
        <dd>returns the representation of color $c$ in the color space $colspace$.
    </dl>

    Valid values for $colspace$ are:

    CMYK: convert to CMYKColor
    Grayscale: convert to GrayLevel
    HSB: convert to Hue
    LAB: concert to LABColor
    LCH: convert to LCHColor
    LUV: convert to LUVColor
    RGB: convert to RGBColor
    XYZ: convert to XYZColor
    """

    _convert = {
        'CMYK': lambda color: CMYKColor(components=color.to_cmyka()),
        'Grayscale': lambda color: GrayLevel(components=color.to_ga()),
        'HSB': lambda color: Hue(components=color.to_hsba()),
        'LAB': lambda color: LABColor(components=color.to_laba()),
        'LCH': lambda color: LCHColor(components=color.to_lcha()),
        'LUV': lambda color: LUVColor(components=color.to_luva()),
        'RGB': lambda color: RGBColor(components=color.to_rgba()),
        'XYZ': lambda color: XYZColor(components=color.to_xyza())
    }

    messages = {
        'ccvinput': '`` should be a color.',
        'imgcstype': '`` is not a valid color space.',
    }

    def apply(self, color, colorspace, evaluation):
        'ColorConvert[color_, colorspace_String]'
        try:
            py_color = _Color.create(color)
        except ColorError:
            evaluation.message('ColorConvert', 'ccvinput', color)
            return

        convert = ColorConvert._convert.get(colorspace.get_string_value())
        if not convert:
            evaluation.message('ColorConvert', 'imgcstype', colorspace)
            return

        converted_color = convert(py_color)
        return Expression(converted_color.get_name(), *converted_color.components)


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
    DeltaL: difference in the L component of LCHColor
    DeltaC: difference in the C component of LCHColor
    DeltaH: difference in the H component of LCHColor

    >> N[ColorDistance[Magenta, Green], 5]
     = 2.2507
    """

    options = {
        'DistanceFunction': '"CIE76"',
    }

    messages = {
        'invdist': '`` is not a valid color distance function.',
        'invarg': '`1` and `2` should be two colors or a color and a lists of colors or ' +
                  'two lists of colors of the same length.'
    }

    _distances = {
        "CIE76": lambda c1, c2: _euclidean_distance(c1.to_laba()[:3], c2.to_laba()[:3]),
        "CIE94": lambda c1, c2: _euclidean_distance(c1.to_lcha()[:3], c2.to_lcha()[:3]),
        "DeltaL": lambda c1, c2: _component_distance(c1.to_lcha(), c2.to_lcha(), 0),
        "DeltaC": lambda c1, c2: _component_distance(c1.to_lcha(), c2.to_lcha(), 1),
        "DeltaH": lambda c1, c2: _component_distance(c1.to_lcha(), c2.to_lcha(), 2),
    }

    def apply(self, c1, c2, evaluation, options):
        'ColorDistance[c1_, c2_, OptionsPattern[ColorDistance]]'
        distance_function = options.get('System`DistanceFunction')
        if isinstance(distance_function, String):
            compute = ColorDistance._distances.get(distance_function.get_string_value())
            if not compute:
                evaluation.message('ColorDistance', 'invdist', distance_function)
                return
        else:
            def compute(a, b):
                Expression(distance_function, a.to_laba(), b.to_laba())

        def distance(a, b):
            try:
                py_a = _Color.create(a)
                py_b = _Color.create(b)
            except ColorError:
                evaluation.message('ColorDistance', 'invarg', a, b)
                raise
            return from_python(compute(py_a, py_b))

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
        return self.graphics.view_width * self.value


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
        svg = (
            '<foreignObject x="%f" y="%f" ox="%f" oy="%f" style="%s">'
            '<math>%s</math></foreignObject>') % (
                x, y, self.opos[0], self.opos[1], style, content)
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


class Style(object):
    def __init__(self, graphics, edge=False, face=False):
        self.styles = []
        self.graphics = graphics
        self.edge = edge
        self.face = face
        self.klass = graphics.get_style_class()

    def append(self, item, allow_forms=True):
        head = item.get_head_name()
        if head in style_heads:
            klass = get_class(head)
            style = klass.create_as_style(klass, self.graphics, item)
        elif head in ('System`EdgeForm', 'System`FaceForm'):
            style = self.klass(self.graphics, edge=head == 'System`EdgeForm',
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
        self.elements = []

        builtins = evaluation.definitions.builtin
        def get_options(name):
            builtin = builtins.get(name)
            if builtin is None:
                return None
            return builtin.options

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
                    new_style = style.clone()
                    for spec in _flatten(item.leaves[1:]):
                        if spec.get_head_name() not in style_and_form_heads:
                            raise BoxConstructError
                        new_style.append(spec)
                    convert(item.leaves[0], new_style)
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
                        self.elements.append(element)
                    else:
                        raise BoxConstructError
                elif head == 'System`List':
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

        base_width, base_height, size_multiplier, size_aspect = \
            self._get_image_size(options, graphics_options, max_width)

        plot_range = graphics_options['System`PlotRange'].to_python()
        if plot_range == 'System`Automatic':
            plot_range = ['System`Automatic', 'System`Automatic']

        if not isinstance(plot_range, list) or len(plot_range) != 2:
            raise BoxConstructError

        elements = GraphicsElements(leaves[0], options['evaluation'], neg_y)

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
            if final_pass and plot_range != ['System`Automatic',
                                             'System`Automatic']:
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

        self.create_axes(elements, graphics_options, xmin, xmax, ymin, ymax)

        return elements, calc_dimensions

    def boxes_to_tex(self, leaves, **options):
        elements, calc_dimensions = self._prepare_elements(
            leaves, options, max_width=450)

        xmin, xmax, ymin, ymax, w, h, width, height = calc_dimensions()
        elements.view_width = w

        asy_completely_visible = '\n'.join(
            element.to_asy() for element in elements.elements
            if element.is_completely_visible)

        asy_regular = '\n'.join(
            element.to_asy() for element in elements.elements
            if not element.is_completely_visible)

        tex = r"""
\begin{asy}
size(%scm, %scm);
%s
clip(box((%s,%s), (%s,%s)));
%s
\end{asy}
""" % (
            asy_number(width / 60), asy_number(height / 60),
            asy_regular,
            asy_number(xmin), asy_number(ymin), asy_number(xmax), asy_number(ymax),
            asy_completely_visible)

        return tex

    def boxes_to_xml(self, leaves, **options):
        elements, calc_dimensions = self._prepare_elements(
            leaves, options, neg_y=True)

        xmin, xmax, ymin, ymax, w, h, width, height = calc_dimensions()
        elements.view_width = w

        svg = elements.to_svg()

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
                    if ticks_int:
                        content = String(str(int(x)))
                    elif x == floor(x):
                        content = String('%.1f' % x)  # e.g. 1.0 (instead of 1.)
                    else:
                        content = String('%g' % x)  # fix e.g. 0.6000000000000001
                    add_element(InsetBox(
                        elements, tick_label_style,
                        content=content,
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

    >> Cases[Options[Plot], _ :> Automatic]
     = {Exclusions :> Automatic, ImageSize :> Automatic, MaxRecursion :> Automatic, PlotRange :> Automatic, PlotRangePadding :> Automatic}
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
    'Rectangle', 'Disk', 'Line', 'FilledCurve', 'BezierCurve', 'Point', 'Circle', 'Polygon', 'Inset', 'Text', 'Sphere', 'Style'))

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
})

style_heads = frozenset(styles.keys())

style_and_form_heads = frozenset(style_heads.union(set(['System`EdgeForm', 'System`FaceForm'])))

GLOBALS = system_symbols_dict({
    'Rectangle': Rectangle,
    'Disk': Disk,
    'Circle': Circle,
    'Polygon': Polygon,
    'Inset': Inset,
    'Text': Text,
    'RectangleBox': RectangleBox,
    'DiskBox': DiskBox,
    'LineBox': LineBox,
    'BezierCurveBox': BezierCurveBox,
    'FilledCurveBox': FilledCurveBox,
    'CircleBox': CircleBox,
    'PolygonBox': PolygonBox,
    'PointBox': PointBox,
    'InsetBox': InsetBox,
})

GLOBALS.update(styles)

GRAPHICS_SYMBOLS = frozenset(
    ['System`List', 'System`Rule', 'System`VertexColors'] +
    list(element_heads) +
    [element + 'Box' for element in element_heads] +
    list(style_heads))

