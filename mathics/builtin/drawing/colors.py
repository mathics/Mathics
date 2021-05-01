#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from math import pi

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.numpy_utils import (
    sqrt,
    floor,
    mod,
    cos,
    sin,
    arctan2,
    minimum,
    maximum,
    dot_t,
)
from mathics.builtin.numpy_utils import (
    stack,
    unstack,
    array,
    clip,
    conditional,
    choose,
    stacked,
)

# in the long run, we might want to implement these functions using Compile[]. until Compile[] is available for all
# Mathics configurations, this implementation works on Python and PyPy and with or without numpy.


def _clip1(t):
    return clip(t, 0, 1)


class _PerfectReflectingDiffuser:
    def __init__(self, *x):  # accepts (x1, y2) or (x, y, z)
        assert 2 <= len(x) <= 3

        if len(x) == 3:
            self.xyz = x
        else:
            x1, x2 = x
            scale = 1.0 / x2
            self.xyz = (x1 * scale, 1.0, (1.0 - x1 - x2) * scale)

        q_r = self.xyz[0] + 15.0 * self.xyz[1] + 3.0 * self.xyz[2]
        self.u_r = 4.0 * self.xyz[0] / q_r
        self.v_r = 9.0 * self.xyz[1] / q_r


# MMA's reference white is a D50, 2 degrees diffuser; the standard values for this configuration
# can be found at https://en.wikipedia.org/wiki/Standard_illuminant, MMA seems to use slightly
# different values though.

_ref_white = _PerfectReflectingDiffuser(0.96422, 1.0, 0.82521)

# use rRGB D50 conversion like MMA. see http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html
# MMA seems to round matrix values to six significant digits. we do the same.

_xyz_from_rgb = [
    [0.436075, 0.385065, 0.14308],
    [0.222504, 0.716879, 0.0606169],
    [0.0139322, 0.0971045, 0.714173],
]

# for matrix, see http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html
# MMA seems to round matrix values to six significant digits. we do the same.

_rgb_from_xyz = [
    [3.13386, -1.61687, -0.490615],
    [-0.978768, 1.91614, 0.033454],
    [0.0719453, -0.228991, 1.40524],
]


def rgb_to_grayscale(r, g, b, *rest):
    # see https://en.wikipedia.org/wiki/Grayscale
    y = 0.299 * r + 0.587 * g + 0.114 * b  # Y of Y'UV
    return (y,) + rest


def grayscale_to_rgb(g, *rest):
    return (g, g, g) + rest


@conditional
def _inverse_compand_srgb(x):
    if x > 0.04045:
        return ((x + 0.055) / 1.055) ** 2.4
    else:
        return x / 12.92


def rgb_to_xyz(r, g, b, *rest):
    r, g, b = map(_inverse_compand_srgb, (r, g, b))
    x, y, z = unstack(_clip1(dot_t(stack(r, g, b), _xyz_from_rgb)))
    return map(_clip1, (x, y, z) + rest)


@conditional
def _compute_hsb_s(m1, c, eps):
    if m1 < eps:
        return 0
    else:
        return c / m1


@conditional
def _compute_hsb_h(m1, c, eps, r, g, b):
    if c < eps:
        return 0
    elif m1 == r:
        return mod(((g - b) / c), 6.0)
    elif m1 == g:
        return (b - r) / c + 2.0
    elif m1 == b:
        return (r - g) / c + 4.0


@conditional
def _wrap_hsb_h(h):
    if h < 0.0:
        return h + 1.0
    else:
        return h


def rgb_to_hsb(r, g, b, *rest):
    # see https://en.wikipedia.org/wiki/HSB_color_space. HSB is also known as HSV.
    r, g, b = _clip1((r, g, b))

    m1 = maximum(r, g, b)
    m0 = minimum(r, g, b)
    c = m1 - m0
    eps = 1e-15

    h = _compute_hsb_h(m1, c, eps, r, g, b)
    h = _wrap_hsb_h(h * (60.0 / 360.0))
    s = _compute_hsb_s(m1, c, eps)

    return (h, s, m1) + rest


def hsb_to_rgb(h, s, v, *rest):
    i = floor(6 * h)
    f = 6 * h - i
    i = mod(i, 6)
    p = v * (1 - s)
    q = v * (1 - f * s)
    t = v * (1 - (1 - f) * s)

    r, g, b = choose(
        i, (v, t, p), (q, v, p), (p, v, t), (p, q, v), (t, p, v), (v, p, q)
    )

    return (r, g, b) + rest


def cmyk_to_rgb(c, m, y, *rest):
    if rest:
        k = rest[0]
        rest = rest[1:]
    else:
        k = 0
    k_ = 1 - k

    cmy = [(x * k_ + k) for x in (c, m, y)]
    r, g, b = [1 - x for x in cmy]

    return (r, g, b) + rest


@conditional
def _scale_rgb_to_cmyk(t, k, k_, eps):
    if k_ < eps:
        return 0
    else:
        return (1 - t - k) / k_


def rgb_to_cmyk(r, g, b, *rest):
    k_ = maximum(r, g, b)
    k = 1 - k_
    c, m, y = map(lambda t: _scale_rgb_to_cmyk(t, k, k_, 1e-15), (r, g, b))
    return (c, m, y, k) + rest


@conditional
def _compand_srgb(t):
    if t > 0.0031308:
        return 1.055 * (t ** (1.0 / 2.4)) - 0.055
    else:
        return t * 12.92


def xyz_to_rgb(x, y, z, *rest):
    x, y, z = map(_clip1, (x, y, z))
    r, g, b = unstack(dot_t(stack(x, y, z), _rgb_from_xyz))
    r, g, b = map(_clip1, (r, g, b))
    r, g, b = map(_compand_srgb, (r, g, b))
    return map(_clip1, (r, g, b) + rest)


@conditional
def _scale_xyz_to_lab(t):
    if t > 0.008856:
        return t ** 0.33333333  # MMA specific
    else:
        return (903.3 * t + 16.0) / 116.0


def xyz_to_lab(x, y, z, *rest):
    # see http://www.brucelindbloom.com/Eqn_XYZ_to_Lab.html
    xyz = array([u / v for u, v in zip([x, y, z], _ref_white.xyz)])
    x, y, z = map(_scale_xyz_to_lab, xyz)

    # MMA scales by 1/100
    return ((1.16 * y) - 0.16, 5.0 * (x - y), 2.0 * (y - z)) + rest


@conditional
def _xyz_to_luv_scale_y(y):
    if y > 0.008856:
        return 116.0 * (y ** (1.0 / 3.0)) - 16
    else:
        return 903.3 * y


def xyz_to_luv(x, y, z, *rest):
    # see http://www.brucelindbloom.com/Eqn_XYZ_to_Luv.html
    # and https://en.wikipedia.org/wiki/CIELUV
    xyz = _clip1((x, y, z))

    x_orig, y_orig, z_orig = xyz
    y = y_orig / _ref_white.xyz[1]

    lum = _xyz_to_luv_scale_y(y)

    q_0 = x_orig + 15.0 * y_orig + 3.0 * z_orig
    u_0 = 4.0 * x_orig / q_0
    v_0 = 9.0 * y_orig / q_0

    lum /= 100.0  # MMA specific
    u = 13.0 * lum * (u_0 - _ref_white.u_r)
    v = 13.0 * lum * (v_0 - _ref_white.v_r)

    return (lum, u, v) + rest


@conditional
def _scale_lab_to_xyz(t):
    if t > 0.2068930:  # 0.008856 ** (1/3)
        return t ** 3
    else:
        return (t - 16.0 / 116.0) / 7.787


@conditional
def _luv_to_xyz_clip_zero(cie_l_is_zero, t):
    if cie_l_is_zero:
        return 0
    else:
        return t


def luv_to_xyz(cie_l, cie_u, cie_v, *rest):
    # see http://www.easyrgb.com/index.php?X=MATH&H=17#text17
    u = cie_u / (13.0 * cie_l) + _ref_white.u_r
    v = cie_v / (13.0 * cie_l) + _ref_white.v_r

    cie_l_100 = cie_l * 100.0  # MMA specific
    y = (cie_l_100 + 16.0) / 116.0
    y = _scale_lab_to_xyz(y)

    x = -(9 * y * u) / ((u - 4) * v - u * v)
    z = (9 * y - (15 * v * y) - (v * x)) / (3 * v)

    cie_l_is_zero = cie_l < 0.078
    x, y, z = map(lambda t: _luv_to_xyz_clip_zero(cie_l_is_zero, t), (x, y, z))
    x, y, z = clip([x, y, z], 0, 1)

    return (x, y, z) + rest


def lch_to_lab(l, c, h, *rest):
    h *= 2 * pi  # MMA specific
    return (l, c * cos(h), c * sin(h)) + rest


@conditional
def _wrap_lch_h(h, pi2):
    if h < 0.0:
        return h + pi2
    else:
        return h


def lab_to_lch(l, a, b, *rest):
    h = _wrap_lch_h(arctan2(b, a), 2.0 * pi)
    h /= 2.0 * pi  # MMA specific
    return (l, sqrt(a * a + b * b), h) + rest


def lab_to_xyz(l, a, b, *rest):
    # see http://www.easyrgb.com/index.php?X=MATH&H=08#text8
    f_y = (l * 100.0 + 16.0) / 116.0
    x, y, z = a / 5.0 + f_y, f_y, f_y - b / 2.0
    x, y, z = map(_scale_lab_to_xyz, (x, y, z))

    x, y, z = [u * v for u, v in zip((x, y, z), _ref_white.xyz)]
    x, y, z = clip([x, y, z], 0, 1)

    return (x, y, z) + rest


# for an overview of color conversions see http://www.brucelindbloom.com/Math.html

# the following table was computed by starting with the allowed hard
# coded conversions from "conversions" and then finding the shortest
# paths:

# g = Graph[{"Grayscale" -> "RGB", "RGB" -> "Grayscale", "CMYK" -> "RGB", "RGB" -> "CMYK", "RGB" -> "HSB",
#   "HSB" -> "RGB", "XYZ" -> "LAB", "XYZ" -> "LUV", "XYZ" -> "RGB", "LAB" -> "XYZ", "LAB" -> "LCH",
#   "LCH" -> "LAB", "LUV" -> "XYZ", "RGB" -> "XYZ"}]
# s = FindShortestPath[g, All, All]; {#, s @@ #} & /@ Permutations[{
#   "Grayscale", "RGB", "CMYK", "HSB", "XYZ", "LAB", "LUV", "LCH"}, {2}] // CForm

_paths = dict(
    (
        (("Grayscale", "RGB"), ("Grayscale", "RGB")),
        (("Grayscale", "CMYK"), ("Grayscale", "RGB", "CMYK")),
        (("Grayscale", "HSB"), ("Grayscale", "RGB", "HSB")),
        (("Grayscale", "XYZ"), ("Grayscale", "RGB", "XYZ")),
        (("Grayscale", "LAB"), ("Grayscale", "RGB", "XYZ", "LAB")),
        (("Grayscale", "LUV"), ("Grayscale", "RGB", "XYZ", "LUV")),
        (("Grayscale", "LCH"), ("Grayscale", "RGB", "XYZ", "LAB", "LCH")),
        (("RGB", "Grayscale"), ("RGB", "Grayscale")),
        (("RGB", "CMYK"), ("RGB", "CMYK")),
        (("RGB", "HSB"), ("RGB", "HSB")),
        (("RGB", "XYZ"), ("RGB", "XYZ")),
        (("RGB", "LAB"), ("RGB", "XYZ", "LAB")),
        (("RGB", "LUV"), ("RGB", "XYZ", "LUV")),
        (("RGB", "LCH"), ("RGB", "XYZ", "LAB", "LCH")),
        (("CMYK", "Grayscale"), ("CMYK", "RGB", "Grayscale")),
        (("CMYK", "RGB"), ("CMYK", "RGB")),
        (("CMYK", "HSB"), ("CMYK", "RGB", "HSB")),
        (("CMYK", "XYZ"), ("CMYK", "RGB", "XYZ")),
        (("CMYK", "LAB"), ("CMYK", "RGB", "XYZ", "LAB")),
        (("CMYK", "LUV"), ("CMYK", "RGB", "XYZ", "LUV")),
        (("CMYK", "LCH"), ("CMYK", "RGB", "XYZ", "LAB", "LCH")),
        (("HSB", "Grayscale"), ("HSB", "RGB", "Grayscale")),
        (("HSB", "RGB"), ("HSB", "RGB")),
        (("HSB", "CMYK"), ("HSB", "RGB", "CMYK")),
        (("HSB", "XYZ"), ("HSB", "RGB", "XYZ")),
        (("HSB", "LAB"), ("HSB", "RGB", "XYZ", "LAB")),
        (("HSB", "LUV"), ("HSB", "RGB", "XYZ", "LUV")),
        (("HSB", "LCH"), ("HSB", "RGB", "XYZ", "LAB", "LCH")),
        (("XYZ", "Grayscale"), ("XYZ", "RGB", "Grayscale")),
        (("XYZ", "RGB"), ("XYZ", "RGB")),
        (("XYZ", "CMYK"), ("XYZ", "RGB", "CMYK")),
        (("XYZ", "HSB"), ("XYZ", "RGB", "HSB")),
        (("XYZ", "LAB"), ("XYZ", "LAB")),
        (("XYZ", "LUV"), ("XYZ", "LUV")),
        (("XYZ", "LCH"), ("XYZ", "LAB", "LCH")),
        (("LAB", "Grayscale"), ("LAB", "XYZ", "RGB", "Grayscale")),
        (("LAB", "RGB"), ("LAB", "XYZ", "RGB")),
        (("LAB", "CMYK"), ("LAB", "XYZ", "RGB", "CMYK")),
        (("LAB", "HSB"), ("LAB", "XYZ", "RGB", "HSB")),
        (("LAB", "XYZ"), ("LAB", "XYZ")),
        (("LAB", "LUV"), ("LAB", "XYZ", "LUV")),
        (("LAB", "LCH"), ("LAB", "LCH")),
        (("LUV", "Grayscale"), ("LUV", "XYZ", "RGB", "Grayscale")),
        (("LUV", "RGB"), ("LUV", "XYZ", "RGB")),
        (("LUV", "CMYK"), ("LUV", "XYZ", "RGB", "CMYK")),
        (("LUV", "HSB"), ("LUV", "XYZ", "RGB", "HSB")),
        (("LUV", "XYZ"), ("LUV", "XYZ")),
        (("LUV", "LAB"), ("LUV", "XYZ", "LAB")),
        (("LUV", "LCH"), ("LUV", "XYZ", "LAB", "LCH")),
        (("LCH", "Grayscale"), ("LCH", "LAB", "XYZ", "RGB", "Grayscale")),
        (("LCH", "RGB"), ("LCH", "LAB", "XYZ", "RGB")),
        (("LCH", "CMYK"), ("LCH", "LAB", "XYZ", "RGB", "CMYK")),
        (("LCH", "HSB"), ("LCH", "LAB", "XYZ", "RGB", "HSB")),
        (("LCH", "XYZ"), ("LCH", "LAB", "XYZ")),
        (("LCH", "LAB"), ("LCH", "LAB")),
        (("LCH", "LUV"), ("LCH", "LAB", "XYZ", "LUV")),
    )
)

conversions = {
    "Grayscale>RGB": grayscale_to_rgb,
    "RGB>Grayscale": rgb_to_grayscale,
    "CMYK>RGB": cmyk_to_rgb,
    "RGB>CMYK": rgb_to_cmyk,
    "RGB>HSB": rgb_to_hsb,
    "HSB>RGB": hsb_to_rgb,
    "XYZ>LAB": xyz_to_lab,
    "XYZ>LUV": xyz_to_luv,
    "XYZ>RGB": xyz_to_rgb,
    "LAB>XYZ": lab_to_xyz,
    "LAB>LCH": lab_to_lch,
    "LCH>LAB": lch_to_lab,
    "LUV>XYZ": luv_to_xyz,
    "RGB>XYZ": rgb_to_xyz,
}

colorspaces = frozenset(
    ("Grayscale", "RGB", "CMYK", "HSB", "XYZ", "LAB", "LCH", "LUV",)
)


def convert(components, src, dst, preserve_alpha=True):
    if not preserve_alpha:
        if src == "Grayscale":
            non_alpha = 1
        elif src == "CMYK":
            non_alpha = 4
        else:
            non_alpha = 3

        def omit_alpha(*c):
            return c[:non_alpha]

        components = stacked(omit_alpha, components)

    if src == dst:
        return components

    path = _paths.get((src, dst), None)
    if path is None:
        return None

    for s, d in zip(path[:-1], path[1:]):
        func = conversions.get("%s>%s" % (s, d))
        if not func:
            return None
        components = stacked(func, components)

    return components
