#!/usr/bin/env python
# -*- coding: utf-8 -*-

from itertools import chain
from math import pi

from mathics.builtin.numpy_utils import sqrt, floor, mod, cos, sin, arctan2, minimum, maximum, dot_t, errstate
from mathics.builtin.numpy_utils import vectorized, stack, unstack, concat, array, clip, conditional, compose, choose
from mathics.builtin.numpy_utils import constant

# use rRGB D50 conversion like MMA. see http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html
# MMA seems to round matrix values to six significant digits. we do the same.

xyz_from_rgb = [
    [0.436075, 0.385065, 0.14308],
    [0.222504, 0.716879, 0.0606169],
    [0.0139322, 0.0971045, 0.714173]
]

# for matrix, see http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html
# MMA seems to round matrix values to six significant digits. we do the same.
rgb_from_xyz = [
    [3.13386, -1.61687, -0.490615],
    [-0.978768, 1.91614, 0.033454],
    [0.0719453, -0.228991, 1.40524]
]


class _PerfectReflectingDiffuser:
    def __init__(self, x1, x2):
        # MMA seems to use the following constants, and not the
        # values derived via calculation (commented out below)
        self.xyz = [0.96422, 1., 0.82521]
        # scale = 1.0 / x2
        # self.xyz = (x1 * scale, 1.0, (1.0 - x1 - x2) * scale)

        q_r = self.xyz[0] + 15. * self.xyz[1] + 3. * self.xyz[2]
        self.u_r = 4. * self.xyz[0] / q_r
        self.v_r = 9. * self.xyz[1] / q_r


# MMA's reference white is a # D50, 2 degrees diffuser; for the
# values, see https://en.wikipedia.org/wiki/Standard_illuminant

_ref_white = _PerfectReflectingDiffuser(0.34567, 0.35850)


def rgb_to_grayscale(pixels):
    # see https://en.wikipedia.org/wiki/Grayscale
    components = unstack(pixels)
    r, g, b = components[:3]
    y = 0.299 * r + 0.587 * g + 0.114 * b  # Y of Y'UV
    return stack(y, *components[3:])


def grayscale_to_rgb(pixels):
    components = unstack(pixels)
    g = components[0]
    return stack(g, g, g, *components[1:])


def rgb_to_xyz(pixels):
    components = unstack(pixels)
    rgb = components[:3]

    x, y, z = conditional(rgb, lambda t: t > 0.04045,
                          lambda t: ((t + 0.055) / 1.055) ** 2.4,
                          lambda t: t / 12.92)
    xyz = dot_t(stack(x, y, z), xyz_from_rgb)

    return clip(concat(xyz, components[3:]), 0, 1)


def rgb_to_hsb(pixels):
    # see https://en.wikipedia.org/wiki/HSB_color_space. HSB is also known as HSV.
    components = unstack(pixels)
    r, g, b = clip(components[:3], 0, 1)

    m1 = maximum(r, g, b)
    m0 = minimum(r, g, b)
    c = m1 - m0

    with errstate(divide='ignore', invalid='ignore'):
        eps = 1e-15

        h = compose(
            c < eps,
            lambda mask: 0,
            m1 == r,
            lambda mask: mod(((mask(g) - mask(b)) / mask(c)), 6.),
            m1 == g,
            lambda mask: (mask(b) - mask(r)) / mask(c) + 2.,
            m1 == b,
            lambda mask: (mask(r) - mask(g)) / mask(c) + 4.)

        s = compose(m1 < eps, lambda mask: 0, m1 >= eps, lambda mask: mask(c / m1))

    h = conditional(h * (60. / 360.), lambda t: t < 0.,
                    lambda t: t + 1.,
                    lambda t: t)

    return stack(h, s, m1, *components[3:])


def hsb_to_rgb(pixels):
    components = unstack(pixels)
    h, s, v = components[:3]

    i = floor(6 * h)
    f = 6 * h - i
    i = mod(i, 6)
    p = v * (1 - s)
    q = v * (1 - f * s)
    t = v * (1 - (1 - f) * s)

    r, g, b = choose(i,
        (v, t, p),
        (q, v, p),
        (p, v, t),
        (p, q, v),
        (t, p, v),
        (v, p, q))

    return stack(r, g, b, *components[3:])


def cmyk_to_rgb(pixels):
    c = unstack(pixels)

    if len(c) >= 4:
        k = c[3]
    else:
        k = 0
    k_ = 1 - k

    cmy = [(x * k_ + k) for x in c[:3]]
    r, g, b = [1 - x for x in cmy]

    return concat(stack(r, g, b), c[4:])


def rgb_to_cmyk(pixels):
    c = unstack(pixels)

    r, g, b = c[:3]
    k_ = maximum(r, g, b)
    k = 1 - k_

    eps = 1e-15
    with errstate(divide='ignore', invalid='ignore'):
        c, m, y = [compose(k_ < eps, lambda s: s(constant(0, a)),
                           k_ >= eps, lambda s: (1 - s(a) - s(k)) / s(k_)) for a in (r, g, b)]

    return stack(c, m, y, k, *c[3:])


def xyz_to_rgb(pixels):
    components = unstack(pixels)
    x, y, z = clip(components[:3], 0, 1)
    xyz = dot_t(stack(x, y, z), rgb_from_xyz)

    rgb = conditional(xyz, lambda t: t > 0.0031308,
                      lambda t: 1.055 * (t ** (1. / 2.4)) - 0.055,
                      lambda t: t * 12.92)

    return concat(clip(rgb, 0, 1), components[3:])


def xyz_to_lab(pixels):
    # see http://www.brucelindbloom.com/Eqn_XYZ_to_Lab.html
    components = unstack(pixels)
    xyz = components[:3]

    xyz = array([u / v for u, v in zip(xyz, _ref_white.xyz)])
    xyz = conditional(xyz, lambda c: c > 0.008856,
                      lambda c: c ** 0.33333333,
                      lambda c: (903.3 * c + 16.) / 116.)
    x, y, z = xyz

    # MMA scales by 1/100
    return stack((1.16 * y) - 0.16, 5. * (x - y), 2. * (y - z), *components[3:])


def xyz_to_luv(pixels):
    # see http://www.brucelindbloom.com/Eqn_XYZ_to_Luv.html
    # and https://en.wikipedia.org/wiki/CIELUV

    components = unstack(pixels)
    xyz = clip(components[:3], 0, 1)

    x_orig, y_orig, z_orig = xyz
    y = y_orig / _ref_white.xyz[1]

    lum = conditional(y, lambda t: t > 0.008856,
                      lambda t: 116. * (t ** (1. / 3.)) - 16,
                      lambda t: 903.3 * t)

    q_0 = x_orig + 15. * y_orig + 3. * z_orig
    u_0 = 4. * x_orig / q_0
    v_0 = 9. * y_orig / q_0

    lum /= 100.0  # MMA specific
    u = 13. * lum * (u_0 - _ref_white.u_r)
    v = 13. * lum * (v_0 - _ref_white.v_r)

    return stack(lum, u, v, *components[3:])


def luv_to_xyz(pixels):
    components = unstack(pixels)
    cie_l, cie_u, cie_v = components[:3]

    # see http://www.easyrgb.com/index.php?X=MATH&H=17#text17
    u = cie_u / (13. * cie_l) + _ref_white.u_r
    v = cie_v / (13. * cie_l) + _ref_white.v_r

    cie_l_100 = cie_l * 100.0  # MMA specific
    y = conditional((cie_l_100 + 16.) / 116., lambda t: t > 0.2068930,  # 0.008856 ** (1/3)
                    lambda t: t ** 3,
                    lambda t: (t - 16. / 116.) / 7.787)

    x = -(9 * y * u) / ((u - 4) * v - u * v)
    z = (9 * y - (15 * v * y) - (v * x)) / (3 * v)

    x, y, z = [compose(cie_l < 0.078, lambda s: s(constant(0, a)),
                       cie_l >= 0.078, lambda s: s(a)) for a in (x, y, z)]
    return clip(stack(x, y, z, *components[3:]), 0, 1)


def lch_to_lab(pixels):
    components = unstack(pixels)
    l, c, h = components[:3]
    h *= 2 * pi  # MMA specific
    return stack(l, c * cos(h), c * sin(h), *components[3:])


def lab_to_lch(pixels):
    components = unstack(pixels)
    l, a, b = components[:3]
    h = conditional(arctan2(b, a), lambda t: t < 0.,
                    lambda t: t + 2. * pi, lambda t: t)
    h /= 2. * pi  # MMA specific
    return stack(l, sqrt(a * a + b * b), h, *components[3:])


def lab_to_xyz(pixels):
    components = unstack(pixels)
    l, a, b = components[:3]

    # see http://www.easyrgb.com/index.php?X=MATH&H=08#text8
    f_y = (l * 100. + 16.) / 116.

    xyz = stack(a / 5. + f_y, f_y, f_y - b / 2.)

    xyz = conditional(xyz, lambda t: t > 0.2068930,  # 0.008856 ** (1/3)
                      lambda t: t ** 3,
                      lambda t: (t - 16. / 116.) / 7.787)

    x, y, z = unstack(xyz)
    x, y, z = [u * v for u, v in zip((x, y, z), _ref_white.xyz)]

    return clip(stack(x, y, z, *components[3:]), 0, 1)


_flows = {  # see http://www.brucelindbloom.com/Math.html
    'XYZ>LCH': ('XYZ', 'LAB', 'LCH'),
    'LAB>LUV': ('LAB', 'XYZ', 'LUV'),
    'LAB>RGB': ('LAB', 'XYZ', 'RGB'),
    'LCH>XYZ': ('LCH', 'LAB', 'XYZ'),
    'LCH>LUV': ('LCH', 'LAB', 'XYZ', 'LUV'),
    'LCH>RGB': ('LCH', 'LAB', 'XYZ', 'RGB'),
    'LUV>LAB': ('LUV', 'XYZ', 'LAB'),
    'LUV>LCH': ('LUV', 'XYZ', 'LAB', 'LCH'),
    'LUV>RGB': ('LUV', 'XYZ', 'RGB'),
    'RGB>LAB': ('RGB', 'XYZ', 'LAB'),
    'RGB>LCH': ('RGB', 'XYZ', 'LAB', 'LCH'),
    'RGB>LUV': ('RGB', 'XYZ', 'LUV'),
}


_rgb_flows = set(['Grayscale', 'CMYK', 'HSB'])


def _flow(src, dst):
    if (src in _rgb_flows and dst != 'RGB') or (dst in _rgb_flows and src != 'RGB'):
        return list(chain(_flow(src, 'RGB'), _flow('RGB', dst)))
    else:
        r = _flows.get('%s>%s' % (src, dst))
        return list(r) if r else [src, dst]


conversions = {
    'Grayscale>RGB': grayscale_to_rgb,
    'RGB>Grayscale': rgb_to_grayscale,
    'CMYK>RGB': cmyk_to_rgb,
    'RGB>CMYK': rgb_to_cmyk,
    'RGB>HSB': rgb_to_hsb,
    'HSB>RGB': hsb_to_rgb,
    'XYZ>LAB': xyz_to_lab,
    'XYZ>LUV': xyz_to_luv,
    'XYZ>RGB': xyz_to_rgb,
    'LAB>XYZ': lab_to_xyz,
    'LAB>LCH': lab_to_lch,
    'LCH>LAB': lch_to_lab,
    'LUV>XYZ': luv_to_xyz,
    'RGB>XYZ': rgb_to_xyz,
}


def convert(components, src, dst):
    if src == dst:
        return components

    flows = _flow(src, dst)
    for s, d in (flows[i:i + 2] for i in range(len(flows) - 1)):
        if s == d:
            continue
        func = conversions.get('%s>%s' % (s, d))
        if not func:
            return None
        components = vectorized(func, components, 1)

    return components
