# -*- coding: utf-8 -*-

""" Asymptote-related functions"""

from itertools import chain


def asy_bezier(*segments):
    # see http://asymptote.sourceforge.net/doc/Bezier-curves.html#Bezier-curves

    while segments and not segments[0][1]:
        segments = segments[1:]

    if not segments:
        return

    def cubic(p0, p1, p2, p3):
        return "..controls(%.5g,%.5g) and (%.5g,%.5g)..(%.5g,%.5g)" % tuple(
            list(chain(p1, p2, p3))
        )

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

        t = 2.0 / 3.0
        cp0 = qp0
        cp1 = (qp0x + t * (qp1x - qp0x), qp0y + t * (qp1y - qp0y))
        cp2 = (qp2x + t * (qp1x - qp2x), qp2y + t * (qp1y - qp2y))
        cp3 = qp2

        return cubic(cp0, cp1, cp2, cp3)

    def linear(p0, p1):
        return "--(%.5g,%.5g)" % p1

    forms = (linear, quadratric, cubic)

    def path(max_degree, p):
        max_degree = min(max_degree, len(forms))
        while p:
            n = min(max_degree, len(p) - 1)  # 1, 2, or 3
            if n < 1:
                break
            yield forms[n - 1](*p[: n + 1])
            p = p[n:]

    k, p = segments[0]
    yield "(%.5g,%.5g)" % p[0]

    connect = []
    for k, p in segments:
        for s in path(k, list(chain(connect, p))):
            yield s
        connect = p[-1:]


def asy_color(self):
    """Return an asymptote rgba string fragment for object's RGB or RGBA
    value and it opacity (alpha) value"""
    rgba = self.to_rgba()
    alpha = rgba[3] if len(rgba) > 3 else 1.0
    return (
        fr"rgb({asy_number(rgba[0])}, {asy_number(rgba[1])}, {asy_number(rgba[2])})",
        alpha,
    )


def asy_create_pens(
    edge_color=None,
    face_color=None,
    stroke_width=None,
    is_face_element=False,
    dotfactor=None,
) -> str:
    """
    Return an asymptote string fragment that creates a drawing pen.
    """
    result = []
    if face_color is not None:
        brush, opacity = asy_color(face_color)
        if opacity != 1:
            brush += f"+opacity({asy_number(opacity)})"
        if dotfactor is not None:
            brush += f"+{dotfactor}"
        result.append(brush)
    elif is_face_element:
        result.append("nullpen")
    if edge_color is not None:
        pen, opacity = asy_color(edge_color)
        if opacity != 1:
            pen += f"+opacity({asy_number(opacity)})"
        if stroke_width is not None:
            pen += f"+linewidth({asy_number(stroke_width)})"
        result.append(pen)
    elif is_face_element:
        result.append("nullpen")

    pen_str = ", ".join(result)
    # print(pen_str)
    return pen_str


def asy_number(value) -> str:
    """Format an asymptote number"""
    return "%.5g" % value
