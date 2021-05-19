# -*- coding: utf-8 -*-

"""
Format a Mathics object as an Aymptote string
"""

from itertools import chain

from mathics.builtin.graphics import (
    ArrowBox,
    _Color,
    BezierCurveBox,
    FilledCurveBox,
    GraphicsBox,
    GraphicsElements,
    InsetBox,
    LineBox,
    PointBox,
    PolygonBox,
    RectangleBox,
    _RoundBox,
)

from mathics.core.formatter import lookup_method, add_conversion_fn


class _ASYTransform:
    _template = """
    add(%s * (new picture() {
        picture saved = currentpicture;
        picture transformed = new picture;
        currentpicture = transformed;
        %s
        currentpicture = saved;
        return transformed;
    })());
    """

    def __init__(self):
        self.transforms = []

    def matrix(self, a, b, c, d, e, f):
        # a c e
        # b d f
        # 0 0 1
        # see http://asymptote.sourceforge.net/doc/Transforms.html#Transforms
        self.transforms.append("(%f, %f, %f, %f, %f, %f)" % (e, f, a, c, b, d))

    def translate(self, x, y):
        self.transforms.append("shift(%f, %f)" % (x, y))

    def scale(self, x, y):
        self.transforms.append("scale(%f, %f)" % (x, y))

    def rotate(self, x):
        self.transforms.append("rotate(%f)" % x)

    def apply(self, asy):
        return self._template % (" * ".join(self.transforms), asy)


def _asy_bezier(*segments):
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


def asy_number(value):
    return "%.5g" % value


def create_pens(
    edge_color=None, face_color=None, stroke_width=None, is_face_element=False
):
    result = []
    if face_color is not None:
        brush, opacity = _color(face_color)
        if opacity != 1:
            brush += "+opacity(%s)" % asy_number(opacity)
        result.append(brush)
    elif is_face_element:
        result.append("nullpen")
    if edge_color is not None:
        pen, opacity = _color(edge_color)
        if opacity != 1:
            pen += "+opacity(%s)" % asy_number(opacity)
        if stroke_width is not None:
            pen += "+linewidth(%s)" % asy_number(stroke_width)
        result.append(pen)
    elif is_face_element:
        result.append("nullpen")
    return ", ".join(result)


def _color(self):
    rgba = self.to_rgba()
    alpha = rgba[3] if len(rgba) > 3 else 1.0
    return (
        r"rgb(%s, %s, %s)"
        % (asy_number(rgba[0]), asy_number(rgba[1]), asy_number(rgba[2])),
        alpha,
    )


add_conversion_fn(_Color)


def arrowbox(self):
    width = self.style.get_line_width(face_element=False)
    pen = create_pens(edge_color=self.edge_color, stroke_width=width)
    polyline = self.curve.make_draw_asy(pen)

    arrow_pen = create_pens(face_color=self.edge_color, stroke_width=width)

    def polygon(points):
        yield "filldraw("
        yield "--".join(["(%.5g,%5g)" % xy for xy in points])
        yield "--cycle, % s);" % arrow_pen

    extent = self.graphics.view_width or 0
    default_arrow = self._default_arrow(polygon)
    custom_arrow = self._custom_arrow("asy", _ASYTransform)
    return "".join(self._draw(polyline, default_arrow, custom_arrow, extent))


add_conversion_fn(ArrowBox)


def beziercurvebox(self):
    line_width = self.style.get_line_width(face_element=False)
    pen = create_pens(edge_color=self.edge_color, stroke_width=line_width)

    asy = ""
    for line in self.lines:
        for path in _asy_bezier((self.spline_degree, [xy.pos() for xy in line])):
            if path[:2] == "..":
                path = "(0.,0.)" + path
            asy += "draw(%s, %s);" % (path, pen)
    return asy
    if self.arc is None:
        return super(_ArcBox, self).to_asy()

    x, y, rx, ry, sx, sy, ex, ey, large_arc = self._arc_params()


add_conversion_fn(BezierCurveBox)


def filledcurvebox(self):
    line_width = self.style.get_line_width(face_element=False)
    pen = create_pens(edge_color=self.edge_color, stroke_width=line_width)

    if not pen:
        pen = "currentpen"

    def components():
        for component in self.components:
            transformed = [(k, [xy.pos() for xy in p]) for k, p in component]
            yield "fill(%s--cycle, %s);" % ("".join(_asy_bezier(*transformed)), pen)

    return "".join(components())


add_conversion_fn(FilledCurveBox)

# FIXME figure out how we can add this.
def graphicsbox(self, leaves=None, **options) -> str:
    if not leaves:
        leaves = self._leaves

    data = options.get("data", None)
    if data:
        elements, xmin, xmax, ymin, ymax, w, h, width, height = data
    else:
        elements, calc_dimensions = self._prepare_elements(leaves, options, neg_y=True)
        xmin, xmax, ymin, ymax, w, h, width, height = calc_dimensions()

    elements.view_width = w

    format_fn = lookup_method(elements, "svg")
    if format_fn is not None:
        svg_body = format_fn(elements, offset=options.get("offset", None))
    else:
        svg_body = elements.to_svg(offset=options.get("offset", None))

    if self.background_color is not None:
        # Wrap svg_elements in a rectangle
        svg_body = '<rect x="%f" y="%f" width="%f" height="%f" style="fill:%s"/>%s' % (
            xmin,
            ymin,
            w,
            h,
            self.background_color.to_css()[0],
            svg_body,
        )

    xmin -= 1
    ymin -= 1
    w += 2
    h += 2

    if options.get("noheader", False):
        return svg_body
    svg_main = """
            <svg xmlns:svg="http://www.w3.org/2000/svg"
                xmlns="http://www.w3.org/2000/svg"
                version="1.1"
                viewBox="%s">
                %s
            </svg>
        """ % (
        " ".join("%f" % t for t in (xmin, ymin, w, h)),
        svg_body,
    )
    return svg_main  # , width, height


add_conversion_fn(GraphicsBox)


def graphicselements(self, offset=None):
    result = []
    for element in self.elements:
        format_fn = lookup_method(element, "asy")
        if format_fn is None:
            result.append(element.to_asy(offset))
        else:
            result.append(format_fn(element, offset))

    return "\n".join(result)


add_conversion_fn(GraphicsElements)


def insetbox(self):
    x, y = self.pos.pos()
    content = self.content.boxes_to_tex(evaluation=self.graphics.evaluation)
    pen = create_pens(edge_color=self.color)
    asy = 'label("$%s$", (%s,%s), (%s,%s), %s);' % (
        content,
        x,
        y,
        -self.opos[0],
        -self.opos[1],
        pen,
    )
    return asy


add_conversion_fn(InsetBox)


def linebox(self):
    line_width = self.style.get_line_width(face_element=False)
    pen = create_pens(edge_color=self.edge_color, stroke_width=line_width)
    asy = ""
    for line in self.lines:
        path = "--".join(["(%.5g,%5g)" % coords.pos() for coords in line])
        asy += "draw(%s, %s);" % (path, pen)
    return asy


add_conversion_fn(LineBox)


def pointbox(self):
    pen = create_pens(face_color=self.face_color, is_face_element=False)

    asy = ""
    for line in self.lines:
        for coords in line:
            asy += "dot(%s, %s);" % (coords.pos(), pen)

    return asy


add_conversion_fn(PointBox)


def polygonbox(self):
    line_width = self.style.get_line_width(face_element=True)
    if self.vertex_colors is None:
        face_color = self.face_color
    else:
        face_color = None
    pens = create_pens(
        edge_color=self.edge_color,
        face_color=face_color,
        stroke_width=line_width,
        is_face_element=True,
    )
    asy = ""
    if self.vertex_colors is not None:
        paths = []
        colors = []
        edges = []
        for index, line in enumerate(self.lines):
            paths.append(
                "--".join(["(%.5g,%.5g)" % coords.pos() for coords in line]) + "--cycle"
            )

            # ignore opacity
            colors.append(
                ",".join([_color(color)[0] for color in self.vertex_colors[index]])
            )

            edges.append(",".join(["0"] + ["1"] * (len(self.vertex_colors[index]) - 1)))

        asy += "gouraudshade(%s, new pen[] {%s}, new int[] {%s});" % (
            "^^".join(paths),
            ",".join(colors),
            ",".join(edges),
        )
    if pens and pens != "nullpen":
        for line in self.lines:
            path = (
                "--".join(["(%.5g,%.5g)" % coords.pos() for coords in line]) + "--cycle"
            )
            asy += "filldraw(%s, %s);" % (path, pens)
    return asy


add_conversion_fn(PolygonBox)


def rectanglebox(self):
    line_width = self.style.get_line_width(face_element=True)
    x1, y1 = self.p1.pos()
    x2, y2 = self.p2.pos()
    pens = create_pens(self.edge_color, self.face_color, line_width, is_face_element=True)
    x1, x2, y1, y2 = asy_number(x1), asy_number(x2), asy_number(y1), asy_number(y2)
    return "filldraw((%s,%s)--(%s,%s)--(%s,%s)--(%s,%s)--cycle, %s);" % (
        x1,
        y1,
        x2,
        y1,
        x2,
        y2,
        x1,
        y2,
        pens,
    )


add_conversion_fn(RectangleBox)


def _roundbox(self):
    x, y = self.c.pos()
    rx, ry = self.r.pos()
    rx -= x
    ry -= y
    line_width = self.style.get_line_width(face_element=self.face_element)
    pen = create_pens(
        edge_color=self.edge_color,
        face_color=self.face_color,
        stroke_width=line_width,
        is_face_element=self.face_element,
    )
    cmd = "filldraw" if self.face_element else "draw"
    return "%s(ellipse((%s,%s),%s,%s), %s);" % (
        cmd,
        asy_number(x),
        asy_number(y),
        asy_number(rx),
        asy_number(ry),
        pen,
    )


add_conversion_fn(_RoundBox)
