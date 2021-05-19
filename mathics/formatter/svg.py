# -*- coding: utf-8 -*-
import json

"""
Format a Mathics object as an SVG string
"""

from mathics.builtin.drawing.graphics3d import Graphics3DElements
from mathics.builtin.graphics import (
    ArrowBox,
    BezierCurveBox,
    FilledCurveBox,
    GraphicsBox,
    GraphicsElements,
    InsetBox,
    LineBox,
    PointBox,
    PointSize,
    PolygonBox,
    RectangleBox,
    _RoundBox,
    _svg_bezier,
)

from mathics.core.formatter import lookup_method, add_conversion_fn


class _SVGTransform:
    def __init__(self):
        self.transforms = []

    def matrix(self, a, b, c, d, e, f):
        # a c e
        # b d f
        # 0 0 1
        self.transforms.append("matrix(%f, %f, %f, %f, %f, %f)" % (a, b, c, d, e, f))

    def translate(self, x, y):
        self.transforms.append("translate(%f, %f)" % (x, y))

    def scale(self, x, y):
        self.transforms.append("scale(%f, %f)" % (x, y))

    def rotate(self, x):
        self.transforms.append("rotate(%f)" % x)

    def apply(self, svg):
        return '<g transform="%s">%s</g>' % (" ".join(self.transforms), svg)


def create_css(
    edge_color=None, face_color=None, stroke_width=None, font_color=None, opacity=1.0
) -> str:
    """
    Return a string suitable for CSS inclusion setting the various parameters passed.
    """
    css = []
    if edge_color is not None:
        color, stroke_opacity = edge_color.to_css()
        css.append("stroke: %s" % color)
        css.append("stroke-opacity: %s" % stroke_opacity)
    else:
        css.append("stroke: none")
    if stroke_width is not None:
        css.append("stroke-width: %fpx" % stroke_width)
    if face_color is not None:
        color, fill_opacity = face_color.to_css()
        css.append("fill: %s" % color)
        css.append("fill-opacity: %s" % fill_opacity)
    else:
        css.append("fill: none")
    if font_color is not None:
        color, _ = font_color.to_css()
        css.append("color: %s" % color)
    css.append("opacity: %s" % opacity)
    return "; ".join(css)


def arrowbox(self, offset=None):
    width = self.style.get_line_width(face_element=False)
    style = create_css(edge_color=self.edge_color, stroke_width=width)
    polyline = self.curve.make_draw_svg(style)

    arrow_style = create_css(face_color=self.edge_color, stroke_width=width)

    def polygon(points):
        yield '<polygon points="'
        yield " ".join("%f,%f" % xy for xy in points)
        yield '" style="%s" />' % arrow_style

    extent = self.graphics.view_width or 0
    default_arrow = self._default_arrow(polygon)
    custom_arrow = self._custom_arrow("svg", _SVGTransform)
    return "".join(self._draw(polyline, default_arrow, custom_arrow, extent))


add_conversion_fn(ArrowBox)


def beziercurvebox(self, offset=None):
    line_width = self.style.get_line_width(face_element=False)
    style = create_css(edge_color=self.edge_color, stroke_width=line_width)

    svg = ""
    for line in self.lines:
        s = " ".join(_svg_bezier((self.spline_degree, [xy.pos() for xy in line])))
        svg += '<path d="%s" style="%s"/>' % (s, style)
    # print("XXX bezier", svg)
    return svg


add_conversion_fn(BezierCurveBox)


def filledcurvebox(self, offset=None):
    line_width = self.style.get_line_width(face_element=False)
    style = create_css(
        edge_color=self.edge_color, face_color=self.face_color, stroke_width=line_width
    )

    def components():
        for component in self.components:
            transformed = [(k, [xy.pos() for xy in p]) for k, p in component]
            yield " ".join(_svg_bezier(*transformed)) + " Z"

    # print("XXX FilledcurveBox", components)
    return '<path d="%s" style="%s" fill-rule="evenodd"/>' % (
        " ".join(components()),
        style,
    )


add_conversion_fn(FilledCurveBox)

def graphicsbox(self, leaves=None, **options) -> str:
        if not leaves:
            leaves = self._leaves

        data = options.get("data", None)
        if data:
            elements, xmin, xmax, ymin, ymax, w, h, width, height = data
        else:
            elements, calc_dimensions = self._prepare_elements(
                leaves, options, neg_y=True
            )
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
        format_fn = lookup_method(element, "svg")
        if format_fn is None:
            result.append(element.to_svg(offset))
        else:
            result.append(format_fn(element, offset))

    return "\n".join(result)


add_conversion_fn(GraphicsElements)
graphics3delements = graphicselements

add_conversion_fn(Graphics3DElements)


def insetbox(self, offset=None):
    x, y = self.pos.pos()
    if offset:
        x = x + offset[0]
        y = y + offset[1]

    if hasattr(self.content, "to_svg"):
        content = self.content.to_svg(noheader=True, offset=(x, y))
        svg = "\n" + content + "\n"
    else:
        css_style = create_css(
            font_color=self.color,
            edge_color=self.color,
            face_color=self.color,
            opacity=self.opacity,
        )
        text_pos_opts = f'x="{x}" y="{y}" ox="{self.opos[0]}" oy="{self.opos[1]}"'
        # FIXME: don't hard code text_style_opts, but allow these to be adjustable.
        text_style_opts = "text-anchor:middle; dominant-baseline:middle;"
        content = self.content.boxes_to_text(evaluation=self.graphics.evaluation)
        svg = f'<text {text_pos_opts} style="{text_style_opts} {css_style}">{content}</text>'

    # content = self.content.boxes_to_mathml(evaluation=self.graphics.evaluation)
    # style = create_css(font_color=self.color)
    # svg = (
    #    '<foreignObject x="%f" y="%f" ox="%f" oy="%f" style="%s">'
    #    "<math>%s</math></foreignObject>")

    return svg


add_conversion_fn(InsetBox)


def linebox(self, offset=None):
    line_width = self.style.get_line_width(face_element=False)
    style = create_css(edge_color=self.edge_color, stroke_width=line_width)
    svg = ""
    for line in self.lines:
        svg += '<polyline points="%s" style="%s" />' % (
            " ".join(["%f,%f" % coords.pos() for coords in line]),
            style,
        )
    # print("XXX linebox", svg)
    return svg


add_conversion_fn(LineBox)


def pointbox(self, offset=None):
    point_size, _ = self.style.get_style(PointSize, face_element=False)
    if point_size is None:
        point_size = PointSize(self.graphics, value=0.005)
    size = point_size.get_size()

    style = create_css(
        edge_color=self.edge_color, stroke_width=0, face_color=self.face_color
    )
    svg = ""
    for line in self.lines:
        for coords in line:
            svg += '<circle cx="%f" cy="%f" r="%f" style="%s" />' % (
                coords.pos()[0],
                coords.pos()[1],
                size,
                style,
            )
    # print("XXX PointBox", svg)
    return svg


add_conversion_fn(PointBox)


def polygonbox(self, offset=None):
    line_width = self.style.get_line_width(face_element=True)
    if self.vertex_colors is None:
        face_color = self.face_color
    else:
        face_color = None
    style = create_css(
        edge_color=self.edge_color, face_color=face_color, stroke_width=line_width
    )
    svg = ""
    if self.vertex_colors is not None:
        mesh = []
        for index, line in enumerate(self.lines):
            data = [
                [coords.pos(), color.to_js()]
                for coords, color in zip(line, self.vertex_colors[index])
            ]
            mesh.append(data)
        svg += '<meshgradient data="%s" />' % json.dumps(mesh)
    for line in self.lines:
        svg += '<polygon points="%s" style="%s" />' % (
            " ".join("%f,%f" % coords.pos() for coords in line),
            style,
        )
    # print("XXX PolygonBox", svg)
    return svg


add_conversion_fn(PolygonBox)


def rectanglebox(self, offset=None):
    line_width = self.style.get_line_width(face_element=True)
    x1, y1 = self.p1.pos()
    x2, y2 = self.p2.pos()
    xmin = min(x1, x2)
    ymin = min(y1, y2)
    w = max(x1, x2) - xmin
    h = max(y1, y2) - ymin
    if offset:
        x1, x2 = x1 + offset[0], x2 + offset[0]
        y1, y2 = y1 + offset[1], y2 + offset[1]
    style = create_css(self.edge_color, self.face_color, line_width)
    return '<rect x="%f" y="%f" width="%f" height="%f" style="%s" />' % (
        xmin,
        ymin,
        w,
        h,
        style,
    )
    "\n".join(element.to_svg() for element in self.elements)


add_conversion_fn(RectangleBox)


def _roundbox(self, offset=None):
    x, y = self.c.pos()
    rx, ry = self.r.pos()
    rx -= x
    ry = y - ry
    line_width = self.style.get_line_width(face_element=self.face_element)
    style = create_css(self.edge_color, self.face_color, stroke_width=line_width)
    return '<ellipse cx="%f" cy="%f" rx="%f" ry="%f" style="%s" />' % (
        x,
        y,
        rx,
        ry,
        style,
    )


add_conversion_fn(_RoundBox)
