# -*- coding: utf-8 -*-
"""
Format a Mathics object as an SVG string
"""

from mathics.builtin.box.graphics import (
    _ArcBox,
    ArrowBox,
    BezierCurveBox,
    FilledCurveBox,
    GraphicsBox,
    InsetBox,
    LineBox,
    PointBox,
    PolygonBox,
    RectangleBox,
    _RoundBox,
)

from mathics.builtin.drawing.graphics3d import Graphics3DElements
from mathics.builtin.graphics import (
    DEFAULT_POINT_FACTOR,
    GraphicsElements,
    PointSize,
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
        self.transforms.append(f"matrix({a:f}, {b:f}, {c:f}, {d:f}, {e:f}, {e:f})")

    def translate(self, x, y):
        self.transforms.append(f"translate({x:f}, {y:f})")

    def scale(self, x, y):
        self.transforms.append(f"scale({x:f}, {y:f})")

    def rotate(self, x):
        self.transforms.append(f"rotate({x:f})")

    def apply(self, svg):
        return f"""<g transform="{' '.join(self.transforms)}">{svg}</g>"""


def create_css(
    edge_color=None, face_color=None, stroke_width=None, font_color=None, opacity=1.0
) -> str:
    """
    Return a string suitable for CSS inclusion setting the various parameters passed.
    """
    css = []
    if edge_color is not None:
        color, stroke_opacity = edge_color.to_css()
        css.append(f"stroke: {color}")
        css.append(f"stroke-opacity: {stroke_opacity}")
    else:
        css.append("stroke: none")
    if stroke_width is not None:
        css.append(f"stroke-width: {stroke_width:f}px")
    if face_color is not None:
        color, fill_opacity = face_color.to_css()
        css.append(f"fill: {color}")
        css.append(f"fill-opacity: {fill_opacity}")
    else:
        css.append("fill: none")
    if font_color is not None:
        color, _ = font_color.to_css()
        css.append(f"color: {color}")
    css.append(f"opacity: {opacity}")
    return "; ".join(css)


def arcbox(self, **options) -> str:
    """
    SVG formatting for arc of a circle.
    """
    if self.arc is None:
        # We have a doughnut graph and this is the inner blank hole of that.
        # It is an empty circle
        return _roundbox(self, **options)

    x, y, rx, ry, sx, sy, ex, ey, large_arc = self._arc_params()

    def path(closed):
        if closed:
            yield "M %f,%f" % (x, y)
            yield "L %f,%f" % (sx, sy)
        else:
            yield "M %f,%f" % (sx, sy)

        yield "A %f,%f,0,%d,0,%f,%f" % (rx, ry, large_arc, ex, ey)

        if closed:
            yield "Z"

    l = self.style.get_line_width(face_element=self.face_element)
    style = create_css(self.edge_color, self.face_color, stroke_width=l)
    svg = '<path d="%s" style="%s" />' % (" ".join(path(self.face_element)), style)
    # print("_Arcbox: ", svg)
    return svg


add_conversion_fn(_ArcBox, arcbox)


def arrow_box(self, **options) -> str:
    width = self.style.get_line_width(face_element=False)
    style = create_css(edge_color=self.edge_color, stroke_width=width)
    polyline = self.curve.make_draw_svg(style)

    arrow_style = create_css(face_color=self.edge_color, stroke_width=width)

    def polygon(points):
        yield '<polygon points="'
        yield " ".join("%f,%f" % xy for xy in points)
        yield f'" style="{arrow_style}" />'

    extent = self.graphics.view_width or 0
    default_arrow = self._default_arrow(polygon)
    custom_arrow = self._custom_arrow("svg", _SVGTransform)
    svg = "\n".join(self._draw(polyline, default_arrow, custom_arrow, extent))
    # print("ArrowBox: ", svg)
    return svg


add_conversion_fn(ArrowBox, arrow_box)


def beziercurvebox(self, **options) -> str:
    line_width = self.style.get_line_width(face_element=False)
    style = create_css(edge_color=self.edge_color, stroke_width=line_width)

    svg = "<!--BezierCurveBox-->\n"
    for line in self.lines:
        s = "\n".join(_svg_bezier((self.spline_degree, [xy.pos() for xy in line])))
        svg += f'<path d="{s}" style="{style}"/>'
    # print("BezierCurveBox: ", svg)
    return svg


add_conversion_fn(BezierCurveBox)


def density_plot_box(self, **options):
    """
    SVG formatter for DensityPlotBox.
    """
    # A DensityPlot is a just a list of triangles each of which have its density color.
    #
    # So this code is similar to PolygonBox.
    #
    # However note that many of the PolygonBox features are a little
    # different here. First, everything is a triangle, so there the
    # notion of odd/even crossing with holes doesn't apply.  Second
    # since each each point/triangle could be a different color, we'll
    # have to write out a separate polygon for each.

    # There is a lot of fanciness one could do here, like sort points into those that have
    # the same color and put all of those into a single polygonbox.

    # Here is an even more elaborate scheme which I won't use, but
    # since it is a cute idea, it is worthy of comment space...  Put
    # two triangles together to get a parallelogram. Compute the
    # midpoint color in the enter and along all four sides. Then use
    # two overlayed rectangular gradients each at opacity 0.5
    # to go from the center to each of the (square) sides.

    svg_data = ["<--DensityPlot-->"]
    for index, triangle_coords in enumerate(self.lines):
        triangle = [coords.pos() for coords in triangle_coords]
        colors = [rgb.to_js() for rgb in self.vertex_colors[index]]
        r = (colors[0][0] + colors[1][0] + colors[2][0]) / 3.0
        g = (colors[0][1] + colors[1][1] + colors[2][1]) / 3.0
        b = (colors[0][2] + colors[1][2] + colors[2][1]) / 3.0
        mid_color = r"rgb(%f%%, %f%%, %f%%)" % (r * 100, g * 100, b * 100)

        points = " ".join("%f,%f" % (point[0], point[1]) for point in triangle)
        svg_data.append(f'<polygon points="{points}" style="fill: {mid_color}" />')

    svg = "\n".join(svg_data)
    # print("DensityPlot: ", svg)
    return svg


# No add_conversion_fn since this is a hacken-on polygonbox


def filled_curve_box(self, **options):
    line_width = self.style.get_line_width(face_element=False)
    style = create_css(
        edge_color=self.edge_color, face_color=self.face_color, stroke_width=line_width
    )

    def components():
        for component in self.components:
            transformed = [(k, [xy.pos() for xy in p]) for k, p in component]
            yield " ".join(_svg_bezier(*transformed)) + " Z"

    # print("FilledCurveBox: ", components)
    return '<path d="%s" style="%s" fill-rule="evenodd"/>' % (
        " ".join(components()),
        style,
    )


add_conversion_fn(FilledCurveBox, filled_curve_box)


def graphics_box(self, leaves=None, **options) -> str:

    if not leaves:
        leaves = self._leaves

    data = options.get("data", None)
    if data:
        (
            elements,
            xmin,
            xmax,
            ymin,
            ymax,
            self.boxwidth,
            self.boxheight,
            width,
            height,
        ) = data
    else:
        elements, calc_dimensions = self._prepare_elements(leaves, options, neg_y=True)
        (
            xmin,
            xmax,
            ymin,
            ymax,
            self.boxwidth,
            self.boxheight,
            width,
            height,
        ) = calc_dimensions()

    elements.view_width = self.boxwidth

    format_fn = lookup_method(elements, "svg")
    if format_fn is not None:
        svg_body = format_fn(elements, **options)
    else:
        svg_body = elements.to_svg(**options)

    self.boxwidth = options.get("width", self.boxwidth)
    self.boxheight = options.get("height", self.boxheight)

    if self.background_color is not None:
        # Wrap svg_elements in a rectangle
        svg_body = '<rect x="%f" y="%f" width="%f" height="%f" style="fill:%s"/>%s' % (
            xmin,
            ymin,
            self.boxwidth,
            self.boxheight,
            self.background_color.to_css()[0],
            svg_body,
        )

    if options.get("noheader", False):
        return svg_body
    svg_main = f"""<svg width="{self.boxwidth}px" height="{self.boxheight}px" xmlns:svg="http://www.w3.org/2000/svg"
                xmlns="http://www.w3.org/2000/svg"
                version="1.1"
                viewBox="%s">
                %s
</svg>
""" % (
        " ".join("%f" % t for t in (xmin, ymin, self.boxwidth, self.boxheight)),
        svg_body,
    )
    # print("svg_main", svg_main)
    return svg_main  # , width, height


add_conversion_fn(GraphicsBox, graphics_box)


def graphics_elements(self, **options) -> str:
    """
    SVG Formatting for a list of graphics elements.
    """
    result = ["<!--GraphicsElements-->"]
    for element in self.elements:
        format_fn = lookup_method(element, "svg")
        if format_fn is None:
            result.append(element.to_svg(**options))
        else:
            result.append(format_fn(element, **options))

    svg = "\n".join(result)
    # print("GraphicsElements: ", svg)
    return svg


add_conversion_fn(GraphicsElements, graphics_elements)
graphics3delements = graphics_elements

add_conversion_fn(Graphics3DElements)


def inset_box(self, **options) -> str:
    """
    SVG Formatting for boxing an Inset in a graphic.
    """
    x, y = self.pos.pos()
    offset = options.get("offset", None)
    if offset is not None:
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
        text_style_opts = "text-anchor:end; dominant-baseline:hanging;"
        content = self.content.boxes_to_text(evaluation=self.graphics.evaluation)
        font_size = f'''font-size="{options.get("point_size", "10px")}"'''
        svg = f'<text {text_pos_opts} {font_size} style="{text_style_opts} {css_style}">{content}</text>'

    # content = self.content.boxes_to_mathml(evaluation=self.graphics.evaluation)
    # style = create_css(font_color=self.color)
    # svg = (
    #    '<foreignObject x="%f" y="%f" ox="%f" oy="%f" style="%s">'
    #    "<math>%s</math></foreignObject>")
    # print(svg)

    return svg


add_conversion_fn(InsetBox, inset_box)


def line_box(self, **options) -> str:
    line_width = self.style.get_line_width(face_element=False)
    style = create_css(edge_color=self.edge_color, stroke_width=line_width)
    svg = "<!--LineBox-->\n"
    for line in self.lines:
        svg += '<polyline points="%s" style="%s" />' % (
            " ".join(["%f,%f" % coords.pos() for coords in line]),
            style,
        )
    # print("LineBox", svg)
    return svg


add_conversion_fn(LineBox, line_box)


def pointbox(self, **options) -> str:
    point_size, _ = self.style.get_style(PointSize, face_element=False)
    if point_size is None:
        point_size = PointSize(self.graphics, value=DEFAULT_POINT_FACTOR)
    size = point_size.get_absolute_size()

    style = create_css(
        edge_color=self.edge_color, stroke_width=0, face_color=self.face_color
    )
    svg = "<!--PointBox-->"
    for line in self.lines:
        for coords in line:
            svg += f"""
  <circle cx="{coords.pos()[0]:f}" cy="{coords.pos()[1]:f}"
          r="{size:f}" style="{style}"/>"""
    # print("PointBox", svg)
    return svg


add_conversion_fn(PointBox)


def polygonbox(self, **options):
    """
    SVG formatter for PolygonBox
    """
    line_width = self.style.get_line_width(face_element=True)

    # Hack alert. Currently we encode density plots as a polygon box where
    # each polygon is a triangle with a color. We know we have this case because
    # self.vertex_colors is not empty here.
    if self.vertex_colors:
        return density_plot_box(self, **options)

    style = create_css(
        edge_color=self.edge_color, face_color=self.face_color, stroke_width=line_width
    )

    svg = "<!--PolygonBox-->\n"
    # WL says this about 2D polygons:
    #   A point is an element of the polygon if a ray from the point in any direction in the plane crosses the boundary line segments an odd number of times.
    #
    # In SVG, this is called the "evenodd" fill rule.
    # Perhaps one day we will find it useful to have other fill_rules specified as an option.
    fill_rule = "evenodd"

    for line in self.lines:
        svg += f"""
  <polygon points="{" ".join("%f,%f" % coords.pos() for coords in line)}"
           fill-rule="{fill_rule}"
           style="{style}" />\n"""
    # print("PolygonBox: ", svg)
    return svg


add_conversion_fn(PolygonBox)


def rectanglebox(self, **options):
    line_width = self.style.get_line_width(face_element=True)
    x1, y1 = self.p1.pos()
    x2, y2 = self.p2.pos()
    xmin = min(x1, x2)
    ymin = min(y1, y2)
    w = max(x1, x2) - xmin
    h = max(y1, y2) - ymin
    offset = options.get("offset", None)
    if offset is not None:
        x1, x2 = x1 + offset[0], x2 + offset[0]
        y1, y2 = y1 + offset[1], y2 + offset[1]
    style = create_css(self.edge_color, self.face_color, line_width)
    svg = '<rect x="%f" y="%f" width="%f" height="%f" style="%s" />' % (
        xmin,
        ymin,
        w,
        h,
        style,
    )
    # print("RectangleBox", svg)
    return svg


add_conversion_fn(RectangleBox)


def _roundbox(self, **options):
    x, y = self.c.pos()
    rx, ry = self.r.pos()
    rx -= x
    ry = y - ry
    line_width = self.style.get_line_width(face_element=self.face_element)
    style = create_css(self.edge_color, self.face_color, stroke_width=line_width)
    svg = '<ellipse cx="%f" cy="%f" rx="%f" ry="%f" style="%s" />' % (
        x,
        y,
        rx,
        ry,
        style,
    )
    # print("_RoundBox: ", svg)
    return svg


add_conversion_fn(_RoundBox)
