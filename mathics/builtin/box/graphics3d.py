# -*- coding: utf-8 -*-
"""
Boxing Routines for 3D Graphics
"""

import html
import json
import numbers

from mathics.builtin.base import BoxConstructError
from mathics.builtin.box.graphics import (
    GraphicsBox,
    ArrowBox,
    LineBox,
    PointBox,
    PolygonBox,
)

from mathics.builtin.colors.color_directives import _Color, RGBColor
from mathics.builtin.drawing.graphics_internals import GLOBALS3D
from mathics.builtin.drawing.graphics3d import (
    _Graphics3DElement,
    Coords3D,
    Graphics3DElements,
)

from mathics.builtin.drawing.graphics_internals import get_class
from mathics.core.expression import Symbol
from mathics.core.formatter import lookup_method
from mathics.format.asy_fns import asy_create_pens, asy_number


class Graphics3DBox(GraphicsBox):
    """Routines which get called when Boxing (adding formatting and bounding-box information)
    a Graphics3D object.
    """

    def _prepare_elements(self, leaves, options, max_width=None):
        if not leaves:
            raise BoxConstructError

        self.graphics_options = self.get_option_values(leaves[1:], **options)

        background = self.graphics_options["System`Background"]
        if (
            isinstance(background, Symbol)
            and background.get_name() == "System`Automatic"
        ):
            self.background_color = None
        else:
            self.background_color = _Color.create(background)

        evaluation = options["evaluation"]

        base_width, base_height, size_multiplier, size_aspect = self._get_image_size(
            options, self.graphics_options, max_width
        )

        # TODO: Handle ImageScaled[], and Scaled[]
        lighting_option = self.graphics_options["System`Lighting"]
        lighting = lighting_option.to_python()  # can take symbols or strings
        self.lighting = []

        if lighting == "System`Automatic":
            self.lighting = [
                {"type": "Ambient", "color": [0.3, 0.2, 0.4]},
                {
                    "type": "Directional",
                    "color": [0.8, 0.0, 0.0],
                    "position": [2, 0, 2],
                },
                {
                    "type": "Directional",
                    "color": [0.0, 0.8, 0.0],
                    "position": [2, 2, 2],
                },
                {
                    "type": "Directional",
                    "color": [0.0, 0.0, 0.8],
                    "position": [0, 2, 2],
                },
            ]
        elif lighting == "Neutral":  # Lighting->"Neutral"
            self.lighting = [
                {"type": "Ambient", "color": [0.3, 0.3, 0.3]},
                {
                    "type": "Directional",
                    "color": [0.3, 0.3, 0.3],
                    "position": [2, 0, 2],
                },
                {
                    "type": "Directional",
                    "color": [0.3, 0.3, 0.3],
                    "position": [2, 2, 2],
                },
                {
                    "type": "Directional",
                    "color": [0.3, 0.3, 0.3],
                    "position": [0, 2, 2],
                },
            ]
        elif lighting == "System`None":
            pass

        elif isinstance(lighting, list) and all(
            isinstance(light, list) for light in lighting
        ):
            for light in lighting:
                if light[0] in ['"Ambient"', '"Directional"', '"Point"', '"Spot"']:
                    try:
                        head = light[1].get_head_name()
                    except AttributeError:
                        break
                    color = get_class(head)(light[1])
                    if light[0] == '"Ambient"':
                        self.lighting.append(
                            {"type": "Ambient", "color": color.to_rgba()}
                        )
                    elif light[0] == '"Directional"':
                        position = [0, 0, 0]
                        if isinstance(light[2], list):
                            if len(light[2]) == 3:
                                position = light[2]
                            if len(light[2]) == 2 and all(  # noqa
                                isinstance(p, list) and len(p) == 3 for p in light[2]
                            ):
                                position = [
                                    light[2][0][i] - light[2][1][i] for i in range(3)
                                ]
                        self.lighting.append(
                            {
                                "type": "Directional",
                                "color": color.to_rgba(),
                                "position": position,
                            }
                        )
                    elif light[0] == '"Point"':
                        position = [0, 0, 0]
                        if isinstance(light[2], list) and len(light[2]) == 3:
                            position = light[2]
                        self.lighting.append(
                            {
                                "type": "Point",
                                "color": color.to_rgba(),
                                "position": position,
                            }
                        )
                    elif light[0] == '"Spot"':
                        position = [0, 0, 1]
                        target = [0, 0, 0]
                        if isinstance(light[2], list):
                            if len(light[2]) == 2:
                                if (
                                    isinstance(light[2][0], list)
                                    and len(light[2][0]) == 3  # noqa
                                ):
                                    position = light[2][0]
                                if (
                                    isinstance(light[2][1], list)
                                    and len(light[2][1]) == 3  # noqa
                                ):
                                    target = light[2][1]
                            if len(light[2]) == 3:
                                position = light[2]
                        angle = light[3]
                        self.lighting.append(
                            {
                                "type": "Spot",
                                "color": color.to_rgba(),
                                "position": position,
                                "target": target,
                                "angle": angle,
                            }
                        )

        else:
            evaluation.message("Graphics3D", "invlight", lighting_option)

        # ViewPoint Option
        viewpoint_option = self.graphics_options["System`ViewPoint"]
        viewpoint = viewpoint_option.to_python(n_evaluation=evaluation)

        if isinstance(viewpoint, list) and len(viewpoint) == 3:
            if all(isinstance(x, numbers.Real) for x in viewpoint):
                # TODO Infinite coordinates e.g. {0, 0, Infinity}
                pass
        else:
            try:
                viewpoint = {
                    "Above": [0, 0, 2],
                    "Below": [0, 0, -2],
                    "Front": [0, -2, 0],
                    "Back": [0, 2, 0],
                    "Left": [-2, 0, 0],
                    "Right": [2, 0, 0],
                }[viewpoint]
            except KeyError:
                # evaluation.message()
                # TODO
                viewpoint = [1.3, -2.4, 2]

        assert (
            isinstance(viewpoint, list)
            and len(viewpoint) == 3
            and all(isinstance(x, numbers.Real) for x in viewpoint)
        )
        self.viewpoint = viewpoint

        # TODO Aspect Ratio
        # aspect_ratio = self.graphics_options['AspectRatio'].to_python()

        boxratios = self.graphics_options["System`BoxRatios"].to_python()
        if boxratios == "System`Automatic":
            boxratios = ["System`Automatic"] * 3
        else:
            boxratios = boxratios
        if not isinstance(boxratios, list) or len(boxratios) != 3:
            raise BoxConstructError

        plot_range = self.graphics_options["System`PlotRange"].to_python()
        if plot_range == "System`Automatic":
            plot_range = ["System`Automatic"] * 3
        if not isinstance(plot_range, list) or len(plot_range) != 3:
            raise BoxConstructError

        elements = Graphics3DElements(leaves[0], evaluation)

        def calc_dimensions(final_pass=True):
            if "System`Automatic" in plot_range:
                xmin, xmax, ymin, ymax, zmin, zmax = elements.extent()
            else:
                xmin = xmax = ymin = ymax = zmin = zmax = None

            try:
                if plot_range[0] == "System`Automatic":
                    if xmin is None and xmax is None:
                        xmin = 0
                        xmax = 1
                    elif xmin == xmax:
                        xmin -= 1
                        xmax += 1
                elif isinstance(plot_range[0], list) and len(plot_range[0]) == 2:
                    xmin, xmax = list(map(float, plot_range[0]))
                    xmin = elements.translate((xmin, 0, 0))[0]
                    xmax = elements.translate((xmax, 0, 0))[0]
                else:
                    raise BoxConstructError

                if plot_range[1] == "System`Automatic":
                    if ymin is None and ymax is None:
                        ymin = 0
                        ymax = 1
                    elif ymin == ymax:
                        ymin -= 1
                        ymax += 1
                elif isinstance(plot_range[1], list) and len(plot_range[1]) == 2:
                    ymin, ymax = list(map(float, plot_range[1]))
                    ymin = elements.translate((0, ymin, 0))[1]
                    ymax = elements.translate((0, ymax, 0))[1]
                else:
                    raise BoxConstructError

                if plot_range[2] == "System`Automatic":
                    if zmin is None and zmax is None:
                        zmin = 0
                        zmax = 1
                    elif zmin == zmax:
                        zmin -= 1
                        zmax += 1
                elif isinstance(plot_range[1], list) and len(plot_range[1]) == 2:
                    zmin, zmax = list(map(float, plot_range[2]))
                    zmin = elements.translate((0, 0, zmin))[2]
                    zmax = elements.translate((0, 0, zmax))[2]
                else:
                    raise BoxConstructError
            except (ValueError, TypeError):
                raise BoxConstructError

            boxscale = [1.0, 1.0, 1.0]
            if boxratios[0] != "System`Automatic":
                boxscale[0] = boxratios[0] / (xmax - xmin)
            if boxratios[1] != "System`Automatic":
                boxscale[1] = boxratios[1] / (ymax - ymin)
            if boxratios[2] != "System`Automatic":
                boxscale[2] = boxratios[2] / (zmax - zmin)

            if final_pass:
                xmin *= boxscale[0]
                xmax *= boxscale[0]
                ymin *= boxscale[1]
                ymax *= boxscale[1]
                zmin *= boxscale[2]
                zmax *= boxscale[2]

                # Rescale lighting
                for i, light in enumerate(self.lighting):
                    if self.lighting[i]["type"] != "Ambient":
                        self.lighting[i]["position"] = [
                            light["position"][j] * boxscale[j] for j in range(3)
                        ]
                    if self.lighting[i]["type"] == "Spot":
                        self.lighting[i]["target"] = [
                            light["target"][j] * boxscale[j] for j in range(3)
                        ]

                # Rescale viewpoint
                self.viewpoint = [
                    vp * max([xmax - xmin, ymax - ymin, zmax - zmin])
                    for vp in self.viewpoint
                ]

            w = 0 if (xmin is None or xmax is None) else xmax - xmin
            h = 0 if (ymin is None or ymax is None) else ymax - ymin

            return xmin, xmax, ymin, ymax, zmin, zmax, boxscale, w, h

        xmin, xmax, ymin, ymax, zmin, zmax, boxscale, w, h = calc_dimensions(
            final_pass=False
        )

        axes, ticks, ticks_style = self.create_axes(
            elements,
            self.graphics_options,
            xmin,
            xmax,
            ymin,
            ymax,
            zmin,
            zmax,
            boxscale,
        )

        return elements, axes, ticks, ticks_style, calc_dimensions, boxscale

    def boxes_to_js(self, leaves=None, **options):
        """Turn the Graphics3DBox to into a something javascript-ish
        We include enclosing script tagging.
        """
        json_repr = self.boxes_to_json(leaves, **options)
        js = f"<graphics3d data='{json_repr}'/>"
        return js

    def boxes_to_json(self, leaves=None, **options):
        """Turn the Graphics3DBox to into a something JSON like.
        This can be used to embed in something else like MathML or Javascript.

        In contrast to to javascript or MathML, no enclosing tags are included.
        the caller will do that if it is needed.
        """
        if not leaves:
            leaves = self._leaves

        (
            elements,
            axes,
            ticks,
            ticks_style,
            calc_dimensions,
            boxscale,
        ) = self._prepare_elements(leaves, options)

        js_ticks_style = [s.to_js() for s in ticks_style]

        elements._apply_boxscaling(boxscale)

        xmin, xmax, ymin, ymax, zmin, zmax, boxscale, w, h = calc_dimensions()
        elements.view_width = w

        # FIXME: json is the only thing we can convert MathML into.
        # Handle other graphics formats.
        format_fn = lookup_method(elements, "json")

        json_repr = format_fn(elements, **options)

        # TODO: Cubeoid (like this)
        # json_repr = [{'faceColor': (1, 1, 1, 1), 'position': [(0,0,0), None],
        # 'size':[(1,1,1), None], 'type': 'cube'}]

        json_repr = json.dumps(
            {
                "elements": json_repr,
                "axes": {
                    "hasaxes": axes,
                    "ticks": ticks,
                    "ticks_style": js_ticks_style,
                },
                "extent": {
                    "xmin": xmin,
                    "xmax": xmax,
                    "ymin": ymin,
                    "ymax": ymax,
                    "zmin": zmin,
                    "zmax": zmax,
                },
                "lighting": self.lighting,
                "viewpoint": self.viewpoint,
            }
        )

        return json_repr

    def boxes_to_mathml(self, leaves=None, **options) -> str:
        """Turn the Graphics3DBox into a MathML string"""
        json_repr = self.boxes_to_json(leaves, **options)
        mathml = f'<graphics3d data="{html.escape(json_repr)}" />'
        mathml = f"<mtable><mtr><mtd>{mathml}</mtd></mtr></mtable>"
        return mathml

    def boxes_to_tex(self, leaves=None, **options):
        if not leaves:
            leaves = self._leaves

        (
            elements,
            axes,
            ticks,
            ticks_style,
            calc_dimensions,
            boxscale,
        ) = self._prepare_elements(leaves, options, max_width=450)

        elements._apply_boxscaling(boxscale)

        format_fn = lookup_method(elements, "asy")
        if format_fn is not None:
            asy = format_fn(elements)
        else:
            asy = elements.to_asy()

        xmin, xmax, ymin, ymax, zmin, zmax, boxscale, w, h = calc_dimensions()

        # TODO: Intelligently place the axes on the longest non-middle edge.
        # See algorithm used by web graphics in mathics/web/media/graphics.js
        # for details of this. (Projection to sceen etc).

        # Choose axes placement (boundbox edge vertices)
        axes_indices = []
        if axes[0]:
            axes_indices.append(0)
        if axes[1]:
            axes_indices.append(6)
        if axes[2]:
            axes_indices.append(8)

        # Draw boundbox and axes
        boundbox_asy = ""
        boundbox_lines = self.get_boundbox_lines(xmin, xmax, ymin, ymax, zmin, zmax)

        for i, line in enumerate(boundbox_lines):
            if i in axes_indices:
                pen = asy_create_pens(
                    edge_color=RGBColor(components=(0, 0, 0, 1)), stroke_width=1.5
                )
            else:
                pen = asy_create_pens(
                    edge_color=RGBColor(components=(0.4, 0.4, 0.4, 1)), stroke_width=1
                )

            path = "--".join(["(%.5g,%.5g,%.5g)" % coords for coords in line])
            boundbox_asy += "draw((%s), %s);\n" % (path, pen)

        # TODO: Intelligently draw the axis ticks such that they are always
        # directed inward and choose the coordinate direction which makes the
        # ticks the longest. Again, details in mathics/web/media/graphics.js

        # Draw axes ticks
        ticklength = 0.05 * max([xmax - xmin, ymax - ymin, zmax - zmin])
        pen = asy_create_pens(
            edge_color=RGBColor(components=(0, 0, 0, 1)), stroke_width=1.2
        )
        for xi in axes_indices:
            if xi < 4:  # x axis
                for i, tick in enumerate(ticks[0][0]):
                    line = [
                        (tick, boundbox_lines[xi][0][1], boundbox_lines[xi][0][2]),
                        (
                            tick,
                            boundbox_lines[xi][0][1],
                            boundbox_lines[xi][0][2] + ticklength,
                        ),
                    ]

                    path = "--".join(
                        ["({0},{1},{2})".format(*coords) for coords in line]
                    )

                    boundbox_asy += "draw(({0}), {1});\n".format(path, pen)
                    boundbox_asy += 'label("{0}",{1},{2});\n'.format(
                        ticks[0][2][i],
                        (tick, boundbox_lines[xi][0][1], boundbox_lines[xi][0][2]),
                        "S",
                    )

                for small_tick in ticks[0][1]:
                    line = [
                        (
                            small_tick,
                            boundbox_lines[xi][0][1],
                            boundbox_lines[xi][0][2],
                        ),
                        (
                            small_tick,
                            boundbox_lines[xi][0][1],
                            boundbox_lines[xi][0][2] + 0.5 * ticklength,
                        ),
                    ]

                    path = "--".join(
                        ["({0},{1},{2})".format(*coords) for coords in line]
                    )

                    boundbox_asy += "draw(({0}), {1});\n".format(path, pen)

            if 4 <= xi < 8:  # y axis
                for i, tick in enumerate(ticks[1][0]):
                    line = [
                        (boundbox_lines[xi][0][0], tick, boundbox_lines[xi][0][2]),
                        (
                            boundbox_lines[xi][0][0],
                            tick,
                            boundbox_lines[xi][0][2] - ticklength,
                        ),
                    ]
                    path = "--".join(
                        ["({0},{1},{2})".format(*coords) for coords in line]
                    )

                    boundbox_asy += "draw(({0}), {1});\n".format(path, pen)

                    boundbox_asy += 'label("{0}",{1},{2});\n'.format(
                        ticks[1][2][i],
                        (boundbox_lines[xi][0][0], tick, boundbox_lines[xi][0][2]),
                        "NW",
                    )

                for small_tick in ticks[1][1]:
                    line = [
                        (
                            boundbox_lines[xi][0][0],
                            small_tick,
                            boundbox_lines[xi][0][2],
                        ),
                        (
                            boundbox_lines[xi][0][0],
                            small_tick,
                            boundbox_lines[xi][0][2] - 0.5 * ticklength,
                        ),
                    ]
                    path = "--".join(
                        ["({0},{1},{2})".format(*coords) for coords in line]
                    )
                    boundbox_asy += "draw(({0}), {1});\n".format(path, pen)
            if 8 <= xi:  # z axis
                for i, tick in enumerate(ticks[2][0]):
                    line = [
                        (boundbox_lines[xi][0][0], boundbox_lines[xi][0][1], tick),
                        (
                            boundbox_lines[xi][0][0],
                            boundbox_lines[xi][0][1] + ticklength,
                            tick,
                        ),
                    ]
                    path = "--".join(
                        ["({0},{1},{2})".format(*coords) for coords in line]
                    )
                    boundbox_asy += "draw(({0}), {1});\n".format(path, pen)
                    boundbox_asy += 'label("{0}",{1},{2});\n'.format(
                        ticks[2][2][i],
                        (boundbox_lines[xi][0][0], boundbox_lines[xi][0][1], tick),
                        "W",
                    )
                for small_tick in ticks[2][1]:
                    line = [
                        (
                            boundbox_lines[xi][0][0],
                            boundbox_lines[xi][0][1],
                            small_tick,
                        ),
                        (
                            boundbox_lines[xi][0][0],
                            boundbox_lines[xi][0][1] + 0.5 * ticklength,
                            small_tick,
                        ),
                    ]
                    path = "--".join(
                        ["({0},{1},{2})".format(*coords) for coords in line]
                    )
                    boundbox_asy += "draw(({0}), {1});\n".format(path, pen)

        (height, width) = (400, 400)  # TODO: Proper size
        tex = r"""
\begin{{asy}}
import three;
import solids;
size({0}cm, {1}cm);
currentprojection=perspective({2[0]},{2[1]},{2[2]});
currentlight=light(rgb(0.5,0.5,1), specular=red, (2,0,2), (2,2,2), (0,2,2));
{3}
{4}
\end{{asy}}
""".format(
            asy_number(width / 60),
            asy_number(height / 60),
            self.viewpoint,
            asy,
            boundbox_asy,
        )
        return tex

    def boxes_to_text(self, leaves=None, **options):
        if not leaves:
            leaves = self._leaves
        return "-Graphics3D-"

    def create_axes(
        self, elements, graphics_options, xmin, xmax, ymin, ymax, zmin, zmax, boxscale
    ):
        axes = graphics_options.get("System`Axes")
        if axes.is_true():
            axes = (True, True, True)
        elif axes.has_form("List", 3):
            axes = (leaf.is_true() for leaf in axes.leaves)
        else:
            axes = (False, False, False)
        ticks_style = graphics_options.get("System`TicksStyle")
        axes_style = graphics_options.get("System`AxesStyle")
        label_style = graphics_options.get("System`LabelStyle")

        # FIXME: Doesn't handle GrayScale
        if ticks_style.has_form("List", 1, 2, 3):
            ticks_style = ticks_style.leaves
        elif ticks_style.has_form("RGBColor", None):
            ticks_style = [ticks_style] * 3
        else:
            ticks_style = []

        if axes_style.has_form("List", 1, 2, 3):
            axes_style = axes_style.leaves
        else:
            axes_style = [axes_style] * 3

        # FIXME: Not quite right. We only handle color
        ticks_style = [
            elements.create_style(s).get_style(_Color, face_element=False)[0]
            for s in ticks_style
        ]

        axes_style = [elements.create_style(s) for s in axes_style]
        label_style = elements.create_style(label_style)

        # For later
        # ticks_style[0].extend(axes_style[0])
        # ticks_style[1].extend(axes_style[1])
        # ticks_style[2].extend(axes_style[2])

        ticks = [
            self.axis_ticks(xmin, xmax),
            self.axis_ticks(ymin, ymax),
            self.axis_ticks(zmin, zmax),
        ]

        # Add zero if required, since axis_ticks does not
        if xmin <= 0 <= xmax:
            ticks[0][0].append(0.0)
            ticks[0][0].sort()
        if ymin <= 0 <= ymax:
            ticks[1][0].append(0.0)
            ticks[1][0].sort()
        if zmin <= 0 <= zmax:
            ticks[2][0].append(0.0)
            ticks[2][0].sort()

        # Convert ticks to nice strings e.g 0.100000000000002 -> '0.1' and
        # scale ticks appropriately
        ticks = [
            [
                [boxscale[i] * x for x in t[0]],
                [boxscale[i] * x for x in t[1]],
                ["%g" % x for x in t[0]],
            ]
            for i, t in enumerate(ticks)
        ]

        return axes, ticks, ticks_style

    def get_boundbox_lines(self, xmin, xmax, ymin, ymax, zmin, zmax):
        return [
            [(xmin, ymin, zmin), (xmax, ymin, zmin)],
            [(xmin, ymax, zmin), (xmax, ymax, zmin)],
            [(xmin, ymin, zmax), (xmax, ymin, zmax)],
            [(xmin, ymax, zmax), (xmax, ymax, zmax)],
            [(xmin, ymin, zmin), (xmin, ymax, zmin)],
            [(xmax, ymin, zmin), (xmax, ymax, zmin)],
            [(xmin, ymin, zmax), (xmin, ymax, zmax)],
            [(xmax, ymin, zmax), (xmax, ymax, zmax)],
            [(xmin, ymin, zmin), (xmin, ymin, zmax)],
            [(xmax, ymin, zmin), (xmax, ymin, zmax)],
            [(xmin, ymax, zmin), (xmin, ymax, zmax)],
            [(xmax, ymax, zmin), (xmax, ymax, zmax)],
        ]


class Arrow3DBox(ArrowBox):
    def init(self, *args, **kwargs):
        super(Arrow3DBox, self).init(*args, **kwargs)

    def process_option(self, name, value):
        super(Arrow3DBox, self).process_option(name, value)

    def extent(self):
        result = []
        for line in self.lines:
            for c in line:
                p, d = c.pos()
                result.append(p)
        return result

    def _apply_boxscaling(self, boxscale):
        for line in self.lines:
            for coords in line:
                coords.scale(boxscale)


class Cylinder3DBox(_Graphics3DElement):
    """
    Internal Python class used when Boxing a 'Cylinder' object.
    """

    def init(self, graphics, style, item):
        super(Cylinder3DBox, self).init(graphics, item, style)

        self.edge_color, self.face_color = style.get_style(_Color, face_element=True)

        if len(item.leaves) != 2:
            raise BoxConstructError

        points = item.leaves[0].to_python()
        if not all(
            len(point) == 3 and all(isinstance(p, numbers.Real) for p in point)
            for point in points
        ):
            raise BoxConstructError

        self.points = [Coords3D(graphics, pos=point) for point in points]
        self.radius = item.leaves[1].to_python()

    def to_asy(self):
        if self.face_color is None:
            face_color = (1, 1, 1)
        else:
            face_color = self.face_color.to_js()

        rgb = f"rgb({face_color[0]}, {face_color[1]}, {face_color[2]})"
        return "".join(
            f"draw(surface(cylinder({tuple(coord.pos()[0])}, {self.radius}, {self.height})), {rgb});"
            for coord in self.points
        )

    def extent(self):
        result = []
        # FIXME: instead of `coords.add(±self.radius, ±self.radius, ±self.radius)` we should do:
        # coords.add(transformation_vector.x * ±self.radius, transformation_vector.y * ±self.radius, transformation_vector.z * ±self.radius)
        result.extend(
            [
                coords.add(self.radius, self.radius, self.radius).pos()[0]
                for coords in self.points
            ]
        )
        result.extend(
            [
                coords.add(-self.radius, -self.radius, -self.radius).pos()[0]
                for coords in self.points
            ]
        )
        return result

    def _apply_boxscaling(self, boxscale):
        # TODO
        pass


class Line3DBox(LineBox):
    def init(self, *args, **kwargs):
        super(Line3DBox, self).init(*args, **kwargs)

    def process_option(self, name, value):
        super(Line3DBox, self).process_option(name, value)

    def extent(self):
        result = []
        for line in self.lines:
            for c in line:
                p, d = c.pos()
                result.append(p)
        return result

    def _apply_boxscaling(self, boxscale):
        for line in self.lines:
            for coords in line:
                coords.scale(boxscale)


class Point3DBox(PointBox):
    def init(self, *args, **kwargs):
        super(Point3DBox, self).init(*args, **kwargs)

    def process_option(self, name, value):
        super(Point3DBox, self).process_option(name, value)

    def extent(self):
        result = []
        for line in self.lines:
            for c in line:
                p, d = c.pos()
                result.append(p)
        return result

    def _apply_boxscaling(self, boxscale):
        for line in self.lines:
            for coords in line:
                coords.scale(boxscale)


class Polygon3DBox(PolygonBox):
    def init(self, *args, **kwargs):
        self.vertex_normals = None
        super(Polygon3DBox, self).init(*args, **kwargs)

    def process_option(self, name, value):
        if name == "System`VertexNormals":
            # TODO: process VertexNormals and use them in rendering
            pass
        else:
            super(Polygon3DBox, self).process_option(name, value)

    def extent(self):
        result = []
        for line in self.lines:
            for c in line:
                p, d = c.pos()
                result.append(p)
        return result

    def _apply_boxscaling(self, boxscale):
        for line in self.lines:
            for coords in line:
                coords.scale(boxscale)


class Sphere3DBox(_Graphics3DElement):
    def init(self, graphics, style, item):
        super(Sphere3DBox, self).init(graphics, item, style)
        self.edge_color, self.face_color = style.get_style(_Color, face_element=True)
        if len(item.leaves) != 2:
            raise BoxConstructError

        points = item.leaves[0].to_python()
        if not all(isinstance(point, list) for point in points):
            points = [points]
        if not all(
            len(point) == 3 and all(isinstance(p, numbers.Real) for p in point)
            for point in points
        ):
            raise BoxConstructError

        self.points = [Coords3D(graphics, pos=point) for point in points]
        self.radius = item.leaves[1].to_python()

    def extent(self):
        result = []
        result.extend(
            [
                coords.add(self.radius, self.radius, self.radius).pos()[0]
                for coords in self.points
            ]
        )
        result.extend(
            [
                coords.add(-self.radius, -self.radius, -self.radius).pos()[0]
                for coords in self.points
            ]
        )
        return result

    def _apply_boxscaling(self, boxscale):
        # TODO
        pass


# FIXME: GLOBALS3D is a horrible name.
GLOBALS3D.update(
    {
        "System`Arrow3DBox": Arrow3DBox,
        "System`Cylinder3DBox": Cylinder3DBox,
        "System`Line3DBox": Line3DBox,
        "System`Point3DBox": Point3DBox,
        "System`Polygon3DBox": Polygon3DBox,
        "System`Sphere3DBox": Sphere3DBox,
    }
)
