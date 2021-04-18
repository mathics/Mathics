# -*- coding: utf-8 -*-

"""
Graphics (3D)
"""


import numbers
from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.core.expression import (
    Expression,
    from_python,
    system_symbols_dict,
    SymbolList,
)
from mathics.builtin.base import BoxConstructError, Builtin, InstanceableBuiltin
from .graphics import (
    Graphics,
    GraphicsBox,
    PolygonBox,
    create_pens,
    _Color,
    LineBox,
    PointBox,
    Style,
    RGBColor,
    get_class,
    asy_number,
    CoordinatesError,
    _GraphicsElements,
)

import json
import html


def coords3D(value):
    if value.has_form("List", 3):
        result = (
            value.leaves[0].round_to_float(),
            value.leaves[1].round_to_float(),
            value.leaves[2].round_to_float(),
        )
        if None not in result:
            return result
    raise CoordinatesError


class Coords3D(object):
    def __init__(self, graphics, expr=None, pos=None, d=None):
        self.graphics = graphics
        self.p = pos
        self.d = d
        if expr is not None:
            if expr.has_form("Offset", 1, 2):
                self.d = coords3D(expr.leaves[0])
                if len(expr.leaves) > 1:
                    self.p = coords3D(expr.leaves[1])
                else:
                    self.p = None
            else:
                self.p = coords3D(expr)

    def pos(self):
        return self.p, self.d

    def add(self, x, y, z):
        p = (self.p[0] + x, self.p[1] + y, self.p[2] + z)
        return Coords3D(self.graphics, pos=p, d=self.d)

    def scale(self, a):
        self.p = (self.p[0] * a[0], self.p[1] * a[1], self.p[2] * a[2])


class Style3D(Style):
    def get_default_face_color(self):
        return RGBColor(components=(1, 1, 1, 1))


class Graphics3D(Graphics):
    r"""
    <dl>
      <dt>'Graphics3D[$primitives$, $options$]'
      <dd>represents a three-dimensional graphic.

    See also the Section "Plotting" for a list of Plot options.
    </dl>

    >> Graphics3D[Polygon[{{0,0,0}, {0,1,1}, {1,0,0}}]]
     = -Graphics3D-

    In 'TeXForm', 'Graphics3D' creates Asymptote figures:
    >> Graphics3D[Sphere[]] // TeXForm
     = #<--#
     . \begin{asy}
     . import three;
     . import solids;
     . size(6.6667cm, 6.6667cm);
     . currentprojection=perspective(2.6,-4.8,4.0);
     . currentlight=light(rgb(0.5,0.5,1), specular=red, (2,0,2), (2,2,2), (0,2,2));
     . draw(surface(sphere((0, 0, 0), 1)), rgb(1,1,1));
     . draw(((-1,-1,-1)--(1,-1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,1,-1)--(1,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,-1,1)--(1,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,1,1)--(1,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,-1,-1)--(-1,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((1,-1,-1)--(1,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,-1,1)--(-1,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((1,-1,1)--(1,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,-1,-1)--(-1,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((1,-1,-1)--(1,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,1,-1)--(-1,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((1,1,-1)--(1,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . \end{asy}

    #> Graphics3D[Point[Table[{Sin[t], Cos[t], 0}, {t, 0, 2. Pi, Pi / 15.}]]] // TeXForm
     = #<--#
     . \begin{asy}
     . import three;
     . import solids;
     . size(6.6667cm, 6.6667cm);
     . currentprojection=perspective(2.6,-4.8,4.0);
     . currentlight=light(rgb(0.5,0.5,1), specular=red, (2,0,2), (2,2,2), (0,2,2));
     . path3 g=(0,1,0)--(0.20791,0.97815,0)--(0.40674,0.91355,0)--(0.58779,0.80902,0)--(0.74314,0.66913,0)--(0.86603,0.5,0)--(0.95106,0.30902,0)--(0.99452,0.10453,0)--(0.99452,-0.10453,0)--(0.95106,-0.30902,0)--(0.86603,-0.5,0)--(0.74314,-0.66913,0)--(0.58779,-0.80902,0)--(0.40674,-0.91355,0)--(0.20791,-0.97815,0)--(5.6655e-16,-1,0)--(-0.20791,-0.97815,0)--(-0.40674,-0.91355,0)--(-0.58779,-0.80902,0)--(-0.74314,-0.66913,0)--(-0.86603,-0.5,0)--(-0.95106,-0.30902,0)--(-0.99452,-0.10453,0)--(-0.99452,0.10453,0)--(-0.95106,0.30902,0)--(-0.86603,0.5,0)--(-0.74314,0.66913,0)--(-0.58779,0.80902,0)--(-0.40674,0.91355,0)--(-0.20791,0.97815,0)--(1.5314e-15,1,0)--cycle;dot(g, rgb(0, 0, 0));
     . draw(((-0.99452,-1,-1)--(0.99452,-1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,1,-1)--(0.99452,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,-1,1)--(0.99452,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,1,1)--(0.99452,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,-1,-1)--(-0.99452,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((0.99452,-1,-1)--(0.99452,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,-1,1)--(-0.99452,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((0.99452,-1,1)--(0.99452,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,-1,-1)--(-0.99452,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((0.99452,-1,-1)--(0.99452,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,1,-1)--(-0.99452,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((0.99452,1,-1)--(0.99452,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . \end{asy}
    """

    options = Graphics.options.copy()
    options.update(
        {"BoxRatios": "Automatic", "Lighting": "Automatic", "ViewPoint": "{1.3,-2.4,2}"}
    )

    box_suffix = "3DBox"

    rules = {
        "MakeBoxes[Graphics3D[content_, OptionsPattern[Graphics3D]], "
        "        OutputForm]": '"-Graphics3D-"'
    }

    messages = {"invlight": "`1` is not a valid list of light sources."}


class Graphics3DBox(GraphicsBox):
    def boxes_to_text(self, leaves=None, **options):
        if not leaves:
            leaves = self._leaves
        return "-Graphics3D-"

    def _prepare_elements(self, leaves, options, max_width=None):
        if not leaves:
            raise BoxConstructError

        graphics_options = self.get_option_values(leaves[1:], **options)

        evaluation = options["evaluation"]

        base_width, base_height, size_multiplier, size_aspect = self._get_image_size(
            options, graphics_options, max_width
        )

        # TODO: Handle ImageScaled[], and Scaled[]
        lighting_option = graphics_options["System`Lighting"]
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
        viewpoint_option = graphics_options["System`ViewPoint"]
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
        # aspect_ratio = graphics_options['AspectRatio'].to_python()

        boxratios = graphics_options["System`BoxRatios"].to_python()
        if boxratios == "System`Automatic":
            boxratios = ["System`Automatic"] * 3
        else:
            boxratios = boxratios
        if not isinstance(boxratios, list) or len(boxratios) != 3:
            raise BoxConstructError

        plot_range = graphics_options["System`PlotRange"].to_python()
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

            return xmin, xmax, ymin, ymax, zmin, zmax, boxscale

        xmin, xmax, ymin, ymax, zmin, zmax, boxscale = calc_dimensions(final_pass=False)

        axes, ticks = self.create_axes(
            elements, graphics_options, xmin, xmax, ymin, ymax, zmin, zmax, boxscale
        )

        return elements, axes, ticks, calc_dimensions, boxscale

    def boxes_to_tex(self, leaves=None, **options):
        if not leaves:
            leaves = self._leaves

        elements, axes, ticks, calc_dimensions, boxscale = self._prepare_elements(
            leaves, options, max_width=450
        )

        elements._apply_boxscaling(boxscale)

        asy = elements.to_asy()

        xmin, xmax, ymin, ymax, zmin, zmax, boxscale = calc_dimensions()

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
                pen = create_pens(
                    edge_color=RGBColor(components=(0, 0, 0, 1)), stroke_width=1.5
                )
            else:
                pen = create_pens(
                    edge_color=RGBColor(components=(0.4, 0.4, 0.4, 1)), stroke_width=1
                )

            path = "--".join(["(%.5g,%.5g,%.5g)" % coords for coords in line])
            boundbox_asy += "draw((%s), %s);\n" % (path, pen)

        # TODO: Intelligently draw the axis ticks such that they are always
        # directed inward and choose the coordinate direction which makes the
        # ticks the longest. Again, details in mathics/web/media/graphics.js

        # Draw axes ticks
        ticklength = 0.05 * max([xmax - xmin, ymax - ymin, zmax - zmin])
        pen = create_pens(
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

    def boxes_to_mathml(self, leaves=None, **options):
        if not leaves:
            leaves = self._leaves

        elements, axes, ticks, calc_dimensions, boxscale = self._prepare_elements(
            leaves, options
        )

        elements._apply_boxscaling(boxscale)

        json_repr = elements.to_json()

        xmin, xmax, ymin, ymax, zmin, zmax, boxscale = calc_dimensions()

        # TODO: Cubeoid (like this)
        # json_repr = [{'faceColor': (1, 1, 1, 1), 'position': [(0,0,0), None],
        # 'size':[(1,1,1), None], 'type': 'cube'}]

        json_repr = json.dumps(
            {
                "elements": json_repr,
                "axes": {"hasaxes": axes, "ticks": ticks},
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

        # return "<mn>3</mn>"

        # xml = ('<graphics3d xmin="%f" xmax="%f" ymin="%f" ymax="%f" '
        #        'zmin="%f" zmax="%f" data="%s" />') % (
        #           xmin, xmax, ymin, ymax, zmin, zmax, json_repr)
        xml = '<graphics3d data="{0}" />'.format(html.escape(json_repr))
        xml = "<mtable><mtr><mtd>{0}</mtd></mtr></mtable>".format(xml)
        return xml

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
        if ticks_style.has_form("List", 3):
            ticks_style = ticks_style.leaves
        else:
            ticks_style = [ticks_style] * 3
        if axes_style.has_form("List", 3):
            axes_style = axes_style.leaves
        else:
            axes_style = [axes_style] * 3

        ticks_style = [elements.create_style(s) for s in ticks_style]
        axes_style = [elements.create_style(s) for s in axes_style]
        label_style = elements.create_style(label_style)
        ticks_style[0].extend(axes_style[0])
        ticks_style[1].extend(axes_style[1])

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

        return axes, ticks

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


def total_extent_3d(extents):
    xmin = xmax = ymin = ymax = zmin = zmax = None
    for extent in extents:
        for x, y, z in extent:
            if xmin is None or x < xmin:
                xmin = x
            if xmax is None or x > xmax:
                xmax = x
            if ymin is None or y < ymin:
                ymin = y
            if ymax is None or y > ymax:
                ymax = y
            if zmin is None or z < zmin:
                zmin = z
            if zmax is None or z > zmax:
                zmax = z
    return xmin, xmax, ymin, ymax, zmin, zmax


class Graphics3DElements(_GraphicsElements):
    coords = Coords3D

    def __init__(self, content, evaluation, neg_y=False):
        super(Graphics3DElements, self).__init__(content, evaluation)
        self.neg_y = neg_y
        self.xmin = (
            self.ymin
        ) = (
            self.pixel_width
        ) = self.pixel_height = self.extent_width = self.extent_height = None

    def extent(self, completely_visible_only=False):
        return total_extent_3d([element.extent() for element in self.elements])

    def to_svg(self):
        return "\n".join(element.to_svg() for element in self.elements)

    def to_asy(self):
        return "\n".join([element.to_asy() for element in self.elements])

    def _apply_boxscaling(self, boxscale):
        for element in self.elements:
            element._apply_boxscaling(boxscale)

    def to_json(self):
        result = []
        for element in self.elements:
            result.extend(element.to_json())
        return result

    def get_style_class(self):
        return Style3D


class Point3DBox(PointBox):
    def init(self, *args, **kwargs):
        super(Point3DBox, self).init(*args, **kwargs)

    def process_option(self, name, value):
        super(Point3DBox, self).process_option(name, value)

    def to_json(self):
        # TODO: account for point size
        data = []

        # Tempoary bug fix: default Point color should be black not white
        face_color = self.face_color
        if list(face_color.to_rgba()[:3]) == [1, 1, 1]:
            face_color = RGBColor(components=(0, 0, 0, face_color.to_rgba()[3]))

        for line in self.lines:
            data.append(
                {
                    "type": "point",
                    "coords": [coords.pos() for coords in line],
                    "color": face_color.to_rgba(),
                }
            )
        return data

    def to_asy(self):
        face_color = self.face_color

        # Tempoary bug fix: default Point color should be black not white
        if list(face_color.to_rgba()[:3]) == [1, 1, 1]:
            face_color = RGBColor(components=(0, 0, 0, face_color.to_rgba()[3]))

        pen = create_pens(face_color=face_color, is_face_element=False)

        return "".join(
            "path3 g={0}--cycle;dot(g, {1});".format(
                "--".join("(%.5g,%.5g,%.5g)" % coords.pos()[0] for coords in line), pen
            )
            for line in self.lines
        )

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


class Line3DBox(LineBox):
    def init(self, *args, **kwargs):
        super(Line3DBox, self).init(*args, **kwargs)

    def process_option(self, name, value):
        super(Line3DBox, self).process_option(name, value)

    def to_json(self):
        # TODO: account for line widths and style
        data = []
        for line in self.lines:
            data.append(
                {
                    "type": "line",
                    "coords": [coords.pos() for coords in line],
                    "color": self.edge_color.to_rgba(),
                }
            )
        return data

    def to_asy(self):
        # l = self.style.get_line_width(face_element=False)
        pen = create_pens(edge_color=self.edge_color, stroke_width=1)

        return "".join(
            "draw({0}, {1});".format(
                "--".join("({0},{1},{2})".format(*coords.pos()[0]) for coords in line),
                pen,
            )
            for line in self.lines
        )

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

    def to_json(self):
        # TODO: account for line widths and style
        if self.vertex_colors is None:
            face_color = self.face_color
        else:
            face_color = None

        if face_color is not None:
            face_color = face_color.to_js()

        data = []
        for line in self.lines:
            data.append(
                {
                    "type": "polygon",
                    "coords": [coords.pos() for coords in line],
                    "faceColor": face_color,
                }
            )
        return data

    def to_asy(self):
        l = self.style.get_line_width(face_element=True)
        if self.vertex_colors is None:
            face_color = self.face_color
        else:
            face_color = None
        pen = create_pens(
            edge_color=self.edge_color,
            face_color=face_color,
            stroke_width=l,
            is_face_element=True,
        )

        asy = ""
        for line in self.lines:
            asy += (
                "path3 g="
                + "--".join(["(%.5g,%.5g,%.5g)" % coords.pos()[0] for coords in line])
                + "--cycle;"
            )
            asy += "draw(surface(g), %s);" % (pen)
        return asy

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


class Sphere(Builtin):
    """
    <dl>
    <dt>'Sphere[{$x$, $y$, $z$}]'
        <dd>is a sphere of radius 1 centered at the point {$x$, $y$, $z$}.
    <dt>'Sphere[{$x$, $y$, $z$}, $r$]'
        <dd>is a sphere of radius $r$ centered at the point {$x$, $y$, $z$}.
    <dt>'Sphere[{{$x1$, $y1$, $z1$}, {$x2$, $y2$, $z2$}, ... }, $r$]'
        <dd>is a collection spheres of radius $r$ centered at the points {$x1$, $y2$, $z2$}, {$x2$, $y2$, $z2$}, ...
    </dl>

    >> Graphics3D[Sphere[{0, 0, 0}, 1]]
     = -Graphics3D-

    >> Graphics3D[{Yellow, Sphere[{{-1, 0, 0}, {1, 0, 0}, {0, 0, Sqrt[3.]}}, 1]}]
     = -Graphics3D-
    """

    rules = {
        "Sphere[]": "Sphere[{0, 0, 0}, 1]",
        "Sphere[positions_]": "Sphere[positions, 1]",
    }


class Cuboid(Builtin):
    """
    <dl>
    <dt>'Cuboid[{$xmin$, $ymin$, $zmin$}]'
        <dd>is a unit cube.
    <dt>'Cuboid[{$xmin$, $ymin$, $zmin$}, {$xmax$, $ymax$, $zmax$}]'
        <dd>represents a cuboid extending from {$xmin$, $ymin$, $zmin$} to {$xmax$, $ymax$, $zmax$}.
    </dl>

    >> Graphics3D[Cuboid[{0, 0, 1}]]
     = -Graphics3D-

    >> Graphics3D[{Red, Cuboid[{0, 0, 0}, {1, 1, 0.5}], Blue, Cuboid[{0.25, 0.25, 0.5}, {0.75, 0.75, 1}]}]
     = -Graphics3D-
    """

    rules = {"Cuboid[]": "Cuboid[{0,0,0}]"}

    def apply_full(self, xmin, ymin, zmin, xmax, ymax, zmax, evaluation):
        "Cuboid[{xmin_, ymin_, zmin_}, {xmax_, ymax_, zmax_}]"

        xmin, ymin, zmin = [
            value.round_to_float(evaluation) for value in (xmin, ymin, zmin)
        ]
        xmax, ymax, zmax = [
            value.round_to_float(evaluation) for value in (xmax, ymax, zmax)
        ]
        if None in (xmin, ymin, zmin, xmax, ymax, zmax):
            return  # TODO

        if (xmax <= xmin) or (ymax <= ymin) or (zmax <= zmin):
            return  # TODO

        polygons = [
            # X
            Expression(
                "List",
                Expression(SymbolList, xmin, ymin, zmin),
                Expression(SymbolList, xmin, ymax, zmin),
                Expression(SymbolList, xmin, ymax, zmax),
            ),
            Expression(
                "List",
                Expression(SymbolList, xmin, ymin, zmin),
                Expression(SymbolList, xmin, ymin, zmax),
                Expression(SymbolList, xmin, ymax, zmax),
            ),
            Expression(
                "List",
                Expression(SymbolList, xmax, ymin, zmin),
                Expression(SymbolList, xmax, ymax, zmin),
                Expression(SymbolList, xmax, ymax, zmax),
            ),
            Expression(
                "List",
                Expression(SymbolList, xmax, ymin, zmin),
                Expression(SymbolList, xmax, ymin, zmax),
                Expression(SymbolList, xmax, ymax, zmax),
            ),
            # Y
            Expression(
                "List",
                Expression(SymbolList, xmin, ymin, zmin),
                Expression(SymbolList, xmax, ymin, zmin),
                Expression(SymbolList, xmax, ymin, zmax),
            ),
            Expression(
                "List",
                Expression(SymbolList, xmin, ymin, zmin),
                Expression(SymbolList, xmin, ymin, zmax),
                Expression(SymbolList, xmax, ymin, zmax),
            ),
            Expression(
                "List",
                Expression(SymbolList, xmin, ymax, zmin),
                Expression(SymbolList, xmax, ymax, zmin),
                Expression(SymbolList, xmax, ymax, zmax),
            ),
            Expression(
                "List",
                Expression(SymbolList, xmin, ymax, zmin),
                Expression(SymbolList, xmin, ymax, zmax),
                Expression(SymbolList, xmax, ymax, zmax),
            ),
            # Z
            Expression(
                "List",
                Expression(SymbolList, xmin, ymin, zmin),
                Expression(SymbolList, xmin, ymax, zmin),
                Expression(SymbolList, xmax, ymax, zmin),
            ),
            Expression(
                "List",
                Expression(SymbolList, xmin, ymin, zmin),
                Expression(SymbolList, xmax, ymin, zmin),
                Expression(SymbolList, xmax, ymax, zmin),
            ),
            Expression(
                "List",
                Expression(SymbolList, xmin, ymin, zmax),
                Expression(SymbolList, xmin, ymax, zmax),
                Expression(SymbolList, xmax, ymax, zmax),
            ),
            Expression(
                "List",
                Expression(SymbolList, xmin, ymin, zmax),
                Expression(SymbolList, xmax, ymin, zmax),
                Expression(SymbolList, xmax, ymax, zmax),
            ),
        ]

        return Expression("Polygon", Expression(SymbolList, *polygons))

    def apply_min(self, xmin, ymin, zmin, evaluation):
        "Cuboid[{xmin_, ymin_, zmin_}]"
        xmin, ymin, zmin = [
            value.round_to_float(evaluation) for value in (xmin, ymin, zmin)
        ]
        if None in (xmin, ymin, zmin):
            return  # TODO

        (xmax, ymax, zmax) = (from_python(value + 1) for value in (xmin, ymin, zmin))
        (xmin, ymin, zmin) = (from_python(value) for value in (xmin, ymin, zmin))

        return self.apply_full(xmin, ymin, zmin, xmax, ymax, zmax, evaluation)


class _Graphics3DElement(InstanceableBuiltin):
    def init(self, graphics, item=None, style=None):
        if item is not None and not item.has_form(self.get_name(), None):
            raise BoxConstructError
        self.graphics = graphics
        self.style = style
        self.is_completely_visible = False  # True for axis elements


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

    def to_asy(self):
        # l = self.style.get_line_width(face_element=True)

        if self.face_color is None:
            face_color = (1, 1, 1)
        else:
            face_color = self.face_color.to_js()

        return "".join(
            "draw(surface(sphere({0}, {1})), rgb({2},{3},{4}));".format(
                tuple(coord.pos()[0]), self.radius, *face_color[:3]
            )
            for coord in self.points
        )

    def to_json(self):
        face_color = self.face_color
        if face_color is not None:
            face_color = face_color.to_js()
        return [
            {
                "type": "sphere",
                "coords": [coords.pos() for coords in self.points],
                "radius": self.radius,
                "faceColor": face_color,
            }
        ]

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


GLOBALS3D = system_symbols_dict(
    {
        "Polygon3DBox": Polygon3DBox,
        "Line3DBox": Line3DBox,
        "Point3DBox": Point3DBox,
        "Sphere3DBox": Sphere3DBox,
    }
)
