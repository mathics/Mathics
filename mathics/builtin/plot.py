# -*- coding: utf-8 -*-

"""
Plotting
"""


from math import sin, cos, pi, sqrt, isnan, isinf
import numbers
import itertools
import palettable

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.core.expression import (
    Expression,
    Real,
    MachineReal,
    Symbol,
    String,
    Integer,
    from_python,
    SymbolList,
    SymbolN,
    SymbolRule,
)
from mathics.builtin.base import Builtin
from mathics.builtin.scoping import dynamic_scoping
from mathics.builtin.options import options_to_rules
from mathics.builtin.numeric import chop
from mathics.builtin.graphics import Graphics
from mathics.builtin.graphics3d import Graphics3D


try:
    from mathics.builtin.compile import _compile, CompileArg, CompileError, real_type

    has_compile = True
except ImportError:
    has_compile = False


def gradient_palette(color_function, n, evaluation):  # always returns RGB values
    if isinstance(color_function, String):
        color_data = Expression("ColorData", color_function).evaluate(evaluation)
        if not color_data.has_form("ColorDataFunction", 4):
            return
        name, kind, interval, blend = color_data.leaves
        if not isinstance(kind, String) or kind.get_string_value() != "Gradients":
            return
        if not interval.has_form("List", 2):
            return
        x0, x1 = (x.round_to_float() for x in interval.leaves)
    else:
        blend = color_function
        x0 = 0.0
        x1 = 1.0

    xd = x1 - x0
    offsets = [MachineReal(x0 + float(xd * i) / float(n - 1)) for i in range(n)]
    colors = Expression("Map", blend, Expression(SymbolList, *offsets)).evaluate(
        evaluation
    )
    if len(colors.leaves) != n:
        return

    from mathics.builtin.graphics import expression_to_color, ColorError

    try:
        objects = [expression_to_color(x) for x in colors.leaves]
        if any(x is None for x in objects):
            return None
        return [x.to_rgba()[:3] for x in objects]
    except ColorError:
        return


class ColorDataFunction(Builtin):
    pass


class _GradientColorScheme(object):
    def color_data_function(self, name):
        colors = Expression(
            "List", *[Expression("RGBColor", *color) for color in self.colors()]
        )
        blend = Expression(
            "Function", Expression("Blend", colors, Expression("Slot", 1))
        )
        arguments = [
            String(name),
            String("Gradients"),
            Expression(SymbolList, 0, 1),
            blend,
        ]
        return Expression("ColorDataFunction", *arguments)


class _PredefinedGradient(_GradientColorScheme):
    def __init__(self, colors):
        self._colors = colors

    def colors(self):
        return self._colors


class _PalettableGradient(_GradientColorScheme):
    def __init__(self, palette, reversed):
        self.palette = palette
        self.reversed = reversed

    def colors(self):
        colors = self.palette.mpl_colors
        if self.reversed:
            colors = list(reversed(colors))
        return colors


class ColorData(Builtin):
    """
    <dl>
    <dt>'ColorData["$name$"]'
        <dd>returns a color function with the given $name$.
    </dl>

    Define a user-defined color function:
    >> Unprotect[ColorData]; ColorData["test"] := ColorDataFunction["test", "Gradients", {0, 1}, Blend[{Red, Green, Blue}, #1] &]; Protect[ColorData]

    Compare it to the default color function, 'LakeColors':
    >> {DensityPlot[x + y, {x, -1, 1}, {y, -1, 1}], DensityPlot[x + y, {x, -1, 1}, {y, -1, 1}, ColorFunction->"test"]}
     = {-Graphics-, -Graphics-}
    """

    # rules = {
    #    'ColorData["LakeColors"]': (
    #        """ColorDataFunction["LakeColors", "Gradients", {0, 1},
    #            Blend[{RGBColor[0.293416, 0.0574044, 0.529412],
    #                RGBColor[0.563821, 0.527565, 0.909499],
    #                RGBColor[0.762631, 0.846998, 0.914031],
    #                RGBColor[0.941176, 0.906538, 0.834043]}, #1] &]"""),
    # }

    messages = {
        "notent": "`1` is not a known color scheme. ColorData[] gives you lists of schemes.",
    }

    palettes = {
        "LakeColors": _PredefinedGradient(
            [
                (0.293416, 0.0574044, 0.529412),
                (0.563821, 0.527565, 0.909499),
                (0.762631, 0.846998, 0.914031),
                (0.941176, 0.906538, 0.834043),
            ]
        ),
        "Pastel": _PalettableGradient(
            palettable.colorbrewer.qualitative.Pastel1_9, False
        ),
        "Rainbow": _PalettableGradient(
            palettable.colorbrewer.diverging.Spectral_9, True
        ),
        "RedBlueTones": _PalettableGradient(
            palettable.colorbrewer.diverging.RdBu_11, False
        ),
        "GreenPinkTones": _PalettableGradient(
            palettable.colorbrewer.diverging.PiYG_9, False
        ),
        "GrayTones": _PalettableGradient(
            palettable.colorbrewer.sequential.Greys_9, False
        ),
        "SolarColors": _PalettableGradient(
            palettable.colorbrewer.sequential.OrRd_9, True
        ),
        "CherryTones": _PalettableGradient(
            palettable.colorbrewer.sequential.Reds_9, True
        ),
        "FuchsiaTones": _PalettableGradient(
            palettable.colorbrewer.sequential.RdPu_9, True
        ),
        "SiennaTones": _PalettableGradient(
            palettable.colorbrewer.sequential.Oranges_9, True
        ),
        # specific to Mathics
        "Paired": _PalettableGradient(
            palettable.colorbrewer.qualitative.Paired_9, False
        ),
        "Accent": _PalettableGradient(
            palettable.colorbrewer.qualitative.Accent_8, False
        ),
        "Aquatic": _PalettableGradient(palettable.wesanderson.Aquatic1_5, False),
        "Zissou": _PalettableGradient(palettable.wesanderson.Zissou_5, False),
        "Tableau": _PalettableGradient(palettable.tableau.Tableau_20, False),
        "TrafficLight": _PalettableGradient(palettable.tableau.TrafficLight_9, False),
        "Moonrise1": _PalettableGradient(palettable.wesanderson.Moonrise1_5, False),
    }

    def apply_directory(self, evaluation):
        "ColorData[]"
        return Expression(SymbolList, String("Gradients"))

    def apply(self, name, evaluation):
        "ColorData[name_String]"
        py_name = name.get_string_value()
        if py_name == "Gradients":
            return Expression(
                SymbolList, *[String(name) for name in self.palettes.keys()]
            )
        palette = ColorData.palettes.get(py_name, None)
        if palette is None:
            evaluation.message("ColorData", "notent", name)
            return
        return palette.color_data_function(py_name)

    @staticmethod
    def colors(name, evaluation):
        palette = ColorData.palettes.get(name, None)
        if palette is None:
            evaluation.message("ColorData", "notent", name)
            return None
        return palette.colors()


# Note In defining these class, documentation is given for them


class Axis(Builtin):
    """
    <dl>
      <dt>'Axis'
      <dd>is a possible value for the 'Filling' option.
    </dl>

    >> ListLinePlot[Table[Sin[x], {x, -5, 5, 0.5}], Filling->Axis]
     = -Graphics-
    """


class Bottom(Builtin):
    """
    <dl>
      <dt>'Bottom'
      <dd>is a possible value for the 'Filling' option.
    </dl>

    >> ListLinePlot[Table[Sin[x], {x, -5, 5, 0.5}], Filling->Bottom]
     = -Graphics-
    """


class Filling(Builtin):
    """
    <dl>
      <dt>'Filling Top |Bottom|Axis'
      <dd>is a an option to Plot to specify what filling to add under point, curves, and surfaces
    </dl>

    >> ListLinePlot[Table[Sin[x], {x, -5, 5, 0.5}], Filling->Axis]
     = -Graphics-
    """


class Full(Builtin):
    """
    <dl>
      <dt>'Full'
      <dd>is a possible value for the 'Mesh' and 'PlotRange' options.
    </dl>
    """


class ImageSize(Builtin):
    """
    <dl>
      <dt>'ImageSize'
      <dd>is an option that specifies the overall size of an image to display.
    </dl>

    Specifications for both width and height can be any of the following:
    <dl>
      <dt>Automatic
      <dd>determined by location or other dimension (default)
      <dt>Tiny, Small, Medium, Large
      <dd>pre defined absolute sizes
    </dl>
    """


class Joined(Builtin):
    """
    <dl>
      <dt>'Joined $boolean$'
      <dd>is an option for 'Plot' that gives whether to join points to make lines.
    </dl>

    >> ListPlot[Table[n ^ 2, {n, 10}], Joined->True]
     = -Graphics-
    """


class Mesh(Builtin):
    """
    <dl>
       <dt>'Mesh'
      <dd>is an option for 'Plot' that specifies the mesh to be drawn. The default is 'Mesh->None'.
     </dl>

    >> Plot[Sin[Cos[x^2]],{x,-4,4},Mesh->All]
     = -Graphics-

    >> Plot[Sin[x], {x,0,4 Pi}, Mesh->Full]
     = -Graphics-

    >> DensityPlot[Sin[x y], {x, -2, 2}, {y, -2, 2}, Mesh->Full]
     = -Graphics-

    >> Plot3D[Sin[x y], {x, -2, 2}, {y, -2, 2}, Mesh->Full]
     = -Graphics3D-
    """

    messages = {
        "ilevels": "`1` is not a valid mesh specification.",
    }


class PlotPoints(Builtin):
    """
    <dl>
      <dt>'PlotPoints $n$'
      <dd>A number specifies how many initial sample points to use.
     </dl>

    >> Plot[Sin[Cos[x^2]],{x,-4,4}, PlotPoints->22]
     = -Graphics-
    """


class PlotRange(Builtin):
    """
    <dl>
      <dt>'PlotRange $n$ | All | Automatic'
      <dd>is an option for 'Plot' that gives the range of coordinates to include in a plot.
    </dl>

    >> Plot[Sin[Cos[x^2]],{x,-4,4}, PlotRange -> All]
     = -Graphics-
    """


class Top(Builtin):
    """
    <dl>
    <dt>'Top'
        <dd>is a possible value for the 'Filling' option.
    </dl>

    >> ListLinePlot[Table[Sin[x], {x, -5, 5, 0.5}], Filling->Axis|Top|Bottom]
     = -Graphics-
    """


def extract_pyreal(value):
    if isinstance(value, Real):
        return chop(value).round_to_float()
    return None


def zero_to_one(value):
    if value == 0:
        return 1
    return value


def compile_quiet_function(expr, arg_names, evaluation, expect_list):
    """
    Given an expression return a quiet callable version.
    Compiles the expression where possible.
    """
    if has_compile and not expect_list:
        try:
            cfunc = _compile(
                expr, [CompileArg(arg_name, real_type) for arg_name in arg_names]
            )
        except CompileError:
            pass
        else:

            def quiet_f(*args):
                try:
                    result = cfunc(*args)
                    if not (isnan(result) or isinf(result)):
                        return result
                except:
                    pass
                return None

            return quiet_f

    expr = Expression(SymbolN, expr)
    quiet_expr = Expression(
        "Quiet",
        expr,
        Expression(
            SymbolList, Expression("MessageName", Symbol("Power"), String("infy"))
        ),
    )

    def quiet_f(*args):
        vars = {arg_name: Real(arg) for arg_name, arg in zip(arg_names, args)}
        value = dynamic_scoping(quiet_expr.evaluate, vars, evaluation)
        if expect_list:
            if value.has_form("List", None):
                value = [extract_pyreal(item) for item in value.leaves]
                if any(item is None for item in value):
                    return None
                return value
            else:
                return None
        else:
            value = extract_pyreal(value)
            if value is None or isinf(value) or isnan(value):
                return None
            return value

    return quiet_f


def automatic_plot_range(values):
    """Calculates mean and standard deviation, throwing away all points
    which are more than 'thresh' number of standard deviations away from
    the mean. These are then used to find good vmin and vmax values. These
    values can then be used to find Automatic Plotrange."""

    if not values:
        return 0, 1

    thresh = 2.0
    values = sorted(values)
    valavg = sum(values) / len(values)
    valdev = sqrt(
        sum([(x - valavg) ** 2 for x in values]) / zero_to_one(len(values) - 1)
    )

    n1, n2 = 0, len(values) - 1
    if valdev != 0:
        for v in values:
            if abs(v - valavg) / valdev < thresh:
                break
            n1 += 1
        for v in values[::-1]:
            if abs(v - valavg) / valdev < thresh:
                break
            n2 -= 1

    vrange = values[n2] - values[n1]
    vmin = values[n1] - 0.05 * vrange  # 5% extra looks nice
    vmax = values[n2] + 0.05 * vrange
    return vmin, vmax


def get_plot_range(values, all_values, option):
    if option == "System`Automatic":
        result = automatic_plot_range(values)
    elif option == "System`All":
        if not all_values:
            result = [0, 1]
        else:
            result = min(all_values), max(all_values)
    else:
        result = option
    if result[0] == result[1]:
        value = result[0]
        if value > 0:
            return 0, value * 2
        if value < 0:
            return value * 2, 0
        return -1, 1
    return result


class _Plot(Builtin):
    from .graphics import Graphics

    attributes = ("HoldAll",)

    options = Graphics.options.copy()
    options.update(
        {
            "Axes": "True",
            "AspectRatio": "1 / GoldenRatio",
            "MaxRecursion": "Automatic",
            "Mesh": "None",
            "PlotRange": "Automatic",
            "PlotPoints": "None",
            "Exclusions": "Automatic",
            "$OptionSyntax": "Strict",
        }
    )

    messages = {
        "invmaxrec": (
            "MaxRecursion must be a non-negative integer; the recursion value "
            "is limited to `2`. Using MaxRecursion -> `1`."
        ),
        "prng": (
            "Value of option PlotRange -> `1` is not All, Automatic or "
            "an appropriate list of range specifications."
        ),
        "ppts": "Value of option PlotPoints -> `1` is not an integer >= 2.",
        "invexcl": (
            "Value of Exclusions -> `1` is not None, Automatic or an "
            "appropriate list of constraints."
        ),
    }

    expect_list = False

    def apply(self, functions, x, start, stop, evaluation, options):
        """%(name)s[functions_, {x_Symbol, start_, stop_},
        OptionsPattern[%(name)s]]"""
        if functions.is_symbol() and functions.name is not x.get_name():
            rules = evaluation.definitions.get_ownvalues(functions.name)
            for rule in rules:
                functions = rule.apply(functions, evaluation, fully=True)

        if functions.get_head_name() == "List":
            functions_param = self.get_functions_param(functions)
            for index, f in enumerate(functions_param):
                if f.is_symbol() and f.name is not x.get_name():
                    rules = evaluation.definitions.get_ownvalues(f.name)
                    for rule in rules:
                        f = rule.apply(f, evaluation, fully=True)
                functions_param[index] = f
            functions = functions.flatten(Symbol("List"))

        expr_limits = Expression(SymbolList, x, start, stop)
        expr = Expression(
            self.get_name(), functions, expr_limits, *options_to_rules(options)
        )
        functions = self.get_functions_param(functions)
        x_name = x.get_name()

        py_start = start.round_to_float(evaluation)
        py_stop = stop.round_to_float(evaluation)
        if py_start is None or py_stop is None:
            return evaluation.message(self.get_name(), "plln", stop, expr)
        if py_start >= py_stop:
            return evaluation.message(self.get_name(), "plld", expr_limits)
        start, stop = py_start, py_stop

        # PlotRange Option
        def check_range(range):
            if range in ("System`Automatic", "System`All"):
                return True
            if isinstance(range, list) and len(range) == 2:
                if isinstance(range[0], numbers.Real) and isinstance(  # noqa
                    range[1], numbers.Real
                ):
                    return True
            return False

        plotrange_option = self.get_option(options, "PlotRange", evaluation)
        plotrange = plotrange_option.to_python(n_evaluation=evaluation)
        x_range, y_range = self.get_plotrange(plotrange, start, stop)
        if not check_range(x_range) or not check_range(y_range):
            evaluation.message(self.get_name(), "prng", plotrange_option)
            x_range, y_range = [start, stop], "Automatic"

        # x_range and y_range are now either Automatic, All, or of the form [min, max]
        assert x_range in ("System`Automatic", "System`All") or isinstance(
            x_range, list
        )
        assert y_range in ("System`Automatic", "System`All") or isinstance(
            y_range, list
        )

        # Mesh Option
        mesh_option = self.get_option(options, "Mesh", evaluation)
        mesh = mesh_option.to_python()
        if mesh not in ["System`None", "System`Full", "System`All"]:
            evaluation.message("Mesh", "ilevels", mesh_option)
            mesh = "System`None"

        # PlotPoints Option
        plotpoints_option = self.get_option(options, "PlotPoints", evaluation)
        plotpoints = plotpoints_option.to_python()
        if plotpoints == "System`None":
            plotpoints = 57
        if not (isinstance(plotpoints, int) and plotpoints >= 2):
            return evaluation.message(self.get_name(), "ppts", plotpoints)

        # MaxRecursion Option
        max_recursion_limit = 15
        maxrecursion_option = self.get_option(options, "MaxRecursion", evaluation)
        maxrecursion = maxrecursion_option.to_python()
        try:
            if maxrecursion == "System`Automatic":
                maxrecursion = 3
            elif maxrecursion == float("inf"):
                maxrecursion = max_recursion_limit
                raise ValueError
            elif isinstance(maxrecursion, int):
                if maxrecursion > max_recursion_limit:
                    maxrecursion = max_recursion_limit
                    raise ValueError
                if maxrecursion < 0:
                    maxrecursion = 0
                    raise ValueError
            else:
                maxrecursion = 0
                raise ValueError
        except ValueError:
            evaluation.message(
                self.get_name(), "invmaxrec", maxrecursion, max_recursion_limit
            )
        assert isinstance(maxrecursion, int)

        # Exclusions Option
        # TODO: Make exclusions option work properly with ParametricPlot
        def check_exclusion(excl):
            if isinstance(excl, list):
                return all(check_exclusion(e) for e in excl)
            if excl == "System`Automatic":
                return True
            if not isinstance(excl, numbers.Real):
                return False
            return True

        exclusions_option = self.get_option(options, "Exclusions", evaluation)
        exclusions = exclusions_option.to_python(n_evaluation=evaluation)
        # TODO Turn expressions into points E.g. Sin[x] == 0 becomes 0, 2 Pi...

        if exclusions in ["System`None", ["System`None"]]:
            exclusions = "System`None"
        elif not isinstance(exclusions, list):
            exclusions = [exclusions]

            if isinstance(exclusions, list) and all(  # noqa
                check_exclusion(excl) for excl in exclusions
            ):
                pass

            else:
                evaluation.message(self.get_name(), "invexcl", exclusions_option)
                exclusions = ["System`Automatic"]

        # exclusions is now either 'None' or a list of reals and 'Automatic'
        assert exclusions == "System`None" or isinstance(exclusions, list)

        # constants to generate colors
        hue = 0.67
        hue_pos = 0.236068
        hue_neg = -0.763932

        def get_points_minmax(points):
            xmin = xmax = ymin = ymax = None
            for line in points:
                for x, y in line:
                    if xmin is None or x < xmin:
                        xmin = x
                    if xmax is None or x > xmax:
                        xmax = x
                    if ymin is None or y < ymin:
                        ymin = y
                    if ymax is None or y > ymax:
                        ymax = y
            return xmin, xmax, ymin, ymax

        def get_points_range(points):
            xmin, xmax, ymin, ymax = get_points_minmax(points)
            if xmin is None or xmax is None:
                xmin, xmax = 0, 1
            if ymin is None or ymax is None:
                ymin, ymax = 0, 1
            return zero_to_one(xmax - xmin), zero_to_one(ymax - ymin)

        function_hues = []
        base_plot_points = []  # list of points in base subdivision
        plot_points = []  # list of all plotted points
        mesh_points = []
        graphics = []  # list of resulting graphics primitives
        for index, f in enumerate(functions):
            points = []
            xvalues = []  # x value for each point in points
            tmp_mesh_points = []  # For this function only
            continuous = False
            d = (stop - start) / (plotpoints - 1)
            cf = compile_quiet_function(f, [x_name], evaluation, self.expect_list)
            for i in range(plotpoints):
                x_value = start + i * d
                point = self.eval_f(cf, x_value)
                if point is not None:
                    if continuous:
                        points[-1].append(point)
                        xvalues[-1].append(x_value)
                    else:
                        points.append([point])
                        xvalues.append([x_value])
                    continuous = True
                else:
                    continuous = False

            base_points = []
            for line in points:
                base_points.extend(line)
            base_plot_points.extend(base_points)

            xmin, xmax = automatic_plot_range([xx for xx, yy in base_points])
            xscale = 1.0 / zero_to_one(xmax - xmin)
            ymin, ymax = automatic_plot_range([yy for xx, yy in base_points])
            yscale = 1.0 / zero_to_one(ymax - ymin)

            if mesh == "System`Full":
                for line in points:
                    tmp_mesh_points.extend(line)

            def find_excl(excl):
                # Find which line the exclusion is in
                for l in range(len(xvalues)):  # TODO: Binary Search faster?
                    if xvalues[l][0] <= excl and xvalues[l][-1] >= excl:
                        break
                    if (
                        xvalues[l][-1] <= excl
                        and xvalues[min(l + 1, len(xvalues) - 1)][0] >= excl  # nopep8
                    ):
                        return min(l + 1, len(xvalues) - 1), 0, False
                xi = 0
                for xi in range(len(xvalues[l]) - 1):
                    if xvalues[l][xi] <= excl and xvalues[l][xi + 1] >= excl:
                        return l, xi + 1, True
                return l, xi + 1, False

            if exclusions != "System`None":
                for excl in exclusions:
                    if excl != "System`Automatic":
                        l, xi, split_required = find_excl(excl)
                        if split_required:
                            xvalues.insert(l + 1, xvalues[l][xi:])
                            xvalues[l] = xvalues[l][:xi]
                            points.insert(l + 1, points[l][xi:])
                            points[l] = points[l][:xi]
                    # assert(xvalues[l][-1] <= excl  <= xvalues[l+1][0])

            # Adaptive Sampling - loop again and interpolate highly angled
            # sections

            # Cos of the maximum angle between successive line segments
            ang_thresh = cos(pi / 180)

            for line, line_xvalues in zip(points, xvalues):
                recursion_count = 0
                smooth = False
                while not smooth and recursion_count < maxrecursion:
                    recursion_count += 1
                    smooth = True
                    i = 2
                    while i < len(line):
                        vec1 = (
                            xscale * (line[i - 1][0] - line[i - 2][0]),
                            yscale * (line[i - 1][1] - line[i - 2][1]),
                        )
                        vec2 = (
                            xscale * (line[i][0] - line[i - 1][0]),
                            yscale * (line[i][1] - line[i - 1][1]),
                        )
                        try:
                            angle = (vec1[0] * vec2[0] + vec1[1] * vec2[1]) / sqrt(
                                (vec1[0] ** 2 + vec1[1] ** 2)
                                * (vec2[0] ** 2 + vec2[1] ** 2)
                            )
                        except ZeroDivisionError:
                            angle = 0.0
                        if abs(angle) < ang_thresh:
                            smooth = False
                            incr = 0

                            x_value = 0.5 * (line_xvalues[i - 1] + line_xvalues[i])

                            point = self.eval_f(cf, x_value)
                            if point is not None:
                                line.insert(i, point)
                                line_xvalues.insert(i, x_value)
                                incr += 1

                            x_value = 0.5 * (line_xvalues[i - 2] + line_xvalues[i - 1])
                            point = self.eval_f(cf, x_value)
                            if point is not None:
                                line.insert(i - 1, point)
                                line_xvalues.insert(i - 1, x_value)
                                incr += 1

                            i += incr
                        i += 1

            if exclusions == "System`None":  # Join all the Lines
                points = [[(xx, yy) for line in points for xx, yy in line]]

            graphics.append(Expression("Hue", hue, 0.6, 0.6))
            graphics.append(Expression("Line", from_python(points)))

            for line in points:
                plot_points.extend(line)

            if mesh == "System`All":
                for line in points:
                    tmp_mesh_points.extend(line)

            if mesh != "System`None":
                mesh_points.append(tmp_mesh_points)

            function_hues.append(hue)

            if index % 4 == 0:
                hue += hue_pos
            else:
                hue += hue_neg
            if hue > 1:
                hue -= 1
            if hue < 0:
                hue += 1

        x_range = get_plot_range(
            [xx for xx, yy in base_plot_points], [xx for xx, yy in plot_points], x_range
        )
        y_range = get_plot_range(
            [yy for xx, yy in base_plot_points], [yy for xx, yy in plot_points], y_range
        )

        options["System`PlotRange"] = from_python([x_range, y_range])

        if mesh != "None":
            for hue, points in zip(function_hues, mesh_points):
                graphics.append(Expression("Hue", hue, 0.6, 0.6))
                meshpoints = [Expression(SymbolList, xx, yy) for xx, yy in points]
                graphics.append(
                    Expression("Point", Expression(SymbolList, *meshpoints))
                )

        return Expression(
            "Graphics", Expression(SymbolList, *graphics), *options_to_rules(options)
        )


class _Chart(Builtin):
    attributes = ("HoldAll",)

    from .graphics import Graphics

    options = Graphics.options.copy()
    options.update(
        {
            "Mesh": "None",
            "PlotRange": "Automatic",
            "ChartLabels": "None",
            "ChartLegends": "None",
            "ChartStyle": "Automatic",
        }
    )

    never_monochrome = False

    def _draw(self, data, color, evaluation, options):
        raise NotImplementedError()

    def apply(self, points, evaluation, options):
        "%(name)s[points_, OptionsPattern[%(name)s]]"

        points = points.evaluate(evaluation)

        if points.get_head_name() != "System`List" or not points.leaves:
            return

        if points.leaves[0].get_head_name() == "System`List":
            if not all(
                group.get_head_name() == "System`List" for group in points.leaves
            ):
                return
            multiple_colors = True
            groups = points.leaves
        else:
            multiple_colors = False
            groups = [points]

        chart_legends = self.get_option(options, "ChartLegends", evaluation)
        has_chart_legends = chart_legends.get_head_name() == "System`List"
        if has_chart_legends:
            multiple_colors = True

        def to_number(x):
            if isinstance(x, Integer):
                return float(x.get_int_value())
            return x.round_to_float(evaluation=evaluation)

        data = [[to_number(x) for x in group.leaves] for group in groups]

        chart_style = self.get_option(options, "ChartStyle", evaluation)
        if (
            isinstance(chart_style, Symbol)
            and chart_style.get_name() == "System`Automatic"
        ):
            chart_style = String("Automatic")

        if chart_style.get_head_name() == "System`List":
            colors = chart_style.leaves
            spread_colors = False
        elif isinstance(chart_style, String):
            if chart_style.get_string_value() == "Automatic":
                mpl_colors = palettable.wesanderson.Moonrise1_5.mpl_colors
            else:
                mpl_colors = ColorData.colors(chart_style.get_string_value())
                if mpl_colors is None:
                    return
                multiple_colors = True

            if not multiple_colors and not self.never_monochrome:
                colors = [Expression("RGBColor", *mpl_colors[0])]
            else:
                colors = [Expression("RGBColor", *c) for c in mpl_colors]
            spread_colors = True
        else:
            return

        def legends(names):
            if not data:
                return

            n = len(data[0])
            for d in data[1:]:
                if len(d) != n:
                    return  # data groups should have same size

            def box(color):
                return Expression(
                    "Graphics",
                    Expression(
                        "List", Expression("FaceForm", color), Expression("Rectangle")
                    ),
                    Expression(
                        SymbolRule, Symbol("ImageSize"), Expression(SymbolList, 50, 50)
                    ),
                )

            rows_per_col = 5

            n_cols = 1 + len(names) // rows_per_col
            if len(names) % rows_per_col == 0:
                n_cols -= 1

            if n_cols == 1:
                n_rows = len(names)
            else:
                n_rows = rows_per_col

            for i in range(n_rows):
                items = []
                for j in range(n_cols):
                    k = 1 + i + j * rows_per_col
                    if k - 1 < len(names):
                        items.extend([box(color(k, n)), names[k - 1]])
                    else:
                        items.extend([String(""), String("")])
                yield Expression(SymbolList, *items)

        def color(k, n):
            if spread_colors and n < len(colors):
                index = int(k * (len(colors) - 1)) // n
                return colors[index]
            else:
                return colors[(k - 1) % len(colors)]

        chart = self._draw(data, color, evaluation, options)

        if has_chart_legends:
            grid = Expression(
                "Grid", Expression(SymbolList, *list(legends(chart_legends.leaves)))
            )
            chart = Expression("Row", Expression(SymbolList, chart, grid))

        return chart


class PieChart(_Chart):
    """
    <dl>
    <dt>'PieChart[{$p1$, $p2$ ...}]'
        <dd>draws a pie chart.
    </dl>

    >> PieChart[{1, 4, 2}]
     = -Graphics-

    >> PieChart[{8, 16, 2}, SectorOrigin -> {Automatic, 1.5}]
     = -Graphics-

    >> PieChart[{{10, 20, 30}, {15, 22, 30}}]
     = -Graphics-

    >> PieChart[{{10, 20, 30}, {15, 22, 30}}, SectorSpacing -> None]
     = -Graphics-

    >> PieChart[{{10, 20, 30}, {15, 22, 30}}, ChartLabels -> {a, b, c}]
     = -Graphics-

    Negative values are clipped to 0.
    >> PieChart[{1, -1, 3}]
     = -Graphics-
    """

    options = _Chart.options.copy()
    options.update(
        {
            "Axes": "{False, False}",
            "AspectRatio": "1",
            "SectorOrigin": "{Automatic, 0}",
            "SectorSpacing": "Automatic",
        }
    )

    never_monochrome = True

    def _draw(self, data, color, evaluation, options):
        data = [[max(0.0, x) for x in group] for group in data]

        sector_origin = self.get_option(options, "SectorOrigin", evaluation)
        if not sector_origin.has_form("List", 2):
            return
        sector_origin = Expression(SymbolN, sector_origin).evaluate(evaluation)

        orientation = sector_origin.leaves[0]
        if (
            isinstance(orientation, Symbol)
            and orientation.get_name() == "System`Automatic"
        ):
            sector_phi = pi
            sector_sign = -1.0
        elif orientation.has_form("List", 2) and isinstance(
            orientation.leaves[1], String
        ):
            sector_phi = orientation.leaves[0].round_to_float()
            clock_name = orientation.leaves[1].get_string_value()
            if clock_name == "Clockwise":
                sector_sign = -1.0
            elif clock_name == "Counterclockwise":
                sector_sign = 1.0
            else:
                return
        else:
            return

        sector_spacing = self.get_option(options, "SectorSpacing", evaluation)
        if isinstance(sector_spacing, Symbol):
            if sector_spacing.get_name() == "System`Automatic":
                sector_spacing = Expression(SymbolList, Integer(0), Real(0.2))
            elif sector_spacing.get_name() == "System`None":
                sector_spacing = Expression(SymbolList, Integer(0), Integer(0))
            else:
                return
        if not sector_spacing.has_form("List", 2):
            return
        segment_spacing = 0.0  # not yet implemented; needs real arc graphics
        radius_spacing = max(0.0, min(1.0, sector_spacing.leaves[1].round_to_float()))

        def vector2(x, y):
            return Expression(SymbolList, Real(x), Real(y))

        def radii():
            outer = 2.0
            inner = sector_origin.leaves[1].round_to_float()
            n = len(data)

            d = (outer - inner) / n

            r0 = outer
            for i in range(n):
                r1 = r0 - d
                if i > 0:
                    r0 -= radius_spacing * d
                yield (r0, r1)
                r0 = r1

        def phis(values):
            s = sum(values)

            t = 0.0
            pi2 = pi * 2.0
            phi0 = pi
            spacing = sector_sign * segment_spacing / 2.0

            for k, value in enumerate(values):
                t += value
                phi1 = sector_phi + sector_sign * (t / s) * pi2

                yield (phi0 + spacing, phi1 - spacing)
                phi0 = phi1

        def segments():
            yield Expression("EdgeForm", Symbol("Black"))

            origin = vector2(0.0, 0.0)

            for values, (r0, r1) in zip(data, radii()):
                radius = vector2(r0, r0)

                n = len(values)

                for k, (phi0, phi1) in enumerate(phis(values)):
                    yield Expression(
                        "Style",
                        Expression("Disk", origin, radius, vector2(phi0, phi1)),
                        color(k + 1, n),
                    )

                if r1 > 0.0:
                    yield Expression(
                        "Style",
                        Expression("Disk", origin, vector2(r1, r1)),
                        Symbol("White"),
                    )

        def labels(names):
            yield Expression("FaceForm", Symbol("Black"))

            for values, (r0, r1) in zip(data, radii()):
                for name, (phi0, phi1) in zip(names, phis(values)):
                    r = (r0 + r1) / 2.0
                    phi = (phi0 + phi1) / 2.0
                    yield Expression("Text", name, vector2(r * cos(phi), r * sin(phi)))

        graphics = list(segments())

        chart_labels = self.get_option(options, "ChartLabels", evaluation)
        if chart_labels.get_head_name() == "System`List":
            graphics.extend(list(labels(chart_labels.leaves)))

        options["System`PlotRange"] = Expression(
            "List", vector2(-2.0, 2.0), vector2(-2.0, 2.0)
        )

        return Expression(
            "Graphics", Expression(SymbolList, *graphics), *options_to_rules(options)
        )


class BarChart(_Chart):
    """
    <dl>
        <dt>'BarChart[{$b1$, $b2$ ...}]'
        <dd>makes a bar chart with lengths $b1$, $b2$, ....
    </dl>

    >> BarChart[{1, 4, 2}]
     = -Graphics-

    >> BarChart[{1, 4, 2}, ChartStyle -> {Red, Green, Blue}]
     = -Graphics-

    >> BarChart[{{1, 2, 3}, {2, 3, 4}}]
     = -Graphics-

    >> BarChart[{{1, 2, 3}, {2, 3, 4}}, ChartLabels -> {"a", "b", "c"}]
     = -Graphics-

    >> BarChart[{{1, 5}, {3, 4}}, ChartStyle -> {{EdgeForm[Thin], White}, {EdgeForm[Thick], White}}]
     = -Graphics-
    """

    options = _Chart.options.copy()
    options.update(
        {"Axes": "{False, True}", "AspectRatio": "1 / GoldenRatio",}
    )

    def _draw(self, data, color, evaluation, options):
        def vector2(x, y):
            return Expression(SymbolList, Real(x), Real(y))

        def boxes():
            w = 0.9
            s = 0.06
            w_half = 0.5 * w
            x = 0.1 + s + w_half

            for y_values in data:
                y_length = len(y_values)
                for i, y in enumerate(y_values):
                    x0 = x - w_half
                    x1 = x0 + w
                    yield (i + 1, y_length), x0, x1, y
                    x = x1 + s + w_half

                x += 0.2

        def rectangles():
            yield Expression("EdgeForm", Symbol("Black"))

            last_x1 = 0

            for (k, n), x0, x1, y in boxes():
                yield Expression(
                    "Style",
                    Expression(
                        "Rectangle",
                        Expression(SymbolList, x0, 0),
                        Expression(SymbolList, x1, y),
                    ),
                    color(k, n),
                )

                last_x1 = x1

            yield Expression(
                "Line", Expression(SymbolList, vector2(0, 0), vector2(last_x1, 0))
            )

        def axes():
            yield Expression("FaceForm", Symbol("Black"))

            def points(x):
                return Expression(SymbolList, vector2(x, 0), vector2(x, -0.2))

            for (k, n), x0, x1, y in boxes():
                if k == 1:
                    yield Expression("Line", points(x0))
                if k == n:
                    yield Expression("Line", points(x1))

        def labels(names):
            yield Expression("FaceForm", Symbol("Black"))

            for (k, n), x0, x1, y in boxes():
                if k <= len(names):
                    name = names[k - 1]
                    yield Expression("Text", name, vector2((x0 + x1) / 2, -0.2))

        x_coords = list(itertools.chain(*[[x0, x1] for (k, n), x0, x1, y in boxes()]))
        y_coords = [0] + [y for (k, n), x0, x1, y in boxes()]

        graphics = list(rectangles()) + list(axes())

        x_range = "System`All"
        y_range = "System`All"

        x_range = list(get_plot_range(x_coords, x_coords, x_range))
        y_range = list(get_plot_range(y_coords, y_coords, y_range))

        chart_labels = self.get_option(options, "ChartLabels", evaluation)
        if chart_labels.get_head_name() == "System`List":
            graphics.extend(list(labels(chart_labels.leaves)))
            y_range[0] = -0.4  # room for labels at the bottom

        # always specify -.1 as the minimum x plot range, as this will make the y axis apppear
        # at origin (0,0); otherwise it will be shifted right; see GraphicsBox.axis_ticks().
        x_range[0] = -0.1

        options["System`PlotRange"] = Expression(
            "List", vector2(*x_range), vector2(*y_range)
        )

        return Expression(
            "Graphics", Expression(SymbolList, *graphics), *options_to_rules(options)
        )


class Histogram(Builtin):
    """
    <dl>
        <dt>'Histogram[{$x1$, $x2$ ...}]'
        <dd>plots a histogram using the values $x1$, $x2$, ....
    </dl>

    >> Histogram[{3, 8, 10, 100, 1000, 500, 300, 200, 10, 20, 200, 100, 200, 300, 500}]
     = -Graphics-

    >> Histogram[{{1, 2, 10, 5, 50, 20}, {90, 100, 101, 120, 80}}]
     = -Graphics-
    """

    from .graphics import Graphics

    attributes = ("HoldAll",)

    options = Graphics.options.copy()
    options.update(
        {
            "Axes": "{True, True}",
            "AspectRatio": "1 / GoldenRatio",
            "Mesh": "None",
            "PlotRange": "Automatic",
        }
    )

    def apply(self, points, spec, evaluation, options):
        "%(name)s[points_, spec___, OptionsPattern[%(name)s]]"

        points = points.evaluate(evaluation)
        spec = spec.evaluate(evaluation).get_sequence()

        if spec and len(spec) not in (1, 2):
            return

        if points.get_head_name() != "System`List" or not points.leaves:
            return

        if points.leaves[0].get_head_name() == "System`List":
            if not all(q.get_head_name() == "System`List" for q in points.leaves):
                return
            input = points.leaves
        else:
            input = [points]

        def to_numbers(l):
            for x in l:
                y = x.to_mpmath()
                if y is not None:
                    yield y

        matrix = [list(to_numbers(data.leaves)) for data in input]
        minima = [min(data) for data in matrix]
        maxima = [max(data) for data in matrix]
        max_bins = max(len(data) for data in matrix)

        minimum = min(minima)
        maximum = max(maxima)

        if minimum > 0:
            minimum = 0

        span = maximum - minimum

        from math import ceil
        from mpmath import floor as mpfloor, ceil as mpceil

        class Distribution:
            def __init__(self, data, n_bins):
                bin_width = span / n_bins
                bins = [0] * n_bins
                for x in data:
                    b = int(mpfloor((x - minimum) / bin_width))
                    if b < 0:
                        b = 0
                    elif b >= n_bins:
                        b = n_bins - 1
                    bins[b] += 1
                self.bins = bins
                self.bin_width = bin_width

            def n_bins(self):
                return len(self.bins)

            def cost(self):
                # see http://toyoizumilab.brain.riken.jp/hideaki/res/histogram.html
                bins = self.bins
                n_bins = len(bins)
                k = sum(bins) / n_bins
                v = sum(x * x for x in ((b - k) for b in bins)) / n_bins
                bin_width = self.bin_width
                return (2 * k - v) / (bin_width * bin_width)

            def graphics(self, color):
                bins = self.bins
                n_bins = len(bins)
                bin_width = self.bin_width

                def boxes():
                    x = minimum

                    for i, count in enumerate(bins):
                        x1 = x + bin_width
                        yield x, x1, count
                        x = minimum + ((i + 1) * span) / n_bins

                def rectangles():
                    yield Expression("EdgeForm", Expression("RGBColor", 0, 0, 0))

                    last_x1 = 0
                    style = Expression("RGBColor", *color)

                    for x0, x1, y in boxes():
                        yield Expression(
                            "Style",
                            Expression(
                                "Rectangle",
                                Expression(SymbolList, x0, 0),
                                Expression(SymbolList, x1, y),
                            ),
                            style,
                        )

                        last_x1 = x1

                    yield Expression(
                        "Line",
                        Expression(
                            "List",
                            Expression(SymbolList, 0, 0),
                            Expression(SymbolList, last_x1, 0),
                        ),
                    )

                return list(rectangles())

        def compute_cost(n_bins):
            distributions = [Distribution(data, n_bins) for data in matrix]
            return sum(d.cost() for d in distributions), distributions

        def best_distributions(n_bins, dir, cost0, distributions0):
            if dir > 0:
                step_size = (max_bins - n_bins) // 2
            else:
                step_size = (n_bins - 1) // 2
            if step_size < 1:
                step_size = 1

            while True:
                new_n_bins = n_bins + dir * step_size
                if new_n_bins < 1 or new_n_bins > max_bins:
                    good = False
                else:
                    cost, distributions = compute_cost(new_n_bins)
                    good = cost < cost0

                if not good:
                    if step_size == 1:
                        break
                    step_size = max(step_size // 2, 1)
                else:
                    n_bins = new_n_bins
                    cost0 = cost
                    distributions0 = distributions

            return cost0, distributions0

        def graphics(distributions):
            palette = palettable.wesanderson.FantasticFox1_5
            colors = list(reversed(palette.mpl_colors))

            from itertools import chain

            n_bins = distributions[0].n_bins()
            x_coords = [minimum + (i * span) / n_bins for i in range(n_bins + 1)]
            y_coords = [0] + list(
                chain(*[distribution.bins for distribution in distributions])
            )

            graphics = []
            for i, distribution in enumerate(distributions):
                color = colors[i % len(colors)]
                graphics.extend(list(chain(*[distribution.graphics(color)])))

            x_range = "System`All"
            y_range = "System`All"

            x_range = list(get_plot_range(x_coords, x_coords, x_range))
            y_range = list(get_plot_range(y_coords, y_coords, y_range))

            # always specify -.1 as the minimum x plot range, as this will make the y axis apppear
            # at origin (0,0); otherwise it will be shifted right; see GraphicsBox.axis_ticks().
            x_range[0] = -0.1

            options["System`PlotRange"] = from_python([x_range, y_range])

            return Expression(
                "Graphics",
                Expression(SymbolList, *graphics),
                *options_to_rules(options)
            )

        def manual_bins(bspec, hspec):
            if isinstance(bspec, Integer):
                distributions = [
                    Distribution(data, bspec.get_int_value()) for data in matrix
                ]
                return graphics(distributions)
            elif bspec.get_head_name() == "System`List" and len(bspec.leaves) == 1:
                bin_width = bspec[0].to_mpmath()
                distributions = [
                    Distribution(data, int(mpceil(span / bin_width))) for data in matrix
                ]
                return graphics(distributions)

        def auto_bins():
            # start with Rice's rule, see https://en.wikipedia.org/wiki/Histogram
            n_bins = int(ceil(2 * (max_bins ** (1.0 / 3.0))))

            # now optimize the bin size by going into both directions and looking
            # for local minima.

            cost0, distributions0 = compute_cost(n_bins)
            cost_r, distributions_r = best_distributions(
                n_bins, 1, cost0, distributions0
            )
            cost_l, distributions_l = best_distributions(
                n_bins, -1, cost0, distributions0
            )

            if cost_r < cost_l:
                distributions = distributions_r
            else:
                distributions = distributions_l

            return graphics(distributions)

        if not spec:
            return auto_bins()
        else:
            if len(spec) < 2:
                spec.append(None)
            return manual_bins(*spec)
        return Expression(
            "Graphics",
            Expression(SymbolList, *graphics),
            *options_to_rules(options, Graphics.options)
        )


class _ListPlot(Builtin):
    messages = {
        "prng": (
            "Value of option PlotRange -> `1` is not All, Automatic or "
            "an appropriate list of range specifications."
        ),
        "joind": "Value of option Joined -> `1` is not True or False.",
    }

    def apply(self, points, evaluation, options):
        "%(name)s[points_, OptionsPattern[%(name)s]]"

        plot_name = self.get_name()
        all_points = points.to_python(n_evaluation=evaluation)
        expr = Expression(self.get_name(), points, *options_to_rules(options))

        # PlotRange Option
        def check_range(range):
            if range in ("System`Automatic", "System`All"):
                return True
            if isinstance(range, list) and len(range) == 2:
                if isinstance(range[0], numbers.Real) and isinstance(  # noqa
                    range[1], numbers.Real
                ):
                    return True
            return False

        plotrange_option = self.get_option(options, "PlotRange", evaluation)
        plotrange = plotrange_option.to_python(n_evaluation=evaluation)
        if plotrange == "System`All":
            plotrange = ["System`All", "System`All"]
        elif plotrange == "System`Automatic":
            plotrange = ["System`Automatic", "System`Automatic"]
        elif isinstance(plotrange, numbers.Real):
            plotrange = [[-plotrange, plotrange], [-plotrange, plotrange]]
        elif isinstance(plotrange, list) and len(plotrange) == 2:
            if all(isinstance(pr, numbers.Real) for pr in plotrange):
                plotrange = ["System`All", plotrange]
            elif all(check_range(pr) for pr in plotrange):
                pass
        else:
            evaluation.message(self.get_name(), "prng", plotrange_option)
            plotrange = ["System`Automatic", "System`Automatic"]

        x_range, y_range = plotrange[0], plotrange[1]
        assert x_range in ("System`Automatic", "System`All") or isinstance(
            x_range, list
        )
        assert y_range in ("System`Automatic", "System`All") or isinstance(
            y_range, list
        )

        # Filling option
        # TODO: Fill between corresponding points in two datasets:
        filling_option = self.get_option(options, "Filling", evaluation)
        filling = filling_option.to_python(n_evaluation=evaluation)
        if filling in [
            "System`Top",
            "System`Bottom",
            "System`Axis",
        ] or isinstance(  # noqa
            filling, numbers.Real
        ):
            pass
        else:
            # Mathematica does not even check that filling is sane
            filling = None

        # Joined Option
        joined_option = self.get_option(options, "Joined", evaluation)
        joined = joined_option.to_python()
        if joined not in [True, False]:
            evaluation.message(plot_name, "joind", joined_option, expr)
            joined = False

        if isinstance(all_points, list) and len(all_points) != 0:
            if all(not isinstance(point, list) for point in all_points):
                # Only y values given
                all_points = [
                    [[float(i + 1), all_points[i]] for i in range(len(all_points))]
                ]
            elif all(isinstance(line, list) and len(line) == 2 for line in all_points):
                # Single list of (x,y) pairs
                all_points = [all_points]
            elif all(isinstance(line, list) for line in all_points):
                # List of lines
                if all(
                    isinstance(point, list) and len(point) == 2
                    for line in all_points
                    for point in line
                ):
                    pass
                elif all(
                    not isinstance(point, list) for line in all_points for point in line
                ):
                    all_points = [
                        [[float(i + 1), l] for i, l in enumerate(line)]
                        for line in all_points
                    ]
                else:
                    return
            else:
                return
        else:
            return

        # Split into segments at missing data
        all_points = [[line] for line in all_points]
        for l, line in enumerate(all_points):
            i = 0
            while i < len(all_points[l]):
                seg = line[i]
                for j, point in enumerate(seg):
                    if not (
                        isinstance(point[0], (int, float))
                        and isinstance(point[1], (int, float))
                    ):
                        all_points[l].insert(i, seg[:j])
                        all_points[l][i + 1] = seg[j + 1 :]
                        i -= 1
                        break

                i += 1

        y_range = get_plot_range(
            [y for line in all_points for seg in line for x, y in seg],
            [y for line in all_points for seg in line for x, y in seg],
            y_range,
        )
        x_range = get_plot_range(
            [x for line in all_points for seg in line for x, y in seg],
            [x for line in all_points for seg in line for x, y in seg],
            x_range,
        )

        if filling == "System`Axis":
            # TODO: Handle arbitary axis intercepts
            filling = 0.0
        elif filling == "System`Bottom":
            filling = y_range[0]
        elif filling == "System`Top":
            filling = y_range[1]

        hue = 0.67
        hue_pos = 0.236068
        hue_neg = -0.763932

        graphics = []
        for indx, line in enumerate(all_points):
            graphics.append(Expression("Hue", hue, 0.6, 0.6))
            for segment in line:
                if joined:
                    graphics.append(Expression("Line", from_python(segment)))
                    if filling is not None:
                        graphics.append(Expression("Hue", hue, 0.6, 0.6, 0.2))
                        fill_area = list(segment)
                        fill_area.append([segment[-1][0], filling])
                        fill_area.append([segment[0][0], filling])
                        graphics.append(Expression("Polygon", from_python(fill_area)))
                else:
                    graphics.append(Expression("Point", from_python(segment)))
                    if filling is not None:
                        for point in segment:
                            graphics.append(
                                Expression(
                                    "Line",
                                    from_python(
                                        [[point[0], filling], [point[0], point[1]]]
                                    ),
                                )
                            )

            if indx % 4 == 0:
                hue += hue_pos
            else:
                hue += hue_neg
            if hue > 1:
                hue -= 1
            if hue < 0:
                hue += 1

        options["System`PlotRange"] = from_python([x_range, y_range])

        return Expression(
            "Graphics",
            Expression(SymbolList, *graphics),
            *options_to_rules(options, Graphics.options)
        )


class _Plot3D(Builtin):
    messages = {
        "invmaxrec": (
            "MaxRecursion must be a non-negative integer; the recursion value "
            "is limited to `2`. Using MaxRecursion -> `1`."
        ),
        "prng": (
            "Value of option PlotRange -> `1` is not All, Automatic or "
            "an appropriate list of range specifications."
        ),
        "invmesh": "Mesh must be one of {None, Full, All}. Using Mesh->None.",
        "invpltpts": (
            "Value of PlotPoints -> `1` is not a positive integer "
            "or appropriate list of positive integers."
        ),
    }

    def apply(self, functions, x, xstart, xstop, y, ystart, ystop, evaluation, options):
        """%(name)s[functions_, {x_Symbol, xstart_, xstop_},
        {y_Symbol, ystart_, ystop_}, OptionsPattern[%(name)s]]"""
        xexpr_limits = Expression(SymbolList, x, xstart, xstop)
        yexpr_limits = Expression(SymbolList, y, ystart, ystop)
        expr = Expression(
            self.get_name(),
            functions,
            xexpr_limits,
            yexpr_limits,
            *options_to_rules(options)
        )

        functions = self.get_functions_param(functions)
        plot_name = self.get_name()

        def convert_limit(value, limits):
            result = value.round_to_float(evaluation)
            if result is None:
                evaluation.message(plot_name, "plln", value, limits)
            return result

        xstart = convert_limit(xstart, xexpr_limits)
        xstop = convert_limit(xstop, xexpr_limits)
        ystart = convert_limit(ystart, yexpr_limits)
        ystop = convert_limit(ystop, yexpr_limits)
        if None in (xstart, xstop, ystart, ystop):
            return

        if ystart >= ystop:
            evaluation.message(plot_name, "plln", ystop, expr)
            return

        if xstart >= xstop:
            evaluation.message(plot_name, "plln", xstop, expr)
            return

        # Mesh Option
        mesh_option = self.get_option(options, "Mesh", evaluation)
        mesh = mesh_option.to_python()
        if mesh not in ["System`None", "System`Full", "System`All"]:
            evaluation.message("Mesh", "ilevels", mesh_option)
            mesh = "System`Full"

        # PlotPoints Option
        plotpoints_option = self.get_option(options, "PlotPoints", evaluation)
        plotpoints = plotpoints_option.to_python()

        def check_plotpoints(steps):
            if isinstance(steps, int) and steps > 0:
                return True
            return False

        if plotpoints == "System`None":
            plotpoints = [7, 7]
        elif check_plotpoints(plotpoints):
            plotpoints = [plotpoints, plotpoints]

        if not (
            isinstance(plotpoints, list)
            and len(plotpoints) == 2
            and check_plotpoints(plotpoints[0])
            and check_plotpoints(plotpoints[1])
        ):
            evaluation.message(self.get_name(), "invpltpts", plotpoints)
            plotpoints = [7, 7]

        # MaxRecursion Option
        maxrec_option = self.get_option(options, "MaxRecursion", evaluation)
        max_depth = maxrec_option.to_python()
        if isinstance(max_depth, int):
            if max_depth < 0:
                max_depth = 0
                evaluation.message(self.get_name(), "invmaxrec", max_depth, 15)
            elif max_depth > 15:
                max_depth = 15
                evaluation.message(self.get_name(), "invmaxrec", max_depth, 15)
            else:
                pass  # valid
        elif max_depth == float("inf"):
            max_depth = 15
            evaluation.message(self.get_name(), "invmaxrec", max_depth, 15)
        else:
            max_depth = 0
            evaluation.message(self.get_name(), "invmaxrec", max_depth, 15)

        # Plot the functions
        graphics = []
        for indx, f in enumerate(functions):
            stored = {}

            cf = compile_quiet_function(
                f, [x.get_name(), y.get_name()], evaluation, False
            )

            def eval_f(x_value, y_value):
                try:
                    return stored[(x_value, y_value)]
                except KeyError:
                    value = cf(x_value, y_value)
                    if value is not None:
                        value = float(value)
                    stored[(x_value, y_value)] = value
                    return value

            triangles = []

            split_edges = set([])  # subdivided edges

            def triangle(x1, y1, x2, y2, x3, y3, depth=0):
                v1, v2, v3 = eval_f(x1, y1), eval_f(x2, y2), eval_f(x3, y3)

                if (v1 is v2 is v3 is None) and (depth > max_depth // 2):
                    # fast finish because the entire region is undefined but
                    # recurse 'a little' to avoid missing well defined regions
                    return
                elif v1 is None or v2 is None or v3 is None:
                    # 'triforce' pattern recursion to find the edge of defined region
                    #         1
                    #         /\
                    #      4 /__\ 6
                    #       /\  /\
                    #      /__\/__\
                    #     2   5    3
                    if depth < max_depth:
                        x4, y4 = 0.5 * (x1 + x2), 0.5 * (y1 + y2)
                        x5, y5 = 0.5 * (x2 + x3), 0.5 * (y2 + y3)
                        x6, y6 = 0.5 * (x1 + x3), 0.5 * (y1 + y3)
                        split_edges.add(
                            ((x1, y1), (x2, y2))
                            if (x2, y2) > (x1, y1)
                            else ((x2, y2), (x1, y1))
                        )
                        split_edges.add(
                            ((x2, y2), (x3, y3))
                            if (x3, y3) > (x2, y2)
                            else ((x3, y3), (x2, y2))
                        )
                        split_edges.add(
                            ((x1, y1), (x3, y3))
                            if (x3, y3) > (x1, y1)
                            else ((x3, y3), (x1, y1))
                        )
                        triangle(x1, y1, x4, y4, x6, y6, depth + 1)
                        triangle(x4, y4, x2, y2, x5, y5, depth + 1)
                        triangle(x6, y6, x5, y5, x3, y3, depth + 1)
                        triangle(x4, y4, x5, y5, x6, y6, depth + 1)
                    return
                triangles.append(sorted(((x1, y1, v1), (x2, y2, v2), (x3, y3, v3))))

            # linear (grid) sampling
            numx = plotpoints[0] * 1.0
            numy = plotpoints[1] * 1.0
            for xi in range(plotpoints[0]):
                for yi in range(plotpoints[1]):
                    # Decide which way to break the square grid into triangles
                    # by looking at diagonal lengths.
                    #
                    # 3___4        3___4
                    # |\  |        |  /|
                    # | \ | versus | / |
                    # |__\|        |/__|
                    # 1   2        1   2
                    #
                    # Approaching the boundary of the well defined region is
                    # important too. Use first stategy if 1 or 4 are undefined
                    # and stategy 2 if either 2 or 3 are undefined.
                    #
                    (x1, x2, x3, x4) = (
                        xstart + value * (xstop - xstart)
                        for value in (
                            xi / numx,
                            (xi + 1) / numx,
                            xi / numx,
                            (xi + 1) / numx,
                        )
                    )
                    (y1, y2, y3, y4) = (
                        ystart + value * (ystop - ystart)
                        for value in (
                            yi / numy,
                            yi / numy,
                            (yi + 1) / numy,
                            (yi + 1) / numy,
                        )
                    )

                    v1 = eval_f(x1, y1)
                    v2 = eval_f(x2, y2)
                    v3 = eval_f(x3, y3)
                    v4 = eval_f(x4, y4)

                    if v1 is None or v4 is None:
                        triangle(x1, y1, x2, y2, x3, y3)
                        triangle(x4, y4, x3, y3, x2, y2)
                    elif v2 is None or v3 is None:
                        triangle(x2, y2, x1, y1, x4, y4)
                        triangle(x3, y3, x4, y4, x1, y1)
                    else:
                        if abs(v3 - v2) > abs(v4 - v1):
                            triangle(x2, y2, x1, y1, x4, y4)
                            triangle(x3, y3, x4, y4, x1, y1)
                        else:
                            triangle(x1, y1, x2, y2, x3, y3)
                            triangle(x4, y4, x3, y3, x2, y2)

            # adaptive resampling
            # TODO: optimise this
            # Cos of the maximum angle between successive line segments
            ang_thresh = cos(20 * pi / 180)
            for depth in range(1, max_depth):
                needs_removal = set([])
                lent = len(triangles)  # number of initial triangles
                for i1 in range(lent):
                    for i2 in range(lent):
                        # find all edge pairings
                        if i1 == i2:
                            continue
                        t1 = triangles[i1]
                        t2 = triangles[i2]

                        edge_pairing = (
                            (t1[0], t1[1]) == (t2[0], t2[1])
                            or (t1[0], t1[1]) == (t2[1], t2[2])
                            or (t1[0], t1[1]) == (t2[0], t2[2])
                            or (t1[1], t1[2]) == (t2[0], t2[1])
                            or (t1[1], t1[2]) == (t2[1], t2[2])
                            or (t1[1], t1[2]) == (t2[0], t2[2])
                            or (t1[0], t1[2]) == (t2[0], t2[1])
                            or (t1[0], t1[2]) == (t2[1], t2[2])
                            or (t1[0], t1[2]) == (t2[0], t2[2])
                        )
                        if not edge_pairing:
                            continue
                        v1 = [t1[1][i] - t1[0][i] for i in range(3)]
                        w1 = [t1[2][i] - t1[0][i] for i in range(3)]
                        v2 = [t2[1][i] - t2[0][i] for i in range(3)]
                        w2 = [t2[2][i] - t2[0][i] for i in range(3)]
                        n1 = (  # surface normal for t1
                            (v1[1] * w1[2]) - (v1[2] * w1[1]),
                            (v1[2] * w1[0]) - (v1[0] * w1[2]),
                            (v1[0] * w1[1]) - (v1[1] * w1[0]),
                        )
                        n2 = (  # surface normal for t2
                            (v2[1] * w2[2]) - (v2[2] * w2[1]),
                            (v2[2] * w2[0]) - (v2[0] * w2[2]),
                            (v2[0] * w2[1]) - (v2[1] * w2[0]),
                        )
                        try:
                            angle = (
                                n1[0] * n2[0] + n1[1] * n2[1] + n1[2] * n2[2]
                            ) / sqrt(
                                (n1[0] ** 2 + n1[1] ** 2 + n1[2] ** 2)
                                * (n2[0] ** 2 + n2[1] ** 2 + n2[2] ** 2)
                            )
                        except ZeroDivisionError:
                            angle = 0.0
                        if abs(angle) < ang_thresh:
                            for i, t in ((i1, t1), (i2, t2)):
                                # subdivide
                                x1, y1 = t[0][0], t[0][1]
                                x2, y2 = t[1][0], t[1][1]
                                x3, y3 = t[2][0], t[2][1]
                                x4, y4 = 0.5 * (x1 + x2), 0.5 * (y1 + y2)
                                x5, y5 = 0.5 * (x2 + x3), 0.5 * (y2 + y3)
                                x6, y6 = 0.5 * (x1 + x3), 0.5 * (y1 + y3)
                                needs_removal.add(i)
                                split_edges.add(
                                    ((x1, y1), (x2, y2))
                                    if (x2, y2) > (x1, y1)
                                    else ((x2, y2), (x1, y1))
                                )
                                split_edges.add(
                                    ((x2, y2), (x3, y3))
                                    if (x3, y3) > (x2, y2)
                                    else ((x3, y3), (x2, y2))
                                )
                                split_edges.add(
                                    ((x1, y1), (x3, y3))
                                    if (x3, y3) > (x1, y1)
                                    else ((x3, y3), (x1, y1))
                                )
                                triangle(x1, y1, x4, y4, x6, y6, depth=depth)
                                triangle(x2, y2, x4, y4, x5, y5, depth=depth)
                                triangle(x3, y3, x5, y5, x6, y6, depth=depth)
                                triangle(x4, y4, x5, y5, x6, y6, depth=depth)
                # remove subdivided triangles which have been divided
                triangles = [
                    t for i, t in enumerate(triangles) if i not in needs_removal
                ]

            # fix up subdivided edges
            #
            # look at every triangle and see if its edges need updating.
            # depending on how many edges require subdivision we proceede with
            # one of two subdivision strategies
            #
            # TODO possible optimisation: don't look at every triangle again
            made_changes = True
            while made_changes:
                made_changes = False
                new_triangles = []
                for i, t in enumerate(triangles):
                    new_points = []
                    if ((t[0][0], t[0][1]), (t[1][0], t[1][1])) in split_edges:
                        new_points.append([0, 1])
                    if ((t[1][0], t[1][1]), (t[2][0], t[2][1])) in split_edges:
                        new_points.append([1, 2])
                    if ((t[0][0], t[0][1]), (t[2][0], t[2][1])) in split_edges:
                        new_points.append([0, 2])

                    if len(new_points) == 0:
                        continue
                    made_changes = True
                    # 'triforce' subdivision
                    #         1
                    #         /\
                    #      4 /__\ 6
                    #       /\  /\
                    #      /__\/__\
                    #     2   5    3
                    # if less than three edges require subdivision bisect them
                    # anyway but fake their values by averaging
                    x4 = 0.5 * (t[0][0] + t[1][0])
                    y4 = 0.5 * (t[0][1] + t[1][1])
                    v4 = stored.get((x4, y4), 0.5 * (t[0][2] + t[1][2]))

                    x5 = 0.5 * (t[1][0] + t[2][0])
                    y5 = 0.5 * (t[1][1] + t[2][1])
                    v5 = stored.get((x5, y5), 0.5 * (t[1][2] + t[2][2]))

                    x6 = 0.5 * (t[0][0] + t[2][0])
                    y6 = 0.5 * (t[0][1] + t[2][1])
                    v6 = stored.get((x6, y6), 0.5 * (t[0][2] + t[2][2]))

                    if not (v4 is None or v6 is None):
                        new_triangles.append(sorted((t[0], (x4, y4, v4), (x6, y6, v6))))
                    if not (v4 is None or v5 is None):
                        new_triangles.append(sorted((t[1], (x4, y4, v4), (x5, y5, v5))))
                    if not (v5 is None or v6 is None):
                        new_triangles.append(sorted((t[2], (x5, y5, v5), (x6, y6, v6))))
                    if not (v4 is None or v5 is None or v6 is None):
                        new_triangles.append(
                            sorted(((x4, y4, v4), (x5, y5, v5), (x6, y6, v6)))
                        )
                    triangles[i] = None

                triangles.extend(new_triangles)
                triangles = [t for t in triangles if t is not None]

            # add the mesh
            mesh_points = []
            if mesh == "System`Full":
                for xi in range(plotpoints[0] + 1):
                    xval = xstart + xi / numx * (xstop - xstart)
                    mesh_row = []
                    for yi in range(plotpoints[1] + 1):
                        yval = ystart + yi / numy * (ystop - ystart)
                        z = stored[(xval, yval)]
                        mesh_row.append((xval, yval, z))
                    mesh_points.append(mesh_row)

                for yi in range(plotpoints[1] + 1):
                    yval = ystart + yi / numy * (ystop - ystart)
                    mesh_col = []
                    for xi in range(plotpoints[0] + 1):
                        xval = xstart + xi / numx * (xstop - xstart)
                        z = stored[(xval, yval)]
                        mesh_col.append((xval, yval, z))
                    mesh_points.append(mesh_col)

                # handle edge subdivisions
                made_changes = True
                while made_changes:
                    made_changes = False
                    for mesh_line in mesh_points:
                        i = 0
                        while i < len(mesh_line) - 1:
                            x1, y1, v1 = mesh_line[i]
                            x2, y2, v2 = mesh_line[i + 1]
                            key = (
                                ((x1, y1), (x2, y2))
                                if (x2, y2) > (x1, y1)
                                else ((x2, y2), (x1, y1))
                            )
                            if key in split_edges:
                                x3 = 0.5 * (x1 + x2)
                                y3 = 0.5 * (y1 + y2)
                                v3 = stored[(x3, y3)]
                                mesh_line.insert(i + 1, (x3, y3, v3))
                                made_changes = True
                                i += 1
                            i += 1

                # handle missing regions
                old_meshpoints, mesh_points = mesh_points, []
                for mesh_line in old_meshpoints:
                    mesh_points.extend(
                        [
                            sorted(g)
                            for k, g in itertools.groupby(
                                mesh_line, lambda x: x[2] is None
                            )
                        ]
                    )
                mesh_points = [
                    mesh_line
                    for mesh_line in mesh_points
                    if not any(x[2] is None for x in mesh_line)
                ]
            elif mesh == "System`All":
                mesh_points = set([])
                for t in triangles:
                    mesh_points.add((t[0], t[1]) if t[1] > t[0] else (t[1], t[0]))
                    mesh_points.add((t[1], t[2]) if t[2] > t[1] else (t[2], t[1]))
                    mesh_points.add((t[0], t[2]) if t[2] > t[0] else (t[2], t[0]))
                mesh_points = list(mesh_points)

            # find the max and min height
            v_min = v_max = None
            for t in triangles:
                for tx, ty, v in t:
                    if v_min is None or v < v_min:
                        v_min = v
                    if v_max is None or v > v_max:
                        v_max = v
            graphics.extend(
                self.construct_graphics(
                    triangles, mesh_points, v_min, v_max, options, evaluation
                )
            )
        return self.final_graphics(graphics, options)


class Plot(_Plot):
    """
    <dl>
      <dt>'Plot[$f$, {$x$, $xmin$, $xmax$}]'
      <dd>plots $f$ with $x$ ranging from $xmin$ to $xmax$.

      <dt>'Plot[{$f1$, $f2$, ...}, {$x$, $xmin$, $xmax$}]'
      <dd>plots several functions $f1$, $f2$, ...

    </dl>

    >> Plot[{Sin[x], Cos[x], x / 3}, {x, -Pi, Pi}]
     = -Graphics-

    >> Plot[Sin[x], {x, 0, 4 Pi}, PlotRange->{{0, 4 Pi}, {0, 1.5}}]
     = -Graphics-

    >> Plot[Tan[x], {x, -6, 6}, Mesh->Full]
     = -Graphics-

    >> Plot[x^2, {x, -1, 1}, MaxRecursion->5, Mesh->All]
     = -Graphics-

    >> Plot[Log[x], {x, 0, 5}, MaxRecursion->0]
     = -Graphics-

    >> Plot[Tan[x], {x, 0, 6}, Mesh->All, PlotRange->{{-1, 5}, {0, 15}}, MaxRecursion->10]
     = -Graphics-

    A constant function:
    >> Plot[3, {x, 0, 1}]
     = -Graphics-

    #> Plot[1 / x, {x, -1, 1}]
     = -Graphics-
    #> Plot[x, {y, 0, 2}]
     = -Graphics-

    #> Plot[{f[x],-49x/12+433/108},{x,-6,6}, PlotRange->{-10,10}, AspectRatio->{1}]
     = -Graphics-

    #> Plot[Sin[t],  {t, 0, 2 Pi}, PlotPoints -> 1]
     : Value of option PlotPoints -> 1 is not an integer >= 2.
     = Plot[Sin[t], {t, 0, 2 Pi}, PlotPoints -> 1]

    #> Plot[x*y, {x, -1, 1}]
     = -Graphics-
    """

    def get_functions_param(self, functions):
        if functions.has_form("List", None):
            functions = functions.leaves
        else:
            functions = [functions]
        return functions

    def get_plotrange(self, plotrange, start, stop):
        x_range = y_range = None
        if isinstance(plotrange, numbers.Real):
            plotrange = ["System`Full", [-plotrange, plotrange]]
        if plotrange == "System`Automatic":
            plotrange = ["System`Full", "System`Automatic"]
        elif plotrange == "System`All":
            plotrange = ["System`All", "System`All"]
        if isinstance(plotrange, list) and len(plotrange) == 2:
            if isinstance(plotrange[0], numbers.Real) and isinstance(  # noqa
                plotrange[1], numbers.Real
            ):
                x_range, y_range = "System`Full", plotrange
            else:
                x_range, y_range = plotrange
            if x_range == "System`Full":
                x_range = [start, stop]
        return x_range, y_range

    def eval_f(self, f, x_value):
        value = f(x_value)
        if value is not None:
            return (x_value, value)


class ParametricPlot(_Plot):
    """
    <dl>
    <dt>'ParametricPlot[{$f_x$, $f_y$}, {$u$, $umin$, $umax$}]'
        <dd>plots a parametric function $f$ with the parameter $u$ ranging from $umin$ to $umax$.
    <dt>'ParametricPlot[{{$f_x$, $f_y$}, {$g_x$, $g_y$}, ...}, {$u$, $umin$, $umax$}]'
        <dd>plots several parametric functions $f$, $g$, ...
    <dt>'ParametricPlot[{$f_x$, $f_y$}, {$u$, $umin$, $umax$}, {$v$, $vmin$, $vmax$}]'
        <dd>plots a parametric area.
    <dt>'ParametricPlot[{{$f_x$, $f_y$}, {$g_x$, $g_y$}, ...}, {$u$, $umin$, $umax$}, {$v$, $vmin$, $vmax$}]'
        <dd>plots several parametric areas.
    </dl>

    >> ParametricPlot[{Sin[u], Cos[3 u]}, {u, 0, 2 Pi}]
     = -Graphics-

    >> ParametricPlot[{Cos[u] / u, Sin[u] / u}, {u, 0, 50}, PlotRange->0.5]
     = -Graphics-

    >> ParametricPlot[{{Sin[u], Cos[u]},{0.6 Sin[u], 0.6 Cos[u]}, {0.2 Sin[u], 0.2 Cos[u]}}, {u, 0, 2 Pi}, PlotRange->1, AspectRatio->1]
    = -Graphics-
    """

    expect_list = True

    def get_functions_param(self, functions):
        if functions.has_form("List", 2) and not (
            functions.leaves[0].has_form("List", None)
            or functions.leaves[1].has_form("List", None)
        ):
            # One function given
            functions = [functions]
        else:
            # Multiple Functions
            functions = functions.leaves
        return functions

    def get_plotrange(self, plotrange, start, stop):
        x_range = y_range = None
        if isinstance(plotrange, numbers.Real):
            plotrange = [[-plotrange, plotrange], [-plotrange, plotrange]]
        if plotrange == "System`Automatic":
            plotrange = ["System`Automatic", "System`Automatic"]
        elif plotrange == "System`All":
            plotrange = ["System`All", "System`All"]
        if isinstance(plotrange, list) and len(plotrange) == 2:
            if isinstance(plotrange[0], numbers.Real) and isinstance(  # noqa
                plotrange[1], numbers.Real
            ):
                x_range = [-plotrange[0], plotrange[1]]
                y_range = [-plotrange[1], plotrange[1]]
            else:
                x_range, y_range = plotrange
        return x_range, y_range

    def eval_f(self, f, x_value):
        value = f(x_value)
        if value is not None and len(value) == 2:
            return value


class PolarPlot(_Plot):
    """
    <dl>
    <dt>'PolarPlot[$r$, {$t$, $tmin$, $tmax$}]'
        <dd>creates a polar plot of $r$ with angle $t$ ranging from
        $tmin$ to $tmax$.
    </dl>

    >> PolarPlot[Cos[5t], {t, 0, Pi}]
     = -Graphics-

    >> PolarPlot[{1, 1 + Sin[20 t] / 5}, {t, 0, 2 Pi}]
     = -Graphics-
    """

    options = _Plot.options.copy()
    options.update(
        {"AspectRatio": "1",}
    )

    def get_functions_param(self, functions):
        if functions.has_form("List", None):
            functions = functions.leaves
        else:
            functions = [functions]
        return functions

    def get_plotrange(self, plotrange, start, stop):
        x_range = y_range = None
        if isinstance(plotrange, numbers.Real):
            plotrange = [[-plotrange, plotrange], [-plotrange, plotrange]]
        if plotrange == "System`Automatic":
            plotrange = ["System`Automatic", "System`Automatic"]
        elif plotrange == "System`All":
            plotrange = ["System`All", "System`All"]
        if isinstance(plotrange, list) and len(plotrange) == 2:
            if isinstance(plotrange[0], numbers.Real) and isinstance(  # noqa
                plotrange[1], numbers.Real
            ):
                x_range = [-plotrange[0], plotrange[1]]
                y_range = [-plotrange[1], plotrange[1]]
            else:
                x_range, y_range = plotrange
        return x_range, y_range

    def eval_f(self, f, x_value):
        value = f(x_value)
        if value is not None:
            return (value * cos(x_value), value * sin(x_value))


class ListPlot(_ListPlot):
    """
    <dl>
    <dt>'ListPlot[{$y_1$, $y_2$, ...}]'
        <dd>plots a list of y-values, assuming integer x-values 1, 2, 3, ...
    <dt>'ListPlot[{{$x_1$, $y_1$}, {$x_2$, $y_2$}, ...}]'
        <dd>plots a list of $x$, $y$ pairs.
    <dt>'ListPlot[{$list_1$, $list_2$, ...}]'
        <dd>plots several lists of points.
    </dl>

    ListPlot accepts a superset of the Graphics options.

    >> ListPlot[Table[n ^ 2, {n, 10}]]
     = -Graphics-
    """

    from .graphics import Graphics

    attributes = ("HoldAll",)

    options = Graphics.options.copy()
    options.update(
        {
            "Axes": "True",
            "AspectRatio": "1 / GoldenRatio",
            "Mesh": "None",
            "PlotRange": "Automatic",
            "PlotPoints": "None",
            "Filling": "None",
            "Joined": "False",
        }
    )


class ListLinePlot(_ListPlot):
    """
    <dl>
      <dt>'ListLinePlot[{$y_1$, $y_2$, ...}]'
      <dd>plots a line through a list of $y$-values, assuming integer $x$-values 1, 2, 3, ...

      <dt>'ListLinePlot[{{$x_1$, $y_1$}, {$x_2$, $y_2$}, ...}]'
      <dd>plots a line through a list of $x$, $y$ pairs.

      <dt>'ListLinePlot[{$list_1$, $list_2$, ...}]'
      <dd>plots several lines.
    </dl>

    ListPlot accepts a superset of the Graphics options.

    >> ListLinePlot[Table[{n, n ^ 0.5}, {n, 10}]]
     = -Graphics-

    >> ListLinePlot[{{-2, -1}, {-1, -1}}]
     = -Graphics-
    """

    from .graphics import Graphics

    attributes = ("HoldAll",)

    options = Graphics.options.copy()
    options.update(
        {
            "Axes": "True",
            "AspectRatio": "1 / GoldenRatio",
            "Mesh": "None",
            "PlotRange": "Automatic",
            "PlotPoints": "None",
            "Filling": "None",
            "Joined": "True",
        }
    )


class Plot3D(_Plot3D):
    """
    <dl>
    <dt>'Plot3D[$f$, {$x$, $xmin$, $xmax$}, {$y$, $ymin$, $ymax$}]'
        <dd>creates a three-dimensional plot of $f$ with $x$ ranging from $xmin$ to $xmax$ and $y$ ranging from $ymin$ to $ymax$.
    </dl>

    >> Plot3D[x ^ 2 + 1 / y, {x, -1, 1}, {y, 1, 4}]
     = -Graphics3D-

    >> Plot3D[x y / (x ^ 2 + y ^ 2 + 1), {x, -2, 2}, {y, -2, 2}]
     = -Graphics3D-

    >> Plot3D[x / (x ^ 2 + y ^ 2 + 1), {x, -2, 2}, {y, -2, 2}, Mesh->None]
     = -Graphics3D-

    >> Plot3D[Sin[x y] /(x y), {x, -3, 3}, {y, -3, 3}, Mesh->All]
     = -Graphics3D-

    >> Plot3D[Log[x + y^2], {x, -1, 1}, {y, -1, 1}]
     = -Graphics3D-

    #> Plot3D[z, {x, 1, 20}, {y, 1, 10}]
     = -Graphics3D-

    ## MaxRecursion Option
    #> Plot3D[0, {x, -2, 2}, {y, -2, 2}, MaxRecursion -> 0]
     = -Graphics3D-
    #> Plot3D[0, {x, -2, 2}, {y, -2, 2}, MaxRecursion -> 15]
     = -Graphics3D-
    #> Plot3D[0, {x, -2, 2}, {y, -2, 2}, MaxRecursion -> 16]
     : MaxRecursion must be a non-negative integer; the recursion value is limited to 15. Using MaxRecursion -> 15.
     = -Graphics3D-
    #> Plot3D[0, {x, -2, 2}, {y, -2, 2}, MaxRecursion -> -1]
     : MaxRecursion must be a non-negative integer; the recursion value is limited to 15. Using MaxRecursion -> 0.
     = -Graphics3D-
    #> Plot3D[0, {x, -2, 2}, {y, -2, 2}, MaxRecursion -> a]
     : MaxRecursion must be a non-negative integer; the recursion value is limited to 15. Using MaxRecursion -> 0.
     = -Graphics3D-
    #> Plot3D[0, {x, -2, 2}, {y, -2, 2}, MaxRecursion -> Infinity]
     : MaxRecursion must be a non-negative integer; the recursion value is limited to 15. Using MaxRecursion -> 15.
     = -Graphics3D-

    #> Plot3D[x ^ 2 + 1 / y, {x, -1, 1}, {y, 1, z}]
     : Limiting value z in {y, 1, z} is not a machine-size real number.
     = Plot3D[x ^ 2 + 1 / y, {x, -1, 1}, {y, 1, z}]
    """

    # FIXME: This test passes but the result is 511 lines long !
    """
    #> Plot3D[x + 2y, {x, -2, 2}, {y, -2, 2}] // TeXForm
    """

    from .graphics import Graphics

    attributes = ("HoldAll",)

    options = Graphics.options.copy()
    options.update(
        {
            "Axes": "True",
            "AspectRatio": "1",
            "Mesh": "Full",
            "PlotPoints": "None",
            "BoxRatios": "{1, 1, 0.4}",
            "MaxRecursion": "2",
        }
    )

    def get_functions_param(self, functions):
        if functions.has_form("List", None):
            return functions.leaves
        else:
            return [functions]

    def construct_graphics(
        self, triangles, mesh_points, v_min, v_max, options, evaluation
    ):
        graphics = []
        for p1, p2, p3 in triangles:
            graphics.append(
                Expression(
                    "Polygon",
                    Expression(
                        "List",
                        Expression(SymbolList, *p1),
                        Expression(SymbolList, *p2),
                        Expression(SymbolList, *p3),
                    ),
                )
            )
        # Add the Grid
        for xi in range(len(mesh_points)):
            line = []
            for yi in range(len(mesh_points[xi])):
                line.append(
                    Expression(
                        "List",
                        mesh_points[xi][yi][0],
                        mesh_points[xi][yi][1],
                        mesh_points[xi][yi][2],
                    )
                )
            graphics.append(Expression("Line", Expression(SymbolList, *line)))
        return graphics

    def final_graphics(self, graphics, options):
        return Expression(
            "Graphics3D",
            Expression(SymbolList, *graphics),
            *options_to_rules(options, Graphics3D.options)
        )


class DensityPlot(_Plot3D):
    """
    <dl>
    <dt>'DensityPlot[$f$, {$x$, $xmin$, $xmax$}, {$y$, $ymin$, $ymax$}]'
        <dd>plots a density plot of $f$ with $x$ ranging from $xmin$ to $xmax$ and $y$ ranging from $ymin$ to $ymax$.
    </dl>

    >> DensityPlot[x ^ 2 + 1 / y, {x, -1, 1}, {y, 1, 4}]
     = -Graphics-

    >> DensityPlot[1 / x, {x, 0, 1}, {y, 0, 1}]
     = -Graphics-

    >> DensityPlot[Sqrt[x * y], {x, -1, 1}, {y, -1, 1}]
     = -Graphics-

    >> DensityPlot[1/(x^2 + y^2 + 1), {x, -1, 1}, {y, -2,2}, Mesh->Full]
     = -Graphics-

    >> DensityPlot[x^2 y, {x, -1, 1}, {y, -1, 1}, Mesh->All]
     = -Graphics-
    """

    from .graphics import Graphics

    attributes = ("HoldAll",)

    options = Graphics.options.copy()
    options.update(
        {
            "Axes": "False",
            "AspectRatio": "1",
            "Mesh": "None",
            "Frame": "True",
            "ColorFunction": "Automatic",
            "ColorFunctionScaling": "True",
            "PlotPoints": "None",
            "MaxRecursion": "0",
            # 'MaxRecursion': '2',  # FIXME causes bugs in svg output see #303
        }
    )

    def get_functions_param(self, functions):
        return [functions]

    def construct_graphics(
        self, triangles, mesh_points, v_min, v_max, options, evaluation
    ):
        color_function = self.get_option(options, "ColorFunction", evaluation, pop=True)
        color_function_scaling = self.get_option(
            options, "ColorFunctionScaling", evaluation, pop=True
        )

        color_function_min = color_function_max = None
        if color_function.get_name() == "System`Automatic":
            color_function = String("LakeColors")
        if color_function.get_string_value():
            func = Expression("ColorData", color_function.get_string_value()).evaluate(
                evaluation
            )
            if func.has_form("ColorDataFunction", 4):
                color_function_min = func.leaves[2].leaves[0].round_to_float()
                color_function_max = func.leaves[2].leaves[1].round_to_float()
                color_function = Expression(
                    "Function", Expression(func.leaves[3], Expression("Slot", 1))
                )
            else:
                evaluation.message("DensityPlot", "color", func)
                return
        if color_function.has_form("ColorDataFunction", 4):
            color_function_min = color_function.leaves[2].leaves[0].round_to_float()
            color_function_max = color_function.leaves[2].leaves[1].round_to_float()

        color_function_scaling = color_function_scaling.is_true()
        v_range = v_max - v_min

        if v_range == 0:
            v_range = 1

        if color_function.has_form("ColorDataFunction", 4):
            color_func = color_function.leaves[3]
        else:
            color_func = color_function
        if (
            color_function_scaling
            and color_function_min is not None  # noqa
            and color_function_max is not None
        ):
            color_function_range = color_function_max - color_function_min

        colors = {}

        def eval_color(x, y, v):
            v_scaled = (v - v_min) / v_range
            if (
                color_function_scaling
                and color_function_min is not None  # noqa
                and color_function_max is not None
            ):
                v_color_scaled = color_function_min + v_scaled * color_function_range
            else:
                v_color_scaled = v

            # Calculate and store 100 different shades max.
            v_lookup = int(v_scaled * 100 + 0.5)

            value = colors.get(v_lookup)
            if value is None:
                value = Expression(color_func, Real(v_color_scaled))
                value = value.evaluate(evaluation)
                colors[v_lookup] = value
            return value

        points = []
        vertex_colors = []
        graphics = []
        for p in triangles:
            points.append(
                Expression(SymbolList, *(Expression(SymbolList, *x[:2]) for x in p))
            )
            vertex_colors.append(Expression(SymbolList, *(eval_color(*x) for x in p)))

        graphics.append(
            Expression(
                "Polygon",
                Expression(SymbolList, *points),
                Expression(
                    "Rule",
                    Symbol("VertexColors"),
                    Expression(SymbolList, *vertex_colors),
                ),
            )
        )

        # add mesh
        for xi in range(len(mesh_points)):
            line = []
            for yi in range(len(mesh_points[xi])):
                line.append(
                    Expression(
                        SymbolList, mesh_points[xi][yi][0], mesh_points[xi][yi][1]
                    )
                )
            graphics.append(Expression("Line", Expression(SymbolList, *line)))

        return graphics

    def final_graphics(self, graphics, options):
        return Expression(
            "Graphics",
            Expression(SymbolList, *graphics),
            *options_to_rules(options, Graphics.options)
        )
