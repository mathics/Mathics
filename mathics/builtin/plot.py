# -*- coding: utf8 -*-

"""
Plotting
"""

import re
from math import floor, cos, pi, sqrt

from mathics.core.expression import Expression, Real, NumberError, Symbol, String, from_python
from mathics.builtin.base import Builtin
from mathics.builtin.scoping import dynamic_scoping
from mathics.builtin.options import options_to_rules
from mathics.builtin.numeric import chop
            
class ColorDataFunction(Builtin):
    pass
    
class ColorData(Builtin):
    rules = {
        'ColorData["LakeColors"]': """ColorDataFunction["LakeColors", "Gradients", {0, 1}, 
             Blend[{RGBColor[0.293416, 0.0574044, 0.529412], 
                RGBColor[0.563821, 0.527565, 0.909499], RGBColor[0.762631, 0.846998, 
                 0.914031], RGBColor[0.941176, 0.906538, 0.834043]}, #1] & ]""",
    }

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
    """

    messages = {
        'ilevels' : "`1` s not a valid mesh specification.",
    }
    
def quiet_evaluate(expr, vars, evaluation, expect_list=False):
    """ Evaluates expr with given dynamic scoping values
    without producing arithmetic error messages. """
    quiet_expr = Expression('Quiet', expr, Expression('List',
        Expression('MessageName', Symbol('Power'), String('infy'))))
    value = dynamic_scoping(quiet_expr.evaluate, vars, evaluation)
    if expect_list:
        if value.has_form('List', None):
            value = [chop(item).get_real_value() for item in value.leaves]
            if any(item is None for item in value):
                return None
            return value
        else:
            return None
    else:
        return chop(value).get_real_value()

class _Plot(Builtin):
    from graphics import Graphics
    
    attributes = ('HoldAll',)
    
    options = Graphics.options.copy()
    options.update({
        'Axes': 'True',
        'AspectRatio': '1 / GoldenRatio',
        'MaxRecursion': 'Automatic',
        'Mesh': 'None',
    })

    messages = {
        'invmaxrec': "MaxRecursion must be a non-negative integer; the recursion value is limited to `2`. Using MaxRecursion -> `1`.",
        'prng': "Value of option PlotRange -> `1` is not All, Automatic or an appropriate list of range specifications.",
        'invmesh': "Mesh must be one of {None, Full, All}. Using Mesh->None.",
    }

    def automatic_plot_range(self, values):
        """ Calculates mean and standard deviation, throwing away all points 
        which are more than 'thresh' number of standard deviations away from 
        the mean. These are then used to find good ymin and ymax values. These 
        values can then be used to find Automatic Plotrange. """
        thresh = 2.0
        values = sorted(values)
        valavg = sum(values) / len(values)
        valdev = sqrt(sum([(x - valavg)**2 for x in values]) / (len(values) - 1))

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
        
        yrange = values[n2] - values[n1]
        ymin = values[n1] - 0.05 * yrange    # 5% extra looks nice
        ymax = values[n2] + 0.05 * yrange
        return ymin, ymax
    
    def apply(self, functions, x, start, stop, evaluation, options):
        '%(name)s[functions_, {x_Symbol, start_, stop_}, OptionsPattern[%(name)s]]'
        
        expr_limits = Expression('List', x, start, stop)
        expr = Expression(self.get_name(), functions, expr_limits, *options_to_rules(options))
        functions = self.get_functions_param(functions)
        x_name = x.get_name()
        
        try:
            start = start.to_number(n_evaluation=evaluation)
        except NumberError:
            evaluation.message(self.get_name(), 'plln', start, expr)
            return
        try:
            stop = stop.to_number(n_evaluation=evaluation)
        except NumberError:
            evaluation.message(self.get_name(), 'plln', stop, expr)
            return
        if start >= stop:
            evaluation.message(self.get_name(), 'plld', expr_limits)
            return

        # PlotRange Option
        def check_range(range):
            if range in ('Automatic', 'All'):
                return True
            if isinstance(range, list) and len(range) == 2:
                if isinstance(range[0], float) and isinstance(range[1], float):
                    return True
            return False
        plotrange_option = self.get_option(options, 'PlotRange', evaluation)
        plotrange = plotrange_option.to_python(n_evaluation=evaluation)
        x_range, y_range = self.get_plotrange(plotrange, start, stop)
        if not check_range(x_range) or not check_range(y_range):
            evaluation.message(self.get_name(), 'prng', plotrange_option)
            x_range, y_range = [start, stop], 'Automatic'
        # x_range and y_range are now either Automatic, All, or of the form [min, max]
        assert x_range in ('Automatic', 'All') or isinstance(x_range, list)
        assert y_range in ('Automatic', 'All') or isinstance(y_range, list)

        # Mesh Option
        mesh_option = self.get_option(options, 'Mesh', evaluation)
        mesh = mesh_option.to_python()
        if mesh not in ['None', 'Full', 'All']:
            evaluation.message('Mesh', 'ilevels', mesh_option)
            mesh = 'None'

        # MaxRecursion Option
        max_recursion_limit = 15
        maxrecursion_option = self.get_option(options, 'MaxRecursion', evaluation)
        maxrecursion = maxrecursion_option.to_python()
        try:
            if maxrecursion == 'Automatic':
                maxrecursion = 3
            elif maxrecursion == float('inf'):
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
            evaluation.message(self.get_name(), 'invmaxrec', maxrecursion, max_recursion_limit)
        assert isinstance(maxrecursion, int)

        # constants to generate colors
        hue = 0.67
        hue_pos = 0.236068
        hue_neg = -0.763932

        def get_points_minmax(points):
            xmin = xmax = ymin = ymax = None
            for line in points:
                for x, y in line:
                    if xmin is None or x < xmin: xmin = x
                    if xmax is None or x > xmax: xmax = x
                    if ymin is None or y < ymin: ymin = y
                    if ymax is None or y > ymax: ymax = y
            return xmin, xmax, ymin, ymax
        
        def zero_to_one(value):
            if value == 0:
                return 1
            return value
            
        def get_points_range(points):
            xmin, xmax, ymin, ymax = get_points_minmax(points)
            if xmin is None or xmax is None:
                xmin, xmax = 0, 1
            if ymin is None or ymax is None:
                ymin, ymax = 0, 1
            return zero_to_one(xmax - xmin), zero_to_one(ymax - ymin)
        
        function_hues = []
        base_plot_points = []   # list of points in base subdivision
        plot_points = []        # list of all plotted points
        mesh_points = []
        graphics = []           # list of resulting graphics primitives
        for index, f in enumerate(functions):
            points = []
            xvalues = [] # x value for each point in points
            tmp_mesh_points = [] # For this function only
            continuous = False
            steps = 57
            d = (stop - start) / steps
            for i in range(steps + 1):
                x_value = start + i * d
                point = self.eval_f(f, x_name, x_value, evaluation)
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
            
            xscale = 1. / (stop - start)
            ymin, ymax = self.automatic_plot_range([y for x, y in base_points])
            if ymin != ymax:
                yscale = 1. / (ymax - ymin)
            else:
                yscale = 1.0

            if mesh == 'Full':
                for line in points:
                    tmp_mesh_points.extend(line)

            # Adaptive Sampling - loop again and interpolate highly angled sections
            ang_thresh = cos(pi / 180)    # Cos of the maximum angle between successive line segments
            for line, line_xvalues in zip(points, xvalues):
                recursion_count = 0
                smooth = False
                while not smooth and recursion_count < maxrecursion:
                    recursion_count += 1
                    smooth = True
                    i = 2
                    while i < len(line):
                        vec1 = (xscale * (line[i-1][0] - line[i-2][0]), yscale * (line[i-1][1] - line[i-2][1]))
                        vec2 = (xscale * (line[i][0] - line[i-1][0]), yscale * (line[i][1] - line[i-1][1]))
                        try:
                            angle = (vec1[0] * vec2[0] + vec1[1] * vec2[1]) / sqrt(
                                (vec1[0]**2 + vec1[1]**2) * (vec2[0]**2 + vec2[1]**2))
                        except ZeroDivisionError:
                            angle = 0.0
                        if abs(angle) < ang_thresh:
                            smooth = False
                            incr = 0
                            
                            x_value = 0.5 * (line_xvalues[i-1] + line_xvalues[i])
                            point = self.eval_f(f, x_name, x_value, evaluation)
                            if point is not None:
                                line.insert(i, point)
                                line_xvalues.insert(i, x_value)
                                incr += 1

                            x_value = 0.5 * (line_xvalues[i-2] + line_xvalues[i-1])
                            point = self.eval_f(f, x_name, x_value, evaluation)
                            if point is not None:
                                line.insert(i - 1, point)
                                line_xvalues.insert(i - 1, x_value)
                                incr += 1
                            
                            i += incr
                        i += 1
                    
            graphics.append(Expression('Hue', hue, 0.6, 0.6))
            graphics.append(Expression('Line', Expression('List', *(Expression('List',
                *(Expression('List', x, y) for x, y in line)) for line in points)
            )))
            for line in points:
                plot_points.extend(line)

            if mesh == 'All':
                for line in points:
                    tmp_mesh_points.extend(line)

            if mesh != 'None':
                mesh_points.append(tmp_mesh_points)

            function_hues.append(hue)

            if index % 4 == 0:
                hue += hue_pos
            else:
                hue += hue_neg
            if hue > 1: hue -= 1
            if hue < 0: hue += 1
            
        def get_plot_range(values, all_values, option):
            if option == 'Automatic':
                return self.automatic_plot_range(values)
            if option == 'All':
                if not all_values:
                    return [0, 1]
                return min(all_values), max(all_values)
            return option
        
        x_range = get_plot_range([x for x, y in base_plot_points],
            [x for x, y in plot_points], x_range)
        y_range = get_plot_range([y for x, y in base_plot_points],
            [y for x, y in plot_points], y_range)
        
        mesh_xscale = 1. / zero_to_one(x_range[1] - x_range[0])
        mesh_yscale = 1. / zero_to_one(y_range[1] - y_range[0])
        
        options['PlotRange'] = from_python([x_range, y_range])
        
        if mesh != 'None':
            for hue, points in zip(function_hues, mesh_points):
                graphics.append(Expression('Hue', hue, 0.6, 0.6))
                for x, y in points:
                    graphics.append(Expression('Disk', Expression('List', x, y), 
                        Expression('List', 0.003 / mesh_xscale, 0.005 / mesh_yscale))
                    )
                #TODO handle non-default AspectRatio
        
        return Expression('Graphics', Expression('List', *graphics), *options_to_rules(options))
    
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
     
    #> Plot[1 / x, {x, -1, 1}]
     = -Graphics-
    """
    
    def get_functions_param(self, functions):
        if functions.has_form('List', None):
            functions = functions.leaves
        else:
            functions = [functions]
        return functions
    
    def get_plotrange(self, plotrange, start, stop):
        x_range = y_range = None
        if isinstance(plotrange, float):
            plotrange = ['Full', [-plotrange, plotrange]]
        if plotrange == 'Automatic':
            plotrange = ['Full', 'Automatic']
        elif plotrange == 'All':
            plotrange = ['All', 'All']
        if isinstance(plotrange, list) and len(plotrange) == 2:
            if isinstance(plotrange[0], float) and isinstance(plotrange[1], float):
                x_range, y_range = 'Full', plotrange
            else:
                x_range, y_range = plotrange
            if x_range == 'Full':
                x_range = [start, stop]
        return x_range, y_range
    
    def eval_f(self, f, x_name, x_value, evaluation):
        value = quiet_evaluate(f, {x_name: Real(x_value)}, evaluation)
        if value is None:
            return None
        return (x_value, value)
    
class ParametricPlot(_Plot):
    """
    <dl>
    <dt>'ParametricPlot[{$f_x$, $f_y$}, {$u$, $umin$, $umax$}]'
        <dd>plots parametric function $f$ with paramater $u$ ranging from $umin$ to $umax$.
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
    
    def get_functions_param(self, functions):
        if functions.has_form('List', 2) and not (functions.leaves[0].has_form('List', None) or functions.leaves[1].has_form('List', None)):
            # One function given
            functions = [functions]
        else:
            # Multiple Functions
            functions = functions.leaves
        return functions
    
    def get_plotrange(self, plotrange, start, stop):
        x_range = y_range = None
        if isinstance(plotrange, float):
            plotrange = [[-plotrange, plotrange], [-plotrange, plotrange]]
        if plotrange == 'Automatic':
            plotrange = ['Automatic', 'Automatic']
        elif plotrange == 'All':
            plotrange = ['All', 'All']
        if isinstance(plotrange, list) and len(plotrange) == 2:
            if isinstance(plotrange[0], float) and isinstance(plotrange[1], float):
                x_range = [-plotrange[0], plotrange[1]]
                y_range = [-plotrange[1], plotrange[1]]
            else:
                x_range, y_range = plotrange
        return x_range, y_range
    
    def eval_f(self, f, x_name, x_value, evaluation):
        value = quiet_evaluate(f, {x_name: Real(x_value)}, evaluation, expect_list=True)
        if value is None or len(value) != 2:
            return None
        return value

class DensityPlot(Builtin):
    """
    <dl>
    <dt>'DensityPlot[$f$, {$x$, $xmin$, $xmax$}, {$y$, $ymin$, $ymax$}]'
        <dd>plots a density plot of $f$ with $x$ ranging from $xmin$ to $xmax$ and $y$ ranging from $ymin$ to $ymax$.
    </dl>
    
    >> DensityPlot[x ^ 2 + 1 / y, {x, -1, 1}, {y, 1, 4}]
     = -Graphics-
     
    #> DensityPlot[1 / x, {x, 0, 1}, {y, 0, 1}]
     = -Graphics-
    #> DensityPlot[Sqrt[x * y], {x, -1, 1}, {y, -1, 1}]
     = -Graphics-
    """

    from graphics import Graphics
    
    attributes = ('HoldAll',)
    
    options = Graphics.options.copy()
    options.update({
        'Axes': 'False',
        'AspectRatio': '1',
        'Frame': 'True',
        'ColorFunction': 'Automatic',
        'ColorFunctionScaling': 'True',
    })
    
    def apply(self, f, x, xstart, xstop, y, ystart, ystop, evaluation, options):
        'DensityPlot[f_, {x_Symbol, xstart_, xstop_}, {y_Symbol, ystart_, ystop_}, OptionsPattern[DensityPlot]]'
        
        x = x.get_name()
        y = y.get_name()
        
        color_function = self.get_option(options, 'ColorFunction', evaluation, pop=True)
        color_function_scaling = self.get_option(options, 'ColorFunctionScaling', evaluation, pop=True)
        
        color_function_min = color_function_max = None
        if color_function.get_name() == 'Automatic':
            color_function = String('LakeColors')
        if color_function.get_string_value():
            func = Expression('ColorData', color_function.get_string_value()).evaluate(evaluation)
            if func.has_form('ColorDataFunction', 4):
                color_function_min = func.leaves[2].leaves[0].get_real_value()
                color_function_max = func.leaves[2].leaves[1].get_real_value()
                color_function = Expression('Function', Expression(func.leaves[3], Expression('Slot', 1)))
            else:
                evaluation.message('DensityPlot', 'color', func)
                return
        if color_function.has_form('ColorDataFunction', 4):
            color_function_min = color_function.leaves[2].leaves[0].get_real_value()
            color_function_max = color_function.leaves[2].leaves[1].get_real_value()
            
        color_function_scaling = color_function_scaling.is_true()
            
        try:
            xstart, xstop, ystart, ystop = [value.to_number(n_evaluation=evaluation) for value in
                (xstart, xstop, ystart, ystop)]
        except NumberError, exc:
            expr = Expression('DensityPlot', f, Expression('List', x, xstart, xstop),
                Expression('List', y, ystart, ystop), *options_to_rules(options))
            evaluation.message('DensityPlot', 'plln', exc.value, expr)
            return
        
        stored = {}
        def eval_f(x_value, y_value):
            value = stored.get((x_value, y_value), False)
            if value == False:
                value = quiet_evaluate(f, {x: Real(x_value), y: Real(y_value)}, evaluation)
                #value = dynamic_scoping(f.evaluate, {x: Real(x_value), y: Real(y_value)}, evaluation)
                #value = chop(value).get_real_value()
                if value is not None:
                    value = float(value)
                stored[(x_value, y_value)] = value
            return value
        
        v_borders = [None, None] 
        
        triangles = []
        
        eps = 0.01
        
        def triangle(x1, y1, x2, y2, x3, y3, depth=None):
            if depth is None:
                x1, x2, x3 = [xstart + value * (xstop - xstart) for value in (x1, x2, x3)]
                y1, y2, y3 = [ystart + value * (ystop - ystart) for value in (y1, y2, y3)]
                depth = 0
            v1, v2, v3 = eval_f(x1, y1), eval_f(x2, y2), eval_f(x3, y3)
            for v in (v1, v2, v3):
                if v is not None:
                    if v_borders[0] is None or v < v_borders[0]:
                        v_borders[0] = v
                    if v_borders[1] is None or v > v_borders[1]:
                        v_borders[1] = v
            if v1 is None or v2 is None or v3 is None:
                return
            limit = (v_borders[1] - v_borders[0]) * eps
            if depth < 2:
                if abs(v1 - v2) > limit:
                    triangle(x1, y1, x3, y3, (x1+x2)/2, (y1+y2)/2, depth+1)
                    triangle(x2, y2, x3, y3, (x1+x2)/2, (y1+y2)/2, depth+1)
                    return
                if abs(v2 - v3) > limit:
                    triangle(x1, y1, x2, y2, (x2+x3)/2, (y2+y3)/2, depth+1)
                    triangle(x1, y1, x3, y3, (x2+x3)/2, (y2+y3)/2, depth+1)
                    return
                if abs(v1 - v3) > limit:
                    triangle(x2, y2, x1, y1, (x1+x3)/2, (y1+y3)/2, depth+1)
                    triangle(x2, y2, x3, y3, (x1+x3)/2, (y1+y3)/2, depth+1)
                    return
            triangles.append([(x1, y1, v1), (x2, y2, v2), (x3, y3, v3)])
        
        points = 7
        num = points * 1.0
        for xi in range(points):
            for yi in range(points):
                triangle(xi/num, yi/num, (xi+1)/num, (yi+1)/num, (xi+1)/num, yi/num)
                triangle(xi/num, yi/num, (xi+1)/num, (yi+1)/num, xi/num, (yi+1)/num)
        
        v_min = v_max = None
              
        for t in triangles:
            for tx, ty, v in t:
                if v_min is None or v < v_min:
                    v_min = v
                if v_max is None or v > v_max:
                    v_max = v
        v_range = v_max - v_min
        if v_range == 0:
            v_range = 1
                
        if color_function.has_form('ColorDataFunction', 4):
            color_func = color_function.leaves[3]
        else:
            color_func = color_function
        if color_function_scaling and color_function_min is not None and color_function_max is not None:
            color_function_range = color_function_max - color_function_min
                    
        colors = {}
        def eval_color(x, y, v):
            v_scaled = (v - v_min) / v_range
            if color_function_scaling and color_function_min is not None and color_function_max is not None:
                v_color_scaled = color_function_min + v_scaled * color_function_range
            else:
                v_color_scaled = v
            v_lookup = int(v_scaled * 100 + 0.5)     # calculate and store 100 different shades max.
            value = colors.get(v_lookup)
            if value is None:
                value = Expression(color_func, Real(v_color_scaled))
                value = value.evaluate(evaluation)
                colors[v_lookup] = value
            return value
        
        points = []
        vertex_colors = []
        for p1, p2, p3 in triangles:
            c1, c2, c3 = eval_color(*p1), eval_color(*p2), eval_color(*p3)
            points.append(Expression('List', Expression('List', *p1[:2]), Expression('List', *p2[:2]),
                Expression('List', *p3[:2])))
            vertex_colors.append(Expression('List', c1, c2, c3))
        
        polygon = Expression('Polygon', Expression('List', *points),
            Expression('Rule', Symbol('VertexColors'), Expression('List', *vertex_colors)))
        result = Expression('Graphics', polygon, *options_to_rules(options))
        return result
