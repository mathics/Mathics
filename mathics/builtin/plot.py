# -*- coding: utf8 -*-

"""
Plotting
"""

import re
from math import floor, cos, pi, sqrt

from mathics.core.expression import Expression, Real, NumberError, Symbol, String
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

class Plot(Builtin):
    """
    <dl>
    <dt>'Plot[$f$, {$x$, $xmin$, $xmax$}]'
        <dd>plots $f$ with $x$ ranging from $xmin$ to $xmax$.
    <dt>'Plot[{$f1$, $f2$, ...}, {$x$, $xmin$, $xmax$}]'
        <dd>plots several functions $f1$, $f2$, ...
    </dl>
    
    >> Plot[{Sin[x], Cos[x], x / 3}, {x, -Pi, Pi}]
     = -Graphics-
    """

    from graphics import Graphics
    
    attributes = ('HoldAll',)
    
    options = Graphics.options.copy()
    options.update({
        'Axes': 'True',
        'AspectRatio': '1 / GoldenRatio',
    })
    def AutomaticPlotRange(self,points):
        """ Calculates mean and standard deviation, throwing away all points 
        which are more than 'thresh' number of standard deviations away from 
        the mean. These are then used to find good ymin and ymax values. """
        thresh = 3.0
        values = []
        for line in points:
            for p in line:
                values.append(p[1])
        values.sort()
        valavg = sum(values)/len(values)
        valdev = sqrt(sum([(x-valavg)**2 for x in values])/(len(values)-1))

        (n1,n2) = (0,len(values)-1)
        for v in values:
            if abs(v-valavg)/valdev < thresh:
                break
            n1+=1
        for v in values[::-1]:
            if abs(v-valavg)/valdev < thresh:
                break
            n2-=1
        
        yrange = values[n2]-values[n1]
        ymin = values[n1]-0.05*yrange
        ymax = values[n2]+0.05*yrange
        return (ymin,ymax)
    
    def apply(self, functions, x, start, stop, evaluation, options):
        'Plot[functions_, {x_Symbol, start_, stop_}, OptionsPattern[Plot]]'
        
        expr = Expression('Plot', functions, Expression('List', x, start, stop), *options_to_rules(options))
        if functions.has_form('List', None):
            functions = functions.leaves
        else:
            functions = [functions]
        x = x.get_name()
        try:
            start = start.to_number(n_evaluation=evaluation)
        except NumberError:
            evaluation.message('Plot', 'plln', start, expr)
            return
        try:
            stop = stop.to_number(n_evaluation=evaluation)
        except NumberError:
            evaluation.message('Plot', 'plln', stop, expr)
            return
            
        def eval_f(f, x_value):
            value = dynamic_scoping(f.evaluate, {x: x_value}, evaluation)
            value = chop(value).get_real_value()
            return value
        
        hue = 0.67
        hue_pos = 0.236068
        hue_neg = -0.763932
        
        graphics = []
        for index, f in enumerate(functions):
            points = []
            continuous = False
            steps = 50
            d = (stop - start) / steps
            for index in range(steps + 1):
                x_value = start + index * d
                y = eval_f(f, Real(x_value))
                if y is not None:
                    point = (x_value, y)
                    if continuous:
                        points[-1].append(point)
                    else:
                        points.append([point])
                    continuous = True
                else:
                    continuous = False    
            
            xscale = 1./(stop-start)
            (ymin,ymax) = self.AutomaticPlotRange(points)     #TODO Use this for PlotRange->Automatic
            yscale = 1./(ymax-ymin)

            # Loop again and interpolate highly angled sections
            ang_thresh = cos(pi/90)    # Cos of the maximum angle between successive line segments
            maxrecursion = 3    #TODO get maxrecursion from Plot[] arguments (not hardcoded)
            for line in points:
                recursion_count = 0
                smooth = False
                while (not smooth and recursion_count < maxrecursion):
                    recursion_count += 1
                    smooth = True
                    i = 2
                    while i < len(line):
                        vec1 = (xscale*(line[i-1][0]-line[i-2][0]), yscale*(line[i-1][1]-line[i-2][1]))
                        vec2 = (xscale*(line[i][0]-line[i-1][0]), yscale*(line[i][1]-line[i-1][1]))
                        try:
                            angle = (vec1[0]*vec2[0] + vec1[1]*vec2[1])/sqrt(\
                            (vec1[0]**2 + vec1[1]**2)*(vec2[0]**2 + vec2[1]**2))
                        except ZeroDivisionError:
                            angle = 0.0
                        if abs(angle) < ang_thresh:
                            smooth = False
                            x_value = 0.5*(line[i-1][0] + line[i][0])
                            y = eval_f(f, Real(x_value))
                            point = (x_value, y)

                            x_value = 0.5*(line[i-2][0] + line[i-1][0])
                            line.insert(i,point)
                            y = eval_f(f, Real(x_value))
                            point = (x_value, y)
                            line.insert(i-1,point)
                            i+=2
                        i+=1

            # Crop the plotted points - this is a hack!
            #TODO Replace this with user specified PlotRange
            i = 0
            while i < len(points):
                for j in range(len(points[i])):
                    if points[i][j][1] > ymax:
                        del points[i][j]
                    elif points[i][j][1] < ymin:
                        del points[i][j]
                    else:
                        continue
                    points.append(points[i][j:])
                    points[i] = points[i][:j]
                    break
                i+=1

            graphics.append(Expression('Hue', hue, 0.6, 0.6))
            graphics.append(Expression('Line', Expression('List', *(Expression('List',
                *(Expression('List', Real(x), Real(y)) for x, y in line)) for line in points)
            )))

            if index % 4 == 0:
                hue += hue_pos
            else:
                hue += hue_neg
            if hue > 1: hue -= 1
            if hue < 0: hue += 1
        
        return Expression('Graphics', Expression('List', *graphics), *options_to_rules(options))
    
class DensityPlot(Builtin):
    """
    <dl>
    <dt>'DensityPlot[$f$, {$x$, $xmin$, $xmax$}, {$y$, $ymin$, $ymax$}]'
        <dd>plots a density plot of $f$ with $x$ ranging from $xmin$ to $xmax$ and $y$ ranging from $ymin$ to $ymax$.
    </dl>
    
    >> DensityPlot[x ^ 2 + 1 / y, {x, -1, 1}, {y, 1, 4}]
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
        
        #print "Initialized"
        
        stored = {}
        def eval_f(x_value, y_value):
            value = stored.get((x_value, y_value), False)
            if value == False:
                value = dynamic_scoping(f.evaluate, {x: Real(x_value), y: Real(y_value)}, evaluation)
                value = chop(value).get_real_value()
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
              
        #if color_function_scaling: 
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
            #v_lookup = int(v * 100)
            #if color_function_scaling:
            v_scaled = (v - v_min) / v_range
            if color_function_scaling and color_function_min is not None and color_function_max is not None:
                v_color_scaled = color_function_min + v_scaled * color_function_range
            else:
                v_color_scaled = v
            v_lookup = int(v_scaled * 100 + 0.5)     # calculate and store 100 different shades max.
            value = colors.get(v_lookup)
            if value is None:
                #print "Calc"
                #print "Scale"
                #print "Expression"
                #print "Calc color for %f" % v_scaled
                value = Expression(color_func, Real(v_color_scaled))
                #print "Evaluate %s" % value
                value = value.evaluate(evaluation)
                #value = Expression('RGBColor', Real(0.5), Real(0.5), Real(0.5))
                #print "Set"
                colors[v_lookup] = value
            return value
        
        #print "Points"
        points = []
        vertex_colors = []
        for p1, p2, p3 in triangles:
            #print "Triangle %s,%s,%s" % (p1, p2, p3)
            c1, c2, c3 = eval_color(*p1), eval_color(*p2), eval_color(*p3)
            #print "Append"
            points.append(Expression('List', Expression('List', *p1[:2]), Expression('List', *p2[:2]),
                Expression('List', *p3[:2])))
            vertex_colors.append(Expression('List', c1, c2, c3))
        
        #print "Polygon"
        polygon = Expression('Polygon', Expression('List', *points),
            Expression('Rule', Symbol('VertexColors'), Expression('List', *vertex_colors)))
        #print "Result"
        result = Expression('Graphics', polygon, *options_to_rules(options))
        #print "Return"
        return result

