# -*- coding: utf8 -*-

"""
Graphics (3D)
"""
        
import numbers
from mathics.core.expression import NumberError, from_python, Real
from mathics.builtin.base import BoxConstruct, BoxConstructError
from graphics import (Graphics, GraphicsBox, _GraphicsElements, PolygonBox, create_pens,
    LineBox, PointBox, Style, RGBColor, color_heads, get_class, asy_number)

from django.utils import simplejson as json

from django.utils.html import escape as escape_html

def coords3D(value):
    if value.has_form('List', 3):
        return (value.leaves[0].to_number(), value.leaves[1].to_number(), value.leaves[2].to_number())
    raise CoordinatesError

class Coords3D(object):
    def __init__(self, graphics, expr=None, pos=None, d=None):
        self.graphics = graphics
        self.p = pos
        self.d = d
        if expr is not None:
            if expr.has_form('Offset', 1, 2):
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
        p = (self.p[0]+x, self.p[1]+y, self.p[2]+z)
        return Coords3D(self.graphics, pos=p, d=self.d)
    
    def scale(self, a):
        self.p = (self.p[0]*a[0], self.p[1]*a[1], self.p[2]*a[2])
    
class Style3D(Style):
    def get_default_face_color(self):
        return RGBColor(components=(1,1,1,1))

class Graphics3D(Graphics):
    """
    <dl>
    <dt>'Graphics3D[$primitives$, $options$]'
        <dd>represents a three-dimensional graphic.
    </dl>
    """
    
    options = Graphics.options.copy()
    options.update({
        #'Axes': 'True',
        'BoxRatios': 'Automatic',
        'Lighting': 'Automatic',
        'ViewPoint': '{1.3,-2.4,2}',
    })
    
    box_suffix = '3DBox'
    
    rules = {
        'MakeBoxes[Graphics3D[content_, OptionsPattern[Graphics3D]], OutputForm]': '"-Graphics3D-"',
    }

    messages = {
        'invlight': "`1` is not a valid list of light sources.", 
    }
    
class Graphics3DBox(GraphicsBox):
    def boxes_to_text(self, leaves, **options):
        return '-Graphics3D-'
        
    def _prepare_elements(self, leaves, options, max_width=None):
        if not leaves:
            raise BoxConstructError
        
        graphics_options = self.get_option_values(leaves[1:], **options)

        evaluation = options['evaluation']
        
        base_width, base_height, size_multiplier, size_aspect = self._get_image_size(options,
            graphics_options, max_width)
        
        #TODO: Handle ImageScaled[], and Scaled[]
        lighting_option = graphics_options['Lighting']
        lighting = lighting_option.to_python()
        self.lighting = []

        if lighting == 'Automatic':
            self.lighting = [
                {"type": "Ambient", "color": [0.3, 0.2, 0.4]},
                {"type": "Directional", "color": [0.8, 0., 0.], "position": [2, 0, 2]},
                {"type": "Directional", "color": [0., 0.8, 0.], "position": [2, 2, 2]},
                {"type": "Directional", "color": [0., 0., 0.8], "position": [0, 2, 2]}
            ]
        elif lighting == 'Neutral':
            self.lighting = [
                {"type": "Ambient", "color": [0.3, 0.3, 0.3]},
                {"type": "Directional", "color": [0.3, 0.3, 0.3], "position": [2, 0, 2]},
                {"type": "Directional", "color": [0.3, 0.3, 0.3], "position": [2, 2, 2]},
                {"type": "Directional", "color": [0.3, 0.3, 0.3], "position": [0, 2, 2]}
            ]
        elif lighting == 'None':
            pass

        elif isinstance(lighting, list) and all(isinstance(light, list) for light in lighting):
            for light in lighting:
                if light[0] in ['"Ambient"', '"Directional"', '"Point"', '"Spot"']:
                    try:
                        head = light[1].get_head_name()
                    except AttributeError:
                        break
                    color = get_class(head)(light[1])
                    if light[0] == '"Ambient"':
                        self.lighting.append({
                            "type": "Ambient",
                             "color": color.to_rgba()
                        })
                    elif light[0] == '"Directional"':
                        position = [0,0,0]
                        if isinstance(light[2], list):
                            if len(light[2]) == 3:
                                position = light[2]
                            if len(light[2]) == 2 and all(isinstance(p, list) and len(p) == 3 for p in light[2]):
                                position = [light[2][0][i] - light[2][1][i] for i in range(3)]
                        self.lighting.append({
                            "type": "Directional",
                            "color": color.to_rgba(),
                            "position": position
                        })
                    elif light[0] == '"Point"':
                        position = [0,0,0]
                        if isinstance(light[2], list) and len(light[2]) == 3:
                            position = light[2]
                        self.lighting.append({
                            "type": "Point",
                            "color": color.to_rgba(),
                            "position": position
                        })
                    elif light[0] == '"Spot"':
                        position = [0,0,1]
                        target = [0,0,0]
                        if isinstance(light[2], list):
                            if len(light[2]) == 2:
                                if isinstance(light[2][0], list) and len(light[2][0]) == 3:
                                    position = light[2][0]
                                if isinstance(light[2][1], list) and len(light[2][1]) == 3:
                                    target = light[2][1]
                            if len(light[2]) == 3:
                                position = light[2]
                        angle = light[3]
                        self.lighting.append({
                            "type": "Spot",
                            "color": color.to_rgba(),
                            "position": position,
                            "target": target,
                            "angle": angle
                        })

        else:
            evaluation.message("Graphics3D", 'invlight', lighting_option)

        # ViewPoint Option
        viewpoint_option  = graphics_options['ViewPoint']
        viewpoint = viewpoint_option.to_python(n_evaluation=evaluation)

        if isinstance(viewpoint, list) and len(viewpoint) == 3:
            if all(isinstance(x, numbers.Real) for x in viewpoint):
                pass
                #TODO Infinite coordinates e.g. {0, 0, Infinity}
        else:
            try:
                viewpoint = {
                    'Above': [0,0,2],
                    'Below': [0,0,-2],
                    'Front': [0,-2,0],
                    'Back': [0,2,0],
                    'Left': [-2,0,0],
                    'Right': [2,0,0]
                }[viewpoint]
            except KeyError:
                #evaluation.message()            
                #TODO
                viewpoint = [1.3,-2.4,2]

        assert(isinstance(viewpoint, list) and len(viewpoint) == 3 and all(isinstance(x, numbers.Real) for x in viewpoint))
        self.viewpoint = viewpoint

        #TODO Aspect Ratio
        #aspect_ratio = graphics_options['AspectRatio'].to_python()
            
        boxratios = graphics_options['BoxRatios'].to_python()
        if boxratios == 'Automatic':
            boxratios = ['Automatic', 'Automatic', 'Automatic']
        else:
            boxratios = boxratios
        if not isinstance(boxratios, list) or len(boxratios) != 3:
            raise BoxConstructError

        plot_range = graphics_options['PlotRange'].to_python()            
        if plot_range == 'Automatic':
            plot_range = ['Automatic', 'Automatic', 'Automatic']
        if not isinstance(plot_range, list) or len(plot_range) != 3:
            raise BoxConstructError
        
        try:
            elements = Graphics3DElements(leaves[0], evaluation)
        except NumberError:
            raise BoxConstructError
        
        def calc_dimensions(final_pass=True):
            if 'Automatic' in plot_range:
                xmin, xmax, ymin, ymax, zmin, zmax = elements.extent()
            else:
                xmin = xmax = ymin = ymax = zmin = zmax = None
            
            try:
                if plot_range[0] == 'Automatic':
                    if xmin is None and xmax is None:
                        xmin = 0
                        xmax = 1
                    elif xmin == xmax:
                        xmin -= 1
                        xmax += 1
                elif isinstance(plot_range[0], list) and len(plot_range[0]) == 2:
                    xmin, xmax = map(float, plot_range[0])
                    xmin = elements.translate((xmin, 0, 0))[0]
                    xmax = elements.translate((xmax, 0, 0))[0]
                else:
                    raise BoxConstructError
                
                if plot_range[1] == 'Automatic':
                    if ymin is None and ymax is None:
                        ymin = 0
                        ymax = 1
                    elif ymin == ymax:
                        ymin -= 1
                        ymax += 1
                elif isinstance(plot_range[1], list) and len(plot_range[1]) == 2:
                    ymin, ymax = map(float, plot_range[1])
                    ymin = elements.translate((0, ymin, 0))[1]
                    ymax = elements.translate((0, ymax, 0))[1]
                else:
                    raise BoxConstructError
                
                if plot_range[2] == 'Automatic':
                    if zmin is None and zmax is None:
                        zmin = 0
                        zmax = 1
                    elif zmin == zmax:
                        zmin -= 1
                        zmax += 1
                elif isinstance(plot_range[1], list) and len(plot_range[1]) == 2:
                    zmin, zmax = map(float, plot_range[2])
                    zmin = elements.translate((0, 0, zmin))[2]
                    zmax = elements.translate((0, 0, zmax))[2]
                else:
                    raise BoxConstructError
            except (ValueError, TypeError):
                raise BoxConstructError
            
            boxscale = [1., 1., 1.]
            if boxratios[0] != 'Automatic':
                boxscale[0] = boxratios[0] / (xmax - xmin)
            if boxratios[1] != 'Automatic':
                boxscale[1] = boxratios[1] / (ymax - ymin)
            if boxratios[2] != 'Automatic':
                boxscale[2] = boxratios[2] / (zmax - zmin)

            if final_pass:
                xmin *= boxscale[0]
                xmax *= boxscale[0]
                ymin *= boxscale[1]
                ymax *= boxscale[1]
                zmin *= boxscale[2]
                zmax *= boxscale[2]

                # Rescale lighting
                for i,light in enumerate(self.lighting):
                    if self.lighting[i]["type"] != "Ambient":
                        self.lighting[i]["position"] = [light["position"][j] * boxscale[j] for j in range(3)]
                    if self.lighting[i]["type"] == "Spot":
                        self.lighting[i]["target"] = [light["target"][j] * boxscale[j] for j in range(3)]

                # Rescale viewpoint
                self.viewpoint = [vp / max([xmax-xmin, ymax-ymin, zmax-zmin]) for vp in self.viewpoint]

            return xmin, xmax, ymin, ymax, zmin, zmax, boxscale

        xmin, xmax, ymin, ymax, zmin, zmax, boxscale = calc_dimensions(final_pass=False)

        axes, ticks = self.create_axes(elements, graphics_options, xmin, xmax, ymin, ymax, zmin, zmax, boxscale)

        return elements, axes, ticks, calc_dimensions, boxscale
    
    def boxes_to_tex(self, leaves, **options):
        elements, axes, ticks, calc_dimensions, boxscale = self._prepare_elements(leaves, options, max_width=450)
        
        elements._apply_boxscaling(boxscale)

        asy = elements.to_asy()

        xmin, xmax, ymin, ymax, zmin, zmax, boxscale = calc_dimensions()

        # draw boundbox
        pen = create_pens(edge_color=RGBColor(components=(0.4,0.4,0.4,1)), stroke_width=1)
        boundbox_asy = ''
        boundbox_lines = self.get_boundbox_lines(xmin, xmax, ymin, ymax, zmin, zmax)
        for line in boundbox_lines:
            path = '--'.join(['(%s,%s,%s)' % coords for coords in line])
            boundbox_asy += 'draw((%s), %s);\n' % (path, pen)

        (height, width) = (400, 400) #TODO: Proper size
        tex = r"""
\begin{asy}
import three;
size(%scm, %scm);
currentprojection=perspective(%s,%s,%s);
currentlight=light(blue, specular=red, (2,0,2), (2,2,2), (0,2,2));
%s
%s
\end{asy}
""" % (asy_number(width/60), asy_number(height/60), self.viewpoint[0], self.viewpoint[1], self.viewpoint[2], asy, boundbox_asy)
        return tex

    def boxes_to_xml(self, leaves, **options):
        elements, axes, ticks, calc_dimensions, boxscale = self._prepare_elements(leaves, options)

        elements._apply_boxscaling(boxscale)

        json_repr = elements.to_json()
        
        xmin, xmax, ymin, ymax, zmin, zmax, boxscale = calc_dimensions()
        
        json_repr = json.dumps({
            'elements': json_repr,
            'axes': {
                'hasaxes': axes,
                'ticks': ticks,
            },
            'extent': {
                'xmin': xmin,
                'xmax': xmax,
                'ymin': ymin,
                'ymax': ymax,
                'zmin': zmin,
                'zmax': zmax,
            },
            'lighting': self.lighting,
        })
        
        #return "<mn>3</mn>"
        
        #xml = """<graphics3d xmin="%f" xmax="%f" ymin="%f" ymax="%f" zmin="%f" zmax="%f" data="%s" />""" % (
        #    xmin, xmax, ymin, ymax, zmin, zmax, json_repr)        
        xml = """<graphics3d data="%s" />""" % escape_html(json_repr)
        xml = """<mtable><mtr><mtd>%s</mtd></mtr></mtable>""" % xml
        return xml
    
    def create_axes(self, elements, graphics_options, xmin, xmax, ymin, ymax, zmin, zmax, boxscale):
        axes = graphics_options.get('Axes')
        if axes.is_true():
            axes = (True, True, True)
        elif axes.has_form('List', 3):
            axes = (axes.leaves[0].is_true(), axes.leaves[1].is_true(), axes.leaves[2].is_true())
        else:
            axes = {}
        ticks_style = graphics_options.get('TicksStyle')
        axes_style = graphics_options.get('AxesStyle')
        label_style = graphics_options.get('LabelStyle')
        if ticks_style.has_form('List', 3):
            ticks_style = ticks_style.leaves
        else:
            ticks_style = [ticks_style] * 3
        if axes_style.has_form('List', 3):
            axes_style = axes_style.leaves
        else:
            axes_style = [axes_style] * 3
        
        ticks_style = [elements.create_style(s) for s in ticks_style]
        axes_style = [elements.create_style(s) for s in axes_style]
        label_style = elements.create_style(label_style)
        ticks_style[0].extend(axes_style[0])
        ticks_style[1].extend(axes_style[1])

        ticks = [self.axis_ticks(xmin, xmax), self.axis_ticks(ymin, ymax), self.axis_ticks(zmin, zmax)]

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

        # Convert ticks to nice strings e.g 0.100000000000002 -> '0.1' and scale ticks appropriately
        ticks = [[map(lambda x: boxscale[i] * x, t[0]), map(lambda x: boxscale[i] * x, t[1]), map(str, t[0])] for i,t in enumerate(ticks)]

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
        [(xmax, ymax, zmin), (xmax, ymax, zmax)]]
            
def total_extent_3d(extents):
    xmin = xmax = ymin = ymax = zmin = zmax = None
    for extent in extents:
        for x, y, z in extent:
            if xmin is None or x < xmin: xmin = x
            if xmax is None or x > xmax: xmax = x
            if ymin is None or y < ymin: ymin = y
            if ymax is None or y > ymax: ymax = y
            if zmin is None or z < zmin: zmin = z
            if zmax is None or z > zmax: zmax = z
    return xmin, xmax, ymin, ymax, zmin, zmax
            
class Graphics3DElements(_GraphicsElements):
    coords = Coords3D
    def __init__(self, content, evaluation, neg_y=False):
        super(Graphics3DElements, self).__init__(content, evaluation)
        self.neg_y = neg_y
        self.xmin = self.ymin = self.pixel_width = self.pixel_height = self.extent_width = self.extent_height = None
    
    def extent(self, completely_visible_only=False):
        return total_extent_3d([element.extent() for element in self.elements])
    
    def to_svg(self):
        return '\n'.join(element.to_svg() for element in self.elements)
    
    def to_asy(self):
        return '\n'.join([element.to_asy() for element in self.elements])

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
        if list(face_color.to_rgba()[:3]) == [1,1,1]:
            face_color = RGBColor(components=(0,0,0,face_color.to_rgba()[3]))

        for line in self.lines:
            data.append({
                'type': 'point',
                'coords': [coords.pos() for coords in line],
                'color': face_color.to_rgba(),
            })
        return data

    def to_asy(self):
        face_color = self.face_color
        
        # Tempoary bug fix: default Point color should be black not white
        if list(face_color.to_rgba()[:3]) == [1,1,1]:
            face_color = RGBColor(components=(0,0,0,face_color.to_rgba()[3]))
        pen = create_pens(face_color=face_color, is_face_element=False)

        asy = ''
        for line in self.lines:
            asy += 'path3 g=' + '--'.join(['(%s,%s,%s)' % coords.pos()[0] for coords in line]) + '--cycle;'
            asy += 'dot(g, %s);' % (pen)
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
    
class Line3DBox(LineBox):
    def init(self, *args, **kwargs):
        super(Line3DBox, self).init(*args, **kwargs)

    def process_option(self, name, value):
        super(Line3DBox, self).process_option(name, value)

    def to_json(self):
        # TODO: account for line widths and style
        data = []
        for line in self.lines:
            data.append({
                'type': 'line',
                'coords': [coords.pos() for coords in line],
                'color': self.edge_color.to_rgba(),
            })
        return data

    def to_asy(self):
        #l = self.style.get_line_width(face_element=False)
        pen = create_pens(edge_color=self.edge_color, stroke_width=1)
        asy = ''
        for line in self.lines:
            path = '--'.join(['(%s,%s,%s)' % coords.pos()[0] for coords in line])
            asy += 'draw(%s, %s);' % (path, pen)
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

class Polygon3DBox(PolygonBox):
    def init(self, *args, **kwargs):
        self.vertex_normals = None
        super(Polygon3DBox, self).init(*args, **kwargs)
    
    def process_option(self, name, value):
        if name == 'VertexNormals':
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
        data = []
        for line in self.lines:
            data.append({
                'type': 'polygon',
                'coords': [coords.pos() for coords in line],
                'faceColor': face_color.to_js() if face_color is not None else None,
            })
        return data
    
    def to_asy(self):
        l = self.style.get_line_width(face_element=True)
        if self.vertex_colors is None:
            face_color = self.face_color
        else:
            face_color = None
        pen = create_pens(edge_color=self.edge_color, face_color=face_color, stroke_width=l, is_face_element=True)

        asy = ''
        for line in self.lines:
            asy += 'path3 g=' + '--'.join(['(%s,%s,%s)' % coords.pos()[0] for coords in line]) + '--cycle;'
            asy += 'draw(surface(g), %s);' % (pen)
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

GLOBALS3D = {
    'Polygon3DBox': Polygon3DBox,
    'Line3DBox': Line3DBox,
    'Point3DBox': Point3DBox,
}
