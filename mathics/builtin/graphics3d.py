# -*- coding: utf8 -*-

"""
Graphics (3D)
"""
        
from mathics.core.expression import NumberError, from_python, Real
from mathics.builtin.base import BoxConstruct, BoxConstructError
from graphics import Graphics, GraphicsBox, _GraphicsElements, PolygonBox, LineBox, PointBox

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
    })
    
    box_suffix = '3DBox'
    
    rules = {
        'MakeBoxes[Graphics3D[content_, OptionsPattern[Graphics3D]], OutputForm]': '"-Graphics3D-"',
    }
    
class Graphics3DBox(GraphicsBox):
    def boxes_to_text(self, leaves, **options):
        return '-Graphics3D-'
        
    def _prepare_elements(self, leaves, options, max_width=None):
        if not leaves:
            raise BoxConstructError
        
        graphics_options = self.get_option_values(leaves[1:], **options)
        
        base_width, base_height, size_multiplier, size_aspect = self._get_image_size(options,
            graphics_options, max_width)
        
        aspect_ratio = graphics_options['AspectRatio']
            
        plot_range = graphics_options['PlotRange'].to_python()            
        if plot_range == 'Automatic':
            plot_range = ['Automatic', 'Automatic', 'Automatic']
        if not isinstance(plot_range, list) or len(plot_range) != 3:
            raise BoxConstructError
        
        try:
            elements = Graphics3DElements(leaves[0], options['evaluation'])
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
            
            return xmin, xmax, ymin, ymax, zmin, zmax
            
        xmin, xmax, ymin, ymax, zmin, zmax = calc_dimensions(final_pass=False)
        
        axes, ticks = self.create_axes(elements, graphics_options, xmin, xmax, ymin, ymax, zmin, zmax)
        
        return elements, axes, ticks, calc_dimensions
    
    def boxes_to_tex(self, leaves, **options):
        elements, axes, ticks, calc_dimensions = self._prepare_elements(leaves, options, max_width=450)
        
        asy = elements.to_asy()
        
        xmin, xmax, ymin, ymax, zmin, zmax = calc_dimensions()
        
        return r"""
\begin{asy}
size{1cm, 1cm};
// TODO: render 3D in Asymptote
\end{asy}
        """
    
    def boxes_to_xml(self, leaves, **options):
        elements, axes, ticks, calc_dimensions = self._prepare_elements(leaves, options)

        json_repr = elements.to_json()

        # Convert ticks to nice strings e.g 0.100000000000002 -> '0.1'
        ticks = [(map(str, x[0]), x[1]) for x in ticks]

        xmin, xmax, ymin, ymax, zmin, zmax = calc_dimensions()
        
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
        })
        
        #return "<mn>3</mn>"
        
        #xml = """<graphics3d xmin="%f" xmax="%f" ymin="%f" ymax="%f" zmin="%f" zmax="%f" data="%s" />""" % (
        #    xmin, xmax, ymin, ymax, zmin, zmax, json_repr)        
        xml = """<graphics3d data="%s" />""" % escape_html(json_repr)
        xml = """<mtable><mtr><mtd>%s</mtd></mtr></mtable>""" % xml
        return xml
    
    def create_axes(self, elements, graphics_options, xmin, xmax, ymin, ymax, zmin, zmax):
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

        return axes, ticks
            
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
    
    def extent(self, completely_visible_only=False):
        return total_extent_3d([element.extent() for element in self.elements])
    
    def to_svg(self):
        return '\n'.join(element.to_svg() for element in self.elements)
    
    def to_asy(self):
        return '\n'.join([element.to_asy() for element in self.elements])
    
    def to_json(self):
        result = []
        for element in self.elements:
            result.extend(element.to_json())
        return result

class Point3DBox(PointBox):
    def init(self, *args, **kwargs):
        super(Point3DBox, self).init(*args, **kwargs)

    def process_option(self, name, value):
        super(Point3DBox, self).process_option(name, value)

    def to_json(self):
        # TODO: account for point size and style
        data = []
        for line in self.lines:
            data.append({
                'type': 'point',
                'coords': [coords.pos() for coords in line],
            })
        return data

    def to_asy(self):
        # TODO
        return ''
    
    def extent(self):
        result = []
        for line in self.lines:
            for c in line:
                p, d = c.pos()
                result.append(p)
        return result
    
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
            })
        return data

    def to_asy(self):
        # TODO
        return ''
    
    def extent(self):
        result = []
        for line in self.lines:
            for c in line:
                p, d = c.pos()
                result.append(p)
        return result

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
        # TODO
        return ''
    
    def extent(self):
        result = []
        for line in self.lines:
            for c in line:
                p, d = c.pos()
                result.append(p)
        return result

class Cylinder3DBox(Graphics3DElements):
    def init(self, *args, **kwargs):
        super(Cylinder3DBox, self).init(*args, **kwargs)

GLOBALS3D = {
    'Polygon3DBox': Polygon3DBox,
    'Line3DBox': Line3DBox,
    'Point3DBox': Point3DBox,
}
