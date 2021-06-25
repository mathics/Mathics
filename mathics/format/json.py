# -*- coding: utf-8 -*-
"""
Format a Mathics object as JSON data
"""

from mathics.builtin.graphics import PointSize, RGBColor

from mathics.builtin.drawing.graphics3d import (
    Graphics3DElements,
)

from mathics.builtin.box.graphics3d import (
    Cylinder3DBox,
    Line3DBox,
    Point3DBox,
    Polygon3DBox,
    Sphere3DBox,
)

# FIXME
# Add 2D elements like DensityPlot


from mathics.core.formatter import lookup_method, add_conversion_fn


def graphics_3D_elements(self, **options):
    result = []
    for element in self.elements:
        format_fn = lookup_method(element, "json")
        if format_fn is None:
            result += element.to_json()
        else:
            result += format_fn(element)

    # print("### json Graphics3DElements", result)
    return result


add_conversion_fn(Graphics3DElements, graphics_3D_elements)


def cylinder_3d_box(self):
    face_color = self.face_color
    if face_color is not None:
        face_color = face_color.to_js()
    return [
        {
            "type": "cylinder",
            "coords": [coords.pos() for coords in self.points],
            "radius": self.radius,
            "faceColor": face_color,
        }
    ]


add_conversion_fn(Cylinder3DBox, cylinder_3d_box)


def line_3d_box(self):
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
    # print("### json Line3DBox", data)
    return data


add_conversion_fn(Line3DBox, line_3d_box)


def point_3d_box(self):
    # TODO: account for point size
    data = []

    # Tempoary bug fix: default Point color should be black not white
    face_color = self.face_color
    if list(face_color.to_rgba()[:3]) == [1, 1, 1]:
        face_color = RGBColor(components=(0, 0, 0, face_color.to_rgba()[3]))

    point_size, _ = self.style.get_style(PointSize, face_element=False)
    relative_point_size = 0.01 if point_size is None else point_size.value

    for line in self.lines:
        data.append(
            {
                "type": "point",
                "coords": [coords.pos() for coords in line],
                "color": face_color.to_rgba(),
                "pointSize": relative_point_size,
            }
        )
    # print("### json Point3DBox", data)
    return data


add_conversion_fn(Point3DBox, point_3d_box)


def polygon_3d_box(self):
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
    # print("### json Polygon3DBox", data)
    return data


add_conversion_fn(Polygon3DBox, polygon_3d_box)


def sphere_3d_box(self):
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


add_conversion_fn(Sphere3DBox, sphere_3d_box)
