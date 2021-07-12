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
    Arrow3DBox,
    Line3DBox,
    Point3DBox,
    Polygon3DBox,
    Sphere3DBox,
)

# FIXME
# Add 2D elements like DensityPlot


from mathics.core.formatter import lookup_method, add_conversion_fn


def convert_coord_collection(
    collection: list, object_type: str, color, default_values: dict = {}
) -> list:
    """Convert collection into a list of dictionary items where each item is some sort of lower-level
    JSON object.
    """
    data = []
    opacity = 1 if len(color) < 4 else color[3]
    for items in collection:
        data.append(
            {
                **default_values,
                **{
                    "type": object_type,
                    "coords": [coords.pos() for coords in items],
                    "opacity": opacity,
                    "rgb_color": color[:3],
                },
            }
        )
    # print(data)
    return data


def graphics_3D_elements(self, **options) -> list:
    """Iterates over self.elements to convert each item.
    The list of converted items is returned.
    """
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


def arrow_3d_box(self):
    """
    Compact (lower-level) JSON formatting of a Arrow3DBox.
    """
    # TODO: account for arrow widths and style
    color = self.edge_color.to_rgba()
    data = convert_coord_collection(self.lines, "arrow", color, {"color": color})
    # print("### json Arrow3DBox", data)
    return data


add_conversion_fn(Arrow3DBox, arrow_3d_box)


def cylinder_3d_box(self):
    """
    Compact (lower-level) JSON formatting of a Cylinder3DBox.
    """
    face_color = self.face_color
    if face_color is not None:
        face_color = face_color.to_js()
    data = convert_coord_collection(
        [self.points],
        "cylinder",
        face_color,
        {"faceColor": face_color, "radius": self.radius},
    )
    # print("### json Cylinder3DBox", data)
    return data


add_conversion_fn(Cylinder3DBox, cylinder_3d_box)


def line_3d_box(self):
    """
    Compact (lower-level) JSON formatting of a Line3DBox.
    """
    # TODO: account for line widths and style
    data = []
    color = self.edge_color.to_rgba()
    data = convert_coord_collection(self.lines, "line", color, {"color": color})
    # print("### json Line3DBox", data)
    return data


add_conversion_fn(Line3DBox, line_3d_box)


def point_3d_box(self) -> list:
    """
    Compact (lower-level) JSON formatting of a Point3DBox.
    """
    # TODO: account for point size
    data = []

    # Tempoary bug fix: default Point color should be black not white
    face_color = self.face_color.to_rgba()
    if list(face_color[:3]) == [1, 1, 1]:
        face_color = RGBColor(components=(0, 0, 0, face_color[3])).to_rgba()

    point_size, _ = self.style.get_style(PointSize, face_element=False)
    relative_point_size = 0.01 if point_size is None else point_size.value

    data = convert_coord_collection(
        self.lines,
        "point",
        face_color,
        {"color": face_color, "pointSize": relative_point_size},
    )

    # print("### json Point3DBox", data)
    return data


add_conversion_fn(Point3DBox, point_3d_box)


def polygon_3d_box(self) -> list:
    """
    Compact (lower-level) JSON formatting of a Polygont3DBox.
    """
    # TODO: account for line widths and style
    if self.vertex_colors is None:
        face_color = self.face_color
    else:
        face_color = None

    if face_color is not None:
        face_color = face_color.to_js()

    data = convert_coord_collection(
        self.lines,
        "polygon",
        face_color,
        {"faceColor": face_color},
    )
    # print("### json Polygon3DBox", data)
    return data


add_conversion_fn(Polygon3DBox, polygon_3d_box)


def sphere_3d_box(self) -> list:
    face_color = self.face_color
    if face_color is not None:
        face_color = face_color.to_js()
    data = convert_coord_collection(
        [self.points],
        "sphere",
        face_color,
        {"faceColor": face_color, "radius": self.radius},
    )
    # print("### json Sphere3DBox", data)
    return data


add_conversion_fn(Sphere3DBox, sphere_3d_box)
