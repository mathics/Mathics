from mathics.builtin.box.graphics3d import _Graphics3DElement, Coords3D

from mathics.builtin.base import BoxConstructError
from mathics.builtin.drawing.graphics_internals import GLOBALS3D
from mathics.builtin.colors.color_directives import _Color

from mathics.builtin.drawing.uniform_polyhedra import uniform_polyhedra_set

import numbers


class UniformPolyhedron3DBox(_Graphics3DElement):
    def init(self, graphics, style, item):
        super(UniformPolyhedron3DBox, self).init(graphics, item, style)
        self.edge_color, self.face_color = style.get_style(_Color, face_element=True)

        if len(item.leaves) != 3:
            raise BoxConstructError

        points = item.leaves[1].to_python()
        if not all(
            len(point) == 3 and all(isinstance(p, numbers.Real) for p in point)
            for point in points
        ):
            raise BoxConstructError

        self.points = [Coords3D(graphics, pos=point) for point in points]
        self.edge_length = item.leaves[2].to_python()
        self.sub_type = item.leaves[0].to_python(string_quotes=False)

    def extent(self):
        result = []

        # TODO: correct extent calculation, the current one is approximated
        result.extend(
            [
                coords.add(self.edge_length, self.edge_length, self.edge_length).pos()[
                    0
                ]
                for coords in self.points
            ]
        )
        result.extend(
            [
                coords.add(
                    -self.edge_length, -self.edge_length, -self.edge_length
                ).pos()[0]
                for coords in self.points
            ]
        )
        return result

    def _apply_boxscaling(self, boxscale):
        # No box scaling for now
        return


# FIXME: GLOBALS3D is a horrible name.
GLOBALS3D.update(
    {
        "System`UniformPolyhedron3DBox": UniformPolyhedron3DBox,
    }
)
