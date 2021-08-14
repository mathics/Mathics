from mathics.builtin.box.graphics3d import _Graphics3DElement

from mathics.builtin.drawing.graphics_internals import GLOBALS3D
from mathics.builtin.colors.color_directives import _Color


class UniformPolyhedron3DBox(_Graphics3DElement):
    def init(self, graphics, style, item):
        super(UniformPolyhedron3DBox, self).init(graphics, item, style)
        self.edge_color, self.face_color = style.get_style(_Color, face_element=True)
        if len(item.leaves) != 1:
            raise BoxConstructError
        self.sub_type = item.leaves[0].to_python(string_quotes=False)

    def extent(self):
        # FIXME: figure this out.
        min_point = [0, 0, 0]
        max_point = [100, 100, 100]
        return [min_point, max_point]

    def _apply_boxscaling(self, boxscale):
        # No box scaling for now
        return


# FIXME: GLOBALS3D is a horrible name.
GLOBALS3D.update(
    {
        "System`UniformPolyhedron3DBox": UniformPolyhedron3DBox,
    }
)
