from mathics.builtin.box.graphics3d import _Graphics3DElement

from mathics.builtin.drawing.graphics_internals import GLOBALS3D

class UniformPolyhedron3DBox(_Graphics3DElement):
    pass

# FIXME: GLOBALS3D is a horrible name.
GLOBALS3D.update(
    {
        "System`UniformPolyhedron3DBox": UniformPolyhedron3DBox,
    }
)
