# -*- coding: utf-8 -*-

"""
Uniform Polyhedra

Uniform polyhedra is the grouping of platonic solids, Archimedean solids, and regular star polyhedra.
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression

uniform_polyhedra_names = "tetrahedron, octahedron, dodecahedron, icosahedron"
uniform_polyhedra_set = frozenset(uniform_polyhedra_names.split(", "))

class UniformPolyhedron(Builtin):
    """
    <dl>
      <dt>'UniformPolyhedron["name"]'
      <dd>is a uniform polyhedron with the given name.
    </dl>

    >> Graphics3D[UniformPolyhedron["tetrahedron"]
     = -Graphics3D-

    >> Graphics3D[UniformPolyhedron["octahedron"]
     = -Graphics3D-
    """

    messages = {
        "argtype": f"Argument `1` is not one of: {uniform_polyhedra_names}",
    }


    def apply_with_name(self, name, evaluation):
        "UniformPolyhedron[name_String]"

        return Expression("UniformPolyhedron", name)


class Tetrahedron(Builtin):
    """
    <dl>
      <dt>'Tetrahedron[]'
      <dd>a regular tetrahedron centered at the origin with unit edge length.
    </dl>

    >> Graphics3D[Tetrahedraon[]]
     = -Graphics3D-
    """
    rules = {"Tetrahedron[]": """UniformPolyhedron["tetrahedron"]"""}
