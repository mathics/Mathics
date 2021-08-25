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
      <dd>return a uniform polyhedron with the given name.
      <dd>Names are "tetrahedron", "octahedron", "dodecahedron", or "icosahedron".
    </dl>

    >> Graphics3D[UniformPolyhedron["octahedron"]]
     = -Graphics3D-

    >> Graphics3D[UniformPolyhedron["dodecahedron"]]
     = -Graphics3D-

    >> Graphics3D[{"Brown", UniformPolyhedron["tetrahedron"]}]
     = -Graphics3D-
    """

    messages = {
        "argtype": f"Argument `1` is not one of: {uniform_polyhedra_names}",
    }

    rules = {
        "UniformPolyhedron[name_String]": "UniformPolyhedron[name, {{0, 0, 0}}, 1]",
    }

    def apply(self, name, positions, edgelength, evaluation):
        "UniformPolyhedron[name_String, positions_List, edgelength_?NumberQ]"

        if name.to_python(string_quotes=False) not in uniform_polyhedra_set:
            evaluation.error("UniformPolyhedron", "argtype", name)

        return Expression("UniformPolyhedron", name, positions, edgelength)


class Dodecahedron(Builtin):
    """
    <dl>
      <dt>'Dodecahedron[]'
      <dd>a regular dodecahedron centered at the origin with unit edge length.
    </dl>

    >> Graphics3D[Dodecahedron[]]
     = -Graphics3D-
    """

    rules = {
        "Dodecahedron[]": """UniformPolyhedron["dodecahedron"]""",
        "Dodecahedron[l_?NumberQ]": """UniformPolyhedron["dodecahedron", {{0, 0, 0}}, l]""",
        "Dodecahedron[positions_List, l_?NumberQ]": """UniformPolyhedron["dodecahedron", positions, l]""",
    }


class Icosahedron(Builtin):
    """
    <dl>
      <dt>'Icosahedron[]'
      <dd>a regular Icosahedron centered at the origin with unit edge length.
    </dl>

    >> Graphics3D[Icosahedron[]]
     = -Graphics3D-
    """

    rules = {
        "Icosahedron[]": """UniformPolyhedron["icosahedron"]""",
        "Icosahedron[l_?NumberQ]": """UniformPolyhedron["icosahedron", {{0, 0, 0}}, l]""",
        "Icosahedron[positions_List, l_?NumberQ]": """UniformPolyhedron["icosahedron", positions, l]""",
    }


class Octahedron(Builtin):
    """
    <dl>
      <dt>'Octahedron[]'
      <dd>a regular octahedron centered at the origin with unit edge length.
    </dl>

    >> Graphics3D[{Red, Octahedron[]}]
     = -Graphics3D-
    """

    rules = {
        "Octahedron[]": """UniformPolyhedron["octahedron"]""",
        "Octahedron[l_?NumberQ]": """UniformPolyhedron["octahedron", {{0, 0, 0}}, l]""",
        "Octahedron[positions_List, l_?NumberQ]": """UniformPolyhedron["octahedron", positions, l]""",
    }


class Tetrahedron(Builtin):
    """
    <dl>
      <dt>'Tetrahedron[]'
      <dd>a regular tetrahedron centered at the origin with unit edge length.
    </dl>

    >> Graphics3D[Tetrahedron[{{0,0,0}, {1,1,1}}, 2], Axes->True]
     = -Graphics3D-
    """

    rules = {
        "Tetrahedron[]": """UniformPolyhedron["tetrahedron"]""",
        "Tetrahedron[l_?NumberQ]": """UniformPolyhedron["tetrahedron", {{0, 0, 0}}, l]""",
        "Tetrahedron[positions_List, l_?NumberQ]": """UniformPolyhedron["tetrahedron", positions, l]""",
    }

    def apply_with_length(self, length, evaluation):
        "Tetrahedron[l_?Numeric]"
