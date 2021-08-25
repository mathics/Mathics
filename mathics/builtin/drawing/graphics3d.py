# -*- coding: utf-8 -*-

"""Three-Dimensional Graphics

Functions for working with 3D graphics.
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.core.expression import Expression, Real

from mathics.builtin.base import BoxConstructError, Builtin, InstanceableBuiltin
from mathics.builtin.colors.color_directives import RGBColor
from mathics.builtin.graphics import (
    _GraphicsElements,
    CoordinatesError,
    Graphics,
    Style,
)
from mathics.builtin.lists import List


def coords3D(value):
    if value.has_form("List", 3):
        result = (
            value.leaves[0].round_to_float(),
            value.leaves[1].round_to_float(),
            value.leaves[2].round_to_float(),
        )
        if None not in result:
            return result
    raise CoordinatesError


class Coords3D(object):
    def __init__(self, graphics, expr=None, pos=None, d=None):
        self.graphics = graphics
        self.p = pos
        self.d = d
        if expr is not None:
            if expr.has_form("Offset", 1, 2):
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
        p = (self.p[0] + x, self.p[1] + y, self.p[2] + z)
        return Coords3D(self.graphics, pos=p, d=self.d)

    def scale(self, a):
        self.p = (self.p[0] * a[0], self.p[1] * a[1], self.p[2] * a[2])


class Style3D(Style):
    def get_default_face_color(self):
        return RGBColor(components=(1, 1, 1, 1))


class _Graphics3DElement(InstanceableBuiltin):
    def init(self, graphics, item=None, style=None):
        if (
            item is not None
            and hasattr(item, "has_form")
            and not item.has_form(self.get_name(), None)
        ):
            raise BoxConstructError
        self.graphics = graphics
        self.style = style
        self.is_completely_visible = False  # True for axis elements


class Graphics3D(Graphics):
    r"""
    <dl>
      <dt>'Graphics3D[$primitives$, $options$]'
      <dd>represents a three-dimensional graphic.

      <dd>See also the Section "Plotting" for a list of Plot options.
    </dl>

    >> Graphics3D[Polygon[{{0,0,0}, {0,1,1}, {1,0,0}}]]
     = -Graphics3D-

    In 'TeXForm', 'Graphics3D' creates Asymptote figures:
    >> Graphics3D[Sphere[]] // TeXForm
     = #<--#
     . \begin{asy}
     . import three;
     . import solids;
     . size(6.6667cm, 6.6667cm);
     . currentprojection=perspective(2.6,-4.8,4.0);
     . currentlight=light(rgb(0.5,0.5,1), specular=red, (2,0,2), (2,2,2), (0,2,2));
     . // Sphere3DBox
     . draw(surface(sphere((0, 0, 0), 1)), rgb(1,1,1));
     . draw(((-1,-1,-1)--(1,-1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,1,-1)--(1,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,-1,1)--(1,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,1,1)--(1,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,-1,-1)--(-1,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((1,-1,-1)--(1,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,-1,1)--(-1,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((1,-1,1)--(1,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,-1,-1)--(-1,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((1,-1,-1)--(1,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-1,1,-1)--(-1,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((1,1,-1)--(1,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . \end{asy}

    #> Graphics3D[Point[Table[{Sin[t], Cos[t], 0}, {t, 0, 2. Pi, Pi / 15.}]]] // TeXForm
     = #<--#
     . \begin{asy}
     . import three;
     . import solids;
     . size(6.6667cm, 6.6667cm);
     . currentprojection=perspective(2.6,-4.8,4.0);
     . currentlight=light(rgb(0.5,0.5,1), specular=red, (2,0,2), (2,2,2), (0,2,2));
     . // Point3DBox
     . path3 g=(0,1,0)--(0.20791,0.97815,0)--(0.40674,0.91355,0)--(0.58779,0.80902,0)--(0.74314,0.66913,0)--(0.86603,0.5,0)--(0.95106,0.30902,0)--(0.99452,0.10453,0)--(0.99452,-0.10453,0)--(0.95106,-0.30902,0)--(0.86603,-0.5,0)--(0.74314,-0.66913,0)--(0.58779,-0.80902,0)--(0.40674,-0.91355,0)--(0.20791,-0.97815,0)--(5.6655e-16,-1,0)--(-0.20791,-0.97815,0)--(-0.40674,-0.91355,0)--(-0.58779,-0.80902,0)--(-0.74314,-0.66913,0)--(-0.86603,-0.5,0)--(-0.95106,-0.30902,0)--(-0.99452,-0.10453,0)--(-0.99452,0.10453,0)--(-0.95106,0.30902,0)--(-0.86603,0.5,0)--(-0.74314,0.66913,0)--(-0.58779,0.80902,0)--(-0.40674,0.91355,0)--(-0.20791,0.97815,0)--(1.5314e-15,1,0)--cycle;dot(g, rgb(0, 0, 0));
     . draw(((-0.99452,-1,-1)--(0.99452,-1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,1,-1)--(0.99452,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,-1,1)--(0.99452,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,1,1)--(0.99452,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,-1,-1)--(-0.99452,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((0.99452,-1,-1)--(0.99452,1,-1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,-1,1)--(-0.99452,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((0.99452,-1,1)--(0.99452,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,-1,-1)--(-0.99452,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((0.99452,-1,-1)--(0.99452,-1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((-0.99452,1,-1)--(-0.99452,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . draw(((0.99452,1,-1)--(0.99452,1,1)), rgb(0.4, 0.4, 0.4)+linewidth(1));
     . \end{asy}
    """

    options = Graphics.options.copy()
    options.update(
        {"BoxRatios": "Automatic", "Lighting": "Automatic", "ViewPoint": "{1.3,-2.4,2}"}
    )

    box_suffix = "3DBox"

    messages = {"invlight": "`1` is not a valid list of light sources."}

    rules = {
        "MakeBoxes[Graphics3D[content_, OptionsPattern[Graphics3D]], "
        "        OutputForm]": '"-Graphics3D-"'
    }

    summary_text = "a three-dimensional graphics image wrapper"


def total_extent_3d(extents):
    xmin = xmax = ymin = ymax = zmin = zmax = None
    for extent in extents:
        for x, y, z in extent:
            if xmin is None or x < xmin:
                xmin = x
            if xmax is None or x > xmax:
                xmax = x
            if ymin is None or y < ymin:
                ymin = y
            if ymax is None or y > ymax:
                ymax = y
            if zmin is None or z < zmin:
                zmin = z
            if zmax is None or z > zmax:
                zmax = z
    return xmin, xmax, ymin, ymax, zmin, zmax


class Graphics3DElements(_GraphicsElements):
    coords = Coords3D

    def __init__(self, content, evaluation, neg_y=False):
        super(Graphics3DElements, self).__init__(content, evaluation)
        self.neg_y = neg_y
        self.xmin = (
            self.ymin
        ) = (
            self.pixel_width
        ) = self.pixel_height = self.extent_width = self.extent_height = None
        self.view_width = None
        self.content = content

    def extent(self, completely_visible_only=False):
        return total_extent_3d([element.extent() for element in self.elements])

    def _apply_boxscaling(self, boxscale):
        for element in self.elements:
            element._apply_boxscaling(boxscale)

    def get_style_class(self):
        return Style3D


class Sphere(Builtin):
    """
    <dl>
    <dt>'Sphere[{$x$, $y$, $z$}]'
        <dd>is a sphere of radius 1 centered at the point {$x$, $y$, $z$}.
    <dt>'Sphere[{$x$, $y$, $z$}, $r$]'
        <dd>is a sphere of radius $r$ centered at the point {$x$, $y$, $z$}.
    <dt>'Sphere[{{$x1$, $y1$, $z1$}, {$x2$, $y2$, $z2$}, ... }, $r$]'
        <dd>is a collection spheres of radius $r$ centered at the points {$x1$, $y2$, $z2$}, {$x2$, $y2$, $z2$}, ...
    </dl>

    >> Graphics3D[Sphere[{0, 0, 0}, 1]]
     = -Graphics3D-

    >> Graphics3D[{Yellow, Sphere[{{-1, 0, 0}, {1, 0, 0}, {0, 0, Sqrt[3.]}}, 1]}]
     = -Graphics3D-
    """

    rules = {
        "Sphere[]": "Sphere[{0, 0, 0}, 1]",
        "Sphere[positions_]": "Sphere[positions, 1]",
    }


class Cuboid(Builtin):
    """
    Cuboid also known as interval, rectangle, square, cube, rectangular parallelepiped, tesseract, orthotope, and box.
    <dl>
      <dt>'Cuboid[$p_min$]'
      <dd>is a unit cube with its lower corner at point $p_min$.

      <dt>'Cuboid[{$p_min$, $p_max$}]'
      <dd>is a cuboid with lower corner $p_min$ and upper corner $p_max$.

      <dt>'Cuboid[{$p1_min$, $p1_max$, ...}]'
      <dd>is a collection of cuboids.

      <dt>'Cuboid[]' is equivalent to 'Cuboid[{0,0,0}]'.
    </dl>

    >> Graphics3D[Cuboid[{0, 0, 1}]]
     = -Graphics3D-

    >> Graphics3D[{Red, Cuboid[{{0, 0, 0}, {1, 1, 0.5}}], Blue, Cuboid[{{0.25, 0.25, 0.5}, {0.75, 0.75, 1}}]}]
     = -Graphics3D-

    ##
    """

    messages = {"oddn": "The number of points must be even."}

    rules = {
        "Cuboid[]": "Cuboid[{{0, 0, 0}, {1, 1, 1}}]",
    }

    summary_text = "unit cube"

    def apply_unit_cube(self, xmin, ymin, zmin, evaluation):
        "Cuboid[{xmin_, ymin_, zmin_}]"

        return Expression(
            "Cuboid",
            List(
                List(xmin, ymin, zmin),
                List(
                    Real(xmin.to_python() + 1),
                    Real(ymin.to_python() + 1),
                    Real(zmin.to_python() + 1),
                ),
            ),
        )

    def apply_check(self, positions, evaluation):
        "Cuboid[positions_List]"

        if len(positions.get_leaves()) % 2 == 1:
            # The number of points is odd, so abort.
            evaluation.error("Cuboid", "oddn", positions)

        return Expression("Cuboid", positions)


class Cylinder(Builtin):
    """
    <dl>
      <dt>'Cylinder[{{$x1$, $y1$, $z1$}, {$x2$, $y2$, $z2$}}]'
      <dd>represents a cylinder of radius 1.

      <dt>'Cylinder[{{$x1$, $y1$, $z1$}, {$x2$, $y2$, $z2$}}, $r$]'
      <dd>is a cylinder of radius $r$ starting at ($x1$, $y1$, $z1$) and ending at ($x2$, $y2$, $z2$).

      <dt>'Cylinder[{{$x1$, $y1$, $z1$}, {$x2$, $y2$, $z2$}, ... }, $r$]'
      <dd>is a collection cylinders of radius $r$.
    </dl>

    >> Graphics3D[Cylinder[{{0, 0, 0}, {1, 1, 1}}, 1]]
     = -Graphics3D-

    >> Graphics3D[{Yellow, Cylinder[{{-1, 0, 0}, {1, 0, 0}, {0, 0, Sqrt[3]}, {1, 1, Sqrt[3]}}, 1]}]
     = -Graphics3D-
    """

    messages = {"oddn": "The number of points must be even."}

    rules = {
        "Cylinder[]": "Cylinder[{{0, 0, 0}, {1, 1, 1}}, 1]",
        "Cylinder[positions_List]": "Cylinder[positions, 1]",
    }

    def apply_check(self, positions, radius, evaluation):
        "Cylinder[positions_List, radius_?RealNumberQ]"

        if len(positions.get_leaves()) % 2 == 1:
            # The number of points is odd, so abort.
            evaluation.error("Cylinder", "oddn", positions)

        return Expression("Cylinder", positions, radius)
