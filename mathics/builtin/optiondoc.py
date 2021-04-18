# -*- coding: utf-8 -*-

"""
Drawing Options and Option Values

The various common Plot and Graphics options, along with the meaning of specific option values are described here.

"""

# Until we have a better documentation system in place, we define classes for
# options. They are Builtins, even though they largely aren't.
#
# Our documentation system extracts, indexes, and provides doctests for
# builtins.


from mathics.builtin.base import Builtin

from mathics.version import __version__  # noqa used in loading to check consistency.


class Axes(Builtin):
    """
    <dl>
      <dt>'Axes'
      <dd>is an option for graphics functions that specifies whether axes should be drawn.
    </dl>

    >> Graphics[Circle[], Axes -> True]
     = -Graphics-
    """


class Axis(Builtin):
    """
    <dl>
      <dt>'Axis'
      <dd>is a possible value for the 'Filling' option.
    </dl>

    >> ListLinePlot[Table[Sin[x], {x, -5, 5, 0.5}], Filling->Axis]
     = -Graphics-
    """


class Bottom(Builtin):
    """
    <dl>
      <dt>'Bottom'
      <dd>is a possible value for the 'Filling' option.
    </dl>

    >> ListLinePlot[Table[Sin[x], {x, -5, 5, 0.5}], Filling->Bottom]
     = -Graphics-
    """


class Filling(Builtin):
    """
    <dl>
      <dt>'Filling Top |Bottom|Axis'
      <dd>is a an option to Plot to specify what filling to add under point, curves, and surfaces
    </dl>

    >> ListLinePlot[Table[Sin[x], {x, -5, 5, 0.5}], Filling->Axis]
     = -Graphics-
    """


class Full(Builtin):
    """
    <dl>
      <dt>'Full'
      <dd>is a possible value for the 'Mesh' and 'PlotRange' options.
    </dl>
    """


class ImageSize(Builtin):
    """
    <dl>
      <dt>'ImageSize'
      <dd>is an option that specifies the overall size of an image to display.
    </dl>

    Specifications for both width and height can be any of the following:
    <dl>
      <dt>Automatic
      <dd>determined by location or other dimension (default)
      <dt>Tiny, Small, Medium, Large
      <dd>pre defined absolute sizes
    </dl>


    >> Plot[Sin[x], {x, 0, 10}, ImageSize -> Small]
     = -Graphics-
    """


class Joined(Builtin):
    """
    <dl>
      <dt>'Joined $boolean$'
      <dd>is an option for 'Plot' that gives whether to join points to make lines.
    </dl>

    >> ListPlot[Table[n ^ 2, {n, 10}], Joined->True]
     = -Graphics-
    """


class MaxRecursion(Builtin):
    """
    <dl>
      <dt>'MaxRecursion'
      <dd>is an option for functions like NIntegrate and Plot that specifies how many recursive subdivisions can be made.
    </dl>

    >> NIntegrate[Exp[-10^8 x^2], {x, -1, 1}, MaxRecursion -> 10]
     =  1.97519*^-207
    """


class Mesh(Builtin):
    """
    <dl>
       <dt>'Mesh'
      <dd>is an option for 'Plot' that specifies the mesh to be drawn. The default is 'Mesh->None'.
     </dl>

    >> Plot[Sin[Cos[x^2]],{x,-4,4},Mesh->All]
     = -Graphics-

    >> Plot[Sin[x], {x,0,4 Pi}, Mesh->Full]
     = -Graphics-

    >> DensityPlot[Sin[x y], {x, -2, 2}, {y, -2, 2}, Mesh->Full]
     = -Graphics-

    >> Plot3D[Sin[x y], {x, -2, 2}, {y, -2, 2}, Mesh->Full]
     = -Graphics3D-
    """

    messages = {
        "ilevels": "`1` is not a valid mesh specification.",
    }


class PlotPoints(Builtin):
    """
    <dl>
      <dt>'PlotPoints $n$'
      <dd>A number specifies how many initial sample points to use.
     </dl>

    >> Plot[Sin[Cos[x^2]],{x,-4,4}, PlotPoints->22]
     = -Graphics-
    """


class PlotRange(Builtin):
    """
    <dl>
      <dt>'PlotRange'
      <dd>is an option for 'Plot' that gives the range of coordinates to include in a plot.
    </dl>
    <ul>
      <li>All all points are included.</li>
      <li>Automatic - outlying points are dropped.</li>
      <li>$max$ - explicit limit for each function.</li>
      <li>{$min$, $max$} - explicit limits for $y$ (2D), $z$ (3D), or array value.s</li>
      <li>{{$x$_$min$, $x$_$max$}, {{$y_min}, {$y_max}} - explit limits for $x$ and $y$.
    </ul>

    >> Plot[Sin[Cos[x^2]],{x,-4,4}, PlotRange -> All]
     = -Graphics-

    >> Graphics[Disk[], PlotRange -> {{-.5, .5}, {0, 1.5}}]
     = -Graphics-
    """


class TicksStyle(Builtin):
    """
    <dl>
      <dt>'TicksStyle'
      <dd>is an option for graphics functions which specifies how ticks should be rendered.
    </dl>

    <ul>
    <li>TicksStyle gives styles for both tick marks and tick labels.</li>
    <li>TicksStyle can be used in both two  and three-dimensional graphics.</li>
    <li>TicksStyle->$list$ specifies the colors of each of the axes.
    </ul>

    >> Graphics[Circle[], Axes-> True, TicksStyle -> {Blue, Red}]
     = -Graphics-
    """


class Top(Builtin):
    """
    <dl>
    <dt>'Top'
        <dd>is a possible value for the 'Filling' option.
    </dl>

    >> ListLinePlot[Table[Sin[x], {x, -5, 5, 0.5}], Filling->Axis|Top|Bottom]
     = -Graphics-
    """
