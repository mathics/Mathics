# -*- coding: utf-8 -*-
"""
Splines

Splines are used both in graphics and computations.
"""

from mathics.builtin.base import (
    Builtin,
)
from mathics.version import __version__  # noqa used in loading to check consistency.


# For a more generic implementation in Python using scipy,
# sympy and numpy, see:
#  https://github.com/Tarheel-Formal-Methods/kaa
class BernsteinBasis(Builtin):
    """
    <dl>
      <dt>'BernsteinBasis[$d$,$n$,$x$]'
      <dd>returns the $n$th Bernstein basis of degree $d$ at $x$.
    </dl>

    A Bernstein polynomial is a polynomial that is a linear combination of Bernstein basis polynomials.

    With the advent of computer graphics, Bernstein polynomials, restricted to the interval [0, 1], became important in the form of Bézier curves.

    'BernsteinBasis[d,n,x]' equals 'Binomial[d, n] x^n (1-x)^(d-n)' in the interval [0, 1] and zero elsewhere.

    >> BernsteinBasis[4, 3, 0.5]
     = 0.25
    """

    attributes = ("Listable", "NumericFunction", "Protected")
    rules = {
        "BernsteinBasis[d_, n_, x_]": "Piecewise[{{Binomial[d, n] * x ^ n * (1 - x) ^ (d - n), 0 < x < 1}}, 0]"
    }


class BezierFunction(Builtin):
    """
    <dl>
      <dt>'BezierFunction[{$pt_1$, $pt_2$, ...}]'
      <dd>returns a Bézier function for the curve defined by points $pt_i$.
      The embedding dimension for the curve represented by 'BezierFunction[{$pt_1$,$pt_2$,...}]' is given by the length of the lists $pt_i$.
    </dl>

    >> f = BezierFunction[{{0, 0}, {1, 1}, {2, 0}, {3, 2}}];
     =

    >> f[.5]
     = {1.5, 0.625}
    #> Clear[f];
     =

    ## Graphics[{Red, Point[pts], Green, Line[pts]}, Axes -> True]

    Plotting the Bézier Function accoss a Bézier curve:
    >> Module[{p={{0, 0},{1, 1},{2, -1},{4, 0}}}, Graphics[{BezierCurve[p], Red, Point[Table[BezierFunction[p][x], {x, 0, 1, 0.1}]]}]]
     = -Graphics-
    """

    rules = {
        "BezierFunction[p_]": "Function[x, Total[p * BernsteinBasis[Length[p] - 1, Range[0, Length[p] - 1], x]]]"
    }


class BezierCurve(Builtin):
    """
    <dl>
    <dt>'BezierCurve[{$pt_1$, $pt_2$ ...}]'
        <dd>represents a Bézier curve with control points $p_i$.
    </dl>

    Option:
    <ul>
      <li>'SplineDegree->$d$' specifies that the underlying polynomial basis should have maximal degree d.
    </ul>


    Set up some points...
    >> pts = {{0, 0}, {1, 1}, {2, -1}, {3, 0}, {5, 2}, {6, -1}, {7, 3}};
     =

     A composite Bézier curve and its control points:
    >> Graphics[{BezierCurve[pts], Green, Line[pts], Red, Point[pts]}]
     = -Graphics-

    #> Clear[pts];
     =
    """

    options = {"SplineDegree": "3"}
