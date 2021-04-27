"""
Legendre and Related Functions
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.arithmetic import _MPMathFunction
from mathics.core.expression import Integer

class LegendreP(_MPMathFunction):
    """
    <dl>
    <dt>'LegendreP[$n$, $x$]'
      <dd>returns the Legendre polynomial P_$n$($x$).
    <dt>'LegendreP[$n$, $m$, $x$]'
      <dd>returns the associated Legendre polynomial P^$m$_$n$($x$).
    </dl>

    >> LegendreP[4, x]
     = 3 / 8 - 15 x ^ 2 / 4 + 35 x ^ 4 / 8

    >> LegendreP[5/2, 1.5]
     = 4.17762

    >> LegendreP[1.75, 1.4, 0.53]
     = -1.32619

    >> LegendreP[1.6, 3.1, 1.5]
     = -0.303998 - 1.91937 I

    'LegendreP' can be used to draw generalized Lissajous figures:
    >> ParametricPlot[ {LegendreP[7, x], LegendreP[5, x]}, {x, -1, 1}]
     = -Graphics-
    """

    # FIXME: Sympy can't handle associated polynomials
    """
    >> LegendreP[2, 1, x]
     = -3 x Sqrt[1 - x^2]
    """

    rules = {
        "LegendreP[n_, x_]": "LegendreP[n, 0, x]",
        "Derivative[0,1][LegendreP]": "(((-1 - #1)*x*LegendreP[#1, #2] + (1 + #1)*LegendreP[1 + #1, #2])/(-1 + #2^2))&",
        "Derivative[0,0,1][LegendreP]": "((LegendreP[1 + #1, #2, #3]*(1 + #1 - #2) + LegendreP[#1, #2, #3]*(-1 - #1)*#3)/(-1 + #3^2))&",
    }

    nargs = 3
    sympy_name = "legendre"
    mpmath_name = "legenp"

    def prepare_sympy(self, leaves):
        if leaves[1] == Integer(0):
            return leaves[:1] + leaves[2:]
        return leaves


class LegendreQ(_MPMathFunction):
    """
    <dl>
    <dt>'LegendreQ[$n$, $x$]'
      <dd>returns the Legendre function of the second kind Q_$n$($x$).
    <dt>'LegendreQ[$n$, $m$, $x$]'
      <dd>returns the associated Legendre function of the second Q^$m$_$n$($x$).
    </dl>

    >> LegendreQ[5/2, 1.5]
     = 0.036211 - 6.56219 I

    >> LegendreQ[1.75, 1.4, 0.53]
     = 2.05499

    >> LegendreQ[1.6, 3.1, 1.5]
     = -1.71931 - 7.70273 I
    """

    # FIXME: Sympy is missing the Legendre function of the second kind so
    # symbolic manipulations are limited
    """
    >> LegendreQ[2, x]
     = -3 x / 2 - 3 x ^ 2 Log[1 - x] / 4 + 3 x ^ 2 Log[1 + x] / 4 - Log[1 + x] / 4 + Log[1 - x] / 4
    """

    rules = {
        "LegendreQ[n_, x_]": "LegendreQ[n, 0, x]",
        "Derivative[0,1][LegendreQ]": "((LegendreQ[1 + #1, #2]*(1 + #1) + LegendreQ[#1, #2]*(-1 - #1)*#2)/(-1 + #2^2))&",
        "Derivative[0,0,1][LegendreQ]": "((LegendreQ[1 + #1, #2, #3]*(1 + #1 - #2) + LegendreQ[#1, #2, #3]*(-1 - #1)*#3)/(-1 + #3^2))&",
    }

    nargs = 3
    sympy_name = ""
    mpmath_name = "legenq"

    def prepare_sympy(self, leaves):
        if leaves[1] == Integer(0):
            return leaves[:1] + leaves[2:]
        return leaves
