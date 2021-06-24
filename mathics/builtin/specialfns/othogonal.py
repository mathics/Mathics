"""
Orthogonal Polynomials
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.arithmetic import _MPMathFunction
from mathics.core.expression import Integer0


class ChebyshevT(_MPMathFunction):
    """
    <dl>
    <dt>'ChebyshevT[$n$, $x$]'
      <dd>returns the Chebyshev polynomial of the first kind T_$n$($x$).
    </dl>

    >> ChebyshevT[8, x]
     = 1 - 32 x ^ 2 + 160 x ^ 4 - 256 x ^ 6 + 128 x ^ 8

    >> ChebyshevT[1 - I, 0.5]
     = 0.800143 + 1.08198 I
    """

    nargs = 2
    sympy_name = "chebyshevt"
    mpmath_name = "chebyt"


class ChebyshevU(_MPMathFunction):
    """
    <dl>
    <dt>'ChebyshevU[$n$, $x$]'
      <dd>returns the Chebyshev polynomial of the second kind U_$n$($x$).
    </dl>

    >> ChebyshevU[8, x]
     = 1 - 40 x ^ 2 + 240 x ^ 4 - 448 x ^ 6 + 256 x ^ 8

    >> ChebyshevU[1 - I, 0.5]
     = 1.60029 + 0.721322 I
    """

    nargs = 2
    sympy_name = "chebyshevu"
    mpmath_name = "chebyu"


class GegenbauerC(_MPMathFunction):
    """
    <dl>
    <dt>'GegenbauerC[$n$, $m$, $x$]'
      <dd>returns the Gegenbauer polynomial C_$n$^($m$)($x$).
    </dl>

    >> GegenbauerC[6, 1, x]
     = -1 + 24 x ^ 2 - 80 x ^ 4 + 64 x ^ 6

    >> GegenbauerC[4 - I, 1 + 2 I, 0.7]
     = -3.2621 - 24.9739 I
    """

    # TODO: Two argument renormalized form GegenbauerC[n, x]

    nargs = 3
    sympy_name = "gegenbauer"
    mpmath_name = "gegenbauer"


class HermiteH(_MPMathFunction):
    """
    <dl>
    <dt>'HermiteH[$n$, $x$]'
      <dd>returns the Hermite polynomial H_$n$($x$).
    </dl>

    >> HermiteH[8, x]
     = 1680 - 13440 x ^ 2 + 13440 x ^ 4 - 3584 x ^ 6 + 256 x ^ 8

    >> HermiteH[3, 1 + I]
     = -28 + 4 I

    >> HermiteH[4.2, 2]
     = 77.5291
    """

    nargs = 2
    sympy_name = "hermite"
    mpmath_name = "hermite"


class JacobiP(_MPMathFunction):
    """
    <dl>
    <dt>'JacobiP[$n$, $a$, $b$, $x$]'
      <dd>returns the Jacobi polynomial P_$n$^($a$,$b$)($x$).
    </dl>

    >> JacobiP[1, a, b, z]
     = a / 2 - b / 2 + z (1 + a / 2 + b / 2)

    >> JacobiP[3.5 + I, 3, 2, 4 - I]
     = 1410.02 + 5797.3 I
    """

    nargs = 4
    sympy_name = "jacobi"
    mpmath_name = "jacobi"


class LaguerreL(_MPMathFunction):
    """
    <dl>
    <dt>'LaguerreL[$n$, $x$]'
      <dd>returns the Laguerre polynomial L_$n$($x$).
    <dt>'LaguerreL[$n$, $a$, $x$]'
      <dd>returns the generalised Laguerre polynomial L^$a$_$n$($x$).
    </dl>

    >> LaguerreL[8, x]
     = 1 - 8 x + 14 x ^ 2 - 28 x ^ 3 / 3 + 35 x ^ 4 / 12 - 7 x ^ 5 / 15 + 7 x ^ 6 / 180 - x ^ 7 / 630 + x ^ 8 / 40320

    >> LaguerreL[3/2, 1.7]
     = -0.947134

    >> LaguerreL[5, 2, x]
     = 21 - 35 x + 35 x ^ 2 / 2 - 7 x ^ 3 / 2 + 7 x ^ 4 / 24 - x ^ 5 / 120
    """

    rules = {
        "LaguerreL[n_, x_]": "LaguerreL[n, 0, x]",
    }

    nargs = 3
    sympy_name = "laguerre_poly"
    mpmath_name = "laguerre"

    def prepare_sympy(self, leaves):
        if len(leaves) == 3:
            return [leaves[0], leaves[2], leaves[1]]
        return leaves


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
        if leaves[1] == Integer0:
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
        if leaves[1] == Integer0:
            return leaves[:1] + leaves[2:]
        return leaves


class SphericalHarmonicY(_MPMathFunction):
    """
    <dl>
    <dt>'SphericalHarmonicY[$l$, $m$, $theta$, $phi$]'
      <dd>returns the spherical harmonic function Y_$l$^$m$(theta, phi).
    </dl>

    >> SphericalHarmonicY[3/4, 0.5, Pi/5, Pi/3]
     = 0.254247 + 0.14679 I

    ## Results depend on sympy version
    >> SphericalHarmonicY[3, 1, theta, phi]
     = ...

    #> SphericalHarmonicY[1,1,x,y]
     = -Sqrt[6] E ^ (I y) Sin[x] / (4 Sqrt[Pi])
    """

    nargs = 4
    sympy_name = "Ynm"
    mpmath_name = "spherharm"
    rules = {
        "Derivative[0,0,1,0][SphericalHarmonicY]": "(Cot[#3]*#2*SphericalHarmonicY[#1, #2, #3, #4] + (Sqrt[Gamma[1 + #1 - #2]]*Sqrt[Gamma[2 + #1 + #2]]*SphericalHarmonicY[#1, 1 + #2, #3, #4])/(E^(I*#4)*Sqrt[Gamma[#1 - #2]]*Sqrt[Gamma[1 + #1 + #2]]))&",
        "Derivative[0,0,0,1][SphericalHarmonicY]": "(I*#2*SphericalHarmonicY[#1, #2, #3, #4])&",
    }

    def prepare_mathics(self, sympy_expr):
        return sympy_expr.expand(func=True).simplify()


# TODO: Zernike polynomials not yet implemented in mpmath nor sympy
#
# class ZernikeR(_MPMathFunction):
#    """
#    <dl>
#    <dt>'ZernikeR[$n$, $m$,  $r$]'
#      <dd>returns the radial Zernike polynomial R_$n$^$m$($r$).
#    </dl>
#
#    >> ZernikeR[3, 1, r]
#     = -2 r + 3 r ^ 3
#
#    >> ZernikeR[5, 1, 1/2]
#     = 5 / 16
#
#    >> ZernikeR[3 - I, 4.5, 1.5 + I]
#     = 1.12642 - 1.21017 I
#    """
#
#    nargs = 3
#    sympy_name = ''
#    mpmath_name = ''
