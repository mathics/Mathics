# -*- coding: utf-8 -*-

"""
Special Functions
"""


import mpmath

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin
from mathics.builtin.arithmetic import _MPMathFunction, _MPMathMultiFunction
from mathics.core.expression import Integer, from_mpmath
from mathics.core.numbers import machine_precision, get_precision, PrecisionValueError
from mathics.core.numbers import prec as _prec


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

class ExpIntegralE(_MPMathFunction):
    """
    <dl>
    <dt>'ExpIntegralE[$n$, $z$]'
      <dd>returns the exponential integral function $E_n(z)$.
    </dl>

    >> ExpIntegralE[2.0, 2.0]
     = 0.0375343
    """

    nargs = 2
    sympy_name = "expint"
    mpmath_name = "expint"


class ExpIntegralEi(_MPMathFunction):
    """
    <dl>
    <dt>'ExpIntegralEi[$z$]'
      <dd>returns the exponential integral function $Ei(z)$.
    </dl>

    >> ExpIntegralEi[2.0]
     = 4.95423
    """

    sympy_name = "Ei"
    mpmath_name = "ei"


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


class LerchPhi(_MPMathFunction):
    """
    <dl>
    <dt>'LerchPhi[z,s,a]'
        <dd>gives the Lerch transcendent Φ(z,s,a).
    </dl>

    >> LerchPhi[2, 3, -1.5]
     = 19.3893 - 2.1346 I

    >> LerchPhi[1, 2, 1/4]
     = 17.1973
    """

    # attributes = ("Listable", "NumericFunction") # inherited

    mpmath_name = "lerchphi"
    sympy_name = "lerchphi"

    def apply(self, z, s, a, evaluation):
        "%(name)s[z_, s_, a_]"

        py_z = z.to_python()
        py_s = s.to_python()
        py_a = a.to_python()
        try:
            return from_mpmath(mpmath.lerchphi(py_z, py_s, py_a))
        except:
            pass
            # return sympy.expand_func(sympy.lerchphi(py_z, py_s, py_a))


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

class ProductLog(_MPMathFunction):
    """
    <dl>
    <dt>'ProductLog[$z$]'
        <dd>returns the value of the Lambert W function at $z$.
    </dl>

    The defining equation:
    >> z == ProductLog[z] * E ^ ProductLog[z]
     = True

    Some special values:
    >> ProductLog[0]
     = 0
    >> ProductLog[E]
     = 1

    The graph of 'ProductLog':
    >> Plot[ProductLog[x], {x, -1/E, E}]
     = -Graphics-
    """

    sympy_name = "LambertW"  # function called LambertW in SymPy
    mpmath_name = "lambertw"

    rules = {
        "ProductLog[0]": "0",
        "ProductLog[E]": "1",
        "ProductLog[z_] * E ^ ProductLog[z_]": "z",
        "Derivative[1][ProductLog]": "ProductLog[#] / (# (ProductLog[#] + 1))&",
    }


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

class Zeta(_MPMathFunction):
    """
    <dl>
    <dt>'Zeta[$z$]'
      <dd>returns the Riemann zeta function of $z$.
    </dl>

    >> Zeta[2]
     = Pi ^ 2 / 6

    >> Zeta[-2.5 + I]
     = 0.0235936 + 0.0014078 I
    """

    sympy_name = "zeta"
    mpmath_name = "zeta"
