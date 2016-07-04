#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Special functions
"""

from __future__ import unicode_literals
from __future__ import absolute_import

import mpmath

from mathics.builtin.base import Builtin
from mathics.builtin.arithmetic import _MPMathFunction
from mathics.core.expression import Integer
from mathics.core.numbers import mpmath2sympy
from mathics.core.convert import from_sympy
from mathics.builtin.numeric import get_precision


class Erf(_MPMathFunction):
    """
    <dl>
    <dt>'Erf[$z$]'
        <dd>returns the error function of $z$.
    </dl>

    >> Erf[1.0]
     = 0.842700792949714869
    >> Erf[0]
     = 0
    >> Plot[Erf[x], {x, -2, 2}]
     = -Graphics-
    """

    mpmath_name = 'erf'


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

    sympy_name = 'LambertW'  # function called LambertW in SymPy
    mpmath_name = 'lambertw'

    rules = {
        'ProductLog[0]': '0',
        'ProductLog[E]': '1',
        'ProductLog[z_] * E ^ ProductLog[z_]': 'z',
    }


class Zeta(_MPMathFunction):
    """
    <dl>
    <dt>'Zeta[$z$]'
      <dd>returns the Riemann zeta function of $z$.
    </dl>

    >> Zeta[2]
     = Pi ^ 2 / 6

    >> Zeta[-2.5 + I]
     = 0.0235936105863796486 + 0.00140779960583837704 I
    """

    sympy_name = 'zeta'
    mpmath_name = 'zeta'


class _Bessel(_MPMathFunction):

    attributes = ('Listable', 'NumericFunction', 'Protected', 'ReadProtected')

    nargs = 2

# Bessel Functions


class BesselJ(_Bessel):
    """
    <dl>
    <dt>'BesselJ[$n$, $z$]'
      <dd>returns the Bessel function of the first kind J_$n$($z$).
    </dl>

    >> BesselJ[0, 5.2]
     = -0.11029043979098654

    #> BesselJ[2.5, 1]
     = 0.0494968102284779423

    ## >> D[BesselJ[n, z], z]
    ##  = BesselJ[n - 1, z] / 2 - BesselJ[n + 1, z] / 2

    #> BesselJ[0., 0.]
     = 1.

    >> Plot[BesselJ[0, x], {x, 0, 10}]
     = -Graphics-
    """

    # TODO: Sympy Backend is not as powerful as Mathmeatica
    """
    >> BesselJ[1/2, x]
     = Sqrt[2 / Pi] Sin[x] / Sqrt[x]
    """

    attributes = ('Listable', 'NumericFunction', 'Protected')

    sympy_name = 'besselj'
    mpmath_name = 'besselj'


class BesselY(_Bessel):
    """
    <dl>
    <dt>'BesselY[$n$, $z$]'
      <dd>returns the Bessel function of the second kind Y_$n$($z$).
    </dl>

    >> BesselY[1.5, 4]
     = 0.367112032460934155

    ## Returns ComplexInfinity instead
    ## #> BesselY[0., 0.]
    ##  = -Infinity

    >> Plot[BesselY[0, x], {x, 0, 10}]
     = -Graphics-
    """

    # TODO: Special Values
    """
    >> BesselY[0, 0]
     = -Infinity
    """

    attributes = ('Listable', 'NumericFunction', 'Protected')

    sympy_name = 'bessely'
    mpmath_name = 'bessely'


class BesselI(_Bessel):
    """
    <dl>
    <dt>'BesselI[$n$, $z$]'
      <dd>returns the modified Bessel function of the first kind I_$n$($z$).
    </dl>

    >> BesselI[1.5, 4]
     = 8.17263323168659544

    >> Plot[BesselI[0, x], {x, 0, 5}]
     = -Graphics-
    """

    attributes = ('Listable', 'NumericFunction', 'Protected')

    sympy_name = 'besseli'
    mpmath_name = 'besseli'


class BesselK(_Bessel):
    """
    <dl>
    <dt>'BesselK[$n$, $z$]'
      <dd>returns the modified Bessel function of the second kind K_$n$($z$).
    </dl>

    >> BesselK[1.5, 4]
     = 0.0143470307207600668

    >> Plot[BesselK[0, x], {x, 0, 5}]
     = -Graphics-
    """

    attributes = ('Listable', 'NumericFunction', 'Protected')

    sympy_name = 'besselk'
    mpmath_name = 'besselk'

# TODO: Spherical Bessel Functions

# Hankel Functions


class HankelH1(_Bessel):
    """
    <dl>
    <dt>'HankelH1[$n$, $z$]'
      <dd>returns the Hankel function of the first kind H_$n$^1 ($z$).
    </dl>

    >> HankelH1[1.5, 4]
     = 0.185285948354268953 + 0.367112032460934155 I
    """

    sympy_name = 'hankel1'
    mpmath_name = 'hankel1'


class HankelH2(_Bessel):
    """
    <dl>
    <dt>'HankelH2[$n$, $z$]'
      <dd>returns the Hankel function of the second kind H_$n$^2 ($z$).
    </dl>

    >> HankelH2[1.5, 4]
     = 0.185285948354268953 - 0.367112032460934155 I
    """

    sympy_name = 'hankel2'
    mpmath_name = 'hankel2'

# Airy Functions


class AiryAi(_MPMathFunction):
    """
    <dl>
    <dt>'AiryAi[$x$]'
      <dd>returns the Airy function Ai($x$).
    </dl>

    >> AiryAi[0.5]
     = 0.23169360648083349

    >> AiryAi[0.5 + I]
     = 0.157118446499986172 - 0.241039813840210768 I

    >> Plot[AiryAi[x], {x, -10, 10}]
     = -Graphics-
    """

    sympy_name = ''
    mpmath_name = 'airyai'


class AiryBi(_MPMathFunction):
    """
    <dl>
    <dt>'AiryBi[$x$]'
      <dd>returns the Airy function Bi($x$).
    </dl>

    >> AiryBi[0.5]
     = 0.854277043103155493

    >> AiryBi[0.5 + I]
     = 0.688145273113482414 + 0.370815390737010831 I

    >> Plot[AiryBi[x], {x, -10, 2}]
     = -Graphics-
    """

    sympy_name = ''
    mpmath_name = 'airybi'

# Kelvin Functions


class KelvinBer(_Bessel):
    """
    <dl>
    <dt>'KelvinBer[$z$]'
      <dd>returns the Kelvin function ber($z$).
    <dt>'KelvinBer[$n$, $z$]'
      <dd>returns the Kelvin function ber_$n$($z$).
    </dl>

    >> KelvinBer[0.5]
     = 0.999023463990838256

    >> KelvinBer[1.5 + I]
     = 1.11620420872233787 - 0.117944469093970067 I

    >> KelvinBer[0.5, 0.25]
     = 0.148824330530639942

    >> Plot[KelvinBer[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        'KelvinBer[z_]': 'KelvinBer[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'ber'


class KelvinBei(_Bessel):
    """
    <dl>
    <dt>'KelvinBei[$z$]'
      <dd>returns the Kelvin function bei($z$).
    <dt>'KelvinBei[$n$, $z$]'
      <dd>returns the Kelvin function bei_$n$($z$).
    </dl>

    >> KelvinBei[0.5]
     = 0.0624932183821994586

    >> KelvinBei[1.5 + I]
     = 0.326323348699806294 + 0.75560557861089228 I

    >> KelvinBei[0.5, 0.25]
     = 0.370152900194021013

    >> Plot[KelvinBei[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        'KelvinBei[z_]': 'KelvinBei[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'bei'


class KelvinKer(_Bessel):
    """
    <dl>
    <dt>'KelvinKer[$z$]'
      <dd>returns the Kelvin function ker($z$).
    <dt>'KelvinKer[$n$, $z$]'
      <dd>returns the Kelvin function ker_$n$($z$).
    </dl>

    >> KelvinKer[0.5]
     = 0.855905872118634214

    >> KelvinKer[1.5 + I]
     = -0.167162242027385125 - 0.184403720314419905 I

    >> KelvinKer[0.5, 0.25]
     = 0.450022838747182502

    >> Plot[KelvinKer[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        'KelvinKer[z_]': 'KelvinKer[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'ker'


class KelvinKei(_Bessel):
    """
    <dl>
    <dt>'KelvinKei[$z$]'
      <dd>returns the Kelvin function kei($z$).
    <dt>'KelvinKei[$n$, $z$]'
      <dd>returns the Kelvin function kei_$n$($z$).
    </dl>

    >> KelvinKei[0.5]
     = -0.671581695094367603

    >> KelvinKei[1.5 + I]
     = -0.248993863536003923 + 0.303326291875385478 I

    >> KelvinKei[0.5, 0.25]
     = -2.05169683896315934

    >> Plot[KelvinKei[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        'KelvinKei[z_]': 'KelvinKei[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'kei'

# Struve and Related Functions


class StruveH(_Bessel):
    """
    <dl>
    <dt>'StruveH[$n$, $z$]'
      <dd>returns the Struve function H_$n$($z$).
    </dl>

    >> StruveH[1.5, 3.5]
     = 1.13192125271801312

    >> Plot[StruveH[0, x], {x, 0, 20}]
     = -Graphics-
    """

    sympy_name = ''
    mpmath_name = 'struveh'


class StruveL(_Bessel):
    """
    <dl>
    <dt>'StruveL[$n$, $z$]'
      <dd>returns the modified Struve function L_$n$($z$).
    </dl>

    >> StruveL[1.5, 3.5]
     = 4.41126360920433996

    >> Plot[StruveL[0, x], {x, 0, 5}]
     = -Graphics-
    """

    sympy_name = ''
    mpmath_name = 'struvel'


class AngerJ(_Bessel):
    """
    <dl>
    <dt>'AngerJ[$n$, $z$]'
      <dd>returns the Anger function J_$n$($z$).
    </dl>

    >> AngerJ[1.5, 3.5]
     = 0.294478574459563408

    >> Plot[AngerJ[1, x], {x, -10, 10}]
     = -Graphics-
    """

    # TODO: Associated Anger function AngerJ[v, u, z]

    sympy_name = ''
    mpmath_name = 'angerj'


class WeberE(_Bessel):
    """
    <dl>
    <dt>'WeberE[$n$, $z$]'
      <dd>returns the Weber function E_$n$($z$).
    </dl>

    >> WeberE[1.5, 3.5]
     = -0.397256259210030809

    >> Plot[WeberE[1, x], {x, -10, 10}]
     = -Graphics-
    """

    # TODO: Associated Weber function WeberE[v, u, z]

    sympy_name = ''
    mpmath_name = 'webere'

# Function Zeros


class BesselJZero(_Bessel):
    """
    <dl>
    <dt>'BesselJZero[$n$, $k$]'
      <dd>returns the $k$th zero of the Bessel function of the first kind J_$n$($z$).
    </dl>

    >> N[BesselJZero[0, 1]]
     = 2.40482555769577277
    """

    sympy_name = ''
    mpmath_name = 'besseljzero'


class BesselYZero(_Bessel):
    """
    <dl>
    <dt>'BesselYZero[$n$, $k$]'
      <dd>returns the $k$th zero of the Bessel function of the second kind Y_$n$($z$).
    </dl>

    >> N[BesselYZero[0, 1]]
     = 0.893576966279167522
    """

    sympy_name = ''
    mpmath_name = 'besselyzero'


class AiryAiZero(Builtin):
    """
    <dl>
    <dt>'AiryAiZero[$k$]'
      <dd>returns the $k$th zero of the Airy function Ai($z$).
    </dl>

    >> N[AiryAiZero[1]]
     = -2.33810741045976704

    #> AiryAiZero[1]
     = AiryAiZero[1]

    #> AiryAiZero[1.]
     = AiryAiZero[1.]

    #> AiryAi[AiryAiZero[1]]
     = 0
    """

    # TODO: 'AiryAiZero[$k$, $x0$]' - $k$th zero less than x0

    attributes = ('Listable', 'NHoldFirst',
                  'NumericFunction', 'Protected', 'ReadProtected')

    rules = {
        'AiryAi[AiryAiZero[k_]]': '0',
    }

    def apply_N(self, k, precision, evaluation):
        'N[AiryAiZero[k_Integer], precision_]'

        prec = get_precision(precision, evaluation)
        k_int = k.get_int_value()

        with mpmath.workprec(prec):
            result = mpmath2sympy(mpmath.airyaizero(k_int), prec)
        return from_sympy(result)


class AiryBiZero(Builtin):
    """
    <dl>
    <dt>'AiryBiZero[$k$]'
      <dd>returns the $k$th zero of the Airy function Bi($z$).
    </dl>

    >> N[AiryBiZero[1]]
     = -1.17371322270912792

    #> AiryBiZero[1]
     = AiryBiZero[1]

    #> AiryBiZero[1.]
     = AiryBiZero[1.]

    #> AiryBi[AiryBiZero[1]]
     = 0
    """

    # TODO: 'AiryBiZero[$k$, $x0$]' - $k$th zero less than x0

    attributes = ('Listable', 'NHoldFirst',
                  'NumericFunction', 'Protected', 'ReadProtected')

    rules = {
        'AiryBi[AiryBiZero[z_]]': '0',
    }

    def apply_N(self, k, precision, evaluation):
        'N[AiryBiZero[k_Integer], precision_]'

        prec = get_precision(precision, evaluation)
        k_int = k.get_int_value()

        with mpmath.workprec(prec):
            result = mpmath2sympy(mpmath.airybizero(k_int), prec)
        return from_sympy(result)

# Orthogonal Polynomials


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
     = 4.17761913892745532

    >> LegendreP[1.75, 1.4, 0.53]
     = -1.32619280980662145

    >> LegendreP[1.6, 3.1, 1.5]
     = -0.303998161489593441 - 1.91936885256334894 I

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
        'LegendreP[n_, x_]': 'LegendreP[n, 0, x]'
    }

    nargs = 3
    sympy_name = 'legendre'
    mpmath_name = 'legenp'

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
     = 0.0362109671796812979 - 6.56218879817530572 I

    >> LegendreQ[1.75, 1.4, 0.53]
     = 2.05498907857609114

    >> LegendreQ[1.6, 3.1, 1.5]
     = -1.71931290970694153 - 7.70273279782676974 I
    """

    # FIXME: Sympy is missing the Legendre function of the second kind so
    # symbolic manipulations are limited
    """
    >> LegendreQ[2, x]
     = -3 x / 2 - 3 x ^ 2 Log[1 - x] / 4 + 3 x ^ 2 Log[1 + x] / 4 - Log[1 + x] / 4 + Log[1 - x] / 4
    """

    rules = {
        'LegendreQ[n_, x_]': 'LegendreQ[n, 0, x]'
    }

    nargs = 3
    sympy_name = ''
    mpmath_name = 'legenq'

    def prepare_sympy(self, leaves):
        if leaves[1] == Integer(0):
            return leaves[:1] + leaves[2:]
        return leaves


class JacobiP(_MPMathFunction):
    """
    <dl>
    <dt>'JacobiP[$n$, $a$, $b$, $x$]'
      <dd>returns the Jacobi polynomial P_$n$^($a$,$b$)($x$).
    </dl>

    >> JacobiP[1, a, b, z]
     = a / 2 - b / 2 + z (1 + a / 2 + b / 2)

    >> JacobiP[3.5 + I, 3, 2, 4 - I]
     = 1410.02011674512937 + 5797.29855312717469 I
    """

    nargs = 4
    sympy_name = 'jacobi'
    mpmath_name = 'jacobi'


class SphericalHarmonicY(_MPMathFunction):
    """
    <dl>
    <dt>'SphericalHarmonicY[$l$, $m$, $theta$, $phi$]'
      <dd>returns the spherical harmonic function Y_$l$^$m$(theta, phi).
    </dl>

    >> SphericalHarmonicY[3/4, 0.5, Pi/5, Pi/3]
     = 0.254247340352667373 + 0.146789770393358909 I

    ## Results depend on sympy version
    >> SphericalHarmonicY[3, 1, theta, phi]
     = ...

    #> SphericalHarmonicY[1,1,x,y]
     = -Sqrt[6] E ^ (I y) Sin[x] / (4 Sqrt[Pi])
    """

    nargs = 4
    sympy_name = 'Ynm'
    mpmath_name = 'spherharm'

    def prepare_mathics(self, sympy_expr):
        return sympy_expr.expand(func=True).simplify()


class GegenbauerC(_MPMathFunction):
    """
    <dl>
    <dt>'GegenbauerC[$n$, $m$, $x$]'
      <dd>returns the Gegenbauer polynomial C_$n$^($m$)($x$).
    </dl>

    >> GegenbauerC[6, 1, x]
     = -1 + 24 x ^ 2 - 80 x ^ 4 + 64 x ^ 6

    >> GegenbauerC[4 - I, 1 + 2 I, 0.7]
     = -3.26209595216525854 - 24.9739397455269944 I
    """

    # TODO: Two argument renormalized form GegenbauerC[n, x]

    nargs = 3
    sympy_name = 'gegenbauer'
    mpmath_name = 'gegenbauer'


class ChebyshevT(_MPMathFunction):
    """
    <dl>
    <dt>'ChebyshevT[$n$, $x$]'
      <dd>returns the Chebyshev polynomial of the first kind T_$n$($x$).
    </dl>

    >> ChebyshevT[8, x]
     = 1 - 32 x ^ 2 + 160 x ^ 4 - 256 x ^ 6 + 128 x ^ 8

    >> ChebyshevT[1 - I, 0.5]
     = 0.800143428851193116 + 1.08198360440499884 I
    """

    nargs = 2
    sympy_name = 'chebyshevt'
    mpmath_name = 'chebyt'


class ChebyshevU(_MPMathFunction):
    """
    <dl>
    <dt>'ChebyshevU[$n$, $x$]'
      <dd>returns the Chebyshev polynomial of the second kind U_$n$($x$).
    </dl>

    >> ChebyshevU[8, x]
     = 1 - 40 x ^ 2 + 240 x ^ 4 - 448 x ^ 6 + 256 x ^ 8

    >> ChebyshevU[1 - I, 0.5]
     = 1.60028685770238623 + 0.721322402936665892 I
    """

    nargs = 2
    sympy_name = 'chebyshevu'
    mpmath_name = 'chebyu'


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
     = 77.5290837369752225
    """

    nargs = 2
    sympy_name = 'hermite'
    mpmath_name = 'hermite'


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
     = -0.94713399725341823

    >> LaguerreL[5, 2, x]
     = 21 - 35 x + 35 x ^ 2 / 2 - 7 x ^ 3 / 2 + 7 x ^ 4 / 24 - x ^ 5 / 120
    """

    rules = {
        'LaguerreL[n_, x_]': 'LaguerreL[n, 0, x]',
    }

    nargs = 3
    sympy_name = 'laguerre_poly'
    mpmath_name = 'laguerre'

    def prepare_sympy(self, leaves):
        if len(leaves) == 3:
            return [leaves[0], leaves[2], leaves[1]]
        return leaves

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
#     = 1.12642179606815007 - 1.21017262353631061 I
#    """
#
#    nargs = 3
#    sympy_name = ''
#    mpmath_name = ''


class ExpIntegralEi(_MPMathFunction):
    '''
    <dl>
    <dt>'ExpIntegralEi[$z$]'
      <dd>returns the exponential integral function $Ei(z)$.
    </dl>

    >> ExpIntegralEi[2.0]
     = 4.95423435600189016
    '''

    sympy_name = 'Ei'
    mpmath_name = 'ei'


class ExpIntegralE(_MPMathFunction):
    '''
    <dl>
    <dt>'ExpIntegralE[$n$, $z$]'
      <dd>returns the exponential integral function $E_n(z)$.
    </dl>

    >> ExpIntegralE[2.0, 2.0]
     = 0.0375342618204904528
    '''

    nargs = 2
    sympy_name = 'expint'
    mpmath_name = 'expint'
