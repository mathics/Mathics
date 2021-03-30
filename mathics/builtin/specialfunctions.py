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


class Erf(_MPMathMultiFunction):
    """
    <dl>
    <dt>'Erf[$z$]'
        <dd>returns the error function of $z$.
    <dt>'Erf[$z0$, $z1$]'
        <dd>returns the result of 'Erf[$z1$] - Erf[$z0$]'.
    </dl>

    'Erf[$x$]' is an odd function:
    >> Erf[-x]
     = -Erf[x]

    >> Erf[1.0]
     = 0.842701
    >> Erf[0]
     = 0
    >> {Erf[0, x], Erf[x, 0]}
     = {Erf[x], -Erf[x]}
    >> Plot[Erf[x], {x, -2, 2}]
     = -Graphics-
    """

    mpmath_names = {
        1: "erf",
    }
    sympy_names = {
        1: "erf",
        2: "erf2",
    }

    rules = {
        "Derivative[1][Erf]": "2 Exp[-#^2] / Sqrt[Pi] &",
    }


class InverseErf(_MPMathFunction):
    """
    <dl>
    <dt>'InverseErf[$z$]'
        <dd>returns the inverse error function of $z$.
    </dl>

    >> InverseErf /@ {-1, 0, 1}
     = {-Infinity, 0, Infinity}
    >> Plot[InverseErf[x], {x, -1, 1}]
     = -Graphics-

    'InverseErf[$z$]' only returns numeric values for '-1 <= $z$ <= 1':
    >> InverseErf /@ {0.9, 1.0, 1.1}
     = {1.16309, Infinity, InverseErf[1.1]}
    """

    sympy_name = "erfinv"
    mpmath_name = "erfinv"

    rules = {
        "Derivative[1][InverseErf]": "Sqrt[Pi] Exp[InverseErf[#]^2] / 2 &",
    }

    def apply(self, z, evaluation):
        "%(name)s[z__]"

        try:
            return super(InverseErf, self).apply(z, evaluation)
        except ValueError as exc:
            if str(exc) == "erfinv(x) is defined only for -1 <= x <= 1":
                return
            else:
                raise


class Erfc(_MPMathFunction):
    """
    <dl>
    <dt>'Erfc[$z$]'
        <dd>returns the complementary error function of $z$.
    </dl>

    >> Erfc[-x] / 2
     = (2 - Erfc[x]) / 2
    >> Erfc[1.0]
     = 0.157299
    >> Erfc[0]
     = 1
    >> Plot[Erfc[x], {x, -2, 2}]
     = -Graphics-
    """

    mpmath_name = "erfc"

    rules = {
        "Derivative[1][Erfc]": "-2 Exp[-#^2] / Sqrt[Pi] &",
    }


class InverseErfc(_MPMathFunction):
    """
    <dl>
    <dt>'InverseErfc[$z$]'
        <dd>returns the inverse complementary error function of $z$.
    </dl>

    >> InverseErfc /@ {0, 1, 2}
     = {Infinity, 0, -Infinity}
    """

    sympy_name = "erfcinv"

    rules = {
        "Derivative[1][InverseErfc]": "-Sqrt[Pi] Exp[InverseErfc[#]^2] / 2 &",
    }


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

    sympy_name = "lerchphi"
    mpmath_name = "lerchphi"

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


class _Bessel(_MPMathFunction):

    attributes = ("Listable", "NumericFunction", "Protected", "ReadProtected")

    nargs = 2


# Bessel Functions


class BesselJ(_Bessel):
    """
    <dl>
    <dt>'BesselJ[$n$, $z$]'
      <dd>returns the Bessel function of the first kind J_$n$($z$).
    </dl>

    >> BesselJ[0, 5.2]
     = -0.11029

    #> BesselJ[2.5, 1]
     = 0.0494968

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

    attributes = ("Listable", "NumericFunction", "Protected")

    sympy_name = "besselj"
    mpmath_name = "besselj"


class BesselY(_Bessel):
    """
    <dl>
    <dt>'BesselY[$n$, $z$]'
      <dd>returns the Bessel function of the second kind Y_$n$($z$).
    </dl>

    >> BesselY[1.5, 4]
     = 0.367112

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

    attributes = ("Listable", "NumericFunction", "Protected")

    sympy_name = "bessely"
    mpmath_name = "bessely"


class BesselI(_Bessel):
    """
    <dl>
    <dt>'BesselI[$n$, $z$]'
      <dd>returns the modified Bessel function of the first kind I_$n$($z$).
    </dl>

    >> BesselI[1.5, 4]
     = 8.17263

    >> Plot[BesselI[0, x], {x, 0, 5}]
     = -Graphics-
    """

    attributes = ("Listable", "NumericFunction", "Protected")

    sympy_name = "besseli"
    mpmath_name = "besseli"


class BesselK(_Bessel):
    """
    <dl>
    <dt>'BesselK[$n$, $z$]'
      <dd>returns the modified Bessel function of the second kind K_$n$($z$).
    </dl>

    >> BesselK[1.5, 4]
     = 0.014347

    >> Plot[BesselK[0, x], {x, 0, 5}]
     = -Graphics-
    """

    attributes = ("Listable", "NumericFunction", "Protected")

    sympy_name = "besselk"
    mpmath_name = "besselk"


# TODO: Spherical Bessel Functions

# Hankel Functions


class HankelH1(_Bessel):
    """
    <dl>
    <dt>'HankelH1[$n$, $z$]'
      <dd>returns the Hankel function of the first kind H_$n$^1 ($z$).
    </dl>

    >> HankelH1[1.5, 4]
     = 0.185286 + 0.367112 I
    """

    sympy_name = "hankel1"
    mpmath_name = "hankel1"


class HankelH2(_Bessel):
    """
    <dl>
    <dt>'HankelH2[$n$, $z$]'
      <dd>returns the Hankel function of the second kind H_$n$^2 ($z$).
    </dl>

    >> HankelH2[1.5, 4]
     = 0.185286 - 0.367112 I
    """

    sympy_name = "hankel2"
    mpmath_name = "hankel2"


# Airy Functions


class AiryAi(_MPMathFunction):
    """
    <dl>
    <dt>'AiryAi[$x$]'
      <dd>returns the Airy function Ai($x$).
    </dl>

    Exact values:
    >> AiryAi[0]
     = 3 ^ (1 / 3) / (3 Gamma[2 / 3])

    'AiryAi' can be evaluated numerically:
    >> AiryAi[0.5]
     = 0.231694
    >> AiryAi[0.5 + I]
     = 0.157118 - 0.24104 I

    >> Plot[AiryAi[x], {x, -10, 10}]
     = -Graphics-
    """

    sympy_name = "airyai"
    mpmath_name = "airyai"

    rules = {
        "Derivative[1][AiryAi]": "AiryAiPrime",
    }


class AiryAiPrime(_MPMathFunction):
    """
    <dl>
    <dt>'AiryAiPrime[$x$]'
        <dd>returns the derivative of the Airy function 'AiryAi[$x$]'.
    </dl>

    Exact values:
    >> AiryAiPrime[0]
     = -3 ^ (2 / 3) / (3 Gamma[1 / 3])

    Numeric evaluation:
    >> AiryAiPrime[0.5]
     = -0.224911
    """

    sympy_name = "airyaiprime"
    mpmath_name = ""

    def get_mpmath_function(self, args):
        return lambda x: mpmath.airyai(x, derivative=1)


class AiryBi(_MPMathFunction):
    """
    <dl>
    <dt>'AiryBi[$x$]'
      <dd>returns the Airy function of the second kind Bi($x$).
    </dl>

    Exact values:
    >> AiryBi[0]
     = 3 ^ (5 / 6) / (3 Gamma[2 / 3])

    Numeric evaluation:
    >> AiryBi[0.5]
     = 0.854277
    >> AiryBi[0.5 + I]
     = 0.688145 + 0.370815 I

    >> Plot[AiryBi[x], {x, -10, 2}]
     = -Graphics-
    """

    sympy_name = "airybi"
    mpmath_name = "airybi"

    rules = {
        "Derivative[1][AiryBi]": "AiryBiPrime",
    }


class AiryBiPrime(_MPMathFunction):
    """
    <dl>
    <dt>'AiryBiPrime[$x$]'
        <dd>returns the derivative of the Airy function of the second
        kind 'AiryBi[$x$]'.
    </dl>

    Exact values:
    >> AiryBiPrime[0]
     = 3 ^ (1 / 6) / Gamma[1 / 3]

    Numeric evaluation:
    >> AiryBiPrime[0.5]
     = 0.544573
    """

    sympy_name = "airybiprime"
    mpmath_name = ""

    def get_mpmath_function(self, args):
        return lambda x: mpmath.airybi(x, derivative=1)


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
     = 0.999023

    >> KelvinBer[1.5 + I]
     = 1.1162 - 0.117944 I

    >> KelvinBer[0.5, 0.25]
     = 0.148824

    >> Plot[KelvinBer[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        "KelvinBer[z_]": "KelvinBer[0, z]",
    }

    sympy_name = ""
    mpmath_name = "ber"


class KelvinBei(_Bessel):
    """
    <dl>
    <dt>'KelvinBei[$z$]'
      <dd>returns the Kelvin function bei($z$).
    <dt>'KelvinBei[$n$, $z$]'
      <dd>returns the Kelvin function bei_$n$($z$).
    </dl>

    >> KelvinBei[0.5]
     = 0.0624932

    >> KelvinBei[1.5 + I]
     = 0.326323 + 0.755606 I

    >> KelvinBei[0.5, 0.25]
     = 0.370153

    >> Plot[KelvinBei[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        "KelvinBei[z_]": "KelvinBei[0, z]",
    }

    sympy_name = ""
    mpmath_name = "bei"


class KelvinKer(_Bessel):
    """
    <dl>
    <dt>'KelvinKer[$z$]'
      <dd>returns the Kelvin function ker($z$).
    <dt>'KelvinKer[$n$, $z$]'
      <dd>returns the Kelvin function ker_$n$($z$).
    </dl>

    >> KelvinKer[0.5]
     = 0.855906

    >> KelvinKer[1.5 + I]
     = -0.167162 - 0.184404 I

    >> KelvinKer[0.5, 0.25]
     = 0.450023

    >> Plot[KelvinKer[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        "KelvinKer[z_]": "KelvinKer[0, z]",
    }

    sympy_name = ""
    mpmath_name = "ker"


class KelvinKei(_Bessel):
    """
    <dl>
    <dt>'KelvinKei[$z$]'
      <dd>returns the Kelvin function kei($z$).
    <dt>'KelvinKei[$n$, $z$]'
      <dd>returns the Kelvin function kei_$n$($z$).
    </dl>

    >> KelvinKei[0.5]
     = -0.671582

    >> KelvinKei[1.5 + I]
     = -0.248994 + 0.303326 I

    >> KelvinKei[0.5, 0.25]
     = -2.0517

    >> Plot[KelvinKei[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        "KelvinKei[z_]": "KelvinKei[0, z]",
    }

    sympy_name = ""
    mpmath_name = "kei"


# Struve and Related Functions


class StruveH(_Bessel):
    """
    <dl>
    <dt>'StruveH[$n$, $z$]'
      <dd>returns the Struve function H_$n$($z$).
    </dl>

    >> StruveH[1.5, 3.5]
     = 1.13192

    >> Plot[StruveH[0, x], {x, 0, 20}]
     = -Graphics-
    """

    sympy_name = ""
    mpmath_name = "struveh"


class StruveL(_Bessel):
    """
    <dl>
    <dt>'StruveL[$n$, $z$]'
      <dd>returns the modified Struve function L_$n$($z$).
    </dl>

    >> StruveL[1.5, 3.5]
     = 4.41126

    >> Plot[StruveL[0, x], {x, 0, 5}]
     = -Graphics-
    """

    sympy_name = ""
    mpmath_name = "struvel"


class AngerJ(_Bessel):
    """
    <dl>
    <dt>'AngerJ[$n$, $z$]'
      <dd>returns the Anger function J_$n$($z$).
    </dl>

    >> AngerJ[1.5, 3.5]
     = 0.294479

    >> Plot[AngerJ[1, x], {x, -10, 10}]
     = -Graphics-
    """

    # TODO: Associated Anger function AngerJ[v, u, z]

    sympy_name = ""
    mpmath_name = "angerj"


class WeberE(_Bessel):
    """
    <dl>
    <dt>'WeberE[$n$, $z$]'
      <dd>returns the Weber function E_$n$($z$).
    </dl>

    >> WeberE[1.5, 3.5]
     = -0.397256

    >> Plot[WeberE[1, x], {x, -10, 10}]
     = -Graphics-
    """

    # TODO: Associated Weber function WeberE[v, u, z]

    sympy_name = ""
    mpmath_name = "webere"


# Function Zeros


class BesselJZero(_Bessel):
    """
    <dl>
    <dt>'BesselJZero[$n$, $k$]'
      <dd>returns the $k$th zero of the Bessel function of the first kind J_$n$($z$).
    </dl>

    >> N[BesselJZero[0, 1]]
     = 2.40483

    #> N[BesselJZero[0, 1], 20]
     = 2.4048255576957727686
    """

    sympy_name = ""
    mpmath_name = "besseljzero"


class BesselYZero(_Bessel):
    """
    <dl>
    <dt>'BesselYZero[$n$, $k$]'
      <dd>returns the $k$th zero of the Bessel function of the second kind Y_$n$($z$).
    </dl>

    >> N[BesselYZero[0, 1]]
     = 0.893577

    #> N[BesselYZero[0, 1]]
     = 0.893577
    """

    sympy_name = ""
    mpmath_name = "besselyzero"


class AiryAiZero(Builtin):
    """
    <dl>
    <dt>'AiryAiZero[$k$]'
      <dd>returns the $k$th zero of the Airy function Ai($z$).
    </dl>

    >> N[AiryAiZero[1]]
     = -2.33811

    #> AiryAiZero[1]
     = AiryAiZero[1]

    #> AiryAiZero[1.]
     = AiryAiZero[1.]

    #> AiryAi[AiryAiZero[1]]
     = 0

    #> N[AiryAiZero[2], 100]
     = -4.087949444130970616636988701457391060224764699108529754984160876025121946836047394331169160758270562
    """

    # TODO: 'AiryAiZero[$k$, $x0$]' - $k$th zero less than x0

    attributes = (
        "Listable",
        "NHoldFirst",
        "NumericFunction",
        "Protected",
        "ReadProtected",
    )

    rules = {
        "AiryAi[AiryAiZero[k_]]": "0",
    }

    def apply_N(self, k, precision, evaluation):
        "N[AiryAiZero[k_Integer], precision_]"

        try:
            d = get_precision(precision, evaluation)
        except PrecisionValueError:
            return

        if d is None:
            p = machine_precision
        else:
            p = _prec(d)

        k_int = k.get_int_value()

        with mpmath.workprec(p):
            result = mpmath.airyaizero(k_int)
            return from_mpmath(result, d)


class AiryBiZero(Builtin):
    """
    <dl>
    <dt>'AiryBiZero[$k$]'
      <dd>returns the $k$th zero of the Airy function Bi($z$).
    </dl>

    >> N[AiryBiZero[1]]
     = -1.17371

    #> AiryBiZero[1]
     = AiryBiZero[1]

    #> AiryBiZero[1.]
     = AiryBiZero[1.]

    #> AiryBi[AiryBiZero[1]]
     = 0

    #> N[AiryBiZero[2], 100]
     = -3.271093302836352715680228240166413806300935969100284801485032396261130864238742879252000673830055014
    """

    # TODO: 'AiryBiZero[$k$, $x0$]' - $k$th zero less than x0

    attributes = (
        "Listable",
        "NHoldFirst",
        "NumericFunction",
        "Protected",
        "ReadProtected",
    )

    rules = {
        "AiryBi[AiryBiZero[z_]]": "0",
    }

    def apply_N(self, k, precision, evaluation):
        "N[AiryBiZero[k_Integer], precision_]"

        try:
            d = get_precision(precision, evaluation)
        except PrecisionValueError:
            return

        if d is None:
            p = machine_precision
        else:
            p = _prec(d)

        k_int = k.get_int_value()

        with mpmath.workprec(p):
            result = mpmath.airybizero(k_int)
            return from_mpmath(result, d)


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

    rules = {"LegendreP[n_, x_]": "LegendreP[n, 0, x]"}

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

    rules = {"LegendreQ[n_, x_]": "LegendreQ[n, 0, x]"}

    nargs = 3
    sympy_name = ""
    mpmath_name = "legenq"

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
     = 1410.02 + 5797.3 I
    """

    nargs = 4
    sympy_name = "jacobi"
    mpmath_name = "jacobi"


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
     = -3.2621 - 24.9739 I
    """

    # TODO: Two argument renormalized form GegenbauerC[n, x]

    nargs = 3
    sympy_name = "gegenbauer"
    mpmath_name = "gegenbauer"


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


class FresnelS(_MPMathFunction):
    """
    <dl>
    <dt>'FresnelS[$z$]'
        <dd>is the Fresnel S integral $S$($z$).
    </dl>

    >> FresnelS[{0, Infinity}]
     = {0, 1 / 2}

    ## SymPy can't currently simplify this all the way to FresnelS[z].
    >> Integrate[Sin[x^2 Pi/2], {x, 0, z}]
     = 3 FresnelS[z] Gamma[3 / 4] / (4 Gamma[7 / 4])
    """

    mpmath_name = "fresnels"


class FresnelC(_MPMathFunction):
    """
    <dl>
    <dt>'FresnelC[$z$]'
        <dd>is the Fresnel C integral $C$($z$).
    </dl>

    >> FresnelC[{0, Infinity}]
     = {0, 1 / 2}

    ## SymPy can't currently simplify this all the way to FresnelC[z].
    >> Integrate[Cos[x^2 Pi/2], {x, 0, z}]
     = FresnelC[z] Gamma[1 / 4] / (4 Gamma[5 / 4])
    """

    mpmath_name = "fresnelc"
