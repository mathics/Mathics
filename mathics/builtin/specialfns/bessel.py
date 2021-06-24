"""
Bessel and Related Functions
"""

import mpmath


from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.arithmetic import _MPMathFunction
from mathics.builtin.base import Builtin
from mathics.core.expression import from_mpmath
from mathics.core.numbers import machine_precision, get_precision, PrecisionValueError
from mathics.core.numbers import prec as _prec


class _Bessel(_MPMathFunction):

    attributes = ("Listable", "NumericFunction", "Protected", "ReadProtected")

    nargs = 2


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

    # attributes = ("Listable", "NumericFunction") # inherited

    mpmath_name = "airyai"
    sympy_name = "airyai"

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

    rules = {
        "Derivative[1][AiryAiPrime]": "(#1 AiryAi[#1])&",
    }

    attributes = ("Listable", "NumericFunction")

    mpmath_name = ""
    sympy_name = "airyaiprime"

    def get_mpmath_function(self, args):
        return lambda x: mpmath.airyai(x, derivative=1)


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

    attributes = ("Listable", "NumericFunction")

    mpmath_name = "airybi"
    sympy_name = "airybi"

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

    # attributes = ("Listable", "NumericFunction") # inherited

    mpmath_name = ""
    sympy_name = "airybiprime"

    rules = {
        "Derivative[1][AiryBiPrime]": "(#1 AiryBi[#1])&",
    }

    def get_mpmath_function(self, args):
        return lambda x: mpmath.airybi(x, derivative=1)


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

    # attributes = ("Listable", "NumericFunction") # inherited

    mpmath_name = "angerj"
    sympy_name = ""


# Bessel Functions


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

    rules = {
        "Derivative[0, 1][BesselI]": "((BesselI[-1 + #1, #2] + BesselI[1 + #1, #2])/2)&",
    }

    attributes = ("Listable", "NumericFunction", "Protected")

    sympy_name = "besseli"
    mpmath_name = "besseli"


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

    >> D[BesselJ[n, z], z]
     = -BesselJ[1 + n, z] / 2 + BesselJ[-1 + n, z] / 2

    #> BesselJ[0., 0.]
     = 1.

    >> Plot[BesselJ[0, x], {x, 0, 10}]
     = -Graphics-
    """

    # TODO: Sympy Backend is not as powerful as Mathematica
    """
    >> BesselJ[1/2, x]
     = Sqrt[2 / Pi] Sin[x] / Sqrt[x]
    """
    rules = {
        "Derivative[0,1][BesselJ]": "(BesselJ[#1- 1, #2] / 2 - BesselJ[#1 + 1, #2] / 2)&",
    }

    attributes = ("Listable", "NumericFunction", "Protected")

    sympy_name = "besselj"
    mpmath_name = "besselj"


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

    mpmath_name = "besselk"
    sympy_name = "besselk"

    rules = {
        "Derivative[0, 1][BesselK]": "((-BesselK[-1 + #1, #2] - BesselK[1 + #1, #2])/2)&",
    }


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
    rules = {
        "Derivative[0,1][BesselY]": "(BesselY[-1 + #1, #2] / 2 - BesselY[1 + #1, #2] / 2)&",
    }

    attributes = ("Listable", "NumericFunction", "Protected")

    mpmath_name = "bessely"
    sympy_name = "bessely"


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

    # attributes = ("Listable", "NumericFunction") # inherited

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

    attributes = ("Listable", "NumericFunction")

    mpmath_name = "besselyzero"
    sympy_name = ""


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

    rules = {
        "Derivative[0, 1][HankelH1]": "((HankelH1[-1 + #1, #2] - HankelH1[1 + #1, #2])/2)&",
    }
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

    rules = {
        "Derivative[0, 1][HankelH2]": "((HankelH2[-1 + #1, #2] - HankelH2[1 + #1, #2])/2)&",
    }

    sympy_name = "hankel2"
    mpmath_name = "hankel2"


# Kelvin Functions


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
        "Derivative[1][KelvinBei]": "((2*KelvinBei[1, #1] - 2*KelvinBer[1, #1])/(2*Sqrt[2]))&",
    }

    attributes = ("Listable", "NumericFunction")

    mpmath_name = "bei"
    sympy_name = ""


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
        "Derivative[1][KelvinBer]": "((2*KelvinBei[1, #1] + 2*KelvinBer[1, #1])/(2*Sqrt[2]))&",
    }

    attributes = ("Listable", "NumericFunction")

    mpmath_name = "ber"
    sympy_name = ""


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

    attributes = ("Listable", "NumericFunction")

    mpmath_name = "ker"
    sympy_name = ""


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

    attributes = ("Listable", "NumericFunction")

    mpmath_name = "struveh"
    sympy_name = ""


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

    attributes = ("Listable", "NumericFunction")

    mpmath_name = "struvel"
    sympy_name = ""


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

    attributes = ("Listable", "NumericFunction")

    mpmath_name = "webere"
    sympy_name = ""
