# -*- coding: utf-8 -*-

"""
Error Function and Related Functions
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.arithmetic import _MPMathFunction, _MPMathMultiFunction


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

    attributes = ("Listable", "NumericFunction")

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

    # attributes = ("Listable", "NumericFunction") # inherited

    mpmath_name = "erfc"

    rules = {
        "Derivative[1][Erfc]": "-2 Exp[-#^2] / Sqrt[Pi] &",
    }


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
     = FresnelC[z]
    """

    rules = {
        "Derivative[1][FresnelC]": "Cos[(Pi*#1^2)/2]&",
    }
    mpmath_name = "fresnelc"


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
     = FresnelS[z]
    """

    rules = {
        "Derivative[1][FresnelS]": "Sin[(Pi*#1^2)/2]&",
    }
    mpmath_name = "fresnels"


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

    # No inherited NumericFunction
    attributes = ("Listable", "Protected")

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


class InverseErfc(_MPMathFunction):
    """
    <dl>
    <dt>'InverseErfc[$z$]'
        <dd>returns the inverse complementary error function of $z$.
    </dl>

    >> InverseErfc /@ {0, 1, 2}
     = {Infinity, 0, -Infinity}
    """

    # No inherited NumericFunction
    attributes = ("Listable", "Protected")
    sympy_name = "erfcinv"

    rules = {
        "Derivative[1][InverseErfc]": "-Sqrt[Pi] Exp[InverseErfc[#]^2] / 2 &",
    }
