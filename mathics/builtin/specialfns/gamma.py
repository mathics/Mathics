"""
Gamma and Related Functions
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.arithmetic import _MPMathMultiFunction
from mathics.builtin.base import SympyFunction
from mathics.core.expression import Expression, Integer0


class Gamma(_MPMathMultiFunction):
    """
    In number theory the logarithm of the gamma function often appears. For positive real numbers, this can be evaluated as 'Log[Gamma[$z$]]'.

    <dl>
      <dt>'Gamma[$z$]'
      <dd>is the gamma function on the complex number $z$.

      <dt>'Gamma[$z$, $x$]'
      <dd>is the upper incomplete gamma function.

      <dt>'Gamma[$z$, $x0$, $x1$]'
      <dd>is equivalent to 'Gamma[$z$, $x0$] - Gamma[$z$, $x1$]'.
    </dl>

    'Gamma[$z$]' is equivalent to '($z$ - 1)!':
    >> Simplify[Gamma[z] - (z - 1)!]
     = 0

    Exact arguments:
    >> Gamma[8]
     = 5040
    >> Gamma[1/2]
     = Sqrt[Pi]
    >> Gamma[1, x]
     = E ^ (-x)
    >> Gamma[0, x]
     = ExpIntegralE[1, x]

    Numeric arguments:
    >> Gamma[123.78]
     = 4.21078*^204
    >> Gamma[1. + I]
     = 0.498016 - 0.15495 I

    Both 'Gamma' and 'Factorial' functions are continuous:
    >> Plot[{Gamma[x], x!}, {x, 0, 4}]
     = -Graphics-

    ## Issue 203
    #> N[Gamma[24/10], 100]
     = 1.242169344504305404913070252268300492431517240992022966055507541481863694148882652446155342679460339
    #> N[N[Gamma[24/10],100]/N[Gamma[14/10],100],100]
     = 1.400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    #> % // Precision
     = 100.

    #> Gamma[1.*^20]
     : Overflow occurred in computation.
     = Overflow[]

    ## Needs mpmath support for lowergamma
    #> Gamma[1., 2.]
     = Gamma[1., 2.]
    """

    mpmath_names = {
        1: "gamma",
    }
    sympy_names = {
        1: "gamma",
        2: "uppergamma",
    }

    rules = {
        "Gamma[z_, x0_, x1_]": "Gamma[z, x0] - Gamma[z, x1]",
        "Gamma[1 + z_]": "z!",
        "Derivative[1][Gamma]": "(Gamma[#1]*PolyGamma[0, #1])&",
        "Derivative[1, 0][Gamma]": "(Gamma[#1, #2]*Log[#2] + MeijerG[{{}, {1, 1}}, {{0, 0, #1}, {}}, #2])&",
        "Derivative[0, 1][Gamma]": "(-(#2^(-1 + #1)/E^#2))&",
    }

    def get_sympy_names(self):
        return ["gamma", "uppergamma", "lowergamma"]

    def from_sympy(self, sympy_name, leaves):
        if sympy_name == "lowergamma":
            # lowergamma(z, x) -> Gamma[z, 0, x]
            z, x = leaves
            return Expression(self.get_name(), z, Integer0, x)
        else:
            return Expression(self.get_name(), *leaves)


class Pochhammer(SympyFunction):
    """
    The Pochhammer symbol or rising factorial often appears in series expansions for hypergeometric functions.
    The Pochammer symbol has a definie value even when the gamma functions which appear in its definition are infinite.
    <dl>
    <dt>'Pochhammer[$a$, $n$]'
        <dd>is the Pochhammer symbol (a)_n.
    </dl>

    >> Pochhammer[4, 8]
     = 6652800
    """

    attributes = ("Listable", "NumericFunction", "Protected")

    sympy_name = "RisingFactorial"

    rules = {
        "Pochhammer[a_, n_]": "Gamma[a + n] / Gamma[a]",
        "Derivative[1,0][Pochhammer]": "(Pochhammer[#1, #2]*(-PolyGamma[0, #1] + PolyGamma[0, #1 + #2]))&",
        "Derivative[0,1][Pochhammer]": "(Pochhammer[#1, #2]*PolyGamma[0, #1 + #2])&",
    }
