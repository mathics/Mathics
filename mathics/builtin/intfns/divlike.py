# -*- coding: utf-8 -*-

"""
Division-Related Functions
"""

import sympy
from itertools import combinations

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin, Test, SympyFunction
from mathics.core.expression import (
    Expression,
    Integer,
    Symbol,
)


class CoprimeQ(Builtin):
    """
    <dl>
      <dt>'CoprimeQ[$x$, $y$]'
        <dd>tests whether $x$ and $y$ are coprime by computing their greatest common divisor.
      </dl>

    >> CoprimeQ[7, 9]
     = True

    >> CoprimeQ[-4, 9]
     = True

    >> CoprimeQ[12, 15]
     = False

    CoprimeQ also works for complex numbers
    >> CoprimeQ[1+2I, 1-I]
     = True

    >> CoprimeQ[4+2I, 6+3I]
     = True

    >> CoprimeQ[2, 3, 5]
     = True

    >> CoprimeQ[2, 4, 5]
     = False
    """

    attributes = ("Listable",)

    def apply(self, args, evaluation):
        "CoprimeQ[args__]"

        py_args = [arg.to_python() for arg in args.get_sequence()]
        if not all(isinstance(i, int) or isinstance(i, complex) for i in py_args):
            return Symbol("False")

        if all(sympy.gcd(n, m) == 1 for (n, m) in combinations(py_args, 2)):
            return Symbol("True")
        else:
            return Symbol("False")


class EvenQ(Test):
    """
    <dl>
      <dt>'EvenQ[$x$]'
      <dd>returns 'True' if $x$ is even, and 'False' otherwise.
    </dl>

    >> EvenQ[4]
     = True
    >> EvenQ[-3]
     = False
    >> EvenQ[n]
     = False
    """

    attributes = ("Listable", "Protected")

    def test(self, n):
        value = n.get_int_value()
        return value is not None and value % 2 == 0


class GCD(Builtin):
    """
    <dl>
      <dt>'GCD[$n1$, $n2$, ...]'
      <dd>computes the greatest common divisor of the given integers.
    </dl>

    >> GCD[20, 30]
     = 10
    >> GCD[10, y]
     = GCD[10, y]

    'GCD' is 'Listable':
    >> GCD[4, {10, 11, 12, 13, 14}]
     = {2, 1, 4, 1, 2}

    'GCD' does not work for rational numbers and Gaussian integers yet.
    """

    attributes = ("Flat", "Listable", "OneIdentity", "Orderless")

    def apply(self, ns, evaluation):
        "GCD[ns___Integer]"

        ns = ns.get_sequence()
        result = 0
        for n in ns:
            value = n.get_int_value()
            if value is None:
                return
            result = sympy.gcd(result, value)
        return Integer(result)


class LCM(Builtin):
    """
    <dl>
      <dt>'LCM[$n1$, $n2$, ...]'
      <dd>computes the least common multiple of the given integers.
    </dl>

    >> LCM[15, 20]
     = 60
    >> LCM[20, 30, 40, 50]
     = 600
    """

    attributes = ("Flat", "Listable", "OneIdentity", "Orderless")

    def apply(self, ns, evaluation):
        "LCM[ns___Integer]"

        ns = ns.get_sequence()
        result = 1
        for n in ns:
            value = n.get_int_value()
            if value is None:
                return
            result = sympy.lcm(result, value)
        return Integer(result)


class Mod(Builtin):
    """
    <dl>
    <dt>'Mod[$x$, $m$]'
        <dd>returns $x$ modulo $m$.
    </dl>

    >> Mod[14, 6]
     = 2
    >> Mod[-3, 4]
     = 1
    >> Mod[-3, -4]
     = -3
    >> Mod[5, 0]
     : The argument 0 should be nonzero.
     = Mod[5, 0]
    """

    attributes = ("Listable", "NumericFunction")

    def apply(self, n, m, evaluation):
        "Mod[n_Integer, m_Integer]"

        n, m = n.get_int_value(), m.get_int_value()
        if m == 0:
            evaluation.message("Mod", "divz", m)
            return
        return Integer(n % m)


class OddQ(Test):
    """
    <dl>
      <dt>'OddQ[$x$]'
      <dd>returns 'True' if $x$ is odd, and 'False' otherwise.
    </dl>

    >> OddQ[-3]
     = True
    >> OddQ[0]
     = False
    """

    attributes = ("Listable", "Protected")

    def test(self, n):
        value = n.get_int_value()
        return value is not None and value % 2 != 0


class PowerMod(Builtin):
    """
    <dl>
      <dt>'PowerMod[$x$, $y$, $m$]'
      <dd>computes $x$^$y$ modulo $m$.
    </dl>

    >> PowerMod[2, 10000000, 3]
     = 1
    >> PowerMod[3, -2, 10]
     = 9
    >> PowerMod[0, -1, 2]
     : 0 is not invertible modulo 2.
     = PowerMod[0, -1, 2]
    >> PowerMod[5, 2, 0]
     : The argument 0 should be nonzero.
     = PowerMod[5, 2, 0]

    'PowerMod' does not support rational coefficients (roots) yet.
    """

    attributes = ("Listable",)

    messages = {
        "ninv": "`1` is not invertible modulo `2`.",
    }

    def apply(self, a, b, m, evaluation):
        "PowerMod[a_Integer, b_Integer, m_Integer]"

        a_int = a
        m_int = m
        a, b, m = a.value, b.value, m.value
        if m == 0:
            evaluation.message("PowerMod", "divz", m)
            return
        if b < 0:
            b = -b
            try:
                a = int(sympy.invert(a, m))
            except sympy.polys.polyerrors.NotInvertible:
                evaluation.message("PowerMod", "ninv", a_int, m_int)
                return
        return Integer(pow(a, b, m))


class PrimeQ(SympyFunction):
    """
    <dl>
      <dt>'PrimeQ[$n$]'
      <dd>returns 'True' if $n$ is a prime number.
    </dl>

    For very large numbers, 'PrimeQ' uses probabilistic prime testing, so it might be wrong sometimes
    (a number might be composite even though 'PrimeQ' says it is prime).
    The algorithm might be changed in the future.

    >> PrimeQ[2]
     = True
    >> PrimeQ[-3]
     = True
    >> PrimeQ[137]
     = True
    >> PrimeQ[2 ^ 127 - 1]
     = True

    #> PrimeQ[1]
     = False
    #> PrimeQ[2 ^ 255 - 1]
     = False

    All prime numbers between 1 and 100:
    >> Select[Range[100], PrimeQ]
     = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97}

    'PrimeQ' has attribute 'Listable':
    >> PrimeQ[Range[20]]
     = {False, True, True, False, True, False, True, False, False, False, True, False, True, False, False, False, True, False, True, False}
    """

    attributes = ("Listable", "NumericFunction")

    sympy_name = "isprime"

    def apply(self, n, evaluation):
        "PrimeQ[n_]"

        n = n.get_int_value()
        if n is None:
            return Symbol("False")

        n = abs(n)
        if sympy.isprime(n):
            return Symbol("True")
        else:
            return Symbol("False")


class Quotient(Builtin):
    """
    <dl>
      <dt>'Quotient[m, n]'
      <dd>computes the integer quotient of $m$ and $n$.
    </dl>

    >> Quotient[23, 7]
     = 3

    #> Quotient[13, 0]
     : Infinite expression Quotient[13, 0] encountered.
     = ComplexInfinity
    #> Quotient[-17, 7]
     = -3
    #> Quotient[-17, -4]
     = 4
    #> Quotient[19, -4]
     = -5
    """

    attributes = ("Listable", "NumericFunction")

    messages = {
        "infy": "Infinite expression `1` encountered.",
    }

    def apply(self, m, n, evaluation):
        "Quotient[m_Integer, n_Integer]"
        py_m = m.get_int_value()
        py_n = n.get_int_value()
        if py_n == 0:
            evaluation.message("Quotient", "infy", Expression("Quotient", m, n))
            return Symbol("ComplexInfinity")
        return Integer(py_m // py_n)


class QuotientRemainder(Builtin):
    """
    <dl>
      <dt>'QuotientRemainder[m, n]'
      <dd>computes a list of the quotient and remainder from division of $m$ by $n$.
    </dl>

    >> QuotientRemainder[23, 7]
     = {3, 2}

    #> QuotientRemainder[13, 0]
     : The argument 0 in QuotientRemainder[13, 0] should be nonzero.
     = QuotientRemainder[13, 0]
    #> QuotientRemainder[-17, 7]
     = {-3, 4}
    #> QuotientRemainder[-17, -4]
     = {4, -1}
    #> QuotientRemainder[19, -4]
     = {-5, -1}
    #> QuotientRemainder[a, 0]
     = QuotientRemainder[a, 0]
    #> QuotientRemainder[a, b]
     = QuotientRemainder[a, b]
    #> QuotientRemainder[5.2,2.5]
     = {2, 0.2}
    #> QuotientRemainder[5, 2.]
     = {2, 1.}
    """

    attributes = ("Listable", "NumericFunction")

    messages = {
        "divz": "The argument 0 in `1` should be nonzero.",
    }

    def apply(self, m, n, evaluation):
        "QuotientRemainder[m_, n_]"
        if m.is_numeric() and n.is_numeric():
            py_m = m.to_python()
            py_n = n.to_python()
            if py_n == 0:
                return evaluation.message(
                    "QuotientRemainder", "divz", Expression("QuotientRemainder", m, n)
                )
            return Expression("List", Integer(py_m // py_n), (py_m % py_n))
        else:
            return Expression("QuotientRemainder", m, n)
