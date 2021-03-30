# -*- coding: utf-8 -*-

"""
Number theoretic functions
"""

import sympy
from itertools import combinations

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin, Test, SympyFunction
from mathics.core.expression import (
    Expression,
    Integer,
    Rational,
    Symbol,
    from_python,
    SymbolN,
)
from mathics.core.convert import from_sympy, SympyPrime
import mpmath


class ContinuedFraction(SympyFunction):
    """
    <dl>
      <dt>'ContinuedFraction[$x$, $n$]'
      <dd>generate the first $n$ terms in the continued fraction reprentation of $x$.
      <dt>'ContinuedFraction[$x$]'
      <dd>the complete continued fraction representation for a rational or quadradic irrational number.
    </dl>

    >> ContinuedFraction[Pi, 10]
     = {3, 7, 15, 1, 292, 1, 1, 1, 2, 1}

    >> ContinuedFraction[(1 + 2 Sqrt[3])/5]
     = {0, 1, {8, 3, 34, 3}}

    >> ContinuedFraction[Sqrt[70]]
     = {8, {2, 1, 2, 1, 2, 16}}
    """

    sympy_name = "continued_fraction"

    attributes = ("Listable", "NumericFunction")

    def apply_1(self, x, evaluation):
        "%(name)s[x_]"
        return super().apply(x)

    def apply_2(self, x, n, evaluation):
        "%(name)s[x_, n_Integer]"
        py_n = n.to_python()
        sympy_x = x.to_sympy()
        it = sympy.continued_fraction_iterator(sympy_x)
        return from_sympy([next(it) for _ in range(py_n)])


class CoprimeQ(Builtin):
    """
    <dl>
    <dt>'CoprimeQ[$x$, $y$]'
        <dd>tests whether $x$ and $y$ are coprime by computing their
        greatest common divisor.
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


class Divisors(Builtin):
    """
    <dl>
    <dt>'Divisors[$n$]'
        <dd>returns a list of the integers that divide $n$.
    </dl>

    >> Divisors[96]
     = {1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 96}
    >> Divisors[704]
     = {1, 2, 4, 8, 11, 16, 22, 32, 44, 64, 88, 176, 352, 704}
    >> Divisors[{87, 106, 202, 305}]
     = {{1, 3, 29, 87}, {1, 2, 53, 106}, {1, 2, 101, 202}, {1, 5, 61, 305}}
    #> Divisors[0]
     = Divisors[0]
    #> Divisors[{-206, -502, -1702, 9}]
     = {{1, 2, 103, 206}, {1, 2, 251, 502}, {1, 2, 23, 37, 46, 74, 851, 1702}, {1, 3, 9}}
    #> Length[Divisors[1000*369]]
     = 96
    #> Length[Divisors[305*176*369*100]]
     = 672
    """

    # TODO: support GaussianIntegers
    # e.g. Divisors[2, GaussianIntegers -> True]

    attributes = ("Listable",)

    def apply(self, n, evaluation):
        "Divisors[n_Integer]"
        if n == Integer(0):
            return None
        return Expression(
            "List", *[from_sympy(i) for i in sympy.divisors(n.to_sympy())]
        )


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

    def test(self, n):
        value = n.get_int_value()
        return value is not None and value % 2 == 0


# FIXME: Previosuly this used gmpy's gcdext. sympy's gcdex is not as powerful
# class ExtendedGCD(Builtin):
#    """
#    >> ExtendedGCD[10, 15]
#     = {5, {-1, 1}}
#
#    'ExtendedGCD' works with any number of arguments:
#    >> ExtendedGCD[10, 15, 7]
#     = {1, {-3, 3, -2}}
#
#    Compute the greated common divisor and check the result:
#    >> numbers = {10, 20, 14};
#    >> {gcd, factors} = ExtendedGCD[Sequence @@ numbers]
#     = {2, {3, 0, -2}}
#    >> Plus @@ (numbers * factors)
#     = 2
#
#    'ExtendedGCD' does not work for rational numbers and Gaussian integers yet
#    """
#
#    attributes = ('Listable',)
#
#    def apply(self, ns, evaluation):
#        'ExtendedGCD[ns___Integer]'
#
#        ns = ns.get_sequence()
#        result = 0
#        coeff = []
#        for n in ns:
#            value = n.get_int_value()
#            if value is None:
#                return
#            new_result, c1, c2 = sympy.gcdex(result, value)
#            result = new_result
#            coeff = [c * c1 for c in coeff] + [c2]
#            return Expression('List', Integer(result), Expression(
#                'List', *(Integer(c) for c in coeff)))


class FactorInteger(Builtin):
    """
    <dl>
    <dt>'FactorInteger[$n$]'
        <dd>returns the factorization of $n$ as a list of factors and exponents.
    </dl>

    >> factors = FactorInteger[2010]
     = {{2, 1}, {3, 1}, {5, 1}, {67, 1}}
    To get back the original number:
    >> Times @@ Power @@@ factors
     = 2010
    'FactorInteger' factors rationals using negative exponents:
    >> FactorInteger[2010 / 2011]
     = {{2, 1}, {3, 1}, {5, 1}, {67, 1}, {2011, -1}}
    """

    # TODO: GausianIntegers option
    # e.g. FactorInteger[5, GaussianIntegers -> True]

    def apply(self, n, evaluation):
        "FactorInteger[n_]"

        if isinstance(n, Integer):
            factors = sympy.factorint(n.value)
            factors = sorted(factors.items())
            return Expression(
                "List", *(Expression("List", factor, exp) for factor, exp in factors)
            )

        elif isinstance(n, Rational):
            factors, factors_denom = list(
                map(sympy.factorint, n.value.as_numer_denom())
            )
            for factor, exp in factors_denom.items():
                factors[factor] = factors.get(factor, 0) - exp
            factors = sorted(factors.items())
            return Expression(
                "List", *(Expression("List", factor, exp) for factor, exp in factors)
            )
        else:
            return evaluation.message("FactorInteger", "exact", n)


def _fractional_part(self, n, expr, evaluation):
    n_sympy = n.to_sympy()
    if n_sympy.is_constant():
        if n_sympy >= 0:
            positive_integer_part = (
                Expression("Floor", n).evaluate(evaluation).to_python()
            )
            result = n - positive_integer_part
        else:
            negative_integer_part = (
                Expression("Ceiling", n).evaluate(evaluation).to_python()
            )
            result = n - negative_integer_part
    else:
        return expr

    return from_python(result)


class FractionalPart(Builtin):
    """
    <dl>
    <dt>'FractionalPart[$n$]'
        <dd>finds the fractional part of $n$.
    </dl>

    >> FractionalPart[4.1]
     = 0.1

    >> FractionalPart[-5.25]
     = -0.25

    #> FractionalPart[b]
     = FractionalPart[b]

    #> FractionalPart[{-2.4, -2.5, -3.0}]
     = {-0.4, -0.5, 0.}

    #> FractionalPart[14/32]
     = 7 / 16

    #> FractionalPart[4/(1 + 3 I)]
     = 2 / 5 - I / 5

    #> FractionalPart[Pi^20]
     = -8769956796 + Pi ^ 20
    """

    attributes = ("Listable", "NumericFunction", "ReadProtected")

    def apply(self, n, evaluation):
        "FractionalPart[n_]"
        expr = Expression("FractionalPart", n)
        return _fractional_part(self.__class__.__name__, n, expr, evaluation)

    def apply_2(self, n, evaluation):
        "FractionalPart[n_Complex]"
        expr = Expression("FractionalPart", n)
        n_real = Expression("Re", n).evaluate(evaluation)
        n_image = Expression("Im", n).evaluate(evaluation)

        real_fractional_part = _fractional_part(
            self.__class__.__name__, n_real, expr, evaluation
        )
        image_fractional_part = _fractional_part(
            self.__class__.__name__, n_image, expr, evaluation
        )
        return Expression("Complex", real_fractional_part, image_fractional_part)


class FromContinuedFraction(SympyFunction):
    """
    <dl>
      <dt>'FromContinuedFraction[$list$]'
      <dd>reconstructs a number from the list of its continued fraction terms.
    </dl>

    >> FromContinuedFraction[{3, 7, 15, 1, 292, 1, 1, 1, 2, 1}]
     = 1146408 / 364913

    >> FromContinuedFraction[Range[5]]
     = 225 / 157
    """

    sympy_name = "continued_fraction_reduce"

    attributes = ("NumericFunction",)

    def apply_1(self, expr, evaluation):
        "%(name)s[expr_?ListQ]"
        nums = expr.to_python()
        if all(isinstance(i, int) for i in nums):
            return from_sympy(sympy.continued_fraction_reduce(nums))


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


class IntegerExponent(Builtin):
    """
    <dl>
    <dt>'IntegerExponent[$n$, $b$]'
        <dd>gives the highest exponent of $b$ that divides $n$.
    </dl>

    >> IntegerExponent[16, 2]
     = 4

    >> IntegerExponent[-510000]
     = 4

    >> IntegerExponent[10, b]
     = IntegerExponent[10, b]
    """

    rules = {
        "IntegerExponent[n_]": "IntegerExponent[n, 10]",
    }

    messages = {
        "int": "Integer expected at position 1 in `1`",
        "ibase": "Base `1` is not an integer greater than 1.",
    }

    def apply(self, n, b, evaluation):
        "IntegerExponent[n_Integer, b_Integer]"

        py_n, py_b = n.to_python(), b.to_python()
        expr = Expression("IntegerExponent", n, b)

        if not isinstance(py_n, int):
            evaluation.message("IntegerExponent", "int", expr)
        py_n = abs(py_n)

        if not (isinstance(py_b, int) and py_b > 1):
            evaluation.message("IntegerExponent", "ibase", b)

        # TODO: Optimise this (dont need to calc. base^result)
        # NOTE: IntegerExponent[a,b] causes a Python error here when a or b are
        # symbols
        result = 1
        while py_n % (py_b ** result) == 0:
            result += 1

        return from_python(result - 1)


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


class MantissaExponent(Builtin):
    """
    <dl>
    <dt>'MantissaExponent[$n$]'
        <dd>finds a list containing the mantissa and exponent of a given number $n$.
    <dt>'MantissaExponent[$n$, $b$]'
        <dd>finds the base b mantissa and exponent of $n$.
    </dl>

    >> MantissaExponent[2.5*10^20]
     = {0.25, 21}

    >> MantissaExponent[125.24]
     = {0.12524, 3}

    >> MantissaExponent[125., 2]
     = {0.976563, 7}

    >> MantissaExponent[10, b]
     = MantissaExponent[10, b]

    #> MantissaExponent[E, Pi]
     = {E / Pi, 1}

    #> MantissaExponent[Pi, Pi]
     = {1 / Pi, 2}

    #> MantissaExponent[5/2 + 3, Pi]
     = {11 / (2 Pi ^ 2), 2}

    #> MantissaExponent[b]
     = MantissaExponent[b]

    #> MantissaExponent[17, E]
     = {17 / E ^ 3, 3}

    #> MantissaExponent[17., E]
     = {0.84638, 3}

    #> MantissaExponent[Exp[Pi], 2]
     = {E ^ Pi / 32, 5}

    #> MantissaExponent[3 + 2 I, 2]
     : The value 3 + 2 I is not a real number
     = MantissaExponent[3 + 2 I, 2]

    #> MantissaExponent[25, 0.4]
     : Base 0.4 is not a real number greater than 1.
     = MantissaExponent[25, 0.4]

    #> MantissaExponent[0.0000124]
     = {0.124, -4}

    #> MantissaExponent[0.0000124, 2]
     = {0.812646, -16}

    #> MantissaExponent[0]
     = {0, 0}

    #> MantissaExponent[0, 2]
     = {0, 0}
    """

    attributes = ("Listable",)

    rules = {
        "MantissaExponent[0]": "{0, 0}",
        "MantissaExponent[0, n_]": "{0, 0}",
    }

    messages = {
        "realx": "The value `1` is not a real number",
        "rbase": "Base `1` is not a real number greater than 1.",
    }

    def apply(self, n, b, evaluation):
        "MantissaExponent[n_, b_]"
        # Handle Input with special cases such as PI and E
        n_sympy, b_sympy = n.to_sympy(), b.to_sympy()

        expr = Expression("MantissaExponent", n, b)

        if isinstance(n.to_python(), complex):
            evaluation.message("MantissaExponent", "realx", n)
            return expr

        if n_sympy.is_constant():
            temp_n = Expression(SymbolN, n).evaluate(evaluation)
            py_n = temp_n.to_python()
        else:
            return expr

        if b_sympy.is_constant():
            temp_b = Expression(SymbolN, b).evaluate(evaluation)
            py_b = temp_b.to_python()
        else:
            return expr

        if not py_b > 1:
            evaluation.message("MantissaExponent", "rbase", b)
            return expr

        base_exp = int(mpmath.log(py_n, py_b))

        exp = (base_exp + 1) if base_exp >= 0 else base_exp

        return Expression("List", Expression("Divide", n, b ** exp), exp)

    def apply_2(self, n, evaluation):
        "MantissaExponent[n_]"
        n_sympy = n.to_sympy()
        expr = Expression("MantissaExponent", n)

        if isinstance(n.to_python(), complex):
            evaluation.message("MantissaExponent", "realx", n)
            return expr
        # Handle Input with special cases such as PI and E
        if n_sympy.is_constant():
            temp_n = Expression(SymbolN, n).evaluate(evaluation)
            py_n = temp_n.to_python()
        else:
            return expr

        base_exp = int(mpmath.log10(py_n))
        exp = (base_exp + 1) if base_exp >= 0 else base_exp

        return Expression("List", Expression("Divide", n, (10 ** exp)), exp)


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


class NextPrime(Builtin):
    """
    <dl>
    <dt>'NextPrime[$n$]'
        <dd>gives the next prime after $n$.
    <dt>'NextPrime[$n$,$k$]'
        <dd>gives the $k$th  prime after $n$.
    </dl>

    >> NextPrime[10000]
     = 10007

    >> NextPrime[100, -5]
     = 73

    >> NextPrime[10, -5]
    = -2

    >> NextPrime[100, 5]
     = 113

    >> NextPrime[5.5, 100]
     = 563

    >> NextPrime[5, 10.5]
     = NextPrime[5, 10.5]
    """

    rules = {
        "NextPrime[n_]": "NextPrime[n, 1]",
    }

    def apply(self, n, k, evaluation):
        "NextPrime[n_?NumericQ, k_?IntegerQ]"
        py_k = k.to_python(n_evaluation=evaluation)
        py_n = n.to_python(n_evaluation=evaluation)

        if py_k >= 0:
            return from_python(sympy.ntheory.nextprime(py_n, py_k))

        # Hack to get earlier primes
        result = n.to_python()
        for i in range(-py_k):
            try:
                result = sympy.ntheory.prevprime(result)
            except ValueError:
                # No earlier primes
                return from_python(-1 * sympy.ntheory.nextprime(0, py_k - i))

        return from_python(result)


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

    def test(self, n):
        value = n.get_int_value()
        return value is not None and value % 2 != 0


class PartitionsP(SympyFunction):
    """
    <dl>
      <dt>'PartitionsP[$n$]'
      <dd>return the number $p$($n$) of unrestricted partitions of the integer $n$.
    </dl>

    >> Table[PartitionsP[k], {k, -2, 12}]
     = {0, 0, 1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77}
    """

    attributes = ("Listable", "NumericFunction", "Orderless")
    sympy_name = "npartitions"

    def apply(self, n, evaluation):
        "PartitionsP[n_Integer]"
        return super().apply(n)


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


class Prime(SympyFunction):
    """
    <dl>
    <dt>'Prime[$n$]'
        <dd>returns the $n$th prime number.
    </dl>

    >> Prime[1]
     = 2

    >> Prime[167]
     = 991
    """

    def apply(self, n, evaluation):
        "Prime[n_]"
        return from_sympy(SympyPrime(n.to_sympy()))

    def to_sympy(self, expr, **kwargs):
        if expr.has_form("Prime", 1):
            return SympyPrime(expr.leaves[0].to_sympy(**kwargs))


class PrimePi(Builtin):
    """
    <dl>
    <dt>'PrimePi[$x$]'
        <dd>gives the number of primes less than or equal to $x$.
    </dl>

    >> PrimePi[100]
     = 25

    >> PrimePi[-1]
     = 0

    >> PrimePi[3.5]
     = 2

    >> PrimePi[E]
     = 1
    """

    # TODO: Traditional Form

    def apply(self, n, evaluation):
        "PrimePi[n_?NumericQ]"
        result = sympy.ntheory.primepi(n.to_python(n_evaluation=evaluation))
        return from_python(result)


class PrimePowerQ(Builtin):
    """
    <dl>
    <dt>'PrimePowerQ[$n$]'
        <dd>returns 'True' if $n$ is a power of a prime number.
    </dl>

    >> PrimePowerQ[9]
     = True

    >> PrimePowerQ[52142]
     = False

    >> PrimePowerQ[-8]
     = True

    >> PrimePowerQ[371293]
     = True

    #> PrimePowerQ[1]
     = False
    """

    rules = {
        "PrimePowerQ[1]": "False",
    }

    attributes = ("Listable", "Protected", "ReadProtected")

    # TODO: GaussianIntegers option
    """
    #> PrimePowerQ[5, GaussianIntegers -> True]
     = False
    """

    # TODO: Complex args
    """
    #> PrimePowerQ[{3 + I, 3 - 2 I, 3 + 4 I, 9 + 7 I}]
     = {False, True, True, False}
    """

    # TODO: Gaussian rationals
    """
    #> PrimePowerQ[2/125 - 11 I/125]
     = True
    """

    def apply(self, n, evaluation):
        "PrimePowerQ[n_]"
        n = n.get_int_value()
        if n is None:
            return Symbol("False")

        n = abs(n)
        if len(sympy.factorint(n)) == 1:
            return Symbol("True")
        else:
            return Symbol("False")


class PrimeQ(Builtin):
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

    attributes = ("Listable",)

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


class RandomPrime(Builtin):
    """
    <dl>
    <dt>'RandomPrime[{$imin$, $imax}]'
        <dd>gives a random prime between $imin$ and $imax$.
    <dt>'RandomPrime[$imax$]'
        <dd>gives a random prime between 2 and $imax$.
    <dt>'RandomPrime[$range$, $n$]'
        <dd>gives a list of $n$ random primes in $range$.
    </dl>

    >> RandomPrime[{14, 17}]
     = 17

    >> RandomPrime[{14, 16}, 1]
     : There are no primes in the specified interval.
     = RandomPrime[{14, 16}, 1]

    >> RandomPrime[{8,12}, 3]
     = {11, 11, 11}

    >> RandomPrime[{10,30}, {2,5}]
     = ...

    #> RandomPrime[{10,12}, {2,2}]
     = {{11, 11}, {11, 11}}

    #> RandomPrime[2, {3,2}]
     = {{2, 2}, {2, 2}, {2, 2}}
    """

    messages = {
        "posdim": (
            "The dimensions parameter `1` is expected to be a positive "
            "integer or a list of positive integers."
        ),
        "noprime": "There are no primes in the specified interval.",
        "prmrng": (
            "First argument `1` is not a positive integer or a list "
            "of two positive integers."
        ),
        "posint": (
            "The paramater `1` describing the interval is expected to "
            "be a positive integer."
        ),
    }

    rules = {
        "RandomPrime[imax_?NotListQ]": "RandomPrime[{1, imax}, 1]",
        "RandomPrime[int_?ListQ]": "RandomPrime[int, 1]",
        "RandomPrime[imax_?ListQ, n_?ArrayQ]": (
            "ConstantArray[RandomPrime[imax, 1], n]"
        ),
        "RandomPrime[imax_?NotListQ, n_?ArrayQ]": (
            "ConstantArray[RandomPrime[{1, imax}, 1], n]"
        ),
    }

    # TODO: Use random state as in other randomised methods within mathics

    def apply(self, interval, n, evaluation):
        "RandomPrime[interval_?ListQ, n_]"

        if not isinstance(n, Integer):
            evaluation.message("RandomPrime", "posdim", n)
            return
        py_n = n.to_python()

        py_int = interval.to_python()
        if not (isinstance(py_int, list) and len(py_int) == 2):
            evaluation.message("RandomPrime", "prmrng", interval)

        imin, imax = min(py_int), max(py_int)
        if imin <= 0 or not isinstance(imin, int):
            evaluation.message("RandomPrime", "posint", interval.leaves[0])
            return

        if imax <= 0 or not isinstance(imax, int):
            evaluation.message("RandomPrime", "posint", interval.leaves[1])
            return

        try:
            if py_n == 1:
                return from_python(sympy.ntheory.randprime(imin, imax + 1))
            return from_python(
                [sympy.ntheory.randprime(imin, imax + 1) for i in range(py_n)]
            )
        except ValueError:
            evaluation.message("RandomPrime", "noprime")
            return


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
