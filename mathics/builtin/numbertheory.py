# -*- coding: utf8 -*-

"""
Number theoretic functions
"""

import sympy
from itertools import combinations

from mathics.builtin.base import Builtin, Test
from mathics.core.expression import Expression, Integer, Rational, Symbol, from_python


class PowerMod(Builtin):
    """
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

    attributes = ('Listable',)

    messages = {
        'ninv': "`1` is not invertible modulo `2`.",
    }

    def apply(self, a, b, m, evaluation):
        'PowerMod[a_Integer, b_Integer, m_Integer]'

        a_int = a
        m_int = m
        a, b, m = a.value, b.value, m.value
        if m == 0:
            evaluation.message('PowerMod', 'divz', m)
            return
        if b < 0:
            b = -b
            if a == 0:
                evaluation.message('PowerMod', 'ninv', a_int, m_int)
                return
            a = sympy.invert(a, m)
        return Integer(sympy.Mod(a ** b, m))


class Mod(Builtin):
    """
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

    attributes = ('Listable', 'NumericFunction')

    def apply(self, n, m, evaluation):
        'Mod[n_Integer, m_Integer]'

        n_int, m_int = n, m
        n, m = n.value, m.value
        if m == 0:
            evaluation.message('Mod', 'divz', m)
            return
        return Integer(n % m)


class EvenQ(Test):
    """
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


class OddQ(Test):
    """
    >> OddQ[-3]
     = True
    >> OddQ[0]
     = False
    """

    def test(self, n):
        value = n.get_int_value()
        return value is not None and value % 2 != 0


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

    attributes = ('Flat', 'Listable', 'OneIdentity', 'Orderless')

    def apply(self, ns, evaluation):
        'GCD[ns___Integer]'

        ns = ns.get_sequence()
        result = 0
        for n in ns:
            value = n.get_int_value()
            if value is None:
                return
            result = sympy.gcd(result, value)
        return Integer(result)

# TODO: Previosuly this used gmpy's gcdext. sympy's gcdex is not as powerful unfortunately
# class ExtendedGCD(Builtin):
#    u"""
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
#    'ExtendedGCD' does not work for rational numbers and Gaussian integers yet.
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
# return Expression('List', Integer(result), Expression('List',
# *(Integer(c) for c in coeff)))


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

    attributes = ('Flat', 'Listable', 'OneIdentity', 'Orderless')

    def apply(self, ns, evaluation):
        'LCM[ns___Integer]'

        ns = ns.get_sequence()
        result = 1
        for n in ns:
            value = n.get_int_value()
            if value is None:
                return
            result = sympy.lcm(result, value)
        return Integer(result)


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

    # TODO: GausianIntegers option e.g. FactorInteger[5, GaussianIntegers ->
    # True]

    def apply(self, n, evaluation):
        'FactorInteger[n_]'

        if isinstance(n, Integer):
            factors = sympy.factorint(n.value)
            factors = sorted(factors.iteritems())
            return Expression('List', *[Expression('List', factor, exp) for factor, exp in factors])
        elif isinstance(n, Rational):
            factors, factors_denom = map(
                sympy.factorint, n.value.as_numer_denom())
            for factor, exp in factors_denom.iteritems():
                factors[factor] = factors.get(factor, 0) - exp
            factors = sorted(factors.iteritems())
            return Expression('List', *[Expression('List', factor, exp) for factor, exp in factors])
        else:
            return evaluation.message('FactorInteger', 'exact', n)


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
        'IntegerExponent[n_]': 'IntegerExponent[n, 10]',
    }

    messages = {
        'int': 'Integer expected at position 1 in `1`',
        'ibase': 'Base `1` is not an integer greater than 1.',
    }

    def apply(self, n, b, evaluation):
        'IntegerExponent[n_Integer, b_Integer]'

        py_n, py_b = n.to_python(), b.to_python()
        expr = Expression('IntegerExponent', n, b)

        if not (isinstance(py_n, int) or isinstance(py_n, long)):
            evaluation.message('IntegerExponent', 'int', expr)
        py_n = abs(py_n)

        if not (isinstance(py_b, int) and py_b > 1):
            evaluation.message('IntegerExponent', 'ibase', b)

        # TODO: Optimise this (dont need to calc. base^result)
        # NOTE: IntegerExponent[a,b] causes a Python error here when a or b are
        # symbols
        result = 1
        while py_n % (py_b ** result) == 0:
            result += 1

        return from_python(result - 1)


class Prime(Builtin):
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

    messages = {
        'intpp': 'Positive integer argument expected in `1`.',
    }

    def apply(self, n, evaluation):
        'Prime[n_]'
        n_int = n.to_python()
        if isinstance(n_int, int) and n_int > 0:
            return Integer(sympy.prime(n_int))

        expr = Expression('Prime', n)
        evaluation.message('Prime', 'intpp', expr)
        return


class PrimeQ(Builtin):
    """
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

    attributes = ('Listable',)

    def apply(self, n, evaluation):
        'PrimeQ[n_]'

        n = n.get_int_value()
        if n is None:
            return Symbol('False')

        n = abs(n)
        if sympy.isprime(n):
            return Symbol('True')
        else:
            return Symbol('False')

        # old variant using gmpy
        """count = 25
        while True:
            evaluation.check_stopped()
            result = n.is_prime(count)
            print result, count
            if result == 0:
                return Symbol('False')
            elif result == 2:
                return Symbol('True')
            count += 50"""


class CoprimeQ(Builtin):
    """
    Test whether two numbers are coprime by computing their greatest common divisor

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
     = False

    >> CoprimeQ[2, 3, 5]
     = True

    >> CoprimeQ[2, 4, 5]
     = False

    """
    attributes = ('Listable',)

    def apply(self, args, evaluation):
        'CoprimeQ[args__]'

        py_args = [arg.to_python() for arg in args.get_sequence()]
        if not all(isinstance(i, int) or isinstance(i, complex) for i in py_args):
            return Symbol('False')

        if all(sympy.gcd(n, m) == 1 for (n, m) in combinations(py_args, 2)):
            return Symbol('True')
        else:
            return Symbol('False')


class PrimePowerQ(Builtin):
    """
    Tests wheter a number is a prime power

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
        'PrimePowerQ[1]': 'False',
    }

    # TODO: GaussianIntegers option e.g. PrimePowerQ[5, GaussianIntegers -> True] (False)
    # TODO: Threading over lists e.g. PrimePowerQ[{1, 2 + I, 3, 4 - 2 I, 5, 6, 7 + 9 I, 8, 9}]
    # TODO: Gaussian rationals e.g. PrimePowerQ[2/125 - 11 I/125] (True)

    def apply(self, n, evaluation):
        'PrimePowerQ[n_]'
        n = n.get_int_value()
        if n is None:
            return Symbol('False')

        n = abs(n)
        if len(sympy.factorint(n)) == 1:
            return Symbol('True')
        else:
            return Symbol('False')


class PrimePi(Builtin):
    """
    <dl>
    <dt>'PrimePi[$x$]'
        <dd>gives the number of primes less than or equal to $x$
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
        'PrimePi[n_?NumericQ]'
        return from_python(sympy.ntheory.primepi(n.to_python(n_evaluation=evaluation)))


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
        'NextPrime[n_]': 'NextPrime[n, 1]',
    }

    def apply(self, n, k, evaluation):
        'NextPrime[n_?NumericQ, k_?IntegerQ]'
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


class RandomPrime(Builtin):
    """
    <dl>
    <dt>'RandomPrime[{$imin$, $imax}]'
        <dd>gives a random prime between $imin$ and $imax$.
    <dt>'RanomPrime[$imax$]
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
        'posdim': 'The dimensions parameter `1` is expected to be a positive integer or a list of positive integers.',
        'noprime': 'There are no primes in the specified interval.',
        'prmrng': 'First argument `1` is not a positive integer or a list of two positive integers.',
        'posint': 'The paramater `1` describing the interval is expected to be a positive integer.',
    }

    rules = {
        'RandomPrime[imax_?NotListQ]': 'RandomPrime[{1, imax}, 1]',
        'RandomPrime[int_?ListQ]': 'RandomPrime[int, 1]',
        'RandomPrime[imax_?ListQ, n_?ArrayQ]': 'ConstantArray[RandomPrime[imax, 1], n]',
        'RandomPrime[imax_?NotListQ, n_?ArrayQ]': 'ConstantArray[RandomPrime[{1, imax}, 1], n]',
    }

    # TODO: Use random state as in other randomised methods within mathics

    def apply(self, interval, n, evaluation):
        'RandomPrime[interval_?ListQ, n_]'

        if not isinstance(n, Integer):
            evaluation.message('RandomPrime', 'posdim', n)
            return
        py_n = n.to_python()

        py_int = interval.to_python()
        if not (isinstance(py_int, list) and len(py_int) == 2):
            evaluation.message('RandomPrime', 'prmrng', interval)

        imin, imax = min(py_int), max(py_int)
        if imin <= 0 or not isinstance(imin, int):
            evaluation.message('RandomPrime', 'posint', interval.leaves[0])
            return

        if imax <= 0 or not isinstance(imax, int):
            evaluation.message('RandomPrime', 'posint', interval.leaves[1])
            return

        try:
            if py_n == 1:
                return from_python(sympy.ntheory.randprime(imin, imax + 1))
            return from_python([sympy.ntheory.randprime(imin, imax + 1) for i in range(py_n)])
        except ValueError:
            evaluation.message('RandomPrime', 'noprime')
            return
