# -*- coding: utf-8 -*-
"""
Recurrence and Sum Functions

A recurrence relation is an equation that recursively defines a sequence or multidimensional array of values, once one or more initial terms are given; each further term of the sequence or array is defined as a function of the preceding terms.
"""


from sympy.functions.combinatorial.numbers import stirling
from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin
from mathics.core.expression import Integer
from mathics.builtin.arithmetic import _MPMathFunction


class Fibonacci(_MPMathFunction):
    """
    <dl>
      <dt>'Fibonacci[$n$]'
      <dd>computes the $n$th Fibonacci number.
    </dl>

    >> Fibonacci[0]
     = 0
    >> Fibonacci[1]
     = 1
    >> Fibonacci[10]
     = 55
    >> Fibonacci[200]
     = 280571172992510140037611932413038677189525
    """

    nargs = 1
    attributes = ("Listable", "NumericFunction", "ReadProtected")
    sympy_name = "fibonacci"
    mpmath_name = "fibonacci"


class HarmonicNumber(_MPMathFunction):
    """
    <dl>
    <dt>'HarmonicNumber[n]'
      <dd>returns the $n$th harmonic number.
    </dl>

    >> Table[HarmonicNumber[n], {n, 8}]
     = {1, 3 / 2, 11 / 6, 25 / 12, 137 / 60, 49 / 20, 363 / 140, 761 / 280}

    >> HarmonicNumber[3.8]
     = 2.03806

    #> HarmonicNumber[-1.5]
     = 0.613706
    """

    rules = {
        "HarmonicNumber[-1]": "ComplexInfinity",
    }

    sympy_name = "harmonic"
    mpmath_name = "harmonic"


# Note: WL allows StirlingS1[{2, 4, 6}, 2], but we don't (yet).
class StirlingS1(Builtin):
    """
    <dl>
      <dt>'StirlingS1[$n$, $m$]'
      <dd>gives the Stirling number of the first kind $ _n^m$.
    </dl>

    Integer mathematical function, suitable for both symbolic and numerical manipulation.
    gives the number of permutations of $n$ elements that contain exactly $m$ cycles.

    >> StirlingS1[50, 1]
    = -608281864034267560872252163321295376887552831379210240000000000
    """

    attributes = ("Listable", "Protected")

    nargs = 2
    sympy_name = "functions.combinatorial.stirling"
    mpmath_name = "stirling1"

    def apply(self, n, m, evaluation):
        "%(name)s[n_Integer, m_Integer]"
        n_value = n.get_int_value()
        m_value = m.get_int_value()
        return Integer(stirling(n_value, m_value, kind=1, signed=True))


class StirlingS2(Builtin):
    """
      <dl>
      <dt>'StirlingS2[$n$, $m$]'
      <dd>gives the Stirling number of the second kind  _n^m.
    </dl>

    returns the number of ways of partitioning a set of $n$ elements into $m$ non empty subsets.

    >> Table[StirlingS2[10, m], {m, 10}]
    = {1, 511, 9330, 34105, 42525, 22827, 5880, 750, 45, 1}
    """

    attributes = ("Listable", "Protected")

    sympy_name = "functions.combinatorial.numbers.stirling"
    mpmath_name = "stirling2"
    nargs = 2

    def apply(self, m, n, evaluation):
        "%(name)s[n_Integer, m_Integer]"
        n_value = n.get_int_value()
        m_value = m.get_int_value()
        return Integer(stirling(n_value, m_value, kind=2))
