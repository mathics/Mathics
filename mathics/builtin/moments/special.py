# -*- coding: utf-8 -*-

"""
Special Moments
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin

from mathics.builtin.lists import _Rectangular, _NotRectangularException

from mathics.core.expression import Expression


class Correlation(Builtin):
    """
    <dl>
      <dt>'Correlation[$a$, $b$]'
      <dd>computes Pearson's correlation of two equal-sized vectors $a$ and $b$.
    </dl>

    An example from Wikipedia:

    >> Correlation[{10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5}, {8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68}]
     = 0.816421
    """

    messages = {
        "shlen": "`` must contain at least two elements.",
        "vctmat": "`1` and `2` need to be of equal length.",
    }

    def apply(self, a, b, evaluation):
        "Correlation[a_List, b_List]"

        if len(a.leaves) != len(b.leaves):
            evaluation.message("Correlation", "vctmat", a, b)
        elif len(a.leaves) < 2:
            evaluation.message("Correlation", "shlen", a)
        elif len(b.leaves) < 2:
            evaluation.message("Correlation", "shlen", b)
        else:
            da = Expression("StandardDeviation", a)
            db = Expression("StandardDeviation", b)
            return Expression(
                "Divide", Expression("Covariance", a, b), Expression("Times", da, db)
            )


class Covariance(Builtin):
    """
    <dl>
    <dt>'Covariance[$a$, $b$]'
      <dd>computes the covariance between the equal-sized vectors $a$ and $b$.
    </dl>

    >> Covariance[{0.2, 0.3, 0.1}, {0.3, 0.3, -0.2}]
     = 0.025
    """

    messages = {
        "shlen": "`` must contain at least two elements.",
        "vctmat": "`1` and `2` need to be of equal length.",
    }

    def apply(self, a, b, evaluation):
        "Covariance[a_List, b_List]"

        if len(a.leaves) != len(b.leaves):
            evaluation.message("Covariance", "vctmat", a, b)
        elif len(a.leaves) < 2:
            evaluation.message("Covariance", "shlen", a)
        elif len(b.leaves) < 2:
            evaluation.message("Covariance", "shlen", b)
        else:
            ma = Expression("Subtract", a, Expression("Mean", a))
            mb = Expression("Subtract", b, Expression("Mean", b))
            return Expression(
                "Divide",
                Expression("Dot", ma, Expression("Conjugate", mb)),
                len(a.leaves) - 1,
            )


class Kurtosis(Builtin):  # see https://en.wikipedia.org/wiki/Kurtosis
    """
    <dl>
      <dt>'Kurtosis[$list$]'
      <dd>gives the Pearson measure of kurtosis for $list$ (a measure of existing outliers).
    </dl>

    >> Kurtosis[{1.1, 1.2, 1.4, 2.1, 2.4}]
     = 1.42098
    """

    rules = {
        "Kurtosis[list_List]": "CentralMoment[list, 4] / (CentralMoment[list, 2] ^ 2)",
    }


class Skewness(Builtin):  # see https://en.wikipedia.org/wiki/Skewness
    """
    <dl>
    <dt>'Skewness[$list$]'
      <dd>gives Pearson's moment coefficient of skewness for $list$ (a measure for estimating
      the symmetry of a distribution).
    </dl>

    >> Skewness[{1.1, 1.2, 1.4, 2.1, 2.4}]
     = 0.407041
    """

    rules = {
        "Skewness[list_List]": "CentralMoment[list, 3] / (CentralMoment[list, 2] ^ (3 / 2))",
    }


class StandardDeviation(_Rectangular):
    """
    <dl>
      <dt>'StandardDeviation[$list$]'
      <dd>computes the standard deviation of $list. $list$ may consist of numerical values or symbols. Numerical values may be real or complex.

      StandardDeviation[{{$a1$, $a2$, ...}, {$b1$, $b2$, ...}, ...}] will yield
      {StandardDeviation[{$a1$, $b1$, ...}, StandardDeviation[{$a2$, $b2$, ...}], ...}.
    </dl>

    >> StandardDeviation[{1, 2, 3}]
     = 1

    >> StandardDeviation[{7, -5, 101, 100}]
     = Sqrt[13297] / 2

    >> StandardDeviation[{a, a}]
     = 0

    >> StandardDeviation[{{1, 10}, {-1, 20}}]
     = {Sqrt[2], 5 Sqrt[2]}
    """

    messages = {
        "shlen": "`` must contain at least two elements.",
        "rectt": "Expected a rectangular array at position 1 in ``.",
    }

    def apply(self, l, evaluation):
        "StandardDeviation[l_List]"
        if len(l.leaves) <= 1:
            evaluation.message("StandardDeviation", "shlen", l)
        elif all(leaf.get_head_name() == "System`List" for leaf in l.leaves):
            try:
                return self.rect(l)
            except _NotRectangularException:
                evaluation.message(
                    "StandardDeviation", "rectt", Expression("StandardDeviation", l)
                )
        else:
            return Expression("Sqrt", Expression("Variance", l))


class Variance(_Rectangular):
    """
    <dl>
      <dt>'Variance[$list$]'
      <dd>computes the variance of $list. $list$ may consist of numerical values or symbols. Numerical values may be real or complex.

      Variance[{{$a1$, $a2$, ...}, {$b1$, $b2$, ...}, ...}] will yield {Variance[{$a1$, $b1$, ...}, Variance[{$a2$, $b2$, ...}], ...}.
    </dl>

    >> Variance[{1, 2, 3}]
     = 1

    >> Variance[{7, -5, 101, 3}]
     = 7475 / 3

    >> Variance[{1 + 2I, 3 - 10I}]
     = 74

    >> Variance[{a, a}]
     = 0

    >> Variance[{{1, 3, 5}, {4, 10, 100}}]
     = {9 / 2, 49 / 2, 9025 / 2}
    """

    messages = {
        "shlen": "`` must contain at least two elements.",
        "rectt": "Expected a rectangular array at position 1 in ``.",
    }

    # for the general formulation of real and complex variance below, see for example
    # https://en.wikipedia.org/wiki/Variance#Generalizations

    def apply(self, l, evaluation):
        "Variance[l_List]"
        if len(l.leaves) <= 1:
            evaluation.message("Variance", "shlen", l)
        elif all(leaf.get_head_name() == "System`List" for leaf in l.leaves):
            try:
                return self.rect(l)
            except _NotRectangularException:
                evaluation.message("Variance", "rectt", Expression("Variance", l))
        else:
            d = Expression("Subtract", l, Expression("Mean", l))
            return Expression(
                "Divide",
                Expression("Dot", d, Expression("Conjugate", d)),
                len(l.leaves) - 1,
            )
