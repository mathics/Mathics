# -*- coding: utf-8 -*-


from mathics.version import __version__  # noqa used in loading to check consistency.

import itertools
from typing import Optional, Union

import sympy

from mathics.builtin.base import (
    BinaryOperator,
    Builtin,
    SympyFunction,
)

from mathics.builtin.constants import mp_convert_constant

from mathics.core.expression import (
    Complex,
    Expression,
    Integer,
    Number,
    Real,
    String,
    Symbol,
    SymbolFalse,
    SymbolTrue,
)
from mathics.core.numbers import dps


def cmp(a, b) -> int:
    "Returns 0 if a == b, -1 if a < b and 1 if a > b"
    return (a > b) - (a < b)


def is_number(sympy_value) -> bool:
    return hasattr(sympy_value, "is_number") or isinstance(sympy_value, sympy.Float)


class SameQ(BinaryOperator):
    """
    <dl>
    <dt>'SameQ[$x$, $y$]'
    <dt>'$x$ === $y$'
        <dd>returns 'True' if $x$ and $y$ are structurally identical.
    </dl>

    Any object is the same as itself:
    >> a===a
     = True

    Unlike 'Equal', 'SameQ' only yields 'True' if $x$ and $y$ have the
    same type:
    >> {1==1., 1===1.}
     = {True, False}
    """

    operator = "==="
    precedence = 290

    def apply(self, lhs, rhs, evaluation):
        "lhs_ === rhs_"

        if lhs.same(rhs):
            return SymbolTrue
        else:
            return SymbolFalse


class UnsameQ(BinaryOperator):
    """
    <dl>
    <dt>'UnsameQ[$x$, $y$]'
    <dt>'$x$ =!= $y$'
        <dd>returns 'True' if $x$ and $y$ are not structurally identical.
    </dl>

    >> a=!=a
     = False
    >> 1=!=1.
     = True
    """

    operator = "=!="
    precedence = 290

    def apply(self, lhs, rhs, evaluation):
        "lhs_ =!= rhs_"

        if lhs.same(rhs):
            return SymbolFalse
        else:
            return SymbolTrue


class TrueQ(Builtin):
    """
    <dl>
    <dt>'TrueQ[$expr$]'
        <dd>returns 'True' if and only if $expr$ is 'True'.
    </dl>

    >> TrueQ[True]
     = True

    >> TrueQ[False]
     = False

    >> TrueQ[a]
     = False
    """

    rules = {
        "TrueQ[expr_]": "If[expr, True, False, False]",
    }


class BooleanQ(Builtin):
    """
    <dl>
    <dt>'BooleanQ[$expr$]'
        <dd>returns 'True' if $expr$ is either 'True' or 'False'.
    </dl>

    >> BooleanQ[True]
     = True

    >> BooleanQ[False]
     = True

    >> BooleanQ[a]
     = False

    >> BooleanQ[1 < 2]
     = True

    #> BooleanQ["string"]
     = False

    #> BooleanQ[Together[x/y + y/x]]
     = False
    """

    rules = {
        "BooleanQ[expr_]": "If[expr, True, True, False]",
    }


class ValueQ(Builtin):
    """
    <dl>
    <dt>'ValueQ[$expr$]'
        <dd>returns 'True' if and only if $expr$ is defined.
    </dl>

    >> ValueQ[x]
     = False
    >> x = 1;
    >> ValueQ[x]
     = True

    #> ValueQ[True]
     = False
    """

    attributes = ("HoldFirst",)

    def apply(self, expr, evaluation):
        "ValueQ[expr_]"
        evaluated_expr = expr.evaluate(evaluation)
        if expr.same(evaluated_expr):
            return SymbolFalse
        return SymbolTrue


operators = {
    "System`Less": (-1,),
    "System`LessEqual": (-1, 0),
    "System`Equal": (0,),
    "System`GreaterEqual": (0, 1),
    "System`Greater": (1,),
    "System`Unequal": (-1, 1),
}


class _InequalityOperator(BinaryOperator):
    precedence = 290
    grouping = "NonAssociative"

    @staticmethod
    def numerify_args(items, evaluation):
        items_sequence = items.get_sequence()
        all_numeric = all(
            item.is_numeric() and item.get_precision() is None
            for item in items_sequence
        )

        # All expressions are numeric but exact and they are not all numbers,
        if all_numeric and any(not isinstance(item, Number) for item in items_sequence):
            # so apply N and compare them.
            items = items_sequence
            n_items = []
            for item in items:
                if not isinstance(item, Number):
                    # TODO: use $MaxExtraPrecision insterad of hard-coded 50
                    n_expr = Expression("N", item, Integer(50))
                    item = n_expr.evaluate(evaluation)
                n_items.append(item)
            items = n_items
        else:
            items = items.numerify(evaluation).get_sequence()
        return items


# Imperical number that seems to work.
# We have to be able to match mpmath values with sympy values
COMPARE_PREC = 50


class _EqualityOperator(_InequalityOperator):
    "Compares all pairs e.g. a == b == c compares a == b, b == c, and a == c."

    def do_compare(self, l1, l2) -> Union[bool, None]:
        if l1.same(l2):
            return True
        elif l1 == SymbolTrue and l2 == SymbolFalse:
            return False
        elif l1 == SymbolFalse and l2 == SymbolTrue:
            return False
        elif isinstance(l1, String) and isinstance(l2, String):
            return False
        elif l1.has_form("List", None) and l2.has_form("List", None):
            if len(l1.leaves) != len(l2.leaves):
                return False
            for item1, item2 in zip(l1.leaves, l2.leaves):
                result = self.do_compare(item1, item2)
                if not result:
                    return result
            return True

        # Use Mathics' built-in comparisons for Real and Integer. These use
        # WL's interpretation of Equal[] which allows for slop in Reals
        # in the least significant digit of precision, while for Integers, comparison
        # has to be exact.

        if (isinstance(l1, Real) and isinstance(l2, Real)) or (
            isinstance(l1, Integer) and isinstance(l2, Integer)
        ):
            return l1 == l2

        # For everything else, use sympy.

        l1_sympy = l1.to_sympy(evaluate=True, prec=COMPARE_PREC)
        l2_sympy = l2.to_sympy(evaluate=True, prec=COMPARE_PREC)

        if l1_sympy is None or l2_sympy is None:
            return None

        if not is_number(l1_sympy):
            l1_sympy = mp_convert_constant(l1_sympy, prec=COMPARE_PREC)
        if not is_number(l2_sympy):
            l2_sympy = mp_convert_constant(l2_sympy, prec=COMPARE_PREC)

        if l1_sympy.is_number and l2_sympy.is_number:
            # assert min_prec(l1, l2) is None
            prec = COMPARE_PREC  # TODO: Use $MaxExtraPrecision
            if l1_sympy.n(dps(prec)) == l2_sympy.n(dps(prec)):
                return True
            return False
        else:
            return None

    def apply(self, items, evaluation):
        "%(name)s[items___]"
        items_sequence = items.get_sequence()
        n = len(items_sequence)
        if n <= 1:
            return SymbolTrue
        is_exact_vals = [
            Expression("ExactNumberQ", arg).evaluate(evaluation)
            for arg in items_sequence
        ]
        if all(val == SymbolTrue for val in is_exact_vals):
            return self.apply_other(items, evaluation)
        args = self.numerify_args(items, evaluation)
        wanted = operators[self.get_name()]
        for x, y in itertools.combinations(args, 2):
            if isinstance(x, String) or isinstance(y, String):
                if not (isinstance(x, String) and isinstance(y, String)):
                    c = 1
                else:
                    c = cmp(x.get_string_value(), y.get_string_value())
            else:
                c = do_cmp(x, y)
            if c is None:
                return
            elif c not in wanted:
                return SymbolFalse
            assert c in wanted
        return SymbolTrue

    def apply_other(self, args, evaluation):
        "%(name)s[args___?(!ExactNumberQ[#]&)]"
        args = args.get_sequence()
        for x, y in itertools.combinations(args, 2):
            c = self.do_compare(x, y)
            if c is None:
                return
            if self._op(c) is False:
                return SymbolFalse
        return SymbolTrue


class _ComparisonOperator(_InequalityOperator):
    "Compares arguments in a chain e.g. a < b < c compares a < b and b < c."

    def apply(self, items, evaluation):
        "%(name)s[items___]"
        items_sequence = items.get_sequence()
        if len(items_sequence) <= 1:
            return SymbolTrue
        items = self.numerify_args(items, evaluation)
        wanted = operators[self.get_name()]
        for i in range(len(items) - 1):
            x = items[i]
            y = items[i + 1]
            c = do_cmp(x, y)
            if c is None:
                return
            elif c not in wanted:
                return SymbolFalse
            assert c in wanted
        return SymbolTrue


class Inequality(Builtin):
    """
    <dl>
    <dt>'Inequality'
        <dd>is the head of expressions involving different inequality
        operators (at least temporarily). Thus, it is possible to
        write chains of inequalities.
    </dl>

    >> a < b <= c
     = a < b && b <= c
    >> Inequality[a, Greater, b, LessEqual, c]
     = a > b && b <= c
    >> 1 < 2 <= 3
     = True
    >> 1 < 2 > 0
     = True
    >> 1 < 2 < -1
     = False
    """

    messages = {
        "ineq": (
            "Inequality called with `` arguments; the number of "
            "arguments is expected to be an odd number >= 3."
        ),
    }

    def apply(self, items, evaluation):
        "Inequality[items___]"

        items = items.numerify(evaluation).get_sequence()
        count = len(items)
        if count == 1:
            return SymbolTrue
        elif count % 2 == 0:
            evaluation.message("Inequality", "ineq", count)
        elif count == 3:
            name = items[1].get_name()
            if name in operators:
                return Expression(name, items[0], items[2])
        else:
            groups = [
                Expression("Inequality", *items[index - 1 : index + 2])
                for index in range(1, count - 1, 2)
            ]
            return Expression("And", *groups)


def do_cmp(x1, x2) -> Optional[int]:

    # don't attempt to compare complex numbers
    for x in (x1, x2):
        # TODO: Send message General::nord
        if isinstance(x, Complex) or (
            x.has_form("DirectedInfinity", 1) and isinstance(x.leaves[0], Complex)
        ):
            return None

    s1 = x1.to_sympy()
    s2 = x2.to_sympy()

    # Use internal comparisons only for Real which is uses
    # WL's interpretation of equal (which allows for slop
    # in the least significant digit of precision), and use
    # use sympy for everything else
    if s1.is_Float and s2.is_Float:
        if x1 == x2:
            return 0
        if x1 < x2:
            return -1
        return 1

    # we don't want to compare anything that
    # cannot be represented as a numeric value
    if s1.is_number and s2.is_number:
        if s1 == s2:
            return 0
        if s1 < s2:
            return -1
        return 1

    return None


class SympyComparison(SympyFunction):
    def to_sympy(self, expr, **kwargs):
        to_sympy = super(SympyComparison, self).to_sympy
        if len(expr.leaves) > 2:

            def pairs(items):
                yield Expression(expr.get_head_name(), *items[:2])
                items = items[1:]
                while len(items) >= 2:
                    yield Expression(expr.get_head_name(), *items[:2])
                    items = items[1:]

            return sympy.And(*[to_sympy(p, **kwargs) for p in pairs(expr.leaves)])
        return to_sympy(expr, **kwargs)


class Equal(_EqualityOperator, SympyComparison):
    """
    <dl>
    <dt>'Equal[$x$, $y$]'
      <dt>'$x$ == $y$'
      <dd>yields 'True' if $x$ and $y$ are known to be equal, or
        'False' if $x$ and $y$ are known to be unequal.
    <dt>'$lhs$ == $rhs$'
        <dd>represents the equation $lhs$ = $rhs$.
    </dl>

    >> a==a
     = True
    >> a==b
     = a == b
    >> 1==1.
     = True

    Strings are allowed:

    >> Equal["11", "11"]
     = True

    >> Equal["121", "11"]
     = False

    Comparision to mismatched types is False:

    >> Equal[11, "11"]
     = False

    Lists are compared based on their elements:
    >> {{1}, {2}} == {{1}, {2}}
     = True
    >> {1, 2} == {1, 2, 3}
     = False

    Real values are considered equal if they only differ in their last digits:
    >> 0.739085133215160642 == 0.739085133215160641
     = True
    >> 0.73908513321516064200000000 == 0.73908513321516064100000000
     = False

    ## TODO Needs power precision tracking
    ## >> 0.1 ^ 10000 == 0.1 ^ 10000 + 0.1 ^ 10012
    ##  = False

    #> 0.1 ^ 10000 == 0.1 ^ 10000 + 0.1 ^ 10013
      = True

    #> 0.1111111111111111 ==  0.1111111111111126
     = True
    #> 0.1111111111111111 ==  0.1111111111111127
     = False

    ## TODO needs better precision tracking
    ## #> 2^^1.000000000000000000000000000000000000000000000000000000000000 ==  2^^1.000000000000000000000000000000000000000000000000000001111111
    ##  = True
    ## #> 2^^1.000000000000000000000000000000000000000000000000000000000000 ==  2^^1.000000000000000000000000000000000000000000000000000010000000
    ##  = False

    Comparisons are done using the lower precision:
    >> N[E, 100] == N[E, 150]
     = True

    Symbolic constants are compared numerically:
    >> E > 1
     = True
    >> Pi == 3.14
     = False

    #> Pi ^ E == E ^ Pi
     = False

    #> N[E, 3] == N[E]
     = True

    #> {1, 2, 3} < {1, 2, 3}
     = {1, 2, 3} < {1, 2, 3}

    #> E == N[E]
     = True

    ## Issue260
    #> {Equal[Equal[0, 0], True], Equal[0, 0] == True}
     = {True, True}
    #> {Mod[6, 2] == 0, Mod[6, 4] == 0, (Mod[6, 2] == 0) == (Mod[6, 4] == 0), (Mod[6, 2] == 0) != (Mod[6, 4] == 0)}
     = {True, False, False, True}

    #> a == a == a
     = True

    #> {Equal[], Equal[x], Equal[1]}
     = {True, True, True}
    """

    operator = "=="
    grouping = "None"
    sympy_name = "Eq"

    @staticmethod
    def _op(x):
        return x


class Unequal(_EqualityOperator, SympyComparison):
    """
    <dl>
    <dt>'Unequal[$x$, $y$]'
    <dt>'$x$ != $y$'
        <dd>yields 'False' if $x$ and $y$ are known to be equal, or
        'True' if $x$ and $y$ are known to be unequal.
    <dt>'$lhs$ == $rhs$'
        <dd>represents the inequality $lhs$   $rhs$.
    </dl>

    >> 1 != 1.
     = False

    Strings are allowed:
    Unequal["11", "11"]
     = False

    Equal["121", "11"]
     = True

    Comparision to mismatched types is True:
    Equal[11, "11"]
     = True

    Lists are compared based on their elements:
    >> {1} != {2}
     = True
    >> {1, 2} != {1, 2}
     = False
    >> {a} != {a}
     = False
    >> "a" != "b"
     = True
    >> "a" != "a"
     = False

    #> Pi != N[Pi]
     = False

    #> a_ != b_
     = a_ != b_

    #> a != a != a
     = False
    #> "abc" != "def" != "abc"
     = False

    ## Reproduce strange MMA behaviour
    #> a != a != b
     = False
    #> a != b != a
     = a != b != a

    #> {Unequal[], Unequal[x], Unequal[1]}
     = {True, True, True}
    """

    operator = "!="
    sympy_name = "Ne"

    @staticmethod
    def _op(x):
        return not x


class Less(_ComparisonOperator, SympyComparison):
    """
    <dl>
    <dt>'Less[$x$, $y$]'
    <dt>'$x$ < $y$'
        <dd>yields 'True' if $x$ is known to be less than $y$.
    <dt>'$lhs$ < $rhs$'
        <dd>represents the inequality $lhs$ < $rhs$.
    </dl>

    #> {Less[], Less[x], Less[1]}
     = {True, True, True}

    """

    operator = "<"
    sympy_name = "StrictLessThan"


class LessEqual(_ComparisonOperator, SympyComparison):
    """
    <dl>
    <dt>'LessEqual[$x$, $y$]'
    <dt>'$x$ <= $y$'
        <dd>yields 'True' if $x$ is known to be less than or equal to $y$.
    <dt>'$lhs$ <= $rhs$'
        <dd>represents the inequality $lhs$   $rhs$.
    </dl>
    """

    operator = "<="
    sympy_name = "LessThan"


class Greater(_ComparisonOperator, SympyComparison):
    """
    <dl>
    <dt>'Greater[$x$, $y$]'
    <dt>'$x$ > $y$'
        <dd>yields 'True' if $x$ is known to be greater than $y$.
    <dt>'$lhs$ > $rhs$'
        <dd>represents the inequality $lhs$ > $rhs$.
    </dl>
    >> a > b > c //FullForm
     = Greater[a, b, c]
    >> Greater[3, 2, 1]
     = True
    """

    operator = ">"
    sympy_name = "StrictGreaterThan"


class GreaterEqual(_ComparisonOperator, SympyComparison):
    """
    <dl>
    <dt>'GreaterEqual[$x$, $y$]'
    <dt>'$x$ >= $y$'
        <dd>yields 'True' if $x$ is known to be greater than or equal
        to $y$.
    <dt>'$lhs$ >= $rhs$'
        <dd>represents the inequality $lhs$   $rhs$.
    </dl>
    """

    operator = ">="
    sympy_name = "GreaterThan"


class Positive(Builtin):
    """
    <dl>
    <dt>'Positive[$x$]'
        <dd>returns 'True' if $x$ is a positive real number.
    </dl>

    >> Positive[1]
     = True

    'Positive' returns 'False' if $x$ is zero or a complex number:
    >> Positive[0]
     = False
    >> Positive[1 + 2 I]
     = False

    #> Positive[Pi]
     = True
    #> Positive[x]
     = Positive[x]
    #> Positive[Sin[{11, 14}]]
     = {False, True}
    """

    attributes = ("Listable",)

    rules = {
        "Positive[x_?NumericQ]": "If[x > 0, True, False, False]",
    }


class Negative(Builtin):
    """
    <dl>
    <dt>'Negative[$x$]'
        <dd>returns 'True' if $x$ is a negative real number.
    </dl>
    >> Negative[0]
     = False
    >> Negative[-3]
     = True
    >> Negative[10/7]
     = False
    >> Negative[1+2I]
     = False
    >> Negative[a + b]
     = Negative[a + b]
    #> Negative[-E]
     = True
    #> Negative[Sin[{11, 14}]]
     = {True, False}
    """

    attributes = ("Listable",)

    rules = {
        "Negative[x_?NumericQ]": "If[x < 0, True, False, False]",
    }


class NonNegative(Builtin):
    """
    <dl>
    <dt>'NonNegative[$x$]'
        <dd>returns 'True' if $x$ is a positive real number or zero.
    </dl>

    >> {Positive[0], NonNegative[0]}
     = {False, True}
    """

    attributes = ("Listable",)

    rules = {
        "NonNegative[x_?NumericQ]": "If[x >= 0, True, False, False]",
    }


class NonPositive(Builtin):
    """
    <dl>
    <dt>'NonPositive[$x$]'
        <dd>returns 'True' if $x$ is a negative real number or zero.
    </dl>

    >> {Negative[0], NonPositive[0]}
     = {False, True}
    """

    attributes = ("Listable",)

    rules = {
        "NonPositive[x_?NumericQ]": "If[x <= 0, True, False, False]",
    }


def expr_max(items):
    result = Expression("DirectedInfinity", -1)
    for item in items:
        c = do_cmp(item, result)
        if c > 0:
            result = item
    return result


def expr_min(items):
    result = Expression("DirectedInfinity", 1)
    for item in items:
        c = do_cmp(item, result)
        if c < 0:
            result = item
    return result


class _MinMax(Builtin):

    attributes = ("Flat", "NumericFunction", "OneIdentity", "Orderless")

    def apply(self, items, evaluation):
        "%(name)s[items___]"

        items = items.flatten(Symbol("List")).get_sequence()
        results = []
        best = None

        for item in items:
            if item.has_form("List", None):
                leaves = item.leaves
            else:
                leaves = [item]
            for leaf in leaves:
                if best is None:
                    best = leaf
                    results.append(best)
                    continue
                c = do_cmp(leaf, best)
                if c is None:
                    results.append(leaf)
                elif (self.sense == 1 and c > 0) or (self.sense == -1 and c < 0):
                    results.remove(best)
                    best = leaf
                    results.append(leaf)

        if not results:
            return Expression("DirectedInfinity", -self.sense)
        if len(results) == 1:
            return results.pop()
        if len(results) < len(items):
            # Some simplification was possible because we discarded
            # elements.
            return Expression(self.get_name(), *results)
        # If we get here, no simplification was possible.
        return None


class Max(_MinMax):
    """
    <dl>
    <dt>'Max[$e_1$, $e_2$, ..., $e_i$]'
        <dd>returns the expression with the greatest value among the $e_i$.
    </dl>

    Maximum of a series of values:
    >> Max[4, -8, 1]
     = 4
    >> Max[E - Pi, Pi, E + Pi, 2 E]
     = E + Pi

    'Max' flattens lists in its arguments:
    >> Max[{1,2},3,{-3,3.5,-Infinity},{{1/2}}]
     = 3.5

    'Max' with symbolic arguments remains in symbolic form:
    >> Max[x, y]
     = Max[x, y]
    >> Max[5, x, -3, y, 40]
     = Max[40, x, y]

    With no arguments, 'Max' gives '-Infinity':
    >> Max[]
     = -Infinity

    #> Max[x]
     = x
    """

    sense = 1


class Min(_MinMax):
    """
    <dl>
    <dt>'Min[$e_1$, $e_2$, ..., $e_i$]'
        <dd>returns the expression with the lowest value among the $e_i$.
    </dl>

    Minimum of a series of values:
    >> Min[4, -8, 1]
     = -8
    >> Min[E - Pi, Pi, E + Pi, 2 E]
     = E - Pi

    'Min' flattens lists in its arguments:
    >> Min[{1,2},3,{-3,3.5,-Infinity},{{1/2}}]
     = -Infinity

    'Min' with symbolic arguments remains in symbolic form:
    >> Min[x, y]
     = Min[x, y]
    >> Min[5, x, -3, y, 40]
     = Min[-3, x, y]

    With no arguments, 'Min' gives 'Infinity':
    >> Min[]
     = Infinity

    #> Min[x]
     = x
    """

    sense = -1
