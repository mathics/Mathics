#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

import sympy

from mathics.builtin.base import Builtin, BinaryOperator, Test, SympyFunction
from mathics.core.expression import (Expression, Number, Integer, Rational,
                                     Real, Symbol, String)
from mathics.core.numbers import get_type, dps
from six.moves import range
from six.moves import zip


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

    operator = '==='
    precedence = 290

    def apply(self, lhs, rhs, evaluation):
        'lhs_ === rhs_'

        if lhs.same(rhs):
            return Symbol('True')
        else:
            return Symbol('False')


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

    operator = '=!='
    precedence = 290

    def apply(self, lhs, rhs, evaluation):
        'lhs_ =!= rhs_'

        if lhs.same(rhs):
            return Symbol('False')
        else:
            return Symbol('True')

operators = {
    'System`Less': (-1,),
    'System`LessEqual': (-1, 0),
    'System`Equal': (0,),
    'System`GreaterEqual': (0, 1),
    'System`Greater': (1,),
    'System`Unequal': (-1, 1),
}


class _InequalityOperator(BinaryOperator):
    precedence = 290
    grouping = 'None'

    def parse(self, args):
        # Parse multiple inequalities.
        # "a op b op c" -> op[a, b, c]
        # "a op1 b op2 c" -> Inequality[a, op1, b, op2, c]

        def inequality_leaves(expression):
            if expression.parenthesized:
                return [expression]
            name = expression.get_head().get_name()
            leaves = expression.get_leaves()
            if name == 'System`Inequality':
                return leaves
            elif name in operators:
                result = []
                for leaf in leaves[:-1]:
                    result.extend([leaf, Symbol(name)])
                result.extend(leaves[-1:])
                return result
            else:
                return [expression]

        left = args[0]
        right = args[2]
        name = self.get_name()

        left_leaves = inequality_leaves(left)
        right_leaves = inequality_leaves(right)
        leaves = left_leaves + [Symbol(name)] + right_leaves
        ops = set(leaves[1::2])
        if len(ops) == 1:
            return Expression(ops.pop(), *leaves[0::2])
        else:
            return Expression('Inequality', *leaves)

    def apply(self, items, evaluation):
        '%(name)s[items__]'

        items_sequence = items.get_sequence()
        all_numeric = all(item.is_numeric() and item.get_precision() is None
                          for item in items_sequence)

        # All expressions are numeric but exact and they are not all numbers,
        if all_numeric and any(not isinstance(item, Number)
                               for item in items_sequence):
            # so apply N and compare them.
            items = items_sequence
            n_items = []
            for item in items:
                if not isinstance(item, Number):
                    # TODO: use $MaxExtraPrecision insterad of hard-coded 50
                    n_expr = Expression('N', item, Integer(50))
                    item = n_expr.evaluate(evaluation)
                n_items.append(item)
            items = n_items
        else:
            items = items.numerify(evaluation).get_sequence()
        wanted = operators[self.get_name()]
        prev = None
        for item in items:
            if (item.get_real_value() is None and
                not item.has_form('DirectedInfinity', None)):   # nopep8
                return
            if prev is not None and do_cmp(prev, item) not in wanted:
                return Symbol('False')
            prev = item
        return Symbol('True')


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
        'ineq': ("Inequality called with `` arguments; the number of "
                 "arguments is expected to be an odd number >= 3."),
    }

    def apply(self, items, evaluation):
        'Inequality[items___]'

        items = items.numerify(evaluation).get_sequence()
        count = len(items)
        if count == 1:
            return Symbol('True')
        elif count % 2 == 0:
            evaluation.message('Inequality', 'ineq', count)
        elif count == 3:
            name = items[1].get_name()
            if name in operators:
                return Expression(name, items[0], items[2])
        else:
            groups = [Expression('Inequality', *items[index - 1:index + 2])
                      for index in range(1, count - 1, 2)]
            return Expression('And', *groups)


def numerify(vars, evaluation):
    return Expression('List', *vars).numerify(evaluation).leaves


def do_cmp(x1, x2):
    real1, real2 = x1.get_real_value(), x2.get_real_value()
    inf1 = inf2 = None
    if x1.has_form('DirectedInfinity', 1):
        inf1 = x1.leaves[0].get_int_value()
    if x2.has_form('DirectedInfinity', 1):
        inf2 = x2.leaves[0].get_int_value()

    if real1 is not None and get_type(real1) != 'f':
        real1 = sympy.Float(real1)
    if real2 is not None and get_type(real2) != 'f':
        real2 = sympy.Float(real2)
    # Bus error when not converting to mpf

    if real1 is not None and real2 is not None:
        if x1 == x2:
            return 0
        elif x1 < x2:
            return -1
        else:
            return 1
    elif inf1 is not None and inf2 is not None:
        return cmp(inf1, inf2)
    elif inf1 is not None and real2 is not None:
        return inf1
    elif real1 is not None and inf2 is not None:
        return -inf2
    else:
        return None


def do_compare(l1, l2):
    if l1.same(l2):
        return True
    elif isinstance(l1, String) and isinstance(l2, String):
        return False
    elif l1.to_sympy().is_number and l2.to_sympy().is_number:
        # assert min_prec(l1, l2) is None
        prec = 64  # TODO: Use $MaxExtraPrecision
        if l1.to_sympy().n(dps(prec)) == l2.to_sympy().n(dps(prec)):
            return True
        return False
    elif l1.has_form('List', None) and l2.has_form('List', None):
        if len(l1.leaves) != len(l2.leaves):
            return False
        for item1, item2 in zip(l1.leaves, l2.leaves):
            result = do_compare(item1, item2)
            if not result:
                return result
        return True
    else:
        return None


class Equal(_InequalityOperator, SympyFunction):
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

    >> 0.1 ^ 10000 == 0.1 ^ 10000 + 0.1 ^ 10016
     = False
    >> 0.1 ^ 10000 == 0.1 ^ 10000 + 0.1 ^ 10017
     = True

    ## TODO: Needs ^^ opperator

    ## Real numbers are considered equal if they only differ in their last seven binary digits
    ## #> 2^^1.000000000000000000000000000000000000000000000000000000000000 ==  2^^1.000000000000000000000000000000000000000000000000000001111111
    ##  = True
    ## 2^^1.000000000000000000000000000000000000000000000000000000000000 ==  2^^1.000000000000000000000000000000000000000000000000000010000000
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
    """
    operator = '=='

    sympy_name = 'Eq'

    def apply_other(self, x, y, evaluation):
        'Equal[x_?(!RealNumberQ[#]&), y_?(!RealNumberQ[#]&)]'

        x, y = numerify([x, y], evaluation)
        result = do_compare(x, y)
        if result is not None:
            if result:
                return Symbol('True')
            else:
                return Symbol('False')


class Unequal(_InequalityOperator, SympyFunction):
    """
    <dl>
    <dt>'Unequal[$x$, $y$]'
    <dt>'$x$ != $y$'
        <dd>yields 'False' if $x$ and $y$ are known to be equal, or
        'True' if $x$ and $y$ are known to be unequal.
    <dt>'$lhs$ == $rhs$'
        <dd>represents the inequality $lhs$ ≠ $rhs$.
    </dl>

    >> 1 != 1.
     = False

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
    """

    operator = '!='

    sympy_name = 'Ne'

    def apply_other(self, x, y, evaluation):
        'Unequal[x_?(!RealNumberQ[#]&), y_?(!RealNumberQ[#]&)]'

        x, y = numerify([x, y], evaluation)
        result = do_compare(x, y)
        if result is not None:
            if result:
                return Symbol('False')
            else:
                return Symbol('True')


class Less(_InequalityOperator, SympyFunction):
    """
    <dl>
    <dt>'Less[$x$, $y$]'
    <dt>'$x$ < $y$'
        <dd>yields 'True' if $x$ is known to be less than $y$.
    <dt>'$lhs$ < $rhs$'
        <dd>represents the inequality $lhs$ < $rhs$.
    </dl>
    """
    operator = '<'
    sympy_name = 'StrictLessThan'


class LessEqual(_InequalityOperator, SympyFunction):
    """
    <dl>
    <dt>'LessEqual[$x$, $y$]'
    <dt>'$x$ <= $y$'
        <dd>yields 'True' if $x$ is known to be less than or equal to $y$.
    <dt>'$lhs$ <= $rhs$'
        <dd>represents the inequality $lhs$ ≤ $rhs$.
    </dl>
    """
    operator = '<='
    sympy_name = 'LessThan'


class Greater(_InequalityOperator, SympyFunction):
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

    operator = '>'
    sympy_name = 'StrictGreaterThan'


class GreaterEqual(_InequalityOperator, SympyFunction):
    """
    <dl>
    <dt>'GreaterEqual[$x$, $y$]'
    <dt>'$x$ >= $y$'
        <dd>yields 'True' if $x$ is known to be greater than or equal
        to $y$.
    <dt>'$lhs$ >= $rhs$'
        <dd>represents the inequality $lhs$ ≥ $rhs$.
    </dl>
    """
    operator = '>='
    sympy_name = 'GreaterThan'


class Positive(Test):
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
    """
    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real)) and expr.value > 0


class Negative(Test):
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
    >> Negative[a+b]
     = False
    """

    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real)) and expr.value < 0


class NonNegative(Test):
    """
    <dl>
    <dt>'NonNegative[$x$]'
        <dd>returns 'True' if $x$ is a positive real number or zero.
    </dl>

    >> {Positive[0], NonNegative[0]}
     = {False, True}
    """
    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real)) and expr.value >= 0


class NonPositive(Test):
    """
    <dl>
    <dt>'NonNegative[$x$]'
        <dd>returns 'True' if $x$ is a negative real number or zero.
    </dl>
    """
    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real)) and expr.value <= 0


def expr_max(items):
    result = Expression('DirectedInfinity', -1)
    for item in items:
        c = do_cmp(item, result)
        if c > 0:
            result = item
    return result


def expr_min(items):
    result = Expression('DirectedInfinity', 1)
    for item in items:
        c = do_cmp(item, result)
        if c < 0:
            result = item
    return result


class _MinMax(Builtin):

    attributes = ('Flat', 'NumericFunction', 'OneIdentity', 'Orderless')

    def apply(self, items, evaluation):
        '%(name)s[items___]'

        items = items.flatten(Symbol('List')).get_sequence()
        results = []
        best = None

        for item in items:
            if item.has_form('List', None):
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
                elif (self.sense == 1 and c > 0) or (
                        self.sense == -1 and c < 0):
                    results.remove(best)
                    best = leaf
                    results.append(leaf)

        if not results:
            return Expression('DirectedInfinity', -self.sense)
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

    Maximum of a series of numbers:
    >> Max[4, -8, 1]
     = 4

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

    Minimum of a series of numbers:
    >> Min[4, -8, 1]
     = -8

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
