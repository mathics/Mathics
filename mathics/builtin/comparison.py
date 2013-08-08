# -*- coding: utf8 -*-

import sympy

from mathics.builtin.base import Builtin, BinaryOperator, Test
from mathics.core.expression import (Expression, Number, Integer, Rational,
                                     Real, Symbol, String)
from mathics.core.numbers import get_type, dps


class SameQ(BinaryOperator):
    """
    >> a===a
     = True
    >> 1===1
     = True
    >> 1===1.
     = False
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
    'Less': (-1,),
    'LessEqual': (-1, 0),
    'Equal': (0,),
    'GreaterEqual': (0, 1),
    'Greater': (1,),
    'Unequal': (-1, 1),
}


class _InequalityOperator(BinaryOperator):
    precedence = 290
    grouping = 'NonAssociative'

    def parse(self, args):
        names = operators.keys()

        def inequality_leaves(expression):
            if expression.parenthesized:
                return [expression]
            name = expression.get_head().get_name()
            leaves = expression.get_leaves()
            if name == 'Inequality':
                return leaves
            elif name in names:
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
            if (item.get_real_value() is None   # noqa
                and not item.has_form('DirectedInfinity', None)):
                return
            if prev is not None and do_cmp(prev, item) not in wanted:
                return Symbol('False')
            prev = item
        return Symbol('True')


class Inequality(Builtin):
    """
    'Inequality' is the head of expressions involving different inequality operators (at least temporarily).
    Thus, it is possible to write chains of inequalities.
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
            if name in operators.keys():
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
        return cmp(x1, x2)
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


class Equal(_InequalityOperator):
    """
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
    grouping = 'None'

    def apply_other(self, x, y, evaluation):
        'Equal[x_?(!RealNumberQ[#]&), y_?(!RealNumberQ[#]&)]'

        x, y = numerify([x, y], evaluation)
        result = do_compare(x, y)
        if result is not None:
            if result:
                return Symbol('True')
            else:
                return Symbol('False')


class Unequal(_InequalityOperator):
    """
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

    def apply_other(self, x, y, evaluation):
        'Unequal[x_?(!RealNumberQ[#]&), y_?(!RealNumberQ[#]&)]'

        x, y = numerify([x, y], evaluation)
        result = do_compare(x, y)
        if result is not None:
            if result:
                return Symbol('False')
            else:
                return Symbol('True')


class Less(_InequalityOperator):
    operator = '<'


class LessEqual(_InequalityOperator):
    operator = '<='


class Greater(_InequalityOperator):
    """
    >> a > b > c //FullForm
     = Greater[a, b, c]
    >> Greater[3, 2, 1]
     = True
    """

    operator = '>'


class GreaterEqual(_InequalityOperator):
    operator = '>='


class Positive(Test):
    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real)) and expr.value > 0


class Negative(Test):
    """
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
    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real)) and expr.value >= 0


class NonPositive(Test):
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


class Max(Builtin):
    """
    >> Max[4, -8, 1]
     = 4
    >> Max[{1,2},3,{-3,3.5,-Infinity},{{1/2}}]
     = 3.5

    #> Max[]
     = -Infinity
    """

    attributes = ('Flat', 'NumericFunction', 'OneIdentity', 'Orderless')

    def apply(self, items, evaluation):
        'Max[items___]'

        items = items.flatten(Symbol('List')).get_sequence()
        result = Expression('DirectedInfinity', -1)
        for item in items:
            if item.has_form('List', None):
                leaves = item.leaves
            else:
                leaves = [item]
            for leaf in leaves:
                c = do_cmp(leaf, result)
                if c > 0:
                    result = leaf
        return result


class Min(Builtin):
    """
    >> Min[4, -8, 1]
     = -8
    >> Min[{1,2},3,{-3,3.5,-Infinity},{{1/2}}]
     = -Infinity

    #> Min[]
     = Infinity
    """

    attributes = ('Flat', 'NumericFunction', 'OneIdentity', 'Orderless')

    def apply(self, items, evaluation):
        'Min[items___]'

        items = items.flatten(Symbol('List')).get_sequence()
        result = Expression('DirectedInfinity', 1)
        for item in items:
            if item.has_form('List', None):
                leaves = item.leaves
            else:
                leaves = [item]
            for leaf in leaves:
                c = do_cmp(leaf, result)
                if c < 0:
                    result = leaf
        return result
