#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

"""
Converts expressions from SymPy to Mathics expressions.
Conversion to SymPy is handled directly in BaseExpression descendants.
"""

import six
from six.moves import range
from six.moves import zip

import sympy

sympy_symbol_prefix = '_Mathics_User_'
sympy_slot_prefix = '_Mathics_Slot_'


def create_symbol(self, name):
    from mathics.core import expression
    return expression.Symbol(name)


class ConvertSubstitutions(object):
    head_name = '___SageSubst___'

    def __init__(self):
        self.subs = []

    def substitute(self, expr):
        from mathics.core import expression

        index = len(self.subs)
        self.subs.append(expr)
        return expression.Expression(self.head_name, expression.Integer(index),
                                     *expr.get_atoms())

BasicSympy = sympy.Expr


class SympyExpression(BasicSympy):
    is_Function = True
    nargs = None

    def __new__(cls, *exprs):
        # sympy simplify may also recreate the object if simplification occured
        # in the leaves
        from mathics.core.expression import Expression

        if all(isinstance(expr, BasicSympy) for expr in exprs):
            # called with SymPy arguments
            obj = BasicSympy.__new__(cls, *exprs)
        elif len(exprs) == 1 and isinstance(exprs[0], Expression):
            # called with Mathics argument
            expr = exprs[0]
            obj = BasicSympy.__new__(
                cls, expr.head.to_sympy(),
                *tuple(leaf.to_sympy() for leaf in expr.leaves))
            obj.expr = expr
        else:
            raise TypeError
        return obj

    """def new(self, *args):
        from mathics.core import expression

        expr = expression.Expression(from_sympy(args[0]),
            *(from_sympy(arg) for arg in args[1:]))
        return SympyExpression(expr)"""

    @property
    def func(self):
        class SympyExpressionFunc(object):
            def __new__(cls, *args):
                return SympyExpression(self.expr)
                # return SympyExpression(expression.Expression(self.expr.head,
                # *(from_sympy(arg) for arg in args[1:])))
        return SympyExpressionFunc

    def has_any_symbols(self, *syms):
        result = any(arg.has_any_symbols(*syms) for arg in self.args)
        return result

    def _eval_subs(self, old, new):
        if self == old:
            return new
        old, new = from_sympy(old), from_sympy(new)
        old_name = old.get_name()
        if old_name:
            new_expr = self.expr.replace_vars({old_name: new})
            return SympyExpression(new_expr)
        return self

    def _eval_rewrite(self, pattern, rule, **hints):
        return self

    @property
    def is_commutative(self):
        if all(getattr(t, 'is_commutative', False) for t in self.args):
            return True
        else:
            return False

    def __str__(self):
        return '%s[%s]' % (super(SympyExpression, self).__str__(), self.expr)


def from_sympy(expr):
    from mathics.builtin import sympy_to_mathics
    from mathics.core.expression import (
        Symbol, Integer, Rational, Real, Complex, String, Expression)

    from sympy.core import numbers, function, symbol

    if isinstance(expr, (tuple, list)):
        return Expression('List', *[from_sympy(item) for item in expr])
    if isinstance(expr, int):
        return Integer(expr)
    if isinstance(expr, float):
        return Real(expr)
    if isinstance(expr, complex):
        return Complex(expr.real, expr.imag)
    if isinstance(expr, six.string_types):
        return String(expr)
    if expr is None:
        return Symbol('Null')
    if isinstance(expr, sympy.Matrix):
        if len(expr.shape) == 2 and (expr.shape[1] == 1):
            # This is a vector (only one column)
            # Transpose and select first row to get result equivalent to Mathematica
            return Expression('List', *[from_sympy(item) for item in expr.T.tolist()[0]])
        else:
            return Expression('List', *[
                [from_sympy(item) for item in row] for row in expr.tolist()])
    if expr.is_Atom:
        name = None
        if expr.is_Symbol:
            name = six.text_type(expr)
            if isinstance(expr, symbol.Dummy):
                name = name + ('__Dummy_%d' % expr.dummy_index)
                return Symbol(name, sympy_dummy=expr)
            if ((not name.startswith(sympy_symbol_prefix) or     # noqa
                 name.startswith(sympy_slot_prefix)) and
                name.startswith('C')):
                return Expression('C', int(name[1:]))
            if name.startswith(sympy_symbol_prefix):
                name = name[len(sympy_symbol_prefix):]
            if name.startswith(sympy_slot_prefix):
                index = name[len(sympy_slot_prefix):]
                return Expression('Slot', int(index))
        elif expr.is_NumberSymbol:
            name = six.text_type(expr)
        if name is not None:
            builtin = sympy_to_mathics.get(name)
            if builtin is not None:
                name = builtin.get_name()
            return Symbol(name)
        elif isinstance(expr, (numbers.Infinity, numbers.ComplexInfinity)):
            return Symbol(expr.__class__.__name__)
        elif isinstance(expr, numbers.NegativeInfinity):
            return Expression('Times', Integer(-1), Symbol('Infinity'))
        elif isinstance(expr, numbers.ImaginaryUnit):
            return Complex(0, 1)
        elif isinstance(expr, numbers.Integer):
            return Integer(expr.p)
        elif isinstance(expr, numbers.Rational):
            if expr.q == 0:
                if expr.p > 0:
                    return Symbol('Infinity')
                elif expr.p < 0:
                    return Expression('Times', Integer(-1), Symbol('Infinity'))
                else:
                    assert expr.p == 0
                    return Symbol('Indeterminate')
            return Rational(expr.p, expr.q)
        elif isinstance(expr, numbers.Float):
            return Real(expr)
        elif isinstance(expr, numbers.NaN):
            return Symbol('Indeterminate')
        elif isinstance(expr, function.FunctionClass):
            return Symbol(six.text_type(expr))
    elif expr.is_number and all([x.is_Number for x in expr.as_real_imag()]):
        # Hack to convert 3 * I to Complex[0, 3]
        return Complex(*[from_sympy(arg) for arg in expr.as_real_imag()])
    elif expr.is_Add:
        return Expression('Plus', *sorted([
            from_sympy(arg) for arg in expr.args]))
    elif expr.is_Mul:
        return Expression('Times', *sorted([
            from_sympy(arg) for arg in expr.args]))
    elif expr.is_Pow:
        return Expression('Power', *[from_sympy(arg) for arg in expr.args])
    elif expr.is_Equality:
        return Expression('Equal', *[from_sympy(arg) for arg in expr.args])

    elif isinstance(expr, SympyExpression):
        return expr.expr

    elif isinstance(expr, sympy.RootSum):
        return Expression('RootSum', from_sympy(expr.poly),
                          from_sympy(expr.fun))
    elif isinstance(expr, sympy.PurePoly):
        coeffs = expr.coeffs()
        monoms = expr.monoms()
        result = []
        for coeff, monom in zip(coeffs, monoms):
            factors = []
            if coeff != 1:
                factors.append(from_sympy(coeff))
            for index, exp in enumerate(monom):
                if exp != 0:
                    slot = Expression('Slot', index + 1)
                    if exp == 1:
                        factors.append(slot)
                    else:
                        factors.append(Expression(
                            'Power', slot, from_sympy(exp)))
            if factors:
                result.append(Expression('Times', *factors))
            else:
                result.append(Integer(1))
        return Expression('Function', Expression('Plus', *result))
    elif isinstance(expr, sympy.Lambda):
        vars = [sympy.Symbol('%s%d' % (sympy_slot_prefix, index + 1))
                for index in range(len(expr.variables))]
        return Expression('Function', from_sympy(expr(*vars)))

    elif expr.is_Function or isinstance(
        expr, (sympy.Integral, sympy.Derivative,
               sympy.Sum, sympy.Product)):
        if isinstance(expr, sympy.Integral):
            name = 'Integral'
        elif isinstance(expr, sympy.Derivative):
            name = 'Derivative'
        else:
            name = expr.func.__name__
            if name.startswith(sympy_symbol_prefix):
                name = name[len(sympy_symbol_prefix):]
        args = [from_sympy(arg) for arg in expr.args]
        builtin = sympy_to_mathics.get(name)
        if builtin is not None:
            return builtin.from_sympy(name, args)
        return Expression(Symbol(name), *args)

    elif isinstance(expr, sympy.Tuple):
        return Expression('List', *[from_sympy(arg) for arg in expr.args])

    # elif isinstance(expr, sympy.Sum):
    #    return Expression('Sum', )

    elif isinstance(expr, sympy.LessThan):
        return Expression('LessEqual',
                          [from_sympy(arg) for arg in expr.args])
    elif isinstance(expr, sympy.StrictLessThan):
        return Expression('Less',
                          [from_sympy(arg) for arg in expr.args])
    elif isinstance(expr, sympy.GreaterThan):
        return Expression('GreaterEqual',
                          [from_sympy(arg) for arg in expr.args])
    elif isinstance(expr, sympy.StrictGreaterThan):
        return Expression('Greater',
                          [from_sympy(arg) for arg in expr.args])
    elif isinstance(expr, sympy.Unequality):
        return Expression('Unequal',
                          [from_sympy(arg) for arg in expr.args])
    elif isinstance(expr, sympy.Equality):
        return Expression('Equal',
                          [from_sympy(arg) for arg in expr.args])
    elif expr is sympy.true:
        return Symbol('True')
    elif expr is sympy.false:
        return Symbol('False')
    else:
        raise ValueError("Unknown SymPy expression: %s" % expr)
