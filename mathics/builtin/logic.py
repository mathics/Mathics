#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

from mathics.builtin.base import BinaryOperator, Predefined, PrefixOperator
from mathics.core.expression import Expression, Symbol


class Or(BinaryOperator):
    """
    <dl>
    <dt>'Or[$expr1$, $expr2$, ...]'
    <dt>'$expr1$ || $expr2$ || ...'
        <dd>evaluates each expression in turn, returning 'True'
        as soon as an expression evaluates to 'True'. If all
        expressions evaluate to 'False', 'Or' returns 'False'.
    </dl>

    >> False || True
     = True

    If an expression does not evaluate to 'True' or 'False', 'Or'
    returns a result in symbolic form:
    >> a || False || b
     = a || b
    """

    operator = '||'
    precedence = 215
    attributes = ('Flat', 'HoldAll', 'OneIdentity')

    def apply(self, args, evaluation):
        'Or[args___]'

        args = args.get_sequence()
        leaves = []
        for arg in args:
            result = arg.evaluate(evaluation)
            if result.is_true():
                return Symbol('True')
            elif result != Symbol('False'):
                leaves.append(result)
        if leaves:
            if len(leaves) == 1:
                return leaves[0]
            else:
                return Expression('Or', *leaves)
        else:
            return Symbol('False')


class And(BinaryOperator):
    """
    <dl>
    <dt>'And[$expr1$, $expr2$, ...]'
    <dt>'$expr1$ && $expr2$ && ...'
        <dd>evaluates each expression in turn, returning 'False'
        as soon as an expression evaluates to 'False'. If all
        expressions evaluate to 'True', 'And' returns 'True'.
    </dl>

    >> True && True && False
     = False

    If an expression does not evaluate to 'True' or 'False', 'And'
    returns a result in symbolic form:
    >> a && b && True && c
     = a && b && c
    """

    operator = '&&'
    precedence = 215
    attributes = ('Flat', 'HoldAll', 'OneIdentity')

    def apply(self, args, evaluation):
        'And[args___]'

        args = args.get_sequence()
        leaves = []
        for arg in args:
            result = arg.evaluate(evaluation)
            if result == Symbol('False'):
                return Symbol('False')
            elif not result.is_true():
                leaves.append(result)
        if leaves:
            if len(leaves) == 1:
                return leaves[0]
            else:
                return Expression('And', *leaves)
        else:
            return Symbol('True')


class Not(PrefixOperator):
    """
    <dl>
    <dt>'Not[$expr$]'
    <dt>'!$expr$'
        <dd>negates the logical expression $expr$.
    </dl>

    >> !True
     = False
    >> !False
     = True
    >> !b
     = !b
    """

    operator = '!'
    precedence = 230

    rules = {
        'Not[True]': 'False',
        'Not[False]': 'True',
    }


class True_(Predefined):
    """
    <dl>
    <dt>'True'
        <dd>represents the Boolean true value.
    </dl>
    """
    name = 'True'


class False_(Predefined):
    """
    <dl>
    <dt>'False'
        <dd>represents the Boolean false value.
    </dl>
    """
    name = 'False'
