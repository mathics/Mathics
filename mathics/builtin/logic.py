#!/usr/bin/env python
# -*- coding: utf-8 -*-

from mathics.builtin.base import BinaryOperator, Predefined, PrefixOperator
from mathics.core.expression import Expression, Symbol


class Or(BinaryOperator):
    """
    'Or[$expr1$, $expr2$, ...]' evaluates expressions until one evaluation results in 'True',
    in which case 'Or' returns 'True'. If all expressions evaluate to 'False', 'Or' returns 'False'.
    >> False || True
     = True
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
        <dd>evaluates expressions until one evaluation results in 'False',
        in which case 'And' returns 'False'. If all expressions evaluate to 'True', 'And' returns 'True'.
    </dl>
    >> True && True && False
     = False
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
    'Not' negates a logical expression.
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
    name = 'True'


class False_(Predefined):
    name = 'False'
