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
        'Not[Not[expr_]]': 'expr',
    }


class Implies(BinaryOperator):
    """
    <dl>
    <dt>'Implies[$expr1$, $expr2$]'
    <dt>'$expr1$ && $expr2$'
        <dd>evaluates each expression in turn, returning 'True'
        as soon as the first expression evaluates to 'False'. If the
        first expression evaluates to 'True', 'Implies' returns the
        second expression.
    </dl>

    >> True \u21D2 False
     = False

    If an expression does not evaluate to 'True' or 'False', 'Implies'
    returns a result in symbolic form:
    >> a \u21D2 b \u21D2 True \u21D2 c
     = a \u21D2 b \u21D2 c
    """

    operator = '\u21D2'
    precedence = 200
    attributes = ('HoldAll', 'OneIdentity')
    narg = 2
    grouping = 'Right'

    rules = {
        'Implies[False, x_]': 'True',
        'Implies[True, x_]': 'x',
    }


class Equivalent(BinaryOperator):
    """
     <dl>
     <dt>'Equivalent[$expr1$, $expr2$, ...]'
     <dt>'$expr1$ \u21D4 $expr2$ \u21D4 ...'
         <dd>is equivalent to
         ($expr1$ && $expr2$ && ...) || (!$expr1$ && !$expr2$ && ...)
     </dl>

     >> True \u21D4 True \u21D4 False
      = False

     If all expressions do not evaluate to 'True' or 'False', 'Equivalent'
     returns a result in symbolic form:
     >> a \u21D4 b \u21D4 True \u21D4 c
      = a \u21D4 b \u21D4 c
      Otherwise, 'Equivalent' returns a result in DNF.
     """

    operator = '\u21D4'
    precedence = 205
    attributes = ('Flat', 'HoldAll', 'OneIdentity')

    def apply(self, args, evaluation):
        'Equivalent[args___]'

        args = args.get_sequence()
        leaves = []
        flag = False
        for arg in args:
            result = arg.evaluate(evaluation)
            if result == Symbol('False') or result.is_true():
                flag = not flag
                break
        if flag:
            return Expression('Or', Expression('And', *args), Expression('And', *[Expression('Not', arg) for arg in args])).evaluate(evaluation)
        else:
            return Expression('Equivalent', *args)


class Xor(BinaryOperator):
    """
    <dl>
    <dt>'Xor[$expr1$, $expr2$, ...]'
    <dt>'$expr1$ \u22BB $expr2$ \u22BB ...'
        <dd>evaluates each expression in turn, returning 'True'
        as soon as not all expressions evaluate to the same value. If all
        expressions evaluate to the same value, 'Xor' returns 'False'.
    </dl>

    >> Xor[False, True]
     = True

    If an expression does not evaluate to 'True' or 'False', 'Xor'
    returns a result in symbolic form:
    >> a \u22BB False \u22BB b]
     = a \u22BB b
    """

    operator = '\u22BB'
    precedence = 215
    attributes = ('Flat', 'HoldAll', 'OneIdentity')

    def apply(self, args, evaluation):
        'Xor[args___]'

        args = args.get_sequence()
        leaves = []
        flag = True
        for arg in args:
            result = arg.evaluate(evaluation)
            if result.is_true():
                flag = not flag
            elif result != Symbol('False'):
                leaves.append(result)
        if leaves and flag:
            if len(leaves) == 1:
                return leaves[0]
            else:
                return Expression('Xor', *leaves)
        elif leaves and not flag:
            if len(leaves) == 1:
                return Expression('Not', leaves[0])
            else:
                return Expression('Not', Expression('Xor', *leaves))
        else:
            return Symbol(repr(not flag))


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
