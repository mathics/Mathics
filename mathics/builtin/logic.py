#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

from mathics.builtin.base import BinaryOperator, Predefined, PrefixOperator, Builtin
from mathics.builtin.lists import InvalidLevelspecError, python_levelspec, walk_levels
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


class _ShortCircuit(Exception):
    def __init__(self, result):
        self.result = result


class _ManyTrue(Builtin):
    rules = {
        '%(name)s[list_List, test_]': '%(name)s[list, test, 1]',
        '%(name)s[test_][list_List]': '%(name)s[list, test]',
    }

    def _short_circuit(self, what):
        raise NotImplementedError

    def _no_short_circuit(self):
        raise NotImplementedError

    def apply(self, expr, test, level, evaluation):
        '%(name)s[expr_, test_, level_]'

        try:
            start, stop = python_levelspec(level)
        except InvalidLevelspecError:
            evaluation.message('Level', 'level', level)
            return

        def callback(node):
            self._short_circuit(Expression(
                test, node).evaluate(evaluation).is_true())
            return node

        try:
            walk_levels(expr, start, stop, callback=callback)
        except _ShortCircuit as e:
            return e.result

        return self._no_short_circuit()


class NoneTrue(_ManyTrue):
    """
    <dl>
    <dt>'NoneTrue[{$expr1$, $expr2$, ...}, $test$]'
        <dd>returns True if no application of $test$ to $expr1$, $expr2$, ... evaluates to True.
    <dt>'NoneTrue[$list$, $test$, $level$]'
        <dd>returns True if no application of $test$ to items of $list$ at $level$ evaluates to True.
    <dt>'NoneTrue[$test$]'
        <dd>gives an operator that may be applied to expressions.
    </dl>

    >> NoneTrue[{1, 3, 5}, EvenQ]
     = True

    >> NoneTrue[{1, 4, 5}, EvenQ]
     = False

    #> NoneTrue[{}, EvenQ]
     = True
    """

    def _short_circuit(self, what):
        if what:
            raise _ShortCircuit(Symbol('False'))

    def _no_short_circuit(self):
        return Symbol('True')


class AnyTrue(_ManyTrue):
    """
    <dl>
    <dt>'AnyTrue[{$expr1$, $expr2$, ...}, $test$]'
        <dd>returns True if any application of $test$ to $expr1$, $expr2$, ... evaluates to True.
    <dt>'AnyTrue[$list$, $test$, $level$]'
        <dd>returns True if any application of $test$ to items of $list$ at $level$ evaluates to True.
    <dt>'AnyTrue[$test$]'
        <dd>gives an operator that may be applied to expressions.
    </dl>

    >> AnyTrue[{1, 3, 5}, EvenQ]
     = False

    >> AnyTrue[{1, 4, 5}, EvenQ]
     = True

    #> AnyTrue[{}, EvenQ]
     = False
    """

    def _short_circuit(self, what):
        if what:
            raise _ShortCircuit(Symbol('True'))

    def _no_short_circuit(self):
        return Symbol('False')


class AllTrue(_ManyTrue):
    """
    <dl>
    <dt>'AllTrue[{$expr1$, $expr2$, ...}, $test$]'
        <dd>returns True if all applications of $test$ to $expr1$, $expr2$, ... evaluate to True.
    <dt>'AllTrue[$list$, $test$, $level$]'
        <dd>returns True if all applications of $test$ to items of $list$ at $level$ evaluate to True.
    <dt>'AllTrue[$test$]'
        <dd>gives an operator that may be applied to expressions.
    </dl>

    >> AllTrue[{2, 4, 6}, EvenQ]
     = True

    >> AllTrue[{2, 4, 7}, EvenQ]
     = False

    #> AllTrue[{}, EvenQ]
     = True
    """
    def _short_circuit(self, what):
        if not what:
            raise _ShortCircuit(Symbol('False'))

    def _no_short_circuit(self):
        return Symbol('True')

