#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Integer functions
"""

import sympy

from mathics.builtin.base import Builtin, SympyObject, SympyFunction
from mathics.core.convert import from_sympy
from mathics.core.expression import Integer


class Floor(SympyFunction):
    """
    <dl>
    <dt>'Floor[$x$]'
        <dd>gives the smallest integer less than or equal to $x$.
    <dt>'Floor[$x$, $a$]'
        <dd>gives the smallest multiple of $a$ less than or equal to $x$.
    </dl>

    >> Floor[10.4]
     = 10
    >> Floor[10/3]
     = 3
    >> Floor[10]
     = 10
    >> Floor[21, 2]
     = 20
    >> Floor[2.6, 0.5]
     = 2.5
    >> Floor[-10.4]
     = -11

    For complex $x$, take the floor of real an imaginary parts.
    >> Floor[1.5 + 2.7 I]
     = 1 + 2 I

    For negative $a$, the smallest multiple of $a$ greater than or equal to $x$
    is returned.
    >> Floor[10.4, -1]
     = 11
    >> Floor[-10.4, -1]
     = -10
    """

    rules = {
        'Floor[x_, a_]': 'Floor[x / a] * a'
    }

    def apply_real(self, x, evaluation):
        'Floor[x_]'
        x = x.to_sympy()
        return from_sympy(sympy.floor(x))


class Ceiling(SympyFunction):
    """
    <dl>
    <dt>'Ceiling[$x$]'
        <dd>Give first integer greater than $x$.
    </dl>

    >> Ceiling[1.2]
     = 2
    >> Ceiling[3/2]
     = 2

    For complex $x$, take the ceiling of real an imaginary parts.
    >> Ceiling[1.3 + 0.7 I]
     = 2 + I
    """

    rules = {
        'Ceiling[x_, a_]': 'Ceiling[x / a] * a'
    }

    def apply(self, x, evaluation):
        'Ceiling[x_]'
        x = x.to_sympy()
        return from_sympy(sympy.ceiling(x))


class IntegerLength(Builtin):
    """
    >> IntegerLength[123456]
     = 6
    >> IntegerLength[10^10000]
     = 10001
    >> IntegerLength[-10^1000]
     = 1001
    'IntegerLength' with base 2:
    >> IntegerLength[8, 2]
     = 4
    Check that 'IntegerLength' is correct for the first 100 powers of 10:
    >> IntegerLength /@ (10 ^ Range[100]) == Range[2, 101]
     = True
    The base must be greater than 1:
    >> IntegerLength[3, -2]
     : Base -2 is not an integer greater than 1.
     = IntegerLength[3, -2]
    """

    rules = {
        'IntegerLength[n_]': 'IntegerLength[n, 10]',
    }

    messages = {
        'base': "Base `1` is not an integer greater than 1.",
    }

    def apply(self, n, b, evaluation):
        'IntegerLength[n_, b_]'

        # Use interval arithmetic to account for "right" rounding

        n, b = n.get_int_value(), b.get_int_value()
        if n is None or b is None:
            evaluation.message('IntegerLength', 'int')
            return
        if b <= 1:
            evaluation.message('IntegerLength', 'base', b)
            return

        result = sympy.Integer(sympy.log(abs(n), b)) + 1
        return Integer(result)
