#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Integer functions
"""

from __future__ import unicode_literals
from __future__ import absolute_import

import sympy

from mathics.builtin.base import Builtin, SympyObject, SympyFunction
from mathics.core.convert import from_sympy
from mathics.core.expression import Integer, String, Expression


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
        <dd>gives the first integer greater than $x$.
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
    <dl>
    <dt>'IntegerLength[$x$]'
        <dd>gives the number of digits in the base-10 representation of $x$.
    <dt>'IntegerLength[$x$, $b$]'
        <dd>gives the number of base-$b$ digits in $x$.
    </dl>

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


class BitLength(Builtin):
    """
    <dl>
    <dt>'BitLength[$x$]'
        <dd>gives the number of bits needed to represent $x$.
    </dl>

    >> BitLength[1023]
     = 10
    >> BitLength[100]
     = 10
    """

    def apply(self, n, evaluation):
        'BitLength[n_Integer]'
        return Integer(n.get_int_value().bit_length())


def _reverse_digits(number, base):
    number = abs(number)
    if number == 0:
        yield 0
    else:
        while number > 0:
            rest, digit = divmod(number, base)
            yield digit
            number = rest


class IntegerString(Builtin):
    rules = {
        'IntegerString[n_Integer]': 'IntegerString[n, 10]'
    }

    list_of_symbols = [chr(i + ord('0')) for i in range(10)] +\
                      [chr(i + ord('a')) for i in range(26)]

    _python_builtin = {
        10: lambda number: str(abs(number)),
        8: lambda number: oct(abs(number)),
        2: lambda number: bin(abs(number))[2:]
    }

    def _build_string(self, n, b):
        builtin = IntegerString._python_builtin.get(b)
        if builtin:
            return builtin(n)
        else:
            list_of_symbols = IntegerString.list_of_symbols
            if b > len(list_of_symbols) or b < 2:
                return False
            return ''.join(reversed([list_of_symbols[r] for r in _reverse_digits(n, b)]))

    def apply_n(self, n, b, evaluation):
        'IntegerString[n_Integer, b_Integer]'
        s = self._build_string(n.get_int_value(), b.get_int_value())
        return String(s) if s else None

    def apply_n_b_length(self, n, b, length, evaluation):
        'IntegerString[n_Integer, b_Integer, length_Integer]'
        s = self._build_string(n.get_int_value(), b.get_int_value())
        if not s:
            return
        pad_length = length.get_int_value() - len(s)
        if pad_length <= 0:
            return String(s[-pad_length:])
        else:
            return String('0' * pad_length + s)


class IntegerDigits(Builtin):
    def apply_n_b(self, n, b, evaluation):
        'IntegerDigits[n_Integer, b_Integer]'
        base = b.get_int_value()
        if base < 2:
            return  # error
        digits = [Integer(d) for d in reversed(_reverse_digits(n.get_int_value(), base))]
        return Expression('List', *digits)

    def apply_n_b_length(self, n, b, length, evaluation):
        'IntegerDigits[n_Integer, b_Integer, length_Integer]'
        base = b.get_int_value()
        if base < 2:
            return  # error
        digits = [Integer(d) for d in reversed(_reverse_digits(n.get_int_value(), base))]
        pad_length = length.get_int_value() - len(digits)
        if pad_length <= 0:
            return Expression('List', *digits[-pad_length:])
        else:
            zero = Integer(0)
            return Expression('List', *([zero for _ in range(pad_length)] + digits))


class DigitCount(Builtin):
    rules = {
        'DigitCount[n_Integer]': 'DigitCount[n, 10]'
    }

    def apply_n_b_d(self, n, b, d, evaluation):
        'DigitCount[n_Integer, b_Integer, d_Integer]'
        base = b.get_int_value()
        if base < 2:
            return  # error
        return Integer(sum(1 for digit in _reverse_digits(n.get_int_value(), base) if digit == d))

    def apply_n_b(self, n, b, evaluation):
        'DigitCount[n_Integer, b_Integer]'
        base = b.get_int_value()
        if base < 2:
            return  # error
        occurence_count = [0] * base
        for digit in _reverse_digits(n.get_int_value(), base):
            occurence_count[digit] += 1
        return Expression('List', *occurence_count)
