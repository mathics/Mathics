# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011-2013 The Mathics Team

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

from __future__ import with_statement

import sympy
import sympy.mpmath as mpmath
from math import log

from mathics.core.util import unicode_superscript


def get_type(value):
    if isinstance(value, sympy.Integer):
        return 'z'
    elif isinstance(value, sympy.Rational):
        return 'q'
    elif isinstance(value, sympy.Float) or isinstance(value, mpmath.mpf):
        return 'f'
    elif (isinstance(value, sympy.Expr) and value.is_number and
          not value.is_real) or isinstance(value, mpmath.mpc):
        return 'c'
    else:
        return None


def same(v1, v2):
    return get_type(v1) == get_type(v2) and v1 == v2


def is_0(value):
    return get_type(value) == 'z' and value == 0


def sympy2mpmath(value, prec=None):
    if prec is None:
        from mathics.builtin.numeric import machine_precision
        prec = machine_precision
    value = value.n(dps(prec))
    if value.is_real:
        return mpmath.mpf(value)
    elif value.is_number:
        return mpmath.mpc(*value.as_real_imag())
    else:
        return None


class SpecialValueError(Exception):
    def __init__(self, name):
        self.name = name


def mpmath2sympy(value, prec=None):
    if prec is None:
        from mathics.builtin.numeric import machine_precision
        prec = machine_precision
    if isinstance(value, mpmath.mpc):
        return (sympy.Float(str(value.real), dps(prec)) +
                sympy.I * sympy.Float(str(value.imag), dps(prec)))
    elif isinstance(value, mpmath.mpf):
        if str(value) in ('+inf', '-inf'):
            raise SpecialValueError('ComplexInfinity')
        return sympy.Float(str(value), dps(prec))
    else:
        return None

C = log(10, 2)  # ~ 3.3219280948873626


def dps(prec):
    return max(1, int(round(int(prec) / C - 1)))


def prec(dps):
    return max(1, int(round((int(dps) + 1) * C)))


def format_float(value, pretty=True, parenthesize_plus=False):
    s = str(value)
    s = s.split('e')
    if len(s) == 2:
        man, exp = s
        if pretty:
            return u'%s\u00d710%s' % (
                format_float(man), unicode_superscript(exp))
        else:
            result = u'%s*10^%s' % (format_float(man), exp)
            if parenthesize_plus:
                result = '(%s)' % result
            return result
    else:
        return s[0]


def mul(x, y):
    return x * y


def add(x, y):
    return x + y


def min_prec(*args):
    result = None
    for arg in args:
        prec = arg.get_precision()
        if result is None or (prec is not None and prec < result):
            result = prec
    return result


def pickle_mp(value):
    return (get_type(value), str(value))


def unpickle_mp(value):
    type, value = value
    if type == 'z':
        return sympy.Integer(value)
    elif type == 'q':
        return sympy.Rational(value)
    elif type == 'f':
        return sympy.Float(value)
    else:
        return value

# algorithm based on
# http://stackoverflow.com/questions/5110177/how-to-convert-floating-point-number-to-base-3-in-python       # nopep8


def convert_base(x, base, precision=10):
    sign = -1 if x < 0 else 1
    x *= sign

    length_of_int = int(log(x, base))
    iexps = range(length_of_int, -1, -1)
    import string
    digits = string.digits + string.lowercase

    def convert(x, base, exponents):
        out = []
        for e in exponents:
            d = int(x // (base ** e))
            x -= d * (base ** e)
            out.append(digits[d])
            if x == 0 and e < 0:
                break
        return out

    int_part = convert(int(x), base, iexps)
    if sign == -1:
        int_part.insert(0, '-')

    if (isinstance(x, float)):
        fexps = range(-1, -int(precision + 1), -1)
        real_part = convert(x - int(x), base, fexps)

        return "%s.%s" % (''.join(int_part), ''.join(real_part))
    else:
        return ''.join(int_part)


def convert_int_to_digit_list(x, base):
    x = abs(x)

    length_of_int = int(log(x, base)) + 1
    iexps = range(length_of_int, -1, -1)

    def convert(x, base, exponents):
        out = []
        for e in exponents:
            d = int(x // (base ** e))
            x -= d * (base ** e)
            if out or d != 0:   # drop any leading zeroes
                out.append(d)
            if x == 0 and e < 0:
                break
        return out

    return convert(x, base, iexps)
