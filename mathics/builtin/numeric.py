#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Numeric evaluation

Support for numeric evaluation with arbitrary precision is just a proof-of-concept.
Precision is not "guarded" through the evaluation process. Only integer precision is supported.
However, things like 'N[Pi, 100]' should work as expected.
"""

from __future__ import unicode_literals
from __future__ import absolute_import

import sympy
import mpmath
from mpmath import mpf
import math
import hashlib
import zlib
import math
from six.moves import range
from collections import namedtuple
from contextlib import contextmanager
from itertools import chain


from mathics.builtin.base import Builtin, Predefined
from mathics.core.numbers import (
    dps, convert_int_to_digit_list, machine_precision, machine_epsilon,
    get_precision, PrecisionValueError)
from mathics.core.expression import (
    Integer, Real, Complex, Expression, Number, Symbol, Rational, from_python,
    MachineReal, PrecisionReal)
from mathics.core.convert import from_sympy

class N(Builtin):
    """
    <dl>
    <dt>'N[$expr$, $prec$]'
        <dd>evaluates $expr$ numerically with a precision of $prec$ digits.
    </dl>
    >> N[Pi, 50]
     = 3.1415926535897932384626433832795028841971693993751

    >> N[1/7]
     = 0.142857

    >> N[1/7, 5]
     = 0.14286

    You can manually assign numerical values to symbols.
    When you do not specify a precision, 'MachinePrecision' is taken.
    >> N[a] = 10.9
     = 10.9
    >> a
     = a

    'N' automatically threads over expressions, except when a symbol has attributes 'NHoldAll', 'NHoldFirst', or 'NHoldRest'.
    >> N[a + b]
     = 10.9 + b
    >> N[a, 20]
     = a
    >> N[a, 20] = 11;
    >> N[a + b, 20]
     = 11.000000000000000000 + b
    >> N[f[a, b]]
     = f[10.9, b]
    >> SetAttributes[f, NHoldAll]
    >> N[f[a, b]]
     = f[a, b]

    The precision can be a pattern:
    >> N[c, p_?(#>10&)] := p
    >> N[c, 3]
     = c
    >> N[c, 11]
     = 11.000000000

    You can also use 'UpSet' or 'TagSet' to specify values for 'N':
    >> N[d] ^= 5;
    However, the value will not be stored in 'UpValues', but in 'NValues' (as for 'Set'):
    >> UpValues[d]
     = {}
    >> NValues[d]
     = {HoldPattern[N[d, MachinePrecision]] :> 5}
    >> e /: N[e] = 6;
    >> N[e]
     = 6.

    Values for 'N[$expr$]' must be associated with the head of $expr$:
    >> f /: N[e[f]] = 7;
     : Tag f not found or too deep for an assigned rule.

    You can use 'Condition':
    >> N[g[x_, y_], p_] := x + y * Pi /; x + y > 3
    >> SetAttributes[g, NHoldRest]
    >> N[g[1, 1]]
     = g[1., 1]
    >> N[g[2, 2]] // InputForm
     = 8.283185307179586

    The precision of the result is no higher than the precision of the input
    >> N[Exp[0.1], 100]
     = 1.10517
    >> % // Precision
     = MachinePrecision
    >> N[Exp[1/10], 100]
     = 1.105170918075647624811707826490246668224547194737518718792863289440967966747654302989143318970748654
    >> % // Precision
     = 100.
    >> N[Exp[1.0`20], 100]
     = 2.7182818284590452354
    >> % // Precision
     = 20.

    #> p=N[Pi,100]
     = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
    #> ToString[p]
     = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
    #> 3.14159 * "a string"
     = 3.14159 a string

    #> N[Pi, Pi]
     = 3.14

    #> N[1/9, 30]
     = 0.111111111111111111111111111111
    #> Precision[%]
     = 30.

    #> N[1.5, 30]
     = 1.5
    #> Precision[%]
     = MachinePrecision
    #> N[1.5, 5]
     = 1.5
    #> Precision[%]
     = MachinePrecision

    #> {N[x], N[x, 30], N["abc"], N["abc", 30]}
     = {x, x, abc, abc}

    #> N[I, 30]
     = 1.00000000000000000000000000000 I

    #> N[1.01234567890123456789]
     = 1.01235
    #> N[1.012345678901234567890123, 20]
     = 1.0123456789012345679
    #> N[1.012345678901234567890123, 5]
     = 1.0123
    #> % // Precision
     = 5.
    #> N[1.012345678901234567890123, 50]
     = 1.01234567890123456789012
    #> % // Precision
     = 24.

    #> N[1.01234567890123456789`]
     = 1.01235
    #> N[1.01234567890123456789`, 20]
     = 1.01235
    #> % // Precision
     = MachinePrecision
    #> N[1.01234567890123456789`, 2]
     = 1.01235
    #> % // Precision
     = MachinePrecision
    """

    messages = {
        'precbd': "Requested precision `1` is not a machine-sized real number.",
        'preclg': ('Requested precision `1` is larger than $MaxPrecision. '
                   'Using current $MaxPrecision of `2` instead. '
                   '$MaxPrecision = Infinity specifies that any precision should be allowed.'),
        'precsm': 'Requested precision `1` is smaller than $MinPrecision. Using current $MinPrecision of `2` instead.',
    }

    rules = {
        'N[expr_]': 'N[expr, MachinePrecision]',
    }

    def apply_other(self, expr, prec, evaluation):
        'N[expr_, prec_]'

        try:
            d = get_precision(prec, evaluation)
        except PrecisionValueError:
            return

        if expr.get_head_name() in ('System`List', 'System`Rule'):
            return Expression(
                expr.head, *[self.apply_other(leaf, prec, evaluation)
                             for leaf in expr.leaves])

        if isinstance(expr, Number):
            return expr.round(d)

        name = expr.get_lookup_name()
        if name != '':
            nexpr = Expression('N', expr, prec)
            result = evaluation.definitions.get_value(
                name, 'System`NValues', nexpr, evaluation)
            if result is not None:
                if not result.same(nexpr):
                    result = Expression('N', result, prec).evaluate(evaluation)
                return result

        if expr.is_atom():
            return expr
        else:
            attributes = expr.head.get_attributes(evaluation.definitions)
            if 'System`NHoldAll' in attributes:
                eval_range = ()
            elif 'System`NHoldFirst' in attributes:
                eval_range = range(1, len(expr.leaves))
            elif 'System`NHoldRest' in attributes:
                if len(expr.leaves) > 0:
                    eval_range = (0,)
                else:
                    eval_range = ()
            else:
                eval_range = range(len(expr.leaves))
            head = Expression('N', expr.head, prec).evaluate(evaluation)
            leaves = expr.leaves[:]
            for index in eval_range:
                leaves[index] = Expression(
                    'N', leaves[index], prec).evaluate(evaluation)
            return Expression(head, *leaves)


class MachinePrecision(Predefined):
    """
    <dl>
    <dt>'MachinePrecision'
        <dd>represents the precision of machine precision numbers.
    </dl>

    >> N[MachinePrecision]
     = 15.9546
    >> N[MachinePrecision, 30]
     = 15.9545897701910033463281614204

    #> N[E, MachinePrecision]
     = 2.71828

    #> Round[MachinePrecision]
     = 16
    """

    rules = {
        'N[MachinePrecision, prec_]': 'N[Log[10, 2] * %i, prec]' % machine_precision,
    }


class MachineEpsilon_(Predefined):
    '''
    <dl>
    <dt>'$MachineEpsilon'
        <dd>is the distance between '1.0' and the next nearest representable machine-precision number.
    </dl>

    >> $MachineEpsilon
     = 2.22045*^-16

    >> x = 1.0 + {0.4, 0.5, 0.6} $MachineEpsilon;
    >> x - 1
     = {0., 0., 2.22045*^-16}
    '''

    name = '$MachineEpsilon'

    def evaluate(self, evaluation):
        return MachineReal(machine_epsilon)


class MachinePrecision_(Predefined):
    '''
    <dl>
    <dt>'$MachinePrecision'
        <dd>is the number of decimal digits of precision for machine-precision numbers.
    </dl>

    >> $MachinePrecision
     = 15.9546
    '''

    name = '$MachinePrecision'

    rules = {
        '$MachinePrecision': 'N[MachinePrecision]',
    }


class Precision(Builtin):
    """
    <dl>
    <dt>'Precision[$expr$]'
        <dd>examines the number of significant digits of $expr$.
    </dl>
    This is rather a proof-of-concept than a full implementation. Precision of
    compound expression is not supported yet.
    >> Precision[1]
     = Infinity
    >> Precision[1/2]
     = Infinity
    >> Precision[0.5]
     = MachinePrecision

    #> Precision[0.0]
     = MachinePrecision
    #> Precision[0.000000000000000000000000000000000000]
     = 0.
    #> Precision[-0.0]
     = MachinePrecision
    #> Precision[-0.000000000000000000000000000000000000]
     = 0.

    #> 1.0000000000000000 // Precision
     = MachinePrecision
    #> 1.00000000000000000 // Precision
     = 17.

    #> 0.4 + 2.4 I // Precision
     = MachinePrecision
    #> Precision[2 + 3 I]
     = Infinity

    #> Precision["abc"]
     = Infinity
    """

    rules = {
        'Precision[z_?MachineNumberQ]': 'MachinePrecision',
    }

    def apply(self, z, evaluation):
        'Precision[z_]'

        if not z.is_inexact():
            return Symbol('Infinity')
        elif z.to_sympy().is_zero:
            return Real(0)
        else:
            return Real(dps(z.get_precision()))


class MinPrecision(Builtin):
    '''
    <dl>
    <dt>'$MinPrecision'
      <dd>represents the minimum number of digits of precision permitted in abitrary-precision numbers.
    </dl>

    >> $MinPrecision
     = 0

    >> $MinPrecision = 10;

    >> N[Pi, 9]
     : Requested precision 9 is smaller than $MinPrecision. Using current $MinPrecision of 10. instead.
     = 3.141592654

    #> N[Pi, 10]
     = 3.141592654

    #> $MinPrecision = x
     : Cannot set $MinPrecision to x; value must be a non-negative number.
     = x
    #> $MinPrecision = -Infinity
     : Cannot set $MinPrecision to -Infinity; value must be a non-negative number.
     = -Infinity
    #> $MinPrecision = -1
     : Cannot set $MinPrecision to -1; value must be a non-negative number.
     = -1
    #> $MinPrecision = 0;

    #> $MaxPrecision = 10;
    #> $MinPrecision = 15
     : Cannot set $MinPrecision such that $MaxPrecision < $MinPrecision.
     = 15
    #> $MinPrecision
     = 0
    #> $MaxPrecision = Infinity;
    '''
    name = '$MinPrecision'
    rules = {
        '$MinPrecision': '0',
    }

    messages = {
        'precset': 'Cannot set `1` to `2`; value must be a non-negative number.',
        'preccon': 'Cannot set `1` such that $MaxPrecision < $MinPrecision.',
    }


class MaxPrecision(Predefined):
    '''
    <dl>
    <dt>'$MaxPrecision'
      <dd>represents the maximum number of digits of precision permitted in abitrary-precision numbers.
    </dl>

    >> $MaxPrecision
     = Infinity

    >> $MaxPrecision = 10;

    >> N[Pi, 11]
     : Requested precision 11 is larger than $MaxPrecision. Using current $MaxPrecision of 10. instead. $MaxPrecision = Infinity specifies that any precision should be allowed.
     = 3.141592654

    #> N[Pi, 10]
     = 3.141592654

    #> $MaxPrecision = x
     : Cannot set $MaxPrecision to x; value must be a positive number or Infinity.
     = x
    #> $MaxPrecision = -Infinity
     : Cannot set $MaxPrecision to -Infinity; value must be a positive number or Infinity.
     = -Infinity
    #> $MaxPrecision = 0
     : Cannot set $MaxPrecision to 0; value must be a positive number or Infinity.
     = 0
    #> $MaxPrecision = Infinity;

    #> $MinPrecision = 15;
    #> $MaxPrecision = 10
     : Cannot set $MaxPrecision such that $MaxPrecision < $MinPrecision.
     = 10
    #> $MaxPrecision
     = Infinity
    #> $MinPrecision = 0;
    '''
    name = '$MaxPrecision'

    rules = {
        '$MaxPrecision': 'Infinity',
    }

    messages = {
        'precset': 'Cannot set `1` to `2`; value must be a positive number or Infinity.',
        'preccon': 'Cannot set `1` such that $MaxPrecision < $MinPrecision.',
    }


class Round(Builtin):
    """
    <dl>
    <dt>'Round[$expr$]'
        <dd>rounds $expr$ to the nearest integer.
    <dt>'Round[$expr$, $k$]'
        <dd>rounds $expr$ to the closest multiple of $k$.
    </dl>

    >> Round[10.6]
     = 11
    >> Round[0.06, 0.1]
     = 0.1
    >> Round[0.04, 0.1]
     = 0.

    Constants can be rounded too
    >> Round[Pi, .5]
     = 3.
    >> Round[Pi^2]
     = 10

    Round to exact value
    >> Round[2.6, 1/3]
     = 8 / 3
    >> Round[10, Pi]
     = 3 Pi

    Round complex numbers
    >> Round[6/(2 + 3 I)]
     = 1 - I
    >> Round[1 + 2 I, 2 I]
     = 2 I

    Round Negative numbers too
    >> Round[-1.4]
     = -1

    Expressions other than numbers remain unevaluated:
    >> Round[x]
     = Round[x]
    >> Round[1.5, k]
     = Round[1.5, k]
    """

    attributes = ('Listable', 'NumericFunction')

    rules = {
        'Round[expr_?NumericQ]': 'Round[Re[expr], 1] + I * Round[Im[expr], 1]',
        'Round[expr_Complex, k_?RealNumberQ]': (
            'Round[Re[expr], k] + I * Round[Im[expr], k]'),
    }

    def apply(self, expr, k, evaluation):
        "Round[expr_?NumericQ, k_?NumericQ]"

        n = Expression('Divide', expr, k).round_to_float(evaluation, permit_complex=True)
        if n is None:
            return
        elif isinstance(n, complex):
            n = round(n.real)
        else:
            n = round(n)
        n = int(n)
        return Expression('Times', Integer(n), k)


class Rationalize(Builtin):
    '''
    <dl>
    <dt>'Rationalize[$x$]'
        <dd>converts a real number $x$ to a nearby rational number.
    <dt>'Rationalize[$x$, $dx$]'
        <dd>finds the rational number within $dx$ of $x$ with the smallest denominator.
    </dl>

    >> Rationalize[2.2]
    = 11 / 5

    Not all numbers can be well approximated.
    >> Rationalize[N[Pi]]
     = 3.14159

    Find the exact rational representation of 'N[Pi]'
    >> Rationalize[N[Pi], 0]
     = 245850922 / 78256779

    #> Rationalize[1.6 + 0.8 I]
     = 8 / 5 + 4 I / 5

    #> Rationalize[N[Pi] + 0.8 I, 1*^-6]
     = 355 / 113 + 4 I / 5

    #> Rationalize[N[Pi] + 0.8 I, x]
     : Tolerance specification x must be a non-negative number.
     = Rationalize[3.14159 + 0.8 I, x]

    #> Rationalize[N[Pi] + 0.8 I, -1]
     : Tolerance specification -1 must be a non-negative number.
     = Rationalize[3.14159 + 0.8 I, -1]

    #> Rationalize[N[Pi] + 0.8 I, 0]
     = 245850922 / 78256779 + 4 I / 5

    #> Rationalize[17 / 7]
     = 17 / 7

    #> Rationalize[x]
     = x

    #> Table[Rationalize[E, 0.1^n], {n, 1, 10}]
     = {8 / 3, 19 / 7, 87 / 32, 193 / 71, 1071 / 394, 2721 / 1001, 15062 / 5541, 23225 / 8544, 49171 / 18089, 419314 / 154257}

    #> Rationalize[x, y]
     : Tolerance specification y must be a non-negative number.
     = Rationalize[x, y]
    '''

    messages = {
        'tolnn': 'Tolerance specification `1` must be a non-negative number.',
    }

    rules = {
        'Rationalize[z_Complex]': 'Rationalize[Re[z]] + I Rationalize[Im[z]]',
        'Rationalize[z_Complex, dx_?Internal`RealValuedNumberQ]/;dx >= 0': 'Rationalize[Re[z], dx] + I Rationalize[Im[z], dx]',
    }

    def apply(self, x, evaluation):
        'Rationalize[x_]'

        py_x = x.to_sympy()
        if py_x is None or (not py_x.is_number) or (not py_x.is_real):
            return x
        return from_sympy(self.find_approximant(py_x))

    @staticmethod
    def find_approximant(x):
        c = 1e-4
        it = sympy.ntheory.continued_fraction_convergents(sympy.ntheory.continued_fraction_iterator(x))
        for i in it:
            p, q = i.as_numer_denom()
            tol = c / q**2
            if abs(i - x) <= tol:
                return i
            if tol < machine_epsilon:
                break
        return x

    @staticmethod
    def find_exact(x):
        p, q = x.as_numer_denom()
        it = sympy.ntheory.continued_fraction_convergents(sympy.ntheory.continued_fraction_iterator(x))
        for i in it:
            p, q = i.as_numer_denom()
            if abs(x - i) < machine_epsilon:
                return i

    def apply_dx(self, x, dx, evaluation):
        'Rationalize[x_, dx_]'
        py_x = x.to_sympy()
        if py_x is None:
            return x
        py_dx = dx.to_sympy()
        if py_dx is None or (not py_dx.is_number) or (not py_dx.is_real) or py_dx.is_negative:
            return evaluation.message('Rationalize', 'tolnn', dx)
        elif py_dx == 0:
            return from_sympy(self.find_exact(py_x))
        a = self.approx_interval_continued_fraction(py_x - py_dx, py_x + py_dx)
        sym_x = sympy.ntheory.continued_fraction_reduce(a)
        return Rational(sym_x)

    @staticmethod
    def approx_interval_continued_fraction(xmin, xmax):
        result = []
        a_gen = sympy.ntheory.continued_fraction_iterator(xmin)
        b_gen = sympy.ntheory.continued_fraction_iterator(xmax)
        while True:
            a, b = next(a_gen), next(b_gen)
            if a == b:
                result.append(a)
            else:
                result.append(min(a, b) + 1)
                break
        return result


def chop(expr, delta=10.0 ** (-10.0)):
    if isinstance(expr, Real):
        if -delta < expr.get_float_value() < delta:
            return Integer(0)
    elif isinstance(expr, Complex) and expr.is_inexact():
        real, imag = expr.real, expr.imag
        if -delta < real.get_float_value() < delta:
            real = Integer(0)
        if -delta < imag.get_float_value() < delta:
            imag = Integer(0)
        return Complex(real, imag)
    elif isinstance(expr, Expression):
        return Expression(chop(expr.head), *[
            chop(leaf) for leaf in expr.leaves])
    return expr


class Chop(Builtin):
    """
    <dl>
    <dt>'Chop[$expr$]'
        <dd>replaces floating point numbers close to 0 by 0.
    <dt>'Chop[$expr$, $delta$]'
        <dd>uses a tolerance of $delta$. The default tolerance is '10^-10'.
    </dl>

    >> Chop[10.0 ^ -16]
     = 0
    >> Chop[10.0 ^ -9]
     = 1.*^-9
    >> Chop[10 ^ -11 I]
     = I / 100000000000
    >> Chop[0. + 10 ^ -11 I]
     = 0
    """

    messages = {
        'tolnn': "Tolerance specification a must be a non-negative number.",
    }

    rules = {
        'Chop[expr_]': 'Chop[expr, 10^-10]',
    }

    def apply(self, expr, delta, evaluation):
        'Chop[expr_, delta_:(10^-10)]'

        delta = delta.round_to_float(evaluation)
        if delta is None or delta < 0:
            return evaluation.message('Chop', 'tolnn')

        return chop(expr, delta=delta)


class NumericQ(Builtin):
    """
    <dl>
    <dt>'NumericQ[$expr$]'
        <dd>tests whether $expr$ represents a numeric quantity.
    </dl>

    >> NumericQ[2]
     = True
    >> NumericQ[Sqrt[Pi]]
     = True
    >> NumberQ[Sqrt[Pi]]
     = False
    """

    def apply(self, expr, evaluation):
        'NumericQ[expr_]'

        def test(expr):
            if isinstance(expr, Expression):
                attr = evaluation.definitions.get_attributes(
                    expr.head.get_name())
                return 'System`NumericFunction' in attr and all(
                    test(leaf) for leaf in expr.leaves)
            else:
                return expr.is_numeric()

        return Symbol('True') if test(expr) else Symbol('False')


class RealValuedNumericQ(Builtin):
    '''
    #> Internal`RealValuedNumericQ /@ {1, N[Pi], 1/2, Sin[1.], Pi, 3/4, aa,  I}
     = {True, True, True, True, True, True, False, False}
    '''

    context = 'Internal`'

    rules = {
        'Internal`RealValuedNumericQ[x_]': 'Head[N[x]] === Real',
    }


class RealValuedNumberQ(Builtin):
    '''
    #>  Internal`RealValuedNumberQ /@ {1, N[Pi], 1/2, Sin[1.], Pi, 3/4, aa, I}
     = {True, True, True, True, False, True, False, False}
    '''

    context = 'Internal`'

    rules = {
        'Internal`RealValuedNumberQ[x_Real]': 'True',
        'Internal`RealValuedNumberQ[x_Integer]': 'True',
        'Internal`RealValuedNumberQ[x_Rational]': 'True',
        'Internal`RealValuedNumberQ[x_]': 'False',
    }


class IntegerDigits(Builtin):
    """
    <dl>
    <dt>'IntegerDigits[$n$]'
        <dd>returns a list of the base-10 digits in the integer $n$.
    <dt>'IntegerDigits[$n$, $base$]'
        <dd>returns a list of the base-$base$ digits in $n$.
    <dt>'IntegerDigits[$n$, $base$, $length$]'
        <dd>returns a list of length $length$, truncating or padding
        with zeroes on the left as necessary.
    </dl>

    >> IntegerDigits[76543]
     = {7, 6, 5, 4, 3}

    The sign of $n$ is discarded:
    >> IntegerDigits[-76543]
     = {7, 6, 5, 4, 3}

    >> IntegerDigits[15, 16]
     = {15}
    >> IntegerDigits[1234, 16]
     = {4, 13, 2}
    >> IntegerDigits[1234, 10, 5]
     = {0, 1, 2, 3, 4}

    #> IntegerDigits[1000, 10]
     = {1, 0, 0, 0}

    #> IntegerDigits[0]
     = {0}
    """

    attributes = ('Listable',)

    messages = {
        'int': 'Integer expected at position 1 in `1`',
        'ibase': 'Base `1` is not an integer greater than 1.',
    }

    rules = {
        'IntegerDigits[n_]': 'IntegerDigits[n, 10]',
    }

    def apply_len(self, n, base, length, evaluation):
        'IntegerDigits[n_, base_, length_]'

        if not(isinstance(length, Integer) and length.get_int_value() >= 0):
            return evaluation.message('IntegerDigits', 'intnn')

        return self.apply(n, base, evaluation,
                          nr_elements=length.get_int_value())

    def apply(self, n, base, evaluation, nr_elements=None):
        'IntegerDigits[n_, base_]'

        if not(isinstance(n, Integer)):
            return evaluation.message('IntegerDigits', 'int',
                                      Expression('IntegerDigits', n, base))

        if not(isinstance(base, Integer) and base.get_int_value() > 1):
            return evaluation.message('IntegerDigits', 'ibase', base)

        if nr_elements == 0:
            # trivial case: we don't want any digits
            return Expression('List')

        digits = convert_int_to_digit_list(
            n.get_int_value(), base.get_int_value())

        if nr_elements is not None:
            if len(digits) >= nr_elements:
                # Truncate, preserving the digits on the right
                digits = digits[-nr_elements:]
            else:
                # Pad with zeroes
                digits = [0] * (nr_elements - len(digits)) + digits

        return Expression('List', *digits)


class _ZLibHash:  # make zlib hashes behave as if they were from hashlib
    def __init__(self, fn):
        self._bytes = b''
        self._fn = fn

    def update(self, bytes):
        self._bytes += bytes

    def hexdigest(self):
        return format(self._fn(self._bytes), 'x')


class Hash(Builtin):
    """
    <dl>
    <dt>'Hash[$expr$]'
      <dd>returns an integer hash for the given $expr$.
    <dt>'Hash[$expr$, $type$]'
      <dd>returns an integer hash of the specified $type$ for the given $expr$.</dd>
      <dd>The types supported are "MD5", "Adler32", "CRC32", "SHA", "SHA224", "SHA256", "SHA384", and "SHA512".</dd>
    </dl>

    > Hash["The Adventures of Huckleberry Finn"]
    = 213425047836523694663619736686226550816

    > Hash["The Adventures of Huckleberry Finn", "SHA256"]
    = 95092649594590384288057183408609254918934351811669818342876362244564858646638

    > Hash[1/3]
    = 56073172797010645108327809727054836008

    > Hash[{a, b, {c, {d, e, f}}}]
    = 135682164776235407777080772547528225284

    > Hash[SomeHead[3.1415]]
    = 58042316473471877315442015469706095084

    >> Hash[{a, b, c}, "xyzstr"]
     = Hash[{a, b, c}, xyzstr]
    """

    rules = {
        'Hash[expr_]': 'Hash[expr, "MD5"]',
    }

    attributes = ('Protected', 'ReadProtected')

    # FIXME md2
    _supported_hashes = {
        'Adler32': lambda: _ZLibHash(zlib.adler32),
        'CRC32': lambda: _ZLibHash(zlib.crc32),
        'MD5': hashlib.md5,
        'SHA': hashlib.sha1,
        'SHA224': hashlib.sha224,
        'SHA256': hashlib.sha256,
        'SHA384': hashlib.sha384,
        'SHA512': hashlib.sha512,
    }

    @staticmethod
    def compute(user_hash, py_hashtype):
        hash_func = Hash._supported_hashes.get(py_hashtype)
        if hash_func is None:  # unknown hash function?
            return  # in order to return original Expression
        h = hash_func()
        user_hash(h.update)
        return from_python(int(h.hexdigest(), 16))

    def apply(self, expr, hashtype, evaluation):
        'Hash[expr_, hashtype_String]'
        return Hash.compute(expr.user_hash, hashtype.get_string_value())


class TypeEscalation(Exception):
    def __init__(self, mode):
        self.mode = mode


class Fold(object):
    # allows inherited classes to specify a single algorithm implementation that
    # can be called with machine precision, arbitrary precision or symbolically.

    ComputationFunctions = namedtuple(
        'ComputationFunctions', ('sin', 'cos'))

    FLOAT = 0
    MPMATH = 1
    SYMBOLIC = 2

    math = {
        FLOAT: ComputationFunctions(
            cos=math.cos,
            sin=math.sin,
        ),
        MPMATH: ComputationFunctions(
            cos=mpmath.cos,
            sin=mpmath.sin,
        ),
        SYMBOLIC: ComputationFunctions(
            cos=lambda x: Expression('Cos', x),
            sin=lambda x: Expression('Sin', x),
        )
    }

    operands = {
        FLOAT: lambda x: None if x is None else x.round_to_float(),
        MPMATH: lambda x: None if x is None else x.to_mpmath(),
        SYMBOLIC: lambda x: x,
    }

    def _operands(self, state, steps, at_least):
        raise NotImplementedError

    def _fold(self, state, steps, math):
        raise NotImplementedError

    def fold(self, x, l):
        # computes fold(x, l) with the internal _fold function. will start
        # its evaluation machine precision, and will escalate to arbitrary
        # precision if or symbolical evaluation only if necessary. folded
        # items already computed are carried over to new evaluation modes.

        mode = self.FLOAT

        n = 0

        init = x
        init_dirty = False

        yield x

        for _ in range(3):
            try:
                if init_dirty:  # is this a continuation from a previous run?
                    init = tuple(from_python(x) for x in init)
                    init_dirty = False

                def at_least(m):
                    if mode < m:
                        raise TypeEscalation(m)

                unconverted_operands = self._operands(init, l[n:], at_least)

                if mode == self.MPMATH:
                    unconverted_operands = list(unconverted_operands)  # might raise TypeEscalation

                    from mathics.core.numbers import min_prec
                    precision = min_prec(*[t for t in chain(*unconverted_operands) if t is not None])
                    working_precision = mpmath.workprec
                else:
                    @contextmanager
                    def working_precision(_):
                        yield
                    precision = None

                if mode == self.FLOAT:
                    def out(z):
                        return Real(z)
                elif mode == self.MPMATH:
                    def out(z):
                        return Real(z, precision)
                else:
                    def out(z):
                        return z

                as_operand = self.operands.get(mode)

                def converted_operands():
                    for y in unconverted_operands:
                        yield tuple(as_operand(t) for t in y)

                with working_precision(precision):
                    operands = converted_operands()

                    generator = self._fold(next(operands), operands, self.math.get(mode))

                    for y in generator:
                        y = tuple(out(t) for t in y)
                        yield y
                        init = y
                        init_dirty = True
                        n += 1

                return
            except TypeEscalation as t:
                assert t.mode > mode
                mode = t.mode

        raise ValueError  # should have evaluated symbolically and succeeded.
