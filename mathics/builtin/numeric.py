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
import hashlib
import zlib
from six.moves import range

from mathics.builtin.base import Builtin, Predefined
from mathics.core.numbers import (dps, prec, convert_base,
                                  convert_int_to_digit_list)
from mathics.core.expression import (Integer, Rational, Real, Complex, String,
                                     Expression, Number, Symbol, from_python,
                                     ensure_context)
from mathics.core.convert import from_sympy
from mathics.settings import MACHINE_PRECISION

machine_precision = MACHINE_PRECISION


def get_precision(precision, evaluation):
    if precision.get_name() == 'System`MachinePrecision':
        return machine_precision
    elif isinstance(precision, (Integer, Rational, Real)):
        return prec(float(precision.to_sympy()))
    else:
        evaluation.message('N', 'precbd', precision)
        return None


class N(Builtin):
    """
    <dl>
    <dt>'N[$expr$, $prec$]'
        <dd>evaluates $expr$ numerically with a precision of $prec$ digits.
    </dl>
    >> N[Pi, 50]
     = 3.1415926535897932384626433832795028841971693993751

    >> N[1/7]
     = 0.142857142857142857

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
     = 11. + b
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
     = 11.

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
    >> N[g[2, 2]]
     = 8.28318530717958648

    #> p=N[Pi,100]
     = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
    #> ToString[p]
     = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
    #> 3.14159 * "a string"
     = 3.14159 a string
    """

    messages = {
        'precbd': (
            "Requested precision `1` is not a machine-sized real number."),
    }

    rules = {
        'N[expr_]': 'N[expr, MachinePrecision]',
    }

    def apply_other(self, expr, prec, evaluation):
        'N[expr_, prec_]'

        valid_prec = get_precision(prec, evaluation)

        if valid_prec is not None:
            if expr.get_head_name() in ('System`List', 'System`Rule'):
                return Expression(
                    expr.head, *[self.apply_other(leaf, prec, evaluation)
                                 for leaf in expr.leaves])
            if isinstance(expr, Number):
                return expr.round(valid_prec)

            name = expr.get_lookup_name()
            if name != '':
                nexpr = Expression('N', expr, prec)
                result = evaluation.definitions.get_value(
                    name, 'System`NValues', nexpr, evaluation)
                if result is not None:
                    if not result.same(nexpr):
                        result = Expression(
                            'N', result, prec).evaluate(evaluation)
                    return result

            if expr.is_atom():
                return expr.round(valid_prec)
            else:
                attributes = expr.head.get_attributes(evaluation.definitions)
                if 'System`NHoldAll' in attributes:
                    eval_range = []
                elif 'System`NHoldFirst' in attributes:
                    eval_range = list(range(1, len(expr.leaves)))
                elif 'System`NHoldRest' in attributes:
                    if len(expr.leaves) > 0:
                        eval_range = (0,)
                    else:
                        eval_range = ()
                else:
                    eval_range = list(range(len(expr.leaves)))
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
        <dd>is a "pessimistic" (integer) estimation of the internally used standard precision.
    </dl>
    >> N[MachinePrecision]
     = 18.
    """

    def apply_N(self, prec, evaluation):
        'N[MachinePrecision, prec_]'

        prec = get_precision(prec, evaluation)
        if prec is not None:
            return Real(dps(machine_precision), prec)


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
     = 18.
    #> Precision[0.0]
     = 0.
    #> Precision[0.000000000000000000000000000000000000]
     = 0.
    #> Precision[-0.0]      (*Matematica gets this wrong *)
     = 0.
    #> Precision[-0.000000000000000000000000000000000000]
     = 0.
    """

    rules = {
        'Precision[_Integer]': 'Infinity',
        'Precision[_Rational]': 'Infinity',
        'Precision[_Symbol]': 'Infinity',
        'Precision[z:0.0]': '0.',
        'Precision[z:-0.0]': '0.',
    }

    def apply_real(self, x, evaluation):
        'Precision[x_Real]'

        return Real(dps(x.get_precision()))

    def apply_complex(self, x, evaluation):
        'Precision[x_Complex]'

        if x.is_inexact():
            return Real(dps(x.get_precision()))
        else:
            return Symbol('Infinity')


def round(value, k):
    n = (1. * value / k).as_real_imag()[0]
    if n >= 0:
        n = sympy.Integer(n + 0.5)
    else:
        n = sympy.Integer(n - 0.5)
    return n * k


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
    ## This should return 0. but doesn't due to a bug in sympy
    >> Round[0.04, 0.1]
     = 0

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
        'Round[expr_Complex, k_RealNumberQ]': (
            'Round[Re[expr], k] + I * Round[Im[expr], k]'),
    }

    def apply(self, expr, k, evaluation):
        "Round[expr_?NumericQ, k_?NumericQ]"
        return from_sympy(round(expr.to_sympy(), k.to_sympy()))


def chop(expr, delta=10.0 ** (-10.0)):
    if isinstance(expr, Real):
        if -delta < expr.to_python() < delta:
            return Integer(0)
        # return expr
    elif isinstance(expr, Complex) and expr.get_precision() is not None:
        real, imag = expr.real, expr.imag
        if -delta < real.to_python() < delta:
            real = sympy.Integer(0)
        if -delta < imag.to_python() < delta:
            imag = sympy.Integer(0)
        if imag != 0:
            return Complex(real, imag)
        else:
            return Number.from_mp(real)
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

        delta = delta.evaluate(evaluation).get_real_value()
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


class BaseForm(Builtin):
    """
    <dl>
    <dt>'BaseForm[$expr$, $n$]'
        <dd>prints numbers in $expr$ in base $n$.
    </dl>

    >> BaseForm[33, 2]
     = 100001_2

    >> BaseForm[234, 16]
     = ea_16

    >> BaseForm[12.3, 2]
     = 1100.010011001100110011_2

    >> BaseForm[-42, 16]
     = -2a_16

    >> BaseForm[x, 2]
     = x

    >> BaseForm[12, 3] // FullForm
     = BaseForm[12, 3]

    Bases must be between 2 and 36:
    >> BaseForm[12, -3]
     : Positive machine-sized integer expected at position 2 in BaseForm[12, -3].
     : MakeBoxes[BaseForm[12, -3], OutputForm] is not a valid box structure.
    >> BaseForm[12, 100]
     : Requested base 100 must be between 2 and 36.
     : MakeBoxes[BaseForm[12, 100], OutputForm] is not a valid box structure.

    #> BaseForm[0, 2]
     = 0_2
    #> BaseForm[0.0, 2]
     = 0.0_2
    """

    messages = {
        'intpm': (
            "Positive machine-sized integer expected at position 2 in "
            "BaseForm[`1`, `2`]."),
        'basf': "Requested base `1` must be between 2 and 36.",
    }

    def apply_makeboxes(self, expr, n, f, evaluation):
        '''MakeBoxes[BaseForm[expr_, n_],
            f:StandardForm|TraditionalForm|OutputForm]'''

        base = n.get_int_value()

        if base <= 0:
            evaluation.message('BaseForm', 'intpm', expr, n)
            return

        if not (isinstance(expr, Integer) or isinstance(expr, Real)):
            return Expression("MakeBoxes", expr, f)

        p = dps(expr.get_precision()) if isinstance(expr, Real) else 0

        try:
            val = convert_base(expr.get_real_value(), base, p)
        except ValueError:
            return evaluation.message('BaseForm', 'basf', n)

        if f.get_name() == 'System`OutputForm':
            return from_python("%s_%d" % (val, base))
        else:
            return Expression(
                'SubscriptBox', from_python(val), from_python(base))

class _NumberForm(Builtin):
    '''
    Base class for NumberForm, AccountingForm, EngineeringForm, and ScientificForm.
    '''

    default_ExponentFunction = None
    default_NumberFormat = None

    messages = {
        'npad': 'Value for option NumberPadding -> `1` should be a string or a pair of strings.',
        'dblk': 'Value for option DigitBlock should be a positive integer, Infinity, or a pair of positive integers.',
        'npt': 'Value for option `1` -> `2` is expected to be a string.',
        'nsgn': 'Value for option NumberSigns -> `1` should be a pair of strings or two pairs of strings.',
        'nspr': 'Value for option NumberSeparator -> `1` should be a string or a pair of strings.',
        'opttf': 'Value of option `1` -> `2` should be True or False.',
        'estep': 'Value of option `1` -> `2` is not a positive integer.',
        'iprf': 'Formatting specification `1` should be a positive integer or a pair of positive integers.',    # NumberFormat only
        'sigz': 'In addition to the number of digits requested, one or more zeros will appear as placeholders.',
    }

    def check_options(self, options, evaluation):
        '''
        Checks options are valid and converts them to python.
        '''
        result = {}
        for option_name in self.options:
            method = getattr(self, 'check_' + option_name)
            arg = options['System`' + option_name]
            value = method(arg, evaluation)
            if value is None:
                return None
            result[option_name] = value
        return result

    def check_DigitBlock(self, value, evaluation):
        py_value = value.get_int_value()
        if value.same(Symbol('Infinity')):
            return [0, 0]
        elif py_value is not None and py_value > 0:
            return [py_value, py_value]
        elif value.has_form('List', 2):
            nleft, nright = value.leaves
            py_left, py_right = nleft.get_int_value(), nright.get_int_value()
            if nleft.same(Symbol('Infinity')):
                nleft = 0
            elif py_left is not None and py_left > 0:
                nleft = py_left
            else:
                nleft = None
            if nright.same(Symbol('Infinity')):
                nright = 0
            elif py_right is not None and py_right > 0:
                nright = py_right
            else:
                nright = None
            result = [nleft, nright]
            if None not in result:
                return result
        return evaluation.message(self.get_name(), 'dblk', value)

    def check_ExponentFunction(self, value, evaluation):
        if value.same(Symbol('Automatic')):
            return self.default_ExponentFunction
        def exp_function(x):
            return Expression(value, x).evaluate(evaluation)
        return exp_function

    def check_NumberFormat(self, value, evaluation):
        if value.same(Symbol('Automatic')):
            return self.default_NumberFormat
        def num_function(man, base, exp, options):
            return Expression(value, man, base, exp).evaluate(evaluation)
        return num_function

    def check_NumberMultiplier(self, value, evaluation):
        result = value.get_string_value()
        if result is None:
            evaluation.message(self.get_name(), 'npt', 'NumberMultiplier', value)
        return result

    def check_NumberPoint(self, value, evaluation):
        result = value.get_string_value()
        if result is None:
            evaluation.message(self.get_name(), 'npt', 'NumberPoint', value)
        return result

    def check_ExponentStep(self, value, evaluation):
        result = value.get_int_value()
        if result is None or result <= 0:
            return evaluation.message(self.get_name(), 'estep', 'ExponentStep', value)
        return result

    def check_SignPadding(self, value, evaluation):
        if value.same(Symbol('True')):
            return True
        elif value.same(Symbol('False')):
            return False
        return evaluation.message(self.get_name(), 'opttf', value)

    def _check_List2str(self, value, msg, evaluation):
        if value.has_form('List', 2):
            result = [leaf.get_string_value() for leaf in value.leaves]
            if None not in result:
                return result
        return evaluation.message(self.get_name(), msg, value)

    def check_NumberSigns(self, value, evaluation):
        return self._check_List2str(value, 'nsgn', evaluation)

    def check_NumberPadding(self, value, evaluation):
        return self._check_List2str(value, 'npad', evaluation)

    def check_NumberSeparator(self, value, evaluation):
        py_str = value.get_string_value()
        if py_str is not None:
            return [py_str, py_str]
        return self._check_List2str(value, 'nspr', evaluation)


class NumberForm(_NumberForm):
    '''
    <dl>
    <dt>'NumberForm[$expr$, $n$]'
        <dd>prints a real number $expr$ with $n$-digits of precision.
    <dt>'NumberForm[$expr$, {$n$, $f$}]'
        <dd>prints with $n$-digits and $f$ digits to the right of the decimal point.
    </dl>

    >> NumberForm[N[Pi], 10]
     = 3.141592654

    >> NumberForm[N[Pi], {10, 5}]
     = 3.14159

    ## Check arguments
    #> NumberForm[1.5, -4]
     : Formatting specification -4 should be a positive integer or a pair of positive integers.
     = 1.5
    #> NumberForm[1.5, {1.5, 2}]
     : Formatting specification {1.5, 2} should be a positive integer or a pair of positive integers.
     = 1.5
    #> NumberForm[1.5, {1, 2.5}]
     : Formatting specification {1, 2.5} should be a positive integer or a pair of positive integers.
     = 1.5

    ## Right padding
    #> NumberForm[153., 2]
     : In addition to the number of digits requested, one or more zeros will appear as placeholders.
     = 150.
    #> NumberForm[0.00125, 1]
     = 0.001
    #> NumberForm[10^5 N[Pi], {5, 3}]
     : In addition to the number of digits requested, one or more zeros will appear as placeholders.
     = 314160.000
    #> NumberForm[10^5 N[Pi], {6, 3}]
     = 314159.000
    #> NumberForm[10^5 N[Pi], {6, 10}]
     = 314159.0000000000

    ## Check options

    ## DigitBlock
    #> NumberForm[12345.123456789, 14, DigitBlock -> 3]
     = 12,345.123 456 789
    #> NumberForm[12345.12345678, 14, DigitBlock -> 3]
     = 12,345.123 456 78
    #> NumberForm[N[10^ 5 Pi], 15, DigitBlock -> {4, 2}]
     = 31,4159.26 53 58 97 9
    #> NumberForm[1.2345, 3, DigitBlock -> -4]
     : Value for option DigitBlock should be a positive integer, Infinity, or a pair of positive integers.
     = 1.2345
    #> NumberForm[1.2345, 3, DigitBlock -> x]
     : Value for option DigitBlock should be a positive integer, Infinity, or a pair of positive integers.
     = 1.2345
    #> NumberForm[1.2345, 3, DigitBlock -> {x, 3}]
     : Value for option DigitBlock should be a positive integer, Infinity, or a pair of positive integers.
     = 1.2345
    #> NumberForm[1.2345, 3, DigitBlock -> {5, -3}]
     : Value for option DigitBlock should be a positive integer, Infinity, or a pair of positive integers.
     = 1.2345

    ## ExponentFunction
    #> NumberForm[12345.123456789, 14, ExponentFunction -> ((#) &)]
     = 1.2345123456789×10^4
    #> NumberForm[12345.123456789, 14, ExponentFunction -> (Null&)]
     = 12345.123456789
    #> y = N[Pi^Range[-20, 40, 15]];
    #> NumberForm[y, 10, ExponentFunction -> (3 Quotient[#, 3] &)]
     =  {114.0256472×10^-12, 3.267763643×10^-3, 93.64804748×10^3, 2.683779414×10^12, 76.91214221×10^18}
    #> NumberForm[y, 10, ExponentFunction -> (Null &)]
     : In addition to the number of digits requested, one or more zeros will appear as placeholders.
     : In addition to the number of digits requested, one or more zeros will appear as placeholders.
     = {0.0000000001140256472, 0.003267763643, 93648.04748, 2683779414000., 76912142210000000000.}

    ## ExponentStep
    #> NumberForm[10^8 N[Pi], 10, ExponentStep -> 3]
     = 314.1592654×10^6
    #> NumberForm[1.2345, 3, ExponentStep -> x]
     : Value of option ExponentStep -> x is not a positive integer.
     = 1.2345
    #> NumberForm[1.2345, 3, ExponentStep -> 0]
     : Value of option ExponentStep -> 0 is not a positive integer.
     = 1.2345
    #> NumberForm[y, 10, ExponentStep -> 6]
     = {114.0256472×10^-12, 3267.763643×10^-6, 93648.04748, 2.683779414×10^12, 76.91214221×10^18}

    ## NumberFormat
    #> NumberForm[y, 10, NumberFormat -> (#1 &)]
     = {1.140256472, 0.003267763643, 93648.04748, 2.683779414, 7.691214221}

    ## NumberMultiplier
    #> NumberForm[1.2345, 3, NumberMultiplier -> 0]
     : Value for option NumberMultiplier -> 0 is expected to be a string.
     = 1.2345
    #> NumberForm[N[10^ 7 Pi], 15, NumberMultiplier -> "*"]
     = 3.14159265358979*10^7

    ## NumberPoint
    #> NumberForm[1.2345, 5, NumberPoint -> ","]
     = 1,2345
    #> NumberForm[1.2345, 3, NumberPoint -> 0]
     : Value for option NumberPoint -> 0 is expected to be a string.
     = 1.2345

    ## NumberPadding
    #> NumberForm[1.41, {10, 5}]
     = 1.41000
    #> NumberForm[1.41, {10, 5}, NumberPadding -> {"", "X"}]
     = 1.41XXX
    #> NumberForm[1.41, {10, 5}, NumberPadding -> {"X", "Y"}]
     = XXXXX1.41YYY
    #> NumberForm[1.41, 10, NumberPadding -> {"X", "Y"}]
     = XXXXXXXX1.41
    #> NumberForm[1.2345, 3, NumberPadding -> 0]
     :  Value for option NumberPadding -> 0 should be a string or a pair of strings.
     = 1.2345
    #> NumberForm[1.41, 10, NumberPadding -> {"X", "Y"}, NumberSigns -> {"-------------", ""}]
     = XXXXXXXXXXXXXXXXXXXX1.41

    ## NumberSeparator
    #> NumberForm[N[10^ 5 Pi], 15, DigitBlock -> 3, NumberSeparator -> " "]
     = 314 159.265 358 979
    #> NumberForm[N[10^ 5 Pi], 15, DigitBlock -> 3, NumberSeparator -> {" ", ","}]
     = 314 159.265,358,979
    #> NumberForm[N[10^ 5 Pi], 15, DigitBlock -> 3, NumberSeparator -> {",", " "}]
     = 314,159.265 358 979
    #> NumberForm[N[10^ 7 Pi], 15, DigitBlock -> 3, NumberSeparator -> {",", " "}]
     = 3.141 592 653 589 79×10^7
    #> NumberForm[1.2345, 3, NumberSeparator -> 0]
     :  Value for option NumberSeparator -> 0 should be a string or a pair of strings.
     = 1.2345

    ## NumberSigns
    #> NumberForm[1.2345, 5, NumberSigns -> {"-", "+"}]
     = +1.2345
    #> NumberForm[-1.2345, 5, NumberSigns -> {"- ", ""}]
     = - 1.2345
    #> NumberForm[1.2345, 3, NumberSigns -> 0]
     : Value for option NumberSigns -> 0 should be a pair of strings or two pairs of strings.
     = 1.2345

    ## SignPadding
    #> NumberForm[1.234, 6, SignPadding -> True, NumberPadding -> {"X", "Y"}]
     = XXX1.234
    #> NumberForm[-1.234, 6, SignPadding -> True, NumberPadding -> {"X", "Y"}]
     = -XX1.234
    #> NumberForm[-1.234, 6, SignPadding -> False, NumberPadding -> {"X", "Y"}]
     = XX-1.234
    #> NumberForm[-1.234, {6, 4}, SignPadding -> False, NumberPadding -> {"X", "Y"}]
     = X-1.234Y
    '''

    options = {
        'DigitBlock': 'Infinity',
        'ExponentFunction': 'Automatic',
        'ExponentStep': '1',
        'NumberFormat': 'Automatic',
        'NumberMultiplier': '"×"',
        'NumberPadding': '{"", "0"}',
        'NumberPoint': '"."',
        'NumberSeparator': '{",", " "}',
        'NumberSigns': '{"-", ""}',
        'SignPadding': 'False',
    }

    @staticmethod
    def default_ExponentFunction(value):
        n = value.get_int_value()
        if -5 <= n <= 5:
            return Symbol('Null')
        else:
            return value

    @staticmethod
    def default_NumberFormat(man, base, exp, options):
        py_exp = exp.get_string_value()
        if py_exp:
            mul = String(options['NumberMultiplier'])
            return Expression('RowBox', Expression('List', man, mul, Expression('SuperscriptBox', base, exp)))
        else:
            return man

    def apply_list_n(self, expr, n, evaluation, options):
        'NumberForm[expr_?ListQ, n_, OptionsPattern[NumberForm]]'
        options = [Expression('RuleDelayed', Symbol(key), value) for key, value in options.items()]
        return Expression('List', *[Expression('NumberForm', leaf, n, *options) for leaf in expr.leaves])

    def apply_list_nf(self, expr, n, f, evaluation, options):
        'NumberForm[expr_?ListQ, {n_, f_}, OptionsPattern[NumberForm]]'
        options = [Expression('RuleDelayed', Symbol(key), value) for key, value in options.items()]
        return Expression('List', *[Expression('NumberForm', leaf, n, f, *options) for leaf in expr.leaves])

    def apply_makeboxes_n(self, expr, n, form, evaluation, options={}):
        '''MakeBoxes[NumberForm[expr_, n_, OptionsPattern[NumberForm]],
            form:StandardForm|TraditionalForm|OutputForm]'''

        fallback = Expression('MakeBoxes', expr, form)

        py_n = n.get_int_value()
        if py_n is None or py_n <= 0:
            evaluation.message('NumberForm', 'iprf', n)
            return fallback

        py_options = self.check_options(options, evaluation)
        if py_options is None:
            return fallback

        if not isinstance(expr, Real):
            # TODO
            return
        return self.do_makeboxes_real(expr, py_n, None, evaluation, py_options)

    def apply_makeboxes_nf(self, expr, n, f, form, evaluation, options={}):
        '''MakeBoxes[NumberForm[expr_, {n_, f_}, OptionsPattern[NumberForm]],
            form:StandardForm|TraditionalForm|OutputForm]'''

        fallback = Expression('MakeBoxes', expr, form)

        nf = Expression('List', n, f)
        py_n = n.get_int_value()
        py_f = f.get_int_value()
        if py_n is None or py_n <= 0 or py_f is None or py_f < 0:
            evaluation.message('NumberForm', 'iprf', nf)
            return fallback

        py_options = self.check_options(options, evaluation)
        if py_options is None:
            return fallback

        if not isinstance(expr, Real):
            # TODO
            return
        return self.do_makeboxes_real(expr, py_n, py_f, evaluation, py_options)

    @staticmethod
    def do_makeboxes_real(expr, n, f, evaluation, options):
        assert isinstance(expr, Real)
        assert isinstance(n, int) and n > 0
        assert f is None or (isinstance(f, int) and f >= 0)

        sym_expr = expr.to_sympy()
        if sym_expr == sympy.Float(0):
            raise NotImplementedError()
        else:
            s = str(sym_expr.n(n))

            # sign prefix
            if s[0] == '-':
                assert sym_expr < 0
                sign_prefix = 0
                s = s[1:]
            else:
                assert sym_expr >= 0
                sign_prefix = 1
            sign_prefix = options['NumberSigns'][sign_prefix]

            # exponent (exp is actual, pexp is printed)
            if 'e' in s:
                s, exp = s.split('e')
                exp = int(exp)
                assert s[1] == '.'
                s = s[0] + s[2:]
            else:
                exp = s.index('.') - 1
                s = s[:exp + 1] + s[exp + 2:]

                # consume leading '0's.
                i = 0
                while s[i] == '0':
                    i += 1
                    exp -= 1
                s = s[i:]

            # round exponent to ExponentStep
            rexp = (exp // options['ExponentStep']) * options['ExponentStep']

            method = options['ExponentFunction']
            pexp = method(Integer(rexp)).get_int_value()
            if pexp is not None:
                exp -= pexp
                pexp = str(pexp)
            else:
                pexp = ''

            s = s.rstrip('0')

            # pad right with '0'.
            if len(s) < exp + 1:
                evaluation.message('NumberForm', 'sigz')
                # TODO NumberPadding?
                s = s + '0' * (1 + exp - len(s))
            # pad left with '0'.
            if exp < 0:
                s = '0' * (-exp) + s
                exp = 0

            # left and right of NumberPoint
            left, right = s[:exp + 1], s[exp + 1:]

            def split_string(s, start, step):
                if start > 0:
                    yield s[:start]
                for i in range(start, len(s), step):
                    yield s[i:i+step]

            # insert NumberSeparator
            digit_block = options['DigitBlock']
            if digit_block[0] != 0:
                left = split_string(left, len(left) % digit_block[0], digit_block[0])
                left = options['NumberSeparator'][0].join(left)
            if digit_block[1] != 0:
                right = split_string(right, 0, digit_block[1])
                right = options['NumberSeparator'][1].join(right)

            # pad with NumberPadding
            if f is not None:
                if len(right) < f:
                    # pad right
                    right = right + (f - len(right)) * options['NumberPadding'][1]
                elif len(right) > f:
                    # truncate right
                    right = right[:f]
            left_padding = ''
            l = len(sign_prefix) + len(left) + len(right) - max(len(options['NumberSigns'][0]), len(options['NumberSigns'][0]))
            if l < n:
                # pad left
                left_padding = (n - l) * options['NumberPadding'][0]

            # insert NumberPoint
            if options['SignPadding']:
                prefix = sign_prefix + left_padding
            else:
                prefix = left_padding + sign_prefix

            s = prefix + left + options['NumberPoint'] + right

            # base
            base = '10'

            # build number
            method = options['NumberFormat']
            return method(String(s), String(base), String(pexp), options)


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
