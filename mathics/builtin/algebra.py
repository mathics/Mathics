#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Integer, Symbol
from mathics.core.convert import from_sympy, sympy_symbol_prefix

import sympy
import mpmath
from six.moves import range


def sympy_factor(expr_sympy):
    try:
        result = sympy.together(expr_sympy)
        numer, denom = result.as_numer_denom()
        if denom == 1:
            result = sympy.factor(expr_sympy)
        else:
            result = sympy.factor(numer) / sympy.factor(denom)
    except sympy.PolynomialError:
        return expr_sympy
    return result


def cancel(expr):
    if expr.has_form('Plus', None):
        return Expression('Plus', *[cancel(leaf) for leaf in expr.leaves])
    else:
        try:
            result = expr.to_sympy()
            if result is None:
                return None

            # result = sympy.powsimp(result, deep=True)
            result = sympy.cancel(result)

            # cancel factors out rationals, so we factor them again
            result = sympy_factor(result)

            return from_sympy(result)
        except sympy.PolynomialError:
            # e.g. for non-commutative expressions
            return expr


def expand(expr, numer=True, denom=False, deep=False, **kwargs):

    if kwargs['modulus'] is not None and kwargs['modulus'] <= 0:
        return Integer(0)

    sub_exprs = []

    def store_sub_expr(expr):
        sub_exprs.append(expr)
        result = sympy.Symbol(sympy_symbol_prefix + str(len(sub_exprs) - 1))
        return result

    def get_sub_expr(expr):
        name = expr.get_name()
        assert isinstance(expr, Symbol) and name.startswith('System`')
        i = int(name[len('System`'):])
        return sub_exprs[i]

    def convert_sympy(expr):
        "converts top-level to sympy"
        leaves = expr.get_leaves()
        if isinstance(expr, Integer):
            return sympy.Integer(expr.get_int_value())
        if expr.has_form('Power', 2):
            # sympy won't expand `(a + b) / x` to `a / x + b / x` if denom is False
            # if denom is False we store negative powers to prevent this.
            n1 = leaves[1].get_int_value()
            if not denom and n1 is not None and n1 < 0:
                return store_sub_expr(expr)
            return sympy.Pow(*[convert_sympy(leaf) for leaf in leaves])
        elif expr.has_form('Times', 2, None):
            return sympy.Mul(*[convert_sympy(leaf) for leaf in leaves])
        elif expr.has_form('Plus', 2, None):
            return sympy.Add(*[convert_sympy(leaf) for leaf in leaves])
        else:
            return store_sub_expr(expr)

    def unconvert_subexprs(expr):
        if expr.is_atom():
            if isinstance(expr, Symbol):
                return get_sub_expr(expr)
            else:
                return expr
        else:
            return Expression(expr.head, *[unconvert_subexprs(leaf) for leaf in expr.get_leaves()])

    sympy_expr = convert_sympy(expr)

    def _expand(expr):
        return expand(expr, numer=numer, denom=denom, deep=deep, **kwargs)

    if deep:
        # thread over everything
        for i, sub_expr,in enumerate(sub_exprs):
            if not sub_expr.is_atom():
                head = _expand(sub_expr.head)    # also expand head
                leaves = sub_expr.get_leaves()
                leaves = [_expand(leaf) for leaf in leaves]
                sub_exprs[i] = Expression(head, *leaves)
    else:
        # thread over Lists etc.
        threaded_heads = ('List', 'Rule')
        for i, sub_expr in enumerate(sub_exprs):
            for head in threaded_heads:
                if sub_expr.has_form(head, None):
                    leaves = sub_expr.get_leaves()
                    leaves = [_expand(leaf) for leaf in leaves]
                    sub_exprs[i] = Expression(head, *leaves)
                    break

    hints = {
        'mul': True,
        'multinomial': True,
        'power_exp': False,
        'power_base': False,
        'basic': False,
        'log': False,
    }

    hints.update(kwargs)

    if numer and denom:
        # don't expand fractions when modulus is True
        if hints['modulus'] is not None:
            hints['frac'] = True
    else:
        # setting both True doesn't expand denom
        hints['numer'] = numer
        hints['denom'] = denom

    sympy_expr = sympy_expr.expand(**hints)
    result = from_sympy(sympy_expr)
    result = unconvert_subexprs(result)

    return result


class Cancel(Builtin):
    """
    <dl>
    <dt>'Cancel[$expr$]'
        <dd>cancels out common factors in numerators and denominators.
    </dl>

    >> Cancel[x / x ^ 2]
     = 1 / x
    'Cancel' threads over sums:
    >> Cancel[x / x ^ 2 + y / y ^ 2]
     = 1 / x + 1 / y

    >> Cancel[f[x] / x + x * f[x] / x ^ 2]
     = 2 f[x] / x
    """

    def apply(self, expr, evaluation):
        'Cancel[expr_]'

        return cancel(expr)


class Simplify(Builtin):
    """
    <dl>
    <dt>'Simplify[$expr$]'
        <dd>simplifies $expr$.
    </dl>

    >> Simplify[2*Sin[x]^2 + 2*Cos[x]^2]
     = 2
    >> Simplify[x]
     = x
    >> Simplify[f[x]]
     = f[x]

    #> Simplify[a*x^2+b*x^2]
     = x ^ 2 (a + b)

    ## triggers TypeError in sympy.simplify
    #> x f[{y}] // Simplify
     = x f[{y}]
    """

    rules = {
        'Simplify[list_List]': 'Simplify /@ list',
        'Simplify[rule_Rule]': 'Simplify /@ rule',
        'Simplify[eq_Equal]': 'Simplify /@ eq',
    }

    def apply(self, expr, evaluation):
        'Simplify[expr_]'

        sympy_expr = expr.to_sympy()
        if sympy_expr is None:
            return
        sympy_result = sympy.simplify(sympy_expr)
        return from_sympy(sympy_result)


class Together(Builtin):
    """
    <dl>
    <dt>'Together[$expr$]'
        <dd>writes sums of fractions in $expr$ together.
    </dl>

    >> Together[a / c + b / c]
     = (a + b) / c
    'Together' operates on lists:
    >> Together[{x / (y+1) + x / (y+1)^2}]
     = {x (2 + y) / (1 + y) ^ 2}
    But it does not touch other functions:
    >> Together[f[a / c + b / c]]
     = f[a / c + b / c]

    #> f[x]/x+f[x]/x^2//Together
     = f[x] (1 + x) / x ^ 2
    """

    attributes = ['Listable']

    def apply(self, expr, evaluation):
        'Together[expr_]'

        expr_sympy = expr.to_sympy()
        if expr_sympy is None:
            return None
        result = sympy.together(expr_sympy)
        result = from_sympy(result)
        result = cancel(result)
        return result


class Factor(Builtin):
    """
    <dl>
    <dt>'Factor[$expr$]'
        <dd>factors the polynomial expression $expr$.
    </dl>

    >> Factor[x ^ 2 + 2 x + 1]
     = (1 + x) ^ 2

    >> Factor[1 / (x^2+2x+1) + 1 / (x^4+2x^2+1)]
     = (2 + 2 x + 3 x ^ 2 + x ^ 4) / ((1 + x) ^ 2 (1 + x ^ 2) ^ 2)
    """

    def apply(self, expr, evaluation):
        'Factor[expr_]'

        expr_sympy = expr.to_sympy()
        if expr_sympy is None:
            return None

        try:
            result = sympy.together(expr_sympy)
            numer, denom = result.as_numer_denom()
            if denom == 1:
                result = sympy.factor(expr_sympy)
            else:
                result = sympy.factor(numer) / sympy.factor(denom)
        except sympy.PolynomialError:
            return expr
        return from_sympy(result)


class Apart(Builtin):
    """
    <dl>
    <dt>'Apart[$expr$]'
        <dd>writes $expr$ as a sum of individual fractions.
    <dt>'Apart[$expr$, $var$]'
        <dd>treats $var$ as the main variable.
    </dl>

    >> Apart[1 / (x^2 + 5x + 6)]
     = 1 / (2 + x) - 1 / (3 + x)

    When several variables are involved, the results can be different
    depending on the main variable:
    >> Apart[1 / (x^2 - y^2), x]
     = -1 / (2 y (x + y)) + 1 / (2 y (x - y))
    >> Apart[1 / (x^2 - y^2), y]
     = 1 / (2 x (x + y)) + 1 / (2 x (x - y))

    'Apart' is 'Listable':
    >> Apart[{1 / (x^2 + 5x + 6)}]
     = {1 / (2 + x) - 1 / (3 + x)}

    But it does not touch other expressions:
    >> Sin[1 / (x ^ 2 - y ^ 2)] // Apart
     = Sin[1 / (x ^ 2 - y ^ 2)]

    #> Attributes[f] = {HoldAll}; Apart[f[x + x]]
     = f[x + x]

    #> Attributes[f] = {}; Apart[f[x + x]]
     = f[2 x]
    """

    attributes = ['Listable']
    rules = {
        'Apart[expr_]': (
            'Block[{vars = Cases[Level[expr, {-1}], _Symbol]},'
            '  If[Length[vars] > 0, Apart[expr, vars[[1]]], expr]]'),
    }

    def apply(self, expr, var, evaluation):
        'Apart[expr_, var_Symbol]'

        expr_sympy = expr.to_sympy()
        var_sympy = var.to_sympy()
        if expr_sympy is None or var_sympy is None:
            return None

        try:
            result = sympy.apart(expr_sympy, var_sympy)
            result = from_sympy(result)
            return result
        except sympy.PolynomialError:
            # raised e.g. for apart(sin(1/(x**2-y**2)))
            return expr


class _Expand(Builtin):

    options = {
        'Trig': 'False',
        'Modulus': '0',
    }

    messages = {
        'modn': 'Value of option `1` -> `2` should be an integer.',
        'opttf': 'Value of option `1` -> `2` should be True or False.',
    }

    def convert_options(self, options, evaluation):
        modulus = options['System`Modulus']
        py_modulus = modulus.get_int_value()
        if py_modulus is None:
            return evaluation.message(self.get_name(), 'modn', Symbol('Modulus'), modulus)
        if py_modulus == 0:
            py_modulus = None

        trig = options['System`Trig']
        if trig == Symbol('True'):
            py_trig = True
        elif trig == Symbol('False'):
            py_trig = False
        else:
            return evaluation.message(self.get_name(), 'opttf', Symbol('Trig'), trig)

        return {'modulus': py_modulus, 'trig': py_trig}


class Expand(_Expand):
    """
    <dl>
    <dt>'Expand[$expr$]'
        <dd>expands out positive integer powers and products of sums in $expr$.
    </dl>

    >> Expand[(x + y) ^ 3]
     = x ^ 3 + 3 x ^ 2 y + 3 x y ^ 2 + y ^ 3
    >> Expand[(a + b) (a + c + d)]
     = a ^ 2 + a b + a c + a d + b c + b d
    >> Expand[(a + b) (a + c + d) (e + f) + e a a]
     = 2 a ^ 2 e + a ^ 2 f + a b e + a b f + a c e + a c f + a d e + a d f + b c e + b c f + b d e + b d f
    >> Expand[(a + b) ^ 2 * (c + d)]
     = a ^ 2 c + a ^ 2 d + 2 a b c + 2 a b d + b ^ 2 c + b ^ 2 d
    >> Expand[(x + y) ^ 2 + x y]
     = x ^ 2 + 3 x y + y ^ 2
    >> Expand[((a + b) (c + d)) ^ 2 + b (1 + a)]
     = a ^ 2 c ^ 2 + 2 a ^ 2 c d + a ^ 2 d ^ 2 + b + a b + 2 a b c ^ 2 + 4 a b c d + 2 a b d ^ 2 + b ^ 2 c ^ 2 + 2 b ^ 2 c d + b ^ 2 d ^ 2

    'Expand' expands items in lists and rules:
    >> Expand[{4 (x + y), 2 (x + y) -> 4 (x + y)}]
     = {4 x + 4 y, 2 x + 2 y -> 4 x + 4 y}

    'Expand' does not change any other expression.
    >> Expand[Sin[x (1 + y)]]
     = Sin[x (1 + y)]

    'Expand' also works in Galois fields
    >> Expand[(1 + a)^12, Modulus -> 3]
     = 1 + a ^ 3 + a ^ 9 + a ^ 12

    >> Expand[(1 + a)^12, Modulus -> 4]
     = 1 + 2 a ^ 2 + 3 a ^ 4 + 3 a ^ 8 + 2 a ^ 10 + a ^ 12

    #> Expand[x, Modulus -> -1]  (* copy odd MMA behaviour *)
     = 0
    #> Expand[x, Modulus -> x]
     : Value of option Modulus -> x should be an integer.
     = Expand[x, Modulus -> x]

    #> a(b(c+d)+e) // Expand
     = a b c + a b d + a e

    #> (y^2)^(1/2)/(2x+2y)//Expand
     = Sqrt[y ^ 2] / (2 x + 2 y)

    ## This caused a program crash!
    #> 2(3+2x)^2/(5+x^2+3x)^3 // Expand
     = 24 x / (5 + 3 x + x ^ 2) ^ 3 + 8 x ^ 2 / (5 + 3 x + x ^ 2) ^ 3 + 18 / (5 + 3 x + x ^ 2) ^ 3
    """

    # TODO unwrap trig expressions in expand() so the following works
    """
    >> Expand[Sin[x + y], Trig -> True]
     = Cos[y] Sin[x] + Cos[x] Sin[y]
    """

    def apply(self, expr, evaluation, options):
        'Expand[expr_, OptionsPattern[Expand]]'

        kwargs = self.convert_options(options, evaluation)
        if kwargs is None:
            return
        return expand(expr, True, False, **kwargs)


class ExpandDenominator(_Expand):
    """
    <dl>
    <dt>'ExpandDenominator[$expr$]'
        <dd>expands out negative integer powers and products of sums in $expr$.
    </dl>

    >> ExpandDenominator[(a + b) ^ 2 / ((c + d)^2 (e + f))]
     = (a + b) ^ 2 / (c ^ 2 e + c ^ 2 f + 2 c d e + 2 c d f + d ^ 2 e + d ^ 2 f)

    ## Modulus option
    #> ExpandDenominator[1 / (x + y)^3, Modulus -> 3]
     = 1 / (x ^ 3 + y ^ 3)
    #> ExpandDenominator[1 / (x + y)^6, Modulus -> 4]
     = 1 / (x ^ 6 + 2 x ^ 5 y + 3 x ^ 4 y ^ 2 + 3 x ^ 2 y ^ 4 + 2 x y ^ 5 + y ^ 6)

    #> ExpandDenominator[2(3+2x)^2/(5+x^2+3x)^3]
     = 2 (3 + 2 x) ^ 2 / (125 + 225 x + 210 x ^ 2 + 117 x ^ 3 + 42 x ^ 4 + 9 x ^ 5 + x ^ 6)
    """

    def apply(self, expr, evaluation, options):
        'ExpandDenominator[expr_, OptionsPattern[ExpandDenominator]]'

        kwargs = self.convert_options(options, evaluation)
        if kwargs is None:
            return
        return expand(expr, False, True, **kwargs)


class ExpandAll(_Expand):
    """
    <dl>
    <dt>'ExpandAll[$expr$]'
        <dd>expands out negative integer powers and products of sums in $expr$.
    </dl>

    >> ExpandAll[(a + b) ^ 2 / (c + d)^2]
     = a ^ 2 / (c ^ 2 + 2 c d + d ^ 2) + 2 a b / (c ^ 2 + 2 c d + d ^ 2) + b ^ 2 / (c ^ 2 + 2 c d + d ^ 2)

    'ExpandAll' descends into sub expressions
    >> ExpandAll[(a + Sin[x (1 + y)])^2]
     = 2 a Sin[x + x y] + a ^ 2 + Sin[x + x y] ^ 2

    'ExpandAll' also expands heads
    >> ExpandAll[((1 + x)(1 + y))[x]]
     = (1 + x + y + x y)[x]

    'ExpandAll' can also work in finite fields
    >> ExpandAll[(1 + a) ^ 6 / (x + y)^3, Modulus -> 3]
     = (1 + 2 a ^ 3 + a ^ 6) / (x ^ 3 + y ^ 3)
    """

    def apply(self, expr, evaluation, options):
        'ExpandAll[expr_, OptionsPattern[ExpandAll]]'

        kwargs = self.convert_options(options, evaluation)
        if kwargs is None:
            return
        return expand(expr, numer=True, denom=True, deep=True, **kwargs)


class PowerExpand(Builtin):
    """
    <dl>
    <dt>'PowerExpand[$expr$]'
        <dd>expands out powers of the form '(x^y)^z' and '(x*y)^z' in $expr$.
    </dl>

    >> PowerExpand[(a ^ b) ^ c]
     = a ^ (b c)
    >> PowerExpand[(a * b) ^ c]
     = a ^ c b ^ c

    'PowerExpand' is not correct without certain assumptions:
    >> PowerExpand[(x ^ 2) ^ (1/2)]
     = x
    """

    rules = {
        'PowerExpand[(x_ ^ y_) ^ z_]': 'x ^ (y * z)',
        'PowerExpand[(x_ * y_) ^ z_]': 'x ^ z * y ^ z',
        'PowerExpand[Log[x_ ^ y_]]': 'y * Log[x]',
        'PowerExpand[x_Plus]': 'PowerExpand /@ x',
        'PowerExpand[x_Times]': 'PowerExpand /@ x',
        'PowerExpand[x_Power]': 'PowerExpand /@ x',
        'PowerExpand[x_List]': 'PowerExpand /@ x',
        'PowerExpand[x_Rule]': 'PowerExpand /@ x',
        'PowerExpand[other_]': 'other',
    }


class Numerator(Builtin):
    """
    <dl>
    <dt>'Numerator[$expr$]'
        <dd>gives the numerator in $expr$.
    </dl>

    >> Numerator[a / b]
     = a
    >> Numerator[2 / 3]
     = 2
    >> Numerator[a + b]
     = a + b
    """

    def apply(self, expr, evaluation):
        'Numerator[expr_]'

        sympy_expr = expr.to_sympy()
        if sympy_expr is None:
            return None
        numer, denom = sympy_expr.as_numer_denom()
        return from_sympy(numer)


class Denominator(Builtin):
    """
    <dl>
    <dt>'Denominator[$expr$]'
        <dd>gives the denominator in $expr$.
    </dl>

    >> Denominator[a / b]
     = b
    >> Denominator[2 / 3]
     = 3
    >> Denominator[a + b]
     = 1
    """

    def apply(self, expr, evaluation):
        'Denominator[expr_]'

        sympy_expr = expr.to_sympy()
        if sympy_expr is None:
            return None
        numer, denom = sympy_expr.as_numer_denom()
        return from_sympy(denom)


class Variables(Builtin):
    # This builtin is incomplete. See the failing test case below.
    """
    <dl>
    <dt>'Variables[$expr$]'
        <dd>gives a list of the variables that appear in the
        polynomial $expr$.
    </dl>

    >> Variables[a x^2 + b x + c]
     = {a, b, c, x}
    >> Variables[{a + b x, c y^2 + x/2}]
     = {a, b, c, x, y}
    >> Variables[x + Sin[y]]
     = {x, Sin[y]}
    """

    """
    ## failing test case from MMA docs
    #> Variables[E^x]
     = {}
    """

    def apply(self, expr, evaluation):
        'Variables[expr_]'

        variables = set()

        def find_vars(e, e_sympy):
            assert e_sympy is not None
            if e_sympy.is_constant():
                return
            elif e.is_symbol():
                variables.add(e)
            elif (e.has_form('Plus', None) or
                  e.has_form('Times', None)):
                for l in e.leaves:
                    l_sympy = l.to_sympy()
                    if l_sympy is not None:
                        find_vars(l, l_sympy)
            elif e.has_form('Power', 2):
                (a, b) = e.leaves  # a^b
                a_sympy, b_sympy = a.to_sympy(), b.to_sympy()
                if a_sympy is None or b_sympy is None:
                    return
                if not(a_sympy.is_constant()) and b_sympy.is_rational:
                    find_vars(a, a_sympy)
            elif not(e.is_atom()):
                variables.add(e)

        exprs = expr.leaves if expr.has_form('List', None) else [expr]
        for e in exprs:
            e_sympy = e.to_sympy()
            if e_sympy is not None:
                find_vars(e, e_sympy)

        variables = Expression('List', *variables)
        variables.sort()        # MMA doesn't do this
        return variables


class UpTo(Builtin):
    messages = {
        'innf': 'Expected non-negative integer or infinity at position 1 in ``.',
        'argx': 'UpTo expects 1 argument, `1` arguments were given.'
    }


class Missing(Builtin):
    pass
