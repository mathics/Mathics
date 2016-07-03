#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

import sympy

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Integer, Symbol
from mathics.builtin.arithmetic import _MPMathFunction


class Fibonacci(Builtin):
    """
    <dl>
    <dt>'Fibonacci[$n$]'
        <dd>computes the $n$th Fibonacci number.
    </dl>

    >> Fibonacci[0]
     = 0
    >> Fibonacci[1]
     = 1
    >> Fibonacci[10]
     = 55
    >> Fibonacci[200]
     = 280571172992510140037611932413038677189525
    """

    attributes = ('Listable', 'NumericFunction', 'ReadProtected')

    def apply(self, n, evaluation):
        'Fibonacci[n_Integer]'

        return Integer(sympy.fibonacci(n.get_int_value()))


class Binomial(_MPMathFunction):
    """
    <dl>
    <dt>'Binomial[$n$, $k$]'
        <dd>gives the binomial coefficient $n$ choose $k$.
    </dl>

    >> Binomial[5, 3]
     = 10

    'Binomial' supports inexact numbers:
    >> Binomial[10.5,3.2]
     = 165.286109367256421

    Some special cases:
    >> Binomial[10, -2]
     = 0
    >> Binomial[-10.5, -3.5]
     = 0.
    >> Binomial[-10, -3.5]
     = ComplexInfinity
    """

    attributes = ('Listable', 'NumericFunction')

    nargs = 2
    sympy_name = 'binomial'
    mpmath_name = 'binomial'


class Multinomial(Builtin):
    """
    <dl>
    <dt>'Multinomial[$n1$, $n2$, ...]'
        <dd>gives the multinomial coefficient '($n1$+$n2$+...)!/($n1$!$n2$!...)'.
    </dl>

    >> Multinomial[2, 3, 4, 5]
     = 2522520
    >> Multinomial[]
     = 1
    Multinomial is expressed in terms of 'Binomial':
    >> Multinomial[a, b, c]
     = Binomial[a + b, b] Binomial[a + b + c, c]
    'Multinomial[$n$-$k$, $k$]' is equivalent to 'Binomial[$n$, $k$]'.
    >> Multinomial[2, 3]
     = 10
    """

    attributes = ('Listable', 'NumericFunction', 'Orderless')

    def apply(self, values, evaluation):
        'Multinomial[values___]'

        values = values.get_sequence()
        result = Expression('Times')
        total = []
        for value in values:
            total.append(value)
            result.leaves.append(Expression(
                'Binomial', Expression('Plus', *total), value))
        return result


class _BooleanDissimilarity(Builtin):
    @staticmethod
    def _to_bool_vector(u):
        class NoBoolVector(Exception):
            pass

        def generate():
            for leaf in u.leaves:
                if isinstance(leaf, Integer):
                    val = leaf.get_int_value()
                    if val in (0, 1):
                        yield val
                    else:
                        raise NoBoolVector
                elif isinstance(leaf, Symbol):
                    name = leaf.name
                    if name == 'System`True':
                        yield 1
                    elif name == 'System`False':
                        yield 0
                    else:
                        raise NoBoolVector
                else:
                    raise NoBoolVector

        try:
            return [x for x in generate()]
        except NoBoolVector:
            return None

    def apply(self, u, v, evaluation):
        '%(name)s[u_List, v_List]'
        if len(u.leaves) != len(v.leaves):
            return
        py_u = _BooleanDissimilarity._to_bool_vector(u)
        if py_u is None:
            return
        py_v = _BooleanDissimilarity._to_bool_vector(v)
        if py_v is None:
            return
        counts = [0, 0, 0, 0]
        for a, b in zip(py_u, py_v):
            counts[(a << 1) + b] += 1
        return self._compute(len(py_u), *counts)


class MatchingDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'MatchingDissimilarity[$u$, $v$]'
      <dd>returns the Matching dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as (c_tf + c_ft) / n, where n is len($u$) and c_ij is the number of
      occurrences of $u$[k]=i and $v$[k]=j for k<n.
    </dl>

    >> MatchingDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 4 / 7
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        return Expression('Divide', c_tf + c_ft, n)


class JaccardDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'JaccardDissimilarity[$u$, $v$]'
      <dd>returns the Jaccard-Needham dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as (c_tf + c_ft) / (c_tt + c_ft + c_tf), where n is len($u$) and c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n.
    </dl>

    >> JaccardDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 2 / 3
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        return Expression('Divide', c_tf + c_ft, c_tt + c_ft + c_tf)


class DiceDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'DiceDissimilarity[$u$, $v$]'
      <dd>returns the Dice dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as (c_tf + c_ft) / (2 * c_tt + c_ft + c_tf), where n is len($u$) and c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n.
    </dl>

    >> DiceDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 1 / 2
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        return Expression('Divide', c_tf + c_ft, 2 * c_tt + c_ft + c_tf)


class YuleDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'YuleDissimilarity[$u$, $v$]'
      <dd>returns the Yule dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as R / (c_tt * c_ff + R / 2) where n is len($u$), c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n, and R = 2 * c_tf * c_ft.
    </dl>

    >> YuleDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 6 / 5
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        r_half = c_tf * c_ft
        return Expression('Divide', 2 * r_half, c_tt * c_ff + r_half)


class SokalSneathDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'SokalSneathDissimilarity[$u$, $v$]'
      <dd>returns the Sokal-Sneath dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as R / (c_tt + R) where n is len($u$), c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n, and R = 2 * (c_tf + c_ft).
    </dl>

    >> SokalSneathDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 4 / 5
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        r = 2 * (c_tf + c_ft)
        return Expression('Divide', r, c_tt + r)


class RussellRaoDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'RussellRaoDissimilarity[$u$, $v$]'
      <dd>returns the Russell-Rao dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as (n - c_tt) / c_tt where n is len($u$) and c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n.
    </dl>

    >> RussellRaoDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 5 / 7
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        return Expression('Divide', n - c_tt, n)


class RogersTanimotoDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'RogersTanimotoDissimilarity[$u$, $v$]'
      <dd>returns the Rogers-Tanimoto dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as R / (c_tt + c_ff + R) where n is len($u$), c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n, and R = 2 * (c_tf + c_ft).
    </dl>

    >> RogersTanimotoDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 8 / 11
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        r = 2 * (c_tf + c_ft)
        return Expression('Divide', r, c_tt + c_ff + r)
