#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

import sympy

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Integer, Symbol
from mathics.builtin.arithmetic import _MPMathFunction
from itertools import combinations

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
     = 165.286

    Some special cases:
    >> Binomial[10, -2]
     = 0
    >> Binomial[-10.5, -3.5]
     = 0.

    ## TODO should be ComplexInfinity but mpmath returns +inf
    #> Binomial[-10, -3.5]
     = Infinity
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


class _NoBoolVector(Exception):
    pass


class _BooleanDissimilarity(Builtin):
    @staticmethod
    def _to_bool_vector(u):

        def generate():
            for leaf in u.leaves:
                if isinstance(leaf, Integer):
                    val = leaf.get_int_value()
                    if val in (0, 1):
                        yield val
                    else:
                        raise _NoBoolVector
                elif isinstance(leaf, Symbol):
                    name = leaf.name
                    if name == 'System`True':
                        yield 1
                    elif name == 'System`False':
                        yield 0
                    else:
                        raise _NoBoolVector
                else:
                    raise _NoBoolVector

        try:
            return [x for x in generate()]
        except _NoBoolVector:
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
    
class Subsets(Builtin):
    """
    <dl>
    <dt>'Subsets[$list$]'
        <dd>finds a list of all possible subsets of $list$.
        
    <dt>'Subsets[$list$, $n$]'
        <dd>finds a list of all possible subsets containing at most $n$ elements.
        
    <dt>'Subsets[$list$, {$n$}]'
        <dd>finds a list of all possible subsets containing exactly $n$ elements.
        
    <dt>'Subsets[$list$, {$min$, $max$}]'
        <dd>finds a list of all possible subsets containing between $min$ and $max$ elements.
        
    <dt>'Subsets[$list$, $spec$, $n$]'
        <dd>finds a list of the first $n$ possible subsets.
        
    <dt>'Subsets[$list$, $spec$, {$n$}]'
        <dd>finds the $n$th possible subset.
    </dl>
    
    >> Subsets[{a, b, c}]
     = {{}, {a}, {b}, {c}, {a, b}, {a, c}, {b, c}, {a, b, c}}
     
    >> Subsets[{a, b, c}, 2]
     = {{}, {a}, {b}, {c}, {a, b}, {a, c}, {b, c}}
     
    >> Subsets[{a, b, c}, {2}]
     = {{a, b}, {a, c}, {b, c}}
     
    #> Subsets[{a, b, c, d, e}, {3}, 5]
     = {{a, b, c}, {a, b, d}, {a, b, e}, {a, c, d}, {a, c, e}}
    
    #> Subsets[{a, b, c, d, e}, {0, 5, 2}]
     = {{}, {a, b}, {a, c}, {a, d}, {a, e}, {b, c}, {b, d}, {b, e}, {c, d}, {c, e}, {d, e}, {a, b, c, d}, {a, b, c, e}, {a, b, d, e}, {a, c, d, e}, {b, c, d, e}}
        
    #> Subsets[Range[5], All, {25}]
     = {{2, 4, 5}}
     
    #> Subsets[{a, b, c, d}, All, {15, 1, -2}]
     = {{b, c, d}, {a, b, d}, {c, d}, {b, c}, {a, c}, {d}, {b}, {}}
    
    #> Subsets[{}]
     = {{}}
     
    #> Subsets[]
     = Subsets[]
     
    #> Subsets[{a, b, c}, 2.5]
     : Position 2 of Subsets[{a, b, c}, 2.5] must be All, Infinity, a non-negative integer, or a List whose first element (required) is a non-negative integer, second element (optional) is a non-negative integer or Infinity, and third element (optional) is a nonzero integer
     = Subsets[{a, b, c}, 2.5]
     
    #> Subsets[{a, b, c}, -1]
     : Position 2 of Subsets[{a, b, c}, -1] must be All, Infinity, a non-negative integer, or a List whose first element (required) is a non-negative integer, second element (optional) is a non-negative integer or Infinity, and third element (optional) is a nonzero integer
     = Subsets[{a, b, c}, -1]
    
    #> Subsets[{a, b, c}, {3, 4, 5, 6}]
     : Position 2 of Subsets[{a, b, c}, {3, 4, 5, 6}] must be All, Infinity, a non-negative integer, or a List whose first element (required) is a non-negative integer, second element (optional) is a non-negative integer or Infinity, and third element (optional) is a nonzero integer
     = Subsets[{a, b, c}, {3, 4, 5, 6}]
     
    #> Subsets[{a, b, c}, {-1, 2}]
     : Position 2 of Subsets[{a, b, c}, {-1, 2}] must be All, Infinity, a non-negative integer, or a List whose first element (required) is a non-negative integer, second element (optional) is a non-negative integer or Infinity, and third element (optional) is a nonzero integer
     = Subsets[{a, b, c}, {-1, 2}]
    
    #> Subsets[{a, b, c}, All]
     = {{}, {a}, {b}, {c}, {a, b}, {a, c}, {b, c}, {a, b, c}}
     
    #> Subsets[{a, b, c}, Infinity]
     = {{}, {a}, {b}, {c}, {a, b}, {a, c}, {b, c}, {a, b, c}}
     
    #> Subsets[{a, b, c}, ALL]
     : Position 2 of Subsets[{a, b, c}, ALL] must be All, Infinity, a non-negative integer, or a List whose first element (required) is a non-negative integer, second element (optional) is a non-negative integer or Infinity, and third element (optional) is a nonzero integer
     = Subsets[{a, b, c}, ALL]
     
    #> Subsets[{a, b, c}, {a}]
     : Position 2 of Subsets[{a, b, c}, {a}] must be All, Infinity, a non-negative integer, or a List whose first element (required) is a non-negative integer, second element (optional) is a non-negative integer or Infinity, and third element (optional) is a nonzero integer
     = Subsets[{a, b, c}, {a}]
    
    #> Subsets[{a, b, c}, {}]
     : Position 2 of Subsets[{a, b, c}, {}] must be All, Infinity, a non-negative integer, or a List whose first element (required) is a non-negative integer, second element (optional) is a non-negative integer or Infinity, and third element (optional) is a nonzero integer
     = Subsets[{a, b, c}, {}]
    
    #> Subsets[{a, b}, 0]
     = {{}}
    """

    rules = {
        'Subsets[list_?ListQ , Pattern[n,_?ListQ|All|DirectedInfinity[1]], spec_]':'Take[Subsets[list, n], spec]',
        }
    messages = {
        'nninfseq': 'Position 2 of `1` must be All, Infinity, a non-negative integer, or a List whose first element (required) is a non-negative integer, second element (optional) is a non-negative integer or Infinity, and third element (optional) is a nonzero integer',
    }
    
    def apply(self, list, evaluation):
        'Subsets[list_?ListQ]'
    
        return self.apply_1(list, Integer(len(list.to_python())), evaluation)
    
    def apply_1(self, list, n, evaluation):
        'Subsets[list_?ListQ, n_]'
        
        expr = Expression('Subsets', list, n)
        
        n_value = n.get_int_value() 
        if n_value == 0:
            return Expression('List', Expression('List'))
        if n_value is None or n_value < 0:
            return evaluation.message('Subsets', 'nninfseq', expr)
        
        nested_list = [Expression('List', *c) for i in range(n_value + 1) for c in combinations([x for x in list.leaves], i)]
        
        return Expression('List', *nested_list)
        
    def apply_2(self, list, n, evaluation):
        'Subsets[list_?ListQ , Pattern[n,_?ListQ|All|DirectedInfinity[1]]]'
        
        expr = Expression('Subsets', list, n)
        
        if n.get_name() == 'System`All' or n.has_form('DirectedInfinity', 1):
            return self.apply(list, evaluation)
        
        n_len = n.leaves.__len__()
        
        if n_len > 3:
            return evaluation.message('Subsets', 'nninfseq', expr)
        
        if n_len == 0:
            return evaluation.message('Subsets', 'nninfseq', expr)
        
        if n_len == 1:
            elem1 = n.leaves[0].get_int_value()
            if not elem1 or elem1 < 0 :
                return evaluation.message('Subsets', 'nninfseq', expr)
            min_n = elem1
            max_n = min_n + 1
            step_n = 1
        
        if n_len == 2:
            elem1 = n.leaves[0].get_int_value()
            elem2 = n.leaves[1].get_int_value()
            if elem1 is None or elem2 is None or elem1 < 0 or elem2 < 0 :
                return evaluation.message('Subsets', 'nninfseq', expr)
            min_n = elem1
            max_n = elem2 + 1
            step_n = 1
            
        if n_len == 3:
            elem1 = n.leaves[0].get_int_value()
            elem2 = n.leaves[1].get_int_value()
            elem3 = n.leaves[2].get_int_value()
            if elem1 is None or elem2 is None or elem3 is None :
                return evaluation.message('Subsets', 'nninfseq', expr)
            step_n = elem3
            if step_n > 0:
                min_n = elem1
                max_n = elem2 + 1
            elif step_n < 0:
                min_n = elem1
                max_n = elem2 - 1
            else:
                return evaluation.message('Subsets', 'nninfseq', expr)
            
        nested_list = [Expression('List', *c) for i in range(min_n, max_n, step_n) for c in combinations([x for x in list.leaves], i)]
        
        return Expression('List', *nested_list)
        
