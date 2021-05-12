# -*- coding: utf-8 -*-
"""
Combinatorial Functions
"""


from sympy.functions.combinatorial.numbers import stirling
from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Integer, Symbol, SymbolTrue, SymbolFalse
from mathics.builtin.arithmetic import _MPMathFunction
from itertools import combinations


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

    attributes = ("Listable", "NumericFunction")

    nargs = 2
    sympy_name = "binomial"
    mpmath_name = "binomial"


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
     = Binomial[a, a] Binomial[a + b, b] Binomial[a + b + c, c]
    'Multinomial[$n$-$k$, $k$]' is equivalent to 'Binomial[$n$, $k$]'.
    >> Multinomial[2, 3]
     = 10
    """

    attributes = ("Listable", "NumericFunction", "Orderless")

    def apply(self, values, evaluation):
        "Multinomial[values___]"

        values = values.get_sequence()
        leaves = []
        total = []
        for value in values:
            total.append(value)
            leaves.append(Expression("Binomial", Expression("Plus", *total), value))
        return Expression("Times", *leaves)


class Fibonacci(_MPMathFunction):
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

    nargs = 1
    attributes = ("Listable", "NumericFunction", "ReadProtected")
    sympy_name = "fibonacci"
    mpmath_name = "fibonacci"


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
                    if leaf == SymbolTrue:
                        yield 1
                    elif leaf == SymbolFalse:
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
        "%(name)s[u_List, v_List]"
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
        return Expression("Divide", c_tf + c_ft, n)


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
        return Expression("Divide", c_tf + c_ft, c_tt + c_ft + c_tf)


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
        return Expression("Divide", c_tf + c_ft, 2 * c_tt + c_ft + c_tf)


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
        return Expression("Divide", 2 * r_half, c_tt * c_ff + r_half)


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
        return Expression("Divide", r, c_tt + r)


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
        return Expression("Divide", n - c_tt, n)


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
        return Expression("Divide", r, c_tt + c_ff + r)


# Note: WL allows StirlingS1[{2, 4, 6}, 2], but we don't (yet).
class StirlingS1(Builtin):
    """
    <dl>
      <dt>'StirlingS1[$n$, $m$]'
      <dd>gives the Stirling number of the first kind $ _n^m$.
    </dl>

    Integer mathematical function, suitable for both symbolic and numerical manipulation.
    gives the number of permutations of $n$ elements that contain exactly $m$ cycles.

    >> StirlingS1[50, 1]
    = -608281864034267560872252163321295376887552831379210240000000000
    """

    nargs = 2
    sympy_name = "functions.combinatorial.stirling"
    mpmath_name = "stirling1"

    def apply(self, n, m, evaluation):
        "%(name)s[n_Integer, m_Integer]"
        n_value = n.get_int_value()
        m_value = m.get_int_value()
        return Integer(stirling(n_value, m_value, kind=1, signed=True))


class StirlingS2(Builtin):
    """
    <dl>
      <dt>'StirlingS2[$n$, $m$]'
      <dd>gives the Stirling number of the second kind  _n^m.
    </dl>

    returns the number of ways of partitioning a set of $n$ elements into $m$ non empty subsets.

    >> Table[StirlingS2[10, m], {m, 10}]
    = {1, 511, 9330, 34105, 42525, 22827, 5880, 750, 45, 1}
    """

    sympy_name = "functions.combinatorial.numbers.stirling"
    mpmath_name = "stirling2"
    nargs = 2

    def apply(self, m, n, evaluation):
        "%(name)s[n_Integer, m_Integer]"
        n_value = n.get_int_value()
        m_value = m.get_int_value()
        return Integer(stirling(n_value, m_value, kind=2))


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

    All possible subsets (power set):
    >> Subsets[{a, b, c}]
     = {{}, {a}, {b}, {c}, {a, b}, {a, c}, {b, c}, {a, b, c}}

    All possible subsets containing up to 2 elements:
    >> Subsets[{a, b, c, d}, 2]
     = {{}, {a}, {b}, {c}, {d}, {a, b}, {a, c}, {a, d}, {b, c}, {b, d}, {c, d}}

    Subsets containing exactly 2 elements:
    >> Subsets[{a, b, c, d}, {2}]
     = {{a, b}, {a, c}, {a, d}, {b, c}, {b, d}, {c, d}}

    The first 5 subsets containing 3 elements:
    >> Subsets[{a, b, c, d, e}, {3}, 5]
     = {{a, b, c}, {a, b, d}, {a, b, e}, {a, c, d}, {a, c, e}}

    All subsets with even length:
    >> Subsets[{a, b, c, d, e}, {0, 5, 2}]
     = {{}, {a, b}, {a, c}, {a, d}, {a, e}, {b, c}, {b, d}, {b, e}, {c, d}, {c, e}, {d, e}, {a, b, c, d}, {a, b, c, e}, {a, b, d, e}, {a, c, d, e}, {b, c, d, e}}

    The 25th subset:
    >> Subsets[Range[5], All, {25}]
     = {{2, 4, 5}}

    The odd-numbered subsets of {a,b,c,d} in reverse order:
    >> Subsets[{a, b, c, d}, All, {15, 1, -2}]
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

    #> Subsets[{1, 2}, x]
     : Position 2 of Subsets[{1, 2}, x] must be All, Infinity, a non-negative integer, or a List whose first element (required) is a non-negative integer, second element (optional) is a non-negative integer or Infinity, and third element (optional) is a nonzero integer
     = Subsets[{1, 2}, x]

    #> Subsets[x]
     : Nonatomic expression expected at position 1 in Subsets[x].
     = Subsets[x]

    #> Subsets[x, {1, 2}]
     : Nonatomic expression expected at position 1 in Subsets[x, {1, 2}].
     = Subsets[x, {1, 2}]

    #> Subsets[x, {1, 2, 3}, {1, 3}]
     : Nonatomic expression expected at position 1 in Subsets[x, {1, 2, 3}, {1, 3}].
     = Subsets[x, {1, 2, 3}, {1, 3}]

    #> Subsets[a + b + c]
     = {0, a, b, c, a + b, a + c, b + c, a + b + c}

    #> Subsets[f[a, b, c]]
     = {f[], f[a], f[b], f[c], f[a, b], f[a, c], f[b, c], f[a, b, c]}

    #> Subsets[a + b + c, {1, 3, 2}]
     = {a, b, c, a + b + c}

    #> Subsets[a* b * c, All, {6}]
     = {a c}

    #> Subsets[{a, b, c}, {1, Infinity}]
     = {{a}, {b}, {c}, {a, b}, {a, c}, {b, c}, {a, b, c}}

    #> Subsets[{a, b, c}, {1, Infinity, 2}]
     = {{a}, {b}, {c}, {a, b, c}}

    #> Subsets[{a, b, c}, {3, Infinity, -1}]
     = {}
    """

    rules = {
        "Subsets[list_ , Pattern[n,_?ListQ|All|DirectedInfinity[1]], spec_]": "Take[Subsets[list, n], spec]",
    }

    messages = {
        "nninfseq": "Position 2 of `1` must be All, Infinity, a non-negative integer, or a List whose first element (required) is a non-negative integer, second element (optional) is a non-negative integer or Infinity, and third element (optional) is a nonzero integer",
        "normal": "Nonatomic expression expected at position 1 in `1`.",
    }

    def apply(self, list, evaluation):
        "Subsets[list_]"

        return (
            evaluation.message("Subsets", "normal", Expression("Subsets", list))
            if list.is_atom()
            else self.apply_1(list, Integer(len(list.leaves)), evaluation)
        )

    def apply_1(self, list, n, evaluation):
        "Subsets[list_, n_]"

        expr = Expression("Subsets", list, n)
        if list.is_atom():
            return evaluation.message("Subsets", "normal", expr)
        else:
            head_t = list.head
            n_value = n.get_int_value()
            if n_value == 0:
                return Expression("List", Expression("List"))
            if n_value is None or n_value < 0:
                return evaluation.message("Subsets", "nninfseq", expr)

            nested_list = [
                Expression(head_t, *c)
                for i in range(n_value + 1)
                for c in combinations(list.leaves, i)
            ]

            return Expression("List", *nested_list)

    def apply_2(self, list, n, evaluation):
        "Subsets[list_, Pattern[n,_?ListQ|All|DirectedInfinity[1]]]"

        expr = Expression("Subsets", list, n)

        if list.is_atom():
            return evaluation.message("Subsets", "normal", expr)
        else:
            head_t = list.head
            if n.get_name() == "System`All" or n.has_form("DirectedInfinity", 1):
                return self.apply(list, evaluation)

            n_len = len(n.leaves)

            if n_len == 0:
                return evaluation.message("Subsets", "nninfseq", expr)

            elif n_len == 1:
                elem1 = n.leaves[0].get_int_value()
                if elem1 is None or elem1 < 0:
                    return evaluation.message("Subsets", "nninfseq", expr)
                min_n = elem1
                max_n = min_n + 1
                step_n = 1

            elif n_len == 2:
                elem1 = n.leaves[0].get_int_value()
                elem2 = (
                    n.leaves[1].get_int_value()
                    if not n.leaves[1].has_form("DirectedInfinity", 1)
                    else len(list.leaves) + 1
                )
                if elem1 is None or elem2 is None or elem1 < 0 or elem2 < 0:
                    return evaluation.message("Subsets", "nninfseq", expr)
                min_n = elem1
                max_n = elem2 + 1
                step_n = 1

            elif n_len == 3:
                elem1 = n.leaves[0].get_int_value()
                elem2 = (
                    n.leaves[1].get_int_value()
                    if not n.leaves[1].has_form("DirectedInfinity", 1)
                    else len(list.leaves) + 1
                )
                elem3 = n.leaves[2].get_int_value()
                if (
                    elem1 is None
                    or elem2 is None
                    or elem3 is None
                    or elem1 < 0
                    or elem2 < 0
                ):
                    return evaluation.message("Subsets", "nninfseq", expr)
                step_n = elem3
                if step_n > 0:
                    min_n = elem1
                    max_n = elem2 + 1
                elif step_n < 0:
                    min_n = elem1
                    max_n = elem2 - 1
                else:
                    return evaluation.message("Subsets", "nninfseq", expr)
            else:
                return evaluation.message("Subsets", "nninfseq", expr)

            nested_list = [
                Expression(head_t, *c)
                for i in range(min_n, max_n, step_n)
                for c in combinations(list.leaves, i)
            ]

            return Expression("List", *nested_list)

    def apply_3(self, list, n, spec, evaluation):
        "Subsets[list_?AtomQ, Pattern[n,_?ListQ|All|DirectedInfinity[1]], spec_]"

        return evaluation.message(
            "Subsets", "normal", Expression("Subsets", list, n, spec)
        )
