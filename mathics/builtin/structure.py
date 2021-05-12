# -*- coding: utf-8 -*-

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import (
    Builtin,
    Predefined,
    BinaryOperator,
    Test,
    MessageException,
    PartRangeError,
)
from mathics.core.expression import (
    Expression,
    String,
    Symbol,
    SymbolNull,
    SymbolFalse,
    SymbolTrue,
    SymbolList,
    Integer,
    Rational,
    strip_context,
)
from mathics.core.rules import Pattern, Rule

from mathics.builtin.lists import (
    python_levelspec,
    walk_levels,
    InvalidLevelspecError,
    List,
)
from mathics.builtin.functional import Identity

import platform

if platform.python_implementation() == "PyPy":
    bytecount_support = False
else:
    from .pympler.asizeof import asizeof as count_bytes

    bytecount_support = True


class Sort(Builtin):
    """
    <dl>
    <dt>'Sort[$list$]'
    <dd>sorts $list$ (or the leaves of any other expression) according to canonical ordering.
    <dt>'Sort[$list$, $p$]'
    <dd>sorts using $p$ to determine the order of two elements.
    </dl>

    >> Sort[{4, 1.0, a, 3+I}]
     = {1., 3 + I, 4, a}

    Sort uses 'OrderedQ' to determine ordering by default.
    You can sort patterns according to their precedence using 'PatternsOrderedQ':
    >> Sort[{items___, item_, OptionsPattern[], item_symbol, item_?test}, PatternsOrderedQ]
     = {item_symbol, item_ ? test, item_, items___, OptionsPattern[]}

    When sorting patterns, values of atoms do not matter:
    >> Sort[{a, b/;t}, PatternsOrderedQ]
     = {b /; t, a}
    >> Sort[{2+c_, 1+b__}, PatternsOrderedQ]
     = {2 + c_, 1 + b__}
    >> Sort[{x_ + n_*y_, x_ + y_}, PatternsOrderedQ]
     = {x_ + n_ y_, x_ + y_}

    #> Sort[{x_, y_}, PatternsOrderedQ]
     = {x_, y_}

    ## Test ordering of monomials:
    #> a^2f+a b f
     = a ^ 2 f + a b f
    #> a^4 b^2 + e^3 b
     = a ^ 4 b ^ 2 + b e ^ 3
    #> Expand[(1+x)^3 y]
     = y + 3 x y + 3 x ^ 2 y + x ^ 3 y
    #> Expand[(x+y)^3]
     = x ^ 3 + 3 x ^ 2 y + 3 x y ^ 2 + y ^ 3
    #> y+x y^(1/2)
     = x Sqrt[y] + y
    ## Numeric parts:
    #> 1+Pi+Pi^2+Sin[9/4*Pi]+x+x^2+Sin[x+x^2]
     = 1 + Pi + Pi ^ 2 + Sqrt[2] / 2 + x + x ^ 2 + Sin[x + x ^ 2]
    """

    def apply(self, list, evaluation):
        "Sort[list_]"

        if list.is_atom():
            evaluation.message("Sort", "normal")
        else:
            new_leaves = sorted(list.leaves)
            return list.restructure(list.head, new_leaves, evaluation)

    def apply_predicate(self, list, p, evaluation):
        "Sort[list_, p_]"

        if list.is_atom():
            evaluation.message("Sort", "normal")
        else:

            class Key(object):
                def __init__(self, leaf):
                    self.leaf = leaf

                def __gt__(self, other):
                    return (
                        not Expression(p, self.leaf, other.leaf)
                        .evaluate(evaluation)
                        .is_true()
                    )

            new_leaves = sorted(list.leaves, key=Key)
            return list.restructure(list.head, new_leaves, evaluation)


class SortBy(Builtin):
    """
    <dl>
    <dt>'SortBy[$list$, $f$]'
    <dd>sorts $list$ (or the leaves of any other expression) according to canonical ordering of the keys that are
    extracted from the $list$'s elements using $f. Chunks of leaves that appear the same under $f are sorted
    according to their natural order (without applying $f).
    <dt>'SortBy[$f$]'
    <dd>creates an operator function that, when applied, sorts by $f.
    </dl>

    >> SortBy[{{5, 1}, {10, -1}}, Last]
    = {{10, -1}, {5, 1}}

    >> SortBy[Total][{{5, 1}, {10, -9}}]
    = {{10, -9}, {5, 1}}
    """

    rules = {
        "SortBy[f_]": "SortBy[#, f]&",
    }

    messages = {
        "list": "List expected at position `2` in `1`.",
        "func": "Function expected at position `2` in `1`.",
    }

    def apply(self, l, f, evaluation):
        "SortBy[l_, f_]"

        if l.is_atom():
            return evaluation.message("Sort", "normal")
        elif l.get_head_name() != "System`List":
            expr = Expression("SortBy", l, f)
            return evaluation.message(self.get_name(), "list", expr, 1)
        else:
            keys_expr = Expression("Map", f, l).evaluate(evaluation)  # precompute:
            # even though our sort function has only (n log n) comparisons, we should
            # compute f no more than n times.

            if (
                keys_expr is None
                or keys_expr.get_head_name() != "System`List"
                or len(keys_expr.leaves) != len(l.leaves)
            ):
                expr = Expression("SortBy", l, f)
                return evaluation.message("SortBy", "func", expr, 2)

            keys = keys_expr.leaves
            raw_keys = l.leaves

            class Key(object):
                def __init__(self, index):
                    self.index = index

                def __gt__(self, other):
                    kx, ky = keys[self.index], keys[other.index]
                    if kx > ky:
                        return True
                    elif kx < ky:
                        return False
                    else:  # if f(x) == f(y), resort to x < y?
                        return raw_keys[self.index] > raw_keys[other.index]

            # we sort a list of indices. after sorting, we reorder the leaves.
            new_indices = sorted(list(range(len(raw_keys))), key=Key)
            new_leaves = [raw_keys[i] for i in new_indices]  # reorder leaves
            return l.restructure(l.head, new_leaves, evaluation)


class BinarySearch(Builtin):
    """
    <dl>
    <dt>'CombinatoricaOld`BinarySearch[$l$, $k$]'
        <dd>searches the list $l$, which has to be sorted, for key $k$ and returns its index in $l$. If $k$ does not
        exist in $l$, 'BinarySearch' returns (a + b) / 2, where a and b are the indices between which $k$ would have
        to be inserted in order to maintain the sorting order in $l$. Please note that $k$ and the elements in $l$
        need to be comparable under a strict total order (see https://en.wikipedia.org/wiki/Total_order).

    <dt>'CombinatoricaOld`BinarySearch[$l$, $k$, $f$]'
        <dd>the index of $k in the elements of $l$ if $f$ is applied to the latter prior to comparison. Note that $f$
        needs to yield a sorted sequence if applied to the elements of $l.
    </dl>

    >> CombinatoricaOld`BinarySearch[{3, 4, 10, 100, 123}, 100]
     = 4

    >> CombinatoricaOld`BinarySearch[{2, 3, 9}, 7] // N
     = 2.5

    >> CombinatoricaOld`BinarySearch[{2, 7, 9, 10}, 3] // N
     = 1.5

    >> CombinatoricaOld`BinarySearch[{-10, 5, 8, 10}, -100] // N
     = 0.5

    >> CombinatoricaOld`BinarySearch[{-10, 5, 8, 10}, 20] // N
     = 4.5

    >> CombinatoricaOld`BinarySearch[{{a, 1}, {b, 7}}, 7, #[[2]]&]
     = 2
    """

    context = "CombinatoricaOld`"

    rules = {
        "CombinatoricaOld`BinarySearch[l_List, k_] /; Length[l] > 0": "CombinatoricaOld`BinarySearch[l, k, Identity]"
    }

    def apply(self, l, k, f, evaluation):
        "CombinatoricaOld`BinarySearch[l_List, k_, f_] /; Length[l] > 0"

        leaves = l.leaves

        lower_index = 1
        upper_index = len(leaves)

        if (
            lower_index > upper_index
        ):  # empty list l? Length[l] > 0 condition should guard us, but check anyway
            return Symbol("$Aborted")

        # "transform" is a handy wrapper for applying "f" or nothing
        if f.get_name() == "System`Identity":

            def transform(x):
                return x

        else:

            def transform(x):
                return Expression(f, x).evaluate(evaluation)

        # loop invariants (true at any time in the following loop):
        # (1) lower_index <= upper_index
        # (2) k > leaves[i] for all i < lower_index
        # (3) k < leaves[i] for all i > upper_index
        while True:
            pivot_index = (lower_index + upper_index) >> 1  # i.e. a + (b - a) // 2
            # as lower_index <= upper_index, lower_index <= pivot_index <= upper_index
            pivot = transform(leaves[pivot_index - 1])  # 1-based to 0-based

            # we assume a trichotomous relation: k < pivot, or k = pivot, or k > pivot
            if k < pivot:
                if pivot_index == lower_index:  # see invariant (2), to see that
                    # k < leaves[pivot_index] and k > leaves[pivot_index - 1]
                    return Rational((pivot_index - 1) + pivot_index, 2)
                upper_index = pivot_index - 1
            elif k == pivot:
                return Integer(pivot_index)
            else:  # k > pivot
                if pivot_index == upper_index:  # see invariant (3), to see that
                    # k > leaves[pivot_index] and k < leaves[pivot_index + 1]
                    return Rational(pivot_index + (pivot_index + 1), 2)
                lower_index = pivot_index + 1


class PatternsOrderedQ(Builtin):
    """
    <dl>
    <dt>'PatternsOrderedQ[$patt1$, $patt2$]'
        <dd>returns 'True' if pattern $patt1$ would be applied before
        $patt2$ according to canonical pattern ordering.
    </dl>

    >> PatternsOrderedQ[x__, x_]
     = False
    >> PatternsOrderedQ[x_, x__]
     = True
    >> PatternsOrderedQ[b, a]
     = True
    """

    def apply(self, p1, p2, evaluation):
        "PatternsOrderedQ[p1_, p2_]"

        if p1.get_sort_key(True) <= p2.get_sort_key(True):
            return SymbolTrue
        else:
            return SymbolFalse


class OrderedQ(Builtin):
    """
    <dl>
    <dt>'OrderedQ[$a$, $b$]'
        <dd>is 'True' if $a$ sorts before $b$ according to canonical
        ordering.
    </dl>

    >> OrderedQ[a, b]
     = True
    >> OrderedQ[b, a]
     = False
    """

    def apply(self, e1, e2, evaluation):
        "OrderedQ[e1_, e2_]"

        if e1 <= e2:
            return SymbolTrue
        else:
            return SymbolFalse


class Order(Builtin):
    """
    <dl>
    <dt>'Order[$x$, $y$]'
        <dd>returns a number indicating the canonical ordering of $x$ and $y$. 1 indicates that $x$ is before $y$,
        -1 that $y$ is before $x$. 0 indicates that there is no specific ordering. Uses the same order as 'Sort'.
    </dl>

    >> Order[7, 11]
     = 1

    >> Order[100, 10]
     = -1

    >> Order[x, z]
     = 1

    >> Order[x, x]
     = 0
    """

    def apply(self, x, y, evaluation):
        "Order[x_, y_]"
        if x < y:
            return Integer(1)
        elif x > y:
            return Integer(-1)
        else:
            return Integer(0)


class Head(Builtin):
    """
    <dl>
    <dt>'Head[$expr$]'
        <dd>returns the head of the expression or atom $expr$.
    </dl>

    >> Head[a * b]
     = Times
    >> Head[6]
     = Integer
    >> Head[x]
     = Symbol
    """

    def apply(self, expr, evaluation):
        "Head[expr_]"

        return expr.get_head()


class ApplyLevel(BinaryOperator):
    """
    <dl>
    <dt>'ApplyLevel[$f$, $expr$]'
    <dt>'$f$ @@@ $expr$'
        <dd>is equivalent to 'Apply[$f$, $expr$, {1}]'.
    </dl>

    >> f @@@ {{a, b}, {c, d}}
     = {f[a, b], f[c, d]}
    """

    operator = "@@@"
    precedence = 620
    grouping = "Right"

    rules = {
        "ApplyLevel[f_, expr_]": "Apply[f, expr, {1}]",
    }


class Apply(BinaryOperator):
    """
    <dl>
    <dt>'Apply[$f$, $expr$]'
    <dt>'$f$ @@ $expr$'
        <dd>replaces the head of $expr$ with $f$.
    <dt>'Apply[$f$, $expr$, $levelspec$]'
        <dd>applies $f$ on the parts specified by $levelspec$.
    </dl>

    >> f @@ {1, 2, 3}
     = f[1, 2, 3]
    >> Plus @@ {1, 2, 3}
     = 6

    The head of $expr$ need not be 'List':
    >> f @@ (a + b + c)
     = f[a, b, c]

    Apply on level 1:
    >> Apply[f, {a + b, g[c, d, e * f], 3}, {1}]
     = {f[a, b], f[c, d, e f], 3}
    The default level is 0:
    >> Apply[f, {a, b, c}, {0}]
     = f[a, b, c]

    Range of levels, including negative level (counting from bottom):
    >> Apply[f, {{{{{a}}}}}, {2, -3}]
     = {{f[f[{a}]]}}

    Convert all operations to lists:
    >> Apply[List, a + b * c ^ e * f[g], {0, Infinity}]
     = {a, {b, {g}, {c, e}}}

    #> Apply[f, {a, b, c}, x+y]
     : Level specification x + y is not of the form n, {n}, or {m, n}.
     = Apply[f, {a, b, c}, x + y]
    """

    operator = "@@"
    precedence = 620
    grouping = "Right"

    options = {
        "Heads": "False",
    }

    def apply_invalidlevel(self, f, expr, ls, evaluation, options={}):
        "Apply[f_, expr_, ls_, OptionsPattern[Apply]]"

        evaluation.message("Apply", "level", ls)

    def apply(self, f, expr, ls, evaluation, options={}):
        """Apply[f_, expr_, Optional[Pattern[ls, _?LevelQ], {0}],
        OptionsPattern[Apply]]"""

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            evaluation.message("Apply", "level", ls)
            return

        def callback(level):
            if level.is_atom():
                return level
            else:
                return Expression(f, *level.leaves)

        heads = self.get_option(options, "Heads", evaluation).is_true()
        result, depth = walk_levels(expr, start, stop, heads=heads, callback=callback)

        return result


class Map(BinaryOperator):
    """
    <dl>
    <dt>'Map[$f$, $expr$]' or '$f$ /@ $expr$'
        <dd>applies $f$ to each part on the first level of $expr$.
    <dt>'Map[$f$, $expr$, $levelspec$]'
        <dd>applies $f$ to each level specified by $levelspec$ of $expr$.
    </dl>

    >> f /@ {1, 2, 3}
     = {f[1], f[2], f[3]}
    >> #^2& /@ {1, 2, 3, 4}
     = {1, 4, 9, 16}

    Map $f$ on the second level:
    >> Map[f, {{a, b}, {c, d, e}}, {2}]
     = {{f[a], f[b]}, {f[c], f[d], f[e]}}

    Include heads:
    >> Map[f, a + b + c, Heads->True]
     = f[Plus][f[a], f[b], f[c]]

    #> Map[f, expr, a+b, Heads->True]
     : Level specification a + b is not of the form n, {n}, or {m, n}.
     = Map[f, expr, a + b, Heads -> True]
    """

    operator = "/@"
    precedence = 620
    grouping = "Right"

    options = {
        "Heads": "False",
    }

    def apply_invalidlevel(self, f, expr, ls, evaluation, options={}):
        "Map[f_, expr_, ls_, OptionsPattern[Map]]"

        evaluation.message("Map", "level", ls)

    def apply_level(self, f, expr, ls, evaluation, options={}):
        """Map[f_, expr_, Optional[Pattern[ls, _?LevelQ], {1}],
        OptionsPattern[Map]]"""

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            evaluation.message("Map", "level", ls)
            return

        def callback(level):
            return Expression(f, level)

        heads = self.get_option(options, "Heads", evaluation).is_true()
        result, depth = walk_levels(expr, start, stop, heads=heads, callback=callback)

        return result


class MapAt(Builtin):
    """
    <dl>
      <dt>'MapAt[$f$, $expr$, $n$]'
      <dd>applies $f$ to the element at position $n$ in $expr$. If $n$ is negative, the position is counted from the end.
      <dt>'MapAt[f, $exp$r, {$i$, $j$ ...}]'
      <dd>applies $f$ to the part of $expr$ at position {$i$, $j$, ...}.
      <dt>'MapAt[$f$,$pos$]'
      <dd>represents an operator form of MapAt that can be applied to an expression.
    </dl>

    Map $f$ onto the part at position 2:
    >> MapAt[f, {a, b, c, d}, 2]
     = {a, f[b], c, d}

    Map $f$ onto multiple parts:
    >> MapAt[f, {a, b, c, d}, {{1}, {4}}]
     = {f[a], b, c, f[d]}

    Map $f$ onto the at the end:
    >> MapAt[f, {a, b, c, d}, -1]
     = {a, b, c, f[d]}

     Map $f$ onto an association:
    >> MapAt[f, <|"a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4, "e" -> 5|>, 3]
     = {a -> 1, b -> 2, c -> f[3], d -> 4, e -> 5}

    Use negative position in an association:
    >> MapAt[f, <|"a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4|>, -3]
     = {a -> 1, b -> f[2], c -> 3, d -> 4}

    Use the operator form of MapAt:
    >> MapAt[f, 1][{a, b, c, d}]
     = {f[a], b, c, d}
    """

    rules = {
        "MapAt[f_, pos_][expr_]": "MapAt[f, expr, pos]",
    }

    def apply(self, f, expr, args, evaluation, options={}):
        "MapAt[f_, expr_, args_]"

        m = len(expr.leaves)

        def map_at_one(i, leaves):
            if 1 <= i <= m:
                j = i - 1
            elif -m <= i <= -1:
                j = m + i
            else:
                raise PartRangeError
            replace_leaf = new_leaves[j]
            if hasattr(replace_leaf, "head") and replace_leaf.head == Symbol(
                "System`Rule"
            ):
                new_leaves[j] = Expression(
                    "System`Rule",
                    replace_leaf.leaves[0],
                    Expression(f, replace_leaf.leaves[1]),
                )
            else:
                new_leaves[j] = Expression(f, replace_leaf)
            return new_leaves

        a = args.to_python()
        if isinstance(a, int):
            new_leaves = list(expr.leaves)
            new_leaves = map_at_one(a, new_leaves)
            return List(*new_leaves)
        elif isinstance(a, list):
            new_leaves = list(expr.leaves)
            for l in a:
                if len(l) == 1 and isinstance(l[0], int):
                    new_leaves = map_at_one(l[0], new_leaves)
            return List(*new_leaves)


class Scan(Builtin):
    """
    <dl>
    <dt>'Scan[$f$, $expr$]'
      <dd>applies $f$ to each element of $expr$ and returns 'Null'.
    <dt>'Scan[$f$, $expr$, $levelspec$]
      <dd>applies $f$ to each level specified by $levelspec$ of $expr$.
    </dl>

    >> Scan[Print, {1, 2, 3}]
     | 1
     | 2
     | 3

    #> Scan[Print, f[g[h[x]]], 2]
     | h[x]
     | g[h[x]]

    #> Scan[Print][{1, 2}]
     | 1
     | 2

    #> Scan[Return, {1, 2}]
     = 1
    """

    options = {
        "Heads": "False",
    }

    rules = {
        "Scan[f_][expr_]": "Scan[f, expr]",
    }

    def apply_invalidlevel(self, f, expr, ls, evaluation, options={}):
        "Scan[f_, expr_, ls_, OptionsPattern[Map]]"

        return evaluation.message("Map", "level", ls)

    def apply_level(self, f, expr, ls, evaluation, options={}):
        """Scan[f_, expr_, Optional[Pattern[ls, _?LevelQ], {1}],
        OptionsPattern[Map]]"""

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            evaluation.message("Map", "level", ls)
            return

        def callback(level):
            Expression(f, level).evaluate(evaluation)
            return level

        heads = self.get_option(options, "Heads", evaluation).is_true()
        result, depth = walk_levels(expr, start, stop, heads=heads, callback=callback)

        return SymbolNull


class MapIndexed(Builtin):
    """
    <dl>
    <dt>'MapIndexed[$f$, $expr$]'
        <dd>applies $f$ to each part on the first level of $expr$, including the part positions
        in the call to $f$.
    <dt>'MapIndexed[$f$, $expr$, $levelspec$]'
        <dd>applies $f$ to each level specified by $levelspec$ of $expr$.
    </dl>

    >> MapIndexed[f, {a, b, c}]
     = {f[a, {1}], f[b, {2}], f[c, {3}]}

    Include heads (index 0):
    >> MapIndexed[f, {a, b, c}, Heads->True]
     = f[List, {0}][f[a, {1}], f[b, {2}], f[c, {3}]]

    Map on levels 0 through 1 (outer expression gets index '{}'):
    >> MapIndexed[f, a + b + c * d, {0, 1}]
     = f[f[a, {1}] + f[b, {2}] + f[c d, {3}], {}]

    Get the positions of atoms in an expression (convert operations to 'List' first
    to disable 'Listable' functions):
    >> expr = a + b * f[g] * c ^ e;
    >> listified = Apply[List, expr, {0, Infinity}];
    >> MapIndexed[#2 &, listified, {-1}]
     = {{1}, {{2, 1}, {{2, 2, 1}}, {{2, 3, 1}, {2, 3, 2}}}}
    Replace the heads with their positions, too:
    >> MapIndexed[#2 &, listified, {-1}, Heads -> True]
     = {0}[{1}, {2, 0}[{2, 1}, {2, 2, 0}[{2, 2, 1}], {2, 3, 0}[{2, 3, 1}, {2, 3, 2}]]]
    The positions are given in the same format as used by 'Extract'.
    Thus, mapping 'Extract' on the indices given by 'MapIndexed' re-constructs the original expression:
    >> MapIndexed[Extract[expr, #2] &, listified, {-1}, Heads -> True]
     = a + b f[g] c ^ e

    #> MapIndexed[f, {1, 2}, a+b]
     : Level specification a + b is not of the form n, {n}, or {m, n}.
     = MapIndexed[f, {1, 2}, a + b]
    """

    options = {
        "Heads": "False",
    }

    def apply_invalidlevel(self, f, expr, ls, evaluation, options={}):
        "MapIndexed[f_, expr_, ls_, OptionsPattern[MapIndexed]]"

        evaluation.message("MapIndexed", "level", ls)

    def apply_level(self, f, expr, ls, evaluation, options={}):
        """MapIndexed[f_, expr_, Optional[Pattern[ls, _?LevelQ], {1}],
        OptionsPattern[MapIndexed]]"""

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            evaluation.message("MapIndexed", "level", ls)
            return

        def callback(level, pos):
            return Expression(f, level, Expression("List", *[Integer(p) for p in pos]))

        heads = self.get_option(options, "Heads", evaluation).is_true()
        result, depth = walk_levels(
            expr, start, stop, heads=heads, callback=callback, include_pos=True
        )

        return result


class MapThread(Builtin):
    """
    <dl>
    <dt>'MapThread[$f$, {{$a1$, $a2$, ...}, {$b1$, $b2$, ...}, ...}]
      <dd>returns '{$f$[$a1$, $b1$, ...], $f$[$a2$, $b2$, ...], ...}'.
    <dt>'MapThread[$f$, {$expr1$, $expr2$, ...}, $n$]'
      <dd>applies $f$ at level $n$.
    </dl>

    >> MapThread[f, {{a, b, c}, {1, 2, 3}}]
     = {f[a, 1], f[b, 2], f[c, 3]}

    >> MapThread[f, {{{a, b}, {c, d}}, {{e, f}, {g, h}}}, 2]
     = {{f[a, e], f[b, f]}, {f[c, g], f[d, h]}}

    #> MapThread[f, {{a, b}, {c, d}}, {1}]
     : Non-negative machine-sized integer expected at position 3 in MapThread[f, {{a, b}, {c, d}}, {1}].
     = MapThread[f, {{a, b}, {c, d}}, {1}]

    #> MapThread[f, {{a, b}, {c, d}}, 2]
     : Object {a, b} at position {2, 1} in MapThread[f, {{a, b}, {c, d}}, 2] has only 1 of required 2 dimensions.
     = MapThread[f, {{a, b}, {c, d}}, 2]

    #> MapThread[f, {{a}, {b, c}}]
     : Incompatible dimensions of objects at positions {2, 1} and {2, 2} of MapThread[f, {{a}, {b, c}}]; dimensions are 1 and 2.
     = MapThread[f, {{a}, {b, c}}]

    #> MapThread[f, {}]
     = {}

    #> MapThread[f, {a, b}, 0]
     = f[a, b]
    #> MapThread[f, {a, b}, 1]
     :  Object a at position {2, 1} in MapThread[f, {a, b}, 1] has only 0 of required 1 dimensions.
     = MapThread[f, {a, b}, 1]

    ## Behaviour extends MMA
    #> MapThread[f, {{{a, b}, {c}}, {{d, e}, {f}}}, 2]
     = {{f[a, d], f[b, e]}, {f[c, f]}}
    """

    messages = {
        "intnm": "Non-negative machine-sized integer expected at position `2` in `1`.",
        "mptc": "Incompatible dimensions of objects at positions {2, `1`} and {2, `2`} of `3`; dimensions are `4` and `5`.",
        "mptd": "Object `1` at position {2, `2`} in `3` has only `4` of required `5` dimensions.",
        "list": "List expected at position `2` in `1`.",
    }

    def apply(self, f, expr, evaluation):
        "MapThread[f_, expr_]"

        return self.apply_n(f, expr, None, evaluation)

    def apply_n(self, f, expr, n, evaluation):
        "MapThread[f_, expr_, n_]"

        if n is None:
            n = 1
            full_expr = Expression("MapThread", f, expr)
        else:
            full_expr = Expression("MapThread", f, expr, n)
            n = n.get_int_value()

        if n is None or n < 0:
            return evaluation.message("MapThread", "intnm", full_expr, 3)

        if expr.has_form("List", 0):
            return Expression(SymbolList)
        if not expr.has_form("List", None):
            return evaluation.message("MapThread", "list", 2, full_expr)

        heads = expr.get_leaves()

        def walk(args, depth=0):
            "walk all trees concurrently and build result"
            if depth == n:
                return Expression(f, *args)
            else:
                dim = None
                for i, arg in enumerate(args):
                    if not arg.has_form("List", None):
                        raise MessageException(
                            "MapThread", "mptd", heads[i], i + 1, full_expr, depth, n
                        )
                    if dim is None:
                        dim = len(arg.leaves)
                    if dim != len(arg.leaves):
                        raise MessageException(
                            "MapThread",
                            "mptc",
                            1,
                            i + 1,
                            full_expr,
                            dim,
                            len(arg.leaves),
                        )
                return Expression(
                    "List",
                    *[
                        walk([arg.leaves[i] for arg in args], depth + 1)
                        for i in range(dim)
                    ]
                )

        try:
            return walk(heads)
        except MessageException as e:
            return e.message(evaluation)


class Thread(Builtin):
    """
    <dl>
    <dt>'Thread[$f$[$args$]]'
        <dd>threads $f$ over any lists that appear in $args$.
    <dt>'Thread[$f$[$args$], $h$]'
        <dd>threads over any parts with head $h$.
    </dl>

    >> Thread[f[{a, b, c}]]
     = {f[a], f[b], f[c]}
    >> Thread[f[{a, b, c}, t]]
     = {f[a, t], f[b, t], f[c, t]}
    >> Thread[f[a + b + c], Plus]
     = f[a] + f[b] + f[c]

    Functions with attribute 'Listable' are automatically threaded over lists:
    >> {a, b, c} + {d, e, f} + g
     = {a + d + g, b + e + g, c + f + g}
    """

    messages = {
        "tdlen": "Objects of unequal length cannot be combined.",
    }

    rules = {
        "Thread[f_[args___]]": "Thread[f[args], List]",
    }

    def apply(self, f, args, h, evaluation):
        "Thread[f_[args___], h_]"

        args = args.get_sequence()
        expr = Expression(f, *args)
        threaded, result = expr.thread(evaluation, head=h)
        return result


class FreeQ(Builtin):
    """
    <dl>
    <dt>'FreeQ[$expr$, $x$]'
        <dd>returns 'True' if $expr$ does not contain the expression
        $x$.
    </dl>

    >> FreeQ[y, x]
     = True
    >> FreeQ[a+b+c, a+b]
     = False
    >> FreeQ[{1, 2, a^(a+b)}, Plus]
     = False
    >> FreeQ[a+b, x_+y_+z_]
     = True
    >> FreeQ[a+b+c, x_+y_+z_]
     = False
    >> FreeQ[x_+y_+z_][a+b]
     = True
    """

    rules = {
        "FreeQ[form_][expr_]": "FreeQ[expr, form]",
    }

    def apply(self, expr, form, evaluation):
        "FreeQ[expr_, form_]"

        form = Pattern.create(form)
        if expr.is_free(form, evaluation):
            return SymbolTrue
        else:
            return SymbolFalse


class Flatten(Builtin):
    """
    <dl>
    <dt>'Flatten[$expr$]'
        <dd>flattens out nested lists in $expr$.
    <dt>'Flatten[$expr$, $n$]'
        <dd>stops flattening at level $n$.
    <dt>'Flatten[$expr$, $n$, $h$]'
        <dd>flattens expressions with head $h$ instead of 'List'.
    </dl>

    >> Flatten[{{a, b}, {c, {d}, e}, {f, {g, h}}}]
     = {a, b, c, d, e, f, g, h}
    >> Flatten[{{a, b}, {c, {e}, e}, {f, {g, h}}}, 1]
     = {a, b, c, {e}, e, f, {g, h}}
    >> Flatten[f[a, f[b, f[c, d]], e], Infinity, f]
     = f[a, b, c, d, e]

    >> Flatten[{{a, b}, {c, d}}, {{2}, {1}}]
     = {{a, c}, {b, d}}

    >> Flatten[{{a, b}, {c, d}}, {{1, 2}}]
     = {a, b, c, d}

    Flatten also works in irregularly shaped arrays
    >> Flatten[{{1, 2, 3}, {4}, {6, 7}, {8, 9, 10}}, {{2}, {1}}]
     = {{1, 4, 6, 8}, {2, 7, 9}, {3, 10}}

    #> Flatten[{{{111, 112, 113}, {121, 122}}, {{211, 212}, {221, 222, 223}}}, {{3}, {1}, {2}}]
     = {{{111, 121}, {211, 221}}, {{112, 122}, {212, 222}}, {{113}, {223}}}

    #> Flatten[{{{1, 2, 3}, {4, 5}}, {{6, 7}, {8, 9,  10}}}, {{3}, {1}, {2}}]
     = {{{1, 4}, {6, 8}}, {{2, 5}, {7, 9}}, {{3}, {10}}}

    #> Flatten[{{{1, 2, 3}, {4, 5}}, {{6, 7}, {8, 9, 10}}}, {{2}, {1, 3}}]
     = {{1, 2, 3, 6, 7}, {4, 5, 8, 9, 10}}

    #> Flatten[{{1, 2}, {3,4}}, {1, 2}]
     = {1, 2, 3, 4}

    #> Flatten[{{1, 2}, {3, 4}}, {{-1, 2}}]
     : Levels to be flattened together in {{-1, 2}} should be lists of positive integers.
     = Flatten[{{1, 2}, {3, 4}}, {{-1, 2}}, List]

    #> Flatten[{a, b}, {{1}, {2}}]
     : Level 2 specified in {{1}, {2}} exceeds the levels, 1, which can be flattened together in {a, b}.
     = Flatten[{a, b}, {{1}, {2}}, List]

    ## Check `n` completion
    #> m = {{{1, 2}, {3}}, {{4}, {5, 6}}};
    #> Flatten[m, {2}]
     = {{{1, 2}, {4}}, {{3}, {5, 6}}}
    #> Flatten[m, {{2}}]
     = {{{1, 2}, {4}}, {{3}, {5, 6}}}
    #> Flatten[m, {{2}, {1}}]
     = {{{1, 2}, {4}}, {{3}, {5, 6}}}
    #> Flatten[m, {{2}, {1}, {3}}]
     = {{{1, 2}, {4}}, {{3}, {5, 6}}}
    #> Flatten[m, {{2}, {1}, {3}, {4}}]
     : Level 4 specified in {{2}, {1}, {3}, {4}} exceeds the levels, 3, which can be flattened together in {{{1, 2}, {3}}, {{4}, {5, 6}}}.
     = Flatten[{{{1, 2}, {3}}, {{4}, {5, 6}}}, {{2}, {1}, {3}, {4}}, List]

    ## #251 tests
    #> m = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    #> Flatten[m, {1}]
     = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}
    #> Flatten[m, {2}]
     = {{1, 4, 7}, {2, 5, 8}, {3, 6, 9}}
    #> Flatten[m, {3}]
     : Level 3 specified in {3} exceeds the levels, 2, which can be flattened together in {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}.
     = Flatten[{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}, {3}, List]
    #> Flatten[m, {2, 1}]
     = {1, 4, 7, 2, 5, 8, 3, 6, 9}

    ## Reproduce strange head behaviour
    #> Flatten[{{1}, 2}, {1, 2}]
     : Level 2 specified in {1, 2} exceeds the levels, 1, which can be flattened together in {{1}, 2}.
     = Flatten[{{1}, 2}, {1, 2}, List]
    #> Flatten[a[b[1, 2], b[3]], {1, 2}, b]     (* MMA BUG: {{1, 2}} not {1, 2}  *)
     : Level 1 specified in {1, 2} exceeds the levels, 0, which can be flattened together in a[b[1, 2], b[3]].
     = Flatten[a[b[1, 2], b[3]], {1, 2}, b]

    #> Flatten[{{1, 2}, {3, {4}}}, {{1, 2}}]
     = {1, 2, 3, {4}}
    #> Flatten[{{1, 2}, {3, {4}}}, {{1, 2, 3}}]
     : Level 3 specified in {{1, 2, 3}} exceeds the levels, 2, which can be flattened together in {{1, 2}, {3, {4}}}.
     = Flatten[{{1, 2}, {3, {4}}}, {{1, 2, 3}}, List]

    #> Flatten[p[1, p[2], p[3]]]
     = p[1, 2, 3]
    #> Flatten[p[1, p[2], p[3]], 2]
     = p[1, 2, 3]
    """

    rules = {
        "Flatten[expr_]": "Flatten[expr, Infinity, Head[expr]]",
        "Flatten[expr_, n_]": "Flatten[expr, n, Head[expr]]",
    }

    messages = {
        "flpi": (
            "Levels to be flattened together in `1` "
            "should be lists of positive integers."
        ),
        "flrep": ("Level `1` specified in `2` should not be repeated."),
        "fldep": (
            "Level `1` specified in `2` exceeds the levels, `3`, "
            "which can be flattened together in `4`."
        ),
    }

    def apply_list(self, expr, n, h, evaluation):
        "Flatten[expr_, n_List, h_]"

        # prepare levels
        # find max depth which matches `h`
        expr, max_depth = walk_levels(expr)
        max_depth = {"max_depth": max_depth}  # hack to modify max_depth from callback

        def callback(expr, pos):
            if len(pos) < max_depth["max_depth"] and (expr.is_atom() or expr.head != h):
                max_depth["max_depth"] = len(pos)
            return expr

        expr, depth = walk_levels(expr, callback=callback, include_pos=True, start=0)
        max_depth = max_depth["max_depth"]

        levels = n.to_python()

        # mappings
        if isinstance(levels, list) and all(isinstance(level, int) for level in levels):
            levels = [levels]

        # verify levels is list of lists of positive ints
        if not (isinstance(levels, list) and len(levels) > 0):
            evaluation.message("Flatten", "flpi", n)
            return
        seen_levels = []
        for level in levels:
            if not (isinstance(level, list) and len(level) > 0):
                evaluation.message("Flatten", "flpi", n)
                return
            for l in level:
                if not (isinstance(l, int) and l > 0):
                    evaluation.message("Flatten", "flpi", n)
                    return
                if l in seen_levels:
                    # level repeated
                    evaluation.message("Flatten", "flrep", l)
                    return
                seen_levels.append(l)

        # complete the level spec e.g. {{2}} -> {{2}, {1}, {3}}
        for l in range(1, max_depth + 1):
            if l not in seen_levels:
                levels.append([l])

        # verify specified levels are smaller max depth
        for level in levels:
            for l in level:
                if l > max_depth:
                    evaluation.message("Flatten", "fldep", l, n, max_depth, expr)
                    return

        # assign new indices to each leaf
        new_indices = {}

        def callback(expr, pos):
            if len(pos) == max_depth:
                new_depth = tuple(tuple(pos[i - 1] for i in level) for level in levels)
                new_indices[new_depth] = expr
            return expr

        expr, depth = walk_levels(expr, callback=callback, include_pos=True)

        # build new tree inserting nodes as needed
        leaves = sorted(new_indices.items())

        def insert_leaf(leaves):
            # gather leaves into groups with the same leading index
            # e.g. [((0, 0), a), ((0, 1), b), ((1, 0), c), ((1, 1), d)]
            # -> [[(0, a), (1, b)], [(0, c), (1, d)]]
            leading_index = None
            grouped_leaves = []
            for index, leaf in leaves:
                if index[0] == leading_index:
                    grouped_leaves[-1].append((index[1:], leaf))
                else:
                    leading_index = index[0]
                    grouped_leaves.append([(index[1:], leaf)])
            # for each group of leaves we either insert them into the current level
            # or make a new level and recurse
            new_leaves = []
            for group in grouped_leaves:
                if len(group[0][0]) == 0:  # bottom level leaf
                    assert len(group) == 1
                    new_leaves.append(group[0][1])
                else:
                    new_leaves.append(Expression(h, *insert_leaf(group)))

            return new_leaves

        return Expression(h, *insert_leaf(leaves))

    def apply(self, expr, n, h, evaluation):
        "Flatten[expr_, n_, h_]"

        if n == Expression("DirectedInfinity", 1):
            n = None
        else:
            n_int = n.get_int_value()
            if n_int is None or n_int < 0:
                return evaluation.message("Flatten", "flpi", n)
            n = n_int

        return expr.flatten(h, level=n)


class Null(Predefined):
    """
    <dl>
    <dt>'Null'
        <dd>is the implicit result of expressions that do not yield a result.
    </dl>

    >> FullForm[a:=b]
     = Null

    It is not displayed in StandardForm,
    >> a:=b
    in contrast to the empty string:
    >> ""
     = #<--#
    """


class AtomQ(Test):
    """
    <dl>
    <dt>'AtomQ[$x$]'
        <dd>is true if $x$ is an atom (an object such as a number or
        string, which cannot be divided into subexpressions using
        'Part').
    </dl>

    >> AtomQ[x]
     = True
    >> AtomQ[1.2]
     = True
    >> AtomQ[2 + I]
     = True
    >> AtomQ[2 / 3]
     = True
    >> AtomQ[x + y]
     = False
    """

    def test(self, expr):
        return expr.is_atom()


class SymbolQ(Test):
    """
    <dl>
    <dt>'SymbolQ[$x$]'
        <dd>is 'True' if $x$ is a symbol, or 'False' otherwise.
    </dl>

    >> SymbolQ[a]
     = True
    >> SymbolQ[1]
     = False
    >> SymbolQ[a + b]
     = False
    """

    def test(self, expr):
        return isinstance(expr, Symbol)


class Symbol_(Builtin):
    """
    <dl>
    <dt>'Symbol'
        <dd>is the head of symbols.
    </dl>

    >> Head[x]
     = Symbol
    You can use 'Symbol' to create symbols from strings:
    >> Symbol["x"] + Symbol["x"]
     = 2 x

    #> {\\[Eta], \\[CapitalGamma]\\[Beta], Z\\[Infinity], \\[Angle]XYZ, \\[FilledSquare]r, i\\[Ellipsis]j}
     = {\u03b7, \u0393\u03b2, Z\u221e, \u2220XYZ, \u25a0r, i\u2026j}
    """

    name = "Symbol"
    attributes = ("Locked",)

    messages = {
        "symname": (
            "The string `1` cannot be used for a symbol name. "
            "A symbol name must start with a letter "
            "followed by letters and numbers."
        ),
    }

    def apply(self, string, evaluation):
        "Symbol[string_String]"

        from mathics.core.parser import is_symbol_name

        text = string.get_string_value()
        if is_symbol_name(text):
            return Symbol(evaluation.definitions.lookup_name(string.value))
        else:
            evaluation.message("Symbol", "symname", string)


class SymbolName(Builtin):
    """
    <dl>
    <dt>'SymbolName[$s$]'
        <dd>returns the name of the symbol $s$ (without any leading
        context name).
    </dl>

    >> SymbolName[x] // InputForm
     = "x"

    #> SymbolName[a`b`x] // InputForm
     = "x"
    """

    def apply(self, symbol, evaluation):
        "SymbolName[symbol_Symbol]"

        # MMA docs say "SymbolName always gives the short name,
        # without any context"
        return String(strip_context(symbol.get_name()))


class Depth(Builtin):
    """
    <dl>
    <dt>'Depth[$expr$]'
        <dd>gives the depth of $expr$.
    </dl>

    The depth of an expression is defined as one plus the maximum
    number of 'Part' indices required to reach any part of $expr$,
    except for heads.

    >> Depth[x]
     = 1
    >> Depth[x + y]
     = 2
    >> Depth[{{{{x}}}}]
     = 5

    Complex numbers are atomic, and hence have depth 1:
    >> Depth[1 + 2 I]
     = 1

    'Depth' ignores heads:
    >> Depth[f[a, b][c]]
     = 2
    """

    def apply(self, expr, evaluation):
        "Depth[expr_]"
        expr, depth = walk_levels(expr)
        return Integer(depth + 1)


class Operate(Builtin):
    """
    <dl>
    <dt>'Operate[$p$, $expr$]'
        <dd>applies $p$ to the head of $expr$.
    <dt>'Operate[$p$, $expr$, $n$]'
        <dd>applies $p$ to the $n$th head of $expr$.
    </dl>

    >> Operate[p, f[a, b]]
     = p[f][a, b]

    The default value of $n$ is 1:
    >> Operate[p, f[a, b], 1]
     = p[f][a, b]

    With $n$=0, 'Operate' acts like 'Apply':
    >> Operate[p, f[a][b][c], 0]
     = p[f[a][b][c]]

    #> Operate[p, f[a][b][c]]
     = p[f[a][b]][c]
    #> Operate[p, f[a][b][c], 1]
     = p[f[a][b]][c]
    #> Operate[p, f[a][b][c], 2]
     = p[f[a]][b][c]
    #> Operate[p, f[a][b][c], 3]
     = p[f][a][b][c]
    #> Operate[p, f[a][b][c], 4]
     = f[a][b][c]
    #> Operate[p, f]
     = f
    #> Operate[p, f, 0]
     = p[f]
    #> Operate[p, f, -1]
     : Non-negative integer expected at position 3 in Operate[p, f, -1].
     = Operate[p, f, -1]
    """

    messages = {
        "intnn": "Non-negative integer expected at position `2` in `1`.",
    }

    def apply(self, p, expr, n, evaluation):
        "Operate[p_, expr_, Optional[n_, 1]]"

        head_depth = n.get_int_value()
        if head_depth is None or head_depth < 0:
            return evaluation.message(
                "Operate", "intnn", Expression("Operate", p, expr, n), 3
            )

        if head_depth == 0:
            # Act like Apply
            return Expression(p, expr)

        if expr.is_atom():
            return expr

        expr = expr.copy()
        e = expr

        for i in range(1, head_depth):
            e = e.head
            if e.is_atom():
                # n is higher than the depth of heads in expr: return
                # expr unmodified.
                return expr

        # Otherwise, if we get here, e.head points to the head we need
        # to apply p to. Python's reference semantics mean that this
        # assignment modifies expr as well.
        e.set_head(Expression(p, e.head))

        return expr


class Through(Builtin):
    """
    <dl>
    <dt>'Through[$p$[$f$][$x$]]'
        <dd>gives $p$[$f$[$x$]].
    </dl>

    >> Through[f[g][x]]
     = f[g[x]]
    >> Through[p[f, g][x]]
     = p[f[x], g[x]]

    #> Through[p[f, g][x, y]]
     = p[f[x, y], g[x, y]]
    #> Through[p[f, g][]]
     = p[f[], g[]]
    #> Through[p[f, g]]
     = Through[p[f, g]]
    #> Through[f[][x]]
     = f[]
    """

    def apply(self, p, args, x, evaluation):
        "Through[p_[args___][x___]]"

        items = []
        for leaf in args.get_sequence():
            items.append(Expression(leaf, *x.get_sequence()))
        return Expression(p, *items)


class ByteCount(Builtin):
    """
    <dl>
    <dt>'ByteCount[$expr$]'
        <dd>gives the internal memory space used by $expr$, in bytes.
    </dl>

    The results may heavily depend on the Python implementation in use.
    """

    def apply(self, expression, evaluation):
        "ByteCount[expression_]"
        if not bytecount_support:
            return evaluation.message("ByteCount", "pypy")
        else:
            return Integer(count_bytes(expression))
