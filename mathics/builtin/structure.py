# -*- coding: utf8 -*-

from mathics.builtin.base import Builtin, Predefined, BinaryOperator, Test
from mathics.core.expression import (Expression, String, Symbol, Integer,
                                     strip_context)
from mathics.core.rules import Pattern

from mathics.builtin.lists import (python_levelspec, walk_levels,
                                   InvalidLevelspecError)


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
        'Sort[list_]'

        if list.is_atom():
            evaluation.message('Sort', 'normal')
        else:
            new_leaves = sorted(list.leaves)
            return Expression(list.head, *new_leaves)

    def apply_predicate(self, list, p, evaluation):
        'Sort[list_, p_]'

        if list.is_atom():
            evaluation.message('Sort', 'normal')
        else:
            def compare(e1, e2):
                result = Expression(p, e1, e2).evaluate(evaluation)
                if result.is_true():
                    result = Expression(p, e2, e1).evaluate(evaluation)
                    if result.is_true():
                        return 0
                    else:
                        return -1
                else:
                    return 1
            new_leaves = sorted(list.leaves, cmp=compare)
            return Expression(list.head, *new_leaves)


class PatternsOrderedQ(Builtin):
    """
    >> PatternsOrderedQ[x__, x_]
     = False
    >> PatternsOrderedQ[x_, x__]
     = True
    >> PatternsOrderedQ[b, a]
     = True
    """

    def apply(self, p1, p2, evaluation):
        'PatternsOrderedQ[p1_, p2_]'

        result = cmp(p1.get_sort_key(True), p2.get_sort_key(True))
        if result <= 0:
            return Symbol('True')
        else:
            return Symbol('False')


class OrderedQ(Builtin):
    """
    >> OrderedQ[a, b]
     = True
    >> OrderedQ[b, a]
     = False
    """

    def apply(self, e1, e2, evaluation):
        'OrderedQ[e1_, e2_]'

        result = cmp(e1, e2)
        if result <= 0:
            return Symbol('True')
        else:
            return Symbol('False')


class Head(Builtin):
    """
    >> Head[a * b]
     = Times
    >> Head[6]
     = Integer
    >> Head[x]
     = Symbol
    """

    def apply(self, expr, evaluation):
        'Head[expr_]'

        return expr.get_head()


class ApplyLevel(BinaryOperator):
    """
    <dl>
    <dt>'ApplyLevel[$f$, $expr$]' or '$f$ @@@ $expr$'
        <dd>is equivalent to 'Apply[$f$, $expr$, {1}]'.
    </dl>

    >> f @@@ {{a, b}, {c, d}}
     = {f[a, b], f[c, d]}
    """

    operator = '@@@'
    precedence = 620
    grouping = 'Right'

    rules = {
        'ApplyLevel[f_, expr_]': 'Apply[f, expr, {1}]',
    }


class Apply(BinaryOperator):
    u"""
    <dl>
    <dt>'Apply[$f$, $expr$]' or '$f$ @@ $expr$'
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

    operator = '@@'
    precedence = 620
    grouping = 'Right'

    options = {
        'Heads': 'False',
    }

    def apply_invalidlevel(self, f, expr, ls, evaluation, options={}):
        'Apply[f_, expr_, ls_, OptionsPattern[Apply]]'

        evaluation.message('Apply', 'level', ls)

    def apply(self, f, expr, ls, evaluation, options={}):
        '''Apply[f_, expr_, Optional[Pattern[ls, _?LevelQ], {0}],
                OptionsPattern[Apply]]'''

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            evaluation.message('Apply', 'level', ls)
            return

        def callback(level):
            if level.is_atom():
                return level
            else:
                return Expression(f, *level.leaves)

        heads = self.get_option(options, 'Heads', evaluation).is_true()
        result, depth = walk_levels(
            expr, start, stop, heads=heads, callback=callback)

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

    operator = '/@'
    precedence = 620
    grouping = 'Right'

    options = {
        'Heads': 'False',
    }

    def apply_invalidlevel(self, f, expr, ls, evaluation, options={}):
        'Map[f_, expr_, ls_, OptionsPattern[Map]]'

        evaluation.message('Map', 'level', ls)

    def apply_level(self, f, expr, ls, evaluation, options={}):
        '''Map[f_, expr_, Optional[Pattern[ls, _?LevelQ], {1}],
                OptionsPattern[Map]]'''

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            evaluation.message('Map', 'level', ls)
            return

        def callback(level):
            return Expression(f, level)

        heads = self.get_option(options, 'Heads', evaluation).is_true()
        result, depth = walk_levels(
            expr, start, stop, heads=heads, callback=callback)

        return result


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
        'Heads': 'False',
    }

    def apply_invalidlevel(self, f, expr, ls, evaluation, options={}):
        'MapIndexed[f_, expr_, ls_, OptionsPattern[MapIndexed]]'

        evaluation.message('MapIndexed', 'level', ls)

    def apply_level(self, f, expr, ls, evaluation, options={}):
        '''MapIndexed[f_, expr_, Optional[Pattern[ls, _?LevelQ], {1}],
                OptionsPattern[MapIndexed]]'''

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            evaluation.message('MapIndexed', 'level', ls)
            return

        def callback(level, pos):
            return Expression(f, level, Expression('List', *[
                Integer(p) for p in pos]))

        heads = self.get_option(options, 'Heads', evaluation).is_true()
        result, depth = walk_levels(expr, start, stop, heads=heads,
                                    callback=callback, include_pos=True)

        return result


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
        'tdlen': "Objects of unequal length cannot be combined.",
    }

    rules = {
        'Thread[f_[args___]]': 'Thread[f[args], List]',
    }

    def apply(self, f, args, h, evaluation):
        'Thread[f_[args___], h_]'

        args = args.get_sequence()
        expr = Expression(f, *args)
        threaded, result = expr.thread(evaluation, head=h)
        return result


class FreeQ(Builtin):
    """
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
    """

    def apply(self, expr, form, evaluation):
        'FreeQ[expr_, form_]'

        form = Pattern.create(form)
        if expr.is_free(form, evaluation):
            return Symbol('True')
        else:
            return Symbol('False')


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
        'Flatten[expr_]': 'Flatten[expr, Infinity, Head[expr]]',
        'Flatten[expr_, n_]': 'Flatten[expr, n, Head[expr]]',
    }

    messages = {
        'flpi': (
            "Levels to be flattened together in `1` "
            "should be lists of positive integers."),
        'flrep': (
            "Level `1` specified in `2` should not be repeated."),
        'fldep': (
            "Level `1` specified in `2` exceeds the levels, `3`, "
            "which can be flattened together in `4`."),
    }

    def apply_list(self, expr, n, h, evaluation):
        'Flatten[expr_, n_List, h_]'

        ## prepare levels
        # find max depth which matches `h`
        expr, max_depth = walk_levels(expr)
        max_depth = {'max_depth': max_depth}    # hack to modify max_depth from callback

        def callback(expr, pos):
            if len(pos) < max_depth['max_depth'] and (expr.is_atom() or expr.head != h):
                max_depth['max_depth'] = len(pos)
            return expr
        expr, depth = walk_levels(expr, callback=callback, include_pos=True, start=0)
        max_depth = max_depth['max_depth']

        levels = n.to_python()

        # mappings
        if isinstance(levels, list) and all(isinstance(level, int) for level in levels):
            levels = [levels]

        # verify levels is list of lists of positive ints
        if not (isinstance(levels, list) and len(levels) > 0):
            evaluation.message('Flatten', 'flpi', n)
            return
        seen_levels = []
        for level in levels:
            if not (isinstance(level, list) and len(level) > 0):
                evaluation.message('Flatten', 'flpi', n)
                return
            for l in level:
                if not (isinstance(l, int) and l > 0):
                    evaluation.message('Flatten', 'flpi', n)
                    return
                if l in seen_levels:
                    # level repeated
                    evaluation.message('Flatten', 'flrep', l)
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
                    evaluation.message('Flatten', 'fldep', l, n, max_depth, expr)
                    return

        ## assign new indices to each leaf
        new_indices = {}

        def callback(expr, pos):
            if len(pos) == max_depth:
                new_depth = tuple(tuple(pos[i - 1] for i in level) for level in levels)
                new_indices[new_depth] = expr
            return expr
        expr, depth = walk_levels(expr, callback=callback, include_pos=True)

        ## build new tree inserting nodes as needed
        result = Expression(h)
        leaves = new_indices.items()
        leaves.sort()

        def insert_leaf(expr, leaves):
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
            for group in grouped_leaves:
                if len(group[0][0]) == 0:   # bottom level leaf
                    assert len(group) == 1
                    expr.leaves.append(group[0][1])
                else:
                    expr.leaves.append(Expression(h))
                    insert_leaf(expr.leaves[-1], group)
        insert_leaf(result, leaves)
        return result

    def apply(self, expr, n, h, evaluation):
        'Flatten[expr_, n_, h_]'

        if n == Expression('DirectedInfinity', 1):
            n = None
        else:
            n_int = n.get_int_value()
            if n_int is None or n_int < 0:
                return evaluation.message('Flatten', 'flpi', n)
            n = n_int

        return expr.flatten(h, level=n)


class Null(Predefined):
    """
    'Null' is the implicit result of expressions that do not yield a result:
    >> FullForm[a:=b]
     = Null

    It is not displayed in StandardForm,
    >> a:=b
    in contrast to the empty string:
    >> ""
     = 
    (watch the empty line).
    """


class AtomQ(Test):
    """
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
    u"""
    'Symbol' is the head of symbols.
    >> Head[x]
     = Symbol
    You can use 'Symbol' to create symbols from strings:
    >> Symbol["x"] + Symbol["x"]
     = 2 x

    #> {\[Eta], \[CapitalGamma]\[Beta], Z\[Infinity], \[Angle]XYZ, \[FilledSquare]r, i\[Ellipsis]j}
     = {\u03b7, \u0393\u03b2, Z\u221e, \u2220XYZ, \u25a0r, i\u2026j}
    """

    name = 'Symbol'
    attributes = ('Locked',)

    messages = {
        'symname': ("The string `1` cannot be used for a symbol name. "
                    "A symbol name must start with a letter "
                    "followed by letters and numbers."),
    }

    def apply(self, string, evaluation):
        'Symbol[string_String]'

        from mathics.core.parser import is_symbol_name

        text = string.get_string_value()
        if is_symbol_name(text):
            return Symbol(evaluation.definitions.lookup_name(string.value))
        else:
            evaluation.message('Symbol', 'symname', string)


class SymbolName(Builtin):
    """
    >> SymbolName[x] // InputForm
     = "x"
    """

    def apply(self, symbol, evaluation):
        'SymbolName[symbol_Symbol]'

        # MMA docs say "SymbolName always gives the short name,
        # without any context"
        return String(strip_context(symbol.get_name()))


class Depth(Builtin):
    """
    <dl>
    <dt>'Depth[$expr$]'
    <dd>gives the depth of $expr$
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
        'Depth[expr_]'
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
        'intnn': "Non-negative integer expected at position `2` in `1`.",
    }

    def apply(self, p, expr, n, evaluation):
        'Operate[p_, expr_, Optional[n_, 1]]'

        head_depth = n.get_int_value()
        if head_depth is None or head_depth < 0:
            return evaluation.message('Operate', 'intnn',
                                      Expression('Operate', p, expr, n), 3)

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
        e.head = Expression(p, e.head)

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
        'Through[p_[args___][x___]]'

        items = []
        for leaf in args.get_sequence():
            items.append(Expression(leaf, *x.get_sequence()))
        return Expression(p, *items)
