# -*- coding: utf8 -*-

"""
List functions
"""

from mathics.builtin.base import (
    Builtin, Test, InvalidLevelspecError,
    PartError, PartDepthError, PartRangeError, SympyFunction)
from mathics.builtin.scoping import dynamic_scoping
from mathics.core.expression import Expression, String, Symbol, Integer, Number
from mathics.core.evaluation import BreakInterrupt, ContinueInterrupt
from mathics.core.rules import Pattern
from mathics.core.convert import from_sympy
from mathics.builtin.algebra import cancel

import sympy


class List(Builtin):
    """
    'List' is the head of lists.
    >> Head[{1, 2, 3}]
     = List

    Lists can be nested:
    >> {{a, b, {c, d}}}
     = {{a, b, {c, d}}}
    """

    attributes = ('Locked',)

    def apply_makeboxes(self, items, f, evaluation):
        '''MakeBoxes[{items___},
            f:StandardForm|TraditionalForm|OutputForm|InputForm]'''

        items = items.get_sequence()
        return Expression(
            'RowBox', Expression('List', *list_boxes(items, f, "{", "}")))


class ListQ(Test):
    """
    <dl>
    <dt>'ListQ[$expr$]'
        <dd>tests whether $expr$ is a 'List'.
    </dl>

    >> ListQ[{1, 2, 3}]
     = True
    >> ListQ[{{1, 2}, {3, 4}}]
     = True
    >> ListQ[x]
     = False
    """

    def test(self, expr):
        return expr.get_head_name() == 'List'


class NotListQ(Test):
    def test(self, expr):
        return expr.get_head_name() != 'List'


def list_boxes(items, f, open=None, close=None):
    result = [Expression('MakeBoxes', item, f) for item in items]
    if f.get_name() in ('OutputForm', 'InputForm'):
        sep = ", "
    else:
        sep = ","
    result = riffle(result, String(sep))
    if len(items) > 1:
        result = Expression('RowBox', Expression('List', *result))
    elif items:
        result = result[0]
    if result:
        result = [result]
    else:
        result = []
    if open is not None and close is not None:
        return [String(open)] + result + [String(close)]
    else:
        return result


class Length(Builtin):
    """
    >> Length[{1, 2, 3}]
     = 3
    'Length' operates on the 'FullForm' of expressions:
    >> Length[Exp[x]]
     = 2
    >> FullForm[Exp[x]]
     = Power[E, x]
    The length of atoms is 0:
    >> Length[a]
     = 0
    Note that rational and complex numbers are atoms, although their 'FullForm' might suggest the opposite:
    >> Length[1/3]
     = 0
    >> FullForm[1/3]
     = Rational[1, 3]
    """

    def apply(self, expr, evaluation):
        'Length[expr_]'

        if expr.is_atom():
            return Integer(0)
        else:
            return Integer(len(expr.leaves))


class Span(Builtin):
    """
    'Span' is the head of span ranges like '1;;3'.
    >> ;; // FullForm
     = Span[1, All]
    >> 1;;4;;2 // FullForm
     = Span[1, 4, 2]
    >> 2;;-2 // FullForm
     = Span[2, -2]
    >> ;;3 // FullForm
     = Span[1, 3]

    ## Test parsing : 8 cases to consider
    #> a ;; b ;; c // FullForm
     = Span[a, b, c]
    #>   ;; b ;; c // FullForm
     = Span[1, b, c]
    #> a ;;   ;; c // FullForm
     = Span[a, All, c]
    #>   ;;   ;; c // FullForm
     = Span[1, All, c]
    #> a ;; b      // FullForm
     = Span[a, b]
    #>   ;; b      // FullForm
     = Span[1, b]
    #> a ;;        // FullForm
     = Span[a, All]
    #>   ;;        // FullForm
     = Span[1, All]
    """

    # operator = ';;'
    # precedence = 305
    pass


def join_lists(lists):
    new_list = []
    for list in lists:
        new_list.extend(list)
    return new_list


def get_part(list, indices):
    " Simple part extraction. indices must be a list of python integers. "

    def rec(cur, rest):
        if rest:
            pos = rest[0]
            if cur.is_atom():
                raise PartDepthError
            try:
                if pos > 0:
                    part = cur.leaves[pos - 1]
                elif pos == 0:
                    part = cur.head
                else:
                    part = cur.leaves[pos]
            except IndexError:
                raise PartRangeError
            return rec(part, rest[1:])
        else:
            return cur
    return rec(list, indices)


def set_part(list, indices, new):
    " Simple part replacement. indices must be a list of python integers. "

    def rec(cur, rest):
        if len(rest) > 1:
            pos = rest[0]
            if cur.is_atom():
                raise PartDepthError
            try:
                if pos > 0:
                    part = cur.leaves[pos - 1]
                elif pos == 0:
                    part = cur.head
                else:
                    part = cur.leaves[pos]
            except IndexError:
                raise PartRangeError
            rec(part, rest[1:])
        elif len(rest) == 1:
            pos = rest[0]
            if cur.is_atom():
                raise PartDepthError
            try:
                if pos > 0:
                    cur.leaves[pos - 1] = new
                elif pos == 0:
                    cur.head = new
                else:
                    cur.leaves[pos] = new
            except IndexError:
                raise PartRangeError

    rec(list, indices)


def walk_parts(list_of_list, indices, evaluation, assign_list=None):
    list = list_of_list[0]

    # To get rid of duplicate entries (TODO: could be made faster!)
    list = list.copy()

    list.set_positions()
    list_of_list = [list]

    result = list.copy()
    result.set_positions()

    inner_list = [result]   # changed in loop

    list_of_result = [result]   # to be able to change it in replace_result

    def replace_item(all, item, new):
        if item.position is None:
            all[0] = new
        else:
            item.position.replace(new)

    for index in indices:
        index = index.evaluate(evaluation)
        if index.has_form('Span', None):
            if len(index.leaves) > 3:
                evaluation.message('Part', 'span', index)
                return False
            start = 1
            stop = None
            step = 1
            if len(index.leaves) > 0:
                start = index.leaves[0].get_int_value()
            if len(index.leaves) > 1:
                stop = index.leaves[1].get_int_value()
                if stop is None:
                    if index.leaves[1].get_name() == 'All':
                        stop = None
                    else:
                        evaluation.message('Part', 'span', index)
                        return False
            if len(index.leaves) > 2:
                step = index.leaves[2].get_int_value()
            if start is None or step is None:
                evaluation.message('Part', 'span', index)
                return False
            start, stop = python_seq(start, stop)
            for inner in inner_list:
                if inner.is_atom():
                    evaluation.message('Part', 'partd')
                    return False
                if stop is None:
                    inner.leaves = inner.leaves[start::step]
                else:
                    inner.leaves = inner.leaves[start:stop:step]
                inner.original = None
                inner.set_positions()
            inner_list = join_lists(inner.leaves for inner in inner_list)
        elif index.has_form('List', None):
            index_list = index
            indices = []
            for index in index_list.leaves:
                if not isinstance(index, Integer):
                    evaluation.message('Part', 'pspec', index_list)
                    return False
                index = index.value
                if index > 0:
                    py_index = index - 1
                else:
                    py_index = index
                indices.append((py_index, index))

            for inner in inner_list:
                if inner.is_atom():
                    evaluation.message('Part', 'partd')
                    return False

                new_leaves = []
                for py_index, index in indices:
                    try:
                        if index != 0:
                            part = inner.leaves[py_index]
                        else:
                            part = inner.head
                        new_leaves.append(part)
                    except IndexError:
                        evaluation.message('Part', 'partw', index, inner)
                        return False
                inner.leaves = new_leaves
                inner.original = None
                inner.set_positions()
            inner_list = join_lists(inner.leaves for inner in inner_list)
        elif isinstance(index, Integer):
            index = index.value
            if index > 0:
                py_index = index - 1
            else:
                py_index = index
            for inner in inner_list:
                if inner.is_atom():
                    evaluation.message('Part', 'partd')
                    return False
                try:
                    if index != 0:
                        part = inner.leaves[py_index]
                    else:
                        part = inner.head
                except IndexError:
                    evaluation.message('Part', 'partw', index, inner)
                    return False
                replace_item(list_of_result, inner, part)
                part.set_positions()
            inner_list = [inner.leaves[py_index] for inner in inner_list]

    result = list_of_result[0]

    if assign_list is not None:
        def process_level(item, assignment):
            if item.is_atom():
                replace_item(list_of_list, item.original, assignment)
            elif (assignment.get_head_name() != 'List' or
                  len(item.leaves) != len(assignment.leaves)):
                if item.original:
                    replace_item(list_of_list, item.original, assignment)
                else:
                    for leaf in item.leaves:
                        process_level(leaf, assignment)
            else:
                for sub_item, sub_assignment in zip(item.leaves,
                                                    assignment.leaves):
                    process_level(sub_item, sub_assignment)
        process_level(result, assign_list)
        return list_of_list[0]
    else:
        return result


def is_in_level(current, depth, start=1, stop=None):
    if stop is None:
        stop = current
    if start < 0:
        start += current + depth + 1
    if stop < 0:
        stop += current + depth + 1
    return start <= current <= stop


def walk_levels(expr, start=1, stop=None, current=0, heads=False,
                callback=lambda l: l, include_pos=False, cur_pos=[]):
    if expr.is_atom():
        depth = 0
        new_expr = expr
    else:
        depth = 0
        if heads:
            head, head_depth = walk_levels(
                expr.head, start, stop, current + 1, heads, callback,
                include_pos, cur_pos + [0])
        else:
            head = expr.head
        leaves = []
        for index, leaf in enumerate(expr.leaves):
            leaf, leaf_depth = walk_levels(
                leaf, start, stop, current + 1, heads, callback, include_pos,
                cur_pos + [index + 1])
            if leaf_depth + 1 > depth:
                depth = leaf_depth + 1
            leaves.append(leaf)
        new_expr = Expression(head, *leaves)
    if is_in_level(current, depth, start, stop):
        if include_pos:
            new_expr = callback(new_expr, cur_pos)
        else:
            new_expr = callback(new_expr)
    return new_expr, depth


def python_levelspec(levelspec):
    def value_to_level(expr):
        value = expr.get_int_value()
        if value is None:
            if expr == Expression('DirectedInfinity', 1):
                return None
            else:
                raise InvalidLevelspecError
        else:
            return value

    if levelspec.has_form('List', None):
        values = [value_to_level(leaf) for leaf in levelspec.leaves]
        if len(values) == 1:
            return values[0], values[0]
        elif len(values) == 2:
            return values[0], values[1]
        else:
            raise InvalidLevelspecError
    else:
        return 1, value_to_level(levelspec)


class Level(Builtin):
    """
    <dl>
    <dt>'Level[$expr$, $levelspec$]'
        <dd>gives a list of all subexpressions of $expr$ at the level(s) specified by $levelspec$.
    </dl>

    Level uses standard level specifications:

    <dl>
    <dt>$n$
        <dd>levels 1 through $n$
    <dt>'Infinity'
        <dd>all levels from level 1
    <dt>'{$n$}'
        <dd>level $n$ only
    <dt>'{$m$, $n$}'
        <dd>levels $m$ through $n$
    </dl>

    Level 0 corresponds to the whole expression.

    A negative level '-$n$' consists of parts with depth $n$.

    Level -1 is the set of atoms in an expression:
    >> Level[a + b ^ 3 * f[2 x ^ 2], {-1}]
     = {a, b, 3, 2, x, 2}

    >> Level[{{{{a}}}}, 3]
     = {{a}, {{a}}, {{{a}}}}
    >> Level[{{{{a}}}}, -4]
     = {{{{a}}}}
    >> Level[{{{{a}}}}, -5]
     = {}

    >> Level[h0[h1[h2[h3[a]]]], {0, -1}]
     = {a, h3[a], h2[h3[a]], h1[h2[h3[a]]], h0[h1[h2[h3[a]]]]}

    Use the option 'Heads -> True' to include heads:
    >> Level[{{{{a}}}}, 3, Heads -> True]
     = {List, List, List, {a}, {{a}}, {{{a}}}}
    >> Level[x^2 + y^3, 3, Heads -> True]
     = {Plus, Power, x, 2, x ^ 2, Power, y, 3, y ^ 3}

    >> Level[a ^ 2 + 2 * b, {-1}, Heads -> True]
     = {Plus, Power, a, 2, Times, 2, b}
    >> Level[f[g[h]][x], {-1}, Heads -> True]
     = {f, g, h, x}
    >> Level[f[g[h]][x], {-2, -1}, Heads -> True]
     = {f, g, h, g[h], x, f[g[h]][x]}
    """

    options = {
        'Heads': 'False',
    }

    def apply(self, expr, ls, evaluation, options={}):
        'Level[expr_, ls_, OptionsPattern[Level]]'

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            evaluation.message('Level', 'level', ls)
            return
        result = []

        def callback(level):
            result.append(level)
            return level

        heads = self.get_option(options, 'Heads', evaluation).is_true()
        walk_levels(expr, start, stop, heads=heads, callback=callback)
        return Expression('List', *result)


class LevelQ(Test):
    """
    <dl>
    <dt>'LevelQ[$expr$]'
        <dd>tests whether $expr$ is a valid level specification.
    </dl>

    >> LevelQ[2]
     = True
    >> LevelQ[{2, 4}]
     = True
    >> LevelQ[Infinity]
     = True
    >> LevelQ[a + b]
     = False
    """

    def test(self, ls):
        try:
            start, stop = python_levelspec(ls)
            return True
        except InvalidLevelspecError:
            return False


def python_seq(start, stop):
    if start > 0:
        start -= 1
    if stop is not None and stop < 0:
        stop += 1
        if stop == 0:
            stop = None
    return start, stop


def convert_seq(seq):
    start, stop, step = 1, None, 1
    name = seq.get_name()
    value = seq.get_int_value()
    if name == 'All':
        pass
    elif name == 'None':
        stop = 0
    elif value is not None:
        if value > 0:
            stop = value
        else:
            start = value
    elif seq.has_form('List', 1, 2, 3):
        if len(seq.leaves) == 1:
            start = stop = seq.leaves[0].get_int_value()
            if stop is None:
                return False
        else:
            start = seq.leaves[0].get_int_value()
            stop = seq.leaves[1].get_int_value()
            if start is None or stop is None:
                return False
        if len(seq.leaves) == 3:
            step = seq.leaves[2].get_int_value()
            if step is None:
                return False
    else:
        return False
    return (start, stop, step)


class Part(Builtin):
    """
    >> A = {a, b, c, d};
    >> A[[3]]
     = c

    Negative indizes count from the end:
    >> {a, b, c}[[-2]]
     = b

    'Part' can be applied on any expression, not necessarily lists.
    >> (a + b + c)[[2]]
     = b
    '$expr$[[0]]' gives the head of $expr$:
    >> (a + b + c)[[0]]
     = Plus

    Parts of nested lists:
    >> M = {{a, b}, {c, d}};
    >> M[[1, 2]]
     = b

    You can use 'Span' to specify a range of parts:
    >> {1, 2, 3, 4}[[2;;4]]
     = {2, 3, 4}
    >> {1, 2, 3, 4}[[2;;-1]]
     = {2, 3, 4}

    A list of parts extracts elements at certain indices:
    >> {a, b, c, d}[[{1, 3, 3}]]
     = {a, c, c}

    Get a certain column of a matrix:
    >> B = {{a, b, c}, {d, e, f}, {g, h, i}};
    >> B[[;;, 2]]
     = {b, e, h}
    Extract a submatrix of 1st and 3rd row and the two last columns:
    >> B = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    >> B[[{1, 3}, -2;;-1]]
     = {{2, 3}, {8, 9}}

    Further examples:
    >> (a+b+c+d)[[-1;;-2]]
     = 0
    >> x[[2]]
     : Part specification is longer than depth of object.
     = x[[2]]

    Assignments to parts are possible:
    >> B[[;;, 2]] = {10, 11, 12}
     = {10, 11, 12}
    >> B
     = {{1, 10, 3}, {4, 11, 6}, {7, 12, 9}}
    >> B[[;;, 3]] = 13
     = 13
    >> B
     = {{1, 10, 13}, {4, 11, 13}, {7, 12, 13}}
    >> B[[1;;-2]] = t;
    >> B
     = {t, t, {7, 12, 13}}

    >> F = Table[i*j*k, {i, 1, 3}, {j, 1, 3}, {k, 1, 3}];
    >> F[[;; All, 2 ;; 3, 2]] = t;
    >> F
     = {{{1, 2, 3}, {2, t, 6}, {3, t, 9}}, {{2, 4, 6}, {4, t, 12}, {6, t, 18}}, {{3, 6, 9}, {6, t, 18}, {9, t, 27}}}
    >> F[[;; All, 1 ;; 2, 3 ;; 3]] = k;
    >> F
     = {{{1, 2, k}, {2, t, k}, {3, t, 9}}, {{2, 4, k}, {4, t, k}, {6, t, 18}}, {{3, 6, k}, {6, t, k}, {9, t, 27}}}

    Of course, part specifications have precedence over most arithmetic operations:
    >> A[[1]] + B[[2]] + C[[3]] // Hold // FullForm
     = Hold[Plus[Part[A, 1], Part[B, 2], Part[C, 3]]]

    #> a = {2,3,4}; i = 1; a[[i]] = 0; a
     = {0, 3, 4}
    """

    attributes = ('NHoldRest', 'ReadProtected')

    def apply_makeboxes(self, list, i, f, evaluation):
        '''MakeBoxes[Part[list_, i___],
            f:StandardForm|TraditionalForm|OutputForm|InputForm]'''

        i = i.get_sequence()
        list = Expression('MakeBoxes', list, f)
        if f.get_name() in ('OutputForm', 'InputForm'):
            open, close = "[[", "]]"
        else:
            open, close = u"\u301a", u"\u301b"
        indices = list_boxes(i, f, open, close)
        result = Expression('RowBox', Expression('List', list, *indices))
        return result

    def apply(self, list, i, evaluation):
        'Part[list_, i___]'

        indices = i.get_sequence()

        result = walk_parts([list], indices, evaluation)
        if result:
            return result


class Partition(Builtin):
    """
    <dl>
    <dt>'Partition[$list$, $n$]'
      <dd>partitions $list$ into sublists of length $n$.
    <dt>'Parition[$list$, $n$, $d$]'
      <dd>partitions $list$ into sublists of length $n$ which overlap $d$ indicies.
    </dl>

    >> Partition[{a, b, c, d, e, f}, 2]
     = {{a, b}, {c, d}, {e, f}}

    >> Partition[{a, b, c, d, e, f}, 3, 1]
     = {{a, b, c}, {b, c, d}, {c, d, e}, {d, e, f}}

    #> Partition[{a, b, c, d, e}, 2]
     = {{a, b}, {c, d}}
    """

    # TODO: Nested list length specifications
    """
    >> Partition[{{11, 12, 13}, {21, 22, 23}, {31, 32, 33}}, {2, 2}, 1]
     = {{{{11, 12}, {21, 22}}, {{12, 13}, {22, 23}}}, {{{21, 22}, {31, 32}}, {{22, 23}, {32, 33}}}}
    """

    rules = {
        'Parition[list_, n_, d_, k]': 'Partition[list, n, d, {k, k}]',
    }

    def chunks(self, l, n, d):
        assert n > 0 and d > 0
        return filter(lambda x: len(x) == n,
                      map(lambda i: l[i:i + n], xrange(0, len(l), d)))

    def apply_no_overlap(self, l, n, evaluation):
        'Partition[l_List, n_Integer]'
        # TODO: Error checking
        return Expression('List', *self.chunks(
            l.get_leaves(), n.get_int_value(), n.get_int_value()))

    def apply(self, l, n, d, evaluation):
        'Partition[l_List, n_Integer, d_Integer]'
        # TODO: Error checking
        return Expression('List', *self.chunks(
            l.get_leaves(), n.get_int_value(), d.get_int_value()))


class Extract(Builtin):
    """
    <dl>
    <dt>'Extract[$expr$, $list$]'
        <dd>extracts parts of $expr$ specified by $list$.
    <dt>'Extract[$expr$, {$list1$, $list2$, ...}]'
        <dd>extracts a list of parts.
    </dl>

    'Extract[$expr$, $i$, $j$, ...]' is equivalent to 'Part[$expr$, {$i$, $j$, ...}]'.

    >> Extract[a + b + c, {2}]
     = b
    >> Extract[{{a, b}, {c, d}}, {{1}, {2, 2}}]
     = {{a, b}, d}
    """

    attributes = ('NHoldRest',)

    rules = {
        'Extract[expr_, list_List]': 'Part[expr, Sequence @@ list]',
        'Extract[expr_, {lists___List}]': u'Extract[expr, #]& /@ {lists}',
    }


class First(Builtin):
    """
    <dl>
    <dt>'First[$expr$]'
        <dd>returns the first elment in $expr$.
    </dl>

    'First[$expr$]' is equivalent to '$expr$[[1]]'.

    >> First[{a, b, c}]
     = a
    >> First[a + b + c]
     = a
    >> First[x]
     : Nonatomic expression expected.
     = First[x]
    """

    def apply(self, expr, evaluation):
        'First[expr_]'

        if expr.is_atom():
            evaluation.message('First', 'normal')
            return
        return expr.leaves[0]


class Last(Builtin):
    """
    <dl>
    <dt>'Last[$expr$]'
        <dd>returns the last elment in $expr$.
    </dl>

    'Last[$expr$]' is equivalent to '$expr$[[-1]]'.

    >> Last[{a, b, c}]
     = c
    >> Last[x]
     : Nonatomic expression expected.
     = Last[x]
    """

    def apply(self, expr, evaluation):
        'Last[expr_]'

        if expr.is_atom():
            evaluation.message('Last', 'normal')
            return
        return expr.leaves[-1]


class Most(Builtin):
    """
    <dl>
    <dt>'Most[$expr$]'
        <dd>returns $expr$ with the last element removed.
    </dl>

    'Most[$expr$]' is equivalent to '$expr$[[;;-2]]'.

    >> Most[{a, b, c}]
     = {a, b}
    >> Most[a + b + c]
     = a + b
    >> Most[x]
     : Nonatomic expression expected.
     = Most[x]
    """

    def apply(self, expr, evaluation):
        'Most[expr_]'

        if expr.is_atom():
            evaluation.message('Most', 'normal')
            return
        return Expression(expr.head, *expr.leaves[:-1])


class Rest(Builtin):
    """
    <dl>
    <dt>'Rest[$expr$]'
        <dd>returns $expr$ with the first element removed.
    </dl>

    'Rest[$expr$]' is equivalent to '$expr$[[2;;]]'.

    >> Rest[{a, b, c}]
     = {b, c}
    >> Rest[a + b + c]
     = b + c
    >> Rest[x]
     : Nonatomic expression expected.
     = Rest[x]
    """

    def apply(self, expr, evaluation):
        'Rest[expr_]'

        if expr.is_atom():
            evaluation.message('Rest', 'normal')
            return
        return Expression(expr.head, *expr.leaves[1:])


class ReplacePart(Builtin):
    """
    >> ReplacePart[{a, b, c}, 1 -> t]
     = {t, b, c}
    >> ReplacePart[{{a, b}, {c, d}}, {2, 1} -> t]
     = {{a, b}, {t, d}}
    >> ReplacePart[{{a, b}, {c, d}}, {{2, 1} -> t, {1, 1} -> t}]
     = {{t, b}, {t, d}}
    >> ReplacePart[{a, b, c}, {{1}, {2}} -> t]
     = {t, t, c}

    Delayed rules are evaluated once for each replacement:
    >> n = 1;
    >> ReplacePart[{a, b, c, d}, {{1}, {3}} :> n++]
     = {1, b, 2, d}

    Non-existing parts are simply ignored:
    >> ReplacePart[{a, b, c}, 4 -> t]
     = {a, b, c}
    You can replace heads by replacing part 0:
    >> ReplacePart[{a, b, c}, 0 -> Times]
     = a b c
    (This is equivalent to 'Apply'.)

    Negative part numbers count from the end:
    >> ReplacePart[{a, b, c}, -1 -> t]
     = {a, b, t}
    """

    messages = {
        'reps': "`1` is not a list of replacement rules.",
    }

    rules = {
        'ReplacePart[expr_, (Rule|RuleDelayed)[i_, new_]]': (
            'ReplacePart[expr, {i -> new}]'),
        'ReplacePart[expr_, Pattern[rule, '
        'Rule|RuleDelayed][{indices___?(Head[#]===List&)}, new_]]': (
            'ReplacePart[expr, rule[#, new]& /@ {indices}]'),
    }

    def apply(self, expr, replacements, evaluation):
        'ReplacePart[expr_, {replacements___}]'

        new_expr = expr.copy()
        replacements = replacements.get_sequence()
        for replacement in replacements:
            if (not replacement.has_form('Rule', 2) and     # noqa
                not replacement.has_form('RuleDelayed', 2)):
                evaluation.message('ReplacePart', 'reps',
                                   Expression('List', *replacements))
                return
            position = replacement.leaves[0]
            replace = replacement.leaves[1]
            if position.has_form('List', None):
                position = position.leaves
            else:
                position = [position]
            for index, pos in enumerate(position):
                value = pos.get_int_value()
                if value is None:
                    position = None
                    break
                else:
                    position[index] = value
            if position is None:
                continue
            try:
                if replacement.get_head_name() == 'RuleDelayed':
                    replace_value = replace.evaluate(evaluation)
                else:
                    replace_value = replace
                set_part(new_expr, position, replace_value)
            except PartError:
                pass

        return new_expr


class Take(Builtin):
    """
    >> Take[{a, b, c, d}, 3]
     = {a, b, c}
    >> Take[{a, b, c, d}, -2]
     = {c, d}
    >> Take[{a, b, c, d, e}, {2, -2}]
     = {b, c, d}

    Take a submatrix:
    >> A = {{a, b, c}, {d, e, f}};
    >> Take[A, 2, 2]
     = {{a, b}, {d, e}}

    Take a single column:
    >> Take[A, All, {2}]
     = {{b}, {e}}
    """

    messages = {
        'take': "Cannot take positions `1` through `2` in `3`.",
    }

    def apply(self, list, seqs, evaluation):
        'Take[list_, seqs___]'

        seqs = seqs.get_sequence()

        list = list.copy()
        inner_list = [list]

        for seq in seqs:
            seq_tuple = convert_seq(seq)
            if not seq_tuple:
                evaluation.message('Take', 'seqs', seq)
                return
            start, stop, step = seq_tuple
            py_start, py_stop = python_seq(start, stop)
            for inner in inner_list:
                if (inner.is_atom() or      # noqa
                    abs(start) > len(inner.leaves) or
                    stop is not None and abs(stop) > len(inner.leaves)):
                    evaluation.message('Take', 'take', start, Symbol(
                        'Infinity') if stop is None else stop, inner)
                    return
                if stop is None:
                    inner.leaves = inner.leaves[py_start::step]
                else:
                    inner.leaves = inner.leaves[py_start:py_stop:step]
            inner_list = join_lists(inner.leaves for inner in inner_list)

        return list


class Drop(Builtin):
    """
    >> Drop[{a, b, c, d}, 3]
     = {d}
    >> Drop[{a, b, c, d}, -2]
     = {a, b}
    >> Drop[{a, b, c, d, e}, {2, -2}]
     = {a, e}

    Drop a submatrix:
    >> A = Table[i*10 + j, {i, 4}, {j, 4}]
     = {{11, 12, 13, 14}, {21, 22, 23, 24}, {31, 32, 33, 34}, {41, 42, 43, 44}}
    >> Drop[A, {2, 3}, {2, 3}]
     = {{11, 14}, {41, 44}}
    """

    messages = {
        'drop': "Cannot drop positions `1` through `2` in `3`.",
    }

    def apply(self, list, seqs, evaluation):
        'Drop[list_, seqs___]'

        seqs = seqs.get_sequence()

        list = list.copy()
        inner_list = [list]

        for seq in seqs:
            seq_tuple = convert_seq(seq)
            if not seq_tuple:
                evaluation.message('Drop', 'seqs', seq)
                return
            start, stop, step = seq_tuple
            py_start, py_stop = python_seq(start, stop)
            for inner in inner_list:
                if (inner.is_atom() or  # noqa
                    abs(start) > len(inner.leaves) or
                    stop is not None and abs(stop) > len(inner.leaves)):
                    evaluation.message('Drop', 'drop', start, stop, inner)
                    return
                if stop is None:
                    del inner.leaves[py_start::step]
                else:
                    del inner.leaves[py_start:py_stop:step]
            inner_list = join_lists(inner.leaves for inner in inner_list)

        return list


class Select(Builtin):
    """
    >> Select[{-3, 0, 1, 3, a}, #>0&]
     = {1, 3}
    >> Select[f[a, 2, 3], NumberQ]
     = f[2, 3]
    >> Select[a, True]
     : Nonatomic expression expected.
     = Select[a, True]
    """

    def apply(self, list, expr, evaluation):
        'Select[list_, expr_]'

        if list.is_atom():
            evaluation.message('Select', 'normal')
            return
        new_leaves = []
        for leaf in list.leaves:
            test = Expression(expr, leaf)
            if test.evaluate(evaluation).is_true():
                new_leaves.append(leaf)
        return Expression(list.head, *new_leaves)


class Split(Builtin):
    """
    <dl>
    <dt>'Split[$list$]'
      <dd>splits $list$ into collections of consecutive identical elements.
    <dt>'Split[$list$, $test$]'
      <dd>splits $list$ based on whether the function $test$ yields 'True' on consecutive elements.
    </dl>

    >> Split[{x, x, x, y, x, y, y, z}]
     = {{x, x, x}, {y}, {x}, {y, y}, {z}}

    #> Split[{x, x, x, y, x, y, y, z}, x]
     = {{x}, {x}, {x}, {y}, {x}, {y}, {y}, {z}}

    Split into increasing or decreasing runs of elements
    >> Split[{1, 5, 6, 3, 6, 1, 6, 3, 4, 5, 4}, Less]
     = {{1, 5, 6}, {3, 6}, {1, 6}, {3, 4, 5}, {4}}

    >> Split[{1, 5, 6, 3, 6, 1, 6, 3, 4, 5, 4}, Greater]
     = {{1}, {5}, {6, 3}, {6, 1}, {6, 3}, {4}, {5, 4}}

    Split based on first element
    >> Split[{x -> a, x -> y, 2 -> a, z -> c, z -> a}, First[#1] === First[#2] &]
     = {{x -> a, x -> y}, {2 -> a}, {z -> c, z -> a}}

    #> Split[{}]
     = {}
    """

    rules = {
        'Split[list_]': 'Split[list, SameQ]',
    }

    messages = {
        'normal': 'Nonatomic expression expected at position `1` in `2`.',
    }

    def apply(self, mlist, test, evaluation):
        'Split[mlist_, test_]'

        expr = Expression('Split', mlist, test)

        if mlist.is_atom():
            evaluation.message('Select', 'normal', 1, expr)
            return

        if len(mlist.leaves) == 0:
            result = []
        else:
            result = [[mlist.leaves[0]]]
            for leaf in mlist.leaves[1:]:
                applytest = Expression(test, result[-1][-1], leaf)
                if applytest.evaluate(evaluation).is_true():
                    result[-1].append(leaf)
                else:
                    result.append([leaf])

        return Expression(mlist.head, *[Expression('List', *l)
                                        for l in result])


class SplitBy(Builtin):
    """
    <dl>
    <dt>'Split[$list$, $f$]'
      <dd>splits $list$ into collections of consecutive elements that give the same result when $f$ is applied.
    </dl>

    >> SplitBy[Range[1, 3, 1/3], Round]
     = {{1, 4 / 3}, {5 / 3, 2, 7 / 3}, {8 / 3, 3}}

    >> SplitBy[{1, 2, 1, 1.2}, {Round, Identity}]
     = {{{1}}, {{2}}, {{1}, {1.2}}}

    >> SplitBy[{1, 2, 1, 1.2}, {Round, Identity}]
     = {{{1}}, {{2}}, {{1}, {1.2}}}

    #> SplitBy[Tuples[{1, 2}, 3], First]
     = {{{1, 1, 1}, {1, 1, 2}, {1, 2, 1}, {1, 2, 2}}, {{2, 1, 1}, {2, 1, 2}, {2, 2, 1}, {2, 2, 2}}}
    """

    rules = {
        'SplitBy[list_]': 'SplitBy[list, Identity]',
    }

    messages = {
        'normal': 'Nonatomic expression expected at position `1` in `2`.',
    }

    def apply(self, mlist, func, evaluation):
        'SplitBy[mlist_, func_?NotListQ]'

        expr = Expression('Split', mlist, func)

        if mlist.is_atom():
            evaluation.message('Select', 'normal', 1, expr)
            return

        plist = [l for l in mlist.leaves]

        result = [[plist[0]]]
        prev = Expression(func, plist[0]).evaluate(evaluation)
        for leaf in plist[1:]:
            curr = Expression(func, leaf).evaluate(evaluation)
            if curr == prev:
                result[-1].append(leaf)
            else:
                result.append([leaf])
            prev = curr

        return Expression(mlist.head, *[Expression('List', *l)
                                        for l in result])

    def apply_multiple(self, mlist, funcs, evaluation):
        'SplitBy[mlist_, funcs_?ListQ]'
        expr = Expression('Split', mlist, funcs)

        if mlist.is_atom():
            evaluation.message('Select', 'normal', 1, expr)
            return

        result = mlist
        for f in funcs.leaves[::-1]:
            result = self.apply(result, f, evaluation)

        return result


class Cases(Builtin):
    rules = {
        'Cases[list_, pattern_]': 'Select[list, MatchQ[#, pattern]&]',
    }


class MemberQ(Builtin):
    rules = {
        'MemberQ[list_, pattern_]': (
            'Length[Select[list, MatchQ[#, pattern]&]] > 0'),
    }


class Range(Builtin):
    """
    >> Range[5]
     = {1, 2, 3, 4, 5}
    >> Range[-3, 2]
     = {-3, -2, -1, 0, 1, 2}
    >> Range[0, 2, 1/3]
     = {0, 1 / 3, 2 / 3, 1, 4 / 3, 5 / 3, 2}
    """

    rules = {
        'Range[imax_?RealNumberQ]': 'Range[1, imax, 1]',
        'Range[imin_?RealNumberQ, imax_?RealNumberQ]': 'Range[imin, imax, 1]',
    }

    def apply(self, imin, imax, di, evaluation):
        'Range[imin_?RealNumberQ, imax_?RealNumberQ, di_?RealNumberQ]'

        imin = imin.value
        imax = imax.value
        di = di.value
        index = imin
        result = []
        while index <= imax:
            evaluation.check_stopped()
            result.append(Number.from_mp(index))
            index += di
        return Expression('List', *result)


class _IterationFunction(Builtin):
    """
    >> Sum[k, {k, Range[5]}]
     = 15
    """

    attributes = ('HoldAll',)
    rules = {
#        '%(name)s[expr_, {i_Symbol, imax_}]': (
#            '%(name)s[expr, {i, 1, imax, 1}]'),
        '%(name)s[expr_, {i_Symbol, imin_, imax_}]': (
            '%(name)s[expr, {i, imin, imax, 1}]'),
    }

    allow_loopcontrol = False
    throw_iterb = True

    def get_result(self, items):
        pass

    def apply_range(self, expr, i, imax, evaluation):
        '%(name)s[expr_, {i_Symbol, imax_}]'

        if imax.get_head_name() == 'Range':
            seq = Expression('Sequence', *(imax.evaluate(evaluation).leaves))
            return self.apply_list(expr, i, seq, evaluation)
        else:
            return self.apply_iter(expr, i, Integer(1), imax,
                                   Integer(1), evaluation)

    def apply_max(self, expr, imax, evaluation):
        '%(name)s[expr_, {imax_}]'

        index = 0
        imax = imax.evaluate(evaluation).get_real_value()
        if imax is None:
            if self.throw_iterb:
                evaluation.message(self.get_name(), 'iterb')
            return
        result = []
        while index < imax:
            evaluation.check_stopped()
            try:
                result.append(expr.evaluate(evaluation))
            except ContinueInterrupt:
                if self.allow_loopcontrol:
                    pass
                else:
                    raise
            except BreakInterrupt:
                if self.allow_loopcontrol:
                    break
                else:
                    raise
            index += 1
        return self.get_result(result)

    def apply_iter(self, expr, i, imin, imax, di, evaluation):
        '%(name)s[expr_, {i_Symbol, imin_, imax_, di_}]'

        if isinstance(self, SympyFunction) and di.get_int_value() == 1:
            whole_expr = Expression(
                self.get_name(), expr, Expression('List', i, imin, imax))
            sympy_expr = whole_expr.to_sympy()

            # apply Together to produce results similar to Mathematica
            result = sympy.together(sympy_expr)
            result = from_sympy(result)
            result = cancel(result)

            if not result.same(whole_expr):
                return result

        index = imin.evaluate(evaluation)
        imax = imax.evaluate(evaluation)
        di = di.evaluate(evaluation)

        result = []
        while True:
            cont = Expression('LessEqual', index, imax).evaluate(evaluation)
            if cont == Symbol('False'):
                break
            if not cont.is_true():
                if self.throw_iterb:
                    evaluation.message(self.get_name(), 'iterb')
                return

            evaluation.check_stopped()
            try:
                item = dynamic_scoping(
                    expr.evaluate, {i.name: index}, evaluation)
                result.append(item)
            except ContinueInterrupt:
                if self.allow_loopcontrol:
                    pass
                else:
                    raise
            except BreakInterrupt:
                if self.allow_loopcontrol:
                    break
                else:
                    raise
            index = Expression('Plus', index, di).evaluate(evaluation)
        return self.get_result(result)

    def apply_list(self, expr, i, items, evaluation):
        '%(name)s[expr_, {i_Symbol, {items___}}]'

        items = items.evaluate(evaluation).get_sequence()
        result = []
        for item in items:
            evaluation.check_stopped()
            try:
                item = dynamic_scoping(
                    expr.evaluate, {i.name: item}, evaluation)
                result.append(item)
            except ContinueInterrupt:
                if self.allow_loopcontrol:
                    pass
                else:
                    raise
            except BreakInterrupt:
                if self.allow_loopcontrol:
                    break
                else:
                    raise
        return self.get_result(result)

    def apply_multi(self, expr, first, sequ, evaluation):
        '%(name)s[expr_, first_, sequ__]'

        sequ = sequ.get_sequence()
        name = self.get_name()
        return Expression(name, Expression(name, expr, *sequ), first)


class ConstantArray(Builtin):
    """
    >> ConstantArray[a, 3]
     = {a, a, a}
    >> ConstantArray[a, {2, 3}]
     = {{a, a, a}, {a, a, a}}
    """

    rules = {
        'ConstantArray[c_, dims_]': 'Apply[Table[c, ##]&, List /@ dims]',
        'ConstantArray[c_, n_Integer]': 'ConstantArray[c, {n}]',
    }


class Array(Builtin):
    """
    >> Array[f, 4]
     = {f[1], f[2], f[3], f[4]}
    >> Array[f, {2, 3}]
     = {{f[1, 1], f[1, 2], f[1, 3]}, {f[2, 1], f[2, 2], f[2, 3]}}
    >> Array[f, {2, 3}, 3]
     = {{f[3, 3], f[3, 4], f[3, 5]}, {f[4, 3], f[4, 4], f[4, 5]}}
    >> Array[f, {2, 3}, {4, 6}]
     = {{f[4, 6], f[4, 7], f[4, 8]}, {f[5, 6], f[5, 7], f[5, 8]}}
    >> Array[f, {2, 3}, 1, Plus]
     = f[1, 1] + f[1, 2] + f[1, 3] + f[2, 1] + f[2, 2] + f[2, 3]

    #> Array[f, {2, 3}, {1, 2, 3}]
     : {2, 3} and {1, 2, 3} should have the same length.
     = Array[f, {2, 3}, {1, 2, 3}]
    #> Array[f, a]
     : Single or list of non-negative integers expected at position 2.
     = Array[f, a]
    #> Array[f, 2, b]
     : Single or list of non-negative integers expected at position 3.
     = Array[f, 2, b]
    """

    messages = {
        'plen': "`1` and `2` should have the same length.",
    }

    def apply(self, f, dimsexpr, origins, head, evaluation):
        'Array[f_, dimsexpr_, origins_:1, head_:List]'

        if dimsexpr.has_form('List', None):
            dims = dimsexpr.leaves[:]
        else:
            dims = [dimsexpr]
        for index, dim in enumerate(dims):
            value = dim.get_int_value()
            if value is None:
                evaluation.message('Array', 'ilsnn', 2)
                return
            dims[index] = value
        if origins.has_form('List', None):
            if len(origins.leaves) != len(dims):
                evaluation.message('Array', 'plen', dimsexpr, origins)
                return
            origins = origins.leaves[:]
        else:
            origins = [origins] * len(dims)
        for index, origin in enumerate(origins):
            value = origin.get_int_value()
            if value is None:
                evaluation.message('Array', 'ilsnn', 3)
                return
            origins[index] = value

        dims = zip(dims, origins)

        def rec(rest_dims, current):
            evaluation.check_stopped()
            if rest_dims:
                level = []
                count, origin = rest_dims[0]
                for index in range(origin, origin + count):
                    level.append(rec(rest_dims[1:], current + [index]))
                return Expression(head, *level)
            else:
                return Expression(f, *(Integer(index) for index in current))

        return rec(dims, [])


class Table(_IterationFunction):
    """
    >> Table[x, {4}]
     = {x, x, x, x}
    >> n = 0;
    >> Table[n = n + 1, {5}]
     = {1, 2, 3, 4, 5}
    >> Table[i, {i, 4}]
     = {1, 2, 3, 4}
    >> Table[i, {i, 2, 5}]
     = {2, 3, 4, 5}
    >> Table[i, {i, 2, 6, 2}]
     = {2, 4, 6}
    >> Table[i, {i, Pi, 2 Pi, Pi / 2}]
     = {Pi, 3 Pi / 2, 2 Pi}
    >> Table[x^2, {x, {a, b, c}}]
     = {a ^ 2, b ^ 2, c ^ 2}

    'Table' supports multi-dimensional tables:
    >> Table[{i, j}, {i, {a, b}}, {j, 1, 2}]
     = {{{a, 1}, {a, 2}}, {{b, 1}, {b, 2}}}

    #> Table[x, {x,0,1/3}]
     = {0}
    #> Table[x, {x, -0.2, 3.9}]
     = {-0.2, 0.8, 1.8, 2.8, 3.8}
    """

    def get_result(self, items):
        return Expression('List', *items)


class Join(Builtin):
    """
    'Join' concatenates lists.
    >> Join[{a, b}, {c, d, e}]
     = {a, b, c, d, e}
    >> Join[{{a, b}, {c, d}}, {{1, 2}, {3, 4}}]
     = {{a, b}, {c, d}, {1, 2}, {3, 4}}

    The concatenated expressions may have any head.
    >> Join[a + b, c + d, e + f]
     = a + b + c + d + e + f
    However, it must be the same for all expressions.
    >> Join[a + b, c * d]
     : Heads Plus and Times are expected to be the same.
     = Join[a + b, c d]

    #> Join[x, y]
     = Join[x, y]
    #> Join[x + y, z]
     = Join[x + y, z]
    #> Join[x + y, y z, a]
     : Heads Plus and Times are expected to be the same.
     = Join[x + y, y z, a]
    #> Join[x, y + z, y z]
     = Join[x, y + z, y z]
    """

    attributes = ('Flat', 'OneIdentity')

    def apply(self, lists, evaluation):
        'Join[lists___]'

        result = []
        head = None
        for list in lists.get_sequence():
            if list.is_atom():
                return
            if head is not None and list.get_head() != head:
                evaluation.message('Join', 'heads', head, list.get_head())
                return
            head = list.get_head()
            result.extend(list.leaves)

        if result:
            return Expression(head, *result)
        else:
            return Expression('List')


def get_tuples(items):
    if not items:
        yield []
    else:
        for item in items[0]:
            for rest in get_tuples(items[1:]):
                yield [item] + rest


class Tuples(Builtin):
    """
    <dl>
    <dt>'Tuples[$list$, $n$]'
        <dd>returns a list of all $n$-tuples of elements in $list$.
    <dt>'Tuples[{$list1$, $list2$, ...}]'
        <dd>returns a list of tuples with elements from the given lists.
    </dl>

    >> Tuples[{a, b, c}, 2]
     = {{a, a}, {a, b}, {a, c}, {b, a}, {b, b}, {b, c}, {c, a}, {c, b}, {c, c}}
    >> Tuples[{}, 2]
     = {}
    >> Tuples[{a, b, c}, 0]
     = {{}}

    >> Tuples[{{a, b}, {1, 2, 3}}]
     = {{a, 1}, {a, 2}, {a, 3}, {b, 1}, {b, 2}, {b, 3}}

    The head of $list$ need not be 'List':
    >> Tuples[f[a, b, c], 2]
     = {f[a, a], f[a, b], f[a, c], f[b, a], f[b, b], f[b, c], f[c, a], f[c, b], f[c, c]}
    However, when specifying multiple expressions, 'List' is always used:
    >> Tuples[{f[a, b], g[c, d]}]
     = {{a, c}, {a, d}, {b, c}, {b, d}}
    """

    def apply_n(self, expr, n, evaluation):
        'Tuples[expr_, n_]'

        if expr.is_atom():
            evaluation.message('Tuples', 'normal')
            return
        n = n.get_int_value()
        if n is None or n < 0:
            evaluation.message('Tuples', 'intnn')
            return
        items = expr.leaves

        def iterate(n_rest):
            evaluation.check_stopped()
            if n_rest <= 0:
                yield []
            else:
                for item in items:
                    for rest in iterate(n_rest - 1):
                        yield [item] + rest

        return Expression('List', *(Expression(expr.head, *leaves)
                                    for leaves in iterate(n)))

    def apply_lists(self, exprs, evaluation):
        'Tuples[{exprs___}]'

        exprs = exprs.get_sequence()
        items = []
        for expr in exprs:
            evaluation.check_stopped()
            if expr.is_atom():
                evaluation.message('Tuples', 'normal')
                return
            items.append(expr.leaves)

        return Expression('List', *(Expression('List', *leaves)
                                    for leaves in get_tuples(items)))


class Reap(Builtin):
    """
    <dl>
    <dt>'Reap[$expr$]'
        <dd>gives the result of evaluating $expr$, together with all values sown during
        this evaluation. Values sown with different tags are given in different lists.
    <dt>'Reap[$expr$, $pattern$]'
        <dd>only yields values sown with a tag matching $pattern$.
        'Reap[$expr$]' is equivalent to 'Reap[$expr$, _]'.
    <dt>'Reap[$expr$, {$pattern1$, $pattern2$, ...}]'
        <dd>uses multiple patterns.
    <dt>'Reap[$expr$, $pattern$, $f$]'
        <dd>applies $f$ on each tag and the corresponding values sown
        in the form '$f$[tag, {e1, e2, ...}]'.
    </dl>

    >> Reap[Sow[3]; Sow[1]]
     = {1, {{3, 1}}}

    >> Reap[Sow[2, {x, x, x}]; Sow[3, x]; Sow[4, y]; Sow[4, 1], {_Symbol, _Integer, x}, f]
     = {4, {{f[x, {2, 2, 2, 3}], f[y, {4}]}, {f[1, {4}]}, {f[x, {2, 2, 2, 3}]}}}

    Find the unique elements of a list, keeping their order:
    >> Reap[Sow[Null, {a, a, b, d, c, a}], _, # &][[2]]
     = {a, b, d, c}

    Sown values are reaped by the innermost matching 'Reap':
    >> Reap[Reap[Sow[a, x]; Sow[b, 1], _Symbol, Print["Inner: ", #1]&];, _, f]
     | Inner: x
     = {Null, {f[1, {b}]}}

    When no value is sown, an empty list is returned:
    >> Reap[x]
     = {x, {}}
    """

    attributes = ('HoldFirst',)

    rules = {
        'Reap[expr_, pattern_, f_]': (
            '{#[[1]], #[[2, 1]]}& [Reap[expr, {pattern}, f]]'),
        'Reap[expr_, pattern_]': 'Reap[expr, pattern, #2&]',
        'Reap[expr_]': 'Reap[expr, _]',
    }

    def apply(self, expr, patterns, f, evaluation):
        'Reap[expr_, {patterns___}, f_]'

        patterns = patterns.get_sequence()
        sown = [(Pattern.create(pattern), []) for pattern in patterns]

        def listener(e, tag):
            result = False
            for pattern, items in sown:
                if pattern.does_match(tag, evaluation):
                    for item in items:
                        if item[0].same(tag):
                            item[1].append(e)
                            break
                    else:
                        items.append((tag, [e]))
                    result = True
            return result

        evaluation.add_listener('sow', listener)
        try:
            result = expr.evaluate(evaluation)
            items = []
            for pattern, tags in sown:
                list = Expression('List')
                for tag, elements in tags:
                    list.leaves.append(Expression(
                        f, tag, Expression('List', *elements)))
                items.append(list)
            return Expression('List', result, Expression('List', *items))
        finally:
            evaluation.remove_listener('sow', listener)


class Sow(Builtin):
    """
    <dl>
    <dt>'Sow[$e$]'
        <dd>sends the value $e$ to the innermost 'Reap'.
    <dt>'Sow[$e$, $tag$]'
        <dd>sows $e$ using $tag$. 'Sow[$e$]' is equivalent to 'Sow[$e$, Null]'.
    <dt>'Sow[$e$, {$tag1$, $tag2$, ...}]'
        <dd>uses multiple tags.
    </dl>
    """

    rules = {
        'Sow[e_]': 'Sow[e, {Null}]',
        'Sow[e_, tag_]': 'Sow[e, {tag}]',
    }

    def apply(self, e, tags, evaluation):
        'Sow[e_, {tags___}]'

        tags = tags.get_sequence()
        for tag in tags:
            evaluation.publish('sow', e, tag)
        return e


class UnitVector(Builtin):
    """
    >> UnitVector[2]
     = {0, 1}
    >> UnitVector[4, 3]
     = {0, 0, 1, 0}
    """

    messages = {
        'nokun': "There is no unit vector in direction `1` in `2` dimensions.",
    }

    rules = {
        'UnitVector[k_Integer]': 'UnitVector[2, k]',
    }

    def apply(self, n, k, evaluation):
        'UnitVector[n_Integer, k_Integer]'

        n = n.get_int_value()
        k = k.get_int_value()
        if n is None or k is None:
            return
        if not 1 <= k <= n:
            evaluation.message('UnitVector', 'nokun', k, n)
            return

        def item(i):
            if i == k:
                return Integer(1)
            else:
                return Integer(0)

        return Expression('List', *(item(i) for i in range(1, n + 1)))


def riffle(items, sep):
    result = items[:1]
    for item in items[1:]:
        result.append(sep)
        result.append(item)
    return result


def riffle_lists(items, seps):
    if len(seps) + 1 < len(items):  # Use seperators cyclically
        seps = seps * (len(items) / len(seps) + 1)
    if len(seps) > len(items):
        seps = seps[:len(items) - 1]
    return [val for pair in (map(None, items, seps))
            for val in pair if val is not None]


class Riffle(Builtin):
    """
    >> Riffle[{a, b, c}, x]
     = {a, x, b, x, c}
    >> Riffle[{a, b, c}, {x, y, z}]
     = {a, x, b, y, c, z}
    >> Riffle[{a, b, c, d, e, f}, {x, y, z}]
     = {a, x, b, y, c, z, d, x, e, y, f}

    #> Riffle[{1, 2, 3, 4}, {x, y, z, t}]
     = {1, x, 2, y, 3, z, 4, t}
    #> Riffle[{1, 2}, {1, 2, 3}]
     = {1, 1, 2}
    #> Riffle[{1, 2}, {1, 2}]
     = {1, 1, 2, 2}
    """

    def apply(self, list, sep, evaluation):
        'Riffle[list_List, sep_]'

        if sep.has_form('List', None):
            return Expression('List',
                              *riffle_lists(list.get_leaves(), sep.leaves))
        else:
            return Expression('List', *riffle_lists(list.get_leaves(), [sep]))


class DeleteDuplicates(Builtin):
    """
    <dl>
    <dt>'DeleteDuplicates[$list$]'
      <dd>deletes duplicates from $list$.
    <dt>'DeleteDuplicates[$list$, $test$]'
      <dd>deletes elements from $list$ based on whether the function $test$ yields 'True' on pairs of elements.
    </dl>

    >> DeleteDuplicates[{1, 7, 8, 4, 3, 4, 1, 9, 9, 2, 1}]
     = {1, 7, 8, 4, 3, 9, 2}

    >> DeleteDuplicates[{3,2,1,2,3,4}, Less]
     = {3, 2, 1}

    #> DeleteDuplicates[{3,2,1,2,3,4}, Greater]
     = {3, 3, 4}

    #> DeleteDuplicates[{}]
     = {}
    """

    rules = {
        'DeleteDuplicates[list_]': 'DeleteDuplicates[list, SameQ]',
    }

    messages = {
        'normal': 'Nonatomic expression expected at position `1` in `2`.',
    }

    def apply(self, mlist, test, evaluation):
        'DeleteDuplicates[mlist_, test_]'

        expr = Expression('DeleteDuplicates', mlist, test)

        if mlist.is_atom():
            evaluation.message('Select', 'normal', 1, expr)
            return

        result = []
        for leaf in mlist.leaves:
            matched = False
            for res in result:
                applytest = Expression(test, res, leaf)
                if applytest.evaluate(evaluation).is_true():
                    matched = True
                    break
            if not matched:
                result.append(leaf)

        return Expression(mlist.head, *result)


class Complement(Builtin):
    """
    <dl>
    <dt>'Complement[$all$, $e1$, $e2$, ...]'
      <dd>returns an expression containing the elements in the set $all$ that
      are not in any of $e1$, $e2$, etc.
    <dt>'Complement[$all$, $e1$, $e2$, ..., SameTest->$test$]'
      <dd>applies $test$ to the elements in $all$ and each of the $ei$
      to determine equality.
    </dl>

    The sets $all$, $e1$, etc can have any head, which must all match.
    The returned expression has the same head as the input
    expressions.

    >> Complement[{a, b, c}, {a, c}]
     = {b}
    >> Complement[{a, b, c}, {a, c}, {b}]
     = {}
    >> Complement[f[z, y, x, w], f[x], f[x, z]]
     = f[w, y]

    #> Complement[a, b]
     : Non-atomic expression expected at position 1 in Complement[a, b].
     = Complement[a, b]
    #> Complement[f[a], g[b]]
     : Heads f and g at positions 1 and 2 are expected to be the same.
     = Complement[f[a], g[b]]
    #> Complement[{a, b, c}, {a, c}, SameTest->(True&)]
     = {}
    #> Complement[{a, b, c}, {a, c}, SameTest->(False&)]
     = {a, b, c}
    """

    messages = {
        'normal': "Non-atomic expression expected at position `1` in `2`.",
        'heads': ("Heads `1` and `2` at positions `3` and `4` are expected "
                  "to be the same."),
        'smtst': ("Application of the SameTest yielded `1`, which evaluates "
                  "to `2`. The SameTest must evaluate to True or False at "
                  "every pair of elements."),
    }

    options = {
        'SameTest': 'SameQ',
    }

    def complement(self, all, others, evaluation, test):
        def test_pair(e1, e2):
            test_expr = Expression(test, e1, e2)
            result = test_expr.evaluate(evaluation)
            if not(result.is_symbol() and (result.has_symbol('True') or
                                           result.has_symbol('False'))):
                evaluation.message('Complement', 'smtst', test_expr, result)
            return result.is_true()
        all = set(all)
        for leaves in others:
            for e2 in leaves:
                for e1 in all.copy():
                    if test_pair(e1, e2):
                        all.discard(e1)
        return all

    def apply(self, all, others, evaluation, options={}):
        'Complement[all_, others__, OptionsPattern[Complement]]'

        # FIXME: is there a better way to get hold of the original
        # expression?
        def get_call():
            return Expression('Complement', all, *others.get_sequence())

        for pos, e in enumerate([all, others]):
            if e.is_atom():
                return evaluation.message(
                    'Complement', 'normal', pos + 1, get_call())

        for pos, e in enumerate([all] + others.get_sequence()):
            if e.is_atom():
                return evaluation.message(
                    'Complement', 'normal', pos + 1, get_call())
            if e.head != all.head:
                return evaluation.message(
                    'Complement', 'heads', all.head, e.head,
                    1, pos + 1)

        result_head = all.head
        same_test = self.get_option(options, 'SameTest', evaluation)
        others_leaves = [e.leaves for e in others.get_sequence()]

        result = Expression(result_head,
                            *self.complement(all.leaves, others_leaves,
                                             evaluation, same_test))
        result.sort()
        return result
