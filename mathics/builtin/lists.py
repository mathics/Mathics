# -*- coding: utf-8 -*-

"""
List Functions
"""


from itertools import chain, permutations

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import (
    Builtin,
    Test,
    InvalidLevelspecError,
    BinaryOperator,
    PartError,
    PartDepthError,
    PartRangeError,
    Predefined,
    SympyFunction,
)
from mathics.builtin.scoping import dynamic_scoping
from mathics.builtin.base import (
    MessageException,
    NegativeIntegerException,
    CountableInteger,
)
from mathics.core.expression import (
    Expression,
    String,
    ByteArrayAtom,
    Symbol,
    SymbolFailed,
    SymbolNull,
    SymbolN,
    SymbolRule,
    SymbolMakeBoxes,
    SymbolAssociation,
    SymbolSequence,
    Integer,
    Number,
    Real,
    strip_context,
    from_python,
    SymbolList,
    SymbolByteArray,
)
from mathics.core.expression import min_prec, machine_precision
from mathics.core.expression import structure
from mathics.core.evaluation import BreakInterrupt, ContinueInterrupt, ReturnInterrupt
from mathics.core.rules import Pattern
from mathics.core.convert import from_sympy
from mathics.builtin.algebra import cancel
from mathics.algorithm.introselect import introselect
from mathics.algorithm.clusters import (
    optimize,
    agglomerate,
    kmeans,
    PrecomputedDistances,
    LazyDistances,
)
from mathics.algorithm.clusters import AutomaticSplitCriterion, AutomaticMergeCriterion
from mathics.builtin.options import options_to_rules

import sympy
import heapq

from collections import defaultdict
import functools


def deletecases_with_levelspec(expr, pattern, evaluation, levelspec=1, n=-1):
    """
    This function walks the expression `expr` and deleting occurrencies of `pattern`

    If levelspec specifies a number, only those positions with  `levelspec` "coordinates" are return. By default, it just return occurences in the first level.

    If a tuple (nmin, nmax) is provided, it just return those occurences with a number of "coordinates" between nmin and nmax.
    n indicates the number of occurrences to return. By default, it returns all the occurences.
    """
    nothing = Symbol("System`Nothing")
    from mathics.builtin.patterns import Matcher

    match = Matcher(pattern)
    match = match.match
    if type(levelspec) is int:
        lsmin = 1
        lsmax = levelspec + 1
    else:
        lsmin = levelspec[0]
        if levelspec[1]:
            lsmax = levelspec[1] + 1
        else:
            lsmax = -1
    tree = [[expr]]
    changed_marks = [
        [False],
    ]
    curr_index = [0]

    while curr_index[0] != 1:
        # If the end of the branch is reached, or no more elements to delete out
        if curr_index[-1] == len(tree[-1]) or n == 0:
            leaves = tree[-1]
            tree.pop()
            # check if some of the leaves was changed
            changed = any(changed_marks[-1])
            changed_marks.pop()
            if changed:
                leaves = [leaf for leaf in leaves if leaf is not nothing]
            curr_index.pop()
            if len(curr_index) == 0:
                break
            idx = curr_index[-1]
            changed = changed or changed_marks[-1][idx]
            changed_marks[-1][idx] = changed
            if changed:
                head = tree[-1][curr_index[-1]].get_head()
                tree[-1][idx] = Expression(head, *leaves)
            if len(curr_index) == 0:
                break
            curr_index[-1] = curr_index[-1] + 1
            continue
        curr_leave = tree[-1][curr_index[-1]]
        if match(curr_leave, evaluation) and (len(curr_index) > lsmin):
            tree[-1][curr_index[-1]] = nothing
            changed_marks[-1][curr_index[-1]] = True
            curr_index[-1] = curr_index[-1] + 1
            n = n - 1
            continue
        if curr_leave.is_atom() or lsmax == len(curr_index):
            curr_index[-1] = curr_index[-1] + 1
            continue
        else:
            tree.append(list(curr_leave.get_leaves()))
            changed_marks.append([False for l in tree[-1]])
            curr_index.append(0)
    return tree[0][0]


def find_matching_indices_with_levelspec(expr, pattern, evaluation, levelspec=1, n=-1):
    """
    This function walks the expression `expr` looking for a pattern `pattern`
    and returns the positions of each occurence.

    If levelspec specifies a number, only those positions with  `levelspec` "coordinates" are return. By default, it just return occurences in the first level.

    If a tuple (nmin, nmax) is provided, it just return those occurences with a number of "coordinates" between nmin and nmax.
    n indicates the number of occurrences to return. By default, it returns all the occurences.
    """
    from mathics.builtin.patterns import Matcher

    match = Matcher(pattern)
    match = match.match
    if type(levelspec) is int:
        lsmin = 0
        lsmax = levelspec
    else:
        lsmin = levelspec[0]
        lsmax = levelspec[1]
    tree = [expr.get_leaves()]
    curr_index = [0]
    found = []
    while len(tree) > 0:
        if n == 0:
            break
        if curr_index[-1] == len(tree[-1]):
            curr_index.pop()
            tree.pop()
            if len(curr_index) != 0:
                curr_index[-1] = curr_index[-1] + 1
            continue
        curr_leave = tree[-1][curr_index[-1]]
        if match(curr_leave, evaluation) and (len(curr_index) >= lsmin):
            found.append([from_python(i) for i in curr_index])
            curr_index[-1] = curr_index[-1] + 1
            n = n - 1
            continue
        if curr_leave.is_atom() or lsmax == len(curr_index):
            curr_index[-1] = curr_index[-1] + 1
            continue
        else:
            tree.append(curr_leave.get_leaves())
            curr_index.append(0)
    return found


class Normal(Builtin):
    """
        <dl>
    <dt>'Normal[expr_]'
       <dd> Brings especial expressions to a normal expression from
       different especial forms.
    </dl>
    """


class ByteArray(Builtin):
    r"""
    <dl>
    <dt>'ByteArray[{$b_1$, $b_2$, ...}]'
       <dd> Represents a sequence of Bytes $b_1$, $b_2$, ...
    <dt>'ByteArray["string"]'
       <dd> Constructs a byte array where bytes comes from decode a b64 encoded String
    </dl>

    >> A=ByteArray[{1, 25, 3}]
     = ByteArray["ARkD"]
    >> A[[2]]
     = 25
    >> Normal[A]
     = {1, 25, 3}
    >> ToString[A]
     = ByteArray["ARkD"]
    >> ByteArray["ARkD"]
     = ByteArray["ARkD"]
    >> B=ByteArray["asy"]
     : The first argument in Bytearray[asy] should be a B64 enconded string or a vector of integers.
     = $Failed
    """

    messages = {
        "aotd": "Elements in `1` are inconsistent with type Byte",
        "lend": "The first argument in Bytearray[`1`] should "
        + "be a B64 enconded string or a vector of integers.",
    }

    def apply_str(self, string, evaluation):
        "ByteArray[string_String]"
        try:
            atom = ByteArrayAtom(string.value)
        except Exception:
            evaluation.message("ByteArray", "lend", string)
            return SymbolFailed
        return Expression("ByteArray", atom)

    def apply_to_str(self, baa, evaluation):
        "ToString[ByteArray[baa_ByteArrayAtom]]"
        return String('ByteArray["' + baa.__str__() + '"]')

    def apply_normal(self, baa, evaluation):
        "System`Normal[ByteArray[baa_ByteArrayAtom]]"
        return Expression(SymbolList, *[Integer(x) for x in baa.value])

    def apply_list(self, values, evaluation):
        "ByteArray[values_List]"
        if not values.has_form("List", None):
            return
        try:
            ba = bytearray([b.get_int_value() for b in values._leaves])
        except:
            evaluation.message("ByteArray", "aotd", values)
            return
        return Expression(SymbolByteArray, ByteArrayAtom(ba))


class List(Builtin):
    """
    <dl>
    <dt>'List[$e1$, $e2$, ..., $ei$]'
    <dt>'{$e1$, $e2$, ..., $ei$}'
        <dd>represents a list containing the elements $e1$...$ei$.
    </dl>

    'List' is the head of lists:
    >> Head[{1, 2, 3}]
     = List

    Lists can be nested:
    >> {{a, b, {c, d}}}
     = {{a, b, {c, d}}}
    """

    attributes = ("Locked",)

    def apply_makeboxes(self, items, f, evaluation):
        """MakeBoxes[{items___},
        f:StandardForm|TraditionalForm|OutputForm|InputForm]"""

        items = items.get_sequence()
        return Expression(
            "RowBox", Expression(SymbolList, *list_boxes(items, f, "{", "}"))
        )


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
        return expr.get_head_name() == "System`List"


class NotListQ(Test):
    """
    <dl>
    <dt>'NotListQ[$expr$]'
        <dd>returns true if $expr$ is not a list.
    </dl>
    """

    def test(self, expr):
        return expr.get_head_name() != "System`List"


def list_boxes(items, f, open=None, close=None):
    result = [Expression(SymbolMakeBoxes, item, f) for item in items]
    if f.get_name() in ("System`OutputForm", "System`InputForm"):
        sep = ", "
    else:
        sep = ","
    result = riffle(result, String(sep))
    if len(items) > 1:
        result = Expression("RowBox", Expression(SymbolList, *result))
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
    <dl>
    <dt>'Length[$expr$]'
        <dd>returns the number of leaves in $expr$.
    </dl>

    Length of a list:
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

    Note that rational and complex numbers are atoms, although their
    'FullForm' might suggest the opposite:
    >> Length[1/3]
     = 0
    >> FullForm[1/3]
     = Rational[1, 3]
    """

    def apply(self, expr, evaluation):
        "Length[expr_]"

        if expr.is_atom():
            return Integer(0)
        else:
            return Integer(len(expr.leaves))


class All(Predefined):
    """
    <dl>
    <dt>'All'
        <dd>is a possible value for 'Span' and 'Quiet'.
    </dl>
    """

    pass


class None_(Predefined):
    """
    <dl>
    <dt>'None'
        <dd>is a possible value for 'Span' and 'Quiet'.
    </dl>
    """

    name = "None"


class Span(BinaryOperator):
    """
    <dl>
    <dt>'Span'
        <dd>is the head of span ranges like '1;;3'.
    </dl>

    >> ;; // FullForm
     = Span[1, All]
    >> 1;;4;;2 // FullForm
     = Span[1, 4, 2]
    >> 2;;-2 // FullForm
     = Span[2, -2]
    >> ;;3 // FullForm
     = Span[1, 3]

    ## Parsing: 8 cases to consider
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

    ## Formatting
    #> a ;; b ;; c
     = a ;; b ;; c
    #> a ;; b
     = a ;; b
    #> a ;; b ;; c ;; d
     = (1 ;; d) (a ;; b ;; c)
    """

    operator = ";;"
    precedence = 305


def join_lists(lists):
    new_list = []
    for list in lists:
        new_list.extend(list)
    return new_list


def get_part(varlist, indices):
    " Simple part extraction. indices must be a list of python integers. "

    def rec(cur, rest):
        if rest:
            if cur.is_atom():
                raise PartDepthError(rest[0])
            pos = rest[0]
            leaves = cur.get_leaves()
            try:
                if pos > 0:
                    part = leaves[pos - 1]
                elif pos == 0:
                    part = cur.get_head()
                else:
                    part = leaves[pos]
            except IndexError:
                raise PartRangeError
            return rec(part, rest[1:])
        else:
            return cur

    return rec(varlist, indices).copy()


def set_part(varlist, indices, newval):
    " Simple part replacement. indices must be a list of python integers. "

    def rec(cur, rest):
        if len(rest) > 1:
            pos = rest[0]
            if cur.is_atom():
                raise PartDepthError
            try:
                if pos > 0:
                    part = cur._leaves[pos - 1]
                elif pos == 0:
                    part = cur.get_head()
                else:
                    part = cur._leaves[pos]
            except IndexError:
                raise PartRangeError
            return rec(part, rest[1:])
        elif len(rest) == 1:
            pos = rest[0]
            if cur.is_atom():
                raise PartDepthError
            try:
                if pos > 0:
                    cur.set_leaf(pos - 1, newval)
                elif pos == 0:
                    cur.set_head(newval)
                else:
                    cur.set_leaf(pos, newval)
            except IndexError:
                raise PartRangeError

    rec(varlist, indices)


def _parts_all_selector():
    start = 1
    stop = None
    step = 1

    def select(inner):
        if inner.is_atom():
            raise MessageException("Part", "partd")
        py_slice = python_seq(start, stop, step, len(inner.leaves))
        if py_slice is None:
            raise MessageException("Part", "take", start, stop, inner)
        return inner.leaves[py_slice]

    return select


def _parts_span_selector(pspec):
    if len(pspec.leaves) > 3:
        raise MessageException("Part", "span", pspec)
    start = 1
    stop = None
    step = 1
    if len(pspec.leaves) > 0:
        start = pspec.leaves[0].get_int_value()
    if len(pspec.leaves) > 1:
        stop = pspec.leaves[1].get_int_value()
        if stop is None:
            if pspec.leaves[1].get_name() == "System`All":
                stop = None
            else:
                raise MessageException("Part", "span", pspec)
    if len(pspec.leaves) > 2:
        step = pspec.leaves[2].get_int_value()

    if start == 0 or stop == 0:
        # index 0 is undefined
        raise MessageException("Part", "span", 0)

    if start is None or step is None:
        raise MessageException("Part", "span", pspec)

    def select(inner):
        if inner.is_atom():
            raise MessageException("Part", "partd")
        py_slice = python_seq(start, stop, step, len(inner.leaves))
        if py_slice is None:
            raise MessageException("Part", "take", start, stop, inner)
        return inner.leaves[py_slice]

    return select


def _parts_sequence_selector(pspec):
    if not isinstance(pspec, (tuple, list)):
        indices = [pspec]
    else:
        indices = pspec

    for index in indices:
        if not isinstance(index, Integer):
            raise MessageException("Part", "pspec", pspec)

    def select(inner):
        if inner.is_atom():
            raise MessageException("Part", "partd")

        leaves = inner.leaves
        n = len(leaves)

        for index in indices:
            int_index = index.value

            if int_index == 0:
                yield inner.head
            elif 1 <= int_index <= n:
                yield leaves[int_index - 1]
            elif -n <= int_index <= -1:
                yield leaves[int_index]
            else:
                raise MessageException("Part", "partw", index, inner)

    return select


def _part_selectors(indices):
    for index in indices:
        if index.has_form("Span", None):
            yield _parts_span_selector(index)
        elif index.get_name() == "System`All":
            yield _parts_all_selector()
        elif index.has_form("List", None):
            yield _parts_sequence_selector(index.leaves)
        elif isinstance(index, Integer):
            yield _parts_sequence_selector(index), lambda x: x[0]
        else:
            raise MessageException("Part", "pspec", index)


def _list_parts(items, selectors, heads, evaluation, assignment):
    if not selectors:
        for item in items:
            yield item
    else:
        selector = selectors[0]
        if isinstance(selector, tuple):
            select, unwrap = selector
        else:
            select = selector
            unwrap = None

        for item in items:
            selected = list(select(item))

            picked = list(
                _list_parts(selected, selectors[1:], heads, evaluation, assignment)
            )

            if unwrap is None:
                if assignment:
                    expr = Expression(item.head, *picked)
                    expr.original = None
                    expr.set_positions()
                else:
                    expr = item.restructure(item.head, picked, evaluation)

                yield expr
            else:
                yield unwrap(picked)


def _parts(items, selectors, evaluation, assignment=False):
    heads = {}
    return list(_list_parts([items], list(selectors), heads, evaluation, assignment))[0]


def walk_parts(list_of_list, indices, evaluation, assign_list=None):
    walk_list = list_of_list[0]

    if assign_list is not None:
        # this double copying is needed to make the current logic in
        # the assign_list and its access to original work.

        walk_list = walk_list.copy()
        walk_list.set_positions()
        list_of_list = [walk_list]

        walk_list = walk_list.copy()
        walk_list.set_positions()

    indices = [index.evaluate(evaluation) for index in indices]

    try:
        result = _parts(
            walk_list, _part_selectors(indices), evaluation, assign_list is not None
        )
    except MessageException as e:
        e.message(evaluation)
        return False

    if assign_list is not None:

        def replace_item(all, item, new):
            if item.position is None:
                all[0] = new
            else:
                item.position.replace(new)

        def process_level(item, assignment):
            if item.is_atom():
                replace_item(list_of_list, item.original, assignment)
            elif assignment.get_head_name() != "System`List" or len(item.leaves) != len(
                assignment.leaves
            ):
                if item.original:
                    replace_item(list_of_list, item.original, assignment)
                else:
                    for leaf in item.leaves:
                        process_level(leaf, assignment)
            else:
                for sub_item, sub_assignment in zip(item.leaves, assignment.leaves):
                    process_level(sub_item, sub_assignment)

        process_level(result, assign_list)

        result = list_of_list[0]
        result.clear_cache()

    return result


def is_in_level(current, depth, start=1, stop=None):
    if stop is None:
        stop = current
    if start < 0:
        start += current + depth + 1
    if stop < 0:
        stop += current + depth + 1
    return start <= current <= stop


def walk_levels(
    expr,
    start=1,
    stop=None,
    current=0,
    heads=False,
    callback=lambda l: l,
    include_pos=False,
    cur_pos=[],
):
    if expr.is_atom():
        depth = 0
        new_expr = expr
    else:
        depth = 0
        if heads:
            head, head_depth = walk_levels(
                expr.head,
                start,
                stop,
                current + 1,
                heads,
                callback,
                include_pos,
                cur_pos + [0],
            )
        else:
            head = expr.head
        leaves = []
        for index, leaf in enumerate(expr.leaves):
            leaf, leaf_depth = walk_levels(
                leaf,
                start,
                stop,
                current + 1,
                heads,
                callback,
                include_pos,
                cur_pos + [index + 1],
            )
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
            if expr == Expression("DirectedInfinity", 1):
                return None
            else:
                raise InvalidLevelspecError
        else:
            return value

    if levelspec.has_form("List", None):
        values = [value_to_level(leaf) for leaf in levelspec.leaves]
        if len(values) == 1:
            return values[0], values[0]
        elif len(values) == 2:
            return values[0], values[1]
        else:
            raise InvalidLevelspecError
    elif isinstance(levelspec, Symbol) and levelspec.get_name() == "System`All":
        return 0, None
    else:
        return 1, value_to_level(levelspec)


class Level(Builtin):
    """
    <dl>
    <dt>'Level[$expr$, $levelspec$]'
        <dd>gives a list of all subexpressions of $expr$ at the
        level(s) specified by $levelspec$.
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
        "Heads": "False",
    }

    def apply(self, expr, ls, evaluation, options={}):
        "Level[expr_, ls_, OptionsPattern[Level]]"

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            evaluation.message("Level", "level", ls)
            return
        result = []

        def callback(level):
            result.append(level)
            return level

        heads = self.get_option(options, "Heads", evaluation).is_true()
        walk_levels(expr, start, stop, heads=heads, callback=callback)
        return Expression(SymbolList, *result)


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


def python_seq(start, stop, step, length):
    """
    Converts mathematica sequence tuple to python slice object.

    Based on David Mashburn's generic slice:
    https://gist.github.com/davidmashburn/9764309
    """
    if step == 0:
        return None

    # special empty case
    if stop is None and length is not None:
        empty_stop = length
    else:
        empty_stop = stop
    if start is not None and empty_stop + 1 == start and step > 0:
        return slice(0, 0, 1)

    if start == 0 or stop == 0:
        return None

    # wrap negative values to postive and convert from 1-based to 0-based
    if start < 0:
        start += length
    else:
        start -= 1

    if stop is None:
        if step < 0:
            stop = 0
        else:
            stop = length - 1
    elif stop < 0:
        stop += length
    else:
        assert stop > 0
        stop -= 1

    # check bounds
    if (
        not 0 <= start < length
        or not 0 <= stop < length
        or step > 0
        and start - stop > 1
        or step < 0
        and stop - start > 1
    ):  # nopep8
        return None

    # include the stop value
    if step > 0:
        stop += 1
    else:
        stop -= 1
        if stop == -1:
            stop = None
        if start == 0:
            start = None

    return slice(start, stop, step)


def convert_seq(seq):
    """
    converts a sequence specification into a (start, stop, step) tuple.
    returns None on failure
    """
    start, stop, step = 1, None, 1
    name = seq.get_name()
    value = seq.get_int_value()
    if name == "System`All":
        pass
    elif name == "System`None":
        stop = 0
    elif value is not None:
        if value > 0:
            stop = value
        else:
            start = value
    elif seq.has_form("List", 1, 2, 3):
        if len(seq.leaves) == 1:
            start = stop = seq.leaves[0].get_int_value()
            if stop is None:
                return None
        else:
            start = seq.leaves[0].get_int_value()
            stop = seq.leaves[1].get_int_value()
            if start is None or stop is None:
                return None
        if len(seq.leaves) == 3:
            step = seq.leaves[2].get_int_value()
            if step is None:
                return None
    else:
        return None
    return (start, stop, step)


class Part(Builtin):
    """
    <dl>
    <dt>'Part[$expr$, $i$]'
        <dd>returns part $i$ of $expr$.
    </dl>

    Extract an element from a list:
    >> A = {a, b, c, d};
    >> A[[3]]
     = c

    Negative indices count from the end:
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

    ## Negative step
    #> {1,2,3,4,5}[[3;;1;;-1]]
     = {3, 2, 1}

    #> {1, 2, 3, 4, 5}[[;; ;; -1]]      (* MMA bug *)
     = {5, 4, 3, 2, 1}

    #> Range[11][[-3 ;; 2 ;; -2]]
     = {9, 7, 5, 3}
    #> Range[11][[-3 ;; -7 ;; -3]]
     = {9, 6}
    #> Range[11][[7 ;; -7;; -2]]
     = {7, 5}

    #> {1, 2, 3, 4}[[1;;3;;-1]]
     : Cannot take positions 1 through 3 in {1, 2, 3, 4}.
     = {1, 2, 3, 4}[[1 ;; 3 ;; -1]]
    #> {1, 2, 3, 4}[[3;;1]]
     : Cannot take positions 3 through 1 in {1, 2, 3, 4}.
     = {1, 2, 3, 4}[[3 ;; 1]]
    """

    attributes = ("NHoldRest", "ReadProtected")

    def apply_makeboxes(self, list, i, f, evaluation):
        """MakeBoxes[Part[list_, i___],
        f:StandardForm|TraditionalForm|OutputForm|InputForm]"""

        i = i.get_sequence()
        list = Expression(SymbolMakeBoxes, list, f)
        if f.get_name() in ("System`OutputForm", "System`InputForm"):
            open, close = "[[", "]]"
        else:
            open, close = "\u301a", "\u301b"
        indices = list_boxes(i, f, open, close)
        result = Expression("RowBox", Expression(SymbolList, list, *indices))
        return result

    def apply(self, list, i, evaluation):
        "Part[list_, i___]"

        indices = i.get_sequence()
        # How to deal with ByteArrays
        if list.get_head_name() == "System`ByteArray":
            list = list.evaluate(evaluation)
            if len(indices) > 1:
                print(
                    "Part::partd1: Depth of object ByteArray[<3>] "
                    + "is not sufficient for the given part specification."
                )
                return
            idx = indices[0]
            if idx.get_head_name() == "System`Integer":
                idx = idx.get_int_value()
                if idx == 0:
                    return Symbol("System`ByteArray")
                data = list._leaves[0].value
                lendata = len(data)
                if idx < 0:
                    idx = data - idx
                    if idx < 0:
                        evaluation.message("Part", "partw", i, list)
                        return
                else:
                    idx = idx - 1
                    if idx > lendata:
                        evaluation.message("Part", "partw", i, list)
                        return
                return Integer(data[idx])
            if idx == Symbol("System`All"):
                return list
            # TODO: handling ranges and lists...
            evaluation.message("Part", "notimplemented")
            return

        # Otherwise...
        result = walk_parts([list], indices, evaluation)
        if result:
            return result


class Partition(Builtin):
    """
    <dl>
    <dt>'Partition[$list$, $n$]'
        <dd>partitions $list$ into sublists of length $n$.
    <dt>'Parition[$list$, $n$, $d$]'
        <dd>partitions $list$ into sublists of length $n$ which
        overlap $d$ indicies.
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
        "Parition[list_, n_, d_, k]": "Partition[list, n, d, {k, k}]",
    }

    def _partition(self, expr, n, d, evaluation):
        assert n > 0 and d > 0

        inner = structure("List", expr, evaluation)
        outer = structure("List", inner, evaluation)

        make_slice = inner.slice

        def slices():
            leaves = expr.leaves
            for lower in range(0, len(leaves), d):
                upper = lower + n

                chunk = leaves[lower:upper]
                if len(chunk) != n:
                    continue

                yield make_slice(expr, slice(lower, upper))

        return outer(slices())

    def apply_no_overlap(self, l, n, evaluation):
        "Partition[l_List, n_Integer]"
        # TODO: Error checking
        return self._partition(l, n.get_int_value(), n.get_int_value(), evaluation)

    def apply(self, l, n, d, evaluation):
        "Partition[l_List, n_Integer, d_Integer]"
        # TODO: Error checking
        return self._partition(l, n.get_int_value(), d.get_int_value(), evaluation)


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

    attributes = ("NHoldRest",)

    rules = {
        "Extract[expr_, list_List]": "Part[expr, Sequence @@ list]",
        "Extract[expr_, {lists___List}]": "Extract[expr, #]& /@ {lists}",
    }


class First(Builtin):
    """
    <dl>
    <dt>'First[$expr$]'
        <dd>returns the first element in $expr$.
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
        "First[expr_]"

        if expr.is_atom():
            evaluation.message("First", "normal")
            return
        return expr.leaves[0]


class Last(Builtin):
    """
    <dl>
    <dt>'Last[$expr$]'
        <dd>returns the last element in $expr$.
    </dl>

    'Last[$expr$]' is equivalent to '$expr$[[-1]]'.

    >> Last[{a, b, c}]
     = c
    >> Last[x]
     : Nonatomic expression expected.
     = Last[x]
    """

    def apply(self, expr, evaluation):
        "Last[expr_]"

        if expr.is_atom():
            evaluation.message("Last", "normal")
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

    #> A[x__] := 7 /; Length[{x}] == 3;
    #> Most[A[1, 2, 3, 4]]
     = 7
    #> ClearAll[A];
    """

    def apply(self, expr, evaluation):
        "Most[expr_]"

        if expr.is_atom():
            evaluation.message("Most", "normal")
            return
        return expr.slice(expr.head, slice(0, -1), evaluation)


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
        "Rest[expr_]"

        if expr.is_atom():
            evaluation.message("Rest", "normal")
            return
        return expr.slice(expr.head, slice(1, len(expr.leaves)), evaluation)


class ReplacePart(Builtin):
    """
    <dl>
    <dt>'ReplacePart[$expr$, $i$ -> $new$]'
        <dd>replaces part $i$ in $expr$ with $new$.
    <dt>'ReplacePart[$expr$, {{$i$, $j$} -> $e1$, {$k$, $l$} -> $e2$}]'
        <dd>replaces parts $i$ and $j$ with $e1$, and parts $k$ and
        $l$ with $e2$.
    </dl>

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
        "reps": "`1` is not a list of replacement rules.",
    }

    rules = {
        "ReplacePart[expr_, (Rule|RuleDelayed)[i_, new_]]": (
            "ReplacePart[expr, {i -> new}]"
        ),
        "ReplacePart[expr_, Pattern[rule, "
        "Rule|RuleDelayed][{indices___?(Head[#]===List&)}, new_]]": (
            "ReplacePart[expr, rule[#, new]& /@ {indices}]"
        ),
    }

    def apply(self, expr, replacements, evaluation):
        "ReplacePart[expr_, {replacements___}]"

        new_expr = expr.copy()
        replacements = replacements.get_sequence()
        for replacement in replacements:
            if not replacement.has_form("Rule", 2) and not replacement.has_form(  # noqa
                "RuleDelayed", 2
            ):
                evaluation.message(
                    "ReplacePart", "reps", Expression(SymbolList, *replacements)
                )
                return
            position = replacement.leaves[0]
            replace = replacement.leaves[1]
            if position.has_form("List", None):
                position = position.get_mutable_leaves()
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
                if replacement.get_head_name() == "System`RuleDelayed":
                    replace_value = replace.evaluate(evaluation)
                else:
                    replace_value = replace
                set_part(new_expr, position, replace_value)
            except PartError:
                pass

        return new_expr


class FirstPosition(Builtin):
    """
    <dl>
    <dt>'FirstPosition[$expr$, $pattern$]'
        <dd>gives the position of the first element in $expr$ that matches $pattern$, or Missing["NotFound"] if no such element is found.
    <dt>'FirstPosition[$expr$, $pattern$, $default$]'
        <dd>gives default if no element matching $pattern$ is found.
    <dt>'FirstPosition[$expr$, $pattern$, $default$, $levelspec$]'
        <dd>finds only objects that appear on levels specified by $levelspec$.
    </dl>

    >> FirstPosition[{a, b, a, a, b, c, b}, b]
     = {2}

    >> FirstPosition[{{a, a, b}, {b, a, a}, {a, b, a}}, b]
     = {1, 3}

    >> FirstPosition[{x, y, z}, b]
     = Missing[NotFound]

    Find the first position at which x^2 to appears:
    >> FirstPosition[{1 + x^2, 5, x^4, a + (1 + x^2)^2}, x^2]
     = {1, 2}

    #> FirstPosition[{1, 2, 3}, _?StringQ, "NoStrings"]
     = NoStrings

    #> FirstPosition[a, a]
     = {}

    #> FirstPosition[{{{1, 2}, {2, 3}, {3, 1}}, {{1, 2}, {2, 3}, {3, 1}}},3]
     = {1, 2, 2}

    #> FirstPosition[{{1, {2, 1}}, {2, 3}, {3, 1}}, 2, Missing["NotFound"],2]
     = {2, 1}

    #> FirstPosition[{{1, {2, 1}}, {2, 3}, {3, 1}}, 2, Missing["NotFound"],4]
     = {1, 2, 1}

    #> FirstPosition[{{1, 2}, {2, 3}, {3, 1}}, 3, Missing["NotFound"], {1}]
     = Missing[NotFound]

    #> FirstPosition[{{1, 2}, {2, 3}, {3, 1}}, 3, Missing["NotFound"], 0]
     = Missing[NotFound]

    #> FirstPosition[{{1, 2}, {1, {2, 1}}, {2, 3}}, 2, Missing["NotFound"], {3}]
     = {2, 2, 1}

    #> FirstPosition[{{1, 2}, {1, {2, 1}}, {2, 3}}, 2, Missing["NotFound"], 3]
     = {1, 2}

    #> FirstPosition[{{1, 2}, {1, {2, 1}}, {2, 3}}, 2,  Missing["NotFound"], {}]
     = {1, 2}

    #> FirstPosition[{{1, 2}, {2, 3}, {3, 1}}, 3, Missing["NotFound"], {1, 2, 3}]
     : Level specification {1, 2, 3} is not of the form n, {n}, or {m, n}.
     = FirstPosition[{{1, 2}, {2, 3}, {3, 1}}, 3, Missing[NotFound], {1, 2, 3}]

    #> FirstPosition[{{1, 2}, {2, 3}, {3, 1}}, 3, Missing["NotFound"], a]
     : Level specification a is not of the form n, {n}, or {m, n}.
     = FirstPosition[{{1, 2}, {2, 3}, {3, 1}}, 3, Missing[NotFound], a]

    #> FirstPosition[{{1, 2}, {2, 3}, {3, 1}}, 3, Missing["NotFound"], {1, a}]
     : Level specification {1, a} is not of the form n, {n}, or {m, n}.
     = FirstPosition[{{1, 2}, {2, 3}, {3, 1}}, 3, Missing[NotFound], {1, a}]

    """

    messages = {
        "level": "Level specification `1` is not of the form n, {n}, or {m, n}.",
    }

    def apply(
        self, expr, pattern, evaluation, default=None, minLevel=None, maxLevel=None
    ):
        "FirstPosition[expr_, pattern_]"

        if expr == pattern:
            return Expression(SymbolList)

        result = []

        def check_pattern(input_list, pat, result, beginLevel):
            for i in range(0, len(input_list.leaves)):
                nested_level = beginLevel
                result.append(i + 1)
                if input_list.leaves[i] == pat:
                    # found the pattern
                    if minLevel is None or nested_level >= minLevel:
                        return True

                else:
                    if isinstance(input_list.leaves[i], Expression) and (
                        maxLevel is None or maxLevel > nested_level
                    ):
                        nested_level = nested_level + 1
                        if check_pattern(
                            input_list.leaves[i], pat, result, nested_level
                        ):
                            return True

                result.pop()
            return False

        is_found = False
        if isinstance(expr, Expression) and (maxLevel is None or maxLevel > 0):
            is_found = check_pattern(expr, pattern, result, 1)
        if is_found:
            return Expression(SymbolList, *result)
        else:
            return Expression("Missing", "NotFound") if default is None else default

    def apply_default(self, expr, pattern, default, evaluation):
        "FirstPosition[expr_, pattern_, default_]"
        return self.apply(expr, pattern, evaluation, default=default)

    def apply_level(self, expr, pattern, default, level, evaluation):
        "FirstPosition[expr_, pattern_, default_, level_]"

        def is_interger_list(expr_list):
            return all(
                isinstance(expr_list.leaves[i], Integer)
                for i in range(len(expr_list.leaves))
            )

        if level.has_form("List", None):
            len_list = len(level.leaves)
            if len_list > 2 or not is_interger_list(level):
                return evaluation.message("FirstPosition", "level", level)
            elif len_list == 0:
                min_Level = max_Level = None
            elif len_list == 1:
                min_Level = max_Level = level.leaves[0].get_int_value()
            elif len_list == 2:
                min_Level = level.leaves[0].get_int_value()
                max_Level = level.leaves[1].get_int_value()
        elif isinstance(level, Integer):
            min_Level = 0
            max_Level = level.get_int_value()
        else:
            return evaluation.message("FirstPosition", "level", level)

        return self.apply(
            expr,
            pattern,
            evaluation,
            default=default,
            minLevel=min_Level,
            maxLevel=max_Level,
        )


def _drop_take_selector(name, seq, sliced):
    seq_tuple = convert_seq(seq)
    if seq_tuple is None:
        raise MessageException(name, "seqs", seq)

    def select(inner):
        start, stop, step = seq_tuple
        if inner.is_atom():
            py_slice = None
        else:
            py_slice = python_seq(start, stop, step, len(inner.leaves))
        if py_slice is None:
            if stop is None:
                stop = Symbol("Infinity")
            raise MessageException(name, name.lower(), start, stop, inner)
        return sliced(inner.leaves, py_slice)

    return select


def _take_span_selector(seq):
    return _drop_take_selector("Take", seq, lambda x, s: x[s])


def _drop_span_selector(seq):
    def sliced(x, s):
        y = list(x[:])
        del y[s]
        return y

    return _drop_take_selector("Drop", seq, sliced)


class Take(Builtin):
    """
    <dl>
    <dt>'Take[$expr$, $n$]'
        <dd>returns $expr$ with all but the first $n$ leaves removed.
    </dl>

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

    #> Take[Range[10], {8, 2, -1}]
     = {8, 7, 6, 5, 4, 3, 2}
    #> Take[Range[10], {-3, -7, -2}]
     = {8, 6, 4}

    #> Take[Range[6], {-5, -2, -2}]
     : Cannot take positions -5 through -2 in {1, 2, 3, 4, 5, 6}.
     = Take[{1, 2, 3, 4, 5, 6}, {-5, -2, -2}]

    #> Take[l, {-1}]
     : Nonatomic expression expected at position 1 in Take[l, {-1}].
     = Take[l, {-1}]

    ## Empty case
    #> Take[{1, 2, 3, 4, 5}, {-1, -2}]
     = {}
    #> Take[{1, 2, 3, 4, 5}, {0, -1}]
     = {}
    #> Take[{1, 2, 3, 4, 5}, {1, 0}]
     = {}
    #> Take[{1, 2, 3, 4, 5}, {2, 1}]
     = {}
    #> Take[{1, 2, 3, 4, 5}, {1, 0, 2}]
     = {}
    #> Take[{1, 2, 3, 4, 5}, {1, 0, -1}]
     : Cannot take positions 1 through 0 in {1, 2, 3, 4, 5}.
     = Take[{1, 2, 3, 4, 5}, {1, 0, -1}]
    """

    messages = {
        "normal": "Nonatomic expression expected at position `1` in `2`.",
    }

    def apply(self, items, seqs, evaluation):
        "Take[items_, seqs___]"

        seqs = seqs.get_sequence()

        if items.is_atom():
            return evaluation.message(
                "Take", "normal", 1, Expression("Take", items, *seqs)
            )

        try:
            return _parts(items, [_take_span_selector(seq) for seq in seqs], evaluation)
        except MessageException as e:
            e.message(evaluation)


class Drop(Builtin):
    """
    <dl>
    <dt>'Drop[$expr$, $n$]'
        <dd>returns $expr$ with the first $n$ leaves removed.
    </dl>

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

    #> Drop[Range[10], {-2, -6, -3}]
     = {1, 2, 3, 4, 5, 7, 8, 10}
    #> Drop[Range[10], {10, 1, -3}]
     = {2, 3, 5, 6, 8, 9}

    #> Drop[Range[6], {-5, -2, -2}]
     : Cannot drop positions -5 through -2 in {1, 2, 3, 4, 5, 6}.
     = Drop[{1, 2, 3, 4, 5, 6}, {-5, -2, -2}]
    """

    messages = {
        "normal": "Nonatomic expression expected at position `1` in `2`.",
        "drop": "Cannot drop positions `1` through `2` in `3`.",
    }

    def apply(self, items, seqs, evaluation):
        "Drop[items_, seqs___]"

        seqs = seqs.get_sequence()

        if items.is_atom():
            return evaluation.message(
                "Drop", "normal", 1, Expression("Drop", items, *seqs)
            )

        try:
            return _parts(items, [_drop_span_selector(seq) for seq in seqs], evaluation)
        except MessageException as e:
            e.message(evaluation)


class Select(Builtin):
    """
    <dl>
    <dt>'Select[{$e1$, $e2$, ...}, $f$]'
        <dd>returns a list of the elements $ei$ for which $f$[$ei$]
        returns 'True'.
    </dl>

    Find numbers greater than zero:
    >> Select[{-3, 0, 1, 3, a}, #>0&]
     = {1, 3}

    'Select' works on an expression with any head:
    >> Select[f[a, 2, 3], NumberQ]
     = f[2, 3]

    >> Select[a, True]
     : Nonatomic expression expected.
     = Select[a, True]

    #> A[x__] := 31415 /; Length[{x}] == 3;
    #> Select[A[5, 2, 7, 1], OddQ]
     = 31415
    #> ClearAll[A];
    """

    def apply(self, items, expr, evaluation):
        "Select[items_, expr_]"

        if items.is_atom():
            evaluation.message("Select", "normal")
            return

        def cond(leaf):
            test = Expression(expr, leaf)
            return test.evaluate(evaluation).is_true()

        return items.filter(items.head, cond, evaluation)


class Split(Builtin):
    """
    <dl>
    <dt>'Split[$list$]'
        <dd>splits $list$ into collections of consecutive identical elements.
    <dt>'Split[$list$, $test$]'
        <dd>splits $list$ based on whether the function $test$ yields
        'True' on consecutive elements.
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

    #> A[x__] := 321 /; Length[{x}] == 5;
    #> Split[A[x, x, x, y, x, y, y, z]]
     = 321
    #> ClearAll[A];
    """

    rules = {
        "Split[list_]": "Split[list, SameQ]",
    }

    messages = {
        "normal": "Nonatomic expression expected at position `1` in `2`.",
    }

    def apply(self, mlist, test, evaluation):
        "Split[mlist_, test_]"

        expr = Expression("Split", mlist, test)

        if mlist.is_atom():
            evaluation.message("Select", "normal", 1, expr)
            return

        if not mlist.leaves:
            return Expression(mlist.head)

        result = [[mlist.leaves[0]]]
        for leaf in mlist.leaves[1:]:
            applytest = Expression(test, result[-1][-1], leaf)
            if applytest.evaluate(evaluation).is_true():
                result[-1].append(leaf)
            else:
                result.append([leaf])

        inner = structure("List", mlist, evaluation)
        outer = structure(mlist.head, inner, evaluation)
        return outer([inner(l) for l in result])


class SplitBy(Builtin):
    """
    <dl>
    <dt>'SplitBy[$list$, $f$]'
        <dd>splits $list$ into collections of consecutive elements
        that give the same result when $f$ is applied.
    </dl>

    >> SplitBy[Range[1, 3, 1/3], Round]
     = {{1, 4 / 3}, {5 / 3, 2, 7 / 3}, {8 / 3, 3}}

    >> SplitBy[{1, 2, 1, 1.2}, {Round, Identity}]
     = {{{1}}, {{2}}, {{1}, {1.2}}}

    #> SplitBy[Tuples[{1, 2}, 3], First]
     = {{{1, 1, 1}, {1, 1, 2}, {1, 2, 1}, {1, 2, 2}}, {{2, 1, 1}, {2, 1, 2}, {2, 2, 1}, {2, 2, 2}}}
    """

    rules = {
        "SplitBy[list_]": "SplitBy[list, Identity]",
    }

    messages = {
        "normal": "Nonatomic expression expected at position `1` in `2`.",
    }

    def apply(self, mlist, func, evaluation):
        "SplitBy[mlist_, func_?NotListQ]"

        expr = Expression("Split", mlist, func)

        if mlist.is_atom():
            evaluation.message("Select", "normal", 1, expr)
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

        inner = structure("List", mlist, evaluation)
        outer = structure(mlist.head, inner, evaluation)
        return outer([inner(l) for l in result])

    def apply_multiple(self, mlist, funcs, evaluation):
        "SplitBy[mlist_, funcs_?ListQ]"
        expr = Expression("Split", mlist, funcs)

        if mlist.is_atom():
            evaluation.message("Select", "normal", 1, expr)
            return

        result = mlist
        for f in funcs.leaves[::-1]:
            result = self.apply(result, f, evaluation)

        return result


class Pick(Builtin):
    """
    <dl>
    <dt>'Pick[$list$, $sel$]'
        <dd>returns those items in $list$ that are True in $sel$.
    <dt>'Pick[$list$, $sel$, $patt$]'
        <dd>returns those items in $list$ that match $patt$ in $sel$.
    </dl>

    >> Pick[{a, b, c}, {False, True, False}]
     = {b}

    >> Pick[f[g[1, 2], h[3, 4]], {{True, False}, {False, True}}]
     = f[g[1], h[4]]

    >> Pick[{a, b, c, d, e}, {1, 2, 3.5, 4, 5.5}, _Integer]
     = {a, b, d}
    """

    def _do(self, items0, sel0, match, evaluation):
        def pick(items, sel):
            for x, s in zip(items, sel):
                if match(s):
                    yield x
                elif not x.is_atom() and not s.is_atom():
                    yield x.restructure(x.head, pick(x.leaves, s.leaves), evaluation)

        r = list(pick([items0], [sel0]))
        if not r:
            return Expression(SymbolSequence)
        else:
            return r[0]

    def apply(self, items, sel, evaluation):
        "Pick[items_, sel_]"
        return self._do(items, sel, lambda s: s.is_true(), evaluation)

    def apply_pattern(self, items, sel, pattern, evaluation):
        "Pick[items_, sel_, pattern_]"
        from mathics.builtin.patterns import Matcher

        match = Matcher(pattern).match
        return self._do(items, sel, lambda s: match(s, evaluation), evaluation)


class Cases(Builtin):
    """
    <dl>
    <dt>'Cases[$list$, $pattern$]'
        <dd>returns the elements of $list$ that match $pattern$.
    <dt>'Cases[$list$, $pattern$, $ls$]'
        <dd>returns the elements matching at levelspec $ls$.
    </dl>

    >> Cases[{a, 1, 2.5, "string"}, _Integer|_Real]
     = {1, 2.5}
    >> Cases[_Complex][{1, 2I, 3, 4-I, 5}]
     = {2 I, 4 - I}

    #> Cases[1, 2]
     = {}

    #> Cases[f[1, 2], 2]
     = {2}

    #> Cases[f[f[1, 2], f[2]], 2]
     = {}
    #> Cases[f[f[1, 2], f[2]], 2, 2]
     = {2, 2}
    #> Cases[f[f[1, 2], f[2], 2], 2, Infinity]
     = {2, 2, 2}

    #> Cases[{1, f[2], f[3, 3, 3], 4, f[5, 5]}, f[x__] :> Plus[x]]
     = {2, 9, 10}
    #> Cases[{1, f[2], f[3, 3, 3], 4, f[5, 5]}, f[x__] -> Plus[x]]
     = {2, 3, 3, 3, 5, 5}

    ## Issue 531
    #> z = f[x, y]; x = 1; Cases[z, _Symbol, Infinity]
     = {y}
    """

    rules = {
        "Cases[pattern_][list_]": "Cases[list, pattern]",
    }

    options = {}

    def apply(self, items, pattern, ls, evaluation):
        "Cases[items_, pattern_, ls_:{1}]"
        if items.is_atom():
            return Expression(SymbolList)

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            return evaluation.message("Position", "level", ls)

        results = []

        from mathics.builtin.patterns import Matcher

        if pattern.has_form("Rule", 2) or pattern.has_form("RuleDelayed", 2):
            from mathics.core.rules import Rule

            match = Matcher(pattern.leaves[0]).match
            rule = Rule(pattern.leaves[0], pattern.leaves[1])

            def callback(level):
                if match(level, evaluation):
                    result = rule.apply(level, evaluation)
                    result = result.evaluate(evaluation)
                    results.append(result)
                return level

        else:
            match = Matcher(pattern).match

            def callback(level):
                if match(level, evaluation):
                    results.append(level)
                return level

        # TODO
        # heads = self.get_option(options, 'Heads', evaluation).is_true()
        heads = False

        walk_levels(items, start, stop, heads=heads, callback=callback)

        return Expression(SymbolList, *results)


class DeleteCases(Builtin):
    """
    <dl>
    <dt>'DeleteCases[$list$, $pattern$]'
        <dd>returns the elements of $list$ that do not match $pattern$.

    <dt>'DeleteCases[$list$, $pattern$, $levelspec$]'
        <dd> removes all parts of $list on levels specified by $levelspec$
             that match pattern (not fully implemented).

    <dt>'DeleteCases[$list$, $pattern$, $levelspec$, $n$]'
        <dd> removes the first $n$ parts of $list$ that match $pattern$.
    </dl>

    >> DeleteCases[{a, 1, 2.5, "string"}, _Integer|_Real]
     = {a, string}

    >> DeleteCases[{a, b, 1, c, 2, 3}, _Symbol]
     = {1, 2, 3}

    ## Issue 531
    #> z = {x, y}; x = 1; DeleteCases[z, _Symbol]
     = {1}
    """

    messages = {
        "level": "Level specification `1` is not of the form n, {n}, or {m, n}.",
        "innf": "Non-negative integer or Infinity expected at position 4 in `1`",
    }

    def apply_ls_n(self, items, pattern, levelspec, n, evaluation):
        "DeleteCases[items_, pattern_, levelspec_:1, n_:System`Infinity]"

        if items.is_atom():
            evaluation.message("Select", "normal")
            return
        # If levelspec is specified to a non-trivial value,
        # we need to proceed with this complicate procedure
        # involving 1) decode what is the levelspec means
        # 2) find all the occurences
        # 3) Set all the occurences to ```System`Nothing```

        levelspec = python_levelspec(levelspec)

        if n == Symbol("Infinity"):
            n = -1
        elif n.get_head_name() == "System`Integer":
            n = n.get_int_value()
            if n < 0:
                evaluation.message(
                    "DeleteCases",
                    "innf",
                    Expression("DeleteCases", items, pattern, levelspec, n),
                )
        else:
            evaluation.message(
                "DeleteCases",
                "innf",
                Expression("DeleteCases", items, pattern, levelspec, n),
            )
            return SymbolNull

        if levelspec[0] != 1 or levelspec[1] != 1:
            return deletecases_with_levelspec(items, pattern, evaluation, levelspec, n)
        # A more efficient way to proceed if levelspec == 1
        from mathics.builtin.patterns import Matcher

        match = Matcher(pattern).match
        if n == -1:

            def cond(leaf):
                return not match(leaf, evaluation)

            return items.filter("List", cond, evaluation)
        else:

            def condn(leaf):
                nonlocal n
                if n == 0:
                    return True
                elif match(leaf, evaluation):
                    n = n - 1
                    return False
                else:
                    return True

            return items.filter("List", condn, evaluation)


class Count(Builtin):
    """
    <dl>
    <dt>'Count[$list$, $pattern$]'
        <dd>returns the number of times $pattern$ appears in $list$.
    <dt>'Count[$list$, $pattern$, $ls$]'
        <dd>counts the elements matching at levelspec $ls$.
    </dl>

    >> Count[{3, 7, 10, 7, 5, 3, 7, 10}, 3]
     = 2

    >> Count[{{a, a}, {a, a, a}, a}, a, {2}]
     = 5
    """

    rules = {
        "Count[pattern_][list_]": "Count[list, pattern]",
        "Count[list_, arguments__]": "Length[Cases[list, arguments]]",
    }


class LeafCount(Builtin):
    """
    <dl>
    <dt>'LeafCount[$expr$]'
        <dd>returns the total number of indivisible subexpressions in $expr$.
    </dl>

    >> LeafCount[1 + x + y^a]
     = 6

    >> LeafCount[f[x, y]]
     = 3

    >> LeafCount[{1 / 3, 1 + I}]
     = 7

    >> LeafCount[Sqrt[2]]
     = 5

    >> LeafCount[100!]
     = 1

    #> LeafCount[f[a, b][x, y]]
     = 5

    #> NestList[# /. s[x_][y_][z_] -> x[z][y[z]] &, s[s][s][s[s]][s][s], 4];
    #> LeafCount /@ %
     = {7, 8, 8, 11, 11}

    #> LeafCount[1 / 3, 1 + I]
     : LeafCount called with 2 arguments; 1 argument is expected.
     = LeafCount[1 / 3, 1 + I]
    """

    messages = {
        "argx": "LeafCount called with `1` arguments; 1 argument is expected.",
    }

    def apply(self, expr, evaluation):
        "LeafCount[expr___]"

        from mathics.core.expression import Rational, Complex

        leaves = []

        def callback(level):
            if isinstance(level, Rational):
                leaves.extend(
                    [level.get_head(), level.numerator(), level.denominator()]
                )
            elif isinstance(level, Complex):
                leaves.extend([level.get_head(), level.real, level.imag])
            else:
                leaves.append(level)
            return level

        expr = expr.get_sequence()
        if len(expr) != 1:
            return evaluation.message("LeafCount", "argx", Integer(len(expr)))

        walk_levels(expr[0], start=-1, stop=-1, heads=True, callback=callback)
        return Integer(len(leaves))


class Position(Builtin):
    """
    <dl>
    <dt>'Position[$expr$, $patt$]'
        <dd>returns the list of positions for which $expr$ matches $patt$.
    <dt>'Position[$expr$, $patt$, $ls$]'
        <dd>returns the positions on levels specified by levelspec $ls$.
    </dl>

    >> Position[{1, 2, 2, 1, 2, 3, 2}, 2]
     = {{2}, {3}, {5}, {7}}

    Find positions upto 3 levels deep
    >> Position[{1 + Sin[x], x, (Tan[x] - y)^2}, x, 3]
     = {{1, 2, 1}, {2}}

    Find all powers of x
    >> Position[{1 + x^2, x y ^ 2,  4 y,  x ^ z}, x^_]
     = {{1, 2}, {4}}

    Use Position as an operator
    >> Position[_Integer][{1.5, 2, 2.5}]
     = {{2}}
    """

    options = {"Heads": "True"}

    rules = {
        "Position[pattern_][expr_]": "Position[expr, pattern]",
    }

    def apply_invalidlevel(self, patt, expr, ls, evaluation, options={}):
        "Position[expr_, patt_, ls_, OptionsPattern[Position]]"

        return evaluation.message("Position", "level", ls)

    def apply_level(self, expr, patt, ls, evaluation, options={}):
        """Position[expr_, patt_, Optional[Pattern[ls, _?LevelQ], {0, DirectedInfinity[1]}],
        OptionsPattern[Position]]"""

        try:
            start, stop = python_levelspec(ls)
        except InvalidLevelspecError:
            return evaluation.message("Position", "level", ls)

        from mathics.builtin.patterns import Matcher

        match = Matcher(patt).match
        result = []

        def callback(level, pos):
            if match(level, evaluation):
                result.append(pos)
            return level

        heads = self.get_option(options, "Heads", evaluation).is_true()
        walk_levels(expr, start, stop, heads=heads, callback=callback, include_pos=True)
        return from_python(result)


class MemberQ(Builtin):
    """
    <dl>
    <dt>'MemberQ[$list$, $pattern$]'
        <dd>returns 'True' if $pattern$ matches any element of $list$,
        or 'False' otherwise.
    </dl>

    >> MemberQ[{a, b, c}, b]
     = True
    >> MemberQ[{a, b, c}, d]
     = False
    >> MemberQ[{"a", b, f[x]}, _?NumericQ]
     = False
    >> MemberQ[_List][{{}}]
     = True
    """

    rules = {
        "MemberQ[list_, pattern_]": ("Length[Select[list, MatchQ[#, pattern]&]] > 0"),
        "MemberQ[pattern_][expr_]": "MemberQ[expr, pattern]",
    }


class Range(Builtin):
    """
    <dl>
    <dt>'Range[$n$]'
        <dd>returns a list of integers from 1 to $n$.
    <dt>'Range[$a$, $b$]'
        <dd>returns a list of integers from $a$ to $b$.
    </dl>
    >> Range[5]
     = {1, 2, 3, 4, 5}
    >> Range[-3, 2]
     = {-3, -2, -1, 0, 1, 2}
    >> Range[0, 2, 1/3]
     = {0, 1 / 3, 2 / 3, 1, 4 / 3, 5 / 3, 2}
    """

    rules = {
        "Range[imax_?RealNumberQ]": "Range[1, imax, 1]",
        "Range[imin_?RealNumberQ, imax_?RealNumberQ]": "Range[imin, imax, 1]",
    }

    def apply(self, imin, imax, di, evaluation):
        "Range[imin_?RealNumberQ, imax_?RealNumberQ, di_?RealNumberQ]"

        imin = imin.to_sympy()
        imax = imax.to_sympy()
        di = di.to_sympy()
        index = imin
        result = []
        while index <= imax:
            evaluation.check_stopped()
            result.append(from_sympy(index))
            index += di
        return Expression(SymbolList, *result)


class _IterationFunction(Builtin):
    """
    >> Sum[k, {k, Range[5]}]
     = 15
    """

    attributes = ("HoldAll",)
    allow_loopcontrol = False
    throw_iterb = True

    def get_result(self, items):
        pass

    def apply_symbol(self, expr, iterator, evaluation):
        "%(name)s[expr_, iterator_Symbol]"
        iterator = iterator.evaluate(evaluation)
        if iterator.has_form(["List", "Range", "Sequence"], None):
            leaves = iterator.leaves
            if len(leaves) == 1:
                return self.apply_max(expr, *leaves, evaluation)
            elif len(leaves) == 2:
                if leaves[1].has_form(["List", "Sequence"], None):
                    seq = Expression(SymbolSequence, *(leaves[1].leaves))
                    return self.apply_list(expr, leaves[0], seq, evaluation)
                else:
                    return self.apply_range(expr, *leaves, evaluation)
            elif len(leaves) == 3:
                return self.apply_iter_nostep(expr, *leaves, evaluation)
            elif len(leaves) == 4:
                return self.apply_iter(expr, *leaves, evaluation)

        if self.throw_iterb:
            evaluation.message(self.get_name(), "iterb")
        return

    def apply_range(self, expr, i, imax, evaluation):
        "%(name)s[expr_, {i_Symbol, imax_}]"
        imax = imax.evaluate(evaluation)
        if imax.has_form("Range", None):
            # Fixme: this should work as an iterator in python3, not
            # building the sequence explicitly...
            seq = Expression(SymbolSequence, *(imax.evaluate(evaluation).leaves))
            return self.apply_list(expr, i, seq, evaluation)
        elif imax.has_form("List", None):
            seq = Expression(SymbolSequence, *(imax.leaves))
            return self.apply_list(expr, i, seq, evaluation)
        else:
            return self.apply_iter(expr, i, Integer(1), imax, Integer(1), evaluation)

    def apply_max(self, expr, imax, evaluation):
        "%(name)s[expr_, {imax_}]"

        index = 0
        imax = imax.evaluate(evaluation)
        imax = imax.numerify(evaluation)
        if isinstance(imax, Number):
            imax = imax.round()
        imax = imax.get_float_value()
        if imax is None:
            if self.throw_iterb:
                evaluation.message(self.get_name(), "iterb")
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
            except ReturnInterrupt as e:
                if self.allow_loopcontrol:
                    return e.expr
                else:
                    raise
            index += 1
        return self.get_result(result)

    def apply_iter_nostep(self, expr, i, imin, imax, evaluation):
        "%(name)s[expr_, {i_Symbol, imin_, imax_}]"
        return self.apply_iter(expr, i, imin, imax, Integer(1), evaluation)

    def apply_iter(self, expr, i, imin, imax, di, evaluation):
        "%(name)s[expr_, {i_Symbol, imin_, imax_, di_}]"

        if isinstance(self, SympyFunction) and di.get_int_value() == 1:
            whole_expr = Expression(
                self.get_name(), expr, Expression(SymbolList, i, imin, imax)
            )
            sympy_expr = whole_expr.to_sympy(evaluation=evaluation)
            if sympy_expr is None:
                return None

            # apply Together to produce results similar to Mathematica
            result = sympy.together(sympy_expr)
            result = from_sympy(result)
            result = cancel(result)

            if not result.same(whole_expr):
                return result
            return

        index = imin.evaluate(evaluation)
        imax = imax.evaluate(evaluation)
        di = di.evaluate(evaluation)

        result = []
        compare_type = (
            "GreaterEqual"
            if Expression("Less", di, Integer(0)).evaluate(evaluation).to_python()
            else "LessEqual"
        )
        while True:
            cont = Expression(compare_type, index, imax).evaluate(evaluation)
            if cont == Symbol("False"):
                break
            if not cont.is_true():
                if self.throw_iterb:
                    evaluation.message(self.get_name(), "iterb")
                return

            evaluation.check_stopped()
            try:
                item = dynamic_scoping(expr.evaluate, {i.name: index}, evaluation)
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
            except ReturnInterrupt as e:
                if self.allow_loopcontrol:
                    return e.expr
                else:
                    raise
            index = Expression("Plus", index, di).evaluate(evaluation)
        return self.get_result(result)

    def apply_list(self, expr, i, items, evaluation):
        "%(name)s[expr_, {i_Symbol, {items___}}]"
        items = items.evaluate(evaluation).get_sequence()
        result = []
        for item in items:
            evaluation.check_stopped()
            try:
                item = dynamic_scoping(expr.evaluate, {i.name: item}, evaluation)
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
            except ReturnInterrupt as e:
                if self.allow_loopcontrol:
                    return e.expr
                else:
                    raise
        return self.get_result(result)

    def apply_multi(self, expr, first, sequ, evaluation):
        "%(name)s[expr_, first_, sequ__]"

        sequ = sequ.get_sequence()
        name = self.get_name()
        return Expression(name, Expression(name, expr, *sequ), first)


class ConstantArray(Builtin):
    """
    <dl>
    <dt>'ConstantArray[$expr$, $n$]'
        <dd>returns a list of $n$ copies of $expr$.
    </dl>

    >> ConstantArray[a, 3]
     = {a, a, a}
    >> ConstantArray[a, {2, 3}]
     = {{a, a, a}, {a, a, a}}
    """

    rules = {
        "ConstantArray[c_, dims_]": "Apply[Table[c, ##]&, List /@ dims]",
        "ConstantArray[c_, n_Integer]": "ConstantArray[c, {n}]",
    }


class Array(Builtin):
    """
    <dl>
    <dt>'Array[$f$, $n$]'
        <dd>returns the $n$-element list '{$f$[1], ..., $f$[$n$]}'.
    <dt>'Array[$f$, $n$, $a$]'
        <dd>returns the $n$-element list '{$f$[$a$], ..., $f$[$a$ + $n$]}'.
    <dt>'Array[$f$, {$n$, $m$}, {$a$, $b$}]'
        <dd>returns an $n$-by-$m$ matrix created by applying $f$ to
        indices ranging from '($a$, $b$)' to '($a$ + $n$, $b$ + $m$)'.
    <dt>'Array[$f$, $dims$, $origins$, $h$]'
        <dd>returns an expression with the specified dimensions and
        index origins, with head $h$ (instead of 'List').
    </dl>

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
        "plen": "`1` and `2` should have the same length.",
    }

    def apply(self, f, dimsexpr, origins, head, evaluation):
        "Array[f_, dimsexpr_, origins_:1, head_:List]"

        if dimsexpr.has_form("List", None):
            dims = dimsexpr.get_mutable_leaves()
        else:
            dims = [dimsexpr]
        for index, dim in enumerate(dims):
            value = dim.get_int_value()
            if value is None:
                evaluation.message("Array", "ilsnn", 2)
                return
            dims[index] = value
        if origins.has_form("List", None):
            if len(origins.leaves) != len(dims):
                evaluation.message("Array", "plen", dimsexpr, origins)
                return
            origins = origins.get_mutable_leaves()
        else:
            origins = [origins] * len(dims)
        for index, origin in enumerate(origins):
            value = origin.get_int_value()
            if value is None:
                evaluation.message("Array", "ilsnn", 3)
                return
            origins[index] = value

        dims = list(zip(dims, origins))

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
    <dl>
    <dt>'Table[$expr$, {$i$, $n$}]'
        <dd>evaluates $expr$ with $i$ ranging from 1 to $n$, returning
        a list of the results.
    <dt>'Table[$expr$, {$i$, $start$, $stop$, $step$}]'
        <dd>evaluates $expr$ with $i$ ranging from $start$ to $stop$,
        incrementing by $step$.
    <dt>'Table[$expr$, {$i$, {$e1$, $e2$, ..., $ei$}}]'
        <dd>evaluates $expr$ with $i$ taking on the values $e1$, $e2$,
        ..., $ei$.
    </dl>
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
        return Expression(SymbolList, *items)


class Join(Builtin):
    """
    <dl>
    <dt>'Join[$l1$, $l2$]'
        <dd>concatenates the lists $l1$ and $l2$.
    </dl>

    'Join' concatenates lists:
    >> Join[{a, b}, {c, d, e}]
     = {a, b, c, d, e}
    >> Join[{{a, b}, {c, d}}, {{1, 2}, {3, 4}}]
     = {{a, b}, {c, d}, {1, 2}, {3, 4}}

    The concatenated expressions may have any head:
    >> Join[a + b, c + d, e + f]
     = a + b + c + d + e + f

    However, it must be the same for all expressions:
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

    attributes = ("Flat", "OneIdentity")

    def apply(self, lists, evaluation):
        "Join[lists___]"

        result = []
        head = None
        sequence = lists.get_sequence()

        for list in sequence:
            if list.is_atom():
                return
            if head is not None and list.get_head() != head:
                evaluation.message("Join", "heads", head, list.get_head())
                return
            head = list.get_head()
            result.extend(list.leaves)

        if result:
            return sequence[0].restructure(head, result, evaluation, deps=sequence)
        else:
            return Expression(SymbolList)


class Catenate(Builtin):
    """
    <dl>
    <dt>'Catenate[{$l1$, $l2$, ...}]'
        <dd>concatenates the lists $l1$, $l2$, ...
    </dl>

    >> Catenate[{{1, 2, 3}, {4, 5}}]
     = {1, 2, 3, 4, 5}
    """

    messages = {"invrp": "`1` is not a list."}

    def apply(self, lists, evaluation):
        "Catenate[lists_List]"

        def parts():
            for l in lists.leaves:
                head_name = l.get_head_name()
                if head_name == "System`List":
                    yield l.leaves
                elif head_name != "System`Missing":
                    raise MessageException("Catenate", "invrp", l)

        try:
            result = list(chain(*list(parts())))
            if result:
                return lists.leaves[0].restructure(
                    "List", result, evaluation, deps=lists.leaves
                )
            else:
                return Expression(SymbolList)
        except MessageException as e:
            e.message(evaluation)


class Insert(Builtin):
    """
    <dl>
      <dt>'Insert[$list$, $elem$, $n$]'
      <dd>inserts $elem$ at position $n$ in $list$. When $n$ is negative, the position is counted from the end.
    </dl>

    >> Insert[{a,b,c,d,e}, x, 3]
     = {a, b, x, c, d, e}

    >> Insert[{a,b,c,d,e}, x, -2]
     = {a, b, c, d, x, e}
    """

    def apply(self, expr, elem, n, evaluation):
        "Insert[expr_List, elem_, n_Integer]"

        py_n = n.to_python()
        new_list = list(expr.get_leaves())

        position = py_n - 1 if py_n > 0 else py_n + 1
        new_list.insert(position, elem)
        return expr.restructure(expr.head, new_list, evaluation, deps=(expr, elem))


class Append(Builtin):
    """
    <dl>
      <dt>'Append[$expr$, $elem$]'
      <dd>returns $expr$ with $elem$ appended.
    </dl>

    >> Append[{1, 2, 3}, 4]
     = {1, 2, 3, 4}

    'Append' works on expressions with heads other than 'List':
    >> Append[f[a, b], c]
     = f[a, b, c]

    Unlike 'Join', 'Append' does not flatten lists in $item$:
    >> Append[{a, b}, {c, d}]
     = {a, b, {c, d}}

    #> Append[a, b]
     : Nonatomic expression expected.
     = Append[a, b]
    """

    def apply(self, expr, item, evaluation):
        "Append[expr_, item_]"

        if expr.is_atom():
            return evaluation.message("Append", "normal")

        return expr.restructure(
            expr.head,
            list(chain(expr.get_leaves(), [item])),
            evaluation,
            deps=(expr, item),
        )


class AppendTo(Builtin):
    """
    <dl>
    <dt>'AppendTo[$s$, $item$]'
        <dd>append $item$ to value of $s$ and sets $s$ to the result.
    </dl>

    >> s = {};
    >> AppendTo[s, 1]
     = {1}
    >> s
     = {1}

    'Append' works on expressions with heads other than 'List':
    >> y = f[];
    >> AppendTo[y, x]
     = f[x]
    >> y
     = f[x]

    #> AppendTo[{}, 1]
     : {} is not a variable with a value, so its value cannot be changed.
     = AppendTo[{}, 1]

    #> AppendTo[a, b]
     : a is not a variable with a value, so its value cannot be changed.
     = AppendTo[a, b]
    """

    attributes = ("HoldFirst",)

    messages = {
        "rvalue": "`1` is not a variable with a value, so its value cannot be changed.",
    }

    def apply(self, s, item, evaluation):
        "AppendTo[s_, item_]"
        if isinstance(s, Symbol):
            resolved_s = s.evaluate(evaluation)
            if not resolved_s.is_atom():
                result = Expression("Set", s, Expression("Append", resolved_s, item))
                return result.evaluate(evaluation)
        return evaluation.message("AppendTo", "rvalue", s)


class Prepend(Builtin):
    """
    <dl>
    <dt>'Prepend[$expr$, $item$]'
        <dd>returns $expr$ with $item$ prepended to its leaves.
    </dl>

    'Prepend' is similar to 'Append', but adds $item$ to the beginning
    of $expr$:
    >> Prepend[{2, 3, 4}, 1]
     = {1, 2, 3, 4}

    'Prepend' works on expressions with heads other than 'List':
    >> Prepend[f[b, c], a]
     = f[a, b, c]

    Unlike 'Join', 'Prepend' does not flatten lists in $item$:
    >> Prepend[{c, d}, {a, b}]
     = {{a, b}, c, d}

    #> Prepend[a, b]
     : Nonatomic expression expected.
     = Prepend[a, b]
    """

    def apply(self, expr, item, evaluation):
        "Prepend[expr_, item_]"

        if expr.is_atom():
            return evaluation.message("Prepend", "normal")

        return expr.restructure(
            expr.head,
            list(chain([item], expr.get_leaves())),
            evaluation,
            deps=(expr, item),
        )


class PrependTo(Builtin):
    """
    <dl>
    <dt>'PrependTo[$s$, $item$]'
        <dd>prepends $item$ to value of $s$ and sets $s$ to the result.
    </dl>

    Assign s to a list
    >> s = {1, 2, 4, 9}
     = {1, 2, 4, 9}

    Add a new value at the beginning of the list:
    >> PrependTo[s, 0]
     = {0, 1, 2, 4, 9}

    The value assigned to s has changed:
    >> s
     = {0, 1, 2, 4, 9}

    'PrependTo' works with a head other than 'List':
    >> y = f[a, b, c];
    >> PrependTo[y, x]
     = f[x, a, b, c]
    >> y
     = f[x, a, b, c]

    #> PrependTo[{a, b}, 1]
     :  {a, b} is not a variable with a value, so its value cannot be changed.
     = PrependTo[{a, b}, 1]

    #> PrependTo[a, b]
     : a is not a variable with a value, so its value cannot be changed.
     = PrependTo[a, b]

    #> x = 1 + 2;
    #> PrependTo[x, {3, 4}]
     : Nonatomic expression expected at position 1 in PrependTo[x, {3, 4}].
     =  PrependTo[x, {3, 4}]
    """

    attributes = ("HoldFirst",)

    messages = {
        "rvalue": "`1` is not a variable with a value, so its value cannot be changed.",
        "normal": "Nonatomic expression expected at position 1 in `1`.",
    }

    def apply(self, s, item, evaluation):
        "PrependTo[s_, item_]"
        if isinstance(s, Symbol):
            resolved_s = s.evaluate(evaluation)

            if not resolved_s.is_atom():
                result = Expression("Set", s, Expression("Prepend", resolved_s, item))
                return result.evaluate(evaluation)
            if s != resolved_s:
                return evaluation.message(
                    "PrependTo", "normal", Expression("PrependTo", s, item)
                )
        return evaluation.message("PrependTo", "rvalue", s)


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
        "Tuples[expr_, n_Integer]"

        if expr.is_atom():
            evaluation.message("Tuples", "normal")
            return
        n = n.get_int_value()
        if n is None or n < 0:
            evaluation.message("Tuples", "intnn")
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

        return Expression(
            "List", *(Expression(expr.head, *leaves) for leaves in iterate(n))
        )

    def apply_lists(self, exprs, evaluation):
        "Tuples[{exprs___}]"

        exprs = exprs.get_sequence()
        items = []
        for expr in exprs:
            evaluation.check_stopped()
            if expr.is_atom():
                evaluation.message("Tuples", "normal")
                return
            items.append(expr.leaves)

        return Expression(
            "List", *(Expression(SymbolList, *leaves) for leaves in get_tuples(items))
        )


class Reap(Builtin):
    """
    <dl>
    <dt>'Reap[$expr$]'
        <dd>gives the result of evaluating $expr$, together with all
        values sown during this evaluation. Values sown with different
        tags are given in different lists.
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

    attributes = ("HoldFirst",)

    rules = {
        "Reap[expr_, pattern_, f_]": (
            "{#[[1]], #[[2, 1]]}& [Reap[expr, {pattern}, f]]"
        ),
        "Reap[expr_, pattern_]": "Reap[expr, pattern, #2&]",
        "Reap[expr_]": "Reap[expr, _]",
    }

    def apply(self, expr, patterns, f, evaluation):
        "Reap[expr_, {patterns___}, f_]"

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

        evaluation.add_listener("sow", listener)
        try:
            result = expr.evaluate(evaluation)
            items = []
            for pattern, tags in sown:
                leaves = []
                for tag, elements in tags:
                    leaves.append(Expression(f, tag, Expression(SymbolList, *elements)))
                items.append(Expression(SymbolList, *leaves))
            return Expression(SymbolList, result, Expression(SymbolList, *items))
        finally:
            evaluation.remove_listener("sow", listener)


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
        "Sow[e_]": "Sow[e, {Null}]",
        "Sow[e_, tag_]": "Sow[e, {tag}]",
    }

    def apply(self, e, tags, evaluation):
        "Sow[e_, {tags___}]"

        tags = tags.get_sequence()
        for tag in tags:
            evaluation.publish("sow", e, tag)
        return e


class UnitVector(Builtin):
    """
    <dl>
    <dt>'UnitVector[$n$, $k$]'
        <dd>returns the $n$-dimensional unit vector with a 1 in position $k$.
    <dt>'UnitVector[$k$]'
        <dd>is equivalent to 'UnitVector[2, $k$]'.
    </dl>
    >> UnitVector[2]
     = {0, 1}
    >> UnitVector[4, 3]
     = {0, 0, 1, 0}
    """

    messages = {
        "nokun": "There is no unit vector in direction `1` in `2` dimensions.",
    }

    rules = {
        "UnitVector[k_Integer]": "UnitVector[2, k]",
    }

    def apply(self, n, k, evaluation):
        "UnitVector[n_Integer, k_Integer]"

        n = n.get_int_value()
        k = k.get_int_value()
        if n is None or k is None:
            return
        if not 1 <= k <= n:
            evaluation.message("UnitVector", "nokun", k, n)
            return

        def item(i):
            if i == k:
                return Integer(1)
            else:
                return Integer(0)

        return Expression(SymbolList, *(item(i) for i in range(1, n + 1)))


def riffle(items, sep):
    result = items[:1]
    for item in items[1:]:
        result.append(sep)
        result.append(item)
    return result


def riffle_lists(items, seps):
    if len(seps) == 0:  # special case
        seps = [Expression(SymbolList)]

    i = 0
    while i < len(items):
        yield items[i]
        if i == len(items) - 1 and len(items) != len(seps):
            return
        yield seps[i % len(seps)]
        i += 1


class Riffle(Builtin):
    """
    <dl>
    <dt>'Riffle[$list$, $x$]'
        <dd>inserts a copy of $x$ between each element of $list$.
    <dt>'Riffle[{$a1$, $a2$, ...}, {$b1$, $b2$, ...}]'
        <dd>interleaves the elements of both lists, returning
        '{$a1$, $b1$, $a2$, $b2$, ...}'.
    </dl>

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

    #> Riffle[{a,b,c}, {}]
     = {a, {}, b, {}, c}
    #> Riffle[{}, {}]
     = {}
    #> Riffle[{}, {a,b}]
     = {}
    """

    def apply(self, list, sep, evaluation):
        "Riffle[list_List, sep_]"

        if sep.has_form("List", None):
            result = riffle_lists(list.get_leaves(), sep.leaves)
        else:
            result = riffle_lists(list.get_leaves(), [sep])

        return list.restructure("List", result, evaluation, deps=(list, sep))


def _is_sameq(same_test):
    # System`SameQ is protected, so nobody should ever be able to change
    # it (see Set::wrsym). We just check for its name here thus.
    return same_test.is_symbol() and same_test.get_name() == "System`SameQ"


def _test_pair(test, a, b, evaluation, name):
    test_expr = Expression(test, a, b)
    result = test_expr.evaluate(evaluation)
    if not (
        result.is_symbol() and (result.has_symbol("True") or result.has_symbol("False"))
    ):
        evaluation.message(name, "smtst", test_expr, result)
    return result.is_true()


class _SlowEquivalence:
    # models an equivalence relation through a user defined test function. for n
    # distinct elements (each in its own bin), we need sum(1, .., n - 1) = O(n^2)
    # comparisons.

    def __init__(self, test, evaluation, name):
        self._groups = []
        self._test = test
        self._evaluation = evaluation
        self._name = name

    def select(self, elem):
        return self._groups

    def same(self, a, b):
        return _test_pair(self._test, a, b, self._evaluation, self._name)


class _FastEquivalence:
    # models an equivalence relation through SameQ. for n distinct elements (each
    # in its own bin), we expect to make O(n) comparisons (if the hash function
    # does not fail us by distributing items very unevenly).

    # IMPORTANT NOTE ON ATOM'S HASH FUNCTIONS / this code relies on this assumption:
    #
    # if SameQ[a, b] == true then hash(a) == hash(b)
    #
    # more specifically, this code bins items based on their hash code, and only if
    # the hash code matches, is SameQ evoked.
    #
    # this assumption has been checked for these types: Integer, Real, Complex,
    # String, Rational (*), Expression, Image; new atoms need proper hash functions
    #
    # (*) Rational values are sympy Rationals which are always held in reduced form
    # and thus are hashed correctly (see sympy/core/numbers.py:Rational.__eq__()).

    def __init__(self):
        self._hashes = defaultdict(list)

    def select(self, elem):
        return self._hashes[hash(elem)]

    def same(self, a, b):
        return a.same(b)


class _GatherBin:
    def __init__(self, item):
        self._items = [item]
        self.add_to = self._items.append

    def from_python(self):
        return Expression(SymbolList, *self._items)


class _TallyBin:
    def __init__(self, item):
        self._item = item
        self._count = 1

    def add_to(self, item):
        self._count += 1

    def from_python(self):
        return Expression(SymbolList, self._item, Integer(self._count))


class _DeleteDuplicatesBin:
    def __init__(self, item):
        self._item = item
        self.add_to = lambda elem: None

    def from_python(self):
        return self._item


class _GatherOperation(Builtin):
    rules = {"%(name)s[list_]": "%(name)s[list, SameQ]"}

    messages = {
        "normal": "Nonatomic expression expected at position `1` in `2`.",
        "list": "List expected at position `2` in `1`.",
        "smtst": (
            "Application of the SameTest yielded `1`, which evaluates "
            "to `2`. The SameTest must evaluate to True or False at "
            "every pair of elements."
        ),
    }

    def apply(self, values, test, evaluation):
        "%(name)s[values_, test_]"
        if not self._check_list(values, test, evaluation):
            return

        if _is_sameq(test):
            return self._gather(values, values, _FastEquivalence())
        else:
            return self._gather(
                values, values, _SlowEquivalence(test, evaluation, self.get_name())
            )

    def _check_list(self, values, arg2, evaluation):
        if values.is_atom():
            expr = Expression(self.get_name(), values, arg2)
            evaluation.message(self.get_name(), "normal", 1, expr)
            return False

        if values.get_head_name() != "System`List":
            expr = Expression(self.get_name(), values, arg2)
            evaluation.message(self.get_name(), "list", expr, 1)
            return False

        return True

    def _gather(self, keys, values, equivalence):
        bins = []
        Bin = self._bin

        for key, value in zip(keys.leaves, values.leaves):
            selection = equivalence.select(key)
            for prototype, add_to_bin in selection:  # find suitable bin
                if equivalence.same(prototype, key):
                    add_to_bin(value)  # add to existing bin
                    break
            else:
                new_bin = Bin(value)  # create new bin
                selection.append((key, new_bin.add_to))
                bins.append(new_bin)

        return Expression(SymbolList, *[b.from_python() for b in bins])


class Gather(_GatherOperation):
    """
    <dl>
    <dt>'Gather[$list$, $test$]'
    <dd>gathers leaves of $list$ into sub lists of items that are the same according to $test$.

    <dt>'Gather[$list$]'
    <dd>gathers leaves of $list$ into sub lists of items that are the same.
    </dl>

    The order of the items inside the sub lists is the same as in the original list.

    >> Gather[{1, 7, 3, 7, 2, 3, 9}]
     = {{1}, {7, 7}, {3, 3}, {2}, {9}}

    >> Gather[{1/3, 2/6, 1/9}]
     = {{1 / 3, 1 / 3}, {1 / 9}}
    """

    _bin = _GatherBin


class GatherBy(_GatherOperation):
    """
    <dl>
    <dt>'GatherBy[$list$, $f$]'
    <dd>gathers leaves of $list$ into sub lists of items whose image under $f identical.

    <dt>'GatherBy[$list$, {$f$, $g$, ...}]'
    <dd>gathers leaves of $list$ into sub lists of items whose image under $f identical.
    Then, gathers these sub lists again into sub sub lists, that are identical under $g.
    </dl>

    >> GatherBy[{{1, 3}, {2, 2}, {1, 1}}, Total]
     = {{{1, 3}, {2, 2}}, {{1, 1}}}

    >> GatherBy[{"xy", "abc", "ab"}, StringLength]
     = {{xy, ab}, {abc}}

    >> GatherBy[{{2, 0}, {1, 5}, {1, 0}}, Last]
     = {{{2, 0}, {1, 0}}, {{1, 5}}}

    >> GatherBy[{{1, 2}, {2, 1}, {3, 5}, {5, 1}, {2, 2, 2}}, {Total, Length}]
     = {{{{1, 2}, {2, 1}}}, {{{3, 5}}}, {{{5, 1}}, {{2, 2, 2}}}}
    """

    rules = {
        "GatherBy[l_]": "GatherBy[l, Identity]",
        "GatherBy[l_, {r__, f_}]": "Map[GatherBy[#, f]&, GatherBy[l, {r}], {Length[{r}]}]",
        "GatherBy[l_, {f_}]": "GatherBy[l, f]",
    }

    _bin = _GatherBin

    def apply(self, values, func, evaluation):
        "%(name)s[values_, func_]"

        if not self._check_list(values, func, evaluation):
            return

        keys = Expression("Map", func, values).evaluate(evaluation)
        if len(keys.leaves) != len(values.leaves):
            return

        return self._gather(keys, values, _FastEquivalence())


class Tally(_GatherOperation):
    """
    <dl>
    <dt>'Tally[$list$]'
    <dd>counts and returns the number of occurences of objects and returns
    the result as a list of pairs {object, count}.
    <dt>'Tally[$list$, $test$]'
    <dd>counts the number of occurences of  objects and uses $test to
    determine if two objects should be counted in the same bin.
    </dl>

    >> Tally[{a, b, c, b, a}]
     = {{a, 2}, {b, 2}, {c, 1}}

    Tally always returns items in the order as they first appear in $list$:
    >> Tally[{b, b, a, a, a, d, d, d, d, c}]
     = {{b, 2}, {a, 3}, {d, 4}, {c, 1}}
    """

    _bin = _TallyBin


class DeleteDuplicates(_GatherOperation):
    """
    <dl>
    <dt>'DeleteDuplicates[$list$]'
        <dd>deletes duplicates from $list$.
    <dt>'DeleteDuplicates[$list$, $test$]'
        <dd>deletes elements from $list$ based on whether the function
        $test$ yields 'True' on pairs of elements.
    DeleteDuplicates does not change the order of the remaining elements.
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

    _bin = _DeleteDuplicatesBin


class _SetOperation(Builtin):
    messages = {
        "normal": "Non-atomic expression expected at position `1` in `2`.",
        "heads": (
            "Heads `1` and `2` at positions `3` and `4` are expected " "to be the same."
        ),
        "smtst": (
            "Application of the SameTest yielded `1`, which evaluates "
            "to `2`. The SameTest must evaluate to True or False at "
            "every pair of elements."
        ),
    }

    options = {
        "SameTest": "SameQ",
    }

    @staticmethod
    def _remove_duplicates(arg, same_test):
        "removes duplicates from a single operand"
        result = []
        for a in arg:
            if not any(same_test(a, b) for b in result):
                result.append(a)
        return result

    def apply(self, lists, evaluation, options={}):
        "%(name)s[lists__, OptionsPattern[%(name)s]]"

        seq = lists.get_sequence()

        for pos, e in enumerate(seq):
            if e.is_atom():
                return evaluation.message(
                    self.get_name(),
                    "normal",
                    pos + 1,
                    Expression(self.get_name(), *seq),
                )

        for pos, e in enumerate(zip(seq, seq[1:])):
            e1, e2 = e
            if e1.head != e2.head:
                return evaluation.message(
                    self.get_name(), "heads", e1.head, e2.head, pos + 1, pos + 2
                )

        same_test = self.get_option(options, "SameTest", evaluation)
        operands = [l.leaves for l in seq]
        if not _is_sameq(same_test):
            same = lambda a, b: _test_pair(same_test, a, b, evaluation, self.get_name())
            operands = [self._remove_duplicates(op, same) for op in operands]
            items = functools.reduce(
                lambda a, b: [e for e in self._elementwise(a, b, same)], operands
            )
        else:
            items = list(
                functools.reduce(getattr(set, self._operation), map(set, operands))
            )

        return Expression(seq[0].get_head(), *sorted(items))


class Union(_SetOperation):
    """
    <dl>
    <dt>'Union[$a$, $b$, ...]'
    <dd>gives the union of the given set or sets. The resulting list will be sorted
    and each element will only occur once.
    </dl>

    >> Union[{5, 1, 3, 7, 1, 8, 3}]
     = {1, 3, 5, 7, 8}

    >> Union[{a, b, c}, {c, d, e}]
     = {a, b, c, d, e}

    >> Union[{c, b, a}]
     = {a, b, c}

    >> Union[{{a, 1}, {b, 2}}, {{c, 1}, {d, 3}}, SameTest->(SameQ[Last[#1],Last[#2]]&)]
     = {{b, 2}, {c, 1}, {d, 3}}

    >> Union[{1, 2, 3}, {2, 3, 4}, SameTest->Less]
     = {1, 2, 2, 3, 4}

    #> Union[{1, -1, 2}, {-2, 3}, SameTest -> (Abs[#1] == Abs[#2] &)]
     = {-2, 1, 3}
    """

    _operation = "union"

    def _elementwise(self, a, b, same):
        for eb in b:
            yield eb
        for ea in a:
            if not any(same(eb, ea) for eb in b):
                yield ea


class Intersection(_SetOperation):
    """
    <dl>
    <dt>'Intersection[$a$, $b$, ...]'
    <dd>gives the intersection of the sets. The resulting list will be sorted
    and each element will only occur once.
    </dl>

    >> Intersection[{1000, 100, 10, 1}, {1, 5, 10, 15}]
     = {1, 10}

    >> Intersection[{{a, b}, {x, y}}, {{x, x}, {x, y}, {x, z}}]
     = {{x, y}}

    >> Intersection[{c, b, a}]
     = {a, b, c}

    >> Intersection[{1, 2, 3}, {2, 3, 4}, SameTest->Less]
     = {3}

    #> Intersection[{1, -1, -2, 2, -3}, {1, -2, 2, 3}, SameTest -> (Abs[#1] == Abs[#2] &)]
     = {-3, -2, 1}
    """

    _operation = "intersection"

    def _elementwise(self, a, b, same):
        for ea in a:
            if any(same(eb, ea) for eb in b):
                yield ea


class Complement(_SetOperation):
    """
    <dl>
    <dt>'Complement[$all$, $e1$, $e2$, ...]'
        <dd>returns an expression containing the elements in the set
        $all$ that are not in any of $e1$, $e2$, etc.
    <dt>'Complement[$all$, $e1$, $e2$, ..., SameTest->$test$]'
        <dd>applies $test$ to the elements in $all$ and each of the
        $ei$ to determine equality.
    </dl>

    The sets $all$, $e1$, etc can have any head, which must all match.
    The returned expression has the same head as the input
    expressions. The expression will be sorted and each element will
    only occur once.

    >> Complement[{a, b, c}, {a, c}]
     = {b}
    >> Complement[{a, b, c}, {a, c}, {b}]
     = {}
    >> Complement[f[z, y, x, w], f[x], f[x, z]]
     = f[w, y]
    >> Complement[{c, b, a}]
     = {a, b, c}

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

    _operation = "difference"

    def _elementwise(self, a, b, same):
        for ea in a:
            if not any(same(eb, ea) for eb in b):
                yield ea


class IntersectingQ(Builtin):
    """
    <dl>
    <dt>'IntersectingQ[$a$, $b$]'
    <dd>gives True if there are any common elements in $a and $b, or False if $a and $b are disjoint.
    </dl>
    """

    rules = {"IntersectingQ[a_List, b_List]": "Length[Intersect[a, b]] > 0"}


class DisjointQ(Test):
    """
    <dl>
    <dt>'DisjointQ[$a$, $b$]'
    <dd>gives True if $a and $b are disjoint, or False if $a and $b have any common elements.
    </dl>
    """

    rules = {"DisjointQ[a_List, b_List]": "Not[IntersectingQ[a, b]]"}


class Fold(Builtin):
    """
    <dl>
    <dt>'Fold[$f$, $x$, $list$]'
        <dd>returns the result of iteratively applying the binary
        operator $f$ to each element of $list$, starting with $x$.
    <dt>'Fold[$f$, $list$]'
        <dd>is equivalent to 'Fold[$f$, First[$list$], Rest[$list$]]'.
    </dl>

    >> Fold[Plus, 5, {1, 1, 1}]
     = 8
    >> Fold[f, 5, {1, 2, 3}]
     = f[f[f[5, 1], 2], 3]
    """

    rules = {
        "Fold[exp_, x_, head_]": "Module[{list = Level[head, 1], res = x, i = 1}, Do[res = exp[res, list[[i]]], {i, 1, Length[list]}]; res]",
        "Fold[exp_, head_] /; Length[head] > 0": "Fold[exp, First[head], Rest[head]]",
    }


class FoldList(Builtin):
    """
    <dl>
    <dt>'FoldList[$f$, $x$, $list$]'
        <dd>returns a list starting with $x$, where each element is
        the result of applying the binary operator $f$ to the previous
        result and the next element of $list$.
    <dt>'FoldList[$f$, $list$]'
        <dd>is equivalent to 'FoldList[$f$, First[$list$], Rest[$list$]]'.
    </dl>

    >> FoldList[f, x, {1, 2, 3}]
     = {x, f[x, 1], f[f[x, 1], 2], f[f[f[x, 1], 2], 3]}
    >> FoldList[Times, {1, 2, 3}]
     = {1, 2, 6}
    """

    rules = {
        "FoldList[exp_, x_, head_]": "Module[{i = 1}, Head[head] @@ Prepend[Table[Fold[exp, x, Take[head, i]], {i, 1, Length[head]}], x]]",
        "FoldList[exp_, head_]": "If[Length[head] == 0, head, FoldList[exp, First[head], Rest[head]]]",
    }


class Accumulate(Builtin):
    """
    <dl>
    <dt>'Accumulate[$list$]'
        <dd>accumulates the values of $list$, returning a new list.
    </dl>

    >> Accumulate[{1, 2, 3}]
     = {1, 3, 6}
    """

    rules = {"Accumulate[head_]": "FoldList[Plus, head]"}


class Total(Builtin):
    """
    <dl>
    <dt>'Total[$list$]'
        <dd>adds all values in $list$.
    <dt>'Total[$list$, $n$]'
        <dd>adds all values up to level $n$.
    <dt>'Total[$list$, {$n$}]'
        <dd>totals only the values at level {$n$}.
    <dt>'Total[$list$, {$n_1$, $n_2$}]'
        <dd>totals at levels {$n_1$, $n_2$}.
    </dl>

    >> Total[{1, 2, 3}]
     = 6
    >> Total[{{1, 2, 3}, {4, 5, 6}, {7, 8 ,9}}]
     = {12, 15, 18}

    Total over rows and columns
    >> Total[{{1, 2, 3}, {4, 5, 6}, {7, 8 ,9}}, 2]
     = 45

    Total over rows instead of columns
    >> Total[{{1, 2, 3}, {4, 5, 6}, {7, 8 ,9}}, {2}]
     = {6, 15, 24}
    """

    rules = {
        "Total[head_]": "Apply[Plus, head]",
        "Total[head_, n_]": "Apply[Plus, Flatten[head, n]]",
    }


class Reverse(Builtin):
    """
    <dl>
    <dt>'Reverse[$expr$]'
        <dd>reverses the order of $expr$'s items (on the top level)
    <dt>'Reverse[$expr$, $n$]'
        <dd>reverses the order of items in $expr$ on level $n$
    <dt>'Reverse[$expr$, {$n1$, $n2$, ...}]'
        <dd>reverses the order of items in $expr$ on levels $n1$, $n2$, ...
    </dl>

    >> Reverse[{1, 2, 3}]
     = {3, 2, 1}
    >> Reverse[x[a, b, c]]
     = x[c, b, a]
    >> Reverse[{{1, 2}, {3, 4}}, 1]
     = {{3, 4}, {1, 2}}
    >> Reverse[{{1, 2}, {3, 4}}, 2]
     = {{2, 1}, {4, 3}}
    >> Reverse[{{1, 2}, {3, 4}}, {1, 2}]
     = {{4, 3}, {2, 1}}
    """

    messages = {
        "ilsmp": "Positive integer or list of positive integers expected at position 2 of ``."
    }

    @staticmethod
    def _reverse(
        expr, level, levels, evaluation
    ):  # depth >= 1, levels are expected to be unique and sorted
        if not isinstance(expr, Expression):
            return expr

        if levels[0] == level:
            expr = expr.restructure(expr.head, reversed(expr.leaves), evaluation)

            if len(levels) > 1:
                expr = expr.restructure(
                    expr.head,
                    [
                        Reverse._reverse(leaf, level + 1, levels[1:], evaluation)
                        for leaf in expr.leaves
                    ],
                    evaluation,
                )
        else:
            expr = expr.restructure(
                expr.head,
                [
                    Reverse._reverse(leaf, level + 1, levels, evaluation)
                    for leaf in expr.leaves
                ],
                evaluation,
            )

        return expr

    def apply_top_level(self, expr, evaluation):
        "Reverse[expr_]"
        return Reverse._reverse(expr, 1, (1,), evaluation)

    def apply(self, expr, levels, evaluation):
        "Reverse[expr_, levels_]"
        if isinstance(levels, Integer):
            py_levels = [levels.get_int_value()]
        elif levels.get_head_name() == "System`List":
            if not levels.leaves:
                return expr
            if any(not isinstance(level, Integer) for level in levels.leaves):
                py_levels = None
            else:
                py_levels = sorted(
                    list(set(level.get_int_value() for level in levels.leaves))
                )
        else:
            py_levels = None
        if py_levels and py_levels[0] < 1:  # if py_level is not None, it's sorted
            py_levels = None
        if py_levels is None:
            evaluation.message("Reverse", "ilsmp", Expression("Reverse", expr, levels))
        else:
            return Reverse._reverse(expr, 1, py_levels, evaluation)


class CentralMoment(Builtin):  # see https://en.wikipedia.org/wiki/Central_moment
    """
    <dl>
    <dt>'CentralMoment[$list$, $r$]'
      <dd>gives the the $r$th central moment (i.e. the $r$th moment about the mean) of $list$.
    </dl>

    >> CentralMoment[{1.1, 1.2, 1.4, 2.1, 2.4}, 4]
     = 0.100845
    """

    rules = {
        "CentralMoment[list_List, r_]": "Total[(list - Mean[list]) ^ r] / Length[list]",
    }


class Skewness(Builtin):  # see https://en.wikipedia.org/wiki/Skewness
    """
    <dl>
    <dt>'Skewness[$list$]'
      <dd>gives Pearson's moment coefficient of skewness for $list$ (a measure for estimating
      the symmetry of a distribution).
    </dl>

    >> Skewness[{1.1, 1.2, 1.4, 2.1, 2.4}]
     = 0.407041
    """

    rules = {
        "Skewness[list_List]": "CentralMoment[list, 3] / (CentralMoment[list, 2] ^ (3 / 2))",
    }


class Kurtosis(Builtin):  # see https://en.wikipedia.org/wiki/Kurtosis
    """
    <dl>
    <dt>'Kurtosis[$list$]'
      <dd>gives the Pearson measure of kurtosis for $list$ (a measure of existing outliers).
    </dl>

    >> Kurtosis[{1.1, 1.2, 1.4, 2.1, 2.4}]
     = 1.42098
    """

    rules = {
        "Kurtosis[list_List]": "CentralMoment[list, 4] / (CentralMoment[list, 2] ^ 2)",
    }


class Mean(Builtin):
    """
    <dl>
    <dt>'Mean[$list$]'
      <dd>returns the statistical mean of $list$.
    </dl>

    >> Mean[{26, 64, 36}]
     = 42

    >> Mean[{1, 1, 2, 3, 5, 8}]
     = 10 / 3

    >> Mean[{a, b}]
     = (a + b) / 2
    """

    rules = {
        "Mean[list_]": "Total[list] / Length[list]",
    }


class _NotRectangularException(Exception):
    pass


class _Rectangular(Builtin):
    # A helper for Builtins X that allow X[{a1, a2, ...}, {b1, b2, ...}, ...] to be evaluated
    # as {X[{a1, b1, ...}, {a1, b2, ...}, ...]}.

    def rect(self, l):
        lengths = [len(leaf.leaves) for leaf in l.leaves]
        if all(length == 0 for length in lengths):
            return  # leave as is, without error

        n_columns = lengths[0]
        if any(length != n_columns for length in lengths[1:]):
            raise _NotRectangularException()

        transposed = [[leaf.leaves[i] for leaf in l.leaves] for i in range(n_columns)]

        return Expression(
            "List",
            *[
                Expression(self.get_name(), Expression(SymbolList, *items))
                for items in transposed
            ],
        )


class Variance(_Rectangular):
    """
    <dl>
    <dt>'Variance[$list$]'
      <dd>computes the variance of $list. $list$ may consist of numerical values
      or symbols. Numerical values may be real or complex.

      Variance[{{$a1$, $a2$, ...}, {$b1$, $b2$, ...}, ...}] will yield
      {Variance[{$a1$, $b1$, ...}, Variance[{$a2$, $b2$, ...}], ...}.
    </dl>

    >> Variance[{1, 2, 3}]
     = 1

    >> Variance[{7, -5, 101, 3}]
     = 7475 / 3

    >> Variance[{1 + 2I, 3 - 10I}]
     = 74

    >> Variance[{a, a}]
     = 0

    >> Variance[{{1, 3, 5}, {4, 10, 100}}]
     = {9 / 2, 49 / 2, 9025 / 2}
    """

    messages = {
        "shlen": "`` must contain at least two elements.",
        "rectt": "Expected a rectangular array at position 1 in ``.",
    }

    # for the general formulation of real and complex variance below, see for example
    # https://en.wikipedia.org/wiki/Variance#Generalizations

    def apply(self, l, evaluation):
        "Variance[l_List]"
        if len(l.leaves) <= 1:
            evaluation.message("Variance", "shlen", l)
        elif all(leaf.get_head_name() == "System`List" for leaf in l.leaves):
            try:
                return self.rect(l)
            except _NotRectangularException:
                evaluation.message("Variance", "rectt", Expression("Variance", l))
        else:
            d = Expression("Subtract", l, Expression("Mean", l))
            return Expression(
                "Divide",
                Expression("Dot", d, Expression("Conjugate", d)),
                len(l.leaves) - 1,
            )


class StandardDeviation(_Rectangular):
    """
    <dl>
    <dt>'StandardDeviation[$list$]'
      <dd>computes the standard deviation of $list. $list$ may consist of numerical values
      or symbols. Numerical values may be real or complex.

      StandardDeviation[{{$a1$, $a2$, ...}, {$b1$, $b2$, ...}, ...}] will yield
      {StandardDeviation[{$a1$, $b1$, ...}, StandardDeviation[{$a2$, $b2$, ...}], ...}.
    </dl>

    >> StandardDeviation[{1, 2, 3}]
     = 1

    >> StandardDeviation[{7, -5, 101, 100}]
     = Sqrt[13297] / 2

    >> StandardDeviation[{a, a}]
     = 0

    >> StandardDeviation[{{1, 10}, {-1, 20}}]
     = {Sqrt[2], 5 Sqrt[2]}
    """

    messages = {
        "shlen": "`` must contain at least two elements.",
        "rectt": "Expected a rectangular array at position 1 in ``.",
    }

    def apply(self, l, evaluation):
        "StandardDeviation[l_List]"
        if len(l.leaves) <= 1:
            evaluation.message("StandardDeviation", "shlen", l)
        elif all(leaf.get_head_name() == "System`List" for leaf in l.leaves):
            try:
                return self.rect(l)
            except _NotRectangularException:
                evaluation.message(
                    "StandardDeviation", "rectt", Expression("StandardDeviation", l)
                )
        else:
            return Expression("Sqrt", Expression("Variance", l))


class Covariance(Builtin):
    """
    <dl>
    <dt>'Covariance[$a$, $b$]'
      <dd>computes the covariance between the equal-sized vectors $a$ and $b$.
    </dl>

    >> Covariance[{0.2, 0.3, 0.1}, {0.3, 0.3, -0.2}]
     = 0.025
    """

    messages = {
        "shlen": "`` must contain at least two elements.",
        "vctmat": "`1` and `2` need to be of equal length.",
    }

    def apply(self, a, b, evaluation):
        "Covariance[a_List, b_List]"

        if len(a.leaves) != len(b.leaves):
            evaluation.message("Covariance", "vctmat", a, b)
        elif len(a.leaves) < 2:
            evaluation.message("Covariance", "shlen", a)
        elif len(b.leaves) < 2:
            evaluation.message("Covariance", "shlen", b)
        else:
            ma = Expression("Subtract", a, Expression("Mean", a))
            mb = Expression("Subtract", b, Expression("Mean", b))
            return Expression(
                "Divide",
                Expression("Dot", ma, Expression("Conjugate", mb)),
                len(a.leaves) - 1,
            )


class Correlation(Builtin):
    """
    <dl>
    <dt>'Correlation[$a$, $b$]'
      <dd>computes Pearson's correlation of two equal-sized vectors $a$ and $b$.
    </dl>

    An example from Wikipedia:

    >> Correlation[{10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5}, {8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68}]
     = 0.816421
    """

    messages = {
        "shlen": "`` must contain at least two elements.",
        "vctmat": "`1` and `2` need to be of equal length.",
    }

    def apply(self, a, b, evaluation):
        "Correlation[a_List, b_List]"

        if len(a.leaves) != len(b.leaves):
            evaluation.message("Correlation", "vctmat", a, b)
        elif len(a.leaves) < 2:
            evaluation.message("Correlation", "shlen", a)
        elif len(b.leaves) < 2:
            evaluation.message("Correlation", "shlen", b)
        else:
            da = Expression("StandardDeviation", a)
            db = Expression("StandardDeviation", b)
            return Expression(
                "Divide", Expression("Covariance", a, b), Expression("Times", da, db)
            )


class _Rotate(Builtin):
    messages = {"rspec": "`` should be an integer or a list of integers."}

    def _rotate(self, expr, n, evaluation):
        if not isinstance(expr, Expression):
            return expr

        leaves = expr.leaves
        if not leaves:
            return expr

        index = (self._sign * n[0]) % len(leaves)  # with Python's modulo: index >= 1
        new_leaves = chain(leaves[index:], leaves[:index])

        if len(n) > 1:
            new_leaves = [self._rotate(item, n[1:], evaluation) for item in new_leaves]

        return expr.restructure(expr.head, new_leaves, evaluation)

    def apply_one(self, expr, evaluation):
        "%(name)s[expr_]"
        return self._rotate(expr, [1], evaluation)

    def apply(self, expr, n, evaluation):
        "%(name)s[expr_, n_]"
        if isinstance(n, Integer):
            py_cycles = [n.get_int_value()]
        elif n.get_head_name() == "System`List" and all(
            isinstance(x, Integer) for x in n.leaves
        ):
            py_cycles = [x.get_int_value() for x in n.leaves]
            if not py_cycles:
                return expr
        else:
            evaluation.message(self.get_name(), "rspec", n)
            return

        return self._rotate(expr, py_cycles, evaluation)


class RotateLeft(_Rotate):
    """
    <dl>
    <dt>'RotateLeft[$expr$]'
        <dd>rotates the items of $expr$' by one item to the left.
    <dt>'RotateLeft[$expr$, $n$]'
        <dd>rotates the items of $expr$' by $n$ items to the left.
    <dt>'RotateLeft[$expr$, {$n1$, $n2$, ...}]'
        <dd>rotates the items of $expr$' by $n1$ items to the left at the first level, by $n2$ items to the left at
        the second level, and so on.
    </dl>

    >> RotateLeft[{1, 2, 3}]
     = {2, 3, 1}
    >> RotateLeft[Range[10], 3]
     = {4, 5, 6, 7, 8, 9, 10, 1, 2, 3}
    >> RotateLeft[x[a, b, c], 2]
     = x[c, a, b]
    >> RotateLeft[{{a, b, c}, {d, e, f}, {g, h, i}}, {1, 2}]
     = {{f, d, e}, {i, g, h}, {c, a, b}}
    """

    _sign = 1


class RotateRight(_Rotate):
    """
    <dl>
    <dt>'RotateRight[$expr$]'
        <dd>rotates the items of $expr$' by one item to the right.
    <dt>'RotateRight[$expr$, $n$]'
        <dd>rotates the items of $expr$' by $n$ items to the right.
    <dt>'RotateRight[$expr$, {$n1$, $n2$, ...}]'
        <dd>rotates the items of $expr$' by $n1$ items to the right at the first level, by $n2$ items to the right at
        the second level, and so on.
    </dl>

    >> RotateRight[{1, 2, 3}]
     = {3, 1, 2}
    >> RotateRight[Range[10], 3]
     = {8, 9, 10, 1, 2, 3, 4, 5, 6, 7}
    >> RotateRight[x[a, b, c], 2]
     = x[b, c, a]
    >> RotateRight[{{a, b, c}, {d, e, f}, {g, h, i}}, {1, 2}]
     = {{h, i, g}, {b, c, a}, {e, f, d}}
    """

    _sign = -1


class Median(_Rectangular):
    """
    <dl>
    <dt>'Median[$list$]'
      <dd>returns the median of $list$.
    </dl>

    >> Median[{26, 64, 36}]
     = 36

    For lists with an even number of elements, Median returns the mean of the two middle values:
    >> Median[{-11, 38, 501, 1183}]
     = 539 / 2

    Passing a matrix returns the medians of the respective columns:
    >> Median[{{100, 1, 10, 50}, {-1, 1, -2, 2}}]
     = {99 / 2, 1, 4, 26}
    """

    messages = {"rectn": "Expected a rectangular array of numbers at position 1 in ``."}

    def apply(self, l, evaluation):
        "Median[l_List]"
        if not l.leaves:
            return
        if all(leaf.get_head_name() == "System`List" for leaf in l.leaves):
            try:
                return self.rect(l)
            except _NotRectangularException:
                evaluation.message("Median", "rectn", Expression("Median", l))
        elif all(leaf.is_numeric() for leaf in l.leaves):
            v = l.get_mutable_leaves()  # copy needed for introselect
            n = len(v)
            if n % 2 == 0:  # even number of elements?
                i = n // 2
                a = introselect(v, i)
                b = introselect(v, i - 1)
                return Expression("Divide", Expression("Plus", a, b), 2)
            else:
                i = n // 2
                return introselect(v, i)
        else:
            evaluation.message("Median", "rectn", Expression("Median", l))


class RankedMin(Builtin):
    """
    <dl>
    <dt>'RankedMin[$list$, $n$]'
      <dd>returns the $n$th smallest element of $list$ (with $n$ = 1 yielding the smallest element,
      $n$ = 2 yielding the second smallest element, and so on).
    </dl>

    >> RankedMin[{482, 17, 181, -12}, 2]
     = 17
    """

    messages = {
        "intpm": "Expected positive integer at position 2 in ``.",
        "rank": "The specified rank `1` is not between 1 and `2`.",
    }

    def apply(self, l, n, evaluation):
        "RankedMin[l_List, n_Integer]"
        py_n = n.get_int_value()
        if py_n < 1:
            evaluation.message("RankedMin", "intpm", Expression("RankedMin", l, n))
        elif py_n > len(l.leaves):
            evaluation.message("RankedMin", "rank", py_n, len(l.leaves))
        else:
            return introselect(l.get_mutable_leaves(), py_n - 1)


class RankedMax(Builtin):
    """
    <dl>
    <dt>'RankedMax[$list$, $n$]'
      <dd>returns the $n$th largest element of $list$ (with $n$ = 1 yielding the largest element,
      $n$ = 2 yielding the second largest element, and so on).
    </dl>

    >> RankedMax[{482, 17, 181, -12}, 2]
     = 181
    """

    messages = {
        "intpm": "Expected positive integer at position 2 in ``.",
        "rank": "The specified rank `1` is not between 1 and `2`.",
    }

    def apply(self, l, n, evaluation):
        "RankedMax[l_List, n_Integer]"
        py_n = n.get_int_value()
        if py_n < 1:
            evaluation.message("RankedMax", "intpm", Expression("RankedMax", l, n))
        elif py_n > len(l.leaves):
            evaluation.message("RankedMax", "rank", py_n, len(l.leaves))
        else:
            return introselect(l.get_mutable_leaves(), len(l.leaves) - py_n)


class Quantile(Builtin):
    """
    <dl>
    <dt>'Quantile[$list$, $q$]'
      <dd>returns the $q$th quantile of $list$.
    </dl>

    >> Quantile[Range[11], 1/3]
     = 4

    >> Quantile[Range[16], 1/4]
     = 5
    """

    rules = {
        "Quantile[list_List, q_, abcd_]": "Quantile[list, {q}, abcd]",
        "Quantile[list_List, q_]": "Quantile[list, q, {{0, 1}, {1, 0}}]",
    }

    messages = {
        "nquan": "The quantile `1` has to be between 0 and 1.",
    }

    def apply(self, l, qs, a, b, c, d, evaluation):
        """Quantile[l_List, qs_List, {{a_, b_}, {c_, d_}}]"""

        n = len(l.leaves)
        partially_sorted = l.get_mutable_leaves()

        def ranked(i):
            return introselect(partially_sorted, min(max(0, i - 1), n - 1))

        numeric_qs = qs.evaluate(evaluation).numerify(evaluation)
        results = []

        for q in numeric_qs.leaves:
            py_q = q.to_mpmath()

            if py_q is None or not 0.0 <= py_q <= 1.0:
                evaluation.message("Quantile", "nquan", q)
                return

            x = Expression(
                "Plus", a, Expression("Times", Expression("Plus", Integer(n), b), q)
            )

            numeric_x = x.evaluate(evaluation).numerify(evaluation)

            if isinstance(numeric_x, Integer):
                results.append(ranked(numeric_x.get_int_value()))
            else:
                py_x = numeric_x.to_mpmath()

                if py_x is None:
                    return

                from mpmath import floor as mpfloor, ceil as mpceil

                if c.get_int_value() == 1 and d.get_int_value() == 0:  # k == 1?
                    results.append(ranked(int(mpceil(py_x))))
                else:
                    py_floor_x = mpfloor(py_x)
                    s0 = ranked(int(py_floor_x))
                    s1 = ranked(int(mpceil(py_x)))

                    k = Expression(
                        "Plus",
                        c,
                        Expression(
                            "Times",
                            d,
                            Expression("Subtract", x, Expression("Floor", x)),
                        ),
                    )

                    results.append(
                        Expression(
                            "Plus",
                            s0,
                            Expression("Times", k, Expression("Subtract", s1, s0)),
                        )
                    )

        if len(results) == 1:
            return results[0]
        else:
            return Expression(SymbolList, *results)


class Quartiles(Builtin):
    """
    <dl>
    <dt>'Quartiles[$list$]'
      <dd>returns the 1/4, 1/2, and 3/4 quantiles of $list$.
    </dl>

    >> Quartiles[Range[25]]
     = {27 / 4, 13, 77 / 4}
    """

    rules = {
        "Quartiles[list_List]": "Quantile[list, {1/4, 1/2, 3/4}, {{1/2, 0}, {0, 1}}]",
    }


class _RankedTake(Builtin):
    messages = {
        "intpm": "Expected non-negative integer at position `1` in `2`.",
        "rank": "The specified rank `1` is not between 1 and `2`.",
    }

    options = {
        "ExcludedForms": "Automatic",
    }

    def _compute(self, l, n, evaluation, options, f=None):
        try:
            limit = CountableInteger.from_expression(n)
        except MessageException as e:
            e.message(evaluation)
            return
        except NegativeIntegerException:
            if f:
                args = (3, Expression(self.get_name(), l, f, n))
            else:
                args = (2, Expression(self.get_name(), l, n))
            evaluation.message(self.get_name(), "intpm", *args)
            return

        if limit is None:
            return

        if limit == 0:
            return Expression(SymbolList)
        else:
            excluded = self.get_option(options, "ExcludedForms", evaluation)
            if excluded:
                if (
                    isinstance(excluded, Symbol)
                    and excluded.get_name() == "System`Automatic"
                ):

                    def exclude(item):
                        if isinstance(item, Symbol) and item.get_name() in (
                            "System`None",
                            "System`Null",
                            "System`Indeterminate",
                        ):
                            return True
                        elif item.get_head_name() == "System`Missing":
                            return True
                        else:
                            return False

                else:
                    excluded = Expression("Alternatives", *excluded.leaves)

                    def exclude(item):
                        return (
                            Expression("MatchQ", item, excluded)
                            .evaluate(evaluation)
                            .is_true()
                        )

                filtered = [leaf for leaf in l.leaves if not exclude(leaf)]
            else:
                filtered = l.leaves

            if limit > len(filtered):
                if not limit.is_upper_limit():
                    evaluation.message(
                        self.get_name(), "rank", limit.get_int_value(), len(filtered)
                    )
                    return
                else:
                    py_n = len(filtered)
            else:
                py_n = limit.get_int_value()

            if py_n < 1:
                return Expression(SymbolList)

            if f:
                heap = [
                    (Expression(f, leaf).evaluate(evaluation), leaf, i)
                    for i, leaf in enumerate(filtered)
                ]
                leaf_pos = 1  # in tuple above
            else:
                heap = [(leaf, i) for i, leaf in enumerate(filtered)]
                leaf_pos = 0  # in tuple above

            if py_n == 1:
                result = [self._get_1(heap)]
            else:
                result = self._get_n(py_n, heap)

            return l.restructure("List", [x[leaf_pos] for x in result], evaluation)


class _RankedTakeSmallest(_RankedTake):
    def _get_1(self, a):
        return min(a)

    def _get_n(self, n, heap):
        return heapq.nsmallest(n, heap)


class _RankedTakeLargest(_RankedTake):
    def _get_1(self, a):
        return max(a)

    def _get_n(self, n, heap):
        return heapq.nlargest(n, heap)


class TakeLargest(_RankedTakeLargest):
    """
    <dl>
    <dt>'TakeLargest[$list$, $f$, $n$]'
        <dd>returns the a sorted list of the $n$ largest items in $list$.
    </dl>

    >> TakeLargest[{100, -1, 50, 10}, 2]
     = {100, 50}

    None, Null, Indeterminate and expressions with head Missing are ignored
    by default:
    >> TakeLargest[{-8, 150, Missing[abc]}, 2]
     = {150, -8}

    You may specify which items are ignored using the option ExcludedForms:
    >> TakeLargest[{-8, 150, Missing[abc]}, 2, ExcludedForms -> {}]
     = {Missing[abc], 150}
    """

    def apply(self, l, n, evaluation, options):
        "TakeLargest[l_List, n_, OptionsPattern[TakeLargest]]"
        return self._compute(l, n, evaluation, options)


class TakeLargestBy(_RankedTakeLargest):
    """
    <dl>
    <dt>'TakeLargestBy[$list$, $f$, $n$]'
        <dd>returns the a sorted list of the $n$ largest items in $list$
        using $f$ to retrieve the items' keys to compare them.
    </dl>

    For details on how to use the ExcludedForms option, see TakeLargest[].

    >> TakeLargestBy[{{1, -1}, {10, 100}, {23, 7, 8}, {5, 1}}, Total, 2]
     = {{10, 100}, {23, 7, 8}}

    >> TakeLargestBy[{"abc", "ab", "x"}, StringLength, 1]
     = {abc}
    """

    def apply(self, l, f, n, evaluation, options):
        "TakeLargestBy[l_List, f_, n_, OptionsPattern[TakeLargestBy]]"
        return self._compute(l, n, evaluation, options, f=f)


class TakeSmallest(_RankedTakeSmallest):
    """
    <dl>
    <dt>'TakeSmallest[$list$, $f$, $n$]'
        <dd>returns the a sorted list of the $n$ smallest items in $list$.
    </dl>

    For details on how to use the ExcludedForms option, see TakeLargest[].

    >> TakeSmallest[{100, -1, 50, 10}, 2]
     = {-1, 10}
    """

    def apply(self, l, n, evaluation, options):
        "TakeSmallest[l_List, n_, OptionsPattern[TakeSmallest]]"
        return self._compute(l, n, evaluation, options)


class TakeSmallestBy(_RankedTakeSmallest):
    """
    <dl>
    <dt>'TakeSmallestBy[$list$, $f$, $n$]'
        <dd>returns the a sorted list of the $n$ smallest items in $list$
        using $f$ to retrieve the items' keys to compare them.
    </dl>

    For details on how to use the ExcludedForms option, see TakeLargest[].

    >> TakeSmallestBy[{{1, -1}, {10, 100}, {23, 7, 8}, {5, 1}}, Total, 2]
     = {{1, -1}, {5, 1}}

    >> TakeSmallestBy[{"abc", "ab", "x"}, StringLength, 1]
     = {x}
    """

    def apply(self, l, f, n, evaluation, options):
        "TakeSmallestBy[l_List, f_, n_, OptionsPattern[TakeSmallestBy]]"
        return self._compute(l, n, evaluation, options, f=f)


class _IllegalPaddingDepth(Exception):
    def __init__(self, level):
        self.level = level


class _Pad(Builtin):
    messages = {
        "normal": "Expression at position 1 in `` must not be an atom.",
        "level": "Cannot pad list `3` which has `4` using padding `1` which specifies `2`.",
        "ilsm": "Expected an integer or a list of integers at position `1` in `2`.",
    }

    rules = {"%(name)s[l_]": "%(name)s[l, Automatic]"}

    @staticmethod
    def _find_dims(expr):
        def dive(expr, level):
            if isinstance(expr, Expression):
                if expr.leaves:
                    return max(dive(x, level + 1) for x in expr.leaves)
                else:
                    return level + 1
            else:
                return level

        def calc(expr, dims, level):
            if isinstance(expr, Expression):
                for x in expr.leaves:
                    calc(x, dims, level + 1)
                dims[level] = max(dims[level], len(expr.leaves))

        dims = [0] * dive(expr, 0)
        calc(expr, dims, 0)
        return dims

    @staticmethod
    def _build(l, n, x, m, level, mode):  # mode < 0 for left pad, > 0 for right pad
        if not n:
            return l
        if not isinstance(l, Expression):
            raise _IllegalPaddingDepth(level)

        if isinstance(m, (list, tuple)):
            current_m = m[0] if m else 0
            next_m = m[1:]
        else:
            current_m = m
            next_m = m

        def clip(a, d, s):
            assert d != 0
            if s < 0:
                return a[-d:]  # end with a[-1]
            else:
                return a[:d]  # start with a[0]

        def padding(amount, sign):
            if amount == 0:
                return []
            elif len(n) > 1:
                return [
                    _Pad._build(
                        Expression(SymbolList), n[1:], x, next_m, level + 1, mode
                    )
                ] * amount
            else:
                return clip(x * (1 + amount // len(x)), amount, sign)

        leaves = l.leaves
        d = n[0] - len(leaves)
        if d < 0:
            new_leaves = clip(leaves, d, mode)
            padding_main = []
        elif d >= 0:
            new_leaves = leaves
            padding_main = padding(d, mode)

        if current_m > 0:
            padding_margin = padding(
                min(current_m, len(new_leaves) + len(padding_main)), -mode
            )

            if len(padding_margin) > len(padding_main):
                padding_main = []
                new_leaves = clip(
                    new_leaves, -(len(padding_margin) - len(padding_main)), mode
                )
            elif len(padding_margin) > 0:
                padding_main = clip(padding_main, -len(padding_margin), mode)
        else:
            padding_margin = []

        if len(n) > 1:
            new_leaves = (
                _Pad._build(e, n[1:], x, next_m, level + 1, mode) for e in new_leaves
            )

        if mode < 0:
            parts = (padding_main, new_leaves, padding_margin)
        else:
            parts = (padding_margin, new_leaves, padding_main)

        return Expression(l.get_head(), *list(chain(*parts)))

    def _pad(self, in_l, in_n, in_x, in_m, evaluation, expr):
        if not isinstance(in_l, Expression):
            evaluation.message(self.get_name(), "normal", expr())
            return

        py_n = None
        if isinstance(in_n, Symbol) and in_n.get_name() == "System`Automatic":
            py_n = _Pad._find_dims(in_l)
        elif in_n.get_head_name() == "System`List":
            if all(isinstance(leaf, Integer) for leaf in in_n.leaves):
                py_n = [leaf.get_int_value() for leaf in in_n.leaves]
        elif isinstance(in_n, Integer):
            py_n = [in_n.get_int_value()]

        if py_n is None:
            evaluation.message(self.get_name(), "ilsm", 2, expr())
            return

        if in_x.get_head_name() == "System`List":
            py_x = in_x.leaves
        else:
            py_x = [in_x]

        if isinstance(in_m, Integer):
            py_m = in_m.get_int_value()
        else:
            if not all(isinstance(x, Integer) for x in in_m.leaves):
                evaluation.message(self.get_name(), "ilsm", 4, expr())
                return
            py_m = [x.get_int_value() for x in in_m.leaves]

        try:
            return _Pad._build(in_l, py_n, py_x, py_m, 1, self._mode)
        except _IllegalPaddingDepth as e:

            def levels(k):
                if k == 1:
                    return "1 level"
                else:
                    return "%d levels" % k

            evaluation.message(
                self.get_name(),
                "level",
                in_n,
                levels(len(py_n)),
                in_l,
                levels(e.level - 1),
            )
            return None

    def apply_zero(self, l, n, evaluation):
        "%(name)s[l_, n_]"
        return self._pad(
            l,
            n,
            Integer(0),
            Integer(0),
            evaluation,
            lambda: Expression(self.get_name(), l, n),
        )

    def apply(self, l, n, x, evaluation):
        "%(name)s[l_, n_, x_]"
        return self._pad(
            l,
            n,
            x,
            Integer(0),
            evaluation,
            lambda: Expression(self.get_name(), l, n, x),
        )

    def apply_margin(self, l, n, x, m, evaluation):
        "%(name)s[l_, n_, x_, m_]"
        return self._pad(
            l, n, x, m, evaluation, lambda: Expression(self.get_name(), l, n, x, m)
        )


class PadLeft(_Pad):
    """
    <dl>
    <dt>'PadLeft[$list$, $n$]'
        <dd>pads $list$ to length $n$ by adding 0 on the left.
    <dt>'PadLeft[$list$, $n$, $x$]'
        <dd>pads $list$ to length $n$ by adding $x$ on the left.
    <dt>'PadLeft[$list$, {$n1$, $n2, ...}, $x$]'
        <dd>pads $list$ to lengths $n1$, $n2$ at levels 1, 2, ... respectively by adding $x$ on the left.
    <dt>'PadLeft[$list$, $n$, $x$, $m$]'
        <dd>pads $list$ to length $n$ by adding $x$ on the left and adding a margin of $m$ on the right.
    <dt>'PadLeft[$list$, $n$, $x$, {$m1$, $m2$, ...}]'
        <dd>pads $list$ to length $n$ by adding $x$ on the left and adding margins of $m1$, $m2$, ...
         on levels 1, 2, ... on the right.
    <dt>'PadLeft[$list$]'
        <dd>turns the ragged list $list$ into a regular list by adding 0 on the left.
    </dl>

    >> PadLeft[{1, 2, 3}, 5]
     = {0, 0, 1, 2, 3}
    >> PadLeft[x[a, b, c], 5]
     = x[0, 0, a, b, c]
    >> PadLeft[{1, 2, 3}, 2]
     = {2, 3}
    >> PadLeft[{{}, {1, 2}, {1, 2, 3}}]
     = {{0, 0, 0}, {0, 1, 2}, {1, 2, 3}}
    >> PadLeft[{1, 2, 3}, 10, {a, b, c}, 2]
     = {b, c, a, b, c, 1, 2, 3, a, b}
    >> PadLeft[{{1, 2, 3}}, {5, 2}, x, 1]
     = {{x, x}, {x, x}, {x, x}, {3, x}, {x, x}}
    """

    _mode = -1


class PadRight(_Pad):
    """
    <dl>
    <dt>'PadRight[$list$, $n$]'
        <dd>pads $list$ to length $n$ by adding 0 on the right.
    <dt>'PadRight[$list$, $n$, $x$]'
        <dd>pads $list$ to length $n$ by adding $x$ on the right.
    <dt>'PadRight[$list$, {$n1$, $n2, ...}, $x$]'
        <dd>pads $list$ to lengths $n1$, $n2$ at levels 1, 2, ... respectively by adding $x$ on the right.
    <dt>'PadRight[$list$, $n$, $x$, $m$]'
        <dd>pads $list$ to length $n$ by adding $x$ on the left and adding a margin of $m$ on the left.
    <dt>'PadRight[$list$, $n$, $x$, {$m1$, $m2$, ...}]'
        <dd>pads $list$ to length $n$ by adding $x$ on the right and adding margins of $m1$, $m2$, ...
         on levels 1, 2, ... on the left.
    <dt>'PadRight[$list$]'
        <dd>turns the ragged list $list$ into a regular list by adding 0 on the right.
    </dl>

    >> PadRight[{1, 2, 3}, 5]
     = {1, 2, 3, 0, 0}
    >> PadRight[x[a, b, c], 5]
     = x[a, b, c, 0, 0]
    >> PadRight[{1, 2, 3}, 2]
     = {1, 2}
    >> PadRight[{{}, {1, 2}, {1, 2, 3}}]
     = {{0, 0, 0}, {1, 2, 0}, {1, 2, 3}}
    >> PadRight[{1, 2, 3}, 10, {a, b, c}, 2]
     = {b, c, 1, 2, 3, a, b, c, a, b}
    >> PadRight[{{1, 2, 3}}, {5, 2}, x, 1]
     = {{x, x}, {x, 1}, {x, x}, {x, x}, {x, x}}
    """

    _mode = 1


class _IllegalDistance(Exception):
    def __init__(self, distance):
        self.distance = distance


class _IllegalDataPoint(Exception):
    pass


def _to_real_distance(d):
    if not isinstance(d, (Real, Integer)):
        raise _IllegalDistance(d)

    mpd = d.to_mpmath()
    if mpd is None or mpd < 0:
        raise _IllegalDistance(d)

    return mpd


class _PrecomputedDistances(PrecomputedDistances):
    # computes all n^2 distances for n points with one big evaluation in the beginning.

    def __init__(self, df, p, evaluation):
        distances_form = [df(p[i], p[j]) for i in range(len(p)) for j in range(i)]
        distances = Expression(
            SymbolN, Expression(SymbolList, *distances_form)
        ).evaluate(evaluation)
        mpmath_distances = [_to_real_distance(d) for d in distances.leaves]
        super(_PrecomputedDistances, self).__init__(mpmath_distances)


class _LazyDistances(LazyDistances):
    # computes single distances only as needed, caches already computed distances.

    def __init__(self, df, p, evaluation):
        super(_LazyDistances, self).__init__()
        self._df = df
        self._p = p
        self._evaluation = evaluation

    def _compute_distance(self, i, j):
        p = self._p
        d = Expression(SymbolN, self._df(p[i], p[j])).evaluate(self._evaluation)
        return _to_real_distance(d)


def _dist_repr(p):
    dist_p = repr_p = None
    if p.has_form("Rule", 2):
        if all(q.get_head_name() == "System`List" for q in p.leaves):
            dist_p, repr_p = (q.leaves for q in p.leaves)
        elif (
            p.leaves[0].get_head_name() == "System`List"
            and p.leaves[1].get_name() == "System`Automatic"
        ):
            dist_p = p.leaves[0].leaves
            repr_p = [Integer(i + 1) for i in range(len(dist_p))]
    elif p.get_head_name() == "System`List":
        if all(q.get_head_name() == "System`Rule" for q in p.leaves):
            dist_p, repr_p = ([q.leaves[i] for q in p.leaves] for i in range(2))
        else:
            dist_p = repr_p = p.leaves
    return dist_p, repr_p


class _Cluster(Builtin):
    options = {
        "Method": "Optimize",
        "DistanceFunction": "Automatic",
        "RandomSeed": "Automatic",
    }

    messages = {
        "amtd": "`1` failed to pick a suitable distance function for `2`.",
        "bdmtd": 'Method in `` must be either "Optimize", "Agglomerate" or "KMeans".',
        "intpm": "Positive integer expected at position 2 in ``.",
        "list": "Expected a list or a rule with equally sized lists at position 1 in ``.",
        "nclst": "Cannot find more clusters than there are elements: `1` is larger than `2`.",
        "xnum": "The distance function returned ``, which is not a non-negative real value.",
        "rseed": "The random seed specified through `` must be an integer or Automatic.",
        "kmsud": "KMeans only supports SquaredEuclideanDistance as distance measure.",
    }

    _criteria = {
        "Optimize": AutomaticSplitCriterion,
        "Agglomerate": AutomaticMergeCriterion,
        "KMeans": None,
    }

    def _cluster(self, p, k, mode, evaluation, options, expr):
        method_string, method = self.get_option_string(options, "Method", evaluation)
        if method_string not in ("Optimize", "Agglomerate", "KMeans"):
            evaluation.message(
                self.get_name(), "bdmtd", Expression(SymbolRule, "Method", method)
            )
            return

        dist_p, repr_p = _dist_repr(p)

        if dist_p is None or len(dist_p) != len(repr_p):
            evaluation.message(self.get_name(), "list", expr)
            return

        if not dist_p:
            return Expression(SymbolList)

        if k is not None:  # the number of clusters k is specified as an integer.
            if not isinstance(k, Integer):
                evaluation.message(self.get_name(), "intpm", expr)
                return
            py_k = k.get_int_value()
            if py_k < 1:
                evaluation.message(self.get_name(), "intpm", expr)
                return
            if py_k > len(dist_p):
                evaluation.message(self.get_name(), "nclst", py_k, len(dist_p))
                return
            elif py_k == 1:
                return Expression(SymbolList, *repr_p)
            elif py_k == len(dist_p):
                return Expression(
                    SymbolList, [Expression(SymbolList, q) for q in repr_p]
                )
        else:  # automatic detection of k. choose a suitable method here.
            if len(dist_p) <= 2:
                return Expression(SymbolList, *repr_p)
            constructor = self._criteria.get(method_string)
            py_k = (constructor, {}) if constructor else None

        seed_string, seed = self.get_option_string(options, "RandomSeed", evaluation)
        if seed_string == "Automatic":
            py_seed = 12345
        elif isinstance(seed, Integer):
            py_seed = seed.get_int_value()
        else:
            evaluation.message(
                self.get_name(), "rseed", Expression(SymbolRule, "RandomSeed", seed)
            )
            return

        distance_function_string, distance_function = self.get_option_string(
            options, "DistanceFunction", evaluation
        )
        if distance_function_string == "Automatic":
            from mathics.builtin.tensors import get_default_distance

            distance_function = get_default_distance(dist_p)
            if distance_function is None:
                name_of_builtin = strip_context(self.get_name())
                evaluation.message(
                    self.get_name(),
                    "amtd",
                    name_of_builtin,
                    Expression(SymbolList, *dist_p),
                )
                return

        if (
            method_string == "KMeans"
            and distance_function != "SquaredEuclideanDistance"
        ):
            evaluation.message(self.get_name(), "kmsud")
            return

        def df(i, j):
            return Expression(distance_function, i, j)

        try:
            if method_string == "Agglomerate":
                clusters = self._agglomerate(mode, repr_p, dist_p, py_k, df, evaluation)
            elif method_string == "Optimize":
                clusters = optimize(
                    repr_p, py_k, _LazyDistances(df, dist_p, evaluation), mode, py_seed
                )
            elif method_string == "KMeans":
                clusters = self._kmeans(mode, repr_p, dist_p, py_k, py_seed, evaluation)
        except _IllegalDistance as e:
            evaluation.message(self.get_name(), "xnum", e.distance)
            return
        except _IllegalDataPoint:
            name_of_builtin = strip_context(self.get_name())
            evaluation.message(
                self.get_name(),
                "amtd",
                name_of_builtin,
                Expression(SymbolList, *dist_p),
            )
            return

        if mode == "clusters":
            return Expression(
                SymbolList, *[Expression(SymbolList, *c) for c in clusters]
            )
        elif mode == "components":
            return Expression(SymbolList, *clusters)
        else:
            raise ValueError("illegal mode %s" % mode)

    def _agglomerate(self, mode, repr_p, dist_p, py_k, df, evaluation):
        if mode == "clusters":
            clusters = agglomerate(
                repr_p, py_k, _PrecomputedDistances(df, dist_p, evaluation), mode
            )
        elif mode == "components":
            clusters = agglomerate(
                repr_p, py_k, _PrecomputedDistances(df, dist_p, evaluation), mode
            )

        return clusters

    def _kmeans(self, mode, repr_p, dist_p, py_k, py_seed, evaluation):
        items = []

        def convert_scalars(p):
            for q in p:
                if not isinstance(q, (Real, Integer)):
                    raise _IllegalDataPoint
                mpq = q.to_mpmath()
                if mpq is None:
                    raise _IllegalDataPoint
                items.append(q)
                yield mpq

        def convert_vectors(p):
            d = None
            for q in p:
                if q.get_head_name() != "System`List":
                    raise _IllegalDataPoint
                v = list(convert_scalars(q.leaves))
                if d is None:
                    d = len(v)
                elif len(v) != d:
                    raise _IllegalDataPoint
                yield v

        if dist_p[0].is_numeric():
            numeric_p = [[x] for x in convert_scalars(dist_p)]
        else:
            numeric_p = list(convert_vectors(dist_p))

        # compute epsilon similar to Real.__eq__, such that "numbers that differ in their last seven binary digits
        # are considered equal"

        prec = min_prec(*items) or machine_precision
        eps = 0.5 ** (prec - 7)

        return kmeans(numeric_p, repr_p, py_k, mode, py_seed, eps)


class FindClusters(_Cluster):
    """
    <dl>
    <dt>'FindClusters[$list$]'
        <dd>returns a list of clusters formed from the elements of $list$. The number of cluster is determined
        automatically.
    <dt>'FindClusters[$list$, $k$]'
        <dd>returns a list of $k$ clusters formed from the elements of $list$.
    </dl>

    >> FindClusters[{1, 2, 20, 10, 11, 40, 19, 42}]
     = {{1, 2, 20, 10, 11, 19}, {40, 42}}

    >> FindClusters[{25, 100, 17, 20}]
     = {{25, 17, 20}, {100}}

    >> FindClusters[{3, 6, 1, 100, 20, 5, 25, 17, -10, 2}]
     = {{3, 6, 1, 5, -10, 2}, {100}, {20, 25, 17}}

    >> FindClusters[{1, 2, 10, 11, 20, 21}]
     = {{1, 2}, {10, 11}, {20, 21}}

    >> FindClusters[{1, 2, 10, 11, 20, 21}, 2]
     = {{1, 2, 10, 11}, {20, 21}}

    >> FindClusters[{1 -> a, 2 -> b, 10 -> c}]
     = {{a, b}, {c}}

    >> FindClusters[{1, 2, 5} -> {a, b, c}]
     = {{a, b}, {c}}

    >> FindClusters[{1, 2, 3, 1, 2, 10, 100}, Method -> "Agglomerate"]
     = {{1, 2, 3, 1, 2, 10}, {100}}

    >> FindClusters[{1, 2, 3, 10, 17, 18}, Method -> "Agglomerate"]
     = {{1, 2, 3}, {10}, {17, 18}}

    >> FindClusters[{{1}, {5, 6}, {7}, {2, 4}}, DistanceFunction -> (Abs[Length[#1] - Length[#2]]&)]
     = {{{1}, {7}}, {{5, 6}, {2, 4}}}

    >> FindClusters[{"meep", "heap", "deep", "weep", "sheep", "leap", "keep"}, 3]
     = {{meep, deep, weep, keep}, {heap, leap}, {sheep}}

    FindClusters' automatic distance function detection supports scalars, numeric tensors, boolean vectors and
    strings.

    The Method option must be either "Agglomerate" or "Optimize". If not specified, it defaults to "Optimize".
    Note that the Agglomerate and Optimize methods usually produce different clusterings.

    The runtime of the Agglomerate method is quadratic in the number of clustered points n, builds the clustering
    from the bottom up, and is exact (no element of randomness). The Optimize method's runtime is linear in n,
    Optimize builds the clustering from top down, and uses random sampling.
    """

    def apply(self, p, evaluation, options):
        "FindClusters[p_, OptionsPattern[%(name)s]]"
        return self._cluster(
            p,
            None,
            "clusters",
            evaluation,
            options,
            Expression("FindClusters", p, *options_to_rules(options)),
        )

    def apply_manual_k(self, p, k, evaluation, options):
        "FindClusters[p_, k_Integer, OptionsPattern[%(name)s]]"
        return self._cluster(
            p,
            k,
            "clusters",
            evaluation,
            options,
            Expression("FindClusters", p, k, *options_to_rules(options)),
        )


class ClusteringComponents(_Cluster):
    """
    <dl>
    <dt>'ClusteringComponents[$list$]'
        <dd>forms clusters from $list$ and returns a list of cluster indices, in which each
        element shows the index of the cluster in which the corresponding element in $list$
        ended up.
    <dt>'ClusteringComponents[$list$, $k$]'
        <dd>forms $k$ clusters from $list$ and returns a list of cluster indices, in which
        each element shows the index of the cluster in which the corresponding element in
        $list$ ended up.
    </dl>

    For more detailed documentation regarding options and behavior, see FindClusters[].

    >> ClusteringComponents[{1, 2, 3, 1, 2, 10, 100}]
     = {1, 1, 1, 1, 1, 1, 2}

    >> ClusteringComponents[{10, 100, 20}, Method -> "KMeans"]
     = {1, 0, 1}
    """

    def apply(self, p, evaluation, options):
        "ClusteringComponents[p_, OptionsPattern[%(name)s]]"
        return self._cluster(
            p,
            None,
            "components",
            evaluation,
            options,
            Expression("ClusteringComponents", p, *options_to_rules(options)),
        )

    def apply_manual_k(self, p, k, evaluation, options):
        "ClusteringComponents[p_, k_Integer, OptionsPattern[%(name)s]]"
        return self._cluster(
            p,
            k,
            "components",
            evaluation,
            options,
            Expression("ClusteringComponents", p, k, *options_to_rules(options)),
        )


class Nearest(Builtin):
    """
    <dl>
    <dt>'Nearest[$list$, $x$]'
        <dd>returns the one item in $list$ that is nearest to $x$.
    <dt>'Nearest[$list$, $x$, $n$]'
        <dd>returns the $n$ nearest items.
    <dt>'Nearest[$list$, $x$, {$n$, $r$}]'
        <dd>returns up to $n$ nearest items that are not farther from $x$ than $r$.
    <dt>'Nearest[{$p1$ -> $q1$, $p2$ -> $q2$, ...}, $x$]'
        <dd>returns $q1$, $q2$, ... but measures the distances using $p1$, $p2$, ...
    <dt>'Nearest[{$p1$, $p2$, ...} -> {$q1$, $q2$, ...}, $x$]'
        <dd>returns $q1$, $q2$, ... but measures the distances using $p1$, $p2$, ...
    </dl>

    >> Nearest[{5, 2.5, 10, 11, 15, 8.5, 14}, 12]
     = {11}

    Return all items within a distance of 5:

    >> Nearest[{5, 2.5, 10, 11, 15, 8.5, 14}, 12, {All, 5}]
     = {11, 10, 14}

    >> Nearest[{Blue -> "blue", White -> "white", Red -> "red", Green -> "green"}, {Orange, Gray}]
     = {{red}, {white}}

    >> Nearest[{{0, 1}, {1, 2}, {2, 3}} -> {a, b, c}, {1.1, 2}]
     = {b}
    """

    options = {
        "DistanceFunction": "Automatic",
        "Method": '"Scan"',
    }

    messages = {
        "amtd": "`1` failed to pick a suitable distance function for `2`.",
        "list": "Expected a list or a rule with equally sized lists at position 1 in ``.",
        "nimp": "Method `1` is not implemented yet.",
    }

    rules = {
        "Nearest[list_, pattern_]": "Nearest[list, pattern, 1]",
        "Nearest[pattern_][list_]": "Nearest[list, pattern]",
    }

    def apply(self, items, pivot, limit, expression, evaluation, options):
        "Nearest[items_, pivot_, limit_, OptionsPattern[%(name)s]]"

        method = self.get_option(options, "Method", evaluation)
        if not isinstance(method, String) or method.get_string_value() != "Scan":
            evaluation("Nearest", "nimp", method)
            return

        dist_p, repr_p = _dist_repr(items)

        if dist_p is None or len(dist_p) != len(repr_p):
            evaluation.message(self.get_name(), "list", expression)
            return

        if limit.has_form("List", 2):
            up_to = limit.leaves[0]
            py_r = limit.leaves[1].to_mpmath()
        else:
            up_to = limit
            py_r = None

        if isinstance(up_to, Integer):
            py_n = up_to.get_int_value()
        elif up_to.get_name() == "System`All":
            py_n = None
        else:
            return

        if not dist_p or (py_n is not None and py_n < 1):
            return Expression(SymbolList)

        multiple_x = False

        distance_function_string, distance_function = self.get_option_string(
            options, "DistanceFunction", evaluation
        )
        if distance_function_string == "Automatic":
            from mathics.builtin.tensors import get_default_distance

            distance_function = get_default_distance(dist_p)
            if distance_function is None:
                evaluation.message(
                    self.get_name(), "amtd", "Nearest", Expression(SymbolList, *dist_p)
                )
                return

            if pivot.get_head_name() == "System`List":
                _, depth_x = walk_levels(pivot)
                _, depth_items = walk_levels(dist_p[0])

                if depth_x > depth_items:
                    multiple_x = True

        def nearest(x):
            calls = [Expression(distance_function, x, y) for y in dist_p]
            distances = Expression(SymbolList, *calls).evaluate(evaluation)

            if not distances.has_form("List", len(dist_p)):
                raise ValueError()

            py_distances = [
                (_to_real_distance(d), i) for i, d in enumerate(distances.leaves)
            ]

            if py_r is not None:
                py_distances = [(d, i) for d, i in py_distances if d <= py_r]

            def pick():
                if py_n is None:
                    candidates = sorted(py_distances)
                else:
                    candidates = heapq.nsmallest(py_n, py_distances)

                for d, i in candidates:
                    yield repr_p[i]

            return Expression(SymbolList, *list(pick()))

        try:
            if not multiple_x:
                return nearest(pivot)
            else:
                return Expression(SymbolList, *[nearest(t) for t in pivot.leaves])
        except _IllegalDistance:
            return SymbolFailed
        except ValueError:
            return SymbolFailed


class Permutations(Builtin):
    """
    <dl>
    <dt>'Permutations[$list$]'
        <dd>gives all possible orderings of the items in $list$.
    <dt>'Permutations[$list$, $n$]'
        <dd>gives permutations up to length $n$.
    <dt>'Permutations[$list$, {$n$}]'
        <dd>gives permutations of length $n$.
    </dl>

    >> Permutations[{y, 1, x}]
     = {{y, 1, x}, {y, x, 1}, {1, y, x}, {1, x, y}, {x, y, 1}, {x, 1, y}}

    Elements are differentiated by their position in $list$, not their value.

    >> Permutations[{a, b, b}]
     = {{a, b, b}, {a, b, b}, {b, a, b}, {b, b, a}, {b, a, b}, {b, b, a}}

    >> Permutations[{1, 2, 3}, 2]
     = {{}, {1}, {2}, {3}, {1, 2}, {1, 3}, {2, 1}, {2, 3}, {3, 1}, {3, 2}}

    >> Permutations[{1, 2, 3}, {2}]
     = {{1, 2}, {1, 3}, {2, 1}, {2, 3}, {3, 1}, {3, 2}}
    """

    messages = {
        "argt": "Permutation expects at least one argument.",
        "nninfseq": "The number specified at position 2 of `` must be a non-negative integer, All, or Infinity.",
    }

    def apply_argt(self, evaluation):
        "Permutations[]"
        evaluation.message(self.get_name(), "argt")

    def apply(self, l, evaluation):
        "Permutations[l_List]"
        return Expression(
            "List",
            *[
                Expression(SymbolList, *p)
                for p in permutations(l.leaves, len(l.leaves))
            ],
        )

    def apply_n(self, l, n, evaluation):
        "Permutations[l_List, n_]"

        rs = None
        if isinstance(n, Integer):
            py_n = min(n.get_int_value(), len(l.leaves))
        elif n.has_form("List", 1) and isinstance(n.leaves[0], Integer):
            py_n = n.leaves[0].get_int_value()
            rs = (py_n,)
        elif (
            n.has_form("DirectedInfinity", 1) and n.leaves[0].get_int_value() == 1
        ) or n.get_name() == "System`All":
            py_n = len(l.leaves)
        else:
            py_n = None

        if py_n is None or py_n < 0:
            evaluation.message(
                self.get_name(), "nninfseq", Expression(self.get_name(), l, n)
            )
            return

        if rs is None:
            rs = range(py_n + 1)

        inner = structure("List", l, evaluation)
        outer = structure("List", inner, evaluation)

        return outer([inner(p) for r in rs for p in permutations(l.leaves, r)])


class SubsetQ(Builtin):
    """
    <dl>
    <dt>'SubsetQ[$list1$, $list2$]'
        <dd>returns True if $list2$ is a subset of $list1$, and False otherwise.
    </dl>

    >> SubsetQ[{1, 2, 3}, {3, 1}]
     = True

    The empty list is a subset of every list:
    >> SubsetQ[{}, {}]
     = True

    >> SubsetQ[{1, 2, 3}, {}]
     = True

    Every list is a subset of itself:
    >> SubsetQ[{1, 2, 3}, {1, 2, 3}]
     = True

    #> SubsetQ[{1, 2, 3}, {0, 1}]
     = False

    #> SubsetQ[{1, 2, 3}, {1, 2, 3, 4}]
     = False

    #> SubsetQ[{1, 2, 3}]
     : SubsetQ called with 1 argument; 2 arguments are expected.
     = SubsetQ[{1, 2, 3}]

    #> SubsetQ[{1, 2, 3}, {1, 2}, {3}]
     : SubsetQ called with 3 arguments; 2 arguments are expected.
     = SubsetQ[{1, 2, 3}, {1, 2}, {3}]

    #> SubsetQ[a + b + c, {1}]
     : Heads Plus and List at positions 1 and 2 are expected to be the same.
     = SubsetQ[a + b + c, {1}]

    #> SubsetQ[{1, 2, 3}, n]
     : Nonatomic expression expected at position 2 in SubsetQ[{1, 2, 3}, n].
     = SubsetQ[{1, 2, 3}, n]

    #> SubsetQ[f[a, b, c], f[a]]
     = True
    """

    messages = {
        "argr": "SubsetQ called with 1 argument; 2 arguments are expected.",
        "argrx": "SubsetQ called with `1` arguments; 2 arguments are expected.",
        "heads": "Heads `1` and `2` at positions 1 and 2 are expected to be the same.",
        "normal": "Nonatomic expression expected at position `1` in `2`.",
    }

    def apply(self, expr, subset, evaluation):
        "SubsetQ[expr_, subset___]"

        if expr.is_atom():
            return evaluation.message(
                "SubsetQ", "normal", Integer(1), Expression("SubsetQ", expr, subset)
            )

        subset = subset.get_sequence()
        if len(subset) > 1:
            return evaluation.message("SubsetQ", "argrx", Integer(len(subset) + 1))
        elif len(subset) == 0:
            return evaluation.message("SubsetQ", "argr")

        subset = subset[0]
        if subset.is_atom():
            return evaluation.message(
                "SubsetQ", "normal", Integer(2), Expression("SubsetQ", expr, subset)
            )
        if expr.get_head_name() != subset.get_head_name():
            return evaluation.message(
                "SubsetQ", "heads", expr.get_head(), subset.get_head()
            )

        if set(subset.leaves).issubset(set(expr.leaves)):
            return Symbol("True")
        else:
            return Symbol("False")


def delete_one(expr, pos):
    if expr.is_atom():
        raise PartDepthError(pos)
    leaves = expr.leaves
    if pos == 0:
        return Expression(Symbol("System`Sequence"), *leaves)
    l = len(leaves)
    truepos = pos
    if truepos < 0:
        truepos = l + truepos
    else:
        truepos = truepos - 1
    if truepos < 0 or truepos >= l:
        raise PartRangeError
    leaves = leaves[:truepos] + (Expression("System`Sequence"),) + leaves[truepos + 1 :]
    return Expression(expr.get_head(), *leaves)


def delete_rec(expr, pos):
    if len(pos) == 1:
        return delete_one(expr, pos[0])
    truepos = pos[0]
    if truepos == 0 or expr.is_atom():
        raise PartDepthError(pos[0])
    leaves = expr.leaves
    l = len(leaves)
    if truepos < 0:
        truepos = truepos + l
        if truepos < 0:
            raise PartRangeError
        newleaf = delete_rec(leaves[truepos], pos[1:])
        leaves = leaves[:truepos] + (newleaf,) + leaves[truepos + 1 :]
    else:
        if truepos > l:
            raise PartRangeError
        newleaf = delete_rec(leaves[truepos - 1], pos[1:])
        leaves = leaves[: truepos - 1] + (newleaf,) + leaves[truepos:]
    return Expression(expr.get_head(), *leaves)


class Delete(Builtin):
    """
    <dl>
    <dt>'Delete[$expr$, $i$]'
        <dd>deletes the element at position $i$ in $expr$. The position is counted from the end if $i$ is negative.
    <dt>'Delete[$expr$, {$m$, $n$, ...}]'
        <dd>deletes the element at position {$m$, $n$, ...}.
    <dt>'Delete[$expr$, {{$m1$, $n1$, ...}, {$m2$, $n2$, ...}, ...}]'
        <dd>deletes the elements at several positions.
    </dl>

    Delete the element at position 3:
    >> Delete[{a, b, c, d}, 3]
     = {a, b, d}

    Delete at position 2 from the end:
    >> Delete[{a, b, c, d}, -2]
     = {a, b, d}

    Delete at positions 1 and 3:
    >> Delete[{a, b, c, d}, {{1}, {3}}]
     = {b, d}

    Delete in a 2D array:
    >> Delete[{{a, b}, {c, d}}, {2, 1}]
     = {{a, b}, {d}}

    Deleting the head of a whole expression gives a Sequence object:
    >> Delete[{a, b, c}, 0]
     = Sequence[a, b, c]

    Delete in an expression with any head:
    >> Delete[f[a, b, c, d], 3]
     = f[a, b, d]

    Delete a head to splice in its arguments:
    >> Delete[f[a, b, u + v, c], {3, 0}]
     = f[a, b, u, v, c]

    >> Delete[{a, b, c}, 0]
     = Sequence[a, b, c]

    #> Delete[1 + x ^ (a + b + c), {2, 2, 3}]
     = 1 + x ^ (a + b)

    #> Delete[f[a, g[b, c], d], {{2}, {2, 1}}]
     = f[a, d]

    #> Delete[f[a, g[b, c], d], m + n]
     : The expression m + n cannot be used as a part specification. Use Key[m + n] instead.
     = Delete[f[a, g[b, c], d], m + n]

    Delete without the position:
    >> Delete[{a, b, c, d}]
     : Delete called with 1 argument; 2 arguments are expected.
     = Delete[{a, b, c, d}]

    Delete with many arguments:
    >> Delete[{a, b, c, d}, 1, 2]
     : Delete called with 3 arguments; 2 arguments are expected.
     = Delete[{a, b, c, d}, 1, 2]

    Delete the element out of range:
    >> Delete[{a, b, c, d}, 5]
     : Part {5} of {a, b, c, d} does not exist.
     = Delete[{a, b, c, d}, 5]

    #> Delete[{a, b, c, d}, {1, 2}]
     : Part 2 of {a, b, c, d} does not exist.
     = Delete[{a, b, c, d}, {1, 2}]

    Delete the position not integer:
    >> Delete[{a, b, c, d}, {1, n}]
     : Position specification n in {a, b, c, d} is not a machine-sized integer or a list of machine-sized integers.
     = Delete[{a, b, c, d}, {1, n}]

    #> Delete[{a, b, c, d}, {{1}, n}]
     : Position specification {n, {1}} in {a, b, c, d} is not a machine-sized integer or a list of machine-sized integers.
     = Delete[{a, b, c, d}, {{1}, n}]

    #> Delete[{a, b, c, d}, {{1}, {n}}]
     : Position specification n in {a, b, c, d} is not a machine-sized integer or a list of machine-sized integers.
     = Delete[{a, b, c, d}, {{1}, {n}}]
    """

    messages = {
        "argr": "Delete called with 1 argument; 2 arguments are expected.",
        "argt": "Delete called with `1` arguments; 2 arguments are expected.",
        "psl": "Position specification `1` in `2` is not a machine-sized integer or a list of machine-sized integers.",
        "pkspec": "The expression `1` cannot be used as a part specification. Use `2` instead.",
    }

    def apply_one(self, expr, position, evaluation):
        "Delete[expr_, position_Integer]"
        pos = position.get_int_value()
        try:
            return delete_one(expr, pos)
        except PartRangeError:
            evaluation.message("Part", "partw", Expression(SymbolList, pos), expr)

    def apply(self, expr, positions, evaluation):
        "Delete[expr_, positions___]"
        positions = positions.get_sequence()
        if len(positions) > 1:
            return evaluation.message("Delete", "argt", Integer(len(positions) + 1))
        elif len(positions) == 0:
            return evaluation.message("Delete", "argr")

        positions = positions[0]
        if not positions.has_form("List", None):
            return evaluation.message(
                "Delete", "pkspec", positions, Expression("Key", positions)
            )

        # Create new python list of the positions and sort it
        positions = (
            [l for l in positions.leaves]
            if positions.leaves[0].has_form("List", None)
            else [positions]
        )
        positions.sort(key=lambda e: e.get_sort_key(pattern_sort=True))
        leaves = expr.leaves
        newexpr = expr
        for position in positions:
            pos = [p.get_int_value() for p in position.get_leaves()]
            if None in pos:
                return evaluation.message(
                    "Delete", "psl", position.leaves[pos.index(None)], expr
                )
            if len(pos) == 0:
                return evaluation.message(
                    "Delete", "psl", Expression(SymbolList, *positions), expr
                )
            try:
                newexpr = delete_rec(newexpr, pos)
            except PartDepthError as exc:
                return evaluation.message("Part", "partw", Integer(exc.index), expr)
            except PartError:
                return evaluation.message(
                    "Part", "partw", Expression(SymbolList, *pos), expr
                )
        return newexpr


class Association(Builtin):
    """
    <dl>
    <dt>'Association[$key1$ -> $val1$, $key2$ -> $val2$, ...]'
    <dt>'<|$key1$ -> $val1$, $key2$ -> $val2$, ...|>'
        <dd> represents an association between keys and values.
    </dl>

    'Association' is the head of associations:
    >> Head[<|a -> x, b -> y, c -> z|>]
     = Association

    >> <|a -> x, b -> y|>
     = <|a -> x, b -> y|>

    >> Association[{a -> x, b -> y}]
     = <|a -> x, b -> y|>

    Associations can be nested:
    >> <|a -> x, b -> y, <|a -> z, d -> t|>|>
     = <|a -> z, b -> y, d -> t|>

    #> <|a -> x, b -> y, c -> <|d -> t|>|>
     = <|a -> x, b -> y, c -> <|d -> t|>|>
    #> %["s"]
     = Missing[KeyAbsent, s]

    #> <|a -> x, b + c -> y, {<|{}|>, a -> {z}}|>
     = <|a -> {z}, b + c -> y|>
    #> %[a]
     = {z}

    #> <|"x" -> 1, {y} -> 1|>
     = <|x -> 1, {y} -> 1|>
    #> %["x"]
     = 1

    #> <|<|a -> v|> -> x, <|b -> y, a -> <|c -> z|>, {}, <||>|>, {d}|>[c]
     =  Association[Association[a -> v] -> x, Association[b -> y, a -> Association[c -> z], {}, Association[]], {d}][c]

    #> <|<|a -> v|> -> x, <|b -> y, a -> <|c -> z|>, {d}|>, {}, <||>|>[a]
     = Association[Association[a -> v] -> x, Association[b -> y, a -> Association[c -> z], {d}], {}, Association[]][a]

    #> <|<|a -> v|> -> x, <|b -> y, a -> <|c -> z, {d}|>, {}, <||>|>, {}, <||>|>
     = <|<|a -> v|> -> x, b -> y, a -> Association[c -> z, {d}]|>
    #> %[a]
     = Association[c -> z, {d}]

    #> <|a -> x, b -> y, c -> <|d -> t|>|> // ToBoxes
     = RowBox[{<|, RowBox[{RowBox[{a, ->, x}], ,, RowBox[{b, ->, y}], ,, RowBox[{c, ->, RowBox[{<|, RowBox[{d, ->, t}], |>}]}]}], |>}]

    #> Association[a -> x, b -> y, c -> Association[d -> t, Association[e -> u]]] // ToBoxes
     = RowBox[{<|, RowBox[{RowBox[{a, ->, x}], ,, RowBox[{b, ->, y}], ,, RowBox[{c, ->, RowBox[{<|, RowBox[{RowBox[{d, ->, t}], ,, RowBox[{e, ->, u}]}], |>}]}]}], |>}]
    """

    error_idx = 0

    attributes = (
        "HoldAllComplete",
        "Protected",
    )

    def apply_makeboxes(self, rules, f, evaluation):
        """MakeBoxes[<|rules___|>,
        f:StandardForm|TraditionalForm|OutputForm|InputForm]"""

        def validate(exprs):
            for expr in exprs:
                if expr.has_form(("Rule", "RuleDelayed"), 2):
                    pass
                elif expr.has_form("List", None) or expr.has_form("Association", None):
                    if validate(expr.leaves) is not True:
                        return False
                else:
                    return False
            return True

        rules = rules.get_sequence()
        if self.error_idx == 0 and validate(rules) is True:
            expr = Expression(
                "RowBox", Expression(SymbolList, *list_boxes(rules, f, "<|", "|>"))
            )
        else:
            self.error_idx += 1
            symbol = Expression(SymbolMakeBoxes, SymbolAssociation, f)
            expr = Expression(
                "RowBox",
                Expression(SymbolList, symbol, *list_boxes(rules, f, "[", "]")),
            )

        expr = expr.evaluate(evaluation)
        if self.error_idx > 0:
            self.error_idx -= 1
        return expr

    def apply(self, rules, evaluation):
        "Association[rules__]"

        def make_flatten(exprs, dic={}, keys=[]):
            for expr in exprs:
                if expr.has_form(("Rule", "RuleDelayed"), 2):
                    key = expr.leaves[0].evaluate(evaluation)
                    value = expr.leaves[1].evaluate(evaluation)
                    dic[key] = Expression(expr.get_head(), key, value)
                    if key not in keys:
                        keys.append(key)
                elif expr.has_form("List", None) or expr.has_form("Association", None):
                    make_flatten(expr.leaves, dic, keys)
                else:
                    raise
            return [dic[key] for key in keys]

        try:
            return Expression(SymbolAssociation, *make_flatten(rules.get_sequence()))
        except:
            return None

    def apply_key(self, rules, key, evaluation):
        "Association[rules__][key_]"

        def find_key(exprs, dic={}):
            for expr in exprs:
                if expr.has_form(("Rule", "RuleDelayed"), 2):
                    if expr.leaves[0] == key:
                        dic[key] = expr.leaves[1]
                elif expr.has_form("List", None) or expr.has_form("Association", None):
                    find_key(expr.leaves)
                else:
                    raise
            return dic

        try:
            result = find_key(rules.get_sequence())
        except:
            return None

        return (
            result[key] if result else Expression("Missing", Symbol("KeyAbsent"), key)
        )


class AssociationQ(Test):
    """
    <dl>
    <dt>'AssociationQ[$expr$]'
        <dd>return True if $expr$ is a valid Association object, and False otherwise.
    </dl>

    >> AssociationQ[<|a -> 1, b :> 2|>]
     = True

    >> AssociationQ[<|a, b|>]
     = False
    """

    def test(self, expr):
        def validate(leaves):
            for leaf in leaves:
                if leaf.has_form(("Rule", "RuleDelayed"), 2):
                    pass
                elif leaf.has_form("List", None) or leaf.has_form("Association", None):
                    if validate(leaf.leaves) is not True:
                        return False
                else:
                    return False
            return True

        return expr.get_head_name() == "System`Association" and validate(expr.leaves)


class Keys(Builtin):
    """
    <dl>
    <dt>'Keys[<|$key1$ -> $val1$, $key2$ -> $val2$, ...|>]'
        <dd>return a list of the keys $keyi$ in an association.
    <dt>'Keys[{$key1$ -> $val1$, $key2$ -> $val2$, ...}]'
        <dd>return a list of the $keyi$ in a list of rules.
    </dl>

    >> Keys[<|a -> x, b -> y|>]
     = {a, b}

    >> Keys[{a -> x, b -> y}]
     = {a, b}

    Keys automatically threads over lists:
    >> Keys[{<|a -> x, b -> y|>, {w -> z, {}}}]
     = {{a, b}, {w, {}}}

    Keys are listed in the order of their appearance:
    >> Keys[{c -> z, b -> y, a -> x}]
     = {c, b, a}

    #> Keys[a -> x]
     = a

    #> Keys[{a -> x, a -> y, {a -> z, <|b -> t|>, <||>, {}}}]
     = {a, a, {a, {b}, {}, {}}}

    #> Keys[{a -> x, a -> y, <|a -> z, {b -> t}, <||>, {}|>}]
     = {a, a, {a, b}}

    #> Keys[<|a -> x, a -> y, <|a -> z, <|b -> t|>, <||>, {}|>|>]
     = {a, b}

    #> Keys[<|a -> x, a -> y, {a -> z, {b -> t}, <||>, {}}|>]
     = {a, b}

    #> Keys[<|a -> x, <|a -> y, b|>|>]
     : The argument Association[a -> x, Association[a -> y, b]] is not a valid Association or a list of rules.
     = Keys[Association[a -> x, Association[a -> y, b]]]

    #> Keys[<|a -> x, {a -> y, b}|>]
     : The argument Association[a -> x, {a -> y, b}] is not a valid Association or a list of rules.
     = Keys[Association[a -> x, {a -> y, b}]]

    #> Keys[{a -> x, <|a -> y, b|>}]
     : The argument Association[a -> y, b] is not a valid Association or a list of rules.
     = Keys[{a -> x, Association[a -> y, b]}]

    #> Keys[{a -> x, {a -> y, b}}]
     : The argument b is not a valid Association or a list of rules.
     = Keys[{a -> x, {a -> y, b}}]

    #> Keys[a -> x, b -> y]
     : Keys called with 2 arguments; 1 argument is expected.
     = Keys[a -> x, b -> y]
    """

    attributes = ("Protected",)

    messages = {
        "argx": "Keys called with `1` arguments; 1 argument is expected.",
        "invrl": "The argument `1` is not a valid Association or a list of rules.",
    }

    def apply(self, rules, evaluation):
        "Keys[rules___]"

        def get_keys(expr):
            if expr.has_form(("Rule", "RuleDelayed"), 2):
                return expr.leaves[0]
            elif expr.has_form("List", None) or (
                expr.has_form("Association", None)
                and AssociationQ(expr).evaluate(evaluation) == Symbol("True")
            ):
                return Expression(SymbolList, *[get_keys(leaf) for leaf in expr.leaves])
            else:
                evaluation.message("Keys", "invrl", expr)
                raise

        rules = rules.get_sequence()
        if len(rules) != 1:
            return evaluation.message("Keys", "argx", Integer(len(rules)))

        try:
            return get_keys(rules[0])
        except:
            return None


class Values(Builtin):
    """
    <dl>
    <dt>'Values[<|$key1$ -> $val1$, $key2$ -> $val2$, ...|>]'
        <dd>return a list of the values $vali$ in an association.
    <dt>'Values[{$key1$ -> $val1$, $key2$ -> $val2$, ...}]'
        <dd>return a list of the $vali$ in a list of rules.
    </dl>

    >> Values[<|a -> x, b -> y|>]
     = {x, y}

    >> Values[{a -> x, b -> y}]
     = {x, y}

    Values automatically threads over lists:
    >> Values[{<|a -> x, b -> y|>, {c -> z, {}}}]
     = {{x, y}, {z, {}}}

    Values are listed in the order of their appearance:
    >> Values[{c -> z, b -> y, a -> x}]
     = {z, y, x}

    #> Values[a -> x]
     = x

    #> Values[{a -> x, a -> y, {a -> z, <|b -> t|>, <||>, {}}}]
     = {x, y, {z, {t}, {}, {}}}

    #> Values[{a -> x, a -> y, <|a -> z, {b -> t}, <||>, {}|>}]
     = {x, y, {z, t}}

    #> Values[<|a -> x, a -> y, <|a -> z, <|b -> t|>, <||>, {}|>|>]
     = {z, t}

    #> Values[<|a -> x, a -> y, {a -> z, {b -> t}, <||>, {}}|>]
     = {z, t}

    #> Values[<|a -> x, <|a -> y, b|>|>]
     : The argument Association[a -> x, Association[a -> y, b]] is not a valid Association or a list of rules.
     = Values[Association[a -> x, Association[a -> y, b]]]

    #> Values[<|a -> x, {a -> y, b}|>]
     : The argument Association[a -> x, {a -> y, b}] is not a valid Association or a list of rules.
     = Values[Association[a -> x, {a -> y, b}]]

    #> Values[{a -> x, <|a -> y, b|>}]
     : The argument {a -> x, Association[a -> y, b]} is not a valid Association or a list of rules.
     = Values[{a -> x, Association[a -> y, b]}]

    #> Values[{a -> x, {a -> y, b}}]
     : The argument {a -> x, {a -> y, b}} is not a valid Association or a list of rules.
     = Values[{a -> x, {a -> y, b}}]

    #> Values[a -> x, b -> y]
     : Values called with 2 arguments; 1 argument is expected.
     = Values[a -> x, b -> y]
    """

    attributes = ("Protected",)

    messages = {
        "argx": "Values called with `1` arguments; 1 argument is expected.",
        "invrl": "The argument `1` is not a valid Association or a list of rules.",
    }

    def apply(self, rules, evaluation):
        "Values[rules___]"

        def get_values(expr):
            if expr.has_form(("Rule", "RuleDelayed"), 2):
                return expr.leaves[1]
            elif expr.has_form("List", None) or (
                expr.has_form("Association", None)
                and AssociationQ(expr).evaluate(evaluation) == Symbol("True")
            ):
                return Expression(
                    SymbolList, *[get_values(leaf) for leaf in expr.leaves]
                )
            else:
                raise

        rules = rules.get_sequence()
        if len(rules) != 1:
            return evaluation.message("Values", "argx", Integer(len(rules)))

        try:
            return get_values(rules[0])
        except:
            return evaluation.message("Values", "invrl", rules[0])


class ContainsOnly(Builtin):
    """
    <dl>
    <dt>'ContainsOnly[$list1$, $list2$]'
        <dd>yields True if $list1$ contains only elements that appear in $list2$.
    </dl>

    >> ContainsOnly[{b, a, a}, {a, b, c}]
     = True

    The first list contains elements not present in the second list:
    >> ContainsOnly[{b, a, d}, {a, b, c}]
     = False

    >> ContainsOnly[{}, {a, b, c}]
     = True

    #> ContainsOnly[1, {1, 2, 3}]
     : List or association expected instead of 1.
     = ContainsOnly[1, {1, 2, 3}]

    #> ContainsOnly[{1, 2, 3}, 4]
     : List or association expected instead of 4.
     = ContainsOnly[{1, 2, 3}, 4]

    Use Equal as the comparison function to have numerical tolerance:
    >> ContainsOnly[{a, 1.0}, {1, a, b}, {SameTest -> Equal}]
     = True

    #> ContainsOnly[{c, a}, {a, b, c}, IgnoreCase -> True]
     : Unknown option IgnoreCase -> True in ContainsOnly.
     : Unknown option IgnoreCase in .
     = True
    """

    attributes = ("ReadProtected",)

    messages = {
        "lsa": "List or association expected instead of `1`.",
        "nodef": "Unknown option `1` for ContainsOnly.",
        "optx": "Unknown option `1` in `2`.",
    }

    options = {
        "SameTest": "SameQ",
    }

    def check_options(self, expr, evaluation, options):
        for key in options:
            if key != "System`SameTest":
                if expr is None:
                    evaluation.message("ContainsOnly", "optx", Symbol(key))
                else:
                    return evaluation.message("ContainsOnly", "optx", Symbol(key), expr)
        return None

    def apply(self, list1, list2, evaluation, options={}):
        "ContainsOnly[list1_?ListQ, list2_?ListQ, OptionsPattern[ContainsOnly]]"

        same_test = self.get_option(options, "SameTest", evaluation)

        def same(a, b):
            result = Expression(same_test, a, b).evaluate(evaluation)
            return result.is_true()

        self.check_options(None, evaluation, options)
        for a in list1.leaves:
            if not any(same(a, b) for b in list2.leaves):
                return Symbol("False")
        return Symbol("True")

    def apply_msg(self, e1, e2, evaluation, options={}):
        "ContainsOnly[e1_, e2_, OptionsPattern[ContainsOnly]]"

        opts = (
            options_to_rules(options)
            if len(options) <= 1
            else [Expression(SymbolList, *options_to_rules(options))]
        )
        expr = Expression("ContainsOnly", e1, e2, *opts)

        if not isinstance(e1, Symbol) and not e1.has_form("List", None):
            evaluation.message("ContainsOnly", "lsa", e1)
            return self.check_options(expr, evaluation, options)

        if not isinstance(e2, Symbol) and not e2.has_form("List", None):
            evaluation.message("ContainsOnly", "lsa", e2)
            return self.check_options(expr, evaluation, options)

        return self.check_options(expr, evaluation, options)


## From backports in CellsToTeX. This functions provides compatibility to WMA 10.
##  TODO:
##  * Add doctests
##  * Translate to python the more complex rules
##  * Complete the support.


class Key(Builtin):
    """
    <dl>
    <dt>Key[$key$]
        <dd> represents a key used to access a value in an association.
    <dt>Key[$key$][$assoc$]
        <dd>
    </dl>
    """

    rules = {
        "Key[key_][assoc_Association]": "assoc[key]",
    }


class Lookup(Builtin):
    """
    <dl>
    <dt>Lookup[$assoc$, $key$]
        <dd> looks up the value associated with $key$ in the association $assoc$, or Missing[$KeyAbsent$].
    </dl>
    """

    attributes = "HoldAllComplete"
    rules = {
        "Lookup[assoc_?AssociationQ, key_, default_]": "FirstCase[assoc, _[Verbatim[key], val_] :> val, default]",
        "Lookup[assoc_?AssociationQ, key_]": 'Lookup[assoc, key, Missing["KeyAbsent", key]]',
    }


class Failure(Builtin):
    """
    <dl>
    <dt>Failure[$tag$, $assoc$]
        <dd> represents a failure of a type indicated by $tag$, with details given by the association $assoc$.
    </dl>
    """

    pass


#    rules = {'Failure /: MakeBoxes[Failure[tag_, assoc_Association], StandardForm]' :
# 		'With[{msg = assoc["MessageTemplate"], msgParam = assoc["MessageParameters"], type = assoc["Type"]}, ToBoxes @ Interpretation["Failure" @ Panel @ Grid[{{Style["\[WarningSign]", "Message", FontSize -> 35], Style["Message:", FontColor->GrayLevel[0.5]], ToString[StringForm[msg, Sequence @@ msgParam], StandardForm]}, {SpanFromAbove, Style["Tag:", FontColor->GrayLevel[0.5]], ToString[tag, StandardForm]},{SpanFromAbove,Style["Type:", FontColor->GrayLevel[0.5]],ToString[type, StandardForm]}},Alignment -> {Left, Top}], Failure[tag, assoc]] /; msg =!= Missing["KeyAbsent", "MessageTemplate"] && msgParam =!= Missing["KeyAbsent", "MessageParameters"] && msgParam =!= Missing["KeyAbsent", "Type"]]',
#     }


class FirstCase(Builtin):
    """
    <dl>
    <dt> FirstCase[{$e1$, $e2$, ...}, $pattern$]
        <dd>gives the first $ei$ to match $pattern$, or $Missing[\"NotFound\"]$ if none matching pattern is found.

    <dt> FirstCase[{$e1$,$e2$, ...}, $pattern$ -> $rhs$]
        <dd> gives the value of $rhs$ corresponding to the first $ei$ to match pattern.
    <dt> FirstCase[$expr$, $pattern$, $default$]
         <dd> gives $default$ if no element matching $pattern$ is found.

    <dt>FirstCase[$expr$, $pattern$, $default$, $levelspec$] \
         <dd>finds only objects that appear on levels specified by $levelspec$.

    <dt>FirstCase[$pattern$]
        <dd>represents an operator form of FirstCase that can be applied to an expression.
    </dl>


    """

    attributes = "HoldRest"
    options = Cases.options
    rules = {
        'FirstCase[expr_, pattOrRule_, Shortest[default_:Missing["NotFound"], 1],Shortest[levelspec_:{1}, 2], opts:OptionsPattern[]]': "Replace[Cases[expr, pattOrRule, levelspec, 1, opts],{{} :> default, {match_} :> match}]",
        "FirstCase[pattOrRule_][expr_]": "FirstCase[expr, pattOrRule]",
    }
