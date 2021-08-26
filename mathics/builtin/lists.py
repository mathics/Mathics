# -*- coding: utf-8 -*-

"""
List Functions - Miscellaneous
"""

import heapq
import sympy

from collections import defaultdict
from itertools import chain

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.algorithm.introselect import introselect

from mathics.algorithm.clusters import (
    AutomaticMergeCriterion,
    AutomaticSplitCriterion,
    LazyDistances,
    PrecomputedDistances,
    agglomerate,
    kmeans,
    optimize,
)

from mathics.builtin.base import (
    Builtin,
    CountableInteger,
    InvalidLevelspecError,
    MessageException,
    NegativeIntegerException,
    PartDepthError,
    PartError,
    PartRangeError,
    Predefined,
    SympyFunction,
    Test,
)

from mathics.builtin.numbers.algebra import cancel
from mathics.builtin.options import options_to_rules
from mathics.builtin.scoping import dynamic_scoping

from mathics.core.convert import from_sympy
from mathics.core.evaluation import BreakInterrupt, ContinueInterrupt, ReturnInterrupt
from mathics.core.expression import (
    ByteArrayAtom,
    Expression,
    Integer,
    Integer0,
    Number,
    Real,
    String,
    Symbol,
    SymbolByteArray,
    SymbolFailed,
    SymbolList,
    SymbolMakeBoxes,
    SymbolN,
    SymbolRule,
    SymbolSequence,
    from_python,
    machine_precision,
    min_prec,
    strip_context,
    structure,
)


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


class All(Predefined):
    """
    <dl>
      <dt>'All'
      <dd>is a possible option value for 'Span', 'Quiet', 'Part' and related functions. 'All' specifies all parts at a particular level.
    </dl>
    """

    pass


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

        def sameQ(a, b) -> bool:
            """Mathics SameQ"""
            result = Expression(same_test, a, b).evaluate(evaluation)
            return result.is_true()

        self.check_options(None, evaluation, options)
        for a in list1.leaves:
            if not any(sameQ(a, b) for b in list2.leaves):
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


class Failure(Builtin):
    """
    <dl>
    <dt>Failure[$tag$, $assoc$]
        <dd> represents a failure of a type indicated by $tag$, with details given by the association $assoc$.
    </dl>
    """

    pass


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


def riffle(items, sep):
    result = items[:1]
    for item in items[1:]:
        result.append(sep)
        result.append(item)
    return result


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


class None_(Predefined):
    """
    <dl>
    <dt>'None'
        <dd>is a possible value for 'Span' and 'Quiet'.
    </dl>
    """

    name = "None"


def join_lists(lists):
    new_list = []
    for list in lists:
        new_list.extend(list)
    return new_list


def get_part(varlist, indices):
    "Simple part extraction. indices must be a list of python integers."

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
    "Simple part replacement. indices must be a list of python integers."

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
            # FIXME: this should work as an iterator in Python3, not
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

            if not result.sameQ(whole_expr):
                return result
            return

        index = imin.evaluate(evaluation)
        imax = imax.evaluate(evaluation)
        di = di.evaluate(evaluation)

        result = []
        compare_type = (
            "GreaterEqual"
            if Expression("Less", di, Integer0).evaluate(evaluation).to_python()
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


def get_tuples(items):
    if not items:
        yield []
    else:
        for item in items[0]:
            for rest in get_tuples(items[1:]):
                yield [item] + rest


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
                return Integer0

        return Expression(SymbolList, *(item(i) for i in range(1, n + 1)))


def _test_pair(test, a, b, evaluation, name):
    test_expr = Expression(test, a, b)
    result = test_expr.evaluate(evaluation)
    if not (
        result.is_symbol() and (result.has_symbol("True") or result.has_symbol("False"))
    ):
        evaluation.message(name, "smtst", test_expr, result)
    return result.is_true()


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

    def sameQ(self, a, b) -> bool:
        """Mathics SameQ"""
        return a.sameQ(b)


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

    def sameQ(self, a, b) -> bool:
        """Mathics SameQ"""
        return _test_pair(self._test, a, b, self._evaluation, self._name)


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
            Integer0,
            Integer0,
            evaluation,
            lambda: Expression(self.get_name(), l, n),
        )

    def apply(self, l, n, x, evaluation):
        "%(name)s[l_, n_, x_]"
        return self._pad(
            l,
            n,
            x,
            Integer0,
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


#    rules = {'Failure /: MakeBoxes[Failure[tag_, assoc_Association], StandardForm]' :
# 		'With[{msg = assoc["MessageTemplate"], msgParam = assoc["MessageParameters"], type = assoc["Type"]}, ToBoxes @ Interpretation["Failure" @ Panel @ Grid[{{Style["\[WarningSign]", "Message", FontSize -> 35], Style["Message:", FontColor->GrayLevel[0.5]], ToString[StringForm[msg, Sequence @@ msgParam], StandardForm]}, {SpanFromAbove, Style["Tag:", FontColor->GrayLevel[0.5]], ToString[tag, StandardForm]},{SpanFromAbove,Style["Type:", FontColor->GrayLevel[0.5]],ToString[type, StandardForm]}},Alignment -> {Left, Top}], Failure[tag, assoc]] /; msg =!= Missing["KeyAbsent", "MessageTemplate"] && msgParam =!= Missing["KeyAbsent", "MessageParameters"] && msgParam =!= Missing["KeyAbsent", "Type"]]',
#     }
