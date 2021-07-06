# -*- coding: utf-8 -*-
"""
Rearranging and Restructuring Lists

These functions reorder and rearrange lists.
"""

import functools

from itertools import chain
from typing import Callable

from mathics.version import __version__  # noqa used in loading to check consistency.


from mathics.builtin.base import (
    Builtin,
    MessageException,
)

from mathics.builtin.lists import _FastEquivalence, _SlowEquivalence, _test_pair

from mathics.core.expression import (
    Expression,
    Integer,
    SymbolList,
    structure,
)


def _is_sameq(same_test):
    # System`SameQ is protected, so nobody should ever be able to change
    # it (see Set::wrsym). We just check for its name here thus.
    return same_test.is_symbol() and same_test.get_name() == "System`SameQ"


class _DeleteDuplicatesBin:
    def __init__(self, item):
        self._item = item
        self.add_to = lambda elem: None

    def from_python(self):
        return self._item


class _GatherBin:
    def __init__(self, item):
        self._items = [item]
        self.add_to = self._items.append

    def from_python(self):
        return Expression(SymbolList, *self._items)


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
                if equivalence.sameQ(prototype, key):
                    add_to_bin(value)  # add to existing bin
                    break
            else:
                new_bin = Bin(value)  # create new bin
                selection.append((key, new_bin.add_to))
                bins.append(new_bin)

        return Expression(SymbolList, *[b.from_python() for b in bins])


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
        operands = [li.leaves for li in seq]
        if not _is_sameq(same_test):

            def sameQ(a, b):
                return _test_pair(same_test, a, b, evaluation, self.get_name())

            operands = [self._remove_duplicates(op, sameQ) for op in operands]
            items = functools.reduce(
                lambda a, b: [e for e in self._elementwise(a, b, sameQ)], operands
            )
        else:
            items = list(
                functools.reduce(getattr(set, self._operation), map(set, operands))
            )

        return Expression(seq[0].get_head(), *sorted(items))


class _TallyBin:
    def __init__(self, item):
        self._item = item
        self._count = 1

    def add_to(self, item):
        self._count += 1

    def from_python(self):
        return Expression(SymbolList, self._item, Integer(self._count))


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
            for li in lists.leaves:
                head_name = li.get_head_name()
                if head_name == "System`List":
                    yield li.leaves
                elif head_name != "System`Missing":
                    raise MessageException("Catenate", "invrp", li)

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


class Complement(_SetOperation):
    """
    <dl>
      <dt>'Complement[$all$, $e1$, $e2$, ...]'
      <dd>returns an expression containing the elements in the set $all$ that are not in any of $e1$, $e2$, etc.

      <dt>'Complement[$all$, $e1$, $e2$, ..., SameTest->$test$]'
      <dd>applies $test$ to the elements in $all$ and each of the $ei$ to determine equality.
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

    def _elementwise(self, a, b, sameQ: Callable[..., bool]):
        for ea in a:
            if not any(sameQ(eb, ea) for eb in b):
                yield ea


class DeleteDuplicates(_GatherOperation):
    """
    <dl>
      <dt>'DeleteDuplicates[$list$]'
      <dd>deletes duplicates from $list$.

      <dt>'DeleteDuplicates[$list$, $test$]'
      <dd>deletes elements from $list$ based on whether the function $test$ yields 'True' on pairs of elements.

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
      <dd>gathers leaves of $list$ into sub lists of items whose image under $f identical. Then, gathers these sub lists again into sub sub lists, that are identical under $g.
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

    def apply_no_overlap(self, li, n, evaluation):
        "Partition[li_List, n_Integer]"
        # TODO: Error checking
        return self._partition(li, n.get_int_value(), n.get_int_value(), evaluation)

    def apply(self, li, n, d, evaluation):
        "Partition[li_List, n_Integer, d_Integer]"
        # TODO: Error checking
        return self._partition(li, n.get_int_value(), d.get_int_value(), evaluation)


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
      <dd>interleaves the elements of both lists, returning '{$a1$, $b1$, $a2$, $b2$, ...}'.
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


class RotateLeft(_Rotate):
    """
    <dl>
      <dt>'RotateLeft[$expr$]'
      <dd>rotates the items of $expr$' by one item to the left.

      <dt>'RotateLeft[$expr$, $n$]'
      <dd>rotates the items of $expr$' by $n$ items to the left.

      <dt>'RotateLeft[$expr$, {$n1$, $n2$, ...}]'
      <dd>rotates the items of $expr$' by $n1$ items to the left at the first level, by $n2$ items to the left at the second level, and so on.
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
      <dd>rotates the items of $expr$' by $n1$ items to the right at the first level, by $n2$ items to the right at the second level, and so on.
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


class Tally(_GatherOperation):
    """
    <dl>
      <dt>'Tally[$list$]'
      <dd>counts and returns the number of occurences of objects and returns the result as a list of pairs {object, count}.

      <dt>'Tally[$list$, $test$]'
      <dd>counts the number of occurences of  objects and uses $test to determine if two objects should be counted in the same bin.
    </dl>

    >> Tally[{a, b, c, b, a}]
     = {{a, 2}, {b, 2}, {c, 1}}

    Tally always returns items in the order as they first appear in $list$:
    >> Tally[{b, b, a, a, a, d, d, d, d, c}]
     = {{b, 2}, {a, 3}, {d, 4}, {c, 1}}
    """

    _bin = _TallyBin


class Union(_SetOperation):
    """
    <dl>
      <dt>'Union[$a$, $b$, ...]'
      <dd>gives the union of the given set or sets. The resulting list will be sorted and each element will only occur once.
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

    def _elementwise(self, a, b, sameQ: Callable[..., bool]):
        for eb in b:
            yield eb
        for ea in a:
            if not any(sameQ(eb, ea) for eb in b):
                yield ea


class Intersection(_SetOperation):
    """
    <dl>
      <dt>'Intersection[$a$, $b$, ...]'
      <dd>gives the intersection of the sets. The resulting list will be sorted and each element will only occur once.
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

    def _elementwise(self, a, b, sameQ: Callable[..., bool]):
        for ea in a:
            if any(sameQ(eb, ea) for eb in b):
                yield ea
