# -*- coding: utf-8 -*-

"""
Constructing Lists

Functions for constructing lists of various sizes and structure.
"""

from itertools import permutations

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin, Pattern

from mathics.builtin.lists import (
    _IterationFunction,
    get_tuples,
)

from mathics.core.convert import from_sympy

from mathics.core.expression import (
    Expression,
    Integer,
    SymbolList,
    structure,
)


class Array(Builtin):
    """
    <dl>
      <dt>'Array[$f$, $n$]'
      <dd>returns the $n$-element list '{$f$[1], ..., $f$[$n$]}'.

      <dt>'Array[$f$, $n$, $a$]'
      <dd>returns the $n$-element list '{$f$[$a$], ..., $f$[$a$ + $n$]}'.

      <dt>'Array[$f$, {$n$, $m$}, {$a$, $b$}]'
      <dd>returns an $n$-by-$m$ matrix created by applying $f$ to indices ranging from '($a$, $b$)' to '($a$ + $n$, $b$ + $m$)'.

      <dt>'Array[$f$, $dims$, $origins$, $h$]'
      <dd>returns an expression with the specified dimensions and index origins, with head $h$ (instead of 'List').
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


class Normal(Builtin):
    """
    <dl>
      <dt>'Normal[expr_]'
      <dd> Brings especial expressions to a normal expression from different especial forms.
    </dl>
    """


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

    attributes = ("Listable", "Protected")

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

    def apply(self, li, evaluation):
        "Permutations[li_List]"
        return Expression(
            "List",
            *[
                Expression(SymbolList, *p)
                for p in permutations(li.leaves, len(li.leaves))
            ],
        )

    def apply_n(self, li, n, evaluation):
        "Permutations[li_List, n_]"

        rs = None
        if isinstance(n, Integer):
            py_n = min(n.get_int_value(), len(li.leaves))
        elif n.has_form("List", 1) and isinstance(n.leaves[0], Integer):
            py_n = n.leaves[0].get_int_value()
            rs = (py_n,)
        elif (
            n.has_form("DirectedInfinity", 1) and n.leaves[0].get_int_value() == 1
        ) or n.get_name() == "System`All":
            py_n = len(li.leaves)
        else:
            py_n = None

        if py_n is None or py_n < 0:
            evaluation.message(
                self.get_name(), "nninfseq", Expression(self.get_name(), li, n)
            )
            return

        if rs is None:
            rs = range(py_n + 1)

        inner = structure("List", li, evaluation)
        outer = structure("List", inner, evaluation)

        return outer([inner(p) for r in rs for p in permutations(li.leaves, r)])


class Reap(Builtin):
    """
    <dl>
      <dt>'Reap[$expr$]'
      <dd>gives the result of evaluating $expr$, together with all values sown during this evaluation. Values sown with different tags are given in different lists.

      <dt>'Reap[$expr$, $pattern$]'
      <dd>only yields values sown with a tag matching $pattern$.
        'Reap[$expr$]' is equivalent to 'Reap[$expr$, _]'.

      <dt>'Reap[$expr$, {$pattern1$, $pattern2$, ...}]'
      <dd>uses multiple patterns.

      <dt>'Reap[$expr$, $pattern$, $f$]'
      <dd>applies $f$ on each tag and the corresponding values sown in the form '$f$[tag, {e1, e2, ...}]'.
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
                        if item[0].sameQ(tag):
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


class Table(_IterationFunction):
    """
    <dl>
      <dt>'Table[$expr$, $n$]'
      <dd>generates a list of $n$ copies of $expr$.

      <dt>'Table[$expr$, {$i$, $n$}]'
      <dd>generates a list of the values of expr when $i$ runs from 1 to $n$.

      <dt>'Table[$expr$, {$i$, $start$, $stop$, $step$}]'
      <dd>evaluates $expr$ with $i$ ranging from $start$ to $stop$,
        incrementing by $step$.

      <dt>'Table[$expr$, {$i$, {$e1$, $e2$, ..., $ei$}}]'
      <dd>evaluates $expr$ with $i$ taking on the values $e1$, $e2$,
        ..., $ei$.
    </dl>
    >> Table[x, 3]
     = {x, x, x}
    >> n = 0; Table[n = n + 1, {5}]
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

    rules = {
        "Table[expr_, n_Integer]": "Table[expr, {n}]",
    }

    def get_result(self, items):
        return Expression(SymbolList, *items)


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
