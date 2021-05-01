# -*- coding: utf-8 -*-

"""
SparseArray Functions
"""


from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.lists import walk_parts

from mathics.builtin.base import (
    Builtin,
)

from mathics.core.expression import (
    Expression,
    Integer,
    Integer0,
    Symbol,
)


class SparseArray(Builtin):
    """
    <dl>
    <dt>'SparseArray[$rules$]'
        <dd>Builds a sparse array acording to the list of $rules$.
    <dt>'SparseArray[$rules$, $dims$]'
        <dd>Builds a sparse array of dimensions $dims$ acording to the $rules$.
    <dt>'SparseArray[$list$]'
        <dd>Builds a sparse representation of $list$.
    </dl>

    >> SparseArray[{{1, 2} -> 1, {2, 1} -> 1}]
     = SparseArray[Automatic, {2, 2}, 0, {{1, 2} -> 1, {2, 1} -> 1}]
    >> SparseArray[{{1, 2} -> 1, {2, 1} -> 1}, {3, 3}]
     = SparseArray[Automatic, {3, 3}, 0, {{1, 2} -> 1, {2, 1} -> 1}]
    >> M=SparseArray[{{0, a}, {b, 0}}]
     = SparseArray[Automatic, {2, 2}, 0, {{1, 2} -> a, {2, 1} -> b}]
    >> M //Normal
     = {{0, a}, {b, 0}}

    """

    messages = {
        "list": "List expected at position 1 in SparseArray[``1``]",
        "rect": "Rectangular array or list of rules is expected at position 1 in SparseArray[``1``]",
        "exdims": "The dimensions cannot be determined from the positions `1`",
    }

    def list_to_sparse(self, array, evaluation):
        # TODO: Simplify and modularize this method.

        leaves = []
        dims = None
        if not array.has_form("List", None):
            return array
        if len(array.leaves) == 0:
            return
        # The first leaf determines the dimensions
        dims = None
        leaf = array.leaves[0]
        if leaf.has_form("List", None):
            leaf = self.list_to_sparse(leaf, evaluation)
            if leaf is None:
                return None
        if leaf.has_form("SparseArray", None):
            dims = leaf.leaves[1]
        if dims:
            leaves = [leaf]
            for i, leaf in enumerate(array.leaves):
                if i == 0:
                    continue
                newleaf = self.list_to_sparse(leaf, evaluation)
                if newleaf is None:
                    return
                if not newleaf.has_form("SparseArray", None):
                    return
                if not dims == newleaf.leaves[1]:
                    return
                leaves.append(newleaf)
        else:
            for i, leaf in enumerate(array.leaves):
                if leaf.has_form("SparseArray", None) or leaf.has_form("List", None):
                    return
                if leaf.is_numeric() and leaf.is_zero:
                    continue
                leaves.append(
                    Expression("Rule", Expression("List", Integer(i + 1)), leaf)
                )

            dims = Expression("List", Integer(len(array.leaves)))
            return Expression(
                "SparseArray",
                Symbol("Automatic"),
                dims,
                Integer0,
                Expression("List", *leaves),
            )
        # Now, reformat the list of sparse arrays as a single sparse array
        dims = Expression("List", Integer(len(array.leaves)), *(dims.leaves))
        rules = []
        for i, leaf in enumerate(leaves):
            for rule in leaf.leaves[3].leaves:
                pat, val = rule.leaves
                pat = Expression("List", Integer(i + 1), *(pat.leaves))
                rules.append(Expression("Rule", pat, val))
        return Expression(
            "SparseArray",
            Symbol("Automatic"),
            dims,
            Integer0,
            Expression("List", *rules),
        )

    def apply_dimensions(self, dims, default, data, evaluation):
        """System`Dimensions[System`SparseArray[System`Automatic, dims_List, default_, data_List]]"""
        return dims

    def apply_normal(self, dims, default, data, evaluation):
        """System`Normal[System`SparseArray[System`Automatic, dims_List, default_, data_List]]"""
        its = [Expression("List", n) for n in dims.leaves]
        table = Expression("Table", default, *its)
        table = table.evaluate(evaluation)
        # Now, apply the rules...
        for item in data.leaves:
            pos, val = item.leaves
            if pos.has_form("List", None):
                table = walk_parts([table], pos.leaves, evaluation, val)
        return table

    def find_dimensions(self, rules, evaluation):
        dims = None
        for rule in rules:
            pos = rule.leaves[0]
            if pos.has_form("List", None):
                if dims is None:
                    dims = [0] * len(pos.leaves)
                for i, idx in enumerate(pos.leaves):
                    if isinstance(idx, Integer):
                        j = idx.get_int_value()
                        if dims[i] < j:
                            dims[i] = j
        if any(d == 0 for d in dims):
            return
        return Expression("List", *[Integer(d) for d in dims])

    def apply_1(self, rules, evaluation):
        """SparseArray[rules_List]"""
        if not (rules.has_form("List", None) and len(rules.leaves) > 0):
            if rules == Symbol("Automatic"):
                return
            print(rules.has_form("List", (1,)))
            evaluation.message("SparseArray", "list", rules)
            return

        if not rules.leaves[0].is_atom() and rules.leaves[0].get_head_name() in (
            "System`Rule",
            "System`DelayedRule",
        ):
            dims = self.find_dimensions(rules.leaves, evaluation)
            if dims is None:
                return
            return self.apply_3(rules, dims, Integer0, evaluation)
        return self.list_to_sparse(rules, evaluation)

    def apply_2(self, rules, dims, evaluation):
        """SparseArray[rules_List, dims_List]"""
        return self.apply_3(rules, dims, Integer0, evaluation)

    def apply_3(self, rules, dims, default, evaluation):
        """SparseArray[rules_List, dims_List, default_]"""
        return Expression("SparseArray", Symbol("Automatic"), dims, default, rules)
