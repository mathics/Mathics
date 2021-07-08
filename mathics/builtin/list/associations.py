# -*- coding: utf-8 -*-

"""
Associations

An Association maps keys to values and is similar to a dictionary in Python; it is often sparse in that their key space is much larger than the number of actual keys found in the collection.
"""

from mathics.version import __version__  # noqa used in loading to check consistency.


from mathics.builtin.base import (
    Builtin,
    Test,
)

from mathics.builtin.lists import list_boxes

from mathics.core.expression import (
    Expression,
    Integer,
    SymbolAssociation,
    Symbol,
    SymbolMakeBoxes,
    SymbolList,
)


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
        except:  # noqa
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
        except:  # noqa
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
        except:  # noqa
            return None


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


class Missing(Builtin):
    pass


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
        except:  # noqa
            return evaluation.message("Values", "invrl", rules[0])
