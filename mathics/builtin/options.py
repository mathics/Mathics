# -*- coding: utf-8 -*-

"""
Options and Default Arguments
"""

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin, Test, get_option
from mathics.core.expression import (
    Symbol,
    String,
    Expression,
    get_default_value,
    ensure_context,
    strip_context,
)
from mathics.builtin.image import Image
from mathics.core.expression import strip_context


class Options(Builtin):
    """
    <dl>
    <dt>'Options[$f$]'
        <dd>gives a list of optional arguments to $f$ and their
        default values.
    </dl>

    You can assign values to 'Options' to specify options.
    >> Options[f] = {n -> 2}
     = {n -> 2}
    >> Options[f]
     = {n :> 2}
    >> f[x_, OptionsPattern[f]] := x ^ OptionValue[n]
    >> f[x]
     = x ^ 2
    >> f[x, n -> 3]
     = x ^ 3

    #> f[x_, OptionsPattern[f]] := x ^ OptionValue["m"];
    #> Options[f] = {"m" -> 7};
    #> f[x]
     = x ^ 7

    Delayed option rules are evaluated just when the corresponding 'OptionValue' is called:
    >> f[a :> Print["value"]] /. f[OptionsPattern[{}]] :> (OptionValue[a]; Print["between"]; OptionValue[a]);
     | value
     | between
     | value
    In contrast to that, normal option rules are evaluated immediately:
    >> f[a -> Print["value"]] /. f[OptionsPattern[{}]] :> (OptionValue[a]; Print["between"]; OptionValue[a]);
     | value
     | between

    Options must be rules or delayed rules:
    >> Options[f] = {a}
     : {a} is not a valid list of option rules.
     = {a}
    A single rule need not be given inside a list:
    >> Options[f] = a -> b
     = a -> b
    >> Options[f]
     = {a :> b}
    Options can only be assigned to symbols:
    >> Options[a + b] = {a -> b}
     : Argument a + b at position 1 is expected to be a symbol.
     = {a -> b}

    #> f /: Options[f] = {a -> b}
     = {a -> b}
    #> Options[f]
     = {a :> b}
    #> f /: Options[g] := {a -> b}
     : Rule for Options can only be attached to g.
     = $Failed

    #> Options[f] = a /; True
     : a /; True is not a valid list of option rules.
     = a /; True
    """

    def apply(self, f, evaluation):
        "Options[f_]"

        name = f.get_name()
        if not name:
            if isinstance(f, Image):
                # FIXME ColorSpace, MetaInformation
                options = f.metadata
            else:
                evaluation.message("Options", "sym", f, 1)
                return
        else:
            options = evaluation.definitions.get_options(name)
        result = []
        for option, value in sorted(options.items(), key=lambda item: item[0]):
            # Don't use HoldPattern, since the returned List should be
            # assignable to Options again!
            result.append(Expression("RuleDelayed", Symbol(option), value))
        return Expression("List", *result)


class OptionValue(Builtin):
    """
    <dl>
    <dt>'OptionValue[$name$]'
        <dd>gives the value of the option $name$ as specified in a
        call to a function with 'OptionsPattern'.
    <dt>'OptionValue[$f$, $name$]'
        <dd>recover the value of the option $name$ associated to the symbol $f$.
    <dt>'OptionValue[$f$, $optvals$, $name$]'
        <dd>recover the value of the option $name$ associated to the symbol $f$,
            extracting the values from $optvals$ if available.
    <dt>'OptionValue[$\\ldots$, $list$]'
        <dd>recover the value of the options in $list$ .
    </dl>

    >> f[a->3] /. f[OptionsPattern[{}]] -> {OptionValue[a]}
     = {3}

    Unavailable options generate a message:
    >> f[a->3] /. f[OptionsPattern[{}]] -> {OptionValue[b]}
     : Option name b not found.
     = {b}

    The argument of 'OptionValue' must be a symbol:
    >> f[a->3] /. f[OptionsPattern[{}]] -> {OptionValue[a+b]}
     : Argument a + b at position 1 is expected to be a symbol.
     = {OptionValue[a + b]}
    However, it can be evaluated dynamically:
    >> f[a->5] /. f[OptionsPattern[{}]] -> {OptionValue[Symbol["a"]]}
     = {5}
    """

    messages = {
        "optnf": "Option name `1` not found.",
    }

    rules = {
        "OptionValue[optnames_List]": "OptionValue/@optnames",
        "OptionValue[f_, optnames_List]": "OptionValue[f,#1]&/@optnames",
        "OptionValue[f_, opts_, optnames_List]": "OptionValue[f,opts, #1]&/@optnames",
    }

    def apply_1(self, optname, evaluation):
        "OptionValue[optname_]"
        if evaluation.options is None:
            return

        if type(optname) is String:
            name = optname.to_python()[1:-1]
        else:
            name = optname.get_name()

        name = optname.get_name()
        if not name:
            name = optname.get_string_value()
            if name:
                name = ensure_context(name)
        if not name:
            evaluation.message("OptionValue", "sym", optname, 1)
            return

        val = get_option(evaluation.options, name, evaluation)
        if val is None:
            evaluation.message("OptionValue", "optnf", optname)
            return Symbol(name)
        return val

    def apply_2(self, f, optname, evaluation):
        "OptionValue[f_, optname_]"
        return self.apply_3(f, None, optname, evaluation)

    def apply_3(self, f, optvals, optname, evaluation):
        "OptionValue[f_, optvals_, optname_]"
        if type(optname) is String:
            name = optname.to_python()[1:-1]
        else:
            name = optname.get_name()

        if not name:
            name = optname.get_string_value()
            if name:
                name = ensure_context(name)
        if not name:
            evaluation.message("OptionValue", "sym", optname, 1)
            return
        # Look first in the explicit list
        if optvals:
            val = get_option(optvals.get_option_values(evaluation), name, evaluation)
        else:
            val = None
        # then, if not found, look at $f$. It could be a symbol, or a list of symbols, rules, and list of rules...
        if val is None:
            if f.is_symbol():
                val = get_option(
                    evaluation.definitions.get_options(f.get_name()), name, evaluation
                )
            else:
                if f.get_head_name() in ("System`Rule", "System`RuleDelayed"):
                    f = Expression("List", f)
                if f.get_head_name() == "System`List":
                    for leave in f.get_leaves():
                        if leave.is_symbol():
                            val = get_option(
                                evaluation.definitions.get_options(leave.get_name()),
                                name,
                                evaluation,
                            )
                            if val:
                                break
                        else:
                            values = leave.get_option_values(evaluation)
                            val = get_option(values, name, evaluation)
                            if val:
                                break

        if val is None and evaluation.options:
            val = get_option(evaluation.options, name, evaluation)
        if val is None:
            evaluation.message("OptionValue", "optnf", optname)
            return Symbol(name)
        return val


class Default(Builtin):
    """
    <dl>
    <dt>'Default[$f$]'
        <dd>gives the default value for an omitted paramter of $f$.
    <dt>'Default[$f$, $k$]'
        <dd>gives the default value for a parameter on the $k$th position.
    <dt>'Default[$f$, $k$, $n$]'
        <dd>gives the default value for the $k$th parameter out of $n$.
    </dl>

    Assign values to 'Default' to specify default values.

    >> Default[f] = 1
     = 1
    >> f[x_.] := x ^ 2
    >> f[]
     = 1

    Default values are stored in 'DefaultValues':
    >> DefaultValues[f]
     = {HoldPattern[Default[f]] :> 1}

    You can use patterns for $k$ and $n$:
    >> Default[h, k_, n_] := {k, n}
    Note that the position of a parameter is relative to the pattern, not the matching expression:
    >> h[] /. h[___, ___, x_., y_., ___] -> {x, y}
     = {{3, 5}, {4, 5}}
    """

    def apply(self, f, i, evaluation):
        "Default[f_, i___]"

        i = i.get_sequence()
        if len(i) > 2:
            evaluation.message("Default", "argb", 1 + len(i), 1, 3)
            return
        i = [index.get_int_value() for index in i]
        for index in i:
            if index is None or index < 1:
                evaluation.message("Default", "intp")
                return
        name = f.get_name()
        if not name:
            evaluation.message("Default", "sym", f, 1)
            return
        result = get_default_value(name, evaluation, *i)
        return result


class OptionQ(Test):
    """
    <dl>
    <dt>'OptionQ[$expr$]'
        <dd>returns 'True' if $expr$ has the form of a valid option
        specification.
    </dl>

    Examples of option specifications:
    >> OptionQ[a -> True]
     = True
    >> OptionQ[a :> True]
     = True
    >> OptionQ[{a -> True}]
     = True
    >> OptionQ[{a :> True}]
     = True

    Options lists are flattened when are applyied, so
    >> OptionQ[{a -> True, {b->1, "c"->2}}]
     = True
    >> OptionQ[{a -> True, {b->1, c}}]
     = False
    >> OptionQ[{a -> True, F[b->1,c->2]}]
     = False

    'OptionQ' returns 'False' if its argument is not a valid option
    specification:
    >> OptionQ[x]
     = False
    """

    def test(self, expr):
        expr = expr.flatten(Symbol("List"))
        if not expr.has_form("List", None):
            expr = [expr]
        else:
            expr = expr.get_leaves()
        return all(
            e.has_form("Rule", None) or e.has_form("RuleDelayed", 2) for e in expr
        )


class NotOptionQ(Test):
    """
    <dl>
    <dt>'NotOptionQ[$expr$]'
        <dd>returns 'True' if $expr$ does not have the form of a valid
        option specification.
    </dl>

    >> NotOptionQ[x]
     = True
    >> NotOptionQ[2]
     = True
    >> NotOptionQ["abc"]
     = True

    >> NotOptionQ[a -> True]
     = False
    """

    def test(self, expr):
        expr = expr.flatten(Symbol("List"))
        if not expr.has_form("List", None):
            expr = [expr]
        else:
            expr = expr.get_leaves()
        return not all(
            e.has_form("Rule", None) or e.has_form("RuleDelayed", 2) for e in expr
        )


class FilterRules(Builtin):
    """
    <dl>
    <dt>'FilterRules[$rules$, $pattern$]'
        <dd>gives those $rules$ that have a left side that matches $pattern$.
    <dt>'FilterRules[$rules$, {$pattern1$, $pattern2$, ...}]'
        <dd>gives those $rules$ that have a left side that match at least one of $pattern1$, $pattern2$, ...
    </dl>

    >> FilterRules[{x -> 100, y -> 1000}, x]
     = {x -> 100}

    >> FilterRules[{x -> 100, y -> 1000, z -> 10000}, {a, b, x, z}]
     = {x -> 100, z -> 10000}
    """

    rules = {
        "FilterRules[rules_List, patterns_List]": "FilterRules[rules, Alternatives @@ patterns]",
    }

    def apply(self, rules, pattern, evaluation):
        "FilterRules[rules_List, pattern_]"
        from mathics.builtin.patterns import Matcher

        match = Matcher(pattern).match

        def matched():
            for rule in rules.leaves:
                if rule.has_form("Rule", 2) and match(rule.leaves[0], evaluation):
                    yield rule

        return Expression("List", *list(matched()))


def options_to_rules(options, filter=None):
    items = sorted(options.items())
    if filter:
        items = [
            (name, value)
            for name, value in items
            if strip_context(name) in filter.keys()
        ]
    return [Expression("Rule", Symbol(name), value) for name, value in items]
