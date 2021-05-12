# -*- coding: utf-8 -*-

from mathics.version import __version__  # noqa used in loading to check consistency.

import mathics.builtin
from mathics.builtin.base import (
    Builtin,
    BinaryOperator,
    PostfixOperator,
    PrefixOperator,
)
from mathics.core.expression import (
    Expression,
    Symbol,
    SymbolFailed,
    SymbolNull,
    valid_context_name,
    system_symbols,
    String,
)
from mathics.core.rules import Rule, BuiltinRule
from mathics.builtin.patterns import RuleDelayed
from mathics.core.definitions import PyMathicsLoadException
from mathics.builtin.lists import walk_parts
from mathics.core.evaluation import MAX_RECURSION_DEPTH, set_python_recursion_limit

from mathics import settings
from mathics.core.definitions import PyMathicsLoadException


def repl_pattern_by_symbol(expr):
    leaves = expr.get_leaves()
    if len(leaves) == 0:
        return expr

    headname = expr.get_head_name()
    if headname == "System`Pattern":
        return leaves[0]

    changed = False
    newleaves = []
    for leave in leaves:
        l = repl_pattern_by_symbol(leave)
        if not (l is leave):
            changed = True
        newleaves.append(l)
    if changed:
        return Expression(headname, *newleaves)
    else:
        return expr


def get_symbol_list(list, error_callback):
    if list.has_form("List", None):
        list = list.leaves
    else:
        list = [list]
    values = []
    for item in list:
        name = item.get_name()
        if name:
            values.append(name)
        else:
            error_callback(item)
            return None
    return values


class _SetOperator(object):
    def assign_elementary(self, lhs, rhs, evaluation, tags=None, upset=False):
        name = lhs.get_head_name()
        lhs._format_cache = None
        condition = None

        if name == "System`Condition" and len(lhs.leaves) == 2:
            # This handle the case of many sucesive conditions:
            # f[x_]/; cond1 /; cond2 ...
            # is summarized to a single condition
            # f[x_]/; And[cond1, cond2, ...]
            condition = [lhs._leaves[1]]
            lhs = lhs._leaves[0]
            name = lhs.get_head_name()
            while name == "System`Condition" and len(lhs.leaves) == 2:
                condition.append(lhs._leaves[1])
                lhs = lhs._leaves[0]
                name = lhs.get_head_name()
            if len(condition) > 1:
                condition = Expression("System`And", *condition)
            else:
                condition = condition[0]
            condition = Expression("System`Condition", lhs, condition)
            name = lhs.get_head_name()
            lhs._format_cache = None
        if name == "System`Pattern":
            lhsleaves = lhs.get_leaves()
            lhs = lhsleaves[1]
            rulerepl = (lhsleaves[0], repl_pattern_by_symbol(lhs))
            rhs, status = rhs.apply_rules([Rule(*rulerepl)], evaluation)
            name = lhs.get_head_name()

        if name in system_symbols(
            "OwnValues",
            "DownValues",
            "SubValues",
            "UpValues",
            "NValues",
            "Options",
            "DefaultValues",
            "Attributes",
            "Messages",
        ):
            if len(lhs.leaves) != 1:
                evaluation.message_args(name, len(lhs.leaves), 1)
                return False
            tag = lhs.leaves[0].get_name()
            if not tag:
                evaluation.message(name, "sym", lhs.leaves[0], 1)
                return False
            if tags is not None and tags != [tag]:
                evaluation.message(name, "tag", Symbol(name), Symbol(tag))
                return False

            if (
                name != "System`Attributes"
                and "System`Protected"  # noqa
                in evaluation.definitions.get_attributes(tag)
            ):
                evaluation.message(name, "wrsym", Symbol(tag))
                return False
            if name == "System`Options":
                option_values = rhs.get_option_values(evaluation)
                if option_values is None:
                    evaluation.message(name, "options", rhs)
                    return False
                evaluation.definitions.set_options(tag, option_values)
            elif name == "System`Attributes":
                attributes = get_symbol_list(
                    rhs, lambda item: evaluation.message(name, "sym", item, 1)
                )
                if attributes is None:
                    return False
                if "System`Locked" in evaluation.definitions.get_attributes(tag):
                    evaluation.message(name, "locked", Symbol(tag))
                    return False
                evaluation.definitions.set_attributes(tag, attributes)
            else:
                rules = rhs.get_rules_list()
                if rules is None:
                    evaluation.message(name, "vrule", lhs, rhs)
                    return False
                evaluation.definitions.set_values(tag, name, rules)
            return True

        form = ""
        nprec = None
        default = False
        message = False

        allow_custom_tag = False

        focus = lhs

        if name == "System`N":
            if len(lhs.leaves) not in (1, 2):
                evaluation.message_args("N", len(lhs.leaves), 1, 2)
                return False
            if len(lhs.leaves) == 1:
                nprec = Symbol("MachinePrecision")
            else:
                nprec = lhs.leaves[1]
            focus = lhs.leaves[0]
            lhs = Expression("N", focus, nprec)
        elif name == "System`MessageName":
            if len(lhs.leaves) != 2:
                evaluation.message_args("MessageName", len(lhs.leaves), 2)
                return False
            focus = lhs.leaves[0]
            message = True
        elif name == "System`Default":
            if len(lhs.leaves) not in (1, 2, 3):
                evaluation.message_args("Default", len(lhs.leaves), 1, 2, 3)
                return False
            focus = lhs.leaves[0]
            default = True
        elif name == "System`Format":
            if len(lhs.leaves) not in (1, 2):
                evaluation.message_args("Format", len(lhs.leaves), 1, 2)
                return False
            if len(lhs.leaves) == 2:
                form = lhs.leaves[1].get_name()
                if not form:
                    evaluation.message("Format", "fttp", lhs.leaves[1])
                    return False
            else:
                form = system_symbols(
                    "StandardForm",
                    "TraditionalForm",
                    "OutputForm",
                    "TeXForm",
                    "MathMLForm",
                )
            lhs = focus = lhs.leaves[0]
        else:
            allow_custom_tag = True

        focus = focus.evaluate_leaves(evaluation)

        if tags is None and not upset:
            name = focus.get_lookup_name()
            if not name:
                evaluation.message(self.get_name(), "setraw", focus)
                return False
            tags = [name]
        elif upset:
            if allow_custom_tag:
                tags = []
                if focus.is_atom():
                    evaluation.message(self.get_name(), "normal")
                    return False
                for leaf in focus.leaves:
                    name = leaf.get_lookup_name()
                    tags.append(name)
            else:
                tags = [focus.get_lookup_name()]
        else:
            allowed_names = [focus.get_lookup_name()]
            if allow_custom_tag:
                for leaf in focus.get_leaves():
                    allowed_names.append(leaf.get_lookup_name())
            for name in tags:
                if name not in allowed_names:
                    evaluation.message(self.get_name(), "tagnfd", Symbol(name))
                    return False

        ignore_protection = False
        rhs_int_value = rhs.get_int_value()
        lhs_name = lhs.get_name()
        if lhs_name == "System`$RecursionLimit":
            # if (not rhs_int_value or rhs_int_value < 20) and not
            # rhs.get_name() == 'System`Infinity':
            if (
                not rhs_int_value
                or rhs_int_value < 20
                or rhs_int_value > MAX_RECURSION_DEPTH
            ):  # nopep8

                evaluation.message("$RecursionLimit", "limset", rhs)
                return False
            try:
                set_python_recursion_limit(rhs_int_value)
            except OverflowError:
                # TODO: Message
                return False
            ignore_protection = True
        if lhs_name == "System`$IterationLimit":
            if (
                not rhs_int_value or rhs_int_value < 20
            ) and not rhs.get_name() == "System`Infinity":
                evaluation.message("$IterationLimit", "limset", rhs)
                return False
            ignore_protection = True
        elif lhs_name == "System`$ModuleNumber":
            if not rhs_int_value or rhs_int_value <= 0:
                evaluation.message("$ModuleNumber", "set", rhs)
                return False
            ignore_protection = True
        elif lhs_name in ("System`$Line", "System`$HistoryLength"):
            if rhs_int_value is None or rhs_int_value < 0:
                evaluation.message(lhs_name, "intnn", rhs)
                return False
            ignore_protection = True
        elif lhs_name == "System`$RandomState":
            # TODO: allow setting of legal random states!
            # (but consider pickle's insecurity!)
            evaluation.message("$RandomState", "rndst", rhs)
            return False
        elif lhs_name == "System`$Context":
            new_context = rhs.get_string_value()
            if new_context is None or not valid_context_name(
                new_context, allow_initial_backquote=True
            ):
                evaluation.message(lhs_name, "cxset", rhs)
                return False

            # With $Context in Mathematica you can do some strange
            # things: e.g. with $Context set to Global`, something
            # like:
            #    $Context = "`test`"; newsym
            # is accepted and creates Global`test`newsym.
            # Implement this behaviour by interpreting
            #    $Context = "`test`"
            # as
            #    $Context = $Context <> "test`"
            #
            if new_context.startswith("`"):
                new_context = evaluation.definitions.get_current_context() + new_context.lstrip(
                    "`"
                )

            evaluation.definitions.set_current_context(new_context)
            ignore_protection = True
            return True
        elif lhs_name == "System`$ContextPath":
            currContext = evaluation.definitions.get_current_context()
            context_path = [s.get_string_value() for s in rhs.get_leaves()]
            context_path = [
                s if (s is None or s[0] != "`") else currContext[:-1] + s
                for s in context_path
            ]
            if rhs.has_form("List", None) and all(
                valid_context_name(s) for s in context_path
            ):
                evaluation.definitions.set_context_path(context_path)
                ignore_protection = True
                return True
            else:
                evaluation.message(lhs_name, "cxlist", rhs)
                return False
        elif lhs_name == "System`$MinPrecision":
            # $MinPrecision = Infinity is not allowed
            if rhs_int_value is not None and rhs_int_value >= 0:
                ignore_protection = True
                max_prec = evaluation.definitions.get_config_value("$MaxPrecision")
                if max_prec is not None and max_prec < rhs_int_value:
                    evaluation.message(
                        "$MinPrecision", "preccon", Symbol("$MinPrecision")
                    )
                    return True
            else:
                evaluation.message(lhs_name, "precset", lhs, rhs)
                return False
        elif lhs_name == "System`$MaxPrecision":
            if (
                rhs.has_form("DirectedInfinity", 1)
                and rhs.leaves[0].get_int_value() == 1
            ):
                ignore_protection = True
            elif rhs_int_value is not None and rhs_int_value > 0:
                ignore_protection = True
                min_prec = evaluation.definitions.get_config_value("$MinPrecision")
                if min_prec is not None and rhs_int_value < min_prec:
                    evaluation.message(
                        "$MaxPrecision", "preccon", Symbol("$MaxPrecision")
                    )
                    ignore_protection = True
                    return True
            else:
                evaluation.message(lhs_name, "precset", lhs, rhs)
                return False

        rhs_name = rhs.get_head_name()
        while rhs_name == "System`Condition":
            if len(rhs.leaves) != 2:
                evaluation.message_args("Condition", len(rhs.leaves), 2)
                return False
            else:
                lhs = Expression("Condition", lhs, rhs.leaves[1])
                rhs = rhs.leaves[0]
            rhs_name = rhs.get_head_name()

        # Now, let's add the conditions on the LHS
        if condition:
            lhs = Expression("Condition", lhs, condition.leaves[1])

        rule = Rule(lhs, rhs)
        count = 0
        defs = evaluation.definitions
        for tag in tags:
            if (
                not ignore_protection
                and "System`Protected"  # noqa
                in evaluation.definitions.get_attributes(tag)
            ):
                if lhs.get_name() == tag:
                    evaluation.message(self.get_name(), "wrsym", Symbol(tag))
                else:
                    evaluation.message(self.get_name(), "write", Symbol(tag), lhs)
                continue
            count += 1
            if form:
                defs.add_format(tag, rule, form)
            elif nprec:
                defs.add_nvalue(tag, rule)
            elif default:
                defs.add_default(tag, rule)
            elif message:
                defs.add_message(tag, rule)
            else:
                if upset:
                    defs.add_rule(tag, rule, position="up")
                else:
                    defs.add_rule(tag, rule)
        if count == 0:
            return False

        return True

    def assign(self, lhs, rhs, evaluation):
        lhs._format_cache = None
        if lhs.get_head_name() == "System`List":
            if not (rhs.get_head_name() == "System`List") or len(lhs.leaves) != len(
                rhs.leaves
            ):  # nopep8

                evaluation.message(self.get_name(), "shape", lhs, rhs)
                return False
            else:
                result = True
                for left, right in zip(lhs.leaves, rhs.leaves):
                    if not self.assign(left, right, evaluation):
                        result = False
                return result
        elif lhs.get_head_name() == "System`Part":
            if len(lhs.leaves) < 1:
                evaluation.message(self.get_name(), "setp", lhs)
                return False
            symbol = lhs.leaves[0]
            name = symbol.get_name()
            if not name:
                evaluation.message(self.get_name(), "setps", symbol)
                return False
            if "System`Protected" in evaluation.definitions.get_attributes(name):
                evaluation.message(self.get_name(), "wrsym", symbol)
                return False
            rule = evaluation.definitions.get_ownvalue(name)
            if rule is None:
                evaluation.message(self.get_name(), "noval", symbol)
                return False
            indices = lhs.leaves[1:]
            result = walk_parts([rule.replace], indices, evaluation, rhs)
            if result:
                evaluation.definitions.set_ownvalue(name, result)
            else:
                return False
        else:
            return self.assign_elementary(lhs, rhs, evaluation)


class Set(BinaryOperator, _SetOperator):
    """
    <dl>
    <dt>'Set[$expr$, $value$]'
    <dt>$expr$ = $value$
        <dd>evaluates $value$ and assigns it to $expr$.
    <dt>{$s1$, $s2$, $s3$} = {$v1$, $v2$, $v3$}
        <dd>sets multiple symbols ($s1$, $s2$, ...) to the
        corresponding values ($v1$, $v2$, ...).
    </dl>

    'Set' can be used to give a symbol a value:
    >> a = 3
     = 3
    >> a
     = 3

    An assignment like this creates an ownvalue:
    >> OwnValues[a]
     = {HoldPattern[a] :> 3}

    You can set multiple values at once using lists:
    >> {a, b, c} = {10, 2, 3}
     = {10, 2, 3}
    >> {a, b, {c, {d}}} = {1, 2, {{c1, c2}, {a}}}
     = {1, 2, {{c1, c2}, {10}}}
    >> d
     = 10

    'Set' evaluates its right-hand side immediately and assigns it to
    the left-hand side:
    >> a
     = 1
    >> x = a
     = 1
    >> a = 2
     = 2
    >> x
     = 1

    'Set' always returns the right-hand side, which you can again use
    in an assignment:
    >> a = b = c = 2;
    >> a == b == c == 2
     = True

    'Set' supports assignments to parts:
    >> A = {{1, 2}, {3, 4}};
    >> A[[1, 2]] = 5
     = 5
    >> A
     = {{1, 5}, {3, 4}}
    >> A[[;;, 2]] = {6, 7}
     = {6, 7}
    >> A
     = {{1, 6}, {3, 7}}
    Set a submatrix:
    >> B = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    >> B[[1;;2, 2;;-1]] = {{t, u}, {y, z}};
    >> B
     = {{1, t, u}, {4, y, z}, {7, 8, 9}}

    #> x = Infinity;
    """

    operator = "="
    precedence = 40
    grouping = "Right"
    attributes = ("HoldFirst", "SequenceHold")

    messages = {
        "setraw": "Cannot assign to raw object `1`.",
        "shape": "Lists `1` and `2` are not the same shape.",
    }

    def apply(self, lhs, rhs, evaluation):
        "lhs_ = rhs_"

        self.assign(lhs, rhs, evaluation)
        return rhs


class SetDelayed(Set):
    """
    <dl>
    <dt>'SetDelayed[$expr$, $value$]'
    <dt>$expr$ := $value$
        <dd>assigns $value$ to $expr$, without evaluating $value$.
    </dl>

    'SetDelayed' is like 'Set', except it has attribute 'HoldAll',
    thus it does not evaluate the right-hand side immediately, but
    evaluates it when needed.

    >> Attributes[SetDelayed]
     = {HoldAll, Protected, SequenceHold}
    >> a = 1
     = 1
    >> x := a
    >> x
     = 1
    Changing the value of $a$ affects $x$:
    >> a = 2
     = 2
    >> x
     = 2

    'Condition' ('/;') can be used with 'SetDelayed' to make an
    assignment that only holds if a condition is satisfied:
    >> f[x_] := p[x] /; x>0
    >> f[3]
     = p[3]
    >> f[-3]
     = f[-3]
    It also works if the condition is set in the LHS:
    >> F[x_, y_] /; x < y /; x>0  := x / y;
    >> F[x_, y_] := y / x;
    >> F[2, 3]
     = 2 / 3
    >> F[3, 2]
     = 2 / 3
    >> F[-3, 2]
     = -2 / 3
    """

    operator = ":="
    attributes = ("HoldAll", "SequenceHold")

    def apply(self, lhs, rhs, evaluation):
        "lhs_ := rhs_"

        if self.assign(lhs, rhs, evaluation):
            return Symbol("Null")
        else:
            return SymbolFailed


class UpSet(BinaryOperator, _SetOperator):
    """
    <dl>
    <dt>$f$[$x$] ^= $expression$
        <dd>evaluates $expression$ and assigns it to the value of
        $f$[$x$], associating the value with $x$.
    </dl>

    'UpSet' creates an upvalue:
    >> a[b] ^= 3;
    >> DownValues[a]
     = {}
    >> UpValues[b]
     = {HoldPattern[a[b]] :> 3}

    >> a ^= 3
     : Nonatomic expression expected.
     = 3

    You can use 'UpSet' to specify special values like format values.
    However, these values will not be saved in 'UpValues':
    >> Format[r] ^= "custom";
    >> r
     = custom
    >> UpValues[r]
     = {}

    #> f[g, a + b, h] ^= 2
     : Tag Plus in f[g, a + b, h] is Protected.
     = 2
    #> UpValues[h]
     = {HoldPattern[f[g, a + b, h]] :> 2}
    """

    operator = "^="
    precedence = 40
    attributes = ("HoldFirst", "SequenceHold")
    grouping = "Right"

    def apply(self, lhs, rhs, evaluation):
        "lhs_ ^= rhs_"

        self.assign_elementary(lhs, rhs, evaluation, upset=True)
        return rhs


class UpSetDelayed(UpSet):
    """
    <dl>
    <dt>'UpSetDelayed[$expression$, $value$]'
    <dt>'$expression$ ^:= $value$'
        <dd>assigns $expression$ to the value of $f$[$x$] (without
        evaluating $expression$), associating the value with $x$.
    </dl>

    >> a[b] ^:= x
    >> x = 2;
    >> a[b]
     = 2
    >> UpValues[b]
     = {HoldPattern[a[b]] :> x}

    #> f[g, a + b, h] ^:= 2
     : Tag Plus in f[g, a + b, h] is Protected.
    #> f[a+b] ^:= 2
     : Tag Plus in f[a + b] is Protected.
     = $Failed
    """

    operator = "^:="
    attributes = ("HoldAll", "SequenceHold")

    def apply(self, lhs, rhs, evaluation):
        "lhs_ ^:= rhs_"

        if self.assign_elementary(lhs, rhs, evaluation, upset=True):
            return Symbol("Null")
        else:
            return SymbolFailed


class TagSet(Builtin, _SetOperator):
    """
    <dl>
    <dt>'TagSet[$f$, $expr$, $value$]'
    <dt>'$f$ /: $expr$ = $value$'
        <dd>assigns $value$ to $expr$, associating the corresponding
        rule with the symbol $f$.
    </dl>

    Create an upvalue without using 'UpSet':
    >> x /: f[x] = 2
     = 2
    >> f[x]
     = 2
    >> DownValues[f]
     = {}
    >> UpValues[x]
     = {HoldPattern[f[x]] :> 2}

    The symbol $f$ must appear as the ultimate head of $lhs$ or as the head of a leaf in $lhs$:
    >> x /: f[g[x]] = 3;
     : Tag x not found or too deep for an assigned rule.
    >> g /: f[g[x]] = 3;
    >> f[g[x]]
     = 3
    """

    attributes = ("HoldAll", "SequenceHold")

    messages = {
        "tagnfd": "Tag `1` not found or too deep for an assigned rule.",
    }

    def apply(self, f, lhs, rhs, evaluation):
        "f_ /: lhs_ = rhs_"

        name = f.get_name()
        if not name:
            evaluation.message(self.get_name(), "sym", f, 1)
            return

        rhs = rhs.evaluate(evaluation)
        self.assign_elementary(lhs, rhs, evaluation, tags=[name])
        return rhs


class TagSetDelayed(TagSet):
    """
    <dl>
    <dt>'TagSetDelayed[$f$, $expr$, $value$]'
    <dt>'$f$ /: $expr$ := $value$'
        <dd>is the delayed version of 'TagSet'.
    </dl>
    """

    attributes = ("HoldAll", "SequenceHold")

    def apply(self, f, lhs, rhs, evaluation):
        "f_ /: lhs_ := rhs_"

        name = f.get_name()
        if not name:
            evaluation.message(self.get_name(), "sym", f, 1)
            return

        rhs = rhs.evaluate(evaluation)
        if self.assign_elementary(lhs, rhs, evaluation, tags=[name]):
            return Symbol("Null")
        else:
            return SymbolFailed


class Definition(Builtin):
    """
    <dl>
    <dt>'Definition[$symbol$]'
        <dd>prints as the user-defined values and rules associated with $symbol$.
    </dl>

    'Definition' does not print information for 'ReadProtected' symbols.
    'Definition' uses 'InputForm' to format values.

    >> a = 2;
    >> Definition[a]
     = a = 2

    >> f[x_] := x ^ 2
    >> g[f] ^:= 2
    >> Definition[f]
     = f[x_] = x ^ 2
     .
     . g[f] ^= 2

    Definition of a rather evolved (though meaningless) symbol:
    >> Attributes[r] := {Orderless}
    >> Format[r[args___]] := Infix[{args}, "~"]
    >> N[r] := 3.5
    >> Default[r, 1] := 2
    >> r::msg := "My message"
    >> Options[r] := {Opt -> 3}
    >> r[arg_., OptionsPattern[r]] := {arg, OptionValue[Opt]}

    Some usage:
    >> r[z, x, y]
     = x ~ y ~ z
    >> N[r]
     = 3.5
    >> r[]
     = {2, 3}
    >> r[5, Opt->7]
     = {5, 7}

    Its definition:
    >> Definition[r]
     = Attributes[r] = {Orderless}
     .
     . arg_. ~ OptionsPattern[r] = {arg, OptionValue[Opt]}
     .
     . N[r, MachinePrecision] = 3.5
     .
     . Format[args___, MathMLForm] = Infix[{args}, "~"]
     .
     . Format[args___, OutputForm] = Infix[{args}, "~"]
     .
     . Format[args___, StandardForm] = Infix[{args}, "~"]
     .
     . Format[args___, TeXForm] = Infix[{args}, "~"]
     .
     . Format[args___, TraditionalForm] = Infix[{args}, "~"]
     .
     . Default[r, 1] = 2
     .
     . Options[r] = {Opt -> 3}

    For 'ReadProtected' symbols, 'Definition' just prints attributes, default values and options:
    >> SetAttributes[r, ReadProtected]
    >> Definition[r]
     = Attributes[r] = {Orderless, ReadProtected}
     .
     . Default[r, 1] = 2
     .
     . Options[r] = {Opt -> 3}
    This is the same for built-in symbols:
    >> Definition[Plus]
     = Attributes[Plus] = {Flat, Listable, NumericFunction, OneIdentity, Orderless, Protected}
     .
     . Default[Plus] = 0
    >> Definition[Level]
     = Attributes[Level] = {Protected}
     .
     . Options[Level] = {Heads -> False}

    'ReadProtected' can be removed, unless the symbol is locked:
    >> ClearAttributes[r, ReadProtected]
    'Clear' clears values:
    >> Clear[r]
    >> Definition[r]
     = Attributes[r] = {Orderless}
     .
     . Default[r, 1] = 2
     .
     . Options[r] = {Opt -> 3}
    'ClearAll' clears everything:
    >> ClearAll[r]
    >> Definition[r]
     = Null

    If a symbol is not defined at all, 'Null' is printed:
    >> Definition[x]
     = Null
    """

    attributes = ("HoldAll",)
    precedence = 670

    def format_definition(self, symbol, evaluation, grid=True):
        "StandardForm,TraditionalForm,OutputForm: Definition[symbol_]"

        lines = []

        def print_rule(rule, up=False, lhs=lambda l: l, rhs=lambda r: r):
            evaluation.check_stopped()
            if isinstance(rule, Rule):
                r = rhs(
                    rule.replace.replace_vars(
                        {
                            "System`Definition": Expression(
                                "HoldForm", Symbol("Definition")
                            )
                        },
                        evaluation,
                    )
                )
                lines.append(
                    Expression(
                        "HoldForm",
                        Expression(up and "UpSet" or "Set", lhs(rule.pattern.expr), r),
                    )
                )

        name = symbol.get_name()
        if not name:
            evaluation.message("Definition", "sym", symbol, 1)
            return
        attributes = evaluation.definitions.get_attributes(name)
        definition = evaluation.definitions.get_user_definition(name, create=False)
        all = evaluation.definitions.get_definition(name)
        if attributes:
            attributes = list(attributes)
            attributes.sort()
            lines.append(
                Expression(
                    "HoldForm",
                    Expression(
                        "Set",
                        Expression("Attributes", symbol),
                        Expression(
                            "List", *(Symbol(attribute) for attribute in attributes)
                        ),
                    ),
                )
            )

        if definition is not None and "System`ReadProtected" not in attributes:
            for rule in definition.ownvalues:
                print_rule(rule)
            for rule in definition.downvalues:
                print_rule(rule)
            for rule in definition.subvalues:
                print_rule(rule)
            for rule in definition.upvalues:
                print_rule(rule, up=True)
            for rule in definition.nvalues:
                print_rule(rule)
            formats = sorted(definition.formatvalues.items())
            for format, rules in formats:
                for rule in rules:

                    def lhs(expr):
                        return Expression("Format", expr, Symbol(format))

                    def rhs(expr):
                        if expr.has_form("Infix", None):
                            expr = Expression(
                                Expression("HoldForm", expr.head), *expr.leaves
                            )
                        return Expression("InputForm", expr)

                    print_rule(rule, lhs=lhs, rhs=rhs)
        for rule in all.defaultvalues:
            print_rule(rule)
        if all.options:
            options = sorted(all.options.items())
            lines.append(
                Expression(
                    "HoldForm",
                    Expression(
                        "Set",
                        Expression("Options", symbol),
                        Expression(
                            "List",
                            *(
                                Expression("Rule", Symbol(name), value)
                                for name, value in options
                            )
                        ),
                    ),
                )
            )
        if grid:
            if lines:
                return Expression(
                    "Grid",
                    Expression("List", *(Expression("List", line) for line in lines)),
                    Expression("Rule", Symbol("ColumnAlignments"), Symbol("Left")),
                )
            else:
                return Symbol("Null")
        else:
            for line in lines:
                evaluation.print_out(Expression("InputForm", line))
            return Symbol("Null")

    def format_definition_input(self, symbol, evaluation):
        "InputForm: Definition[symbol_]"
        return self.format_definition(symbol, evaluation, grid=False)


def _get_usage_string(symbol, evaluation, htmlout=False):
    """
    Returns a python string with the documentation associated to a given symbol.
    """
    definition = evaluation.definitions.get_definition(symbol.name)
    ruleusage = definition.get_values_list("messages")
    usagetext = None
    import re

    # First look at user definitions:
    for rulemsg in ruleusage:
        if rulemsg.pattern.expr.leaves[1].__str__() == '"usage"':
            usagetext = rulemsg.replace.value
    if usagetext is not None:
        # Maybe, if htmltout is True, we should convert
        # the value to a HTML form...
        return usagetext
    # Otherwise, look at the pymathics, and builtin docstrings:
    builtins = evaluation.definitions.builtin
    pymathics = evaluation.definitions.pymathics
    bio = pymathics.get(definition.name)
    if bio is None:
        bio = builtins.get(definition.name)

    if bio is not None:
        from mathics.doc.doc import Doc

        docstr = bio.builtin.__class__.__doc__
        if docstr is None:
            return None
        if htmlout:
            usagetext = Doc(docstr).html()
        else:
            usagetext = Doc(docstr).text(0)
        usagetext = re.sub(r"\$([0-9a-zA-Z]*)\$", r"\1", usagetext)
        return usagetext
    return None


class Information(PrefixOperator):
    """
    <dl>
    <dt>'Information[$symbol$]'
        <dd>Prints information about a $symbol$
    </dl>
    'Information' does not print information for 'ReadProtected' symbols.
    'Information' uses 'InputForm' to format values.

    #> a = 2;
    #> Information[a]
     | a = 2
     .
     = Null

    #> f[x_] := x ^ 2;
    #> g[f] ^:= 2;
    #> f::usage = "f[x] returns the square of x";
    #> Information[f]
     | f[x] returns the square of x
     .
     . f[x_] = x ^ 2
     .
     . g[f] ^= 2
     .
     = Null


    #> ? Table
     | 
     .   'Table[expr, {i, n}]'
     .     evaluates expr with i ranging from 1 to n, returning
     . a list of the results.
     .   'Table[expr, {i, start, stop, step}]'
     .     evaluates expr with i ranging from start to stop,
     . incrementing by step.
     .   'Table[expr, {i, {e1, e2, ..., ei}}]'
     .     evaluates expr with i taking on the values e1, e2,
     . ..., ei.
     .
     = Null

    #> Information[Table]
     | 
     .   'Table[expr, {i, n}]'
     .     evaluates expr with i ranging from 1 to n, returning
     . a list of the results.
     .   'Table[expr, {i, start, stop, step}]'
     .     evaluates expr with i ranging from start to stop,
     . incrementing by step.
     .   'Table[expr, {i, {e1, e2, ..., ei}}]'
     .     evaluates expr with i taking on the values e1, e2,
     . ..., ei.
     .
     . Attributes[Table] = {HoldAll, Protected}
     .
     = Null
    """

    operator = "??"
    precedence = 0
    attributes = ("HoldAll", "SequenceHold", "Protect", "ReadProtect")
    messages = {"notfound": "Expression `1` is not a symbol"}
    options = {
        "LongForm": "True",
    }

    def format_definition(self, symbol, evaluation, options, grid=True):
        "StandardForm,TraditionalForm,OutputForm: Information[symbol_, OptionsPattern[Information]]"
        ret = SymbolNull
        lines = []
        if isinstance(symbol, String):
            evaluation.print_out(symbol)
            return ret
        if not isinstance(symbol, Symbol):
            evaluation.message("Information", "notfound", symbol)
            return ret
        # Print the "usage" message if available.
        usagetext = _get_usage_string(symbol, evaluation)
        if usagetext is not None:
            lines.append(usagetext)

        if self.get_option(options, "LongForm", evaluation).to_python():
            self.show_definitions(symbol, evaluation, lines)

        if grid:
            if lines:
                infoshow = Expression(
                    "Grid",
                    Expression("List", *(Expression("List", line) for line in lines)),
                    Expression("Rule", Symbol("ColumnAlignments"), Symbol("Left")),
                )
                evaluation.print_out(infoshow)
        else:
            for line in lines:
                evaluation.print_out(Expression("InputForm", line))
        return ret

        # It would be deserable to call here the routine inside Definition, but for some reason it fails...
        # Instead, I just copy the code from Definition

    def show_definitions(self, symbol, evaluation, lines):
        def print_rule(rule, up=False, lhs=lambda l: l, rhs=lambda r: r):
            evaluation.check_stopped()
            if isinstance(rule, Rule):
                r = rhs(
                    rule.replace.replace_vars(
                        {
                            "System`Definition": Expression(
                                "HoldForm", Symbol("Definition")
                            )
                        }
                    )
                )
                lines.append(
                    Expression(
                        "HoldForm",
                        Expression(up and "UpSet" or "Set", lhs(rule.pattern.expr), r),
                    )
                )

        name = symbol.get_name()
        if not name:
            evaluation.message("Definition", "sym", symbol, 1)
            return
        attributes = evaluation.definitions.get_attributes(name)
        definition = evaluation.definitions.get_user_definition(name, create=False)
        all = evaluation.definitions.get_definition(name)
        if attributes:
            attributes = list(attributes)
            attributes.sort()
            lines.append(
                Expression(
                    "HoldForm",
                    Expression(
                        "Set",
                        Expression("Attributes", symbol),
                        Expression(
                            "List", *(Symbol(attribute) for attribute in attributes)
                        ),
                    ),
                )
            )

        if definition is not None and "System`ReadProtected" not in attributes:
            for rule in definition.ownvalues:
                print_rule(rule)
            for rule in definition.downvalues:
                print_rule(rule)
            for rule in definition.subvalues:
                print_rule(rule)
            for rule in definition.upvalues:
                print_rule(rule, up=True)
            for rule in definition.nvalues:
                print_rule(rule)
            formats = sorted(definition.formatvalues.items())
            for format, rules in formats:
                for rule in rules:

                    def lhs(expr):
                        return Expression("Format", expr, Symbol(format))

                    def rhs(expr):
                        if expr.has_form("Infix", None):
                            expr = Expression(
                                Expression("HoldForm", expr.head), *expr.leaves
                            )
                        return Expression("InputForm", expr)

                    print_rule(rule, lhs=lhs, rhs=rhs)
        for rule in all.defaultvalues:
            print_rule(rule)
        if all.options:
            options = sorted(all.options.items())
            lines.append(
                Expression(
                    "HoldForm",
                    Expression(
                        "Set",
                        Expression("Options", symbol),
                        Expression(
                            "List",
                            *(
                                Expression("Rule", Symbol(name), value)
                                for name, value in options
                            )
                        ),
                    ),
                )
            )
        return

    def format_definition_input(self, symbol, evaluation, options):
        "InputForm: Information[symbol_, OptionsPattern[Information]]"
        self.format_definition(symbol, evaluation, options, grid=False)
        ret = SymbolNull
        return ret


class Clear(Builtin):
    """
    <dl>
    <dt>'Clear[$symb1$, $symb2$, ...]'
        <dd>clears all values of the given symbols.
        The arguments can also be given as strings containing symbol names.
    </dl>

    >> x = 2;
    >> Clear[x]
    >> x
     = x

    >> x = 2;
    >> y = 3;
    >> Clear["Global`*"]
    >> x
     = x
    >> y
     = y

    'ClearAll' may not be called for 'Protected' symbols.
    >> Clear[Sin]
     : Symbol Sin is Protected.
    The values and rules associated with built-in symbols will not get lost when applying 'Clear'
    (after unprotecting them):
    >> Unprotect[Sin]
    >> Clear[Sin]
    >> Sin[Pi]
     = 0

    'Clear' does not remove attributes, messages, options, and default values associated
    with the symbols. Use 'ClearAll' to do so.
    >> Attributes[r] = {Flat, Orderless};
    >> Clear["r"]
    >> Attributes[r]
     = {Flat, Orderless}
    """

    attributes = ("HoldAll",)

    messages = {
        "ssym": "`1` is not a symbol or a string.",
    }

    allow_locked = True

    def do_clear(self, definition):
        definition.ownvalues = []
        definition.downvalues = []
        definition.subvalues = []
        definition.upvalues = []
        definition.formatvalues = {}
        definition.nvalues = []

    def apply(self, symbols, evaluation):
        "%(name)s[symbols___]"
        if isinstance(symbols, Symbol):
            symbols = [symbols]
        elif isinstance(symbols, Expression):
            symbols = symbols.get_leaves()
        elif isinstance(symbols, String):
            symbols = [symbols]
        else:
            symbols = symbols.get_sequence()

        for symbol in symbols:
            if isinstance(symbol, Symbol):
                names = [symbol.get_name()]
            else:
                pattern = symbol.get_string_value()
                if not pattern:
                    evaluation.message("Clear", "ssym", symbol)
                    continue
                if pattern[0] == "`":
                    pattern = evaluation.definitions.get_current_context() + pattern[1:]

                names = evaluation.definitions.get_matching_names(pattern)
            for name in names:
                attributes = evaluation.definitions.get_attributes(name)
                if "System`Protected" in attributes:
                    evaluation.message("Clear", "wrsym", Symbol(name))
                    continue
                if not self.allow_locked and "System`Locked" in attributes:
                    evaluation.message("Clear", "locked", Symbol(name))
                    continue
                definition = evaluation.definitions.get_user_definition(name)
                self.do_clear(definition)

        return Symbol("Null")

    def apply_all(self, evaluation):
        "Clear[System`All]"
        evaluation.definitions.set_user_definitions({})
        evaluation.definitions.clear_pymathics_modules()
        return


class ClearAll(Clear):
    """
    <dl>
    <dt>'ClearAll[$symb1$, $symb2$, ...]'
        <dd>clears all values, attributes, messages and options associated with the given symbols.
        The arguments can also be given as strings containing symbol names.
    </dl>

    >> x = 2;
    >> ClearAll[x]
    >> x
     = x
    >> Attributes[r] = {Flat, Orderless};
    >> ClearAll[r]
    >> Attributes[r]
     = {}

    'ClearAll' may not be called for 'Protected' or 'Locked' symbols.
    >> Attributes[lock] = {Locked};
    >> ClearAll[lock]
     : Symbol lock is locked.
    """

    allow_locked = False

    def do_clear(self, definition):
        super(ClearAll, self).do_clear(definition)
        definition.attributes = set()
        definition.messages = []
        definition.options = []
        definition.defaultvalues = []

    def apply_all(self, evaluation):
        "ClearAll[System`All]"
        evaluation.definitions.set_user_definitions({})
        evaluation.definitions.clear_pymathics_modules()
        return


class Unset(PostfixOperator):
    """
    <dl>
    <dt>'Unset[$x$]'
    <dt>'$x$=.'
        <dd>removes any value belonging to $x$.
    </dl>
    >> a = 2
     = 2
    >> a =.
    >> a
     = a

    Unsetting an already unset or never defined variable will not
    change anything:
    >> a =.
    >> b =.

    'Unset' can unset particular function values. It will print a message
    if no corresponding rule is found.
    >> f[x_] =.
     : Assignment on f for f[x_] not found.
     = $Failed
    >> f[x_] := x ^ 2
    >> f[3]
     = 9
    >> f[x_] =.
    >> f[3]
     = f[3]

    You can also unset 'OwnValues', 'DownValues', 'SubValues', and 'UpValues' directly.
    This is equivalent to setting them to '{}'.
    >> f[x_] = x; f[0] = 1;
    >> DownValues[f] =.
    >> f[2]
     = f[2]

    'Unset' threads over lists:
    >> a = b = 3;
    >> {a, {b}} =.
     = {Null, {Null}}

    #> x = 2;
    #> OwnValues[x] =.
    #> x
     = x
    #> f[a][b] = 3;
    #> SubValues[f] =.
    #> f[a][b]
     = f[a][b]
    #> PrimeQ[p] ^= True
     = True
    #> PrimeQ[p]
     = True
    #> UpValues[p] =.
    #> PrimeQ[p]
     = False

    #> a + b ^= 5;
    #> a =.
    #> a + b
     = 5
    #> {UpValues[a], UpValues[b]} =.
     = {Null, Null}
    #> a + b
     = a + b

    #> Unset[Messages[1]]
     : First argument in Messages[1] is not a symbol or a string naming a symbol.
     = $Failed
    """

    operator = "=."
    precedence = 670
    attributes = ("HoldFirst", "Listable", "ReadProtected")

    messages = {
        "norep": "Assignment on `2` for `1` not found.",
        "usraw": "Cannot unset raw object `1`.",
    }

    def apply(self, expr, evaluation):
        "Unset[expr_]"

        name = expr.get_head_name()
        if name in system_symbols(
            "OwnValues",
            "DownValues",
            "SubValues",
            "UpValues",
            "NValues",
            "Options",
            "Messages",
        ):
            if len(expr.leaves) != 1:
                evaluation.message_args(name, len(expr.leaves), 1)
                return SymbolFailed
            symbol = expr.leaves[0].get_name()
            if not symbol:
                evaluation.message(name, "fnsym", expr)
                return SymbolFailed
            if name == "System`Options":
                empty = {}
            else:
                empty = []
            evaluation.definitions.set_values(symbol, name, empty)
            return Symbol("Null")
        name = expr.get_lookup_name()
        if not name:
            evaluation.message("Unset", "usraw", expr)
            return SymbolFailed
        if not evaluation.definitions.unset(name, expr):
            if not expr.is_atom():
                evaluation.message("Unset", "norep", expr, Symbol(name))
                return SymbolFailed
        return Symbol("Null")


def get_symbol_values(symbol, func_name, position, evaluation):
    name = symbol.get_name()
    if not name:
        evaluation.message(func_name, "sym", symbol, 1)
        return
    if position in ("default",):
        definition = evaluation.definitions.get_definition(name)
    else:
        definition = evaluation.definitions.get_user_definition(name)
    leaves = []
    for rule in definition.get_values_list(position):
        if isinstance(rule, Rule):
            pattern = rule.pattern
            if pattern.has_form("HoldPattern", 1):
                pattern = pattern.expr
            else:
                pattern = Expression("HoldPattern", pattern.expr)
            leaves.append(Expression("RuleDelayed", pattern, rule.replace))
    return Expression("List", *leaves)


class DownValues(Builtin):
    """
    <dl>
    <dt>'DownValues[$symbol$]'
        <dd>gives the list of downvalues associated with $symbol$.
    </dl>

    'DownValues' uses 'HoldPattern' and 'RuleDelayed' to protect the
    downvalues from being evaluated. Moreover, it has attribute
    'HoldAll' to get the specified symbol instead of its value.

    >> f[x_] := x ^ 2
    >> DownValues[f]
     = {HoldPattern[f[x_]] :> x ^ 2}

    Mathics will sort the rules you assign to a symbol according to
    their specificity. If it cannot decide which rule is more special,
    the newer one will get higher precedence.
    >> f[x_Integer] := 2
    >> f[x_Real] := 3
    >> DownValues[f]
     = {HoldPattern[f[x_Real]] :> 3, HoldPattern[f[x_Integer]] :> 2, HoldPattern[f[x_]] :> x ^ 2}
    >> f[3]
     = 2
    >> f[3.]
     = 3
    >> f[a]
     = a ^ 2

    The default order of patterns can be computed using 'Sort' with
    'PatternsOrderedQ':
    >> Sort[{x_, x_Integer}, PatternsOrderedQ]
     = {x_Integer, x_}

    By assigning values to 'DownValues', you can override the default
    ordering:
    >> DownValues[g] := {g[x_] :> x ^ 2, g[x_Integer] :> x}
    >> g[2]
     = 4

    Fibonacci numbers:
    >> DownValues[fib] := {fib[0] -> 0, fib[1] -> 1, fib[n_] :> fib[n - 1] + fib[n - 2]}
    >> fib[5]
     = 5
    """

    attributes = ("HoldAll",)

    def apply(self, symbol, evaluation):
        "DownValues[symbol_]"

        return get_symbol_values(symbol, "DownValues", "down", evaluation)


class OwnValues(Builtin):
    """
    <dl>
    <dt>'OwnValues[$symbol$]'
        <dd>gives the list of ownvalues associated with $symbol$.
    </dl>

    >> x = 3;
    >> x = 2;
    >> OwnValues[x]
     = {HoldPattern[x] :> 2}
    >> x := y
    >> OwnValues[x]
     = {HoldPattern[x] :> y}
    >> y = 5;
    >> OwnValues[x]
     = {HoldPattern[x] :> y}
    >> Hold[x] /. OwnValues[x]
     = Hold[y]
    >> Hold[x] /. OwnValues[x] // ReleaseHold
     = 5
    """

    attributes = ("HoldAll",)

    def apply(self, symbol, evaluation):
        "OwnValues[symbol_]"

        return get_symbol_values(symbol, "OwnValues", "own", evaluation)


class SubValues(Builtin):
    """
    <dl>
    <dt>'SubValues[$symbol$]'
        <dd>gives the list of subvalues associated with $symbol$.
    </dl>

    >> f[1][x_] := x
    >> f[2][x_] := x ^ 2
    >> SubValues[f]
     = {HoldPattern[f[2][x_]] :> x ^ 2, HoldPattern[f[1][x_]] :> x}
    >> Definition[f]
     = f[2][x_] = x ^ 2
     .
     . f[1][x_] = x
    """

    attributes = ("HoldAll",)

    def apply(self, symbol, evaluation):
        "SubValues[symbol_]"

        return get_symbol_values(symbol, "SubValues", "sub", evaluation)


class UpValues(Builtin):
    """
    <dl>
    <dt>'UpValues[$symbol$]'
        <dd>gives the list of upvalues associated with $symbol$.
    </dl>

    >> a + b ^= 2
     = 2
    >> UpValues[a]
     = {HoldPattern[a + b] :> 2}
    >> UpValues[b]
     = {HoldPattern[a + b] :> 2}

    You can assign values to 'UpValues':
    >> UpValues[pi] := {Sin[pi] :> 0}
    >> Sin[pi]
     = 0
    """

    attributes = ("HoldAll",)

    def apply(self, symbol, evaluation):
        "UpValues[symbol_]"

        return get_symbol_values(symbol, "UpValues", "up", evaluation)


class NValues(Builtin):
    """
    <dl>
    <dt>'NValues[$symbol$]'
        <dd>gives the list of numerical values associated with $symbol$.
    </dl>

    >> NValues[a]
     = {}
    >> N[a] = 3;
    >> NValues[a]
     = {HoldPattern[N[a, MachinePrecision]] :> 3}

    You can assign values to 'NValues':
    >> NValues[b] := {N[b, MachinePrecision] :> 2}
    >> N[b]
     = 2.
    Be sure to use 'SetDelayed', otherwise the left-hand side of the transformation rule will be evaluated immediately,
    causing the head of 'N' to get lost. Furthermore, you have to include the precision in the rules; 'MachinePrecision'
    will not be inserted automatically:
    >> NValues[c] := {N[c] :> 3}
    >> N[c]
     = c

    Mathics will gracefully assign any list of rules to 'NValues'; however, inappropriate rules will never be used:
    >> NValues[d] = {foo -> bar};
    >> NValues[d]
     = {HoldPattern[foo] :> bar}
    >> N[d]
     = d
    """

    attributes = ("HoldAll",)

    def apply(self, symbol, evaluation):
        "NValues[symbol_]"

        return get_symbol_values(symbol, "NValues", "n", evaluation)


class Messages(Builtin):
    """
    <dl>
    <dt>'Messages[$symbol$]'
        <dd>gives the list of messages associated with $symbol$.
    </dl>

    >> a::b = "foo"
     = foo
    >> Messages[a]
     = {HoldPattern[a::b] :> foo}
    >> Messages[a] = {a::c :> "bar"};
    >> a::c // InputForm
     = "bar"
    >> Message[a::c]
     : bar
    """

    attributes = ("HoldAll",)

    def apply(self, symbol, evaluation):
        "Messages[symbol_]"

        return get_symbol_values(symbol, "Messages", "messages", evaluation)


class DefaultValues(Builtin):
    """
    <dl>
    <dt>'DefaultValues[$symbol$]'
        <dd>gives the list of default values associated with $symbol$.
    </dl>

    >> Default[f, 1] = 4
     = 4
    >> DefaultValues[f]
     = {HoldPattern[Default[f, 1]] :> 4}

    You can assign values to 'DefaultValues':
    >> DefaultValues[g] = {Default[g] -> 3};
    >> Default[g, 1]
     = 3
    >> g[x_.] := {x}
    >> g[a]
     = {a}
    >> g[]
     = {3}
    """

    attributes = ("HoldAll",)

    def apply(self, symbol, evaluation):
        "DefaultValues[symbol_]"

        return get_symbol_values(symbol, "System`DefaultValues", "default", evaluation)


class AddTo(BinaryOperator):
    """
    <dl>
    <dt>'AddTo[$x$, $dx$]'</dt>
    <dt>'$x$ += $dx$'</dt>
        <dd>is equivalent to '$x$ = $x$ + $dx$'.
    </dl>

    >> a = 10;
    >> a += 2
     = 12
    >> a
     = 12
    """

    operator = "+="
    precedence = 100
    attributes = ("HoldFirst",)
    grouping = "Right"

    rules = {
        "x_ += dx_": "x = x + dx",
    }


class SubtractFrom(BinaryOperator):
    """
    <dl>
    <dt>'SubtractFrom[$x$, $dx$]'</dt>
    <dt>'$x$ -= $dx$'</dt>
        <dd>is equivalent to '$x$ = $x$ - $dx$'.
    </dl>

    >> a = 10;
    >> a -= 2
     = 8
    >> a
     = 8
    """

    operator = "-="
    precedence = 100
    attributes = ("HoldFirst",)
    grouping = "Right"

    rules = {
        "x_ -= dx_": "x = x - dx",
    }


class TimesBy(BinaryOperator):
    """
    <dl>
    <dt>'TimesBy[$x$, $dx$]'</dt>
    <dt>'$x$ *= $dx$'</dt>
        <dd>is equivalent to '$x$ = $x$ * $dx$'.
    </dl>

    >> a = 10;
    >> a *= 2
     = 20
    >> a
     = 20
    """

    operator = "*="
    precedence = 100
    attributes = ("HoldFirst",)
    grouping = "Right"

    rules = {
        "x_ *= dx_": "x = x * dx",
    }


class DivideBy(BinaryOperator):
    """
    <dl>
    <dt>'DivideBy[$x$, $dx$]'</dt>
    <dt>'$x$ /= $dx$'</dt>
        <dd>is equivalent to '$x$ = $x$ / $dx$'.
    </dl>

    >> a = 10;
    >> a /= 2
     = 5
    >> a
     = 5
    """

    operator = "/="
    precedence = 100
    attributes = ("HoldFirst",)
    grouping = "Right"

    rules = {
        "x_ /= dx_": "x = x / dx",
    }


class Increment(PostfixOperator):
    """
    <dl>
    <dt>'Increment[$x$]'</dt>
    <dt>'$x$++'</dt>
        <dd>increments $x$ by 1, returning the original value of $x$.
    </dl>

    >> a = 2;
    >> a++
     = 2
    >> a
     = 3
    Grouping of 'Increment', 'PreIncrement' and 'Plus':
    >> ++++a+++++2//Hold//FullForm
     = Hold[Plus[PreIncrement[PreIncrement[Increment[Increment[a]]]], 2]]
    """

    operator = "++"
    precedence = 660
    attributes = ("HoldFirst", "ReadProtected")

    rules = {
        "x_++": (
            "Module[{Internal`IncrementTemporary = x},"
            "       x = x + 1;"
            "       Internal`IncrementTemporary"
            "]"
        ),
    }


class PreIncrement(PrefixOperator):
    """
    <dl>
    <dt>'PreIncrement[$x$]'</dt>
    <dt>'++$x$'</dt>
        <dd>increments $x$ by 1, returning the new value of $x$.
    </dl>

    '++$a$' is equivalent to '$a$ = $a$ + 1':
    >> a = 2;
    >> ++a
     = 3
    >> a
     = 3
    """

    operator = "++"
    precedence = 660
    attributes = ("HoldFirst", "ReadProtected")

    rules = {
        "++x_": "x = x + 1",
    }


class Decrement(PostfixOperator):
    """
    <dl>
    <dt>'Decrement[$x$]'</dt>
    <dt>'$x$--'</dt>
        <dd>decrements $x$ by 1, returning the original value of $x$.
    </dl>

    >> a = 5;
    X> a--
     = 5
    X> a
     = 4
    """

    operator = "--"
    precedence = 660
    attributes = ("HoldFirst", "ReadProtected")

    rules = {
        "x_--": "Module[{t=x}, x = x - 1; t]",
    }


class PreDecrement(PrefixOperator):
    """
    <dl>
    <dt>'PreDecrement[$x$]'</dt>
    <dt>'--$x$'</dt>
        <dd>decrements $x$ by 1, returning the new value of $x$.
    </dl>

    '--$a$' is equivalent to '$a$ = $a$ - 1':
    >> a = 2;
    >> --a
     = 1
    >> a
     = 1
    """

    operator = "--"
    precedence = 660
    attributes = ("HoldFirst", "ReadProtected")

    rules = {
        "--x_": "x = x - 1",
    }


class LoadModule(Builtin):
    """
    <dl>
    <dt>'LoadModule[$module$]'</dt>
    <dd>'Load Mathics definitions from the python module $module$</dd>
    </dl>
    >> LoadModule["nomodule"]
     : Python module nomodule does not exist.
     = $Failed
    >> LoadModule["sys"]
     : Python module sys is not a pymathics module.
     = $Failed
    """

    name = "LoadModule"
    messages = {
        "notfound": "Python module `1` does not exist.",
        "notmathicslib": "Python module `1` is not a pymathics module.",
    }

    def apply(self, module, evaluation):
        "LoadModule[module_String]"
        try:
            module_loaded = evaluation.definitions.load_pymathics_module(module.value)
        except PyMathicsLoadException as e:
            evaluation.message(self.name, "notmathicslib", module)
            return SymbolFailed
        except ImportError as e:
            evaluation.message(self.get_name(), "notfound", module)
            return SymbolFailed
        else:
            # Add Pymathics` to $ContextPath so that when user don't
            # have to qualify Pymathics variables and functions,
            # as the those in the module just loaded.
            # Following the example of $ContextPath in the WL
            # reference manual where PackletManager appears first in
            # the list, it seems to be preferable to add this PyMathics
            # at the beginning.
            context_path = evaluation.definitions.get_context_path()
            if "Pymathics`" not in context_path:
                context_path.insert(0, "Pymathics`")
                evaluation.definitions.set_context_path(context_path)
        return module
