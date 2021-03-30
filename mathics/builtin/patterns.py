# -*- coding: utf-8 -*-

"""
Patterns and Rules

Some examples:
>> a + b + c /. a + b -> t
 = c + t
>> a + 2 + b + c + x * y /. n_Integer + s__Symbol + rest_ -> {n, s, rest}
 = {2, a, b + c + x y}
>> f[a, b, c, d] /. f[first_, rest___] -> {first, {rest}}
 = {a, {b, c, d}}

Tests and Conditions:
>> f[4] /. f[x_?(# > 0&)] -> x ^ 2
 = 16
>> f[4] /. f[x_] /; x > 0 -> x ^ 2
 = 16

Leaves in the beginning of a pattern rather match fewer leaves:
>> f[a, b, c, d] /. f[start__, end__] -> {{start}, {end}}
 = {{a}, {b, c, d}}

Optional arguments using 'Optional':
>> f[a] /. f[x_, y_:3] -> {x, y}
 = {a, 3}

Options using 'OptionsPattern' and 'OptionValue':
>> f[y, a->3] /. f[x_, OptionsPattern[{a->2, b->5}]] -> {x, OptionValue[a], OptionValue[b]}
 = {y, 3, 5}

The attributes 'Flat', 'Orderless', and 'OneIdentity' affect pattern matching.
"""


from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin, BinaryOperator, PostfixOperator
from mathics.builtin.base import PatternObject, PatternError
from mathics.builtin.lists import python_levelspec, InvalidLevelspecError

from mathics.core.expression import (
    Symbol,
    Expression,
    Number,
    Integer,
    Rational,
    Real,
    SymbolList,
)
from mathics.core.rules import Rule
from mathics.core.pattern import Pattern, StopGenerator


class Rule_(BinaryOperator):
    """
    <dl>
    <dt>'Rule[$x$, $y$]'
    <dt>'$x$ -> $y$'
        <dd>represents a rule replacing $x$ with $y$.
    </dl>

    >> a+b+c /. c->d
    = a + b + d
    >> {x,x^2,y} /. x->3
     = {3, 9, y}
    """

    # TODO: An error message should appear when Rule is called with a wrong
    # number of arguments
    """
    >> a /. Rule[1, 2, 3] -> t
     : Rule called with 3 arguments; 2 arguments are expected.
     = a
    """

    name = "Rule"
    operator = "->"
    precedence = 120
    attributes = ("SequenceHold",)
    grouping = "Right"
    needs_verbatim = True


class RuleDelayed(BinaryOperator):
    """
    <dl>
    <dt>'RuleDelayed[$x$, $y$]'
    <dt>'$x$ :> $y$'
        <dd>represents a rule replacing $x$ with $y$, with $y$ held
        unevaluated.
    </dl>

    >> Attributes[RuleDelayed]
     = {HoldRest, Protected, SequenceHold}
    """

    operator = ":>"
    precedence = 120
    attributes = ("SequenceHold", "HoldRest")
    needs_verbatim = True


def create_rules(rules_expr, expr, name, evaluation, extra_args=[]):
    if rules_expr.has_form("List", None):
        rules = rules_expr.leaves
    else:
        rules = [rules_expr]
    any_lists = any(item.has_form("List", None) for item in rules)
    if any_lists:
        all_lists = all(item.has_form("List", None) for item in rules)
        if all_lists:
            return (
                Expression(
                    "List",
                    *[Expression(name, expr, item, *extra_args) for item in rules]
                ),
                True,
            )
        else:
            evaluation.message(name, "rmix", rules_expr)
            return None, True
    else:
        result = []
        for rule in rules:
            if rule.get_head_name() not in ("System`Rule", "System`RuleDelayed"):
                evaluation.message(name, "reps", rule)
                return None, True
            elif len(rule.leaves) != 2:
                evaluation.message(
                    # TODO: shorten names here
                    rule.get_head_name(),
                    "argrx",
                    rule.get_head_name(),
                    3,
                    2,
                )
                return None, True
            else:
                result.append(Rule(rule.leaves[0], rule.leaves[1]))
        return result, False


class Replace(Builtin):
    """
    <dl>
    <dt>'Replace[$expr$, $x$ -> $y$]'
        <dd>yields the result of replacing $expr$ with $y$ if it
        matches the pattern $x$.
    <dt>'Replace[$expr$, $x$ -> $y$, $levelspec$]'
        <dd>replaces only subexpressions at levels specified through
        $levelspec$.
    <dt>'Replace[$expr$, {$x$ -> $y$, ...}]'
        <dd>performs replacement with multiple rules, yielding a
        single result expression.
    <dt>'Replace[$expr$, {{$a$ -> $b$, ...}, {$c$ -> $d$, ...}, ...}]'
        <dd>returns a list containing the result of performing each
        set of replacements.
    </dl>

    >> Replace[x, {x -> 2}]
     = 2

    By default, only the top level is searched for matches
    >> Replace[1 + x, {x -> 2}]
     = 1 + x

    >> Replace[x, {{x -> 1}, {x -> 2}}]
     = {1, 2}

    Replace stops after the first replacement
    >> Replace[x, {x -> {}, _List -> y}]
     = {}

    Replace replaces the deepest levels first
    >> Replace[x[1], {x[1] -> y, 1 -> 2}, All]
     = x[2]

    By default, heads are not replaced
    >> Replace[x[x[y]], x -> z, All]
     = x[x[y]]

    Heads can be replaced using the Heads option
    >> Replace[x[x[y]], x -> z, All, Heads -> True]
     = z[z[y]]

    Note that heads are handled at the level of leaves
    >> Replace[x[x[y]], x -> z, {1}, Heads -> True]
     = z[x[y]]

    You can use Replace as an operator
    >> Replace[{x_ -> x + 1}][10]
     = 11
    """

    messages = {
        "reps": "`1` is not a valid replacement rule.",
        "rmix": "Elements of `1` are a mixture of lists and nonlists.",
    }

    rules = {"Replace[rules_][expr_]": "Replace[expr, rules]"}

    options = {"Heads": "False"}

    def apply_levelspec(self, expr, rules, ls, evaluation, options):
        "Replace[expr_, rules_, Optional[Pattern[ls, _?LevelQ], {0}], OptionsPattern[Replace]]"
        try:
            rules, ret = create_rules(rules, expr, "Replace", evaluation)
            if ret:
                return rules

            heads = self.get_option(options, "Heads", evaluation).is_true()

            result, applied = expr.apply_rules(
                rules,
                evaluation,
                level=0,
                options={"levelspec": python_levelspec(ls), "heads": heads},
            )
            return result
        except InvalidLevelspecError:
            evaluation.message("General", "level", ls)

        except PatternError:
            evaluation.message("Replace", "reps", rules)


class ReplaceAll(BinaryOperator):
    """
    <dl>
    <dt>'ReplaceAll[$expr$, $x$ -> $y$]'
    <dt>'$expr$ /. $x$ -> $y$'
        <dd>yields the result of replacing all subexpressions of
        $expr$ matching the pattern $x$ with $y$.
    <dt>'$expr$ /. {$x$ -> $y$, ...}'
        <dd>performs replacement with multiple rules, yielding a
        single result expression.
    <dt>'$expr$ /. {{$a$ -> $b$, ...}, {$c$ -> $d$, ...}, ...}'
        <dd>returns a list containing the result of performing each
        set of replacements.
    </dl>

    >> a+b+c /. c->d
     = a + b + d
    >> g[a+b+c,a]/.g[x_+y_,x_]->{x,y}
     = {a, b + c}

    If $rules$ is a list of lists, a list of all possible respective
    replacements is returned:
    >> {a, b} /. {{a->x, b->y}, {a->u, b->v}}
     = {{x, y}, {u, v}}
    The list can be arbitrarily nested:
    >> {a, b} /. {{{a->x, b->y}, {a->w, b->z}}, {a->u, b->v}}
     = {{{x, y}, {w, z}}, {u, v}}
    >> {a, b} /. {{{a->x, b->y}, a->w, b->z}, {a->u, b->v}}
     : Elements of {{a -> x, b -> y}, a -> w, b -> z} are a mixture of lists and nonlists.
     = {{a, b} /. {{a -> x, b -> y}, a -> w, b -> z}, {u, v}}

    ReplaceAll also can be used as an operator:
    >> ReplaceAll[{a -> 1}][{a, b}]
     = {1, b}

    #> a + b /. x_ + y_ -> {x, y}
     = {a, b}

    ReplaceAll replaces the shallowest levels first:
    >> ReplaceAll[x[1], {x[1] -> y, 1 -> 2}]
     = y
    """

    operator = "/."
    precedence = 110
    grouping = "Left"
    needs_verbatim = True

    messages = {
        "reps": "`1` is not a valid replacement rule.",
        "rmix": "Elements of `1` are a mixture of lists and nonlists.",
    }

    rules = {"ReplaceAll[rules_][expr_]": "ReplaceAll[expr, rules]"}

    def apply(self, expr, rules, evaluation):
        "ReplaceAll[expr_, rules_]"
        try:
            rules, ret = create_rules(rules, expr, "ReplaceAll", evaluation)

            if ret:
                return rules

            result, applied = expr.apply_rules(rules, evaluation)
            return result
        except PatternError:
            evaluation.message("Replace", "reps", rules)


class ReplaceRepeated(BinaryOperator):
    """
    <dl>
    <dt>'ReplaceRepeated[$expr$, $x$ -> $y$]'
    <dt>'$expr$ //. $x$ -> $y$'
        <dd>repeatedly applies the rule '$x$ -> $y$' to $expr$ until
        the result no longer changes.
    </dl>

    >> a+b+c //. c->d
     = a + b + d

    Simplification of logarithms:
    >> logrules = {Log[x_ * y_] :> Log[x] + Log[y], Log[x_ ^ y_] :> y * Log[x]};
    >> Log[a * (b * c) ^ d ^ e * f] //. logrules
     = Log[a] + Log[f] + (Log[b] + Log[c]) d ^ e
    'ReplaceAll' just performs a single replacement:
    >> Log[a * (b * c) ^ d ^ e * f] /. logrules
     = Log[a] + Log[f (b c) ^ d ^ e]
    """

    operator = "//."
    precedence = 110
    grouping = "Left"
    needs_verbatim = True

    messages = {
        "reps": "`1` is not a valid replacement rule.",
        "rmix": "Elements of `1` are a mixture of lists and nonlists.",
    }

    def apply_list(self, expr, rules, evaluation):
        "ReplaceRepeated[expr_, rules_]"
        try:
            rules, ret = create_rules(rules, expr, "ReplaceRepeated", evaluation)
        except PatternError:
            evaluation.message("Replace", "reps", rules)
            return None

        if ret:
            return rules

        while True:
            evaluation.check_stopped()
            result, applied = expr.apply_rules(rules, evaluation)
            if applied:
                result = result.evaluate(evaluation)
            if applied and not result.same(expr):
                expr = result
            else:
                break

        return result


class ReplaceList(Builtin):
    """
    <dl>
    <dt>'ReplaceList[$expr$, $rules$]'
        <dd>returns a list of all possible results of applying $rules$
        to $expr$.
    </dl>

    Get all subsequences of a list:
    >> ReplaceList[{a, b, c}, {___, x__, ___} -> {x}]
     = {{a}, {a, b}, {a, b, c}, {b}, {b, c}, {c}}
    You can specify the maximum number of items:
    >> ReplaceList[{a, b, c}, {___, x__, ___} -> {x}, 3]
     = {{a}, {a, b}, {a, b, c}}
    >> ReplaceList[{a, b, c}, {___, x__, ___} -> {x}, 0]
     = {}
    If no rule matches, an empty list is returned:
    >> ReplaceList[a, b->x]
     = {}

    Like in 'ReplaceAll', $rules$ can be a nested list:
    >> ReplaceList[{a, b, c}, {{{___, x__, ___} -> {x}}, {{a, b, c} -> t}}, 2]
     = {{{a}, {a, b}}, {t}}
    >> ReplaceList[expr, {}, -1]
     : Non-negative integer or Infinity expected at position 3.
     = ReplaceList[expr, {}, -1]

    Possible matches for a sum:
    >> ReplaceList[a + b + c, x_ + y_ -> {x, y}]
     = {{a, b + c}, {b, a + c}, {c, a + b}, {a + b, c}, {a + c, b}, {b + c, a}}
    """

    messages = {
        "reps": "`1` is not a valid replacement rule.",
        "rmix": "Elements of `1` are a mixture of lists and nonlists.",
    }

    def apply(self, expr, rules, max, evaluation):
        "ReplaceList[expr_, rules_, max_:Infinity]"

        if max.get_name() == "System`Infinity":
            max_count = None
        else:
            max_count = max.get_int_value()
            if max_count is None or max_count < 0:
                evaluation.message("ReplaceList", "innf", 3)
                return
        try:
            rules, ret = create_rules(
                rules, expr, "ReplaceList", evaluation, extra_args=[max]
            )
        except PatternError:
            evaluation.message("Replace", "reps", rules)
            return None

        if ret:
            return rules

        list = []
        for rule in rules:
            result = rule.apply(expr, evaluation, return_list=True, max_list=max_count)
            list.extend(result)

        return Expression(SymbolList, *list)


class PatternTest(BinaryOperator, PatternObject):
    """
    <dl>
    <dt>'PatternTest[$pattern$, $test$]'
    <dt>'$pattern$ ? $test$'
        <dd>constrains $pattern$ to match $expr$ only if the
        evaluation of '$test$[$expr$]' yields 'True'.
    </dl>

    >> MatchQ[3, _Integer?(#>0&)]
     = True
    >> MatchQ[-3, _Integer?(#>0&)]
     = False
    """

    operator = "?"
    precedence = 680

    arg_counts = [2]

    def init(self, expr):
        super(PatternTest, self).init(expr)
        self.pattern = Pattern.create(expr.leaves[0])
        self.test = expr.leaves[1]
        self.test_name = self.test.get_name()

    def quick_pattern_test(self, candidate, test, evaluation):
        if test == "System`NumberQ":
            return isinstance(candidate, Number)
        elif test == "System`Negative":
            if isinstance(candidate, (Integer, Rational, Real)):
                return candidate.value < 0
            return False
            # pass
        elif test == "System`NegativePowerQ":
            return (
                candidate.has_form("Power", 2)
                and isinstance(candidate.leaves[1], (Integer, Rational, Real))
                and candidate.leaves[1].value < 0
            )
        elif test == "System`NotNegativePowerQ":
            return not (
                candidate.has_form("Power", 2)
                and isinstance(candidate.leaves[1], (Integer, Rational, Real))
                and candidate.leaves[1].value < 0
            )
        else:
            from mathics.builtin.base import Test

            builtin = None
            builtin = evaluation.definitions.get_definition(test)
            if builtin:
                builtin = builtin.builtin
            if builtin is not None and isinstance(builtin, Test):
                return builtin.test(candidate)
        return None

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        # for vars_2, rest in self.pattern.match(expression, vars, evaluation):
        def yield_match(vars_2, rest):
            items = expression.get_sequence()
            for item in items:
                item = item.evaluate(evaluation)
                quick_test = self.quick_pattern_test(item, self.test_name, evaluation)
                if quick_test is not None:
                    if not quick_test:
                        break
                        # raise StopGenerator
                else:
                    test_expr = Expression(self.test, item)
                    test_value = test_expr.evaluate(evaluation)
                    if not test_value.is_true():
                        break
                        # raise StopGenerator
            else:
                yield_func(vars_2, None)

        # try:
        self.pattern.match(yield_match, expression, vars, evaluation)
        # except StopGenerator:
        #    pass

    def get_match_count(self, vars={}):
        return self.pattern.get_match_count(vars)


class Alternatives(BinaryOperator, PatternObject):
    """
    <dl>
    <dt>'Alternatives[$p1$, $p2$, ..., $p_i$]'
    <dt>'$p1$ | $p2$ | ... | $p_i$'
        <dd>is a pattern that matches any of the patterns '$p1$, $p2$,
        ...., $p_i$'.
    </dl>

    >> a+b+c+d/.(a|b)->t
     = c + d + 2 t

    Alternatives can also be used for string expressions
    >> StringReplace["0123 3210", "1" | "2" -> "X"]
     = 0XX3 3XX0

    #> StringReplace["h1d9a f483", DigitCharacter | WhitespaceCharacter -> ""]
     = hdaf
    """

    operator = "|"
    precedence = 160
    needs_verbatim = True

    arg_counts = None

    def init(self, expr):
        super(Alternatives, self).init(expr)
        self.alternatives = [Pattern.create(leaf) for leaf in expr.leaves]

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        for alternative in self.alternatives:
            # for new_vars, rest in alternative.match(
            #     expression, vars, evaluation):
            #     yield_func(new_vars, rest)
            alternative.match(yield_func, expression, vars, evaluation)

    def get_match_count(self, vars={}):
        range = None
        for alternative in self.alternatives:
            sub = alternative.get_match_count(vars)
            if range is None:
                range = list(sub)
            else:
                if sub[0] < range[0]:
                    range[0] = sub[0]
                if range[1] is None or sub[1] > range[1]:
                    range[1] = sub[1]
        return range


class _StopGeneratorExcept(StopGenerator):
    pass


class Except(PatternObject):
    """
    <dl>
    <dt>'Except[$c$]'
        <dd>represents a pattern object that matches any expression except those matching $c$.
    <dt>'Except[$c$, $p$]'
        <dd>represents a pattern object that matches $p$ but not $c$.
    </dl>

    >> Cases[{x, a, b, x, c}, Except[x]]
     = {a, b, c}

    >> Cases[{a, 0, b, 1, c, 2, 3}, Except[1, _Integer]]
     = {0, 2, 3}

    Except can also be used for string expressions:
    >> StringReplace["Hello world!", Except[LetterCharacter] -> ""]
     = Helloworld

    #> StringReplace["abc DEF 123!", Except[LetterCharacter, WordCharacter] -> "0"]
     = abc DEF 000!
    """

    arg_counts = [1, 2]

    def init(self, expr):
        super(Except, self).init(expr)
        self.c = Pattern.create(expr.leaves[0])
        if len(expr.leaves) == 2:
            self.p = Pattern.create(expr.leaves[1])
        else:
            self.p = Pattern.create(Expression("Blank"))

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        def except_yield_func(vars, rest):
            raise _StopGeneratorExcept(True)

        try:
            self.c.match(except_yield_func, expression, vars, evaluation)
        except _StopGeneratorExcept:
            pass
        else:
            self.p.match(yield_func, expression, vars, evaluation)


class _StopGeneratorMatchQ(StopGenerator):
    pass


class Matcher(object):
    def __init__(self, form):
        self.form = Pattern.create(form)

    def match(self, expr, evaluation):
        def yield_func(vars, rest):
            raise _StopGeneratorMatchQ(True)

        try:
            self.form.match(yield_func, expr, {}, evaluation)
        except _StopGeneratorMatchQ:
            return True
        return False


def match(expr, form, evaluation):
    return Matcher(form).match(expr, evaluation)


class MatchQ(Builtin):
    """
    <dl>
    <dt>'MatchQ[$expr$, $form$]'
        <dd>tests whether $expr$ matches $form$.
    </dl>

    >> MatchQ[123, _Integer]
     = True
    >> MatchQ[123, _Real]
     = False
    >> MatchQ[_Integer][123]
     = True
    """

    rules = {"MatchQ[form_][expr_]": "MatchQ[expr, form]"}

    def apply(self, expr, form, evaluation):
        "MatchQ[expr_, form_]"

        if match(expr, form, evaluation):
            return Symbol("True")
        return Symbol("False")


class Verbatim(PatternObject):
    """
    <dl>
    <dt>'Verbatim[$expr$]'
        <dd>prevents pattern constructs in $expr$ from taking effect,
        allowing them to match themselves.
    </dl>

    Create a pattern matching 'Blank':
    >> _ /. Verbatim[_]->t
     = t
    >> x /. Verbatim[_]->t
     = x

    Without 'Verbatim', 'Blank' has its normal effect:
    >> x /. _->t
     = t
    """

    arg_counts = [1, 2]

    def init(self, expr):
        super(Verbatim, self).init(expr)
        self.content = expr.leaves[0]

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        if self.content.same(expression):
            yield_func(vars, None)


class HoldPattern(PatternObject):
    """
    <dl>
    <dt>'HoldPattern[$expr$]'
        <dd>is equivalent to $expr$ for pattern matching, but
        maintains it in an unevaluated form.
    </dl>

    >> HoldPattern[x + x]
     = HoldPattern[x + x]
    >> x /. HoldPattern[x] -> t
     = t

    'HoldPattern' has attribute 'HoldAll':
    >> Attributes[HoldPattern]
     = {HoldAll, Protected}
    """

    attributes = ("HoldAll",)

    arg_counts = [1]

    def init(self, expr):
        super(HoldPattern, self).init(expr)
        self.pattern = Pattern.create(expr.leaves[0])

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        # for new_vars, rest in self.pattern.match(
        #     expression, vars, evaluation):
        #     yield new_vars, rest
        self.pattern.match(yield_func, expression, vars, evaluation)


class Pattern_(PatternObject):
    """
    <dl>
    <dt>'Pattern[$symb$, $patt$]'
    <dt>'$symb$ : $patt$'
        <dd>assigns the name $symb$ to the pattern $patt$.
    <dt>'$symb$_$head$'
        <dd>is equivalent to '$symb$ : _$head$' (accordingly with '__'
        and '___').
    <dt>'$symb$ : $patt$ : $default$'
        <dd>is a pattern with name $symb$ and default value $default$,
        equivalent to 'Optional[$patt$ : $symb$, $default$]'.
    </dl>

    >> FullForm[a_b]
     = Pattern[a, Blank[b]]
    >> FullForm[a:_:b]
     = Optional[Pattern[a, Blank[]], b]

    'Pattern' has attribute 'HoldFirst', so it does not evaluate its name:
    >> x = 2
     = 2
    >> x_
     = x_

    Nested 'Pattern' assign multiple names to the same pattern. Still,
    the last parameter is the default value.
    >> f[y] /. f[a:b,_:d] -> {a, b}
     = f[y]
    This is equivalent to:
    >> f[a] /. f[a:_:b] -> {a, b}
     = {a, b}
    'FullForm':
    >> FullForm[a:b:c:d:e]
     = Optional[Pattern[a, b], Optional[Pattern[c, d], e]]

    >> f[] /. f[a:_:b] -> {a, b}
     = {b, b}
    """

    name = "Pattern"

    arg_counts = [2]

    attributes = ("HoldFirst",)

    messages = {
        "patvar": "First element in pattern `1` is not a valid pattern name.",
        "nodef": (
            "No default setting found for `1` in " "position `2` when length is `3`."
        ),
    }

    rules = {
        "MakeBoxes[Verbatim[Pattern][symbol_Symbol, blank_Blank|blank_BlankSequence|blank_BlankNullSequence], f:StandardForm|TraditionalForm|InputForm|OutputForm]": "MakeBoxes[symbol, f] <> MakeBoxes[blank, f]",
        # 'StringForm["`1``2`", HoldForm[symbol], blank]',
    }

    formats = {
        "Verbatim[Pattern][symbol_, "
        "pattern_?(!MatchQ[#, _Blank|_BlankSequence|_BlankNullSequence]&)]": (
            'Infix[{symbol, pattern}, ":", 150, Left]'
        )
    }

    def init(self, expr):
        super(Pattern_, self).init(expr)
        self.varname = expr.leaves[0].get_name()
        if self.varname is None:
            self.error("patvar", expr)
        self.pattern = Pattern.create(expr.leaves[1])

    def __repr__(self):
        return "<Pattern: %s>" % repr(self.pattern)

    def get_match_count(self, vars={}):
        return self.pattern.get_match_count(vars)

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        existing = vars.get(self.varname, None)
        if existing is None:
            new_vars = vars.copy()
            new_vars[self.varname] = expression
            # for vars_2, rest in self.pattern.match(
            #    expression, new_vars, evaluation):
            #    yield vars_2, rest
            if type(self.pattern) is OptionsPattern:
                self.pattern.match(
                    yield_func, expression, new_vars, evaluation, **kwargs
                )
            else:
                self.pattern.match(yield_func, expression, new_vars, evaluation)
        else:
            if existing.same(expression):
                yield_func(vars, None)

    def get_match_candidates(self, leaves, expression, attributes, evaluation, vars={}):
        existing = vars.get(self.varname, None)
        if existing is None:
            return self.pattern.get_match_candidates(
                leaves, expression, attributes, evaluation, vars
            )
        else:
            # Treat existing variable as verbatim
            verbatim_expr = Expression("Verbatim", existing)
            verbatim = Verbatim(verbatim_expr)
            return verbatim.get_match_candidates(
                leaves, expression, attributes, evaluation, vars
            )


class Optional(BinaryOperator, PatternObject):
    """
    <dl>
    <dt>'Optional[$patt$, $default$]'
    <dt>'$patt$ : $default$'
        <dd>is a pattern which matches $patt$, which if omitted
        should be replaced by $default$.
    </dl>

    >> f[x_, y_:1] := {x, y}
    >> f[1, 2]
     = {1, 2}
    >> f[a]
     = {a, 1}

    Note that '$symb$ : $patt$' represents a 'Pattern' object. However, there is no
    disambiguity, since $symb$ has to be a symbol in this case.

    >> x:_ // FullForm
     = Pattern[x, Blank[]]
    >> _:d // FullForm
     = Optional[Blank[], d]
    >> x:_+y_:d // FullForm
     = Pattern[x, Plus[Blank[], Optional[Pattern[y, Blank[]], d]]]

    's_.' is equivalent to 'Optional[s_]' and represents an optional parameter which, if omitted,
    gets its value from 'Default'.
    >> FullForm[s_.]
     = Optional[Pattern[s, Blank[]]]

    >> Default[h, k_] := k
    >> h[a] /. h[x_, y_.] -> {x, y}
     = {a, 2}

    #> a:b:c
     = a : b : c
    #> FullForm[a:b:c]
     = Optional[Pattern[a, b], c]
    #> (a:b):c
     = a : b : c
    #> a:(b:c)
     = a : (b : c)
    """

    operator = ":"
    precedence = 140
    grouping = "Right"

    default_formats = False

    rules = {
        "MakeBoxes[Verbatim[Optional][Verbatim[Pattern][symbol_Symbol, Verbatim[_]]], f:StandardForm|TraditionalForm|InputForm|OutputForm]": 'MakeBoxes[symbol, f] <> "_."',
        "MakeBoxes[Verbatim[Optional][Verbatim[_]], f:StandardForm|TraditionalForm|InputForm|OutputForm]": '"_."',
    }

    formats = {
        "Verbatim[Optional][pattern_Pattern, default_]": 'Infix[{HoldForm[pattern], HoldForm[default]}, ":", 140, Right]'
    }

    arg_counts = [1, 2]

    def init(self, expr):
        super(Optional, self).init(expr)
        self.pattern = Pattern.create(expr.leaves[0])
        if len(expr.leaves) == 2:
            self.default = expr.leaves[1]
        else:
            self.default = None

    def match(
        self,
        yield_func,
        expression,
        vars,
        evaluation,
        head=None,
        leaf_index=None,
        leaf_count=None,
        **kwargs
    ):
        if expression.has_form("Sequence", 0):
            if self.default is None:
                if head is None:  # head should be given by match_leaf!
                    default = None
                else:
                    name = head.get_name()
                    default = get_default_value(
                        name, evaluation, leaf_index, leaf_count
                    )
                if default is None:
                    evaluation.message("Pattern", "nodef", head, leaf_index, leaf_count)
                    return
            else:
                default = self.default

            expression = default
        # for vars_2, rest in self.pattern.match(expression, vars, evaluation):
        #    yield vars_2, rest
        self.pattern.match(yield_func, expression, vars, evaluation)

    def get_match_count(self, vars={}):
        return (0, 1)


def get_default_value(name, evaluation, k=None, n=None):
    pos = []
    if k is not None:
        pos.append(k)
    if n is not None:
        pos.append(n)
    for pos_len in reversed(range(len(pos) + 1)):
        # Try patterns from specific to general
        defaultexpr = Expression(
            "Default", Symbol(name), *[Integer(index) for index in pos[:pos_len]]
        )
        result = evaluation.definitions.get_value(
            name, "System`DefaultValues", defaultexpr, evaluation
        )
        if result is not None:
            if result.same(defaultexpr):
                result = result.evaluate(evaluation)
            return result
    return None


class _Blank(PatternObject):
    arg_counts = [0, 1]

    def init(self, expr):
        super(_Blank, self).init(expr)
        if expr.leaves:
            self.head = expr.leaves[0]
        else:
            self.head = None


class Blank(_Blank):
    """
    <dl>
    <dt>'Blank[]'
    <dt>'_'
        <dd>represents any single expression in a pattern.
    <dt>'Blank[$h$]'
    <dt>'_$h$'
        <dd>represents any expression with head $h$.
    </dl>

    >> MatchQ[a + b, _]
     = True

    Patterns of the form '_'$h$ can be used to test the types of
    objects:
    >> MatchQ[42, _Integer]
     = True
    >> MatchQ[1.0, _Integer]
     = False
    >> {42, 1.0, x} /. {_Integer -> "integer", _Real -> "real"} // InputForm
     = {"integer", "real", x}

    'Blank' only matches a single expression:
    >> MatchQ[f[1, 2], f[_]]
     = False

    #> StringReplace["hello world!", _ -> "x"]
     = xxxxxxxxxxxx
    """

    rules = {
        "MakeBoxes[Verbatim[Blank][], f:StandardForm|TraditionalForm|OutputForm|InputForm]": '"_"',
        "MakeBoxes[Verbatim[Blank][head_Symbol], f:StandardForm|TraditionalForm|OutputForm|InputForm]": '"_" <> MakeBoxes[head, f]',
    }

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        if not expression.has_form("Sequence", 0):
            if self.head is not None:
                if expression.get_head().same(self.head):
                    yield_func(vars, None)
            else:
                yield_func(vars, None)


class BlankSequence(_Blank):
    """
    <dl>
    <dt>'BlankSequence[]'
    <dt>'__'
        <dd>represents any non-empty sequence of expression leaves in
        a pattern.
    <dt>'BlankSequence[$h$]'
    <dt>'__$h$'
        <dd>represents any sequence of leaves, all of which have head $h$.
    </dl>

    Use a 'BlankSequence' pattern to stand for a non-empty sequence of
    arguments:
    >> MatchQ[f[1, 2, 3], f[__]]
     = True
    >> MatchQ[f[], f[__]]
     = False

    '__'$h$ will match only if all leaves have head $h$:
    >> MatchQ[f[1, 2, 3], f[__Integer]]
     = True
    >> MatchQ[f[1, 2.0, 3], f[__Integer]]
     = False

    The value captured by a named 'BlankSequence' pattern is a
    'Sequence' object:
    >> f[1, 2, 3] /. f[x__] -> x
     = Sequence[1, 2, 3]

    #> f[a, b, c, d] /. f[x__, c, y__] -> {{x},{y}}
     = {{a, b}, {d}}
    #> a + b + c + d /. Plus[x__, c] -> {x}
     = {a, b, d}

    #> StringReplace[{"ab", "abc", "abcd"}, "b" ~~ __ -> "x"]
     = {ab, ax, ax}
    """

    rules = {
        "MakeBoxes[Verbatim[BlankSequence][], f:StandardForm|TraditionalForm|OutputForm|InputForm]": '"__"',
        "MakeBoxes[Verbatim[BlankSequence][head_Symbol], f:StandardForm|TraditionalForm|OutputForm|InputForm]": '"__" <> MakeBoxes[head, f]',
    }

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        leaves = expression.get_sequence()
        if not leaves:
            return
        if self.head:
            ok = True
            for leaf in leaves:
                if leaf.get_head() != self.head:
                    ok = False
                    break
            if ok:
                yield_func(vars, None)
        else:
            yield_func(vars, None)

    def get_match_count(self, vars={}):
        return (1, None)


class BlankNullSequence(_Blank):
    """
    <dl>
    <dt>'BlankNullSequence[]'
    <dt>'___'
        <dd>represents any sequence of expression leaves in a pattern,
        including an empty sequence.
    </dl>

    'BlankNullSequence' is like 'BlankSequence', except it can match an
    empty sequence:
    >> MatchQ[f[], f[___]]
     = True

    ## This test hits infinite recursion
    ##
    ##The value captured by a named 'BlankNullSequence' pattern is a
    ##'Sequence' object, which can have no leaves:
    ##>> f[] /. f[x___] -> x
    ## = Sequence[]

    #> ___symbol
     = ___symbol
    #> ___symbol //FullForm
     = BlankNullSequence[symbol]

    #> StringReplace[{"ab", "abc", "abcd"}, "b" ~~ ___ -> "x"]
     = {ax, ax, ax}
    """

    rules = {
        "MakeBoxes[Verbatim[BlankNullSequence][], f:StandardForm|TraditionalForm|OutputForm|InputForm]": '"___"',
        "MakeBoxes[Verbatim[BlankNullSequence][head_Symbol], f:StandardForm|TraditionalForm|OutputForm|InputForm]": '"___" <> MakeBoxes[head, f]',
    }

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        leaves = expression.get_sequence()
        if self.head:
            ok = True
            for leaf in leaves:
                if leaf.get_head() != self.head:
                    ok = False
                    break
            if ok:
                yield_func(vars, None)
        else:
            yield_func(vars, None)

    def get_match_count(self, vars={}):
        return (0, None)


class Repeated(PostfixOperator, PatternObject):
    """
    <dl>
    <dt>'Repeated[$pattern$]'
        <dd>matches one or more occurrences of $pattern$.
    </dl>

    >> a_Integer.. // FullForm
     = Repeated[Pattern[a, Blank[Integer]]]
    >> 0..1//FullForm
     = Repeated[0]
    >> {{}, {a}, {a, b}, {a, a, a}, {a, a, a, a}} /. {Repeated[x : a | b, 3]} -> x
     = {{}, a, {a, b}, a, {a, a, a, a}}
    >> f[x, 0, 0, 0] /. f[x, s:0..] -> s
     = Sequence[0, 0, 0]

    #> 1.. // FullForm
     = Repeated[1]
    #> 8^^1.. // FullForm   (* Mathematica gets this wrong *)
     = Repeated[1]

    #> StringReplace["010110110001010", "01".. -> "a"]
     = a1a100a0
    #> StringMatchQ[#, "a" ~~ ("b"..) ~~ "a"] &/@ {"aa", "aba", "abba"}
     = {False, True, True}
    """

    messages = {
        "range": (
            "Range specification in integers (max or {min, max}) "
            "expected at position `1` in `2`."
        )
    }

    operator = ".."
    precedence = 170

    arg_counts = [1, 2]

    def init(self, expr, min=1):
        self.pattern = Pattern.create(expr.leaves[0])

        self.max = None
        self.min = min
        if len(expr.leaves) == 2:
            leaf_1 = expr.leaves[1]
            allnumbers = not any(
                leaf.get_int_value() is None for leaf in leaf_1.get_leaves()
            )
            if leaf_1.has_form("List", 1, 2) and allnumbers:
                self.max = leaf_1.leaves[-1].get_int_value()
                self.min = leaf_1.leaves[0].get_int_value()
            elif leaf_1.get_int_value():
                self.max = leaf_1.get_int_value()
            else:
                self.error("range", 2, expr)

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        leaves = expression.get_sequence()
        if len(leaves) < self.min:
            return
        if self.max is not None and len(leaves) > self.max:
            return

        def iter(yield_iter, rest_leaves, vars):
            if rest_leaves:
                # for new_vars, rest in self.pattern.match(rest_leaves[0],
                # vars, evaluation):
                def yield_match(new_vars, rest):
                    # for sub_vars, sub_rest in iter(rest_leaves[1:],
                    #                                new_vars):
                    #    yield sub_vars, rest
                    iter(yield_iter, rest_leaves[1:], new_vars)

                self.pattern.match(yield_match, rest_leaves[0], vars, evaluation)
            else:
                yield_iter(vars, None)

        # for vars, rest in iter(leaves, vars):
        #    yield_func(vars, rest)
        iter(yield_func, leaves, vars)

    def get_match_count(self, vars={}):
        return (self.min, self.max)


class RepeatedNull(Repeated):
    """
    <dl>
    <dt>'RepeatedNull[$pattern$]'
        <dd>matches zero or more occurrences of $pattern$.
    </dl>

    >> a___Integer...//FullForm
     = RepeatedNull[Pattern[a, BlankNullSequence[Integer]]]
    >> f[x] /. f[x, 0...] -> t
     = t

    #> 1... // FullForm
     = RepeatedNull[1]
    #> 8^^1... // FullForm   (* Mathematica gets this wrong *)
     = RepeatedNull[1]

    #> StringMatchQ[#, "a" ~~ ("b"...) ~~ "a"] &/@ {"aa", "aba", "abba"}
     = {True, True, True}
    """

    operator = "..."
    precedence = 170

    def init(self, expr):
        super(RepeatedNull, self).init(expr, min=0)


class Shortest(Builtin):
    pass


class Longest(Builtin):
    """
    >> StringCases["aabaaab", Longest["a" ~~ __ ~~ "b"]]
     = {aabaaab}

    >> StringCases["aabaaab", Longest[RegularExpression["a+b"]]]
     = {aab, aaab}
    """

    pass


class Condition(BinaryOperator, PatternObject):
    """
    <dl>
    <dt>'Condition[$pattern$, $expr$]'
    <dt>'$pattern$ /; $expr$'
        <dd>places an additional constraint on $pattern$ that only
        allows it to match if $expr$ evaluates to 'True'.
    </dl>

    The controlling expression of a 'Condition' can use variables from
    the pattern:
    >> f[3] /. f[x_] /; x>0 -> t
     = t
    >> f[-3] /. f[x_] /; x>0 -> t
     = f[-3]

    'Condition' can be used in an assignment:
    >> f[x_] := p[x] /; x>0
    >> f[3]
     = p[3]
    >> f[-3]
     = f[-3]
    """

    operator = "/;"
    precedence = 130

    # Don't know why this has attribute HoldAll in Mathematica
    attributes = ("HoldRest",)

    arg_counts = [2]

    def init(self, expr):
        super(Condition, self).init(expr)
        self.test = expr.leaves[1]
        # if (expr.leaves[0].get_head_name() == "System`Condition" and
        #    len(expr.leaves[0].leaves) == 2):
        #    self.test = Expression("And", self.test, expr.leaves[0].leaves[1])
        #    self.pattern = Pattern.create(expr.leaves[0].leaves[0])
        # else:
        self.pattern = Pattern.create(expr.leaves[0])

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        # for new_vars, rest in self.pattern.match(expression, vars,
        # evaluation):
        def yield_match(new_vars, rest):
            test_expr = self.test.replace_vars(new_vars)
            test_result = test_expr.evaluate(evaluation)
            if test_result.is_true():
                yield_func(new_vars, rest)

        self.pattern.match(yield_match, expression, vars, evaluation)


class OptionsPattern(PatternObject):
    """
    <dl>
    <dt>'OptionsPattern[$f$]'
        <dd>is a pattern that stands for a sequence of options given
        to a function, with default values taken from 'Options[$f$]'.
        The options can be of the form '$opt$->$value$' or
        '$opt$:>$value$', and might be in arbitrarily nested lists.
    <dt>'OptionsPattern[{$opt1$->$value1$, ...}]'
        <dd>takes explicit default values from the given list. The
        list may also contain symbols $f$, for which 'Options[$f$]' is
        taken into account; it may be arbitrarily nested.
        'OptionsPattern[{}]' does not use any default values.
    </dl>

    The option values can be accessed using 'OptionValue'.

    >> f[x_, OptionsPattern[{n->2}]] := x ^ OptionValue[n]
    >> f[x]
     = x ^ 2
    >> f[x, n->3]
     = x ^ 3

    Delayed rules as options:
    >> e = f[x, n:>a]
     = x ^ a
    >> a = 5;
    >> e
     = x ^ 5

    Options might be given in nested lists:
    >> f[x, {{{n->4}}}]
     = x ^ 4

    #> {opt -> b} /. OptionsPattern[{}] -> t
     = t

    #> Clear[f]
    #> Options[f] = {Power -> 2};
    #> f[x_, OptionsPattern[f]] := x ^ OptionValue[Power]
    #> f[10]
     = 100
    #> f[10, Power -> 3]
     = 1000
    #> Clear[f]

    #> Options[f] = {Power -> 2};
    #> f[x_, OptionsPattern[]] := x ^ OptionValue[Power]
    #> f[10]
     = 100
    #> f[10, Power -> 3]
     = 1000
    #> Clear[f]
    """

    arg_counts = [0, 1]

    def init(self, expr):
        super(OptionsPattern, self).init(expr)
        try:
            self.defaults = expr.leaves[0]
        except IndexError:
            # OptionsPattern[] takes default options of the nearest enclosing
            # function. Set to not None in self.match
            self.defaults = None

    def match(self, yield_func, expression, vars, evaluation, **kwargs):
        if self.defaults is None:
            self.defaults = kwargs.get("head")
            if self.defaults is None:
                # we end up here with OptionsPattern that do not have any
                # default options defined, e.g. with this code:
                # f[x:OptionsPattern[]] := x; f["Test" -> 1]
                # set self.defaults to an empty List, so we don't crash.
                self.defaults = Expression(SymbolList)
        values = self.defaults.get_option_values(
            evaluation, allow_symbols=True, stop_on_error=False
        )
        sequence = expression.get_sequence()
        for options in sequence:
            option_values = options.get_option_values(evaluation)
            if option_values is None:
                return
            values.update(option_values)
        new_vars = vars.copy()
        for name, value in values.items():
            new_vars["_option_" + name] = value
        yield_func(new_vars, None)

    def get_match_count(self, vars={}):
        return (0, None)

    def get_match_candidates(self, leaves, expression, attributes, evaluation, vars={}):
        def _match(leaf):
            return leaf.has_form(("Rule", "RuleDelayed"), 2) or leaf.has_form(
                "List", None
            )

        return [leaf for leaf in leaves if _match(leaf)]


class _StopGeneratorBaseExpressionIsFree(StopGenerator):
    pass


def item_is_free(item, form, evaluation):
    # for vars, rest in form.match(self, {}, evaluation, fully=False):
    def yield_match(vars, rest):
        raise _StopGeneratorBaseExpressionIsFree(False)
        # return False

    try:
        form.match(yield_match, item, {}, evaluation, fully=False)
    except _StopGeneratorBaseExpressionIsFree as exc:
        return exc.value

    if item.is_atom():
        return True
    else:
        return item_is_free(item.head, form, evaluation) and all(
            item_is_free(leaf, form, evaluation) for leaf in item.leaves
        )
