# -*- coding: utf8 -*-

from mathics.builtin.base import (
    Builtin, BinaryOperator, PostfixOperator, PrefixOperator)
from mathics.core.expression import Expression, Symbol
from mathics.core.rules import Rule
from mathics.builtin.lists import walk_parts
from mathics.builtin.evaluation import set_recursionlimit

from mathics import settings


def get_symbol_list(list, error_callback):
    if list.has_form('List', None):
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

        if name in ('OwnValues', 'DownValues', 'SubValues', 'UpValues',
                    'NValues', 'Options', 'DefaultValues', 'Attributes',
                    'Messages'):
            if len(lhs.leaves) != 1:
                evaluation.message_args(name, len(lhs.leaves), 1)
                return False
            tag = lhs.leaves[0].get_name()
            if not tag:
                evaluation.message(name, 'sym', lhs.leaves[0], 1)
                return False
            if tags is not None and tags != [tag]:
                evaluation.message(name, 'tag', name, tag)
                return False

            if (name != 'Attributes' and 'Protected'    # noqa
                in evaluation.definitions.get_attributes(tag)):
                evaluation.message(name, 'wrsym', tag)
                return False
            if name == 'Options':
                option_values = rhs.get_option_values(evaluation)
                if option_values is None:
                    evaluation.message(name, 'options', rhs)
                    return False
                evaluation.definitions.set_options(tag, option_values)
            elif name == 'Attributes':
                attributes = get_symbol_list(
                    rhs, lambda item: evaluation.message(name, 'sym', item, 1))
                if attributes is None:
                    return False
                if 'Locked' in evaluation.definitions.get_attributes(tag):
                    evaluation.message(name, 'locked', tag)
                    return False
                evaluation.definitions.set_attributes(tag, attributes)
            else:
                rules = rhs.get_rules_list()
                if rules is None:
                    evaluation.message(name, 'vrule', lhs, rhs)
                    return False
                evaluation.definitions.set_values(tag, name, rules)
            return True

        form = ''
        nprec = None
        default = False
        message = False

        allow_custom_tag = False

        focus = lhs

        if name == 'N':
            if len(lhs.leaves) not in (1, 2):
                evaluation.message_args('N', len(lhs.leaves), 1, 2)
                return False
            if len(lhs.leaves) == 1:
                nprec = Symbol('MachinePrecision')
            else:
                nprec = lhs.leaves[1]
            focus = lhs.leaves[0]
            lhs = Expression('N', focus, nprec)
        elif name == 'MessageName':
            if len(lhs.leaves) != 2:
                evaluation.message_args('MessageName', len(lhs.leaves), 2)
                return False
            focus = lhs.leaves[0]
            message = True
        elif name == 'Default':
            if len(lhs.leaves) not in (1, 2, 3):
                evaluation.message_args('Default', len(lhs.leaves), 1, 2, 3)
                return False
            focus = lhs.leaves[0]
            default = True
        elif name == 'Format':
            if len(lhs.leaves) not in (1, 2):
                evaluation.message_args('Format', len(lhs.leaves), 1, 2)
                return False
            if len(lhs.leaves) == 2:
                form = lhs.leaves[1].get_name()
                if not form:
                    evaluation.message('Format', 'fttp', lhs.leaves[1])
                    return False
            else:
                form = ('StandardForm', 'TraditionalForm', 'OutputForm',
                        'TeXForm', 'MathMLForm',
                        )
            lhs = focus = lhs.leaves[0]
        else:
            allow_custom_tag = True

        focus = focus.evaluate_leaves(evaluation)

        if tags is None and not upset:
            name = focus.get_lookup_name()
            if not name:
                evaluation.message(self.get_name(), 'setraw', focus)
                return False
            tags = [name]
        elif upset:
            if allow_custom_tag:
                tags = []
                if focus.is_atom():
                    evaluation.message(self.get_name(), 'normal')
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
                    evaluation.message(self.get_name(), 'tagnfd', name)
                    return False

        ignore_protection = False
        rhs_int_value = rhs.get_int_value()
        lhs_name = lhs.get_name()
        if lhs_name == '$RecursionLimit':
            # if (not rhs_int_value or rhs_int_value < 20) and not
            # rhs.get_name() == 'Infinity':
            if (not rhs_int_value or rhs_int_value < 20  # noqa
                or rhs_int_value > settings.MAX_RECURSION_DEPTH):

                evaluation.message('$RecursionLimit', 'limset', rhs)
                return False
            try:
                set_recursionlimit(rhs_int_value)
            except OverflowError:
                # TODO: Message
                return False
            ignore_protection = True
        elif lhs_name == '$ModuleNumber':
            if not rhs_int_value or rhs_int_value <= 0:
                evaluation.message('$ModuleNumber', 'set', rhs)
                return False
            ignore_protection = True
        elif lhs_name in ('$Line', '$HistoryLength'):
            if rhs_int_value is None or rhs_int_value < 0:
                evaluation.message(lhs_name, 'intnn', rhs)
                return False
            ignore_protection = True
        elif lhs_name == '$RandomState':
            # TODO: allow setting of legal random states!
            # (but consider pickle's insecurity!)
            evaluation.message('$RandomState', 'rndst', rhs)
            return False

        rhs_name = rhs.get_head_name()
        if rhs_name == 'Condition':
            if len(rhs.leaves) != 2:
                evaluation.message_args('Condition', len(rhs.leaves), 2)
                return False
            else:
                lhs = Expression('Condition', lhs, rhs.leaves[1])
                rhs = rhs.leaves[0]

        rule = Rule(lhs, rhs)
        count = 0
        defs = evaluation.definitions
        for tag in tags:
            if (not ignore_protection and 'Protected'   # noqa
                in evaluation.definitions.get_attributes(tag)):
                if lhs.get_name() == tag:
                    evaluation.message(self.get_name(), 'wrsym', tag)
                else:
                    evaluation.message(self.get_name(), 'write', tag, lhs)
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
                    defs.add_rule(tag, rule, position='up')
                else:
                    defs.add_rule(tag, rule)
        if count == 0:
            return False

        return True

    def assign(self, lhs, rhs, evaluation):
        if lhs.get_head_name() == 'List':
            if (not (rhs.get_head_name() == 'List')     # noqa
                or len(lhs.leaves) != len(rhs.leaves)):

                evaluation.message(self.get_name(), 'shape', lhs, rhs)
                return False
            else:
                result = True
                for left, right in zip(lhs.leaves, rhs.leaves):
                    if not self.assign(left, right, evaluation):
                        result = False
                return result
        elif lhs.get_head_name() == 'Part':
            if len(lhs.leaves) < 1:
                evaluation.message(self.get_name(), 'setp', lhs)
                return False
            symbol = lhs.leaves[0]
            name = symbol.get_name()
            if not name:
                evaluation.message(self.get_name(), 'setps', symbol)
                return False
            if 'Protected' in evaluation.definitions.get_attributes(name):
                evaluation.message(self.get_name(), 'wrsym', name)
                return False
            rule = evaluation.definitions.get_ownvalue(name)
            if rule is None:
                evaluation.message(self.get_name(), 'noval', symbol)
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
    >> a = 3
     = 3
    >> a
     = 3
    >> f[x_] = x^2
     = x ^ 2
    >> f[10]
     = 100

    You can set multiple values at once using lists:
    >> {a, b, c} = {10, 2, 3}
     = {10, 2, 3}
    >> {a, b, {c, {d}}} = {1, 2, {{c1, c2}, {a}}}
     = {1, 2, {{c1, c2}, {10}}}
    >> d
     = 10

    'Set' evaluates its right-hand side immediately and assigns it to the left-hand side:
    >> a
     = 1
    >> x = a
     = 1
    >> a = 2
     = 2
    >> x
     = 1

    'Set' always returns the right-hand side, which you can again use in an assignment:
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
    """

    operator = '='
    precedence = 40
    grouping = 'Right'
    attributes = ('HoldFirst', 'SequenceHold')

    messages = {
        'setraw': "Cannot assign to raw object `1`.",
        'shape': "Lists `1` and `2` are not the same shape.",
    }

    def apply(self, lhs, rhs, evaluation):
        'lhs_ = rhs_'

        self.assign(lhs, rhs, evaluation)
        return rhs


class SetDelayed(Set):
    """
    'SetDelayed' has attribute 'HoldAll', thus it does not evaluate the right-hand side immediately,
    but evaluates it when needed.
    >> Attributes[SetDelayed]
     = {HoldAll, Protected, SequenceHold}
    >> a = 1
     = 1
    >> x := a
    >> a = 2
     = 2
    >> x
     = 2

    'Condition' can be used to make a conditioned assignment:
    >> f[x_] := p[x] /; x>0
    >> f[3]
     = p[3]
    >> f[-3]
     = f[-3]
    """

    operator = ':='
    attributes = ('HoldAll', 'SequenceHold')

    def apply(self, lhs, rhs, evaluation):
        'lhs_ := rhs_'

        if self.assign(lhs, rhs, evaluation):
            return Symbol('Null')
        else:
            return Symbol('$Failed')


class UpSet(BinaryOperator, _SetOperator):
    """
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

    operator = '^='
    precedence = 40
    attributes = ('HoldFirst', 'SequenceHold')
    grouping = 'Right'

    def apply(self, lhs, rhs, evaluation):
        'lhs_ ^= rhs_'

        self.assign_elementary(lhs, rhs, evaluation, upset=True)
        return rhs


class UpSetDelayed(UpSet):
    """
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

    operator = '^:='
    attributes = ('HoldAll', 'SequenceHold')

    def apply(self, lhs, rhs, evaluation):
        'lhs_ ^:= rhs_'

        if self.assign_elementary(lhs, rhs, evaluation, upset=True):
            return Symbol('Null')
        else:
            return Symbol('$Failed')


class TagSet(Builtin, _SetOperator):
    """
    <dl>
    <dt>'TagSet[$f$, $lhs$, $rhs$]' or 'f /: lhs = rhs'</dt>
        <dd>sets $lhs$ to be $rhs$ and assigns the corresonding rule to the symbol $f$.
    </dl>

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

    attributes = ('HoldAll', 'SequenceHold')

    messages = {
        'tagnfd': "Tag `1` not found or too deep for an assigned rule.",
    }

    def apply(self, f, lhs, rhs, evaluation):
        'f_ /: lhs_ = rhs_'

        name = f.get_name()
        if not name:
            evaluation.message(self.get_name(), 'sym', f, 1)
            return

        rhs = rhs.evaluate(evaluation)
        self.assign_elementary(lhs, rhs, evaluation, tags=[name])
        return rhs


class TagSetDelayed(TagSet):
    """
    <dl>
    <dt>'TagSetDelayed[$f$, $lhs$, $rhs$]' or 'f /: lhs := rhs'
        <dd>is the delayed version of 'TagSet'.
    </dl>
    """

    attributes = ('HoldAll', 'SequenceHold')

    def apply(self, f, lhs, rhs, evaluation):
        'f_ /: lhs_ := rhs_'

        name = f.get_name()
        if not name:
            evaluation.message(self.get_name(), 'sym', f, 1)
            return

        rhs = rhs.evaluate(evaluation)
        if self.assign_elementary(lhs, rhs, evaluation, tags=[name]):
            return Symbol('Null')
        else:
            return Symbol('$Failed')


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

    attributes = ('HoldAll',)

    def format_definition(self, symbol, evaluation, grid=True):
        'StandardForm,TraditionalForm,OutputForm: Definition[symbol_]'

        lines = []

        def print_rule(rule, up=False, lhs=lambda l: l, rhs=lambda r: r):
            evaluation.check_stopped()
            if isinstance(rule, Rule):
                r = rhs(rule.replace.replace_vars({'Definition': Expression(
                    'HoldForm', Symbol('Definition'))}))
                lines.append(Expression('HoldForm', Expression(
                    up and 'UpSet' or 'Set', lhs(rule.pattern.expr), r)))

        name = symbol.get_name()
        if not name:
            evaluation.message('Definition', 'sym', symbol, 1)
            return
        attributes = evaluation.definitions.get_attributes(name)
        definition = evaluation.definitions.get_user_definition(
            name, create=False)
        all = evaluation.definitions.get_definition(name)
        if attributes:
            attributes = list(attributes)
            attributes.sort()
            lines.append(Expression(
                'HoldForm', Expression(
                    'Set', Expression('Attributes', symbol), Expression(
                        'List',
                        *(Symbol(attribute) for attribute in attributes)))))

        if definition is not None and not 'ReadProtected' in attributes:
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
            formats = definition.formatvalues.items()
            formats.sort()
            for format, rules in formats:
                for rule in rules:
                    def lhs(expr):
                        return Expression('Format', expr, Symbol(format))

                    def rhs(expr):
                        if expr.has_form('Infix', None):
                            expr = Expression(Expression(
                                'HoldForm', expr.head), *expr.leaves)
                        return Expression('InputForm', expr)
                    print_rule(rule, lhs=lhs, rhs=rhs)
        for rule in all.defaultvalues:
            print_rule(rule)
        if all.options:
            options = all.options.items()
            options.sort()
            lines.append(
                Expression('HoldForm', Expression(
                    'Set', Expression('Options', symbol),
                    Expression('List', *(
                        Expression('Rule', Symbol(name), value)
                        for name, value in options)))))
        if grid:
            if lines:
                return Expression(
                    'Grid', Expression(
                        'List', *(Expression('List', line) for line in lines)),
                    Expression(
                        'Rule', Symbol('ColumnAlignments'), Symbol('Left')))
            else:
                return Symbol('Null')
        else:
            for line in lines:
                evaluation.print_out(Expression('InputForm', line))
            return Symbol('Null')

    def format_definition_input(self, symbol, evaluation):
        'InputForm: Definition[symbol_]'

        return self.format_definition(symbol, evaluation, grid=False)


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

    attributes = ('HoldAll',)

    messages = {
        'ssym': "`1` is not a symbol or a string.",
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
        '%(name)s[symbols___]'

        for symbol in symbols.get_sequence():
            name = symbol.get_name()
            if not name:
                name = symbol.get_string_value()
            if not name:
                evaluation.message('Clear', 'ssym', symbol)
                continue
            attributes = evaluation.definitions.get_attributes(name)
            if 'Protected' in attributes:
                evaluation.message('Clear', 'wrsym', name)
                continue
            if not self.allow_locked and 'Locked' in attributes:
                evaluation.message('Clear', 'locked', name)
                continue
            definition = evaluation.definitions.get_user_definition(name)
            self.do_clear(definition)

        return Symbol('Null')


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


class Unset(PostfixOperator):
    """
    >> a = 2
     = 2
    >> a =.
    >> a
     = a
    Unsetting an already unset or never defined variable will not cause anything:
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

    operator = '=.'
    precedence = 670
    attributes = ('HoldFirst', 'Listable', 'ReadProtected')

    messages = {
        'norep': "Assignment on `2` for `1` not found.",
        'usraw': "Cannot unset raw object `1`.",
    }

    def apply(self, expr, evaluation):
        'Unset[expr_]'

        name = expr.get_head_name()
        if name in ('OwnValues', 'DownValues', 'SubValues', 'UpValues',
                    'NValues', 'Options', 'Messages'):
            if len(expr.leaves) != 1:
                evaluation.message_args(name, len(expr.leaves), 1)
                return Symbol('$Failed')
            symbol = expr.leaves[0].get_name()
            if not symbol:
                evaluation.message(name, 'fnsym', expr)
                return Symbol('$Failed')
            if name == 'Options':
                empty = {}
            else:
                empty = []
            evaluation.definitions.set_values(symbol, name, empty)
            return Symbol('Null')
        name = expr.get_lookup_name()
        if not name:
            evaluation.message('Unset', 'usraw', expr)
            return Symbol('$Failed')
        if not evaluation.definitions.unset(name, expr):
            if not expr.is_atom():
                evaluation.message('Unset', 'norep', expr, name)
                return Symbol('$Failed')
        return Symbol('Null')


class Quit(Builtin):
    """
    'Quit' removes all user-defined definitions.

    >> a = 3
     = 3
    >> Quit[]
    >> a
     = a

    'Quit' even removes the definitions of protected and locked symbols:
    >> x = 5;
    >> Attributes[x] = {Locked, Protected};
    >> Quit[]
    >> x
     = x
    """

    def apply(self, evaluation):
        'Quit[]'

        evaluation.definitions.set_user_definitions({})
        return Symbol('Null')


def get_symbol_values(symbol, func_name, position, evaluation):
    name = symbol.get_name()
    if not name:
        evaluation.message(func_name, 'sym', symbol, 1)
        return
    if position in ('default',):
        definition = evaluation.definitions.get_definition(name)
    else:
        definition = evaluation.definitions.get_user_definition(name)
    result = Expression('List')
    for rule in definition.get_values_list(position):
        if isinstance(rule, Rule):
            pattern = rule.pattern
            if pattern.has_form('HoldPattern', 1):
                pattern = pattern.expr
            else:
                pattern = Expression('HoldPattern', pattern.expr)
            result.leaves.append(Expression(
                'RuleDelayed', pattern, rule.replace))
    return result


class DownValues(Builtin):
    """
    'DownValues[$symbol$]' gives the list of downvalues associated with $symbol$.

    'DownValues' uses 'HoldPattern' and 'RuleDelayed' to protect the downvalues from being
    evaluated. Moreover, it has attribute 'HoldAll' to get the specified symbol instead of its value.

    >> f[x_] := x ^ 2
    >> DownValues[f]
     = {HoldPattern[f[x_]] :> x ^ 2}

    Mathics will sort the rules you assign to a symbol according to their specifity. If it cannot decide
    which rule is more special, the newer one will get higher precedence.
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
    The default order of patterns can be computed using 'Sort' with 'PatternsOrderedQ':
    >> Sort[{x_, x_Integer}, PatternsOrderedQ]
     = {x_Integer, x_}
    By assigning values to 'DownValues', you can override the default ordering:
    >> DownValues[g] := {g[x_] :> x ^ 2, g[x_Integer] :> x}
    >> g[2]
     = 4

    Fibonacci numbers:
    >> DownValues[fib] := {fib[0] -> 0, fib[1] -> 1, fib[n_] :> fib[n - 1] + fib[n - 2]}
    >> fib[5]
     = 5
    """

    attributes = ('HoldAll',)

    def apply(self, symbol, evaluation):
        'DownValues[symbol_]'

        return get_symbol_values(symbol, 'DownValues', 'down', evaluation)


class OwnValues(Builtin):
    """
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

    attributes = ('HoldAll',)

    def apply(self, symbol, evaluation):
        'OwnValues[symbol_]'

        return get_symbol_values(symbol, 'OwnValues', 'own', evaluation)


class SubValues(Builtin):
    """
    >> f[1][x_] := x
    >> f[2][x_] := x ^ 2
    >> SubValues[f]
     = {HoldPattern[f[2][x_]] :> x ^ 2, HoldPattern[f[1][x_]] :> x}
    >> Definition[f]
     = f[2][x_] = x ^ 2
     .
     . f[1][x_] = x
    """

    attributes = ('HoldAll',)

    def apply(self, symbol, evaluation):
        'SubValues[symbol_]'

        return get_symbol_values(symbol, 'SubValues', 'sub', evaluation)


class UpValues(Builtin):
    """
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

    attributes = ('HoldAll',)

    def apply(self, symbol, evaluation):
        'UpValues[symbol_]'

        return get_symbol_values(symbol, 'UpValues', 'up', evaluation)


class NValues(Builtin):
    """
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

    attributes = ('HoldAll',)

    def apply(self, symbol, evaluation):
        'NValues[symbol_]'

        return get_symbol_values(symbol, 'NValues', 'n', evaluation)


class Messages(Builtin):
    """
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

    attributes = ('HoldAll',)

    def apply(self, symbol, evaluation):
        'Messages[symbol_]'

        return get_symbol_values(symbol, 'Messages', 'messages', evaluation)


class DefaultValues(Builtin):
    """
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

    attributes = ('HoldAll',)

    def apply(self, symbol, evaluation):
        'DefaultValues[symbol_]'

        return get_symbol_values(
            symbol, 'DefaultValues', 'default', evaluation)


class AddTo(BinaryOperator):
    """
    '$x$ += $dx$' is equivalent to '$x$ = $x$ + $dx$'.
    >> a = 10;
    >> a += 2
     = 12
    >> a
     = 12
    """

    operator = '+='
    precedence = 100
    attributes = ('HoldFirst',)
    grouping = 'Right'

    rules = {
        'x_ += dx_': 'x = x + dx',
    }


class SubtractFrom(BinaryOperator):
    """
    '$x$ -= $dx$' is equivalent to '$x$ = $x$ - $dx$'.
    >> a = 10;
    >> a -= 2
     = 8
    >> a
     = 8
    """

    operator = '-='
    precedence = 100
    attributes = ('HoldFirst',)
    grouping = 'Right'

    rules = {
        'x_ -= dx_': 'x = x - dx',
    }


class TimesBy(BinaryOperator):
    """
    '$x$ *= $dx$' is equivalent to '$x$ = $x$ * $dx$'.
    >> a = 10;
    >> a *= 2
     = 20
    >> a
     = 20
    """

    operator = '*='
    precedence = 100
    attributes = ('HoldFirst',)
    grouping = 'Right'

    rules = {
        'x_ *= dx_': 'x = x * dx',
    }


class DivideBy(BinaryOperator):
    """
    '$x$ /= $dx$' is equivalent to '$x$ = $x$ / $dx$'.
    >> a = 10;
    >> a /= 2
     = 5
    >> a
     = 5
    """

    operator = '/='
    precedence = 100
    attributes = ('HoldFirst',)
    grouping = 'Right'

    rules = {
        'x_ /= dx_': 'x = x / dx',
    }


class Increment(PostfixOperator):
    """
    >> a = 2;
    >> a++
     = 2
    >> a
     = 3
    Grouping of 'Increment', 'PreIncrement' and 'Plus':
    >> ++++a+++++2//Hold//FullForm
     = Hold[Plus[PreIncrement[PreIncrement[Increment[Increment[a]]]], 2]]
    """

    operator = '++'
    precedence = 660
    attributes = ('HoldFirst', 'ReadProtected')

    rules = {
        'x_++': 'Module[{t=x}, x = x + 1; t]',
    }


class PreIncrement(PrefixOperator):
    """
    <dl>
    <dt>'PreIncrement[$x$]' or '++$x$'
        <dd>is equivalent to '$x$ = $x$ + 1'.
    </dl>
    >> a = 2;
    >> ++a
     = 3
    >> a
     = 3
    """

    operator = '++'
    precedence = 660
    attributes = ('HoldFirst', 'ReadProtected')

    rules = {
        '++x_': 'x = x + 1',
    }


class Decrement(PostfixOperator):
    """
    >> a = 5;
    >> a--
     = 5
    >> a
     = 4
    """

    operator = '--'
    precedence = 660
    attributes = ('HoldFirst', 'ReadProtected')

    rules = {
        'x_--': 'Module[{t=x}, x = x - 1; t]',
    }


class PreDecrement(PrefixOperator):
    """
    >> a = 2;
    >> --a
     = 1
    >> a
     = 1
    """

    operator = '--'
    precedence = 660
    attributes = ('HoldFirst', 'ReadProtected')

    rules = {
        '--x_': 'x = x - 1',
    }
