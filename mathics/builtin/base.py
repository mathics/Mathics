# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011-2013 The Mathics Team

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

from mathics.core.definitions import Definition
from mathics.core.rules import Rule, BuiltinRule, Pattern
from mathics.core.expression import BaseExpression, Expression, Symbol, String, Integer

import re
import sympy


class Builtin(object):
    name = None
    abstract = False
    attributes = ()
    rules = {}
    formats = {}
    messages = {}
    options = {}
    defaults = {}

    def __new__(cls, *args, **kwargs):
        if kwargs.get('expression', None) is not False:
            return Expression(cls.get_name(), *args)
        else:
            instance = super(Builtin, cls).__new__(cls)
            if not instance.formats:
                # Reset formats so that not every instance shares the same
                # empty dict {}
                instance.formats = {}
            return instance

    def __init__(self, *args, **kwargs):
        super(Builtin, self).__init__()

    def contribute(self, definitions):
        from mathics.core.parser import parse

        name = self.get_name()
        rules = []
        for pattern, function in self.get_functions():
            rules.append(BuiltinRule(pattern, function, system=True))
        for pattern, replace in self.rules.items():
            if not isinstance(pattern, BaseExpression):
                pattern = pattern % {'name': name}
                pattern = parse(pattern)
            replace = replace % {'name': name}
            rules.append(Rule(pattern, parse(replace), system=True))

        box_rules = []
        if name != 'MakeBoxes':
            new_rules = []
            for rule in rules:
                if rule.pattern.get_head_name() == 'MakeBoxes':
                    box_rules.append(rule)
                else:
                    new_rules.append(rule)
            rules = new_rules

        formatvalues = {'': []}
        for pattern, function in self.get_functions('format_'):
            if isinstance(pattern, tuple):
                forms, pattern = pattern
            else:
                forms = ['']
            for form in forms:
                if form not in formatvalues:
                    formatvalues[form] = []
                formatvalues[form].append(BuiltinRule(
                    pattern, function, system=True))
        for pattern, replace in self.formats.items():
            if isinstance(pattern, tuple):
                forms, pattern = pattern
                if not isinstance(forms, tuple):
                    forms = [forms]
            else:
                forms, pattern = [''], pattern
            for form in forms:
                if not form in formatvalues:
                    formatvalues[form] = []
                if not isinstance(pattern, BaseExpression):
                    pattern = parse(pattern)
                formatvalues[form].append(Rule(
                    pattern, parse(replace), system=True))
        for form, formatrules in formatvalues.items():
            formatrules.sort()

        messages = [Rule(Expression('MessageName', Symbol(name), String(
            msg)), String(value), system=True) for msg, value in self.messages.items()]
        if name == 'MakeBoxes':
            attributes = []
        else:
            attributes = ['Protected']
        attributes += list(self.attributes)
        options = {}
        for option, value in self.options.iteritems():
            options[option] = parse(value)
        defaults = []
        for spec, value in self.defaults.iteritems():
            value = parse(value)
            pattern = None
            if spec is None:
                pattern = Expression('Default', Symbol(name))
            elif isinstance(spec, int):
                pattern = Expression('Default', Symbol(name), Integer(spec))
            if pattern is not None:
                defaults.append(Rule(pattern, value, system=True))
        definition = Definition(
            name=name, rules=rules, formatvalues=formatvalues,
            messages=messages, attributes=attributes, options=options,
            defaultvalues=defaults)
        definitions.builtin[name] = definition

        makeboxes_def = definitions.builtin['MakeBoxes']
        for rule in box_rules:
            makeboxes_def.add_rule(rule)

    @classmethod
    def get_name(cls):
        if cls.name is None:
            return cls.__name__
        else:
            return cls.name

    def get_operator(self):
        return None

    def get_operator_display(self):
        return None

    def get_functions(self, prefix='apply'):
        from mathics.core.parser import parse

        for name in dir(self):
            if name.startswith(prefix):
                function = getattr(self, name)
                pattern = function.__doc__
                if pattern is None:  # Fixes PyPy bug
                    continue
                else:
                    m = re.match(r'([\w,]+)\:\s*(.*)', pattern)
                if m is not None:
                    attrs = m.group(1).split(',')
                    pattern = m.group(2)
                else:
                    attrs = []
                pattern = pattern % {'name': self.get_name()}
                pattern = parse(pattern)
                if attrs:
                    yield (attrs, pattern), function
                else:
                    yield (pattern, function)

    def get_option(self, options, name, evaluation, pop=False):
        value = options.pop(name, None) if pop else options.get(name)
        if value is not None:
            return value.evaluate(evaluation)
        else:
            return None


class InstancableBuiltin(Builtin):
    def __new__(cls, *args, **kwargs):
        new_kwargs = kwargs.copy()
        new_kwargs['expression'] = False
        instance = super(InstancableBuiltin, cls).__new__(
            cls, *args, **new_kwargs)
        if not instance.formats:
            # Reset formats so that not every instance shares the same empty
            # dict {}
            instance.formats = {}
        if kwargs.get('expression', None) is not False:
            try:
                instance.init(*args, **kwargs)
            except TypeError:
                # TypeError occurs when unpickling instance, e.g. PatterObject,
                # because parameter expr is not given.
                # This should no be a problem, as pickled objects need their init-method
                # not being called.
                pass
        return instance

    def init(self, *args, **kwargs):
        pass


class Operator(Builtin):
    operator = None
    precedence = None
    precedence_parse = None
    needs_verbatim = False

    default_formats = True

    def get_operator(self):
        return self.operator

    def get_operator_display(self):
        if hasattr(self, 'operator_display'):
            return self.operator_display
        else:
            return self.operator

    def is_binary(self):
        return False

    def is_prefix(self):
        return False

    def is_postfix(self):
        return False

    def post_parse(self, expression):
        return Expression(expression.head.post_parse(), *[leaf.post_parse() for leaf in expression.leaves])


class Predefined(Builtin):
    def get_functions(self, prefix='apply'):
        functions = list(super(Predefined, self).get_functions(prefix))
        if prefix == 'apply' and hasattr(self, 'evaluate'):
            functions.append((Symbol(self.get_name()), self.evaluate))
        return functions


class UnaryOperator(Operator):
    def __init__(self, format_function, *args, **kwargs):
        super(UnaryOperator, self).__init__(*args, **kwargs)
        name = self.get_name()
        if self.needs_verbatim:
            name = 'Verbatim[%s]' % name
        if self.default_formats:
            op_pattern = '%s[item_]' % name
            if not op_pattern in self.formats:
                operator = self.get_operator_display()
                if operator is not None:
                    self.formats[op_pattern] = '%s[{HoldForm[item]},"%s",%d]' % (
                        format_function, operator, self.precedence)


class PrefixOperator(UnaryOperator):
    def __init__(self, *args, **kwargs):
        super(PrefixOperator, self).__init__('Prefix', *args, **kwargs)

    def parse(self, args):
        from mathics.core.parser import MathicsParser, Token

        rest = args[0].parse_tokens
        if rest:
            parser = MathicsParser()
            items = rest + [Token('(')] + [args[
                                           1]] + args[2].parse_tokens + [Token(')')]
            result = parser.parse(items)
            return result
        else:
            return Expression(self.get_name(), args[2], parse_operator=self)

    def is_prefix(self):
        return True


class PostfixOperator(UnaryOperator):
    def __init__(self, *args, **kwargs):
        super(PostfixOperator, self).__init__('Postfix', *args, **kwargs)

    def parse(self, args):
        from mathics.core.parser import MathicsParser, Token

        rest = args[2].parse_tokens
        if rest:
            parser = MathicsParser()    # construct our own parser!
            items = [Token('(')] + args[0].parse_tokens + [
                args[1]] + [Token(')')] + rest
            result = parser.parse(items)
            return result
        else:
            return Expression(self.get_name(), args[0], parse_operator=self)

    def is_postfix(self):
        return True


class BinaryOperator(Operator):
    grouping = 'None'  # NonAssociative, None, Left, Right

    def __init__(self, *args, **kwargs):
        super(BinaryOperator, self).__init__(*args, **kwargs)
        name = self.get_name()
        # Prevent pattern matching symbols from gaining meaning here using
        # Verbatim
        name = 'Verbatim[%s]' % name
        if self.grouping in ('None', 'NonAssociative'):
            op_pattern = '%s[items__]' % name
            replace_items = 'items'
        else:
            op_pattern = '%s[x_, y_]' % name
            replace_items = 'x, y'

        if self.default_formats:
            operator = self.get_operator_display()
            formatted = 'MakeBoxes[Infix[{%s},"%s",%d,%s], form]' % (
                replace_items, operator, self.precedence, self.grouping)
            formatted_output = 'MakeBoxes[Infix[{%s}," %s ",%d,%s], form]' % (
                replace_items, operator, self.precedence, self.grouping)
            default_rules = {
                'MakeBoxes[%s, form:StandardForm|TraditionalForm]' % op_pattern: formatted,
                'MakeBoxes[%s, form:InputForm|OutputForm]' % op_pattern: formatted_output,
            }
            default_rules.update(self.rules)
            self.rules = default_rules

    def parse(self, args):
        left = args[0]
        right = args[2]
        name = self.get_name()
        grouping = self.grouping
        if grouping != 'NonAssociative':
            def collect_leaves(expr):
                if expr.parenthesized or expr.get_head_name() != name:
                    return [expr]
                else:
                    result = []
                    for leaf in expr.leaves:
                        result.extend(collect_leaves(leaf))
                    return result
            leaves = collect_leaves(left) + collect_leaves(right)
            if grouping == 'None':
                return Expression(name, parse_operator=self, *leaves)
            elif grouping == 'Right':
                result = Expression(name, parse_operator=self, *leaves[-2:])
                for leaf in reversed(leaves[:-2]):
                    result = Expression(
                        name, leaf, result, parse_operator=self)
                return result
            elif grouping == 'Left':
                result = Expression(name, parse_operator=self, *leaves[:2])
                for leaf in leaves[2:]:
                    result = Expression(
                        name, result, leaf, parse_operator=self)
                return result
        else:
            return Expression(self.get_name(), left, right, parse_operator=self)

    def is_binary(self):
        return True


class Test(Builtin):
    def apply(self, expr, evaluation):
        '%(name)s[expr_]'

        if self.test(expr):
            return Symbol('True')
        else:
            return Symbol('False')


class SympyObject(Builtin):
    sympy_name = None

    def __init__(self, *args, **kwargs):
        super(SympyObject, self).__init__(*args, **kwargs)
        if self.sympy_name is None:
            self.sympy_name = self.get_name().lower()

    def is_constant(self):
        return False


class SympyFunction(SympyObject):
    def prepare_sympy(self, leaves):
        return leaves

    def to_sympy(self, expr, **kwargs):
        try:
            if self.sympy_name:
                leaves = self.prepare_sympy(expr.leaves)
                return getattr(sympy, self.sympy_name)(*(leaf.to_sympy(**kwargs) for leaf in leaves))
        except TypeError:
            pass

    def from_sympy(self, leaves):
        return leaves


class SympyConstant(SympyObject, Predefined):
    attributes = ('Constant', 'ReadProtected')

    def is_constant(self):
        # free Symbol will be converted to corresponding SymPy symbol
        return True

    def to_sympy(self, expr, **kwargs):
        # there is no "native" SymPy expression for e.g. E[x]
        return None


class InvalidLevelspecError(Exception):
    pass


class PartError(Exception):
    pass


class PartDepthError(PartError):
    pass


class PartRangeError(PartError):
    pass


class BoxConstructError(Exception):
    pass


class BoxConstruct(Builtin):
    def get_option_values(self, leaves, evaluation=None, **options):
        default = evaluation.definitions.get_options(self.get_name()).copy()
        options = Expression('List', *leaves).get_option_values(evaluation)
        default.update(options)
        return default

    def boxes_to_text(self, leaves, **options):
        raise BoxConstructError

    def boxes_to_xml(self, leaves, **options):
        raise BoxConstructError

    def boxes_to_tex(self, leaves, **options):
        raise BoxConstructError


class PatternError(Exception):
    def __init__(self, name, tag, *args):
        super(PatternError).__init__()


class PatternArgumentError(PatternError):
    def __init__(self, name, count, expected):
        super(PatternArgumentError, self).__init__(None, None)


class PatternObject(InstancableBuiltin, Pattern):
    needs_verbatim = True

    arg_counts = []

    def init(self, expr):
        super(PatternObject, self).init(expr)
        if self.arg_counts is not None:
            if len(expr.leaves) not in self.arg_counts:
                self.error_args(len(expr.leaves), *self.arg_counts)
        self.expr = expr
        self.head = Pattern.create(expr.head)
        self.leaves = [Pattern.create(leaf) for leaf in expr.leaves]

    def error(self, tag, *args):
        raise PatternError(self.get_name(), tag, *args)

    def error_args(self, count, *expected):
        raise PatternArgumentError(self.get_name(), count, *expected)

    def get_lookup_name(self):
        return self.get_name()

    def get_head_name(self):
        return self.get_name()

    def get_sort_key(self, pattern_sort=False):
        return self.expr.get_sort_key(pattern_sort=pattern_sort)

    def get_match_count(self, vars={}):
        return (1, 1)

    def get_match_candidates(self, leaves, expression, attributes, evaluation, vars={}):
        return leaves

    def get_attributes(self, definitions):
        return self.head.get_attributes(definitions)
