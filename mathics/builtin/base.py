#!/usr/bin/env python
# -*- coding: utf-8 -*-

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

import re
import sympy

from mathics.core.definitions import Definition
from mathics.core.rules import Rule, BuiltinRule, Pattern
from mathics.core.expression import (BaseExpression, Expression, Symbol,
                                     String, Integer, ensure_context,
                                     strip_context)


class Builtin(object):
    name = None
    context = 'System`'
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
        from mathics.core.parser import parse_builtin_rule

        name = self.get_name()
        rules = []
        for pattern, function in self.get_functions():
            rules.append(BuiltinRule(pattern, function, system=True))
        for pattern, replace in self.rules.items():
            if not isinstance(pattern, BaseExpression):
                pattern = pattern % {'name': name}
                pattern = parse_builtin_rule(pattern)
            replace = replace % {'name': name}
            rules.append(Rule(pattern, parse_builtin_rule(replace), system=True))

        box_rules = []
        if name != 'System`MakeBoxes':
            new_rules = []
            for rule in rules:
                if rule.pattern.get_head_name() == 'System`MakeBoxes':
                    box_rules.append(rule)
                else:
                    new_rules.append(rule)
            rules = new_rules

        def extract_forms(name, pattern):
            # Handle a tuple of (forms, pattern) as well as a pattern
            # on the left-hand side of a format rule. 'forms' can be
            # an empty string (=> the rule applies to all forms), or a
            # form name (like 'System`TraditionalForm'), or a sequence
            # of form names.
            def contextify_form_name(f):
                # Handle adding 'System`' to a form name, unless it's
                # '' (meaning the rule applies to all forms).
                return '' if f == '' else ensure_context(f)
            if isinstance(pattern, tuple):
                forms, pattern = pattern
                if isinstance(forms, str):
                    forms = [contextify_form_name(forms)]
                else:
                    forms = [contextify_form_name(f) for f in forms]
            else:
                forms = ['']
            return forms, pattern

        formatvalues = {'': []}
        for pattern, function in self.get_functions('format_'):
            forms, pattern = extract_forms(name, pattern)
            for form in forms:
                if form not in formatvalues:
                    formatvalues[form] = []
                formatvalues[form].append(BuiltinRule(
                    pattern, function, system=True))
        for pattern, replace in self.formats.items():
            forms, pattern = extract_forms(name, pattern)
            for form in forms:
                if not form in formatvalues:
                    formatvalues[form] = []
                if not isinstance(pattern, BaseExpression):
                    pattern = parse_builtin_rule(pattern)
                formatvalues[form].append(Rule(
                    pattern, parse_builtin_rule(replace), system=True))
        for form, formatrules in formatvalues.items():
            formatrules.sort()

        messages = [Rule(Expression('MessageName', Symbol(name), String(msg)),
                         String(value), system=True)
                    for msg, value in self.messages.items()]

        if name == 'System`MakeBoxes':
            attributes = []
        else:
            attributes = ['System`Protected']
        attributes += list(ensure_context(a) for a in self.attributes)
        options = {}
        for option, value in self.options.iteritems():
            option = ensure_context(option)
            options[option] = parse_builtin_rule(value)
            if option.startswith('System`'):
                # Create a definition for the option's symbol.
                # Otherwise it'll be created in Global` when it's
                # used, so it won't work.
                if option not in definitions.builtin:
                    definitions.builtin[option] = Definition(
                        name=name, attributes=set())
        defaults = []
        for spec, value in self.defaults.iteritems():
            value = parse_builtin_rule(value)
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

        makeboxes_def = definitions.builtin['System`MakeBoxes']
        for rule in box_rules:
            makeboxes_def.add_rule(rule)

    @classmethod
    def get_name(cls):
        if cls.name is None:
            shortname = cls.__name__
        else:
            shortname = cls.name
        return cls.context + shortname

    def get_operator(self):
        return None

    def get_operator_display(self):
        return None

    def get_functions(self, prefix='apply'):
        from mathics.core.parser import parse_builtin_rule

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
                pattern = parse_builtin_rule(pattern)
                if attrs:
                    yield (attrs, pattern), function
                else:
                    yield (pattern, function)

    def get_option(self, options, name, evaluation, pop=False):
        name = ensure_context(name)
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
                # because parameter expr is not given. This should no be a
                # problem, as pickled objects need their init-method not
                # being called.
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
        return Expression(expression.head.post_parse(), *[
            leaf.post_parse() for leaf in expression.leaves])


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
                    form = '%s[{HoldForm[item]},"%s",%d]' % (
                        format_function, operator, self.precedence)
                    self.formats[op_pattern] = form


class PrefixOperator(UnaryOperator):
    def __init__(self, *args, **kwargs):
        super(PrefixOperator, self).__init__('Prefix', *args, **kwargs)

    def is_prefix(self):
        return True


class PostfixOperator(UnaryOperator):
    def __init__(self, *args, **kwargs):
        super(PostfixOperator, self).__init__('Postfix', *args, **kwargs)

    def is_postfix(self):
        return True


class BinaryOperator(Operator):
    grouping = 'System`None'  # NonAssociative, None, Left, Right

    def __init__(self, *args, **kwargs):
        super(BinaryOperator, self).__init__(*args, **kwargs)
        name = self.get_name()
        # Prevent pattern matching symbols from gaining meaning here using
        # Verbatim
        name = 'Verbatim[%s]' % name

        # For compatibility, allow grouping symbols in builtins to be
        # specified without System`.
        self.grouping = ensure_context(self.grouping)

        if self.grouping in ('System`None', 'System`NonAssociative'):
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
                'MakeBoxes[{0}, form:StandardForm|TraditionalForm]'.format(
                    op_pattern): formatted,
                'MakeBoxes[{0}, form:InputForm|OutputForm]'.format(
                    op_pattern): formatted_output,
            }
            default_rules.update(self.rules)
            self.rules = default_rules

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
            self.sympy_name = strip_context(self.get_name()).lower()

    def is_constant(self):
        return False


class SympyFunction(SympyObject):
    def prepare_sympy(self, leaves):
        return leaves

    def to_sympy(self, expr, **kwargs):
        try:
            if self.sympy_name:
                leaves = self.prepare_sympy(expr.leaves)
                return getattr(sympy, self.sympy_name)(*(
                    leaf.to_sympy(**kwargs) for leaf in leaves))
        except TypeError:
            pass

    def from_sympy(self, leaves):
        return leaves

    def prepare_mathics(self, sympy_expr):
        return sympy_expr


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

    def get_match_candidates(self, leaves, expression, attributes, evaluation,
                             vars={}):
        return leaves

    def get_attributes(self, definitions):
        return self.head.get_attributes(definitions)
