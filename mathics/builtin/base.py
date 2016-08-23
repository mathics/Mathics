#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

import re
import sympy
from functools import total_ordering
import importlib

from mathics.core.definitions import Definition
from mathics.core.rules import Rule, BuiltinRule, Pattern
from mathics.core.expression import (BaseExpression, Expression, Symbol,
                                     String, Integer, ensure_context,
                                     strip_context)
import six


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
                if isinstance(forms, six.string_types):
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
                if form not in formatvalues:
                    formatvalues[form] = []
                if not isinstance(pattern, BaseExpression):
                    pattern = pattern % {'name': name}
                    pattern = parse_builtin_rule(pattern)
                replace = replace % {'name': name}
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
        for option, value in six.iteritems(self.options):
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
        for spec, value in six.iteritems(self.defaults):
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
    def get_name(cls, short=False):
        if cls.name is None:
            shortname = cls.__name__
        else:
            shortname = cls.name
        if short:
            return shortname
        return cls.context + shortname

    def get_operator(self):
        return None

    def get_operator_display(self):
        return None

    def get_functions(self, prefix='apply'):
        from mathics.core.parser import parse_builtin_rule

        unavailable_function = self._get_unavailable_function()
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
                if unavailable_function:
                    function = unavailable_function
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

    def _get_unavailable_function(self):
        requires = getattr(self, 'requires', [])

        for package in requires:
            try:
                importlib.import_module(package)
            except ImportError:
                def apply(**kwargs):  # will override apply method
                    kwargs['evaluation'].message(
                        'General', 'pyimport',  # see inout.py
                        strip_context(self.get_name()), package)

                return apply

        return None

    def get_option_string(self, *params):
        s = self.get_option(*params)
        if isinstance(s, String):
            return s.get_string_value(), s
        elif isinstance(s, Symbol):
            for prefix in ('Global`', 'System`'):
                if s.get_name().startswith(prefix):
                    return s.get_name()[len(prefix):], s
        return None, s

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


class AtomBuiltin(Builtin):
    # allows us to define apply functions, rules, messages, etc. for Atoms
    # which are by default not in the definitions' contribution pipeline.
    # see Image[] for an example of this.

    def get_name(self):
        name = super(AtomBuiltin, self).get_name()
        return re.sub(r"Atom$", "", name)


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
            if op_pattern not in self.formats:
                operator = self.get_operator_display()
                if operator is not None:
                    form = '%s[{HoldForm[item]},"%s",%d]' % (
                        format_function, operator, self.precedence)
                    self.formats[op_pattern] = form


class PrefixOperator(UnaryOperator):
    def __init__(self, *args, **kwargs):
        super(PrefixOperator, self).__init__('Prefix', *args, **kwargs)


class PostfixOperator(UnaryOperator):
    def __init__(self, *args, **kwargs):
        super(PostfixOperator, self).__init__('Postfix', *args, **kwargs)


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

    def get_sympy_names(self):
        if self.sympy_name:
            return [self.sympy_name]
        return []


class SympyFunction(SympyObject):
    def prepare_sympy(self, leaves):
        return leaves

    def get_sympy_function(self, leaves):
        if self.sympy_name:
            return getattr(sympy, self.sympy_name)
        return None

    def to_sympy(self, expr, **kwargs):
        try:
            if self.sympy_name:
                leaves = self.prepare_sympy(expr.leaves)
                sympy_args = [leaf.to_sympy(**kwargs) for leaf in leaves]
                if None in sympy_args:
                    return None
                sympy_function = self.get_sympy_function(leaves)
                return sympy_function(*sympy_args)
        except TypeError:
            pass

    def from_sympy(self, sympy_name, leaves):
        return Expression(self.get_name(), *leaves)

    def prepare_mathics(self, sympy_expr):
        return sympy_expr


class SympyConstant(SympyObject, Predefined):
    attributes = ('Constant', 'ReadProtected')

    def is_constant(self):
        # free Symbol will be converted to corresponding SymPy symbol
        return True

    def to_sympy(self, expr, **kwargs):
        if expr.is_atom():
            return getattr(sympy, self.sympy_name)
        else:
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


class MessageException(Exception):
    def __init__(self, *message):
        self._message = message

    def message(self, evaluation):
        evaluation.message(*self._message)


class NegativeIntegerException(Exception):
    pass


@total_ordering
class CountableInteger:
    """
    CountableInteger is an integer specifying a countable amount (including
    zero) that can optionally be specified as an upper bound through UpTo[].
    """

    # currently MMA does not support UpTo[Infinity], but Infinity already shows
    # up in UpTo's parameter error messages as supported option; it would make
    # perfect sense. currently, we stick with MMA's current behaviour and set
    # _support_infinity to False.
    _support_infinity = False

    def __init__(self, value='Infinity', upper_limit=True):
        self._finite = (value != 'Infinity')
        if self._finite:
            assert isinstance(value, int) and value >= 0
            self._integer = value
        else:
            assert upper_limit
            self._integer = None
        self._upper_limit = upper_limit

    def is_upper_limit(self):
        return self._upper_limit

    def get_int_value(self):
        assert self._finite
        return self._integer

    def __eq__(self, other):
        if isinstance(other, CountableInteger):
            if self._finite:
                return other._finite and self._integer == other._integer
            else:
                return not other._finite
        elif isinstance(other, int):
            return self._finite and self._integer == other
        else:
            return False

    def __lt__(self, other):
        if isinstance(other, CountableInteger):
            if self._finite:
                return other._finite and self._integer < other._value
            else:
                return False
        elif isinstance(other, int):
            return self._finite and self._integer < other
        else:
            return False

    @staticmethod
    def from_expression(expr):
        """
        :param expr: expression from which to build a CountableInteger
        :return: an instance of CountableInteger or None, if the whole
        original expression should remain unevaluated.
        :raises: MessageException, NegativeIntegerException
        """

        if isinstance(expr, Integer):
            py_n = expr.get_int_value()
            if py_n >= 0:
                return CountableInteger(py_n, upper_limit=False)
            else:
                raise NegativeIntegerException()
        elif expr.get_head_name() == 'System`UpTo':
            if len(expr.leaves) != 1:
                raise MessageException('UpTo', 'argx', len(expr.leaves))
            else:
                n = expr.leaves[0]
                if isinstance(n, Integer):
                    py_n = n.get_int_value()
                    if py_n < 0:
                        raise MessageException('UpTo', 'innf', expr)
                    else:
                        return CountableInteger(py_n, upper_limit=True)
                elif CountableInteger._support_infinity:
                    if n.get_head_name() == 'System`DirectedInfinity' and len(n.leaves) == 1:
                        if n.leaves[0].get_int_value() > 0:
                            return CountableInteger('Infinity', upper_limit=True)
                        else:
                            return CountableInteger(0, upper_limit=True)

        return None  # leave original expression unevaluated
