# -*- coding: utf-8 -*-
# cython: language_level=3

import re
import sympy
from functools import total_ordering
import importlib
from itertools import chain
import typing
from typing import Any, cast

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.core.convert import from_sympy
from mathics.core.definitions import Definition
from mathics.core.parser.util import SystemDefinitions, PyMathicsDefinitions
from mathics.core.rules import Rule, BuiltinRule, Pattern
from mathics.core.expression import (
    BaseExpression,
    Expression,
    Integer,
    MachineReal,
    PrecisionReal,
    String,
    Symbol,
    SymbolTrue,
    SymbolFalse,
    ensure_context,
    strip_context,
)
from mathics.core.numbers import get_precision, PrecisionValueError


def get_option(options, name, evaluation, pop=False, evaluate=True):
    # we do not care whether an option X is given as System`X,
    # Global`X, or with any prefix from $ContextPath for that
    # matter. Also, the quoted string form "X" is ok. all these
    # variants name the same option. this matches Wolfram Language
    # behaviour.
    name = strip_context(name)
    contexts = (s + "%s" for s in evaluation.definitions.get_context_path())

    for variant in chain(contexts, ('"%s"',)):
        resolved_name = variant % name
        if pop:
            value = options.pop(resolved_name, None)
        else:
            value = options.get(resolved_name)
        if value is not None:
            return value.evaluate(evaluation) if evaluate else value
    return None


def has_option(options, name, evaluation):
    return get_option(options, name, evaluation, evaluate=False) is not None


mathics_to_python = {}


class Builtin(object):
    name: typing.Optional[str] = None
    context = ""
    abstract = False
    attributes: typing.Tuple[Any, ...] = ()
    rules: typing.Dict[str, Any] = {}
    formats: typing.Dict[str, Any] = {}
    messages: typing.Dict[str, Any] = {}
    options: typing.Dict[str, Any] = {}
    defaults = {}

    def __new__(cls, *args, **kwargs):
        if kwargs.get("expression", None) is not False:
            return Expression(cls.get_name(), *args)
        else:
            instance = super().__new__(cls)
            if not instance.formats:
                # Reset formats so that not every instance shares the same
                # empty dict {}
                instance.formats = {}
            return instance

    def __init__(self, *args, **kwargs):
        super().__init__()
        if hasattr(self, "python_equivalent"):
            mathics_to_python[self.get_name()] = self.python_equivalent

    def contribute(self, definitions, is_pymodule=False):
        from mathics.core.parser import parse_builtin_rule

        # Set the default context
        if not self.context:
            self.context = "Pymathics`" if is_pymodule else "System`"

        name = self.get_name()
        options = {}
        option_syntax = "Warn"

        for option, value in self.options.items():
            if option == "$OptionSyntax":
                option_syntax = value
                continue
            option = ensure_context(option)
            options[option] = parse_builtin_rule(value)
            if option.startswith("System`"):
                # Create a definition for the option's symbol.
                # Otherwise it'll be created in Global` when it's
                # used, so it won't work.
                if option not in definitions.builtin:
                    definitions.builtin[option] = Definition(
                        name=name, attributes=set()
                    )

        # Check if the given options are actually supported by the Builtin.
        # If not, we might issue an optx error and abort. Using '$OptionSyntax'
        # in your Builtin's 'options', you can specify the exact behaviour
        # using one of the following values:

        # - 'Strict': warn and fail with unsupported options
        # - 'Warn': warn about unsupported options, but continue
        # - 'Ignore': allow unsupported options, do not warn

        if option_syntax in ("Strict", "Warn", "System`Strict", "System`Warn"):

            def check_options(options_to_check, evaluation):
                name = self.get_name()
                for key, value in options_to_check.items():
                    short_key = strip_context(key)
                    if not has_option(options, short_key, evaluation):
                        evaluation.message(
                            name,
                            "optx",
                            Expression("Rule", short_key, value),
                            strip_context(name),
                        )
                        if option_syntax in ("Strict", "System`Strict"):
                            return False
                return True

        elif option_syntax in ("Ignore", "System`Ignore"):
            check_options = None
        else:
            raise ValueError(
                "illegal option mode %s; check $OptionSyntax." % option_syntax
            )

        rules = []
        definition_class = (
            PyMathicsDefinitions() if is_pymodule else SystemDefinitions()
        )

        for pattern, function in self.get_functions(is_pymodule=is_pymodule):
            rules.append(
                BuiltinRule(name, pattern, function, check_options, system=True)
            )
        for pattern, replace in self.rules.items():
            if not isinstance(pattern, BaseExpression):
                pattern = pattern % {"name": name}
                pattern = parse_builtin_rule(pattern, definition_class)
            replace = replace % {"name": name}
            # FIXME: Should system=True be system=not is_pymodule ?
            rules.append(Rule(pattern, parse_builtin_rule(replace), system=True))

        box_rules = []
        if name != "System`MakeBoxes":
            new_rules = []
            for rule in rules:
                if rule.pattern.get_head_name() == "System`MakeBoxes":
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
                return "" if f == "" else ensure_context(f)

            if isinstance(pattern, tuple):
                forms, pattern = pattern
                if isinstance(forms, str):
                    forms = [contextify_form_name(forms)]
                else:
                    forms = [contextify_form_name(f) for f in forms]
            else:
                forms = [""]
            return forms, pattern

        formatvalues = {"": []}
        for pattern, function in self.get_functions("format_"):
            forms, pattern = extract_forms(name, pattern)
            for form in forms:
                if form not in formatvalues:
                    formatvalues[form] = []
                formatvalues[form].append(
                    BuiltinRule(name, pattern, function, None, system=True)
                )
        for pattern, replace in self.formats.items():
            forms, pattern = extract_forms(name, pattern)
            for form in forms:
                if form not in formatvalues:
                    formatvalues[form] = []
                if not isinstance(pattern, BaseExpression):
                    pattern = pattern % {"name": name}
                    pattern = parse_builtin_rule(pattern)
                replace = replace % {"name": name}
                formatvalues[form].append(
                    Rule(pattern, parse_builtin_rule(replace), system=True)
                )
        for form, formatrules in formatvalues.items():
            formatrules.sort()

        messages = [
            Rule(
                Expression("MessageName", Symbol(name), String(msg)),
                String(value),
                system=True,
            )
            for msg, value in self.messages.items()
        ]

        messages.append(
            Rule(
                Expression("MessageName", Symbol(name), String("optx")),
                String("`1` is not a supported option for `2`[]."),
                system=True,
            )
        )

        if "Unprotected" in self.attributes:
            attributes = []
            self.attributes = list(self.attributes)
            self.attributes.remove("Unprotected")
        else:
            attributes = ["System`Protected"]

        attributes += list(ensure_context(a) for a in self.attributes)
        options = {}
        for option, value in self.options.items():
            option = ensure_context(option)
            options[option] = parse_builtin_rule(value)
            if option.startswith("System`"):
                # Create a definition for the option's symbol.
                # Otherwise it'll be created in Global` when it's
                # used, so it won't work.
                if option not in definitions.builtin:
                    definitions.builtin[option] = Definition(
                        name=name, attributes=set()
                    )
        defaults = []
        for spec, value in self.defaults.items():
            value = parse_builtin_rule(value)
            pattern = None
            if spec is None:
                pattern = Expression("Default", Symbol(name))
            elif isinstance(spec, int):
                pattern = Expression("Default", Symbol(name), Integer(spec))
            if pattern is not None:
                defaults.append(Rule(pattern, value, system=True))

        definition = Definition(
            name=name,
            rules=rules,
            formatvalues=formatvalues,
            messages=messages,
            attributes=attributes,
            options=options,
            defaultvalues=defaults,
            builtin=self,
        )
        if is_pymodule:
            definitions.pymathics[name] = definition
        else:
            definitions.builtin[name] = definition

        makeboxes_def = definitions.builtin["System`MakeBoxes"]
        for rule in box_rules:
            makeboxes_def.add_rule(rule)

    @classmethod
    def get_name(cls, short=False) -> str:
        if cls.name is None:
            shortname = cls.__name__
        else:
            shortname = cls.name
        if short:
            return shortname
        return cls.context + shortname

    def get_operator(self) -> typing.Optional[str]:
        return None

    def get_operator_display(self) -> typing.Optional[str]:
        return None

    def get_functions(self, prefix="apply", is_pymodule=False):
        from mathics.core.parser import parse_builtin_rule

        unavailable_function = self._get_unavailable_function()
        for name in dir(self):
            if name.startswith(prefix):

                function = getattr(self, name)
                pattern = function.__doc__
                if pattern is None:  # Fixes PyPy bug
                    continue
                else:
                    m = re.match(r"([\w,]+)\:\s*(.*)", pattern)
                if m is not None:
                    attrs = m.group(1).split(",")
                    pattern = m.group(2)
                else:
                    attrs = []
                # if is_pymodule:
                #    name = ensure_context(self.get_name(short=True), "Pymathics")
                # else:
                name = self.get_name()
                pattern = pattern % {"name": name}
                definition_class = (
                    PyMathicsDefinitions() if is_pymodule else SystemDefinitions()
                )
                pattern = parse_builtin_rule(pattern, definition_class)
                if unavailable_function:
                    function = unavailable_function
                if attrs:
                    yield (attrs, pattern), function
                else:
                    yield (pattern, function)

    @staticmethod
    def get_option(options, name, evaluation, pop=False):
        return get_option(options, name, evaluation, pop)

    def _get_unavailable_function(self):
        requires = getattr(self, "requires", [])

        for package in requires:
            try:
                importlib.import_module(package)
            except ImportError:

                def apply(**kwargs):  # will override apply method
                    kwargs["evaluation"].message(
                        "General",
                        "pyimport",  # see inout.py
                        strip_context(self.get_name()),
                        package,
                    )

                return apply

        return None

    def get_option_string(self, *params):
        s = self.get_option(*params)
        if isinstance(s, String):
            return s.get_string_value(), s
        elif isinstance(s, Symbol):
            for prefix in ("Global`", "System`"):
                if s.get_name().startswith(prefix):
                    return s.get_name()[len(prefix) :], s
        return None, s


class InstanceableBuiltin(Builtin):
    def __new__(cls, *args, **kwargs):
        new_kwargs = kwargs.copy()
        new_kwargs["expression"] = False
        instance = super().__new__(cls, *args, **new_kwargs)
        if not instance.formats:
            # Reset formats so that not every instance shares the same empty
            # dict {}
            instance.formats = {}
        if kwargs.get("expression", None) is not False:
            try:
                instance.init(*args, **kwargs)
            except TypeError:
                # TypeError occurs when unpickling instance, e.g. PatternObject,
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

    def get_name(self, short=False) -> str:
        name = super().get_name(short=short)
        return re.sub(r"Atom$", "", name)


class Operator(Builtin):
    operator: typing.Optional[str] = None
    precedence: typing.Optional[int] = None
    precedence_parse = None
    needs_verbatim = False

    default_formats = True

    def get_operator(self) -> typing.Optional[str]:
        return self.operator

    def get_operator_display(self) -> typing.Optional[str]:
        if hasattr(self, "operator_display"):
            return self.operator_display
        else:
            return self.operator


class Predefined(Builtin):
    def get_functions(self, prefix="apply", is_pymodule=False):
        functions = list(super().get_functions(prefix))
        if prefix == "apply" and hasattr(self, "evaluate"):
            functions.append((Symbol(self.get_name()), self.evaluate))
        return functions


class SympyObject(Builtin):
    sympy_name: typing.Optional[str] = None

    mathics_to_sympy = {}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if self.sympy_name is None:
            self.sympy_name = strip_context(self.get_name()).lower()
        self.mathics_to_sympy[self.__class__.__name__] = self.sympy_name

    def is_constant(self) -> bool:
        return False

    def get_sympy_names(self) -> typing.List[str]:
        if self.sympy_name:
            return [self.sympy_name]
        return []


class UnaryOperator(Operator):
    def __init__(self, format_function, *args, **kwargs):
        super().__init__(*args, **kwargs)
        name = self.get_name()
        if self.needs_verbatim:
            name = "Verbatim[%s]" % name
        if self.default_formats:
            op_pattern = "%s[item_]" % name
            if op_pattern not in self.formats:
                operator = self.get_operator_display()
                if operator is not None:
                    form = '%s[{HoldForm[item]},"%s",%d]' % (
                        format_function,
                        operator,
                        self.precedence,
                    )
                    self.formats[op_pattern] = form


class PrefixOperator(UnaryOperator):
    def __init__(self, *args, **kwargs):
        super().__init__("Prefix", *args, **kwargs)


class PostfixOperator(UnaryOperator):
    def __init__(self, *args, **kwargs):
        super().__init__("Postfix", *args, **kwargs)


class BinaryOperator(Operator):
    grouping = "System`None"  # NonAssociative, None, Left, Right

    def __init__(self, *args, **kwargs):
        super(BinaryOperator, self).__init__(*args, **kwargs)
        name = self.get_name()
        # Prevent pattern matching symbols from gaining meaning here using
        # Verbatim
        name = "Verbatim[%s]" % name

        # For compatibility, allow grouping symbols in builtins to be
        # specified without System`.
        self.grouping = ensure_context(self.grouping)

        if self.grouping in ("System`None", "System`NonAssociative"):
            op_pattern = "%s[items__]" % name
            replace_items = "items"
        else:
            op_pattern = "%s[x_, y_]" % name
            replace_items = "x, y"

        if self.default_formats:
            operator = self.get_operator_display()
            formatted = 'MakeBoxes[Infix[{%s},"%s",%d,%s], form]' % (
                replace_items,
                operator,
                self.precedence,
                self.grouping,
            )
            formatted_output = 'MakeBoxes[Infix[{%s}," %s ",%d,%s], form]' % (
                replace_items,
                operator,
                self.precedence,
                self.grouping,
            )
            default_rules = {
                "MakeBoxes[{0}, form:StandardForm|TraditionalForm]".format(
                    op_pattern
                ): formatted,
                "MakeBoxes[{0}, form:InputForm|OutputForm]".format(
                    op_pattern
                ): formatted_output,
            }
            default_rules.update(self.rules)
            self.rules = default_rules


class Test(Builtin):
    def apply(self, expr, evaluation) -> Symbol:
        "%(name)s[expr_]"

        if self.test(expr):
            return SymbolTrue
        else:
            return SymbolFalse


class SympyFunction(SympyObject):
    def apply(self, *args):
        """
        Generic apply method that uses the class sympy_name.
        to call the corresponding sympy function. Arguments are
        converted to python and the result is converted from sympy
        """
        sympy_args = [a.to_sympy() for a in args]
        sympy_fn = getattr(sympy, self.sympy_name)
        return from_sympy(sympy_fn(*sympy_args))

    def get_constant(self, precision, evaluation, have_mpmath=False):
        try:
            d = get_precision(precision, evaluation)
        except PrecisionValueError:
            return

        sympy_fn = self.to_sympy()
        if d is None:
            result = self.get_mpmath_function() if have_mpmath else sympy_fn()
            return MachineReal(result)
        else:
            return PrecisionReal(sympy_fn.n(d))

    def get_sympy_function(self, leaves=None):
        if self.sympy_name:
            return getattr(sympy, self.sympy_name)
        return None

    def prepare_sympy(self, leaves):
        return leaves

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


class InvalidLevelspecError(Exception):
    pass


class PartError(Exception):
    pass


class PartDepthError(PartError):
    def __init__(self, index=0):
        self.index = index


class PartRangeError(PartError):
    pass


class BoxConstructError(Exception):
    pass


class BoxConstruct(InstanceableBuiltin):
    def __new__(cls, *leaves, **kwargs):
        instance = super().__new__(cls, *leaves, **kwargs)
        instance._leaves = leaves
        return instance

    def evaluate(self, evaluation):
        # THINK about: Should we evaluate the leaves here?
        return

    def get_head_name(self):
        return self.get_name()

    def get_lookup_name(self):
        return self.get_name()

    def get_string_value(self):
        return "-@" + self.get_head_name() + "@-"

    def same(self, expr):
        return expr.same(self)

    def is_atom(self):
        return False

    def do_format(self, evaluation, format):
        return self

    def format(self, evaluation, fmt):
        return self

    def get_head(self):
        return Symbol(self.get_name())

    @property
    def head(self):
        return self.get_head()

    @head.setter
    def head(self, value):
        raise ValueError("BoxConstruct.head is write protected.")

    @property
    def leaves(self):
        return self._leaves

    @leaves.setter
    def leaves(self, value):
        raise ValueError("BoxConstruct.leaves is write protected.")

    # I need to repeat this, because this is not
    # an expression...
    def has_form(self, heads, *leaf_counts):
        """
        leaf_counts:
            (,):        no leaves allowed
            (None,):    no constraint on number of leaves
            (n, None):  leaf count >= n
            (n1, n2, ...):    leaf count in {n1, n2, ...}
        """

        head_name = self.get_name()
        if isinstance(heads, (tuple, list, set)):
            if head_name not in [ensure_context(h) for h in heads]:
                return False
        else:
            if head_name != ensure_context(heads):
                return False
        if not leaf_counts:
            return False
        if leaf_counts and leaf_counts[0] is not None:
            count = len(self._leaves)
            if count not in leaf_counts:
                if (
                    len(leaf_counts) == 2
                    and leaf_counts[1] is None  # noqa
                    and count >= leaf_counts[0]
                ):
                    return True
                else:
                    return False
        return True

    def flatten_pattern_sequence(self, evaluation) -> "BoxConstruct":
        return self

    def get_option_values(self, leaves, **options):
        evaluation = options.get("evaluation", None)
        if evaluation:
            default = evaluation.definitions.get_options(self.get_name()).copy()
            options = Expression("List", *leaves).get_option_values(evaluation)
            default.update(options)
        else:
            from mathics.core.parser import parse_builtin_rule

            default = {}
            for option, value in self.options.items():
                option = ensure_context(option)
                default[option] = parse_builtin_rule(value)
        return default

    def boxes_to_text(self, leaves, **options) -> str:
        raise BoxConstructError

    def boxes_to_mathml(self, leaves, **options) -> str:
        raise BoxConstructError

    def boxes_to_tex(self, leaves, **options) -> str:
        raise BoxConstructError


class PatternError(Exception):
    def __init__(self, name, tag, *args):
        super().__init__()


class PatternArgumentError(PatternError):
    def __init__(self, name, count, expected):
        super().__init__(None, None)


class PatternObject(InstanceableBuiltin, Pattern):
    needs_verbatim = True

    arg_counts: typing.List[int] = []

    def init(self, expr):
        super().init(expr)
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

    def get_lookup_name(self) -> str:
        return self.get_name()

    def get_head_name(self) -> str:
        return self.get_name()

    def get_sort_key(self, pattern_sort=False):
        return self.expr.get_sort_key(pattern_sort=pattern_sort)

    def get_match_count(self, vars={}):
        return (1, 1)

    def get_match_candidates(self, leaves, expression, attributes, evaluation, vars={}):
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
    _finite: bool
    _upper_limit: bool
    _integer: typing.Union[str, int]
    _support_infinity = False

    def __init__(self, value="Infinity", upper_limit=True):
        self._finite = value != "Infinity"
        if self._finite:
            assert isinstance(value, int) and value >= 0
            self._integer = value
        else:
            assert upper_limit
            self._integer = None
        self._upper_limit = upper_limit

    def is_upper_limit(self) -> bool:
        return self._upper_limit

    def get_int_value(self) -> int:
        assert self._finite
        return cast(int, self._integer)

    def __eq__(self, other) -> bool:
        if isinstance(other, CountableInteger):
            if self._finite:
                return other._finite and cast(int, self._integer) == other._integer
            else:
                return not other._finite
        elif isinstance(other, int):
            return self._finite and cast(int, self._integer) == other
        else:
            return False

    def __lt__(self, other) -> bool:
        if isinstance(other, CountableInteger):
            if self._finite:
                return other._finite and cast(int, self._integer) < cast(
                    int, other._integer
                )
            else:
                return False
        elif isinstance(other, int):
            return self._finite and cast(int, self._integer) < other
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
        elif expr.get_head_name() == "System`UpTo":
            if len(expr.leaves) != 1:
                raise MessageException("UpTo", "argx", len(expr.leaves))
            else:
                n = expr.leaves[0]
                if isinstance(n, Integer):
                    py_n = n.get_int_value()
                    if py_n < 0:
                        raise MessageException("UpTo", "innf", expr)
                    else:
                        return CountableInteger(py_n, upper_limit=True)
                elif CountableInteger._support_infinity:
                    if (
                        n.get_head_name() == "System`DirectedInfinity"
                        and len(n.leaves) == 1
                    ):
                        if n.leaves[0].get_int_value() > 0:
                            return CountableInteger("Infinity", upper_limit=True)
                        else:
                            return CountableInteger(0, upper_limit=True)

        return None  # leave original expression unevaluated
