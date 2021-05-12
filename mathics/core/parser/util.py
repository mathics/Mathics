#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from typing import Any, FrozenSet, Tuple

from mathics.core.parser.parser import Parser
from mathics.core.parser.feed import MathicsSingleLineFeeder
from mathics.core.parser.convert import convert
from mathics.core.expression import ensure_context

parser = Parser()


def parse(definitions, feeder) -> Any:
    """
    Parse input (from the frontend, -e, input files, ToExpression etc).
    Look up symbols according to the Definitions instance supplied.

    Feeder must implement the feed and empty methods, see core/parser/feed.py.
    """
    return parse_returning_code(definitions, feeder)[0]


def parse_returning_code(definitions, feeder) -> Tuple[Any, str]:
    """
    Parse input (from the frontend, -e, input files, ToExpression etc).
    Look up symbols according to the Definitions instance supplied.

    Feeder must implement the feed and empty methods, see core/parser/feed.py.
    """
    ast = parser.parse(feeder)
    source_code = parser.tokeniser.code if hasattr(parser.tokeniser, "code") else ""
    if ast is not None:
        return convert(ast, definitions), source_code
    else:
        return None, source_code


class SystemDefinitions(object):
    """
    Dummy Definitions object that puts every unqualified symbol in
    System`.
    """

    def lookup_name(self, name):
        assert isinstance(name, str)
        return ensure_context(name)


# FIXME: there has to be a better way, to get this
# from the current System list.
#  For now we'll hack these in and figure this out
# later
SYSTEM_LIST: FrozenSet[str] = frozenset(
    [
        "Alternatives",
        "Complex",
        "Integer",
        "List",
        "MachineReal",
        "Number",
        "OptionsPattern",
        "PrecisionReal",
        "Real",
        "String",
        "StringExpression",
        "Symbol",
    ]
)


class PyMathicsDefinitions(object):
    """
    Dummy Definitions object that puts every unqualified symbol in
    Pymathics`.
    """

    def lookup_name(self, name):
        assert isinstance(name, str)
        context = "System`" if name in SYSTEM_LIST else "Pymathics`"
        # print("XXX", name, context)
        return ensure_context(name, context)


def parse_builtin_rule(string, definitions=SystemDefinitions()):
    """
    Parse rules specified in builtin docstrings/attributes. Every symbol
    in the input is created in the System` context.
    """
    return parse(definitions, MathicsSingleLineFeeder(string, "<builtin_rules>"))
