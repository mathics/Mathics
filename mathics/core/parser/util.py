#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from mathics.core.parser.parser import Parser
from mathics.core.parser.convert import convert
from mathics.core.parser.feed import SingleLineFeeder
from mathics.core.expression import ensure_context


parser = Parser()

def parse(definitions, feeder):
    '''
    Parse input (from the frontend, -e, input files, ToExpression etc).
    Look up symbols according to the Definitions instance supplied.

    Feeder must implement the feed and empty methods, see core/parser/feed.py.
    '''
    return parse_returning_code(definitions, feeder)[0]


def parse_returning_code(definitions, feeder):
    '''
    Parse input (from the frontend, -e, input files, ToExpression etc).
    Look up symbols according to the Definitions instance supplied.

    Feeder must implement the feed and empty methods, see core/parser/feed.py.
    '''
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


def parse_builtin_rule(string):
    '''
    Parse rules specified in builtin docstrings/attributes. Every symbol
    in the input is created in the System` context.
    '''
    return parse(SystemDefinitions(), SingleLineFeeder(string, '<builtin_rules>'))
