#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import absolute_import
from __future__ import unicode_literals

from mathics.core.parser.prescanner import prescan
from mathics.core.parser.parser import Parser
from mathics.core.parser.convert import convert

parser = Parser()


# Parse input (from the frontend, -e, input files, ToExpression etc).
# Look up symbols according to the Definitions instance supplied.
def parse(raw_code, definitions):
    code = prescan(raw_code)
    ast = parser.parse(code)
    return convert(ast, definitions)


class SystemDefinitions(object):
    """
    Dummy Definitions object that puts every unqualified symbol in
    System`.
    """
    def lookup_name(self, name):
        assert isinstance(name, six.string_types)
        return ensure_context(name)


# Parse rules specified in builtin docstrings/attributes. Every symbol
# in the input is created in the System` context.
def parse_builtin_rule(string):
    return parse(string, SystemDefinitions())


def parse_lines(lines, definitions):
    '''
    Given some lines of code try to construct a list of expressions.

    In the case of incomplete lines append more lines until a complete
    expression is found. If the end is reached and no complete expression is
    found then reraise the exception.

    We use a generator so that each expression can be evaluated (as the parser
    is dependent on defintions and evaluation may change the definitions).
    '''
    query = ''
    if isinstance(lines, six.text_type):
        lines = lines.splitlines()

    incomplete_exc = None
    for line in lines:
        if not line:
            query += ' '
            continue
        query += line
        if query.endswith('\\'):
            query = query.rstrip('\\')
            incomplete_exc = IncompleteSyntaxError(len(query) - 1)
            continue
        try:
            expression = parse(query, definitions)
        except IncompleteSyntaxError as exc:
            incomplete_exc = exc
        else:
            if expression is not None:
                yield expression
            query = ''
            incomplete_exc = None

    if incomplete_exc is not None:
        # ran out of lines
        raise incomplete_exc

    raise StopIteration
