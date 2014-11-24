# -*- coding: utf8 -*-

"""
System functions
"""

import re

from mathics.core.expression import Expression, String
from mathics.builtin.base import Builtin, Predefined
from mathics import get_version_string


class Version(Predefined):
    """
    <dl>
    <dt>'$Version'
        <dd>returns a string with the current Mathics version and the versions of relevant libraries.
    </dl>

    >> $Version
     = Mathics ...
    """

    name = '$Version'

    def evaluate(self, evaluation):
        return String(get_version_string(True))


class Names(Builtin):
    """
    <dl>
    <dt>'Names["$pattern$"]'
        <dd>returns the list of names matching $pattern$.
    </dl>

    >> Names["List"]
     = {List}
    >> Names["List*"]
     = {List, ListLinePlot, ListPlot, ListQ, Listable}
    >> Names["List@"]
     = {Listable}

    >> x = 5;
    >> Names["Global`*"]
     = {x}

    The number of built-in symbols:
    >> Length[Names["System`*"]]
     = ...

    #> Length[Names["System`*"]] > 350
     = True
    """

    def apply(self, pattern, evaluation):
        'Names[pattern_String]'

        pattern = pattern.get_string_value()
        if pattern is None:
            return

        if pattern.startswith('System`'):
            names = evaluation.definitions.get_builtin_names()
        elif pattern.startswith('Global`'):
            names = (evaluation.definitions.get_user_names() -
                     evaluation.definitions.get_builtin_names())
        else:
            names = evaluation.definitions.get_names()
        if '`' in pattern:
            pattern = pattern[pattern.find('`') + 1:]

        pattern = re.escape(pattern).replace('\@', '[a-z]+').replace('\*', '.*')
        pattern = re.compile('^' + pattern + '$')

        def match_pattern(name):
            return pattern.match(name) is not None

        names = [name for name in names if match_pattern(name)]
        names.sort()

        return Expression('List', *[String(name) for name in names])
