#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
System functions
"""

from __future__ import unicode_literals
from __future__ import absolute_import

import sys

from mathics.core.expression import Expression, String, strip_context
from mathics.builtin.base import Builtin, Predefined
from mathics import version_string


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
        return String(version_string.replace('\n', ' '))


class Names(Builtin):
    """
    <dl>
    <dt>'Names["$pattern$"]'
        <dd>returns the list of names matching $pattern$.
    </dl>

    >> Names["List"]
     = {List}

    The wildcard '*' matches any character:
    >> Names["List*"]
     = {List, ListLinePlot, ListPlot, ListQ, Listable}

    The wildcard '@' matches only lowercase characters:
    >> Names["List@"]
     = {Listable}

    >> x = 5;
    >> Names["Global`*"]
     = {$OutputSizeLimit, x}

    The number of built-in symbols:
    >> Length[Names["System`*"]]
     = ...

    #> Length[Names["System`*"]] > 350
     = True
    """

    def apply(self, pattern, evaluation):
        'Names[pattern_]'

        pattern = pattern.get_string_value()
        if pattern is None:
            return

        names = set([])
        for full_name in evaluation.definitions.get_matching_names(pattern):
            short_name = strip_context(full_name)
            names.add(short_name if short_name not in names else full_name)

        # TODO: Mathematica ignores contexts when it sorts the list of
        # names.
        return Expression('List', *[String(name) for name in sorted(names)])


class Aborted(Predefined):
    """
    <dl>
    <dt>'$Aborted'
        <dd>is returned by a calculation that has been aborted.
    </dl>
    """

    name = '$Aborted'


class Failed(Predefined):
    """
    <dl>
    <dt>'$Failed'
        <dd>is returned by some functions in the event of an error.
    </dl>

    >> Get["nonexistent_file.m"]
     : Cannot open nonexistent_file.m.
     = $Failed
    """

    name = '$Failed'


class CommandLine(Predefined):
    '''
    <dl>
    <dt>'$CommandLine'
      <dd>is a list of strings passed on the command line to launch the Mathics session.
    </dl>
    >> $CommandLine
     = {...}
    '''

    name = '$CommandLine'

    def evaluate(self, evaluation):
        return Expression('List', *(String(arg) for arg in sys.argv))


class ScriptCommandLine(Predefined):
    '''
    <dl>
    <dt>'$ScriptCommandLine'
      <dd>is a list of string arguments when running the kernel is script mode.
    </dl>
    >> $ScriptCommandLine
     = {...}
    '''

    name = '$ScriptCommandLine'

    def evaluate(self, evaluation):
        try:
            dash_index = sys.argv.index('--')
        except ValueError:
            # not run in script mode
            return Expression('List')

        return Expression('List', *(String(arg) for arg in sys.argv[dash_index + 1:]))
