# -*- coding: utf8 -*-

"""
System functions
"""

import re

from mathics.core.expression import Expression, String, strip_context
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
     = {CSVExport, DataImport, ImportCSV, LinesImport, PlaintextImport, StringImport, TextExport, WordsImport, importJSON, x}

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

        # Names["ctx_pattern`short_pattern"] returns symbols whose
        # context and short name match 'ctx_pattern' and
        # 'short_pattern'. Names["short_pattern"] returns a list of
        # the symbols accessible through $Context and $ContextPath
        # whose short names match the pattern, and it only includes
        # one symbol with a particular short name.
        #
        # '*' matches any sequence of symbol characters or an empty
        # string. '@' matches a non-empty sequence of symbol
        # characters which aren't uppercase letters. In the context
        # part, both '*' and '@' match context marks.

        if '`' in pattern:
            ctx_pattern, short_pattern = pattern.rsplit('`', 1)
            ctx_pattern = ((ctx_pattern + '`')
                           .replace('@', '[^A-Z`]+')
                           .replace('*', '.*'))
        else:
            short_pattern = pattern
            accessible_ctxts = set(evaluation.definitions.context_path)
            accessible_ctxts.add(evaluation.definitions.current_context)
            # start with a group matching the accessible contexts
            ctx_pattern = ("(?:"
                           + "|".join(re.escape(c) for c in accessible_ctxts)
                           + ")")

        short_pattern = (short_pattern
                         .replace('@', '[^A-Z]+')
                         .replace('*', '[^`]*'))
        regex = re.compile('^' + ctx_pattern + short_pattern + '$')

        names = set([])
        for full_name in evaluation.definitions.get_names():
            if regex.match(full_name) is not None:
                short_name = strip_context(full_name)
                names.add(short_name if short_name not in names else full_name)

        # TODO: Mathematica ignores contexts when it sorts the list of
        # names.
        return Expression('List', *[String(name) for name in sorted(names)])
