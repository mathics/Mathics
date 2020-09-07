#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
System functions
"""


import os
import platform
import sys

from mathics.core.expression import Expression, Integer, String, strip_context
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
     = {x}

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


class Machine(Predefined):
    """
    <dl>
    <dt>'$Machine'
        <dd>returns a string describing the type of computer system on which the Mathics is being run.
    </dl>

    Example:
    <pre>
    In[1] = $Machine
    Out[1] = linux
    </pre>
    """

    name = '$Machine'

    def evaluate(self, evaluation):
        return String(sys.platform)


class MachineName(Predefined):
    """
    <dl>
    <dt>'$MachineName'
        <dd>returns a string that gives the assigned name of the computer on which Mathics is being run, if such a name is defined.
    </dl>

    Example:
    <pre>
    In[1] = $MachineName
    Out[1] = buster
    </pre>
    """

    name = '$MachineName'

    def evaluate(self, evaluation):
        return String(os.uname().nodename)


class ProcessorType(Predefined):
    """
    <dl>
    <dt>'$ProcessorType'
        <dd>returns a string giving the architecture of the processor on which the Wolfram System is being run.
    </dl>

    Example:
    <pre>
    In[1] = $ProcessorType
    Out[1] = x86_64
    </pre>
    """

    name = '$ProcessorType'

    def evaluate(self, evaluation):
        return String(platform.machine())


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

class SystemID(Predefined):
    """
    <dl>
    <dt>'$SystemID'
        <dd>returns a short string that identifies the type of computer system on which the Mathics is being run.
    </dl>

    Example:
    <pre>
    In[1] = $SystemID
    Out[1] = linux
    </pre>
    """

    name = '$SystemID'

    def evaluate(self, evaluation):
        return String(sys.platform)

class SystemWordLength(Predefined):
    """
    <dl>
    <dt>'$SystemWordLength'
        <dd>returns the effective number of bits in raw machine words on the computer system where Mathics is running
    </dl>

    Example:
    <pre>
    In[1] = $SystemWordLength
    Out[1] = 64
    </pre>
    """

    name = '$SystemWordLength'

    def evaluate(self, evaluation):
        # https://docs.python.org/3/library/platform.html#module-platform
        # says it is more reliable to get bits using sys.maxsize
        # than platform.architecture()[0]
        size = 128
        while not sys.maxsize > 2**size:
            size >>= 1
        return Integer(size << 1)
