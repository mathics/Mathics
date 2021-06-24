#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Global System Information
"""


import os
import platform
import sys
import re
import subprocess

from mathics.version import __version__
from mathics.core.expression import (
    Expression,
    Integer,
    Integer0,
    Real,
    String,
    SymbolFailed,
    SymbolList,
    SymbolRule,
    strip_context,
)
from mathics.builtin.base import Builtin, Predefined
from mathics import version_string
from mathics.builtin.strings import to_regex

try:
    import psutil
except:
    have_psutil = False
else:
    have_psutil = True


class Aborted(Predefined):
    """
    <dl>
    <dt>'$Aborted'
        <dd>is returned by a calculation that has been aborted.
    </dl>
    """

    name = "$Aborted"


class ByteOrdering(Predefined):
    """
    <dl>
      <dt>'$ByteOrdering'
      <dd>returns the native ordering of bytes in binary data on your computer system.
    </dl>

    X> $ByteOrdering
     = 1

    #> $ByteOrdering == -1 || $ByteOrdering == 1
     = True
    """

    name = "$ByteOrdering"

    def evaluate(self, evaluation) -> Integer:
        return Integer(1 if sys.byteorder == "big" else -1)


class CommandLine(Predefined):
    """
    <dl>
    <dt>'$CommandLine'
      <dd>is a list of strings passed on the command line to launch the Mathics session.
    </dl>
    >> $CommandLine
     = {...}
    """

    name = "$CommandLine"

    def evaluate(self, evaluation) -> Expression:
        return Expression(SymbolList, *(String(arg) for arg in sys.argv))


class Environment(Builtin):
    """
    <dl>
      <dt>'Environment[$var$]'
      <dd>gives the value of an operating system environment variable.
    </dl>
    X> Environment["HOME"]
     = ...
    """

    def apply(self, var, evaluation):
        "Environment[var_?StringQ]"
        env_var = var.get_string_value()
        if env_var not in os.environ:
            return SymbolFailed
        else:
            return String(os.environ[env_var])


class Failed(Predefined):
    """
    <dl>
    <dt>'$Failed'
        <dd>is returned by some functions in the event of an error.
    </dl>

    #> Get["nonexistent_file.m"]
     : Cannot open nonexistent_file.m.
     = $Failed
    """

    name = "$Failed"


class GetEnvironment(Builtin):
    """
    <dl>
    <dt>'GetEnvironment["$var$"]'
        <dd>gives the setting corresponding to the variable "var" in the operating system environment.
    </dl>

    X> GetEnvironment["HOME"]
    = ...
    """

    def apply(self, var, evaluation):
        "GetEnvironment[var___]"
        if isinstance(var, String):
            env_var = var.get_string_value()
            tup = (
                env_var,
                "System`None"
                if env_var not in os.environ
                else String(os.environ[env_var]),
            )

            return Expression(SymbolRule, *tup)

        env_vars = var.get_sequence()
        if len(env_vars) == 0:
            rules = [
                Expression(SymbolRule, name, value)
                for name, value in os.environ.items()
            ]
            return Expression(SymbolList, *rules)


class Machine(Predefined):
    """
    <dl>
    <dt>'$Machine'
        <dd>returns a string describing the type of computer system on which the Mathics is being run.
    </dl>
    X> $Machine
     = linux
    """

    name = "$Machine"

    def evaluate(self, evaluation) -> String:
        return String(sys.platform)


class MachineName(Predefined):
    """
    <dl>
      <dt>'$MachineName'
      <dd>is a string that gives the assigned name of the computer on which Mathics is being run, if such a name is defined.
    </dl>
    X> $MachineName
     = buster
    """

    name = "$MachineName"

    def evaluate(self, evaluation) -> String:
        return String(platform.uname().node)


class MathicsVersion(Predefined):
    r"""
    <dl>
      <dt>'MathicsVersion'
      <dd>this string is the version of Mathics we are running.
    </dl>

    >> MathicsVersion
    = ...
    """

    def evaluate(self, evaluation) -> String:
        return String(__version__)


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
        "Names[pattern_]"
        headname = pattern.get_head_name()
        if headname == "System`StringExpression":
            pattern = re.compile(to_regex(pattern, evaluation))
        else:
            pattern = pattern.get_string_value()

        if pattern is None:
            return

        names = set([])
        for full_name in evaluation.definitions.get_matching_names(pattern):
            short_name = strip_context(full_name)
            names.add(short_name if short_name not in names else full_name)

        # TODO: Mathematica ignores contexts when it sorts the list of
        # names.
        return Expression(SymbolList, *[String(name) for name in sorted(names)])


class Packages(Predefined):
    """
    <dl>
      <dt>'$Packages'
      <dd>returns a list of the contexts corresponding to all packages which have been loaded into Mathics.
    </dl>

    X> $Packages
    = {ImportExport`,XML`,Internal`,System`,Global`}
    #> MemberQ[$Packages, "System`"]
    = True
    """

    name = "$Packages"
    rules = {
        "$Packages": '{"ImportExport`",  "XML`","Internal`", "System`", "Global`"}'
    }


class ParentProcessID(Predefined):
    r"""
    <dl>
      <dt>'$ParentProcesID'
      <dd>gives the ID assigned to the process which invokes the \Mathics by the operating system under which it is run.
    </dl>

    >> $ParentProcessID
     = ...

    #> Head[$ParentProcessID] == Integer
     = True
    """

    name = "$ParentProcessID"

    def evaluate(self, evaluation) -> Integer:
        return Integer(os.getppid())


class ProcessID(Predefined):
    r"""
    <dl>
      <dt>'$ProcessID'
      <dd>gives the ID assigned to the \Mathics process by the operating system under which it is run.
    </dl>

    >> $ProcessID
     = ...

    #> Head[$ProcessID] == Integer
     = True
    """

    name = "$ProcessID"

    def evaluate(self, evaluation) -> Integer:
        return Integer(os.getpid())


class ProcessorType(Predefined):
    r"""
    <dl>
    <dt>'$ProcessorType'
        <dd>gives a string giving the architecture of the processor on which the \Mathics is being run.
    </dl>
    X> $ProcessorType
    = x86_64
    """

    name = "$ProcessorType"

    def evaluate(self, evaluation):
        return String(platform.machine())


class ScriptCommandLine(Predefined):
    """
    <dl>
    <dt>'$ScriptCommandLine'
      <dd>is a list of string arguments when running the kernel is script mode.
    </dl>
    >> $ScriptCommandLine
     = {...}
    """

    name = "$ScriptCommandLine"

    def evaluate(self, evaluation):
        try:
            dash_index = sys.argv.index("--")
        except ValueError:
            # not run in script mode
            return Expression(SymbolList)

        return Expression(
            SymbolList, *(String(arg) for arg in sys.argv[dash_index + 1 :])
        )


class Run(Builtin):
    """
    <dl>
      <dt>'Run[$command$]'
      <dd>runs command as an external operating system command, returning the exit code obtained.
    </dl>
    X> Run["date"]
     = ...
    """

    def apply(self, command, evaluation):
        "Run[command_?StringQ]"
        command_str = command.to_python()
        return Integer(subprocess.call(command_str, shell=True))


class SystemID(Predefined):
    r"""
    <dl>
       <dt>'$SystemID'
       <dd>is a short string that identifies the type of computer system on which the \Mathics is being run.
    </dl>
    X> $SystemID
     = linux
    """

    name = "$SystemID"

    def evaluate(self, evaluation) -> String:
        return String(sys.platform)


class SystemWordLength(Predefined):
    r"""
    <dl>
      <dt>'$SystemWordLength'
      <dd>gives the effective number of bits in raw machine words on the computer system where \Mathics is running.
    </dl>
    X> $SystemWordLength
    = 64

    #> Head[$SystemWordLength] == Integer
     = True
    """

    name = "$SystemWordLength"

    def evaluate(self, evaluation) -> Integer:
        # https://docs.python.org/3/library/platform.html#module-platform
        # says it is more reliable to get bits using sys.maxsize
        # than platform.architecture()[0]
        size = 128
        while not sys.maxsize > 2 ** size:
            size >>= 1
        return Integer(size << 1)


class UserName(Predefined):
    r"""
    <dl>
      <dt>$UserName
      <dd>returns a string describing the type of computer system on which
      \Mathics is being run.
    </dl>

    X> $UserName
     = ...
    """

    name = "$UserName"

    def evaluate(self, evaluation) -> String:
        try:
            user = os.getlogin()
        except:
            import pwd

            user = pwd.getpwuid(os.getuid())[0]
        return String(user)


class Version(Predefined):
    """
    <dl>
      <dt>'$Version'
      <dd>returns a string with the current Mathics version and the versions of relevant libraries.
    </dl>

    >> $Version
     = Mathics ...
    """

    name = "$Version"

    def evaluate(self, evaluation) -> String:
        return String(version_string.replace("\n", " "))


class VersionNumber(Predefined):
    r"""
    <dl>
      <dt>'$VersionNumber'
      <dd>is a real number which gives the current Wolfram Language version that \Mathics tries to be compatible with.
    </dl>

    >> $VersionNumber
    = ...
    """

    name = "$VersionNumber"
    value = 10.0

    def evaluate(self, evaluation) -> Real:
        # Make this be whatever the latest Mathematica release is,
        # assuming we are trying to be compatible with this.
        return Real(self.value)


if have_psutil:

    class SystemMemory(Predefined):
        """
        <dl>
          <dt>'$SystemMemory'
          <dd>Returns the total amount of physical memory.
        </dl>

        >> $SystemMemory
         = ...
        """

        name = "$SystemMemory"

        def evaluate(self, evaluation) -> Integer:
            totalmem = psutil.virtual_memory().total
            return Integer(totalmem)

    class MemoryAvailable(Builtin):
        """
        <dl>
          <dt>'MemoryAvailable'
          <dd>Returns the amount of the available physical memory.
        </dl>

        >> MemoryAvailable[]
         = ...

        The relationship between $SystemMemory, MemoryAvailable, and MemoryInUse:
        >> $SystemMemory > MemoryAvailable[] > MemoryInUse[]
         = True
        """

        def apply(self, evaluation) -> Integer:
            """MemoryAvailable[]"""
            totalmem = psutil.virtual_memory().available
            return Integer(totalmem)


else:

    class SystemMemory(Predefined):
        """
        <dl>
          <dt>'$SystemMemory'
          <dd>Returns the total amount of physical memory when Python module "psutil" is installed.
          This system however doesn't have that installed, so -1 is returned instead.
        </dl>

        >> $SystemMemory
         = -1
        """

        name = "$SystemMemory"

        def evaluate(self, evaluation) -> Integer:
            return Integer(-1)

    class MemoryAvailable(Builtin):
        """
        <dl>
          <dt>'MemoryAvailable'
          <dd>Returns the amount of the available physical when Python module "psutil" is installed.
          This system however doesn't have that installed, so -1 is returned instead.
        </dl>

        >> MemoryAvailable[]
         = -1
        """

        def apply(self, evaluation) -> Integer:
            """MemoryAvailable[]"""
            return Integer(-1)


class MemoryInUse(Builtin):
    """
    <dl>
      <dt>'MemoryInUse[]'
      <dd>Returns the amount of memory used by the definitions object.
    </dl>

    >> MemoryInUse[]
     = ...
    """

    def apply_0(self, evaluation) -> Integer:
        """MemoryInUse[]"""
        # Partially borrowed from https://code.activestate.com/recipes/577504/
        from itertools import chain
        from sys import getsizeof

        definitions = evaluation.definitions
        seen = set()
        default_size = getsizeof(0)
        handlers = {
            tuple: iter,
            list: iter,
            dict: (lambda d: chain.from_iterable(d.items())),
            set: iter,
            frozenset: iter,
        }

        def sizeof(obj):
            if id(obj) in seen:
                return 0
            seen.add(id(obj))
            s = getsizeof(obj, default_size)
            for typ, handler in handlers.items():
                if isinstance(obj, typ):
                    s += sum(map(sizeof, handler(obj)))
                    break
            return s

        return Integer(sizeof(definitions))


class Share(Builtin):
    """
    <dl>
      <dt>'Share[]'
      <dd>Tries to reduce the amount of memory required to store definitions, by reducing duplicated definitions.
          Now it just do nothing.
      <dt>'Share[Symbol]'
      <dd>Tries to reduce the amount of memory required to store definitions associated to $Symbol$.
    </dl>

    >> Share[]
     = ...
    """

    def apply_0(self, evaluation) -> Integer:
        """Share[]"""
        # TODO: implement a routine that swap all the definitions,
        # collecting repeated symbols and expressions, and then
        # remplace them by references.
        # Return the amount of memory recovered.
        return Integer0

    def apply_1(self, symbol, evaluation) -> Integer:
        """Share[symbol_Symbol]"""
        # TODO: implement a routine that swap all the definitions,
        # collecting repeated symbols and expressions, and then
        # remplace them by references.
        # Return the amount of memory recovered.
        return Integer0
