# -*- coding: utf-8 -*-
# cython: language_level=3

"""
File Operations
"""

import os
import sys
import io
import shutil
import zlib
import base64
import tempfile
import time
import struct
import mpmath
import math
import sympy
import requests
import pathlib

from io import BytesIO
import os.path as osp
from itertools import chain

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics_scanner.errors import IncompleteSyntaxError, InvalidSyntaxError
from mathics_scanner import TranslateError
from mathics.core.parser import MathicsFileLineFeeder, MathicsMultiLineFeeder, parse


from mathics.core.expression import (
    BoxError,
    Complex,
    Expression,
    Integer,
    MachineReal,
    Real,
    String,
    Symbol,
    SymbolFailed,
    SymbolFalse,
    SymbolNull,
    SymbolTrue,
    SymbolList,
    from_mpmath,
    from_python,
    valid_context_name,
)
from mathics.core.numbers import dps
from mathics.builtin.base import Builtin, Predefined, BinaryOperator, PrefixOperator
from mathics.builtin.numeric import Hash
from mathics.builtin.strings import to_python_encoding, to_regex
from mathics.builtin.base import MessageException
from mathics.settings import ROOT_DIR
import re

INITIAL_DIR = os.getcwd()
HOME_DIR = osp.expanduser("~")
SYS_ROOT_DIR = "/" if os.name == "posix" else "\\"
TMP_DIR = tempfile.gettempdir()
DIRECTORY_STACK = [INITIAL_DIR]
INPUT_VAR = ""
INPUTFILE_VAR = ""
PATH_VAR = [".", HOME_DIR, osp.join(ROOT_DIR, "data"), osp.join(ROOT_DIR, "packages")]


def create_temporary_file(suffix=None, delete=False):
    if suffix == "":
        suffix = None

    fp = tempfile.NamedTemporaryFile(delete=delete, suffix=suffix)
    result = fp.name
    fp.close()
    return result


def urlsave_tmp(url, location=None, **kwargs):
    suffix = ""
    strip_url = url.split("/")
    if len(strip_url) > 3:
        strip_url = strip_url[-1]
        if strip_url != "":
            suffix = strip_url[len(strip_url.split(".")[0]) :]
        try:
            r = requests.get(url, allow_redirects=True)
            if location is None:
                location = create_temporary_file(suffix=suffix)
            with open(location, "wb") as fp:
                fp.write(r.content)
                result = fp.name
        except Exception:
            result = None
    return result


def path_search(filename):
    # For names of the form "name`", search for name.mx and name.m
    if filename[-1] == "`":
        filename = filename[:-1].replace("`", osp.sep)
        for ext in [".mx", ".m"]:
            result = path_search(filename + ext)
            if result is not None:
                filename = None
                break
    if filename is not None:
        result = None
        # If filename is an internet address, download the file
        # and store it in a temporal location
        lenfn = len(filename)
        if (
            (lenfn > 7 and filename[:7] == "http://")
            or (lenfn > 8 and filename[:8] == "https://")
            or (lenfn > 6 and filename[:6] == "ftp://")
        ):
            result = urlsave_tmp(filename)
        else:
            for p in PATH_VAR + [""]:
                path = osp.join(p, filename)
                if osp.exists(path):
                    result = path
                    break

            # If FindFile resolves to a dir, search within for Kernel/init.m and init.m
            if result is not None and osp.isdir(result):
                for ext in [osp.join("Kernel", "init.m"), "init.m"]:
                    tmp = osp.join(result, ext)
                    if osp.isfile(tmp):
                        return tmp
    return result


def count():
    n = 0
    while True:
        yield n
        n += 1


NSTREAMS = count()  # use next(NSTREAMS)
STREAMS = [sys.stdin, sys.stdout, sys.stderr]
next(NSTREAMS)
next(NSTREAMS)
next(NSTREAMS)


def _channel_to_stream(channel, mode="r"):
    if isinstance(channel, String):
        name = channel.get_string_value()
        opener = mathics_open(name, mode)
        opener.__enter__()
        n = opener.n
        if mode in ["r", "rb"]:
            head = "InputStream"
        elif mode in ["w", "a", "wb", "ab"]:
            head = "OutputStream"
        else:
            raise ValueError("Unknown format {0}".format(mode))
        return Expression(head, channel, Integer(n))
    elif channel.has_form("InputStream", 2):
        return channel
    elif channel.has_form("OutputStream", 2):
        return channel
    else:
        return None


def _lookup_stream(n=None):
    if n is None:
        return None
    elif n is not None:
        try:
            return STREAMS[n]
        except IndexError:
            return None


class mathics_open:
    def __init__(self, name, mode="r", encoding=None):
        self.name = name
        self.mode = mode
        self.encoding = encoding
        self.old_inputfile_var = None  # Set in __enter__ and __exit__

        if mode not in ["r", "w", "a", "rb", "wb", "ab"]:
            raise ValueError("Can't handle mode {0}".format(mode))

    def __enter__(self):
        # find path
        path = path_search(self.name)
        if path is None and self.mode in ["w", "a", "wb", "ab"]:
            path = self.name
        if path is None:
            raise IOError

        # determine encoding
        if "b" not in self.mode:
            encoding = self.encoding
            if encoding is None:
                python_encoding = None
            else:
                python_encoding = to_python_encoding(encoding)
                if python_encoding is None:
                    raise MessageException("General", "charcode", encoding)
        else:
            python_encoding = None

        # open the stream
        stream = io.open(path, self.mode, encoding=python_encoding)
        global INPUTFILE_VAR
        self.old_inputfile_var = INPUTFILE_VAR
        INPUTFILE_VAR = osp.abspath(path)

        # build the Expression
        n = next(NSTREAMS)
        if self.mode in ["r", "rb"]:
            self.expr = Expression("InputStream", String(path), Integer(n))
        elif self.mode in ["w", "a", "wb", "ab"]:
            self.expr = Expression("OutputStream", String(path), Integer(n))
        else:
            raise IOError

        STREAMS.append(stream)

        self.n = n

        return stream

    def __exit__(self, type, value, traceback):
        strm = STREAMS[self.n]
        global INPUTFILE_VAR
        INPUTFILE_VAR = self.old_inputfile_var or ""
        self.oldinputfile_var = None
        if strm is not None:
            strm.close()
            STREAMS[self.n] = None


class InitialDirectory(Predefined):
    """
    <dl>
    <dt>'$InitialDirectory'
      <dd>returns the directory from which \\Mathics was started.
    </dl>

    >> $InitialDirectory
     = ...
    """

    name = "$InitialDirectory"

    def evaluate(self, evaluation):
        global INITIAL_DIR
        return String(INITIAL_DIR)


class InstallationDirectory(Predefined):
    """
    <dl>
    <dt>'$InstallationDirectory'
      <dd>returns the directory in which \\Mathics was installed.
    </dl>

    >> $InstallationDirectory
     = ...
    """

    name = "$InstallationDirectory"

    def evaluate(self, evaluation):
        global ROOT_DIR
        return String(ROOT_DIR)


class HomeDirectory(Predefined):
    """
    <dl>
    <dt>'$HomeDirectory'
      <dd>returns the users HOME directory.
    </dl>

    >> $HomeDirectory
     = ...
    """

    name = "$HomeDirectory"

    attributes = "Protected"

    def evaluate(self, evaluation):
        global HOME_DIR
        return String(HOME_DIR)


class RootDirectory(Predefined):
    """
    <dl>
    <dt>'$RootDirectory'
      <dd>returns the system root directory.
    </dl>

    >> $RootDirectory
     = ...
    """

    name = "$RootDirectory"

    attributes = "Protected"

    def evaluate(self, evaluation):
        global SYS_ROOT_DIR
        return String(SYS_ROOT_DIR)


class TemporaryDirectory(Predefined):
    """
    <dl>
    <dt>'$TemporaryDirectory'
      <dd>returns the directory used for temporary files.
    </dl>

    >> $TemporaryDirectory
     = ...
    """

    name = "$TemporaryDirectory"

    def evaluate(self, evaluation):
        return String(TMP_DIR)


class Input(Predefined):
    """
    <dl>
    <dt>'$Input'
      <dd>is the name of the stream from which input is currently being read.
    </dl>

    >> $Input
     = #<--#
    """

    attributes = ("Protected", "ReadProtected")
    name = "$Input"

    def evaluate(self, evaluation):
        global INPUT_VAR
        return String(INPUT_VAR)


class InputFileName(Predefined):
    """
    <dl>
    <dt>'$InputFileName'
      <dd>is the name of the file from which input is currently being read.
    </dl>

    While in interactive mode, '$InputFileName' is "".
    X> $InputFileName
    """

    name = "$InputFileName"

    def evaluate(self, evaluation):
        global INPUTFILE_VAR
        return String(INPUTFILE_VAR)


class PathnameSeparator(Predefined):
    """
    <dl>
    <dt>'$PathnameSeparator'
      <dd>returns a string for the seperator in paths.
    </dl>

    >> $PathnameSeparator
     = ...
    """

    name = "$PathnameSeparator"

    def evaluate(self, evaluation):
        return String(os.sep)


class Path(Predefined):
    """
    <dl>
    <dt>'$Path'
      <dd>returns the list of directories to search when looking for a file.
    </dl>

    >> $Path
     = ...
    """

    attributes = ("Unprotected",)
    name = "$Path"

    def evaluate(self, evaluation):
        return Expression(SymbolList, *[String(p) for p in PATH_VAR])


class OperatingSystem(Predefined):
    """
    <dl>
    <dt>'$OperatingSystem'
      <dd>gives the type of operating system running Mathics.
    </dl>

    >> $OperatingSystem
     = ...
    """

    attributes = ("Locked", "Protected")
    name = "$OperatingSystem"

    def evaluate(self, evaluation):
        if os.name == "posix":
            return String("Unix")
        elif os.name == "nt":
            return String("Windows")
        elif os.name == "os2":
            return String("MacOSX")
        else:
            return String("Unknown")


class EndOfFile(Builtin):
    """
    <dl>
    <dt>'EndOfFile'
      <dd>is returned by 'Read' when the end of an input stream is reached.
    </dl>
    """


# TODO: Improve docs for these Read[] arguments.
class Byte(Builtin):
    """
    <dl>
    <dt>'Byte'
      <dd>is a data type for 'Read'.
    </dl>
    """


class Character(Builtin):
    """
    <dl>
    <dt>'Character'
      <dd>is a data type for 'Read'.
    </dl>
    """


class Expression_(Builtin):
    """
    <dl>
    <dt>'Expression'
      <dd>is a data type for 'Read'.
    </dl>
    """

    name = "Expression"


class Number_(Builtin):
    """
    <dl>
    <dt>'Number'
      <dd>is a data type for 'Read'.
    </dl>
    """

    name = "Number"


class Record(Builtin):
    """
    <dl>
    <dt>'Record'
      <dd>is a data type for 'Read'.
    </dl>
    """


class Word(Builtin):
    """
    <dl>
    <dt>'Word'
      <dd>is a data type for 'Read'.
    </dl>
    """


class Read(Builtin):
    """
    <dl>
      <dt>'Read[stream]'
      <dd>reads the input stream and returns one expression.

      <dt>'Read[stream, type]'
      <dd>reads the input stream and returns an object of the given type.
    </dl>

    ## Malformed InputString
    #> Read[InputStream[String], {Word, Number}]
     = Read[InputStream[String], {Word, Number}]

    ## Correctly formed InputString but not open
    #> Read[InputStream[String, -1], {Word, Number}]
     : InputStream[String, -1] is not open.
     = Read[InputStream[String, -1], {Word, Number}]

    ## String
    >> str = StringToStream["abc123"];
    >> Read[str, String]
     = abc123
    #> Read[str, String]
     = EndOfFile
    #> Close[str];

    ## Word
    >> str = StringToStream["abc 123"];
    >> Read[str, Word]
     = abc
    >> Read[str, Word]
     = 123
    #> Read[str, Word]
     = EndOfFile
    #> Close[str];
    #> str = StringToStream[""];
    #> Read[str, Word]
     = EndOfFile
    #> Read[str, Word]
     = EndOfFile
    #> Close[str];

    ## Number
    >> str = StringToStream["123, 4"];
    >> Read[str, Number]
     = 123
    >> Read[str, Number]
     = 4
    #> Read[str, Number]
     = EndOfFile
    #> Close[str];
    #> str = StringToStream["123xyz 321"];
    #> Read[str, Number]
     = 123
    #> Quiet[Read[str, Number]]
     = $Failed

    ## Real
    #> str = StringToStream["123, 4abc"];
    #> Read[str, Real]
     = 123.
    #> Read[str, Real]
     = 4.
    #> Quiet[Read[str, Number]]
     = $Failed

    #> Close[str];
    #> str = StringToStream["1.523E-19"]; Read[str, Real]
     = 1.523*^-19
    #> Close[str];
    #> str = StringToStream["-1.523e19"]; Read[str, Real]
     = -1.523*^19
    #> Close[str];
    #> str = StringToStream["3*^10"]; Read[str, Real]
     = 3.*^10
    #> Close[str];
    #> str = StringToStream["3.*^10"]; Read[str, Real]
     = 3.*^10
    #> Close[str];

    ## Expression
    #> str = StringToStream["x + y Sin[z]"]; Read[str, Expression]
     = x + y Sin[z]
    #> Close[str];
    ## #> str = Quiet[StringToStream["Sin[1 123"]; Read[str, Expression]]
    ##  = $Failed

    ## Multiple types
    >> str = StringToStream["123 abc"];
    >> Read[str, {Number, Word}]
     = {123, abc}
    #> Read[str, {Number, Word}]
     = EndOfFile
    #> Close[str];

    #> str = StringToStream["123 abc"];
    #> Quiet[Read[str, {Word, Number}]]
     = $Failed
    #> Close[str];

    #> str = StringToStream["123 123"];  Read[str, {Real, Number}]
     = {123., 123}
    #> Close[str];

    #> Quiet[Read[str, {Real}]]
     = Read[InputStream[String, ...], {Real}]

    Multiple lines:
    >> str = StringToStream["\\"Tengo una\\nvaca lechera.\\""]; Read[str]
     = Tengo una
     . vaca lechera.

    """

    messages = {
        "openx": "`1` is not open.",
        "readf": "`1` is not a valid format specification.",
        "readn": "Invalid real number found when reading from `1`.",
        "readt": "Invalid input found when reading `1` from `2`.",
        "intnm": (
            "Non-negative machine-sized integer expected at " "position 3 in `1`."
        ),
    }

    rules = {
        "Read[stream_]": "Read[stream, Expression]",
    }

    options = {
        "NullRecords": "False",
        "NullWords": "False",
        "RecordSeparators": '{"\r\n", "\n", "\r"}',
        "TokenWords": "{}",
        "WordSeparators": '{" ", "\t"}',
    }

    attributes = "Protected"

    def check_options(self, options):
        # Options
        # TODO Proper error messages

        result = {}
        keys = list(options.keys())

        # AnchoredSearch
        if "System`AnchoredSearch" in keys:
            anchored_search = options["System`AnchoredSearch"].to_python()
            assert anchored_search in [True, False]
            result["AnchoredSearch"] = anchored_search

        # IgnoreCase
        if "System`IgnoreCase" in keys:
            ignore_case = options["System`IgnoreCase"].to_python()
            assert ignore_case in [True, False]
            result["IgnoreCase"] = ignore_case

        # WordSearch
        if "System`WordSearch" in keys:
            word_search = options["System`WordSearch"].to_python()
            assert word_search in [True, False]
            result["WordSearch"] = word_search

        # RecordSeparators
        if "System`RecordSeparators" in keys:
            record_separators = options["System`RecordSeparators"].to_python()
            assert isinstance(record_separators, list)
            assert all(
                isinstance(s, str) and s[0] == s[-1] == '"' for s in record_separators
            )
            record_separators = [s[1:-1] for s in record_separators]
            result["RecordSeparators"] = record_separators

        # WordSeparators
        if "System`WordSeparators" in keys:
            word_separators = options["System`WordSeparators"].to_python()
            assert isinstance(word_separators, list)
            assert all(
                isinstance(s, str) and s[0] == s[-1] == '"' for s in word_separators
            )
            word_separators = [s[1:-1] for s in word_separators]
            result["WordSeparators"] = word_separators

        # NullRecords
        if "System`NullRecords" in keys:
            null_records = options["System`NullRecords"].to_python()
            assert null_records in [True, False]
            result["NullRecords"] = null_records

        # NullWords
        if "System`NullWords" in keys:
            null_words = options["System`NullWords"].to_python()
            assert null_words in [True, False]
            result["NullWords"] = null_words

        # TokenWords
        if "System`TokenWords" in keys:
            token_words = options["System`TokenWords"].to_python()
            assert token_words == []
            result["TokenWords"] = token_words

        return result

    def apply(self, channel, types, evaluation, options):
        "Read[channel_, types_, OptionsPattern[Read]]"

        if channel.has_form("OutputStream", 2):
            evaluation.message("General", "openw", channel)
            return

        strm = _channel_to_stream(channel, "r")

        if strm is None:
            return

        [name, n] = strm.get_leaves()

        stream = _lookup_stream(n.get_int_value())

        if stream is None or stream.closed:
            evaluation.message("Read", "openx", strm)
            return

        # Wrap types in a list (if it isn't already one)
        if types.has_form("List", None):
            types = types._leaves
        else:
            types = (types,)

        types = (
            typ._leaves[0] if typ.get_head_name() == "System`Hold" else typ
            for typ in types
        )
        types = Expression("List", *types)

        READ_TYPES = [
            Symbol(k)
            for k in [
                "Byte",
                "Character",
                "Expression",
                "Number",
                "Real",
                "Record",
                "String",
                "Word",
            ]
        ]

        for typ in types.leaves:
            if typ not in READ_TYPES:
                evaluation.message("Read", "readf", typ)
                return SymbolFailed

        # Options
        # TODO Implement extra options
        py_options = self.check_options(options)
        # null_records = py_options['NullRecords']
        # null_words = py_options['NullWords']
        record_separators = py_options["RecordSeparators"]
        # token_words = py_options['TokenWords']
        word_separators = py_options["WordSeparators"]

        name = name.to_python()

        result = []

        def reader(stream, word_separators, accepted=None):
            while True:
                word = ""
                while True:
                    try:
                        tmp = stream.read(1)
                    except UnicodeDecodeError:
                        tmp = " "  # ignore
                        evaluation.message("General", "ucdec")

                    if tmp == "":
                        if word == "":
                            pos = stream.tell()
                            newchar = stream.read(1)
                            if pos == stream.tell():
                                raise EOFError
                            else:
                                if newchar:
                                    word = newchar
                                    continue
                                else:
                                    yield word
                                    continue
                        last_word = word
                        word = ""
                        yield last_word
                        break

                    if tmp in word_separators:
                        if word == "":
                            continue
                        if stream.seekable():
                            # stream.seek(-1, 1) #Python3
                            stream.seek(stream.tell() - 1)
                        last_word = word
                        word = ""
                        yield last_word
                        break

                    if accepted is not None and tmp not in accepted:
                        last_word = word
                        word = ""
                        yield last_word
                        break

                    word += tmp

        read_word = reader(stream, word_separators)
        read_record = reader(stream, record_separators)
        read_number = reader(
            stream,
            word_separators + record_separators,
            ["+", "-", "."] + [str(i) for i in range(10)],
        )
        read_real = reader(
            stream,
            word_separators + record_separators,
            ["+", "-", ".", "e", "E", "^", "*"] + [str(i) for i in range(10)],
        )
        for typ in types.leaves:
            try:
                if typ == Symbol("Byte"):
                    tmp = stream.read(1)
                    if tmp == "":
                        raise EOFError
                    result.append(ord(tmp))
                elif typ == Symbol("Character"):
                    tmp = stream.read(1)
                    if tmp == "":
                        raise EOFError
                    result.append(tmp)
                elif typ == Symbol("Expression"):
                    tmp = next(read_record)
                    while True:
                        try:
                            feeder = MathicsMultiLineFeeder(tmp)
                            expr = parse(evaluation.definitions, feeder)
                            break
                        except (IncompleteSyntaxError, InvalidSyntaxError) as e:
                            try:
                                nextline = next(read_record)
                                tmp = tmp + "\n" + nextline
                            except EOFError:
                                expr = Symbol("EndOfFile")
                                break

                    if expr is None:
                        evaluation.message(
                            "Read", "readt", tmp, Expression("InputSteam", name, n)
                        )
                        return SymbolFailed
                    else:
                        result.append(expr)

                elif typ == Symbol("Number"):
                    tmp = next(read_number)
                    try:
                        tmp = int(tmp)
                    except ValueError:
                        try:
                            tmp = float(tmp)
                        except ValueError:
                            evaluation.message(
                                "Read", "readn", Expression("InputSteam", name, n)
                            )
                            return SymbolFailed
                    result.append(tmp)

                elif typ == Symbol("Real"):
                    tmp = next(read_real)
                    tmp = tmp.replace("*^", "E")
                    try:
                        tmp = float(tmp)
                    except ValueError:
                        evaluation.message(
                            "Read", "readn", Expression("InputSteam", name, n)
                        )
                        return SymbolFailed
                    result.append(tmp)
                elif typ == Symbol("Record"):
                    result.append(next(read_record))
                elif typ == Symbol("String"):
                    tmp = stream.readline()
                    if len(tmp) == 0:
                        raise EOFError
                    result.append(tmp.rstrip("\n"))
                elif typ == Symbol("Word"):
                    result.append(next(read_word))

            except EOFError:
                return Symbol("EndOfFile")
            except UnicodeDecodeError:
                evaluation.message("General", "ucdec")

        if len(result) == 1:
            return from_python(*result)

        return from_python(result)

    def apply_nostream(self, arg1, arg2, evaluation):
        "Read[arg1_, arg2_]"
        evaluation.message("General", "stream", arg1)
        return


class Write(Builtin):
    """
    <dl>
    <dt>'Write[$channel$, $expr1$, $expr2$, ...]'
      <dd>writes the expressions to the output channel followed by a newline.
    </dl>

    >> str = OpenWrite[]
     = ...
    >> Write[str, 10 x + 15 y ^ 2]
    >> Write[str, 3 Sin[z]]
    >> Close[str]
     = ...
    >> str = OpenRead[%];
    >> ReadList[str]
     = {10 x + 15 y ^ 2, 3 Sin[z]}
    #> Close[str];
    """

    attributes = "Protected"

    def apply(self, channel, expr, evaluation):
        "Write[channel_, expr___]"

        strm = _channel_to_stream(channel)

        if strm is None:
            return

        n = strm.leaves[1].get_int_value()
        stream = _lookup_stream(n)

        if stream is None or stream.closed:
            evaluation.message("General", "openx", channel)
            return SymbolNull

        expr = expr.get_sequence()
        expr = Expression("Row", Expression("List", *expr))

        evaluation.format = "text"
        text = evaluation.format_output(from_python(expr))
        stream.write(str(text) + "\n")
        return SymbolNull


class _BinaryFormat(object):
    """
    Container for BinaryRead readers and BinaryWrite writers
    """

    @staticmethod
    def _IEEE_real(real):
        if math.isnan(real):
            return Symbol("Indeterminate")
        elif math.isinf(real):
            return Expression("DirectedInfinity", Integer((-1) ** (real < 0)))
        else:
            return Real(real)

    @staticmethod
    def _IEEE_cmplx(real, imag):
        if math.isnan(real) or math.isnan(imag):
            return Symbol("Indeterminate")
        elif math.isinf(real) or math.isinf(imag):
            if math.isinf(real) and math.isinf(imag):
                return Symbol("Indeterminate")
            return Expression(
                "DirectedInfinity",
                Expression(
                    "Complex",
                    (-1) ** (real < 0) if math.isinf(real) else 0,
                    (-1) ** (imag < 0) if math.isinf(imag) else 0,
                ),
            )
        else:
            return Complex(MachineReal(real), MachineReal(imag))

    @classmethod
    def get_readers(cls):
        readers = {}
        for funcname in dir(cls):
            if funcname.startswith("_") and funcname.endswith("_reader"):
                readers[funcname[1:-7]] = getattr(cls, funcname)
        return readers

    @classmethod
    def get_writers(cls):
        writers = {}
        for funcname in dir(cls):
            if funcname.startswith("_") and funcname.endswith("_writer"):
                writers[funcname[1:-7]] = getattr(cls, funcname)
        return writers

    # Reader Functions

    @staticmethod
    def _Byte_reader(s):
        "8-bit unsigned integer"
        return Integer(*struct.unpack("B", s.read(1)))

    @staticmethod
    def _Character8_reader(s):
        "8-bit character"
        return String(struct.unpack("c", s.read(1))[0].decode("ascii"))

    @staticmethod
    def _Character16_reader(s):
        "16-bit character"
        return String(chr(*struct.unpack("H", s.read(2))))

    @staticmethod
    def _Complex64_reader(s):
        "IEEE single-precision complex number"
        return _BinaryFormat._IEEE_cmplx(*struct.unpack("ff", s.read(8)))

    @staticmethod
    def _Complex128_reader(s):
        "IEEE double-precision complex number"
        return _BinaryFormat._IEEE_cmplx(*struct.unpack("dd", s.read(16)))

    def _Complex256_reader(self, s):
        "IEEE quad-precision complex number"
        return Complex(self._Real128_reader(s), self._Real128_reader(s))

    @staticmethod
    def _Integer8_reader(s):
        "8-bit signed integer"
        return Integer(*struct.unpack("b", s.read(1)))

    @staticmethod
    def _Integer16_reader(s):
        "16-bit signed integer"
        return Integer(*struct.unpack("h", s.read(2)))

    @staticmethod
    def _Integer24_reader(s):
        "24-bit signed integer"
        b = s.read(3)
        return Integer(struct.unpack("<i", b"\x00" + b)[0] >> 8)

    @staticmethod
    def _Integer32_reader(s):
        "32-bit signed integer"
        return Integer(*struct.unpack("i", s.read(4)))

    @staticmethod
    def _Integer64_reader(s):
        "64-bit signed integer"
        return Integer(*struct.unpack("q", s.read(8)))

    @staticmethod
    def _Integer128_reader(s):
        "128-bit signed integer"
        a, b = struct.unpack("Qq", s.read(16))
        return Integer((b << 64) + a)

    @staticmethod
    def _Real32_reader(s):
        "IEEE single-precision real number"
        return _BinaryFormat._IEEE_real(*struct.unpack("f", s.read(4)))

    @staticmethod
    def _Real64_reader(s):
        "IEEE double-precision real number"
        return _BinaryFormat._IEEE_real(*struct.unpack("d", s.read(8)))

    @staticmethod
    def _Real128_reader(s):
        "IEEE quad-precision real number"
        # Workaround quad missing from struct
        # correctness is not guaranteed
        b = s.read(16)
        sig, sexp = b[:14], b[14:]

        # Sign / Exponent
        (sexp,) = struct.unpack("H", sexp)
        signbit = sexp // 0x8000
        expbits = sexp % 0x8000

        # Signifand
        try:
            fracbits = int.from_bytes(sig, byteorder="little")
        except AttributeError:  # Py2
            fracbits = int(sig[::-1].encode("hex"), 16)

        if expbits == 0x0000 and fracbits == 0:
            return Real(sympy.Float(0, 4965))
        elif expbits == 0x7FFF:
            if fracbits == 0:
                return Expression("DirectedInfinity", Integer((-1) ** signbit))
            else:
                return Symbol("Indeterminate")

        with mpmath.workprec(112):
            core = mpmath.fdiv(fracbits, 2 ** 112)
            if expbits == 0x000:
                assert fracbits != 0
                exp = -16382
                core = mpmath.fmul((-1) ** signbit, core)
            else:
                assert 0x0001 <= expbits <= 0x7FFE
                exp = expbits - 16383
                core = mpmath.fmul((-1) ** signbit, mpmath.fadd(1, core))

            if exp >= 0:
                result = mpmath.fmul(core, 2 ** exp)
            else:
                result = mpmath.fdiv(core, 2 ** -exp)

            return from_mpmath(result, dps(112))

    @staticmethod
    def _TerminatedString_reader(s):
        "null-terminated string of 8-bit characters"
        b = s.read(1)
        contents = b""
        while b != b"\x00":
            if b == b"":
                raise struct.error
            contents += b
            b = s.read(1)
        return String(contents.decode("ascii"))

    @staticmethod
    def _UnsignedInteger8_reader(s):
        "8-bit unsigned integer"
        return Integer(*struct.unpack("B", s.read(1)))

    @staticmethod
    def _UnsignedInteger16_reader(s):
        "16-bit unsigned integer"
        return Integer(*struct.unpack("H", s.read(2)))

    @staticmethod
    def _UnsignedInteger24_reader(s):
        "24-bit unsigned integer"
        return Integer(*struct.unpack("I", s.read(3) + b"\0"))

    @staticmethod
    def _UnsignedInteger32_reader(s):
        "32-bit unsigned integer"
        return Integer(*struct.unpack("I", s.read(4)))

    @staticmethod
    def _UnsignedInteger64_reader(s):
        "64-bit unsigned integer"
        return Integer(*struct.unpack("Q", s.read(8)))

    @staticmethod
    def _UnsignedInteger128_reader(s):
        "128-bit unsigned integer"
        a, b = struct.unpack("QQ", s.read(16))
        return Integer((b << 64) + a)

    # Writer Functions

    @staticmethod
    def _Byte_writer(s, x):
        "8-bit unsigned integer"
        s.write(struct.pack("B", x))

    @staticmethod
    def _Character8_writer(s, x):
        "8-bit character"
        s.write(struct.pack("c", x.encode("ascii")))

    # TODO
    # @staticmethod
    # def _Character16_writer(s, x):
    #     "16-bit character"
    #     pass

    @staticmethod
    def _Complex64_writer(s, x):
        "IEEE single-precision complex number"
        s.write(struct.pack("ff", x.real, x.imag))
        # return _BinaryFormat._IEEE_cmplx(*struct.unpack('ff', s.read(8)))

    @staticmethod
    def _Complex128_writer(s, x):
        "IEEE double-precision complex number"
        s.write(struct.pack("dd", x.real, x.imag))

    # TODO
    # @staticmethod
    # def _Complex256_writer(s, x):
    #     "IEEE quad-precision complex number"
    #     pass

    @staticmethod
    def _Integer8_writer(s, x):
        "8-bit signed integer"
        s.write(struct.pack("b", x))

    @staticmethod
    def _Integer16_writer(s, x):
        "16-bit signed integer"
        s.write(struct.pack("h", x))

    @staticmethod
    def _Integer24_writer(s, x):
        "24-bit signed integer"
        s.write(struct.pack("i", x << 8)[1:])

    @staticmethod
    def _Integer32_writer(s, x):
        "32-bit signed integer"
        s.write(struct.pack("i", x))

    @staticmethod
    def _Integer64_writer(s, x):
        "64-bit signed integer"
        s.write(struct.pack("q", x))

    @staticmethod
    def _Integer128_writer(s, x):
        "128-bit signed integer"
        a, b = x & 0xFFFFFFFFFFFFFFFF, x >> 64
        s.write(struct.pack("Qq", a, b))

    @staticmethod
    def _Real32_writer(s, x):
        "IEEE single-precision real number"
        s.write(struct.pack("f", x))

    @staticmethod
    def _Real64_writer(s, x):
        "IEEE double-precision real number"
        s.write(struct.pack("d", x))

    # TODO
    # @staticmethod
    # def _Real128_writer(s, x):
    #     "IEEE quad-precision real number"
    #     pass

    @staticmethod
    def _TerminatedString_writer(s, x):
        "null-terminated string of 8-bit characters"
        s.write(x.encode("utf-8"))

    @staticmethod
    def _UnsignedInteger8_writer(s, x):
        "8-bit unsigned integer"
        s.write(struct.pack("B", x))

    @staticmethod
    def _UnsignedInteger16_writer(s, x):
        "16-bit unsigned integer"
        s.write(struct.pack("H", x))

    @staticmethod
    def _UnsignedInteger24_writer(s, x):
        "24-bit unsigned integer"
        s.write(struct.pack("I", x << 8)[1:])

    @staticmethod
    def _UnsignedInteger32_writer(s, x):
        "32-bit unsigned integer"
        s.write(struct.pack("I", x))

    @staticmethod
    def _UnsignedInteger64_writer(s, x):
        "64-bit unsigned integer"
        s.write(struct.pack("Q", x))

    @staticmethod
    def _UnsignedInteger128_writer(s, x):
        "128-bit unsigned integer"
        a, b = x & 0xFFFFFFFFFFFFFFFF, x >> 64
        s.write(struct.pack("QQ", a, b))


class BinaryWrite(Builtin):
    """
    <dl>
    <dt>'BinaryWrite[$channel$, $b$]'
      <dd>writes a single byte given as an integer from 0 to 255.
    <dt>'BinaryWrite[$channel$, {b1, b2, ...}]'
      <dd>writes a sequence of byte.
    <dt>'BinaryWrite[$channel$, "string"]'
      <dd>writes the raw characters in a string.
    <dt>'BinaryWrite[$channel$, $x$, $type$]'
      <dd>writes $x$ as the specified type.
    <dt>'BinaryWrite[$channel$, {$x1$, $x2$, ...}, $type$]'
      <dd>writes a sequence of objects as the specified type.
    <dt>'BinaryWrite[$channel$, {$x1$, $x2$, ...}, {$type1$, $type2$, ...}]'
      <dd>writes a sequence of objects using a sequence of specified types.
    </dl>

    >> strm = OpenWrite[BinaryFormat -> True]
     = OutputStream[...]
    >> BinaryWrite[strm, {39, 4, 122}]
     = OutputStream[...]
    >> Close[strm]
     = ...
    >> strm = OpenRead[%, BinaryFormat -> True]
     = InputStream[...]
    >> BinaryRead[strm]
     = 39
    >> BinaryRead[strm, "Byte"]
     = 4
    >> BinaryRead[strm, "Character8"]
     = z
    >> Close[strm];

    Write a String
    >> strm = OpenWrite[BinaryFormat -> True]
     = OutputStream[...]
    >> BinaryWrite[strm, "abc123"]
     = OutputStream[...]
    >> Close[%]
     = ...

    Read as Bytes
    >> strm = OpenRead[%, BinaryFormat -> True]
     = InputStream[...]
    >> BinaryRead[strm, {"Character8", "Character8", "Character8", "Character8", "Character8", "Character8", "Character8"}]
     = {a, b, c, 1, 2, 3, EndOfFile}
    >> Close[strm]
     = ...

    Read as Characters
    >> strm = OpenRead[%, BinaryFormat -> True]
     = InputStream[...]
    >> BinaryRead[strm, {"Byte", "Byte", "Byte", "Byte", "Byte", "Byte", "Byte"}]
     = {97, 98, 99, 49, 50, 51, EndOfFile}
    >> Close[strm]
     = ...

    Write Type
    >> strm = OpenWrite[BinaryFormat -> True]
     = OutputStream[...]
    >> BinaryWrite[strm, 97, "Byte"]
     = OutputStream[...]
    >> BinaryWrite[strm, {97, 98, 99}, {"Byte", "Byte", "Byte"}]
     = OutputStream[...]
    >> Close[%]
     = ...

    ## Write then Read as Bytes
    #> WRb[bytes_, form_] := Module[{str, res={}, byte}, str = OpenWrite[BinaryFormat -> True]; BinaryWrite[str, bytes, form]; str = OpenRead[Close[str], BinaryFormat -> True]; While[Not[SameQ[byte = BinaryRead[str], EndOfFile]], res = Join[res, {byte}];]; Close[str]; res]

    ## Byte
    #> WRb[{149, 2, 177, 132}, {"Byte", "Byte", "Byte", "Byte"}]
     = {149, 2, 177, 132}
    #> WRb[{149, 2, 177, 132}, {"Byte", "Byte", "Byte", "Byte"}]
     = {149, 2, 177, 132}
    #> (# == WRb[#, Table["Byte", {50}]]) & [RandomInteger[{0, 255}, 50]]
     = True

    ## Character8
    #> WRb[{"a", "b", "c"}, {"Character8", "Character8", "Character8"}]
     = {97, 98, 99}
    #> WRb[{34, 60, 39}, {"Character8", "Character8", "Character8"}]
     = {51, 52, 54, 48, 51, 57}
    #> WRb[{"ab", "c", "d"}, {"Character8", "Character8", "Character8", "Character8"}]
     = {97, 98, 99, 100}

    ## Character16
    ## TODO

    ## Complex64
    #> WRb[-6.36877988924*^28 + 3.434203392*^9 I, "Complex64"]
     = {80, 201, 77, 239, 201, 177, 76, 79}
    #> WRb[-6.98948862335*^24 + 1.52209021297*^23 I, "Complex64"]
     = {158, 2, 185, 232, 18, 237, 0, 102}
    #> WRb[-1.41079828148*^-19 - 0.013060791418 I, "Complex64"]
     = {195, 142, 38, 160, 238, 252, 85, 188}
    #> WRb[{5, -2054}, "Complex64"]
     = {0, 0, 160, 64, 0, 0, 0, 0, 0, 96, 0, 197, 0, 0, 0, 0}
    #> WRb[Infinity, "Complex64"]
     = {0, 0, 128, 127, 0, 0, 0, 0}
    #> WRb[-Infinity, "Complex64"]
     = {0, 0, 128, 255, 0, 0, 0, 0}
    #> WRb[DirectedInfinity[1 + I], "Complex64"]
     = {0, 0, 128, 127, 0, 0, 128, 127}
    #> WRb[DirectedInfinity[I], "Complex64"]
     = {0, 0, 0, 0, 0, 0, 128, 127}
    ## FIXME (different convention to MMA)
    #> WRb[Indeterminate, "Complex64"]
     = {0, 0, 192, 127, 0, 0, 192, 127}

    ## Complex128
    #> WRb[1.19839770357*^-235 - 2.64656391494*^-54 I,"Complex128"]
     = {102, 217, 1, 163, 234, 98, 40, 15, 243, 104, 116, 15, 48, 57, 208, 180}
    #> WRb[3.22170267142*^134 - 8.98364297498*^198 I,"Complex128"]
     = {219, 161, 12, 126, 47, 94, 220, 91, 189, 66, 29, 68, 147, 11, 62, 233}
    #> WRb[-Infinity, "Complex128"]
     = {0, 0, 0, 0, 0, 0, 240, 255, 0, 0, 0, 0, 0, 0, 0, 0}
    #> WRb[DirectedInfinity[1 - I], "Complex128"]
     = {0, 0, 0, 0, 0, 0, 240, 127, 0, 0, 0, 0, 0, 0, 240, 255}
    #> WRb[DirectedInfinity[I], "Complex128"]
     = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 127}
    ## FIXME (different convention to MMA)
    #> WRb[Indeterminate, "Complex128"]
     = {0, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 248, 127}

    ## Complex256
    ## TODO

    ## Integer8
    #> WRb[{5, 2, 11, -4}, {"Integer8", "Integer8", "Integer8", "Integer8"}]
     = {5, 2, 11, 252}
    #> WRb[{127, -128, 0}, {"Integer8", "Integer8", "Integer8"}]
     = {127, 128, 0}

    ## Integer16
    #> WRb[{661, -31567, 6256}, {"Integer16", "Integer16", "Integer16"}]
     = {149, 2, 177, 132, 112, 24}
    #> WRb[{0, 255, -1, 32640, -32640}, Table["Integer16", {5}]]
     = {0, 0, 255, 0, 255, 255, 128, 127, 128, 128}

    ## Integer24
    #> WRb[{-6247016, -6631492}, {"Integer24", "Integer24"}]
     = {152, 173, 160, 188, 207, 154}
    #> WRb[{-1593967, 1989169}, {"Integer24", "Integer24"}]
     = {145, 173, 231, 49, 90, 30}

    ## Integer32
    #> WRb[{-636001327, -236143729}, {"Integer32", "Integer32"}]
     = {209, 99, 23, 218, 143, 187, 236, 241}
    #> WRb[{2024611599, -1139645195}, {"Integer32", "Integer32"}]
     = {15, 31, 173, 120, 245, 100, 18, 188}

    ## Integer64
    #> WRb[{1176115612243989203}, "Integer64"]
     = {211, 18, 152, 2, 235, 102, 82, 16}
    #> WRb[{-8526737900550694619}, "Integer64"]
     = {37, 217, 208, 88, 14, 241, 170, 137}

    ## Integer128
    #> WRb[139827542997232652313568968616424513676, "Integer128"]
     = {140, 32, 24, 199, 10, 169, 248, 117, 123, 184, 75, 76, 34, 206, 49, 105}
    #> WRb[103439096823027953602112616165136677221, "Integer128"]
     = {101, 57, 184, 108, 43, 214, 186, 120, 153, 51, 132, 225, 56, 165, 209, 77}
    #> WRb[-49058912464625098822365387707690163087, "Integer128"]
     = {113, 100, 125, 144, 211, 83, 140, 24, 206, 11, 198, 118, 222, 152, 23, 219}

    ## Real32
    #> WRb[{8.398086656*^9, 1.63880017681*^16}, {"Real32", "Real32"}]
     = {81, 72, 250, 79, 52, 227, 104, 90}
    #> WRb[{5.6052915284*^32, 9.631141*^6}, {"Real32", "Real32"}]
     = {251, 22, 221, 117, 165, 245, 18, 75}
    #> WRb[Infinity, "Real32"]
     = {0, 0, 128, 127}
    #> WRb[-Infinity, "Real32"]
     = {0, 0, 128, 255}
    ## FIXME (different convention to MMA)
    #> WRb[Indeterminate, "Real32"]
     = {0, 0, 192, 127}

    ## Real64
    #> WRb[-5.14646619426*^227, "Real64"]
     = {91, 233, 20, 87, 129, 185, 53, 239}
    #> WRb[-9.69531698809*^20, "Real64"]
     = {187, 67, 162, 67, 122, 71, 74, 196}
    #> WRb[9.67355569764*^159, "Real64"]
     = {132, 48, 80, 125, 157, 4, 38, 97}
    #> WRb[Infinity, "Real64"]
     = {0, 0, 0, 0, 0, 0, 240, 127}
    #> WRb[-Infinity, "Real64"]
     = {0, 0, 0, 0, 0, 0, 240, 255}
    ## FIXME (different convention to MMA)
    #> WRb[Indeterminate, "Real64"]
     = {0, 0, 0, 0, 0, 0, 248, 127}

    ## Real128
    ## TODO

    ## TerminatedString
    #> WRb["abc", "TerminatedString"]
     = {97, 98, 99, 0}
    #> WRb[{"123", "456"}, {"TerminatedString", "TerminatedString", "TerminatedString"}]
     = {49, 50, 51, 0, 52, 53, 54, 0}
    #> WRb["", "TerminatedString"]
    = {0}

    ## UnsignedInteger8
    #> WRb[{96, 94, 141, 162, 141}, Table["UnsignedInteger8", {5}]]
     = {96, 94, 141, 162, 141}
    #> (#==WRb[#,Table["UnsignedInteger8",{50}]])&[RandomInteger[{0, 255}, 50]]
     = True

    ## UnsignedInteger16
    #> WRb[{18230, 47466, 9875, 59141}, Table["UnsignedInteger16", {4}]]
     = {54, 71, 106, 185, 147, 38, 5, 231}
    #> WRb[{0, 32896, 65535}, Table["UnsignedInteger16", {3}]]
     = {0, 0, 128, 128, 255, 255}

    ## UnsignedInteger24
    #> WRb[{14820174, 15488225}, Table["UnsignedInteger24", {2}]]
     = {78, 35, 226, 225, 84, 236}
    #> WRb[{5374629, 3889391}, Table["UnsignedInteger24", {2}]]
     = {165, 2, 82, 239, 88, 59}

    ## UnsignedInteger32
    #> WRb[{1885507541, 4157323149}, Table["UnsignedInteger32", {2}]]
     = {213, 143, 98, 112, 141, 183, 203, 247}
    #> WRb[{384206740, 1676316040}, Table["UnsignedInteger32", {2}]]
     = {148, 135, 230, 22, 136, 141, 234, 99}
    """

    messages = {
        "writex": "`1`.",
    }

    writers = _BinaryFormat.get_writers()

    def apply_notype(self, name, n, b, evaluation):
        "BinaryWrite[OutputStream[name_, n_], b_]"
        return self.apply(name, n, b, None, evaluation)

    def apply(self, name, n, b, typ, evaluation):
        "BinaryWrite[OutputStream[name_, n_], b_, typ_]"

        channel = Expression("OutputStream", name, n)

        # Check Empty Type
        if typ is None:
            expr = Expression("BinaryWrite", channel, b)
            typ = Expression("List")
        else:
            expr = Expression("BinaryWrite", channel, b, typ)

        # Check channel
        stream = _lookup_stream(n.get_int_value())

        if stream is None or stream.closed:
            evaluation.message("General", "openx", name)
            return expr

        if stream.mode not in ["wb", "ab"]:
            evaluation.message("BinaryWrite", "openr", channel)
            return expr

        # Check b
        if b.has_form("List", None):
            pyb = b.leaves
        else:
            pyb = [b]

        # Check Type
        if typ.has_form("List", None):
            types = typ.get_leaves()
        else:
            types = [typ]

        if len(types) == 0:  # Default type is "Bytes"
            types = [String("Byte")]

        types = [t.get_string_value() for t in types]
        if not all(t in self.writers for t in types):
            evaluation.message("BinaryRead", "format", typ)
            return expr

        # Write to stream
        i = 0
        while i < len(pyb):
            x = pyb[i]
            # Types are "repeated as many times as necessary"
            t = types[i % len(types)]

            # Coerce x
            if t == "TerminatedString":
                x = x.get_string_value() + "\x00"
            elif t.startswith("Real"):
                if isinstance(x, Real):
                    x = x.to_python()
                elif x.has_form("DirectedInfinity", 1):
                    if x.leaves[0].get_int_value() == 1:
                        x = float("+inf")
                    elif x.leaves[0].get_int_value() == -1:
                        x = float("-inf")
                    else:
                        x = None
                elif isinstance(x, Symbol) and x.get_name() == "System`Indeterminate":
                    x = float("nan")
                else:
                    x = None
                assert x is None or isinstance(x, float)
            elif t.startswith("Complex"):
                if isinstance(x, (Complex, Real, Integer)):
                    x = x.to_python()
                elif x.has_form("DirectedInfinity", 1):
                    x = x.leaves[0].to_python(n_evaluation=evaluation)

                    # x*float('+inf') creates nan if x.real or x.imag are zero
                    x = complex(
                        x.real * float("+inf") if x.real != 0 else 0,
                        x.imag * float("+inf") if x.imag != 0 else 0,
                    )
                elif isinstance(x, Symbol) and x.get_name() == "System`Indeterminate":
                    x = complex(float("nan"), float("nan"))
                else:
                    x = None
            elif t.startswith("Character"):
                if isinstance(x, Integer):
                    x = [String(char) for char in str(x.get_int_value())]
                    pyb = list(chain(pyb[:i], x, pyb[i + 1 :]))
                    x = pyb[i]
                if isinstance(x, String) and len(x.get_string_value()) > 1:
                    x = [String(char) for char in x.get_string_value()]
                    pyb = list(chain(pyb[:i], x, pyb[i + 1 :]))
                    x = pyb[i]
                x = x.get_string_value()
            elif t == "Byte" and isinstance(x, String):
                if len(x.get_string_value()) > 1:
                    x = [String(char) for char in x.get_string_value()]
                    pyb = list(chain(pyb[:i], x, pyb[i + 1 :]))
                    x = pyb[i]
                x = ord(x.get_string_value())
            else:
                x = x.get_int_value()

            if x is None:
                return evaluation.message("BinaryWrite", "nocoerce", b)

            try:
                self.writers[t](stream, x)
            except struct.error:
                return evaluation.message("BinaryWrite", "nocoerce", b)
            i += 1

        try:
            stream.flush()
        except IOError as err:
            evaluation.message("BinaryWrite", "writex", err.strerror)
        return channel


class BinaryRead(Builtin):
    """
    <dl>
    <dt>'BinaryRead[$stream$]'
      <dd>reads one byte from the stream as an integer from 0 to 255.
    <dt>'BinaryRead[$stream$, $type$]'
      <dd>reads one object of specified type from the stream.
    <dt>'BinaryRead[$stream$, {$type1$, $type2$, ...}]'
      <dd>reads a sequence of objects of specified types.
    </dl>

    >> strm = OpenWrite[BinaryFormat -> True]
     = OutputStream[...]
    >> BinaryWrite[strm, {97, 98, 99}]
     = OutputStream[...]
    >> Close[strm]
     = ...
    >> strm = OpenRead[%, BinaryFormat -> True]
     = InputStream[...]
    >> BinaryRead[strm, {"Character8", "Character8", "Character8"}]
     = {a, b, c}
    >> Close[strm];

    ## Write as Bytes then Read
    #> WbR[bytes_, form_] := Module[{str, res}, str = OpenWrite[BinaryFormat -> True]; BinaryWrite[str, bytes]; str = OpenRead[Close[str], BinaryFormat -> True]; res = BinaryRead[str, form]; Close[str]; res]

    ## Byte
    #> WbR[{149, 2, 177, 132}, {"Byte", "Byte", "Byte", "Byte"}]
     = {149, 2, 177, 132}
    #> (# == WbR[#, Table["Byte", {50}]]) & [RandomInteger[{0, 255}, 50]]
     = True

    ## Character8
    #> WbR[{97, 98, 99}, {"Character8", "Character8", "Character8"}]
     = {a, b, c}
    #> WbR[{34, 60, 39}, {"Character8", "Character8", "Character8"}]
     = {", <, '}

    ## Character16
    #> WbR[{97, 0, 98, 0, 99, 0}, {"Character16", "Character16", "Character16"}]
     = {a, b, c}
    #> ToCharacterCode[WbR[{50, 154, 182, 236}, {"Character16", "Character16"}]]
     = {{39474}, {60598}}
    ## #> WbR[ {91, 146, 206, 54}, {"Character16", "Character16"}]
    ##  = {\\:925b, \\:36ce}

    ## Complex64
    #> WbR[{80, 201, 77, 239, 201, 177, 76, 79}, "Complex64"] // InputForm
     = -6.368779889243691*^28 + 3.434203392*^9*I
    #> % // Precision
     = MachinePrecision
    #> WbR[{158, 2, 185, 232, 18, 237, 0, 102}, "Complex64"] // InputForm
     = -6.989488623351118*^24 + 1.522090212973691*^23*I
    #> WbR[{195, 142, 38, 160, 238, 252, 85, 188}, "Complex64"] // InputForm
     = -1.4107982814807285*^-19 - 0.013060791417956352*I

    ## Complex128
    #> WbR[{15,114,1,163,234,98,40,15,214,127,116,15,48,57,208,180},"Complex128"] // InputForm
     = 1.1983977035653814*^-235 - 2.6465639149433955*^-54*I
    #> WbR[{148,119,12,126,47,94,220,91,42,69,29,68,147,11,62,233},"Complex128"] // InputForm
     = 3.2217026714156333*^134 - 8.98364297498066*^198*I
    #> % // Precision
     = MachinePrecision
    #> WbR[{15,42,80,125,157,4,38,97, 0,0,0,0,0,0,240,255}, "Complex128"]
      = -I Infinity
    #> WbR[{15,42,80,125,157,4,38,97, 0,0,0,0,0,0,240,127}, "Complex128"]
      = I Infinity
    #> WbR[{15,42,80,125,157,4,38,97, 1,0,0,0,0,0,240,255}, "Complex128"]
     = Indeterminate
    #> WbR[{0,0,0,0,0,0,240,127, 15,42,80,125,157,4,38,97}, "Complex128"]
     = Infinity
    #> WbR[{0,0,0,0,0,0,240,255, 15,42,80,125,157,4,38,97}, "Complex128"]
     = -Infinity
    #> WbR[{1,0,0,0,0,0,240,255, 15,42,80,125,157,4,38,97}, "Complex128"]
     = Indeterminate
    #> WbR[{0,0,0,0,0,0,240,127, 0,0,0,0,0,0,240,127}, "Complex128"]
     = Indeterminate
    #> WbR[{0,0,0,0,0,0,240,127, 0,0,0,0,0,0,240,255}, "Complex128"]
     = Indeterminate

    ## Complex256
    ## TODO

    ## Integer8
    #> WbR[{149, 2, 177, 132}, {"Integer8", "Integer8", "Integer8", "Integer8"}]
     = {-107, 2, -79, -124}
    #> WbR[{127, 128, 0, 255}, {"Integer8", "Integer8", "Integer8", "Integer8"}]
     = {127, -128, 0, -1}

    ## Integer16
    #> WbR[{149, 2, 177, 132, 112, 24}, {"Integer16", "Integer16", "Integer16"}]
     = {661, -31567, 6256}
    #> WbR[{0, 0, 255, 0, 255, 255, 128, 127, 128, 128}, Table["Integer16", {5}]]
     = {0, 255, -1, 32640, -32640}

    ## Integer24
    #> WbR[{152, 173, 160, 188, 207, 154}, {"Integer24", "Integer24"}]
     = {-6247016, -6631492}
    #> WbR[{145, 173, 231, 49, 90, 30}, {"Integer24", "Integer24"}]
     = {-1593967, 1989169}

    ## Integer32
    #> WbR[{209, 99, 23, 218, 143, 187, 236, 241}, {"Integer32", "Integer32"}]
     = {-636001327, -236143729}
    #> WbR[{15, 31, 173, 120, 245, 100, 18, 188}, {"Integer32", "Integer32"}]
     = {2024611599, -1139645195}

    ## Integer64
    #> WbR[{211, 18, 152, 2, 235, 102, 82, 16}, "Integer64"]
     = 1176115612243989203
    #> WbR[{37, 217, 208, 88, 14, 241, 170, 137}, "Integer64"]
     = -8526737900550694619

    ## Integer128
    #> WbR[{140,32,24,199,10,169,248,117,123,184,75,76,34,206,49,105}, "Integer128"]
     = 139827542997232652313568968616424513676
    #> WbR[{101,57,184,108,43,214,186,120,153,51,132,225,56,165,209,77}, "Integer128"]
     = 103439096823027953602112616165136677221
    #> WbR[{113,100,125,144,211,83,140,24,206,11,198,118,222,152,23,219}, "Integer128"]
     = -49058912464625098822365387707690163087

    ## Real32
    #> WbR[{81, 72, 250, 79, 52, 227, 104, 90}, {"Real32", "Real32"}] // InputForm
     = {8.398086656*^9, 1.6388001768669184*^16}
    #> WbR[{251, 22, 221, 117, 165, 245, 18, 75}, {"Real32", "Real32"}] // InputForm
     = {5.605291528399748*^32, 9.631141*^6}
    #> WbR[{126, 82, 143, 43}, "Real32"] // InputForm
     = 1.0183657302847982*^-12
    #> % // Precision
     = MachinePrecision
    #> WbR[{0, 0, 128, 127}, "Real32"]
     = Infinity
    #> WbR[{0, 0, 128, 255}, "Real32"]
     = -Infinity
    #> WbR[{1, 0, 128, 255}, "Real32"]
     = Indeterminate
    #> WbR[{1, 0, 128, 127}, "Real32"]
     = Indeterminate

    ## Real64
    #> WbR[{45, 243, 20, 87, 129, 185, 53, 239}, "Real64"] // InputForm
     = -5.146466194262116*^227
    #> WbR[{192, 60, 162, 67, 122, 71, 74, 196}, "Real64"] // InputForm
     = -9.695316988087658*^20
    #> WbR[{15, 42, 80, 125, 157, 4, 38, 97}, "Real64"] // InputForm
     = 9.67355569763742*^159
    #> % // Precision
     = MachinePrecision
    #> WbR[{0, 0, 0, 0, 0, 0, 240, 127}, "Real64"]
     = Infinity
    #> WbR[{0, 0, 0, 0, 0, 0, 240, 255}, "Real64"]
     = -Infinity
    #> WbR[{1, 0, 0, 0, 0, 0, 240, 127}, "Real64"]
     = Indeterminate
    #> WbR[{1, 0, 0, 0, 0, 0, 240, 255}, "Real64"]
     = Indeterminate

    ## Real128
    ## 0x0000
    #> WbR[{0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0}, "Real128"]
     = 0.*^-4965
    #> WbR[{0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,128}, "Real128"]
     = 0.*^-4965
    ## 0x0001 - 0x7FFE
    #> WbR[{0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,255,63}, "Real128"]
     = 1.00000000000000000000000000000000
    #> WbR[{0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,255,191}, "Real128"]
     = -1.00000000000000000000000000000000
    #> WbR[{135, 62, 233, 137, 22, 208, 233, 210, 133, 82, 251, 92, 220, 216, 255, 63}, "Real128"]
     = 1.84711247573661489653389674493896
    #> WbR[{135, 62, 233, 137, 22, 208, 233, 210, 133, 82, 251, 92, 220, 216, 207, 72}, "Real128"]
     = 2.45563355727491021879689747166252*^679
    #> WbR[{74, 95, 30, 234, 116, 130, 1, 84, 20, 133, 245, 221, 113, 110, 219, 212}, "Real128"]
     = -4.52840681592341879518366539335138*^1607
    #> % // Precision
     = 33.
    ## 0x7FFF
    #> WbR[{0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,255,127}, "Real128"]
     = Infinity
    #> WbR[{0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,255,255}, "Real128"]
     = -Infinity
    #> WbR[{1,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,255,127}, "Real128"]
     = Indeterminate
    #> WbR[{1,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,255,255}, "Real128"]
     = Indeterminate

    ## TerminatedString
    #> WbR[{97, 98, 99, 0}, "TerminatedString"]
     = abc
    #> WbR[{49, 50, 51, 0, 52, 53, 54, 0, 55, 56, 57}, Table["TerminatedString", {3}]]
     = {123, 456, EndOfFile}
    #> WbR[{0}, "TerminatedString"] // InputForm
     = ""

    ## UnsignedInteger8
    #> WbR[{96, 94, 141, 162, 141}, Table["UnsignedInteger8", {5}]]
     = {96, 94, 141, 162, 141}
    #> (#==WbR[#,Table["UnsignedInteger8",{50}]])&[RandomInteger[{0, 255}, 50]]
     = True

    ## UnsignedInteger16
    #> WbR[{54, 71, 106, 185, 147, 38, 5, 231}, Table["UnsignedInteger16", {4}]]
     = {18230, 47466, 9875, 59141}
    #> WbR[{0, 0, 128, 128, 255, 255}, Table["UnsignedInteger16", {3}]]
     = {0, 32896, 65535}

    ## UnsignedInteger24
    #> WbR[{78, 35, 226, 225, 84, 236}, Table["UnsignedInteger24", {2}]]
     = {14820174, 15488225}
    #> WbR[{165, 2, 82, 239, 88, 59}, Table["UnsignedInteger24", {2}]]
     = {5374629, 3889391}

    ## UnsignedInteger32
    #> WbR[{213,143,98,112,141,183,203,247}, Table["UnsignedInteger32", {2}]]
     = {1885507541, 4157323149}
    #> WbR[{148,135,230,22,136,141,234,99}, Table["UnsignedInteger32", {2}]]
     = {384206740, 1676316040}

    ## UnsignedInteger64
    #> WbR[{95, 5, 33, 229, 29, 62, 63, 98}, "UnsignedInteger64"]
     = 7079445437368829279
    #> WbR[{134, 9, 161, 91, 93, 195, 173, 74}, "UnsignedInteger64"]
     = 5381171935514265990

    ## UnsignedInteger128
    #> WbR[{108,78,217,150,88,126,152,101,231,134,176,140,118,81,183,220}, "UnsignedInteger128"]
     = 293382001665435747348222619884289871468
    #> WbR[{53,83,116,79,81,100,60,126,202,52,241,48,5,113,92,190}, "UnsignedInteger128"]
     = 253033302833692126095975097811212718901

    ## EndOfFile
    #> WbR[{148}, {"Integer32", "Integer32","Integer32"}]
     = {EndOfFile, EndOfFile, EndOfFile}
    """

    readers = _BinaryFormat.get_readers()

    messages = {
        "format": "`1` is not a recognized binary format.",
        "openw": "`1` is open for output.",
        "bfmt": "The stream `1` has been opened with BinaryFormat -> False and cannot be used with binary data.",
    }

    def apply_empty(self, name, n, evaluation):
        "BinaryRead[InputStream[name_, n_]]"
        return self.apply(name, n, None, evaluation)

    def apply(self, name, n, typ, evaluation):
        "BinaryRead[InputStream[name_, n_], typ_]"

        channel = Expression("InputStream", name, n)

        # Check typ
        if typ is None:
            expr = Expression("BinaryRead", channel)
            typ = String("Byte")
        else:
            expr = Expression("BinaryRead", channel, typ)

        # Check channel
        stream = _lookup_stream(n.get_int_value())

        if stream is None or stream.closed:
            evaluation.message("General", "openx", name)
            return expr

        if stream.mode not in ["rb"]:
            evaluation.message("BinaryRead", "bfmt", channel)
            return expr

        if typ.has_form("List", None):
            types = typ.get_leaves()
        else:
            types = [typ]

        types = [t.get_string_value() for t in types]
        if not all(t in self.readers for t in types):
            evaluation.message("BinaryRead", "format", typ)
            return expr

        # Read from stream
        result = []
        for t in types:
            try:
                result.append(self.readers[t](stream))
            except struct.error:
                result.append(Symbol("EndOfFile"))

        if typ.has_form("List", None):
            return Expression("List", *result)
        else:
            if len(result) == 1:
                return result[0]


class WriteString(Builtin):
    """
    <dl>
    <dt>'WriteString[$stream$, $str1, $str2$, ... ]'
      <dd>writes the strings to the output stream.
    </dl>

    >> str = OpenWrite[];
    >> WriteString[str, "This is a test 1"]
    >> WriteString[str, "This is also a test 2"]
    >> Close[str]
     = ...
    >> FilePrint[%]
     | This is a test 1This is also a test 2

    >> str = OpenWrite[];
    >> WriteString[str, "This is a test 1", "This is also a test 2"]
    >> Close[str]
     = ...
    >> FilePrint[%]
     | This is a test 1This is also a test 2

    #> str = OpenWrite[];
    #> WriteString[str, 100, 1 + x + y, Sin[x  + y]]
    #> Close[str]
     = ...
    #> FilePrint[%]
     | 1001 + x + ySin[x + y]

    #> str = OpenWrite[];
    #> WriteString[str]
    #> Close[str]
     = ...
    #> FilePrint[%]

    #> WriteString[%%, abc]
    #> Streams[%%%][[1]]
     = ...
    #> Close[%]
     = ...
    #> FilePrint[%]
     | abc

    """

    messages = {
        "strml": ("`1` is not a string, stream, " "or list of strings and streams."),
        "writex": "`1`.",
    }

    attributes = "Protected"

    def apply(self, channel, expr, evaluation):
        "WriteString[channel_, expr___]"
        strm = _channel_to_stream(channel, "w")

        if strm is None:
            return

        stream = _lookup_stream(strm.leaves[1].get_int_value())

        if stream is None or stream.closed:
            return None

        exprs = []
        for expri in expr.get_sequence():
            result = expri.format(evaluation, "System`OutputForm")
            try:
                result = result.boxes_to_text(evaluation=evaluation)
            except BoxError:
                return evaluation.message(
                    "General",
                    "notboxes",
                    Expression("FullForm", result).evaluate(evaluation),
                )
            exprs.append(result)
        line = "".join(exprs)
        if type(stream) is BytesIO:
            line = line.encode("utf8")
        stream.write(line)
        try:
            stream.flush()
        except IOError as err:
            evaluation.message("WriteString", "writex", err.strerror)
        return SymbolNull


class _OpenAction(Builtin):

    attributes = "Protected"

    # BinaryFormat: 'False',
    # CharacterEncoding :> Automatic,
    # DOSTextFormat :> True,
    # FormatType -> InputForm,
    # NumberMarks :> $NumberMarks,
    # PageHeight -> 22, PageWidth -> 78,
    # TotalHeight -> Infinity,
    # TotalWidth -> Infinity

    options = {
        "BinaryFormat": "False",
        "CharacterEncoding": "$CharacterEncoding",
    }

    messages = {
        "argx": "OpenRead called with 0 arguments; 1 argument is expected.",
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
    }

    def apply_empty(self, evaluation, options):
        "%(name)s[OptionsPattern[]]"

        if isinstance(self, (OpenWrite, OpenAppend)):
            tmpf = tempfile.NamedTemporaryFile(dir=TMP_DIR)
            path = String(tmpf.name)
            tmpf.close()
            return self.apply_path(path, evaluation, options)
        else:
            evaluation.message("OpenRead", "argx")
            return

    def apply_path(self, path, evaluation, options):
        "%(name)s[path_?NotOptionQ, OptionsPattern[]]"

        # Options
        # BinaryFormat
        mode = self.mode
        if options["System`BinaryFormat"].is_true():
            if not self.mode.endswith("b"):
                mode += "b"

        if not (isinstance(path, String) and len(path.to_python()) > 2):
            evaluation.message(self.__class__.__name__, "fstr", path)
            return

        path_string = path.get_string_value()

        tmp = path_search(path_string)
        if tmp is None:
            if mode in ["r", "rb"]:
                evaluation.message("General", "noopen", path)
                return
        else:
            path_string = tmp

        try:
            encoding = self.get_option(options, "CharacterEncoding", evaluation)
            if not isinstance(encoding, String):
                return

            opener = mathics_open(
                path_string, mode=mode, encoding=encoding.get_string_value()
            )
            opener.__enter__()
            n = opener.n
        except IOError:
            evaluation.message("General", "noopen", path)
            return
        except MessageException as e:
            e.message(evaluation)
            return

        return Expression(self.stream_type, path, Integer(n))


class OpenRead(_OpenAction):
    """
    <dl>
    <dt>'OpenRead["file"]'
      <dd>opens a file and returns an InputStream.
    </dl>

    >> OpenRead["ExampleData/EinsteinSzilLetter.txt"]
     = InputStream[...]
    #> Close[%];

    S> OpenRead["https://raw.githubusercontent.com/mathics/Mathics/master/README.rst"]
     = InputStream[...]
    S> Close[%];

    #> OpenRead[]
     : OpenRead called with 0 arguments; 1 argument is expected.
     = OpenRead[]

    #> OpenRead[y]
     : File specification y is not a string of one or more characters.
     = OpenRead[y]

    #> OpenRead[""]
     : File specification  is not a string of one or more characters.
     = OpenRead[]

    #> OpenRead["MathicsNonExampleFile"]
     : Cannot open MathicsNonExampleFile.
     = OpenRead[MathicsNonExampleFile]

    #> OpenRead["ExampleData/EinsteinSzilLetter.txt", BinaryFormat -> True]
     = InputStream[...]
    #> Close[%];
    """

    mode = "r"
    stream_type = "InputStream"


class OpenWrite(_OpenAction):
    """
    <dl>
    <dt>'OpenWrite["file"]'
      <dd>opens a file and returns an OutputStream.
    </dl>

    >> OpenWrite[]
     = OutputStream[...]
    #> Close[%];

    #> OpenWrite[BinaryFormat -> True]
     = OutputStream[...]
    #> Close[%];
    """

    mode = "w"
    stream_type = "OutputStream"


class OpenAppend(_OpenAction):
    """
    <dl>
    <dt>'OpenAppend["file"]'
      <dd>opens a file and returns an OutputStream to which writes are appended.
    </dl>

    >> OpenAppend[]
     = OutputStream[...]
    #> Close[%];

    #> appendFile = OpenAppend["MathicsNonExampleFile"]
     = OutputStream[MathicsNonExampleFile, ...]

    #> Close[appendFile]
     = MathicsNonExampleFile
    #> DeleteFile["MathicsNonExampleFile"]
    """

    mode = "a"
    stream_type = "OutputStream"


class Get(PrefixOperator):
    r"""
    <dl>
      <dt>'<<$name$'
      <dd>reads a file and evaluates each expression, returning only the last one.
    </dl>

    S> filename = $TemporaryDirectory <> "/example_file";
    S> Put[x + y, filename]
    S> Get[filename]
     = x + y

    S> filename = $TemporaryDirectory <> "/example_file";
    S> Put[x + y, 2x^2 + 4z!, Cos[x] + I Sin[x], filename]
    S> Get[filename]
     = Cos[x] + I Sin[x]
    S> DeleteFile[filename]

    ## TODO: Requires EndPackage implemented
    ## 'Get' can also load packages:
    ## >> << "VectorAnalysis`"

    #> Get["SomeTypoPackage`"]
     : Cannot open SomeTypoPackage`.
     = $Failed

    ## Parser Tests
    #> Hold[<< ~/some_example/dir/] // FullForm
     = Hold[Get["~/some_example/dir/"]]
    #> Hold[<<`/.\-_:$*~?] // FullForm
     = Hold[Get["`/.\\\\-_:$*~?"]]
    """

    operator = "<<"
    precedence = 720
    attributes = "Protected"

    options = {
        "Trace": "False",
    }

    def apply(self, path, evaluation, options):
        "Get[path_String, OptionsPattern[Get]]"

        def check_options(options):
            # Options
            # TODO Proper error messages

            result = {}
            trace_get = evaluation.parse("Settings`$TraceGet")
            if (
                options["System`Trace"].to_python()
                or trace_get.evaluate(evaluation) == SymbolTrue
            ):
                import builtins

                result["TraceFn"] = builtins.print
            else:
                result["TraceFn"] = None

            return result

        py_options = check_options(options)
        trace_fn = py_options["TraceFn"]
        result = None
        pypath = path.get_string_value()
        definitions = evaluation.definitions
        try:
            if trace_fn:
                trace_fn(pypath)
            with mathics_open(pypath, "r") as f:
                feeder = MathicsFileLineFeeder(f, trace_fn)
                while not feeder.empty():
                    try:
                        query = parse(definitions, feeder)
                    except TranslateError:
                        return SymbolNull
                    finally:
                        feeder.send_messages(evaluation)
                    if query is None:  # blank line / comment
                        continue
                    result = query.evaluate(evaluation)
        except IOError:
            evaluation.message("General", "noopen", path)
            return SymbolFailed
        except MessageException as e:
            e.message(evaluation)
            return SymbolFailed
        return result

    def apply_default(self, filename, evaluation):
        "Get[filename_]"
        expr = Expression("Get", filename)
        evaluation.message("General", "stream", filename)
        return expr


class Put(BinaryOperator):
    """
    <dl>
    <dt>'$expr$ >> $filename$'
      <dd>write $expr$ to a file.
    <dt>'Put[$expr1$, $expr2$, ..., $filename$]'
      <dd>write a sequence of expressions to a file.
    </dl>

    ## Note a lot of these tests are:
    ## * a bit fragile, somewhat
    ## * somewhat OS dependent,
    ## * can leave crap in the filesystem
    ## * put in a pytest
    ##
    ## For these reasons this should be done a a pure test
    ## rather than intermingled with the doc system.

    S> Put[40!, fortyfactorial]
     : fortyfactorial is not string, InputStream[], or OutputStream[]
     = 815915283247897734345611269596115894272000000000 >> fortyfactorial
    ## FIXME: final line should be
    ## = Put[815915283247897734345611269596115894272000000000, fortyfactorial]

    S> filename = $TemporaryDirectory <> "/fortyfactorial";
    S> Put[40!, filename]
    S> FilePrint[filename]
     | 815915283247897734345611269596115894272000000000
    S> Get[filename]
     = 815915283247897734345611269596115894272000000000
    S> DeleteFile[filename]

    S> filename = $TemporaryDirectory <> "/fiftyfactorial";
    S> Put[10!, 20!, 30!, filename]
    S> FilePrint[filename]
     | 3628800
     | 2432902008176640000
     | 265252859812191058636308480000000

    S> DeleteFile[filename]
     =

    S> filename = $TemporaryDirectory <> "/example_file";
    S> Put[x + y, 2x^2 + 4z!, Cos[x] + I Sin[x], filename]
    S> FilePrint[filename]
     | x + y
     | 2*x^2 + 4*z!
     | Cos[x] + I*Sin[x]
    S> DeleteFile[filename]
    """

    operator = ">>"
    precedence = 30

    def apply(self, exprs, filename, evaluation):
        "Put[exprs___, filename_String]"
        instream = Expression("OpenWrite", filename).evaluate(evaluation)
        if len(instream.leaves) == 2:
            name, n = instream.leaves
        else:
            return  # opening failed
        result = self.apply_input(exprs, name, n, evaluation)
        Expression("Close", instream).evaluate(evaluation)
        return result

    def apply_input(self, exprs, name, n, evaluation):
        "Put[exprs___, OutputStream[name_, n_]]"
        stream = _lookup_stream(n.get_int_value())

        if stream is None or stream.closed:
            evaluation.message("Put", "openx", Expression("OutputSteam", name, n))
            return

        text = [
            evaluation.format_output(Expression("InputForm", expr))
            for expr in exprs.get_sequence()
        ]
        text = "\n".join(text) + "\n"
        text.encode("utf-8")

        stream.write(text)

        return SymbolNull

    def apply_default(self, exprs, filename, evaluation):
        "Put[exprs___, filename_]"
        expr = Expression("Put", exprs, filename)
        evaluation.message("General", "stream", filename)
        return expr


class PutAppend(BinaryOperator):
    """
    <dl>
    <dt>'$expr$ >>> $filename$'
      <dd>append $expr$ to a file.
    <dt>'PutAppend[$expr1$, $expr2$, ..., $"filename"$]'
      <dd>write a sequence of expressions to a file.
    </dl>

    >> Put[50!, "factorials"]
    >> FilePrint["factorials"]
     | 30414093201713378043612608166064768844377641568960512000000000000

    >> PutAppend[10!, 20!, 30!, "factorials"]
    >> FilePrint["factorials"]
     | 30414093201713378043612608166064768844377641568960512000000000000
     | 3628800
     | 2432902008176640000
     | 265252859812191058636308480000000

    >> 60! >>> "factorials"
    >> FilePrint["factorials"]
     | 30414093201713378043612608166064768844377641568960512000000000000
     | 3628800
     | 2432902008176640000
     | 265252859812191058636308480000000
     | 8320987112741390144276341183223364380754172606361245952449277696409600000000000000

    >> "string" >>> factorials
    >> FilePrint["factorials"]
     | 30414093201713378043612608166064768844377641568960512000000000000
     | 3628800
     | 2432902008176640000
     | 265252859812191058636308480000000
     | 8320987112741390144276341183223364380754172606361245952449277696409600000000000000
     | "string"
    #> DeleteFile["factorials"];

    ## writing to dir
    #> x >>> /var/
     : Cannot open /var/.
     = x >>> /var/

    ## writing to read only file
    #> x >>> /proc/uptime
     : Cannot open /proc/uptime.
     = x >>> /proc/uptime
    """

    operator = ">>>"
    precedence = 30
    attributes = "Protected"

    def apply(self, exprs, filename, evaluation):
        "PutAppend[exprs___, filename_String]"
        instream = Expression("OpenAppend", filename).evaluate(evaluation)
        if len(instream.leaves) == 2:
            name, n = instream.leaves
        else:
            return  # opening failed
        result = self.apply_input(exprs, name, n, evaluation)
        Expression("Close", instream).evaluate(evaluation)
        return result

    def apply_input(self, exprs, name, n, evaluation):
        "PutAppend[exprs___, OutputStream[name_, n_]]"
        stream = _lookup_stream(n.get_int_value())

        if stream is None or stream.closed:
            evaluation.message("Put", "openx", Expression("OutputSteam", name, n))
            return

        text = [
            str(e.do_format(evaluation, "System`OutputForm").__str__())
            for e in exprs.get_sequence()
        ]
        text = "\n".join(text) + "\n"
        text.encode("ascii")

        stream.write(text)

        return SymbolNull

    def apply_default(self, exprs, filename, evaluation):
        "PutAppend[exprs___, filename_]"
        expr = Expression("PutAppend", exprs, filename)
        evaluation.message("General", "stream", filename)
        return expr


class FindFile(Builtin):
    """
    <dl>
    <dt>'FindFile[$name$]'
      <dd>searches '$Path' for the given filename.
    </dl>

    >> FindFile["ExampleData/sunflowers.jpg"]
     = ...

    >> FindFile["VectorAnalysis`"]
     = ...

    >> FindFile["VectorAnalysis`VectorAnalysis`"]
     = ...

    #> FindFile["SomeTypoPackage`"]
     = $Failed
    """

    attributes = "Protected"

    messages = {
        "string": "String expected at position 1 in `1`.",
    }

    def apply(self, name, evaluation):
        "FindFile[name_]"

        py_name = name.to_python()

        if not (isinstance(py_name, str) and py_name[0] == py_name[-1] == '"'):
            evaluation.message("FindFile", "string", Expression("FindFile", name))
            return
        py_name = py_name[1:-1]

        result = path_search(py_name)

        if result is None:
            return SymbolFailed

        return String(osp.abspath(result))


class FileNameSplit(Builtin):
    """
    <dl>
    <dt>'FileNameSplit["$filenams$"]'
      <dd>splits a $filename$ into a list of parts.
    </dl>

    >> FileNameSplit["example/path/file.txt"]
     = {example, path, file.txt}

    #> FileNameSplit["example/path", OperatingSystem -> x]
     : The value of option OperatingSystem -> x must be one of "MacOSX", "Windows", or "Unix".
     = {example, path}
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    messages = {
        "ostype": (
            "The value of option OperatingSystem -> `1` "
            'must be one of "MacOSX", "Windows", or "Unix".'
        ),
    }

    def apply(self, filename, evaluation, options):
        "FileNameSplit[filename_String, OptionsPattern[FileNameSplit]]"

        path = filename.to_python()[1:-1]

        operating_system = (
            options["System`OperatingSystem"].evaluate(evaluation).to_python()
        )

        if operating_system not in ['"MacOSX"', '"Windows"', '"Unix"']:
            evaluation.message(
                "FileNameSplit", "ostype", options["System`OperatingSystem"]
            )
            if os.name == "posix":
                operating_system = "Unix"
            elif os.name == "nt":
                operating_system = "Windows"
            elif os.name == "os2":
                operating_system = "MacOSX"
            else:
                return

        # TODO Implement OperatingSystem Option

        result = []
        while path not in ["", SYS_ROOT_DIR]:
            path, ext = osp.split(path)
            if ext != "":
                result.insert(0, ext)

        return from_python(result)


class ToFileName(Builtin):
    """
    <dl>
    <dt>'ToFileName[{"$dir_1$", "$dir_2$", ...}]'
      <dd>joins the $dir_i$ together into one path.
    </dl>

    'ToFileName' has been superseded by 'FileNameJoin'.

    >> ToFileName[{"dir1", "dir2"}, "file"]
     = dir1...dir2...file

    >> ToFileName["dir1", "file"]
     = dir1...file

    >> ToFileName[{"dir1", "dir2", "dir3"}]
     = dir1...dir2...dir3
    """

    rules = {
        "ToFileName[dir_String, name_String]": "FileNameJoin[{dir, name}]",
        "ToFileName[dirs_?ListQ, name_String]": "FileNameJoin[Append[dirs, name]]",
        "ToFileName[dirs_?ListQ]": "FileNameJoin[dirs]",
    }


class FileNameJoin(Builtin):
    """
        <dl>
          <dt>'FileNameJoin[{"$dir_1$", "$dir_2$", ...}]'
          <dd>joins the $dir_i$ together into one path.

          <dt>'FileNameJoin[..., OperatingSystem->"os"]'
          <dd>yields a file name in the format for the specified operating system. Possible choices are "Windows", "MacOSX", and "Unix".
        </dl>

        >> FileNameJoin[{"dir1", "dir2", "dir3"}]
         = ...

        >> FileNameJoin[{"dir1", "dir2", "dir3"}, OperatingSystem -> "Unix"]
         = dir1/dir2/dir3

        >> FileNameJoin[{"dir1", "dir2", "dir3"}, OperatingSystem -> "Windows"]
         = dir1\\dir2\\dir3
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    messages = {
        "ostype": (
            "The value of option OperatingSystem -> `1` "
            'must be one of "MacOSX", "Windows", or "Unix".'
        ),
    }

    def apply(self, pathlist, evaluation, options):
        "FileNameJoin[pathlist_?ListQ, OptionsPattern[FileNameJoin]]"

        py_pathlist = pathlist.to_python()
        if not all(isinstance(p, str) and p[0] == p[-1] == '"' for p in py_pathlist):
            return
        py_pathlist = [p[1:-1] for p in py_pathlist]

        operating_system = (
            options["System`OperatingSystem"].evaluate(evaluation).get_string_value()
        )

        if operating_system not in ["MacOSX", "Windows", "Unix"]:
            evaluation.message(
                "FileNameSplit", "ostype", options["System`OperatingSystem"]
            )
            if os.name == "posix":
                operating_system = "Unix"
            elif os.name == "nt":
                operating_system = "Windows"
            elif os.name == "os2":
                operating_system = "MacOSX"
            else:
                return

        if operating_system in ("Unix", "MacOSX"):
            import posixpath

            result = posixpath.join(*py_pathlist)
        elif operating_system in ("Windows",):
            import ntpath

            result = ntpath.join(*py_pathlist)
        else:
            result = osp.join(*py_pathlist)

        return from_python(result)


class FileExtension(Builtin):
    """
    <dl>
    <dt>'FileExtension["$file$"]'
      <dd>gives the extension for the specified file name.
    </dl>

    >> FileExtension["file.txt"]
     = txt

    >> FileExtension["file.tar.gz"]
     = gz

    #> FileExtension["file."]
     = #<--#
    #> FileExtension["file"]
     = #<--#
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    def apply(self, filename, evaluation, options):
        "FileExtension[filename_String, OptionsPattern[FileExtension]]"
        path = filename.to_python()[1:-1]
        filename_base, filename_ext = osp.splitext(path)
        filename_ext = filename_ext.lstrip(".")
        return from_python(filename_ext)


class FileBaseName(Builtin):
    """
    <dl>
    <dt>'FileBaseName["$file$"]'
      <dd>gives the base name for the specified file name.
    </dl>

    >> FileBaseName["file.txt"]
     = file

    >> FileBaseName["file.tar.gz"]
     = file.tar

    #> FileBaseName["file."]
     = file

    #> FileBaseName["file"]
     = file
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    def apply(self, filename, evaluation, options):
        "FileBaseName[filename_String, OptionsPattern[FileBaseName]]"
        path = filename.to_python()[1:-1]

        filename_base, filename_ext = osp.splitext(path)
        return from_python(filename_base)


class FileNameTake(Builtin):
    """
    <dl>
      <dt>'FileNameTake["$file$"]'
      <dd>returns the last path element in the file name $name$.

      <dt>'FileNameTake["$file$", $n$]'
      <dd>returns the first $n$ path elements in the file name $name$.

      <dt>'FileNameTake["$file$", $-n$]'
      <dd>returns the last $n$ path elements in the file name $name$.
    </dl>

    """

    # mmatura: please put in a pytest
    # >> FileNameTake["/tmp/file.txt"]
    #  = file.txt
    # >> FileNameTake["tmp/file.txt", 1]
    #  = tmp
    # >> FileNameTake["tmp/file.txt", -1]
    #  = file.txt

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    def apply(self, filename, evaluation, options):
        "FileNameTake[filename_String, OptionsPattern[FileBaseName]]"
        path = pathlib.Path(filename.to_python()[1:-1])
        return from_python(path.name)

    def apply_n(self, filename, n, evaluation, options):
        "FileNameTake[filename_String, n_Integer, OptionsPattern[FileBaseName]]"
        n_int = n.get_int_value()
        parts = pathlib.Path(filename.to_python()[1:-1]).parts
        if n_int >= 0:
            subparts = parts[:n_int]
        else:
            subparts = parts[n_int:]
        return from_python(str(pathlib.PurePath(*subparts)))


class DirectoryName(Builtin):
    """
    <dl>
    <dt>'DirectoryName["$name$"]'
      <dd>extracts the directory name from a filename.
    </dl>

    >> DirectoryName["a/b/c"]
     = a/b

    >> DirectoryName["a/b/c", 2]
     = a

    #> DirectoryName["a/b/c", 3] // InputForm
     = ""
    #> DirectoryName[""] // InputForm
     = ""

    #> DirectoryName["a/b/c", x]
     : Positive machine-sized integer expected at position 2 in DirectoryName[a/b/c, x].
     = DirectoryName[a/b/c, x]

    #> DirectoryName["a/b/c", -1]
     : Positive machine-sized integer expected at position 2 in DirectoryName[a/b/c, -1].
     = DirectoryName[a/b/c, -1]

    #> DirectoryName[x]
     : String expected at position 1 in DirectoryName[x].
     = DirectoryName[x]
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    messages = {
        "string": "String expected at position 1 in `1`.",
        "intpm": ("Positive machine-sized integer expected at " "position 2 in `1`."),
    }

    def apply(self, name, n, evaluation, options):
        "DirectoryName[name_, n_, OptionsPattern[DirectoryName]]"

        if n is None:
            expr = Expression("DirectoryName", name)
            py_n = 1
        else:
            expr = Expression("DirectoryName", name, n)
            py_n = n.to_python()

        if not (isinstance(py_n, int) and py_n > 0):
            evaluation.message("DirectoryName", "intpm", expr)
            return

        py_name = name.to_python()
        if not (isinstance(py_name, str) and py_name[0] == py_name[-1] == '"'):
            evaluation.message("DirectoryName", "string", expr)
            return
        py_name = py_name[1:-1]

        result = py_name
        for i in range(py_n):
            (result, tmp) = osp.split(result)

        return String(result)

    def apply1(self, name, evaluation, options):
        "DirectoryName[name_, OptionsPattern[DirectoryName]]"
        return self.apply(name, None, evaluation, options)


class FileNameDepth(Builtin):
    """
    <dl>
    <dt>'FileNameDepth["$name$"]'
      <dd>gives the number of path parts in the given filename.
    </dl>

    >> FileNameDepth["a/b/c"]
     = 3

    >> FileNameDepth["a/b/c/"]
     = 3

    #> FileNameDepth[x]
     = FileNameDepth[x]

    #> FileNameDepth[$RootDirectory]
     = 0
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    rules = {
        "FileNameDepth[name_String]": "Length[FileNameSplit[name]]",
    }


class AbsoluteFileName(Builtin):
    """
    <dl>
    <dt>'AbsoluteFileName["$name$"]'
      <dd>returns the absolute version of the given filename.
    </dl>

    >> AbsoluteFileName["ExampleData/sunflowers.jpg"]
     = ...

    #> AbsoluteFileName["Some/NonExistant/Path.ext"]
     : File not found during AbsoluteFileName[Some/NonExistant/Path.ext].
     = $Failed
    """

    attributes = "Protected"

    messages = {
        "fstr": ("File specification x is not a string of one or more characters."),
        "nffil": "File not found during `1`.",
    }

    def apply(self, name, evaluation):
        "AbsoluteFileName[name_]"

        py_name = name.to_python()

        if not (isinstance(py_name, str) and py_name[0] == py_name[-1] == '"'):
            evaluation.message("AbsoluteFileName", "fstr", name)
            return
        py_name = py_name[1:-1]

        result = path_search(py_name)

        if result is None:
            evaluation.message(
                "AbsoluteFileName", "nffil", Expression("AbsoluteFileName", name)
            )
            return SymbolFailed

        return String(osp.abspath(result))


class ExpandFileName(Builtin):
    """
    <dl>
    <dt>'ExpandFileName["$name$"]'
      <dd>expands $name$ to an absolute filename for your system.
    </dl>

    >> ExpandFileName["ExampleData/sunflowers.jpg"]
     = ...
    """

    attributes = "Protected"

    messages = {
        "string": "String expected at position 1 in `1`.",
    }

    def apply(self, name, evaluation):
        "ExpandFileName[name_]"

        py_name = name.to_python()

        if not (isinstance(py_name, str) and py_name[0] == py_name[-1] == '"'):
            evaluation.message(
                "ExpandFileName", "string", Expression("ExpandFileName", name)
            )
            return
        py_name = py_name[1:-1]

        return String(osp.abspath(py_name))


class FileInformation(Builtin):
    """
    <dl>
    <dt>'FileInformation["$file$"]'
      <dd>returns information about $file$.
    </dl>

    This function is totally undocumented in MMA!

    >> FileInformation["ExampleData/sunflowers.jpg"]
     = {File -> ..., FileType -> File, ByteCount -> 142286, Date -> ...}

    #> FileInformation["ExampleData/missing_file.jpg"]
     = {}
    """

    rules = {
        "FileInformation[name_String]": "If[FileExistsQ[name], {File -> ExpandFileName[name], FileType -> FileType[name], ByteCount -> FileByteCount[name], Date -> AbsoluteTime[FileDate[name]]}, {}]",
    }


class ReadList(Read):
    """
    <dl>
    <dt>'ReadList["$file$"]'
      <dd>Reads all the expressions until the end of file.
    <dt>'ReadList["$file$", $type$]'
      <dd>Reads objects of a specified type until the end of file.
    <dt>'ReadList["$file$", {$type1$, $type2$, ...}]'
      <dd>Reads a sequence of specified types until the end of file.
    </dl>

    >> ReadList[StringToStream["a 1 b 2"], {Word, Number}]
     = {{a, 1}, {b, 2}}

    >> str = StringToStream["\\"abc123\\""];
    >> ReadList[str]
     = {abc123}
    >> InputForm[%]
     = {"abc123"}

    #> ReadList[str, "Invalid"]
     : Invalid is not a valid format specification.
     = ReadList[..., Invalid]
    #> Close[str];


    #> ReadList[StringToStream["a 1 b 2"], {Word, Number}, 1]
     = {{a, 1}}
    """

    # TODO
    """
    #> ReadList[StringToStream["a 1 b 2"], {Word, Number}, -1]
     : Non-negative machine-sized integer expected at position 3 in ReadList[InputStream[String, ...], {Word, Number}, -1].
     = ReadList[InputStream[String, ...], {Word, Number}, -1]
    """

    # TODO: Expression type
    """
    #> ReadList[StringToStream["123 45 x y"], Expression]
     = {5535 x y}
    """

    # TODO: Accept newlines in input
    """
    >> ReadList[StringToStream["123\nabc"]]
     = {123, abc}
    >> InputForm[%]
     = {123, abc}
    """

    rules = {
        "ReadList[stream_]": "ReadList[stream, Expression]",
    }

    attributes = "Protected"

    options = {
        "NullRecords": "False",
        "NullWords": "False",
        "RecordSeparators": '{"\r\n", "\n", "\r"}',
        "TokenWords": "{}",
        "WordSeparators": '{" ", "\t"}',
    }

    def apply(self, channel, types, evaluation, options):
        "ReadList[channel_, types_, OptionsPattern[ReadList]]"

        # Options
        # TODO: Implement extra options
        # py_options = self.check_options(options)
        # null_records = py_options['NullRecords']
        # null_words = py_options['NullWords']
        # record_separators = py_options['RecordSeparators']
        # token_words = py_options['TokenWords']
        # word_separators = py_options['WordSeparators']

        result = []
        while True:
            tmp = super(ReadList, self).apply(channel, types, evaluation, options)

            if tmp is None:
                return

            if tmp == SymbolFailed:
                return

            if tmp == Symbol("EndOfFile"):
                break
            result.append(tmp)
        return from_python(result)

    def apply_m(self, channel, types, m, evaluation, options):
        "ReadList[channel_, types_, m_, OptionsPattern[ReadList]]"

        # Options
        # TODO: Implement extra options
        # py_options = self.check_options(options)
        # null_records = py_options['NullRecords']
        # null_words = py_options['NullWords']
        # record_separators = py_options['RecordSeparators']
        # token_words = py_options['TokenWords']
        # word_separators = py_options['WordSeparators']

        py_m = m.get_int_value()
        if py_m < 0:
            evaluation.message(
                "ReadList", "intnm", Expression("ReadList", channel, types, m)
            )
            return

        result = []
        for i in range(py_m):
            tmp = super(ReadList, self).apply(channel, types, evaluation, options)

            if tmp == SymbolFailed:
                return

            if tmp.to_python() == "EndOfFile":
                break
            result.append(tmp)
        return from_python(result)


class FilePrint(Builtin):
    """
    <dl>
    <dt>'FilePrint[$file$]'
      <dd>prints the raw contents of $file$.
    </dl>

    #> exp = Sin[1];
    #> FilePrint[exp]
     : File specification Sin[1] is not a string of one or more characters.
     = FilePrint[Sin[1]]

    #> FilePrint["somenonexistantpath_h47sdmk^&h4"]
     : Cannot open somenonexistantpath_h47sdmk^&h4.
     = FilePrint[somenonexistantpath_h47sdmk^&h4]

    #> FilePrint[""]
     : File specification  is not a string of one or more characters.
     = FilePrint[]
    """

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
    }

    options = {
        "CharacterEncoding": "$CharacterEncoding",
        "RecordSeparators": '{"\r\n", "\n", "\r"}',
        "WordSeparators": '{" ", "\t"}',
    }

    attributes = "Protected"

    def apply(self, path, evaluation, options):
        "FilePrint[path_ OptionsPattern[FilePrint]]"
        pypath = path.to_python()
        if not (
            isinstance(pypath, str)
            and pypath[0] == pypath[-1] == '"'
            and len(pypath) > 2
        ):
            evaluation.message("FilePrint", "fstr", path)
            return
        pypath = path_search(pypath[1:-1])

        # Options
        record_separators = options["System`RecordSeparators"].to_python()
        assert isinstance(record_separators, list)
        assert all(
            isinstance(s, str) and s[0] == s[-1] == '"' for s in record_separators
        )
        record_separators = [s[1:-1] for s in record_separators]

        if pypath is None:
            evaluation.message("General", "noopen", path)
            return

        if not osp.isfile(pypath):
            return SymbolFailed

        try:
            with mathics_open(pypath, "r") as f:
                result = f.read()
        except IOError:
            evaluation.message("General", "noopen", path)
            return
        except MessageException as e:
            e.message(evaluation)
            return

        result = [result]
        for sep in record_separators:
            result = [item for res in result for item in res.split(sep)]

        if result[-1] == "":
            result = result[:-1]

        for res in result:
            evaluation.print_out(from_python(res))

        return SymbolNull


class Close(Builtin):
    """
    <dl>
    <dt>'Close[$stream$]'
      <dd>closes an input or output stream.
    </dl>

    >> Close[StringToStream["123abc"]]
     = String

    >> Close[OpenWrite[]]
     = ...

    #> Streams[] == (Close[OpenWrite[]]; Streams[])
     = True

    #> Close["abc"]
     : abc is not open.
     = Close[abc]

    #> strm = OpenWrite[];
    #> Close[strm];
    #> Quiet[Close[strm]]
     = Close[OutputStream[...]]
    """

    attributes = "Protected"

    messages = {
        "closex": "`1`.",
    }

    def apply(self, channel, evaluation):
        "Close[channel_]"

        if channel.has_form("InputStream", 2) or channel.has_form(  # noqa
            "OutputStream", 2
        ):
            [name, n] = channel.get_leaves()
            stream = _lookup_stream(n.get_int_value())
        else:
            stream = None

        if stream is None or stream.closed:
            evaluation.message("General", "openx", channel)
            return

        try:
            stream.close()
        except IOError as err:
            evaluation.message("Close", "closex", err.strerror)
        return name


class StreamPosition(Builtin):
    """
    <dl>
    <dt>'StreamPosition[$stream$]'
      <dd>returns the current position in a stream as an integer.
    </dl>

    >> str = StringToStream["Mathics is cool!"]
     = ...

    >> Read[str, Word]
     = Mathics

    >> StreamPosition[str]
     = 7
    """

    attributes = "Protected"

    def apply_input(self, name, n, evaluation):
        "StreamPosition[InputStream[name_, n_]]"
        stream = _lookup_stream(n.get_int_value())

        if stream is None or stream.closed:
            evaluation.message("General", "openx", name)
            return

        return from_python(stream.tell())

    def apply_output(self, name, n, evaluation):
        "StreamPosition[OutputStream[name_, n_]]"
        stream = _lookup_stream(n.get_int_value())

        if stream is None or stream.closed:
            evaluation.message("General", "openx", name)
            return

        return from_python(stream.tell())

    def apply_default(self, stream, evaluation):
        "StreamPosition[stream_]"
        evaluation.message("General", "stream", stream)
        return


class SetStreamPosition(Builtin):
    """
    <dl>
    <dt>'SetStreamPosition[$stream$, $n$]'
      <dd>sets the current position in a stream.
    </dl>

    >> str = StringToStream["Mathics is cool!"]
     = ...

    >> SetStreamPosition[str, 8]
     = 8

    >> Read[str, Word]
     = is

    #> SetStreamPosition[str, -5]
     : Python2 cannot handle negative seeks.
     = 10

    >> SetStreamPosition[str, Infinity]
     = 16
    """

    # TODO: Seeks beyond stream should return stmrng message
    """
    #> SetStreamPosition[str, 40]
     = ERROR_MESSAGE_HERE
    """

    messages = {
        "int": "Integer expected at position 2 in `1`.",
        "stmrng": (
            "Cannot set the current point in stream `1` to position `2`. The "
            "requested position exceeds the number of characters in the file"
        ),
        "python2": "Python2 cannot handle negative seeks.",  # FIXME: Python3?
    }

    attributes = "Protected"

    def apply_input(self, name, n, m, evaluation):
        "SetStreamPosition[InputStream[name_, n_], m_]"
        stream = _lookup_stream(n.get_int_value())

        if stream is None or stream.closed:
            evaluation.message("General", "openx", name)
            return

        if not stream.seekable:
            raise NotImplementedError

        seekpos = m.to_python()
        if not (isinstance(seekpos, int) or seekpos == float("inf")):
            evaluation.message(
                "SetStreamPosition", "stmrng", Expression("InputStream", name, n), m
            )
            return

        try:
            if seekpos == float("inf"):
                stream.seek(0, 2)
            else:
                if seekpos < 0:
                    stream.seek(seekpos, 2)
                else:
                    stream.seek(seekpos)
        except IOError:
            evaluation.message("SetStreamPosition", "python2")

        return from_python(stream.tell())

    def apply_output(self, name, n, m, evaluation):
        "SetStreamPosition[OutputStream[name_, n_], m_]"
        return self.apply_input(name, n, m, evaluation)

    def apply_default(self, stream, evaluation):
        "SetStreamPosition[stream_]"
        evaluation.message("General", "stream", stream)
        return


class Skip(Read):
    """
    <dl>
    <dt>'Skip[$stream$, $type$]'
      <dd>skips ahead in an input steream by one object of the specified $type$.
    <dt>'Skip[$stream$, $type$, $n$]'
      <dd>skips ahead in an input steream by $n$ objects of the specified $type$.
    </dl>

    >> str = StringToStream["a b c d"];
    >> Read[str, Word]
     = a
    >> Skip[str, Word]
    >> Read[str, Word]
     = c
    #> Close[str];

    >> str = StringToStream["a b c d"];
    >> Read[str, Word]
     = a
    >> Skip[str, Word, 2]
    >> Read[str, Word]
     = d
    #> Skip[str, Word]
     = EndOfFile
    #> Close[str];
    """

    rules = {
        "Skip[InputStream[name_, n_], types_]": "Skip[InputStream[name, n], types, 1]",
    }

    messages = {
        "intm": "Non-negative machine-sized integer expected at position 3 in `1`",
    }

    options = {
        "AnchoredSearch": "False",
        "IgnoreCase": "False",
        "WordSearch": "False",
        "RecordSeparators": '{"\r\n", "\n", "\r"}',
        "WordSeparators": '{" ", "\t"}',
    }

    attributes = "Protected"

    def apply(self, name, n, types, m, evaluation, options):
        "Skip[InputStream[name_, n_], types_, m_, OptionsPattern[Skip]]"

        channel = Expression("InputStream", name, n)

        # Options
        # TODO Implement extra options
        # py_options = self.check_options(options)
        # null_records = py_options['NullRecords']
        # null_words = py_options['NullWords']
        # record_separators = py_options['RecordSeparators']
        # token_words = py_options['TokenWords']
        # word_separators = py_options['WordSeparators']

        py_m = m.to_python()
        if not (isinstance(py_m, int) and py_m > 0):
            evaluation.message(
                "Skip",
                "intm",
                Expression("Skip", Expression("InputStream", name, n), types, m),
            )
            return
        for i in range(py_m):
            result = super(Skip, self).apply(channel, types, evaluation, options)
            if result == Symbol("EndOfFile"):
                return result
        return SymbolNull


class Find(Read):
    """
    <dl>
    <dt>'Find[$stream$, $text$]'
      <dd>find the first line in $stream$ that contains $text$.
    </dl>

    >> str = OpenRead["ExampleData/EinsteinSzilLetter.txt"];
    >> Find[str, "uranium"]
     = in manuscript, leads me to expect that the element uranium may be turned into
    >> Find[str, "uranium"]
     = become possible to set up a nuclear chain reaction in a large mass of uranium,
    >> Close[str]
     = ...

    >> str = OpenRead["ExampleData/EinsteinSzilLetter.txt"];
    >> Find[str, {"energy", "power"} ]
     = a new and important source of energy in the immediate future. Certain aspects
    >> Find[str, {"energy", "power"} ]
     = by which vast amounts of power and large quantities of new radium-like
    >> Close[str]
     = ...
    """

    attributes = "Protected"

    options = {
        "AnchoredSearch": "False",
        "IgnoreCase": "False",
        "WordSearch": "False",
        "RecordSeparators": '{"\r\n", "\n", "\r"}',
        "WordSeparators": '{" ", "\t"}',
    }

    def apply(self, name, n, text, evaluation, options):
        "Find[InputStream[name_, n_], text_, OptionsPattern[Find]]"

        # Options
        # TODO Implement extra options
        # py_options = self.check_options(options)
        # anchored_search = py_options['AnchoredSearch']
        # ignore_case = py_options['IgnoreCase']
        # word_search = py_options['WordSearch']
        # record_separators = py_options['RecordSeparators']
        # word_separators = py_options['WordSeparators']

        py_text = text.to_python()

        channel = Expression("InputStream", name, n)

        if not isinstance(py_text, list):
            py_text = [py_text]

        if not all(isinstance(t, str) and t[0] == t[-1] == '"' for t in py_text):
            evaluation.message("Find", "unknown", Expression("Find", channel, text))
            return

        py_text = [t[1:-1] for t in py_text]

        while True:
            tmp = super(Find, self).apply(
                channel, Symbol("Record"), evaluation, options
            )
            py_tmp = tmp.to_python()[1:-1]

            if py_tmp == "System`EndOfFile":
                evaluation.message(
                    "Find", "notfound", Expression("Find", channel, text)
                )
                return SymbolFailed

            for t in py_text:
                if py_tmp.find(t) != -1:
                    return from_python(py_tmp)


class FindList(Builtin):
    """
    <dl>
    <dt>'FindList[$file$, $text$]'
      <dd>returns a list of all lines in $file$ that contain $text$.
    <dt>'FindList[$file$, {$text1$, $text2$, ...}]'
      <dd>returns a list of all lines in $file$ that contain any of the specified string.
    <dt>'FindList[{$file1$, $file2$, ...}, ...]'
      <dd>returns a list of all lines in any of the $filei$ that contain the specified strings.
    </dl>

    >> str = FindList["ExampleData/EinsteinSzilLetter.txt", "uranium"];
    #> Length[str]
     = 7

    >> FindList["ExampleData/EinsteinSzilLetter.txt", "uranium", 1]
     = {in manuscript, leads me to expect that the element uranium may be turned into}

    #> FindList["ExampleData/EinsteinSzilLetter.txt", "project"]
     = {}

    #> FindList["ExampleData/EinsteinSzilLetter.txt", "uranium", 0]
     = $Failed
    """

    messages = {
        "strs": "String or non-empty list of strings expected at position `1` in `2`.",
        "intnm": "Non-negative machine-sized integer expected at position `1` in `2`.",
    }

    attributes = "Protected"

    options = {
        "AnchoredSearch": "False",
        "IgnoreCase": "False",
        "RecordSeparators": '{"\r\n", "\n", "\r"}',
        "WordSearch": "False",
        "WordSeparators": '{" ", "\t"}',
    }

    # TODO: Extra options AnchoredSearch, IgnoreCase RecordSeparators,
    # WordSearch, WordSeparators this is probably best done with a regex

    def apply_without_n(self, filename, text, evaluation, options):
        "FindList[filename_, text_, OptionsPattern[FindList]]"
        return self.apply(filename, text, None, evaluation, options)

    def apply(self, filename, text, n, evaluation, options):
        "FindList[filename_, text_, n_, OptionsPattern[FindList]]"
        py_text = text.to_python()
        py_name = filename.to_python()
        if n is None:
            py_n = None
            expr = Expression("FindList", filename, text)
        else:
            py_n = n.to_python()
            expr = Expression("FindList", filename, text, n)

        if not isinstance(py_text, list):
            py_text = [py_text]

        if not isinstance(py_name, list):
            py_name = [py_name]

        if not all(isinstance(t, str) and t[0] == t[-1] == '"' for t in py_name):
            evaluation.message("FindList", "strs", "1", expr)
            return SymbolFailed

        if not all(isinstance(t, str) and t[0] == t[-1] == '"' for t in py_text):
            evaluation.message("FindList", "strs", "2", expr)
            return SymbolFailed

        if not ((isinstance(py_n, int) and py_n >= 0) or py_n is None):
            evaluation.message("FindList", "intnm", "3", expr)
            return SymbolFailed

        if py_n == 0:
            return SymbolFailed

        py_text = [t[1:-1] for t in py_text]
        py_name = [t[1:-1] for t in py_name]

        results = []
        for path in py_name:
            try:
                with mathics_open(path, "r") as f:
                    lines = f.readlines()
            except IOError:
                evaluation.message("General", "noopen", path)
                return
            except MessageException as e:
                e.message(evaluation)
                return

            result = []
            for line in lines:
                for t in py_text:
                    if line.find(t) != -1:
                        result.append(line[:-1])
            results.append(result)

        results = [r for result in results for r in result]

        if isinstance(py_n, int):
            results = results[: min(py_n, len(results))]

        return from_python(results)


class InputStream(Builtin):
    """
    <dl>
    <dt>'InputStream[$name$, $n$]'
      <dd>represents an input stream.
    </dl>

    >> str = StringToStream["Mathics is cool!"]
     = ...
    >> Close[str]
     = String
    """

    attributes = "Protected"

    def apply(self, name, n, evaluation):
        "InputStream[name_, n_]"
        return


class OutputStream(Builtin):
    """
    <dl>
    <dt>'OutputStream[$name$, $n$]'
      <dd>represents an output stream.
    </dl>

    >> OpenWrite[]
     = ...
    >> Close[%]
     = ...
    """

    attributes = "Protected"

    def apply(self, name, n, evaluation):
        "OutputStream[name_, n_]"
        return


class StringToStream(Builtin):
    """
    <dl>
    <dt>'StringToStream[$string$]'
      <dd>converts a $string$ to an open input stream.
    </dl>

    >> strm = StringToStream["abc 123"]
     = InputStream[String, ...]

    #> Read[strm, Word]
     = abc

    #> Read[strm, Number]
     = 123

    #> Close[strm]
     = String
    """

    attributes = "Protected"

    def apply(self, string, evaluation):
        "StringToStream[string_]"
        pystring = string.to_python()[1:-1]
        stream = io.StringIO(str(pystring))

        name = Symbol("String")
        n = next(NSTREAMS)

        result = Expression("InputStream", name, Integer(n))

        STREAMS.append(stream)
        return result


class Streams(Builtin):
    """
    <dl>
    <dt>'Streams[]'
      <dd>returns a list of all open streams.
    </dl>

    >> Streams[]
     = ...

    #> OpenWrite[]
     = ...
    #> Streams[%[[1]]]
     = {OutputStream[...]}

    #> Streams["some_nonexistant_name"]
     = {}
    """

    attributes = "Protected"

    def apply(self, evaluation):
        "Streams[]"
        return self.apply_name(None, evaluation)

    def apply_name(self, name, evaluation):
        "Streams[name_String]"
        result = []
        for n in range(len(STREAMS)):
            stream = _lookup_stream(n)
            if stream is None or stream.closed:
                continue
            if isinstance(stream, io.StringIO):
                head = "InputStream"
                _name = Symbol("String")
            else:
                mode = stream.mode
                if mode in ["r", "rb"]:
                    head = "InputStream"
                elif mode in ["w", "a", "wb", "ab"]:
                    head = "OutputStream"
                else:
                    raise ValueError("Unknown mode {0}".format(mode))
                _name = String(stream.name)
            expr = Expression(head, _name, Integer(n))
            if name is None or _name == name:
                result.append(expr)
        return Expression("List", *result)


class Compress(Builtin):
    u"""
    <dl>
    <dt>'Compress[$expr$]'
      <dd>gives a compressed string representation of $expr$.
    </dl>

    >> Compress[N[Pi, 10]]
     = eJwz1jM0MTS1NDIzNQEADRsCNw==

    """

    attributes = ("Protected",)

    options = {
        "Method": "{}",
    }

    def apply(self, expr, evaluation, options):
        "Compress[expr_, OptionsPattern[Compress]]"
        string = expr.format(evaluation, "System`FullForm")
        string = string.boxes_to_text(
            evaluation=evaluation, show_string_characters=True
        )
        string = string.encode("utf-8")

        # TODO Implement other Methods
        # Shouldn't be this a ByteArray?
        result = zlib.compress(string)
        result = base64.b64encode(result).decode("utf8")
        return String(result)


class Uncompress(Builtin):
    """
    <dl>
    <dt>'Uncompress["$string$"]'
      <dd>recovers an expression from a string generated by 'Compress'.
    </dl>

    >> Compress["Mathics is cool"]
     = eJxT8k0sychMLlbILFZIzs/PUQIANFwF1w==
    >> Uncompress[%]
     = Mathics is cool

    >> a = x ^ 2 + y Sin[x] + 10 Log[15];
    >> b = Compress[a];
    >> Uncompress[b]
     = x ^ 2 + y Sin[x] + 10 Log[15]
    """

    attributes = ("Protected",)

    def apply(self, string, evaluation):
        "Uncompress[string_String]"
        string = string.get_string_value()  # .encode("utf-8")
        string = base64.b64decode(string)
        tmp = zlib.decompress(string)
        tmp = tmp.decode("utf-8")
        return evaluation.parse(tmp)


class FileByteCount(Builtin):
    """
    <dl>
    <dt>'FileByteCount[$file$]'
      <dd>returns the number of bytes in $file$.
    </dl>

    >> FileByteCount["ExampleData/sunflowers.jpg"]
     = 142286
    """

    messages = {
        "fstr": "File specification `1` is not a string of one or more characters.",
    }

    def apply(self, filename, evaluation):
        "FileByteCount[filename_]"
        py_filename = filename.to_python()
        if not (
            isinstance(py_filename, str) and py_filename[0] == py_filename[-1] == '"'
        ):
            evaluation.message("FileByteCount", "fstr", filename)
            return
        py_filename = py_filename[1:-1]

        try:
            with mathics_open(py_filename, "rb") as f:
                count = 0
                tmp = f.read(1)
                while tmp != b"":
                    count += 1
                    tmp = f.read(1)

        except IOError:
            evaluation.message("General", "noopen", filename)
            return
        except MessageException as e:
            e.message(evaluation)
            return

        return from_python(count)


class FileHash(Builtin):
    """
    <dl>
    <dt>'FileHash[$file$]'
      <dd>returns an integer hash for the given $file$.
    <dt>'FileHash[$file$, $type$]'
      <dd>returns an integer hash of the specified $type$ for the given $file$.</dd>
      <dd>The types supported are "MD5", "Adler32", "CRC32", "SHA", "SHA224", "SHA256", "SHA384", and "SHA512".</dd>
    <dt>'FileHash[$file$, $type$, $format$]'
      <dd>gives a hash code in the specified format.</dd>
    </dl>

    >> FileHash["ExampleData/sunflowers.jpg"]
     = 109937059621979839952736809235486742106

    >> FileHash["ExampleData/sunflowers.jpg", "MD5"]
     = 109937059621979839952736809235486742106

    >> FileHash["ExampleData/sunflowers.jpg", "Adler32"]
     = 1607049478

    >> FileHash["ExampleData/sunflowers.jpg", "SHA256"]
     = 111619807552579450300684600241129773909359865098672286468229443390003894913065

    #> FileHash["ExampleData/sunflowers.jpg", "CRC32"]
     = 933095683
    #> FileHash["ExampleData/sunflowers.jpg", "SHA"]
     = 851696818771101405642332645949480848295550938123
    #> FileHash["ExampleData/sunflowers.jpg", "SHA224"]
     = 8723805623766373862936267623913366865806344065103917676078120867011
    #> FileHash["ExampleData/sunflowers.jpg", "SHA384"]
     = 28288410602533803613059815846847184383722061845493818218404754864571944356226472174056863474016709057507799332611860
    #> FileHash["ExampleData/sunflowers.jpg", "SHA512"]
     = 10111462070211820348006107532340854103555369343736736045463376555356986226454343186097958657445421102793096729074874292511750542388324853755795387877480102

    #> FileHash["ExampleData/sunflowers.jpg", xyzsymbol]
     = FileHash[ExampleData/sunflowers.jpg, xyzsymbol]
    #> FileHash["ExampleData/sunflowers.jpg", "xyzstr"]
     = FileHash[ExampleData/sunflowers.jpg, xyzstr, Integer]
    #> FileHash[xyzsymbol]
     = FileHash[xyzsymbol]
    """

    rules = {
        "FileHash[filename_String]": 'FileHash[filename, "MD5", "Integer"]',
        "FileHash[filename_String, hashtype_String]": 'FileHash[filename, hashtype, "Integer"]',
    }

    attributes = ("Protected", "ReadProtected")

    def apply(self, filename, hashtype, format, evaluation):
        "FileHash[filename_String, hashtype_String, format_String]"
        py_filename = filename.get_string_value()

        try:
            with mathics_open(py_filename, "rb") as f:
                dump = f.read()
        except IOError:
            evaluation.message("General", "noopen", filename)
            return
        except MessageException as e:
            e.message(evaluation)
            return

        return Hash.compute(
            lambda update: update(dump),
            hashtype.get_string_value(),
            format.get_string_value(),
        )


class FileDate(Builtin):
    """
    <dl>
    <dt>'FileDate[$file$, $types$]'
      <dd>returns the time and date at which the file was last modified.
    </dl>

    >> FileDate["ExampleData/sunflowers.jpg"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Access"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Creation"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Change"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Modification"]
     = ...

    >>  FileDate["ExampleData/sunflowers.jpg", "Rules"]
     = ...

    #>  FileDate["MathicsNonExistantExample"]
     : File not found during FileDate[MathicsNonExistantExample].
     = FileDate[MathicsNonExistantExample]
    #>  FileDate["MathicsNonExistantExample", "Modification"]
     : File not found during FileDate[MathicsNonExistantExample, Modification].
     = FileDate[MathicsNonExistantExample, Modification]

    #> FileDate["ExampleData/sunflowers.jpg", "Fail"]
     : Date type Fail should be "Access", "Modification", "Creation" (Windows only), "Change" (Macintosh and Unix only), or "Rules".
     = FileDate[ExampleData/sunflowers.jpg, Fail]
    """

    messages = {
        "nffil": "File not found during `1`.",
        "datetype": (
            'Date type Fail should be "Access", "Modification", '
            '"Creation" (Windows only), '
            '"Change" (Macintosh and Unix only), or "Rules".'
        ),
    }

    rules = {
        'FileDate[filepath_String, "Rules"]': """{"Access" -> FileDate[filepath, "Access"],
            "Creation" -> FileDate[filepath, "Creation"],
            "Change" -> FileDate[filepath, "Change"],
            "Modification" -> FileDate[filepath, "Modification"]}""",
    }

    attributes = "Protected"

    def apply(self, path, timetype, evaluation):
        "FileDate[path_, timetype_]"
        py_path = path_search(path.to_python()[1:-1])

        if py_path is None:
            if timetype is None:
                evaluation.message("FileDate", "nffil", Expression("FileDate", path))
            else:
                evaluation.message(
                    "FileDate", "nffil", Expression("FileDate", path, timetype)
                )
            return

        if timetype is None:
            time_type = "Modification"
        else:
            time_type = timetype.to_python()[1:-1]

        if time_type == "Access":
            result = osp.getatime(py_path)
        elif time_type == "Creation":
            if os.name == "posix":
                return Expression("Missing", "NotApplicable")
            result = osp.getctime(py_path)
        elif time_type == "Change":
            if os.name != "posix":
                return Expression("Missing", "NotApplicable")
            result = osp.getctime(py_path)
        elif time_type == "Modification":
            result = osp.getmtime(py_path)
        else:
            evaluation.message("FileDate", "datetype")
            return

        # Offset for system epoch
        epochtime = Expression(
            "AbsoluteTime", time.strftime("%Y-%m-%d %H:%M", time.gmtime(0))
        ).to_python(n_evaluation=evaluation)
        result += epochtime

        return Expression("DateList", from_python(result))

    def apply_default(self, path, evaluation):
        "FileDate[path_]"
        return self.apply(path, None, evaluation)


class SetFileDate(Builtin):
    """
    <dl>
    <dt>'SetFileDate["$file$"]'
      <dd>set the file access and modification dates of $file$ to the current date.
    <dt>'SetFileDate["$file$", $date$]'
      <dd>set the file access and modification dates of $file$ to the specified date list.
    <dt>'SetFileDate["$file$", $date$, "$type$"]'
      <dd>set the file date of $file$ to the specified date list.
      The "$type$" can be one of "$Access$", "$Creation$", "$Modification$", or 'All'.
    </dl>

    Create a temporary file (for example purposes)
    >> tmpfilename = $TemporaryDirectory <> "/tmp0";
    >> Close[OpenWrite[tmpfilename]];

    >> SetFileDate[tmpfilename, {2002, 1, 1, 0, 0, 0.}, "Access"];

    >> FileDate[tmpfilename, "Access"]
     = {2002, 1, 1, 0, 0, 0.}

    #> SetFileDate[tmpfilename, {2002, 1, 1, 0, 0, 0.}];
    #> FileDate[tmpfilename, "Access"]
     = {2002, 1, 1, 0, 0, 0.}

    #> SetFileDate[tmpfilename]
    #> FileDate[tmpfilename, "Access"]
     = {...}

    #> DeleteFile[tmpfilename]

    #> SetFileDate["MathicsNonExample"]
     : File not found during SetFileDate[MathicsNonExample].
     = $Failed
    """

    messages = {
        "fstr": (
            "File specification `1` is not a string of one or " "more characters."
        ),
        "nffil": "File not found during `1`.",
        "fdate": (
            "Date specification should be either the number of seconds "
            "since January 1, 1900 or a {y, m, d, h, m, s} list."
        ),
        "datetype": (
            'Date type a should be "Access", "Modification", '
            '"Creation" (Windows only), or All.'
        ),
        "nocreationunix": (
            "The Creation date of a file cannot be set on " "Macintosh or Unix."
        ),
    }

    attributes = "Protected"

    def apply(self, filename, datelist, attribute, evaluation):
        "SetFileDate[filename_, datelist_, attribute_]"

        py_filename = filename.to_python()

        if datelist is None:
            py_datelist = Expression("DateList").evaluate(evaluation).to_python()
            expr = Expression("SetFileDate", filename)
        else:
            py_datelist = datelist.to_python()

        if attribute is None:
            py_attr = "All"
            if datelist is not None:
                expr = Expression("SetFileDate", filename, datelist)
        else:
            py_attr = attribute.to_python()
            expr = Expression("SetFileDate", filename, datelist, attribute)

        # Check filename
        if not (
            isinstance(py_filename, str) and py_filename[0] == py_filename[-1] == '"'
        ):
            evaluation.message("SetFileDate", "fstr", filename)
            return
        py_filename = path_search(py_filename[1:-1])

        if py_filename is None:
            evaluation.message("SetFileDate", "nffil", expr)
            return SymbolFailed

        # Check datelist
        if not (
            isinstance(py_datelist, list)
            and len(py_datelist) == 6
            and all(isinstance(d, int) for d in py_datelist[:-1])
            and isinstance(py_datelist[-1], float)
        ):
            evaluation.message("SetFileDate", "fdate", expr)

        # Check attribute
        if py_attr not in ['"Access"', '"Creation"', '"Modification"', "All"]:
            evaluation.message("SetFileDate", "datetype")
            return

        epochtime = (
            Expression("AbsoluteTime", time.strftime("%Y-%m-%d %H:%M", time.gmtime(0)))
            .evaluate(evaluation)
            .to_python()
        )

        stattime = Expression("AbsoluteTime", from_python(py_datelist))
        stattime = stattime.to_python(n_evaluation=evaluation)

        stattime -= epochtime

        try:
            os.stat(py_filename)
            if py_attr == '"Access"':
                os.utime(py_filename, (stattime, osp.getatime(py_filename)))
            if py_attr == '"Creation"':
                if os.name == "posix":
                    evaluation.message("SetFileDate", "nocreationunix")
                    return SymbolFailed
                else:
                    # TODO: Note: This is windows only
                    return SymbolFailed
            if py_attr == '"Modification"':
                os.utime(py_filename, (osp.getatime(py_filename), stattime))
            if py_attr == "All":
                os.utime(py_filename, (stattime, stattime))
        except OSError as e:
            print(e)
            # evaluation.message(...)
            return SymbolFailed

        return SymbolNull

    def apply_1arg(self, filename, evaluation):
        "SetFileDate[filename_]"
        return self.apply(filename, None, None, evaluation)

    def apply_2arg(self, filename, datelist, evaluation):
        "SetFileDate[filename_, datelist_]"
        return self.apply(filename, datelist, None, evaluation)


class CopyFile(Builtin):
    """
    <dl>
    <dt>'CopyFile["$file1$", "$file2$"]'
      <dd>copies $file1$ to $file2$.
    </dl>

    X> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers.jpg"]
     = MathicsSunflowers.jpg
    X> DeleteFile["MathicsSunflowers.jpg"]
    """

    messages = {
        "filex": "Cannot overwrite existing file `1`.",
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "nffil": "File not found during `1`.",
    }

    attributes = "Protected"

    def apply(self, source, dest, evaluation):
        "CopyFile[source_, dest_]"

        py_source = source.to_python()
        py_dest = dest.to_python()

        # Check filenames
        if not (isinstance(py_source, str) and py_source[0] == py_source[-1] == '"'):
            evaluation.message("CopyFile", "fstr", source)
            return
        if not (isinstance(py_dest, str) and py_dest[0] == py_dest[-1] == '"'):
            evaluation.message("CopyFile", "fstr", dest)
            return

        py_source = py_source[1:-1]
        py_dest = py_dest[1:-1]

        py_source = path_search(py_source)

        if py_source is None:
            evaluation.message("CopyFile", "filex", source)
            return SymbolFailed

        if osp.exists(py_dest):
            evaluation.message("CopyFile", "filex", dest)
            return SymbolFailed

        try:
            shutil.copy(py_source, py_dest)
        except IOError:
            evaluation.message(
                "CopyFile", "nffil", Expression("CopyFile", source, dest)
            )
            return SymbolFailed

        return dest


class RenameFile(Builtin):
    """
    <dl>
    <dt>'RenameFile["$file1$", "$file2$"]'
      <dd>renames $file1$ to $file2$.
    </dl>

    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers.jpg"]
     = MathicsSunflowers.jpg
    >> RenameFile["MathicsSunflowers.jpg", "MathicsSunnyFlowers.jpg"]
     = MathicsSunnyFlowers.jpg
    >> DeleteFile["MathicsSunnyFlowers.jpg"]
    """

    messages = {
        "filex": "Cannot overwrite existing file `1`.",
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "nffil": "File not found during `1`.",
    }

    attributes = "Protected"

    def apply(self, source, dest, evaluation):
        "RenameFile[source_, dest_]"

        py_source = source.to_python()
        py_dest = dest.to_python()

        # Check filenames
        if not (isinstance(py_source, str) and py_source[0] == py_source[-1] == '"'):
            evaluation.message("RenameFile", "fstr", source)
            return
        if not (isinstance(py_dest, str) and py_dest[0] == py_dest[-1] == '"'):
            evaluation.message("RenameFile", "fstr", dest)
            return

        py_source = py_source[1:-1]
        py_dest = py_dest[1:-1]

        py_source = path_search(py_source)

        if py_source is None:
            evaluation.message("RenameFile", "filex", source)
            return SymbolFailed

        if osp.exists(py_dest):
            evaluation.message("RenameFile", "filex", dest)
            return SymbolFailed

        try:
            shutil.move(py_source, py_dest)
        except IOError:
            evaluation.message("RenameFile", "nffil", dest)
            return SymbolFailed

        return dest


class DeleteFile(Builtin):
    """
    <dl>
    <dt>'Delete["$file$"]'
      <dd>deletes $file$.
    <dt>'Delete[{"$file1$", "$file2$", ...}]'
      <dd>deletes a list of files.
    </dl>

    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers.jpg"];
    >> DeleteFile["MathicsSunflowers.jpg"]

    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers1.jpg"];
    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers2.jpg"];
    >> DeleteFile[{"MathicsSunflowers1.jpg", "MathicsSunflowers2.jpg"}]
    """

    messages = {
        "filex": "Cannot overwrite existing file `1`.",
        "strs": (
            "String or non-empty list of strings expected at " "position `1` in `2`."
        ),
        "nffil": "File not found during `1`.",
    }

    attributes = "Protected"

    def apply(self, filename, evaluation):
        "DeleteFile[filename_]"

        py_path = filename.to_python()
        if not isinstance(py_path, list):
            py_path = [py_path]

        py_paths = []
        for path in py_path:
            # Check filenames
            if not (isinstance(path, str) and path[0] == path[-1] == '"'):
                evaluation.message(
                    "DeleteFile", "strs", filename, Expression("DeleteFile", filename)
                )
                return

            path = path[1:-1]
            path = path_search(path)

            if path is None:
                evaluation.message(
                    "DeleteFile", "nffil", Expression("DeleteFile", filename)
                )
                return SymbolFailed
            py_paths.append(path)

        for path in py_paths:
            try:
                os.remove(path)
            except OSError:
                return SymbolFailed

        return SymbolNull


class DirectoryStack(Builtin):
    """
    <dl>
    <dt>'DirectoryStack[]'
      <dd>returns the directory stack.
    </dl>

    >> DirectoryStack[]
    = ...
    """

    attributes = "Protected"

    def apply(self, evaluation):
        "DirectoryStack[]"
        global DIRECTORY_STACK
        return from_python(DIRECTORY_STACK)


class Directory(Builtin):
    """
    <dl>
    <dt>'Directory[]'
      <dd>returns the current working directory.
    </dl>

    >> Directory[]
    = ...
    """

    attributes = "Protected"

    def apply(self, evaluation):
        "Directory[]"
        result = os.getcwd()
        return String(result)


class ParentDirectory(Builtin):
    """
    <dl>
    <dt>'ParentDirectory[]'
      <dd>returns the parent of the current working directory.
    <dt>'ParentDirectory["$dir$"]'
      <dd>returns the parent $dir$.
    </dl>

    >> ParentDirectory[]
     = ...
    """

    rules = {
        "ParentDirectory[]": "ParentDirectory[Directory[]]",
    }

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
    }

    attributes = "Protected"

    def apply(self, path, evaluation):
        "ParentDirectory[path_]"

        if not isinstance(path, String):
            evaluation.message("ParentDirectory", "fstr", path)
            return

        pypath = path.to_python()[1:-1]

        result = osp.abspath(osp.join(pypath, osp.pardir))
        return String(result)


class File(Builtin):
    attributes = "Protected"


class SetDirectory(Builtin):
    """
    <dl>
    <dt>'SetDirectory[$dir$]'
      <dd>sets the current working directory to $dir$.
    </dl>

    S> SetDirectory[]
    = ...

    #> SetDirectory["MathicsNonExample"]
     : Cannot set current directory to MathicsNonExample.
     = $Failed
    """

    rules = {
        "SetDirectory[]": "SetDirectory[$HomeDirectory]",
    }

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "cdir": "Cannot set current directory to `1`.",
    }

    attributes = "Protected"

    def apply(self, path, evaluation):
        "SetDirectory[path_]"

        if not isinstance(path, String):
            evaluation.message("SetDirectory", "fstr", path)
            return

        py_path = path.__str__()[1:-1]

        if py_path is None or not osp.isdir(py_path):
            evaluation.message("SetDirectory", "cdir", path)
            return SymbolFailed

        try:
            os.chdir(py_path)
        except:
            return SymbolFailed

        DIRECTORY_STACK.append(os.getcwd())
        return String(os.getcwd())


class ResetDirectory(Builtin):
    """
    <dl>
    <dt>'ResetDirectory[]'
      <dd>pops a directory from the directory stack and returns it.
    </dl>

    >> ResetDirectory[]
    = ...
    """

    messages = {
        "dtop": "Directory stack is empty.",
    }

    attributes = "Protected"

    def apply(self, evaluation):
        "ResetDirectory[]"
        try:
            tmp = DIRECTORY_STACK.pop()
        except IndexError:
            tmp = os.getcwd()
            evaluation.message("ResetDirectory", "dtop")
        else:
            os.chdir(tmp)
        return String(tmp)


class CreateDirectory(Builtin):
    """
    <dl>
    <dt>'CreateDirectory["$dir$"]'
      <dd>creates a directory called $dir$.
    <dt>'CreateDirectory[]'
      <dd>creates a temporary directory.
    </dl>

    >> dir = CreateDirectory[]
     = ...
    #> DirectoryQ[dir]
     = True
    #> DeleteDirectory[dir]
    """

    attributes = ("Listable", "Protected")

    options = {
        "CreateIntermediateDirectories": "True",
    }

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "nffil": "File not found during `1`.",
        "filex": "`1` already exists.",
    }

    def apply(self, dirname, evaluation, options):
        "CreateDirectory[dirname_, OptionsPattern[CreateDirectory]]"

        expr = Expression("CreateDirectory", dirname)
        py_dirname = dirname.to_python()

        if not (isinstance(py_dirname, str) and py_dirname[0] == py_dirname[-1] == '"'):
            evaluation.message("CreateDirectory", "fstr", dirname)
            return

        py_dirname = py_dirname[1:-1]

        if osp.isdir(py_dirname):
            evaluation.message("CreateDirectory", "filex", osp.abspath(py_dirname))
            return

        os.mkdir(py_dirname)

        if not osp.isdir(py_dirname):
            evaluation.message("CreateDirectory", "nffil", expr)
            return

        return String(osp.abspath(py_dirname))

    def apply_empty(self, evaluation, options):
        "CreateDirectory[OptionsPattern[CreateDirectory]]"
        dirname = tempfile.mkdtemp(prefix="m", dir=TMP_DIR)
        return String(dirname)


class DeleteDirectory(Builtin):
    """
    <dl>
    <dt>'DeleteDirectory["$dir$"]'
      <dd>deletes a directory called $dir$.
    </dl>

    >> dir = CreateDirectory[]
     = ...
    >> DeleteDirectory[dir]
    >> DirectoryQ[dir]
     = False
    #> Quiet[DeleteDirectory[dir]]
     = $Failed
    """

    attributes = "Protected"

    options = {
        "DeleteContents": "False",
    }

    messages = {
        "strs": (
            "String or non-empty list of strings expected at " "position 1 in `1`."
        ),
        "nodir": "Directory `1` not found.",
        "dirne": "Directory `1` not empty.",
        "optx": "Unknown option `1` in `2`",
        "idcts": "DeleteContents expects either True or False.",  # MMA Bug
    }

    def apply(self, dirname, evaluation, options):
        "DeleteDirectory[dirname_, OptionsPattern[DeleteDirectory]]"

        expr = Expression("DeleteDirectory", dirname)
        py_dirname = dirname.to_python()

        delete_contents = options["System`DeleteContents"].to_python()
        if delete_contents not in [True, False]:
            evaluation.message("DeleteDirectory", "idcts")
            return

        if not (isinstance(py_dirname, str) and py_dirname[0] == py_dirname[-1] == '"'):
            evaluation.message("DeleteDirectory", "strs", expr)
            return

        py_dirname = py_dirname[1:-1]

        if not osp.isdir(py_dirname):
            evaluation.message("DeleteDirectory", "nodir", dirname)
            return SymbolFailed

        if delete_contents:
            shutil.rmtree(py_dirname)
        else:
            if os.listdir(py_dirname) != []:
                evaluation.message("DeleteDirectory", "dirne", dirname)
                return SymbolFailed
            os.rmdir(py_dirname)

        return SymbolNull


class CopyDirectory(Builtin):
    """
    <dl>
    <dt>'CopyDirectory["$dir1$", "$dir2$"]'
      <dd>copies directory $dir1$ to $dir2$.
    </dl>
    """

    attributes = "Protected"

    messages = {
        "argr": "called with `1` argument; 2 arguments are expected.",
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "filex": "Cannot overwrite existing file `1`.",
        "nodir": "Directory `1` not found.",
    }

    def apply(self, dirs, evaluation):
        "CopyDirectory[dirs__]"

        seq = dirs.get_sequence()
        if len(seq) != 2:
            evaluation.message("CopyDirectory", "argr", len(seq))
            return
        (dir1, dir2) = (s.to_python() for s in seq)

        if not (isinstance(dir1, str) and dir1[0] == dir1[-1] == '"'):
            evaluation.message("CopyDirectory", "fstr", seq[0])
            return
        dir1 = dir1[1:-1]

        if not (isinstance(dir2, str) and dir2[0] == dir2[-1] == '"'):
            evaluation.message("CopyDirectory", "fstr", seq[1])
            return
        dir2 = dir2[1:-1]

        if not osp.isdir(dir1):
            evaluation.message("CopyDirectory", "nodir", seq[0])
            return SymbolFailed
        if osp.isdir(dir2):
            evaluation.message("CopyDirectory", "filex", seq[1])
            return SymbolFailed

        shutil.copytree(dir1, dir2)

        return String(osp.abspath(dir2))


class RenameDirectory(Builtin):
    """
    <dl>
    <dt>'RenameDirectory["$dir1$", "$dir2$"]'
      <dd>renames directory $dir1$ to $dir2$.
    </dl>
    """

    attributes = "Protected"

    messages = {
        "argr": "called with `1` argument; 2 arguments are expected.",
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "filex": "Cannot overwrite existing file `1`.",
        "nodir": "Directory `1` not found.",
    }

    def apply(self, dirs, evaluation):
        "RenameDirectory[dirs__]"

        seq = dirs.get_sequence()
        if len(seq) != 2:
            evaluation.message("RenameDirectory", "argr", len(seq))
            return
        (dir1, dir2) = (s.to_python() for s in seq)

        if not (isinstance(dir1, str) and dir1[0] == dir1[-1] == '"'):
            evaluation.message("RenameDirectory", "fstr", seq[0])
            return
        dir1 = dir1[1:-1]

        if not (isinstance(dir2, str) and dir2[0] == dir2[-1] == '"'):
            evaluation.message("RenameDirectory", "fstr", seq[1])
            return
        dir2 = dir2[1:-1]

        if not osp.isdir(dir1):
            evaluation.message("RenameDirectory", "nodir", seq[0])
            return SymbolFailed
        if osp.isdir(dir2):
            evaluation.message("RenameDirectory", "filex", seq[1])
            return SymbolFailed

        shutil.move(dir1, dir2)

        return String(osp.abspath(dir2))


class FileType(Builtin):
    """
    <dl>
    <dt>'FileType["$file$"]'
      <dd>gives the type of a file, a string. This is typically 'File', 'Directory' or 'None'.
    </dl>

    >> FileType["ExampleData/sunflowers.jpg"]
     = File
    >> FileType["ExampleData"]
     = Directory
    >> FileType["ExampleData/nonexistant"]
     = None

    #> FileType[x]
     : File specification x is not a string of one or more characters.
     = FileType[x]
    """

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
    }

    attributes = "Protected"

    def apply(self, filename, evaluation):
        "FileType[filename_]"
        if not isinstance(filename, String):
            evaluation.message("FileType", "fstr", filename)
            return
        path = filename.to_python()[1:-1]

        path = path_search(path)

        if path is None:
            return Symbol("None")

        if osp.isfile(path):
            return Symbol("File")
        else:
            return Symbol("Directory")


class FileExistsQ(Builtin):
    """
    <dl>
    <dt>'FileExistsQ["$file$"]'
      <dd>returns 'True' if $file$ exists and 'False' otherwise.
    </dl>

    >> FileExistsQ["ExampleData/sunflowers.jpg"]
     = True
    >> FileExistsQ["ExampleData/sunflowers.png"]
     = False
    """

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
    }

    attributes = "Protected"

    def apply(self, filename, evaluation):
        "FileExistsQ[filename_]"
        path = filename.to_python()
        if not (isinstance(path, str) and path[0] == path[-1] == '"'):
            evaluation.message("FileExistsQ", "fstr", filename)
            return
        path = path[1:-1]

        path = path_search(path)

        if path is None:
            return SymbolFalse
        return SymbolTrue


class DirectoryQ(Builtin):
    """
    <dl>
    <dt>'DirectoryQ["$name$"]'
      <dd>returns 'True' if the directory called $name$ exists and 'False' otherwise.
    </dl>

    >> DirectoryQ["ExampleData/"]
     = True
    >> DirectoryQ["ExampleData/MythicalSubdir/"]
     = False

    #> DirectoryQ["ExampleData"]
     = True

    #> DirectoryQ["ExampleData/MythicalSubdir/NestedDir/"]
     = False
    """

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
    }

    attributes = "Protected"

    def apply(self, pathname, evaluation):
        "DirectoryQ[pathname_]"
        path = pathname.to_python()

        if not (isinstance(path, str) and path[0] == path[-1] == '"'):
            evaluation.message("DirectoryQ", "fstr", pathname)
            return
        path = path[1:-1]

        path = path_search(path)

        if path is not None and osp.isdir(path):
            return SymbolTrue
        return SymbolFalse


class Needs(Builtin):
    """
    <dl>
    <dt>'Needs["context`"]'
        <dd>loads the specified context if not already in '$Packages'.
    </dl>

    >> Needs["VectorAnalysis`"]
    #> Needs["VectorAnalysis`"]

    #> Needs["SomeFakePackageOrTypo`"]
     : Cannot open SomeFakePackageOrTypo`.
     : Context SomeFakePackageOrTypo` was not created when Needs was evaluated.
     = $Failed

    #> Needs["VectorAnalysis"]
     : Invalid context specified at position 1 in Needs[VectorAnalysis]. A context must consist of valid symbol names separated by and ending with `.
     = Needs[VectorAnalysis]

    ## --- VectorAnalysis ---

    #> Needs["VectorAnalysis`"]

    #> DotProduct[{1,2,3}, {4,5,6}]
     = 32
    #> DotProduct[{-1.4, 0.6, 0.2}, {0.1, 0.6, 1.7}]
     = 0.56

    #> CrossProduct[{1,2,3}, {4,5,6}]
     = {-3, 6, -3}
    #> CrossProduct[{-1.4, 0.6, 0.2}, {0.1, 0.6, 1.7}]
     = {0.9, 2.4, -0.9}

    #> ScalarTripleProduct[{-2,3,1},{0,4,0},{-1,3,3}]
     = -20
    #> ScalarTripleProduct[{-1.4,0.6,0.2}, {0.1,0.6,1.7}, {0.7,-1.5,-0.2}]
     = -2.79

    #> CoordinatesToCartesian[{2, Pi, 3}, Spherical]
     = {0, 0, -2}
    #> CoordinatesFromCartesian[%, Spherical]
     = {2, Pi, 0}
    #> CoordinatesToCartesian[{2, Pi, 3}, Cylindrical]
     = {-2, 0, 3}
    #> CoordinatesFromCartesian[%, Cylindrical]
     = {2, Pi, 3}
    ## Needs Sin/Cos exact value (PR #100) for these tests to pass
    ## #> CoordinatesToCartesian[{2, Pi / 4, Pi / 3}, Spherical]
    ##  = {Sqrt[2] / 2, Sqrt[6] / 2, Sqrt[2]}
    ## #> CoordinatesFromCartesian[%, Spherical]
    ##  = {2, Pi / 4, Pi / 3}
    ## #> CoordinatesToCartesian[{2, Pi / 4, -1}, Cylindrical]
    ##  = {Sqrt[2], Sqrt[2], -1}
    ## #> CoordinatesFromCartesian[%, Cylindrical]
    ##  = {2, Pi / 4, -1}
    #> CoordinatesToCartesian[{0.27, 0.51, 0.92}, Cylindrical]
     = {0.235641, 0.131808, 0.92}
    #> CoordinatesToCartesian[{0.27, 0.51, 0.92}, Spherical]
     = {0.0798519, 0.104867, 0.235641}

    #> Coordinates[]
     = {Xx, Yy, Zz}
    #> Coordinates[Spherical]
     = {Rr, Ttheta, Pphi}
    #> SetCoordinates[Cylindrical]
     = Cylindrical[Rr, Ttheta, Zz]
    #> Coordinates[]
     = {Rr, Ttheta, Zz}
    #> CoordinateSystem
     = Cylindrical
    #> Parameters[]
     = {}
    #> CoordinateRanges[]
    ## = {0 <= Rr < Infinity, -Pi < Ttheta <= Pi, -Infinity < Zz < Infinity}
     = {0 <= Rr && Rr < Infinity, -Pi < Ttheta && Ttheta <= Pi, -Infinity < Zz < Infinity}
    #> CoordinateRanges[Cartesian]
     = {-Infinity < Xx < Infinity, -Infinity < Yy < Infinity, -Infinity < Zz < Infinity}
    #> ScaleFactors[Cartesian]
     = {1, 1, 1}
    #> ScaleFactors[Spherical]
     = {1, Rr, Rr Sin[Ttheta]}
    #> ScaleFactors[Cylindrical]
     = {1, Rr, 1}
    #> ScaleFactors[{2, 1, 3}, Cylindrical]
     = {1, 2, 1}
    #> JacobianDeterminant[Cartesian]
     = 1
    #> JacobianDeterminant[Spherical]
     = Rr ^ 2 Sin[Ttheta]
    #> JacobianDeterminant[Cylindrical]
     = Rr
    #> JacobianDeterminant[{2, 1, 3}, Cylindrical]
     = 2
    #> JacobianMatrix[Cartesian]
     = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
    #> JacobianMatrix[Spherical]
     = {{Cos[Pphi] Sin[Ttheta], Rr Cos[Pphi] Cos[Ttheta], -Rr Sin[Pphi] Sin[Ttheta]}, {Sin[Pphi] Sin[Ttheta], Rr Cos[Ttheta] Sin[Pphi], Rr Cos[Pphi] Sin[Ttheta]}, {Cos[Ttheta], -Rr Sin[Ttheta], 0}}
    #> JacobianMatrix[Cylindrical]
     = {{Cos[Ttheta], -Rr Sin[Ttheta], 0}, {Sin[Ttheta], Rr Cos[Ttheta], 0}, {0, 0, 1}}
    """

    messages = {
        "ctx": (
            "Invalid context specified at position `2` in `1`. "
            "A context must consist of valid symbol names separated by "
            "and ending with `3`."
        ),
        "nocont": "Context `1` was not created when Needs was evaluated.",
    }

    def apply(self, context, evaluation):
        "Needs[context_String]"
        contextstr = context.get_string_value()
        if contextstr == "":
            return SymbolNull
        if contextstr[0] == "`":
            curr_ctxt = evaluation.definitions.get_current_context()
            contextstr = curr_ctxt + contextstr[1:]
            context = String(contextstr)
        if not valid_context_name(contextstr):
            evaluation.message("Needs", "ctx", Expression("Needs", context), 1, "`")
            return
        test_loaded = Expression("MemberQ", Symbol("$Packages"), context)
        test_loaded = test_loaded.evaluate(evaluation)
        if test_loaded.is_true():
            # Already loaded
            return SymbolNull
        result = Expression("Get", context).evaluate(evaluation)

        if result == SymbolFailed:
            evaluation.message("Needs", "nocont", context)
            return SymbolFailed

        return SymbolNull


class URLSave(Builtin):
    """
    <dl>
    <dt>'URLSave["url"]'
        <dd>Save "url" in a temporary file.
    <dt>'URLSave["url", $filename$]'
        <dd>Save "url" in $filename$.
    </dl>
    """

    messages = {
        "invfile": "`1` is not a valid Filename",
        "invhttp": "`1` is not a valid URL",
    }

    def apply_1(self, url, evaluation, **options):
        "URLSave[url_String, OptionsPattern[URLSave]]"
        return self.apply_2(url, None, evaluation, **options)

    def apply_2(self, url, filename, evaluation, **options):
        "URLSave[url_String, filename_, OptionsPattern[URLSave]]"
        url = url.value
        if filename is None:
            result = urlsave_tmp(url, None, **options)
        elif filename.get_head_name() == "String":
            filename = filename.value
            result = urlsave_tmp(url, filename, **options)
        else:
            evaluation.message("URLSave", "invfile", filename)
            return SymbolFailed
        if result is None:
            return SymbolFailed
        return String(result)


class CreateFile(Builtin):
    """
    <dl>
    <dt>'CreateFile["filename"]'
        <dd>Creates a file named "filename" temporary file, but do not open it.
    <dt>'CreateFile[]'
        <dd>Creates a temporary file, but do not open it.
    </dl>
    """

    rules = {
        "CreateFile[]": "CreateTemporary[]",
    }
    options = {
        "CreateIntermediateDirectories": "True",
        "OverwriteTarget": "True",
    }

    def apply_1(self, filename, evaluation, **options):
        "CreateFile[filename_String, OptionsPattern[CreateFile]]"
        try:
            # TODO: Implement options
            if not osp.isfile(filename.value):
                f = open(filename.value, "w")
                res = f.name
                f.close()
                return String(res)
            else:
                return filename
        except:
            return SymbolFailed


class CreateTemporary(Builtin):
    """
    <dl>
    <dt>'CreateTemporary[]'
        <dd>Creates a temporary file, but do not open it.
    </dl>
    """

    def apply_0(self, evaluation):
        "CreateTemporary[]"
        try:
            res = create_temporary_file()
        except:
            return SymbolFailed
        return String(res)


class FileNames(Builtin):
    r"""
    <dl>
    <dt>'FileNames[]'
        <dd>Returns a list with the filenames in the current working folder.
    <dt>'FileNames[$form$]'
        <dd>Returns a list with the filenames in the current working folder that matches with $form$.
    <dt>'FileNames[{$form_1$, $form_2$, ...}]'
        <dd>Returns a list with the filenames in the current working folder that matches with one of $form_1$, $form_2$, ....
    <dt>'FileNames[{$form_1$, $form_2$, ...},{$dir_1$, $dir_2$, ...}]'
        <dd>Looks into the directories $dir_1$, $dir_2$, ....
    <dt>'FileNames[{$form_1$, $form_2$, ...},{$dir_1$, $dir_2$, ...}]'
        <dd>Looks into the directories $dir_1$, $dir_2$, ....
    <dt>'FileNames[{$forms$, $dirs$, $n$]'
        <dd>Look for files up to the level $n$.
    </dl>

    >> SetDirectory[$InstallationDirectory <> "/autoload"];
    >> FileNames["*.m", "formats"]//Length
     = 0
    >> FileNames["*.m", "formats", 3]//Length
     = 12
    >> FileNames["*.m", "formats", Infinity]//Length
     = 12
    """
    # >> FileNames[]//Length
    #  = 2
    fmtmaps = {Symbol("System`All"): "*"}
    options = {
        "IgnoreCase": "Automatic",
    }

    messages = {
        "nofmtstr": "`1` is not a format or a list of formats.",
        "nodirstr": "`1` is not a directory name  or a list of directory names.",
        "badn": "`1` is not an integer number.",
    }

    def apply_0(self, evaluation, **options):
        """FileNames[OptionsPattern[FileNames]]"""
        return self.apply_3(
            String("*"), String(os.getcwd()), None, evaluation, **options
        )

    def apply_1(self, forms, evaluation, **options):
        """FileNames[forms_, OptionsPattern[FileNames]]"""
        return self.apply_3(forms, String(os.getcwd()), None, evaluation, **options)

    def apply_2(self, forms, paths, evaluation, **options):
        """FileNames[forms_, paths_, OptionsPattern[FileNames]]"""
        return self.apply_3(forms, paths, None, evaluation, **options)

    def apply_3(self, forms, paths, n, evaluation, **options):
        """FileNames[forms_, paths_, n_, OptionsPattern[FileNames]]"""
        filenames = set()
        # Building a list of forms
        if forms.get_head_name() == "System`List":
            str_forms = []
            for p in forms._leaves:
                if self.fmtmaps.get(p, None):
                    str_forms.append(self.fmtmaps[p])
                else:
                    str_forms.append(p)
        else:
            str_forms = [
                self.fmtmaps[forms] if self.fmtmaps.get(forms, None) else forms
            ]
        # Building a list of directories
        if paths.get_head_name() == "System`String":
            str_paths = [paths.value]
        elif paths.get_head_name() == "System`List":
            str_paths = []
            for p in paths._leaves:
                if p.get_head_name() == "System`String":
                    str_paths.append(p.value)
                else:
                    evaluation.message("FileNames", "nodirstr", paths)
                    return
        else:
            evaluation.message("FileNames", "nodirstr", paths)
            return

        if n is not None:
            if n.get_head_name() == "System`Integer":
                n = n.get_int_value()
            elif n.get_head_name() == "System`DirectedInfinity":
                n = None
            else:
                print(n)
                evaluation.message("FileNames", "badn", n)
                return
        else:
            n = 1

        # list the files
        if options.get("System`IgnoreCase", None) == SymbolTrue:
            patterns = [
                re.compile(
                    "^" + to_regex(p, evaluation, abbreviated_patterns=True),
                    re.IGNORECASE,
                )
                + "$"
                for p in str_forms
            ]
        else:
            patterns = [
                re.compile(
                    "^" + to_regex(p, evaluation, abbreviated_patterns=True) + "$"
                )
                for p in str_forms
            ]

        for path in str_paths:
            if not osp.isdir(path):
                continue
            if n == 1:
                for fn in os.listdir(path):
                    fullname = osp.join(path, fn)
                    for pattern in patterns:
                        if pattern.match(fn):
                            filenames.add(fullname)
                            break
            else:
                pathlen = len(path)
                for root, dirs, files in os.walk(path):
                    # FIXME: This is an ugly and inefficient way
                    # to avoid looking deeper than the level n, but I do not realize
                    # how to do this better without a lot of code...
                    if n is not None and len(root[pathlen:].split(osp.sep)) > n:
                        continue
                    for fn in files + dirs:
                        for pattern in patterns:
                            if pattern.match(fn):
                                filenames.add(osp.join(root, fn))
                                break

        return Expression("List", *[String(s) for s in filenames])
