# -*- coding: utf-8 -*-
# cython: language_level=3

"""
File and Stream Operations
"""

import io
import math
import mpmath
import os
import struct
import sympy
import tempfile

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
    BaseExpression,
    Expression,
    Integer,
    MachineReal,
    Real,
    String,
    Symbol,
    SymbolFailed,
    SymbolNull,
    SymbolTrue,
    from_mpmath,
    from_python,
)
from mathics.core.numbers import dps
from mathics.core.streams import (
    Stream,
    path_search,
    stream_manager,
)
import mathics
from mathics.builtin.base import Builtin, Predefined, BinaryOperator, PrefixOperator
from mathics.builtin.strings import to_python_encoding
from mathics.builtin.base import MessageException

INITIAL_DIR = os.getcwd()
DIRECTORY_STACK = [INITIAL_DIR]

INPUT_VAR = ""
INPUTFILE_VAR = ""

TMP_DIR = tempfile.gettempdir()
SymbolPath = Symbol("$Path")


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
            raise ValueError(f"Unknown format {mode}")
        return Expression(head, channel, Integer(n))
    elif channel.has_form("InputStream", 2):
        return channel
    elif channel.has_form("OutputStream", 2):
        return channel
    else:
        return None


class mathics_open(Stream):
    def __init__(self, name, mode="r", encoding=None):
        if encoding is not None:
            encoding = to_python_encoding(encoding)
            if "b" in mode:
                # We should not specify an encoding for a binary mode
                encoding = None
            elif encoding is None:
                raise MessageException("General", "charcode", self.encoding)
        self.encoding = encoding
        super().__init__(name, mode, self.encoding)
        self.old_inputfile_var = None  # Set in __enter__ and __exit__

    def __enter__(self):
        # find path
        path = path_search(self.name)
        if path is None and self.mode in ["w", "a", "wb", "ab"]:
            path = self.name
        if path is None:
            raise IOError

        # open the stream
        fp = io.open(path, self.mode, encoding=self.encoding)
        global INPUTFILE_VAR
        INPUTFILE_VAR = osp.abspath(path)

        stream_manager.add(
            name=path,
            mode=self.mode,
            encoding=self.encoding,
            io=fp,
            num=stream_manager.next,
        )
        return fp

    def __exit__(self, type, value, traceback):
        global INPUTFILE_VAR
        INPUTFILE_VAR = self.old_inputfile_var or ""
        super().__exit__(type, value, traceback)


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


class EndOfFile(Builtin):
    """
    <dl>
    <dt>'EndOfFile'
      <dd>is returned by 'Read' when the end of an input stream is reached.
    </dl>
    """


SymbolEndOfFile = Symbol("EndOfFile")


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
      <dt>'Read[$stream$]'
      <dd>reads the input stream and returns one expression.

      <dt>'Read[$stream$, $type$]'
      <dd>reads the input stream and returns an object of the given type.

      <dt>'Read[$stream$, $type$]'
      <dd>reads the input stream and returns an object of the given type.

      <dt>'Read[$stream$, Hold[Expression]]'
      <dd>reads the input stream for an Expression and puts it inside 'Hold'.

    </dl>
    $type$ is one of:
    <ul>
      <li>Byte
      <li>Character
      <li>Expression
      <li>HoldExpression
      <li>Number
      <li>Real
      <li>Record
      <li>String
      <li>Word
    </ul>

    ## Malformed InputString
    #> Read[InputStream[String], {Word, Number}]
     = Read[InputStream[String], {Word, Number}]

    ## Correctly formed InputString but not open
    #> Read[InputStream[String, -1], {Word, Number}]
     : InputStream[String, -1] is not open.
     = Read[InputStream[String, -1], {Word, Number}]

    ## Reading Strings
    >> stream = StringToStream["abc123"];
    >> Read[stream, String]
     = abc123
    #> Read[stream, String]
     = EndOfFile
    #> Close[stream];

    ## Reading Words
    >> stream = StringToStream["abc 123"];
    >> Read[stream, Word]
     = abc
    >> Read[stream, Word]
     = 123
    #> Read[stream, Word]
     = EndOfFile
    #> Close[stream];
    #> stream = StringToStream[""];
    #> Read[stream, Word]
     = EndOfFile
    #> Read[stream, Word]
     = EndOfFile
    #> Close[stream];

    ## Number
    >> stream = StringToStream["123, 4"];
    >> Read[stream, Number]
     = 123
    >> Read[stream, Number]
     = 4
    #> Read[stream, Number]
     = EndOfFile
    #> Close[stream];
    #> stream = StringToStream["123xyz 321"];
    #> Read[stream, Number]
     = 123
    #> Quiet[Read[stream, Number]]
     = $Failed

    ## Real
    #> stream = StringToStream["123, 4abc"];
    #> Read[stream, Real]
     = 123.
    #> Read[stream, Real]
     = 4.
    #> Quiet[Read[stream, Number]]
     = $Failed

    #> Close[stream];
    #> stream = StringToStream["1.523E-19"]; Read[stream, Real]
     = 1.523*^-19
    #> Close[stream];
    #> stream = StringToStream["-1.523e19"]; Read[stream, Real]
     = -1.523*^19
    #> Close[stream];
    #> stream = StringToStream["3*^10"]; Read[stream, Real]
     = 3.*^10
    #> Close[stream];
    #> stream = StringToStream["3.*^10"]; Read[stream, Real]
     = 3.*^10
    #> Close[stream];

    ## Expression
    #> stream = StringToStream["x + y Sin[z]"]; Read[stream, Expression]
     = x + y Sin[z]
    #> Close[stream];
    ## #> stream = Quiet[StringToStream["Sin[1 123"]; Read[stream, Expression]]
    ##  = $Failed

    ## HoldExpression:
    >> stream = StringToStream["2+2\\n2+3"];

    'Read' with a 'Hold[Expression]' returns the expression it reads unevaluated so it can be later inspected and evaluated:

    >> Read[stream, Hold[Expression]]
     = Hold[2 + 2]

    >> Read[stream, Expression]
     = 5
    >> Close[stream];

    Reading a comment however will return the empy list:
    >> stream = StringToStream["(* ::Package:: *)"];

    >> Read[stream, Hold[Expression]]
     = {}

    >> Close[stream];

    ## Multiple types
    >> stream = StringToStream["123 abc"];
    >> Read[stream, {Number, Word}]
     = {123, abc}
    #> Read[stream, {Number, Word}]
     = EndOfFile
    #> Close[stream];

    #> stream = StringToStream["123 abc"];
    #> Quiet[Read[stream, {Word, Number}]]
     = $Failed
    #> Close[stream];

    #> stream = StringToStream["123 123"];  Read[stream, {Real, Number}]
     = {123., 123}
    #> Close[stream];

    #> Quiet[Read[stream, {Real}]]
     = Read[InputStream[String, ...], {Real}]

    Multiple lines:
    >> stream = StringToStream["\\"Tengo una\\nvaca lechera.\\""]; Read[stream]
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

        stream = stream_manager.lookup_stream(n.get_int_value())
        if stream is None:
            evaluation.message("Read", "openx", strm)
            return
        if stream.io is None:
            stream.__enter__()

        if stream.io.closed:
            evaluation.message("Read", "openx", strm)
            return

        # Wrap types in a list (if it isn't already one)
        if types.has_form("List", None):
            types = types._leaves
        else:
            types = (types,)

        # TODO: look for a better implementation handling "Hold[Expression]".
        #
        types = (
            Symbol("HoldExpression")
            if (
                typ.get_head_name() == "System`Hold"
                and typ.leaves[0].get_name() == "System`Expression"
            )
            else typ
            for typ in types
        )
        types = Expression("List", *types)

        READ_TYPES = [
            Symbol(k)
            for k in [
                "Byte",
                "Character",
                "Expression",
                "HoldExpression",
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
                        tmp = stream.io.read(1)
                    except UnicodeDecodeError:
                        tmp = " "  # ignore
                        evaluation.message("General", "ucdec")

                    if tmp == "":
                        if word == "":
                            pos = stream.io.tell()
                            newchar = stream.io.read(1)
                            if pos == stream.io.tell():
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
                        if stream.io.seekable():
                            # stream.io.seek(-1, 1) #Python3
                            stream.io.seek(stream.io.tell() - 1)
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
                    tmp = stream.io.read(1)
                    if tmp == "":
                        raise EOFError
                    result.append(ord(tmp))
                elif typ == Symbol("Character"):
                    tmp = stream.io.read(1)
                    if tmp == "":
                        raise EOFError
                    result.append(tmp)
                elif typ == Symbol("Expression") or typ == Symbol("HoldExpression"):
                    tmp = next(read_record)
                    while True:
                        try:
                            feeder = MathicsMultiLineFeeder(tmp)
                            expr = parse(evaluation.definitions, feeder)
                            break
                        except (IncompleteSyntaxError, InvalidSyntaxError):
                            try:
                                nextline = next(read_record)
                                tmp = tmp + "\n" + nextline
                            except EOFError:
                                expr = SymbolEndOfFile
                                break
                        except Exception as e:
                            print(e)

                    if expr == SymbolEndOfFile:
                        evaluation.message(
                            "Read", "readt", tmp, Expression("InputSteam", name, n)
                        )
                        return SymbolFailed
                    elif isinstance(expr, BaseExpression):
                        if typ == Symbol("HoldExpression"):
                            expr = Expression("Hold", expr)
                        result.append(expr)
                    # else:
                    #  TODO: Supposedly we can't get here
                    # what code should we put here?

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
                    tmp = stream.io.readline()
                    if len(tmp) == 0:
                        raise EOFError
                    result.append(tmp.rstrip("\n"))
                elif typ == Symbol("Word"):
                    result.append(next(read_word))

            except EOFError:
                return SymbolEndOfFile
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

    >> stream = OpenWrite[]
     = ...
    >> Write[stream, 10 x + 15 y ^ 2]
    >> Write[stream, 3 Sin[z]]
    >> Close[stream]
     = ...
    >> stream = OpenRead[%];
    >> ReadList[stream]
     = {10 x + 15 y ^ 2, 3 Sin[z]}
    #> Close[stream];
    """

    attributes = "Protected"

    def apply(self, channel, expr, evaluation):
        "Write[channel_, expr___]"

        strm = _channel_to_stream(channel)

        if strm is None:
            return

        n = strm.leaves[1].get_int_value()
        stream = stream_manager.lookup_stream(n)

        if stream is None or stream.io is None or stream.io.closed:
            evaluation.message("General", "openx", channel)
            return SymbolNull

        expr = expr.get_sequence()
        expr = Expression("Row", Expression("List", *expr))

        evaluation.format = "text"
        text = evaluation.format_output(from_python(expr))
        stream.io.write(str(text) + "\n")
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
    #> WRb[bytes_, form_] := Module[{stream, res={}, byte}, stream = OpenWrite[BinaryFormat -> True]; BinaryWrite[stream, bytes, form]; stream = OpenRead[Close[stream], BinaryFormat -> True]; While[Not[SameQ[byte = BinaryRead[stream], EndOfFile]], res = Join[res, {byte}];]; Close[stream]; res]

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
        stream = stream_manager.lookup_stream(n.get_int_value())

        if stream is None or stream.io.closed:
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
                self.writers[t](stream.io, x)
            except struct.error:
                return evaluation.message("BinaryWrite", "nocoerce", b)
            i += 1

        try:
            stream.io.flush()
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
    #> WbR[bytes_, form_] := Module[{stream, res}, stream = OpenWrite[BinaryFormat -> True]; BinaryWrite[stream, bytes]; stream = OpenRead[Close[stream], BinaryFormat -> True]; res = BinaryRead[stream, form]; Close[stream]; res]

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
        stream = stream_manager.lookup_stream(n.get_int_value())

        if stream is None or stream.io.closed:
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
                result.append(self.readers[t](stream.io))
            except struct.error:
                result.append(SymbolEndOfFile)

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

    >> stream = OpenWrite[];
    >> WriteString[stream, "This is a test 1"]
    >> WriteString[stream, "This is also a test 2"]
    >> Close[stream]
     = ...
    >> FilePrint[%]
     | This is a test 1This is also a test 2

    >> stream = OpenWrite[];
    >> WriteString[stream, "This is a test 1", "This is also a test 2"]
    >> Close[stream]
     = ...
    >> FilePrint[%]
     | This is a test 1This is also a test 2

    #> stream = OpenWrite[];
    #> WriteString[stream, 100, 1 + x + y, Sin[x  + y]]
    #> Close[stream]
     = ...
    #> FilePrint[%]
     | 1001 + x + ySin[x + y]

    #> stream = OpenWrite[];
    #> WriteString[stream]
    #> Close[stream]
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

        stream = stream_manager.lookup_stream(strm.leaves[1].get_int_value())

        if stream is None or stream.io is None or stream.io.closed:
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
        stream.io.write(line)
        try:
            stream.io.flush()
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

      <dt>'Get[$name$, Trace->True]'
      <dd>Runs Get tracing each line before it is evaluated.
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
        mathics.core.streams.PATH_VAR = SymbolPath.evaluate(evaluation).to_python(
            string_quotes=False
        )
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
        stream = stream_manager.lookup_stream(n.get_int_value())

        if stream is None or stream.io.closed:
            evaluation.message("Put", "openx", Expression("OutputSteam", name, n))
            return

        text = [
            evaluation.format_output(Expression("InputForm", expr))
            for expr in exprs.get_sequence()
        ]
        text = "\n".join(text) + "\n"
        text.encode("utf-8")

        stream.io.write(text)

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
        stream = stream_manager.lookup_stream(n.get_int_value())

        if stream is None or stream.io.closed:
            evaluation.message("Put", "openx", Expression("OutputSteam", name, n))
            return

        text = [
            str(e.do_format(evaluation, "System`OutputForm").__str__())
            for e in exprs.get_sequence()
        ]
        text = "\n".join(text) + "\n"
        text.encode("ascii")

        stream.io.write(text)

        return SymbolNull

    def apply_default(self, exprs, filename, evaluation):
        "PutAppend[exprs___, filename_]"
        expr = Expression("PutAppend", exprs, filename)
        evaluation.message("General", "stream", filename)
        return expr


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

    >> stream = StringToStream["\\"abc123\\""];
    >> ReadList[stream]
     = {abc123}
    >> InputForm[%]
     = {"abc123"}

    #> ReadList[stream, "Invalid"]
     : Invalid is not a valid format specification.
     = ReadList[..., Invalid]
    #> Close[stream];


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

            if tmp == SymbolEndOfFile:
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

        if channel.has_form(("InputStream", "OutputStream"), 2):
            [name, n] = channel.get_leaves()
            stream = stream_manager.lookup_stream(n.get_int_value())
        else:
            stream = None

        if stream is None or stream.io is None or stream.io.closed:
            evaluation.message("General", "openx", channel)
            return

        stream.io.close()
        return name


class StreamPosition(Builtin):
    """
    <dl>
    <dt>'StreamPosition[$stream$]'
      <dd>returns the current position in a stream as an integer.
    </dl>

    >> stream = StringToStream["Mathics is cool!"]
     = ...

    >> Read[stream, Word]
     = Mathics

    >> StreamPosition[stream]
     = 7
    """

    attributes = "Protected"

    def apply_input(self, name, n, evaluation):
        "StreamPosition[InputStream[name_, n_]]"
        stream = stream_manager.lookup_stream(n.get_int_value())

        if stream is None or stream.io is None or stream.io.closed:
            evaluation.message("General", "openx", name)
            return

        return from_python(stream.io.tell())

    def apply_output(self, name, n, evaluation):
        "StreamPosition[OutputStream[name_, n_]]"
        self.input_apply(name, n, evaluation)

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

    >> stream = StringToStream["Mathics is cool!"]
     = ...

    >> SetStreamPosition[stream, 8]
     = 8

    >> Read[stream, Word]
     = is

    #> SetStreamPosition[stream, -5]
     : Python2 cannot handle negative seeks.
     = 10

    >> SetStreamPosition[stream, Infinity]
     = 16
    """

    # TODO: Seeks beyond stream should return stmrng message
    """
    #> SetStreamPosition[stream, 40]
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
        stream = stream_manager.lookup_stream(n.get_int_value())

        if stream is None or stream.io is None or stream.io.closed:
            evaluation.message("General", "openx", name)
            return

        if not stream.io.seekable:
            raise NotImplementedError

        seekpos = m.to_python()
        if not (isinstance(seekpos, int) or seekpos == float("inf")):
            evaluation.message(
                "SetStreamPosition", "stmrng", Expression("InputStream", name, n), m
            )
            return

        try:
            if seekpos == float("inf"):
                stream.io.seek(0, 2)
            else:
                if seekpos < 0:
                    stream.io.seek(seekpos, 2)
                else:
                    stream.io.seek(seekpos)
        except IOError:
            evaluation.message("SetStreamPosition", "python2")

        return from_python(stream.io.tell())

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

    >> stream = StringToStream["a b c d"];
    >> Read[stream, Word]
     = a
    >> Skip[stream, Word]
    >> Read[stream, Word]
     = c
    #> Close[stream];

    >> stream = StringToStream["a b c d"];
    >> Read[stream, Word]
     = a
    >> Skip[stream, Word, 2]
    >> Read[stream, Word]
     = d
    #> Skip[stream, Word]
     = EndOfFile
    #> Close[stream];
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
            if result == SymbolEndOfFile:
                return result
        return SymbolNull


class Find(Read):
    """
    <dl>
    <dt>'Find[$stream$, $text$]'
      <dd>find the first line in $stream$ that contains $text$.
    </dl>

    >> stream = OpenRead["ExampleData/EinsteinSzilLetter.txt"];
    >> Find[stream, "uranium"]
     = in manuscript, leads me to expect that the element uranium may be turned into
    >> Find[stream, "uranium"]
     = become possible to set up a nuclear chain reaction in a large mass of uranium,
    >> Close[stream]
     = ...

    >> stream = OpenRead["ExampleData/EinsteinSzilLetter.txt"];
    >> Find[stream, {"energy", "power"} ]
     = a new and important source of energy in the immediate future. Certain aspects
    >> Find[stream, {"energy", "power"} ]
     = by which vast amounts of power and large quantities of new radium-like
    >> Close[stream]
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


class InputStream(Builtin):
    """
    <dl>
    <dt>'InputStream[$name$, $n$]'
      <dd>represents an input stream.
    </dl>

    >> stream = StringToStream["Mathics is cool!"]
     = ...
    >> Close[stream]
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
        fp = io.StringIO(str(pystring))

        name = Symbol("String")
        stream = stream_manager.add(pystring, io=fp)
        return Expression("InputStream", name, Integer(stream.n))


class Streams(Builtin):
    """
    <dl>
    <dt>'Streams[]'
      <dd>returns a list of all open streams.
    </dl>

    >> Streams[]
     = ...

    >> Streams["stdout"]
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
        for stream in stream_manager.STREAMS.values():
            if stream is None or stream.io.closed:
                continue
            if isinstance(stream.io, io.StringIO):
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
            expr = Expression(head, _name, Integer(stream.n))
            if name is None or _name == name:
                result.append(expr)
        return Expression("List", *result)
