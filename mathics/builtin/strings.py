# -*- coding: utf-8 -*-
"""
Strings and Characters - Miscellaneous
"""

import io
import re
import unicodedata
from binascii import hexlify, unhexlify
from heapq import heappush, heappop
from typing import Any, List

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import (
    Builtin,
    Test,
    Predefined,
    PrefixOperator,
)
from mathics.core.expression import (
    Expression,
    Symbol,
    SymbolFailed,
    SymbolFalse,
    SymbolTrue,
    SymbolList,
    String,
    Integer,
    Integer0,
    Integer1,
)
from mathics.core.parser import MathicsFileLineFeeder, parse
from mathics.settings import SYSTEM_CHARACTER_ENCODING
from mathics_scanner import TranslateError

_regex_longest = {
    "+": "+",
    "*": "*",
}

_regex_shortest = {
    "+": "+?",
    "*": "*?",
}


alphabet_descriptions = {
    "English": {
        "Lowercase": "abcdefghijklmnopqrstuvwxyz",
        "Uppercase": "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    },
    "Spanish": {
        "Lowercase": "abcdefghijklmnñopqrstuvwxyz",
        "Uppercase": "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ",
    },
    "Greek": {
        "Lowercase": "αβγδεζηθικλμνξοπρστυφχψω",
        "Uppercase": "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ",
    },
    "Cyrillic": {
        "Lowercase": "абвгґдђѓеёєжзѕиіїйјклљмнњопрстћќуўфхцчџшщъыьэюя",
        "Uppercase": "АБВГҐДЂЃЕЁЄЖЗЅИІЇЙЈКЛЉМНЊОПРСТЋЌУЎФХЦЧЏШЩЪЫЬЭЮЯ",
    },
}

alphabet_alias = {
    "English": "English",
    "French": "English",
    "German": "English",
    "Spanish": "Spanish",
    "Greek": "Greek",
    "Cyrillic": "Cyrillic",
    "Russian": "Cyrillic",
}


def _encode_pname(name):
    return "n" + hexlify(name.encode("utf8")).decode("utf8")


def _decode_pname(name):
    return unhexlify(name[1:]).decode("utf8")


def _evaluate_match(s, m, evaluation):
    replace = dict(
        (_decode_pname(name), String(value)) for name, value in m.groupdict().items()
    )
    return s.replace_vars(replace, in_scoping=False).evaluate(evaluation)


def _parallel_match(text, rules, flags, limit):
    heap = []

    def push(i, iter, form):
        m = None
        try:
            m = next(iter)
        except StopIteration:
            pass
        if m is not None:
            heappush(heap, (m.start(), i, m, form, iter))

    for i, (patt, form) in enumerate(rules):
        push(i, re.finditer(patt, text, flags=flags), form)

    k = 0
    n = 0

    while heap:
        start, i, match, form, iter = heappop(heap)

        if start >= k:
            yield match, form

            n += 1
            if n >= limit > 0:
                break

            k = match.end()

        push(i, iter, form)


def to_regex(
    expr, evaluation, q=_regex_longest, groups=None, abbreviated_patterns=False
):
    if expr is None:
        return None

    if groups is None:
        groups = {}

    def recurse(x, quantifiers=q):
        return to_regex(x, evaluation, q=quantifiers, groups=groups)

    if isinstance(expr, String):
        result = expr.get_string_value()
        if abbreviated_patterns:
            pieces = []
            i, j = 0, 0
            while j < len(result):
                c = result[j]
                if c == "\\" and j + 1 < len(result):
                    pieces.append(re.escape(result[i:j]))
                    pieces.append(re.escape(result[j + 1]))
                    j += 2
                    i = j
                elif c == "*":
                    pieces.append(re.escape(result[i:j]))
                    pieces.append("(.*)")
                    j += 1
                    i = j
                elif c == "@":
                    pieces.append(re.escape(result[i:j]))
                    # one or more characters, excluding uppercase letters
                    pieces.append("([^A-Z]+)")
                    j += 1
                    i = j
                else:
                    j += 1
            pieces.append(re.escape(result[i:j]))
            result = "".join(pieces)
        else:
            result = re.escape(result)
        return result
    if expr.has_form("RegularExpression", 1):
        regex = expr.leaves[0].get_string_value()
        if regex is None:
            return regex
        try:
            re.compile(regex)
            # Don't return the compiled regex because it may need to composed
            # further e.g. StringExpression["abc", RegularExpression[regex2]].
            return regex
        except re.error:
            return None  # invalid regex

    if isinstance(expr, Symbol):
        return {
            "System`NumberString": r"[-|+]?(\d+(\.\d*)?|\.\d+)?",
            "System`Whitespace": r"(?u)\s+",
            "System`DigitCharacter": r"\d",
            "System`WhitespaceCharacter": r"(?u)\s",
            "System`WordCharacter": r"(?u)[^\W_]",
            "System`StartOfLine": r"^",
            "System`EndOfLine": r"$",
            "System`StartOfString": r"\A",
            "System`EndOfString": r"\Z",
            "System`WordBoundary": r"\b",
            "System`LetterCharacter": r"(?u)[^\W_0-9]",
            "System`HexidecimalCharacter": r"[0-9a-fA-F]",
        }.get(expr.get_name())

    if expr.has_form("CharacterRange", 2):
        (start, stop) = (leaf.get_string_value() for leaf in expr.leaves)
        if all(x is not None and len(x) == 1 for x in (start, stop)):
            return "[{0}-{1}]".format(re.escape(start), re.escape(stop))

    if expr.has_form("Blank", 0):
        return r"(.|\n)"
    if expr.has_form("BlankSequence", 0):
        return r"(.|\n)" + q["+"]
    if expr.has_form("BlankNullSequence", 0):
        return r"(.|\n)" + q["*"]
    if expr.has_form("Except", 1, 2):
        if len(expr.leaves) == 1:
            leaves = [expr.leaves[0], Expression("Blank")]
        else:
            leaves = [expr.leaves[0], expr.leaves[1]]
        leaves = [recurse(leaf) for leaf in leaves]
        if all(leaf is not None for leaf in leaves):
            return "(?!{0}){1}".format(*leaves)
    if expr.has_form("Characters", 1):
        leaf = expr.leaves[0].get_string_value()
        if leaf is not None:
            return "[{0}]".format(re.escape(leaf))
    if expr.has_form("StringExpression", None):
        leaves = [recurse(leaf) for leaf in expr.leaves]
        if None in leaves:
            return None
        return "".join(leaves)
    if expr.has_form("Repeated", 1):
        leaf = recurse(expr.leaves[0])
        if leaf is not None:
            return "({0})".format(leaf) + q["+"]
    if expr.has_form("RepeatedNull", 1):
        leaf = recurse(expr.leaves[0])
        if leaf is not None:
            return "({0})".format(leaf) + q["*"]
    if expr.has_form("Alternatives", None):
        leaves = [recurse(leaf) for leaf in expr.leaves]
        if all(leaf is not None for leaf in leaves):
            return "|".join(leaves)
    if expr.has_form("Shortest", 1):
        return recurse(expr.leaves[0], quantifiers=_regex_shortest)
    if expr.has_form("Longest", 1):
        return recurse(expr.leaves[0], quantifiers=_regex_longest)
    if expr.has_form("Pattern", 2) and isinstance(expr.leaves[0], Symbol):
        name = expr.leaves[0].get_name()
        patt = groups.get(name, None)
        if patt is not None:
            if expr.leaves[1].has_form("Blank", 0):
                pass  # ok, no warnings
            elif not expr.leaves[1].sameQ(patt):
                evaluation.message(
                    "StringExpression", "cond", expr.leaves[0], expr, expr.leaves[0]
                )
            return "(?P=%s)" % _encode_pname(name)
        else:
            groups[name] = expr.leaves[1]
            return "(?P<%s>%s)" % (_encode_pname(name), recurse(expr.leaves[1]))

    return None


def anchor_pattern(patt):
    """
    anchors a regex in order to force matching against an entire string.
    """
    if not patt.endswith(r"\Z"):
        patt = patt + r"\Z"
    if not patt.startswith(r"\A"):
        patt = r"\A" + patt
    return patt


def mathics_split(patt, string, flags):
    """
    Python's re.split includes the text of groups if they are capturing.

    Furthermore, you can't split on empty matches. Trying to do this returns
    the original string for Python < 3.5, raises a ValueError for
    Python >= 3.5, <= X and works as expected for Python >= X, where 'X' is
    some future version of Python (> 3.6).

    For these reasons we implement our own split.
    """
    # (start, end) indices of splits
    indices = list((m.start(), m.end()) for m in re.finditer(patt, string, flags))

    # (start, end) indices of stuff to keep
    indices = [(None, 0)] + indices + [(len(string), None)]
    indices = [(indices[i][1], indices[i + 1][0]) for i in range(len(indices) - 1)]

    # slice up the string
    return [string[start:stop] for start, stop in indices]


class SystemCharacterEncoding(Predefined):
    """
    <dl>
    <dt>$SystemCharacterEncoding

    </dl>
    """

    name = "$SystemCharacterEncoding"

    rules = {
        "$SystemCharacterEncoding": '"' + SYSTEM_CHARACTER_ENCODING + '"',
    }


class CharacterEncoding(Predefined):
    """
    <dl>
    <dt>'CharacterEncoding'
        <dd>specifies the default character encoding to use if no other encoding is
        specified.
    </dl>
    """

    name = "$CharacterEncoding"
    value = '"UTF-8"'

    rules = {
        "$CharacterEncoding": value,
    }


_encodings = {
    # see https://docs.python.org/2/library/codecs.html#standard-encodings
    "ASCII": "ascii",
    "CP949": "cp949",
    "CP950": "cp950",
    "EUC-JP": "euc_jp",
    "IBM-850": "cp850",
    "ISOLatin1": "iso8859_1",
    "ISOLatin2": "iso8859_2",
    "ISOLatin3": "iso8859_3",
    "ISOLatin4": "iso8859_4",
    "ISOLatinCyrillic": "iso8859_5",
    "ISO8859-1": "iso8859_1",
    "ISO8859-2": "iso8859_2",
    "ISO8859-3": "iso8859_3",
    "ISO8859-4": "iso8859_4",
    "ISO8859-5": "iso8859_5",
    "ISO8859-6": "iso8859_6",
    "ISO8859-7": "iso8859_7",
    "ISO8859-8": "iso8859_8",
    "ISO8859-9": "iso8859_9",
    "ISO8859-10": "iso8859_10",
    "ISO8859-13": "iso8859_13",
    "ISO8859-14": "iso8859_14",
    "ISO8859-15": "iso8859_15",
    "ISO8859-16": "iso8859_16",
    "koi8-r": "koi8_r",
    "MacintoshCyrillic": "mac_cyrillic",
    "MacintoshGreek": "mac_greek",
    "MacintoshIcelandic": "mac_iceland",
    "MacintoshRoman": "mac_roman",
    "MacintoshTurkish": "mac_turkish",
    "ShiftJIS": "shift_jis",
    "Unicode": "utf_16",
    "UTF-8": "utf_8",
    "UTF8": "utf_8",
    "WindowsANSI": "cp1252",
    "WindowsBaltic": "cp1257",
    "WindowsCyrillic": "cp1251",
    "WindowsEastEurope": "cp1250",
    "WindowsGreek": "cp1253",
    "WindowsTurkish": "cp1254",
}


def to_python_encoding(encoding):
    return _encodings.get(encoding)


class CharacterEncodings(Predefined):
    name = "$CharacterEncodings"
    value = "{%s}" % ",".join(map(lambda s: '"%s"' % s, _encodings.keys()))

    rules = {
        "$CharacterEncodings": value,
    }


class NumberString(Builtin):
    """
    <dl>
    <dt>'NumberString'
      <dd>represents the characters in a number.
    </dl>

    >> StringMatchQ["1234", NumberString]
     = True

    >> StringMatchQ["1234.5", NumberString]
    = True

    >> StringMatchQ["1.2`20", NumberString]
     = False
    """


class Whitespace(Builtin):
    r"""
    <dl>
    <dt>'Whitespace'
      <dd>represents a sequence of whitespace characters.
    </dl>

    >> StringMatchQ["\r \n", Whitespace]
     = True

    >> StringSplit["a  \n b \r\n c d", Whitespace]
     = {a, b, c, d}

    >> StringReplace[" this has leading and trailing whitespace \n ", (StartOfString ~~ Whitespace) | (Whitespace ~~ EndOfString) -> ""] <> " removed" // FullForm
     = "this has leading and trailing whitespace removed"
    """


# FIXME: Generalize string.lower() and ord()
def letter_number(chars: List[str], start_ord) -> List["Integer"]:
    # Note caller has verified that everything isalpha() and
    # each char has length 1.
    return [Integer(ord(char.lower()) - start_ord) for char in chars]


class Alphabet(Builtin):
    """
     <dl>
      <dt>'Alphabet'[]
      <dd>gives the list of lowercase letters a-z in the English alphabet .

      <dt>'Alphabet[$type$]'
      <dd> gives the alphabet for the language or class $type$.
    </dl>

    >> Alphabet[]
     = {a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z}
    >> Alphabet["German"]
     = {a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z}

    """

    messages = {
        "nalph": "The alphabet `` is not known or not available.",
    }

    rules = {
        "Alphabet[]": """Alphabet["English"]""",
    }

    def apply(self, alpha, evaluation):
        """Alphabet[alpha_String]"""
        alphakey = alpha.get_string_value()
        alphakey = alphabet_alias[alphakey]
        if alphakey is None:
            evaluation.message("Alphabet", "nalph", alpha)
            return
        alphabet = alphabet_descriptions.get(alphakey, None)
        if alphabet is None:
            evaluation.message("Alphabet", "nalph", alpha)
            return
        return Expression(SymbolList, *[String(c) for c in alphabet["Lowercase"]])


class LetterNumber(Builtin):
    r"""
    <dl>
      <dt>'LetterNumber'[$c$]
      <dd>returns the position of the character $c$ in the English alphabet.

      <dt>'LetterNumber["string"]'
      <dd>returns a list of the positions of characters in string.
      <dt>'LetterNumber["string", $alpha$]'
      <dd>returns a list of the positions of characters in string, regarding the alphabet $alpha$.
    </dl>

    >> LetterNumber["b"]
     = 2

    LetterNumber also works with uppercase characters
    >> LetterNumber["B"]
     = 2

    >> LetterNumber["ss2!"]
     = {19, 19, 0, 0}

    Get positions of each of the letters in a string:
    >> LetterNumber[Characters["Peccary"]]
    = {16, 5, 3, 3, 1, 18, 25}

    >> LetterNumber[{"P", "Pe", "P1", "eck"}]
    = {16, {16, 5}, {16, 0}, {5, 3, 11}}

    #> LetterNumber[4]
     : The argument 4 is not a string.
     = LetterNumber[4]

    >> LetterNumber["\[Beta]", "Greek"]
     = 2

    """
    # FIXME: put the right unicode characters in a way that the
    # following test works...
    r"""
    # #> LetterNumber["\[CapitalBeta]", "Greek"]
    #  = 2

    """
    messages = {
        "nalph": "The alphabet `` is not known or not available.",
        "nas": ("The argument `1` is not a string."),
    }

    def apply_alpha_str(self, chars: List[Any], alpha: String, evaluation):
        "LetterNumber[chars_, alpha_String]"
        alphakey = alpha.get_string_value()
        alphakey = alphabet_alias.get(alphakey, None)
        if alphakey is None:
            evaluation.message("LetterNumber", "nalph", alpha)
            return
        if alphakey == "English":
            return self.apply(chars, evaluation)
        alphabet = alphabet_descriptions.get(alphakey, None)
        if alphabet is None:
            evaluation.message("LetterNumber", "nalph", alpha)
            return
        # TODO: handle Uppercase
        if isinstance(chars, String):
            py_chars = chars.get_string_value()
            if len(py_chars) == 1:
                # FIXME generalize ord("a")
                res = alphabet["Lowercase"].find(py_chars) + 1
                if res == -1:
                    res = alphabet["Uppercase"].find(py_chars) + 1
                return Integer(res)
            else:
                r = []
                for c in py_chars:
                    cp = alphabet["Lowercase"].find(c) + 1
                    if cp == -1:
                        cp = alphabet["Uppercase"].find(c) + 1
                    r.append(cp)
                return Expression(SymbolList, *r)
        elif chars.has_form("List", 1, None):
            result = []
            for leaf in chars.leaves:
                result.append(self.apply_alpha_str(leaf, alpha, evaluation))
            return Expression(SymbolList, *result)
        else:
            return evaluation.message(self.__class__.__name__, "nas", chars)
        return None

    def apply(self, chars: List[Any], evaluation):
        "LetterNumber[chars_]"

        start_ord = ord("a") - 1
        if isinstance(chars, String):
            py_chars = chars.get_string_value()
            if len(py_chars) == 1:
                # FIXME generalize ord("a")
                return letter_number([py_chars[0]], start_ord)[0]
            else:
                r = [
                    letter_number(c, start_ord)[0] if c.isalpha() else 0
                    for c in py_chars
                ]
                return Expression(SymbolList, *r)
        elif chars.has_form("List", 1, None):
            result = []
            for leaf in chars.leaves:
                result.append(self.apply(leaf, evaluation))
            return Expression(SymbolList, *result)
        else:
            return evaluation.message(self.__class__.__name__, "nas", chars)
        return None


class HexidecimalCharacter(Builtin):
    """
    <dl>
    <dt>'HexidecimalCharacter'
      <dd>represents the characters 0-9, a-f and A-F.
    </dl>

    >> StringMatchQ[#, HexidecimalCharacter] & /@ {"a", "1", "A", "x", "H", " ", "."}
     = {True, True, True, False, False, False, False}
    """


class _StringFind(Builtin):
    attributes = "Protected"

    options = {
        "IgnoreCase": "False",
        "MetaCharacters": "None",
    }

    messages = {
        "strse": "String or list of strings expected at position `1` in `2`.",
        "srep": "`1` is not a valid string replacement rule.",
        "innf": (
            "Non-negative integer or Infinity expected at " "position `1` in `2`."
        ),
    }

    def _find(py_stri, py_rules, py_n, flags):
        raise NotImplementedError()

    def _apply(self, string, rule, n, evaluation, options, cases):
        if n.sameQ(Symbol("System`Private`Null")):
            expr = Expression(self.get_name(), string, rule)
            n = None
        else:
            expr = Expression(self.get_name(), string, rule, n)

        # convert string
        if string.has_form("List", None):
            py_strings = [stri.get_string_value() for stri in string.leaves]
            if None in py_strings:
                return evaluation.message(self.get_name(), "strse", Integer1, expr)
        else:
            py_strings = string.get_string_value()
            if py_strings is None:
                return evaluation.message(self.get_name(), "strse", Integer1, expr)

        # convert rule
        def convert_rule(r):
            if r.has_form("Rule", None) and len(r.leaves) == 2:
                py_s = to_regex(r.leaves[0], evaluation)
                if py_s is None:
                    return evaluation.message(
                        "StringExpression", "invld", r.leaves[0], r.leaves[0]
                    )
                py_sp = r.leaves[1]
                return py_s, py_sp
            elif cases:
                py_s = to_regex(r, evaluation)
                if py_s is None:
                    return evaluation.message("StringExpression", "invld", r, r)
                return py_s, None

            return evaluation.message(self.get_name(), "srep", r)

        if rule.has_form("List", None):
            py_rules = [convert_rule(r) for r in rule.leaves]
        else:
            py_rules = [convert_rule(rule)]
        if None in py_rules:
            return None

        # convert n
        if n is None:
            py_n = 0
        elif n == Expression("DirectedInfinity", Integer1):
            py_n = 0
        else:
            py_n = n.get_int_value()
            if py_n is None or py_n < 0:
                return evaluation.message(self.get_name(), "innf", Integer(3), expr)

        # flags
        flags = re.MULTILINE
        if options["System`IgnoreCase"] == SymbolTrue:
            flags = flags | re.IGNORECASE

        if isinstance(py_strings, list):
            return Expression(
                "List",
                *[
                    self._find(py_stri, py_rules, py_n, flags, evaluation)
                    for py_stri in py_strings
                ]
            )
        else:
            return self._find(py_strings, py_rules, py_n, flags, evaluation)


class StringRepeat(Builtin):
    """
    <dl>
    <dt>'StringRepeat["$string$", $n$]'
        <dd>gives $string$ repeated $n$ times.
    <dt>'StringRepeat["$string$", $n$, $max$]'
        <dd>gives $string$ repeated $n$ times, but not more than $max$ characters.
    </dl>

    >> StringRepeat["abc", 3]
     = abcabcabc

    >> StringRepeat["abc", 10, 7]
     = abcabca

    #> StringRepeat["x", 0]
     : A positive integer is expected at position 2 in StringRepeat[x, 0].
     = StringRepeat[x, 0]
    """

    messages = {
        "intp": "A positive integer is expected at position `1` in `2`.",
    }

    def apply(self, s, n, expression, evaluation):
        "StringRepeat[s_String, n_]"
        py_n = n.get_int_value() if isinstance(n, Integer) else 0
        if py_n < 1:
            evaluation.message("StringRepeat", "intp", 2, expression)
        else:
            return String(s.get_string_value() * py_n)

    def apply_truncated(self, s, n, m, expression, evaluation):
        "StringRepeat[s_String, n_Integer, m_Integer]"
        py_n = n.get_int_value() if isinstance(n, Integer) else 0
        py_m = m.get_int_value() if isinstance(m, Integer) else 0

        if py_n < 1:
            evaluation.message("StringRepeat", "intp", 2, expression)
        elif py_m < 1:
            evaluation.message("StringRepeat", "intp", 3, expression)
        else:
            py_s = s.get_string_value()
            py_n = min(1 + py_m // len(py_s), py_n)

            return String((py_s * py_n)[:py_m])


class String_(Builtin):
    """
    <dl>
    <dt>'String'
        <dd>is the head of strings.
    </dl>

    >> Head["abc"]
     = String
    >> "abc"
     = abc

    Use 'InputForm' to display quotes around strings:
    >> InputForm["abc"]
     = "abc"

    'FullForm' also displays quotes:
    >> FullForm["abc" + 2]
     = Plus[2, "abc"]
    """

    name = "String"


class ToString(Builtin):
    """
    <dl>
    <dt>'ToString[$expr$]'
        <dd>returns a string representation of $expr$.
    <dt>'ToString[$expr$, $form$]'
        <dd>returns a string representation of $expr$ in the form
          $form$.
    </dl>

    >> ToString[2]
     = 2
    >> ToString[2] // InputForm
     = "2"
    >> ToString[a+b]
     = a + b
    >> "U" <> 2
     : String expected.
     = U <> 2
    >> "U" <> ToString[2]
     = U2
    >> ToString[Integrate[f[x],x], TeXForm]
     = \\int f\\left[x\\right] \\, dx

    """

    options = {
        "CharacterEncoding": '"Unicode"',
        "FormatType": "OutputForm",
        "NumberMarks": "$NumberMarks",
        "PageHeight": "Infinity",
        "PageWidth": "Infinity",
        "TotalHeight": "Infinity",
        "TotalWidth": "Infinity",
    }

    def apply_default(self, value, evaluation, options):
        "ToString[value_, OptionsPattern[ToString]]"
        return self.apply_form(value, Symbol("System`OutputForm"), evaluation, options)

    def apply_form(self, value, form, evaluation, options):
        "ToString[value_, form_, OptionsPattern[ToString]]"
        encoding = options["System`CharacterEncoding"]
        text = value.format(evaluation, form.get_name(), encoding=encoding)
        text = text.boxes_to_text(evaluation=evaluation)
        return String(text)


# This isn't your normal Box class. We'll keep this here rather than
# in mathics.builtin.box for now.
class InterpretedBox(PrefixOperator):
    r"""
    <dl>
      <dt>'InterpretedBox[$box$]'
      <dd>is the ad hoc fullform for \! $box$. just
          for internal use...

    >> \! \(2+2\)
     = 4
    </dl>
    """

    operator = "\\!"
    precedence = 670

    def apply_dummy(self, boxes, evaluation):
        """InterpretedBox[boxes_]"""
        # TODO: the following is a very raw and dummy way to
        # handle these expressions.
        # In the first place, this should handle different kind
        # of boxes in different ways.
        reinput = boxes.boxes_to_text()
        return Expression("ToExpression", reinput).evaluate(evaluation)


class ToExpression(Builtin):
    r"""
    <dl>
      <dt>'ToExpression[$input$]'
      <dd>inteprets a given string as Mathics input.

      <dt>'ToExpression[$input$, $form$]'
      <dd>reads the given input in the specified $form$.

      <dt>'ToExpression[$input$, $form$, $h$]'
      <dd>applies the head $h$ to the expression before evaluating it.

    </dl>

    >> ToExpression["1 + 2"]
     = 3

    >> ToExpression["{2, 3, 1}", InputForm, Max]
     = 3

    >> ToExpression["2 3", InputForm]
     = 6

    Note that newlines are like semicolons, not blanks. So so the return value is the second-line value.
    >> ToExpression["2\[NewLine]3"]
     = 3

    #> ToExpression["log(x)", InputForm]
     = log x

    #> ToExpression["1+"]
     : Incomplete expression; more input is needed (line 1 of "ToExpression['1+']").
     = $Failed

    #> ToExpression[]
     : ToExpression called with 0 arguments; between 1 and 3 arguments are expected.
     = ToExpression[]
    """

    # TODO: Other forms
    """
    >> ToExpression["log(x)", TraditionalForm]
     = Log[x]
    >> ToExpression["log(x)", TraditionalForm]
     = Log[x]
    #> ToExpression["log(x)", StandardForm]
     = log x
    """
    attributes = ("Listable", "Protected")

    messages = {
        "argb": (
            "`1` called with `2` arguments; "
            "between `3` and `4` arguments are expected."
        ),
        "interpfmt": (
            "`1` is not a valid interpretation format. "
            "Valid interpretation formats include InputForm "
            "and any member of $BoxForms."
        ),
        "notstr": "The format type `1` is valid only for string input.",
    }

    def apply(self, seq, evaluation):
        "ToExpression[seq__]"

        # Organise Arguments
        py_seq = seq.get_sequence()
        if len(py_seq) == 1:
            (inp, form, head) = (py_seq[0], Symbol("InputForm"), None)
        elif len(py_seq) == 2:
            (inp, form, head) = (py_seq[0], py_seq[1], None)
        elif len(py_seq) == 3:
            (inp, form, head) = (py_seq[0], py_seq[1], py_seq[2])
        else:
            assert len(py_seq) > 3  # 0 case handled by apply_empty
            evaluation.message(
                "ToExpression",
                "argb",
                "ToExpression",
                Integer(len(py_seq)),
                Integer1,
                Integer(3),
            )
            return

        # Apply the different forms
        if form == Symbol("InputForm"):
            if isinstance(inp, String):

                # TODO: turn the below up into a function and call that.
                s = inp.get_string_value()
                short_s = s[:15] + "..." if len(s) > 16 else s
                with io.StringIO(s) as f:
                    f.name = """ToExpression['%s']""" % short_s
                    feeder = MathicsFileLineFeeder(f)
                    while not feeder.empty():
                        try:
                            query = parse(evaluation.definitions, feeder)
                        except TranslateError:
                            return SymbolFailed
                        finally:
                            feeder.send_messages(evaluation)
                        if query is None:  # blank line / comment
                            continue
                        result = query.evaluate(evaluation)

            else:
                result = inp
        else:
            evaluation.message("ToExpression", "interpfmt", form)
            return

        # Apply head if present
        if head is not None:
            result = Expression(head, result).evaluate(evaluation)

        return result

    def apply_empty(self, evaluation):
        "ToExpression[]"
        evaluation.message(
            "ToExpression", "argb", "ToExpression", Integer0, Integer1, Integer(3)
        )
        return


class StringQ(Test):
    """
    <dl>
    <dt>'StringQ[$expr$]'
      <dd>returns 'True' if $expr$ is a 'String', or 'False' otherwise.
    </dl>

    >> StringQ["abc"]
     = True
    >> StringQ[1.5]
     = False
    >> Select[{"12", 1, 3, 5, "yz", x, y}, StringQ]
     = {12, yz}
    """

    def test(self, expr):
        return isinstance(expr, String)


class RemoveDiacritics(Builtin):
    """
    <dl>
    <dt>'RemoveDiacritics[$s$]'
        <dd>returns a version of $s$ with all diacritics removed.
    </dl>

    >> RemoveDiacritics["en prononçant pêcher et pécher"]
     = en prononcant pecher et pecher

    >> RemoveDiacritics["piñata"]
     = pinata
    """

    def apply(self, s, evaluation):
        "RemoveDiacritics[s_String]"
        return String(
            unicodedata.normalize("NFKD", s.get_string_value())
            .encode("ascii", "ignore")
            .decode("ascii")
        )


class Transliterate(Builtin):
    """
    <dl>
    <dt>'Transliterate[$s$]'
        <dd>transliterates a text in some script into an ASCII string.
    </dl>

    ASCII translateration examples can be found in:
    <ul>
      <li><url>https://en.wikipedia.org/wiki/Iliad,</url>
      <li><url>https://en.wikipedia.org/wiki/Russian_language</url>, and
      <li><url>https://en.wikipedia.org/wiki/Hiragana</url>
    </ul>
    """

    # Causes XeTeX to barf. Put this inside a unit test.
    # >> Transliterate["つかう"]
    #  = tsukau

    # >> Transliterate["Алекса́ндр Пу́шкин"]
    #  = Aleksandr Pushkin

    # > Transliterate["μήτηρ γάρ τέ μέ φησι θεὰ Θέτις ἀργυρόπεζα"]
    # = meter gar te me phesi thea Thetis arguropeza

    requires = ("unidecode",)

    def apply(self, s, evaluation):
        "Transliterate[s_String]"
        from unidecode import unidecode

        return String(unidecode(s.get_string_value()))


def _pattern_search(name, string, patt, evaluation, options, matched):
    # Get the pattern list and check validity for each
    if patt.has_form("List", None):
        patts = patt.get_leaves()
    else:
        patts = [patt]
    re_patts = []
    for p in patts:
        py_p = to_regex(p, evaluation)
        if py_p is None:
            return evaluation.message("StringExpression", "invld", p, patt)
        re_patts.append(py_p)

    flags = re.MULTILINE
    if options["System`IgnoreCase"] == SymbolTrue:
        flags = flags | re.IGNORECASE

    def _search(patts, str, flags, matched):
        if any(re.search(p, str, flags=flags) for p in patts):
            return SymbolTrue if matched else SymbolFalse
        return SymbolFalse if matched else SymbolTrue

    # Check string validity and perform regex searchhing
    if string.has_form("List", None):
        py_s = [s.get_string_value() for s in string.leaves]
        if any(s is None for s in py_s):
            return evaluation.message(
                name, "strse", Integer1, Expression(name, string, patt)
            )
        return Expression(
            SymbolList, *[_search(re_patts, s, flags, matched) for s in py_s]
        )
    else:
        py_s = string.get_string_value()
        if py_s is None:
            return evaluation.message(
                name, "strse", Integer1, Expression(name, string, patt)
            )
        return _search(re_patts, py_s, flags, matched)


class StringContainsQ(Builtin):
    """
    <dl>
    <dt>'StringContainsQ["$string$", $patt$]'
        <dd>returns True if any part of $string$ matches $patt$, and returns False otherwise.
    <dt>'StringContainsQ[{"s1", "s2", ...}, patt]'
        <dd>returns the list of results for each element of string list.
    <dt>'StringContainsQ[patt]'
        <dd>represents an operator form of StringContainsQ that can be applied to an expression.
    </dl>

    >> StringContainsQ["mathics", "m" ~~ __ ~~ "s"]
     = True

    >> StringContainsQ["mathics", "a" ~~ __ ~~ "m"]
     = False

    #> StringContainsQ["Hello", "o"]
     = True

    #> StringContainsQ["a"]["abcd"]
     = True

    #> StringContainsQ["Mathics", "ma", IgnoreCase -> False]
     = False

    >> StringContainsQ["Mathics", "MA" , IgnoreCase -> True]
     = True

    #> StringContainsQ["", "Empty String"]
     = False

    #> StringContainsQ["", ___]
     = True

    #> StringContainsQ["Empty Pattern", ""]
     = True

    #> StringContainsQ[notastring, "n"]
     : String or list of strings expected at position 1 in StringContainsQ[notastring, n].
     = StringContainsQ[notastring, n]

    #> StringContainsQ["Welcome", notapattern]
     : Element notapattern is not a valid string or pattern element in notapattern.
     = StringContainsQ[Welcome, notapattern]

    >> StringContainsQ[{"g", "a", "laxy", "universe", "sun"}, "u"]
     = {False, False, False, True, True}

    #> StringContainsQ[{}, "list of string is empty"]
     = {}

    >> StringContainsQ["e" ~~ ___ ~~ "u"] /@ {"The Sun", "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"}
     = {True, True, True, False, False, False, False, False, True}

    ## special cases, Mathematica allows list of patterns
    #> StringContainsQ[{"A", "Galaxy", "Far", "Far", "Away"}, {"F" ~~ __ ~~ "r", "aw" ~~ ___}]
     = {False, False, True, True, False}

    #> StringContainsQ[{"A", "Galaxy", "Far", "Far", "Away"}, {"F" ~~ __ ~~ "r", "aw" ~~ ___}, IgnoreCase -> True]
     = {False, False, True, True, True}

    #> StringContainsQ[{"A", "Galaxy", "Far", "Far", "Away"}, {}]
     = {False, False, False, False, False}

    #> StringContainsQ[{"A", Galaxy, "Far", "Far", Away}, {"F" ~~ __ ~~ "r", "aw" ~~ ___}]
     : String or list of strings expected at position 1 in StringContainsQ[{A, Galaxy, Far, Far, Away}, {F ~~ __ ~~ r, aw ~~ ___}].
     = StringContainsQ[{A, Galaxy, Far, Far, Away}, {F ~~ __ ~~ r, aw ~~ ___}]

    #> StringContainsQ[{"A", "Galaxy", "Far", "Far", "Away"}, {F ~~ __ ~~ "r", aw ~~ ___}]
     : Element F ~~ __ ~~ r is not a valid string or pattern element in {F ~~ __ ~~ r, aw ~~ ___}.
     = StringContainsQ[{A, Galaxy, Far, Far, Away}, {F ~~ __ ~~ r, aw ~~ ___}]
    ## Mathematica can detemine correct invalid element in the pattern, it reports error:
    ## Element F is not a valid string or pattern element in {F ~~ __ ~~ r, aw ~~ ___}.
    """

    options = {
        "IgnoreCase": "False",
    }

    rules = {
        "StringContainsQ[patt_][expr_]": "StringContainsQ[expr, patt]",
    }

    messages = {
        "strse": "String or list of strings expected at position `1` in `2`.",
    }

    def apply(self, string, patt, evaluation, options):
        "StringContainsQ[string_, patt_, OptionsPattern[%(name)s]]"
        return _pattern_search(
            self.__class__.__name__, string, patt, evaluation, options, True
        )
