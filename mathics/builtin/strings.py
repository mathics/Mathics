# -*- coding: utf-8 -*-
"""
Unsorted Strings and Characters
"""

import io
import re
import sys
import unicodedata
from binascii import hexlify, unhexlify
from heapq import heappush, heappop
from typing import Any, Callable, List

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import (
    BinaryOperator,
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
    string_list,
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


def pack_bytes(codes):
    return bytes(codes)

def unpack_bytes(codes):
    return [int(code) for code in codes]

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


class StringExpression(BinaryOperator):
    """
    <dl>
    <dt>'StringExpression[s_1, s_2, ...]'
      <dd>represents a sequence of strings and symbolic string objects $s_i$.
    </dl>

    >> "a" ~~ "b" // FullForm
     = "ab"

    #> "a" ~~ "b" ~~ "c" // FullForm
     = "abc"

    #> a ~~ b
     = a ~~ b
    """

    operator = "~~"
    precedence = 135
    attributes = ("Flat", "OneIdentity", "Protected")

    messages = {
        "invld": "Element `1` is not a valid string or pattern element in `2`.",
        "cond": "Ignored restriction given for `1` in `2` as it does not match previous occurences of `1`.",
    }

    def apply(self, args, evaluation):
        "StringExpression[args__String]"
        args = args.get_sequence()
        args = [arg.get_string_value() for arg in args]
        if None in args:
            return
        return String("".join(args))


class RegularExpression(Builtin):
    r"""
    <dl>
    <dt>'RegularExpression["regex"]'
      <dd>represents the regex specified by the string $"regex"$.
    </dl>

    >> StringSplit["1.23, 4.56  7.89", RegularExpression["(\\s|,)+"]]
     = {1.23, 4.56, 7.89}

    #> RegularExpression["[abc]"]
     = RegularExpression[[abc]]

    ## Mathematica doesn't seem to verify the correctness of regex
    #> StringSplit["ab23c", RegularExpression["[0-9]++"]]
     : Element RegularExpression[[0-9]++] is not a valid string or pattern element in RegularExpression[[0-9]++].
     = StringSplit[ab23c, RegularExpression[[0-9]++]]

    #> StringSplit["ab23c", RegularExpression[2]]
     : Element RegularExpression[2] is not a valid string or pattern element in RegularExpression[2].
     = StringSplit[ab23c, RegularExpression[2]]
    """


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


class DigitCharacter(Builtin):
    """
    <dl>
    <dt>'DigitCharacter'
      <dd>represents the digits 0-9.
    </dl>

    >> StringMatchQ["1", DigitCharacter]
     = True
    >> StringMatchQ["a", DigitCharacter]
     = False
    >> StringMatchQ["12", DigitCharacter]
     = False

    >> StringMatchQ["123245", DigitCharacter..]
     = True

    #> StringMatchQ["123245a6", DigitCharacter..]
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


class WhitespaceCharacter(Builtin):
    r"""
    <dl>
    <dt>'WhitespaceCharacter'
      <dd>represents a single whitespace character.
    </dl>

    >> StringMatchQ["\n", WhitespaceCharacter]
     = True

    >> StringSplit["a\nb\r\nc\rd", WhitespaceCharacter]
     = {a, b, c, d}

    For sequences of whitespace characters use 'Whitespace':
    >> StringMatchQ[" \n", WhitespaceCharacter]
     = False
    >> StringMatchQ[" \n", Whitespace]
     = True
    """


class WordCharacter(Builtin):
    r"""
    <dl>
    <dt>'WordCharacter'
      <dd>represents a single letter or digit character.
    </dl>

    >> StringMatchQ[#, WordCharacter] &/@ {"1", "a", "A", ",", " "}
     = {True, True, True, False, False}

    Test whether a string is alphanumeric:
    >> StringMatchQ["abc123DEF", WordCharacter..]
     = True
    >> StringMatchQ["$b;123", WordCharacter..]
     = False
    """


class StartOfString(Builtin):
    r"""
    <dl>
    <dt>'StartOfString'
      <dd>represents the start of a string.
    </dl>

    Test whether strings start with "a":
    >> StringMatchQ[#, StartOfString ~~ "a" ~~ __] &/@ {"apple", "banana", "artichoke"}
     = {True, False, True}

    >> StringReplace["aba\nabb", StartOfString ~~ "a" -> "c"]
     = cba
     . abb
    """


class EndOfString(Builtin):
    r"""
    <dl>
    <dt>'EndOfString'
      <dd>represents the end of a string.
    </dl>

    Test whether strings end with "e":
    >> StringMatchQ[#, __ ~~ "e" ~~ EndOfString] &/@ {"apple", "banana", "artichoke"}
     = {True, False, True}

    >> StringReplace["aab\nabb", "b" ~~ EndOfString -> "c"]
     = aab
     . abc
    """


class StartOfLine(Builtin):
    r"""
    <dl>
    <dt>'StartOfString'
      <dd>represents the start of a line in a string.
    </dl>

    >> StringReplace["aba\nbba\na\nab", StartOfLine ~~ "a" -> "c"]
     = cba
     . bba
     . c
     . cb

    >> StringSplit["abc\ndef\nhij", StartOfLine]
     = {abc
     . , def
     . , hij}
    """


class EndOfLine(Builtin):
    r"""
    <dl>
    <dt>'EndOfString'
      <dd>represents the end of a line in a string.
    </dl>

    >> StringReplace["aba\nbba\na\nab", "a" ~~ EndOfLine -> "c"]
     = abc
     . bbc
     . c
     . ab

    >> StringSplit["abc\ndef\nhij", EndOfLine]
     = {abc,
     . def,
     . hij}
    """


class WordBoundary(Builtin):
    """
    <dl>
    <dt>'WordBoundary'
      <dd>represents the boundary between words.
    </dl>

    >> StringReplace["apple banana orange artichoke", "e" ~~ WordBoundary -> "E"]
     = applE banana orangE artichokE
    """


class LetterCharacter(Builtin):
    """
    <dl>
    <dt>'LetterCharacter'
      <dd>represents letters.
    </dl>

    >> StringMatchQ[#, LetterCharacter] & /@ {"a", "1", "A", " ", "."}
     = {True, False, True, False, False}

    LetterCharacter also matches unicode characters.
    >> StringMatchQ["\\[Lambda]", LetterCharacter]
     = True
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


class StringSplit(Builtin):
    """
    <dl>
    <dt>'StringSplit["$s$"]'
        <dd>splits the string $s$ at whitespace, discarding the
        whitespace and returning a list of strings.
    <dt>'StringSplit["$s$", "$d$"]'
        <dd>splits $s$ at the delimiter $d$.
    <dt>'StringSplit[$s$, {"$d1$", "$d2$", ...}]'
        <dd>splits $s$ using multiple delimiters.
    <dt>'StringSplit[{$s_1$, $s_2, ...}, {"$d1$", "$d2$", ...}]'
        <dd>returns a list with the result of applying the function to
            each element.
    </dl>

    >> StringSplit["abc,123", ","]
     = {abc, 123}

    >> StringSplit["abc 123"]
     = {abc, 123}

    #> StringSplit["  abc    123  "]
     = {abc, 123}

    >> StringSplit["abc,123.456", {",", "."}]
     = {abc, 123, 456}

    >> StringSplit["a  b    c", RegularExpression[" +"]]
     = {a, b, c}

    >> StringSplit[{"a  b", "c  d"}, RegularExpression[" +"]]
     = {{a, b}, {c, d}}

    #> StringSplit["x", "x"]
     = {}

    #> StringSplit[x]
     : String or list of strings expected at position 1 in StringSplit[x].
     = StringSplit[x, Whitespace]

    #> StringSplit["x", x]
     : Element x is not a valid string or pattern element in x.
     = StringSplit[x, x]

    #> StringSplit["12312123", "12"..]
     = {3, 3}

    #> StringSplit["abaBa", "b"]
     = {a, aBa}
    #> StringSplit["abaBa", "b", IgnoreCase -> True]
     = {a, a, a}
    """

    rules = {
        "StringSplit[s_]": "StringSplit[s, Whitespace]",
    }

    options = {
        "IgnoreCase": "False",
        "MetaCharacters": "None",
    }

    messages = {
        "strse": "String or list of strings expected at position `1` in `2`.",
        "pysplit": "As of Python 3.5 re.split does not handle empty pattern matches.",
    }

    def apply(self, string, patt, evaluation, options):
        "StringSplit[string_, patt_, OptionsPattern[%(name)s]]"

        if string.get_head_name() == "System`List":
            leaves = [self.apply(s, patt, evaluation, options) for s in string._leaves]
            return Expression(SymbolList, *leaves)

        py_string = string.get_string_value()

        if py_string is None:
            return evaluation.message(
                "StringSplit", "strse", Integer1, Expression("StringSplit", string)
            )

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

        result = [py_string]
        for re_patt in re_patts:
            result = [t for s in result for t in mathics_split(re_patt, s, flags=flags)]

        return string_list(
            SymbolList, [String(x) for x in result if x != ""], evaluation
        )


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


class StringCases(_StringFind):
    """
    <dl>
    <dt>'StringCases["$string$", $pattern$]'
        <dd>gives all occurences of $pattern$ in $string$.
    <dt>'StringReplace["$string$", $pattern$ -> $form$]'
        <dd>gives all instances of $form$ that stem from occurences of $pattern$ in $string$.
    <dt>'StringCases["$string$", {$pattern1$, $pattern2$, ...}]'
        <dd>gives all occurences of $pattern1$, $pattern2$, ....
    <dt>'StringReplace["$string$", $pattern$, $n$]'
        <dd>gives only the first $n$ occurences.
    <dt>'StringReplace[{"$string1$", "$string2$", ...}, $pattern$]'
        <dd>gives occurences in $string1$, $string2$, ...
    </dl>

    >> StringCases["axbaxxb", "a" ~~ x_ ~~ "b"]
     = {axb}

    >> StringCases["axbaxxb", "a" ~~ x__ ~~ "b"]
     = {axbaxxb}

    >> StringCases["axbaxxb", Shortest["a" ~~ x__ ~~ "b"]]
     = {axb, axxb}

    >> StringCases["-abc- def -uvw- xyz", Shortest["-" ~~ x__ ~~ "-"] -> x]
     = {abc, uvw}

    >> StringCases["-öhi- -abc- -.-", "-" ~~ x : WordCharacter .. ~~ "-" -> x]
     = {öhi, abc}

    >> StringCases["abc-abc xyz-uvw", Shortest[x : WordCharacter .. ~~ "-" ~~ x_] -> x]
     = {abc}

    #> StringCases["abc-abc xyz-uvw", Shortest[x : WordCharacter .. ~~ "-" ~~ x : LetterCharacter] -> x]
     : Ignored restriction given for x in x : LetterCharacter as it does not match previous occurences of x.
     = {abc}

    >> StringCases["abba", {"a" -> 10, "b" -> 20}, 2]
     = {10, 20}

    >> StringCases["a#ä_123", WordCharacter]
     = {a, ä, 1, 2, 3}

    >> StringCases["a#ä_123", LetterCharacter]
     = {a, ä}
    """

    rules = {
        "StringCases[rule_][string_]": "StringCases[string, rule]",
    }

    def _find(self, py_stri, py_rules, py_n, flags, evaluation):
        def cases():
            for match, form in _parallel_match(py_stri, py_rules, flags, py_n):
                if form is None:
                    yield String(match.group(0))
                else:
                    yield _evaluate_match(form, match, evaluation)

        return Expression(SymbolList, *list(cases()))

    def apply(self, string, rule, n, evaluation, options):
        "%(name)s[string_, rule_, OptionsPattern[%(name)s], n_:System`Private`Null]"
        # this pattern is a slight hack to get around missing Shortest/Longest.
        return self._apply(string, rule, n, evaluation, options, True)


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


class ToCharacterCode(Builtin):
    """
    <dl>
    <dt>'ToCharacterCode["$string$"]'
      <dd>converts the string to a list of character codes (Unicode
      codepoints).
    <dt>'ToCharacterCode[{"$string1$", "$string2$", ...}]'
      <dd>converts a list of strings to character codes.
    </dl>

    >> ToCharacterCode["abc"]
     = {97, 98, 99}
    >> FromCharacterCode[%]
     = abc

    >> ToCharacterCode["\\[Alpha]\\[Beta]\\[Gamma]"]
     = {945, 946, 947}

    >> ToCharacterCode["ä", "UTF8"]
     = {195, 164}

    >> ToCharacterCode["ä", "ISO8859-1"]
     = {228}

    >> ToCharacterCode[{"ab", "c"}]
     = {{97, 98}, {99}}

    #> ToCharacterCode[{"ab"}]
     = {{97, 98}}

    #> ToCharacterCode[{{"ab"}}]
     : String or list of strings expected at position 1 in ToCharacterCode[{{ab}}].
     = ToCharacterCode[{{ab}}]

    >> ToCharacterCode[{"ab", x}]
     : String or list of strings expected at position 1 in ToCharacterCode[{ab, x}].
     = ToCharacterCode[{ab, x}]

    >> ListPlot[ToCharacterCode["plot this string"], Filling -> Axis]
     = -Graphics-

    #> ToCharacterCode[x]
     : String or list of strings expected at position 1 in ToCharacterCode[x].
     = ToCharacterCode[x]

    #> ToCharacterCode[""]
     = {}
    """

    messages = {
        "strse": "String or list of strings expected at position `1` in `2`.",
    }

    def _encode(self, string, encoding, evaluation):
        exp = Expression("ToCharacterCode", string)

        if string.has_form("List", None):
            string = [substring.get_string_value() for substring in string.leaves]
            if any(substring is None for substring in string):
                evaluation.message("ToCharacterCode", "strse", Integer1, exp)
                return None
        else:
            string = string.get_string_value()
            if string is None:
                evaluation.message("ToCharacterCode", "strse", Integer1, exp)
                return None

        if encoding == "Unicode":

            def convert(s):
                return Expression(SymbolList, *[Integer(ord(code)) for code in s])

        else:
            py_encoding = to_python_encoding(encoding)
            if py_encoding is None:
                evaluation.message("General", "charcode", encoding)
                return

            def convert(s):
                return Expression(
                    "List", *[Integer(x) for x in unpack_bytes(s.encode(py_encoding))]
                )

        if isinstance(string, list):
            return Expression(SymbolList, *[convert(substring) for substring in string])
        elif isinstance(string, str):
            return convert(string)

    def apply_default(self, string, evaluation):
        "ToCharacterCode[string_]"
        return self._encode(string, "Unicode", evaluation)

    def apply(self, string, encoding, evaluation):
        "ToCharacterCode[string_, encoding_String]"
        return self._encode(string, encoding.get_string_value(), evaluation)


class _InvalidCodepointError(ValueError):
    pass


class FromCharacterCode(Builtin):
    """
    <dl>
    <dt>'FromCharacterCode[$n$]'
        <dd>returns the character corresponding to Unicode codepoint $n$.
    <dt>'FromCharacterCode[{$n1$, $n2$, ...}]'
        <dd>returns a string with characters corresponding to $n_i$.
    <dt>'FromCharacterCode[{{$n11$, $n12$, ...}, {$n21$, $n22$, ...}, ...}]'
        <dd>returns a list of strings.
    </dl>

    >> FromCharacterCode[100]
     = d

    >> FromCharacterCode[228, "ISO8859-1"]
     = ä

    >> FromCharacterCode[{100, 101, 102}]
     = def
    >> ToCharacterCode[%]
     = {100, 101, 102}

    >> FromCharacterCode[{{97, 98, 99}, {100, 101, 102}}]
     = {abc, def}

    >> ToCharacterCode["abc 123"] // FromCharacterCode
     = abc 123

    #> #1 == ToCharacterCode[FromCharacterCode[#1]] & [RandomInteger[{0, 65535}, 100]]
     = True

    #> FromCharacterCode[{}] // InputForm
     = ""

    #> FromCharacterCode[65536]
     : A character code, which should be a non-negative integer less than 65536, is expected at position 1 in {65536}.
     = FromCharacterCode[65536]
    #> FromCharacterCode[-1]
     : Non-negative machine-sized integer expected at position 1 in FromCharacterCode[-1].
     = FromCharacterCode[-1]
    #> FromCharacterCode[444444444444444444444444444444444444]
     : Non-negative machine-sized integer expected at position 1 in FromCharacterCode[444444444444444444444444444444444444].
     = FromCharacterCode[444444444444444444444444444444444444]

    #> FromCharacterCode[{100, 101, -1}]
     : A character code, which should be a non-negative integer less than 65536, is expected at position 3 in {100, 101, -1}.
     = FromCharacterCode[{100, 101, -1}]
    #> FromCharacterCode[{100, 101, 65536}]
     : A character code, which should be a non-negative integer less than 65536, is expected at position 3 in {100, 101, 65536}.
     = FromCharacterCode[{100, 101, 65536}]
    #> FromCharacterCode[{100, 101, x}]
     : A character code, which should be a non-negative integer less than 65536, is expected at position 3 in {100, 101, x}.
     = FromCharacterCode[{100, 101, x}]
    #> FromCharacterCode[{100, {101}}]
     : A character code, which should be a non-negative integer less than 65536, is expected at position 2 in {100, {101}}.
     = FromCharacterCode[{100, {101}}]

    #> FromCharacterCode[{{97, 98, 99}, {100, 101, x}}]
     : A character code, which should be a non-negative integer less than 65536, is expected at position 3 in {100, 101, x}.
     = FromCharacterCode[{{97, 98, 99}, {100, 101, x}}]
    #> FromCharacterCode[{{97, 98, x}, {100, 101, x}}]
     : A character code, which should be a non-negative integer less than 65536, is expected at position 3 in {97, 98, x}.
     = FromCharacterCode[{{97, 98, x}, {100, 101, x}}]
    """

    messages = {
        "notunicode": (
            "A character code, which should be a non-negative integer less "
            "than 65536, is expected at position `2` in `1`."
        ),
        "intnm": (
            "Non-negative machine-sized integer expected at " "position `2` in `1`."
        ),
        "utf8": "The given codes could not be decoded as utf-8.",
    }

    def _decode(self, n, encoding, evaluation):
        exp = Expression("FromCharacterCode", n)

        py_encoding = to_python_encoding(encoding)
        if py_encoding is None:
            evaluation.message("General", "charcode", encoding)
            return

        def convert_codepoint_list(l):
            if encoding == "Unicode":
                s = ""
                for i, ni in enumerate(l):
                    pyni = ni.get_int_value()
                    if not (pyni is not None and 0 <= pyni <= 0xFFFF):
                        evaluation.message(
                            "FromCharacterCode",
                            "notunicode",
                            Expression(SymbolList, *l),
                            Integer(i + 1),
                        )
                        raise _InvalidCodepointError
                    s += chr(pyni)
                return s
            else:
                codes = [x.get_int_value() & 0xFF for x in l]
                return pack_bytes(codes).decode(py_encoding)

        try:
            if n.has_form("List", None):
                if not n.get_leaves():
                    return String("")
                # Mathematica accepts FromCharacterCode[{{100}, 101}],
                # so to match this, just check the first leaf to see
                # if we're dealing with nested lists.
                elif n.get_leaves()[0].has_form("List", None):
                    list_of_strings = []
                    for leaf in n.get_leaves():
                        if leaf.has_form("List", None):
                            stringi = convert_codepoint_list(leaf.get_leaves())
                        else:
                            stringi = convert_codepoint_list([leaf])
                        list_of_strings.append(String(stringi))
                    return Expression(SymbolList, *list_of_strings)
                else:
                    return String(convert_codepoint_list(n.get_leaves()))
            else:
                pyn = n.get_int_value()
                if not (isinstance(pyn, int) and pyn > 0 and pyn < sys.maxsize):
                    return evaluation.message(
                        "FromCharacterCode", "intnm", exp, Integer1
                    )
                return String(convert_codepoint_list([n]))
        except _InvalidCodepointError:
            return
        except UnicodeDecodeError:
            evaluation.message(self.get_name(), "utf8")
            return

        assert False, "can't get here"

    def apply_default(self, n, evaluation):
        "FromCharacterCode[n_]"
        return self._decode(n, "Unicode", evaluation)

    def apply(self, n, encoding, evaluation):
        "FromCharacterCode[n_, encoding_String]"
        return self._decode(n, encoding.get_string_value(), evaluation)


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


class HammingDistance(Builtin):
    """
    <dl>
    <dt>'HammingDistance[$u$, $v$]'
      <dd>returns the Hamming distance between $u$ and $v$, i.e. the number of different elements.
      $u$ and $v$ may be lists or strings.
    </dl>

    >> HammingDistance[{1, 0, 1, 0}, {1, 0, 0, 1}]
    = 2

    >> HammingDistance["time", "dime"]
    = 1

    >> HammingDistance["TIME", "dime", IgnoreCase -> True]
    = 1
    """

    messages = {
        "idim": "`1` and `2` must be of same length.",
    }

    options = {
        "IgnoreCase": "False",
    }

    @staticmethod
    def _compute(u, v, sameQ, evaluation):
        if len(u) != len(v):
            evaluation.message("HammingDistance", "idim", u, v)
            return None
        else:
            return Integer(sum(0 if sameQ(x, y) else 1 for x, y in zip(u, v)))

    def apply_list(self, u, v, evaluation):
        "HammingDistance[u_List, v_List]"
        return HammingDistance._compute(
            u.leaves, v.leaves, lambda x, y: x.sameQ(y), evaluation
        )

    def apply_string(self, u, v, evaluation, options):
        "HammingDistance[u_String, v_String, OptionsPattern[HammingDistance]]"
        ignore_case = self.get_option(options, "IgnoreCase", evaluation)
        py_u = u.get_string_value()
        py_v = v.get_string_value()
        if ignore_case and ignore_case.is_true():
            py_u = py_u.lower()
            py_v = py_v.lower()
        return HammingDistance._compute(py_u, py_v, lambda x, y: x == y, evaluation)


class _StringDistance(Builtin):
    options = {"IgnoreCase": "False"}

    def apply(self, a, b, evaluation, options):
        "%(name)s[a_, b_, OptionsPattern[%(name)s]]"
        if isinstance(a, String) and isinstance(b, String):
            py_a = a.get_string_value()
            py_b = b.get_string_value()
            if options["System`IgnoreCase"] == SymbolTrue:
                if hasattr(str, "casefold"):

                    def normalize(c):
                        return unicodedata.normalize("NFKD", c.casefold())

                    py_a = [normalize(c) for c in py_a]
                    py_b = [normalize(c) for c in py_b]
                else:  # python2, PyPy
                    py_a = py_a.lower()
                    py_b = py_b.lower()
            return Integer(self._distance(py_a, py_b, lambda u, v: u == v))
        elif a.get_head_name() == "System`List" and b.get_head_name() == "System`List":
            return Integer(self._distance(a.leaves, b.leaves, lambda u, v: u.sameQ(v)))
        else:
            return Expression("EditDistance", a, b)


# Levenshtein's algorithm is defined by the following construction:
# (adapted from https://de.wikipedia.org/wiki/Levenshtein-Distanz)
#
# given two strings s1, s2, we build a matrix D sized (len(s1) + 1,
# len(s2) + 1) and fill it using the following rules:
#
# (1) D(0, 0) = 0
# (2) D(i, 0) = i, 1 <= i <= len(s1)
# (3) D(0, j) = j, 1 <= j <= len(s2)
# (4) D(i, j) = minimum of
#     D(i - 1, j - 1) + 0 if s1(j) = s2(j)
#     D(i - 1, j - 1) + 1 (substitution)
#     D(i, j - 1) + 1     (insertion)
#     D(i - 1, j) + 1     (deletion)
#
# The computed distance will be in D(len(s1) + 1, len(s2) + 1).
#
# note: double brackets indicate 1-based indices below, e.g. s1[[1]]


def _one_based(l):  # makes an enumerated generator 1-based
    return ((i + 1, x) for i, x in l)


def _prev_curr(l):  # yields pairs of (x[i - 1], x[i]) for i in 1, 2, ...
    prev = None
    for curr in l:
        yield prev, curr
        prev = curr


def _levenshtein_d0(s2):  # compute D(0, ...)
    return list(range(len(s2) + 1))  # see (1), (3)


def _levenshtein_di(c1, s2, i, d_prev, sameQ, cost):  # compute one new row
    # given c1 = s1[i], s2, i, d_prev = D(i - 1, ...), compute D(i, ...)

    yield i  # start with D(i, 0) = i, see (2)
    d_curr_prev_j = i  # d_curr_prev_j stores D(i, j - 1)

    for j, c2 in _one_based(enumerate(s2)):  # c2 = s2[[j]]
        cond = 0 if sameQ(c1, c2) else cost

        d_curr_j = min(  # see (4)
            d_prev[j - 1] + cond,  # D(i - 1, j - 1) + cond; substitution
            d_curr_prev_j + 1,  # D(i, j - 1) + 1; insertion
            d_prev[j] + 1,
        )  # D(i - 1, j) + 1; deletion

        yield d_curr_j
        d_curr_prev_j = d_curr_j


def _levenshtein(s1, s2, sameQ: Callable[..., bool]):
    d_prev = _levenshtein_d0(s2)
    for i, c1 in _one_based(enumerate(s1)):  # c1 = s1[[i]]
        d_prev = list(_levenshtein_di(c1, s2, i, d_prev, sameQ, 1))
    return d_prev[-1]


def _damerau_levenshtein(s1, s2, sameQ: Callable[..., bool]):
    # _damerau_levenshtein works like _levenshtein, except for one additional
    # rule covering transposition:
    #
    # if i > 1 and j > 1 and a[i] == b[j - 1] and a[i - 1] == b[j] then
    #     D(i, j) = minimum(D(i, j), D(i - 2, j - 2) + transposition_cost)

    def row(d_prev_prev, d_prev, i, prev_c1, c1, cost):
        # given c1 = s1[i], d_prev_prev = D(i - 2), d_prev = D(i - 1),
        # prev_c1 = s1[[i - 1]], c1 = s1[[i]], compute D(i, ...)
        for j, d_curr_j in enumerate(_levenshtein_di(c1, s2, i, d_prev, sameQ, cost)):
            if i > 1 and j > 1:
                if sameQ(c1, s2[j - 2]) and sameQ(prev_c1, s2[j - 1]):  # transposition?
                    # i.e. if s1[[i]] = s2[[j-1]] and s1[[i-1]] = s2[[j]]
                    d_curr_j = min(d_curr_j, d_prev_prev[j - 2] + cost)
            yield d_curr_j

    d_prev_prev = None
    d_prev = _levenshtein_d0(s2)
    for i, (prev_c1, c1) in _one_based(enumerate(_prev_curr(s1))):
        d_curr = list(row(d_prev_prev, d_prev, i, prev_c1, c1, 1))
        d_prev_prev = d_prev
        d_prev = d_curr

    return d_prev[-1]


def _levenshtein_like_or_border_cases(s1, s2, sameQ: Callable[..., bool], compute):
    if len(s1) == len(s2) and all(sameQ(c1, c2) for c1, c2 in zip(s1, s2)):
        return 0

    if len(s1) < len(s2):
        s1, s2 = s2, s1

    if len(s2) == 0:
        return len(s1)

    return compute(s1, s2, sameQ)


class EditDistance(_StringDistance):
    """
    <dl>
    <dt>'EditDistance[$a$, $b$]'
        <dd>returns the Levenshtein distance of $a$ and $b$, which is defined as the minimum number of
        insertions, deletions and substitutions on the constituents of $a$ and $b$ needed to transform
        one into the other.
    </dl>

    >> EditDistance["kitten", "kitchen"]
     = 2

    >> EditDistance["abc", "ac"]
     = 1

    >> EditDistance["abc", "acb"]
     = 2

    >> EditDistance["azbc", "abxyc"]
     = 3

    The IgnoreCase option makes EditDistance ignore the case of letters:
    >> EditDistance["time", "Thyme"]
     = 3

    >> EditDistance["time", "Thyme", IgnoreCase -> True]
     = 2

    EditDistance also works on lists:
    >> EditDistance[{1, E, 2, Pi}, {1, E, Pi, 2}]
     = 2
    """

    def _distance(self, s1, s2, sameQ: Callable[..., bool]):
        return _levenshtein_like_or_border_cases(s1, s2, sameQ, _levenshtein)


class DamerauLevenshteinDistance(_StringDistance):
    """
    <dl>
    <dt>'DamerauLevenshteinDistance[$a$, $b$]'
        <dd>returns the Damerau-Levenshtein distance of $a$ and $b$, which is defined as the minimum number of
        transpositions, insertions, deletions and substitutions needed to transform one into the other.
        In contrast to EditDistance, DamerauLevenshteinDistance counts transposition of adjacent items (e.g.
        "ab" into "ba") as one operation of change.
    </dl>

    >> DamerauLevenshteinDistance["kitten", "kitchen"]
     = 2

    >> DamerauLevenshteinDistance["abc", "ac"]
     = 1

    >> DamerauLevenshteinDistance["abc", "acb"]
     = 1

    >> DamerauLevenshteinDistance["azbc", "abxyc"]
     = 3

    The IgnoreCase option makes DamerauLevenshteinDistance ignore the case of letters:
    >> DamerauLevenshteinDistance["time", "Thyme"]
     = 3

    >> DamerauLevenshteinDistance["time", "Thyme", IgnoreCase -> True]
     = 2

    DamerauLevenshteinDistance also works on lists:
    >> DamerauLevenshteinDistance[{1, E, 2, Pi}, {1, E, Pi, 2}]
     = 1
    """

    def _distance(self, s1, s2, sameQ: Callable[..., bool]):
        return _levenshtein_like_or_border_cases(s1, s2, sameQ, _damerau_levenshtein)


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

    # The following examples were taken from
    # https://en.wikipedia.org/wiki/Iliad,
    # https://en.wikipedia.org/wiki/Russian_language, and
    # https://en.wikipedia.org/wiki/Hiragana

    >> Transliterate["μήτηρ γάρ τέ μέ φησι θεὰ Θέτις ἀργυρόπεζα"]
     = meter gar te me phesi thea Thetis arguropeza

    >> Transliterate["Алекса́ндр Пу́шкин"]
     = Aleksandr Pushkin

    >> Transliterate["つかう"]
     = tsukau
    """

    requires = ("unidecode",)

    def apply(self, s, evaluation):
        "Transliterate[s_String]"
        from unidecode import unidecode

        return String(unidecode(s.get_string_value()))


class StringTrim(Builtin):
    """
    <dl>
    <dt>'StringTrim[$s$]'
        <dd>returns a version of $s$ with whitespace removed from start and end.
    </dl>

    >> StringJoin["a", StringTrim["  \\tb\\n "], "c"]
     = abc

    >> StringTrim["ababaxababyaabab", RegularExpression["(ab)+"]]
     = axababya
    """

    def apply(self, s, evaluation):
        "StringTrim[s_String]"
        return String(s.get_string_value().strip(" \t\n"))

    def apply_pattern(self, s, patt, expression, evaluation):
        "StringTrim[s_String, patt_]"
        text = s.get_string_value()
        if not text:
            return s

        py_patt = to_regex(patt, evaluation)
        if py_patt is None:
            return evaluation.message("StringExpression", "invld", patt, expression)

        if not py_patt.startswith(r"\A"):
            left_patt = r"\A" + py_patt
        else:
            left_patt = py_patt

        if not py_patt.endswith(r"\Z"):
            right_patt = py_patt + r"\Z"
        else:
            right_patt = py_patt

        m = re.search(left_patt, text)
        left = m.end(0) if m else 0

        m = re.search(right_patt, text)
        right = m.start(0) if m else len(text)

        return String(text[left:right])


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


class StringFreeQ(Builtin):
    """
    <dl>
    <dt>'StringFreeQ["$string$", $patt$]'
        <dd>returns True if no substring in $string$ matches the string expression $patt$, and returns False otherwise.
    <dt>'StringFreeQ[{"s1", "s2", ...}, patt]'
        <dd>returns the list of results for each element of string list.
    <dt>'StringFreeQ["string", {p1, p2, ...}]'
        <dd>returns True if no substring matches any of the $pi$.
    <dt>'StringFreeQ[patt]'
        <dd>represents an operator form of StringFreeQ that can be applied to an expression.
    </dl>

    >> StringFreeQ["mathics", "m" ~~ __ ~~ "s"]
     = False

    >> StringFreeQ["mathics", "a" ~~ __ ~~ "m"]
     = True

    #> StringFreeQ["Hello", "o"]
     = False

    #> StringFreeQ["a"]["abcd"]
     = False

    #> StringFreeQ["Mathics", "ma", IgnoreCase -> False]
     = True

    >> StringFreeQ["Mathics", "MA" , IgnoreCase -> True]
     = False

    #> StringFreeQ["", "Empty String"]
     = True

    #> StringFreeQ["", ___]
     = False

    #> StringFreeQ["Empty Pattern", ""]
     = False

    #> StringFreeQ[notastring, "n"]
     : String or list of strings expected at position 1 in StringFreeQ[notastring, n].
     = StringFreeQ[notastring, n]

    #> StringFreeQ["Welcome", notapattern]
     : Element notapattern is not a valid string or pattern element in notapattern.
     = StringFreeQ[Welcome, notapattern]

    >> StringFreeQ[{"g", "a", "laxy", "universe", "sun"}, "u"]
     = {True, True, True, False, False}

    #> StringFreeQ[{}, "list of string is empty"]
     = {}

    >> StringFreeQ["e" ~~ ___ ~~ "u"] /@ {"The Sun", "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"}
     = {False, False, False, True, True, True, True, True, False}

    #> StringFreeQ[{"A", "Galaxy", "Far", "Far", "Away"}, {"F" ~~ __ ~~ "r", "aw" ~~ ___}]
     = {True, True, False, False, True}

    >> StringFreeQ[{"A", "Galaxy", "Far", "Far", "Away"}, {"F" ~~ __ ~~ "r", "aw" ~~ ___}, IgnoreCase -> True]
     = {True, True, False, False, False}

    #> StringFreeQ[{"A", "Galaxy", "Far", "Far", "Away"}, {}]
     = {True, True, True, True, True}

    #> StringFreeQ[{"A", Galaxy, "Far", "Far", Away}, {"F" ~~ __ ~~ "r", "aw" ~~ ___}]
     : String or list of strings expected at position 1 in StringFreeQ[{A, Galaxy, Far, Far, Away}, {F ~~ __ ~~ r, aw ~~ ___}].
     = StringFreeQ[{A, Galaxy, Far, Far, Away}, {F ~~ __ ~~ r, aw ~~ ___}]

    #> StringFreeQ[{"A", "Galaxy", "Far", "Far", "Away"}, {F ~~ __ ~~ "r", aw ~~ ___}]
     : Element F ~~ __ ~~ r is not a valid string or pattern element in {F ~~ __ ~~ r, aw ~~ ___}.
     = StringFreeQ[{A, Galaxy, Far, Far, Away}, {F ~~ __ ~~ r, aw ~~ ___}]
    ## Mathematica can detemine correct invalid element in the pattern, it reports error:
    ## Element F is not a valid string or pattern element in {F ~~ __ ~~ r, aw ~~ ___}.
    """

    options = {
        "IgnoreCase": "False",
    }

    rules = {
        "StringFreeQ[patt_][expr_]": "StringFreeQ[expr, patt]",
    }

    messages = {
        "strse": "String or list of strings expected at position `1` in `2`.",
    }

    def apply(self, string, patt, evaluation, options):
        "StringFreeQ[string_, patt_, OptionsPattern[%(name)s]]"
        return _pattern_search(
            self.__class__.__name__, string, patt, evaluation, options, False
        )


class StringRiffle(Builtin):
    """
    <dl>
    <dt>'StringRiffle[{s1, s2, s3, ...}]'
      <dd>returns a new string by concatenating all the $si$, with spaces inserted between them.
    <dt>'StringRiffle[list, sep]'
      <dd>inserts the separator $sep$ between all elements in $list$.
    <dt>'StringRiffle[list, {"left", "sep", "right"}]'
      <dd>use $left$ and $right$ as delimiters after concatenation.

    ## These 2 forms are not currently implemented
    ## <dt>'StringRiffle[{{s11, s12, ...}, {s21, s22, ...}, ...}]'
    ##   <dd>returns a new string by concatenating the $sij$, and inserting spaces at the lowest level and newlines at the higher level.
    ## <dt>'StringRiffle[list, sep1, sep2, ...]'
    ##   <dd>inserts separator $sepi$ between elements of list at level i.
    </dl>

    >> StringRiffle[{"a", "b", "c", "d", "e"}]
     = a b c d e

    #> StringRiffle[{a, b, c, "d", e, "f"}]
     = a b c d e f

    ## 1st is not a list
    #> StringRiffle["abcdef"]
     : List expected at position 1 in StringRiffle[abcdef].
     : StringRiffle called with 1 argument; 2 or more arguments are expected.
     = StringRiffle[abcdef]

    #> StringRiffle[{"", "", ""}] // FullForm
     = "  "

    ## This form is not supported
    #> StringRiffle[{{"a", "b"}, {"c", "d"}}]
     : Sublist form in position 1 is is not implemented yet.
     = StringRiffle[{{a, b}, {c, d}}]

    >> StringRiffle[{"a", "b", "c", "d", "e"}, ", "]
     = a, b, c, d, e

    #> StringRiffle[{"a", "b", "c", "d", "e"}, sep]
     : String expected at position 2 in StringRiffle[{a, b, c, d, e}, sep].
     = StringRiffle[{a, b, c, d, e}, sep]

    >> StringRiffle[{"a", "b", "c", "d", "e"}, {"(", " ", ")"}]
     = (a b c d e)

    #> StringRiffle[{"a", "b", "c", "d", "e"}, {" ", ")"}]
     : String expected at position 2 in StringRiffle[{a, b, c, d, e}, { , )}].
     = StringRiffle[{a, b, c, d, e}, { , )}]
    #> StringRiffle[{"a", "b", "c", "d", "e"}, {left, " ", "."}]
     : String expected at position 2 in StringRiffle[{a, b, c, d, e}, {left,  , .}].
     = StringRiffle[{a, b, c, d, e}, {left,  , .}]

    ## This form is not supported
    #> StringRiffle[{"a", "b", "c"}, "+", "-"]
    ## Mathematica result: a+b+c, but we are not support multiple separators
     :  Multiple separators form is not implemented yet.
     = StringRiffle[{a, b, c}, +, -]
    """

    attributes = ("ReadProtected",)

    messages = {
        "list": "List expected at position `1` in `2`.",
        "argmu": "StringRiffle called with 1 argument; 2 or more arguments are expected.",
        "argm": "StringRiffle called with 0 arguments; 2 or more arguments are expected.",
        "string": "String expected at position `1` in `2`.",
        "sublist": "Sublist form in position 1 is is not implemented yet.",
        "mulsep": "Multiple separators form is not implemented yet.",
    }

    def apply(self, liststr, seps, evaluation):
        "StringRiffle[liststr_, seps___]"
        separators = seps.get_sequence()
        exp = (
            Expression("StringRiffle", liststr, seps)
            if separators
            else Expression("StringRiffle", liststr)
        )

        # Validate separators
        if len(separators) > 1:
            return evaluation.message("StringRiffle", "mulsep")
        elif len(separators) == 1:
            if separators[0].has_form("List", None):
                if len(separators[0].leaves) != 3 or any(
                    not isinstance(s, String) for s in separators[0].leaves
                ):
                    return evaluation.message("StringRiffle", "string", Integer(2), exp)
            elif not isinstance(separators[0], String):
                return evaluation.message("StringRiffle", "string", Integer(2), exp)

        # Validate list of string
        if not liststr.has_form("List", None):
            evaluation.message("StringRiffle", "list", Integer1, exp)
            return evaluation.message("StringRiffle", "argmu", exp)
        elif any(leaf.has_form("List", None) for leaf in liststr.leaves):
            return evaluation.message("StringRiffle", "sublist")

        # Determine the separation token
        left, right = "", ""
        if len(separators) == 0:
            sep = " "
        else:
            if separators[0].has_form("List", None):
                left = separators[0].leaves[0].value
                sep = separators[0].leaves[1].value
                right = separators[0].leaves[2].value
            else:
                sep = separators[0].get_string_value()

        # Getting all together
        result = left
        for i in range(len(liststr.leaves)):
            text = (
                liststr.leaves[i]
                .format(evaluation, "System`OutputForm")
                .boxes_to_text(evaluation=evaluation)
            )
            if i == len(liststr.leaves) - 1:
                result += text + right
            else:
                result += text + sep

        return String(result)
