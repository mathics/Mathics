# -*- coding: utf-8 -*-
"""
Characters in Strings
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin, Test

from mathics.core.expression import (
    Expression,
    String,
    SymbolList,
)


class Characters(Builtin):
    """
    <dl>
    <dt>'Characters["$string$"]'
        <dd>returns a list of the characters in $string$.
    </dl>

    >> Characters["abc"]
     = {a, b, c}

    #> \\.78\\.79\\.7A
     = xyz

    #> \\:0078\\:0079\\:007A
     = xyz

    #> \\101\\102\\103\\061\\062\\063
     = ABC123

    #> \\[Alpha]\\[Beta]\\[Gamma]
     = \u03B1\u03B2\u03B3
    """

    attributes = ("Listable",)

    def apply(self, string, evaluation):
        "Characters[string_String]"

        return Expression(SymbolList, *(String(c) for c in string.value))


class CharacterRange(Builtin):
    """
    <dl>
    <dt>'CharacterRange["$a$", "$b$"]'
        <dd>returns a list of the Unicode characters from $a$ to $b$
        inclusive.
    </dl>

    >> CharacterRange["a", "e"]
     = {a, b, c, d, e}
    >> CharacterRange["b", "a"]
     = {}
    """

    attributes = ("ReadProtected",)

    messages = {
        "argtype": "Arguments `1` and `2` are not both strings of length 1.",
    }

    def apply(self, start, stop, evaluation):
        "CharacterRange[start_String, stop_String]"

        if len(start.value) != 1 or len(stop.value) != 1:
            evaluation.message("CharacterRange", "argtype", start, stop)
            return
        start = ord(start.value[0])
        stop = ord(stop.value[0])
        return Expression(
            "List", *[String(chr(code)) for code in range(start, stop + 1)]
        )


class DigitQ(Builtin):
    """
    <dl>
    <dt>'DigitQ[$string$]'
        yields 'True' if all the characters in the $string$ are digits, and yields 'False' otherwise.
    </dl>

    >> DigitQ["9"]
     = True

    >> DigitQ["a"]
     = False

    >> DigitQ["01001101011000010111010001101000011010010110001101110011"]
     = True

    >> DigitQ["-123456789"]
     = False

    """

    rules = {
        "DigitQ[string_]": (
            "If[StringQ[string], StringMatchQ[string, DigitCharacter...], False, False]"
        ),
    }


class LetterQ(Builtin):
    """
    <dl>
    <dt>'LetterQ[$string$]'
        yields 'True' if all the characters in the $string$ are letters, and yields 'False' otherwise.
    </dl>

    >> LetterQ["m"]
     = True

    >> LetterQ["9"]
     = False

    >> LetterQ["Mathics"]
     = True

    >> LetterQ["Welcome to Mathics"]
     = False

    #> LetterQ[""]
     = True

    #> LetterQ["\\[Alpha]\\[Beta]\\[Gamma]\\[Delta]\\[Epsilon]\\[Zeta]\\[Eta]\\[Theta]"]
     = True
    """

    rules = {
        "LetterQ[string_]": (
            "If[StringQ[string], StringMatchQ[string, LetterCharacter...], False, False]"
        ),
    }


class LowerCaseQ(Test):
    """
    <dl>
    <dt>'LowerCaseQ[$s$]'
        <dd>returns True if $s$ consists wholly of lower case characters.
    </dl>

    >> LowerCaseQ["abc"]
     = True

    An empty string returns True.
    >> LowerCaseQ[""]
     = True
    """

    def test(self, s):
        return isinstance(s, String) and all(c.islower() for c in s.get_string_value())


class ToLowerCase(Builtin):
    """
    <dl>
    <dt>'ToLowerCase[$s$]'
        <dd>returns $s$ in all lower case.
    </dl>

    >> ToLowerCase["New York"]
     = new york
    """

    attributes = ("Listable", "Protected")

    def apply(self, s, evaluation):
        "ToLowerCase[s_String]"
        return String(s.get_string_value().lower())


class ToUpperCase(Builtin):
    """
    <dl>
    <dt>'ToUpperCase[$s$]'
        <dd>returns $s$ in all upper case.
    </dl>

    >> ToUpperCase["New York"]
     = NEW YORK
    """

    attributes = ("Listable", "Protected")

    def apply(self, s, evaluation):
        "ToUpperCase[s_String]"
        return String(s.get_string_value().upper())


class UpperCaseQ(Test):
    """
    <dl>
    <dt>'UpperCaseQ[$s$]'
        <dd>returns True if $s$ consists wholly of upper case characters.
    </dl>

    >> UpperCaseQ["ABC"]
     = True

    An empty string returns True.
    >> UpperCaseQ[""]
     = True
    """

    def test(self, s):
        return isinstance(s, String) and all(c.isupper() for c in s.get_string_value())
