# -*- coding: utf-8 -*-
"""
String Patterns
"""

import re

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin
from mathics.core.expression import (
    Expression,
    Integer1,
    SymbolFalse,
    SymbolTrue,
    )


from mathics.builtin.strings import (
    anchor_pattern,
    to_regex,
)

class StringMatchQ(Builtin):
    r"""
    >> StringMatchQ["abc", "abc"]
     = True

    >> StringMatchQ["abc", "abd"]
     = False

    >> StringMatchQ["15a94xcZ6", (DigitCharacter | LetterCharacter)..]
     = True

    #> StringMatchQ["abc1", LetterCharacter]
     = False

    #> StringMatchQ["abc", "ABC"]
     = False
    #> StringMatchQ["abc", "ABC", IgnoreCase -> True]
     = True

    ## Words containing nonword characters
    #> StringMatchQ[{"monkey", "don't", "AAA", "S&P"}, ___ ~~ Except[WordCharacter] ~~ ___]
     = {False, True, False, True}

    ## Try to match a literal number
    #> StringMatchQ[1.5, NumberString]
     : String or list of strings expected at position 1 in StringMatchQ[1.5, NumberString].
     = StringMatchQ[1.5, NumberString]

    Use StringMatchQ as an operator
    >> StringMatchQ[LetterCharacter]["a"]
     = True

    ## Abbreviated string patterns Issue #517
    #> StringMatchQ["abcd", "abc*"]
     = True
    #> StringMatchQ["abc", "abc*"]
     = True
    #> StringMatchQ["abc\\", "abc\\"]
     = True
    #> StringMatchQ["abc*d", "abc\\*d"]
     = True
    #> StringMatchQ["abc*d", "abc\\**"]
     = True
    #> StringMatchQ["abcde", "a*f"]
     = False

    #> StringMatchQ["abcde", "a@e"]
     = True
    #> StringMatchQ["aBCDe", "a@e"]
     = False
    #> StringMatchQ["ae", "a@e"]
     = False
    """

    attributes = ("Listable",)

    options = {
        "IgnoreCase": "False",
        "SpellingCorrections": "None",
    }

    messages = {
        "strse": "String or list of strings expected at position `1` in `2`.",
    }

    rules = {
        "StringMatchQ[patt_][expr_]": "StringMatchQ[expr, patt]",
    }

    def apply(self, string, patt, evaluation, options):
        "StringMatchQ[string_, patt_, OptionsPattern[%(name)s]]"
        py_string = string.get_string_value()
        if py_string is None:
            return evaluation.message(
                "StringMatchQ",
                "strse",
                Integer1,
                Expression("StringMatchQ", string, patt),
            )

        re_patt = to_regex(patt, evaluation, abbreviated_patterns=True)
        if re_patt is None:
            return evaluation.message(
                "StringExpression", "invld", patt, Expression("StringExpression", patt)
            )

        re_patt = anchor_pattern(re_patt)

        flags = re.MULTILINE
        if options["System`IgnoreCase"] == SymbolTrue:
            flags = flags | re.IGNORECASE

        if re.match(re_patt, py_string, flags=flags) is None:
            return SymbolFalse
        else:
            return SymbolTrue
