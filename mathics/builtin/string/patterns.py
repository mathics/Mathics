# -*- coding: utf-8 -*-
"""
String Patterns
"""

import re

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import BinaryOperator, Builtin

from mathics.core.expression import (
    Expression,
    Integer1,
    String,
    SymbolFalse,
    SymbolList,
    SymbolTrue,
)


from mathics.builtin.strings import (
    _StringFind,
    _evaluate_match,
    _pattern_search,
    _parallel_match,
    anchor_pattern,
    to_regex,
)


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


# strings.to_regex() seems to have the implementation here.
class WordBoundary(Builtin):
    """
    <dl>
      <dt>'WordBoundary'
      <dd>represents the boundary between words.
    </dl>

    >> StringReplace["apple banana orange artichoke", "e" ~~ WordBoundary -> "E"]
     = applE banana orangE artichokE
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
