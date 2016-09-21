#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
String functions
"""

from __future__ import unicode_literals
from __future__ import absolute_import

import sys
import re
import unicodedata
from binascii import hexlify, unhexlify
from heapq import heappush, heappop

import six
from six.moves import range
from six import unichr

from mathics.builtin.base import BinaryOperator, Builtin, Test
from mathics.core.expression import (Expression, Symbol, String, Integer,
                                     from_python)
from mathics.builtin.lists import python_seq, convert_seq


_regex_longest = {
    '+': '+',
    '*': '*',
}

_regex_shortest = {
    '+': '+?',
    '*': '*?',
}


def _encode_pname(name):
    return 'n' + hexlify(name.encode('utf8')).decode('utf8')


def _decode_pname(name):
    return unhexlify(name[1:]).decode('utf8')


def _evaluate_match(s, m, evaluation):
    replace = dict((_decode_pname(name), String(value)) for name, value in m.groupdict().items())
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


def to_regex(expr, q=_regex_longest, abbreviated_patterns=False):
    if expr is None:
        return None

    if isinstance(expr, String):
        result = expr.get_string_value()
        if abbreviated_patterns:
            pieces = []
            i, j = 0, 0
            while j < len(result):
                c = result[j]
                if c == '\\' and j + 1 < len(result):
                    pieces.append(re.escape(result[i:j]))
                    pieces.append(re.escape(result[j+1]))
                    j += 2
                    i = j
                elif c == '*':
                    pieces.append(re.escape(result[i:j]))
                    pieces.append('(.*)')
                    j += 1
                    i = j
                elif c == '@':
                    pieces.append(re.escape(result[i:j]))
                    # one or more characters, excluding uppercase letters
                    pieces.append('([^A-Z]+)')
                    j += 1
                    i = j
                else:
                    j += 1
            pieces.append(re.escape(result[i:j]))
            result = ''.join(pieces)
        else:
            result = re.escape(result)
        return result
    if expr.has_form('RegularExpression', 1):
        regex = expr.leaves[0].get_string_value()
        if regex is None:
            return regex
        try:
            re.compile(regex)
            # Don't return the compiled regex because it may need to composed
            # further e.g. StringExpression["abc", RegularExpression[regex2]].
            return regex
        except re.error:
            return None     # invalid regex

    if isinstance(expr, Symbol):
        return {
            'System`NumberString': r'[-|+]?(\d+(\.\d*)?|\.\d+)?',
            'System`Whitespace': r'\s+',
            'System`DigitCharacter': r'\d',
            'System`WhitespaceCharacter': r'(?u)\s',
            'System`WordCharacter': r'(?u)[^\W_]',
            'System`StartOfLine': r'^',
            'System`EndOfLine': r'$',
            'System`StartOfString': r'\A',
            'System`EndOfString': r'\Z',
            'System`WordBoundary': r'\b',
            'System`LetterCharacter': r'(?u)[^\W_0-9]',
            'System`HexidecimalCharacter': r'[0-9a-fA-F]',
        }.get(expr.get_name())

    if expr.has_form('CharacterRange', 2):
        (start, stop) = (leaf.get_string_value() for leaf in expr.leaves)
        if all(x is not None and len(x) == 1 for x in (start, stop)):
            return "[{0}-{1}]".format(re.escape(start), re.escape(stop))

    if expr.has_form('Blank', 0):
        return r'(.|\n)'
    if expr.has_form('BlankSequence', 0):
        return r'(.|\n)' + q['+']
    if expr.has_form('BlankNullSequence', 0):
        return r'(.|\n)' + q['*']
    if expr.has_form('Except', 1, 2):
        if len(expr.leaves) == 1:
            leaves = [expr.leaves[0], Expression('Blank')]
        else:
            leaves = [expr.leaves[0], expr.leaves[1]]
        leaves = [to_regex(leaf, q) for leaf in leaves]
        if all(leaf is not None for leaf in leaves):
            return '(?!{0}){1}'.format(*leaves)
    if expr.has_form('Characters', 1):
        leaf = expr.leaves[0].get_string_value()
        if leaf is not None:
            return '[{0}]'.format(re.escape(leaf))
    if expr.has_form('StringExpression', None):
        leaves = [to_regex(leaf, q) for leaf in expr.leaves]
        if None in leaves:
            return None
        return "".join(leaves)
    if expr.has_form('Repeated', 1):
        leaf = to_regex(expr.leaves[0], q)
        if leaf is not None:
            return '({0})'.format(leaf) + q['+']
    if expr.has_form('RepeatedNull', 1):
        leaf = to_regex(expr.leaves[0], q)
        if leaf is not None:
            return '({0})'.format(leaf) + q['*']
    if expr.has_form('Alternatives', None):
        leaves = [to_regex(leaf, q) for leaf in expr.leaves]
        if all(leaf is not None for leaf in leaves):
            return "|".join(leaves)
    if expr.has_form('Shortest', 1):
        return to_regex(expr.leaves[0], _regex_shortest)
    if expr.has_form('Longest', 1):
        return to_regex(expr.leaves[0], _regex_longest)
    if expr.has_form('Pattern', 2) and isinstance(expr.leaves[0], Symbol):
        return '(?P<%s>%s)' % (
            _encode_pname(expr.leaves[0].get_name()),
            to_regex(expr.leaves[1], q))

    return None


def anchor_pattern(patt):
    '''
    anchors a regex in order to force matching against an entire string.
    '''
    if not patt.endswith(r'\Z'):
        patt = patt + r'\Z'
    if not patt.startswith(r'\A'):
        patt = r'\A' + patt
    return patt


def mathics_split(patt, string, flags):
    '''
    Python's re.split includes the text of groups if they are capturing.

    Furthermore, you can't split on empty matches. Trying to do this returns
    the original string for Python < 3.5, raises a ValueError for
    Python >= 3.5, <= X and works as expected for Python >= X, where 'X' is
    some future version of Python (> 3.6).

    For these reasons we implement our own split.
    '''
    # (start, end) indices of splits
    indices = list((m.start(), m.end()) for m in re.finditer(patt, string, flags))

    # (start, end) indices of stuff to keep
    indices = [(None, 0)] + indices + [(len(string), None)]
    indices = [(indices[i][1], indices[i + 1][0]) for i in range(len(indices) - 1)]

    # slice up the string
    return [string[start:stop] for start, stop in indices]


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

    operator = '~~'
    precedence = 135
    attributes = ('Flat', 'OneIdentity', 'Protected')

    messages = {
        'invld': 'Element `1` is not a valid string or pattern element in `2`.',
    }

    def apply(self, args, evaluation):
        'StringExpression[args__String]'
        args = args.get_sequence()
        args = [arg.get_string_value() for arg in args]
        if None in args:
            return
        return String(''.join(args))


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

    #> StringMatchQ[".12", NumberString]
     = True
    #> StringMatchQ["12.", NumberString]
     = True
    #> StringMatchQ["12.31.31", NumberString]
     = False
    #> StringMatchQ[".", NumberString]
     = False
    #> StringMatchQ["-1.23", NumberString]
     = True
    #> StringMatchQ["+12.3", NumberString]
     = True
    #> StringMatchQ["+.2", NumberString]
     = True
    #> StringMatchQ["1.2e4", NumberString]
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
      <dd>represents the letters a-z and A-Z.
    </dl>

    >> StringMatchQ[#, LetterCharacter] & /@ {"a", "1", "A", " ", "."}
     = {True, False, True, False, False}

    LetterCharacter does not match unicode characters.
    >> StringMatchQ["\[Lambda]", LetterCharacter]
     = False
    """


class HexidecimalCharacter(Builtin):
    """
    <dl>
    <dt>'HexidecimalCharacter'
      <dd>represents the characters 0-9, a-f and A-F.
    </dl>

    >> StringMatchQ[#, HexidecimalCharacter] & /@ {"a", "1", "A", "x", "H", " ", "."}
     = {True, True, True, False, False, False, False}
    """


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

    attributes = ('Listable',)

    options = {
        'IgnoreCase': 'False',
        'SpellingCorrections': 'None',
    }

    messages = {
        'strse': 'String or list of strings expected at position `1` in `2`.',
    }

    rules = {
        'StringMatchQ[patt_][expr_]': 'StringMatchQ[expr, patt]',
    }

    def apply(self, string, patt, evaluation, options):
        'StringMatchQ[string_, patt_, OptionsPattern[%(name)s]]'
        py_string = string.get_string_value()
        if py_string is None:
            return evaluation.message('StringMatchQ', 'strse', Integer(1),
                                      Expression('StringMatchQ', string, patt))

        re_patt = to_regex(patt, abbreviated_patterns=True)
        if re_patt is None:
            return evaluation.message('StringExpression', 'invld', patt,
                                      Expression('StringExpression', patt))

        re_patt = anchor_pattern(re_patt)

        flags = re.MULTILINE
        if options['System`IgnoreCase'] == Symbol('True'):
            flags = flags | re.IGNORECASE

        if re.match(re_patt, py_string, flags=flags) is None:
            return Symbol('False')
        else:
            return Symbol('True')


class StringJoin(BinaryOperator):
    """
    <dl>
    <dt>'StringJoin["$s1$", "$s2$", ...]'
        <dd>returns the concatenation of the strings $s1$, $s2$, ….
    </dl>

    >> StringJoin["a", "b", "c"]
     = abc
    >> "a" <> "b" <> "c" // InputForm
     = "abc"

    'StringJoin' flattens lists out:
    >> StringJoin[{"a", "b"}] // InputForm
     = "ab"
    >> Print[StringJoin[{"Hello", " ", {"world"}}, "!"]]
     | Hello world!
    """

    operator = '<>'
    precedence = 600
    attributes = ('Flat', 'OneIdentity')

    def apply(self, items, evaluation):
        'StringJoin[items___]'

        result = ''
        items = items.flatten(Symbol('List'))
        if items.get_head_name() == 'System`List':
            items = items.leaves
        else:
            items = items.get_sequence()
        for item in items:
            if not isinstance(item, String):
                evaluation.message('StringJoin', 'string')
                return
            result += item.value
        return String(result)


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
        'StringSplit[s_]': 'StringSplit[s, Whitespace]',
    }

    options = {
        'IgnoreCase': 'False',
        'MetaCharacters': 'None',
    }

    messages = {
        'strse': 'String or list of strings expected at position `1` in `2`.',
        'pysplit': 'As of Python 3.5 re.split does not handle empty pattern matches.',
    }

    def apply(self, string, patt, evaluation, options):
        'StringSplit[string_, patt_, OptionsPattern[%(name)s]]'
        py_string = string.get_string_value()

        if py_string is None:
            return evaluation.message('StringSplit', 'strse', Integer(1),
                                      Expression('StringSplit', string))

        if patt.has_form('List', None):
            patts = patt.get_leaves()
        else:
            patts = [patt]
        re_patts = []
        for p in patts:
            py_p = to_regex(p)
            if py_p is None:
                return evaluation.message('StringExpression', 'invld', p, patt)
            re_patts.append(py_p)

        flags = re.MULTILINE
        if options['System`IgnoreCase'] == Symbol('True'):
            flags = flags | re.IGNORECASE

        result = [py_string]
        for re_patt in re_patts:
            result = [t for s in result for t in mathics_split(re_patt, s, flags=flags)]

        return Expression('List', *[String(x) for x in result if x != ''])


class StringPosition(Builtin):
    '''
    <dl>
    <dt>'StringPosition["$string$", $patt$]'
      <dd>gives a list of starting and ending positions where $patt$ matches "$string$".
    <dt>'StringPosition["$string$", $patt$, $n$]'
      <dd>returns the first $n$ matches only.
    <dt>'StringPosition["$string$", {$patt1$, $patt2$, ...}, $n$]'
      <dd>matches multiple patterns.
    <dt>'StringPosition[{$s1$, $s2$, ...}, $patt$]'
      <dd>returns a list of matches for multiple strings.
    </dl>

    >> StringPosition["123ABCxyABCzzzABCABC", "ABC"]
     = {{4, 6}, {9, 11}, {15, 17}, {18, 20}}

    >> StringPosition["123ABCxyABCzzzABCABC", "ABC", 2]
     = {{4, 6}, {9, 11}}

    'StringPosition' can be useful for searching through text.
    >> data = Import["ExampleData/EinsteinSzilLetter.txt"];
    >> StringPosition[data, "uranium"]
     = {{299, 305}, {870, 876}, {1538, 1544}, {1671, 1677}, {2300, 2306}, {2784, 2790}, {3093, 3099}}

    #> StringPosition["123ABCxyABCzzzABCABC", "ABC", -1]
     : Non-negative integer or Infinity expected at position 3 in StringPosition[123ABCxyABCzzzABCABC, ABC, -1].
     = StringPosition[123ABCxyABCzzzABCABC, ABC, -1]

    ## Overlaps
    #> StringPosition["1231221312112332", RegularExpression["[12]+"]]
     = {{1, 2}, {2, 2}, {4, 7}, {5, 7}, {6, 7}, {7, 7}, {9, 13}, {10, 13}, {11, 13}, {12, 13}, {13, 13}, {16, 16}}
    #> StringPosition["1231221312112332", RegularExpression["[12]+"], Overlaps -> False]
     = {{1, 2}, {4, 7}, {9, 13}, {16, 16}}
    #> StringPosition["1231221312112332", RegularExpression["[12]+"], Overlaps -> x]
     = {{1, 2}, {4, 7}, {9, 13}, {16, 16}}
    #> StringPosition["1231221312112332", RegularExpression["[12]+"], Overlaps -> All]
     : Overlaps -> All option is not currently implemented in Mathics.
     = {{1, 2}, {2, 2}, {4, 7}, {5, 7}, {6, 7}, {7, 7}, {9, 13}, {10, 13}, {11, 13}, {12, 13}, {13, 13}, {16, 16}}

    #> StringPosition["21211121122", {"121", "11"}]
     = {{2, 4}, {4, 5}, {5, 6}, {6, 8}, {8, 9}}
    #> StringPosition["21211121122", {"121", "11"}, Overlaps -> False]
     = {{2, 4}, {5, 6}, {8, 9}}

    #> StringPosition[{"abc", "abcda"}, "a"]
     = {{{1, 1}}, {{1, 1}, {5, 5}}}

    #> StringPosition[{"abc"}, "a", Infinity]
     = {{{1, 1}}}

    #> StringPosition["abc"]["123AabcDEabc"]
     = {{5, 7}, {10, 12}}
    '''

    options = {
        'IgnoreCase': 'False',
        'MetaCharacters': 'None',
        'Overlaps': 'True',
    }

    messages = {
        'strse': 'String or list of strings expected at position `1` in `2`.',
        'overall': 'Overlaps -> All option is not currently implemented in Mathics.',
        'innf': 'Non-negative integer or Infinity expected at position `2` in `1`.',
    }

    rules = {
        'StringPosition[patt_][s_]': 'StringPosition[s, patt]',
    }

    def apply(self, string, patt, evaluation, options):
        'StringPosition[string_, patt_, OptionsPattern[StringPosition]]'

        return self.apply_n(string, patt, Expression('DirectedInfinity', Integer(1)), evaluation, options)

    def apply_n(self, string, patt, n, evaluation, options):
        'StringPosition[string_, patt_, n:(_Integer|DirectedInfinity[1]), OptionsPattern[StringPosition]]'

        expr = Expression('StringPosition', string, patt, n)

        # check n
        if n.has_form('DirectedInfinity', 1):
            py_n = float('inf')
        else:
            py_n = n.get_int_value()
            if py_n is None or py_n < 0:
                return evaluation.message('StringPosition', 'innf', expr, Integer(3))

        # check options
        if options['System`Overlaps'] == Symbol('True'):
            overlap = True
        elif options['System`Overlaps'] == Symbol('False'):
            overlap = False
        elif options['System`Overlaps'] == Symbol('All'):
            # TODO
            evaluation.message('StringPosition', 'overall')
            overlap = True
        else:
            overlap = False    # unknown options are teated as False

        # convert patterns
        if patt.has_form('List', None):
            patts = patt.get_leaves()
        else:
            patts = [patt]
        re_patts = []
        for p in patts:
            py_p = to_regex(p)
            if py_p is None:
                return evaluation.message('StringExpression', 'invld', p, patt)
            re_patts.append(py_p)
        compiled_patts = [re.compile(re_patt) for re_patt in re_patts]

        # string or list of strings
        if string.has_form('List', None):
            py_strings = [s.get_string_value() for s in string.leaves]
            if None in py_strings:
                return
            results = [self.do_apply(py_string, compiled_patts, py_n, overlap) for py_string in py_strings]
            return Expression('List', *results)
        else:
            py_string = string.get_string_value()
            if py_string is None:
                return
            return self.do_apply(py_string, compiled_patts, py_n, overlap)

    @staticmethod
    def do_apply(py_string, compiled_patts, py_n, overlap):
        result = []
        start = 0
        while start < len(py_string):
            found_match = False
            for compiled_patt in compiled_patts:
                m = compiled_patt.match(py_string, start)
                if m is None:
                    continue
                found_match = True
                result.append([m.start() + 1, m.end()])    # 0 to 1 based indexing
                if len(result) == py_n:
                    return from_python(result)
                if not overlap:
                    start = m.end()
            if overlap or not found_match:
                start += 1
        return from_python(result)


class StringLength(Builtin):
    """
    <dl>
    <dt>'StringLength["$string$"]'
        <dd>gives the length of $string$.
    </dl>

    >> StringLength["abc"]
     = 3
    'StringLength' is listable:
    >> StringLength[{"a", "bc"}]
     = {1, 2}

    >> StringLength[x]
     : String expected.
     = StringLength[x]
    """

    attributes = ('Listable',)

    def apply(self, str, evaluation):
        'StringLength[str_]'

        if not isinstance(str, String):
            evaluation.message('StringLength', 'string')
            return
        return Integer(len(str.value))


class _StringFind(Builtin):
    attributes = ('Protected')

    options = {
        'IgnoreCase': 'False',
        'MetaCharacters': 'None',
    }

    messages = {
        'strse': 'String or list of strings expected at position `1` in `2`.',
        'srep': '`1` is not a valid string replacement rule.',
        'innf': ('Non-negative integer or Infinity expected at '
                 'position `1` in `2`.'),
    }

    def _find(py_stri, py_rules, py_n, flags):
        raise NotImplementedError()

    def _apply(self, string, rule, n, evaluation, options, cases):
        if n.same(Symbol('System`Private`Null')):
            expr = Expression(self.get_name(), string, rule)
            n = None
        else:
            expr = Expression(self.get_name(), string, rule, n)

        # convert string
        if string.has_form('List', None):
            py_strings = [stri.get_string_value() for stri in string.leaves]
            if None in py_strings:
                return evaluation.message(
                    self.get_name(), 'strse', Integer(1), expr)
        else:
            py_strings = string.get_string_value()
            if py_strings is None:
                return evaluation.message(
                    self.get_name(), 'strse', Integer(1), expr)

        # convert rule
        def convert_rule(r):
            if r.has_form('Rule', None) and len(r.leaves) == 2:
                py_s = to_regex(r.leaves[0])
                if py_s is None:
                    return evaluation.message(
                        'StringExpression', 'invld', r.leaves[0], r.leaves[0])
                py_sp = r.leaves[1]
                return py_s, py_sp
            elif cases:
                py_s = to_regex(r)
                if py_s is None:
                    return evaluation.message('StringExpression', 'invld', r, r)
                return py_s, None

            return evaluation.message(self.get_name(), 'srep', r)

        if rule.has_form('List', None):
            py_rules = [convert_rule(r) for r in rule.leaves]
        else:
            py_rules = [convert_rule(rule)]
        if None in py_rules:
            return None

        # convert n
        if n is None:
            py_n = 0
        elif n == Expression('DirectedInfinity', Integer(1)):
            py_n = 0
        else:
            py_n = n.get_int_value()
            if py_n is None or py_n < 0:
                return evaluation.message(self.get_name(), 'innf', Integer(3), expr)

        # flags
        flags = re.MULTILINE
        if options['System`IgnoreCase'] == Symbol('True'):
            flags = flags | re.IGNORECASE

        if isinstance(py_strings, list):
            return Expression(
                'List', *[self._find(py_stri, py_rules, py_n, flags, evaluation) for py_stri in py_strings])
        else:
            return self._find(py_strings, py_rules, py_n, flags, evaluation)


class StringReplace(_StringFind):
    """
    <dl>
    <dt>'StringReplace["$string$", "$a$"->"$b$"]'
        <dd>replaces each occurrence of $old$ with $new$ in $string$.
    <dt>'StringReplace["$string$", {"$s1$"->"$sp1$", "$s2$"->"$sp2$"}]'
        <dd>performs multiple replacements of each $si$ by the
        corresponding $spi$ in $string$.
    <dt>'StringReplace["$string$", $srules$, $n$]'
        <dd>only performs the first $n$ replacements.
    <dt>'StringReplace[{"$string1$", "$string2$", ...}, $srules$]'
        <dd>performs the replacements specified by $srules$ on a list
        of strings.
    </dl>

    StringReplace replaces all occurrences of one substring with another:
    >> StringReplace["xyxyxyyyxxxyyxy", "xy" -> "A"]
     = AAAyyxxAyA

    Multiple replacements can be supplied:
    >> StringReplace["xyzwxyzwxxyzxyzw", {"xyz" -> "A", "w" -> "BCD"}]
     = ABCDABCDxAABCD

    Only replace the first 2 occurences:
    >> StringReplace["xyxyxyyyxxxyyxy", "xy" -> "A", 2]
     = AAxyyyxxxyyxy

    Also works for multiple rules:
    >> StringReplace["abba", {"a" -> "A", "b" -> "B"}, 2]
     = ABba

    StringReplace acts on lists of strings too:
    >> StringReplace[{"xyxyxxy", "yxyxyxxxyyxy"}, "xy" -> "A"]
     = {AAxA, yAAxxAyA}

    #> StringReplace["abcabc", "a" -> "b", Infinity]
     = bbcbbc
    #> StringReplace[x, "a" -> "b"]
     : String or list of strings expected at position 1 in StringReplace[x, a -> b].
     = StringReplace[x, a -> b]
    #> StringReplace["xyzwxyzwaxyzxyzw", x]
     : x is not a valid string replacement rule.
     = StringReplace[xyzwxyzwaxyzxyzw, x]
    #> StringReplace["xyzwxyzwaxyzxyzw", x -> y]
     : Element x is not a valid string or pattern element in x.
     = StringReplace[xyzwxyzwaxyzxyzw, x -> y]
    #> StringReplace["abcabc", "a" -> "b", -1]
     : Non-negative integer or Infinity expected at position 3 in StringReplace[abcabc, a -> b, -1].
     = StringReplace[abcabc, a -> b, -1]
    #> StringReplace["abc", "b" -> 4]
     : String expected.
     = a <> 4 <> c

    #> StringReplace["01101100010", "01" .. -> "x"]
     = x1x100x0

    #> StringReplace["abc abcb abdc", "ab" ~~ _ -> "X"]
     = X Xb Xc

    #> StringReplace["abc abcd abcd",  WordBoundary ~~ "abc" ~~ WordBoundary -> "XX"]
     = XX abcd abcd

    #> StringReplace["abcd acbd", RegularExpression["[ab]"] -> "XX"]
     = XXXXcd XXcXXd

    #> StringReplace["abcd acbd", RegularExpression["[ab]"] ~~ _ -> "YY"]
     = YYcd YYYY

    #> StringReplace["abcdabcdaabcabcd", {"abc" -> "Y", "d" -> "XXX"}]
     = YXXXYXXXaYYXXX


    #> StringReplace["  Have a nice day.  ", (StartOfString ~~ Whitespace) | (Whitespace ~~ EndOfString) -> ""] // FullForm
     = "Have a nice day."

    #> StringReplace["xyXY", "xy" -> "01"]
     = 01XY
    #> StringReplace["xyXY", "xy" -> "01", IgnoreCase -> True]
     = 0101

    StringReplace also can be used as an operator:
    >> StringReplace["y" -> "ies"]["city"]
     = cities
    """

    # TODO Special Characters
    """
    #> StringReplace["product: A \[CirclePlus] B" , "\[CirclePlus]" -> "x"]
     = A x B
    """

    rules = {
        'StringReplace[rule_][string_]': 'StringReplace[string, rule]',
    }

    def _find(self, py_stri, py_rules, py_n, flags, evaluation):
        def cases():
            k = 0
            for match, form in _parallel_match(py_stri, py_rules, flags, py_n):
                start, end = match.span()
                if start > k:
                    yield String(py_stri[k:start])
                yield _evaluate_match(form, match, evaluation)
                k = end
            if k < len(py_stri):
                yield String(py_stri[k:])

        return Expression('StringJoin', *list(cases()))

    def apply(self, string, rule, n, evaluation, options):
        '%(name)s[string_, rule_, OptionsPattern[%(name)s], n_:System`Private`Null]'
        # this pattern is a slight hack to get around missing Shortest/Longest.
        return self._apply(string, rule, n, evaluation, options, False)


class StringCases(_StringFind):
    '''
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
     = {"axb"}

    >> StringCases["axbaxxb", "a" ~~ x__ ~~ "b"]
     = {"axbaxxb"}

    >> StringCases["axbaxxb", Shortest["a" ~~ x__ ~~ "b"]]
     = {"axb", "axxb"}

    >> StringCases["-abc- def -uvw- xyz", Shortest["-" ~~ x__ ~~ "-"] -> x]
     = {abc, uvw}

    >> StringCases["-öhi- -abc- -.-", "-" ~~ x : WordCharacter .. ~~ "-" -> x]
     = {"öhi", "abc"}

    >> StringCases["abba", {"a" -> 10, "b" -> 20}, 2]
     = {10, 20}

    >> StringCases["a#ä_123", WordCharacter]
     = {a, ä, 1, 2, 3}

    >> StringCases["a#ä_123", LetterCharacter]
     = {a, ä}
    '''

    rules = {
        'StringCases[rule_][string_]': 'StringCases[string, rule]',
    }

    def _find(self, py_stri, py_rules, py_n, flags, evaluation):
        def cases():
            for match, form in _parallel_match(py_stri, py_rules, flags, py_n):
                if form is None:
                    yield String(match.group(0))
                else:
                    yield _evaluate_match(form, match, evaluation)

        return Expression('List', *list(cases()))

    def apply(self, string, rule, n, evaluation, options):
        '%(name)s[string_, rule_, OptionsPattern[%(name)s], n_:System`Private`Null]'
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
    """

    messages = {
        'intp': 'A positive integer is expected at position `1` in `2`.',
    }

    def apply(self, s, n, evaluation):
        'StringRepeat[s_String, n_]'
        py_n = n.get_int_value() if isinstance(n, Integer) else 0
        if py_n < 1:
            evaluation.message('StringRepeat', 'intp', 2, Expression('StringRepeat', s, n))
        return String(s.get_string_value() * py_n)

    def apply_truncated(self, s, n, m, evaluation):
        'StringRepeat[s_String, n_Integer, m_Integer]'
        py_n = n.get_int_value() if isinstance(n, Integer) else 0
        py_m = m.get_int_value() if isinstance(m, Integer) else 0

        if py_n < 1:
            evaluation.message('StringRepeat', 'intp', 2, Expression('StringRepeat', s, n, m))
        if py_m < 1:
            evaluation.message('StringRepeat', 'intp', 3, Expression('StringRepeat', s, n, m))

        py_s = s.get_string_value()
        py_n = min(1 + py_m // len(py_s), py_n)

        return String((py_s * py_n)[:py_m])


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

    attributes = ('Listable',)

    def apply(self, string, evaluation):
        'Characters[string_String]'

        return Expression('List', *(String(c) for c in string.value))


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

    attributes = ('ReadProtected',)

    messages = {
        'argtype': "Arguments `1` and `2` are not both strings of length 1.",
    }

    def apply(self, start, stop, evaluation):
        'CharacterRange[start_String, stop_String]'

        if len(start.value) != 1 or len(stop.value) != 1:
            evaluation.message('CharacterRange', 'argtype', start, stop)
            return
        start = ord(start.value[0])
        stop = ord(stop.value[0])
        return Expression('List', *[
            String(unichr(code)) for code in range(start, stop + 1)])


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

    name = 'String'


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

    def apply(self, s, evaluation):
        'ToLowerCase[s_String]'
        return String(s.get_string_value().lower())


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


class ToUpperCase(Builtin):
    """
    <dl>
    <dt>'ToUpperCase[$s$]'
        <dd>returns $s$ in all upper case.
    </dl>

    >> ToUpperCase["New York"]
     = NEW YORK
    """

    def apply(self, s, evaluation):
        'ToUpperCase[s_String]'
        return String(s.get_string_value().upper())


class ToString(Builtin):
    """
    <dl>
    <dt>'ToString[$expr$]'
        <dd>returns a string representation of $expr$.
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
    """

    def apply(self, value, evaluation):
        'ToString[value_]'

        text = value.format(evaluation, 'System`OutputForm').boxes_to_text(
            evaluation=evaluation)
        return String(text)


class ToExpression(Builtin):
    """
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

    #> ToExpression["log(x)", InputForm]
     = log x

    #> ToExpression["1+"]
     : Incomplete expression; more input is needed (line 1 of "").
     = $Failed

    #> ToExpression[]
     : ToExpression called with 0 arguments; between 1 and 3 arguments are expected.
     = ToExpression[]
    """

    # TODO: Other forms
    """
    >> ToExpression["log(x)", TraditionalForm]
     = Log[x]
    #> ToExpression["log(x)", StandardForm]
     = log x
    """

    attributes = ('Listable', 'Protected')

    messages = {
        'argb': ('`1` called with `2` arguments; '
                 'between `3` and `4` arguments are expected.'),
        'interpfmt': ('`1` is not a valid interpretation format. '
                      'Valid interpretation formats include InputForm '
                      'and any member of $BoxForms.'),
        'notstr': 'The format type `1` is valid only for string input.',
    }

    def apply(self, seq, evaluation):
        'ToExpression[seq__]'

        # Organise Arguments
        py_seq = seq.get_sequence()
        if len(py_seq) == 1:
            (inp, form, head) = (py_seq[0], Symbol('InputForm'), None)
        elif len(py_seq) == 2:
            (inp, form, head) = (py_seq[0], py_seq[1], None)
        elif len(py_seq) == 3:
            (inp, form, head) = (py_seq[0], py_seq[1], py_seq[2])
        else:
            assert len(py_seq) > 3  # 0 case handled by apply_empty
            evaluation.message('ToExpression', 'argb', 'ToExpression',
                               Integer(len(py_seq)), Integer(1), Integer(3))
            return

        # Apply the differnet forms
        if form == Symbol('InputForm'):
            if isinstance(inp, String):
                result = evaluation.parse(inp.get_string_value())
                if result is None:
                    return Symbol('$Failed')
            else:
                result = inp
        else:
            evaluation.message('ToExpression', 'interpfmt', form)
            return

        # Apply head if present
        if head is not None:
            result = Expression(head, result).evaluate(evaluation)

        return result

    def apply_empty(self, evaluation):
        'ToExpression[]'
        evaluation.message('ToExpression', 'argb', 'ToExpression',
                           Integer(0), Integer(1), Integer(3))
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

    >> ToCharacterCode["\[Alpha]\[Beta]\[Gamma]"]
     = {945, 946, 947}

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
        'strse': 'String or list of strings expected at position `1` in `2`.',
    }

    # TODO encoding

    def apply(self, string, evaluation):
        "ToCharacterCode[string_]"

        exp = Expression('ToCharacterCode', string)

        if string.has_form('List', None):
            string = [substring.get_string_value()
                      for substring in string.leaves]
            if any(substring is None for substring in string):
                evaluation.message('ToCharacterCode', 'strse', Integer(1), exp)
                return None
        else:
            string = string.get_string_value()
            if string is None:
                evaluation.message('ToCharacterCode', 'strse', Integer(1), exp)
                return None

        if isinstance(string, list):
            codes = [[ord(char) for char in substring] for substring in string]
        elif isinstance(string, six.string_types):
            codes = [ord(char) for char in string]
        return from_python(codes)


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
        'notunicode': (
            'A character code, which should be a non-negative integer less '
            'than 65536, is expected at position `2` in `1`.'),
        'intnm': (
            'Non-negative machine-sized integer expected at '
            'position `2` in `1`.'),
    }

    def apply(self, n, evaluation):
        "FromCharacterCode[n_]"
        exp = Expression('FromCharacterCode', n)

        class InvalidCodepointError(ValueError):
            pass

        def convert_codepoint_list(l, encoding=None):
            if encoding is not None:
                raise NotImplementedError

            s = ''
            for i, ni in enumerate(l):
                pyni = ni.get_int_value()
                if not(pyni is not None and 0 <= pyni <= 0xffff):
                    evaluation.message(
                        'FromCharacterCode', 'notunicode',
                        Expression('List', *l), Integer(i + 1))
                    raise InvalidCodepointError
                s += unichr(pyni)

            return s

        try:
            if n.has_form('List', None):
                if not n.get_leaves():
                    return String('')
                # Mathematica accepts FromCharacterCode[{{100}, 101}],
                # so to match this, just check the first leaf to see
                # if we're dealing with nested lists.
                elif n.get_leaves()[0].has_form('List', None):
                    list_of_strings = []
                    for leaf in n.get_leaves():
                        if leaf.has_form('List', None):
                            stringi = convert_codepoint_list(leaf.get_leaves())
                        else:
                            stringi = convert_codepoint_list([leaf])
                        list_of_strings.append(String(stringi))
                    return Expression('List', *list_of_strings)
                else:
                    return String(convert_codepoint_list(n.get_leaves()))
            else:
                pyn = n.get_int_value()
                if not (isinstance(pyn, six.integer_types) and pyn > 0 and pyn < sys.maxsize):
                    return evaluation.message(
                        'FromCharacterCode', 'intnm', exp, Integer(1))
                return String(convert_codepoint_list([n]))
        except InvalidCodepointError:
            return

        assert False, "can't get here"


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


class StringTake(Builtin):
    """
    <dl>
    <dt>'StringTake["$string$", $n$]'
        <dd>gives the first $n$ characters in $string$.
    <dt>'StringTake["$string$", -$n$]'
        <dd>gives the last $n$ characters in $string$.
    <dt>'StringTake["$string$", {$n$}]'
        <dd>gives the $n$th character in $string$.
    <dt>'StringTake["$string$", {$m$, $n$}]'
        <dd>gives characters $m$ through $n$ in $string$.
    <dt>'StringTake["$string$", {$m$, $n$, $s$}]'
        <dd>gives characters $m$ through $n$ in steps of $s$.
    </dl>

    >> StringTake["abcde", 2]
    = ab
    >> StringTake["abcde", 0]
    = 
    >> StringTake["abcde", -2]
    = de
    >> StringTake["abcde", {2}]
    = b
    >> StringTake["abcd", {2,3}]
    = bc
    >> StringTake["abcdefgh", {1, 5, 2}]
     = ace

    StringTake also supports standard sequence specifications
    >> StringTake["abcdef", All]
     = abcdef

    #> StringTake["abcd", 0] // InputForm
    = ""
    #> StringTake["abcd", {3, 2}] // InputForm
    = ""
    #> StringTake["", {1, 0}] // InputForm
    = ""

    #> StringTake["abc", {0, 0}]
    : Cannot take positions 0 through 0 in "abc".
    = StringTake[abc, {0, 0}]
    """

    messages = {
        'strse': 'String expected at position 1.',
        'mseqs': 'Integer or list of two Intergers are expected at position 2.',
        'take': 'Cannot take positions `1` through `2` in "`3`".',
    }

    def apply(self, string, seqspec, evaluation):
        'StringTake[string_, seqspec_]'

        result = string.get_string_value()
        if result is None:
            return evaluation.message('StringTake', 'strse')

        if isinstance(seqspec, Integer):
            pos = seqspec.get_int_value()
            if pos >= 0:
                seq = (1, pos, 1)
            else:
                seq = (pos, None, 1)
        else:
            seq = convert_seq(seqspec)

        if seq is None:
            return evaluation.message('StringTake', 'mseqs')

        start, stop, step = seq
        py_slice = python_seq(start, stop, step, len(result))

        if py_slice is None:
            return evaluation.message('StringTake', 'take', start, stop, string)

        return String(result[py_slice])


class StringDrop(Builtin):
    """
    <dl>
    <dt>'StringDrop["$string$", $n$]'
        <dd>gives $string$ with the first $n$ characters dropped.
    <dt>'StringDrop["$string$", -$n$]'
        <dd>gives $string$ with the last $n$ characters dropped.
    <dt>'StringDrop["$string$", {$n$}]'
        <dd>gives $string$ with the $n$th character dropped.
    <dt>'StringDrop["$string$", {$m$, $n$}]'
        <dd>gives $string$ with the characters $m$ through $n$ dropped.
    </dl>

    >> StringDrop["abcde", 2]
    = cde
    >> StringDrop["abcde", -2]
    = abc
    >> StringDrop["abcde", {2}]
    = acde
    >> StringDrop["abcde", {2,3}]
    = ade
    >> StringDrop["abcd",{3,2}]
    = abcd
    >> StringDrop["abcd",0]
    = abcd
    """
    messages = {
        'strse': 'String expected at position 1.',
        'mseqs': 'Integer or list of two Intergers are expected at position 2.',
        'drop': 'Cannot drop positions `1` through `2` in "`3`".',
    }

    def apply1_(self, string, n, evaluation):
        'StringDrop[string_,n_Integer]'
        if not isinstance(string, String):
            return evaluation.message('StringDrop', 'strse')
        if isinstance(n, Integer):
            pos = n.value
            if pos > len(string.get_string_value()):
                return evaluation.message('StringDrop', 'drop', 1, pos, string)
            if pos < -len(string.get_string_value()):
                return evaluation.message('StringDrop', 'drop', pos, -1, string)
            if pos > 0:
                return String(string.get_string_value()[pos:])
            if pos < 0:
                return String(string.get_string_value()[:(pos)])
            if pos == 0:
                return string
        return evaluation.message('StringDrop', 'mseqs')

    def apply2_(self, string, ni, nf, evaluation):
        'StringDrop[string_,{ni_Integer,nf_Integer}]'
        if not isinstance(string, String):
            return evaluation.message('StringDrop', 'strse', string)

        if ni.value == 0 or nf.value == 0:
            return evaluation.message('StringDrop', 'drop', ni, nf)
        fullstring = string.get_string_value()
        lenfullstring = len(fullstring)
        posi = ni.value
        if posi < 0:
            posi = lenfullstring + posi + 1
        posf = nf.value
        if posf < 0:
            posf = lenfullstring + posf + 1
        if (posf > lenfullstring or posi > lenfullstring or posf <= 0 or posi <= 0):
            # positions out or range
            return evaluation.message('StringDrop',
                                      'drop', ni, nf, fullstring)
        if posf < posi:
            return string           # this is what actually mma does
        return String(fullstring[:(posi - 1)] + fullstring[posf:])

    def apply3_(self, string, ni, evaluation):
        'StringDrop[string_,{ni_Integer}]'
        if not isinstance(string, String):
            return evaluation.message('StringDrop', 'strse', string)
        if ni.value == 0:
            return evaluation.message('StringDrop', 'drop', ni, ni)
        fullstring = string.get_string_value()
        lenfullstring = len(fullstring)
        posi = ni.value
        if posi < 0:
            posi = lenfullstring + posi + 1
        if posi > lenfullstring or posi <= 0:
            return evaluation.message('StringDrop', 'drop', ni, ni, fullstring)
        return String(fullstring[:(posi - 1)] + fullstring[posi:])

    def apply4_(self, string, something, evaluation):
        'StringDrop[string_,something___]'
        if not isinstance(string, String):
            return evaluation.message('StringDrop', 'strse')
        return evaluation.message('StringDrop', 'mseqs')


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
        'idim': '`1` and `2` must be of same length.',
    }

    options = {
        'IgnoreCase': 'False',
    }

    @staticmethod
    def _compute(u, v, same, evaluation):
        if len(u) != len(v):
            evaluation.message('HammingDistance', 'idim', u, v)
            return None
        else:
            return Integer(sum(0 if same(x, y) else 1 for x, y in zip(u, v)))

    def apply_list(self, u, v, evaluation):
        'HammingDistance[u_List, v_List]'
        return HammingDistance._compute(u.leaves, v.leaves, lambda x, y: x.same(y), evaluation)

    def apply_string(self, u, v, evaluation, options):
        'HammingDistance[u_String, v_String, OptionsPattern[HammingDistance]]'
        ignore_case = self.get_option(options, 'IgnoreCase', evaluation)
        py_u = u.get_string_value()
        py_v = v.get_string_value()
        if ignore_case and ignore_case.is_true():
            py_u = py_u.lower()
            py_v = py_v.lower()
        return HammingDistance._compute(py_u, py_v, lambda x, y: x == y, evaluation)


class _StringDistance(Builtin):
    options = {
        'IgnoreCase': 'False'
    }

    def apply(self, a, b, evaluation, options):
        '%(name)s[a_, b_, OptionsPattern[%(name)s]]'
        if isinstance(a, String) and isinstance(b, String):
            py_a = a.get_string_value()
            py_b = b.get_string_value()
            if options['System`IgnoreCase'] == Symbol('True'):
                if hasattr(str, 'casefold'):
                    def normalize(c):
                        return unicodedata.normalize("NFKD", c.casefold())
                    py_a = [normalize(c) for c in py_a]
                    py_b = [normalize(c) for c in py_b]
                else:  # python2, PyPy
                    py_a = py_a.lower()
                    py_b = py_b.lower()
            return Integer(self._distance(
                py_a, py_b, lambda u, v: u == v))
        elif a.get_head_name() == 'System`List' and b.get_head_name() == 'System`List':
            return Integer(self._distance(
                a.leaves, b.leaves, lambda u, v: u.same(v)))
        else:
            return Expression('EditDistance', a, b)


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


def _levenshtein_di(c1, s2, i, d_prev, same, cost):  # compute one new row
    # given c1 = s1[i], s2, i, d_prev = D(i - 1, ...), compute D(i, ...)

    yield i  # start with D(i, 0) = i, see (2)
    d_curr_prev_j = i  # d_curr_prev_j stores D(i, j - 1)

    for j, c2 in _one_based(enumerate(s2)):  # c2 = s2[[j]]
        cond = (0 if same(c1, c2) else cost)

        d_curr_j = min(  # see (4)
            d_prev[j - 1] + cond,  # D(i - 1, j - 1) + cond; substitution
            d_curr_prev_j + 1,  # D(i, j - 1) + 1; insertion
            d_prev[j] + 1)  # D(i - 1, j) + 1; deletion

        yield d_curr_j
        d_curr_prev_j = d_curr_j


def _levenshtein(s1, s2, same):
    d_prev = _levenshtein_d0(s2)
    for i, c1 in _one_based(enumerate(s1)):  # c1 = s1[[i]]
        d_prev = list(_levenshtein_di(c1, s2, i, d_prev, same, 1))
    return d_prev[-1]


def _damerau_levenshtein(s1, s2, same):
    # _damerau_levenshtein works like _levenshtein, except for one additional
    # rule covering transposition:
    #
    # if i > 1 and j > 1 and a[i] == b[j - 1] and a[i - 1] == b[j] then
    #     D(i, j) = minimum(D(i, j), D(i - 2, j - 2) + transposition_cost)

    def row(d_prev_prev, d_prev, i, prev_c1, c1, cost):
        # given c1 = s1[i], d_prev_prev = D(i - 2), d_prev = D(i - 1),
        # prev_c1 = s1[[i - 1]], c1 = s1[[i]], compute D(i, ...)
        for j, d_curr_j in enumerate(_levenshtein_di(c1, s2, i, d_prev, same, cost)):
            if i > 1 and j > 1:
                if same(c1, s2[j - 2]) and same(prev_c1, s2[j - 1]):  # transposition?
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


def _levenshtein_like_or_border_cases(s1, s2, same, compute):
    if len(s1) == len(s2) and all(same(c1, c2) for c1, c2 in zip(s1, s2)):
        return 0

    if len(s1) < len(s2):
        s1, s2 = s2, s1

    if len(s2) == 0:
        return len(s1)

    return compute(s1, s2, same)


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

    def _distance(self, s1, s2, same):
        return _levenshtein_like_or_border_cases(s1, s2, same, _levenshtein)


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

    def _distance(self, s1, s2, same):
        return _levenshtein_like_or_border_cases(s1, s2, same, _damerau_levenshtein)
