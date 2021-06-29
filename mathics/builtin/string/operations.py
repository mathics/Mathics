# -*- coding: utf-8 -*-

"""
Operations on Strings
"""

import re
from sys import version_info
from binascii import hexlify, unhexlify
from heapq import heappush, heappop

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import (
    BinaryOperator,
    Builtin,
)
from mathics.core.expression import (
    Expression,
    Symbol,
    SymbolFalse,
    SymbolTrue,
    SymbolList,
    String,
    Integer,
    Integer1,
    from_python,
)
from mathics.builtin.lists import python_seq, convert_seq
from mathics.builtin.strings import (
    _StringFind,
    _decode_pname,
    _encode_pname,
    _evaluate_match,
    _parallel_match,
    to_regex,
)


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
        "strse": "String expected at position 1.",
        "mseqs": "Integer or list of two Integers are expected at position 2.",
        "drop": 'Cannot drop positions `1` through `2` in "`3`".',
    }

    def apply_with_n(self, string, n, evaluation):
        "StringDrop[string_,n_Integer]"
        if not isinstance(string, String):
            return evaluation.message("StringDrop", "strse")
        if isinstance(n, Integer):
            pos = n.value
            if pos > len(string.get_string_value()):
                return evaluation.message("StringDrop", "drop", 1, pos, string)
            if pos < -len(string.get_string_value()):
                return evaluation.message("StringDrop", "drop", pos, -1, string)
            if pos > 0:
                return String(string.get_string_value()[pos:])
            if pos < 0:
                return String(string.get_string_value()[:(pos)])
            if pos == 0:
                return string
        return evaluation.message("StringDrop", "mseqs")

    def apply_with_ni_nf(self, string, ni, nf, evaluation):
        "StringDrop[string_,{ni_Integer,nf_Integer}]"
        if not isinstance(string, String):
            return evaluation.message("StringDrop", "strse", string)

        if ni.value == 0 or nf.value == 0:
            return evaluation.message("StringDrop", "drop", ni, nf)
        fullstring = string.get_string_value()
        lenfullstring = len(fullstring)
        posi = ni.value
        if posi < 0:
            posi = lenfullstring + posi + 1
        posf = nf.value
        if posf < 0:
            posf = lenfullstring + posf + 1
        if posf > lenfullstring or posi > lenfullstring or posf <= 0 or posi <= 0:
            # positions out or range
            return evaluation.message("StringDrop", "drop", ni, nf, fullstring)
        if posf < posi:
            return string  # this is what actually mma does
        return String(fullstring[: (posi - 1)] + fullstring[posf:])

    def apply_with_ni(self, string, ni, evaluation):
        "StringDrop[string_,{ni_Integer}]"
        if not isinstance(string, String):
            return evaluation.message("StringDrop", "strse", string)
        if ni.value == 0:
            return evaluation.message("StringDrop", "drop", ni, ni)
        fullstring = string.get_string_value()
        lenfullstring = len(fullstring)
        posi = ni.value
        if posi < 0:
            posi = lenfullstring + posi + 1
        if posi > lenfullstring or posi <= 0:
            return evaluation.message("StringDrop", "drop", ni, ni, fullstring)
        return String(fullstring[: (posi - 1)] + fullstring[posi:])

    def apply(self, string, something, evaluation):
        "StringDrop[string_,something___]"
        if not isinstance(string, String):
            return evaluation.message("StringDrop", "strse")
        return evaluation.message("StringDrop", "mseqs")


class StringInsert(Builtin):
    """
    <dl>
      <dt>'StringInsert["$string$", "$snew$", $n$]'
      <dd>yields a string with $snew$ inserted starting at position $n$ in $string$.

      <dt>'StringInsert["$string$", "$snew$", -$n$]'
      <dd>inserts a at position $n$ from the end of "$string$".

      <dt>'StringInsert["$string$", "$snew$", {$n_1$, $n_2$, ...}]'
      <dd>inserts a copy of $snew$ at each position $n_i$ in $string$;
        the $n_i$ are taken before any insertion is done.

      <dt>'StringInsert[{$s_1$, $s_2$, ...}, "$snew$", $n$]'
      <dd>gives the list of resutls for each of the $s_i$.
    </dl>

    >> StringInsert["noting", "h", 4]
     = nothing

    #> StringInsert["abcdefghijklm", "X", 15]
     : Cannot insert at position 15 in abcdefghijklm.
     = StringInsert[abcdefghijklm, X, 15]

    #> StringInsert[abcdefghijklm, "X", 4]
     : String or list of strings expected at position 1 in StringInsert[abcdefghijklm, X, 4].
     = StringInsert[abcdefghijklm, X, 4]

    #> StringInsert["abcdefghijklm", X, 4]
     : String expected at position 2 in StringInsert[abcdefghijklm, X, 4].
     = StringInsert[abcdefghijklm, X, 4]

    #> StringInsert["abcdefghijklm", "X", a]
     : Position specification a in StringInsert[abcdefghijklm, X, a] is not a machine-sized integer or a list of machine-sized integers.
     = StringInsert[abcdefghijklm, X, a]

    #> StringInsert["abcdefghijklm", "X", 0]
     : Cannot insert at position 0 in abcdefghijklm.
     =  StringInsert[abcdefghijklm, X, 0]

    >> StringInsert["note", "d", -1]
     = noted

    >> StringInsert["here", "t", -5]
     = there

    #> StringInsert["abcdefghijklm", "X", -15]
     : Cannot insert at position -15 in abcdefghijklm.
     = StringInsert[abcdefghijklm, X, -15]

    >> StringInsert["adac", "he", {1, 5}]
     = headache

    #> StringInsert["abcdefghijklm", "X", {1, -1, 14, -14}]
     = XXabcdefghijklmXX

    #> StringInsert["abcdefghijklm", "X", {1, 0}]
     : Cannot insert at position 0 in abcdefghijklm.
     = StringInsert[abcdefghijklm, X, {1, 0}]

    #> StringInsert["", "X", {1}]
     = X

    #> StringInsert["", "X", {1, -1}]
     = XX

    #> StringInsert["", "", {1}]
     = #<--#

    #> StringInsert["", "X", {1, 2}]
     : Cannot insert at position 2 in .
     = StringInsert[, X, {1, 2}]

    #> StringInsert["abcdefghijklm", "", {1, 2, 3, 4 ,5, -6}]
     = abcdefghijklm

    #> StringInsert["abcdefghijklm", "X", {}]
     = abcdefghijklm

    >> StringInsert[{"something", "sometimes"}, " ", 5]
     = {some thing, some times}

    #> StringInsert[{"abcdefghijklm", "Mathics"}, "X", 13]
     : Cannot insert at position 13 in Mathics.
     = {abcdefghijklXm, StringInsert[Mathics, X, 13]}

    #> StringInsert[{"", ""}, "", {1, 1, 1, 1}]
     = {, }

    #> StringInsert[{"abcdefghijklm", "Mathics"}, "X", {0, 2}]
     : Cannot insert at position 0 in abcdefghijklm.
     : Cannot insert at position 0 in Mathics.
     = {StringInsert[abcdefghijklm, X, {0, 2}], StringInsert[Mathics, X, {0, 2}]}

    #> StringInsert[{"abcdefghijklm", Mathics}, "X", {1, 2}]
     : String or list of strings expected at position 1 in StringInsert[{abcdefghijklm, Mathics}, X, {1, 2}].
     = StringInsert[{abcdefghijklm, Mathics}, X, {1, 2}]

    #> StringInsert[{"", "Mathics"}, "X", {1, 1, -1}]
     = {XXX, XXMathicsX}

    >> StringInsert["1234567890123456", ".", Range[-16, -4, 3]]
     = 1.234.567.890.123.456"""

    messages = {
        "strse": "String or list of strings expected at position `1` in `2`.",
        "string": "String expected at position `1` in `2`.",
        "ins": "Cannot insert at position `1` in `2`.",
        "psl": "Position specification `1` in `2` is not a machine-sized integer or a list of machine-sized integers.",
    }

    def _insert(self, str, add, lpos, evaluation):
        for pos in lpos:
            if abs(pos) < 1 or abs(pos) > len(str) + 1:
                evaluation.message("StringInsert", "ins", Integer(pos), String(str))
                return evaluation.format_output(
                    Expression(
                        "StringInsert", str, add, lpos[0] if len(lpos) == 1 else lpos
                    )
                )

        # Create new list of position which are rearranged
        pos_limit = len(str) + 2
        listpos = [p if p > 0 else pos_limit + p for p in lpos]
        listpos.sort()

        result = ""
        start = 0
        for pos in listpos:
            stop = pos - 1
            result += str[start:stop] + add
            start = stop
        else:
            result += str[start : len(str)]

        return result

    def apply(self, strsource, strnew, pos, evaluation):
        "StringInsert[strsource_, strnew_, pos_]"

        exp = Expression("StringInsert", strsource, strnew, pos)

        py_strnew = strnew.get_string_value()
        if py_strnew is None:
            return evaluation.message("StringInsert", "string", Integer(2), exp)

        # Check and create list of position
        listpos = []
        if pos.has_form("List", None):
            leaves = pos.get_leaves()
            if not leaves:
                return strsource
            else:
                for i, posi in enumerate(leaves):
                    py_posi = posi.get_int_value()
                    if py_posi is None:
                        return evaluation.message("StringInsert", "psl", pos, exp)
                    listpos.append(py_posi)
        else:
            py_pos = pos.get_int_value()
            if py_pos is None:
                return evaluation.message("StringInsert", "psl", pos, exp)
            listpos.append(py_pos)

        # Check and perform the insertion
        if strsource.has_form("List", None):
            py_strsource = [sub.get_string_value() for sub in strsource.leaves]
            if any(sub is None for sub in py_strsource):
                return evaluation.message("StringInsert", "strse", Integer1, exp)
            return Expression(
                "List",
                *[
                    String(self._insert(s, py_strnew, listpos, evaluation))
                    for s in py_strsource
                ]
            )
        else:
            py_strsource = strsource.get_string_value()
            if py_strsource is None:
                return evaluation.message("StringInsert", "strse", Integer1, exp)
            return String(self._insert(py_strsource, py_strnew, listpos, evaluation))


class StringJoin(BinaryOperator):
    """
    <dl>
    <dt>'StringJoin["$s1$", "$s2$", ...]'
        <dd>returns the concatenation of the strings $s1$, $s2$,  .
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

    operator = "<>"
    precedence = 600
    attributes = ("Flat", "OneIdentity")

    def apply(self, items, evaluation):
        "StringJoin[items___]"

        result = ""
        items = items.flatten(SymbolList)
        if items.get_head_name() == "System`List":
            items = items.leaves
        else:
            items = items.get_sequence()
        for item in items:
            if not isinstance(item, String):
                evaluation.message("StringJoin", "string")
                return
            result += item.value
        return String(result)


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

    attributes = ("Listable",)

    def apply(self, str, evaluation):
        "StringLength[str_]"

        if not isinstance(str, String):
            evaluation.message("StringLength", "string")
            return
        return Integer(len(str.value))


class StringPosition(Builtin):
    """
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
    """

    options = {
        "IgnoreCase": "False",
        "MetaCharacters": "None",
        "Overlaps": "True",
    }

    messages = {
        "strse": "String or list of strings expected at position `1` in `2`.",
        "overall": "Overlaps -> All option is not currently implemented in Mathics.",
        "innf": "Non-negative integer or Infinity expected at position `2` in `1`.",
    }

    rules = {
        "StringPosition[patt_][s_]": "StringPosition[s, patt]",
    }

    def apply(self, string, patt, evaluation, options):
        "StringPosition[string_, patt_, OptionsPattern[StringPosition]]"

        return self.apply_n(
            string,
            patt,
            Expression("DirectedInfinity", Integer1),
            evaluation,
            options,
        )

    def apply_n(self, string, patt, n, evaluation, options):
        "StringPosition[string_, patt_, n:(_Integer|DirectedInfinity[1]), OptionsPattern[StringPosition]]"

        expr = Expression("StringPosition", string, patt, n)

        # check n
        if n.has_form("DirectedInfinity", 1):
            py_n = float("inf")
        else:
            py_n = n.get_int_value()
            if py_n is None or py_n < 0:
                return evaluation.message("StringPosition", "innf", expr, Integer(3))

        # check options
        if options["System`Overlaps"] == SymbolTrue:
            overlap = True
        elif options["System`Overlaps"] == SymbolFalse:
            overlap = False
        elif options["System`Overlaps"] == Symbol("All"):
            # TODO
            evaluation.message("StringPosition", "overall")
            overlap = True
        else:
            overlap = False  # unknown options are teated as False

        # convert patterns
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
        compiled_patts = [re.compile(re_patt) for re_patt in re_patts]

        # string or list of strings
        if string.has_form("List", None):
            py_strings = [s.get_string_value() for s in string.leaves]
            if None in py_strings:
                return
            results = [
                self.do_apply(py_string, compiled_patts, py_n, overlap)
                for py_string in py_strings
            ]
            return Expression(SymbolList, *results)
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
                result.append([m.start() + 1, m.end()])  # 0 to 1 based indexing
                if len(result) == py_n:
                    return from_python(result)
                if not overlap:
                    start = m.end()
            if overlap or not found_match:
                start += 1
        return from_python(result)


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
    #> StringReplace["product: A \\[CirclePlus] B" , "\\[CirclePlus]" -> "x"]
     = A x B
    """

    rules = {
        "StringReplace[rule_][string_]": "StringReplace[string, rule]",
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

        return Expression("StringJoin", *list(cases()))

    def apply(self, string, rule, n, evaluation, options):
        "%(name)s[string_, rule_, OptionsPattern[%(name)s], n_:System`Private`Null]"
        # this pattern is a slight hack to get around missing Shortest/Longest.
        return self._apply(string, rule, n, evaluation, options, False)


class StringReverse(Builtin):
    """
    <dl>
      <dt>'StringReverse["$string$"]'
      <dd>reverses the order of the characters in "string".
      </dl>

      >> StringReverse["live"]
       = evil
    """

    attributes = ("Listable", "Protected")

    def apply(self, string, evaluation):
        "StringReverse[string_String]"
        return String(string.get_string_value()[::-1])


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

      <dt>'StringTake[{$s1$, $s2$, ...} $spec$}]'
      <dd>gives the list of results for each of the $si$.
    </dl>

    >> StringTake["abcde", 2]
     = ab
    >> StringTake["abcde", 0]
     = #<--#
    >> StringTake["abcde", -2]
     = de
    >> StringTake["abcde", {2}]
     = b
    >> StringTake["abcd", {2,3}]
     = bc
    >> StringTake["abcdefgh", {1, 5, 2}]
     = ace

    Take the last 2 characters from several strings:
    >> StringTake[{"abcdef", "stuv", "xyzw"}, -2]
     = {ef, uv, zw}

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

    #> StringTake[{2, 4},2]
     : String or list of strings expected at position 1.
     = StringTake[{2, 4}, 2]

    #> StringTake["kkkl",Graphics[{}]]
     : Integer or a list of sequence specifications expected at position 2.
     = StringTake[kkkl, -Graphics-]
    """

    messages = {
        "strse": "String or list of strings expected at position 1.",
        # FIXME: mseqs should be: Sequence specification (+n, -n, {+n}, {-n}, {m, n}, or {m, n, s}) or a list
        # of sequence specifications expected at position 2 in
        "mseqs": "Integer or a list of sequence specifications expected at position 2.",
        "take": 'Cannot take positions `1` through `2` in "`3`".',
    }

    def apply(self, string, seqspec, evaluation):
        "StringTake[string_String, seqspec_]"
        result = string.get_string_value()
        if result is None:
            return evaluation.message("StringTake", "strse")

        if isinstance(seqspec, Integer):
            pos = seqspec.get_int_value()
            if pos >= 0:
                seq = (1, pos, 1)
            else:
                seq = (pos, None, 1)
        else:
            seq = convert_seq(seqspec)

        if seq is None:
            return evaluation.message("StringTake", "mseqs")

        start, stop, step = seq
        py_slice = python_seq(start, stop, step, len(result))

        if py_slice is None:
            return evaluation.message("StringTake", "take", start, stop, string)

        return String(result[py_slice])

    def apply_strings(self, strings, spec, evaluation):
        "StringTake[strings__, spec_]"
        result_list = []
        for string in strings.leaves:
            result = self.apply(string, spec, evaluation)
            if result is None:
                return None
            result_list.append(result)
        return Expression("List", *result_list)
