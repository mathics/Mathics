# -*- coding: utf8 -*-

"""
String functions
"""

from mathics.builtin.base import BinaryOperator, Builtin, Test
from mathics.core.expression import (Expression, Symbol, String, Integer,
                                     from_python)


class StringJoin(BinaryOperator):
    """
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
        if items.get_head_name() == 'List':
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
    >> StringSplit["abc,123", ","]
     = {abc, 123}

    >> StringSplit["abc 123"]
     = {abc, 123}

    #> StringSplit["  abc    123  "]
     = {abc, 123}

    >> StringSplit["abc,123.456", {",", "."}]
     = {abc, 123, 456}

    #> StringSplit["x", "x"]
     = {}

    #> StringSplit[x]
     : String or list of strings expected at position 1 in StringSplit[x].
     = StringSplit[x]

    #> StringSplit["x", x]      (* Mathematica uses StringExpression *)
     : String or list of strings expected at position 2 in StringSplit[x, x].
     = StringSplit[x, x]
    """

    messages = {
        'strse': 'String or list of strings expected at position `1` in `2`.',
    }

    def apply(self, string, seps, evaluation):
        'StringSplit[string_String, seps_List]'
        py_string, py_seps = string.get_string_value(), seps.get_leaves()
        result = [py_string]

        for py_sep in py_seps:
            if not isinstance(py_sep, String):
                evaluation.message('StringSplit', 'strse', Integer(2),
                                   Expression('StringSplit', string, seps))
                return

        py_seps = [py_sep.get_string_value() for py_sep in py_seps]

        for py_sep in py_seps:
            result = [t for s in result for t in s.split(py_sep)]
        return from_python(filter(lambda x: x != u'', result))

    def apply_single(self, string, sep, evaluation):
        'StringSplit[string_String, sep_?NotListQ]'
        if not isinstance(sep, String):
            evaluation.message('StringSplit', 'strse', Integer(2),
                               Expression('StringSplit', string, sep))
            return
        return self.apply(string, Expression('List', sep), evaluation)

    def apply_empty(self, string, evaluation):
        'StringSplit[string_String]'
        py_string = string.get_string_value()
        result = py_string.split()
        return from_python(filter(lambda x: x != u'', result))

    def apply_strse1(self, x, evaluation):
        'StringSplit[x_/;Not[StringQ[x]]]'
        evaluation.message('StringSplit', 'strse', Integer(1),
                           Expression('StringSplit', x))
        return

    def apply_strse2(self, x, y, evaluation):
        'StringSplit[x_/;Not[StringQ[x]], y_]'
        evaluation.message('StringSplit', 'strse', Integer(1),
                           Expression('StringSplit', x))
        return


class StringLength(Builtin):
    """
    'StringLength' gives the length of a string.
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


class StringReplace(Builtin):
    """
    <dl>
    <dt>'StringReplace["$string$", $s$->$sp$]' or 'StringReplace["$string$", {$s1$->$sp1$, $s2$->$sp2$}]'
      <dd>replace the string $si$ by $spi$ for all occurances in "$string$".
    <dt>'StringReplace["$string$", $srules$, $n$]'
      <dd>only perform the first $n$ replacements.
    <dt>'StringReplace[{"$string1$", "$string2$", ...}, srules]'
      <dd>perform replacements on a list of strings
    </dl>

    StringReplace replaces all occurances of one substring with another:
    >> StringReplace["xyxyxyyyxxxyyxy", "xy" -> "A"]
     = AAAyyxxAyA

    Multiple replacements can be supplied:
    >> StringReplace["xyzwxyzwxxyzxyzw", {"xyz" -> "A", "w" -> "BCD"}]
     = ABCDABCDxAABCD

    Only replace the first 2 occurances:
    >> StringReplace["xyxyxyyyxxxyyxy", "xy" -> "A", 2]
     = AAxyyyxxxyyxy

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
     : x -> y is not a valid string replacement rule.
     = StringReplace[xyzwxyzwaxyzxyzw, x -> y]
    #> StringReplace["abcabc", "a" -> "b", x]
     : Non-negative integer or Infinity expected at position 3 in StringReplace[abcabc, a -> b, x].
     = StringReplace[abcabc, a -> b, x]
    """

    attributes = ('Protected')

    # TODO: Implement these options
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

    # TODO: Implement StringExpression replacements
    def check_arguments(self, string, rule, n, evaluation):
        if n is None:
            expr = Expression('StringReplace', string, rule)
        else:
            expr = Expression('StringReplace', string, rule, n)

        # Check first argument
        if string.has_form('List', None):
            py_string = [s.get_string_value() for s in string.get_leaves()]
            if None in py_string:
                evaluation.message('StringReplace', 'strse', Integer(1), expr)
                return
        else:
            py_string = string.get_string_value()
            if py_string is None:
                evaluation.message('StringReplace', 'strse', Integer(1), expr)
                return

        # Check second argument
        def check_rule(r):
            tmp = [s.get_string_value() for s in r.get_leaves()]
            if not (r.has_form('Rule', None) and len(tmp) == 2 and
                    all(r is not None for r in tmp)):
                evaluation.message('StringReplace', 'srep', r)
                return None
            return tmp

        if rule.has_form('List', None):
            tmp_rules = rule.get_leaves()
            py_rules = []
            for r in tmp_rules:
                tmp = check_rule(r)
                if tmp is None:
                    return None
                py_rules.append(tmp)
        else:
            tmp = check_rule(rule)
            if tmp is None:
                return None
            py_rules = [tmp]

        if n is None:
            return (py_string, py_rules)
        elif n == Expression('DirectedInfinity', Integer(1)):
            return (py_string, py_rules, None)
        else:
            py_n = n.get_int_value()
            if py_n < 0:
                evaluation.message('StringReplace', 'innf', Integer(3), expr)
                return None
            return (py_string, py_rules, py_n)

    def apply(self, string, rule, evaluation):
        'StringReplace[string_, rule_]'

        args = self.check_arguments(string, rule, None, evaluation)
        if args is None:
            return None
        (py_string, py_rules) = args

        def do_replace(s):
            for sp in py_rules:
                s = s.replace(sp[0], sp[1])
            return s

        if isinstance(py_string, list):
            result = [do_replace(s) for s in py_string]
        else:
            result = do_replace(py_string)

        return from_python(result)

    def apply_n(self, string, rule, n, evaluation):
        'StringReplace[string_, rule_, n_]'

        args = self.check_arguments(string, rule, n, evaluation)

        if args is None:
            return None
        (py_string, py_rules, py_n) = args

        def do_replace(s):
            for sp in py_rules:
                if py_n is None:
                    s = s.replace(sp[0], sp[1])
                else:
                    s = s.replace(sp[0], sp[1], py_n)
            return s

        if isinstance(py_string, list):
            result = [do_replace(s) for s in py_string]
        else:
            result = do_replace(py_string)

        return from_python(result)


class Characters(Builtin):
    u"""
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
            String(unichr(code)) for code in xrange(start, stop + 1)])


class String_(Builtin):
    """
    'String' is the head of strings.
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


class ToString(Builtin):
    """
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
      <dd>reads the given input in the specified form.
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
     : Incomplete expression; more input is needed .
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
        'sntxi': 'Incomplete expression; more input is needed `1`.',
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
                from mathics.core.parser import parse, ParseError
                try:
                    result = parse(inp.get_string_value())
                except ParseError:
                    evaluation.message('ToExpression', 'sntxi', String(''))
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
    <dt>'ToCharacterCode["string"]'
      <dd>converts the string to a list of integer character codes.
    <dt>'ToCharacterCode[{"string1", "string2", ...}]'
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

    #TODO: encoding

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
        elif isinstance(string, basestring):
            codes = [ord(char) for char in string]
        return from_python(codes)


class FromCharacterCode(Builtin):
    """
    <dl>
    <dt>'FromCharacterCode[$n$]'
        <dd>returns the character corresponding to character code $n$.
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
        pyn = n.to_python()

        def convert(pyn, encoding=None):
            if encoding is not None:
                raise NotImplementedError

            for i, pyni in enumerate(pyn):
                if not (isinstance(pyni, int) and 0 <= pyni <= 0xffff):
                    return evaluation.message(
                        'FromCharacterCode', 'notunicode', pyn, Integer(i + 1))

            return ''.join(unichr(pyni) for pyni in pyn)

        if isinstance(pyn, list):
            if pyn == []:
                string = ''
            elif all(isinstance(ni, list) for ni in pyn):
                string = []
                for pyni in pyn:
                    stringi = convert(pyni)
                    if stringi is None:
                        string = None
                        break
                    else:
                        string.append(stringi)
            else:
                string = convert(pyn)
        else:
            if not (isinstance(pyn, int) and pyn > 0):
                evaluation.message(
                    'FromCharacterCode', 'intnm', exp, Integer(1))
                return
            string = convert([pyn])

        if string is not None:
            return from_python(string)


class StringQ(Test):
    """
    <dl>
    <dt>'StringQ[$expr$]'
      <dd>returns 'True' if $expr$ is a 'String' or 'False' otherwise.
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
