# -*- coding: utf-8 -*-

"""
Input and Output
"""

import re
import mpmath

import typing
from typing import Any

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import (
    Builtin,
    BinaryOperator,
    BoxConstruct,
    BoxConstructError,
    Operator,
    Predefined,
)
from mathics.builtin.tensors import get_dimensions
from mathics.builtin.comparison import expr_min
from mathics.builtin.lists import list_boxes
from mathics.builtin.options import options_to_rules
from mathics.core.expression import (
    Expression,
    String,
    StringFromPython,
    Symbol,
    Integer,
    Real,
    BoxError,
    from_python,
    MachineReal,
    PrecisionReal,
    SymbolList,
    SymbolMakeBoxes,
    SymbolRule
)
from mathics.core.numbers import (
    dps,
    convert_base,
    machine_precision,
    reconstruct_digits,
)

from mathics.core.evaluation import Message as EvaluationMessage

MULTI_NEWLINE_RE = re.compile(r"\n{2,}")


class UseSansSerif(Predefined):
    """
    <dl>
      <dt>'$UseSansSerif'
      <dd>controls whether the Web interfaces use a Sans-Serif font.
    </dl>

    When set True, the output in MathMLForm uses SansSerif fonts instead
    of the standard fonts.

    X> $UseSansSerif
     = True
    X> $UseSansSerif = False

    """

    context = "System`"
    name = "$UseSansSerif"
    attributes = ("Unprotected",)
    value = True

    rules = {"$UseSansSerif": str(value)}

    messages = {}

    def evaluate(self, evaluation):
        return Integer(self.value)


class Format(Builtin):
    """
    <dl>
      <dt>'Format[$expr$]'
      <dd>holds values specifying how $expr$ should be printed.
    </dl>

    Assign values to 'Format' to control how particular expressions
    should be formatted when printed to the user.
    >> Format[f[x___]] := Infix[{x}, "~"]
    >> f[1, 2, 3]
     = 1 ~ 2 ~ 3
    >> f[1]
     = 1

    Raw objects cannot be formatted:
    >> Format[3] = "three";
     : Cannot assign to raw object 3.

    Format types must be symbols:
    >> Format[r, a + b] = "r";
     : Format type a + b is not a symbol.

    Formats must be attached to the head of an expression:
    >> f /: Format[g[f]] = "my f";
     : Tag f not found or too deep for an assigned rule.
    """

    messages = {"fttp": "Format type `1` is not a symbol."}


def parenthesize(precedence, leaf, leaf_boxes, when_equal):
    from mathics.builtin import builtins_precedence

    while leaf.has_form("HoldForm", 1):
        leaf = leaf.leaves[0]
    if leaf.has_form(("Infix", "Prefix", "Postfix"), 3, None):
        leaf_prec = leaf.leaves[2].get_int_value()
    elif leaf.has_form("PrecedenceForm", 2):
        leaf_prec = leaf.leaves[1].get_int_value()
    else:
        leaf_prec = builtins_precedence.get(leaf.get_head_name())
    if precedence is not None and leaf_prec is not None:
        if precedence > leaf_prec or (precedence == leaf_prec and when_equal):
            return Expression(
                "RowBox", Expression(SymbolList, String("("), leaf_boxes, String(")"))
            )
    return leaf_boxes


def make_boxes_infix(leaves, ops, precedence, grouping, form):
    result = []
    for index, leaf in enumerate(leaves):
        if index > 0:
            result.append(ops[index - 1])
        parenthesized = False
        if grouping == "System`NonAssociative":
            parenthesized = True
        elif grouping == "System`Left" and index > 0:
            parenthesized = True
        elif grouping == "System`Right" and index == 0:
            parenthesized = True

        leaf_boxes = MakeBoxes(leaf, form)
        leaf = parenthesize(precedence, leaf, leaf_boxes, parenthesized)

        result.append(leaf)
    return Expression("RowBox", Expression(SymbolList, *result))


def real_to_s_exp(expr, n):
    if expr.is_zero:
        s = "0"
        sign_prefix = ""
        if expr.is_machine_precision():
            exp = 0
        else:
            p = expr.get_precision()
            exp = -dps(p)
        nonnegative = 1
    else:
        if n is None:
            if expr.is_machine_precision():
                value = expr.get_float_value()
                s = repr(value)
            else:
                with mpmath.workprec(expr.get_precision()):
                    value = expr.to_mpmath()
                    s = mpmath.nstr(value, dps(expr.get_precision()) + 1)
        else:
            with mpmath.workprec(expr.get_precision()):
                value = expr.to_mpmath()
                s = mpmath.nstr(value, n)

        # sign prefix
        if s[0] == "-":
            assert value < 0
            nonnegative = 0
            s = s[1:]
        else:
            assert value >= 0
            nonnegative = 1

        # exponent (exp is actual, pexp is printed)
        if "e" in s:
            s, exp = s.split("e")
            exp = int(exp)
            if len(s) > 1 and s[1] == ".":
                # str(float) doesn't always include '.' if 'e' is present.
                s = s[0] + s[2:].rstrip("0")
        else:
            exp = s.index(".") - 1
            s = s[: exp + 1] + s[exp + 2 :].rstrip("0")

            # consume leading '0's.
            i = 0
            while s[i] == "0":
                i += 1
                exp -= 1
            s = s[i:]

        # add trailing zeros for precision reals
        if n is not None and not expr.is_machine_precision() and len(s) < n:
            s = s + "0" * (n - len(s))
    return s, exp, nonnegative


def int_to_s_exp(expr, n):
    n = expr.get_int_value()
    if n < 0:
        nonnegative = 0
        s = str(-n)
    else:
        nonnegative = 1
        s = str(n)
    exp = len(s) - 1
    return s, exp, nonnegative


def number_form(expr, n, f, evaluation, options):
    """
    Converts a Real or Integer instance to Boxes.

    n digits of precision with f (can be None) digits after the decimal point.
    evaluation (can be None) is used for messages.

    The allowed options are python versions of the options permitted to
    NumberForm and must be supplied. See NumberForm or Real.make_boxes
    for correct option examples.
    """

    assert isinstance(n, int) and n > 0 or n is None
    assert f is None or (isinstance(f, int) and f >= 0)

    is_int = False
    if isinstance(expr, Integer):
        assert n is not None
        s, exp, nonnegative = int_to_s_exp(expr, n)
        if f is None:
            is_int = True
    elif isinstance(expr, Real):
        if n is not None:
            n = min(n, dps(expr.get_precision()) + 1)
        s, exp, nonnegative = real_to_s_exp(expr, n)
        if n is None:
            n = len(s)
    else:
        raise ValueError("Expected Real or Integer.")

    assert isinstance(n, int) and n > 0

    sign_prefix = options["NumberSigns"][nonnegative]

    # round exponent to ExponentStep
    rexp = (exp // options["ExponentStep"]) * options["ExponentStep"]

    if is_int:
        # integer never uses scientific notation
        pexp = ""
    else:
        method = options["ExponentFunction"]
        pexp = method(Integer(rexp)).get_int_value()
        if pexp is not None:
            exp -= pexp
            pexp = str(pexp)
        else:
            pexp = ""

    # pad right with '0'.
    if len(s) < exp + 1:
        if evaluation is not None:
            evaluation.message("NumberForm", "sigz")
        # TODO NumberPadding?
        s = s + "0" * (1 + exp - len(s))
    # pad left with '0'.
    if exp < 0:
        s = "0" * (-exp) + s
        exp = 0

    # left and right of NumberPoint
    left, right = s[: exp + 1], s[exp + 1 :]

    def _round(number, ndigits):
        """
        python round() for integers but with correct rounding.
        e.g. `_round(14225, -1)` is `14230` not `14220`.
        """
        assert isinstance(ndigits, int)
        assert ndigits < 0
        assert isinstance(number, int)
        assert number >= 0
        number += 5 * int(10 ** -(1 + ndigits))
        number //= int(10 ** -ndigits)
        return number

    # pad with NumberPadding
    if f is not None:
        if len(right) < f:
            # pad right
            right = right + (f - len(right)) * options["NumberPadding"][1]
        elif len(right) > f:
            # round right
            tmp = int(left + right)
            tmp = _round(tmp, f - len(right))
            tmp = str(tmp)
            left, right = tmp[: exp + 1], tmp[exp + 1 :]

    def split_string(s, start, step):
        if start > 0:
            yield s[:start]
        for i in range(start, len(s), step):
            yield s[i : i + step]

    # insert NumberSeparator
    digit_block = options["DigitBlock"]
    if digit_block[0] != 0:
        left = split_string(left, len(left) % digit_block[0], digit_block[0])
        left = options["NumberSeparator"][0].join(left)
    if digit_block[1] != 0:
        right = split_string(right, 0, digit_block[1])
        right = options["NumberSeparator"][1].join(right)

    left_padding = 0
    max_sign_len = max(len(options["NumberSigns"][0]), len(options["NumberSigns"][1]))
    l = len(sign_prefix) + len(left) + len(right) - max_sign_len
    if l < n:
        left_padding = n - l
    elif len(sign_prefix) < max_sign_len:
        left_padding = max_sign_len - len(sign_prefix)
    left_padding = left_padding * options["NumberPadding"][0]

    # insert NumberPoint
    if options["SignPadding"]:
        prefix = sign_prefix + left_padding
    else:
        prefix = left_padding + sign_prefix

    if is_int:
        s = prefix + left
    else:
        s = prefix + left + options["NumberPoint"] + right

    # base
    base = "10"

    # build number
    method = options["NumberFormat"]
    return method(String(s), String(base), String(pexp), options)


class MakeBoxes(Builtin):
    """
    <dl>
    <dt>'MakeBoxes[$expr$]'
        <dd>is a low-level formatting primitive that converts $expr$
        to box form, without evaluating it.
    <dt>'\\( ... \\)'
        <dd>directly inputs box objects.
    </dl>

    String representation of boxes
    >> \\(x \\^ 2\\)
     = SuperscriptBox[x, 2]

    >> \\(x \\_ 2\\)
     = SubscriptBox[x, 2]

    >> \\( a \\+ b \\% c\\)
     = UnderoverscriptBox[a, b, c]

    >> \\( a \\& b \\% c\\)
     = UnderoverscriptBox[a, c, b]

    #> \\( \\@ 5 \\)
     = SqrtBox[5]

    >> \\(x \\& y \\)
     = OverscriptBox[x, y]

    >> \\(x \\+ y \\)
     = UnderscriptBox[x, y]

    #> \\( x \\^ 2 \\_ 4 \\)
     = SuperscriptBox[x, SubscriptBox[2, 4]]

    ## Tests for issue 151 (infix operators in heads)
    #> (a + b)[x]
     = (a + b)[x]
    #> (a b)[x]
     = (a b)[x]
    #> (a <> b)[x]
     : String expected.
     = (a <> b)[x]
    """

    # TODO: Convert operators to appropriate representations e.g. 'Plus' to '+'
    """
    >> \\(a + b\\)
     = RowBox[{a, +, b}]

    >> \\(TraditionalForm \\` a + b\\)
     = FormBox[RowBox[{a, +, b}], TraditionalForm]

    >> \\(x \\/ \\(y + z\\)\\)
     =  FractionBox[x, RowBox[{y, +, z}]]
    """

    # TODO: Constructing boxes from Real
    """
    ## Test Real MakeBoxes
    #> MakeBoxes[1.4]
     = 1.4`
    #> MakeBoxes[1.4`]
     = 1.4`
    #> MakeBoxes[1.5`20]
     = 1.5`20.
    #> MakeBoxes[1.4`20]
     = 1.4`20.
    #> MakeBoxes[1.5``20]
     = 1.5`20.1760912591
    #> MakeBoxes[-1.4]
     = RowBox[{-, 1.4`}]
    #> MakeBoxes[34.*^3]
     = 34000.`

    #> MakeBoxes[0`]
     = 0.`
    #> MakeBoxes[0`3]
     = 0
    #> MakeBoxes[0``30]
     = 0.``30.
    #> MakeBoxes[0.`]
     = 0.`
    #> MakeBoxes[0.`3]
     = 0.`
    #> MakeBoxes[0.``30]
     = 0.``30.

    #> MakeBoxes[14]
     = 14
    #> MakeBoxes[-14]
     = RowBox[{-, 14}]
    """

    # TODO: Correct precedence
    """
    >> \\(x \\/ y + z\\)
     = RowBox[{FractionBox[x, y], +, z}]
    >> \\(x \\/ (y + z)\\)
     = FractionBox[x, RowBox[{(, RowBox[{y, +, z}], )}]]

    #> \\( \\@ a + b \\)
     = RowBox[{SqrtBox[a], +, b}]
    """

    # FIXME: Don't insert spaces with brackets
    """
    #> \\(c (1 + x)\\)
     = RowBox[{c, RowBox[{(, RowBox[{1, +, x}], )}]}]
    """

    # TODO: Required MakeExpression
    """
    #> \\!\\(x \\^ 2\\)
     = x ^ 2
    #> FullForm[%]
     = Power[x, 2]
    """

    # TODO: Fix Infix operators
    """
    >> MakeBoxes[1 + 1]
     = RowBox[{1, +, 1}]
    """

    # TODO: Parsing of special characters (like commas)
    """
    >> \\( a, b \\)
     = RowBox[{a, ,, b}]
    """

    attributes = ("HoldAllComplete", "Unprotected")

    rules = {
        "MakeBoxes[Infix[head_[leaves___]], "
        "    f:StandardForm|TraditionalForm|OutputForm|InputForm]": (
            'MakeBoxes[Infix[head[leaves], StringForm["~`1`~", head]], f]'
        ),
        "MakeBoxes[expr_]": "MakeBoxes[expr, StandardForm]",
        "MakeBoxes[(form:StandardForm|TraditionalForm|OutputForm|TeXForm|"
        "MathMLForm)[expr_], StandardForm|TraditionalForm]": ("MakeBoxes[expr, form]"),
        "MakeBoxes[(form:OutputForm|MathMLForm|TeXForm)[expr_], OutputForm]": "MakeBoxes[expr, form]",
        "MakeBoxes[StandardForm[expr_], OutputForm]": "MakeBoxes[expr, OutputForm]",
        "MakeBoxes[FullForm[expr_], StandardForm|TraditionalForm|OutputForm]": "StyleBox[MakeBoxes[expr, FullForm], ShowStringCharacters->True]",
        "MakeBoxes[InputForm[expr_], StandardForm|TraditionalForm|OutputForm]": "StyleBox[MakeBoxes[expr, InputForm], ShowStringCharacters->True]",
        "MakeBoxes[PrecedenceForm[expr_, prec_], f_]": "MakeBoxes[expr, f]",
        "MakeBoxes[Style[expr_, OptionsPattern[Style]], f_]": (
            "StyleBox[MakeBoxes[expr, f], "
            "ImageSizeMultipliers -> OptionValue[ImageSizeMultipliers]]"
        ),
    }

    def apply_general(self, expr, f, evaluation):
        """MakeBoxes[expr_,
            f:TraditionalForm|StandardForm|OutputForm|InputForm|FullForm]"""

        if expr.is_atom():
            return expr.atom_to_boxes(f, evaluation)
        else:
            head = expr.head
            leaves = expr.leaves

            f_name = f.get_name()
            if f_name == "System`TraditionalForm":
                left, right = "(", ")"
            else:
                left, right = "[", "]"

            # Parenthesize infix operators at the head of expressions,
            # like (a + b)[x], but not f[a] in f[a][b].
            #
            head_boxes = parenthesize(670, head, MakeBoxes(head, f), False)
            result = [head_boxes, String(left)]

            if len(leaves) > 1:
                row = []
                if f_name in (
                    "System`InputForm",
                    "System`OutputForm",
                    "System`FullForm",
                ):
                    sep = ", "
                else:
                    sep = ","
                for index, leaf in enumerate(leaves):
                    if index > 0:
                        row.append(String(sep))
                    row.append(MakeBoxes(leaf, f))
                result.append(RowBox(Expression(SymbolList, *row)))
            elif len(leaves) == 1:
                result.append(MakeBoxes(leaves[0], f))
            result.append(String(right))
            return RowBox(Expression(SymbolList, *result))

    def _apply_atom(self, x, f, evaluation):
        """MakeBoxes[x_?AtomQ,
            f:TraditionalForm|StandardForm|OutputForm|InputForm|FullForm]"""

        return x.atom_to_boxes(f, evaluation)

    def apply_outerprecedenceform(self, expr, prec, f, evaluation):
        """MakeBoxes[OuterPrecedenceForm[expr_, prec_],
            f:StandardForm|TraditionalForm|OutputForm|InputForm]"""

        precedence = prec.get_int_value()
        boxes = MakeBoxes(expr)
        return parenthesize(precedence, expr, boxes, True)

    def apply_postprefix(self, p, expr, h, prec, f, evaluation):
        """MakeBoxes[(p:Prefix|Postfix)[expr_, h_, prec_:None],
            f:StandardForm|TraditionalForm|OutputForm|InputForm]"""

        if not isinstance(h, String):
            h = MakeBoxes(h, f)

        precedence = prec.get_int_value()

        leaves = expr.get_leaves()
        if len(leaves) == 1:
            leaf = leaves[0]
            leaf_boxes = MakeBoxes(leaf, f)
            leaf = parenthesize(precedence, leaf, leaf_boxes, True)
            if p.get_name() == "System`Postfix":
                args = (leaf, h)
            else:
                args = (h, leaf)

            return Expression("RowBox", Expression(SymbolList, *args))
        else:
            return MakeBoxes(expr, f)

    def apply_infix(self, expr, h, prec, grouping, f, evaluation):
        """MakeBoxes[Infix[expr_, h_, prec_:None, grouping_:None],
            f:StandardForm|TraditionalForm|OutputForm|InputForm]"""

        def get_op(op):
            if not isinstance(op, String):
                op = MakeBoxes(op, f)
            else:
                op_value = op.get_string_value()
                if f.get_name() == "System`InputForm" and op_value in ["*", "^"]:
                    pass
                elif (
                    f.get_name() in ("System`InputForm", "System`OutputForm")
                    and not op_value.startswith(" ")
                    and not op_value.endswith(" ")
                ):
                    op = String(" " + op_value + " ")
            return op

        precedence = prec.get_int_value()
        grouping = grouping.get_name()

        leaves = expr.get_leaves()
        if len(leaves) > 1:
            if h.has_form("List", len(leaves) - 1):
                ops = [get_op(op) for op in h.leaves]
            else:
                ops = [get_op(h)] * (len(leaves) - 1)
            return make_boxes_infix(leaves, ops, precedence, grouping, f)
        elif len(leaves) == 1:
            return MakeBoxes(leaves[0], f)
        else:
            return MakeBoxes(expr, f)


class ToBoxes(Builtin):
    """
    <dl>
    <dt>'ToBoxes[$expr$]'
        <dd>evaluates $expr$ and converts the result to box form.
    </dl>

    Unlike 'MakeBoxes', 'ToBoxes' evaluates its argument:
    >> ToBoxes[a + a]
     = RowBox[{2,  , a}]

    >> ToBoxes[a + b]
     = RowBox[{a, +, b}]
    >> ToBoxes[a ^ b] // FullForm
     = SuperscriptBox["a", "b"]
    """

    def apply(self, expr, form, evaluation):
        "ToBoxes[expr_, form_:StandardForm]"

        form_name = form.get_name()
        if form_name is None:
            evaluation.message("ToBoxes", "boxfmt", form)
        boxes = expr.format(evaluation, form_name)
        return boxes


class RowBox(Builtin):
    """
    <dl>
    <dt>'RowBox[{...}]'
        <dd>is a box construct that represents a sequence of boxes
        arranged in a horizontal row.
    </dl>
    """


class BoxData(Builtin):
    """
    <dl>
    <dt>'BoxData[...]'
        <dd>is a low-level representation of the contents of a typesetting
    cell.
    </dl>
    """


class TextData(Builtin):
    """
    <dl>
    <dt>'TextData[...]'
        <dd>is a low-level representation of the contents of a textual
    cell.
    </dl>
    """


class TooltipBox(Builtin):
    """
    <dl>
    <dt>'TooltipBox[{...}]'
        <dd>undocumented...
    </dl>
    """


class InterpretationBox(Builtin):
    """
    <dl>
    <dt>'InterpretationBox[{...}]'
        <dd> is a low-level box construct that displays as
    boxes but is interpreted on input as expr.
    </dl>
    """

    attributes = ("HoldAllComplete", "Protected", "ReadProtected")


class StyleBox(Builtin):
    """
    <dl>
    <dt>'StyleBox[boxes, options]'
        <dd> is a low-level representation of boxes
     to be shown with the specified option settings.
    <dt>'StyleBox[boxes, style]'
        <dd> uses the option setting for the specified style in
    the current notebook.
    </dl>
    """

    attributes = ("Protected", "ReadProtected")


class ButtonBox(Builtin):
    """
    <dl>
    <dt>'ButtonBox[$boxes$]'
        <dd> is a low-level box construct that represents a button in a
    notebook expression.
    </dl>
    """

    attributes = ("Protected", "ReadProtected")


class TagBox(Builtin):
    """
    <dl>
    <dt>'TagBox[boxes, tag]'
        <dd> is a low-level box construct that displays as
    boxes but is interpreted on input as expr
    </dl>
    """

    attributes = ("HoldAllComplete", "Protected", "ReadProtected")


class TemplateBox(Builtin):
    """
    <dl>
    <dt>'TemplateBox[{$box_1$, $box_2$,...}, tag]'
        <dd>is a low-level box structure that parameterizes the display and evaluation     of the boxes $box_i$ .
    </dl>
    """

    attributes = ("HoldAllComplete", "Protected", "ReadProtected")


class Row(Builtin):
    """
    <dl>
    <dt>'Row[{$expr$, ...}]'
        <dd>formats several expressions inside a 'RowBox'.
    </dl>
    """

    def apply_makeboxes(self, items, sep, f, evaluation):
        """MakeBoxes[Row[{items___}, sep_:""],
            f:StandardForm|TraditionalForm|OutputForm]"""

        items = items.get_sequence()
        if not isinstance(sep, String):
            sep = MakeBoxes(sep, f)
        if len(items) == 1:
            return MakeBoxes(items[0], f)
        else:
            result = []
            for index, item in enumerate(items):
                if index > 0 and not sep.same(String("")):
                    result.append(sep)
                result.append(MakeBoxes(item, f))
            return RowBox(Expression(SymbolList, *result))


def is_constant(list):
    if list:
        return all(item == list[0] for item in list[1:])
    return True


class GridBox(BoxConstruct):
    r"""
    <dl>
    <dt>'GridBox[{{...}, {...}}]'
        <dd>is a box construct that represents a sequence of boxes
        arranged in a grid.
    </dl>

    #> Grid[{{a,bc},{d,e}}, ColumnAlignments:>Symbol["Rig"<>"ht"]]
     = a   bc
     .
     . d   e

    #> TeXForm@Grid[{{a,bc},{d,e}}, ColumnAlignments->Left]
     = \begin{array}{ll} a & \text{bc}\\ d & e\end{array}

    #> TeXForm[TableForm[{{a,b},{c,d}}]]
     = \begin{array}{cc} a & b\\ c & d\end{array}

    # >> MathMLForm[TableForm[{{a,b},{c,d}}]]
    #  = ...
    """

    options = {"ColumnAlignments": "Center"}

    def get_array(self, leaves, evaluation):
        options = self.get_option_values(leaves=leaves[1:], evaluation=evaluation)
        if not leaves:
            raise BoxConstructError
        expr = leaves[0]
        if not expr.has_form("List", None):
            if not all(leaf.has_form("List", None) for leaf in expr.leaves):
                raise BoxConstructError
        items = [leaf.leaves for leaf in expr.leaves]
        if not is_constant([len(row) for row in items]):
            raise BoxConstructError
        return items, options

    def boxes_to_tex(self, leaves=None, **box_options) -> str:
        if not leaves:
            leaves = self._leaves
        evaluation = box_options.get("evaluation")
        items, options = self.get_array(leaves, evaluation)
        new_box_options = box_options.copy()
        new_box_options["inside_list"] = True
        column_alignments = options["System`ColumnAlignments"].get_name()
        try:
            column_alignments = {
                "System`Center": "c",
                "System`Left": "l",
                "System`Right": "r",
            }[column_alignments]
        except KeyError:
            # invalid column alignment
            raise BoxConstructError
        column_count = 0
        for row in items:
            column_count = max(column_count, len(row))
        result = r"\begin{array}{%s} " % (column_alignments * column_count)
        for index, row in enumerate(items):
            result += " & ".join(
                item.evaluate(evaluation).boxes_to_tex(**new_box_options)
                for item in row
            )
            if index != len(items) - 1:
                result += "\\\\ "
        result += r"\end{array}"
        return result

    def boxes_to_mathml(self, leaves=None, **box_options) -> str:
        if not leaves:
            leaves = self._leaves
        evaluation = box_options.get("evaluation")
        items, options = self.get_array(leaves, evaluation)
        attrs = {}
        column_alignments = options["System`ColumnAlignments"].get_name()
        try:
            attrs["columnalign"] = {
                "System`Center": "center",
                "System`Left": "left",
                "System`Right": "right",
            }[column_alignments]
        except KeyError:
            # invalid column alignment
            raise BoxConstructError
        joined_attrs = " ".join(
            '{0}="{1}"'.format(name, value) for name, value in attrs.items()
        )
        result = "<mtable {0}>\n".format(joined_attrs)
        new_box_options = box_options.copy()
        new_box_options["inside_list"] = True
        for row in items:
            result += "<mtr>"
            for item in row:
                result += "<mtd {0}>{1}</mtd>".format(
                    joined_attrs,
                    item.evaluate(evaluation).boxes_to_mathml(**new_box_options),
                )
            result += "</mtr>\n"
        result += "</mtable>"
        return result

    def boxes_to_text(self, leaves=None, **box_options) -> str:
        if not leaves:
            leaves = self._leaves
        evaluation = box_options.get("evaluation")
        items, options = self.get_array(leaves, evaluation)
        result = ""
        if not items:
            return ""
        widths = [0] * len(items[0])
        cells = [
            [
                item.evaluate(evaluation).boxes_to_text(**box_options).splitlines()
                for item in row
            ]
            for row in items
        ]
        for row in cells:
            for index, cell in enumerate(row):
                if index >= len(widths):
                    raise BoxConstructError
                for line in cell:
                    widths[index] = max(widths[index], len(line))
        for row_index, row in enumerate(cells):
            if row_index > 0:
                result += "\n"
            k = 0
            while True:
                line_exists = False
                line = ""
                for cell_index, cell in enumerate(row):
                    if len(cell) > k:
                        line_exists = True
                        text = cell[k]
                    else:
                        text = ""
                    line += text
                    if cell_index < len(row) - 1:
                        line += " " * (widths[cell_index] - len(text))
                        # if cell_index < len(row) - 1:
                        line += "   "
                if line_exists:
                    result += line + "\n"
                else:
                    break
                k += 1
        return result


class Grid(Builtin):
    """
    <dl>
    <dt>'Grid[{{$a1$, $a2$, ...}, {$b1$, $b2$, ...}, ...}]'
        <dd>formats several expressions inside a 'GridBox'.
    </dl>

    >> Grid[{{a, b}, {c, d}}]
     = a   b
     .
     . c   d
    """

    options = GridBox.options

    def apply_makeboxes(self, array, f, evaluation, options) -> Expression:
        """MakeBoxes[Grid[array_?MatrixQ, OptionsPattern[Grid]],
            f:StandardForm|TraditionalForm|OutputForm]"""
        return GridBox(
            Expression(
                "List",
                *(
                    Expression(
                        "List",
                        *(Expression(SymbolMakeBoxes, item, f) for item in row.leaves)
                    )
                    for row in array.leaves
                )
            ),
            *options_to_rules(options)
        )


#        return Expression('GridBox',Expression('List', *(Expression('List', *(Expression('MakeBoxes', item, f) for item in row.leaves)) for row in array.leaves)),            *options_to_rules(options))


class TableForm(Builtin):
    """
    <dl>
    <dt>'TableForm[$expr$]'
        <dd>displays $expr$ as a table.
    </dl>

    >> TableForm[Array[a, {3,2}],TableDepth->1]
     = {a[1, 1], a[1, 2]}
     .
     . {a[2, 1], a[2, 2]}
     .
     . {a[3, 1], a[3, 2]}

    A table of Graphics:
    >> Table[Style[Graphics[{EdgeForm[{Black}], RGBColor[r,g,b], Rectangle[]}], ImageSizeMultipliers->{0.2, 1}], {r,0,1,1/2}, {g,0,1,1/2}, {b,0,1,1/2}] // TableForm
     = -Graphics-   -Graphics-   -Graphics-
     .
     . -Graphics-   -Graphics-   -Graphics-
     .
     . -Graphics-   -Graphics-   -Graphics-
     .
     . -Graphics-   -Graphics-   -Graphics-
     .
     . -Graphics-   -Graphics-   -Graphics-
     .
     . -Graphics-   -Graphics-   -Graphics-
     .
     . -Graphics-   -Graphics-   -Graphics-
     .
     . -Graphics-   -Graphics-   -Graphics-
     .
     . -Graphics-   -Graphics-   -Graphics-

    #> TableForm[{}]
     = #<--#
    """

    options = {"TableDepth": "Infinity"}

    def apply_makeboxes(self, table, f, evaluation, options):
        """MakeBoxes[%(name)s[table_, OptionsPattern[%(name)s]],
            f:StandardForm|TraditionalForm|OutputForm]"""

        dims = len(get_dimensions(table, head=Symbol("List")))
        depth = self.get_option(options, "TableDepth", evaluation).unformatted
        depth = expr_min((Integer(dims), depth))
        depth = depth.get_int_value()
        if depth is None:
            evaluation.message(self.get_name(), "int")
            return

        if depth <= 0:
            return Expression(SymbolMakeBoxes, table, f)
        elif depth == 1:
            return GridBox(
                Expression(
                    "List",
                    *(
                        Expression(SymbolList, Expression(SymbolMakeBoxes, item, f))
                        for item in table.leaves
                    )
                )
            )
            # return Expression(
            #    'GridBox', Expression('List', *(
            #        Expression('List', Expression('MakeBoxes', item, f))
            #        for item in table.leaves)))
        else:
            new_depth = Expression(SymbolRule, Symbol("TableDepth"), depth - 2)

            def transform_item(item):
                if depth > 2:
                    return Expression(self.get_name(), item, new_depth)
                else:
                    return item

            return GridBox(
                Expression(
                    "List",
                    *(
                        Expression(
                            "List",
                            *(
                                Expression(SymbolMakeBoxes, transform_item(item), f)
                                for item in row.leaves
                            )
                        )
                        for row in table.leaves
                    )
                )
            )


class MatrixForm(TableForm):
    """
    <dl>
    <dt>'MatrixForm[$m$]'
        <dd>displays a matrix $m$, hiding the underlying list
        structure.
    </dl>

    >> Array[a,{4,3}]//MatrixForm
     = a[1, 1]   a[1, 2]   a[1, 3]
     .
     . a[2, 1]   a[2, 2]   a[2, 3]
     .
     . a[3, 1]   a[3, 2]   a[3, 3]
     .
     . a[4, 1]   a[4, 2]   a[4, 3]

    ## Issue #182
    #> {{2*a, 0},{0,0}}//MatrixForm
     = 2 a   0
     .
     . 0     0
    """

    def apply_makeboxes_matrix(self, table, f, evaluation, options):
        """MakeBoxes[%(name)s[table_, OptionsPattern[%(name)s]],
            f:StandardForm|TraditionalForm]"""

        result = super(MatrixForm, self).apply_makeboxes(table, f, evaluation, options)
        if result.get_head_name() == "System`GridBox":
            return Expression(
                "RowBox", Expression(SymbolList, String("("), result, String(")"))
            )
        return result


class Superscript(Builtin):
    """
    <dl>
    <dt>'Superscript[$x$, $y$]'
        <dd>displays as $x$^$y$.
    </dl>

    >> Superscript[x,3] // TeXForm
     = x^3
    """

    rules = {
        "MakeBoxes[Superscript[x_, y_], f:StandardForm|TraditionalForm]": (
            "SuperscriptBox[MakeBoxes[x, f], MakeBoxes[y, f]]"
        )
    }


class SuperscriptBox(Builtin):
    pass


class Subscript(Builtin):
    """
    <dl>
    <dt>'Subscript[$a$, $i$]'
        <dd>displays as $a_i$.
    </dl>

    >> Subscript[x,1,2,3] // TeXForm
     = x_{1,2,3}
    """

    def apply_makeboxes(self, x, y, f, evaluation) -> Expression:
        "MakeBoxes[Subscript[x_, y__], f:StandardForm|TraditionalForm]"

        y = y.get_sequence()
        return Expression(
            "SubscriptBox", Expression(SymbolMakeBoxes, x, f), *list_boxes(y, f)
        )


class SubscriptBox(Builtin):
    pass


class Subsuperscript(Builtin):
    """
    <dl>
    <dt>'Subsuperscript[$a$, $b$, $c$]'
        <dd>displays as $a_b^c$.
    </dl>

    >> Subsuperscript[a, b, c] // TeXForm
     = a_b^c
    """

    rules = {
        "MakeBoxes[Subsuperscript[x_, y_, z_], "
        "f:StandardForm|TraditionalForm]": (
            "SubsuperscriptBox[MakeBoxes[x, f], MakeBoxes[y, f], " "MakeBoxes[z, f]]"
        )
    }


class SubsuperscriptBox(Builtin):
    pass


class Postfix(BinaryOperator):
    """
    <dl>
    <dt>'$x$ // $f$'
        <dd>is equivalent to '$f$[$x$]'.
    </dl>

    >> b // a
     = a[b]
    >> c // b // a
     = a[b[c]]

    The postfix operator '//' is parsed to an expression before evaluation:
    >> Hold[x // a // b // c // d // e // f]
     = Hold[f[e[d[c[b[a[x]]]]]]]
    """

    operator = "//"
    operator_display = None
    precedence = 70
    grouping = "Left"


class Prefix(BinaryOperator):
    """
    <dl>
    <dt>'$f$ @ $x$'
        <dd>is equivalent to '$f$[$x$]'.
    </dl>

    >> a @ b
     = a[b]
    >> a @ b @ c
     = a[b[c]]
    >> Format[p[x_]] := Prefix[{x}, "*"]
    >> p[3]
     = *3
    >> Format[q[x_]] := Prefix[{x}, "~", 350]
    >> q[a+b]
     = ~(a + b)
    >> q[a*b]
     = ~a b
    >> q[a]+b
     = b + ~a

    The prefix operator '@' is parsed to an expression before evaluation:
    >> Hold[a @ b @ c @ d @ e @ f @ x]
     = Hold[a[b[c[d[e[f[x]]]]]]]
    """

    operator = "@"
    operator_display = None
    precedence = 640
    grouping = "Right"


class Infix(Builtin):
    """
    <dl>
    <dt>'Infix[$expr$, $oper$, $prec$, $assoc$]'
        <dd>displays $expr$ with the infix operator $oper$, with
        precedence $prec$ and associativity $assoc$.
    </dl>

    'Infix' can be used with 'Format' to display certain forms with
    user-defined infix notation:
    >> Format[g[x_, y_]] := Infix[{x, y}, "#", 350, Left]
    >> g[a, g[b, c]]
     = a # (b # c)
    >> g[g[a, b], c]
     = a # b # c
    >> g[a + b, c]
     = (a + b) # c
    >> g[a * b, c]
     = a b # c
    >> g[a, b] + c
     = c + a # b
    >> g[a, b] * c
     = c (a # b)

    >> Infix[{a, b, c}, {"+", "-"}]
     = a + b - c

    #> Format[r[items___]] := Infix[If[Length[{items}] > 1, {items}, {ab}], "~"]
    #> r[1, 2, 3]
     = 1 ~ 2 ~ 3
    #> r[1]
     = ab
    """


class NonAssociative(Builtin):
    """
    <dl>
    <dt>'NonAssociative'
        <dd>is used with operator formatting constructs to specify a
        non-associative operator.
    </dl>
    """


class Left(Builtin):
    """
    <dl>
    <dt>'Left'
        <dd>is used with operator formatting constructs to specify a
        left-associative operator.
    </dl>
    """


class Right(Builtin):
    """
    <dl>
    <dt>'Right'
        <dd>is used with operator formatting constructs to specify a
        right-associative operator.
    </dl>
    """


class Center(Builtin):
    """
    <dl>
    <dt>'Center'
        <dd>is used with the 'ColumnAlignments' option to 'Grid' or
        'TableForm' to specify a centered column.
    </dl>
    """


class StringForm(Builtin):
    """
    <dl>
    <dt>'StringForm[$str$, $expr1$, $expr2$, ...]'
        <dd>displays the string $str$, replacing placeholders in $str$
        with the corresponding expressions.
    </dl>

    >> StringForm["`1` bla `2` blub `` bla `2`", a, b, c]
     = a bla b blub c bla b
    """

    def apply_makeboxes(self, s, args, f, evaluation):
        """MakeBoxes[StringForm[s_String, args___],
            f:StandardForm|TraditionalForm|OutputForm]"""

        s = s.value
        args = args.get_sequence()
        result = []
        pos = 0
        last_index = 0
        for match in re.finditer(r"(\`(\d*)\`)", s):
            start, end = match.span(1)
            if match.group(2):
                index = int(match.group(2))
            else:
                index = last_index + 1
            if index > last_index:
                last_index = index
            if start > pos:
                result.append(String(s[pos:start]))
            pos = end
            if 1 <= index <= len(args):
                arg = args[index - 1]
                result.append(MakeBoxes(arg, f))
        if pos < len(s):
            result.append(String(s[pos:]))
        return RowBox(Expression(SymbolList, *result))


class Message(Builtin):
    """
    <dl>
    <dt>'Message[$symbol$::$msg$, $expr1$, $expr2$, ...]'
        <dd>displays the specified message, replacing placeholders in
        the message text with the corresponding expressions.
    </dl>

    >> a::b = "Hello world!"
     = Hello world!
    >> Message[a::b]
     : Hello world!
    >> a::c := "Hello `1`, Mr 00`2`!"
    >> Message[a::c, "you", 3 + 4]
     : Hello you, Mr 007!
    """

    attributes = ("HoldFirst",)

    messages = {
        "name": "Message name `1` is not of the form symbol::name or symbol::name::language."
    }

    def apply(self, symbol, tag, params, evaluation):
        "Message[MessageName[symbol_Symbol, tag_String], params___]"

        params = params.get_sequence()
        evaluation.message(symbol.name, tag.value, *params)
        return Symbol("Null")


def check_message(expr) -> bool:
    "checks if an expression is a valid message"
    if expr.has_form("MessageName", 2):
        symbol, tag = expr.get_leaves()
        if symbol.get_name() and tag.get_string_value():
            return True
    return False


class Check(Builtin):
    """
    <dl>
    <dt>'Check[$expr$, $failexpr$]'
        <dd>evaluates $expr$, and returns the result, unless messages were generated, in which case it evaluates and $failexpr$ will be returned.
    <dt>'Check[$expr$, $failexpr$, {s1::t1,s2::t2,…}]'
        <dd>checks only for the specified messages.
    </dl>

    Return err when a message is generated:
    >> Check[1/0, err]
     : Infinite expression 1 / 0 encountered.
     = err

    #> Check[1^0, err]
     = 1

    Check only for specific messages:
    >> Check[Sin[0^0], err, Sin::argx]
     : Indeterminate expression 0 ^ 0 encountered.
     = Indeterminate

    >> Check[1/0, err, Power::infy]
     : Infinite expression 1 / 0 encountered.
     = err

    #> Check[1 + 2]
     : Check called with 1 argument; 2 or more arguments are expected.
     = Check[1 + 2]

    #> Check[1 + 2, err, 3 + 1]
     : Message name 3 + 1 is not of the form symbol::name or symbol::name::language.
     = Check[1 + 2, err, 3 + 1]

    #> Check[1 + 2, err, hello]
     : Message name hello is not of the form symbol::name or symbol::name::language.
     = Check[1 + 2, err, hello]

    #> Check[1/0, err, Compile::cpbool]
     : Infinite expression 1 / 0 encountered.
     = ComplexInfinity

    #> Check[{0^0, 1/0}, err]
     : Indeterminate expression 0 ^ 0 encountered.
     : Infinite expression 1 / 0 encountered.
     = err

    #> Check[0^0/0, err, Power::indet]
     : Indeterminate expression 0 ^ 0 encountered.
     : Infinite expression 1 / 0 encountered.
     = err

    #> Check[{0^0, 3/0}, err, Power::indet]
     : Indeterminate expression 0 ^ 0 encountered.
     : Infinite expression 1 / 0 encountered.
     = err

    #> Check[1 + 2, err, {a::b, 2 + 5}]
     : Message name 2 + 5 is not of the form symbol::name or symbol::name::language.
     = Check[1 + 2, err, {a::b, 2 + 5}]

    #> Off[Power::infy]
    #> Check[1 / 0, err]
     = ComplexInfinity

    #> On[Power::infy]
    #> Check[1 / 0, err]
     : Infinite expression 1 / 0 encountered.
     = err
    """

    attributes = ("HoldAll",)

    messages = {
        "argmu": "Check called with 1 argument; 2 or more arguments are expected.",
        "name": "Message name `1` is not of the form symbol::name or symbol::name::language.",
    }

    def apply_1_argument(self, expr, evaluation):
        "Check[expr_]"
        return evaluation.message("Check", "argmu")

    def apply(self, expr, failexpr, params, evaluation):
        "Check[expr_, failexpr_, params___]"

        # Todo: To implement the third form of this function , we need to implement the function $MessageGroups first
        # <dt>'Check[$expr$, $failexpr$, "name"]'
        # <dd>checks only for messages in the named message group.

        def get_msg_list(exprs):
            messages = []
            for expr in exprs:
                if expr.has_form("List", None):
                    messages.extend(get_msg_list(expr.leaves))
                elif check_message(expr):
                    messages.append(expr)
                else:
                    raise Exception(expr)
            return messages

        check_messages = set(evaluation.get_quiet_messages())
        display_fail_expr = False

        params = params.get_sequence()
        if len(params) == 0:
            result = expr.evaluate(evaluation)
            if len(evaluation.out):
                display_fail_expr = True
        else:
            try:
                msgs = get_msg_list(params)
                for x in msgs:
                    check_messages.add(x)
            except Exception as inst:
                evaluation.message("Check", "name", inst.args[0])
                return
            curr_msg = len(evaluation.out)
            result = expr.evaluate(evaluation)
            own_messages = evaluation.out[curr_msg:]
            for out_msg in own_messages:
                if type(out_msg) is not EvaluationMessage:
                    continue
                pattern = Expression(
                    "MessageName", Symbol(out_msg.symbol), String(out_msg.tag)
                )
                if pattern in check_messages:
                    display_fail_expr = True
                    break
        return failexpr if display_fail_expr is True else result


class Quiet(Builtin):
    """
    <dl>
    <dt>'Quiet[$expr$, {$s1$::$t1$, ...}]'
        <dd>evaluates $expr$, without messages '{$s1$::$t1$, ...}' being displayed.
    <dt>'Quiet[$expr$, All]'
        <dd>evaluates $expr$, without any messages being displayed.
    <dt>'Quiet[$expr$, None]'
        <dd>evaluates $expr$, without all messages being displayed.
    <dt>'Quiet[$expr$, $off$, $on$]'
        <dd>evaluates $expr$, with messages $off$ being suppressed, but messages $on$ being displayed.
    </dl>

    >> a::b = "Hello";
    >> Quiet[x+x, {a::b}]
     = 2 x
    >> Quiet[Message[a::b]; x+x, {a::b}]
     = 2 x

    >> Message[a::b]; y=Quiet[Message[a::b]; x+x, {a::b}]; Message[a::b]; y
     : Hello
     : Hello
     = 2 x

    >> Quiet[expr, All, All]
     : Arguments 2 and 3 of Quiet[expr, All, All] should not both be All.
     = Quiet[expr, All, All]
    >> Quiet[x + x, {a::b}, {a::b}]
     : In Quiet[x + x, {a::b}, {a::b}] the message name(s) {a::b} appear in both the list of messages to switch off and the list of messages to switch on.
     = Quiet[x + x, {a::b}, {a::b}]
    """

    attributes = ("HoldAll",)

    messages = {
        "anmlist": (
            "Argument `1` of `2` should be All, None, a message name, "
            "or a list of message names."
        ),
        "allall": "Arguments 2 and 3 of `1` should not both be All.",
        "conflict": (
            "In `1` the message name(s) `2` appear in both the list of "
            "messages to switch off and the list of messages to switch on."
        ),
    }

    rules = {
        "Quiet[expr_]": "Quiet[expr, All]",
        "Quiet[expr_, moff_]": "Quiet[expr, moff, None]",
    }

    def apply(self, expr, moff, mon, evaluation):
        "Quiet[expr_, moff_, mon_]"

        def get_msg_list(expr):
            if check_message(expr):
                expr = Expression(SymbolList, expr)
            if expr.get_name() == "System`All":
                all = True
                messages = []
            elif expr.get_name() == "System`None":
                all = False
                messages = []
            elif expr.has_form("List", None):
                all = False
                messages = []
                for item in expr.leaves:
                    if check_message(item):
                        messages.append(item)
                    else:
                        raise ValueError
            else:
                raise ValueError
            return all, messages

        old_quiet_all = evaluation.quiet_all
        old_quiet_messages = set(evaluation.get_quiet_messages())
        quiet_messages = old_quiet_messages.copy()
        try:
            quiet_expr = Expression("Quiet", expr, moff, mon)
            try:
                off_all, off_messages = get_msg_list(moff)
            except ValueError:
                evaluation.message("Quiet", "anmlist", 2, quiet_expr)
                return
            try:
                on_all, on_messages = get_msg_list(mon)
            except ValueError:
                evaluation.message("Quiet", "anmlist", 2, quiet_expr)
                return
            if off_all and on_all:
                evaluation.message("Quiet", "allall", quiet_expr)
                return
            evaluation.quiet_all = off_all
            conflict = []
            for off in off_messages:
                if off in on_messages:
                    conflict.append(off)
                    break
            if conflict:
                evaluation.message(
                    "Quiet", "conflict", quiet_expr, Expression(SymbolList, *conflict)
                )
                return
            for off in off_messages:
                quiet_messages.add(off)
            for on in on_messages:
                quiet_messages.discard(on)
            if on_all:
                quiet_messages = set()
            evaluation.set_quiet_messages(quiet_messages)

            return expr.evaluate(evaluation)
        finally:
            evaluation.quiet_all = old_quiet_all
            evaluation.set_quiet_messages(old_quiet_messages)


class Off(Builtin):
    """
    <dl>
    <dt>'Off[$symbol$::$tag$]'
        <dd>turns a message off so it is no longer printed.
    </dl>

    >> Off[Power::infy]
    >> 1 / 0
     = ComplexInfinity

    >> Off[Power::indet, Syntax::com]
    >> {0 ^ 0,}
     = {Indeterminate, Null}

    #> Off[1]
     :  Message name 1 is not of the form symbol::name or symbol::name::language.
    #> Off[Message::name, 1]

    #> On[Power::infy, Power::indet, Syntax::com]
    """

    attributes = ("HoldAll",)

    def apply(self, expr, evaluation):
        "Off[expr___]"

        seq = expr.get_sequence()
        quiet_messages = set(evaluation.get_quiet_messages())

        if not seq:
            # TODO Off[s::trace] for all symbols
            return

        for e in seq:
            if isinstance(e, Symbol):
                quiet_messages.add(Expression("MessageName", e, String("trace")))
            elif check_message(e):
                quiet_messages.add(e)
            else:
                evaluation.message("Message", "name", e)
            evaluation.set_quiet_messages(quiet_messages)

        return Symbol("Null")


class On(Builtin):
    """
    <dl>
    <dt>'On[$symbol$::$tag$]'
        <dd>turns a message on for printing.
    </dl>

    >> Off[Power::infy]
    >> 1 / 0
     = ComplexInfinity
    >> On[Power::infy]
    >> 1 / 0
     : Infinite expression 1 / 0 encountered.
     = ComplexInfinity
    """

    # TODO
    """
    #> On[f::x]
     : Message f::x not found.
    """

    attributes = ("HoldAll",)

    def apply(self, expr, evaluation):
        "On[expr___]"

        seq = expr.get_sequence()
        quiet_messages = set(evaluation.get_quiet_messages())

        if not seq:
            # TODO On[s::trace] for all symbols
            return

        for e in seq:
            if isinstance(e, Symbol):
                quiet_messages.discard(Expression("MessageName", e, String("trace")))
            elif check_message(e):
                quiet_messages.discard(e)
            else:
                evaluation.message("Message", "name", e)
            evaluation.set_quiet_messages(quiet_messages)
        return Symbol("Null")


class MessageName(BinaryOperator):
    """
    <dl>
    <dt>'MessageName[$symbol$, $tag$]'</dt>
    <dt>'$symbol$::$tag$'</dt>
        <dd>identifies a message.
    </dl>

    'MessageName' is the head of message IDs of the form 'symbol::tag'.
    >> FullForm[a::b]
     = MessageName[a, "b"]

    The second parameter 'tag' is interpreted as a string.
    >> FullForm[a::"b"]
     = MessageName[a, "b"]
    """

    messages = {"messg": "Message cannot be set to `1`. It must be set to a string."}

    operator = "::"
    precedence = 750
    attributes = ("HoldFirst",)

    default_formats = False

    formats: typing.Dict[str, Any] = {}

    rules = {
        "MakeBoxes[MessageName[symbol_Symbol, tag_String], "
        "f:StandardForm|TraditionalForm|OutputForm]": (
            'RowBox[{MakeBoxes[symbol, f], "::", MakeBoxes[tag, f]}]'
        ),
        "MakeBoxes[MessageName[symbol_Symbol, tag_String], InputForm]": (
            'RowBox[{MakeBoxes[symbol, InputForm], "::", tag}]'
        ),
    }

    def apply(self, symbol, tag, evaluation):
        "MessageName[symbol_Symbol, tag_String]"

        pattern = Expression("MessageName", symbol, tag)
        return evaluation.definitions.get_value(
            symbol.get_name(), "System`Messages", pattern, evaluation
        )


class Syntax(Builtin):
    r"""
    <dl>
    <dt>'Syntax'
        <dd>is a symbol to which all syntax messages are assigned.
    </dl>

    >> 1 +
     : Incomplete expression; more input is needed (line 1 of "<test>").

    >> Sin[1)
     : "Sin[1" cannot be followed by ")" (line 1 of "<test>").

    >> ^ 2
     : Expression cannot begin with "^ 2" (line 1 of "<test>").

    >> 1.5``
     : "1.5`" cannot be followed by "`" (line 1 of "<test>").

    #> (x]
     : "(x" cannot be followed by "]" (line 1 of "<test>").

    #> (x,)
     : "(x" cannot be followed by ",)" (line 1 of "<test>").

    #> {x]
     : "{x" cannot be followed by "]" (line 1 of "<test>").

    #> f[x)
     : "f[x" cannot be followed by ")" (line 1 of "<test>").

    #> a[[x)]
     : "a[[x" cannot be followed by ")]" (line 1 of "<test>").

    #> x /: y , z
     : "x /: y " cannot be followed by ", z" (line 1 of "<test>").

    #> a :: 1
     : "a :: " cannot be followed by "1" (line 1 of "<test>").

    #> a ? b ? c
     : "a ? b " cannot be followed by "? c" (line 1 of "<test>").

    #> \:000G
     : 4 hexadecimal digits are required after \: to construct a 16-bit character (line 1 of "<test>").
     : Expression cannot begin with "\:000G" (line 1 of "<test>").

    #> \:000
     : 4 hexadecimal digits are required after \: to construct a 16-bit character (line 1 of "<test>").
     : Expression cannot begin with "\:000" (line 1 of "<test>").

    #> \009
     : 3 octal digits are required after \ to construct an 8-bit character (line 1 of "<test>").
     : Expression cannot begin with "\009" (line 1 of "<test>").

    #> \00
     : 3 octal digits are required after \ to construct an 8-bit character (line 1 of "<test>").
     : Expression cannot begin with "\00" (line 1 of "<test>").

    #> \.0G
     : 2 hexadecimal digits are required after \. to construct an 8-bit character (line 1 of "<test>").
     : Expression cannot begin with "\.0G" (line 1 of "<test>").

    #> \.0
     : 2 hexadecimal digits are required after \. to construct an 8-bit character (line 1 of "<test>").
     : Expression cannot begin with "\.0" (line 1 of "<test>").

    #> "abc \[fake]"
     : Unknown unicode longname "fake" (line 1 of "<test>").
     = abc \[fake]

    #> a ~ b + c
     : "a ~ b " cannot be followed by "+ c" (line 1 of "<test>").

    #> {1,}
     : Warning: comma encountered with no adjacent expression. The expression will be treated as Null (line 1 of "<test>").
     = {1, Null}
    #> {, 1}
     : Warning: comma encountered with no adjacent expression. The expression will be treated as Null (line 1 of "<test>").
     = {Null, 1}
    #> {,,}
     : Warning: comma encountered with no adjacent expression. The expression will be treated as Null (line 1 of "<test>").
     : Warning: comma encountered with no adjacent expression. The expression will be treated as Null (line 1 of "<test>").
     : Warning: comma encountered with no adjacent expression. The expression will be treated as Null (line 1 of "<test>").
     = {Null, Null, Null}
    """

    # Extension: MMA does not provide lineno and filename in its error messages
    messages = {
        "snthex": r"4 hexadecimal digits are required after \: to construct a 16-bit character (line `4` of `5`).",
        "sntoct1": r"3 octal digits are required after \ to construct an 8-bit character (line `4` of `5`).",
        "sntoct2": r"2 hexadecimal digits are required after \. to construct an 8-bit character (line `4` of `5`).",
        "sntxi": "Incomplete expression; more input is needed (line `4` of `5`).",
        "sntxb": "Expression cannot begin with `1` (line `4` of `5`).",
        "sntxf": "`1` cannot be followed by `2` (line `4` of `5`).",
        "bktwrn": "`1` represents multiplication; use `2` to represent a function (line `4` of `5`).",  # TODO
        "bktmch": "`1` must be followed by `2`, not `3` (line `4` of `5`).",
        "sntue": "Unexpected end of file; probably unfinished expression (line `4` of `5`).",
        "sntufn": "Unknown unicode longname `1` (line `4` of `5`).",
        "com": "Warning: comma encountered with no adjacent expression. The expression will be treated as Null (line `4` of `5`).",
    }


class General(Builtin):
    """
    <dl>
    <dt>'General'
        <dd>is a symbol to which all general-purpose messages are assigned.
    </dl>

    >> General::argr
     = `1` called with 1 argument; `2` arguments are expected.
    >> Message[Rule::argr, Rule, 2]
     : Rule called with 1 argument; 2 arguments are expected.
    """

    messages = {
        "argb": (
            "`1` called with `2` arguments; "
            "between `3` and `4` arguments are expected."
        ),
        "argct": "`1` called with `2` arguments.",
        "argctu": "`1` called with 1 argument.",
        "argr": "`1` called with 1 argument; `2` arguments are expected.",
        "argrx": "`1` called with `2` arguments; `3` arguments are expected.",
        "argx": "`1` called with `2` arguments; 1 argument is expected.",
        "argt": (
            "`1` called with `2` arguments; " "`3` or `4` arguments are expected."
        ),
        "argtu": ("`1` called with 1 argument; `2` or `3` arguments are expected."),
        "base": "Requested base `1` in `2` should be between 2 and `3`.",
        "boxfmt": "`1` is not a box formatting type.",
        "color": "`1` is not a valid color or gray-level specification.",
        "cxt": "`1` is not a valid context name.",
        "divz": "The argument `1` should be nonzero.",
        "digit": "Digit at position `1` in `2` is too large to be used in base `3`.",
        "exact": "Argument `1` is not an exact number.",
        "fnsym": (
            "First argument in `1` is not a symbol " "or a string naming a symbol."
        ),
        "heads": "Heads `1` and `2` are expected to be the same.",
        "ilsnn": (
            "Single or list of non-negative integers expected at " "position `1`."
        ),
        "indet": "Indeterminate expression `1` encountered.",
        "innf": "Non-negative integer or Infinity expected at position `1`.",
        "int": "Integer expected.",
        "intp": "Positive integer expected.",
        "intnn": "Non-negative integer expected.",
        "iterb": "Iterator does not have appropriate bounds.",
        "ivar": "`1` is not a valid variable.",
        "level": ("Level specification `1` is not of the form n, " "{n}, or {m, n}."),
        "locked": "Symbol `1` is locked.",
        "matsq": "Argument `1` is not a non-empty square matrix.",
        "newpkg": "In WL, there is a new package for this.",
        "noopen": "Cannot open `1`.",
        "nord": "Invalid comparison with `1` attempted.",
        "normal": "Nonatomic expression expected.",
        "noval": ("Symbol `1` in part assignment does not have an immediate value."),
        "obspkg": "In WL, this package is obsolete.",
        "openx": "`1` is not open.",
        "optb": "Optional object `1` in `2` is not a single blank.",
        "ovfl": "Overflow occurred in computation.",
        "partd": "Part specification is longer than depth of object.",
        "partw": "Part `1` of `2` does not exist.",
        "plld": "Endpoints in `1` must be distinct machine-size real numbers.",
        "plln": "Limiting value `1` in `2` is not a machine-size real number.",
        "pspec": (
            "Part specification `1` is neither an integer nor " "a list of integer."
        ),
        "seqs": "Sequence specification expected, but got `1`.",
        "setp": "Part assignment to `1` could not be made",
        "setps": "`1` in the part assignment is not a symbol.",
        "span": "`1` is not a valid Span specification.",
        "stream": "`1` is not string, InputStream[], or OutputStream[]",
        "string": "String expected.",
        "sym": "Argument `1` at position `2` is expected to be a symbol.",
        "tag": "Rule for `1` can only be attached to `2`.",
        "take": "Cannot take positions `1` through `2` in `3`.",
        "vrule": (
            "Cannot set `1` to `2`, " "which is not a valid list of replacement rules."
        ),
        "write": "Tag `1` in `2` is Protected.",
        "wrsym": "Symbol `1` is Protected.",
        "ucdec": "An invalid unicode sequence was encountered and ignored.",
        "charcode": "The character encoding `1` is not supported. Use $CharacterEncodings to list supported encodings.",
        # Self-defined messages
        # 'rep': "`1` is not a valid replacement rule.",
        "options": "`1` is not a valid list of option rules.",
        "timeout": "Timeout reached.",
        "syntax": "`1`",
        "invalidargs": "Invalid arguments.",
        "notboxes": "`1` is not a valid box structure.",
        "pyimport": '`1`[] is not available. Your Python installation misses the "`2`" module.',
    }


class Print(Builtin):
    """
    <dl>
    <dt>'Print[$expr$, ...]'
        <dd>prints each $expr$ in string form.
    </dl>

    >> Print["Hello world!"]
     | Hello world!
    >> Print["The answer is ", 7 * 6, "."]
     | The answer is 42.

    #> Print["-Hola\\n-Qué tal?"]
     | -Hola
     . -Qué tal?
    """

    def apply(self, expr, evaluation):
        "Print[expr__]"

        expr = expr.get_sequence()
        expr = Expression("Row", Expression(SymbolList, *expr))
        evaluation.print_out(expr)
        return Symbol("Null")


class FullForm(Builtin):
    """
    <dl>
    <dt>'FullForm[$expr$]'
        <dd>displays the underlying form of $expr$.
    </dl>

    >> FullForm[a + b * c]
     = Plus[a, Times[b, c]]
    >> FullForm[2/3]
     = Rational[2, 3]
    >> FullForm["A string"]
     = "A string"
    """


class StandardForm(Builtin):
    """
    <dl>
    <dt>'StandardForm[$expr$]'
        <dd>displays $expr$ in the default form.
    </dl>

    >> StandardForm[a + b * c]
     = a + b c
    >> StandardForm["A string"]
     = A string
    'StandardForm' is used by default:
    >> "A string"
     = A string
    >> f'[x]
     = f'[x]
    """


class InputForm(Builtin):
    r"""
    <dl>
    <dt>'InputForm[$expr$]'
        <dd>displays $expr$ in an unambiguous form suitable for input.
    </dl>

    >> InputForm[a + b * c]
     = a + b*c
    >> InputForm["A string"]
     = "A string"
    >> InputForm[f'[x]]
     = Derivative[1][f][x]
    >> InputForm[Derivative[1, 0][f][x]]
     = Derivative[1, 0][f][x]
    #> InputForm[2 x ^ 2 + 4z!]
     = 2*x^2 + 4*z!
    #> InputForm["\$"]
     = "\\$"
    """


class OutputForm(Builtin):
    """
    <dl>
    <dt>'OutputForm[$expr$]'
        <dd>displays $expr$ in a plain-text form.
    </dl>

    >> OutputForm[f'[x]]
     = f'[x]
    >> OutputForm[Derivative[1, 0][f][x]]
     = Derivative[1, 0][f][x]
    >> OutputForm["A string"]
     = A string
    >> OutputForm[Graphics[Rectangle[]]]
     = -Graphics-
    """


class MathMLForm(Builtin):
    """
    <dl>
    <dt>'MathMLForm[$expr$]'
        <dd>displays $expr$ as a MathML expression.
    </dl>

    >> MathMLForm[HoldForm[Sqrt[a^3]]]
     = ...

    ## Test cases for Unicode - redo please as a real test
    >> MathMLForm[\\[Mu]]
    = ...

    # This can causes the TeX to fail
    # >> MathMLForm[Graphics[Text["\u03bc"]]]
    #  = ...

    ## The <mo> should contain U+2062 INVISIBLE TIMES
    ## MathMLForm[MatrixForm[{{2*a, 0},{0,0}}]]
    = ...
    """

    def apply_mathml(self, expr, evaluation) -> Expression:
        "MakeBoxes[expr_, MathMLForm]"

        boxes = MakeBoxes(expr).evaluate(evaluation)
        try:
            xml = boxes.boxes_to_mathml(evaluation=evaluation)
        except BoxError:
            evaluation.message(
                "General",
                "notboxes",
                Expression("FullForm", boxes).evaluate(evaluation),
            )
            xml = ""
        is_a_picture = xml[:6] == "<mtext"

        # mathml = '<math><mstyle displaystyle="true">%s</mstyle></math>' % xml
        # #convert_box(boxes)
        query = evaluation.parse("System`$UseSansSerif")
        usesansserif = query.evaluate(evaluation).to_python()
        if not is_a_picture:
            if usesansserif:
                xml = '<mstyle mathvariant="sans-serif">%s</mstyle>' % xml

        mathml = '<math display="block">%s</math>' % xml  # convert_box(boxes)
        return Expression("RowBox", Expression(SymbolList, String(mathml)))


class PythonForm(Builtin):
    """
    <dl>
      <dt>'PythonForm[$expr$]'
      <dd>returns an approximate equivalent of $expr$ in Python, when that is possible. We assume
      that Python has sympy imported. No explicit import will be include in the result.
    </dl>

    >> PythonForm[Infinity]
    = math.inf
    >> PythonForm[Pi]
    = sympy.pi
    >> E // PythonForm
    = sympy.E
    >> {1, 2, 3} // PythonForm
    = [1, 2, 3]
    """

    # >> PythonForm[HoldForm[Sqrt[a^3]]]
    #  = sympy.sqrt{a**3} # or something like this

    def apply_python(self, expr, evaluation) -> Expression:
        "MakeBoxes[expr_, PythonForm]"

        try:
            # from trepan.api import debug; debug()
            python_equivalent = expr.to_python(python_form=True)
        except:
            return
        return StringFromPython(python_equivalent)

    def apply(self, expr, evaluation) -> Expression:
        "PythonForm[expr_]"
        return self.apply_python(expr, evaluation)


class SympyForm(Builtin):
    """
    <dl>
      <dt>'SympyForm[$expr$]'
      <dd>returns an Sympy $expr$ in Python. Sympy is used internally
      to implement a number of Mathics functions, like Simplify.
    </dl>

    >> SympyForm[Pi^2]
    = pi**2
    >> E^2 + 3E // SympyForm
    = exp(2) + 3*E
    """

    def apply_sympy(self, expr, evaluation) -> Expression:
        "MakeBoxes[expr_, SympyForm]"

        try:
            # from trepan.api import debug; debug()
            sympy_equivalent = expr.to_sympy()
        except:
            return
        return StringFromPython(sympy_equivalent)

    def apply(self, expr, evaluation) -> Expression:
        "SympyForm[expr_]"
        return self.apply_sympy(expr, evaluation)


class TeXForm(Builtin):
    r"""
    <dl>
    <dt>'TeXForm[$expr$]'
        <dd>displays $expr$ using TeX math mode commands.
    </dl>

    >> TeXForm[HoldForm[Sqrt[a^3]]]
     = \sqrt{a^3}

    #> {"hi","you"} //InputForm //TeXForm
     = \left\{\text{"hi"}, \text{"you"}\right\}

    #> TeXForm[a+b*c]
     = a+b c
    #> TeXForm[InputForm[a+b*c]]
     = a\text{ + }b*c
    """

    def apply_tex(self, expr, evaluation) -> Expression:
        "MakeBoxes[expr_, TeXForm]"

        boxes = MakeBoxes(expr).evaluate(evaluation)
        try:
            tex = boxes.boxes_to_tex(evaluation=evaluation)

            # Replace multiple newlines by a single one e.g. between asy-blocks
            tex = MULTI_NEWLINE_RE.sub("\n", tex)

            tex = tex.replace(" \uF74c", " \\, d")  # tmp hack for Integrate
        except BoxError:
            evaluation.message(
                "General",
                "notboxes",
                Expression("FullForm", boxes).evaluate(evaluation),
            )
            tex = ""
        return Expression("RowBox", Expression(SymbolList, String(tex)))


class Style(Builtin):
    options = {"ImageSizeMultipliers": "Automatic"}

    rules = {
        "MakeBoxes[Style[expr_, OptionsPattern[Style]], f_]": (
            "StyleBox[MakeBoxes[expr, f], "
            "ImageSizeMultipliers -> OptionValue[ImageSizeMultipliers]]"
        )
    }


class Precedence(Builtin):
    """
    <dl>
    <dt>'Precedence[$op$]'
        <dd>returns the precedence of the built-in operator $op$.
    </dl>

    >> Precedence[Plus]
     = 310.
    >> Precedence[Plus] < Precedence[Times]
     = True

    Unknown symbols have precedence 670:
    >> Precedence[f]
     = 670.
    Other expressions have precedence 1000:
    >> Precedence[a + b]
     = 1000.
    """

    def apply(self, expr, evaluation) -> Real:
        "Precedence[expr_]"

        name = expr.get_name()
        precedence = 1000
        if name:
            builtin = evaluation.definitions.get_definition(name, only_if_exists=True)
            if builtin:
                builtin = builtin.builtin
            if builtin is not None and isinstance(builtin, Operator):
                precedence = builtin.precedence
            else:
                precedence = 670
        return Real(precedence)


class _NumberForm(Builtin):
    """
    Base class for NumberForm, AccountingForm, EngineeringForm, and ScientificForm.
    """

    default_ExponentFunction = None
    default_NumberFormat = None

    messages = {
        "npad": "Value for option NumberPadding -> `1` should be a string or a pair of strings.",
        "dblk": "Value for option DigitBlock should be a positive integer, Infinity, or a pair of positive integers.",
        "npt": "Value for option `1` -> `2` is expected to be a string.",
        "nsgn": "Value for option NumberSigns -> `1` should be a pair of strings or two pairs of strings.",
        "nspr": "Value for option NumberSeparator -> `1` should be a string or a pair of strings.",
        "opttf": "Value of option `1` -> `2` should be True or False.",
        "estep": "Value of option `1` -> `2` is not a positive integer.",
        "iprf": "Formatting specification `1` should be a positive integer or a pair of positive integers.",  # NumberFormat only
        "sigz": "In addition to the number of digits requested, one or more zeros will appear as placeholders.",
    }

    def check_options(self, options, evaluation):
        """
        Checks options are valid and converts them to python.
        """
        result = {}
        for option_name in self.options:
            method = getattr(self, "check_" + option_name)
            arg = options["System`" + option_name]
            value = method(arg, evaluation)
            if value is None:
                return None
            result[option_name] = value
        return result

    def check_DigitBlock(self, value, evaluation):
        py_value = value.get_int_value()
        if value.same(Symbol("Infinity")):
            return [0, 0]
        elif py_value is not None and py_value > 0:
            return [py_value, py_value]
        elif value.has_form("List", 2):
            nleft, nright = value.leaves
            py_left, py_right = nleft.get_int_value(), nright.get_int_value()
            if nleft.same(Symbol("Infinity")):
                nleft = 0
            elif py_left is not None and py_left > 0:
                nleft = py_left
            else:
                nleft = None
            if nright.same(Symbol("Infinity")):
                nright = 0
            elif py_right is not None and py_right > 0:
                nright = py_right
            else:
                nright = None
            result = [nleft, nright]
            if None not in result:
                return result
        return evaluation.message(self.get_name(), "dblk", value)

    def check_ExponentFunction(self, value, evaluation):
        if value.same(Symbol("Automatic")):
            return self.default_ExponentFunction

        def exp_function(x):
            return Expression(value, x).evaluate(evaluation)

        return exp_function

    def check_NumberFormat(self, value, evaluation):
        if value.same(Symbol("Automatic")):
            return self.default_NumberFormat

        def num_function(man, base, exp, options):
            return Expression(value, man, base, exp).evaluate(evaluation)

        return num_function

    def check_NumberMultiplier(self, value, evaluation):
        result = value.get_string_value()
        if result is None:
            evaluation.message(self.get_name(), "npt", "NumberMultiplier", value)
        return result

    def check_NumberPoint(self, value, evaluation):
        result = value.get_string_value()
        if result is None:
            evaluation.message(self.get_name(), "npt", "NumberPoint", value)
        return result

    def check_ExponentStep(self, value, evaluation):
        result = value.get_int_value()
        if result is None or result <= 0:
            return evaluation.message(self.get_name(), "estep", "ExponentStep", value)
        return result

    def check_SignPadding(self, value, evaluation):
        if value.same(Symbol("True")):
            return True
        elif value.same(Symbol("False")):
            return False
        return evaluation.message(self.get_name(), "opttf", value)

    def _check_List2str(self, value, msg, evaluation):
        if value.has_form("List", 2):
            result = [leaf.get_string_value() for leaf in value.leaves]
            if None not in result:
                return result
        return evaluation.message(self.get_name(), msg, value)

    def check_NumberSigns(self, value, evaluation):
        return self._check_List2str(value, "nsgn", evaluation)

    def check_NumberPadding(self, value, evaluation):
        return self._check_List2str(value, "npad", evaluation)

    def check_NumberSeparator(self, value, evaluation):
        py_str = value.get_string_value()
        if py_str is not None:
            return [py_str, py_str]
        return self._check_List2str(value, "nspr", evaluation)


class NumberForm(_NumberForm):
    """
    <dl>
    <dt>'NumberForm[$expr$, $n$]'
        <dd>prints a real number $expr$ with $n$-digits of precision.
    <dt>'NumberForm[$expr$, {$n$, $f$}]'
        <dd>prints with $n$-digits and $f$ digits to the right of the decimal point.
    </dl>

    >> NumberForm[N[Pi], 10]
     = 3.141592654

    >> NumberForm[N[Pi], {10, 5}]
     = 3.14159


    ## Undocumented edge cases
    #> NumberForm[Pi, 20]
     = Pi
    #> NumberForm[2/3, 10]
     = 2 / 3

    ## No n or f
    #> NumberForm[N[Pi]]
     = 3.14159
    #> NumberForm[N[Pi, 20]]
     = 3.1415926535897932385
    #> NumberForm[14310983091809]
     = 14310983091809

    ## Zero case
    #> z0 = 0.0;
    #> z1 = 0.0000000000000000000000000000;
    #> NumberForm[{z0, z1}, 10]
     = {0., 0.×10^-28}
    #> NumberForm[{z0, z1}, {10, 4}]
     = {0.0000, 0.0000×10^-28}

    ## Trailing zeros
    #> NumberForm[1.0, 10]
     = 1.
    #> NumberForm[1.000000000000000000000000, 10]
     = 1.000000000
    #> NumberForm[1.0, {10, 8}]
     = 1.00000000
    #> NumberForm[N[Pi, 33], 33]
     = 3.14159265358979323846264338327950

    ## Correct rounding - see sympy/issues/11472
    #> NumberForm[0.645658509, 6]
     = 0.645659
    #> NumberForm[N[1/7], 30]
     = 0.1428571428571428

    ## Integer case
    #> NumberForm[{0, 2, -415, 83515161451}, 5]
     = {0, 2, -415, 83515161451}
    #> NumberForm[{2^123, 2^123.}, 4, ExponentFunction -> ((#1) &)]
     = {10633823966279326983230456482242756608, 1.063×10^37}
    #> NumberForm[{0, 10, -512}, {10, 3}]
     = {0.000, 10.000, -512.000}

    ## Check arguments
    #> NumberForm[1.5, -4]
     : Formatting specification -4 should be a positive integer or a pair of positive integers.
     = 1.5
    #> NumberForm[1.5, {1.5, 2}]
     : Formatting specification {1.5, 2} should be a positive integer or a pair of positive integers.
     = 1.5
    #> NumberForm[1.5, {1, 2.5}]
     : Formatting specification {1, 2.5} should be a positive integer or a pair of positive integers.
     = 1.5

    ## Right padding
    #> NumberForm[153., 2]
     : In addition to the number of digits requested, one or more zeros will appear as placeholders.
     = 150.
    #> NumberForm[0.00125, 1]
     = 0.001
    #> NumberForm[10^5 N[Pi], {5, 3}]
     : In addition to the number of digits requested, one or more zeros will appear as placeholders.
     = 314160.000
    #> NumberForm[10^5 N[Pi], {6, 3}]
     = 314159.000
    #> NumberForm[10^5 N[Pi], {6, 10}]
     = 314159.0000000000
    #> NumberForm[1.0000000000000000000, 10, NumberPadding -> {"X", "Y"}]
     = X1.000000000

    ## Check options

    ## DigitBlock
    #> NumberForm[12345.123456789, 14, DigitBlock -> 3]
     = 12,345.123 456 789
    #> NumberForm[12345.12345678, 14, DigitBlock -> 3]
     = 12,345.123 456 78
    #> NumberForm[N[10^ 5 Pi], 15, DigitBlock -> {4, 2}]
     = 31,4159.26 53 58 97 9
    #> NumberForm[1.2345, 3, DigitBlock -> -4]
     : Value for option DigitBlock should be a positive integer, Infinity, or a pair of positive integers.
     = 1.2345
    #> NumberForm[1.2345, 3, DigitBlock -> x]
     : Value for option DigitBlock should be a positive integer, Infinity, or a pair of positive integers.
     = 1.2345
    #> NumberForm[1.2345, 3, DigitBlock -> {x, 3}]
     : Value for option DigitBlock should be a positive integer, Infinity, or a pair of positive integers.
     = 1.2345
    #> NumberForm[1.2345, 3, DigitBlock -> {5, -3}]
     : Value for option DigitBlock should be a positive integer, Infinity, or a pair of positive integers.
     = 1.2345

    ## ExponentFunction
    #> NumberForm[12345.123456789, 14, ExponentFunction -> ((#) &)]
     = 1.2345123456789×10^4
    #> NumberForm[12345.123456789, 14, ExponentFunction -> (Null&)]
     = 12345.123456789
    #> y = N[Pi^Range[-20, 40, 15]];
    #> NumberForm[y, 10, ExponentFunction -> (3 Quotient[#, 3] &)]
     =  {114.0256472×10^-12, 3.267763643×10^-3, 93.64804748×10^3, 2.683779414×10^12, 76.91214221×10^18}
    #> NumberForm[y, 10, ExponentFunction -> (Null &)]
     : In addition to the number of digits requested, one or more zeros will appear as placeholders.
     : In addition to the number of digits requested, one or more zeros will appear as placeholders.
     = {0.0000000001140256472, 0.003267763643, 93648.04748, 2683779414000., 76912142210000000000.}

    ## ExponentStep
    #> NumberForm[10^8 N[Pi], 10, ExponentStep -> 3]
     = 314.1592654×10^6
    #> NumberForm[1.2345, 3, ExponentStep -> x]
     : Value of option ExponentStep -> x is not a positive integer.
     = 1.2345
    #> NumberForm[1.2345, 3, ExponentStep -> 0]
     : Value of option ExponentStep -> 0 is not a positive integer.
     = 1.2345
    #> NumberForm[y, 10, ExponentStep -> 6]
     = {114.0256472×10^-12, 3267.763643×10^-6, 93648.04748, 2.683779414×10^12, 76.91214221×10^18}

    ## NumberFormat
    #> NumberForm[y, 10, NumberFormat -> (#1 &)]
     = {1.140256472, 0.003267763643, 93648.04748, 2.683779414, 7.691214221}

    ## NumberMultiplier
    #> NumberForm[1.2345, 3, NumberMultiplier -> 0]
     : Value for option NumberMultiplier -> 0 is expected to be a string.
     = 1.2345
    #> NumberForm[N[10^ 7 Pi], 15, NumberMultiplier -> "*"]
     = 3.14159265358979*10^7

    ## NumberPoint
    #> NumberForm[1.2345, 5, NumberPoint -> ","]
     = 1,2345
    #> NumberForm[1.2345, 3, NumberPoint -> 0]
     : Value for option NumberPoint -> 0 is expected to be a string.
     = 1.2345

    ## NumberPadding
    #> NumberForm[1.41, {10, 5}]
     = 1.41000
    #> NumberForm[1.41, {10, 5}, NumberPadding -> {"", "X"}]
     = 1.41XXX
    #> NumberForm[1.41, {10, 5}, NumberPadding -> {"X", "Y"}]
     = XXXXX1.41YYY
    #> NumberForm[1.41, 10, NumberPadding -> {"X", "Y"}]
     = XXXXXXXX1.41
    #> NumberForm[1.2345, 3, NumberPadding -> 0]
     :  Value for option NumberPadding -> 0 should be a string or a pair of strings.
     = 1.2345
    #> NumberForm[1.41, 10, NumberPadding -> {"X", "Y"}, NumberSigns -> {"-------------", ""}]
     = XXXXXXXXXXXXXXXXXXXX1.41
    #> NumberForm[{1., -1., 2.5, -2.5}, {4, 6}, NumberPadding->{"X", "Y"}]
     = {X1.YYYYYY, -1.YYYYYY, X2.5YYYYY, -2.5YYYYY}

    ## NumberSeparator
    #> NumberForm[N[10^ 5 Pi], 15, DigitBlock -> 3, NumberSeparator -> " "]
     = 314 159.265 358 979
    #> NumberForm[N[10^ 5 Pi], 15, DigitBlock -> 3, NumberSeparator -> {" ", ","}]
     = 314 159.265,358,979
    #> NumberForm[N[10^ 5 Pi], 15, DigitBlock -> 3, NumberSeparator -> {",", " "}]
     = 314,159.265 358 979
    #> NumberForm[N[10^ 7 Pi], 15, DigitBlock -> 3, NumberSeparator -> {",", " "}]
     = 3.141 592 653 589 79×10^7
    #> NumberForm[1.2345, 3, NumberSeparator -> 0]
     :  Value for option NumberSeparator -> 0 should be a string or a pair of strings.
     = 1.2345

    ## NumberSigns
    #> NumberForm[1.2345, 5, NumberSigns -> {"-", "+"}]
     = +1.2345
    #> NumberForm[-1.2345, 5, NumberSigns -> {"- ", ""}]
     = - 1.2345
    #> NumberForm[1.2345, 3, NumberSigns -> 0]
     : Value for option NumberSigns -> 0 should be a pair of strings or two pairs of strings.
     = 1.2345

    ## SignPadding
    #> NumberForm[1.234, 6, SignPadding -> True, NumberPadding -> {"X", "Y"}]
     = XXX1.234
    #> NumberForm[-1.234, 6, SignPadding -> True, NumberPadding -> {"X", "Y"}]
     = -XX1.234
    #> NumberForm[-1.234, 6, SignPadding -> False, NumberPadding -> {"X", "Y"}]
     = XX-1.234
    #> NumberForm[-1.234, {6, 4}, SignPadding -> False, NumberPadding -> {"X", "Y"}]
     = X-1.234Y

    ## 1-arg, Option case
    #> NumberForm[34, ExponentFunction->(Null&)]
     = 34

    ## zero padding integer x0.0 case
    #> NumberForm[50.0, {5, 1}]
     = 50.0
    #> NumberForm[50, {5, 1}]
     = 50.0

    ## Rounding correctly
    #> NumberForm[43.157, {10, 1}]
     = 43.2
    #> NumberForm[43.15752525, {10, 5}, NumberSeparator -> ",", DigitBlock -> 1]
     = 4,3.1,5,7,5,3
    #> NumberForm[80.96, {16, 1}]
     = 81.0
    #> NumberForm[142.25, {10, 1}]
     = 142.3
    """

    options = {
        "DigitBlock": "Infinity",
        "ExponentFunction": "Automatic",
        "ExponentStep": "1",
        "NumberFormat": "Automatic",
        "NumberMultiplier": '"×"',
        "NumberPadding": '{"", "0"}',
        "NumberPoint": '"."',
        "NumberSeparator": '{",", " "}',
        "NumberSigns": '{"-", ""}',
        "SignPadding": "False",
    }

    @staticmethod
    def default_ExponentFunction(value):
        n = value.get_int_value()
        if -5 <= n <= 5:
            return Symbol("Null")
        else:
            return value

    @staticmethod
    def default_NumberFormat(man, base, exp, options):
        py_exp = exp.get_string_value()
        if py_exp:
            mul = String(options["NumberMultiplier"])
            return Expression(
                "RowBox",
                Expression(
                    SymbolList, man, mul, Expression("SuperscriptBox", base, exp)
                ),
            )
        else:
            return man

    def apply_list_n(self, expr, n, evaluation, options) -> Expression:
        "NumberForm[expr_?ListQ, n_, OptionsPattern[NumberForm]]"
        options = [
            Expression("RuleDelayed", Symbol(key), value)
            for key, value in options.items()
        ]
        return Expression(
            "List",
            *[Expression("NumberForm", leaf, n, *options) for leaf in expr.leaves]
        )

    def apply_list_nf(self, expr, n, f, evaluation, options) -> Expression:
        "NumberForm[expr_?ListQ, {n_, f_}, OptionsPattern[NumberForm]]"
        options = [
            Expression("RuleDelayed", Symbol(key), value)
            for key, value in options.items()
        ]
        return Expression(
            "List",
            *[
                Expression("NumberForm", leaf, Expression(SymbolList, n, f), *options)
                for leaf in expr.leaves
            ]
        )

    def apply_makeboxes(self, expr, form, evaluation, options={}):
        """MakeBoxes[NumberForm[expr_, OptionsPattern[NumberForm]],
            form:StandardForm|TraditionalForm|OutputForm]"""

        fallback = Expression(SymbolMakeBoxes, expr, form)

        py_options = self.check_options(options, evaluation)
        if py_options is None:
            return fallback

        if isinstance(expr, Integer):
            py_n = len(str(abs(expr.get_int_value())))
        elif isinstance(expr, Real):
            if expr.is_machine_precision():
                py_n = 6
            else:
                py_n = dps(expr.get_precision())
        else:
            py_n = None

        if py_n is not None:
            return number_form(expr, py_n, None, evaluation, py_options)
        return Expression(SymbolMakeBoxes, expr, form)

    def apply_makeboxes_n(self, expr, n, form, evaluation, options={}):
        """MakeBoxes[NumberForm[expr_, n_?NotOptionQ, OptionsPattern[NumberForm]],
            form:StandardForm|TraditionalForm|OutputForm]"""

        fallback = Expression(SymbolMakeBoxes, expr, form)

        py_n = n.get_int_value()
        if py_n is None or py_n <= 0:
            evaluation.message("NumberForm", "iprf", n)
            return fallback

        py_options = self.check_options(options, evaluation)
        if py_options is None:
            return fallback

        if isinstance(expr, (Integer, Real)):
            return number_form(expr, py_n, None, evaluation, py_options)
        return Expression(SymbolMakeBoxes, expr, form)

    def apply_makeboxes_nf(self, expr, n, f, form, evaluation, options={}):
        """MakeBoxes[NumberForm[expr_, {n_, f_}, OptionsPattern[NumberForm]],
            form:StandardForm|TraditionalForm|OutputForm]"""

        fallback = Expression(SymbolMakeBoxes, expr, form)

        nf = Expression(SymbolList, n, f)
        py_n = n.get_int_value()
        py_f = f.get_int_value()
        if py_n is None or py_n <= 0 or py_f is None or py_f < 0:
            evaluation.message("NumberForm", "iprf", nf)
            return fallback

        py_options = self.check_options(options, evaluation)
        if py_options is None:
            return fallback

        if isinstance(expr, (Integer, Real)):
            return number_form(expr, py_n, py_f, evaluation, py_options)
        return Expression(SymbolMakeBoxes, expr, form)


class BaseForm(Builtin):
    """
    <dl>
    <dt>'BaseForm[$expr$, $n$]'
        <dd>prints numbers in $expr$ in base $n$.
    </dl>

    >> BaseForm[33, 2]
     = 100001_2

    >> BaseForm[234, 16]
     = ea_16

    >> BaseForm[12.3, 2]
     = 1100.01001100110011001_2

    >> BaseForm[-42, 16]
     = -2a_16

    >> BaseForm[x, 2]
     = x

    >> BaseForm[12, 3] // FullForm
     = BaseForm[12, 3]

    Bases must be between 2 and 36:
    >> BaseForm[12, -3]
     : Positive machine-sized integer expected at position 2 in BaseForm[12, -3].
     : MakeBoxes[BaseForm[12, -3], OutputForm] is not a valid box structure.
    >> BaseForm[12, 100]
     : Requested base 100 must be between 2 and 36.
     : MakeBoxes[BaseForm[12, 100], OutputForm] is not a valid box structure.

    #> BaseForm[0, 2]
     = 0_2
    #> BaseForm[0.0, 2]
     = 0.0_2

    #> BaseForm[N[Pi, 30], 16]
     = 3.243f6a8885a308d313198a2e_16
    """

    messages = {
        "intpm": (
            "Positive machine-sized integer expected at position 2 in "
            "BaseForm[`1`, `2`]."
        ),
        "basf": "Requested base `1` must be between 2 and 36.",
    }

    def apply_makeboxes(self, expr, n, f, evaluation):
        """MakeBoxes[BaseForm[expr_, n_],
            f:StandardForm|TraditionalForm|OutputForm]"""

        base = n.get_int_value()

        if base <= 0:
            evaluation.message("BaseForm", "intpm", expr, n)
            return

        if isinstance(expr, PrecisionReal):
            x = expr.to_sympy()
            p = reconstruct_digits(expr.get_precision())
        elif isinstance(expr, MachineReal):
            x = expr.get_float_value()
            p = reconstruct_digits(machine_precision)
        elif isinstance(expr, Integer):
            x = expr.get_int_value()
            p = 0
        else:
            return Expression(SymbolMakeBoxes, expr, f)

        try:
            val = convert_base(x, base, p)
        except ValueError:
            return evaluation.message("BaseForm", "basf", n)

        if f.get_name() == "System`OutputForm":
            return from_python("%s_%d" % (val, base))
        else:
            return Expression("SubscriptBox", String(val), String(base))
