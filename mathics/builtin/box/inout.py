# -*- coding: utf-8 -*-

from mathics.builtin.base import (
    BoxConstruct,
    BoxConstructError,
    Builtin,
    )

def is_constant(list):
    if list:
        return all(item == list[0] for item in list[1:])
    return True


class ButtonBox(Builtin):
    """
    <dl>
    <dt>'ButtonBox[$boxes$]'
        <dd> is a low-level box construct that represents a button in a
    notebook expression.
    </dl>
    """

    attributes = ("Protected", "ReadProtected")


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


class InterpretationBox(Builtin):
    """
    <dl>
    <dt>'InterpretationBox[{...}]'
        <dd> is a low-level box construct that displays as
    boxes but is interpreted on input as expr.
    </dl>
    """

    attributes = ("HoldAllComplete", "Protected", "ReadProtected")


class SubscriptBox(Builtin):
    pass


class SubsuperscriptBox(Builtin):
    pass


class SuperscriptBox(Builtin):
    pass


class RowBox(Builtin):
    """
    <dl>
    <dt>'RowBox[{...}]'
        <dd>is a box construct that represents a sequence of boxes
        arranged in a horizontal row.
    </dl>
    """

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

class TooltipBox(Builtin):
    """
    <dl>
    <dt>'TooltipBox[{...}]'
        <dd>undocumented...
    </dl>
    """
