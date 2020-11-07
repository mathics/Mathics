from mathics.core.parser import parse, SingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
from mathics.core.expression import Symbol
import pytest


definitions = Definitions(add_builtin=True)
evaluation = Evaluation(definitions=definitions, catch_interrupt=False)


def _evaluate(str_expression):
    expr = parse(definitions, SingleLineFeeder(str_expression))
    return expr.evaluate(evaluation)


def test_load():
    str_expr1 = 'Import@"https://raw.githubusercontent.com/jkuczm/MathematicaCellsToTeX/master/NoInstall.m"'
    str_expected1 = "{}"
    message1 = ""
    result1 = _evaluate(str_expr1)
    expected1 = _evaluate(str_expected1)

    if result1 == Symbol("System`$Failed"):
        return 0

    if message1:
        assert result1 == expected1, message1
    else:
        assert result1 == expected1


def test_load_and_run():
    str_expr1 = 'Import@"https://raw.githubusercontent.com/jkuczm/MathematicaCellsToTeX/master/NoInstall.m"'
    str_expected1 = "None"
    message1 = "Import::nffil: File not found during Import."
    result1 = _evaluate(str_expr1)
    print(result1)
    expected1 = _evaluate(str_expected1)

    if result1 == Symbol("System`$Failed"):
        return 0

    str_expr2 = 'CellToTeX[Cell[BoxData[MakeBoxes[Subscript[x, 1] == (-b \\[PlusMinus] Sqrt[b^2 - 4 a c])/(2 a)]], "Input"]]'
    str_expected2 = '"\\begin{mmaCell}{Input}\\n  \\mmaSub{x}{1}==\\mmaFrac{-b\\(\\pmb{\\pm}\\)\\mmaSqrt{\\mmaSup{b}{2}-4 a c}}{2 a}\\n\\end{mmaCell}"'
    message2 = ""
    result2 = _evaluate(str_expr2)
    expected2 = _evaluate(str_expected2)

    if message2:
        assert result2 == expected2, message2
    else:
        assert result2 == expected2

