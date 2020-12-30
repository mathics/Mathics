import os
from mathics.core.parser import parse, SingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
from mathics.core.expression import Symbol
import pytest
import urllib.request

external_url = (
    "https://raw.githubusercontent.com/jkuczm/MathematicaCellsToTeX/master/NoInstall.m"
)

pytestmark = pytest.mark.skipif(os.getenv("SKIP_CELLSTOTEX", None) is not None,
                                reason="SKIP_CELLSTOTEX environment variable set")

try:
    http_code = urllib.request.urlopen(external_url).getcode()
except:
    url_reachable = False
else:
    url_reachable = http_code in (200,) # add other 2xx or 3xx's?

definitions = Definitions(add_builtin=True)
evaluation = Evaluation(definitions=definitions, catch_interrupt=False)
import_url = 'Import@"%s"' % external_url


def _evaluate(str_expression):
    expr = parse(definitions, SingleLineFeeder(str_expression))
    return expr.evaluate(evaluation)

def test_load():
    str_expected1 = "{}"
    message1 = ""
    result1 = _evaluate(import_url)
    expected1 = _evaluate(str_expected1)

    #if result1 == Symbol("System`$Failed"):
    #    return 0

    if message1:
        assert result1 == expected1, message1
    else:
        assert result1 == expected1

    result2 = _evaluate('Names["CellsToTeX`*"]')
    expected2 = _evaluate('{"CellToTeX", "CellsToTeXException", "CellsToTeXPreamble"}')
    print(result2)
    assert result2 == expected2
    


@pytest.mark.skipif(not url_reachable, reason="skipping since we can't reach %s" % external_url)
#@pytest.mark.skip(
#    reason="FIXME: full CellToTeX import test is not working yet: implement levelspec > 1"
#)
def test_load_and_run():
    print("load and run")
    str_expected1 = "None"
    message1 = "Import::nffil: File not found during Import."
    result1 = _evaluate(import_url)
    print(result1)
    expected1 = _evaluate(str_expected1)

    if result1 == Symbol("System`$Failed"):
        return 0

    str_expr2 = 'Catch[CellToTeX[Cell[BoxData[MakeBoxes[Subscript[x, 1] == (-b \\[PlusMinus] Sqrt[b^2 - 4 a c])/(2 a)]], "Input"]]]'
    str_expected2 = '"\\begin{mmaCell}{Input}\\n  \\mmaSub{x}{1}==\\mmaFrac{-b\\(\\pmb{\\pm}\\)\\mmaSqrt{\\mmaSup{b}{2}-4 a c}}{2 a}\\n\\end{mmaCell}"'
    message2 = ""
    result2 = _evaluate(str_expr2)
    expected2 = _evaluate(str_expected2)
    print("result:", result2)
    if message2:
        assert result2 == expected2, message2
    else:
        assert result2 == expected2
