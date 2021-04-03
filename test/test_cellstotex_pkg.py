from .helper import session, check_evaluation

session.evaluate('Needs["TeXUtilities`CellsToTeX`CellsToTeX`"]')

def test_cells_to_tex():
    for str_expr, str_expected, message in (
        (
            "CellsToTeXPreamble[]",
            '"\\mmaSet{morefv={gobble=2,},}\\n"',
            "CellsToTeXPreamble",
        ),
        # (
        #     'boxes=MakeBoxes[Pi];\
        #          cell = Cell[BoxData[boxes], "Input"];res=Catch[CellToTeX[cell, Style->"Input"]]',
        #     '"\\begin{mmaCell}{Input}\n  \\pi\n\\end{mmaCell}"', None
        # ),
    ):
        check_evaluation(str_expr, str_expected, message)
