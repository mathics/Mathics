# -*- coding: utf-8 -*-
from .helper import check_evaluation


def test_import():
    eaccent = "\xe9"
    for str_expr, str_expected, message in (
        (
         """StringTake[Import["ExampleData/Middlemarch.txt", CharacterEncoding -> "ISO8859-1"], {49, 69}]""",
         f"des plaisirs pr{eaccent}sents",
         "accented characters in Import"
        ),
    ):
        check_evaluation(str_expr, str_expected, message)
