# -*- coding: utf-8 -*-
from .helper import check_evaluation


def test_to_infinity():
    for str_expr, str_expected, message in (
        (
            "PythonForm[Infinity]",
            '"math.inf"',
            "Infinity",
        ),
        (
            "PythonForm[{1, 2, 3, 4}]",
            '"[1, 2, 3, 4]"',
            "Simple List of integers",
        ),
        (
            "Pi // PythonForm",
            '"sympy.pi"',
            "Pi",
        ),
    ):
        check_evaluation(str_expr, str_expected, message)
