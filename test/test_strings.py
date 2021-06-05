# -*- coding: utf-8 -*-
from .helper import check_evaluation


def test_string_matchq():
    for str_expr, str_expected in (
        ('StringMatchQ[".12", NumberString]', "True"),
        ('StringMatchQ["12.", NumberString]', "True"),
        ('StringMatchQ["12.31.31", NumberString]', "False"),
        ('StringMatchQ[".", NumberString]', "False"),
        ('StringMatchQ["-1.23", NumberString]', "True"),
        ('StringMatchQ["+.2", NumberString]', "True"),
        ('StringMatchQ["1.2e4", NumberString]', "False"),
        ('StringMatchQ["abc", "ABC"]', "False"),
        ('StringMatchQ["abc", "ABC", IgnoreCase -> True]', "True"),
        # ('StringMatchQ["abc1", LetterCharacter]', "False"),
    ):
        check_evaluation(str_expr, str_expected)


def test_digitq():
    for str_expr, str_expected in (
        ('DigitQ[""]', "True"),
        ('DigitQ["."]', "False"),
        ("DigitQ[1==2]", "False"),
        ("DigitQ[a=1]", "False"),
    ):
        check_evaluation(str_expr, str_expected)
