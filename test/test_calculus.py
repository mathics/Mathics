# -*- coding: utf-8 -*-
from .helper import check_evaluation


def test_calculus():
    for str_expr, str_expected, message in (
        (
            "Solve[{(7+x)*ma == 167, (5+x)*mb == 167, (7+5)*(ma+mb) == 334}, {ma, mb, x}]",
            "{{ma -> 1169 / 12 - 167 Sqrt[37] / 12, mb -> -835 / 12 + 167 Sqrt[37] / 12, x -> Sqrt[37]}, {ma -> 1169 / 12 + 167 Sqrt[37] / 12, mb -> -835 / 12 - 167 Sqrt[37] / 12, x -> -Sqrt[37]}}",
            "Issue63",
        ),
        (
            "Solve[{(7+x)*ma == 167, (5+x)*mb == 167, (7+5)*(ma+mb) == 334}, {x, ma, mb}]",
            "{{x -> -Sqrt[37], ma -> 1169 / 12 + 167 Sqrt[37] / 12, mb -> -835 / 12 - 167 Sqrt[37] / 12}, {x -> Sqrt[37], ma -> 1169 / 12 - 167 Sqrt[37] / 12, mb -> -835 / 12 + 167 Sqrt[37] / 12}}",
            "Issue 208",
        ),
        (
            "Solve[x + 1 == 2, x]",
            "{{x -> 1}}",
            "",
        ),
        (
            "Solve[{a == 1, 0==0, b==I, 1==1},{a}]",
            "{{a -> 1}}",
            "Issue #1168",
        ),
    ):
        check_evaluation(str_expr, str_expected, message)
