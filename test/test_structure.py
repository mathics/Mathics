# -*- coding: utf-8 -*-
from .helper import check_evaluation


def test_flatten():
    for str_expr, str_expected in (
        (
            "Flatten[{{{111, 112, 113}, {121, 122}}, {{211, 212}, {221, 222, 223}}}, {{3}, {1}, {2}}]",
            "{{{111, 121}, {211, 221}}, {{112, 122}, {212, 222}}, {{113}, {223}}}",
        ),
        (
            "Flatten[{{{1, 2, 3}, {4, 5}}, {{6, 7}, {8, 9,  10}}}, {{3}, {1}, {2}}]",
            "{{{1, 4}, {6, 8}}, {{2, 5}, {7, 9}}, {{3}, {10}}}",
        ),
        (
            "Flatten[{{{1, 2, 3}, {4, 5}}, {{6, 7}, {8, 9, 10}}}, {{2}, {1, 3}}]",
            "{{1, 2, 3, 6, 7}, {4, 5, 8, 9, 10}}",
        ),
        ("Flatten[{{1, 2}, {3,4}}, {1, 2}]", "{1, 2, 3, 4}"),
        (
            "m = {{{1, 2}, {3}}, {{4}, {5, 6}}}; Flatten[m, {2}]",
            "{{{1, 2}, {4}}, {{3}, {5, 6}}}",
        ),
        ("Flatten[m, {{2}}]", "{{{1, 2}, {4}}, {{3}, {5, 6}}}"),
        ("Flatten[m, {{2}, {1}}]", "{{{1, 2}, {4}}, {{3}, {5, 6}}}"),
        ("Flatten[m, {{2}, {1}, {3}}]", "{{{1, 2}, {4}}, {{3}, {5, 6}}}"),
        # Tests from Issue #251
        (
            "m = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}; Flatten[m, {1}]",
            "{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}",
        ),
        ("Flatten[m, {2}]", "{{1, 4, 7}, {2, 5, 8}, {3, 6, 9}}"),
        ("Flatten[m, {2, 1}]", "{1, 4, 7, 2, 5, 8, 3, 6, 9}"),
        #
        ("Flatten[{{1, 2}, {3, {4}}}, {{1, 2}}]", "{1, 2, 3, {4}}"),
        ("Flatten[p[1, p[2], p[3]]]", "p[1, 2, 3]"),
        ("Flatten[p[1, p[2], p[3]], 2]", "p[1, 2, 3]"),
    ):
        check_evaluation(str_expr, str_expected)


def test_operate():
    for str_expr, str_expected in (
        ("Operate[p, f[a][b][c]]", "p[f[a][b]][c]"),
        (
            "Operate[p, f[a][b][c], 1]",
            "p[f[a][b]][c]",
        ),
        (
            "Operate[p, f[a][b][c], 2]",
            "p[f[a]][b][c]",
        ),
        ("Operate[p, f[a][b][c], 3]", "p[f][a][b][c]"),
        ("Operate[p, f[a][b][c], 4]", "f[a][b][c]"),
        ("Operate[p, f]", "f"),
        ("Operate[p, f, 0]", "p[f]"),
    ):
        check_evaluation(
            str_expr, str_expected, to_string_expected=False, to_python_expected=True
        )


def test_sort():
    for str_expr, str_expected in (
        # Test ordering of monomials:
        ("a^2f+a b f", "a ^ 2 f + a b f"),
        ("a^4 b^2 + e^3 b", "a ^ 4 b ^ 2 + b e ^ 3"),
        ("Expand[(1+x)^3 y]", "y + 3 x y + 3 x ^ 2 y + x ^ 3 y"),
        ("Expand[(x+y)^3]", "x ^ 3 + 3 x ^ 2 y + 3 x y ^ 2 + y ^ 3"),
        ("y+x y^(1/2)", "x Sqrt[y] + y"),
        # Numeric parts:
        (
            "1+Pi+Pi^2+Sin[9/4*Pi]+x+x^2+Sin[x+x^2]",
            "1 + Pi + Pi ^ 2 + Sqrt[2] / 2 + x + x ^ 2 + Sin[x + x ^ 2]",
        ),
    ):
        check_evaluation(
            str_expr, str_expected, to_string_expected=False, to_python_expected=True
        )


def test_through():
    for str_expr, str_expected in (
        ("Through[p[f, g][x, y]]", "p[f[x, y], g[x, y]]"),
        ("Through[p[f, g][]]", "p[f[], g[]]"),
        ("Through[p[f, g]]", "Through[p[f, g]]"),
        ("Through[f[][x]]", "f[]"),
    ):
        check_evaluation(
            str_expr, str_expected, to_string_expected=False, to_python_expected=True
        )
