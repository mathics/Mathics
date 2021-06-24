# -*- coding: utf-8 -*-
"""
Unit tests from builtins ... algebra.py
"""
from .helper import check_evaluation


def test_coefficient():
    for str_expr, str_expected in (
        # Form 1: Coefficent[expr, form]
        (
            "Coefficient[(x + 2)/(y - 3) + (x + 3)/(y - 2), z, 0]",
            "(2 + x) / (-3 + y) + (3 + x) / (-2 + y)",
        ),
        (
            "Coefficient[y (x - 2)/((y^2 - 9)) + (x + 5)/(y + 2), x]",
            "y / (-9 + y ^ 2) + 1 / (2 + y)",
        ),
        # MMA returns better one: (-2 + x) / (-9 + y ^ 2)"
        (
            "Coefficient[y (x - 2)/((y^2 - 9)) + (x + 5)/(y + 2), y]",
            "x / (-9 + y ^ 2) - 2 / (-9 + y ^ 2)",
        ),
        (
            "Coefficient[y (x - 2)/((y - 3)(y + 3)) + (x + 5)/(y + 2), x]",
            "y / (-9 + y ^ 2) + 1 / (2 + y)",
        ),
        # MMA returns better one: (-2 + x) / ((-3 + y) (3 + y))
        (
            "Coefficient[y (x - 2)/((y - 3)(y + 3)) + (x + 5)/(y + 2), y]",
            "x / (-9 + y ^ 2) - 2 / (-9 + y ^ 2)",
        ),
        (
            "Coefficient[x^3 - 2 x/y + 3 x z, y]",
            "0",
        ),
        # Form 2: Coefficent[expr, form, n]
        (
            "Coefficient[x^2 + axy^2 - bSin[c], c]",
            "0",
        ),
    ):
        check_evaluation(str_expr, str_expected)


def test_coefficient_list():
    for str_expr, str_expected in (
        # Form 1: Coefficent[expr, form]
        (
            "CoefficientList[x^2 + a x y^2 - b Sin[c], y]",
            "{-b Sin[c] + x ^ 2, 0, a x}",
        ),
        (
            "CoefficientList[0, x]",
            "{}",
        ),
        (
            "CoefficientList[1, x]",
            "{1}",
        ),
        (
            "CoefficientList[x + 1, {}]",
            "1 + x",
        ),
        # Form 2 CoefficientList[poly, {var1, var2, ...}]
        (
            "CoefficientList[a x^2 + b y^3 + c x + d y + 5, {x}]",
            "{5 + b y ^ 3 + d y, c, a}",
        ),
        (
            "CoefficientList[a x^2 + b y^3 + c x + d y + 5, {}]",
            "5 + a x ^ 2 + b y ^ 3 + c x + d y",
        ),
        (
            "CoefficientList[a x^2 + b y^3 + c x + d y + 5, {x, y + 1}]",
            "{{5 + b y ^ 3 + d y}, {c}, {a}}",
        ),
        (
            "CoefficientList[a x^2 + b y^3 + c x + d y + 5, {x + 1, y}]",
            "{{5 + a x ^ 2 + c x, d, 0, b}}",
        ),
        (
            "CoefficientList[a x^2 + b y^3 + c x + d y + 5, {x + 1, y + 1}]",
            "{{5 + a x ^ 2 + b y ^ 3 + c x + d y}}",
        ),
        (
            "CoefficientList[y (x - 2)/((z - 3) (z + 3)) + (x + 5)/(z + 2), {x, y}]",
            "{{5 / (2 + z), -2 / (-9 + z ^ 2)}, {1 / (2 + z), 1 / (-9 + z ^ 2)}}",
        ),
        (
            "CoefficientList[0, {x, y}]",
            "{}",
        ),
        (
            "CoefficientList[1, {x, y}]",
            "{{1}}",
        ),
    ):
        check_evaluation(str_expr, str_expected)


def test_exponent():
    for str_expr, str_expected in (
        (
            "Exponent[(x^2 + 1)^3 - 1, x, List]",
            "{2, 4, 6}",
        ),
        (
            "Exponent[5 x^2 - 3 x + 7, x, List]",
            "{0, 1, 2}",
        ),
        (
            "Exponent[(x + 1) + (x + 1)^2, x, List]",
            "{0, 1, 2}",
        ),
        (
            "Exponent[(x + 3 y  - 2 z)^3 * (5 y + z), {x, y}, List]",
            "{{0, 1, 2, 3}, {0, 1, 2, 3, 4}}",
        ),
        (
            'Exponent[(x + 3 y - 2 z)^3*(5 y + z), {"x", "y"}, List]',
            "{{0}, {0}}",
        ),
        (
            "Exponent[(x + 3 y - 2 z)^3*(5 y + z), {}]",
            "{}",
        ),
        (
            "Exponent[x^a + b y^3 + c x + 2 y^e + 5, {x, y}, List]",
            "{{0, 1, a}, {0, 3, e}}",
        ),
        (
            "Exponent[x^2 / y^3, {x, y}]",
            "{2, -3}",
        ),
        (
            "Exponent[(x + 2)/(y - 3) + (x + 3)/(y - 2), {x, y, z}, List]",
            "{{0, 1}, {0}, {0}}",
        ),
        (
            "Exponent[x + 6 x^3 y^2 - 3/((x^2) (y^2)), {x, y}, List]",
            "{{-2, 1, 3}, {-2, 0, 2}}",
        ),
        (
            "Exponent[x^5 Sin[x^2] + x * x^3 Cos[x], x, List]",
            "{4, 5}",
        ),
        (
            "Exponent[x^5 Sin[x^2] + y Cos[y^2] + Log[x^3] + 6 y^4, {x, y}, List]",
            "{{0, 5}, {0, 1, 4}}",
        ),
    ):
        check_evaluation(str_expr, str_expected)


def test_factor_terms_list():
    for str_expr, str_expected in (
        (
            "f = 3 (-1 + 2 x) (-1 + y) (1 - a); FactorTermsList[f, y]",
            "{-3, 1 - a - 2 x + 2 a x, -1 + y}",
        ),
        (
            "FactorTermsList[f, {x, y}]",
            "{-3, -1 + a, -1 + y, -1 + 2 x}",
        ),
        (
            "FactorTermsList[f, {y, x}]",
            "{-3, -1 + a, -1 + 2 x, -1 + y}",
        ),
        (
            "FactorTermsList[f, {x, y, z}]",
            "{-3, -1 + a, 1, -1 + y, -1 + 2 x}",
        ),
        (
            "FactorTermsList[f, {x, y, z, t}]",
            "{-3, -1 + a, 1, 1, -1 + y, -1 + 2 x}",
        ),
        (
            "FactorTermsList[f, 3/5]",
            "{-3, -1 + a - 2 a x - a y + 2 x + y - 2 x y + 2 a x y}",
        ),
        (
            "FactorTermsList[f, {x, 3, y}]",
            "{-3, -1 + a, -1 + y, -1 + 2 x}",
        ),
        (
            "FactorTermsList[f/c]",
            "{-3, -1 / c + a / c - 2 a x / c - a y / c + 2 x / c + y / c - 2 x y / c + 2 a x y / c}",
        ),
        (
            "FactorTermsList[f/c, x] == FactorTermsList[f/c, {x, y}]",
            "True",
        ),
        (
            "g = Sin[x]*Cos[y]*(1 - 2 a)",
            "Cos[y] (1 - 2 a) Sin[x]",
        ),
        (
            "FactorTermsList[g]",
            "{-1, 2 a Cos[y] Sin[x] - Cos[y] Sin[x]}",
        ),
        (
            "FactorTermsList[g, x]",
            "{-1, 2 a Cos[y] Sin[x] - Cos[y] Sin[x]}",
        ),
        (
            "FactorTermsList[g, x] == FactorTermsList[g, y] == FactorTermsList[g, {x, y}]",
            "True",
        ),
        (
            "v = 3 * y * (1 - b) a^x",
            "3 y (1 - b) a ^ x",
        ),
        (
            "FactorTermsList[v]",
            "{-3, -y a ^ x + b y a ^ x}",
        ),
        (
            "FactorTermsList[v, x]",
            "{-3, -y a ^ x + b y a ^ x}",
        ),
        (
            "FactorTermsList[v, y]",
            "{-3, b a ^ x - a ^ x, y}",
        ),
        (
            "FactorTermsList[7]",
            "{7, 1}",
        ),
        (
            "FactorTermsList[0]",
            "{1, 0}",
        ),
        (
            "FactorTermsList[-3]",
            "{-3, 1}",
        ),
        (
            "FactorTermsList[7, {y, x}]",
            "{7, 1}",
        ),
        (
            "FactorTermsList[7, x]",
            "{7, 1}",
        ),
        (
            "FactorTermsList[7 - I, x]",
            "{7 - I, 1}",
        ),
        (
            "FactorTermsList[(x - 1) (1 + a), {c, d}]",
            "{1, -1 - a + x + a x}",
        ),
        (
            "FactorTermsList[(x - 1) (1 + a), {c, x}]",
            "{1, 1 + a, -1 + x, 1}",
        ),
        (
            "FactorTermsList[(x - 1) (1 + a), {}] == FactorTermsList[(x - 1) (1 + a)]",
            "True",
        ),
        (
            "FactorTermsList[x]",
            "{1, x}",
        ),
        (
            'FactorTermsList["x"]',
            "{1, x}",
        ),
    ):
        check_evaluation(str_expr, str_expected)


def test_simplify():
    for str_expr, str_expected in (
        (
            "Simplify[a*x^2+b*x^2]",
            "x ^ 2 (a + b)",
        ),
        # triggers TypeError in sympy.simplify
        (
            "Clear[f]; x f[{y}] // Simplify",
            "x f[{y}]",
        ),
    ):
        check_evaluation(str_expr, str_expected)
