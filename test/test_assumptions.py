# -*- coding: utf-8 -*-
from .helper import check_evaluation
import pytest

list_test_assumptions_integrate = [
    (
        "Integrate[x^n, {x, 0, 1}]",
        "Piecewise[{{1 / (1 + n), 1 + Re[n] > 0 && n > -Infinity && n < Infinity && n != -1}}, Infinity]",
        "This is so complicated due the sympy result is wrong...",
    ),
    (
        "Assuming[0 < n < 1, Integrate[x^n, {x, 0, 1}]]",
        "Piecewise[{{1 / (1 + n), 1 + Re[n] > 0 && n > -Infinity && n < Infinity && n != -1}}, Infinity]",
        "",
    ),
    (
        "Assuming[0 < Re[n] + 1, Integrate[x^n, {x, 0, 1}]]",
        "Piecewise[{{1 / (1 + n), n > -Infinity && n < Infinity && n != -1}}, Infinity]",
        "",
    ),
    ("Assuming[n == 1, Integrate[x^n, {x, 0, 1}]]", "1 / 2", ""),
    ("Assuming[n == 2, Integrate[x^n, {x, 0, 1}]]", "1 / 3", ""),
    ("Assuming[n == -1, Integrate[x^n, {x, 0, 1}]]", "Infinity", ""),
    #    ("Assuming[1<n<3, Integrate[x^n, {x, 0, 1}]]", "x^(n+1)/(n+1)", ""),
    #    ("Assuming[Or[n==1, n==2], Integrate[x^n, {x, 0, 1}]]", "x^(n+1)/(n+1)", ""),
    #    ("Assuming[Or[n>2, n>=3], Integrate[x^n, {x, 0, 1}]]", "x^(n+1)/(n+1)", ""),
]

list_test_assumptions_simplify = [
    (
        "Simplify[a==b || a!=b]",
        "True",
        "",
    ),
    (
        "Simplify[a==b && a!=b]",
        "False",
        "",
    ),
    (
        "Simplify[a<=b && a>b]",
        "False",
        "",
    ),
    (
        "Simplify[a==b, ! a!=b]",
        "True",
        "",
    ),
    (
        "Simplify[a==b,   a!=b]",
        "False",
        "",
    ),
    (
        "Simplify[a > b,  {a==4}]",
        "b < 4",
        "",
    ),
    (
        "Simplify[And[a>b, b<a]]",
        "a>b",
        "",
    ),
    (
        "Simplify[Or[a>b, a<b]]",
        "a!=b",
        "",
    ),
    (
        "Simplify[Or[a>b, b<a]]",
        "a>b",
        "",
    ),
    (
        "Simplify[a>b,  {b<=a}]",
        "a>b",
        "",
    ),
]


@pytest.mark.parametrize(
    ("str_expr", "str_expected", "message"),
    list_test_assumptions_integrate,
)
@pytest.mark.xfail
def test_assumptions_integrate(str_expr, str_expected, message):
    check_evaluation(str_expr, str_expected)


@pytest.mark.parametrize(
    ("str_expr", "str_expected", "message"),
    list_test_assumptions_simplify,
)
@pytest.mark.xfail
def test_assumptions_simplify(str_expr, str_expected, message):
    check_evaluation(str_expr, str_expected)
