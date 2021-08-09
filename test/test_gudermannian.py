# -*- coding: utf-8 -*-
"""
Unit tests from autoload/GudermannianRules.m
"""

from .helper import check_evaluation


def test_gudermannian():
    for str_expr, str_expected, message in (
        (
            "Gudermannian[4.2]",
            "1.54081",
            "https://reference.wolfram.com/language/ref/Gudermannian.html",
        ),
    ):
        check_evaluation(str_expr, str_expected, message)
