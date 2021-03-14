#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import unittest
import pytest


from mathics.session import MathicsSession


session = MathicsSession()

try:
    import scipy.integrate
    usescipy = True
except:
    usescipy = False


if usescipy:
    methods = ["Automatic", "Romberg", "Internal", "NQuadrature"]
    
    generic_tests_for_nintegrate =     [
     (r'NIntegrate[x^2, {x,0,1}, {method} ]', r'1/3.', ''),
     (r'NIntegrate[x^2 y^(-1.+1/3.), {x,0,1},{y,0,1}, {method}]', r'1.', ''),
    ]
    
    tests_for_nintegrate = sum([
        [ (tst[0].replace("{method}", "Method->"+ method),
           tst[1],tst[2])
          for tst in generic_tests_for_nintegrate]
        for method in methods], [])
else:
    tests_for_nintegrate =     [(r'NIntegrate[x^2, {x,0,1}]', r'1/3.', ''),
                                (r'NIntegrate[x^2 y^(-.5), {x,0,1},{y,0,1}]', r'1.', ''),
    ]
    
@pytest.mark.parametrize(
    "str_expr, str_expected, msg",
    tests_for_nintegrate
)
def test_nintegrate(str_expr: str, str_expected: str, msg: str, message=""):
    result = session.evaluate(str_expr)
    expected = session.evaluate(str_expected)
    if msg:
        assert result == expected, msg
    else:
        assert result == expected

