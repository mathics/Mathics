# -*- coding: utf-8 -*-
from .helper import session

import sys


def test_datentime():
    str_expr = "TimeConstrained[1+2; TimeRemaining[], 0.9]"
    result = session.evaluate(str_expr)
    assert result is None or 0 < result.to_python() < 9
