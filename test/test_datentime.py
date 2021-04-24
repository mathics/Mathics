# -*- coding: utf-8 -*-
from .helper import evaluate

import sys
import time

if sys.platform not in ("win32",):
    def test_datentime():
        str_expr = "TimeConstrained[1+2; TimeRemaining[], 0.9]"
        result = evaluate(str_expr)
        assert result is None or 0 < result.to_python() < 9


if sys.platform not in ("darwin", "win32",):
    # FIXME figure out why this doesn't work on macos and win32
    def test_timeconstrained1():
        #
        str_expr1 = "a=1.; TimeConstrained[Do[Pause[.1];a=a+1,{1000}],1]"
        result = evaluate(str_expr1)
        str_expected = "$Aborted"
        expected = evaluate(str_expected)
        assert result == expected
        time.sleep(1)
        assert evaluate("a").to_python() == 10
