# -*- coding: utf-8 -*-
from .helper import check_evaluation, evaluate

import sys
import time

if sys.platform not in ("win32",):

    def test_datentime():
        str_expr = "TimeConstrained[1+2; TimeRemaining[], 0.9]"
        result = evaluate(str_expr)
        assert result is None or 0 < result.to_python() < 9


if sys.platform not in (
    "darwin",
    "win32",
):
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


def test_datelist():
    for str_expr, str_expected in (
        ('DateList["2016-09-09"]', "{2016, 9, 9, 0, 0, 0.}"),
        # strptime should ignore leading 0s
        (
            'DateList[{"6/6/91", {"Day", "Month", "YearShort"}}]',
            "{1991, 6, 6, 0, 0, 0.}",
        ),
        (
            'DateList[{"6/06/91", {"Day", "Month", "YearShort"}}]',
            "{1991, 6, 6, 0, 0, 0.}",
        ),
        (
            'DateList[{"06/06/91", {"Day", "Month", "YearShort"}}]',
            "{1991, 6, 6, 0, 0, 0.}",
        ),
        (
            'DateList[{"06/6/91", {"Day", "Month", "YearShort"}}]',
            "{1991, 6, 6, 0, 0, 0.}",
        ),
        ('DateList[{"5/18", {"Month", "Day"}}][[1]] == DateList[][[1]]', "True"),
        ("Quiet[DateList[abc]]", "DateList[abc]"),
    ):
        check_evaluation(str_expr, str_expected)


def test_datestring():
    for str_expr, str_expected in (
        ## Check Leading 0s
        # (
        #  'DateString[{1979, 3, 14}, {"DayName", "  ", "MonthShort", "-", "YearShort"}',
        # "Wednesday  3-79"
        # ),
        (
            "DateString[{1979, 3, 4}]",
            "Sun 4 Mar 1979 00:00:00",
        ),
        ('DateString[{"5/19"}]', "5/19"),
        ('DateString["2000-12-1", "Year"]', "2000"),
    ):
        check_evaluation(
            str_expr, str_expected, to_string_expected=False, to_python_expected=True
        )
