# -*- coding: utf-8 -*-
import time
from mathics.session import MathicsSession

session = MathicsSession(add_builtin=True, catch_interrupt=False)


def check_evaluation(
    str_expr: str,
    str_expected: str,
    message="",
    to_string_expr=True,
    to_string_expected=True,
):
    """Helper function to test Mathics expression against
    its results"""
    if to_string_expr:
        str_expr = f"ToString[{str_expr}]"
        result = session.evaluate(str_expr).value
    else:
        result = session.evaluate(str_expr)

    if to_string_expected:
        str_expected = f"ToString[{str_expected}]"
        expected = session.evaluate(str_expected).value
    else:
        expected = session.evaluate(str_expected)

    print(time.asctime())
    print(message)
    if message:
        print((result, expected))
        assert result == expected, message
    else:
        print((result, expected))
        assert result == expected
