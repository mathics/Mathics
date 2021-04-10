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
        str_expr = f'ToString[{str_expr}]'
    if to_string_expected:
        str_expected = f'ToString[{str_expected}]'
    # print(str_expr)
    # print(str_expected)

    result = session.evaluate(str_expr).value
    print("result=", result)
    expected = session.evaluate(str_expected).value
    print("expected=", expected)
    print(time.asctime())
    print(message)
    if message:
        assert result == expected, message
    else:
        assert result == expected
