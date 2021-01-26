from mathics.session import MathicsSession

import pytest

session = MathicsSession(add_builtin=True, catch_interrupt=False)


@pytest.mark.parametrize(
    "str_expr,str_expected",
    [
        # DeleteCases tests
        (r"DeleteCases[A,{_,_}]", "{{{1, 2, 3}, {a, b}, {1, 2, 3}}}"),
        (r"DeleteCases[A,{_,_},1]", "{{{1, 2, 3}, {a, b}, {1, 2, 3}}}"),
        (r"DeleteCases[A,{_,_},1,1]", "{{{1, 2, 3}, {a, b}, {1, 2, 3}}, {10, 11}}"),
        (r"DeleteCases[A,{_,_},2]", "{{{1, 2, 3}, {1, 2, 3}}}"),
        (r"DeleteCases[A,{_,_},3]", "{{{1, 2, 3}, {1, 2, 3}}}"),
        (r"DeleteCases[A,{_,_},{2}]", "{{{1, 2, 3}, {1, 2, 3}},{a, b}, {10, 11}}"),
        (r"DeleteCases[A,{_,_},{2,3}]", "{{{1, 2, 3}, {1, 2, 3}}, {a, b}, {10, 11}}"),
        (r"DeleteCases[A,{_,_},{1,3},2]", "{{{1, 2, 3}, {1, 2, 3}},{10, 11}}"),
    ],
)
def test_evaluation(str_expr: str, str_expected: str, message=""):
    global session
    session.evaluate("A={{{1, 2, 3}, {a, b}, {1, 2, 3}}, {a, b}, {10, 11}};")
    result = session.evaluate(str_expr)
    expected = session.evaluate(str_expected)

    if message:
        assert result == expected, message
    else:
        assert result == expected
