import time
from mathics.core.parser import parse, MathicsSingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
from mathics.session import MathicsSession

session = MathicsSession(add_builtin=True, catch_interrupt=False)

def check_evaluation(str_expr: str, str_expected: str, message=""):
    """Helper function to test that a WL expression against
    its results"""
    result = session.evaluate("ToString[" + str_expr + "]").value
    print("result=",result)
    expected = session.evaluate("ToString[" + str_expected + "]").value
    print("expected=",expected)
    print(time.asctime())
    print(message)
    if message:
        assert result == expected, message
    else:
        assert result == expected
