import time
from mathics.core.parser import parse, MathicsSingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
from mathics.session import MathicsSession

session = MathicsSession(add_builtin=True, catch_interrupt=False)

def check_evaluation(str_expr: str, str_expected: str, message=""):
    """Helper function to test that a WL expression against
    its results"""
    print("expr: ",str_expr)
    try:
        result = session.evaluate(str_expr)
    except NotImplementedError as e: 
        print(str_expr, " raised a not implemented exception:", e)
        return
    # print("    result=",result)
    try:
        expected = session.evaluate(str_expected)
    except NotImplementedError as e: 
        print(str_expected, " raised a not implemented exception:", e)
        return        
    # print("    expected=",expected)
    print(time.asctime())
    #print(message)
    if message:
        assert result == expected, message
    else:
        assert result == expected
