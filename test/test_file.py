from mathics.core.parser import parse, SingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
import pytest


definitions = Definitions(add_builtin=True)
evaluation = Evaluation(definitions=definitions, catch_interrupt=False)


def _evaluate(str_expression):
    expr = parse(definitions, SingleLineFeeder(str_expression))
    return expr.evaluate(evaluation)


def test_get_and_put():
    temp_directory = _evaluate("$TemporaryDirectory").to_python()
    if len(temp_directory)<3:
        return
    temp_directory = temp_directory[1:-1]
    temp_filename = f"{temp_directory}/testfile"
    print(temp_filename)
    result = _evaluate(f"40! >> {temp_filename}").to_python()
    assert result is None

    result = _evaluate(f"<< {temp_filename}")
    assert result == _evaluate("40!")

    result = _evaluate(f"DeleteFile[\"{temp_filename}\"]").to_python()
    assert result is None






