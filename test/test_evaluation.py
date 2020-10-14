from mathics.core.parser import parse, SingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
import pytest


definitions = Definitions(add_builtin=True)
evaluation = Evaluation(definitions=definitions, catch_interrupt=False)


def _evaluate(str_expression):
    expr = parse(definitions, SingleLineFeeder(str_expression))
    return expr.evaluate(evaluation)


@pytest.mark.parametrize('str_expr,str_expected', [
    (r'Table[F[x],{x,1,3}]', '{F[1],F[2],F[3]}'),
    (r'Table[F[x],{x,{1,2,3}}]', '{F[1],F[2],F[3]}'),
    (r's={1,2,3};Table[F[x],{x,s}]', '{F[1],F[2],F[3]}'),
    (r's={x,1,3};Table[F[x],s]', '{F[1],F[2],F[3]}'),
    (r's={x,{1,2,3}};Table[F[x],s]', '{F[1],F[2],F[3]}'),
])
def test_evaluation(str_expr, str_expected):
    result = _evaluate(str_expr)
    expected = _evaluate(str_expected)

    assert result == expected



def test_exit():
    try:
        _evaluate("Exit[-37]")
    except SystemExit as e:
        assert e.code == -37


def test_exit():
    try:
        _evaluate("Quit[-37]")
    except SystemExit as e:
        assert e.code == -37

