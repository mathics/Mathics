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
    (r'SetAttributes[or, Orderless] ; or[p, p] /. or[p_, p_] :> p', 'p'),
    (r'SetAttributes[or, Orderless] ; or[p, p, p] /. or[p_, p_] :> p', 'or[p, p, p]'),
    (r'SetAttributes[or, Orderless] ; or[p] /. or[p_, p_] :> p', 'or[p]')
])
def test_orderless(str_expr, str_expected):
    result = _evaluate(str_expr)
    expected = _evaluate(str_expected)

    assert result == expected
