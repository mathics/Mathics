from mathics.core.parser import parse, SingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
import pytest


definitions = Definitions(add_builtin=True)
evaluation = Evaluation(definitions=definitions, catch_interrupt=False)


def _evaluate(str_expression):
    expr = parse(definitions, SingleLineFeeder(str_expression))
    return expr.evaluate(evaluation)


@pytest.mark.parametrize(
    "str_expr,str_expected",
    [
        # Table tests
        (r"Table[F[x],{x,1,3}]", "{F[1],F[2],F[3]}"),
        (r"Table[F[x],{x,{1,2,3}}]", "{F[1],F[2],F[3]}"),
        (r"s={1,2,3};Table[F[x],{x,s}]", "{F[1],F[2],F[3]}"),
        (r"s={x,1,3};Table[F[x],s]", "{F[1],F[2],F[3]}"),
        (r"s={x,{1,2,3}};Table[F[x],s]", "{F[1],F[2],F[3]}"),
        (r"s={x,{1,2,3}};Table[F[x],s]", "{F[1],F[2],F[3]}"),
        # Global System Information
        (r"Abs[$ByteOrdering]", "1"),
        (r"Head[$CommandLine]", "List"),
        (r"Head[$Machine]", "String"),
        (r"Head[$MachineName]", "String"),
        (r"""Length[Names["System`*"]] > 1024""", "True"),
        (r"Length[$Packages] >= 5", "True"),
        (r"Head[$ParentProcessID]", "Integer"),
        (r"Head[$ProcessID]", "Integer"),
        (r"Head[$ProcessorType]", "String"),
        (r"Head[$ScriptCommandLine]", "List"),
        (r"Head[$SystemID]", "String"),
        (r"Head[$SystemWordLength]", "Integer"),
        # This doesn't work if not logged or in some OS's
        # (r"Head[$UserName]", "String"),
        (r"Head[$Version]", "String"),
        # Strings and Characters
        (r'StringInsert["abcdefghijklm", "X", 1]', r'"Xabcdefghijklm"'),
        (r'StringInsert["abcdefghijklm", "X", 14]', r'"abcdefghijklmX"'),
        (r'StringInsert["abcdefghijklm", "", 1]', r'"abcdefghijklm"'),
        (r'StringInsert["", "X", 1]', r'"X"'),
        (r'StringInsert["", "X", -1]', r'"X"'),
        (r'StringInsert["abcdefghijklm", "", -1]', r'"abcdefghijklm"'),
        (r'StringInsert[{"abcdefghijklm", "Mathics"}, "X", {}]', r'{"abcdefghijklm", "Mathics"}'),
    ],
)
def test_evaluation(str_expr: str, str_expected: str, message=""):
    result = _evaluate(str_expr)
    expected = _evaluate(str_expected)

    if message:
        assert result == expected, message
    else:
        assert result == expected


def test_exit():
    try:
        _evaluate("Exit[-37]")
    except SystemExit as e:
        assert e.code == -37


def test_quit():
    try:
        _evaluate("Quit[-37]")
    except SystemExit as e:
        assert e.code == -37
