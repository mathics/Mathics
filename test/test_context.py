# -*- coding: utf-8 -*-
from .helper import check_evaluation
from mathics_scanner.errors import IncompleteSyntaxError


str_test_context = """
BeginPackage["FeynCalc`"];

AppendTo[$ContextPath, "FeynCalc`Package`"];

Begin["`Package`"];(*Symbols to be shared between subpackages*)
sharedSymbol;
End[];


foo::usage = "";
bar::usage = "";(*-----------------------------------------*)

Begin["`MySubpackageA`Private`"];
intVarA::usage = "";
foo[x_] := (
   Print["I can access sharedSymbol directly, since it is in ",
    Context[sharedSymbol], " and not in ",
    Context[intVarA]];
   sharedSymbol = x;
   x
   );
End[];(*-----------------------------------------*)

Begin["`MySubpackageB`Private`"];
intVarB::usage = "";
bar[] := (
   Print["I can access sharedSymbol directly, since it is in ",
    Context[sharedSymbol], " and not in ",
    Context[intVarB]];
   sharedSymbol
   );
End[];

EndPackage[];
"""


def test_context1():
    expr = ""
    for line in str_test_context.split("\n"):
        if line in ("", "\n"):
            continue
        expr = expr + line
        try:
            print("expr=", expr)
            check_evaluation(
                expr, "Null", to_string_expr=False, to_string_expected=False
            )
            expr = ""
            print("  OK")
        except IncompleteSyntaxError:
            continue
    check_evaluation("foo[42]", "42", to_string_expr=False, to_string_expected=False)
    check_evaluation("bar[]", "42", to_string_expr=False, to_string_expected=False)
