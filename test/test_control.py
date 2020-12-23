# -*- coding: utf-8 -*-
from .helper import session, check_evaluation

def test_catch():
    session.evaluate(
        """
        (* Define a function that can "throw an exception": *)

         f[x_] := If[x > 10, Throw[overflow], x!]
        """
    )
    for str_expr, str_expected, message in (
        (
            "Catch[a; b; Throw[c]; d; e]",
            "c",
            "Exit to the enclosing Catch as soon as the Throw is evaluated",
        ),
        (
            "Catch[f[2] + f[11]]",
            "overflow",
            "The result of the Catch is just what is thrown by Throw in f[]",
        ),
        (
            "Catch[f[2] + f[3]]",
            "8",
            "A Catch[] where nothing is thrown",
        ),
        (
            "Catch[Do[If[i! > 10^10, Throw[i]], {i, 100}]]",
            "14",
            "Use Throw to exit a loop when a criterion is satisfied",
        ),
        (
            "Catch[If[# < 0, Throw[#]] & /@ {1, 2, 0, -1, 5, 6}]",
            "-1",
            "Catch can catch a Throw from inside essentially any function (1)",
        ),
        (
            "Catch[{a, Throw[b], c}]",
            "b",
            "Catch can catch a Throw from inside essentially any function (2)",
        ),
        (
            "Catch[a^2 + b^2 + c^2 /. b :> Throw[bbb]]",
            "bbb",
            "Catch can catch a Throw from inside essentially any function (3)",
        ),
        (
            "Catch[{Catch[{a, Throw[b], c}], d, e}]",
            "{b, d, e}",
            "The nearest enclosing Catch catches the Throw",
        ),
        (
            "Catch[{Throw[a], Throw[b], Throw[c]}]",
            "a",
            "Catch picks up the first Throw that is evaluated (1)",
        ),
        (
            "Catch[Throw /@ {a, b, c}]",
            "a",
            "Catch picks up the first Throw that is evaluated (2)",
        ),
    ):
        check_evaluation(str_expr, str_expected, message)


def test_condition():
    session.evaluate(
        """
        (* Define a function that can "throw an exception": *)

         f[x_] := ppp[x]/; x>0
        """
    )
    for str_expr, str_expected, message in (
        (
            "f[5]",
            "ppp[5]",
            "/; with True condition",
        ),
        (
            "f[-6]",
            "f[-6]",
            "/; with False condition",
        ),
        (
            "{6, -7, 3, 2, -1, -2} /. x_ /; x < 0 -> w",
            "{6, w, 3, 2, w, w}",
            "Replace all exlements which satisfy the condition of being negative",
        ),
        (
            "{6, -7, 3, 2, -1, -2} /. x_ /; x < 0 -> w",
            "{6, w, 3, 2, w, w}",
            "Replace all elements which satisfy the condition of being negative",
        ),
    ):
        check_evaluation(str_expr, str_expected, message)
