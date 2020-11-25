# -*- coding: utf-8 -*-
from .helper import session, check_evaluation

import sys
from mathics.core.parser import parse, SingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
import pytest


def test_control():
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
