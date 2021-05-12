# -*- coding: utf-8 -*-
import os
import pytest

from .helper import evaluate, check_evaluation

evaluate(
    """
    Needs["KnotTheory`"]
    """
)


@pytest.mark.skipif(
    not os.environ.get("KnotTheory", False),
    reason="set environment variable KnotTheory to run this test",
)
def test_knottheory():
    evaluate(
        """K = PD[X[1,9,2,8], X[3,10,4,11],
                  X[5,3,6,2], X[7,1,8,12],
                  X[9,4,10,5], X[11,7,12,6]]
        """
    )
    for str_expr, str_expected in (
        (r"Crossings[K]", "6"),
        # ("PD[BR[4, {-1, 2, 3, -2, -1}]]",
        #  "D[X[8, 2, 3, 1], X[10, 4, 9, 3], X[5, 5, 6, 4], X[9, 6, 10, 7], X[1, 7, 2, 8]]"),
        (
            "BR[TorusKnot[5, 4]]",
            "BR[4, {1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3}]",
        ),
        ("Crossings[TorusKnot[5, 4]]", "15"),
        ("Crossings[TorusKnot[20, 34]]", "660"),
        # ("SymmetryType[Knot[4, 1]]", "FullyAmphicheiral"),
        # (
        #     "ColouredJones[Knot[4, 1], 3][q]",
        #     """3 + 1 / q ^ 12 - 1 / q ^ 11 - 1 / q ^ 10
        #          + 2 / q ^  8 - 2 / q ^ 6 + 3  / q ^ 4
        #          - 3 / q ^  2 - 3 q ^ 2 + 3 q ^ 4
        #          - 2   q ^  6 + 2 q ^ 8 - q ^ 10
        #          -     q ^ 11 +   q ^ 12""",
        # ),
    ):
        check_evaluation(str_expr, str_expected, to_string_expr=True)
