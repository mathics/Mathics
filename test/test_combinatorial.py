# -*- coding: utf-8 -*-
from .helper import session, check_evaluation

import sys
from mathics.core.parser import parse, SingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
import pytest

def test_combinatorial():

    for str_expr, str_expected, message in (
        # WL allows: StirlingS1[{2, 4, 6}, 2]
        (
            "Table[StirlingS1[n, 2], {n, 2, 6, 2}]",
            "{1, 11, 274}",
            "StirlingS1 short",
        ),
        (
            "Table[StirlingS2[10, m], {m, 10}]",
            "{1, 511, 9330, 34105, 42525, 22827, 5880, 750, 45, 1}",
            "StirlingS2",
        ),
        (
            "StirlingS1[50, 1]",
            "-608281864034267560872252163321295376887552831379210240000000000",
            "StirlingS1",
        ),
    ):
        check_evaluation(str_expr, str_expected, message)
