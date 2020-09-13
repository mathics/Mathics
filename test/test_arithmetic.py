#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import unittest
from mathics.core.expression import Expression, Integer, Rational, Symbol
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation


class ArithmeticTest(unittest.TestCase):
    def setUp(self):
        definitions = Definitions(add_builtin=True)
        self.evaluation = Evaluation(definitions, format="xml")

    def testAdd(self):
        cases = (
            (Symbol("a") + 3, Expression("Plus", 3, Symbol("a"))),
            (Integer(1) + 3, Integer(4)),
            (Integer(1) + (-3), Integer(-2)),
            (
                Expression("List", 1, 2) + Expression("List", -1, 8),
                Expression("List", 0, 10),
            ),
        )
        self._testCases(cases)

    def testSub(self):
        cases = (
            (Symbol("a") - 3, Expression("Plus", -3, Symbol("a"))),
            (Integer(1) - 3, Integer(-2)),
            (Integer(1) - (-3), Integer(4)),
            (
                Expression("List", 1, 2) - Expression("List", -1, 8),
                Expression("List", 2, -6),
            ),
        )
        self._testCases(cases)

    def testMul(self):
        cases = (
            (Symbol("a") * 3, Expression("Times", 3, Symbol("a"))),
            (Integer(3) * 7, Integer(21)),
            (Integer(3) * (-7), Integer(-21)),
            (
                Expression("List", 1, 2) * Expression("List", -1, 8),
                Expression("List", -1, 16),
            ),
        )
        self._testCases(cases)

    def testTrueDiv(self):
        cases = (
            (Symbol("a") / 3, Expression("Times", Rational(1, 3), Symbol("a"))),
            (Integer(8) / 2, Integer(4)),
            (Integer(8) / (-2), Integer(-4)),
            (Integer(7) / 2, Rational(7, 2)),
            (
                Expression("List", 1, 9) / Expression("List", -1, 3),
                Expression("List", -1, 3),
            ),
        )
        self._testCases(cases)

    def testFloorDiv(self):
        cases = (
            (Integer(8) // 2, Integer(4)),
            (Integer(8) // (-2), Integer(-4)),
            (Integer(7) // 2, Integer(3)),
        )
        self._testCases(cases)

    def testPow(self):
        cases = (
            (Integer(8) ** 2, Integer(64)),
            (
                Expression("List", 2, 5) ** Expression("List", 3, 4),
                Expression("List", 8, 625),
            ),
        )
        self._testCases(cases)

    def testAbs(self):
        cases = (
            (abs(Integer(-8)), Integer(8)),
            (abs(Expression("List")), Expression("Abs", Expression("List"))),
        )
        self._testCases(cases)

    def _testCases(self, cases):
        for expression, result in cases:
            self.assertEqual(
                Expression("FullSimplify", expression).evaluate(self.evaluation),
                Expression("FullSimplify", result).evaluate(self.evaluation),
            )


if __name__ == "__main__":
    unittest.main()
