import unittest
import random
import sys

from mathics_scanner import (
    IncompleteSyntaxError,
    InvalidSyntaxError,
    ScanError,
    SingleLineFeeder,
)

from mathics.core.definitions import Definitions
from mathics.core.parser import parse
from mathics.core.expression import Symbol, Integer, Expression, Real, Rational, String


definitions = Definitions(add_builtin=True)


class ConvertTests(unittest.TestCase):
    def parse(self, code):
        return parse(definitions, SingleLineFeeder(code))

    def check(self, expr1, expr2):
        if isinstance(expr1, str):
            expr1 = self.parse(expr1)
        if isinstance(expr2, str):
            expr2 = self.parse(expr2)

        if expr1 is None:
            self.assertIsNone(expr2)
        else:
            self.assertTrue(expr1.same(expr2))

    def scan_error(self, string):
        self.assertRaises(ScanError, self.parse, string)

    def incomplete_error(self, string):
        self.assertRaises(IncompleteSyntaxError, self.parse, string)

    def invalid_error(self, string):
        self.assertRaises(InvalidSyntaxError, self.parse, string)

    def testSymbol(self):
        self.check("xX", Symbol("Global`xX"))
        self.check("context`name", Symbol("context`name"))
        self.check("`name", Symbol("Global`name"))
        self.check("`context`name", Symbol("Global`context`name"))

    def testInteger(self):
        self.check("0", Integer(0))
        self.check("1", Integer(1))
        self.check("-1", Integer(-1))

        self.check("8^^23", Integer(19))
        self.check("10*^3", Integer(10000))
        self.check("10*^-3", Rational(1, 100))
        self.check("8^^23*^2", Integer(1216))

        n = random.randint(-sys.maxsize, sys.maxsize)
        self.check(str(n), Integer(n))

        n = random.randint(sys.maxsize, sys.maxsize * sys.maxsize)
        self.check(str(n), Integer(n))

    def testReal(self):
        self.check("1.5", Real("1.5"))
        self.check("1.5`", Real("1.5"))
        self.check("0.0", Real(0))
        self.check("-1.5`", Real("-1.5"))

        self.check("0.00000000000000000", "0.")
        self.check("0.000000000000000000`", "0.")
        self.check("0.000000000000000000", "0.``18")

    def testString(self):
        self.check(r'"abc"', String("abc"))
        self.incomplete_error(r'"abc')
        self.check(r'"abc(*def*)"', String("abc(*def*)"))
        self.check(r'"a\"b\\c"', String(r'a"b\c'))
        self.incomplete_error(r'"\"')
        self.invalid_error(r'\""')

    def testAccuracy(self):
        self.scan_error("1.5``")
        self.check("1.0``20", Real("1.0", p=20))

    @unittest.expectedFailure
    def testLowAccuracy(self):
        self.check("1.4``0", Real(0))
        self.check("1.4``-20", Real(0))

    def testPrecision(self):
        self.check("1.`20", Real(1, p=20))
        self.check("1.00000000000000000000000`", Real(1))
        self.check("1.00000000000000000000000`30", Real(1, p=30))

    @unittest.expectedFailure
    def testLowPrecision(self):
        self.check("1.4`1", Real("1", p=1))
        self.check("1.4`0", Real(0, p=0))
        self.check("1.4`-5", Real(0, p=0))

    def testDerivative(self):
        f = Symbol("Global`f")
        self.check("f'", Expression(Expression("Derivative", Integer(1)), f))
        self.check("f''", Expression(Expression("Derivative", Integer(2)), f))
        self.check(
            "(f'')'''",
            Expression(
                Expression("Derivative", Integer(3)),
                Expression(Expression("Derivative", Integer(2)), f),
            ),
        )
        self.check("Derivative[f]", Expression("Derivative", f))
        self.check("Derivative[1][f]'", "(f')'")
