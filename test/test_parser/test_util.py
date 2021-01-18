import unittest

from mathics_scanner import (
    IncompleteSyntaxError,
    InvalidSyntaxError,
    MultiLineFeeder,
    SingleLineFeeder,
)

from mathics.core.definitions import Definitions
from mathics.core.parser import parse


definitions = Definitions(add_builtin=True)


class UtilTests(unittest.TestCase):
    def parse(self, code):
        raise NotImplementedError

    def compare(self, expr1, expr2):
        raise NotImplementedError

    def check(self, expr1, expr2):
        if isinstance(expr1, str):
            expr1 = self.parse(expr1)
        if isinstance(expr2, str):
            expr2 = self.parse(expr2)

        if expr1 is None:
            self.assertIsNone(expr2)
        else:
            self.compare(expr1, expr2)

    def incomplete_error(self, string):
        self.assertRaises(IncompleteSyntaxError, self.parse, string)

    def invalid_error(self, string):
        self.assertRaises(InvalidSyntaxError, self.parse, string)


class SingleLineParserTests(UtilTests):
    def parse(self, code):
        return parse(definitions, SingleLineFeeder(code))

    def compare(self, expr1, expr2):
        self.assertTrue(expr1.same(expr2))

    def test_continuation(self):
        self.incomplete_error("Sin[")
        self.check("Sin[\n0]", "Sin[0]")
        self.check("Sin[\n\n0]", "Sin[0]")

    def test_trailing_backslash(self):
        self.incomplete_error("x \\")
        self.check("x \\\ny", "Times[x, y]")


class MultiLineParserTests(UtilTests):
    def parse(self, code):
        return parse(definitions, MultiLineFeeder(code))

    def compare(self, expr1, expr2):
        self.assertTrue(expr1.same(expr2))

    def test_trailing_backslash(self):
        self.incomplete_error("x \\")
        self.check("x \\\ny", "Times[x, y]")

    def test_continuation(self):
        self.incomplete_error("Sin[")
        self.check("Sin[\n0]", "Sin[0]")
        self.check("Sin[0\n]", "Sin[0]")
        self.check("Sin[\n\n0]", "Sin[0]")

    def test_CompoundExpression(self):
        self.check("f[a;\nb]", "f[CompoundExpression[a, b]]")
        self.check("f[a;\nb;\nc;]", "f[CompoundExpression[a, b, c, Null]]")
        self.check("f[a;\nb;\nc;\n]", "f[CompoundExpression[a, b, c, Null]]")

        self.check("a;^b", "Power[CompoundExpression[a, Null], b]")

        feeder = MultiLineFeeder("a;\n^b")
        self.compare(
            parse(definitions, feeder), self.parse("CompoundExpression[a, Null]")
        )
        self.assertRaises(InvalidSyntaxError, lambda f: parse(definitions, f), feeder)

    def test_Span(self):
        self.check("a;;^b", "Power[Span[a, All], b]")
        feeder = MultiLineFeeder("a;;\n^b")
        self.compare(parse(definitions, feeder), self.parse("Span[a, All]"))
        self.assertRaises(InvalidSyntaxError, lambda f: parse(definitions, f), feeder)
