import unittest
import six

from mathics.core.definitions import Definitions
from mathics.core.parser import parse, InvalidSyntaxError, IncompleteSyntaxError
from mathics.core.parser.feed import SingleLineFeeder, MultiLineFeeder


definitions = Definitions(add_builtin=True)


class UtilTests(unittest.TestCase):
    def parse(self, code):
        raise NotImplementedError

    def compare(self, expr1, expr2):
        raise NotImplementedError

    def check(self, expr1, expr2):
        if isinstance(expr1, six.string_types):
            expr1 = self.parse(expr1)
        if isinstance(expr2, six.string_types):
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
        self.incomplete_error('Sin[')
        self.check('Sin[\n0]', 'Sin[0]')
        self.check('Sin[\n\n0]', 'Sin[0]')

    def test_trailing_backslash(self):
        self.incomplete_error('x \\')
        self.check('x \\\ny', 'Times[x, y]')


class MultiLineParserTests(UtilTests):
    def parse(self, code):
        return parse(definitions, MultiLineFeeder(code))

    def compare(self, expr1, expr2):
        self.assertTrue(expr1.same(expr2))

    def test_trailing_backslash(self):
        self.incomplete_error('x \\')
        self.check('x \\\ny', 'Times[x, y]')

    def test_continuation(self):
        self.incomplete_error('Sin[')
        self.check('Sin[\n0]', 'Sin[0]')
        self.check('Sin[0\n]', 'Sin[0]')
        self.check('Sin[\n\n0]', 'Sin[0]')
