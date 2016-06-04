import unittest
import six

from mathics.core.definitions import Definitions
from mathics.core.parser import parse
from mathics.core.expression import Symbol, Integer


class UtilTests(unittest.TestCase):
    def setUp(self):
        self.definitions = Definitions(add_builtin=True)
        self.parse = lambda code: parse(code, self.definitions)

    def parse(self, s):
        return self.parser.parse(s)

    def check(self, expr1, expr2):
        if isinstance(expr1, six.string_types):
            expr1 = self.parse(expr1)
        if isinstance(expr2, six.string_types):
            expr2 = self.parse(expr2)

        if expr1 is None:
            self.assertIsNone(expr2)
        else:
            self.assertIsTrue(expr1.same(expr2))

    def incomplete_error(self, string):
        self.assertRaises(IncompleteSyntaxError, self.parse, string)

    def invalid_error(self, string):
        self.assertRaises(InvalidSyntaxError, self.parse, string)

class MultiLineParserTests(UtilTests):
    def parse(self, s):
        exprs = list(parse_lines(s, definitions))
        assert len(exprs) == 1
        return exprs[0]

    def test_trailing_backslash(self):
        self.incomplete_error('x \\')
        self.check('x \\\ny', Node('Times', Symbol('Global`x'), Symbol('Global`y')))

    def test_continuation(self):
        self.incomplete_error('Sin[')
        self.check('Sin[\n0]', Node('Sin', Integer(0)))

    @unittest.expectedFailure
    def test_blanknewline(self):
        # currently handled in the frontend
        self.incomplete_error('Sin[\n\n0]')

