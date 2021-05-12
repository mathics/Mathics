
import unittest
import random
import sys

from mathics.core.parser.tokeniser import Tokeniser, Token
from mathics.core.parser.errors import ScanError, IncompleteSyntaxError, InvalidSyntaxError
from mathics.core.parser.feed import SingleLineFeeder


class TokeniserTest(unittest.TestCase):

    def tokens(self, code):
        tokeniser = Tokeniser(SingleLineFeeder(code))
        tokens = []
        while True:
            token = tokeniser.next()
            if token.tag == 'END':
                break
            else:
                tokens.append(token)
        return tokens

    def tags(self, code):
        return [token.tag for token in self.tokens(code)]

    def single_token(self, code):
        tokens = self.tokens(code)
        self.assertEqual(len(tokens), 1)
        token = tokens[0]
        return token

    def check_number(self, code):
        token = self.single_token(code)
        self.assertEqual(token, Token('Number', code, 0))

    def check_symbol(self, code):
        token = self.single_token(code)
        self.assertEqual(token, Token('Symbol', code, 0))

    def check_string(self, code):
        token = self.single_token(code)
        self.assertEqual(token, Token('String', code, 0))

    def test_number(self):
        self.assertEqual(self.tags('1.5'), ['Number'])
        self.assertEqual(self.tags('1.5*^10'), ['Number'])

    def scan_error(self, string):
        self.assertRaises(ScanError, self.tokens, string)

    def incomplete_error(self, string):
        self.assertRaises(IncompleteSyntaxError, self.tokens, string)

    def invalid_error(self, string):
        self.assertRaises(InvalidSyntaxError, self.tokens, string)

    def testSymbol(self):
        self.check_symbol('xX')
        self.check_symbol('context`name')
        self.check_symbol('`name')
        self.check_symbol('`context`name')

    def testNumber(self):
        self.check_number('0')

    def testNumberBase(self):
        self.check_number('8^^23')
        self.check_number('10*^3')
        self.check_number('10*^-3')
        self.check_number('8^^23*^2')

    def testNumberBig(self):
        for _ in range(10):
            self.check_number(str(random.randint(0, sys.maxsize)))
            self.check_number(str(random.randint(sys.maxsize, sys.maxsize * sys.maxsize)))

    def testNumberReal(self):
        self.check_number('1.5')
        self.check_number('1.5`')
        self.check_number('0.0')

    def testString(self):
        self.check_string(r'"abc"')
        self.incomplete_error(r'"abc')
        self.check_string(r'"abc(*def*)"')
        self.check_string(r'"a\"b\\c"')
        self.incomplete_error(r'"\"')

    def testPrecision(self):
        self.check_number('1.5`-5')
        self.check_number('1.5`0')
        self.check_number('1.5`10')

    def testAccuracy(self):
        self.scan_error('1.5``')
        self.check_number('1.0``20')
        self.check_number('1.0``0')
        self.check_number('1.4``-20')

    def testSet(self):
        self.assertEqual(self.tokens('x = y'), [Token('Symbol', 'x', 0), Token('Set', '=', 2), Token('Symbol', 'y', 4)])
        self.assertEqual(self.tokens('x /: y = z'), [Token('Symbol', 'x', 0), Token('TagSet', '/:', 2), Token('Symbol', 'y', 5), Token('Set', '=', 7), Token('Symbol', 'z', 9)])

    def testUnset(self):
        self.assertEqual(self.tokens('=.'), [Token('Unset', '=.', 0)])
        self.assertEqual(self.tokens('= .'), [Token('Unset', '= .', 0)])
        self.assertEqual(self.tokens('=.5'), [Token('Set', '=', 0), Token('Number', '.5', 1)])
        self.assertEqual(self.tokens('= ..'), [Token('Set', '=', 0), Token('Repeated', '..', 2)])

    def testIntRepeated(self):
        self.assertEqual(self.tokens('1..'), [Token('Number', '1', 0), Token('Repeated', '..', 1)])
        self.assertEqual(self.tokens('1. .'), [Token('Number', '1.', 0), Token('Dot', '.', 3)])

    def testIntegeral(self):
        self.assertEqual(self.tokens('\u222B x \uF74C y'), [Token('Integral', '\u222B', 0), Token('Symbol', 'x', 2), Token('DifferentialD', '\uF74C', 4), Token('Symbol', 'y', 6)])

    def testPre(self):
        self.assertEqual(self.tokens('++x++'), [Token('Increment', '++', 0), Token('Symbol', 'x', 2), Token('Increment', '++', 3)])

    def testFunction(self):
        self.assertEqual(self.tokens('x&'), [Token('Symbol', 'x', 0), Token('Function', '&', 1)])
        self.assertEqual(self.tokens('x\uf4a1'), [Token('Symbol', 'x', 0), Token('Function', '\uf4a1', 1)])

    def testApply(self):
        self.assertEqual(self.tokens('f // x'), [Token('Symbol', 'f', 0), Token('Postfix', '//', 2), Token('Symbol', 'x', 5)])
        self.assertEqual(self.tokens('f @ x'), [Token('Symbol', 'f', 0), Token('Prefix', '@', 2), Token('Symbol', 'x', 4)])
        self.assertEqual(self.tokens('f ~ x'), [Token('Symbol', 'f', 0), Token('Infix', '~', 2), Token('Symbol', 'x', 4)])

    def testBackslash(self):
        self.assertEqual(self.tokens('\\[Backslash]'), [Token('Backslash', '\u2216', 0)])
        self.assertEqual(self.tokens('\\ a'), [Token('RawBackslash', '\\', 0), Token('Symbol', 'a', 2)])
        self.incomplete_error('\\')

    def testBoxes(self):
        self.assertEqual(self.tokens('\\(1\\)'), [Token('LeftRowBox', '\\(', 0), Token('Number', "1", 2), Token('RightRowBox', '\\)', 3)])

    def testInformation(self):
        self.assertEqual(self.tokens('??Sin'), [Token('Information', '??', 0), Token('Symbol', 'Sin', 2)])
        self.assertEqual(self.tokens('? ?Sin'), [Token('PatternTest', '?', 0), Token('PatternTest', '?', 2), Token('Symbol', 'Sin', 3)])

    def testAssociation(self):
        self.assertEqual(self.tokens('<|x -> m|>'), [Token('RawLeftAssociation', '<|', 0), Token('Symbol', "x", 2), Token('Rule', '->', 4), Token('Symbol', "m", 7), Token('RawRightAssociation', '|>', 8)])
