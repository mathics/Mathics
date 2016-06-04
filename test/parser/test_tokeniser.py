from __future__ import absolute_import
from __future__ import unicode_literals

import unittest
import random
import sys
import urllib.request

from mathics.core.parser.tokeniser import Tokeniser, ScanError, IncompleteSyntaxError, Token
from mathics.core.parser.errors import *


class TokeniserTest(unittest.TestCase):

    def tokens(self, code):
        tokeniser = Tokeniser(code)
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

    def test_combinatorica(self):
        with urllib.request.urlopen('http://www.cs.uiowa.edu/~sriram/Combinatorica/NewCombinatorica.m') as f:
            code = f.read().decode('utf-8')
        t = Tokeniser(code)
        while True:
            tag = t.next().tag
            if tag == 'END':
                break

    def testSymbol(self):
        self.check_symbol('xX')
        self.check_symbol('context`name')
        self.check_symbol('`name')
        self.check_symbol('`context`name')

    def testNumber(self):
        self.check_number('0')
        self.check_number('-1')

    def testNumberBase(self):
        self.check_number('8^^23')
        self.check_number('10*^3')
        self.check_number('10*^-3')
        self.check_number('8^^23*^2')

    def testNumberBig(self):
        for _ in range(10):
            self.check_number(str(random.randint(-sys.maxsize, sys.maxsize)))
            self.check_number(str(random.randint(sys.maxsize, sys.maxsize * sys.maxsize)))

    def testNumberReal(self):
        self.check_number('1.5')
        self.check_number('1.5`')
        self.check_number('0.0')
        self.check_number('-1.5`')

    def testString(self):
        self.check_string(r'"abc"')
        self.incomplete_error(r'"abc')
        self.check_string(r'"abc(*def*)"')
        self.check_string(r'"a\"b\\c"')
        self.incomplete_error(r'"\"')
        self.incomplete_error(r'\""')

    def testPrecision(self):
        self.check_number('1.5`-5')
        self.check_number('1.5`0')
        self.check_number('1.5`10')

    def testAccuracy(self):
        self.scan_error('1.5``')
        self.check_number('1.0``20')
        self.check_number('1.0``0')
        self.check_number('1.4``-20')
