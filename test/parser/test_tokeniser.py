from __future__ import absolute_import
from __future__ import unicode_literals

import unittest
import urllib.request

from mathics.core.parser.tokeniser import Tokeniser, ScanError, IncompleteSyntaxError


class TokeniserTest(unittest.TestCase):
    def tags(self, code):
        tokeniser = Tokeniser(code)
        tokens = []
        while True:
            tag = tokeniser.next().tag
            if tag == 'END':
                break
            else:
                tokens.append(tag)
        return tokens

    def test_number(self):
        self.assertEqual(self.tags('1.5'), ['Number'])
        self.assertEqual(self.tags('1.5*^10'), ['Number'])

    def test_combinatorica(self):
        with urllib.request.urlopen('http://www.cs.uiowa.edu/~sriram/Combinatorica/NewCombinatorica.m') as f:
            code = f.read().decode('utf-8')
        t = Tokeniser(code)
        while True:
            tag = t.next().tag
            if tag == 'END':
                break
