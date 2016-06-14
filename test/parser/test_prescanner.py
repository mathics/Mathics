from __future__ import absolute_import
from __future__ import unicode_literals

import unittest

from mathics.core.parser import IncompleteSyntaxError, InvalidSyntaxError, ScanError
from mathics.core.parser.prescanner import Prescanner
from mathics.core.parser.feed import SingleLineFeeder


class PrescannerTest(unittest.TestCase):
    def prescan(self, code):
        prescanner = Prescanner(SingleLineFeeder(code))
        return prescanner.scan()

    def invalid(self, code):
        self.assertRaises(ScanError, self.prescan, code)

    def incomplete(self, code):
        self.assertRaises(IncompleteSyntaxError, self.prescan, code)

    def equal(self, code, result):
        self.assertEqual(self.prescan(code), result)

    def test_longnames(self):
        self.equal(r'\[Theta]', '\u03B8')
        self.equal(r'\[CapitalPi]', '\u03A0')
        self.equal(r'\[Fake]', r'\[Fake]')
        self.equal('z \\[Conjugate]', 'z \uF3C8')

    def test_oct(self):
        self.equal(r'\051', ')')

    def test_hex_dot(self):
        self.equal(r'\.30', '0')

    def test_hex_colon(self):
        self.equal(r'\:0030', '0')
        self.equal(r'\:03B8', '\u03B8')
        self.equal(r'\:03b8', '\u03B8')

    def test_incomplete(self):
        self.incomplete(r'\[')
        self.incomplete(r'\[Theta')

    def test_invalid_oct(self):
        self.invalid(r'\093')
        self.invalid(r'\01')

    def test_invalid_colon(self):
        self.invalid(r'\:')
        self.invalid(r'\:A')
        self.invalid(r'\:01')
        self.invalid(r'\:A1')
        self.invalid(r'\:ak')
        self.invalid(r'\:A10')
        self.invalid(r'\:a1g')
        self.invalid(r'\:A1g9')
        self.invalid(r'\:01-2')

    def test_invalid_dot(self):
        self.invalid(r'\.')
        self.invalid(r'\.0')

    def test_combined(self):
        self.equal(r'\:03B8\[Theta]\.30\052', '\u03B8\u03B80*')

    def test_nested(self):
        self.equal(r'\[Thet\141]', r'\[Thet\141]')

    def test_trailing_backslash(self):
        self.incomplete('x \\')
