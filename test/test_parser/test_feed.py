import unittest
import six
import tempfile

from mathics.core.parser.feed import SingleLineFeeder, MultiLineFeeder, FileLineFeeder

class LineFeederTests(unittest.TestCase):
    def test_multi(self):
        feeder = MultiLineFeeder('abc\ndef')
        self.assertEqual(feeder.feed(), 'abc\n')
        self.assertEqual(feeder.feed(), 'def')
        self.assertEqual(feeder.feed(), '')
        self.assertTrue(feeder.empty())

    def test_single(self):
        feeder = SingleLineFeeder('abc\ndef')
        self.assertEqual(feeder.feed(), 'abc\ndef')
        self.assertTrue(feeder.empty())
        self.assertEqual(feeder.feed(), '')

    def test_file(self):
        with tempfile.TemporaryFile('w+') as f:
            f.write('abc\ndef\n')
            f.seek(0)
            feeder = FileLineFeeder(f)
            self.assertEqual(feeder.feed(), 'abc\n')
            self.assertEqual(feeder.feed(), 'def\n')
            self.assertEqual(feeder.feed(), '')
            self.assertTrue(feeder.empty())
