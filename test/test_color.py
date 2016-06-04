#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import absolute_import
from __future__ import unicode_literals

import os
import sys
import pexpect
import unittest
from six.moves import range
from mathics.core.expression import Expression, Integer, Rational, Symbol
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation


class ColorTest(unittest.TestCase):
    def setUp(self):
        definitions = Definitions(add_builtin=True)
        self.evaluation = Evaluation(definitions, format='xml')

    def testInverseConversions(self):
        # check that a conversion A -> B -> A restores the original
        # components. this tests color space transformations and their
        # inverse transformations.

        spaces = ("CMYK", "HSB", "LAB", "LCH", "LUV", "RGB", "XYZ")
        places = 4
        for i, from_space in enumerate(spaces):
            for to_space in spaces[i + 1:]:
                try:
                    if from_space == 'HSB':
                        construct_name = 'Hue'
                    else:
                        construct_name = from_space + 'Color'

                    original = (0.5, 0.1, 0.2)  # carefully chosen components
                    # that give useful transformations along all color spaces

                    # now calculate from_space -> to_space -> from_space
                    inverted = [c.to_python() for c in Expression('ColorConvert',
                        Expression('ColorConvert',
                            Expression(construct_name, *original),
                            to_space),
                        from_space).evaluate(self.evaluation).leaves]
                    if from_space == 'CMYK':  # if cmyk, cmyk -> cmy
                        k = inverted[3]
                        inverted = [c * (1 - k) + k for c in inverted[:3]]
                    self.assertEqual(len(original), len(inverted))
                    for x, y in zip(original, inverted):
                        self.assertAlmostEqual(x, y, places)
                except:
                    print('test failed for %s -> %s -> %s' %
                          (from_space, to_space, from_space))
                    raise

    def testConversionsFromXYZ(self):
        self._checkConversion("XYZ", (0.5, 0.5, 0.5), "LAB", (0.7606, 0.0484, -0.1049))
        self._checkConversion("XYZ", (0.5, 0.5, 0.5), "RGB", (0.7440, 0.7257, 0.8118))

    def _checkConversion(self, from_space, from_components, to_space, to_components):
        places = 3

        if from_space == 'HSB':
            construct_name = 'Hue'
        else:
            construct_name = from_space + 'Color'

        components = [c.to_python() for c in Expression('ColorConvert', Expression(construct_name, *from_components),
                                                        to_space).evaluate(self.evaluation).leaves]
        self.assertEqual(len(components), len(to_components))
        for x, y in zip(components, to_components):
            self.assertAlmostEqual(x, y, places)

if __name__ == "__main__":
    unittest.main()
