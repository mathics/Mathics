#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import absolute_import
from __future__ import unicode_literals

import unittest
from random import random

from six.moves import range

import mathics.builtin.colors as colors
from mathics.builtin.numpy.with_numpy import array, vectorized
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
from mathics.core.expression import Expression


class ColorTest(unittest.TestCase):
    def setUp(self):
        definitions = Definitions(add_builtin=True)
        self.evaluation = Evaluation(definitions, format='xml')

    def testInverseConversions(self):
        # check that a conversion A -> B -> A restores the original
        # components. this tests color space transformations and their
        # inverse transformations.

        spaces = ("CMYK", "HSB", "LAB", "LCH", "LUV", "RGB", "XYZ")
        places = 3
        for original in ((0.5, 0.1, 0.2), (0.9, 0.1, 0.1)):
            for i, from_space in enumerate(spaces):
                for to_space in spaces[i + 1:]:
                    try:
                        if from_space == 'HSB':
                            construct_name = 'Hue'
                        else:
                            construct_name = from_space + 'Color'

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
                        print('test failed for %s(%f, %f, %f) -> %s -> %s' %
                              (from_space, original[0], original[1], original[2], to_space, from_space))
                        raise

    def testConversions(self):
        self._checkConversion("RGB", (0.5, 0.5, 0.5),
                              "XYZ", (.20638274847577825, 0.2140411190781185, 0.17662882532500096))
        self._checkConversion("RGB", (0.4, 0.2, 0.3),
                              "XYZ", (0.08116707006828128, 0.05773536343816594, 0.057371054671583044))

        self._checkConversion("XYZ", (0.5, 0.5, 0.5),
                              "RGB", (0.743976775016277, 0.7256665017497576, 0.8118438573490818))
        self._checkConversion("XYZ", (0.4, 0.2, 0.3),
                              "RGB", (0.8977592548573999, 0.022700440000000155, 0.6685886356522144))

        self._checkConversion("XYZ", (0.1, 0.1, 0.1),
                              "LAB", (0.3784243088316416, 0.02835852516741566, -0.06139347105996884))

    def testImageConversions(self):
        # test that f([x, y, ...]) = [f(x), f(y), ...] for rectangular image arrays.

        for convert in colors.conversions.values():
            self._checkImageConversion(4, convert)

    def _checkConversion(self, from_space, from_components, to_space, to_components):
        places = 12

        if from_space == 'HSB':
            construct_name = 'Hue'
        else:
            construct_name = from_space + 'Color'

        components = [c.to_python() for c in Expression('ColorConvert', Expression(construct_name, *from_components),
                                                        to_space).evaluate(self.evaluation).leaves]
        self.assertEqual(len(components), len(to_components))
        for x, y in zip(components, to_components):
            self.assertAlmostEqual(x, y, places)

    def _checkImageConversion(self, size, convert):
        pixels = [[random(), random(), random()] for _ in range(size * size)]
        refs = [vectorized(convert, p, 1) for p in pixels]

        image = [[pixels[x * size + y] for y in range(size)] for x in range(size)]
        image = vectorized(convert, array(image), 1)

        for x in range(size):
            for y in range(size):
                p1 = image[x][y]
                p2 = refs[x * size + y]
                self.assertEqual(len(p1), len(p2))
                for a, b in zip(p1, p2):
                    self.assertAlmostEqual(a, b, 12)

if __name__ == "__main__":
    unittest.main()
