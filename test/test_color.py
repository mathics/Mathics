#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import unittest
from random import random


import mathics.builtin.colors as colors
from mathics.builtin.numpy_utils import array, stacked, vectorize
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
from mathics.core.expression import Expression


_color_tests = [[
    [
        [0.1],
        [0.1, 0.1, 0.1],
        [0., 0., 0., 0.9],
        [0., 0., 0.1],
        [0.009664, 0.010023, 0.008271],
        [0.090104, 0., 0.],
        [0.090104, 0., 0.129196],
        [0.090104, 0., 0.]
    ],
    [
        [0.1815],
        [0.1, 0.2, 0.3],
        [0.666667, 0.333333, 0., 0.7],
        [0.583333, 0.666667, 0.3],
        [0.027597, 0.030402, 0.05566],
        [0.202041, -0.031078, -0.189912],
        [0.202041, 0.192438, 0.724184],
        [0.202041, -0.103717, -0.177331]
    ],
    [
        [0.8185],
        [0.9, 0.8, 0.7],
        [0.1, 0.2, 0.3, 0.],
        [0.083333, 0.222222, 0.9],
        [0.639982, 0.635229, 0.389546],
        [0.837168, 0.063343, 0.161993],
        [0.837168, 0.173937, 0.190676],
        [0.837168, 0.181115, 0.176395]
    ],
    [
        [0.279072],
        [0.3, 0.276, 0.24],
        [0., 0.08, 0.2, 0.7],
        [0.1, 0.2, 0.3],
        [0.062498, 0.063527, 0.040573],
        [0.302854, 0.013415, 0.065333],
        [0.302854, 0.066696, 0.217769],
        [0.302854, 0.042072, 0.057975]
    ],
    [
        [0.414652],
        [0., 0.57972, 0.652246],
        [1., 0.111193, 0., 0.347754],
        [0.518532, 1., 0.652246],
        [0.1, 0.2, 0.3],
        [0.518372, -0.574865, -0.257804],
        [0.518372, 0.630026, 0.567095],
        [0.518372, -0.735612, -0.256568]
    ],
    [
        [0.094788],
        [0.213536, 0.05271, 0.],
        [0., 0.753156, 1., 0.786464],
        [0.041141, 1., 0.213536],
        [0.017769, 0.01126, 0.],
        [0.1, 0.2, 0.3],
        [0.1, 0.360555, 0.156416],
        [0.1, 0.223077, 0.071257]
    ],
    [
        [0.098639],
        [0.102999, 0.115575, 0.],
        [0.10882, 0., 1., 0.884425],
        [0.184803, 1., 0.115575],
        [0.009158, 0.01126, 0.],
        [0.1, -0.061803, 0.190211],
        [0.1, 0.2, 0.3],
        [0.1, -0.004455, 0.105387]
    ],
    [
        [0.102653],
        [0.159623, 0.09357, 0.],
        [0., 0.41381, 1., 0.840377],
        [0.097698, 1., 0.159623],
        [0.012794, 0.01126, 0.],
        [0.1, 0.063027, 0.172414],
        [0.1, 0.183573, 0.194222],
        [0.1, 0.2, 0.3]
    ]
],
[
    [
        [0.001],
        [0.001, 0.001, 0.001],
        [0., 0., 0., 0.999],
        [0., 0., 0.001],
        [0.000075, 0.000077, 0.000064],
        [0.000699, 0., 0.],
        [0.000699, 0., 0.129196],
        [0.000699, 0., 0.]
    ],
    [
        [0.001815],
        [0.001, 0.002, 0.003],
        [0.666667, 0.333333, 0., 0.997],
        [0.583333, 0.666667, 0.003],
        [0.000127, 0.000142, 0.000182],
        [0.001285, -0.000428, -0.001218],
        [0.001285, 0.001291, 0.696235],
        [0.001285, -0.00048, -0.000532]
    ],
    [
        [0.998185],
        [0.999, 0.998, 0.997],
        [0.001, 0.002, 0.003, 0.],
        [0.083333, 0.002002, 0.999],
        [0.960504, 0.995824, 0.819873],
        [0.998383, 0.000539, 0.001533],
        [0.998383, 0.001625, 0.196175],
        [0.998383, 0.001674, 0.001856]
    ],
    [
        [0.002996],
        [0.003, 0.002994, 0.002994],
        [0., 0.001988, 0.002, 0.997],
        [0.001, 0.002, 0.003],
        [0.000224, 0.000232, 0.000191],
        [0.002094, 4.e-6, 2.e-6],
        [0.002094, 4.e-6, 0.056043],
        [0.002094, 3.e-6, 0.]
    ],
    [
        [0.027967],
        [0., 0.038164, 0.048809],
        [1., 0.218102, 0., 0.951191],
        [0.53635, 1., 0.048809],
        [0.001, 0.002, 0.003],
        [0.018066, -0.03749, -0.02547],
        [0.018066, 0.045324, 0.594977],
        [0.018066, -0.025637, -0.008942]
    ],
    [
        [0.001649],
        [0.004015, 0.000765, 0.],
        [0., 0.80957, 1., 0.995985],
        [0.031738, 1., 0.004015],
        [0.000156, 0.000111, 0.],
        [0.001, 0.002, 0.003],
        [0.001, 0.003606, 0.156416],
        [0.001, 0.001754, 0.000784]
    ],
    [
        [0.001667],
        [0.003448, 0.000803, 0.00144],
        [0., 0.767033, 0.582337, 0.996552],
        [0.959868, 0.767033, 0.003448],
        [0.000156, 0.000111, 0.000089],
        [0.001, 0.002, 0.000038],
        [0.001, 0.002, 0.003],
        [0.001, 0.001178, -0.000132]
    ],
    [
        [0.],
        [0., 0., 0.],
        [0., 0., 0., 1.],
        [0., 0., 0.],
        [0., 0., 0.],
        [0., 0., 0.],
        [0., 0., 0.],
        [0.001, 0.002, 0.003]
    ]
]]


def _color_string(head, components):
    return '%s[%s]' % (head, ', '.join(['%.6f' % c for c in components]))


class ColorTest(unittest.TestCase):
    def setUp(self):
        definitions = Definitions(add_builtin=True)
        self.evaluation = Evaluation(definitions, format='xml')

    def testInverseConversions(self):
        # check that a conversion A -> B -> A restores the original
        # components. this tests color space transformations and their
        # inverse transformations.

        def space_to_head(name):
            if name == 'HSB':
                return 'System`Hue'
            else:
                return 'System`%sColor' % name

        spaces = ("CMYK", "HSB", "LAB", "LCH", "LUV", "RGB", "XYZ")
        places = 3
        for original in ((0.5, 0.1, 0.2), (0.9, 0.1, 0.1)):
            for i, from_space in enumerate(spaces):
                for to_space in spaces[i + 1:]:
                    try:
                        construct_name = space_to_head(from_space)
                        source_color = Expression(construct_name, *original)

                        # now calculate from_space -> to_space -> from_space
                        target_color = Expression('ColorConvert', source_color, to_space).evaluate(self.evaluation)
                        self.assertEqual(target_color.get_head_name(), space_to_head(to_space))

                        checked_color = Expression('ColorConvert', target_color, from_space).evaluate(self.evaluation)
                        self.assertEqual(checked_color.get_head_name(), source_color.get_head_name())

                        checked_components = [c.to_python() for c in checked_color.leaves]
                        if from_space == 'CMYK':  # if cmyk, cmyk -> cmy
                            k = checked_components[3]
                            checked_components = [c * (1 - k) + k for c in checked_components[:3]]

                        self.assertEqual(len(original), len(checked_components))

                        for x, y in zip(original, checked_components):
                            self.assertAlmostEqual(x, y, places)
                    except:
                        print('test failed for %s -> %s -> %s' %
                              (_color_string(from_space, original), to_space, from_space))
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

        values = [[0.1, 0.2, 0.3, 0.0], [0.001, 0.002, 0.003, 0.0]]
        spaces = ["Grayscale", "RGB", "CMYK", "HSB", "XYZ", "LAB", "LCH", "LUV"]

        for components, t1 in zip(values, _color_tests):
            for src, t2 in zip(spaces, t1):
                for dst, expected in zip(spaces, t2):
                    components = list(components)
                    if src == "Grayscale":
                        c = components[:1]
                    elif src == 'CMYK':
                        c = components
                    else:
                        c = components[:3]
                    result = colors.convert(c, src, dst)
                    res_name = _color_string(src, c)
                    dst_name = _color_string(dst, result)
                    exp_name = _color_string(dst, expected)
                    self.assertIsNotNone(result, '%s -> %s != %s' % (res_name, dst_name, exp_name))
                    self.assertEqual(len(result), len(expected), '%s -> %s != %s' % (res_name, dst_name, exp_name))
                    for a, b in zip(result, expected):
                        self.assertAlmostEqual(a, b, 4, '%s -> %s != %s' % (res_name, dst_name, exp_name))

    def testImageConversions(self):
        # test that f([x, y, ...]) = [f(x), f(y), ...] for rectangular image arrays.

        for name, convert in colors.conversions.items():
            if name.find('CMYK') < 0:
                self._checkImageConversion(4, lambda p: vectorize(p, 1, lambda q: stacked(convert, q)))

    def _checkConversion(self, from_space, from_components, to_space, to_components):
        places = 6

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
        refs = [convert(p) for p in pixels]

        image = [[pixels[x * size + y] for y in range(size)] for x in range(size)]
        image = convert(array(image))

        for x in range(size):
            for y in range(size):
                p1 = image[x][y]
                p2 = refs[x * size + y]
                self.assertEqual(len(p1), len(p2))
                for a, b in zip(p1, p2):
                    self.assertAlmostEqual(a, b, 12)

if __name__ == "__main__":
    unittest.main()
