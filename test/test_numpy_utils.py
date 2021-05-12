#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import unittest

from mathics.builtin.numpy_utils import stack, unstack, concat, vectorize, conditional, clip, array, choose
from mathics.builtin.numpy_utils import minimum, maximum,  dot_t, mod, floor, sqrt, allclose


@conditional
def _test_simple_conditional(t):
    if t > 0.5:
        return t + 1.
    else:
        return -t

@conditional
def _test_complex_conditional(t):
    if t > 10:
        return t * 10 + 1
    elif t > 3:
        return t * 10
    elif t <= 3:
        return -1


class Numpy(unittest.TestCase):
    def testUnstack(self):
        # flat lists remain flat lists.
        self.assertEqualArrays(
            unstack([1, 2, 3]),
            [1, 2, 3])

        # lists of lists get unstacked.
        self.assertEqualArrays(
            unstack([[1, 2], [3, 4]]),
            [[1, 3], [2, 4]])

        # matrices stay matrices, e.g. each r, g, b
        # components is split into grayscale images.
        self.assertEqualArrays(
            unstack([
                [[1, 2, 3], [4, 5, 6]],
                [[7, 8, 9], [10, 11, 12]]
            ]),
            [
                [[1, 4], [7, 10]],
                [[2,  5], [8, 11]],
                [[3,  6], [9, 12]]
            ])

    def testStackUnstackIdentity(self):
        a = [[[1, 2, 3], [4, 5, 6]], [[7, 8, 9], [10, 11, 12]]]
        b = [1, 2, 3]
        c = [[1, 2], [3, 4]]

        for m in (a, b, c):
            self.assertEqualArrays(stack(*unstack(m)), m)

    def testConcatSimple(self):
        # concat concatenates arrays.
        self.assertEqualArrays(concat([1], [2]), [1, 2])

    def testConcatComplex(self):
        # concat concatenates the most inner axis.
        a = [[[1, 2], [4, 5]], [[7, 8], [10, 11]]]
        b = [[[3], [6]], [[9], [12]]]
        c = [[[1, 2, 3], [4, 5, 6]], [[7, 8, 9], [10, 11, 12]]]
        self.assertEqualArrays(concat(a, b), c)

    def testChooseSimple(self):
        # select a single value from a list of values.
        options = [
            [0, 1, 2],
            [3, 4, 5],
            [6, 7, 8]
        ]

        for i in range(len(options)):
            self.assertEqual(choose(i, *options), options[i])

    def testChooseComplex(self):
        def m(i):
            return [[10 * i + 0, 10 * i + 1],
                    [10 * i + 2, 10 * i + 3]]

        selector = [[0, 1],
                    [1, 2]]

        a = choose(selector,
                    (m(1), m(2), m(3)),
                    (m(4), m(5), m(6)),
                    (m(7), m(8), m(9)))

        # choose() operates on column after column of the options matrix, i.e.
        # in the example above, the selector is first applied to (m(1), m(4), m(7)),
        # then to (m(2), m(5), m(8)), and so on. let's calculate the result for the
        # first column:

        # selector (integers indicate from which option to take a value):
        # [0 1]
        # [1 2]

        # option #0 / column 1 (i.e. m(1)):
        # [10 11]
        # [12 13]

        # option #1 / column 1 (i.e. m(4)):
        # [40 41]
        # [42 43]

        # option #2 / column 1 (i.e. m(7)):
        # [70 71]
        # [72 73]

        # choose() now picks the right value from each options depending on selector:

        # [10 41]
        # [42 73]

        self.assertEqual(len(a), 3)
        self.assertEqualArrays(a[0], [[10, 41], [42, 73]])
        self.assertEqualArrays(a[1], [[20, 51], [52, 83]])
        self.assertEqualArrays(a[2], [[30, 61], [62, 93]])

    def testClip(self):
        a = array([[[-0.1, 0.6], [-1.8, -0.4]], [[0.1, 0.8], [1.1, 0.5]]])
        a = clip(a, 0, 1)
        self.assertEqualArrays(a, [[[0., 0.6], [0., 0.]], [[0.1, 0.8], [1., 0.5]]])

    def testDot(self):
        self.assertEqual(dot_t([1, 2, 3], [4, 5, 6]), 32)
        self.assertEqualArrays(dot_t([1, 2, 3], [[4, 5, 6], [7, 8, 9], [10, 11, 12]]), [32, 50, 68])

    def testMod(self):
        self.assertEqualArrays(mod(array([[10, 20], [30, 40]]), [[7, 7], [7, 7]]), [[3, 6], [2, 5]])

    def testMaximum(self):
        self.assertEqualArrays(maximum(
            [[1, 2], [3, 4]],
            [[-1, 4], [-8, 5]],
            [[8, -4], [1, 10]]), [[8, 4], [3, 10]])

    def testMinimum(self):
        self.assertEqualArrays(minimum(
            [[1, 2], [3, 4]],
            [[-1, 4], [-8, 5]],
            [[8, -4], [1, 10]]), [[-1, -4], [-8, 4]])

    def testFloor(self):
        self.assertEqualArrays(floor([[1.2, 5.8], [-1.2, 3.5]]), [[1., 5.], [-2., 3.]])

    def testSqrt(self):
        self.assertEqualArrays(sqrt([[9, 100], [25, 16]]), [[3, 10], [5, 4]])

    def testSimpleConditional(self):
        a = array([[[0.1, 0.6], [1.8, 0.4]], [[-0.1, -0.8], [1.1, 0.5]]])
        a = vectorize(a, 0, _test_simple_conditional)
        self.assertEqualArrays(a, [[[-0.1, 1.6], [2.8, -0.4]], [[0.1, 0.8], [2.1, -0.5]]])

    def testConditionalComplex(self):
        a = array([[[1, 2], [4, 5]], [[7, 8], [10, 11]]])
        a = vectorize(a, 0, _test_complex_conditional)
        self.assertEqualArrays(a, [[[-1, -1], [40, 50]], [[70, 80], [100, 111]]])

    def assertEqualArrays(self, a, b):
        self.assertEqual(allclose(a, b), True)

if __name__ == '__main__':
    unittest.main()
