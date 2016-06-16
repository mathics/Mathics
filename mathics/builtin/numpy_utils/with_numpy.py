#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
A couple of helper functions for doing numpy-like stuff with numpy.
"""

from mathics.core.expression import Expression
from functools import reduce
import numpy


def is_numpy_available():
    return True


def instantiate_elements(a, new_element, d=1):
    # given a numpy array 'a' and a python element constructor 'new_element', generate a python array of the
    # same shape as 'a' with python elements constructed through 'new_element'. 'new_element' will get called
    # if an array of dimension 'd' is reached.

    if len(a.shape) == d:
        leaves = [new_element(x) for x in a]
    else:
        leaves = [instantiate_elements(e, new_element, d) for e in a]
    return Expression('List', *leaves)


def array(a):
    return numpy.array(a)


def vectorized(f, a, depth):
    # we ignore depth, as numpy arrays will handle the vectorized cases.
    a = array(a)
    if len(a.shape) == 1:
        # if a is a flat array, we embed the components in an array, i.e.
        # [[a, b, c]], so that unstack will yield [a], [b], [c], and we
        # operate on arrays of length 1 instead of scalars.
        return f(array([a]))[0]
    else:
        return f(a)


def unstack(a):
    a = array(a)
    b = array(numpy.split(a, a.shape[-1], axis=-1))
    if b.shape[-1] == 1:
        b = b.reshape(b.shape[:-1])
    return b

def stack(*a):
    # numpy.stack with axis=-1 stacks arrays along the most inner axis:

    # e.g. numpy.stack([ [1, 2], [3, 4] ], axis=-1)
    # gives: array([ [1, 3], [2, 4] ])

    # e.g. numpy.stack([ [[1, 2], [3, 4]], [[4, 5], [6, 7]] ], axis=-1)
    # gives: array([[[1, 4], [2, 5]], [[3, 6], [4, 7]]])

    a = array(a)
    b = numpy.stack(a, axis=-1)
    return b


def concat(*a):
    a = [array(x) for x in a]
    a = [x for x in a if x.shape[0]]  # skip empty
    return numpy.concatenate(a, axis=-1)


def conditional(a, cond, t, f):
    b = array(a)[:]
    mask = cond(a)
    b[mask] = t(b[mask])
    b[~mask] = f(b[~mask])
    return b


def compose(*a):
    assert a and len(a) % 2 == 0
    b = numpy.ndarray(a[0].shape)
    # we apply the rules in reversed order, so that the first rules
    # always make the last changes, which corresponds to the unreversed
    # processing in the non-vectorized (non-numpy) implementation.
    for mask, v in reversed([a[i:i + 2] for i in range(0, len(a), 2)]):
        b[mask] = v(lambda x: x[mask])
    return b


def clip(a, t0, t1):
    return numpy.clip(array(a), t0, t1)


def dot_t(u, v):
    return numpy.dot(array(u), array(v).T)


def mod(a, b):
    return numpy.mod(a, b)


def sin(a):
    return numpy.sin(array(a))


def cos(a):
    return numpy.cos(array(a))


def arctan2(y, x):
    return numpy.arctan2(array(y), array(x))


def sqrt(a):
    return numpy.sqrt(array(a))


def floor(a):
    return numpy.floor(array(a))


def maximum(*a):
    return reduce(numpy.maximum, [array(x) for x in a])


def minimum(*a):
    return reduce(numpy.minimum, [array(x) for x in a])


def choose(i, *options):
    assert options
    dim = len(options[0])
    columns = [[o[d] for o in options] for d in range(dim)]
    if isinstance(i, (int, float)):
        return [column[int(i)] for column in columns]  # int cast needed for PyPy
    else:
        assert len(options) < 256
        i_int = array(i).astype(numpy.uint8)
        return [numpy.choose(i_int, column) for column in columns]


def allclose(a, b):
    return numpy.allclose(array(a), array(b))
