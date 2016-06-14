#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
A couple of helper functions for working with numpy (and a couple of fallbacks, if numpy is unavailable)
"""

from mathics.core.expression import Expression
from itertools import chain
from functools import reduce
from math import sin as sinf, cos as cosf, sqrt as sqrtf, atan2 as atan2f, floor as floorf
import operator

try:
    import numpy
    _numpy = True
except ImportError:  # no numpy?
    _numpy = False


def py_instantiate_elements(a, new_element, d=1):
    # given a python array 'a' and a python element constructor 'new_element', generate a python array of the
    # same shape as 'a' with python elements constructed through 'new_element'. 'new_element' will get called
    # if an array of dimension 'd' is reached.

    e = a[0]
    depth = 1
    while depth <= d and isinstance(e, list):
        e = e[0]
        depth += 1
    if d == depth:
        leaves = [new_element(x) for x in a]
    else:
        leaves = [py_instantiate_elements(e, new_element, d) for e in a]
    return Expression('List', *leaves)

if _numpy:
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

    def unstack(a):
        a = array(a)
        return array(numpy.split(a, a.shape[-1], axis=-1))

    def stack(*a):
        # numpy.stack with axis=-1 stacks arrays along the most inner axis:

        # e.g. numpy.stack([ [1, 2], [3, 4] ], axis=-1)
        # gives: array([ [1, 3], [2, 4] ])

        # e.g. numpy.stack([ [[1, 2], [3, 4]], [[4, 5], [6, 7]] ], axis=-1)
        # gives: array([[[1, 4], [2, 5]], [[3, 6], [4, 7]]])

        a = array(a)
        b = numpy.stack(a, axis=-1)

        if a.shape[-1] == 1 and b.shape[0] > 0:  # e.g. [[a], [b], [c]]
            b = b[0]  # makes stack(unstack(x)) == x
        return b

    def concat(*a):
        a = [x for x in a if x.shape[0]]  # skip empty
        return numpy.concatenate(a, axis=-1)

    def conditional(a, cond, t, f):
        b = array(a)[:]
        mask = cond(a)
        b[mask] = t(b[mask])
        b[~mask] = f(b[~mask])
        return b

    def switch(*a):
        assert a and len(a) % 2 == 0
        b = numpy.ndarray(a[0].shape)
        # we apply the rules in reversed order, so that the first rules
        # always make the last changes, which corresponds to the unreversed
        # processing in the non-vectorized (non-numpy) implementation.
        for mask, v in reversed([a[i:i + 2] for i in range(0, len(a), 2)]):
            b[mask] = v(lambda x: x[mask])
        return b

    def choose(i, *options):
        assert 0 < len(options) < 256
        i_int = i.astype(numpy.uint8)
        dim = len(options[0])
        return [numpy.choose(i_int, [o[d] for o in options]) for d in range(dim)]

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
else:
    # If numpy is not available, we define the following fallbacks that are useful for implementing a similar
    # logic in pure python without numpy. They obviously work on regular python array though, not numpy arrays.

    instantiate_elements = py_instantiate_elements

    def array(a):
        return a

    def _is_bottom(a):
        return any(not isinstance(x, list) for x in a)

    def unstack(a):
        if not a:
            return []

        def length(b):
            return max(length(x) for x in b) if not _is_bottom(b) else len(b)

        def split(b, i):
            if not _is_bottom(b):
                return [split(x, i) for x in b]
            else:
                return b[i]

        return [split(a, i) for i in range(length(a))]

    def stack(*a):
        if _is_bottom(a):
            return list(chain(a))
        else:
            return [stack(*[x[i] for x in a]) for i in range(len(a[0]))]

    def concat(a, b):
        return stack(*(unstack(a) + unstack(b)))

    def _apply(f, a):
        if isinstance(a, (list, tuple)):
            return [_apply(f, t) for t in a]
        else:
            return f(a)

    def _apply_n(f, *p):
        if isinstance(p[0], list):
            return [_apply_n(f, *q) for q in zip(*p)]
        else:
            return f(*p)

    def conditional(a, cond, t, f):
        def _eval(x):
            return t(x) if cond(x) else f(x)
        return _apply(_eval, a)

    def switch(*a):
        assert a and len(a) % 2 == 0
        for cond, v in (a[i:i + 2] for i in range(0, len(a), 2)):
            if cond:
                return v(lambda x: x)
        raise ValueError('no matching case in switch')

    def choose(i, *options):
        return options[int(i)]  # int cast needed for PyPy

    def clip(a, t0, t1):
        def _eval(x):
            return max(t0, min(t1, x))
        return _apply(_eval, a)

    def dot_t(u, v):
        return [sum(x * y for x, y in zip(u, r)) for r in v]

    def mod(a, b):
        return _apply_n(operator.mod, a, b)

    def sin(a):
        return _apply(sinf, a)

    def cos(a):
        return _apply(cosf, a)

    def arctan2(y, x):
        return _apply_n(atan2f, y, x)

    def sqrt(a):
        return _apply(sqrtf, a)

    def floor(a):
        return _apply(floorf, a)

    def maximum(*a):
        return _apply_n(max, *a)

    def minimum(*a):
        return _apply_n(min, *a)
