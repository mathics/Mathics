#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
A couple of helper functions for doing numpy-like stuff without numpy.
"""

from mathics.core.expression import Expression
from itertools import chain
from contextlib import contextmanager
from math import sin as sinf, cos as cosf, sqrt as sqrtf, atan2 as atan2f, floor as floorf
import operator
import inspect

# If numpy is not available, we define the following fallbacks that are useful for implementing a similar
# logic in pure python without numpy. They obviously work on regular python array though, not numpy arrays.


#
# INTERNAL FUNCTIONS
#

def _is_bottom(a):
    return any(not isinstance(x, list) for x in a)


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


#
# ARRAY CREATION AND REORGANIZATION: STACK, UNSTACK, CONCAT, ...
#

def array(a):
    return a


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


def stacked(f, a):
    components = unstack(a)
    result = f(*components)
    return stack(*result)


def concat(a, b):
    return stack(*(unstack(a) + unstack(b)))


def vectorize(a, depth, f):
    # depth == 0 means: f will get only scalars. depth == 1 means: f will
    # get lists of scalars.
    if _is_bottom(a):
        if depth == 0:
            return [f(x) for x in a]
        else:
            return f(a)
    else:
        return [vectorize(x, f, depth) for x in a]


#
# MATHEMATICAL OPERATIONS
#

def clip(a, t0, t1):
    def _eval(x):
        return max(t0, min(t1, x))

    return _apply(_eval, a)


def dot_t(u, v):
    if not isinstance(v[0], list):
        return sum(x * y for x, y in zip(u, v))
    else:
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


#
# PUBLIC HELPER FUNCTIONS
#

def is_numpy_available():
    return False


def allclose(a, b):
    if isinstance(a, list) and isinstance(b, list):
        if len(a) != len(b):
            return False
        return all(allclose(x, y) for x, y in zip(a, b))
    elif isinstance(a, list) or isinstance(b, list):
        return False
    else:
        return abs(a - b) < 1e-12


@contextmanager
def errstate(**kwargs):
    yield


def instantiate_elements(a, new_element, d=1):
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
        leaves = [instantiate_elements(e, new_element, d) for e in a]
    return Expression('List', *leaves)


#
# CONDITIONALS AND PROGRAM FLOW
#


def _choose_descend(i, options):
    if isinstance(i, (int, float)):
        return options[int(i)]  # int cast needed for PyPy
    else:
        return [_choose_descend(next_i, [o[k] for o in options]) for k, next_i in enumerate(i)]


def choose(i, *options):
    assert options
    dim = len(options[0])
    columns = [[o[d] for o in options] for d in range(dim)]
    return [_choose_descend(i, column) for column in columns]


def conditional(*args):  # essentially a noop
    if len(args) == 1 and callable(args[0]):
        f = args[0]  # @conditional without arguments?
    else:
        return lambda f: conditional(f)  # with arguments

    if not inspect.isfunction(f):
        raise Exception('@conditional can only be applied to functions')

    def wrapper(*a):
        return f(*a)

    return wrapper
