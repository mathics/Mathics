#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
A couple of helper functions for working with numpy (and a couple of fallbacks, if numpy is unavailable)
"""

from mathics.core.expression import Expression
from itertools import chain

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


def py_stack_along_inner_axis(a):
    # explanation of functionality: see numpy version of _stack_array above

    if not isinstance(a[0], list):
        return list(chain(a))
    else:
        return [py_stack_along_inner_axis([x[i] for x in a]) for i in range(len(a[0]))]

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

    def stack_along_inner_axis(a):
        # numpy.stack with axis=-1 stacks arrays along the most inner axis:

        # e.g. numpy.stack([ [1, 2], [3, 4] ], axis=-1)
        # gives: array([ [1, 3], [2, 4] ])

        # e.g. numpy.stack([ [[1, 2], [3, 4]], [[4, 5], [6, 7]] ], axis=-1)
        # gives: array([[[1, 4], [2, 5]], [[3, 6], [4, 7]]])

        return numpy.stack(a, axis=-1)
else:
    # If numpy is not available, we define the following fallbacks that are useful for implementing a similar
    # logic in pure python without numpy. They obviously work on regular python array though, not numpy arrays.

    instantiate_elements = py_instantiate_elements
    stack_along_inner_axis = py_stack_along_inner_axis


