#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from mathics.builtin.compile.types import int_type, real_type, bool_type, void_type
from ctypes import c_int64, c_double, c_bool, c_void_p


def pairwise(args):
    '''
    [a, b, c] -> [(a, b), (b, c)]
    >>> list(pairwise([1, 2, 3]))
    [(1, 2), (2, 3)]
    '''
    first = True
    for arg in args:
        if not first:
            yield last, arg
        first = False
        last = arg


def llvm_to_ctype(t):
    'converts llvm types to ctypes'
    if t == int_type:
        return c_int64
    elif t == real_type:
        return c_double
    elif t == bool_type:
        return c_bool
    elif t == void_type:
        return c_void_p
    else:
        raise TypeError(t)
