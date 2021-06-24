#!/usr/bin/env python
"""
Program to time mpmath pi vs sympy pi
"""

from timeit import timeit
import mpmath
import math
import sympy

PRECISION = 100
mpmath.mp.dps = PRECISION
ITERATIONS = 2000

# print(mpmath.pi, "\n")


def math_pi():
    return math.pi


def mpmath_pi():
    return mpmath.pi


def sympy_pi():
    return sympy.pi.n(PRECISION)


def mpmath_e():
    return mpmath.e


def sympy_e():
    return sympy.E.n(PRECISION)


def mpmath_degree():
    return mpmath.degree


def sympy_degree():
    return (sympy.pi / 180).n(PRECISION)


# print(timeit(math_pi, number=ITERATIONS))

for sympy_fn, mpmath_fn, fn_name in (
    (sympy_pi, mpmath_pi, "pi"),
    (sympy_e, mpmath_e, "e"),
    (sympy_degree, mpmath_degree, "degree"),
):
    print(fn_name + ":")
    print(timeit(mpmath_fn, number=ITERATIONS), "seconds for mpmath")
    print(timeit(sympy_fn, number=ITERATIONS), "seconds for sympy")
    print("\n", mpmath_fn(), "\n", sympy_fn())
    print()
