#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from mathics.core.evaluation import Evaluation
from mathics.core.expression import (
    Complex,
    Expression,
    Integer,
    MachineReal,
    PrecisionReal,
    Rational,
    Real,
    String,
    Symbol,
    SymbolFalse,
)
from mathics.core.definitions import Definitions
import sys
import unittest

definitions = Definitions(add_builtin=True)


def _symbol_truth_value(x):
    if x.is_true():
        return True
    elif isinstance(x, Symbol) and x == SymbolFalse:
        return False
    else:
        return "undefined"


def _test_group(k, *group):
    for i, a in enumerate(group):
        for j, b in enumerate(group):
            if i == j:
                continue
            evaluation = Evaluation(definitions, catch_interrupt=False)
            try:
                is_same_under_sameq = Expression("SameQ", a, b).evaluate(evaluation)
            except Exception as exc:
                print("Exception %s" % exc)
                info = sys.exc_info()
                sys.excepthook(*info)
                return False

            is_same = a.same(b)
            if is_same != _symbol_truth_value(is_same_under_sameq):
                print(
                    "%sTest failed: %s and %s are inconsistent under same() and SameQ\n"
                    % (sep, repr(a), repr(b))
                )
                return False

            if is_same and hash(a) != hash(b):
                print(
                    "%sTest failed: hashes for %s and %s must be equal but are not\n"
                    % (sep, repr(a), repr(b))
                )
                return False


class HashAndSameQ(unittest.TestCase):
    # tests that the following assumption holds: if objects are same under SameQ, their
    # hash is always equals. this assumption is relied upon by Gather[] and similar operations

    def testInteger(self):
        _test_group(Integer(5), Integer(3242), Integer(-1372))

    def testRational(self):
        _test_group(
            Rational(1, 3),
            Rational(1, 3),
            Rational(2, 6),
            Rational(-1, 3),
            Rational(-10, 30),
            Rational(10, 5),
        )

    def testReal(self):
        _test_group(
            MachineReal(1.17361),
            MachineReal(-1.42),
            MachineReal(42.846195714),
            MachineReal(42.846195714),
            MachineReal(42.846195713),
            Real("42.846195713", 18),
            Real("-1.42", 3),
        )

    def testComplex(self):
        def c(i, r):
            return Complex(MachineReal(i), MachineReal(i))

        _test_group(c(1.2, 1.2), c(0.7, 1.8), c(1.8, 0.7), c(-0.7, 1.8), c(0.7, 1.81))

    def testString(self):
        _test_group(String("xy"), String("x"), String("xyz"), String("abc"))

    def testSymbol(self):
        _test_group(Symbol("xy"), Symbol("x"), Symbol("xyz"), Symbol("abc"))

    def testAcrossTypes(self):
        _test_group(
            Integer(1),
            Rational(1, 1),
            Real(1),
            Complex(Integer(1), Integer(1)),
            String("1"),
            Symbol("1"),
        )

    def testInstances(self):
        # duplicate instantiations of same content (like Integer 5) to test for potential instantiation randomness.
        _test_group(
            list(
                map(
                    lambda f: (f(), f()),
                    (
                        lambda: Integer(5),
                        lambda: Rational(5, 2),
                        lambda: MachineReal(5.12345678),
                        lambda: Complex(Integer(5), Integer(2)),
                        lambda: String("xy"),
                        lambda: Symbol("xy"),
                    ),
                )
            )
        )


if __name__ == "__main__":
    unittest.main()
