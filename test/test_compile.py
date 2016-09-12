import unittest

from mathics.builtin.compile import _compile, MathicsArg, int_type, real_type
from mathics.core.expression import Expression, Symbol, Integer, MachineReal


class ArithmeticTest(unittest.TestCase):
    def test_a(self):
        expr = Expression('Plus', Symbol('x'), Integer(2))
        args = [MathicsArg('System`x', int_type), MathicsArg('System`y', int_type)]
        cfunc = _compile(expr, args, int_type)
        self.assertEqual(cfunc(2, 4), 4)

    def test_b(self):
        expr = Expression('Plus', Symbol('x'), Symbol('y'))
        args = [MathicsArg('System`x', int_type), MathicsArg('System`y', real_type)]
        cfunc = _compile(expr, args, real_type)
        self.assertEqual(cfunc(1, 2.5), 3.5)

    def test_c(self):
        expr = Expression('Plus', Expression('Plus', Symbol('x'), Symbol('y')),  Integer(5))
        args = [MathicsArg('System`x', int_type), MathicsArg('System`y', real_type)]
        cfunc = _compile(expr, args, real_type)
        self.assertEqual(cfunc(1, 2.5), 8.5)

    def test_d(self):
        expr = Expression('Plus', Symbol('x'), MachineReal(1.5))
        args = [MathicsArg('System`x', real_type)]
        cfunc = _compile(expr, args, real_type)
        self.assertEqual(cfunc(2.5), 4.0)
