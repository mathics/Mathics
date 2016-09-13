import unittest
import math
import random

from mathics.builtin.compile import _compile, MathicsArg, int_type, real_type
from mathics.core.expression import Expression, Symbol, Integer, MachineReal


class ArithmeticTest(unittest.TestCase):
    def test_a(self):
        expr = Expression('Plus', Symbol('x'), Integer(2))
        args = [MathicsArg('System`x', int_type), MathicsArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(2, 4), 4)

    def test_b(self):
        expr = Expression('Plus', Symbol('x'), Symbol('y'))
        args = [MathicsArg('System`x', int_type), MathicsArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(1, 2.5), 3.5)

    def test_c(self):
        expr = Expression('Plus', Expression('Plus', Symbol('x'), Symbol('y')),  Integer(5))
        args = [MathicsArg('System`x', int_type), MathicsArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(1, 2.5), 8.5)

    def test_d(self):
        expr = Expression('Plus', Symbol('x'), MachineReal(1.5), Integer(2), Symbol('x'))
        args = [MathicsArg('System`x', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(2.5), 8.5)

    def _test_unary_math(self, name, fn):
        expr = Expression(name, Symbol('x'))
        args = [MathicsArg('System`x', real_type)]
        cfunc = _compile(expr, args)
        for _ in range(100):
            x = random.random()
            self.assertEqual(cfunc(x), fn(x))

    def _test_binary_math(self, name, fn):
        expr = Expression(name, Symbol('x'), Symbol('y'))
        args = [MathicsArg('System`x', real_type), MathicsArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        for _ in range(100):
            x = random.random()
            y = random.random()
            self.assertEqual(cfunc(x, y), fn(x, y))

    def test_plus(self):
        self._test_binary_math('Plus', lambda x, y: x + y)

    def test_times(self):
        self._test_binary_math('Times', lambda x, y: x * y)

    def test_sin(self):
        self._test_unary_math('Sin', math.sin)

    def test_cos(self):
        self._test_unary_math('Cos', math.cos)

    @unittest.expectedFailure
    def test_tan(self):
        self._test_unary_math('Tan', math.tan)

    def test_pow(self):
        self._test_binary_math('Power', lambda x, y : x ** y)

    def test_div0(self):
        expr = Expression('Power', Symbol('x'), Symbol('y'))
        args = [MathicsArg('System`x', real_type), MathicsArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(0.0, -1.5), float('+inf'))
        self.assertEqual(cfunc(0.0, -1.0), float('+inf'))
        self.assertEqual(cfunc(-0.0, -1.0), float('-inf'))
        self.assertEqual(cfunc(0.0, 0.0), float('1.0'))     # NaN?

    def test_exp(self):
        self._test_unary_math('Exp', math.exp)

    def test_log(self):
        self._test_unary_math('Log', math.log)

    def test_abs(self):
        self._test_unary_math('Abs', abs)

    def test_max(self):
        self._test_binary_math('Max', max)

    def test_min(self):
        self._test_binary_math('Min', min)
