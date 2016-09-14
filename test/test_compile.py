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


class FlowControlTest(unittest.TestCase):

    def test_if(self):
        expr = Expression('If', Symbol('x'), Symbol('y'), Symbol('z'))
        args = [MathicsArg('System`x', int_type), MathicsArg('System`y', real_type), MathicsArg('System`z', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(0, 3.0, 4.0), 4.0)
        self.assertEqual(cfunc(1, 3.0, 4.0), 3.0)
        self.assertEqual(cfunc(2, 3.0, 4.0), 3.0)

    def test_if_cont(self):
        expr = Expression('Plus', Integer(1), Expression('If', Symbol('x'), Expression('Sin', Symbol('y')), Expression('Cos', Symbol('y'))))
        args = [MathicsArg('System`x', int_type), MathicsArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(0, 0.0), 2.0)
        self.assertEqual(cfunc(1, 0.0), 1.0)

    def test_if_eq(self):
        expr = Expression('If', Expression('Equal', Symbol('x'), Integer(1)), Integer(2), Integer(3))
        args = [MathicsArg('System`x', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(1), 2)
        self.assertEqual(cfunc(2), 3)

    def test_if_types(self):
        expr = Expression('If', Expression('Equal', Symbol('x'), Integer(1)), Integer(2), MachineReal(3))
        args = [MathicsArg('System`x', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(1), 2.0)
        self.assertEqual(cfunc(2), 3.0)


class ComparisonTest(unittest.TestCase):
    def test_int_equal(self):
        expr = Expression('Equal', Symbol('x'), Symbol('y'), Integer(3))
        args = [MathicsArg('System`x', int_type), MathicsArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(3, 3), True)
        self.assertEqual(cfunc(2, 2), False)
        self.assertEqual(cfunc(2, 3), False)

    def test_int_unequal(self):
        expr = Expression('Unequal', Symbol('x'), Symbol('y'), Integer(3))
        args = [MathicsArg('System`x', int_type), MathicsArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(1, 2), True)
        self.assertEqual(cfunc(3, 2), False)
        self.assertEqual(cfunc(2, 3), False)
        self.assertEqual(cfunc(2, 2), False)
        self.assertEqual(cfunc(3, 3), False)

    def test_real_equal(self):
        expr = Expression('Equal', Symbol('x'), Symbol('y'))
        args = [MathicsArg('System`x', real_type), MathicsArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(3.0, 3.0), True)
        self.assertEqual(cfunc(3.0, 2.0), False)
        self.assertEqual(cfunc(2.0, 3.0), False)
        # TODO NaN/+inf/-inf comparisons

    def test_int_real_equal(self):
        expr = Expression('Equal', Symbol('x'), Symbol('y'))
        args = [MathicsArg('System`x', real_type), MathicsArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(3.0, 3), True)
        self.assertEqual(cfunc(3.0, 2), False)
        self.assertEqual(cfunc(2.0, 3), False)

    def test_inequalities(self):
        cases = [
            ('Less', (1, 2), True),
            ('Less', (1, 1), False),
            ('Less', (2, 1), False),
            ('LessEqual', (1, 2), True),
            ('LessEqual', (1, 1), True),
            ('LessEqual', (2, 1), False),
            ('Greater', (1, 2), False),
            ('Greater', (1, 1), False),
            ('Greater', (2, 1), True),
            ('GreaterEqual', (1, 2), False),
            ('GreaterEqual', (1, 1), True),
            ('GreaterEqual', (2, 1), True),
        ]
        for head, args, result in cases:
            expr = Expression(head, Symbol('x'), Symbol('y'))
            int_args = [MathicsArg('System`x', int_type), MathicsArg('System`y', int_type)]
            cfunc = _compile(expr, int_args)
            self.assertEqual(cfunc(*args), result)

            real_args = [MathicsArg('System`x', real_type), MathicsArg('System`y', real_type)]
            cfunc = _compile(expr, real_args)
            self.assertEqual(cfunc(*(float(arg) for arg in args)), result)
