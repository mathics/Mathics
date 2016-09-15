import unittest
import mpmath
import random

from mathics.core.expression import Expression, Symbol, Integer, MachineReal

from mathics.builtin.compile import has_llvmlite

if has_llvmlite:
    from mathics.builtin.compile import _compile, CompileArg, int_type, real_type, bool_type


class CompileTest(unittest.TestCase):
    def setUp(self):
        if not has_llvmlite:
            self.skipTest('No llvmlite detected. Skipping all compile tests')


class ArithmeticTest(CompileTest):
    def test_a(self):
        expr = Expression('Plus', Symbol('x'), Integer(2))
        args = [CompileArg('System`x', int_type), CompileArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(2, 4), 4)

    def test_b(self):
        expr = Expression('Plus', Symbol('x'), Symbol('y'))
        args = [CompileArg('System`x', int_type), CompileArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(1, 2.5), 3.5)

    def test_c(self):
        expr = Expression('Plus', Expression('Plus', Symbol('x'), Symbol('y')),  Integer(5))
        args = [CompileArg('System`x', int_type), CompileArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(1, 2.5), 8.5)

    def test_d(self):
        expr = Expression('Plus', Symbol('x'), MachineReal(1.5), Integer(2), Symbol('x'))
        args = [CompileArg('System`x', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(2.5), 8.5)

    def _test_unary_math(self, name, fn):
        expr = Expression(name, Symbol('x'))
        args = [CompileArg('System`x', real_type)]
        cfunc = _compile(expr, args)
        for _ in range(1000):
            x = random.random()
            self.assertAlmostEqual(cfunc(x), float(fn(x)))

    def _test_binary_math(self, name, fn):
        expr = Expression(name, Symbol('x'), Symbol('y'))
        args = [CompileArg('System`x', real_type), CompileArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        for _ in range(1000):
            x = random.random()
            y = random.random()
            self.assertAlmostEqual(cfunc(x, y), fn(x, y))

    def test_plus(self):
        self._test_binary_math('Plus', lambda x, y: x + y)

    def test_times(self):
        self._test_binary_math('Times', lambda x, y: x * y)

    def test_sin(self):
        self._test_unary_math('Sin', mpmath.sin)

    def test_cos(self):
        self._test_unary_math('Cos', mpmath.cos)

    def test_tan(self):
        self._test_unary_math('Tan', mpmath.tan)

    def test_cot(self):
        self._test_unary_math('Cot', mpmath.cot)

    def test_sec(self):
        self._test_unary_math('Sec', mpmath.sec)

    def test_csc(self):
        self._test_unary_math('Csc', mpmath.csc)

    def test_pow_real(self):
        self._test_binary_math('Power', mpmath.power)

    def test_pow_int(self):
        expr = Expression('Power', MachineReal(2.5), Symbol('x'))
        args = [CompileArg('System`x', int_type)]
        cfunc = _compile(expr, args)
        self.assertEquals(cfunc(4), 2.5 ** 4)

    def test_pow_2(self):
        # 2 ^ x
        expr = Expression('Power', Integer(2), Symbol('x'))
        args = [CompileArg('System`x', real_type)]
        cfunc = _compile(expr, args)
        for _ in range(1000):
            x = random.random()
            self.assertAlmostEqual(cfunc(x), mpmath.power(2, x))

    def test_pow_E(self):
        # E ^ x
        expr = Expression('Power', Symbol('E'), Symbol('x'))
        args = [CompileArg('System`x', real_type)]
        cfunc = _compile(expr, args)
        for _ in range(1000):
            x = random.random()
            self.assertAlmostEqual(cfunc(x), mpmath.exp(x))

    def test_hyperbolics(self):
        self._test_unary_math('Sinh', mpmath.sinh)
        self._test_unary_math('Cosh', mpmath.cosh)
        self._test_unary_math('Tanh', mpmath.tanh)
        self._test_unary_math('Csch', mpmath.csch)
        self._test_unary_math('Sech', mpmath.sech)
        self._test_unary_math('Coth', mpmath.coth)

    def test_div0(self):
        expr = Expression('Power', Symbol('x'), Symbol('y'))
        args = [CompileArg('System`x', real_type), CompileArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(0.0, -1.5), float('+inf'))
        self.assertEqual(cfunc(0.0, -1.0), float('+inf'))
        self.assertEqual(cfunc(-0.0, -1.0), float('-inf'))
        self.assertEqual(cfunc(0.0, 0.0), float('1.0'))     # NaN?

    def test_exp(self):
        self._test_unary_math('Exp', mpmath.exp)

    def test_log(self):
        self._test_unary_math('Log', mpmath.log)

    def test_abs(self):
        self._test_unary_math('Abs', abs)

    def test_max(self):
        self._test_binary_math('Max', max)

    def test_min(self):
        self._test_binary_math('Min', min)


class FlowControlTest(CompileTest):

    def test_if(self):
        expr = Expression('If', Symbol('x'), Symbol('y'), Symbol('z'))
        args = [CompileArg('System`x', int_type), CompileArg('System`y', real_type), CompileArg('System`z', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(0, 3.0, 4.0), 4.0)
        self.assertEqual(cfunc(1, 3.0, 4.0), 3.0)
        self.assertEqual(cfunc(2, 3.0, 4.0), 3.0)

    def test_if_cont(self):
        expr = Expression('Plus', Integer(1), Expression('If', Symbol('x'), Expression('Sin', Symbol('y')), Expression('Cos', Symbol('y'))))
        args = [CompileArg('System`x', int_type), CompileArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(0, 0.0), 2.0)
        self.assertEqual(cfunc(1, 0.0), 1.0)

    def test_if_eq(self):
        expr = Expression('If', Expression('Equal', Symbol('x'), Integer(1)), Integer(2), Integer(3))
        args = [CompileArg('System`x', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(1), 2)
        self.assertEqual(cfunc(2), 3)

    def test_if_int_real(self):
        expr = Expression('If', Symbol('x'), Integer(2), MachineReal(3))
        args = [CompileArg('System`x', bool_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(True), 2.0)
        self.assertEqual(cfunc(False), 3.0)

    def test_if_real_int(self):
        expr = Expression('If', Symbol('x'), MachineReal(3), Integer(2))
        args = [CompileArg('System`x', bool_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(True), 3.0)
        self.assertEqual(cfunc(False), 2.0)

    def test_if_bool_bool(self):
        expr = Expression('If', Symbol('x'), Symbol('y'), Symbol('z'))
        args = [CompileArg('System`x', bool_type), CompileArg('System`y', bool_type), CompileArg('System`z', bool_type)]
        cfunc = _compile(expr, args)
        self.assertTrue(cfunc(True, True, False))
        self.assertTrue(cfunc(False, False, True))
        self.assertFalse(cfunc(True, False, True))
        self.assertFalse(cfunc(False, True, False))

    def test_return(self):
        expr = Expression('Return', Symbol('x'))
        args = [CompileArg('System`x', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(1), 1)

    def test_if_return1(self):
        expr = Expression('If', Symbol('x'), Expression('Return', Symbol('y')), Integer(3))
        args = [CompileArg('System`x', bool_type), CompileArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(True, 1), 1)
        self.assertEqual(cfunc(False, 1), 3)

    def test_if_return2(self):
        expr = Expression('If', Symbol('x'), Expression('Return', Symbol('y')), Expression('Return', Integer(3)))
        args = [CompileArg('System`x', bool_type), CompileArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(True, 1), 1)
        self.assertEqual(cfunc(False, 1), 3)

    def test_if_return3(self):
        expr = Expression('If', Symbol('x'), Symbol('y'), Expression('Return', Integer(3)))
        args = [CompileArg('System`x', bool_type), CompileArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(True, 1), 1)
        self.assertEqual(cfunc(False, 1), 3)

    def test_expr_return(self):
        expr = Expression('Plus', Integer(3), Expression('Return', Symbol('x')))
        args = [CompileArg('System`x', int_type)]
        cfunc = _compile(expr, args)
        self.assertEqual(cfunc(1), 1)
        self.assertEqual(cfunc(4), 4)

class ComparisonTest(CompileTest):
    def test_int_equal(self):
        expr = Expression('Equal', Symbol('x'), Symbol('y'), Integer(3))
        args = [CompileArg('System`x', int_type), CompileArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertTrue(cfunc(3, 3))
        self.assertFalse(cfunc(2, 2))
        self.assertFalse(cfunc(2, 3))

    def test_int_unequal(self):
        expr = Expression('Unequal', Symbol('x'), Symbol('y'), Integer(3))
        args = [CompileArg('System`x', int_type), CompileArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertTrue(cfunc(1, 2))
        self.assertFalse(cfunc(3, 2))
        self.assertFalse(cfunc(2, 3))
        self.assertFalse(cfunc(2, 2))
        self.assertFalse(cfunc(3, 3))

    def test_real_equal(self):
        expr = Expression('Equal', Symbol('x'), Symbol('y'))
        args = [CompileArg('System`x', real_type), CompileArg('System`y', real_type)]
        cfunc = _compile(expr, args)
        self.assertTrue(cfunc(3.0, 3.0))
        self.assertFalse(cfunc(3.0, 2.0))
        self.assertFalse(cfunc(2.0, 3.0))
        # TODO NaN/+inf/-inf comparisons

    def test_int_real_equal(self):
        expr = Expression('Equal', Symbol('x'), Symbol('y'))
        args = [CompileArg('System`x', real_type), CompileArg('System`y', int_type)]
        cfunc = _compile(expr, args)
        self.assertTrue(cfunc(3.0, 3))
        self.assertFalse(cfunc(3.0, 2))
        self.assertFalse(cfunc(2.0, 3))

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
            check = getattr(self, 'assert' + str(result))

            expr = Expression(head, Symbol('x'), Symbol('y'))
            int_args = [CompileArg('System`x', int_type), CompileArg('System`y', int_type)]
            cfunc = _compile(expr, int_args)
            check(cfunc(*args))

            real_args = [CompileArg('System`x', real_type), CompileArg('System`y', real_type)]
            cfunc = _compile(expr, real_args)
            check(cfunc(*(float(arg) for arg in args)))

class LogicTest(CompileTest):
    def _test_logic(self, head, args, result):
            check = getattr(self, 'assert' + str(result))
            arg_names = ['x%i' % i for i in range(len(args))]
            expr = Expression(head, *(Symbol(arg_name) for arg_name in arg_names))
            bool_args = [CompileArg('System`' + arg_name, bool_type) for arg_name in arg_names]
            cfunc = _compile(expr, bool_args)
            check(cfunc(*args))

    def test_and(self):
        self._test_logic('And', [False], False)
        self._test_logic('And', [True], True)
        self._test_logic('And', [True, True], True)
        self._test_logic('And', [True, False], False)
        self._test_logic('And', [False, True], False)
        self._test_logic('And', [False, False], False)
        self._test_logic('And', [True, True, True], True)
        self._test_logic('And', [True, True, False], False)

    def test_or(self):
        self._test_logic('Or', [True, True], True)
        self._test_logic('Or', [True, False], True)
        self._test_logic('Or', [False, True], True)
        self._test_logic('Or', [False, False], False)
        self._test_logic('Or', [True, True, True], True)
        self._test_logic('Or', [True, True, False], True)
        self._test_logic('Or', [True, False, False], True)
        self._test_logic('Or', [False, False, False], False)

    def test_xor(self):
        # result determined by even/odd number of True/False
        self._test_logic('Xor', [True, True], False)
        self._test_logic('Xor', [True, False], True)
        self._test_logic('Xor', [False, True], True)
        self._test_logic('Xor', [False, False], False)
        self._test_logic('Xor', [True, True, True], True)
        self._test_logic('Xor', [True, True, False], False)
        self._test_logic('Xor', [True, False, True], False)
        self._test_logic('Xor', [False, True, True], False)
        self._test_logic('Xor', [True, False, False], True)
        self._test_logic('Xor', [False, True, False], True)
        self._test_logic('Xor', [False, False, True], True)
        self._test_logic('Xor', [False, False, False], False)


    def test_not(self):
        self._test_logic('Not', [True], False)
        self._test_logic('Not', [False], True)


class BitwiseTest(CompileTest):
    def _test_bitwise(self, head, args, result):
            arg_names = ['x%i' % i for i in range(len(args))]
            expr = Expression(head, *(Symbol(arg_name) for arg_name in arg_names))
            int_args = [CompileArg('System`' + arg_name, int_type) for arg_name in arg_names]
            cfunc = _compile(expr, int_args)
            self.assertEqual(cfunc(*args), result)

    def test_bitand(self):
        self._test_bitwise('BitAnd', [17], 17)
        self._test_bitwise('BitAnd', [318931, 313144, 34141], 34064)

    def test_bitor(self):
        self._test_bitwise('BitOr', [17], 17)
        self._test_bitwise('BitOr', [318931, 313144, 34141], 319487)

    def test_bitxor(self):
        self._test_bitwise('BitXor', [17], 17)
        self._test_bitwise('BitXor', [318931, 313144, 34141], 40886)

    def test_bitnot(self):
        self._test_bitwise('BitNot', [0], -1)
        self._test_bitwise('BitNot', [13413], -13414)
