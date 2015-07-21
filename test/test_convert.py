import sympy
import mathics
import random
import sys
import unittest


class SympyConvert(unittest.TestCase):
    def compare(self, mathics_expr, sympy_expr):
        self.assertEqual(mathics_expr.to_sympy(), sympy_expr)
        self.assertEqual(mathics_expr, mathics.from_sympy(sympy_expr))

    def testSymbol(self):
        self.compare(
            mathics.Symbol('Global`x'),
            sympy.Symbol('_Mathics_User_Global`x'))
        self.compare(
            mathics.Symbol('_Mathics_User_x'),
            sympy.Symbol('_Mathics_User_System`_Mathics_User_x'))

    def testReal(self):
        self.compare(mathics.Real('1.0'), sympy.Float('1.0'))
        self.compare(mathics.Real(1.0), sympy.Float(1.0))

    def testInteger(self):
        self.compare(mathics.Integer(0), sympy.Integer(0))
        self.compare(mathics.Integer(1), sympy.Integer(1))

        n = random.randint(-sys.maxint, sys.maxint)
        self.compare(mathics.Integer(n), sympy.Integer(n))

        n = random.randint(sys.maxint, sys.maxint * sys.maxint)
        self.compare(mathics.Integer(n), sympy.Integer(n))

    def testComplex(self):
        self.compare(
            mathics.Complex(mathics.Real('1.0'), mathics.Real('1.0')),
            sympy.Add(sympy.Float('1.0'), sympy.Float('1.0') * sympy.I))

        self.compare(
            mathics.Complex(mathics.Integer(0), mathics.Integer(1)),
            sympy.I)

        self.compare(
            mathics.Complex(mathics.Integer(-1), mathics.Integer(1)),
            sympy.Integer(-1) + sympy.I)

    def testString(self):
        self.compare(
            mathics.String('abcd'),
            "abcd")

    def testAdd(self):
        self.compare(
            mathics.Expression(
                'Plus', mathics.Integer(1), mathics.Symbol('Global`x')),
            sympy.Add(
                sympy.Integer(1),
                sympy.Symbol('_Mathics_User_Global`x')))

    def testIntegrate(self):
        self.compare(
            mathics.Expression(
                'Integrate', mathics.Symbol('Global`x'),
                mathics.Symbol('Global`y')),
            sympy.Integral(
                sympy.Symbol('_Mathics_User_Global`x'),
                sympy.Symbol('_Mathics_User_Global`y')))

    def testDerivative(self):
        self.compare(
            mathics.Expression(
                'D', mathics.Symbol('Global`x'), mathics.Symbol('Global`y')),
            sympy.Derivative(
                sympy.Symbol('_Mathics_User_Global`x'),
                sympy.Symbol('_Mathics_User_Global`y')))

    def testExpression(self):
        self.compare(
            mathics.Expression('Sin', mathics.Symbol('Global`x')),
            sympy.sin(sympy.Symbol('_Mathics_User_Global`x')))


class PythonConvert(unittest.TestCase):
    def compare(self, mathics_expr, python_expr):
        self.assertEqual(mathics_expr.to_python(), python_expr)
        self.assertEqual(mathics_expr, mathics.from_python(python_expr))

    def testReal(self):
        self.compare(mathics.Real('0.0'), 0.0)
        self.compare(mathics.Real('1.5'), 1.5)
        self.compare(mathics.Real('-1.5'), -1.5)

    def testInteger(self):
        self.compare(mathics.Integer(1), 1)

    @unittest.expectedFailure
    def testString(self):
        self.compare(mathics.String("abc"), '"abc"')

    @unittest.expectedFailure
    def testSymbol(self):
        self.compare(mathics.Symbol("abc"), "abc")

    def testComplex(self):
        self.compare(mathics.Complex(1, 1), 1 + 1j)
        self.compare(mathics.Complex(1.0, 1.0), 1.0 + 1.0j)
        self.compare(mathics.Complex(1, 1.0), 1 + 1.0j)
        self.compare(mathics.Complex(1.0, 1), 1.0 + 1j)

        self.compare(mathics.Complex(0, 1), 1j)
        self.compare(mathics.Complex(1, 0), 1)

    def testList(self):
        self.compare(mathics.Expression('List', mathics.Integer(1)), [1])


if __name__ == '__main__':
    unittest.main()
