#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Exponential, trigonometric and hyperbolic functions

Mathics basically supports all important trigonometric and hyperbolic functions.
Numerical values and derivatives can be computed; however, most special exact values and simplification
rules are not implemented yet.
"""

from __future__ import unicode_literals
from __future__ import absolute_import

import sympy
import mpmath

from mathics.builtin.base import Builtin, SympyConstant
from mathics.core.expression import Real, Expression, Integer
from mathics.core.numbers import dps

from mathics.builtin.numeric import get_precision
from mathics.builtin.arithmetic import _MPMathFunction


class Pi(SympyConstant):
    """
    <dl>
    <dt>'Pi'
        <dd>is the constant \u03c0.
    </dl>

    >> N[Pi]
     = 3.14159265358979324
    >> N[Pi, 50]
     = 3.1415926535897932384626433832795028841971693993751

    >> Attributes[Pi]
     = {Constant, Protected, ReadProtected}
    """

    sympy_name = 'pi'

    def apply_N(self, precision, evaluation):
        'N[Pi, precision_]'
        precision = get_precision(precision, evaluation)
        if precision is not None:
            return Real(sympy.pi.n(dps(precision)), p=precision)


class E(SympyConstant):
    """
    <dl>
    <dt>'E'
        <dd>is the constant e.
    </dl>

    >> N[E]
     = 2.71828182845904524
    >> N[E, 50]
     = 2.7182818284590452353602874713526624977572470937

    >> Attributes[E]
     = {Constant, Protected, ReadProtected}

    #> 5. E
     = 13.5914091422952262
    """

    sympy_name = 'E'

    def apply_N(self, precision, evaluation):
        'N[E, precision_]'
        precision = get_precision(precision, evaluation)
        if precision is not None:
            return Real(sympy.E.n(dps(precision)), p=precision)


class GoldenRatio(SympyConstant):
    """
    <dl>
    <dt>'GoldenRatio'
        <dd>is the golden ratio.
    </dl>

    >> N[GoldenRatio]
     = 1.61803398874989485
    """

    sympy_name = 'GoldenRatio'

    rules = {
        'N[GoldenRatio, prec_]': 'N[(1+Sqrt[5])/2, prec]',
    }


class Exp(_MPMathFunction):
    """
    <dl>
    <dt>'Exp[$z$]'
        <dd>returns the exponential function of $z$.
    </dl>

    >> Exp[1]
     = E
    >> Exp[10.0]
     = 22026.4657948067165
    >> Exp[x] //FullForm
     = Power[E, x]

    >> Plot[Exp[x], {x, 0, 3}]
     = -Graphics-
    """

    rules = {
        'Exp[x_]': 'E ^ x',
        'Derivative[1][Exp]': 'Exp',
    }


class Log(_MPMathFunction):
    """
    <dl>
    <dt>'Log[$z$]'
        <dd>returns the natural logarithm of $z$.
    </dl>

    >> Log[{0, 1, E, E * E, E ^ 3, E ^ x}]
     = {-Infinity, 0, 1, 2, 3, Log[E ^ x]}
    >> Log[0.]
     = Indeterminate
    >> Plot[Log[x], {x, 0, 5}]
     = -Graphics-

    #> Log[1000] / Log[10] // Simplify
     = 3

    #> Log[1.4]
     = 0.336472236621212931

    #> Log[1.4]
     = 0.336472236621212931

    #> Log[-1.4]
     = 0.336472236621212931 + 3.14159265358979324 I
    """

    nargs = 2
    mpmath_name = 'log'
    sympy_name = 'log'

    rules = {
        'Log[0.]': 'Indeterminate',
        'Log[0]': 'DirectedInfinity[-1]',
        'Log[1]': '0',
        'Log[E]': '1',
        'Log[E^x_Integer]': 'x',
        'Derivative[1][Log]': '1/#&',
        'Log[x_?InexactNumberQ]': 'Log[E, x]',
    }

    def prepare_sympy(self, leaves):
        if len(leaves) == 2:
            leaves = [leaves[1], leaves[0]]
        return leaves

    def get_mpmath_function(self, args):
        return lambda base, x: mpmath.log(x, base)


class Log2(Builtin):
    """
    <dl>
    <dt>'Log2[$z$]'
        <dd>returns the base-2 logarithm of $z$.
    </dl>

    >> Log2[4 ^ 8]
     = 16
    >> Log2[5.6]
     = 2.48542682717024176
    >> Log2[E ^ 2]
     = 2 / Log[2]
    """

    rules = {
        'Log2[x_]': 'Log[2, x]',
    }


class Log10(Builtin):
    """
    <dl>
    <dt>'Log10[$z$]'
        <dd>returns the base-10 logarithm of $z$.
    </dl>

    >> Log10[1000]
     = 3
    >> Log10[{2., 5.}]
     = {0.301029995663981195, 0.698970004336018805}
    >> Log10[E ^ 3]
     = 3 / Log[10]
    """

    rules = {
        'Log10[x_]': 'Log[10, x]',
    }


class Sin(_MPMathFunction):
    """
    <dl>
    <dt>'Sin[$z$]'
        <dd>returns the sine of $z$.
    </dl>

    >> Sin[0]
     = 0
    >> Sin[0.5]
     = 0.479425538604203
    >> Sin[3 Pi]
     = 0
    >> Sin[1.0 + I]
     = 1.29845758141597729 + 0.634963914784736108 I

    >> Plot[Sin[x], {x, -Pi, Pi}]
     = -Graphics-

    #> N[Sin[1], 40]
     = 0.8414709848078965066525023216302989996226
    """

    mpmath_name = 'sin'

    rules = {
        'Sin[Pi]': '0',
        'Sin[n_Integer*Pi]': '0',
        'Sin[(1/2) * Pi]': '1',
        'Sin[0]': '0',
        'Derivative[1][Sin]': 'Cos[#]&',
    }


class Cos(_MPMathFunction):
    """
    <dl>
    <dt>'Cos[$z$]'
        <dd>returns the cosine of $z$.
    </dl>

    >> Cos[3 Pi]
     = -1
    """

    mpmath_name = 'cos'

    rules = {
        'Cos[Pi]': '-1',
        'Cos[n_Integer * Pi]': '(-1)^n',
        'Cos[(1/2) * Pi]': '0',
        'Cos[0]': '1',
        'Derivative[1][Cos]': '-Sin[#]&',
    }


class Tan(_MPMathFunction):
    """
    <dl>
    <dt>'Tan[$z$]'
        <dd>returns the tangent of $z$.
    </dl>

    >> Tan[0]
     = 0
    >> Tan[Pi / 2]
     = ComplexInfinity
    """

    mpmath_name = 'tan'

    rules = {
        'Tan[(1/2) * Pi]': 'ComplexInfinity',
        'Tan[0]': '0',
        'Derivative[1][Tan]': 'Sec[#]^2&',
    }


class Sec(_MPMathFunction):
    """
    <dl>
    <dt>'Sec[$z$]'
        <dd>returns the secant of $z$.
    </dl>

    >> Sec[0]
     = 1
    >> Sec[1] (* Sec[1] in Mathematica *)
     = 1 / Cos[1]
    >> Sec[1.]
     = 1.85081571768092562
    """

    mpmath_name = 'sec'

    rules = {
        'Derivative[1][Sec]': 'Sec[#] Tan[#]&',
        'Sec[0]': '1',
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression('Power', Expression('Cos', expr.leaves[0]),
                              Integer(-1)).to_sympy()


class Csc(_MPMathFunction):
    """
    <dl>
    <dt>'Csc[$z$]'
        <dd>returns the cosecant of $z$.
    </dl>

    >> Csc[0]
     = ComplexInfinity
    >> Csc[1] (* Csc[1] in Mathematica *)
     = 1 / Sin[1]
    >> Csc[1.]
     = 1.18839510577812122
    """

    mpmath_name = 'csc'

    rules = {
        'Derivative[1][Csc]': '-Cot[#] Csc[#]&',
        'Csc[0]': 'ComplexInfinity',
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression('Power', Expression('Sin', expr.leaves[0]),
                              Integer(-1)).to_sympy()


class Cot(_MPMathFunction):
    """
    <dl>
    <dt>'Cot[$z$]'
        <dd>returns the cotangent of $z$.
    </dl>

    >> Cot[0]
     = ComplexInfinity
    >> Cot[1.]
     = 0.642092615934330703
    """

    mpmath_name = 'cot'

    rules = {
        'Derivative[1][Cot]': '-Csc[#]^2&',
        'Cot[0]': 'ComplexInfinity',
    }


class ArcSin(_MPMathFunction):
    """
    <dl>
    <dt>'ArcSin[$z$]'
        <dd>returns the inverse sine of $z$.
    </dl>

    >> ArcSin[0]
     = 0
    >> ArcSin[1]
     = Pi / 2
    """

    sympy_name = 'asin'
    mpmath_name = 'asin'

    rules = {
        'Derivative[1][ArcSin]': '1/Sqrt[1-#^2]&',
        'ArcSin[0]': '0',
        'ArcSin[1]': 'Pi / 2',
    }


class ArcCos(_MPMathFunction):
    """
    <dl>
    <dt>'ArcCos[$z$]'
        <dd>returns the inverse cosine of $z$.
    </dl>

    >> ArcCos[1]
     = 0
    >> ArcCos[0]
     = Pi / 2
    >> Integrate[ArcCos[x], {x, -1, 1}]
     = Pi
    """

    sympy_name = 'acos'
    mpmath_name = 'acos'

    rules = {
        'Derivative[1][ArcCos]': '-1/Sqrt[1-#^2]&',
        'ArcCos[0]': 'Pi / 2',
        'ArcCos[1]': '0',
    }


class ArcTan(_MPMathFunction):
    """
    <dl>
    <dt>'ArcTan[$z$]'
        <dd>returns the inverse tangent of $z$.
    </dl>

    >> ArcTan[1]
     = Pi / 4
    >> ArcTan[1.0]
     = 0.78539816339744831
    >> ArcTan[-1.0]
     = -0.78539816339744831

    >> ArcTan[1, 1]
     = Pi / 4
    #> ArcTan[-1, 1]
     = 3 Pi / 4
    #> ArcTan[1, -1]
     = -Pi / 4
    #> ArcTan[-1, -1]
     = -3 Pi / 4

    #> ArcTan[1, 0]
     = 0
    #> ArcTan[-1, 0]
     = Pi
    #> ArcTan[0, 1]
     = Pi / 2
    #> ArcTan[0, -1]
     = -Pi / 2
    """

    sympy_name = 'atan'
    mpmath_name = 'atan'

    rules = {
        'ArcTan[1]': 'Pi/4',
        'ArcTan[0]': '0',
        'Derivative[1][ArcTan]': '1/(1+#^2)&',
        'ArcTan[x_?RealNumberQ, y_?RealNumberQ]':
        '''If[x == 0, If[y == 0, 0, If[y > 0, Pi/2, -Pi/2]], If[x > 0,
            ArcTan[y/x], If[y >= 0, ArcTan[y/x] + Pi, ArcTan[y/x] - Pi]]]''',
    }


class ArcSec(_MPMathFunction):
    """
    <dl>
    <dt>'ArcSec[$z$]'
        <dd>returns the inverse secant of $z$.
    </dl>

    >> ArcSec[1]
     = 0
    >> ArcSec[-1]
     = Pi
    """

    sympy_name = ''
    mpmath_name = 'asec'

    rules = {
        'Derivative[1][ArcSec]': '1 / (Sqrt[1 - 1/#^2] * #^2)&',
        'ArcSec[0]': 'ComplexInfinity',
        'ArcSec[1]': '0',
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression('ArcCos', Expression('Power', expr.leaves[0],
                              Integer(-1))).to_sympy()


class ArcCsc(_MPMathFunction):
    """
    <dl>
    <dt>'ArcCsc[$z$]'
        <dd>returns the inverse cosecant of $z$.
    </dl>

    >> ArcCsc[1]
     = Pi / 2
    >> ArcCsc[-1]
     = -Pi / 2
    """

    sympy_name = ''
    mpmath_name = 'acsc'

    rules = {
        'Derivative[1][ArcCsc]': '-1 / (Sqrt[1 - 1/#^2] * #^2)&',
        'ArcCsc[0]': 'ComplexInfinity',
        'ArcCsc[1]': 'Pi / 2',
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression('ArcSin', Expression('Power', expr.leaves[0],
                              Integer(-1))).to_sympy()


class ArcCot(_MPMathFunction):
    """
    <dl>
    <dt>'ArcCot[$z$]'
        <dd>returns the inverse cotangent of $z$.
    </dl>

    >> ArcCot[0]
     = Pi / 2
    >> ArcCot[1]
     = Pi / 4
    """

    sympy_name = 'acot'
    mpmath_name = 'acot'

    rules = {
        'Derivative[1][ArcCot]': '-1/(1+#^2)&',
        'ArcCot[0]': 'Pi / 2',
        'ArcCot[1]': 'Pi / 4',
    }


class Sinh(_MPMathFunction):
    """
    <dl>
    <dt>'Sinh[$z$]'
        <dd>returns the hyperbolic sine of $z$.
    </dl>

    >> Sinh[0]
     = 0
    """

    mpmath_name = 'sinh'

    rules = {
        'Derivative[1][Sinh]': 'Cosh[#]&',
    }


class Cosh(_MPMathFunction):
    """
    <dl>
    <dt>'Cosh[$z$]'
        <dd>returns the hyperbolic cosine of $z$.
    </dl>

    >> Cosh[0]
     = 1
    """

    mpmath_name = 'cosh'

    rules = {
        'Derivative[1][Cosh]': 'Sinh[#]&',
    }


class Tanh(_MPMathFunction):
    """
    <dl>
    <dt>'Tanh[$z$]'
        <dd>returns the hyperbolic tangent of $z$.
    </dl>

    >> Tanh[0]
     = 0
    """

    mpmath_name = 'tanh'

    rules = {
        'Derivative[1][Tanh]': 'Sech[#1]^2&',
    }


class Sech(_MPMathFunction):
    """
    <dl>
    <dt>'Sech[$z$]'
        <dd>returns the hyperbolic secant of $z$.
    </dl>

    >> Sech[0]
     = 1
    """

    sympy_name = ''
    mpmath_name = 'sech'

    rules = {
        'Derivative[1][Sech]': '-Sech[#1] Tanh[#1]&',
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression('Power', Expression('Cosh', expr.leaves[0]),
                              Integer(-1)).to_sympy()


class Csch(_MPMathFunction):
    """
    <dl>
    <dt>'Csch[$z$]'
        <dd>returns the hyperbolic cosecant of $z$.
    </dl>

    >> Csch[0]
     = ComplexInfinity
    """

    sympy_name = ''
    mpmath_name = 'csch'

    rules = {
        'Csch[0]': 'ComplexInfinity',
        'Csch[0.]': 'ComplexInfinity',
        'Derivative[1][Csch]': '-Coth[#1] Csch[#1]&',
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression('Power', Expression('Sinh', expr.leaves[0]),
                              Integer(-1)).to_sympy()


class Coth(_MPMathFunction):
    """
    <dl>
    <dt>'Coth[$z$]'
        <dd>returns the hyperbolic cotangent of $z$.
    </dl>

    >> Coth[0]
     = ComplexInfinity
    """

    mpmath_name = 'coth'

    rules = {
        'Coth[0]': 'ComplexInfinity',
        'Coth[0.]': 'ComplexInfinity',
        'Derivative[1][Coth]': '-Csch[#1]^2&',
    }


class ArcSinh(_MPMathFunction):
    """
    <dl>
    <dt>'ArcSinh[$z$]'
        <dd>returns the inverse hyperbolic sine of $z$.
    </dl>

    >> ArcSinh[0]
     = 0
    >> ArcSinh[0.]
     = 0.
    >> ArcSinh[1.0]
     = 0.881373587019543025
    """

    sympy_name = 'asinh'
    mpmath_name = 'asinh'

    rules = {
        'Derivative[1][ArcSinh]': '1/Sqrt[1+#^2]&',
    }


class ArcCosh(_MPMathFunction):
    """
    <dl>
    <dt>'ArcCosh[$z$]'
        <dd>returns the inverse hyperbolic cosine of $z$.
    </dl>

    >> ArcCosh[0]
     = I / 2 Pi
    >> ArcCosh[0.]
     = 0. + 1.57079632679489662 I
    >> ArcCosh[0.00000000000000000000000000000000000000]
     = 0.*^-38 + 1.5707963267948966192313216916397514421 I

    #> ArcCosh[1.4]
     = 0.867014726490565104
    """

    sympy_name = 'acosh'
    mpmath_name = 'acosh'

    rules = {
        'ArcCosh[z:0.0]': 'N[I / 2 Pi, Precision[1+z]]',
        'Derivative[1][ArcCosh]': '1/(Sqrt[#-1]*Sqrt[#+1])&',
    }


class ArcTanh(_MPMathFunction):
    """
    <dl>
    <dt>'ArcTanh[$z$]'
        <dd>returns the inverse hyperbolic tangent of $z$.
    </dl>

    >> ArcTanh[0]
     = 0
    >> ArcTanh[1]
     = Infinity
    >> ArcTanh[0]
     = 0
    >> ArcTanh[.5 + 2 I]
     = 0.0964156202029961672 + 1.12655644083482235 I
    >> ArcTanh[2 + I]
     = ArcTanh[2 + I]
    """

    sympy_name = 'atanh'
    mpmath_name = 'atanh'

    rules = {
        'Derivative[1][ArcTanh]': '1/(1-#^2)&',
    }


class ArcSech(_MPMathFunction):
    """
    <dl>
    <dt>'ArcSech[$z$]'
        <dd>returns the inverse hyperbolic secant of $z$.
    </dl>

    >> ArcSech[0]
     = Infinity
    >> ArcSech[1]
     = 0
    >> ArcSech[0.5]
     = 1.31695789692481671
    """

    sympy_name = ''
    mpmath_name = 'asech'

    rules = {
        'ArcSech[0]': 'Infinity',
        'ArcSech[0.]': 'Indeterminate',
        'Derivative[1][ArcSech]': '-1 / (# * Sqrt[(1-#)/(1+#)] (1+#)) &',
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression('ArcCosh', Expression('Power', expr.leaves[0],
                              Integer(-1))).to_sympy()


class ArcCsch(_MPMathFunction):
    """
    <dl>
    <dt>'ArcCsch[$z$]'
        <dd>returns the inverse hyperbolic cosecant of $z$.
    </dl>

    >> ArcCsch[0]
     = ComplexInfinity
    >> ArcCsch[1.0]
     = 0.881373587019543025
    """

    sympy_name = ''
    mpmath_name = 'acsch'

    rules = {
        'ArcCsch[0]': 'ComplexInfinity',
        'ArcCsch[0.]': 'ComplexInfinity',
        'Derivative[1][ArcCsch]': '-1 / (Sqrt[1+1/#^2] * #^2) &',
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression('ArcSinh', Expression('Power', expr.leaves[0],
                              Integer(-1))).to_sympy()


class ArcCoth(_MPMathFunction):
    """
    <dl>
    <dt>'ArcCoth[$z$]'
        <dd>returns the inverse hyperbolic cotangent of $z$.
    </dl>

    >> ArcCoth[0]
     = I / 2 Pi
    >> ArcCoth[1]
     = Infinity
    >> ArcCoth[0.0]
     = 0. + 1.57079632679489662 I
    >> ArcCoth[0.5]
     = 0.549306144334054846 - 1.57079632679489662 I
    """

    sympy_name = 'acoth'
    mpmath_name = 'acoth'

    rules = {
        'ArcCoth[z:0.0]': 'N[I / 2 Pi, Precision[1+z]]',
        'Derivative[1][ArcCoth]': '1/(1-#^2)&',
    }


class Haversine(_MPMathFunction):
    """
    <dl>
    <dt>'Haversine[$z$]'
        <dd>returns the haversine function of $z$.
    </dl>

    >> Haversine[1.5]
     = 0.464631399166148545

    >> Haversine[0.5 + 2I]
     = -1.15081866645704728 + 0.869404752237158167 I
    """

    rules = {
        'Haversine[z_]': 'Power[Sin[z/2], 2]'
    }


class InverseHaversine(_MPMathFunction):
    """
    <dl>
    <dt>'Haversine[$z$]'
        <dd>returns the inverse haversine function of $z$.
    </dl>

    >> InverseHaversine[0.5]
     = 1.57079632679489662

    >> InverseHaversine[1 + 2.5 I]
     = 1.76458946334982881 + 2.33097465304931242 I
    """

    rules = {
        'InverseHaversine[z_]': '2 * ArcSin[Sqrt[z]]'
    }
