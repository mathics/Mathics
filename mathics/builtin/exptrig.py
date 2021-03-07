# -*- coding: utf-8 -*-

r"""
Exponential, Trigonometric and Hyperbolic Functions

\Mathics basically supports all important trigonometric and hyperbolic functions.

Numerical values and derivatives can be computed; however, most special exact values and simplification rules are not implemented yet.
"""

import mpmath

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin
from mathics.core.expression import (
    Expression,
    Real,
    Integer,
    Symbol,
)

from mathics.builtin.numeric import Fold
from mathics.builtin.arithmetic import _MPMathFunction


class AnglePath(Builtin):
    """
    <dl>
    <dt>'AnglePath[{$phi1$, $phi2$, ...}]'
        <dd>returns the points formed by a turtle starting at {0, 0} and angled at 0 degrees going through
        the turns given by angles $phi1$, $phi2$, ... and using distance 1 for each step.
    <dt>'AnglePath[{{$r1$, $phi1$}, {$r2$, $phi2$}, ...}]'
        <dd>instead of using 1 as distance, use $r1$, $r2$, ... as distances for the respective steps.
    <dt>'AngleVector[$phi0$, {$phi1$, $phi2$, ...}]'
        <dd>returns the points on a path formed by a turtle starting with direction $phi0$ instead of 0.
    <dt>'AngleVector[{$x$, $y$}, {$phi1$, $phi2$, ...}]'
        <dd>returns the points on a path formed by a turtle starting at {$x, $y} instead of {0, 0}.
    <dt>'AngleVector[{{$x$, $y$}, $phi0$}, {$phi1$, $phi2$, ...}]'
        <dd>specifies initial position {$x$, $y$} and initial direction $phi0$.
    <dt>'AngleVector[{{$x$, $y$}, {$dx$, $dy$}}, {$phi1$, $phi2$, ...}]'
        <dd>specifies initial position {$x$, $y$} and a slope {$dx$, $dy$} that is understood to be the
        initial direction of the turtle.
    </dl>

    >> AnglePath[{90 Degree, 90 Degree, 90 Degree, 90 Degree}]
     = {{0, 0}, {0, 1}, {-1, 1}, {-1, 0}, {0, 0}}

    >> AnglePath[{{1, 1}, 90 Degree}, {{1, 90 Degree}, {2, 90 Degree}, {1, 90 Degree}, {2, 90 Degree}}]
     = {{1, 1}, {0, 1}, {0, -1}, {1, -1}, {1, 1}}

    >> AnglePath[{a, b}]
     = {{0, 0}, {Cos[a], Sin[a]}, {Cos[a] + Cos[a + b], Sin[a] + Sin[a + b]}}

    >> Precision[Part[AnglePath[{N[1/3, 100], N[2/3, 100]}], 2, 1]]
     = 100.

    >> Graphics[Line[AnglePath[Table[1.7, {50}]]]]
     = -Graphics-

    >> Graphics[Line[AnglePath[RandomReal[{-1, 1}, {100}]]]]
     = -Graphics-
    """

    messages = {"steps": "`1` is not a valid description of steps."}

    @staticmethod
    def _compute(x0, y0, phi0, steps, evaluation):
        if not steps:
            return Expression("List")

        if steps[0].get_head_name() == "System`List":

            def parse(step):
                if step.get_head_name() != "System`List":
                    raise _IllegalStepSpecification
                arguments = step.leaves
                if len(arguments) != 2:
                    raise _IllegalStepSpecification
                return arguments

        else:

            def parse(step):
                if step.get_head_name() == "System`List":
                    raise _IllegalStepSpecification
                return None, step

        try:
            fold = AnglePathFold(parse)
            leaves = [
                Expression("List", x, y) for x, y, _ in fold.fold((x0, y0, phi0), steps)
            ]
            return Expression("List", *leaves)
        except _IllegalStepSpecification:
            evaluation.message("AnglePath", "steps", Expression("List", *steps))

    def apply(self, steps, evaluation):
        "AnglePath[{steps___}]"
        return AnglePath._compute(
            Integer(0), Integer(0), None, steps.get_sequence(), evaluation
        )

    def apply_phi0(self, phi0, steps, evaluation):
        "AnglePath[phi0_, {steps___}]"
        return AnglePath._compute(
            Integer(0), Integer(0), phi0, steps.get_sequence(), evaluation
        )

    def apply_xy(self, x, y, steps, evaluation):
        "AnglePath[{x_, y_}, {steps___}]"
        return AnglePath._compute(x, y, None, steps.get_sequence(), evaluation)

    def apply_xy_phi0(self, x, y, phi0, steps, evaluation):
        "AnglePath[{{x_, y_}, phi0_}, {steps___}]"
        return AnglePath._compute(x, y, phi0, steps.get_sequence(), evaluation)

    def apply_xy_dx(self, x, y, dx, dy, steps, evaluation):
        "AnglePath[{{x_, y_}, {dx_, dy_}}, {steps___}]"
        phi0 = Expression("ArcTan", dx, dy)
        return AnglePath._compute(x, y, phi0, steps.get_sequence(), evaluation)


class AnglePathFold(Fold):
    def __init__(self, parse):
        self._parse = parse

    def _operands(self, state, steps):
        SYMBOLIC = self.SYMBOLIC
        MPMATH = self.MPMATH
        FLOAT = self.FLOAT

        def check_pos_operand(x):
            if x is not None:
                if isinstance(x, Integer) and x.get_int_value() in (0, 1):
                    pass
                elif not isinstance(x, Real):
                    return SYMBOLIC
                elif not x.is_machine_precision():
                    return MPMATH
            return FLOAT

        def check_angle_operand(phi):
            if phi is not None:
                if not isinstance(phi, Real):
                    return SYMBOLIC
                elif not phi.is_machine_precision():
                    return MPMATH
            return FLOAT

        parse = self._parse

        x, y, phi = state
        mode = max(check_pos_operand(x), check_pos_operand(y), check_angle_operand(phi))
        yield mode, x, y, phi

        for step in steps:
            distance, delta_phi = parse(step)
            mode = max(check_angle_operand(delta_phi), check_pos_operand(distance))
            yield mode, distance, delta_phi

    def _fold(self, state, steps, math):
        sin = math.sin
        cos = math.cos

        x, y, phi = state

        for distance, delta_phi in steps:
            if phi is None:
                phi = delta_phi
            else:
                phi += delta_phi

            dx = cos(phi)
            dy = sin(phi)

            if distance is not None:
                dx *= distance
                dy *= distance

            x += dx
            y += dy

            yield x, y, phi


class AngleVector(Builtin):
    """
    <dl>
    <dt>'AngleVector[$phi$]'
        <dd>returns the point at angle $phi$ on the unit circle.
    <dt>'AngleVector[{$r$, $phi$}]'
        <dd>returns the point at angle $phi$ on a circle of radius $r$.
    <dt>'AngleVector[{$x$, $y$}, $phi$]'
        <dd>returns the point at angle $phi$ on a circle of radius 1 centered at {$x$, $y$}.
    <dt>'AngleVector[{$x$, $y$}, {$r$, $phi$}]'
        <dd>returns point at angle $phi$ on a circle of radius $r$ centered at {$x$, $y$}.
    </dl>

    >> AngleVector[90 Degree]
     = {0, 1}

    >> AngleVector[{1, 10}, a]
     = {1 + Cos[a], 10 + Sin[a]}
    """

    rules = {
        "AngleVector[phi_]": "{Cos[phi], Sin[phi]}",
        "AngleVector[{r_, phi_}]": "{r * Cos[phi], r * Sin[phi]}",
        "AngleVector[{x_, y_}, phi_]": "{x + Cos[phi], y + Sin[phi]}",
        "AngleVector[{x_, y_}, {r_, phi_}]": "{x + r * Cos[phi], y + r * Sin[phi]}",
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

    sympy_name = "acos"
    mpmath_name = "acos"

    rules = {
        "Derivative[1][ArcCos]": "-1/Sqrt[1-#^2]&",
        "ArcCos[0]": "Pi / 2",
        "ArcCos[1]": "0",
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
     = 0. + 1.5708 I
    >> ArcCosh[0.00000000000000000000000000000000000000]
     = 1.5707963267948966192313216916397514421 I

    #> ArcCosh[1.4]
     = 0.867015
    """

    sympy_name = "acosh"
    mpmath_name = "acosh"

    rules = {
        "Derivative[1][ArcCosh]": "1/(Sqrt[#-1]*Sqrt[#+1])&",
    }


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

    sympy_name = "acot"
    mpmath_name = "acot"

    rules = {
        "Derivative[1][ArcCot]": "-1/(1+#^2)&",
        "ArcCot[0]": "Pi / 2",
        "ArcCot[1]": "Pi / 4",
    }


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
     = 0. + 1.5708 I
    >> ArcCoth[0.5]
     = 0.549306 - 1.5708 I

    #> ArcCoth[0.000000000000000000000000000000000000000]
     = 1.57079632679489661923132169163975144210 I
    """

    sympy_name = "acoth"
    mpmath_name = "acoth"

    rules = {
        "ArcCoth[z:0.0]": "N[I / 2 Pi, Precision[1+z]]",
        "Derivative[1][ArcCoth]": "1/(1-#^2)&",
    }


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

    sympy_name = ""
    mpmath_name = "acsc"

    rules = {
        "Derivative[1][ArcCsc]": "-1 / (Sqrt[1 - 1/#^2] * #^2)&",
        "ArcCsc[0]": "ComplexInfinity",
        "ArcCsc[1]": "Pi / 2",
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression(
                "ArcSin", Expression("Power", expr.leaves[0], Integer(-1))
            ).to_sympy()


class ArcCsch(_MPMathFunction):
    """
    <dl>
    <dt>'ArcCsch[$z$]'
        <dd>returns the inverse hyperbolic cosecant of $z$.
    </dl>

    >> ArcCsch[0]
     = ComplexInfinity
    >> ArcCsch[1.0]
     = 0.881374
    """

    sympy_name = ""
    mpmath_name = "acsch"

    rules = {
        "ArcCsch[0]": "ComplexInfinity",
        "ArcCsch[0.]": "ComplexInfinity",
        "Derivative[1][ArcCsch]": "-1 / (Sqrt[1+1/#^2] * #^2) &",
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression(
                "ArcSinh", Expression("Power", expr.leaves[0], Integer(-1))
            ).to_sympy()


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

    sympy_name = ""
    mpmath_name = "asec"

    rules = {
        "Derivative[1][ArcSec]": "1 / (Sqrt[1 - 1/#^2] * #^2)&",
        "ArcSec[0]": "ComplexInfinity",
        "ArcSec[1]": "0",
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression(
                "ArcCos", Expression("Power", expr.leaves[0], Integer(-1))
            ).to_sympy()


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
     = 1.31696
    """

    sympy_name = ""
    mpmath_name = "asech"

    rules = {
        "ArcSech[0]": "Infinity",
        "ArcSech[0.]": "Indeterminate",
        "Derivative[1][ArcSech]": "-1 / (# * Sqrt[(1-#)/(1+#)] (1+#)) &",
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression(
                "ArcCosh", Expression("Power", expr.leaves[0], Integer(-1))
            ).to_sympy()


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

    sympy_name = "asin"
    mpmath_name = "asin"

    rules = {
        "Derivative[1][ArcSin]": "1/Sqrt[1-#^2]&",
        "ArcSin[0]": "0",
        "ArcSin[1]": "Pi / 2",
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
     = 0.881374
    """

    sympy_name = "asinh"
    mpmath_name = "asinh"

    rules = {
        "Derivative[1][ArcSinh]": "1/Sqrt[1+#^2]&",
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
     = 0.785398
    >> ArcTan[-1.0]
     = -0.785398

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

    sympy_name = "atan"
    mpmath_name = "atan"

    rules = {
        "ArcTan[1]": "Pi/4",
        "ArcTan[0]": "0",
        "Derivative[1][ArcTan]": "1/(1+#^2)&",
        "ArcTan[x_?RealNumberQ, y_?RealNumberQ]": """If[x == 0, If[y == 0, 0, If[y > 0, Pi/2, -Pi/2]], If[x > 0,
            ArcTan[y/x], If[y >= 0, ArcTan[y/x] + Pi, ArcTan[y/x] - Pi]]]""",
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
     = 0.0964156 + 1.12656 I
    >> ArcTanh[2 + I]
     = ArcTanh[2 + I]
    """

    sympy_name = "atanh"
    mpmath_name = "atanh"
    numpy_name = "arctanh"

    rules = {
        "Derivative[1][ArcTanh]": "1/(1-#^2)&",
    }


class Cos(_MPMathFunction):
    """
    <dl>
    <dt>'Cos[$z$]'
        <dd>returns the cosine of $z$.
    </dl>

    >> Cos[3 Pi]
     = -1

    #> Cos[1.5 Pi]
     = -1.83697*^-16
    """

    mpmath_name = "cos"

    rules = {
        "Cos[Pi]": "-1",
        "Cos[n_Integer * Pi]": "(-1)^n",
        "Cos[(1/2) * Pi]": "0",
        "Cos[0]": "1",
        "Derivative[1][Cos]": "-Sin[#]&",
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

    mpmath_name = "cosh"

    rules = {
        "Derivative[1][Cosh]": "Sinh[#]&",
    }


class Cot(_MPMathFunction):
    """
    <dl>
    <dt>'Cot[$z$]'
        <dd>returns the cotangent of $z$.
    </dl>

    >> Cot[0]
     = ComplexInfinity
    >> Cot[1.]
     = 0.642093
    """

    mpmath_name = "cot"

    rules = {
        "Derivative[1][Cot]": "-Csc[#]^2&",
        "Cot[0]": "ComplexInfinity",
    }


class Coth(_MPMathFunction):
    """
    <dl>
    <dt>'Coth[$z$]'
        <dd>returns the hyperbolic cotangent of $z$.
    </dl>

    >> Coth[0]
     = ComplexInfinity
    """

    mpmath_name = "coth"

    rules = {
        "Coth[0]": "ComplexInfinity",
        "Coth[0.]": "ComplexInfinity",
        "Derivative[1][Coth]": "-Csch[#1]^2&",
    }


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
     = 1.1884
    """

    mpmath_name = "csc"

    rules = {
        "Derivative[1][Csc]": "-Cot[#] Csc[#]&",
        "Csc[0]": "ComplexInfinity",
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression(
                "Power", Expression("Sin", expr.leaves[0]), Integer(-1)
            ).to_sympy()


class Csch(_MPMathFunction):
    """
    <dl>
    <dt>'Csch[$z$]'
        <dd>returns the hyperbolic cosecant of $z$.
    </dl>

    >> Csch[0]
     = ComplexInfinity
    """

    sympy_name = ""
    mpmath_name = "csch"

    rules = {
        "Csch[0]": "ComplexInfinity",
        "Csch[0.]": "ComplexInfinity",
        "Derivative[1][Csch]": "-Coth[#1] Csch[#1]&",
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression(
                "Power", Expression("Sinh", expr.leaves[0]), Integer(-1)
            ).to_sympy()


class Exp(_MPMathFunction):
    """
    <dl>
    <dt>'Exp[$z$]'
        <dd>returns the exponential function of $z$.
    </dl>

    >> Exp[1]
     = E
    >> Exp[10.0]
     = 22026.5
    >> Exp[x] //FullForm
     = Power[E, x]

    >> Plot[Exp[x], {x, 0, 3}]
     = -Graphics-
    #> Exp[1.*^20]
     : Overflow occurred in computation.
     = Overflow[]
    """

    rules = {
        "Exp[x_]": "E ^ x",
        "Derivative[1][Exp]": "Exp",
    }

    def from_sympy(self, sympy_name, leaves):
        return Expression("Power", Symbol("E"), leaves[0])


class Haversine(_MPMathFunction):
    """
    <dl>
      <dt>'Haversine[$z$]'
      <dd>returns the haversine function of $z$.
    </dl>

    >> Haversine[1.5]
     = 0.464631

    >> Haversine[0.5 + 2I]
     = -1.15082 + 0.869405 I
    """

    rules = {"Haversine[z_]": "Power[Sin[z/2], 2]"}


class _IllegalStepSpecification(Exception):
    pass


class InverseHaversine(_MPMathFunction):
    """
    <dl>
      <dt>'InverseHaversine[$z$]'
      <dd>returns the inverse haversine function of $z$.
    </dl>

    >> InverseHaversine[0.5]
     = 1.5708

    >> InverseHaversine[1 + 2.5 I]
     = 1.76459 + 2.33097 I
    """

    rules = {"InverseHaversine[z_]": "2 * ArcSin[Sqrt[z]]"}


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
     = 0.336472

    #> Log[Exp[1.4]]
     = 1.4

    #> Log[-1.4]
     = 0.336472 + 3.14159 I

    #> N[Log[10], 30]
     = 2.30258509299404568401799145468
    """

    nargs = 2
    mpmath_name = "log"
    sympy_name = "log"

    rules = {
        "Log[0.]": "Indeterminate",
        "Log[0]": "DirectedInfinity[-1]",
        "Log[1]": "0",
        "Log[E]": "1",
        "Log[E^x_Integer]": "x",
        "Derivative[1][Log]": "1/#&",
        "Log[x_?InexactNumberQ]": "Log[E, x]",
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
     = 2.48543
    >> Log2[E ^ 2]
     = 2 / Log[2]
    """

    rules = {
        "Log2[x_]": "Log[2, x]",
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
     = {0.30103, 0.69897}
    >> Log10[E ^ 3]
     = 3 / Log[10]
    """

    rules = {
        "Log10[x_]": "Log[10, x]",
    }


class LogisticSigmoid(Builtin):
    """
    <dl>
    <dt>'LogisticSigmoid[$z$]'
        <dd>returns the logistic sigmoid of $z$.
    </dl>

    >> LogisticSigmoid[0.5]
     = 0.622459

    >> LogisticSigmoid[0.5 + 2.3 I]
     = 1.06475 + 0.808177 I

    >> LogisticSigmoid[{-0.2, 0.1, 0.3}]
     = {0.450166, 0.524979, 0.574443}

    #> LogisticSigmoid[I Pi]
     = LogisticSigmoid[I Pi]
    """

    attributes = (
        "Listable",
        "NumericFunction",
    )

    rules = {"LogisticSigmoid[z_?NumberQ]": "1 / (1 + Exp[-z])"}


# Look over and add
# class PolyGamma(_MPMathFunction):
#     """
#     <dl>
#       <dt>'Polygama[$z$]'
#       <dd>returns the digamma function .

#       <dt>'Polygama[$n$, $z$]'
#       <dd>gives the n^(th) derivative of the digamma function .
#     </dl>

#     >> PolyGamma[5]

#     >> PolyGamma[3, 5]
#     """

#     sympy_name = "polygamma"
#     mpmath_name = "polygamma"

#     def apply_N(self, precision, evaluation):
#         "N[PolyGamma, precision_]"

#         try:
#             d = get_precision(precision, evaluation)
#         except PrecisionValueError:
#             return

#         if d is None:
#             return MachineReal(mpmath.polygamma)
#         else:
#             return PrecisionReal(sympy.polygamma.n(d))


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
     = 1.85082
    """

    mpmath_name = "sec"

    rules = {
        "Derivative[1][Sec]": "Sec[#] Tan[#]&",
        "Sec[0]": "1",
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression(
                "Power", Expression("Cos", expr.leaves[0]), Integer(-1)
            ).to_sympy()


class Sech(_MPMathFunction):
    """
    <dl>
    <dt>'Sech[$z$]'
        <dd>returns the hyperbolic secant of $z$.
    </dl>

    >> Sech[0]
     = 1
    """

    sympy_name = ""
    mpmath_name = "sech"

    rules = {
        "Derivative[1][Sech]": "-Sech[#1] Tanh[#1]&",
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr.leaves) == 1:
            return Expression(
                "Power", Expression("Cosh", expr.leaves[0]), Integer(-1)
            ).to_sympy()


class Sin(_MPMathFunction):
    """
    <dl>
    <dt>'Sin[$z$]'
        <dd>returns the sine of $z$.
    </dl>

    >> Sin[0]
     = 0
    >> Sin[0.5]
     = 0.479426
    >> Sin[3 Pi]
     = 0
    >> Sin[1.0 + I]
     = 1.29846 + 0.634964 I

    >> Plot[Sin[x], {x, -Pi, Pi}]
     = -Graphics-

    #> N[Sin[1], 40]
     = 0.8414709848078965066525023216302989996226
    """

    mpmath_name = "sin"

    rules = {
        "Sin[Pi]": "0",
        "Sin[n_Integer*Pi]": "0",
        "Sin[(1/2) * Pi]": "1",
        "Sin[0]": "0",
        "Derivative[1][Sin]": "Cos[#]&",
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

    mpmath_name = "sinh"

    rules = {
        "Derivative[1][Sinh]": "Cosh[#]&",
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

    #> Tan[0.5 Pi]
     = 1.63312*^16
    """

    mpmath_name = "tan"

    rules = {
        "Tan[(1/2) * Pi]": "ComplexInfinity",
        "Tan[0]": "0",
        "Derivative[1][Tan]": "Sec[#]^2&",
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

    mpmath_name = "tanh"

    rules = {
        "Derivative[1][Tanh]": "Sech[#1]^2&",
    }
