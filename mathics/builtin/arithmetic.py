# -*- coding: utf-8 -*-
# cython: language_level=3

"""
Mathematical Functions

Basic arithmetic functions, including complex number arithmetic.
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

import sympy
import sys
import mpmath
from functools import lru_cache

from mathics.builtin.base import (
    Builtin,
    PostfixOperator,
    Predefined,
    SympyFunction,
    Test,
)

from mathics.core.expression import (
    Complex,
    Expression,
    Integer,
    Integer0,
    Integer1,
    Number,
    Rational,
    Real,
    String,
    Symbol,
    SymbolN,
    SymbolTrue,
    SymbolFalse,
    SymbolUndefined,
    SymbolList,
    from_mpmath,
    from_python,
)
from mathics.core.numbers import min_prec, dps, SpecialValueError

from mathics.builtin.lists import _IterationFunction
from mathics.core.convert import from_sympy, SympyExpression, sympy_symbol_prefix
from mathics.builtin.scoping import dynamic_scoping
from mathics.builtin.inference import get_assumptions_list, evaluate_predicate
from mathics.builtin.numeric import apply_N


@lru_cache(maxsize=1024)
def call_mpmath(mpmath_function, mpmath_args):
    try:
        return mpmath_function(*mpmath_args)
    except ValueError as exc:
        text = str(exc)
        if text == "gamma function pole":
            return Symbol("ComplexInfinity")
        else:
            raise
    except ZeroDivisionError:
        return
    except SpecialValueError as exc:
        return Symbol(exc.name)


class _MPMathFunction(SympyFunction):

    # These below attributes are the default attributes:
    #
    # * functions take lists as an argument
    # * functions take numeric values only
    # * functions can't be changed
    #
    # However hey are not correct for some derived classes, like
    # InverseErf or InverseErfc.
    # So those classes should expclicitly set/override this.
    attributes = ("Listable", "NumericFunction", "Protected")

    mpmath_name = None

    nargs = 1

    @lru_cache(maxsize=1024)
    def get_mpmath_function(self, args):
        if self.mpmath_name is None or len(args) != self.nargs:
            return None
        return getattr(mpmath, self.mpmath_name)

    def apply(self, z, evaluation):
        "%(name)s[z__]"

        args = z.numerify(evaluation).get_sequence()
        mpmath_function = self.get_mpmath_function(tuple(args))
        result = None

        # if no arguments are inexact attempt to use sympy
        if all(not x.is_inexact() for x in args):
            result = Expression(self.get_name(), *args).to_sympy()
            result = self.prepare_mathics(result)
            result = from_sympy(result)
            # evaluate leaves to convert e.g. Plus[2, I] -> Complex[2, 1]
            return result.evaluate_leaves(evaluation)
        elif mpmath_function is None:
            return

        if not all(isinstance(arg, Number) for arg in args):
            return

        if any(arg.is_machine_precision() for arg in args):
            # if any argument has machine precision then the entire calculation
            # is done with machine precision.
            float_args = [
                arg.round().get_float_value(permit_complex=True) for arg in args
            ]
            if None in float_args:
                return

            result = call_mpmath(mpmath_function, tuple(float_args))
            if isinstance(result, (mpmath.mpc, mpmath.mpf)):
                if mpmath.isinf(result) and isinstance(result, mpmath.mpc):
                    result = Symbol("ComplexInfinity")
                elif mpmath.isinf(result) and result > 0:
                    result = Expression("DirectedInfinity", Integer1)
                elif mpmath.isinf(result) and result < 0:
                    result = Expression("DirectedInfinity", Integer(-1))
                elif mpmath.isnan(result):
                    result = Symbol("Indeterminate")
                else:
                    result = from_mpmath(result)
        else:
            prec = min_prec(*args)
            d = dps(prec)
            args = [apply_N(arg, evaluation, Integer(d)) for arg in args]
            with mpmath.workprec(prec):
                mpmath_args = [x.to_mpmath() for x in args]
                if None in mpmath_args:
                    return
                result = call_mpmath(mpmath_function, tuple(mpmath_args))
                if isinstance(result, (mpmath.mpc, mpmath.mpf)):
                    result = from_mpmath(result, d)
        return result

    def call_mpmath(self, mpmath_function, mpmath_args):
        try:
            return mpmath_function(*mpmath_args)
        except ValueError as exc:
            text = str(exc)
            if text == "gamma function pole":
                return Symbol("ComplexInfinity")
            else:
                raise
        except ZeroDivisionError:
            return
        except SpecialValueError as exc:
            return Symbol(exc.name)


class _MPMathMultiFunction(_MPMathFunction):

    sympy_names = None
    mpmath_names = None

    def get_sympy_names(self):
        if self.sympy_names is None:
            return [self.sympy_name]
        return self.sympy_names.values()

    def get_function(self, module, names, fallback_name, leaves):
        try:
            name = fallback_name
            if names is not None:
                name = names[len(leaves)]
            if name is None:
                return None
            return getattr(module, name)
        except KeyError:
            return None

    def get_sympy_function(self, leaves):
        return self.get_function(sympy, self.sympy_names, self.sympy_name, leaves)

    def get_mpmath_function(self, leaves):
        return self.get_function(mpmath, self.mpmath_names, self.mpmath_name, leaves)


def create_infix(items, operator, prec, grouping):
    if len(items) == 1:
        return items[0]
    else:
        return Expression(
            "Infix",
            Expression("List", *items),
            String(operator),
            prec,
            Symbol(grouping),
        )


class DirectedInfinity(SympyFunction):
    """
    <dl>
    <dt>'DirectedInfinity[$z$]'</dt>
        <dd>represents an infinite multiple of the complex number $z$.
    <dt>'DirectedInfinity[]'</dt>
        <dd>is the same as 'ComplexInfinity'.</dd>
    </dl>

    >> DirectedInfinity[1]
     = Infinity
    >> DirectedInfinity[]
     = ComplexInfinity
    >> DirectedInfinity[1 + I]
     = (1 / 2 + I / 2) Sqrt[2] Infinity

    >> 1 / DirectedInfinity[1 + I]
     = 0
    >> DirectedInfinity[1] + DirectedInfinity[-1]
     : Indeterminate expression -Infinity + Infinity encountered.
     = Indeterminate

    >> DirectedInfinity[0]
     : Indeterminate expression 0 Infinity encountered.
     = Indeterminate

    #> DirectedInfinity[1+I]+DirectedInfinity[2+I]
     = (2 / 5 + I / 5) Sqrt[5] Infinity + (1 / 2 + I / 2) Sqrt[2] Infinity

    #> DirectedInfinity[Sqrt[3]]
     = Infinity
    """

    rules = {
        "DirectedInfinity[Indeterminate]": "Indeterminate",
        "DirectedInfinity[args___] ^ -1": "0",
        "0 * DirectedInfinity[args___]": "Message[Infinity::indet, Unevaluated[0 DirectedInfinity[args]]]; Indeterminate",
        "DirectedInfinity[a_?NumericQ] /; N[Abs[a]] != 1": "DirectedInfinity[a / Abs[a]]",
        "DirectedInfinity[a_] * DirectedInfinity[b_]": "DirectedInfinity[a*b]",
        "DirectedInfinity[] * DirectedInfinity[args___]": "DirectedInfinity[]",
        # Rules already implemented in Times.apply
        #        "z_?NumberQ * DirectedInfinity[]": "DirectedInfinity[]",
        #        "z_?NumberQ * DirectedInfinity[a_]": "DirectedInfinity[z * a]",
        "DirectedInfinity[a_] + DirectedInfinity[b_] /; b == -a": (
            "Message[Infinity::indet,"
            "  Unevaluated[DirectedInfinity[a] + DirectedInfinity[b]]];"
            "Indeterminate"
        ),
        "DirectedInfinity[] + DirectedInfinity[args___]": (
            "Message[Infinity::indet,"
            "  Unevaluated[DirectedInfinity[] + DirectedInfinity[args]]];"
            "Indeterminate"
        ),
        "DirectedInfinity[args___] + _?NumberQ": "DirectedInfinity[args]",
        "DirectedInfinity[0]": (
            "Message[Infinity::indet,"
            "  Unevaluated[DirectedInfinity[0]]];"
            "Indeterminate"
        ),
        "DirectedInfinity[0.]": (
            "Message[Infinity::indet,"
            "  Unevaluated[DirectedInfinity[0.]]];"
            "Indeterminate"
        ),
    }

    formats = {
        "DirectedInfinity[1]": "HoldForm[Infinity]",
        "DirectedInfinity[-1]": "HoldForm[-Infinity]",
        "DirectedInfinity[]": "HoldForm[ComplexInfinity]",
        "DirectedInfinity[DirectedInfinity[z_]]": "DirectedInfinity[z]",
        "DirectedInfinity[z_?NumericQ]": "HoldForm[z Infinity]",
    }

    def to_sympy(self, expr, **kwargs):
        if len(expr._leaves) == 1:
            dir = expr.leaves[0].get_int_value()
            if dir == 1:
                return sympy.oo
            elif dir == -1:
                return -sympy.oo
            else:
                return sympy.Mul((expr._leaves[0].to_sympy()), sympy.zoo)
        else:
            return sympy.zoo


class Re(SympyFunction):
    """
    <dl>
    <dt>'Re[$z$]'
        <dd>returns the real component of the complex number $z$.
    </dl>

    >> Re[3+4I]
     = 3

    >> Plot[{Cos[a], Re[E^(I a)]}, {a, 0, 2 Pi}]
     = -Graphics-

    #> Im[0.5 + 2.3 I]
     = 2.3
    #> % // Precision
     = MachinePrecision
    """

    attributes = ("Listable", "NumericFunction")
    sympy_name = "re"

    def apply_complex(self, number, evaluation):
        "Re[number_Complex]"

        return number.real

    def apply_number(self, number, evaluation):
        "Re[number_?NumberQ]"

        return number

    def apply(self, number, evaluation):
        "Re[number_]"

        return from_sympy(sympy.re(number.to_sympy().expand(complex=True)))


class Im(SympyFunction):
    """
    <dl>
    <dt>'Im[$z$]'
        <dd>returns the imaginary component of the complex number $z$.
    </dl>

    >> Im[3+4I]
     = 4

    >> Plot[{Sin[a], Im[E^(I a)]}, {a, 0, 2 Pi}]
     = -Graphics-

    #> Re[0.5 + 2.3 I]
     = 0.5
    #> % // Precision
     = MachinePrecision
    """

    attributes = ("Listable", "NumericFunction")

    def apply_complex(self, number, evaluation):
        "Im[number_Complex]"

        return number.imag

    def apply_number(self, number, evaluation):
        "Im[number_?NumberQ]"

        return Integer0

    def apply(self, number, evaluation):
        "Im[number_]"

        return from_sympy(sympy.im(number.to_sympy().expand(complex=True)))


class Conjugate(_MPMathFunction):
    """
    <dl>
    <dt>'Conjugate[$z$]'
        <dd>returns the complex conjugate of the complex number $z$.
    </dl>

    >> Conjugate[3 + 4 I]
     = 3 - 4 I

    >> Conjugate[3]
     = 3

    >> Conjugate[a + b * I]
     = Conjugate[a] - I Conjugate[b]

    >> Conjugate[{{1, 2 + I 4, a + I b}, {I}}]
     = {{1, 2 - 4 I, Conjugate[a] - I Conjugate[b]}, {-I}}

    ## Issue #272
    #> {Conjugate[Pi], Conjugate[E]}
     = {Pi, E}

    >> Conjugate[1.5 + 2.5 I]
     = 1.5 - 2.5 I
    """

    mpmath_name = "conj"


class Abs(_MPMathFunction):
    """
    <dl>
    <dt>'Abs[$x$]'
        <dd>returns the absolute value of $x$.
    </dl>
    >> Abs[-3]
     = 3

    'Abs' returns the magnitude of complex numbers:
    >> Abs[3 + I]
     = Sqrt[10]
    >> Abs[3.0 + I]
     = 3.16228
    >> Plot[Abs[x], {x, -4, 4}]
     = -Graphics-

    #> Abs[I]
     = 1
    #> Abs[a - b]
     = Abs[a - b]

    #> Abs[Sqrt[3]]
     = Sqrt[3]
    """

    sympy_name = "Abs"
    mpmath_name = "fabs"  # mpmath actually uses python abs(x) / x.__abs__()


class Arg(_MPMathFunction):
    """
     <dl>
       <dt>'Arg'[$z$, $method_option$]</dt>
       <dd>returns the argument of a complex value $z$.</dd>

       <ul>
         <li>'Arg'[$z$] is left unevaluated if $z$ is not a numeric quantity.
         <li>'Arg'[$z$] gives the phase angle of $z$ in radians.
         <li>The result from 'Arg'[$z$] is always between -Pi and +Pi.
         <li>'Arg'[$z$] has a branch cut discontinuity in the complex $z$ plane running from -Infinity to 0.
         <li>'Arg'[0] is 0.
      </ul>
     </dl>

     >> Arg[-3]
      = Pi

     Same as above using sympy's method:
     >> Arg[-3, Method->"sympy"]
      = Pi

    >> Arg[1-I]
     = -Pi / 4

    Arg evaluate the direction of DirectedInfinity quantities by
    the Arg of they arguments:
    >> Arg[DirectedInfinity[1+I]]
     = Pi / 4
    >> Arg[DirectedInfinity[]]
     = 1
    Arg for 0 is assumed to be 0:
    >> Arg[0]
     = 0
    """

    rules = {
        "Arg[0]": "0",
        "Arg[DirectedInfinity[]]": "1",
        "Arg[DirectedInfinity[a_]]": "Arg[a]",
    }

    attributes = ("Listable", "NumericFunction")
    options = {"Method": "Automatic"}

    numpy_name = "angle"  # for later
    mpmath_name = "arg"
    sympy_name = "arg"

    def apply(self, z, evaluation, options={}):
        "%(name)s[z_, OptionsPattern[%(name)s]]"
        if Expression("PossibleZeroQ", z).evaluate(evaluation) == SymbolTrue:
            return Integer0
        preference = self.get_option(options, "Method", evaluation).get_string_value()
        if preference is None or preference == "Automatic":
            return super(Arg, self).apply(z, evaluation)
        elif preference == "mpmath":
            return _MPMathFunction.apply(self, z, evaluation)
        elif preference == "sympy":
            return SympyFunction.apply(self, z, evaluation)
        # TODO: add NumpyFunction
        evaluation.message(
            "meth", f'Arg Method {preference} not in ("sympy", "mpmath")'
        )
        return


class Sign(SympyFunction):
    """
    <dl>
    <dt>'Sign[$x$]'
        <dd>return -1, 0, or 1 depending on whether $x$ is negative, zero, or positive.
    </dl>

    >> Sign[19]
     = 1
    >> Sign[-6]
     = -1
    >> Sign[0]
     = 0
    >> Sign[{-5, -10, 15, 20, 0}]
     = {-1, -1, 1, 1, 0}
    #> Sign[{1, 2.3, 4/5, {-6.7, 0}, {8/9, -10}}]
     = {1, 1, 1, {-1, 0}, {1, -1}}
    >> Sign[3 - 4*I]
     = 3 / 5 - 4 I / 5
    #> Sign[1 - 4*I] == (1/17 - 4 I/17) Sqrt[17]
     = True
    #> Sign[4, 5, 6]
     : Sign called with 3 arguments; 1 argument is expected.
     = Sign[4, 5, 6]
    #> Sign["20"]
     = Sign[20]
    """

    sympy_name = "sign"
    # mpmath_name = 'sign'

    attributes = ("Listable", "NumericFunction")

    messages = {
        "argx": "Sign called with `1` arguments; 1 argument is expected.",
    }

    def apply(self, x, evaluation):
        "%(name)s[x_]"
        # Sympy and mpmath do not give the desired form of complex number
        if isinstance(x, Complex):
            return Expression("Times", x, Expression("Power", Expression("Abs", x), -1))

        sympy_x = x.to_sympy()
        if sympy_x is None:
            return None
        return super().apply(x, evaluation)

    def apply_error(self, x, seqs, evaluation):
        "Sign[x_, seqs__]"
        return evaluation.message("Sign", "argx", Integer(len(seqs.get_sequence()) + 1))


class I(Predefined):
    """
    <dl>
    <dt>'I'
        <dd>represents the imaginary number 'Sqrt[-1]'.
    </dl>

    >> I^2
     = -1
    >> (3+I)*(3-I)
     = 10
    """

    python_equivalent = 1j

    def evaluate(self, evaluation):
        return Complex(Integer0, Integer1)


class NumberQ(Test):
    """
    <dl>
      <dt>'NumberQ[$expr$]'
      <dd>returns 'True' if $expr$ is an explicit number, and 'False' otherwise.
    </dl>

    >> NumberQ[3+I]
     = True
    >> NumberQ[5!]
     = True
    >> NumberQ[Pi]
     = False
    """

    def test(self, expr):
        return isinstance(expr, Number)


class PossibleZeroQ(SympyFunction):
    """
    <dl>
      <dt>'PossibleZeroQ[$expr$]'
      <dd>returns 'True' if basic symbolic and numerical methods suggest that expr has value zero, and 'False' otherwise.
    </dl>

    Test whether a numeric expression is zero:
    >> PossibleZeroQ[E^(I Pi/4) - (-1)^(1/4)]
     = True

    The determination is approximate.

    Test whether a symbolic expression is likely to be identically zero:
    >> PossibleZeroQ[(x + 1) (x - 1) - x^2 + 1]
     = True


    >> PossibleZeroQ[(E + Pi)^2 - E^2 - Pi^2 - 2 E Pi]
     = True

    Show that a numeric expression is nonzero:
    >> PossibleZeroQ[E^Pi - Pi^E]
     = False

    >> PossibleZeroQ[1/x + 1/y - (x + y)/(x y)]
     = True

    Decide that a numeric expression is zero, based on approximate computations:
    >> PossibleZeroQ[2^(2 I) - 2^(-2 I) - 2 I Sin[Log[4]]]
     = True

    >> PossibleZeroQ[Sqrt[x^2] - x]
     = False
    """

    attributes = ("Listable", "NumericFunction", "Protected")

    sympy_name = "_iszero"

    def apply(self, expr, evaluation):
        "%(name)s[expr_]"
        from sympy.matrices.utilities import _iszero

        sympy_expr = expr.to_sympy()
        result = _iszero(sympy_expr)
        if result is None:
            # try expanding the expression
            exprexp = Expression("ExpandAll", expr).evaluate(evaluation)
            exprexp = exprexp.to_sympy()
            result = _iszero(exprexp)
        if result is None:
            # Can't get exact answer, so try approximate equal
            numeric_val = Expression(SymbolN, expr).evaluate(evaluation)
            if numeric_val and hasattr(numeric_val, "is_approx_zero"):
                result = numeric_val.is_approx_zero
            elif not numeric_val.is_numeric(evaluation):
                return (
                    SymbolTrue
                    if Expression("Simplify", expr).evaluate(evaluation) == Integer0
                    else SymbolFalse
                )

        return from_python(result)


class RealNumberQ(Test):
    """
    <dl>
    <dt>'RealNumberQ[$expr$]'
        <dd>returns 'True' if $expr$ is an explicit number with no imaginary component.
    </dl>

    >> RealNumberQ[10]
     = True
    >> RealNumberQ[4.0]
     = True
    >> RealNumberQ[1+I]
     = False
    >> RealNumberQ[0 * I]
     = True
    >> RealNumberQ[0.0 * I]
     = True
    """

    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real))


class MachineNumberQ(Test):
    """
    <dl>
    <dt>'MachineNumberQ[$expr$]'
        <dd>returns 'True' if $expr$ is a machine-precision real or complex number.
    </dl>

     = True
    >> MachineNumberQ[3.14159265358979324]
     = False
    >> MachineNumberQ[1.5 + 2.3 I]
     = True
    >> MachineNumberQ[2.71828182845904524 + 3.14159265358979324 I]
     = False
    #> MachineNumberQ[1.5 + 3.14159265358979324 I]
     = True
    #> MachineNumberQ[1.5 + 5 I]
     = True
    """

    def test(self, expr):
        return expr.is_machine_precision()


class ExactNumberQ(Test):
    """
    <dl>
    <dt>'ExactNumberQ[$expr$]'
        <dd>returns 'True' if $expr$ is an exact number, and 'False' otherwise.
    </dl>

    >> ExactNumberQ[10]
     = True
    >> ExactNumberQ[4.0]
     = False
    >> ExactNumberQ[n]
     = False

    'ExactNumberQ' can be applied to complex numbers:
    >> ExactNumberQ[1 + I]
     = True
    >> ExactNumberQ[1 + 1. I]
     = False
    """

    def test(self, expr):
        return isinstance(expr, Number) and not expr.is_inexact()


class InexactNumberQ(Test):
    """
    <dl>
    <dt>'InexactNumberQ[$expr$]'
        <dd>returns 'True' if $expr$ is not an exact number, and 'False' otherwise.
    </dl>

    >> InexactNumberQ[a]
     = False
    >> InexactNumberQ[3.0]
     = True
    >> InexactNumberQ[2/3]
     = False

    'InexactNumberQ' can be applied to complex numbers:
    >> InexactNumberQ[4.0+I]
     = True
    """

    def test(self, expr):
        return isinstance(expr, Number) and expr.is_inexact()


class IntegerQ(Test):
    """
    <dl>
    <dt>'IntegerQ[$expr$]'
        <dd>returns 'True' if $expr$ is an integer, and 'False' otherwise.
    </dl>

    >> IntegerQ[3]
     = True
    >> IntegerQ[Pi]
     = False
    """

    def test(self, expr):
        return isinstance(expr, Integer)


class Integer_(Builtin):
    """
    <dl>
    <dt>'Integer'
        <dd>is the head of integers.
    </dl>

    >> Head[5]
     = Integer

    ## Test large Integer comparison bug
    #> {a, b} = {2^10000, 2^10000 + 1}; {a == b, a < b, a <= b}
     = {False, True, True}
    """

    name = "Integer"


class Real_(Builtin):
    """
    <dl>
    <dt>'Real'
        <dd>is the head of real (inexact) numbers.
    </dl>

    >> x = 3. ^ -20;
    >> InputForm[x]
     = 2.8679719907924413*^-10
    >> Head[x]
     = Real

    ## Formatting tests
    #> 1. * 10^6
     = 1.*^6
    #> 1. * 10^5
     = 100000.
    #> -1. * 10^6
     = -1.*^6
    #> -1. * 10^5
     = -100000.
    #> 1. * 10^-6
     = 1.*^-6
    #> 1. * 10^-5
     = 0.00001
    #> -1. * 10^-6
     = -1.*^-6
    #> -1. * 10^-5
     = -0.00001

    ## Mathematica treats zero strangely
    #> 0.0000000000000
     = 0.
    #> 0.0000000000000000000000000000
     = 0.*^-28

    ## Parse *^ Notation
    #> 1.5*^24
     = 1.5*^24
    #> 1.5*^+24
     = 1.5*^24
    #> 1.5*^-24
     = 1.5*^-24

    ## Don't accept *^ with spaces
    #> 1.5 *^10
     : "1.5 *" cannot be followed by "^10" (line 1 of "<test>").
    #> 1.5*^ 10
     : "1.5*" cannot be followed by "^ 10" (line 1 of "<test>").

    ## Issue654
    #> 1^^2
     : Requested base 1 in 1^^2 should be between 2 and 36.
     : Expression cannot begin with "1^^2" (line 1 of "<test>").
    #> 2^^0101
     = 5
    #> 2^^01210
     : Digit at position 3 in 01210 is too large to be used in base 2.
     : Expression cannot begin with "2^^01210" (line 1 of "<test>").
    #> 16^^5g
     : Digit at position 2 in 5g is too large to be used in base 16.
     : Expression cannot begin with "16^^5g" (line 1 of "<test>").
    #> 36^^0123456789abcDEFxyzXYZ
     = 14142263610074677021975869033659
    #> 37^^3
     : Requested base 37 in 37^^3 should be between 2 and 36.
     : Expression cannot begin with "37^^3" (line 1 of "<test>").
    """

    name = "Real"


class Rational_(Builtin):
    """
    <dl>
    <dt>'Rational'</dt>
        <dd>is the head of rational numbers.</dd>
    <dt>'Rational[$a$, $b$]'</dt>
        <dd>constructs the rational number $a$ / $b$.</dd>
    </dl>

    >> Head[1/2]
     = Rational

    >> Rational[1, 2]
     = 1 / 2

    #> -2/3
     = -2 / 3
    """

    name = "Rational"

    def apply(self, n, m, evaluation):
        "%(name)s[n_Integer, m_Integer]"

        if m.value == 1:
            return n
        else:
            return Rational(n.value, m.value)


class Complex_(Builtin):
    """
    <dl>
    <dt>'Complex'
        <dd>is the head of complex numbers.
    <dt>'Complex[$a$, $b$]'
        <dd>constructs the complex number '$a$ + I $b$'.
    </dl>

    >> Head[2 + 3*I]
     = Complex
    >> Complex[1, 2/3]
     = 1 + 2 I / 3
    >> Abs[Complex[3, 4]]
     = 5

    #> OutputForm[Complex[2.0 ^ 40, 3]]
     = 1.09951*^12 + 3. I
    #> InputForm[Complex[2.0 ^ 40, 3]]
     = 1.099511627776*^12 + 3.*I

    #> -2 / 3 - I
     = -2 / 3 - I

    #> Complex[10, 0]
     = 10

    #> 0. + I
     = 0. + 1. I

    #> 1 + 0 I
     = 1
    #> Head[%]
     = Integer

    #> Complex[0.0, 0.0]
     = 0. + 0. I
    #> 0. I
     = 0.
    #> 0. + 0. I
     = 0.

    #> 1. + 0. I
     = 1.
    #> 0. + 1. I
     = 0. + 1. I

    ## Check Nesting Complex
    #> Complex[1, Complex[0, 1]]
     = 0
    #> Complex[1, Complex[1, 0]]
     = 1 + I
    #> Complex[1, Complex[1, 1]]
     = I
    """

    name = "Complex"

    def apply(self, r, i, evaluation):
        "%(name)s[r_?NumberQ, i_?NumberQ]"

        if isinstance(r, Complex) or isinstance(i, Complex):
            sym_form = r.to_sympy() + sympy.I * i.to_sympy()
            r, i = sym_form.simplify().as_real_imag()
            r, i = from_sympy(r), from_sympy(i)
        return Complex(r, i)


class Factorial(PostfixOperator, _MPMathFunction):
    """
    <dl>
    <dt>'Factorial[$n$]'
    <dt>'$n$!'
        <dd>computes the factorial of $n$.
    </dl>

    >> 20!
     = 2432902008176640000

    'Factorial' handles numeric (real and complex) values using the gamma function:
    >> 10.5!
     = 1.18994*^7
    >> (-3.0+1.5*I)!
     = 0.0427943 - 0.00461565 I

    However, the value at poles is 'ComplexInfinity':
    >> (-1.)!
     = ComplexInfinity

    'Factorial' has the same operator ('!') as 'Not', but with higher precedence:
    >> !a! //FullForm
     = Not[Factorial[a]]

    #> 0!
     = 1
    """

    attributes = ("NumericFunction",)

    operator = "!"
    precedence = 610
    mpmath_name = "factorial"


class Factorial2(PostfixOperator, _MPMathFunction):
    """
    <dl>
      <dt>'Factorial2[$n$]'
      <dt>'$n$!!'
      <dd>computes the double factorial of $n$.
    </dl>
    The double factorial or semifactorial of a number $n$, is the product of all the integers from 1 up to n that have the same parity (odd or even) as $n$.

    >> 5!!
     = 15.

    >> Factorial2[-3]
     = -1.

    'Factorial2' accepts Integers, Rationals, Reals, or Complex Numbers:
    >> I!! + 1
     = 3.71713 + 0.279527 I

    Irrationals can be handled by using numeric approximation:
    >> N[Pi!!, 6]
     = 3.35237
    """

    attributes = ("NumericFunction",)
    operator = "!!"
    precedence = 610
    mpmath_name = "fac2"
    sympy_name = "factorial2"
    messages = {
        "ndf": "`1` evaluation error: `2`.",
        "unknownp": "'`1`' not in ('Automatic', 'sympy', 'mpmath')",
    }

    options = {"Method": "Automatic"}

    def apply(self, number, evaluation, options={}):
        "Factorial2[number_?NumberQ, OptionsPattern[%(name)s]]"

        try:
            import scipy.special as sp
            from numpy import pi

            # From https://stackoverflow.com/a/36779406/546218
            def fact2_generic(x):
                n = (x + 1.0) / 2.0
                return 2.0 ** n * sp.gamma(n + 0.5) / (pi ** (0.5))

        except ImportError:
            fact2_generic = None

        pref_expr = self.get_option(options, "Method", evaluation)
        is_automatic = False
        if pref_expr == Symbol("System`Automatic"):
            is_automatic = True
            preference = "mpmath"
        else:
            preference = pref_expr.get_string_value()

        if preference in ("mpmath", "Automatic"):
            number_arg = number.to_mpmath()
            convert_from_fn = from_mpmath
            fact2_fn = getattr(mpmath, self.mpmath_name)
        elif preference == "sympy":
            number_arg = number.to_sympy()
            convert_from_fn = from_sympy
            fact2_fn = getattr(sympy, self.sympy_name)
        else:
            return evaluation.message("Factorial2", "unknownp", preference)

        try:
            result = fact2_fn(number_arg)
        except:  # noqa
            number_arg = number.to_python()
            # Maybe an even negative number? Try generic routine
            if is_automatic and fact2_generic:
                return from_python(fact2_generic(number_arg))
            return evaluation.message(
                "Factorial2", "ndf", preference, str(sys.exc_info()[1])
            )
        return convert_from_fn(result)


class Sum(_IterationFunction, SympyFunction):
    """
    <dl>
      <dt>'Sum[$expr$, {$i$, $imin$, $imax$}]'
      <dd>evaluates the discrete sum of $expr$ with $i$ ranging from $imin$ to $imax$.

      <dt>'Sum[$expr$, {$i$, $imax$}]'
      <dd>same as 'Sum[$expr$, {$i$, 1, $imax$}]'.

      <dt>'Sum[$expr$, {$i$, $imin$, $imax$, $di$}]'
      <dd>$i$ ranges from $imin$ to $imax$ in steps of $di$.

      <dt>'Sum[$expr$, {$i$, $imin$, $imax$}, {$j$, $jmin$, $jmax$}, ...]'
      <dd>evaluates $expr$ as a multiple sum, with {$i$, ...}, {$j$, ...}, ... being in outermost-to-innermost order.
    </dl>

    A sum that Gauss in elementary school was asked to do to kill time:
    >> Sum[k, {k, 1, 10}]
     = 55

    The symbolic form he used:
    >> Sum[k, {k, 1, n}]
     = n (1 + n) / 2

    A Geometric series with a finite limit:
    >> Sum[1 / 2 ^ i, {i, 1, k}]
     = 1 - 2 ^ (-k)

    A Geometric series using Infinity:
    >> Sum[1 / 2 ^ i, {i, 1, Infinity}]
     = 1

    Leibniz forumla used in computing Pi:
    >> Sum[1 / ((-1)^k (2k + 1)), {k, 0, Infinity}]
     = Pi / 4

    A table of double sums to compute squares:
    >> Table[ Sum[i * j, {i, 0, n}, {j, 0, n}], {n, 0, 4} ]
     = {0, 1, 9, 36, 100}

    Computing Harmonic using a sum
    >> Sum[1 / k ^ 2, {k, 1, n}]
     = HarmonicNumber[n, 2]

    Other symbolic sums:
    >> Sum[k, {k, n, 2 n}]
     = 3 n (1 + n) / 2

    A sum with Complex-number iteration values
    >> Sum[k, {k, I, I + 1}]
     = 1 + 2 I

    >> Sum[f[i], {i, 1, 7}]
     = f[1] + f[2] + f[3] + f[4] + f[5] + f[6] + f[7]

    Verify algebraic identities:
    >> Sum[x ^ 2, {x, 1, y}] - y * (y + 1) * (2 * y + 1) / 6
     = 0

    ## >> (-1 + a^n) Sum[a^(k n), {k, 0, m-1}] // Simplify
    ## = -1 + (a ^ n) ^ m  # this is what I am getting
    ## = Piecewise[{{m (-1 + a ^ n), a ^ n == 1}, {-1 + (a ^ n) ^ m, True}}]

    #> a=Sum[x^k*Sum[y^l,{l,0,4}],{k,0,4}]]
     : "a=Sum[x^k*Sum[y^l,{l,0,4}],{k,0,4}]" cannot be followed by "]" (line 1 of "<test>").

    ## Issue #302
    ## The sum should not converge since the first term is 1/0.
    #> Sum[i / Log[i], {i, 1, Infinity}]
     = Sum[i / Log[i], {i, 1, Infinity}]
    #> Sum[Cos[Pi i], {i, 1, Infinity}]
     = Sum[Cos[Pi i], {i, 1, Infinity}]
    """

    # Do not throw warning message for symbolic iteration bounds
    throw_iterb = False

    sympy_name = "Sum"

    rules = _IterationFunction.rules.copy()
    rules.update(
        {
            "MakeBoxes[Sum[f_, {i_, a_, b_, 1}],"
            "  form:StandardForm|TraditionalForm]": (
                r'RowBox[{SubsuperscriptBox["\\[Sum]",'
                r'  RowBox[{MakeBoxes[i, form], "=", MakeBoxes[a, form]}],'
                r"  MakeBoxes[b, form]], MakeBoxes[f, form]}]"
            ),
        }
    )

    def get_result(self, items):
        return Expression("Plus", *items)

    def to_sympy(self, expr, **kwargs) -> SympyExpression:
        """
        Perform summation via sympy.summation
        """
        if expr.has_form("Sum", 2) and expr.leaves[1].has_form("List", 3):
            index = expr.leaves[1]
            arg_kwargs = kwargs.copy()
            arg_kwargs["convert_all_global_functions"] = True
            f_sympy = expr.leaves[0].to_sympy(**arg_kwargs)
            if f_sympy is None:
                return

            evaluation = kwargs.get("evaluation", None)

            # Handle summation parameters: variable, min, max
            var_min_max = index.leaves[:3]
            bounds = [expr.to_sympy(**kwargs) for expr in var_min_max]

            if evaluation:
                # Min and max might be Mathics expressions. If so, evaluate them.
                for i in (1, 2):
                    min_max_expr = var_min_max[i]
                    if not isinstance(expr, Symbol):
                        min_max_expr_eval = min_max_expr.evaluate(evaluation)
                        value = min_max_expr_eval.to_sympy(**kwargs)
                        bounds[i] = value

            # FIXME: The below tests on SympyExpression, but really the
            # test should be broader.
            if isinstance(f_sympy, sympy.core.basic.Basic):
                # sympy.summation() won't be able to handle Mathics functions in
                # in its first argument, the function paramameter.
                # For example in Sum[Identity[x], {x, 3}], sympy.summation can't
                # evaluate Indentity[x].
                # In general we want to avoid using Sympy if we can.
                # If we have integer bounds, we'll use Mathics's iterator Sum
                # (which is Plus)

                if all(hasattr(i, "is_integer") and i.is_integer for i in bounds[1:]):
                    # When we have integer bounds, it is better to not use Sympy but
                    # use Mathics evaluation. We turn:
                    # Sum[f[x], {<limits>}] into
                    #   MathicsSum[Table[f[x], {<limits>}]]
                    # where MathicsSum is self.get_result() our Iteration iterator.
                    values = Expression("Table", *expr.leaves).evaluate(evaluation)
                    ret = self.get_result(values.leaves).evaluate(evaluation)
                    # Make sure to convert the result back to sympy.
                    return ret.to_sympy()

            if None not in bounds:
                return sympy.summation(f_sympy, bounds)


class Product(_IterationFunction, SympyFunction):
    """
    <dl>
    <dt>'Product[$expr$, {$i$, $imin$, $imax$}]'
        <dd>evaluates the discrete product of $expr$ with $i$ ranging from $imin$ to $imax$.
    <dt>'Product[$expr$, {$i$, $imax$}]'
        <dd>same as 'Product[$expr$, {$i$, 1, $imax$}]'.
    <dt>'Product[$expr$, {$i$, $imin$, $imax$, $di$}]'
        <dd>$i$ ranges from $imin$ to $imax$ in steps of $di$.
    <dt>'Product[$expr$, {$i$, $imin$, $imax$}, {$j$, $jmin$, $jmax$}, ...]'
        <dd>evaluates $expr$ as a multiple product, with {$i$, ...}, {$j$, ...}, ... being in outermost-to-innermost order.
    </dl>

    >> Product[k, {k, 1, 10}]
     = 3628800
    >> 10!
     = 3628800
    >> Product[x^k, {k, 2, 20, 2}]
     = x ^ 110
    >> Product[2 ^ i, {i, 1, n}]
     = 2 ^ (n / 2 + n ^ 2 / 2)
    >> Product[f[i], {i, 1, 7}]
     = f[1] f[2] f[3] f[4] f[5] f[6] f[7]

    Symbolic products involving the factorial are evaluated:
    >> Product[k, {k, 3, n}]
     = n! / 2

    Evaluate the $n$th primorial:
    >> primorial[0] = 1;
    >> primorial[n_Integer] := Product[Prime[k], {k, 1, n}];
    >> primorial[12]
     = 7420738134810

    ## Used to be a bug in sympy, but now it is solved exactly!
    ## Again a bug in sympy - regressions between 0.7.3 and 0.7.6 (and 0.7.7?)
    ## #> Product[1 + 1 / i ^ 2, {i, Infinity}]
    ##  = 1 / ((-I)! I!)
    """

    throw_iterb = False

    sympy_name = "Product"

    rules = _IterationFunction.rules.copy()
    rules.update(
        {
            "MakeBoxes[Product[f_, {i_, a_, b_, 1}],"
            "  form:StandardForm|TraditionalForm]": (
                r'RowBox[{SubsuperscriptBox["\\[Product]",'
                r'  RowBox[{MakeBoxes[i, form], "=", MakeBoxes[a, form]}],'
                r"  MakeBoxes[b, form]], MakeBoxes[f, form]}]"
            ),
        }
    )

    def get_result(self, items):
        return Expression("Times", *items)

    def to_sympy(self, expr, **kwargs):
        if expr.has_form("Product", 2) and expr.leaves[1].has_form("List", 3):
            index = expr.leaves[1]
            try:
                e_kwargs = kwargs.copy()
                e_kwargs["convert_all_global_functions"] = True
                e = expr.leaves[0].to_sympy(**e_kwargs)
                i = index.leaves[0].to_sympy(**kwargs)
                start = index.leaves[1].to_sympy(**kwargs)
                stop = index.leaves[2].to_sympy(**kwargs)

                return sympy.product(e, (i, start, stop))
            except ZeroDivisionError:
                pass


class Piecewise(SympyFunction):
    """
    <dl>
    <dt>'Piecewise[{{expr1, cond1}, ...}]'
      <dd>represents a piecewise function.
    <dt>'Piecewise[{{expr1, cond1}, ...}, expr]'
      <dd>represents a piecewise function with default 'expr'.
    </dl>

    Heaviside function
    >> Piecewise[{{0, x <= 0}}, 1]
     = Piecewise[{{0, x <= 0}}, 1]

    ## D[%, x]
    ## Piecewise({{0, Or[x < 0, x > 0]}}, Indeterminate).

    >> Integrate[Piecewise[{{1, x <= 0}, {-1, x > 0}}], x]
     = Piecewise[{{x, x <= 0}}, -x]

    >> Integrate[Piecewise[{{1, x <= 0}, {-1, x > 0}}], {x, -1, 2}]
     = -1

    Piecewise defaults to 0 if no other case is matching.
    >> Piecewise[{{1, False}}]
     = 0

    >> Plot[Piecewise[{{Log[x], x > 0}, {x*-0.5, x < 0}}], {x, -1, 1}]
     = -Graphics-

    >> Piecewise[{{0 ^ 0, False}}, -1]
     = -1
    """

    sympy_name = "Piecewise"

    attributes = ("HoldAll",)

    def apply(self, items, evaluation):
        "%(name)s[items__]"
        result = self.to_sympy(
            Expression("Piecewise", *items.get_sequence()), evaluation=evaluation
        )
        if result is None:
            return
        if not isinstance(result, sympy.Piecewise):
            result = from_sympy(result)
            return result

    def to_sympy(self, expr, **kwargs):
        leaves = expr.leaves
        evaluation = kwargs.get("evaluation", None)
        if len(leaves) not in (1, 2):
            return

        sympy_cases = []
        for case in leaves[0].leaves:
            if case.get_head_name() != "System`List":
                return
            if len(case.leaves) != 2:
                return
            then, cond = case.leaves
            if evaluation:
                cond = evaluate_predicate(cond, evaluation)

            sympy_cond = None
            if isinstance(cond, Symbol):
                if cond == SymbolTrue:
                    sympy_cond = True
                elif cond == SymbolFalse:
                    sympy_cond = False
            if sympy_cond is None:
                sympy_cond = cond.to_sympy(**kwargs)
                if not (sympy_cond.is_Relational or sympy_cond.is_Boolean):
                    return

            sympy_cases.append((then.to_sympy(**kwargs), sympy_cond))

        if len(leaves) == 2:  # default case
            sympy_cases.append((leaves[1].to_sympy(**kwargs), True))
        else:
            sympy_cases.append((Integer0.to_sympy(**kwargs), True))

        return sympy.Piecewise(*sympy_cases)

    def from_sympy(self, sympy_name, args):
        # Hack to get around weird sympy.Piecewise 'otherwise' behaviour
        if str(args[-1].leaves[1]).startswith("System`_True__Dummy_"):
            args[-1].leaves[1] = SymbolTrue
        return Expression(self.get_name(), args)


class Boole(Builtin):
    """
    <dl>
    <dt>'Boole[expr]'
      <dd>returns 1 if expr is True and 0 if expr is False.
    </dl>

    >> Boole[2 == 2]
     = 1
    >> Boole[7 < 5]
     = 0
    >> Boole[a == 7]
     = Boole[a == 7]
    """

    attributes = ("Listable",)

    def apply(self, expr, evaluation):
        "%(name)s[expr_]"
        if isinstance(expr, Symbol):
            if expr == SymbolTrue:
                return Integer1
            elif expr == SymbolFalse:
                return Integer0
        return None


class Assumptions(Predefined):
    """
     <dl>
     <dt>'$Assumptions'
       <dd>is the default setting for the Assumptions option used in such
    functions as Simplify, Refine, and Integrate.
     </dl>
    """

    name = "$Assumptions"
    attributes = ("Unprotected",)
    rules = {
        "$Assumptions": "True",
    }


class Assuming(Builtin):
    """
    <dl>
    <dt>'Assuming[$cond$, $expr$]'
      <dd>Evaluates $expr$ assuming the conditions $cond$.
    </dl>
    >> $Assumptions = { x > 0 }
     = {x > 0}
    >> Assuming[y>0, ConditionalExpression[y x^2, y>0]//Simplify]
     = x ^ 2 y
    >> Assuming[Not[y>0], ConditionalExpression[y x^2, y>0]//Simplify]
     = Undefined
    >> ConditionalExpression[y x ^ 2, y > 0]//Simplify
     = ConditionalExpression[x ^ 2 y, y > 0]
    """

    attributes = ("HoldRest",)

    def apply_assuming(self, assumptions, expr, evaluation):
        "Assuming[assumptions_, expr_]"
        assumptions = assumptions.evaluate(evaluation)
        if assumptions.is_true():
            cond = []
        elif assumptions.is_symbol() or not assumptions.has_form("List", None):
            cond = [assumptions]
        else:
            cond = assumptions._leaves
        cond = tuple(cond) + get_assumptions_list(evaluation)
        list_cond = Expression("List", *cond)
        # TODO: reduce the list of predicates
        return dynamic_scoping(
            lambda ev: expr.evaluate(ev), {"System`$Assumptions": list_cond}, evaluation
        )


class ConditionalExpression(Builtin):
    """
    <dl>
      <dt>'ConditionalExpression[$expr$, $cond$]'
      <dd>returns $expr$ if $cond$ evaluates to $True$, $Undefined$ if $cond$ evaluates to $False$.
    </dl>

    >> ConditionalExpression[x^2, True]
     = x ^ 2

     >> ConditionalExpression[x^2, False]
     = Undefined

    >> f = ConditionalExpression[x^2, x>0]
     = ConditionalExpression[x ^ 2, x > 0]
    >> f /. x -> 2
     = 4
    >> f /. x -> -2
     = Undefined
    'ConditionalExpression' uses assumptions to evaluate the condition:
    >> $Assumptions = x > 0;
    >> ConditionalExpression[x ^ 2, x>0]//Simplify
     = x ^ 2
    >> $Assumptions = True;
    # >> ConditionalExpression[ConditionalExpression[s,x>a], x<b]
    # = ConditionalExpression[s, And[x>a, x<b]]
    """

    sympy_name = "Piecewise"

    rules = {
        "ConditionalExpression[expr_, True]": "expr",
        "ConditionalExpression[expr_, False]": "Undefined",
        "ConditionalExpression[ConditionalExpression[expr_, cond1_], cond2_]": "ConditionalExpression[expr, And@@Flatten[{cond1, cond2}]]",
        "ConditionalExpression[expr1_, cond_] + expr2_": "ConditionalExpression[expr1+expr2, cond]",
        "ConditionalExpression[expr1_, cond_]  expr2_": "ConditionalExpression[expr1 expr2, cond]",
        "ConditionalExpression[expr1_, cond_]^expr2_": "ConditionalExpression[expr1^expr2, cond]",
        "expr1_ ^ ConditionalExpression[expr2_, cond_]": "ConditionalExpression[expr1^expr2, cond]",
    }

    def apply_generic(self, expr, cond, evaluation):
        "ConditionalExpression[expr_, cond_]"
        # What we need here is a way to evaluate
        # cond as a predicate, using assumptions.
        # Let's delegate this to the And (and Or) symbols...
        if not cond.is_atom() and cond._head == SymbolList:
            cond = Expression("System`And", *(cond._leaves))
        else:
            cond = Expression("System`And", cond)
        if cond is None:
            return
        if cond.is_true():
            return expr
        if cond == SymbolFalse:
            return SymbolUndefined
        return

    def to_sympy(self, expr, **kwargs):
        leaves = expr.leaves
        if len(leaves) != 2:
            return
        expr, cond = leaves

        sympy_cond = None
        if isinstance(cond, Symbol):
            if cond == SymbolTrue:
                sympy_cond = True
            elif cond == SymbolFalse:
                sympy_cond = False
        if sympy_cond is None:
            sympy_cond = cond.to_sympy(**kwargs)
            if not (sympy_cond.is_Relational or sympy_cond.is_Boolean):
                return

        sympy_cases = (
            (expr.to_sympy(**kwargs), sympy_cond),
            (sympy.Symbol(sympy_symbol_prefix + "System`Undefined"), True),
        )
        return sympy.Piecewise(*sympy_cases)
