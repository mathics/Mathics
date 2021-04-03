# -*- coding: utf-8 -*-
# cython: language_level=3

"""
Mathematical Functions

Basic arithmetic functions, including complex number arithmetic.
"""

from mathics.version import __version__  # noqa used in loading to check consistency.
import sympy
import mpmath
from functools import lru_cache

from mathics.builtin.base import (
    Builtin,
    Predefined,
    BinaryOperator,
    PrefixOperator,
    PostfixOperator,
    Test,
    SympyFunction,
)

from mathics.core.expression import (
    Complex,
    Expression,
    Integer,
    Number,
    Rational,
    Real,
    String,
    Symbol,
    SymbolComplexInfinity,
    SymbolDirectedInfinity,
    SymbolFalse,
    SymbolInfinity,
    SymbolN,
    SymbolNull,
    SymbolSequence,
    SymbolTrue,
    SymbolSequence,
    from_python,
    from_mpmath,
    from_python,
)
from mathics.core.numbers import min_prec, dps, SpecialValueError

from mathics.builtin.lists import _IterationFunction
from mathics.core.convert import from_sympy, SympyExpression


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

    attributes = ("Listable", "NumericFunction")

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
                    result = Expression("DirectedInfinity", Integer(1))
                elif mpmath.isinf(result) and result < 0:
                    result = Expression("DirectedInfinity", Integer(-1))
                elif mpmath.isnan(result):
                    result = Symbol("Indeterminate")
                else:
                    result = from_mpmath(result)
        else:
            prec = min_prec(*args)
            d = dps(prec)
            args = [
                Expression(SymbolN, arg, Integer(d)).evaluate(evaluation)
                for arg in args
            ]
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
            return getattr(module, name)
        except KeyError:
            return None

    def get_sympy_function(self, leaves):
        return self.get_function(sympy, self.sympy_names, self.sympy_name, leaves)

    def get_mpmath_function(self, leaves):
        return self.get_function(mpmath, self.mpmath_names, self.mpmath_name, leaves)


class Plus(BinaryOperator, SympyFunction):
    """
    <dl>
    <dt>'Plus[$a$, $b$, ...]'</dt>
    <dt>$a$ + $b$ + ...</dt>
        <dd>represents the sum of the terms $a$, $b$, ...
    </dl>

    >> 1 + 2
     = 3

    'Plus' performs basic simplification of terms:

    >> a + b + a
     = 2 a + b
    >> a + a + 3 * a
     = 5 a
    >> a + b + 4.5 + a + b + a + 2 + 1.5 b
     = 6.5 + 3 a + 3.5 b

    Apply 'Plus' on a list to sum up its elements:
    >> Plus @@ {2, 4, 6}
     = 12
    The sum of the first 1000 integers:
    >> Plus @@ Range[1000]
     = 500500

    'Plus' has default value 0:
    >> DefaultValues[Plus]
     = {HoldPattern[Default[Plus]] :> 0}
    >> a /. n_. + x_ :> {n, x}
     = {0, a}

    The sum of 2 red circles and 3 red circles is...
    >> 2 Graphics[{Red,Disk[]}] + 3 Graphics[{Red,Disk[]}]
     = 5 -Graphics-

    #> -2a - 2b
     = -2 a - 2 b
    #> -4+2x+2*Sqrt[3]
     = -4 + 2 Sqrt[3] + 2 x
    #> 2a-3b-c
     = 2 a - 3 b - c
    #> 2a+5d-3b-2c-e
     = 2 a - 3 b - 2 c + 5 d - e

    #> 1 - I * Sqrt[3]
     = 1 - I Sqrt[3]

    #> Head[3 + 2 I]
     = Complex

    #> N[Pi, 30] + N[E, 30]
     = 5.85987448204883847382293085463
    #> % // Precision
     = 30.
    """

    operator = "+"
    precedence = 310
    attributes = (
        "Flat",
        "Listable",
        "NumericFunction",
        "OneIdentity",
        "Orderless",
        "Protected",
    )

    default_formats = False

    defaults = {
        None: "0",
    }

    sympy_name = "Add"

    def format_plus(self, items, evaluation):
        "Plus[items__]"

        def negate(item):
            if item.has_form("Times", 1, None):
                if isinstance(item.leaves[0], Number):
                    neg = -item.leaves[0]
                    if neg.same(Integer(1)):
                        if len(item.leaves) == 1:
                            return neg
                        else:
                            return Expression("Times", *item.leaves[1:])
                    else:
                        return Expression("Times", neg, *item.leaves[1:])
                else:
                    return Expression("Times", -1, *item.leaves)
            elif isinstance(item, Number):
                return -item.to_sympy()
            else:
                return Expression("Times", -1, item)

        def is_negative(value):
            if isinstance(value, Complex):
                real, imag = value.to_sympy().as_real_imag()
                if real <= 0 and imag <= 0:
                    return True
            elif isinstance(value, Number) and value.to_sympy() < 0:
                return True
            return False

        items = items.get_sequence()
        values = [Expression("HoldForm", item) for item in items[:1]]
        ops = []
        for item in items[1:]:
            if (
                item.has_form("Times", 1, None) and is_negative(item.leaves[0])
            ) or is_negative(item):
                item = negate(item)
                op = "-"
            else:
                op = "+"
            values.append(Expression("HoldForm", item))
            ops.append(String(op))
        return Expression(
            "Infix",
            Expression("List", *values),
            Expression("List", *ops),
            310,
            Symbol("Left"),
        )

    def apply(self, items, evaluation):
        "Plus[items___]"

        items = items.numerify(evaluation).get_sequence()
        leaves = []
        last_item = last_count = None

        prec = min_prec(*items)
        is_machine_precision = any(item.is_machine_precision() for item in items)
        numbers = []

        def append_last():
            if last_item is not None:
                if last_count == 1:
                    leaves.append(last_item)
                else:
                    if last_item.has_form("Times", None):
                        leaves.append(
                            Expression(
                                "Times", from_sympy(last_count), *last_item.leaves
                            )
                        )
                    else:
                        leaves.append(
                            Expression("Times", from_sympy(last_count), last_item)
                        )

        for item in items:
            if isinstance(item, Number):
                numbers.append(item)
            else:
                count = rest = None
                if item.has_form("Times", None):
                    for leaf in item.leaves:
                        if isinstance(leaf, Number):
                            count = leaf.to_sympy()
                            rest = item.get_mutable_leaves()
                            rest.remove(leaf)
                            if len(rest) == 1:
                                rest = rest[0]
                            else:
                                rest.sort()
                                rest = Expression("Times", *rest)
                            break
                if count is None:
                    count = sympy.Integer(1)
                    rest = item
                if last_item is not None and last_item == rest:
                    last_count = last_count + count
                else:
                    append_last()
                    last_item = rest
                    last_count = count
        append_last()

        if numbers:
            if prec is not None:
                if is_machine_precision:
                    numbers = [item.to_mpmath() for item in numbers]
                    number = mpmath.fsum(numbers)
                    number = from_mpmath(number)
                else:
                    with mpmath.workprec(prec):
                        numbers = [item.to_mpmath() for item in numbers]
                        number = mpmath.fsum(numbers)
                        number = from_mpmath(number, dps(prec))
            else:
                number = from_sympy(sum(item.to_sympy() for item in numbers))
        else:
            number = Integer(0)

        if not number.same(Integer(0)):
            leaves.insert(0, number)

        if not leaves:
            return Integer(0)
        elif len(leaves) == 1:
            return leaves[0]
        else:
            leaves.sort()
            return Expression("Plus", *leaves)


class Subtract(BinaryOperator):
    """
    <dl>
    <dt>'Subtract[$a$, $b$]'</dt>
    <dt>$a$ - $b$</dt>
        <dd>represents the subtraction of $b$ from $a$.</dd>
    </dl>

    >> 5 - 3
     = 2
    >> a - b // FullForm
     = Plus[a, Times[-1, b]]
    >> a - b - c
     = a - b - c
    >> a - (b - c)
     = a - b + c
    """

    operator = "-"
    precedence_parse = 311
    precedence = 310
    attributes = ("Listable", "NumericFunction")
    grouping = "Left"

    rules = {
        "Subtract[x_, y_]": "Plus[x, Times[-1, y]]",
    }


class Minus(PrefixOperator):
    """
    <dl>
    <dt>'Minus[$expr$]'
        <dd> is the negation of $expr$.
    </dl>

    >> -a //FullForm
     = Times[-1, a]

    'Minus' automatically distributes:
    >> -(x - 2/3)
     = 2 / 3 - x

    'Minus' threads over lists:
    >> -Range[10]
    = {-1, -2, -3, -4, -5, -6, -7, -8, -9, -10}
    """

    operator = "-"
    precedence = 480
    attributes = ("Listable", "NumericFunction")

    rules = {
        "Minus[x_]": "Times[-1, x]",
    }

    formats = {
        "Minus[x_]": 'Prefix[{HoldForm[x]}, "-", 480]',
        # don't put e.g. -2/3 in parentheses
        "Minus[expr_Divide]": 'Prefix[{HoldForm[expr]}, "-", 399]',
        "Minus[Infix[expr_, op_, 400, grouping_]]": (
            'Prefix[{Infix[expr, op, 400, grouping]}, "-", 399]'
        ),
    }

    def apply_int(self, x, evaluation):
        "Minus[x_Integer]"

        return Integer(-x.to_sympy())


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


class Times(BinaryOperator, SympyFunction):
    """
    <dl>
    <dt>'Times[$a$, $b$, ...]'</dt>
    <dt>'$a$ * $b$ * ...'</dt>
    <dt>'$a$ $b$ ...'</dt>
        <dd>represents the product of the terms $a$, $b$, ...
    </dl>
    >> 10 * 2
     = 20
    >> 10 2
     = 20
    >> a * a
     = a ^ 2
    >> x ^ 10 * x ^ -2
     = x ^ 8
    >> {1, 2, 3} * 4
     = {4, 8, 12}
    >> Times @@ {1, 2, 3, 4}
     = 24
    >> IntegerLength[Times@@Range[5000]]
     = 16326

    'Times' has default value 1:
    >> DefaultValues[Times]
     = {HoldPattern[Default[Times]] :> 1}
    >> a /. n_. * x_ :> {n, x}
     = {1, a}

    #> -a*b // FullForm
     = Times[-1, a, b]
    #> -(x - 2/3)
     = 2 / 3 - x
    #> -x*2
     = -2 x
    #> -(h/2) // FullForm
     = Times[Rational[-1, 2], h]

    #> x / x
     = 1
    #> 2x^2 / x^2
     = 2

    #> 3. Pi
     = 9.42478

    #> Head[3 * I]
     = Complex

    #> Head[Times[I, 1/2]]
     = Complex

    #> Head[Pi * I]
     = Times

    #> 3 * a //InputForm
     = 3*a
    #> 3 * a //OutputForm
     = 3 a

    #> -2.123456789 x
     = -2.12346 x
    #> -2.123456789 I
     = 0. - 2.12346 I

    #> N[Pi, 30] * I
     = 3.14159265358979323846264338328 I
    #> N[I Pi, 30]
     = 3.14159265358979323846264338328 I

    #> N[Pi * E, 30]
     = 8.53973422267356706546355086955
    #> N[Pi, 30] * N[E, 30]
     = 8.53973422267356706546355086955
    #> N[Pi, 30] * E
     = 8.53973422267356706546355086955
    #> % // Precision
     = 30.
    """

    operator = "*"
    operator_display = " "
    precedence = 400
    attributes = (
        "Flat",
        "Listable",
        "NumericFunction",
        "OneIdentity",
        "Orderless",
        "Protected",
    )

    defaults = {
        None: "1",
    }

    default_formats = False

    sympy_name = "Mul"

    rules = {}

    formats = {}

    def format_times(self, items, evaluation, op="\u2062"):
        "Times[items__]"

        def inverse(item):
            if item.has_form("Power", 2) and isinstance(  # noqa
                item.leaves[1], (Integer, Rational, Real)
            ):
                neg = -item.leaves[1]
                if neg.same(Integer(1)):
                    return item.leaves[0]
                else:
                    return Expression("Power", item.leaves[0], neg)
            else:
                return item

        items = items.get_sequence()
        positive = []
        negative = []
        for item in items:
            if (
                item.has_form("Power", 2)
                and isinstance(item.leaves[1], (Integer, Rational, Real))
                and item.leaves[1].to_sympy() < 0
            ):  # nopep8

                negative.append(inverse(item))
            elif isinstance(item, Rational):
                numerator = item.numerator()
                if not numerator.same(Integer(1)):
                    positive.append(numerator)
                negative.append(item.denominator())
            else:
                positive.append(item)
        if positive and positive[0].get_int_value() == -1:
            del positive[0]
            minus = True
        else:
            minus = False
        positive = [Expression("HoldForm", item) for item in positive]
        negative = [Expression("HoldForm", item) for item in negative]
        if positive:
            positive = create_infix(positive, op, 400, "None")
        else:
            positive = Integer(1)
        if negative:
            negative = create_infix(negative, op, 400, "None")
            result = Expression(
                "Divide",
                Expression("HoldForm", positive),
                Expression("HoldForm", negative),
            )
        else:
            result = positive
        if minus:
            result = Expression(
                "Minus", result
            )  # Expression('PrecedenceForm', result, 481))
        result = Expression("HoldForm", result)
        return result

    def format_inputform(self, items, evaluation):
        "InputForm: Times[items__]"
        return self.format_times(items, evaluation, op="*")

    def format_standardform(self, items, evaluation):
        "StandardForm: Times[items__]"
        return self.format_times(items, evaluation, op=" ")

    def format_outputform(self, items, evaluation):
        "OutputForm: Times[items__]"
        return self.format_times(items, evaluation, op=" ")

    def apply(self, items, evaluation):
        "Times[items___]"
        items = items.numerify(evaluation).get_sequence()
        leaves = []
        numbers = []
        infinity_factor = False

        prec = min_prec(*items)
        is_machine_precision = any(item.is_machine_precision() for item in items)

        # find numbers and simplify Times -> Power
        for item in items:
            if isinstance(item, Number):
                numbers.append(item)
            elif leaves and item == leaves[-1]:
                leaves[-1] = Expression("Power", leaves[-1], Integer(2))
            elif (
                leaves
                and item.has_form("Power", 2)
                and leaves[-1].has_form("Power", 2)
                and item.leaves[0].same(leaves[-1].leaves[0])
            ):
                leaves[-1] = Expression(
                    "Power",
                    leaves[-1].leaves[0],
                    Expression("Plus", item.leaves[1], leaves[-1].leaves[1]),
                )
            elif (
                leaves and item.has_form("Power", 2) and item.leaves[0].same(leaves[-1])
            ):
                leaves[-1] = Expression(
                    "Power", leaves[-1], Expression("Plus", item.leaves[1], Integer(1))
                )
            elif (
                leaves
                and leaves[-1].has_form("Power", 2)
                and leaves[-1].leaves[0].same(item)
            ):
                leaves[-1] = Expression(
                    "Power", item, Expression("Plus", Integer(1), leaves[-1].leaves[1])
                )
            elif item.get_head().same(SymbolDirectedInfinity):
                infinity_factor = True
                if len(item.leaves) > 1:
                    direction = item.leaves[0]
                    if isinstance(direction, Number):
                        numbers.append(direction)
                    else:
                        leaves.append(direction)
            elif item.same(SymbolInfinity) or item.same(SymbolComplexInfinity):
                infinity_factor = True
            else:
                leaves.append(item)

        if numbers:
            if prec is not None:
                if is_machine_precision:
                    numbers = [item.to_mpmath() for item in numbers]
                    number = mpmath.fprod(numbers)
                    number = from_mpmath(number)
                else:
                    with mpmath.workprec(prec):
                        numbers = [item.to_mpmath() for item in numbers]
                        number = mpmath.fprod(numbers)
                        number = from_mpmath(number, dps(prec))
            else:
                number = sympy.Mul(*[item.to_sympy() for item in numbers])
                number = from_sympy(number)
        else:
            number = Integer(1)

        if number.same(Integer(1)):
            number = None
        elif number.is_zero:
            if infinity_factor:
                return Symbol("Indeterminate")
            return number
        elif number.same(Integer(-1)) and leaves and leaves[0].has_form("Plus", None):
            leaves[0] = Expression(
                leaves[0].get_head(),
                *[Expression("Times", Integer(-1), leaf) for leaf in leaves[0].leaves]
            )
            number = None

        for leaf in leaves:
            leaf.clear_cache()

        if number is not None:
            leaves.insert(0, number)

        if not leaves:
            if infinity_factor:
                return SymbolComplexInfinity
            return Integer(1)

        if len(leaves) == 1:
            ret = leaves[0]
        else:
            ret = Expression("Times", *leaves)
        if infinity_factor:
            return Expression(SymbolDirectedInfinity, ret)
        else:
            return ret


class Divide(BinaryOperator):
    """
    <dl>
    <dt>'Divide[$a$, $b$]'</dt>
    <dt>'$a$ / $b$'</dt>
        <dd>represents the division of $a$ by $b$.
    </dl>
    >> 30 / 5
     = 6
    >> 1 / 8
     = 1 / 8
    >> Pi / 4
     = Pi / 4

    Use 'N' or a decimal point to force numeric evaluation:
    >> Pi / 4.0
     = 0.785398
    >> 1 / 8
     = 1 / 8
    >> N[%]
     = 0.125

    Nested divisions:
    >> a / b / c
     = a / (b c)
    >> a / (b / c)
     = a c / b
    >> a / b / (c / (d / e))
     = a d / (b c e)
    >> a / (b ^ 2 * c ^ 3 / e)
     = a e / (b ^ 2 c ^ 3)

    #> 1 / 4.0
     = 0.25
    #> 10 / 3 // FullForm
     = Rational[10, 3]
    #> a / b // FullForm
     = Times[a, Power[b, -1]]

    """

    operator = "/"
    precedence = 470
    attributes = ("Listable", "NumericFunction")
    grouping = "Left"

    default_formats = False

    rules = {
        "Divide[x_, y_]": "Times[x, Power[y, -1]]",
        "MakeBoxes[Divide[x_, y_], f:StandardForm|TraditionalForm]": (
            "FractionBox[MakeBoxes[x, f], MakeBoxes[y, f]]"
        ),
    }

    formats = {
        (("InputForm", "OutputForm"), "Divide[x_, y_]"): (
            'Infix[{HoldForm[x], HoldForm[y]}, "/", 400, Left]'
        ),
    }


class Power(BinaryOperator, _MPMathFunction):
    """
    <dl>
    <dt>'Power[$a$, $b$]'</dt>
    <dt>'$a$ ^ $b$'</dt>
        <dd>represents $a$ raised to the power of $b$.
    </dl>

    >> 4 ^ (1/2)
     = 2
    >> 4 ^ (1/3)
     = 2 ^ (2 / 3)
    >> 3^123
     = 48519278097689642681155855396759336072749841943521979872827

    >> (y ^ 2) ^ (1/2)
     = Sqrt[y ^ 2]
    >> (y ^ 2) ^ 3
     = y ^ 6

    >> Plot[Evaluate[Table[x^y, {y, 1, 5}]], {x, -1.5, 1.5}, AspectRatio -> 1]
     = -Graphics-

    Use a decimal point to force numeric evaluation:
    >> 4.0 ^ (1/3)
     = 1.5874

    'Power' has default value 1 for its second argument:
    >> DefaultValues[Power]
     = {HoldPattern[Default[Power, 2]] :> 1}
    >> a /. x_ ^ n_. :> {x, n}
     = {a, 1}

    'Power' can be used with complex numbers:
    >> (1.5 + 1.0 I) ^ 3.5
     = -3.68294 + 6.95139 I
    >> (1.5 + 1.0 I) ^ (3.5 + 1.5 I)
     = -3.19182 + 0.645659 I

    #> 1/0
     : Infinite expression 1 / 0 encountered.
     = ComplexInfinity
    #> 0 ^ -2
     : Infinite expression 1 / 0 ^ 2 encountered.
     = ComplexInfinity
    #> 0 ^ (-1/2)
     : Infinite expression 1 / Sqrt[0] encountered.
     = ComplexInfinity
    #> 0 ^ -Pi
     : Infinite expression 1 / 0 ^ 3.14159 encountered.
     = ComplexInfinity
    #> 0 ^ (2 I E)
     : Indeterminate expression 0 ^ (0. + 5.43656 I) encountered.
     = Indeterminate
    #> 0 ^ - (Pi + 2 E I)
     : Infinite expression 0 ^ (-3.14159 - 5.43656 I) encountered.
     = ComplexInfinity

    #> 0 ^ 0
     : Indeterminate expression 0 ^ 0 encountered.
     = Indeterminate

    #> Sqrt[-3+2. I]
     = 0.550251 + 1.81735 I
    #> Sqrt[-3+2 I]
     = Sqrt[-3 + 2 I]
    #> (3/2+1/2I)^2
     = 2 + 3 I / 2
    #> I ^ I
     = -1 ^ (I / 2)

    #> 2 ^ 2.0
     = 4.

    #> Pi ^ 4.
     = 97.4091

    #> a ^ b
     = a ^ b
    """

    operator = "^"
    precedence = 590
    attributes = ("Listable", "NumericFunction", "OneIdentity")
    grouping = "Right"

    default_formats = False

    sympy_name = "Pow"
    mpmath_name = "power"
    nargs = 2

    messages = {
        "infy": "Infinite expression `1` encountered.",
        "indet": "Indeterminate expression `1` encountered.",
    }

    defaults = {
        2: "1",
    }

    formats = {
        Expression(
            "Power",
            Expression("Pattern", Symbol("x"), Expression("Blank")),
            Rational(1, 2),
        ): "HoldForm[Sqrt[x]]",
        (("InputForm", "OutputForm"), "x_ ^ y_"): (
            'Infix[{HoldForm[x], HoldForm[y]}, "^", 590, Right]'
        ),
        ("", "x_ ^ y_"): (
            "PrecedenceForm[Superscript[OuterPrecedenceForm[HoldForm[x], 590],"
            "  HoldForm[y]], 590]"
        ),
        ("", "x_ ^ y_?Negative"): (
            "HoldForm[Divide[1, #]]&[If[y==-1, HoldForm[x], HoldForm[x]^-y]]"
        ),
        ("", "x_?Negative ^ y_"): (
            'Infix[{HoldForm[(x)], HoldForm[y]},"^", 590, Right]'
        ),
    }

    rules = {
        "Power[]": "1",
        "Power[x_]": "x",
    }

    def apply_check(self, x, y, evaluation):
        "Power[x_, y_]"

        # Power uses _MPMathFunction but does some error checking first
        if isinstance(x, Number) and x.is_zero:
            if isinstance(y, Number):
                y_err = y
            else:
                y_err = Expression(SymbolN, y).evaluate(evaluation)
            if isinstance(y_err, Number):
                py_y = y_err.round_to_float(permit_complex=True).real
                if py_y > 0:
                    return x
                elif py_y == 0.0:
                    evaluation.message("Power", "indet", Expression("Power", x, y_err))
                    return Symbol("Indeterminate")
                elif py_y < 0:
                    evaluation.message("Power", "infy", Expression("Power", x, y_err))
                    return Symbol("ComplexInfinity")
        if isinstance(x, Complex) and x.real.is_zero:
            yhalf = Expression("Times", y, Rational(1, 2))
            factor = self.apply(Expression("Sequence", x.imag, y), evaluation)
            return Expression("Times", factor, Expression("Power", Integer(-1), yhalf))

        result = self.apply(Expression(SymbolSequence, x, y), evaluation)
        if result is None or result != SymbolNull:
            return result


class Sqrt(SympyFunction):
    """
    <dl>
    <dt>'Sqrt[$expr$]'
        <dd>returns the square root of $expr$.
    </dl>

    >> Sqrt[4]
     = 2
    >> Sqrt[5]
     = Sqrt[5]
    >> Sqrt[5] // N
     = 2.23607
    >> Sqrt[a]^2
     = a

    Complex numbers:
    >> Sqrt[-4]
     = 2 I
    >> I == Sqrt[-1]
     = True

    >> Plot[Sqrt[a^2], {a, -2, 2}]
     = -Graphics-

    #> N[Sqrt[2], 50]
     = 1.4142135623730950488016887242096980785696718753769
    """

    attributes = ("Listable", "NumericFunction")

    rules = {
        "Sqrt[x_]": "x ^ (1/2)",
        "MakeBoxes[Sqrt[x_], f:StandardForm|TraditionalForm]": (
            "SqrtBox[MakeBoxes[x, f]]"
        ),
    }


class CubeRoot(Builtin):
    """
    <dl>
    <dt>'CubeRoot[$n$]'
        <dd>finds the real-valued cube root of the given $n$.
    </dl>

    >> CubeRoot[16]
     = 2 2 ^ (1 / 3)

    #> CubeRoot[-5]
     = -5 ^ (1 / 3)

    #> CubeRoot[-510000]
     = -10 510 ^ (1 / 3)

    #> CubeRoot[-5.1]
     = -1.7213

    #> CubeRoot[b]
     = b ^ (1 / 3)

    #> CubeRoot[-0.5]
     = -0.793701

    #> CubeRoot[3 + 4 I]
     : The parameter 3 + 4 I should be real valued.
     = (3 + 4 I) ^ (1 / 3)
    """

    attributes = {"Listable", "NumericFunction", "ReadProtected"}

    messages = {
        "preal": "The parameter `1` should be real valued.",
    }

    rules = {
        "CubeRoot[n_?NumericQ]": "If[n > 0, Power[n, Divide[1, 3]], Times[-1, Power[Times[-1, n], Divide[1, 3]]]]",
        "CubeRoot[n_]": "Power[n, Divide[1, 3]]",
        "MakeBoxes[CubeRoot[x_], f:StandardForm|TraditionalForm]": (
            "RadicalBox[MakeBoxes[x, f], 3]"
        ),
    }

    def apply(self, n, evaluation):
        "CubeRoot[n_Complex]"

        evaluation.message("CubeRoot", "preal", n)
        return Expression("Power", n, Expression("Divide", 1, 3))


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
        if len(expr.leaves) == 1:
            dir = expr.leaves[0].get_int_value()
            if dir == 1:
                return sympy.oo
            elif dir == -1:
                return -sympy.oo


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

        return Integer(0)

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
        return super().apply(x)

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
        return Complex(Integer(0), Integer(1))


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
            elif (
                Expression("NumericQ", numeric_val).evaluate(evaluation) == SymbolFalse
            ):
                return (
                    SymbolTrue
                    if Expression("Simplify", expr).evaluate(evaluation) == Integer(0)
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

        if m.to_sympy() == 1:
            return Integer(n.to_sympy())
        else:
            return Rational(n.to_sympy(), m.to_sympy())


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

    operator = "!"
    precedence = 610
    mpmath_name = "factorial"


class Gamma(_MPMathMultiFunction):
    """
    <dl>
    <dt>'Gamma[$z$]'
        <dd>is the gamma function on the complex number $z$.
    <dt>'Gamma[$z$, $x$]'
        <dd>is the upper incomplete gamma function.
    <dt>'Gamma[$z$, $x0$, $x1$]'
        <dd>is equivalent to 'Gamma[$z$, $x0$] - Gamma[$z$, $x1$]'.
    </dl>

    'Gamma[$z$]' is equivalent to '($z$ - 1)!':
    >> Simplify[Gamma[z] - (z - 1)!]
     = 0

    Exact arguments:
    >> Gamma[8]
     = 5040
    >> Gamma[1/2]
     = Sqrt[Pi]
    >> Gamma[1, x]
     = E ^ (-x)
    >> Gamma[0, x]
     = ExpIntegralE[1, x]

    Numeric arguments:
    >> Gamma[123.78]
     = 4.21078*^204
    >> Gamma[1. + I]
     = 0.498016 - 0.15495 I

    Both 'Gamma' and 'Factorial' functions are continuous:
    >> Plot[{Gamma[x], x!}, {x, 0, 4}]
     = -Graphics-

    ## Issue 203
    #> N[Gamma[24/10], 100]
     = 1.242169344504305404913070252268300492431517240992022966055507541481863694148882652446155342679460339
    #> N[N[Gamma[24/10],100]/N[Gamma[14/10],100],100]
     = 1.400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    #> % // Precision
     = 100.

    #> Gamma[1.*^20]
     : Overflow occurred in computation.
     = Overflow[]

    ## Needs mpmath support for lowergamma
    #> Gamma[1., 2.]
     = Gamma[1., 2.]
    """

    mpmath_names = {
        1: "gamma",
    }
    sympy_names = {
        1: "gamma",
        2: "uppergamma",
    }

    rules = {
        "Gamma[z_, x0_, x1_]": "Gamma[z, x0] - Gamma[z, x1]",
        "Gamma[1 + z_]": "z!",
    }

    def get_sympy_names(self):
        return ["gamma", "uppergamma", "lowergamma"]

    def from_sympy(self, sympy_name, leaves):
        if sympy_name == "lowergamma":
            # lowergamma(z, x) -> Gamma[z, 0, x]
            z, x = leaves
            return Expression(self.get_name(), z, Integer(0), x)
        else:
            return Expression(self.get_name(), *leaves)


class Pochhammer(SympyFunction):
    """
    <dl>
    <dt>'Pochhammer[$a$, $n$]'
        <dd>is the Pochhammer symbol (a)_n.
    </dl>

    >> Pochhammer[4, 8]
     = 6652800
    """

    sympy_name = "RisingFactorial"

    rules = {
        "Pochhammer[a_, n_]": "Gamma[a + n] / Gamma[a]",
    }


class HarmonicNumber(_MPMathFunction):
    """
    <dl>
    <dt>'HarmonicNumber[n]'
      <dd>returns the $n$th harmonic number.
    </dl>

    >> Table[HarmonicNumber[n], {n, 8}]
     = {1, 3 / 2, 11 / 6, 25 / 12, 137 / 60, 49 / 20, 363 / 140, 761 / 280}

    >> HarmonicNumber[3.8]
     = 2.03806

    #> HarmonicNumber[-1.5]
     = 0.613706
    """

    rules = {
        "HarmonicNumber[-1]": "ComplexInfinity",
    }

    sympy_name = "harmonic"
    mpmath_name = "harmonic"


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
    >> Sum[k, {k, 1, 10}]
     = 55

    Double sum:
    >> Sum[i * j, {i, 1, 10}, {j, 1, 10}]
     = 3025

    Symbolic sums are evaluated:
    >> Sum[k, {k, 1, n}]
     = n (1 + n) / 2
    >> Sum[k, {k, n, 2 n}]
     = 3 n (1 + n) / 2
    >> Sum[k, {k, I, I + 1}]
     = 1 + 2 I
    >> Sum[1 / k ^ 2, {k, 1, n}]
     = HarmonicNumber[n, 2]
    >> Sum[f[i], {i, 1, 7}]
     = f[1] + f[2] + f[3] + f[4] + f[5] + f[6] + f[7]

    Verify algebraic identities:
    >> Sum[x ^ 2, {x, 1, y}] - y * (y + 1) * (2 * y + 1) / 6
     = 0

    ## >> (-1 + a^n) Sum[a^(k n), {k, 0, m-1}] // Simplify
    ## = -1 + (a ^ n) ^ m  # this is what I am getting
    ## = Piecewise[{{m (-1 + a ^ n), a ^ n == 1}, {-1 + (a ^ n) ^ m, True}}]

    Infinite sums:
    >> Sum[1 / 2 ^ i, {i, 1, Infinity}]
     = 1
    >> Sum[1 / k ^ 2, {k, 1, Infinity}]
     = Pi ^ 2 / 6

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
     = Piecewise[{{x, x <= 0}, {-x, True}}]

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
        result = self.to_sympy(Expression("Piecewise", *items.get_sequence()))
        if result is None:
            return
        if not isinstance(result, sympy.Piecewise):
            return from_sympy(result)

    def to_sympy(self, expr, **kwargs):
        leaves = expr.leaves

        if len(leaves) not in (1, 2):
            return

        sympy_cases = []
        for case in leaves[0].leaves:
            if case.get_head_name() != "System`List":
                return
            if len(case.leaves) != 2:
                return
            then, cond = case.leaves

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
            sympy_cases.append((Integer(0).to_sympy(**kwargs), True))

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
                return Integer(1)
            elif expr == SymbolFalse:
                return Integer(0)
        return None
