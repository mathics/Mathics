# -*- coding: utf8 -*-

"""
Arithmetic functions

Basic arithmetic functions, including complex number arithmetic.
"""

from __future__ import with_statement

import mpmath
import sympy

from mathics.builtin.base import Builtin, Predefined, BinaryOperator, PrefixOperator, PostfixOperator, Test, SympyFunction, SympyConstant
from mathics.core.expression import Expression, Number, Integer, Rational, Real, Symbol, Complex, String
from mathics.core.numbers import get_type, mul, add, sympy2mpmath, mpmath2sympy, SpecialValueError
from mathics.builtin.lists import _IterationFunction
from mathics.core.convert import from_sympy
from mathics.core.numbers import sympy2mpmath, mpmath2sympy, min_prec, prec

class _MPMathFunction(SympyFunction):

    attributes = ('Listable', 'NumericFunction')

    mpmath_name = None

    nargs = 1

    def eval(self, *args):
        if self.mpmath_name is None:
            return None
        
        mpmath_function = getattr(mpmath, self.mpmath_name)
        return mpmath_function(*args)

    def apply(self, z, evaluation):
        '%(name)s[z__]'

        args = z.get_sequence()

        if len(args) != self.nargs:
            return

        # if no arguments are inexact attempt to use sympy
        if len([True for x in args if Expression('InexactNumberQ', x).evaluate(evaluation).is_true()]) == 0:
            expr = Expression(self.get_name(), *args).to_sympy()
            result = from_sympy(expr)
            # evaluate leaves to convert e.g. Plus[2, I] -> Complex[2, 1]
            result = result.evaluate_leaves(evaluation)
        else:
            p = min_prec(*args)
            with mpmath.workprec(prec(p)):
                mpmath_args = [sympy2mpmath(z.to_sympy()) for z in args]
                if None in mpmath_args:
                    return
                try:
                    result = self.eval(*mpmath_args)
                    if mpmath.isinf(result):
                        result = mpmath.rect(1, mpmath.arg(result))
                        if result.imag == 0: # Avoid DirectedInfinity[1. + 0. I]
                            result = int(result.real)
                        result = Number.from_mp(result)
                        return Expression('DirectedInfinity', result)
                    elif mpmath.isnan(result):
                        return Symbol('Indeterminate')
                    result = Number.from_mp(result, p)
                except ValueError, exc:
                    text = str(exc)
                    if text == 'gamma function pole':
                        return Symbol('ComplexInfinity')
                    else:
                        raise
                except ZeroDivisionError:
                    return
                except SpecialValueError, exc:
                    return Symbol(exc.name)
        return result

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
     = 6.5 + 3. a + 3.5 b
     
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
    """
    
    operator = '+'
    precedence = 310
    attributes = ('Flat', 'Listable', 'NumericFunction', 'OneIdentity', 'Orderless', 'Protected')
    
    default_formats = False
    
    defaults = {
        None: '0',
    }
    
    sympy_name = 'Add'
    
    def format_plus(self, items, evaluation):
        'Plus[items__]'
        
        def negate(item):
            if item.has_form('Times', 1, None):
                if isinstance(item.leaves[0], (Integer, Rational, Real, Complex)):
                    neg = Number.from_mp(-item.leaves[0].to_sympy())
                    if neg.same(Integer(1)):
                        if len(item.leaves) == 1:
                            return neg
                        else:
                            return Expression ('Times', *item.leaves[1:])
                    else:
                        return Expression('Times', neg, *item.leaves[1:])
                else:
                    return Expression('Times', -1, *item.leaves)
            elif isinstance(item, (Integer, Rational, Real, Complex)):
                return Number.from_mp(-item.to_sympy())
            else:
                return Expression('Times', -1, item)
            
        def is_negative(value):
            if isinstance(value, (Integer, Rational, Real)) and value.to_sympy() < 0:
                return True
            if isinstance(value, Complex):
                real, imag = value.to_sympy().as_real_imag()
                if real <= 0 and imag <= 0:
                    return True
            return False
        
        items = items.get_sequence()
        values = [Expression('HoldForm', item) for item in items[:1]]
        ops = []
        for item in items[1:]:
            if (item.has_form('Times', 1, None) and is_negative(item.leaves[0])) or is_negative(item):
                item = negate(item)
                op = "-"
            else:
                op = "+"
            values.append(Expression('HoldForm', item))
            ops.append(String(op))
        return Expression('Infix', Expression('List', *values), Expression('List', *ops), 310, Symbol('Left'))
            
    def apply(self, items, evaluation):
        'Plus[items___]'
        
        items = items.numerify(evaluation).get_sequence()
        leaves = []
        last_item = last_count = None

        prec = min_prec(*items)
        is_real = all([not isinstance(i, Complex) for i in items])

        if prec is None:
            number = (sympy.Integer(0), sympy.Integer(0))
        else:
            number = (sympy.Float('0.0', prec), sympy.Float('0.0', prec))
        
        def append_last():
            if last_item is not None:
                if last_count == 1:
                    leaves.append(last_item)
                else:
                    if last_item.has_form('Times', None):
                        last_item.leaves.insert(0, Number.from_mp(last_count))
                        leaves.append(last_item)
                    else:
                        leaves.append(Expression('Times', Number.from_mp(last_count), last_item))
        
        for item in items:
            if isinstance(item, Number):
                #TODO: Optimise this for the case of adding many real numbers
                if isinstance(item, Complex):
                    sym_real, sym_imag = item.real.to_sympy(), item.imag.to_sympy()
                else:
                    sym_real, sym_imag = item.to_sympy(), sympy.Integer(0)

                if prec is not None:
                    sym_real = sym_real.n(prec)
                    sym_imag = sym_imag.n(prec)

                number = (number[0] + sym_real, number[1] + sym_imag)
            else:
                count = rest = None
                if item.has_form('Times', None):
                    for leaf in item.leaves:
                        if isinstance(leaf, Number):
                            count = leaf.to_sympy()
                            rest = item.leaves[:]
                            rest.remove(leaf)
                            if len(rest) == 1:
                                rest = rest[0]
                            else:
                                rest.sort()
                                rest = Expression('Times', *rest)
                            break
                if count is None:
                    count = sympy.Integer(1)
                    rest = item
                if last_item is not None and last_item == rest:
                    last_count = add(last_count, count)
                else:
                    append_last()
                    last_item = rest
                    last_count = count
        append_last()
        if prec is not None or number != (0, 0):
            if number[1].is_zero and is_real:
                leaves.insert(0, Number.from_mp(number[0], prec))
            elif number[1].is_zero and number[1].is_Integer and prec is None:
                leaves.insert(0, Number.from_mp(number[0], prec))
            else:
                leaves.insert(0, Complex(number[0], number[1], prec))
        if not leaves:
            return Integer(0)
        elif len(leaves) == 1:
            return leaves[0]
        else:
            leaves.sort()
            return Expression('Plus', *leaves)
        
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
    
    operator = '-'
    precedence_parse = 311
    precedence = 310
    attributes = ('Listable', 'NumericFunction')
    grouping = 'Left'
    
    rules = {
        'Subtract[x_, y_]': 'Plus[x, Times[-1, y]]',
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
    
    operator = '-'
    precedence = 480
    attributes = ('Listable', 'NumericFunction')
    
    rules = {
        'Minus[x_]': 'Times[-1, x]',
    }
    
    formats = {
        'Minus[x_]': 'Prefix[{HoldForm[x]}, "-", 480]',
        'Minus[expr_Divide]': 'Prefix[{HoldForm[expr]}, "-", 399]',  # don't put e.g. -2/3 in parentheses
        'Minus[Infix[expr_, op_, 400, grouping_]]': 'Prefix[{Infix[expr, op, 400, grouping]}, "-", 399]',
    }
    
    def apply_int(self, x, evaluation):
        'Minus[x_Integer]'
        
        return Integer(-x.to_sympy())
    
    def post_parse(self, expression):
        if expression.get_head().get_name() == 'Minus' and len(expression.leaves) == 1 and isinstance(expression.leaves[0], Number):
            return Number.from_mp(-expression.leaves[0].to_sympy())
        else:
            return super(Minus, self).post_parse(expression)
    
class PrePlus(PrefixOperator):
    """
    Hack to help the parser distinguish between binary and unary Plus.

    >> +a //FullForm
     = a
    
    #> +(x - 2/3 + y)
     = -2 / 3 + x + y

    #> +Infinity
     = Infinity
    """
    
    operator = '+'
    precedence = 480
    attributes = ('Listable', 'NumericFunction')
    
    def apply_int(self, x, evaluation):
        'PrePlus[x_]'
        return x
    
    def post_parse(self, expression):
        return expression.leaves[0]
    
def create_infix(items, operator, prec, grouping):
    if len(items) == 1:
        return items[0]
    else:
        return Expression('Infix', Expression('List', *items), String(operator), prec, Symbol(grouping))        

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

    #> Precision[1 * 2.8]
     = MachinePrecision
    #> Precision[1.4 * 2.8]
     = MachinePrecision
    #> Precision[1 * 8]
     = Infinity
    """
    
    operator = '*'
    operator_display = ' '
    precedence = 400
    attributes = ('Flat', 'Listable', 'NumericFunction', 'OneIdentity', 'Orderless', 'Protected')
    
    defaults = {
        None: '1',
    }
    
    default_formats = False
    
    sympy_name = 'Mul'
    
    rules = {
    }
    
    formats = {
    }
    
    
    def format_times(self, items, evaluation, op=u'\u2062'):
        'Times[items__]'
        
        def inverse(item):
            if item.has_form('Power', 2) and isinstance(item.leaves[1], (Integer, Rational, Real)):
                neg = Number.from_mp(-item.leaves[1].to_sympy())
                if neg.same(Integer(1)):
                    return item.leaves[0]
                else:
                    return Expression('Power', item.leaves[0], neg)
            else:
                return item
        
        items = items.get_sequence()
        positive = []
        negative = []
        for item in items:
            if item.has_form('Power', 2) and isinstance(item.leaves[1], (Integer, Rational, Real)) and item.leaves[1].to_sympy() < 0:
                negative.append(inverse(item))
            elif isinstance(item, Rational):
                numerator = item.numerator()
                if not numerator.same(Integer(1)):
                    positive.append(numerator)
                negative.append(item.denominator())
            else:
                positive.append(item)
        if positive and isinstance(positive[0], (Integer, Real)) and positive[0].to_sympy() < 0:
            positive[0] = Number.from_mp(-positive[0].to_sympy())
            if positive[0].same(Integer(1)):
                del positive[0]
            minus = True
        else:
            minus = False
        positive = [Expression('HoldForm', item) for item in positive]
        negative = [Expression('HoldForm', item) for item in negative]
        if positive:
            positive = create_infix(positive, op, 400, 'None')
        else:
            positive = Integer(1)
        if negative:
            negative = create_infix(negative, op, 400, 'None')
            result = Expression('Divide', Expression('HoldForm', positive), Expression('HoldForm', negative))
        else:
            result = positive
        if minus:
            result = Expression('Minus', result) #Expression('PrecedenceForm', result, 481))
        result = Expression('HoldForm', result)
        return result
    
    def format_inputform(self, items, evaluation):
        'InputForm: Times[items__]'
        return self.format_times(items, evaluation, op='*')

    def format_outputform(self, items, evaluation):
        'OutputForm: Times[items__]'
        return self.format_times(items, evaluation, op=' ')
    
    def apply(self, items, evaluation):
        'Times[items___]'

        #TODO: Clean this up and optimise it 

        items = items.numerify(evaluation).get_sequence()
        number = (sympy.Integer(1), sympy.Integer(0))
        leaves = []

        prec = min_prec(*items)
        is_real = all([not isinstance(i, Complex) for i in items])

        for item in items:
            if isinstance(item, Number):
                if isinstance(item, Complex):
                    sym_real, sym_imag = item.real.to_sympy(), item.imag.to_sympy()
                else:
                    sym_real, sym_imag = item.to_sympy(), sympy.Integer(0)

                if prec is not None:
                    sym_real = sym_real.n(prec)
                    sym_imag = sym_imag.n(prec)

                if sym_real.is_zero and sym_imag.is_zero and prec is None:
                    return Integer('0')
                number = (number[0]*sym_real - number[1]*sym_imag, number[0]*sym_imag + number[1]*sym_real)
            elif leaves and item == leaves[-1]:
                leaves[-1] = Expression('Power', leaves[-1], Integer(2))
            elif leaves and item.has_form('Power', 2) and leaves[-1].has_form('Power', 2) and item.leaves[0].same(leaves[-1].leaves[0]):
                leaves[-1].leaves[1] = Expression('Plus', item.leaves[1], leaves[-1].leaves[1])
            elif leaves and item.has_form('Power', 2) and item.leaves[0].same(leaves[-1]):
                leaves[-1] = Expression('Power', leaves[-1], Expression('Plus', item.leaves[1], Integer(1)))
            elif leaves and leaves[-1].has_form('Power', 2) and leaves[-1].leaves[0].same(item):
                leaves[-1] = Expression('Power', item, Expression('Plus', Integer(1), leaves[-1].leaves[1]))
            else:
                leaves.append(item)
        if number == (1, 0):
            number = None
        elif number == (-1, 0) and leaves and leaves[0].has_form('Plus', None):
            leaves[0].leaves = [Expression('Times', Integer(-1), leaf) for leaf in leaves[0].leaves]
            number = None

        if number is not None:
            if number[1].is_zero and is_real:
                leaves.insert(0, Number.from_mp(number[0], prec))
            elif number[1].is_zero and number[1].is_Integer and prec is None:
                leaves.insert(0, Number.from_mp(number[0], prec))
            else:
                leaves.insert(0, Complex(Number.from_mp(number[0], prec), Number.from_mp(number[1], prec), prec))
        if not leaves:
            return Integer(1)
        elif len(leaves) == 1:
            return leaves[0]
        else:
            return Expression('Times', *leaves)
        
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
    
    operator = '/'
    precedence = 470
    attributes = ('Listable', 'NumericFunction')
    grouping = 'Left'
    
    default_formats = False
    
    rules = {
        'Divide[x_, y_]': 'Times[x, Power[y, -1]]',
        
        'MakeBoxes[Divide[x_, y_], f:StandardForm|TraditionalForm]': 'FractionBox[MakeBoxes[x, f], MakeBoxes[y, f]]',
    }
    
    formats = {
        (('InputForm', 'OutputForm'), 'Divide[x_, y_]'): 'Infix[{HoldForm[x], HoldForm[y]}, "/", 400, Left]',
    }
    
    def post_parse(self, expression):
        if len(expression.leaves) == 2:
            if isinstance(expression.leaves[0], Integer) and \
                isinstance(expression.leaves[1], Integer) and expression.leaves[1].to_sympy() != 0:
                return Number.from_mp(Rational(expression.leaves[0].to_sympy(), expression.leaves[1].to_sympy()).to_sympy())
            else:
                if isinstance(expression.leaves[0], Integer) and expression.leaves[0].to_sympy() == 1:
                    return Expression('Power', expression.leaves[1].post_parse(), Integer(-1))
                else:
                    return Expression('Times', expression.leaves[0].post_parse(), Expression('Power', expression.leaves[1].post_parse(), Integer(-1)))
        else:
            return super(Divide, self).post_parse(expression)
        
class Power(BinaryOperator, SympyFunction):
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
    #> Precision[%]
     = MachinePrecision
    #> 4.0`20 ^ (1/3)
     = 1.58740105196819947475
     
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
     : Infinite expression (division by zero) encountered.
     = ComplexInfinity
    #> Sqrt[-3+2. I]
     = 0.550251 + 1.81735 I
    #> Sqrt[-3+2 I]
     = Sqrt[-3 + 2 I]
    #> (3/2+1/2I)^2
     = 2 + 3 I / 2
    #> I ^ I
     = I ^ I

    #> 2 ^ 2.0
     = 4.

    #> Pi ^ 4.
     = 97.4091
    """
    
    operator = '^'
    precedence = 590
    attributes = ('Listable', 'NumericFunction', 'OneIdentity')
    grouping = 'Right'
    
    default_formats = False
    
    sympy_name = 'Pow'
    
    messages = {
        'infy': "Infinite expression (division by zero) encountered.",
    }
    
    defaults = {
        2: '1',
    }
    
    formats = {
        'x_ ^ (1/2)': 'HoldForm[Sqrt[x]]',
        
        (('InputForm', 'OutputForm'), 'x_ ^ y_'): 'Infix[{HoldForm[x], HoldForm[y]}, "^", 590, Right]',
        ('', 'x_ ^ y_'): 'PrecedenceForm[Superscript[OuterPrecedenceForm[HoldForm[x], 590], HoldForm[y]], 590]',
        
        ('', 'x_ ^ y_?Negative'): 'HoldForm[Divide[1, #]]&[If[y==-1, HoldForm[x], HoldForm[x]^-y]]',
    }
    
    rules = {
    }
    
    def apply(self, items, evaluation):
        'Power[items__]'
        
        items_sequence = items.get_sequence()
        
        if len(items_sequence) == 2:
            x, y = items_sequence
        else:
            return Expression('Power', *items_sequence)

        if y.get_int_value() == 1:
            return x
        elif x.get_int_value() == 1:
            return x
        elif y.get_int_value() == 0:
            if x.get_int_value() == 0:
                evaluation.message('Power', 'indet', Expression('Power', x, y))
                return Symbol('Indeterminate')
            else:
                return Integer(1)
        
        elif x.has_form('Power', 2) and isinstance(y, Integer):
            return Expression('Power', x.leaves[0], Expression('Times', x.leaves[1], y))
        elif x.has_form('Times', None) and isinstance(y, Integer):
            return Expression('Times', *[Expression('Power', leaf, y) for leaf in x.leaves])
        
        elif isinstance(x, Number) and isinstance(y, Number) and not (x.is_inexact() or y.is_inexact()):
            sym_x, sym_y = x.to_sympy(), y.to_sympy()
            try:
                if sym_y >= 0:
                    result = sym_x ** sym_y
                else:
                    if sym_x == 0:
                        evaluation.message('Power', 'infy')
                        return Symbol('ComplexInfinity')
                    result = sympy.Integer(1) / (sym_x ** (-sym_y))
                if isinstance(result, sympy.Pow):
                    result = result.simplify()
                    args = [from_sympy(expr) for expr in result.as_base_exp()]
                    result = Expression('Power', *args)
                    result = result.evaluate_leaves(evaluation)
                    return result
                return from_sympy(result)
            except ValueError:
                return Expression('Power', x, y)
            except ZeroDivisionError:
                evaluation.message('Power', 'infy')
                return Symbol('ComplexInfinity')

        elif isinstance(x, Number) and isinstance(y, Number) and (x.is_inexact() or y.is_inexact()):
            try:
                p = min_prec(x, y)
                with mpmath.workprec(prec(p)):
                    mp_x = sympy2mpmath(x.to_sympy())
                    mp_y = sympy2mpmath(y.to_sympy())
                    result = mp_x ** mp_y
                    if isinstance(result, mpmath.mpf):
                        return Real(str(result), p)
                    elif isinstance(result, mpmath.mpc):
                        return Complex(str(result.real), str(result.imag), p)
            except ZeroDivisionError:
                evaluation.message('Power', 'infy')
                return Symbol('ComplexInfinity')
        else:
            numerified_items = items.numerify(evaluation)
            return Expression('Power', *numerified_items.get_sequence())


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
    """
    
    attributes = ('Listable', 'NumericFunction')
    
    rules = {
        'Sqrt[x_]': 'x ^ (1/2)',
        
        'MakeBoxes[Sqrt[x_], f:StandardForm|TraditionalForm]': 'SqrtBox[MakeBoxes[x, f]]',
    }
    
class Infinity(SympyConstant):
    """
    <dl>
    <dt>'Infinity'
        <dd>represents an infinite real quantity.
    </dl>

    >> 1 / Infinity
     = 0
    >> Infinity + 100
     = Infinity

    Use 'Infinity' in sum and limit calculations:
    >> Sum[1/x^2, {x, 1, Infinity}]
     = Pi ^ 2 / 6
     
    #> FullForm[Infinity]
     = DirectedInfinity[1]
    #> (2 + 3.5*I) / Infinity
     = 0. + 0. I
    #> Infinity + Infinity
     = Infinity
    #> Infinity / Infinity
     : Indeterminate expression 0 Infinity encountered.
     = Indeterminate
    """
    
    sympy_name = 'oo'
    
    rules = {
        'Infinity': 'DirectedInfinity[1]',
        
        'MakeBoxes[Infinity, f:StandardForm|TraditionalForm]': '"\\[Infinity]"',
    }
    
class ComplexInfinity(SympyConstant):
    """
    <dl>
    <dt>'ComplexInfinity'
        <dd>represents an infinite complex quantity of undetermined direction.
    </dl>

    >> 1 / ComplexInfinity
     = 0
    >> ComplexInfinity + ComplexInfinity
     = ComplexInfinity
    >> ComplexInfinity * Infinity
     = ComplexInfinity
    >> FullForm[ComplexInfinity]
     = DirectedInfinity[]
    """
    
    sympy_name = 'ComplexInfinity'
    
    rules = {
        'ComplexInfinity': 'DirectedInfinity[]',
    }
    
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
     = DirectedInfinity[(1 / 2 + I / 2) Sqrt[2]]
    >> 1 / DirectedInfinity[1 + I]
     = 0
    >> DirectedInfinity[1] + DirectedInfinity[-1]
     : Indeterminate expression -Infinity + Infinity encountered.
     = Indeterminate
     
    #> DirectedInfinity[1+I]+DirectedInfinity[2+I]
     = DirectedInfinity[(2 / 5 + I / 5) Sqrt[5]] + DirectedInfinity[(1 / 2 + I / 2) Sqrt[2]] 
    """
        
    rules = {
        '1 / DirectedInfinity[args___]': '0',
        '0 * DirectedInfinity[args___]': 'Message[Infinity::indet, Unevaluated[0 DirectedInfinity[args]]]; Indeterminate',
        'DirectedInfinity[a_?NumberQ] /; N[Abs[a]] != 1': 'DirectedInfinity[a / Abs[a]]',
        'DirectedInfinity[a_] * DirectedInfinity[b_]': 'DirectedInfinity[a*b]',
        'DirectedInfinity[] * DirectedInfinity[args___]': 'DirectedInfinity[]',
        'DirectedInfinity[0]': 'DirectedInfinity[]',
        'z_?NumberQ * DirectedInfinity[]': 'DirectedInfinity[]',
        'z_?NumberQ * DirectedInfinity[a_]': 'DirectedInfinity[z * a]',
        'DirectedInfinity[a_] + DirectedInfinity[b_] /; b == -a': 'Message[Infinity::indet, Unevaluated[DirectedInfinity[a] + DirectedInfinity[b]]]; Indeterminate',
        'DirectedInfinity[args___] + _?NumberQ': 'DirectedInfinity[args]',
    }
    
    formats = {
        'DirectedInfinity[1]': 'HoldForm[Infinity]',
        'DirectedInfinity[-1]': 'HoldForm[-Infinity]',
        'DirectedInfinity[]': 'HoldForm[ComplexInfinity]',
    }
    
    #TODO: Improve formatting

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
    """
    
    attributes = ('Listable', 'NumericFunction')
    
    def apply_complex(self, number, evaluation):
        'Re[number_Complex]'
        
        real, imag = number.to_sympy().as_real_imag()
        return Number.from_mp(real)
    
    def apply_number(self, number, evaluation):
        'Re[number_?NumberQ]'
        
        return number
                
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
    """
    
    attributes = ('Listable', 'NumericFunction')
    
    def apply_complex(self, number, evaluation):
        'Im[number_Complex]'
        
        real, imag = number.to_sympy().as_real_imag()
        return Number.from_mp(imag)
    
    def apply_number(self, number, evaluation):
        'Im[number_?NumberQ]'
        
        return Integer(0)
    
class Abs(SympyFunction):
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
    """
    
    sympy_name = 'Abs'
    
    def apply_real(self, x, evaluation):
        'Abs[x_?RealNumberQ]'
        
        sym_x = x.to_sympy()
        if sym_x < 0:
            return Number.from_mp(-sym_x)
        else:
            return x
        
    def apply_complex(self, z, evaluation):
        'Abs[z_Complex]'
        
        real, imag = z.to_sympy().as_real_imag()
        return Expression('Sqrt', Expression('Plus', Number.from_mp(real ** 2), Number.from_mp(imag ** 2)))
                
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
    
    def evaluate(self, evaluation):
        return Complex(sympy.Integer(0), sympy.Integer(1))
    
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
     = False
    """
    
    def test(self, expr):        
        return isinstance(expr, (Integer, Rational, Real))
    
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

    Specify a base
    >> 8^^23
     = 19

    >> 2^^101001
     = 41

    #> 16^^F4A308C
     = 256520332
    #> 36^^f4z5
     = 706289
    #> 19^^3Ai
     = 1291

    Give an exponent
    >> 10*^3
     = 10000

    #> 10*^-3
     = 1 / 100
    #> 10*^+3
     = 10000

    Both base and exponent
    >> 8^^23*^2
     = 1216

    #> 8^^23*^-2
     = 19 / 64
    #> 8^^23*^+2
     = 1216

    Precision is specified with `number
    >> Precision[195`45]
     = 45.

    #> 15`4
     = 15.00
    #> Precision[15`.4]
     = 0.4
    #> Precision[15`4.]
     = 4.
    #> 15``045.5
     = 15.000000000000000000000000000000000000000000000
    #> Precision[%]
     = 46.6761
    #> 8^^23`20
     = 19.0000000000000000
    #> 10`20*^3
     = 10000.000000000000000
    #> 10`20*^-3
     = 0.010000000000000000000
    #> 8^^23`20*^2
     = 1216.00000000000000
    #> 8^^23`20*^-2
     = 0.296875000000000000
    #> 10`+4
     = 10.00
    #> 10`-4
     : Requested precision -4. is smaller than $MinPrecision. Using $MinPrecision instead.
     = 0.*^1

    If number is not specified MachinePrecision is used
    >> Precision[195`]
     = MachinePrecision

    #> 8^^23`
     = 19.
    #> 10`*^3
     = 10000.
    #> 10`*^-3
     = 0.01
    #> 8^^23`*^2
     = 1216.
    #> 8^^23`*^-2
     = 0.296875

    Accuracy is specified with ``number
    >> Accuracy[195``40]
     = 40.

    #> Accuracy[15`.4]
     = -0.776091
    #> Accuracy[15`4.]
     = 2.82391
    #> Accuracy[15``045.5]
     = 45.5
    #> 8^^23``20
     = 19.000000000000000000
    #> 10``20*^3
     = 10000.0000000000000000000
    #> 10``20*^-3
     = 0.0100000000000000000
    #> 8^^23``20*^2
     = 1216.000000000000000000
    #> 8^^23``20*^-2
     = 0.296875000000000000
    #> 10``+4
     = 10.000

    ## Zero Edge case
    #> 0``14
     = 0.*^-14
    #> {Accuracy[%], Precision[%]}
     = {14., 0.}
    ## MMA is weirdly inconsistent 0` -> 0. but 0`x -> 0 *)
    #> 0`13
     = 0
    #> 0`
     = 0.
    #> {Accuracy[0.], Precision[0.]}
     = {307.653, MachinePrecision}
    #> {0`-4, 0`+4, 0`3.4, 0`+5.6, 0`-.1, 0`1.}
     = {0, 0, 0, 0, 0, 0}
    #> {0``-4, 0``+4, 0``3.4, 0``+5.6, 0``-.1, 0``1.}
     = {0``-4., 0``4., 0``3.4, 0``5.6, 0``-0.1, 0``1.}
    #> 25`0
     = 0.*^2
    #> 25``0
     = 2.*^1
    #> 25`0 == 25``0 == 25``-0 ==  25``+0. == 25``.0
     = True
    """

    name = 'Integer'
    
class Real_(Builtin):
    u"""
    <dl>
    <dt>'Real'
        <dd>is the head of real (inexact) numbers.
    </dl>
    
    >> x = 3. ^ -20;
    >> InputForm[x]
     = 2.86797199079244131*^-10
    >> Head[x]
     = Real

    Machine precision numbers
    >> 10.5
     = 10.5

    Arbitary precision numbers
    >> 10.95834905890234859042134832740172304897123
     = 10.9583490589023485904213483274017230489712
    >> Precision[%]
     = 42.0397

    ## By Copying ans pasting you slowly loose one digit of precision
    ## until you reach machine precision numbers.
    #> 1.5904213483274017230489712
     = 1.590421348327401723048971
    #> 1.590421348327401723048971
     = 1.59042134832740172304897
    #> 1.59042134832740172304897
     = 1.5904213483274017230490
    #> 1.5904213483274017230490
     = 1.590421348327401723049
    #> 1.590421348327401723049
     = 1.59042134832740172305
    #> 1.59042134832740172305
     = 1.5904213483274017231
    #> 1.5904213483274017231
     = 1.590421348327401723
    #> 1.590421348327401723
     = 1.59042134832740172
    #> 1.59042134832740172
     = 1.59042
    #> Precision[%]
     = MachinePrecision
    #> 1.59042
     = 1.59042

    Machine precision can be forced by appending the ` character
    >> 10.958349058902348590421348327401723048971230`
     = 10.9583
    >> Precision[%]
     = MachinePrecision

    Precision can be specified by appending `prec as follows
    >> 123.123`30
     = 123.123000000000000000000000000
    >> Precision[%]
     = 30.

    Accuracy is specified by appending `acc as follows
    >> 123.123``30
     = 123.12300000000000000000000000000
    >> Accuracy[%]
     = 30.

    Numbers can be entered using scientific notation
    >> 1.1234*^8
     = 1.1234*^8
    >> 1.1234*^5
     = 112340.
    >> 1.1234*^-5
     = 0.000011234
    ## Permit explicit exponent signs
    #> 1.5*^+24
     = 1.5*^24
    #> 1.5*^-24
     = 1.5*^-24
    ## Don't accept *^ with spaces
    #> 1.5 *^10
     : Parse error at or near token ^.
    #> 1.5*^ 10
     : Parse error at or near token ^.

    Numbers can also be specified in arbitary bases
    >> 16^^94A.D31E
     = 2378.82
    #> 8^^1.5
     = 1.625
    #> 8^^32432.423143242134231213
     = 13594.5374882383664860
    #> {Accuracy[%], Precision[%]}
     = {16.2556, 20.389}

    All the previous forms can be combined
    >> 1.1234`30*^8
     = 1.12340000000000000000000000000*^8
    >> 8^^1743.232`30
     = 995.300781250000000000000000
    #> {Accuracy[%], Precision[%]}
     = {24.0947, 27.0927}
    >> 16^^5AC3.94F`20*^8 // InputForm  (* Check me *)
     = 9.9796063879168`24.0823996531*^13
    #> 8^^1.73*^5
     = 62976.
    #> 8^^1.73*^-5
     = 0.000058651
    #> {Accuracy[%], Precision[%]}  (* Check me *)
     = {22.2317, MachinePrecision}
    #> 8^^1.73``20*^-5
     = 0.000058650970458984
    #> {Accuracy[%], Precision[%]}
     = {18.0618, 13.8301}
    #> 8^^1.73`20*^-5
     = 0.0000586509704589843750
    #> {Accuracy[%], Precision[%]}
     = {22.2935, 18.0618}

    ## Formatting tests
    #> 1.*^6
     = 1.*^6
    #> 1.*^5
     = 100000.
    #> -1.*^6
     = -1.*^6
    #> -1.*^5
     = -100000.
    #> 1.*^-6
     = 1.*^-6
    #> 1.*^-5
     = 0.00001
    #> -1.*^-6
     = -1.*^-6
    #> -1.*^-5
     = -0.00001

    ## Mathematica treats zero strangely
    #> 0.0000000000000
     = 0.
    #> 0.0000000000000000000000000000
     = 0.*^-28
    #> 0.``14
     = 0.*^-14
    #> {Accuracy[%], Precision[%]}
     = {14., 0.}
    #> 0.`13    (* Check me !! *)
     = 0.
    #> 0.`
     = 0.
    """
    
    name = 'Real'
    
class Rational_(Builtin):
    """
    <dl>
    <dt>'Rational'</dt>
        <dd>is the head of rational numbers.</dd>
    <dt>'Rational[$a$, $b$]'</dt>
        <dd>constructs the rational number a / b.</dd>
    </dl>

    >> Head[1/2]
     = Rational

    >> Rational[1, 2]
     = 1 / 2
     
    #> -2/3
     = -2 / 3
    """
    
    name = 'Rational'
    
    def apply(self, n, m, evaluation):
        'Rational[n_Integer, m_Integer]'
        
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

    #> Precision[Complex[1, 4.5]]
     = MachinePrecision
    #> Precision[Complex[1.``30, 4.4]]
     = MachinePrecision
    #> Precision[Complex[1., 4.4``30]]
     = MachinePrecision
    #> Precision[Complex[1.``30, 1.``30]]
     = 30.

    #> 0. + I
     = 0. + 1. I

    #> 1 + 0 I
     = 1
    #> Head[%]
     = Integer
    
    #> Complex[0.0, 0.0]
     = 0. + 0. I
    #> 0. I
     = 0. + 0. I
    #> 0. + 0. I
     = 0. + 0. I

    #> 1. + 0. I
     = 1. + 0. I
    #> 0. + 1. I
     = 0. + 1. I

    ## Check Nesting Complex
    #> Complex[1, Complex[0, 1]]
     = 0
    #> Complex[1, Complex[1, 0]]
     = 1 + I
    #> Complex[1, Complex[1, 1]]
     = I

    #> 0.5 E + 0.5 Pi I
     = 1.35914 + 1.5708 I
    """
    
    name = 'Complex'
    
    def apply(self, r, i, evaluation):
        'Complex[r_?NumberQ, i_?NumberQ]'

        if isinstance(r, Complex) or isinstance(i, Complex):
            sym_form = r.to_sympy() + sympy.I * i.to_sympy()
            sym_r, sym_i = sym_form.simplify().as_real_imag()
        else:
            sym_r, sym_i = r.to_sympy(), i.to_sympy()

        if isinstance(sym_i, sympy.Integer) and sym_i == 0:
            return Number.from_mp(sym_r)
        else:
            return Complex(sym_r, sym_i)
        
class Factorial(PostfixOperator, _MPMathFunction):
    """
    <dl>
    <dt>'Factorial[$n$]'</dt>
    <dt>'$n$!'</dt>
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
    
    operator = '!'
    precedence = 610
    
    def apply_int(self, n, evaluation):
        'Factorial[n_Integer]'
        
        n = n.to_sympy()
        if n < 0:
            return Symbol('ComplexInfinity')
        else:
            return Integer(sympy.factorial(n))
        
    def eval(self, z):
        return mpmath.factorial(z)
    
class Gamma(SympyFunction):
    #TODO implement the incomplete Gamma functions
    """
    <dl>
    <dt>'Gamma[$z$]'
        <dd>is the Gamma function on the complex number $z$.
    </dl>

    >> Gamma[8]
     = 5040
    >> Gamma[1. + I]
     = 0.498016 - 0.15495 I

    Both 'Gamma' and 'Factorial' functions are continuous:
    >> Plot[{Gamma[x], x!}, {x, 0, 4}]
     = -Graphics-
    """

    rules = {
        'Gamma[x_]': '(x - 1)!',
    }
    
class Pochhammer(SympyFunction):
    """
    <dl>
    <dt>'Pochhammer[$a$, $n$]'
        <dd>is the Pochhammer symbol (a)_n.
    </dl>

    >> Pochhammer[4, 8]
     = 6652800
    """


    sympy_name = 'RisingFactorial'
    
    rules = {
        'Pochhammer[a_, n_]': 'Gamma[a + n] / Gamma[a]',
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
        'HarmonicNumber[-1]': 'ComplexInfinity',
    }

    sympy_name = 'harmonic'
    mpmath_name = 'harmonic'
    
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
     
    Verify algebraic identities:
    >> Sum[x ^ 2, {x, 1, y}] - y * (y + 1) * (2 * y + 1) / 6
     = 0

    >> (-1 + a^n) Sum[a^(k n), {k, 0, m-1}] // Simplify
     = -1 + a ^ (m n)
     
    Infinite sums:
    >> Sum[1 / 2 ^ i, {i, 1, Infinity}]
     = 1
    >> Sum[1 / k ^ 2, {k, 1, Infinity}]
     = Pi ^ 2 / 6
     
    #> a=Sum[x^k*Sum[y^l,{l,0,4}],{k,0,4}]]
     : Parse error at or near token ].
    """
    
    throw_iterb = False # do not throw warning message for symbolic iteration bounds
    
    sympy_name = 'Sum'
    
    rules = _IterationFunction.rules.copy()
    rules.update({
        'MakeBoxes[Sum[f_, {i_, a_, b_, 1}], form:StandardForm|TraditionalForm]':
            r'RowBox[{SubsuperscriptBox["\[Sum]", RowBox[{MakeBoxes[i, form], "=", MakeBoxes[a, form]}], MakeBoxes[b, form]], MakeBoxes[f, form]}]',
    })
    
    def get_result(self, items):
        return Expression('Plus', *items)
    
    def to_sympy(self, expr, **kwargs):
        if expr.has_form('Sum', 2) and expr.leaves[1].has_form('List', 3):
            index = expr.leaves[1]
            result = sympy.summation(expr.leaves[0].to_sympy(), (
                index.leaves[0].to_sympy(), index.leaves[1].to_sympy(),
                index.leaves[2].to_sympy()))            
            return result
    
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
    
    Symbolic products involving the factorial are evaluated:
    >> Product[k, {k, 3, n}]
     = n! / 2

    Evaluate the $n$th primorial:
    >> primorial[0] = 1;
    >> primorial[n_Integer] := Product[Prime[k], {k, 1, n}];
    >> primorial[12]
     = 7420738134810

    ## Used to be a bug in sympy, but now it is solved exactly!
    #> Product[1 + 1 / i ^ 2, {i, Infinity}]
     = 1 / ((-I)! I!)
    """
    
    throw_iterb = False
    
    sympy_name = 'Product'
    
    rules = _IterationFunction.rules.copy()
    rules.update({
        'MakeBoxes[Product[f_, {i_, a_, b_, 1}], form:StandardForm|TraditionalForm]':
            r'RowBox[{SubsuperscriptBox["\[Product]", RowBox[{MakeBoxes[i, form], "=", MakeBoxes[a, form]}], MakeBoxes[b, form]], MakeBoxes[f, form]}]',
    })
    
    def get_result(self, items):
        return Expression('Times', *items)
    
    def to_sympy(self, expr, **kwargs):
        if expr.has_form('Product', 2) and expr.leaves[1].has_form('List', 3):
            index = expr.leaves[1]
            try:
                return sympy.product(expr.leaves[0].to_sympy(), (
                    index.leaves[0].to_sympy(), index.leaves[1].to_sympy(),
                    index.leaves[2].to_sympy()))
            except ZeroDivisionError:
                pass
