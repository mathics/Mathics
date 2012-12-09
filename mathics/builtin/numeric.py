# -*- coding: utf8 -*-

"""
Numeric evaluation

Support for numeric evaluation with arbitrary precision is just a proof-of-concept.
Precision is not "guarded" through the evaluation process. Only integer precision is supported.
However, things like 'N[Pi, 100]' should work as expected.
"""

import mpmath
import sympy

from mathics.builtin.base import Builtin, Predefined
from mathics.core.numbers import dps, mpmath2sympy, prec, int2base
from mathics.core import numbers
from mathics.core.expression import Integer, Rational, Real, Complex, Atom, Expression, Number, Symbol
from mathics.core.convert import from_sympy
from mathics.settings import MACHINE_PRECISION

machine_precision = MACHINE_PRECISION

def get_precision(precision, evaluation):
    if precision.get_name() == 'MachinePrecision':
        return machine_precision
    elif isinstance(precision, (Integer, Rational, Real)):
        return prec(float(precision.to_sympy()))
    else:
        evaluation.message('N', 'precbd', precision)
        return None

class N(Builtin):
    """
    <dl>
    <dt>'N[$expr$, $prec$]'
        <dd>evaluates $expr$ numerically with a precision of $prec$ digits.
    </dl>
    >> N[Pi, 50]
     = 3.1415926535897932384626433832795028841971693993751

    >> N[1/7]
     = 0.142857142857142857

    >> N[1/7, 5]
     = 0.14286
    
    You can manually assign numerical values to symbols.
    When you do not specify a precision, 'MachinePrecision' is taken.
    >> N[a] = 10.9
     = 10.9
    >> a
     = a
     
    'N' automatically threads over expressions, except when a symbol has attributes 'NHoldAll', 'NHoldFirst', or 'NHoldRest'.
    >> N[a + b]
     = 10.9 + b
    >> N[a, 20]
     = a
    >> N[a, 20] = 11;
    >> N[a + b, 20]
     = 11. + b
    >> N[f[a, b]]
     = f[10.9, b]
    >> SetAttributes[f, NHoldAll]
    >> N[f[a, b]]
     = f[a, b]
    
    The precision can be a pattern:
    >> N[c, p_?(#>10&)] := p
    >> N[c, 3]
     = c
    >> N[c, 11]
     = 11.
     
    You can also use 'UpSet' or 'TagSet' to specify values for 'N':
    >> N[d] ^= 5;
    However, the value will not be stored in 'UpValues', but in 'NValues' (as for 'Set'):
    >> UpValues[d]
     = {}
    >> NValues[d]
     = {HoldPattern[N[d, MachinePrecision]] :> 5}
    >> e /: N[e] = 6;
    >> N[e]
     = 6.
     
    Values for 'N[$expr$]' must be associated with the head of $expr$:
    >> f /: N[e[f]] = 7;
     : Tag f not found or too deep for an assigned rule.
     
    You can use 'Condition':
    >> N[g[x_, y_], p_] := x + y * Pi /; x + y > 3
    >> SetAttributes[g, NHoldRest]
    >> N[g[1, 1]]
     = g[1., 1]
    >> N[g[2, 2]]
     = 8.28318530717958648
     
    #> p=N[Pi,100]
     = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
    #> ToString[p]
     = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
    """
    
    messages = {
        'precbd': "Requested precision `1` is not a machine-sized real number.",
    }
    
    rules = {
        'N[expr_]': 'N[expr, MachinePrecision]',
    }
        
    def apply_other(self, expr, prec, evaluation):
        'N[expr_, prec_]'
        
        valid_prec = get_precision(prec, evaluation)
        
        if valid_prec is not None:
            if expr.get_head_name() in ('List', 'Rule'):
                return Expression(expr.head, *[self.apply_other(leaf, prec, evaluation) for leaf in expr.leaves])
            if isinstance(expr, Number):
                return expr.round(valid_prec)
            
            name = expr.get_lookup_name()
            nexpr = Expression('N', expr, prec)
            result = evaluation.definitions.get_value(name, 'NValues', nexpr, evaluation)
            if result is not None:
                if not result.same(nexpr):
                    result = Expression('N', result, prec).evaluate(evaluation)
                return result
            
            if expr.is_atom():
                return expr.round(valid_prec)
            else:
                attributes = expr.head.get_attributes(evaluation.definitions)
                if 'NHoldAll' in attributes:
                    eval_range = []
                elif 'NHoldFirst' in attributes:
                    eval_range = range(1, len(expr.leaves))
                elif 'NHoldRest' in attributes:
                    if len(expr.leaves) > 0:
                        eval_range = (0,)
                    else:
                        eval_range = ()
                else:
                    eval_range = range(len(expr.leaves))
                head = Expression('N', expr.head, prec).evaluate(evaluation)
                leaves = expr.leaves[:]
                for index in eval_range:
                    leaves[index] = Expression('N', leaves[index], prec).evaluate(evaluation)
                return Expression(head, *leaves)
    
class MachinePrecision(Predefined):
    """
    <dl>
    <dt>'MachinePrecision'
        <dd>is a "pessimistic" (integer) estimation of the internally used standard precision.
    </dl>
    >> N[MachinePrecision]
     = 18.
    """
    
    def apply_N(self, prec, evaluation):
        'N[MachinePrecision, prec_]'
        
        prec = get_precision(prec, evaluation)
        if prec is not None:
            return Real(dps(machine_precision), prec)
    
class Precision(Builtin):
    """
    <dl>
    <dt>'Precision[$expr$]'
        <dd>examines the number of significant digits of $expr$.
    </dl>
    This is rather a proof-of-concept than a full implementation. Precision of
    compound expression is not supported yet.
    >> Precision[1]
     = Infinity
    >> Precision[1/2]
     = Infinity
    >> Precision[0.5]
     = 18.
    #> Precision[0.0]
     = 0.
    #> Precision[0.000000000000000000000000000000000000]
     = 0.
    #> Precision[-0.0]      (*Matematica gets this wrong *)
     = 0.
    #> Precision[-0.000000000000000000000000000000000000]  
     = 0.
    """
    
    rules = {
        'Precision[_Integer]': 'Infinity',
        'Precision[_Rational]': 'Infinity',
        'Precision[_Symbol]': 'Infinity',
        'Precision[z:0.0]': '0.',
        'Precision[z:-0.0]': '0.',
    }
    
    def apply_real(self, x, evaluation):
        'Precision[x_Real]'
        
        return Real(dps(x.get_precision()))
    
    def apply_complex(self, x, evaluation):
        'Precision[x_Complex]'
        
        if x.is_inexact():
            return Real(dps(x.get_precision()))
        else:
            return Symbol('Infinity')
        
def round(value, k):
    n = (1. * value / k).as_real_imag()[0]
    if n >= 0:
        n = sympy.Integer(n + 0.5)
    else:
        n = sympy.Integer(n - 0.5)
    return n * k
        
class Round(Builtin):
    """
    <dl>
    <dt>'Round[$expr$]'
        <dd>rounds $expr$ to the nearest integer.
    <dt>'Round[$expr$, $k$]'
        <dd>rounds $expr$ to the closest multiple of $k$.
    </dl>
    
    >> Round[10.6]
     = 11
    >> Round[0.06, 0.1]
     = 0.1
    ## This should return 0. but doesn't due to a bug in sympy
    >> Round[0.04, 0.1]
     = 0

    Constants can be rounded too
    >> Round[Pi, .5]
     = 3.
    >> Round[Pi^2]
     = 10

    Round to exact value
    >> Round[2.6, 1/3]
     = 8 / 3
    >> Round[10, Pi]
     = 3 Pi

    Round complex numbers
    >> Round[6/(2 + 3 I)]
     = 1 - I
    >> Round[1 + 2 I, 2 I]
     = 2 I

    Round Negative numbers too
    >> Round[-1.4]
     = -1
     
    Expressions other than numbers remain unevaluated:
    >> Round[x]
     = Round[x]
    >> Round[1.5, k]
     = Round[1.5, k]
    """

    attributes = ('Listable', 'NumericFunction')
    
    rules = {
        'Round[expr_?NumericQ]': 'Round[Re[expr], 1] + I * Round[Im[expr], 1]',
        'Round[expr_Complex, k_RealNumberQ]': 'Round[Re[expr], k] + I * Round[Im[expr], k]',
    }
    
    def apply(self, expr, k, evaluation):
        "Round[expr_?NumericQ, k_?NumericQ]"
        return from_sympy(round(expr.to_sympy(), k.to_sympy()))
    
def chop(expr, delta=10.0**(-10.0)):
    if isinstance(expr, Real):
        if -delta < expr.to_python() < delta:
            return Integer(0)
        #return expr
    elif isinstance(expr, Complex) and expr.get_precision() is not None:
        real, imag = expr.real, expr.imag
        if -delta < real.to_python() < delta:
            real = sympy.Integer(0)
        if -delta < imag.to_python() < delta:
            imag = sympy.Integer(0)
        if imag != 0:
            return Complex(real, imag)
        else:
            return Number.from_mp(real)
    elif isinstance(expr, Expression):
        return Expression(chop(expr.head), *[chop(leaf) for leaf in expr.leaves])
    return expr
        
class Chop(Builtin):
    """
    <dl>
    <dt>'Chop[$expr$]'
        <dd>replaces floating point numbers close to 0 by 0.
    <dt>'Chop[$expr$, $delta$]'
        <dd>uses a tolerance of $delta$. The default tolerance is '10^-10'.
    </dl>
    
    >> Chop[10.0 ^ -16]
     = 0
    >> Chop[10.0 ^ -9]
     = 1.*^-9
    >> Chop[10 ^ -11 I]
     = I / 100000000000
    >> Chop[0. + 10 ^ -11 I]
     = 0    
    """
    
    messages = {
        'tolnn': "Tolerance specification a must be a non-negative number.",
    }
    
    rules = {
        'Chop[expr_]': 'Chop[expr, 10^-10]',
    }
    
    def apply(self, expr, delta, evaluation):
        'Chop[expr_, delta_:(10^-10)]'
        
        delta = delta.evaluate(evaluation).get_real_value()
        if delta is None or delta < 0:
            return evaluation.message('Chop', 'tolnn')
        
        return chop(expr, delta=delta)
    
class NumericQ(Builtin):
    """
    <dl>
    <dt>'NumericQ[$expr$]'
        <dd>tests whether $expr$ represents a numeric quantity.
    </dl>
    
    >> NumericQ[2]
     = True
    >> NumericQ[Sqrt[Pi]]
     = True
    >> NumberQ[Sqrt[Pi]]
     = False
    """
    
    def apply(self, expr, evaluation):
        'NumericQ[expr_]'

        def test(expr):
            if isinstance(expr, Expression):
                attr = evaluation.definitions.get_attributes(expr.head.get_name())
                return 'NumericFunction' in attr and all(test(leaf) for leaf in expr.leaves)
            else:
                return expr.is_numeric()
            
        return Symbol('True') if test(expr) else Symbol('False')
                
class BaseForm(Builtin):
    """
    <dl> 
    <dt>'BaseForm[$expr$, $n$]'
        <dd>prints mumbers in $expr$ in base $n$.
    </dl>

    >> BaseForm[33, 2]
     = 100001_2

    >> BaseForm[234, 16]
     = ea_16

    >> BaseForm[12.3, 2]
     = 1100.0100110011001100110011001_2

    >> BaseForm[-42, 16]
     = -2a_16
    """

    messages = {
        'base': "Positive machine-sized integer expected at position 2 in BaseForm[`1`, `2`].",
    }
 

    def apply_makeboxes(self, expr, n, f, evaluation):
        'MakeBoxes[BaseForm[expr_, n_], f:StandardForm|TraditionalForm|OutputForm]'

        from mathics.core.expression import from_python
        base = n.get_int_value()

        if base <= 0:
            evaluation.message('BaseForm', 'base', expr, n)
            return

        if expr.get_int_value() is None and expr.get_real_value() is None:
            return from_python(expr)

        if isinstance(expr, Real):
            (num, real) = divmod(expr.get_real_value(), 1)

            # converts the decimal part to an integer
            # check http://stackoverflow.com/questions/4838994/float-to-binary
            # for more answers
            if num < 0:
                num += 1

            real = int(real * base**25)
            num = int(num)

            out = "%s.%s" % (int2base(num, base), 
                                int2base(real, base, zero_prefill=True))

            if f.same(Symbol('OutputForm')):
                return from_python("%s_%d" % (out, base))
            else:
                return Expression('SubscriptBox', from_python(out),
                    from_python(base))
        else:
            num = expr.get_int_value()

            if f.same(Symbol('OutputForm')):
                return from_python("%s_%d" % (int2base(num, base), base))
            else:
                return Expression('SubscriptBox', from_python(int2base(num, base)),
                    from_python(base))
