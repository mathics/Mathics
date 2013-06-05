# -*- coding: utf8 -*-

"""
Numeric evaluation

Support for numeric evaluation with arbitrary precision is just a proof-of-concept.
Precision is not "guarded" through the evaluation process. Only integer precision is supported.
However, things like 'N[Pi, 100]' should work as expected.
"""

import mpmath
import sympy

from mathics.builtin.base import Builtin, Predefined, SympyConstant
from mathics.core.numbers import dps, mpmath2sympy, prec, convert_base
from mathics.core import numbers
from mathics.core.expression import (Integer, Rational, Real, Complex, Atom,
        Expression, Number, Symbol, from_python)
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
     = 11.000000000000000000 + b
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
     = 11.000000000
     
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

    ## Atoms
    #> N[5, 25]
     = 5.000000000000000000000000
    #> N[5/2, 25]
     = 2.500000000000000000000000
    #> N[3 + I, 25]
     = 3.000000000000000000000000 + 1.000000000000000000000000 I
    #> N[3/2 + I, 25]
     = 1.500000000000000000000000 + 1.000000000000000000000000 I
    #> N[3.5, 25]
     = 3.500000000000000000000000
     
    ## Mathemaitcal Constants
    #> p = N[Pi,100]
     = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
    #> ToString[p]
     = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068

    ## Elementary Functions
    #> N[Cos[1/2], 100]
     = 0.8775825618903727161162815826038296519916451971097440529976108683159507632742139474057941840846822584
    #> N[ArcTan[3/2], 50]
     = 0.98279372324732906798571061101466601449687745363163
    #> N[ArcSin[3], 30]
     = 1.57079632679489661923132169164 - 1.76274717403908605046521864996 I

    ## Special Function
    #> N[Zeta[3], 50]
     = 1.2020569031595942853997381615114499907649862923405
    #> N[Erf[3/2], 30]
     = 0.966105146475310727066976261646
    #> N[ProductLog[5/2], 25]
     = 0.9585863567287029121698668
    """

    # TODO
    """
    ## Big Numbers
    #> N[500!]
     = 1.22013682599111007*^1134
    #> N[500!, 25]
     = 1.220136825991110068701239*^1134
    """

    # TODO
    """
    #> N[Cos[Exp[Sqrt[3]] - BesselY[4, 1]], 34]
     = 0.3327813156758121757179731972059867
    """

    # TODO: Fix MachinePrecision Numbers
    """
    #> N[1, 10]
     = 1.000000000
    #> Precision[%]
     = 10.
    #> N[1]
     = 1.
    #> Precision[%]
     = MachinePrecision
    #> N[1, $MachinePrecision]
     = 1.00000000000000000
    """

    # XXX: Don't affect "exact numeric powers" - buggy in MMA8
    """
    #> N[x ^ (5 / 2)]
     = x ^ (5 / 2)

    #> N[x ^ Pi]
     = x ^ Pi

    #> N[(5 / 2) ^ x]
     = 2.5 ^ x
    """
    
    messages = {
        'precbd': "Requested precision `1` is not a machine-sized real number.",
        'preclg': ("Requested precision `1` is larger than $MaxPrecision. "
                   "Using current $MaxPrecision of `2` instead. "
                   "$MaxPrecision = Infinity specifies that any precision "
                   "should be allowed."),
        'precsm': ("Requested precision `1` is smaller than $MinPrecision. "
                   "Using $MinPrecision instead.")
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
    
class MachinePrecision(SympyConstant):
    """
    <dl>
    <dt>'MachinePrecision'
        <dd>is a symbol used to represent the internally used standard precision.
    </dl>
    >> N[MachinePrecision]
     = 18.

    #> MachinePrecision
     = MachinePrecision

    #> Attributes[MachinePrecision]
     = {Constant, Protected}
    """

    attributes = ('Constant', 'Protected')

    def apply_N(self, prec, evaluation):
        'N[MachinePrecision, prec_]'
        
        prec = get_precision(prec, evaluation)
        if prec is not None:
            return Real(dps(machine_precision), prec)
    
class MachinePrecision_Symbol(Predefined):
    """
    <dl>
    <dt>'$MachinePrecision'
        <dd>is a "pessimistic" (integer) estimation of the internally used standard precision.
    </dl>

    >> $MachinePrecision
     = 18.

    #> Attributes[$MachinePrecision]
     = {Protected}
    """

    # TODO
    """
    #> Precision[$MachinePrecision]
     = MachinePrecision
    """

    name = '$MachinePrecision'

    attributes = ('Protected',)

    def evaluate(self, evaluation):
        prec = get_precision(Symbol('MachinePrecision'), evaluation)
        if prec is not None:
            return Real(dps(machine_precision), prec)


class MinPrecision(Predefined):
    """
    <dl>
    <dt>'$MinPrecision'
        <dd>is the minimum number of digits of precision allowed in arbitrary-precision numbers.
    </dl>

    >> $MinPrecision
     = 0

    ## #> Block[{$MinPrecision = 50}, N[Pi, 25]]
    ##  : Requested precision 25 is smaller than $MinPrecision. Using $MinPrecision instead.
    ##  = 3.1415926535897932384626433832795028841971693993751

    #> $MinPrecision = -5
     : Cannot set $MinPrecision to -5; value must be a non-negative number or Infinity.
     = -5
    #> $MinPrecision
     = 0
    #> $MinPrecison = 4
     = 4
    #> $MinPrecision = 10.4
     = 10.4
    #> $MinPrecision = 10/3
     = 10 / 3
    #> $MinPrecision = Infinity
     = Infinity

    #> $MinPrecision = 0; $MaxPrecision = 10;

    #> $MinPrecision = 15
     : Cannot set $MinPrecision such that $MaxPrecision < $MinPrecision.
     = 15
    #> $MinPrecision = 10
     = 10
    #> $MinPrecision = 5
     = 5

    #> $MaxPrecision = Infinity; $MinPrecision = 0;
    """

    name = '$MinPrecision'

    messages = {
        'preccon': "Cannot set `1` such that $MaxPrecision < $MinPrecision.",
        'precset': ("Cannot set `1` to `2`; value must be a non-negative "
                    "number or Infinity."),
    }

    rules = {
        '$MinPrecision': '0',
    }

    attributes = ()

    #def evaluate(self, evaluation):
    #    return MIN_PRECISION


class MaxPrecision(Predefined):
    """
    <dl>
    <dt>'$MaxPrecision'
        <dd>is the number of digits of precision allowed in arbitrary-precision numbers.
    </dl>

    >> $MaxPrecision
     = Infinity

    ## #> Block[{$MaxPrecision = 50}, N[Pi, 100]]
    ##  : Requested precision 100 is larger than $MaxPrecision. Using current $MaxPrecision of 50. instead. $MaxPrecision = Infinity specifies that any precision should be allowed.
    ##  = 3.1415926535897932384626433832795028841971693993751

    #> $MaxPrecision = 0
     : Cannot set $MaxPrecision to 0; value must be a positive number or Infinity.
     = 0
    #> $MaxPrecision = -5
     : Cannot set $MaxPrecision to -5; value must be a positive number or Infinity.
     = -5
    #> $MaxPrecision
     = Infinity
    #> $MaxPrecison = 4
     = 4
    #> $MaxPrecision = 10.4
     = 10.4
    #> $MaxPrecision = 10/3
     = 10 / 3

    #> $MaxPrecision = Infinity; $MinPrecision = 10;
    #> $MaxPrecision = 15
     = 15
    #> $MaxPrecision = 10
     = 10
    #> $MaxPrecision = 5
     : Cannot set $MaxPrecision such that $MaxPrecision < $MinPrecision.
     = 5

    #> $MaxPrecision = Infinity; $MinPrecision = 0;
    """

    name = '$MaxPrecision'

    messages = {
        'preccon': "Cannot set `1` such that $MaxPrecision < $MinPrecision.",
        'precset': ("Cannot set `1` to `2`; value must be a positive number "
                    "or Infinity."),
    }

    attributes = ()

    rules = {
        '$MaxPrecision': 'Infinity',
    }


class MaxExtraPrecision(Predefined):
    """
    <dl>
    <dt>'$MaxExtraPrecision'
        <dd>is the maximum of extra digits used in numerical calculations.
    </dl>

    >> $MaxExtraPrecision
     = 50.

    ## >> N[Cos[Exp[500]], 20]
    ##  Internal precision limit $MaxExtraPrecision = 50. reached while evaluating Cos[Exp[500]].
    ##  = 0.

    ## >> Block[{$MaxExtraPrecision = 1000}, N[Cos[Exp[500]], 20]]
    ##  = 0.88536064016933422109

    #> $MaxExtraPrecision = -1
     : Cannot set $MaxExtraPrecision to -1; value must be a non-negative number or Infinity.
     = -1

    #> $MaxExtraPrecision = x
     : Cannot set $MaxExtraPrecision to x; value must be a non-negative number or Infinity.
     = x

    #> $MaxExtraPrecision = 0
     = 0
    #> MaxExtraPrecision = 11/3
     = 11 / 3
    #> MaxRxtraPrecision = 50.
     = 50.
    """

    name = '$MaxExtraPrecision'

    messages = {
        'precset': ("Cannot set `1` to `2`; value must be a non-negative "
                    "number or Infinity."),
    }

    attributes = ()

    rules = {
        '$MaxExtraPrecision': '50.',
    }


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
     = 1100.010011001100110011_2

    >> BaseForm[-42, 16]
     = -2a_16

    >> BaseForm[x, 2]
     = x

    >> BaseForm[12, 3] // FullForm
     = BaseForm[12, 3]

    >> BaseForm[12, -3]
     : Positive machine-sized integer expected at position 2 in BaseForm[12, -3]. 
     : MakeBoxes[BaseForm[12, -3], OutputForm] is not a valid box structure. 
    """

    messages = {
        'intpm': "Positive machine-sized integer expected at position 2 in BaseForm[`1`, `2`].",
    }
 

    def apply_makeboxes(self, expr, n, f, evaluation):
        'MakeBoxes[BaseForm[expr_, n_], f:StandardForm|TraditionalForm|OutputForm]'

        base = n.get_int_value()

        if base <= 0:
            evaluation.message('BaseForm', 'intpm', expr, n)
            return

        if not (isinstance(expr, Integer) or isinstance(expr, Real)):
            return Expression("MakeBoxes", expr, f)

        p = dps(expr.get_precision()) if isinstance(expr, Real) else 0
        val = convert_base(expr.get_real_value(), base, p)

        if f.get_name() == 'OutputForm':
            return from_python("%s_%d" % (val, base))
        else:
            return Expression('SubscriptBox', from_python(val),
                from_python(base))
