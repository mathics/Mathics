# -*- coding: utf8 -*-

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Integer, from_sympy

import sympy

from gmpy import mpz, bincoef

def sympy_factor(expr_sympy):
    try:
        result = sympy.together(expr_sympy)
        numer, denom = result.as_numer_denom()
        if denom == 1:
            result = sympy.factor(expr_sympy)
        else:
            result = sympy.factor(numer) / sympy.factor(denom)
    except sympy.PolynomialError:
        return expr_sympy
    return result

def cancel(expr):
    if expr.has_form('Plus', None):
        return Expression('Plus', *[cancel(leaf) for leaf in expr.leaves])
    else:
        try:
            result = expr.to_sympy()
            #result = sympy.powsimp(result, deep=True)
            result = sympy.cancel(result)
            result = sympy_factor(result)   # cancel factors out rationals, so we factor them again
            return from_sympy(result)
        except sympy.PolynomialError:
            # e.g. for non-commutative expressions
            return expr
    
class Cancel(Builtin):
    """
    <dl>
    <dt>'Cancel[$expr$]'
        <dd>cancels out common factors in numerators and denominators. 
    </dl>
    
    >> Cancel[x / x ^ 2]
     = 1 / x
    'Cancel' threads over sums:
    >> Cancel[x / x ^ 2 + y / y ^ 2]
     = 1 / x + 1 / y
     
    >> Cancel[f[x] / x + x * f[x] / x ^ 2]
     = 2 f[x] / x
    """
    
    def apply(self, expr, evaluation):
        'Cancel[expr_]'
        
        return cancel(expr)

class Simplify(Builtin):
    """
    <dl>
    <dt>'Simplify[$expr$]'
        <dd>simplifies $expr$.
    </dl>
    
    >> Simplify[2*Sin[x]^2 + 2*Cos[x]^2]
     = 2
    >> Simplify[x]
     = x
    >> Simplify[f[x]]
     = f[x]
    """
    
    rules = {
        'Simplify[list_List]': 'Simplify /@ list',
        'Simplify[rule_Rule]': 'Simplify /@ rule',
        'Simplify[eq_Equal]': 'Simplify /@ eq',
    }
    
    def apply(self, expr, evaluation):
        'Simplify[expr_]'
        
        expr_sympy = expr.to_sympy()
        result = expr_sympy
        result = sympy.simplify(result)
        result = sympy.trigsimp(result)
        result = sympy.together(result)
        result = sympy.cancel(result)
        result = from_sympy(result)
        return result
    
class Together(Builtin):
    """
    <dl>
    <dt>'Together[$expr$]'
        <dd>writes sums of fractions in $expr$ together.
    </dl>
    
    >> Together[a / c + b / c]
     = (a + b) / c
    'Together' operates on lists:
    >> Together[{x / (y+1) + x / (y+1)^2}]
     = {x (2 + y) / (1 + y) ^ 2}
    But it does not touch other functions:
    >> Together[f[a / c + b / c]]
     = f[a / c + b / c]
     
    #> f[x]/x+f[x]/x^2//Together
     = (1 + x) f[x] / x ^ 2
    """
    
    attributes = ['Listable']
    
    def apply(self, expr, evaluation):
        'Together[expr_]'
        
        expr_sympy = expr.to_sympy()
        result = sympy.together(expr_sympy)
        result = from_sympy(result)
        result = cancel(result)
        return result
    
class Factor(Builtin):
    """
    <dl>
    <dt>'Factor[$expr$]'
        <dd>factors the polynomial expression $expr$.
    </dl>
    
    >> Factor[x ^ 2 + 2 x + 1]
     = (1 + x) ^ 2
    
    >> Factor[1 / (x^2+2x+1) + 1 / (x^4+2x^2+1)]
     = (2 + 2 x + 3 x ^ 2 + x ^ 4) / ((1 + x) ^ 2 (1 + x ^ 2) ^ 2)
    """
    
    def apply(self, expr, evaluation):
        'Factor[expr_]'
        
        expr_sympy = expr.to_sympy()
        try:
            result = sympy.together(expr_sympy)
            numer, denom = result.as_numer_denom()
            if denom == 1:
                result = sympy.factor(expr_sympy)
            else:
                result = sympy.factor(numer) / sympy.factor(denom)
        except sympy.PolynomialError:
            return expr
        return from_sympy(result)
    
class Apart(Builtin):
    """
    <dl>
    <dt>'Apart[$expr$]'
        <dd>writes $expr$ as sum of individual fractions.
    <dt>'Apart[$expr$, $var$]'
        <dd>treats $var$ as main variable.
    </dl>
    
    >> Apart[1 / (x^2 + 5x + 6)]
     = 1 / (2 + x) - 1 / (3 + x)
     
    When several variables are involved, the results can be different depending on the main variable: 
    >> Apart[1 / (x^2 - y^2), x]
     = -1 / (2 y (x + y)) + 1 / (2 y (x - y))
    >> Apart[1 / (x^2 - y^2), y]
     = 1 / (2 x (x + y)) + 1 / (2 x (x - y))
     
    'Apart' is 'Listable':
    >> Apart[{1 / (x^2 + 5x + 6)}]
     = {1 / (2 + x) - 1 / (3 + x)}
    
    But it does not touch other expressions:
    >> Sin[1 / (x ^ 2 - y ^ 2)] // Apart
     = Sin[1 / (x ^ 2 - y ^ 2)]
    """
    
    attributes = ['Listable']
    rules = {
        'Apart[expr_]': 'Block[{vars=Cases[Level[expr, {-1}], _Symbol]}, If[Length[vars] > 0, Apart[expr, vars[[1]]], expr]]',
    }
    
    def apply(self, expr, var, evaluation):
        'Apart[expr_, var_Symbol]'
        
        expr_sympy = expr.to_sympy()
        var_sympy = var.to_sympy()
        try:
            result = sympy.apart(expr_sympy, var_sympy)
            result = from_sympy(result)
            return result
        except sympy.PolynomialError:
            # raised e.g. for apart(sin(1/(x**2-y**2)))
            return expr

class Expand(Builtin):
    """
    <dl>
    <dt>'Expand[$expr$]'
        <dd>expands out positive integer powers and products of sums in $expr$.
    </dl>
    >> Expand[(x + y) ^ 3]
     = x ^ 3 + 3 x ^ 2 y + 3 x y ^ 2 + y ^ 3
    >> Expand[(a + b) (a + c + d)]
     = a ^ 2 + a b + a c + a d + b c + b d
    >> Expand[(a + b) (a + c + d) (e + f) + e a a]
     = 2 a ^ 2 e + a ^ 2 f + a b e + a b f + a c e + a c f + a d e + a d f + b c e + b c f + b d e + b d f
    >> Expand[(a + b) ^ 2 * (c + d)]
     = a ^ 2 c + a ^ 2 d + 2 a b c + 2 a b d + b ^ 2 c + b ^ 2 d
    >> Expand[(x + y) ^ 2 + x y]
     = x ^ 2 + 3 x y + y ^ 2
    >> Expand[((a + b) (c + d)) ^ 2 + b (1 + a)]
     = a ^ 2 c ^ 2 + 2 a ^ 2 c d + a ^ 2 d ^ 2 + b + a b + 2 a b c ^ 2 + 4 a b c d + 2 a b d ^ 2 + b ^ 2 c ^ 2 + 2 b ^ 2 c d + b ^ 2 d ^ 2
    
    'Expand' expands items in lists and rules:
    >> Expand[{4 (x + y), 2 (x + y) -> 4 (x + y)}]
     = {4 x + 4 y, 2 x + 2 y -> 4 x + 4 y}
    
    'Expand' does not change any other expression.
    >> Expand[Sin[x (1 + y)]]
     = Sin[x (1 + y)]
    
    #> a(b(c+d)+e) // Expand
     = a b c + a b d + a e
     
    #> (y^2)^(1/2)/(2x+2y)//Expand
     = Sqrt[y ^ 2] / (2 x + 2 y)
     
    ## This caused a program crash!
    #> 2(3+2x)^2/(5+x^2+3x)^3 // Expand
     = 24 x / (5 + 3 x + x ^ 2) ^ 3 + 8 x ^ 2 / (5 + 3 x + x ^ 2) ^ 3 + 18 / (5 + 3 x + x ^ 2) ^ 3
    """ 
    
    def apply(self, expr, evaluation):
        'Expand[expr_]'
        
        def expand(expr):
            head_name = expr.get_head_name()
            if head_name in ('List', 'Rule'):
                return Expression(head_name, *[expand(leaf) for leaf in expr.leaves])
            leaves = expr.get_leaves()
            if expr.has_form('Times', 2, None):
                " Group negative powers into one negative power "
                neg_powers = []
                other_leaves = []
                for leaf in leaves:
                    if leaf.has_form('Power', 2) and leaf.leaves[1].get_int_value() is not None and leaf.leaves[1].get_int_value() < 0:
                        neg_powers.append(leaf)
                    else:
                        other_leaves.append(leaf)
                if len(neg_powers) > 1:
                    leaves = other_leaves + [Expression('Power', Expression('Times', *[Expression('Power', leaf.leaves[0], Integer(mpz(-leaf.leaves[1].value))) for leaf in neg_powers]), Integer(-1))]
            if head_name in ('Plus', 'Times', 'Power'):
                leaves = [expand(leaf) for leaf in leaves]
            if expr.has_form('Times', 2, None):
                result = [[]]
                has_plus = False
                for leaf in leaves:
                    if leaf.has_form('Plus', 1):
                        leaf = leaf.leaves[0]
                    if leaf.has_form('Plus', 2, None):
                        new_result = []
                        for summand in leaf.leaves:
                            if summand.has_form('Times', None):
                                add = summand.leaves
                            else:
                                add = [summand]
                            new_result.extend(item + add for item in result)
                        result = new_result
                        has_plus = True
                    else:
                        if leaf.has_form('Times', None):
                            add = leaf.leaves
                        else:
                            add = [leaf]
                        result = [item + add for item in result]
                if has_plus:
                    return Expression('Plus', *(expand(Expression('Times', *item)) for item in result))
                else:
                    return Expression('Plus', *(Expression('Times', *item) for item in result))
            elif expr.has_form('Power', 2):
                n = leaves[1].get_int_value()
                sum = leaves[0]
                if sum.has_form('Plus', None) and n is not None and n > 0:
                    result = []
                    items = sum.leaves
                    
                    def iterate(rest, n_rest):
                        if rest and n_rest > 0:
                            for k in range(n_rest + 1):
                                for coeff, next in iterate(rest[1:], n_rest - k):
                                    if k == 0:
                                        this_factor = []
                                    else:
                                        this_factor = [Expression('Power', rest[0], Integer(k))]
                                    yield (bincoef(n_rest, k) * coeff, this_factor + next)
                        elif n_rest == 0:
                            yield (mpz(1), [])
                            
                    def times(coeff, factors):
                        if coeff == 1:
                            return Expression('Times', *factors)
                        else:
                            return Expression('Times', Integer(coeff), *factors)
                          
                    return Expression('Plus', *[times(coeff, factors) for coeff, factors in iterate(items, n)])
                else:
                    return Expression(expr.head, *[expand(leaf) for leaf in expr.leaves])
            elif expr.has_form('Plus', 2, None):
                return Expression('Plus', *leaves)
            else:
                return expr
            
        result = expand(expr)
        return result
    
class PowerExpand(Builtin):
    """
    <dl>
    <dt>'PowerExpand[$expr$]'
        <dd>expands out powers of the form '(x^y)^z' and '(x*y)^z' in $expr$.
    </dl>
    
    >> PowerExpand[(a ^ b) ^ c]
     = a ^ (b c)
    >> PowerExpand[(a * b) ^ c]
     = a ^ c b ^ c
     
    'PowerExpand' is not correct without certain assumptions:
    >> PowerExpand[(x ^ 2) ^ (1/2)]
     = x
    """
    
    rules = {
        'PowerExpand[(x_ ^ y_) ^ z_]': 'x ^ (y * z)',
        'PowerExpand[(x_ * y_) ^ z_]': 'x ^ z * y ^ z',
        'PowerExpand[Log[x_ ^ y_]]': 'y * Log[x]',
        'PowerExpand[x_Plus]': 'PowerExpand /@ x',
        'PowerExpand[x_Times]': 'PowerExpand /@ x',
        'PowerExpand[x_Power]': 'PowerExpand /@ x',
        'PowerExpand[x_List]': 'PowerExpand /@ x',
        'PowerExpand[x_Rule]': 'PowerExpand /@ x',
        'PowerExpand[other_]': 'other',
    }
    
class Numerator(Builtin):
    """
    <dl>
    <dt>'Numerator[$expr$]'
        <dd>gives the numerator in $expr$.
    </dl>
    
    >> Numerator[a / b]
     = a
    >> Numerator[2 / 3]
     = 2
    >> Numerator[a + b]
     = a + b
    """
    
    def apply(self, expr, evaluation):
        'Numerator[expr_]'
        
        sympy_expr = expr.to_sympy()
        numer, denom = sympy_expr.as_numer_denom()
        return from_sympy(numer)
    
class Denominator(Builtin):
    """
    <dl>
    <dt>'Denominator[$expr$]'
        <dd>gives the denominator in $expr$.
    </dl>
    
    >> Denominator[a / b]
     = b
    >> Denominator[2 / 3]
     = 3
    >> Denominator[a + b]
     = 1
    """
    
    def apply(self, expr, evaluation):
        'Denominator[expr_]'
        
        sympy_expr = expr.to_sympy()
        numer, denom = sympy_expr.as_numer_denom()
        return from_sympy(denom)
