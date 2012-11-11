# -*- coding: utf8 -*-

from __future__ import with_statement

import sympy

from mathics.builtin.base import Builtin, Predefined, BinaryOperator
from mathics.core.expression import Expression, Integer, Real, Number, Symbol, from_sympy
from mathics.core.numbers import min_prec, SpecialValueError
from mathics.builtin.numeric import dps

class Fibonacci(Builtin):
    """
    <dl>
    <dt>'Fibonacci[$n$]'
        <dd>computes the $n$th Fibonacci number.
    </dl>
    
    >> Fibonacci[0]
     = 0
    >> Fibonacci[1]
     = 1
    >> Fibonacci[10]
     = 55
    >> Fibonacci[200]
     = 280571172992510140037611932413038677189525
    """
    
    attributes = ('Listable', 'NumericFunction', 'ReadProtected')
    
    def apply(self, n, evaluation):
        'Fibonacci[n_Integer]'
        
        return Integer(sympy.fibonacci(n.to_sympy()))
    
class Binomial(Builtin):
    """
    <dl>
    <dt>'Binomial[$n$, $k$]'
        <dd>gives the binomial coefficient $n$ choose $k$.
    </dl>
    
    >> Binomial[5, 3]
     = 10
     
    'Binomial' supports inexact numbers:
    >> Binomial[10.5,3.2]
     = 165.286109367256421
     
    Some special cases:
    >> Binomial[10, -2]
     = 0
    >> Binomial[-10.5, -3.5]
     = 0.
    >> Binomial[-10, -3.5]
     = ComplexInfinity
    """
    
    attributes = ('Listable', 'NumericFunction')
    
    def apply_exact(self, n, k, evaluation):
        'Binomial[n_Integer, k_Integer]'
        
        if k.to_sympy() < 0:
            return Integer(0)
        return Integer(sympy.binomial(n.to_sympy(), k.to_sympy()))
    
    def apply_inexact(self, n, k, evaluation):
        'Binomial[n_?InexactNumberQ, k_?NumberQ]'
        
        prec = min_prec(n, k)
        n = n.to_sympy()
        k = k.to_sympy()
        result = sympy.binomial(n, k).n(dps(prec))
        if result == sympy.Float('inf'):
            return Symbol('ComplexInfinity')
        return Real(result, prec)
        
    def apply_inexact_2(self, n, k, evaluation):
        'Binomial[n_?NumberQ, k_?InexactNumberQ]'
        
        return self.apply_inexact(n, k, evaluation)
        
class Multinomial(Builtin):
    """
    <dl>
    <dt>'Multinomial[$n1$, $n2$, ...]'
        <dd>gives the multinomial coefficient '($n1$+$n2$+...)!/($n1$!$n2$!...)'.
    </dl>
    
    >> Multinomial[2, 3, 4, 5]
     = 2522520
    >> Multinomial[]
     = 1
    Multinomial is expressed in terms of 'Binomial':
    >> Multinomial[a, b, c]
     = Binomial[a, a] Binomial[a + b, b] Binomial[a + b + c, c]
    'Multinomial[$n$-$k$, $k$]' is equivalent to 'Binomial[$n$, $k$]'.
    >> Multinomial[2, 3]
     = 10
    """
    
    attributes = ('Listable', 'NumericFunction', 'Orderless')
    
    def apply(self, values, evaluation):
        'Multinomial[values___]'
        
        values = values.get_sequence()
        result = Expression('Times')
        total = []
        for value in values:
            total.append(value)
            result.leaves.append(Expression('Binomial', Expression('Plus', *total), value))
        return result
    
