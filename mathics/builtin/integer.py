# -*- coding: utf8 -*-

"""
Integer functions
"""

from gmpy import mpz
from mpmath import iv, mp

from mathics.builtin.base import Builtin
from mathics.core.numbers import mpmath2gmpy, mpz2mpmath
from mathics.core import numbers
from mathics.core.expression import Integer, Rational, Real, Expression

class Floor(Builtin):
    """
    <dl>
    <dt>'Floor[$x$]'
        <dd>gives the smallest integer less than or equal to $x$.
    <dt>'Floor[$x$, $a$]'
        <dd>gives the smallest multiple of $a$ less than or equal to $x$.
    </dl>
    
    >> Floor[10.4]
     = 10
    >> Floor[10/3]
     = 3
    >> Floor[10]
     = 10
    >> Floor[21, 2]
     = 20
    >> Floor[2.6, 0.5]
     = 2.5
    >> Floor[-10.4]
     = -11
     
    For negative $a$, the smallest multiple of $a$ greater than or equal to $x$
    is returned.
    >> Floor[10.4, -1]
     = 11
    >> Floor[-10.4, -1]
     = -10
    """
    
    rules = {
        'Floor[x_, a_]': 'Floor[x / a] * a',
        'Floor[z_Complex]': 'Complex[Floor[Re[z]], Floor[Im[z]]]',
    }
    
    def apply_real(self, x, evaluation):
        'Floor[x_?RealNumberQ]'
        
        x = x.value
        if x < 0:
            floor = - mpz(abs(x))
            if x != floor:
                floor -= 1
        else:
            floor = mpz(x)
        return Integer(floor)
    
class IntegerLength(Builtin):
    """
    >> IntegerLength[123456]
     = 6
    >> IntegerLength[10^10000]
     = 10001
    >> IntegerLength[-10^1000]
     = 1001
    'IntegerLength' with base 2:
    >> IntegerLength[8, 2]
     = 4
    Check that 'IntegerLength' is correct for the first 100 powers of 10:
    >> IntegerLength /@ (10 ^ Range[100]) == Range[2, 101]
     = True
    The base must be greater than 1:
    >> IntegerLength[3, -2]
     : Base -2 is not an integer greater than 1.
     = IntegerLength[3, -2]
    """
    
    rules = {
        'IntegerLength[n_]': 'IntegerLength[n, 10]',
    }
    
    messages = {
        'base': "Base `1` is not an integer greater than 1.",
    }
    
    def apply(self, n, b, evaluation):
        'IntegerLength[n_, b_]'
        
        # Use interval arithmetic to account for "right" rounding
        
        n, b = n.get_int_value(), b.get_int_value()
        if n is None or b is None:
            evaluation.message('IntegerLength', 'int')
            return
        if b <= 1:
            evaluation.message('IntegerLength', 'base', b)
            return
         
        result = mp.mpf(iv.log(iv.mpf(mpz2mpmath(abs(n))), mpz2mpmath(b)).b)
        result = mpz(mpmath2gmpy(result)) + 1
        return Integer(result)
