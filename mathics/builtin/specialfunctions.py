# -*- coding: utf8 -*-

"""
Special functions
"""

import mpmath

from mathics.builtin.arithmetic import _MPMathFunction
from mathics.core.expression import Integer

class Erf(_MPMathFunction):
    """
    <dl>
    <dt>'Erf[$z$]'
        <dd>returns the error function of $z$.
    </dl>
    
    >> Erf[1.0]
     = 0.842700792949714869
    >> Erf[0]
     = 0
    >> Plot[Erf[x], {x, -2, 2}]
     = -Graphics-
    """
    
    def eval(self, z):
        return mpmath.erf(z)
    
class ProductLog(_MPMathFunction):
    """
    <dl>
    <dt>'ProductLog[$z$]'
        <dd>returns the value of the Lambert W function at $z$.
    </dl>
    
    The defining equation:
    >> z == ProductLog[z] * E ^ ProductLog[z]
     = True
     
    Some special values:
    >> ProductLog[0]
     = 0
    >> ProductLog[E]
     = 1
     
    The graph of 'ProductLog':
    >> Plot[ProductLog[x], {x, -1/E, E}]
     = -Graphics-
    """
    
    sympy_name = 'LambertW' # function called LambertW in SymPy
    sage_name = ''          # function not present in Sage
    
    rules = {
        'ProductLog[0]': '0',
        'ProductLog[E]': '1',
        'ProductLog[z_] * E ^ ProductLog[z_]': 'z',
    }
    
    def eval(self, z):
        return mpmath.lambertw(z)

class Legendre(_MPMathFunction):
    def eval(self, z):
        return mpmath.legendre(1, z)
    
    def prepare_sympy(self, leaves):
        return [Integer(1)] + leaves
