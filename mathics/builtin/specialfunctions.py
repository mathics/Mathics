# -*- coding: utf8 -*-

"""
Special functions
"""

import mpmath

from mathics.builtin.arithmetic import _MPMathFunction
from mathics.core.expression import Integer
from mathics.core.numbers import min_prec, sympy2mpmath, mpmath2sympy
from mathics.core.convert import from_sympy
from mathics.builtin.base import SympyFunction

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

    mpmath_name = 'erf'
    
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
    mpmath_name = 'lambertw'
    
    rules = {
        'ProductLog[0]': '0',
        'ProductLog[E]': '1',
        'ProductLog[z_] * E ^ ProductLog[z_]': 'z',
    }
    
class Zeta(_MPMathFunction):
    """
    >> Zeta[2]
     = Pi ^ 2 / 6

    >> Zeta[-2.5 + I]
     = 0.0235936105863796486 + 0.00140779960583837704 I
    """

    sympy_name = 'zeta'
    mpmath_name = 'zeta'

class _Bessel(SympyFunction):
    def apply_inexact1(self, n, z, evaluation):
        '%(name)s[n_, z_?InexactNumberQ]'

        prec = min_prec(n, z)
        with mpmath.workprec(prec):
            n, z = sympy2mpmath(n.to_sympy()), sympy2mpmath(z.to_sympy())
            if n is None or z is None:
                return
            try:
                result = self.eval(n, z)
                result = mpmath2sympy(result, prec)
            except ValueError, exc:
                text = str(exc)
                if text == 'gamma function pole':
                    return Symbol('ComplexInfinity')
                else:
                    raise
            except ZeroDivisionError:
                return
        return from_sympy(result)
            
    def apply_inexact2(self, n, z, evaluation):
        '%(name)s[n_?InexactNumberQ, z_]'

        return self.apply_inexact1(n, z, evaluation)

    def eval(self, n, z):
        if self.mpmath_name is None:
            return None
        
        mpmath_function = getattr(mpmath, self.mpmath_name)
        return mpmath_function(n, z)

class BesselJ(_Bessel):
    """
    >> BesselJ[0, 5.2]
     = -0.11029043979098654

    #> BesselJ[2.5, 1]
     = 0.0494968102284779423

    ## >> D[BesselJ[n, z], z]
    ##  = BesselJ[n - 1, z] / 2 - BesselJ[n + 1, z] / 2
    """

    #TODO: Sympy Backend is not as powerful as Mathmeatica
    """
    >> BesselJ[1/2, x]
     = Sqrt[2 / Pi] Sin[x] / Sqrt[x]
    """

    sympy_name = 'besselj'
    mpmath_name = 'besselj'

class BesselY(_Bessel):
    """
    >> BesselY[1.5, 4]
     = 0.367112032460934155
    """

    #TODO: Special Values
    """
    >> BesselY[0, 0]
     = - Infinity
    """

    sympy_name = 'bessely'
    mpmath_name = 'bessely'

class BesselI(_Bessel):
    """
    >> BesselI[1.5, 4]
     = 8.17263323168659544
    """

    sympy_name = 'besseli'
    mpmath_name = 'besseli'

class BesselK(_Bessel):
    """
    >> BesselK[1.5, 4]
     = 0.0143470307207600668
    """

    sympy_name = 'besselk'
    mpmath_name = 'besselk'

class Legendre(_MPMathFunction):
    def eval(self, z):
        return mpmath.legendre(1, z)
    
    def prepare_sympy(self, leaves):
        return [Integer(1)] + leaves
