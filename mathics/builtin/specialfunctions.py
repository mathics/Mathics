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

# Bessel Functions

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
     = -Infinity
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

#TODO: Spherical Bessel Functions

# Hankel Functions

class HankelH1(_Bessel):
    """
    >> HankelH1[1.5, 4]
     = 0.185285948354268953 + 0.367112032460934155 I
    """

    sympy_name = 'hankel1'
    mpmath_name = 'hankel1'

class HankelH2(_Bessel):
    """
    >> HankelH2[1.5, 4]
     = 0.185285948354268953 - 0.367112032460934155 I
    """

    sympy_name = 'hankel2'
    mpmath_name = 'hankel2'

# Airy Functions

class AiryAi(_MPMathFunction):
    """
    >> AiryAi[0.5]
     = 0.23169360648083349

    >> AiryAi[0.5 + I]
     = 0.157118446499986172 - 0.241039813840210768 I
    """

    sympy_name = ''
    mpmath_name = 'airyai'

class AiryBi(_MPMathFunction):
    """
    >> AiryBi[0.5]
     = 0.854277043103155493

    >> AiryBi[0.5 + I]
     = 0.688145273113482414 + 0.370815390737010831 I
    """

    sympy_name = ''
    mpmath_name = 'airybi'

# Kelvin Functions

class KelvinBer(_Bessel):
    """
    >> KelvinBer[0.5]
     = 0.999023463990838256

    >> KelvinBer[1.5 + I]
     = 1.11620420872233787 - 0.117944469093970067 I

    >> KelvinBer[0.5, 0.25]
     = 0.148824330530639942
    """

    rules = {
        'KelvinBer[z_]': 'KelvinBer[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'ber'

class KelvinBei(_Bessel):
    """
    >> KelvinBei[0.5]
     = 0.0624932183821994586

    >> KelvinBei[1.5 + I]
     = 0.326323348699806294 + 0.75560557861089228 I

    >> KelvinBei[0.5, 0.25]
     = 0.370152900194021013
    """

    rules = {
        'KelvinBei[z_]': 'KelvinBei[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'bei'

class KelvinKer(_Bessel):
    """
    >> KelvinKer[0.5]
     = 0.855905872118634214

    >> KelvinKer[1.5 + I]
     = -0.167162242027385125 - 0.184403720314419905 I

    >> KelvinKer[0.5, 0.25]
     = 0.450022838747182502
    """

    rules = {
        'KelvinKer[z_]': 'KelvinKer[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'ker'

class KelvinKei(_Bessel):
    """
    >> KelvinKei[0.5]
     = -0.671581695094367603

    >> KelvinKei[1.5 + I]
     = -0.248993863536003923 + 0.303326291875385478 I

    >> KelvinKei[0.5, 0.25]
     = -2.05169683896315934
    """

    rules = {
        'KelvinKei[z_]': 'KelvinKei[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'kei'

# Struve and Related Functions

class StruveH(_Bessel):
    """
    >> StruveH[1.5, 3.5]
     = 1.13192125271801312
    """

    sympy_name = ''
    mpmath_name = 'struveh'

class StruveL(_Bessel):
    """
    >> StruveL[1.5, 3.5]
     = 4.41126360920433996
    """

    sympy_name = ''
    mpmath_name = 'struvel'

class AngerJ(_Bessel):
    """
    >> AngerJ[1.5, 3.5]
     = 0.294478574459563408
    """

    sympy_name = ''
    mpmath_name = 'angerj'

class WeberE(_Bessel):
    """
    >> WeberE[1.5, 3.5]
     = -0.397256259210030809
    """

    sympy_name = ''
    mpmath_name = 'webere'

# Function Zeros

class BesselJZero(_Bessel):
    """
    >> N[BesselJZero[0, 1]]
     = 2.40482555769577277
    """

    sympy_name = ''
    mpmath_name = 'besseljzero'
    
class BesselYZero(_Bessel):
    """
    >> N[BesselYZero[0, 1]]
     = 0.893576966279167522
    """

    sympy_name = ''
    mpmath_name = 'besselyzero'

class AiryAiZero(_MPMathFunction):
    """
    >> N[AiryAiZero[1]]
     = -2.33810741045976704
    """

    sympy_name = ''
    mpmath_name = 'airyaizero'

class AiryBiZero(_MPMathFunction):
    """
    >> N[AiryBiZero[1]]
     = -1.17371322270912792
    """

    sympy_name = ''
    mpmath_name = 'airybizero'

class Legendre(_MPMathFunction):
    def eval(self, z):
        return mpmath.legendre(1, z)
    
    def prepare_sympy(self, leaves):
        return [Integer(1)] + leaves
