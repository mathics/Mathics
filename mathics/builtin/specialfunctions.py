# -*- coding: utf8 -*-

"""
Special functions
"""

import mpmath

from mathics.builtin.arithmetic import _MPMathFunction
from mathics.core.expression import Integer, Symbol
from mathics.core.numbers import min_prec, sympy2mpmath, mpmath2sympy, SpecialValueError
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
    <dl>
    <dt>'Zeta[$z$]'
      <dd>returns the Riemann zeta function of $z$.
    </dl>

    >> Zeta[2]
     = Pi ^ 2 / 6

    >> Zeta[-2.5 + I]
     = 0.0235936105863796486 + 0.00140779960583837704 I
    """

    sympy_name = 'zeta'
    mpmath_name = 'zeta'

class _Bessel(SympyFunction):

    attributes = ('Listable', 'NumericFunction', 'Protected', 'ReadProtected')

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
            except SpecialValueError, exc:
                return Symbol(exc.name)

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
    <dl>
    <dt>'BesselJ[$n$, $z$]'
      <dd>returns the Bessel function of the first kind $J_n(z)$.
    </dl>

    >> BesselJ[0, 5.2]
     = -0.11029043979098654

    #> BesselJ[2.5, 1]
     = 0.0494968102284779423

    ## >> D[BesselJ[n, z], z]
    ##  = BesselJ[n - 1, z] / 2 - BesselJ[n + 1, z] / 2

    #> BesselJ[0., 0.]
     = 1.

    >> Plot[BesselJ[0, x], {x, 0, 10}]
     = -Graphics-
    """

    #TODO: Sympy Backend is not as powerful as Mathmeatica
    """
    >> BesselJ[1/2, x]
     = Sqrt[2 / Pi] Sin[x] / Sqrt[x]
    """

    attributes = ('Listable', 'NumericFunction', 'Protected')

    sympy_name = 'besselj'
    mpmath_name = 'besselj'

class BesselY(_Bessel):
    """
    <dl>
    <dt>'BesselY[$n$, $z$]'
      <dd>returns the Bessel function of the second kind $Y_n(z)$.
    </dl>

    >> BesselY[1.5, 4]
     = 0.367112032460934155

    ## Returns ComplexInfinity instead
    ## #> BesselY[0., 0.]
    ##  = -Infinity

    >> Plot[BesselY[0, x], {x, 0, 10}]
     = -Graphics-
    """

    #TODO: Special Values
    """
    >> BesselY[0, 0]
     = -Infinity
    """

    attributes = ('Listable', 'NumericFunction', 'Protected')

    sympy_name = 'bessely'
    mpmath_name = 'bessely'

class BesselI(_Bessel):
    """
    <dl>
    <dt>'BesselI[$n$, $z$]'
      <dd>returns the modified Bessel function of the first kind $I_n(z)$.
    </dl>

    >> BesselI[1.5, 4]
     = 8.17263323168659544

    >> Plot[BesselI[0, x], {x, 0, 5}]
     = -Graphics-
    """

    attributes = ('Listable', 'NumericFunction', 'Protected')

    sympy_name = 'besseli'
    mpmath_name = 'besseli'

class BesselK(_Bessel):
    """
    <dl>
    <dt>'BesselK[$n$, $z$]'
      <dd>returns the modified Bessel function of the second kind $K_n(z)$.
    </dl>

    >> BesselK[1.5, 4]
     = 0.0143470307207600668

    >> Plot[BesselK[0, x], {x, 0, 5}]
     = -Graphics-
    """

    attributes = ('Listable', 'NumericFunction', 'Protected')

    sympy_name = 'besselk'
    mpmath_name = 'besselk'

#TODO: Spherical Bessel Functions

# Hankel Functions

class HankelH1(_Bessel):
    """
    <dl>
    <dt>'HankelH1[$n$, $z$]'
      <dd>returns the Hankel function of the first kind $H_n^1 (z)$.
    </dl>

    >> HankelH1[1.5, 4]
     = 0.185285948354268953 + 0.367112032460934155 I
    """

    sympy_name = 'hankel1'
    mpmath_name = 'hankel1'

class HankelH2(_Bessel):
    """
    <dl>
    <dt>'HankelH2[$n$, $z$]'
      <dd>returns the Hankel function of the second kind $H_n^2 (z)$.
    </dl>

    >> HankelH2[1.5, 4]
     = 0.185285948354268953 - 0.367112032460934155 I
    """

    sympy_name = 'hankel2'
    mpmath_name = 'hankel2'

# Airy Functions

class AiryAi(_MPMathFunction):
    """
    <dl>
    <dt>'AiryAi[$x$]'
      <dd>returns the Airy function $Ai(x)$.
    </dl>

    >> AiryAi[0.5]
     = 0.23169360648083349

    >> AiryAi[0.5 + I]
     = 0.157118446499986172 - 0.241039813840210768 I

    >> Plot[AiryAi[x], {x, -10, 10}]
     = -Graphics-
    """

    sympy_name = ''
    mpmath_name = 'airyai'

class AiryBi(_MPMathFunction):
    """
    <dl>
    <dt>'AiryBi[$x$]'
      <dd>returns the Airy function $Bi(x)$.
    </dl>

    >> AiryBi[0.5]
     = 0.854277043103155493

    >> AiryBi[0.5 + I]
     = 0.688145273113482414 + 0.370815390737010831 I

    >> Plot[AiryBi[x], {x, -10, 2}]
     = -Graphics-
    """

    sympy_name = ''
    mpmath_name = 'airybi'

# Kelvin Functions

class KelvinBer(_Bessel):
    """
    <dl>
    <dt>'KelvinBer[$z$]'
      <dd>returns the Kelvin function $ber(z)$.
    <dt>'KelvinBer[$n$, $z$]'
      <dd>returns the Kelvin function $ber_n(z)$.
    </dl>

    >> KelvinBer[0.5]
     = 0.999023463990838256

    >> KelvinBer[1.5 + I]
     = 1.11620420872233787 - 0.117944469093970067 I

    >> KelvinBer[0.5, 0.25]
     = 0.148824330530639942

    >> Plot[KelvinBer[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        'KelvinBer[z_]': 'KelvinBer[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'ber'

class KelvinBei(_Bessel):
    """
    <dl>
    <dt>'KelvinBei[$z$]'
      <dd>returns the Kelvin function $bei(z)$.
    <dt>'KelvinBei[$n$, $z$]'
      <dd>returns the Kelvin function $bei_n(z)$.
    </dl>

    >> KelvinBei[0.5]
     = 0.0624932183821994586

    >> KelvinBei[1.5 + I]
     = 0.326323348699806294 + 0.75560557861089228 I

    >> KelvinBei[0.5, 0.25]
     = 0.370152900194021013

    >> Plot[KelvinBei[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        'KelvinBei[z_]': 'KelvinBei[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'bei'

class KelvinKer(_Bessel):
    """
    <dl>
    <dt>'KelvinKer[$z$]'
      <dd>returns the Kelvin function $ker(z)$.
    <dt>'KelvinKer[$n$, $z$]'
      <dd>returns the Kelvin function $ker_n(z)$.
    </dl>

    >> KelvinKer[0.5]
     = 0.855905872118634214

    >> KelvinKer[1.5 + I]
     = -0.167162242027385125 - 0.184403720314419905 I

    >> KelvinKer[0.5, 0.25]
     = 0.450022838747182502

    >> Plot[KelvinKer[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        'KelvinKer[z_]': 'KelvinKer[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'ker'

class KelvinKei(_Bessel):
    """
    <dl>
    <dt>'KelvinKei[$z$]'
      <dd>returns the Kelvin function $kei(z)$.
    <dt>'KelvinKei[$n$, $z$]'
      <dd>returns the Kelvin function $kei_n(z)$.
    </dl>

    >> KelvinKei[0.5]
     = -0.671581695094367603

    >> KelvinKei[1.5 + I]
     = -0.248993863536003923 + 0.303326291875385478 I

    >> KelvinKei[0.5, 0.25]
     = -2.05169683896315934

    >> Plot[KelvinKei[x], {x, 0, 10}]
     = -Graphics-
    """

    rules = {
        'KelvinKei[z_]': 'KelvinKei[0, z]',
    }

    sympy_name = ''
    mpmath_name = 'kei'

# Struve and Related Functions

class StruveH(_Bessel):
    """
    <dl>
    <dt>'StruveH[$n$, $z$]'
      <dd>returns the Struve function $H_n(z)$.
    </dl>

    >> StruveH[1.5, 3.5]
     = 1.13192125271801312

    >> Plot[StruveH[0, x], {x, 0, 20}]
     = -Graphics-
    """

    sympy_name = ''
    mpmath_name = 'struveh'

class StruveL(_Bessel):
    """
    <dl>
    <dt>'StruveL[$n$, $z$]'
      <dd>returns the modified Struve function $L_n(z)$.
    </dl>

    >> StruveL[1.5, 3.5]
     = 4.41126360920433996

    >> Plot[StruveL[0, x], {x, 0, 5}]
     = -Graphics-
    """

    sympy_name = ''
    mpmath_name = 'struvel'

class AngerJ(_Bessel):
    """
    <dl>
    <dt>'AngerJ[$n$, $z$]'
      <dd>returns the Anger function $J_n(z)$.
    </dl>

    >> AngerJ[1.5, 3.5]
     = 0.294478574459563408

    >> Plot[AngerJ[1, x], {x, -10, 10}]
     = -Graphics-
    """

    #TODO: Associated Anger function AngerJ[v, u, z]

    sympy_name = ''
    mpmath_name = 'angerj'

class WeberE(_Bessel):
    """
    <dl>
    <dt>'WeberE[$n$, $z$]'
      <dd>returns the Weber function $E_n(z)$.
    </dl>

    >> WeberE[1.5, 3.5]
     = -0.397256259210030809

    >> Plot[WeberE[1, x], {x, -10, 10}]
     = -Graphics-
    """

    #TODO: Associated Weber function WeberE[v, u, z]

    sympy_name = ''
    mpmath_name = 'webere'

# Function Zeros

class BesselJZero(_Bessel):
    """
    <dl>
    <dt>'BesselJZero[$n$, $k$]'
      <dd>returns the $k$th zero of the Bessel function of the first kind $J_n(z)$.
    </dl>

    >> N[BesselJZero[0, 1]]
     = 2.40482555769577277
    """

    sympy_name = ''
    mpmath_name = 'besseljzero'
    
class BesselYZero(_Bessel):
    """
    <dl>
    <dt>'BesselJZero[$n$, $k$]'
      <dd>returns the $k$th zero of the Bessel function of the second kind $Y_n(z)$.
    </dl>

    >> N[BesselYZero[0, 1]]
     = 0.893576966279167522
    """

    sympy_name = ''
    mpmath_name = 'besselyzero'

class AiryAiZero(_MPMathFunction):
    """
    <dl>
    <dt>'AiryAiZero[$k$]'
      <dd>returns the $k$th zero of the Airy function $Ai(z)$.
    </dl>

    >> N[AiryAiZero[1]]
     = -2.33810741045976704
    """

    #TODO: 'AiryAiZero[$k$, $x0$]' - $k$th zero less than x0 

    attributes = ('Listable', 'NHoldFirst', 'NumericFunction', 'Protected', 'ReadProtected')

    sympy_name = ''
    mpmath_name = 'airyaizero'

class AiryBiZero(_MPMathFunction):
    """
    <dl>
    <dt>'AiryBiZero[$k$]'
      <dd>returns the $k$th zero of the Airy function $Bi(z)$.
    </dl>

    >> N[AiryBiZero[1]]
     = -1.17371322270912792
    """

    #TODO: 'AiryBiZero[$k$, $x0$]' - $k$th zero less than x0 

    attributes = ('Listable', 'NHoldFirst', 'NumericFunction', 'Protected', 'ReadProtected')

    sympy_name = ''
    mpmath_name = 'airybizero'

class Legendre(_MPMathFunction):
    def eval(self, z):
        return mpmath.legendre(1, z)
    
    def prepare_sympy(self, leaves):
        return [Integer(1)] + leaves
