import sympy
import sympy.stats
from mathics.builtin.base import SympyFunction, Builtin
from mathics.core.expression import Symbol, Complex, Integer, Real
from mathics.core.convert import from_sympy, sympy_symbol_prefix



class DistributionParam(object):
    msg = ''

    @staticmethod
    def convert(value):
        return

    @staticmethod
    def get(value): #gets value for check_too -- todo
        return


class RealParam(DistributionParam):
    msg = 'realprm'

    @staticmethod
    def convert(value):
        if isinstance(value, Complex):
            return None
        else:
            # TODO
            return value.to_sympy()

    @staticmethod
    def get(value): #gets value for check_too -- todo
        return


class PositiveRealParam(DistributionParam):
    msg = 'posprm'

    @staticmethod
    def convert(value):
        if isinstance(value, Symbol):
            return sympy.Symbol(sympy_symbol_prefix + value.name, positive=True)
        elif isinstance(value, Complex):
            return None
        else:
            # TODO
            return value.to_sympy()

    @staticmethod
    def get(value): #gets value for check_too -- todo
        return


class IntegerParam(DistributionParam):
    msg = 'intprm'

    @staticmethod
    def convert(value):
        if isinstance(value, Symbol):
            # TODO
            return value.to_sympy()
        elif isinstance(value, Integer):
            return value.to_sympy()
        else:
            return None

    @staticmethod
    def get(value): #gets value for check_too -- todo
        return 


class PositiveIntParam(DistributionParam):
    msg = 'pintprm'

    @staticmethod
    def convert(value):
        if isinstance(value, Integer):
            result = value.to_sympy()
            if result > 0:
                return result
        else: 
            return None

    @staticmethod
    def get(value):
        return


class _SympyDistribution(SympyFunction):
    
    params = ()      # sequence of DistributionParam
    sympy_name = None

    def to_sympy(self, expr, **kwargs):
        # convert params
        if len(expr.leaves) != len(self.params):
            return #evaluation.message(type(self).__name__, 'argr')
        check, to_be_used = self.check_addition()
        if to_be_used == True:
            if check == True:
                
        params = [param.convert(leaf) for param, leaf in zip(self.params, expr.leaves)]
        if None in params:
            return

        # convert distribution
        try:
            return getattr(sympy.stats, self.sympy_name)('x', *params)
        except ValueError as e:
            return

        
    def check_additional(self): #todo
        try:
            val = self.check_too()
            return val, True
        except AttributeError:
            return True, False
    

class CDF(Builtin):
    '''
    <dl>
    <dt>'CDF[$dist$, $x$]'
      <dd>returns the cumulative distribution for the distribution $dist$ at $x$.
    <dt>'CDF[$dist$, {$x_1$, $x_2$, ...}]'
      <dd>returns the CDF evaluated at '{$x_1$, $x_2$, ...}'.
    <dt>'CDF[$dist$]'
      <dd>returns the CDF as a pure function.
    </dl>
    >> CDF[NormalDistribution[mu, sigma], x]
     = 1 / 2 + Erf[Sqrt[2] (-mu + x) / (2 sigma)] / 2
    >> CDF[NormalDistribution[mu, sigma], {x, y}]
     = {1 / 2 + Erf[Sqrt[2] (-mu + x) / (2 sigma)] / 2, 1 / 2 + Erf[Sqrt[2] (-mu + y) / (2 sigma)] / 2}
    >> CDF[NormalDistribution[mu, sigma]]
     = 1 / 2 + Erf[Sqrt[2] (-mu + #1) / (2 sigma)] / 2&
    '''

    rules = {
        'CDF[dist_, x_]': 'CDF[dist][x]',
        'CDF[dist_, xs_List]': 'CDF[dist] /@ xs',
    }

    def apply(self, dist, evaluation):
        'CDF[dist_]'
        dist = dist.to_sympy()
        try:
            result = sympy.stats.cdf(dist)
        except ValueError:
            return
        return from_sympy(result.simplify())


class PDF(Builtin):
    '''
    <dl>
    <dt>'PDF[$dist$, $x$]'
      <dd>returns the probability density function for the distribution $dist$ at $x$.
    <dt>'PDF[$dist$, {$x_1$, $x_2$, ...}]'
      <dd>returns the PDF evaluated at '{$x_1$, $x_2$, ...}'.
    <dt>'PDF[$dist$]'
      <dd>returns the PDF as a pure function.
    </dl>
    >> PDF[NormalDistribution[0, 1], x]
     = Sqrt[2] E ^ (-x ^ 2 / 2) / (2 Sqrt[Pi])
    >> PDF[NormalDistribution[0, 1]]
     = Sqrt[2] Exp[-#1 ^ 2 / 2] / (2 Sqrt[Pi])&
    >> PDF[NormalDistribution[0, 1], {x, y}]
     = {Sqrt[2] E ^ (-x ^ 2 / 2) / (2 Sqrt[Pi]), Sqrt[2] E ^ (-y ^ 2 / 2) / (2 Sqrt[Pi])}
    '''

    rules = {
        'PDF[dist_, x_]': 'PDF[dist][x]',
        'PDF[dist_, xs_List]': 'PDF[dist] /@ xs',
    }

    def apply(self, dist, evaluation):
        'PDF[dist_]'
        dist = dist.to_sympy()
        dummy_arg = sympy.Symbol('PDFDummyArg')
        try:
            result = sympy.stats.density(dist)
            result = sympy.Lambda(dummy_arg, result.pdf(dummy_arg))
        except ValueError:
            return
        return from_sympy(result.simplify()) #has problems with BetaDistribution


class InverseCDF(Builtin):
    '''
    <dl>
    <dt>'InverseCDF[$dist$, $q$]'
      <dd>returns the inverse cumulative distribution for the distribution $dist$ as a function of $q$.
    <dt>'InverseCDF[$dist$, {$x_1$, $x_2$, ...}]'
      <dd>returns the inverse CDF evaluated at '{$x_1$, $x_2$, ...}'.
    <dt>'InverseCDF[$dist$]'
      <dd>returns the inverse CDF as a pure function.
    </dl>
    >> InverseCDF[NormalDistribution[0, 1], x]
     = Sqrt[2] InverseErfc[2 - 2 x]
    >> InverseCDF[NormalDistribution[0, 1], {x, y}]
     = {Sqrt[2] InverseErfc[2 - 2 x], Sqrt[2] InverseErfc[2 - 2 y]}
    >> InverseCDF[NormalDistribution[0, 1]]
     = Sqrt[2] InverseErfc[2 - 2 #1]&
    '''

    rules = {
        'InverseCDF[dist_, x_]': 'InverseCDF[dist][x]',
        'InverseCDF[dist_, xs_List]': 'InverseCDF[dist] /@ xs',
    }

    def apply(self, dist, evaluation):
        'InverseCDF[dist_, x]' #does not support operator form!
        dist = dist.to_sympy()
        try:
            result = sympy.stats.density(dist)
            result = result._inverse_cdf_expression()
        except ValueError:
            return
        return from_sympy(result.simplify())

    #InverseCDF[NormalDistribution[], x] returns direct expression, while
    #it should return a ConditionalExpression[..., etc]


class RandomVariate(Builtin): #to check
    # TODO:
    # RandomVariate[dist, {n1, n2, ... }] -> array of n1 x n2 ...

    # parts taken from bnjones's branch
    rules = {
        'RandomVariate[dist_]': 'RandomVariate[dist, 1]'
    }

    def apply(self, dist, samples, evaluation):
        'RandomVariate[dist_, samples_Integer]'

        dist = dist.to_sympy()
        samples = samples.to_sympy()
        if not isinstance(dist, Distribution):
            return
        elif samples == 1:
            return from_sympy(stats.sample(dist))
        elif samples > 1:
            samples_out = stats.sample_iter(dist, numsamples=samples)
            return Expression('List', *list(samples_out))


class SurvivalFunction(Builtin):
    '''
    <dl>
    <dt>'SurvivalFunction[$dist$, $x$]'
      <dd>returns the survival function for the distribution $dist$ evaluated at $x$.
    <dt>'SurvivalFunction[$dist$, {$x_1$, $x_2$, ...}]'
      <dd>returns the survival function at '{$x_1$, $x_2$, ...}'.
    <dt>'SurvivalFunction[$dist$]'
      <dd>returns the survival function as a pure function.
    </dl>
    >> SurvivalFunction[NormalDistribution[0, 1], x]
     = Erfc[Sqrt[2] x / 2] / 2
    '''

    rules = {
        'SurvivalFunction[dist_]': '1 - CDF[dist]',
        'SurvivalFunction[dist_, x_]': '1 - CDF[dist, x]',
        'SurvivalFunction[dist_, xs_List]': '1 - CDF[dist, xs]',
    }


    
class Distributed(Builtin): #todo, not ready

    valid = False
    dist = None
    def apply(self, var, dist, evaluation):
        'Distributed[var, dist]'

        if isinstance(dist, Distribution):
            valid = True
            dist = dist.to_sympy()

'''
class Expectation(Builtin): #todo


    def apply(self, dist, distributed, evaluation):
        'Expectation[expr_, distributed_]'

        if isinstance(distributed, Distributed):
            if distributed.valid:
                dist = distributed.dist
                try:
                    result = sympy.stats.E(dist)
            
        dist = dist.to_sympy()
        try:
            result = 



class Probability(Builtin): #todo

'''

class NormalDistribution(_SympyDistribution):
    '''
    <dl>
    <dt>'NormalDistribution[$mu$, $sigma$]'
      <dd>represents the normal distribution with mean $mu$ and standard deviation $sigma$.
    <dt>'NormalDistribution[]'
      <dd>represents the normal distribution with mean zero and standard deviation one.
    </dl>
    >> CDF[NormalDistribution[mu, sigma]]
     = 1 / 2 + Erf[Sqrt[2] (-mu + #1) / (2 sigma)] / 2&
    >> PDF[NormalDistribution[0, 1]]
     = Sqrt[2] Exp[-#1 ^ 2 / 2] / (2 Sqrt[Pi])&
    >> Mean[NormalDistribution[mu, sigma]]
     = mu
    >> Variance[NormalDistribution[mu, sigma]]
     = sigma ^ 2
    >> Plot[PDF[NormalDistribution[], x], {x, -6, 6}, PlotRange->All]
     = -Graphics-
    #> CDF[NormalDistribution[0, 1]]
     = 1 - Erfc[Sqrt[2] #1 / 2] / 2&
    '''

    params = (RealParam, PositiveRealParam)
    sympy_name = 'Normal'

    rules = {
        'NormalDistribution[]': 'NormalDistribution[0, 1]',
    }

    def check_too(self):
        sigma = self.params[0].get() #if it is number, not a symbol
        if sigma**2 > 0:
            return True
        else:
            return False


class BetaDistribution(_SympyDistribution):
    
    params = (PositiveRealParam, PositiveRealParam)
    sympy_name = 'Beta'

    
class BernoulliDistribution(_SympyDistribution):
    
    params = (RealParam,) #between 0 and 1
    sympy_name = 'Bernoulli'

    def check_too(self):
        par = self.params[0].get()
        if par < 1 and par > 0:
            return True
        else:
            return False


class CauchyDistribution(_SympyDistribution):
    
    params = (RealParam, PositiveRealParam)
    sympy_name = 'Cauchy'


class ChiDistribution(_SympyDistribution):

    params = (PositiveIntParam)
    sympy_name = 'Chi'


class ChiSquareDistribution(_SympyDistribution):
    
    params = (PositiveIntParam)
    sympy_name = 'ChiSquared'


class DiscreteUniformDistribution(_SympyDistribution):

    params = (IntegerParam, IntegerParam)
    sympy_name = 'DiscreteUniform'

    def check_too(self): #the first param must be smaller than the second
        par1 = self.params[0].get()
        par2 = self.params[1].get()
        if par1 < par2:
            return True
        else:
            return False
    

class ExponentialDistribution(_SympyDistribution):
    
    params = (PositiveRealParam)
    sympy_name = 'Exponential'


class FRatioDistribution(_SympyDistribution):

    params = (PositiveRealParam, PositiveRealParam)
    sympy_name = 'FDistribution'


class GammaDistribution(_SympyDistribution):

    params = (PositiveRealParam, PositiveRealParam)
    sympy_name = 'Gamma'


class LaplaceDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = 'Laplace'

    rules = {
        'LaplaceDistribution[]': 'LaplaceDistribution[0, 1]'
    }


class LogisticDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = 'Logistic'

    rules = {
        'LogisticDistribution[]': 'LogisticDistribution[0, 1]'
    }


class LogNormalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam) #for second (sigma) holds sigma^2>0
    sympy_name = 'LogNormal'

    def check_too(self):
        sigma = self.params[1].get()
        if sigma**2 > 0:
            return True
        else:
            return False


class MaxwellDistribution(_SympyDistribution):

    params = (PositiveRealParam)
    sympy_name = 'Maxwell'



class ParetoDistribution(_SympyDistribution):

    params = (PositiveRealParam, PositiveRealParam)
    sympy_name = 'Pareto'


class PoissonDistribution(_SympyDistribution):

    params = (PositiveRealParam)
    sympy_name = 'Poisson'


class RayleighDistribution(_SympyDistribution):

    params = (PositiveRealParam)
    sympy_name = 'Rayleigh'


class TriangularDistribution(_SympyDistribution):

    params = (RealParam, RealParam, RealParam) #first < second, first<=third<=secnd
    sympy_name = 'Triangular'
                                    #HAS LIST AS PARAMETER! 
    rules = {
        'TriangularDistribution[]': 'TriangularDistribution[{0, 1}, 1/2]',
        'TriangularDistribution[{min, max}]': 'TriangularDistribution[{min, max}, (max+min)/2]'
    }


class UniformDistribution(_SympyDistribution):

    params = (RealParam, RealParam) #-infinity<first, first<b<infinity
    sympy_name = 'Uniform'

    rules = {                       #HAS LIST AS PARAMETER! 
        'UniformDistribution[]': 'UniformDistribution[{0, 1}]'
    }


class WeibullDistribution(_SympyDistribution):

    params = (PositiveRealParam, PositiveRealParam)
    sympy_name = 'Weibull'


class ArcSinDistribution(_SympyDistribution):

    params = (RealParam, RealParam) #second greater than first
    sympy_name = 'ArcSin'

    rules = {
        'ArcSinDistribution[]': 'ArcSinDistribution[{0, 1}]'
    }

    def check_too(self):
        par1 = self.params[0].get()
        par2 = self.params[1].get()
        if par1 < par2:
            return True
        else:
            return False


class BeniniDistribution(_SympyDistribution):

    params = (PositiveRealParam, PositiveRealParam, PositiveRealParam)
    sympy_name = 'Benini'


class InverseGammaDistribution(_SympyDistribution):

    params = (PositiveRealParam, PositiveRealParam)
    sympy_name = 'GammaInverse'


class KumaraswamyDistribution(_SympyDistribution):

    params = (PositiveRealParam, PositiveRealParam)
    sympy_name = 'Kumaraswamy'


class NakagamiDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam) #for first (mu) holds mu >= 1/2
    sympy_name = 'Nakagami'

    def check_too(self):
        par = self.params[0].get()
        if par >= 0.5:     #check...
            return True
        else:
            return False


class StudentTDistribution(_SympyDistribution):
    
    params = (PositiveRealParam)
    sympy_name = 'StudentT'


class VonMisesDistribution(_SympyDistribution):

    params = (RealParam, RealParam)
    sympy_name = 'VonMises'


class WignerSemicircleDistribution(_SympyDistribution):

    params = (PositiveRealParam)
    sympy_name = 'WignerSemicircle'


class FisherZDistribution(_SympyDistribution):

    params = (PositiveRealParam, PositiveRealParam)
    sympy_name = 'FisherZ'


class UniformSumDistribution(_SympyDistribution):

    params = (PositiveIntParam) 
    sympy_name = 'UniformSum'


class BetaPrimeDistribution(_SympyDistribution):

    params = (PositiveRealParam, PositiveRealParam)
    sympy_name = 'BetaPrime'


class DagumDistribution(_SympyDistribution):

    params = (PositiveRealParam, PositiveRealParam, PositiveRealParam)
    sympy_name = 'Dagum'


class ErlangDistribution(_SympyDistribution):

    params = (IntegerParam, PositiveRealParam)
    sympy_name = 'Erlang'


class FrechetDistribution(_SympyDistribution):
    
    params = (PositiveRealParam, PositiveRealParam, RealParam)
    sympy_name = 'Frechet'

    #wrong PDF, must return piecewise((currentPDF, x>thirdparam), (0, true))
    #CDF freezes


class GeometricDistribution(_SympyDistribution):

    params = (RealParam) #must be between 0 and 1
    sympy_name = 'Geometric'

    def check_too(self):
        par = self.params[0].get()
        if par > 0 and par < 1:
            return True
        else:
            return False
        
'''
class ExpGammaDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class DavisDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BinomialDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BirnbaumSaundersDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class ExponentialPowerDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }

class GompertzMakehamDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class HotellingTSquareDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class HoytDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class HyperbolicDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class HypergeometricDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class InverseChiSquareDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class JohnsonDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class LandauDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class LevyDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class LindleyDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class LogGammaDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class LogLogisticDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MarginalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MaxStableDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MeixnerDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MinStableDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MixtureDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MoyalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MultinomialDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MultinormalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MultivariateHypergeometricDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam) 
    sympy_name = ''

    rules = {
        '': ''
    }


class NoncentralBetaDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class NoncentralStudentTDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class PearsonDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class PowerDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class RiceDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class SechDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class SinghMaddalaDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class SuzukiDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class TsallisQExponentialDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class TsallisQGaussianDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class TukeyLambdaDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class UniformGraphDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class WakebyDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BarabasiAlbertGraphDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BenfordDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BinormalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BorelTannerDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class CensoredDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class CircularOrthogonalMatrixDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class CircularQuaternionMatrixDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class CircularRealMatrixDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class CircularSymplecticMatrixDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class CircularUnitaryMatrixDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }
    

class CompoundPoissonDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class CoxianDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class DegreeGraphDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class DirichletDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class EmpiricalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class FirstPassageTimeDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class FisherHypergeometricDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class GaussianOrthogonalMatrixDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class GaussianSymplecticMatrixDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class GaussianUnitaryMatrixDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class GraphPropertyDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class HistogramDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class HyperexponentialDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class HypoexponentialDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class InverseWishartMatrixDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class KDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class LogMultinormalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MatrixNormalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MatrixPropertyDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MatrixTDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MultivariatePoissonDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class MultivariateTDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class NegativeMultinomialDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class OrderDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class ParameterMixtureDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class PascalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class PERTDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class PoissonConsulDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class PolyaAeppliDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class PriceGraphDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class QuantityDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class ReliabilityDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class ShiftedGompertzDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class SkellamDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class SliceDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class SmoothKernelDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class SplicedDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class StableDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class StandbyDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class TracyWidomDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class TransformedDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class TruncatedDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class WalleniusHypergeometricDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class WaringYuleDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class WattsStrogatzGraphDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class WishartMatrixDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class SurvivalDistribution(_SympyDistribution):
    
    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BetaBinomialDistribution(_SympyDistribution):
    
    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BetaNegativeBinomialDistribution(_SympyDistribution):
    
    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }

    
class ExtremeValueDistribution(_SympyDistribution):
    
    params = (PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class GumbelDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class HalfNormalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class InverseGaussianDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class LogSeriesDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class NegativeBinomialDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class NoncentralChiSquareDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class NoncentralFRatioDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }
    

class BatesDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BeckmannDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class SkewNormalDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class ZipfDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }
    
    
class BenktanderGibratDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BenktanderWeibullDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }


class BernoulliGraphDistribution(_SympyDistribution):

    params = (RealParam, PositiveRealParam)
    sympy_name = ''

    rules = {
        '': ''
    }
'''
