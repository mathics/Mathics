#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

import sympy

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Integer, Symbol
from mathics.builtin.arithmetic import _MPMathFunction


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


class Binomial(_MPMathFunction):
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

    nargs = 2
    sympy_name = 'binomial'
    mpmath_name = 'binomial'


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
     = Binomial[a + b, b] Binomial[a + b + c, c]
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
            result.leaves.append(Expression(
                'Binomial', Expression('Plus', *total), value))
        return result


class _BooleanDissimilarity(Builtin):
    @staticmethod
    def _to_bool_vector(u):
        class NoBoolVector(Exception):
            pass

        def generate():
            for leaf in u.leaves:
                if isinstance(leaf, Integer):
                    val = leaf.get_int_value()
                    if val in (0, 1):
                        yield val
                    else:
                        raise NoBoolVector
                elif isinstance(leaf, Symbol):
                    name = leaf.name
                    if name == 'System`True':
                        yield 1
                    elif name == 'System`False':
                        yield 0
                    else:
                        raise NoBoolVector
                else:
                    raise NoBoolVector

        try:
            return [x for x in generate()]
        except NoBoolVector:
            return None

    def apply(self, u, v, evaluation):
        '%(name)s[u_List, v_List]'
        if len(u.leaves) != len(v.leaves):
            return
        py_u = _BooleanDissimilarity._to_bool_vector(u)
        if py_u is None:
            return
        py_v = _BooleanDissimilarity._to_bool_vector(v)
        if py_v is None:
            return
        counts = [0, 0, 0, 0]
        for a, b in zip(py_u, py_v):
            counts[(a << 1) + b] += 1
        return self._compute(len(py_u), *counts)


class MatchingDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'MatchingDissimilarity[$u$, $v$]'
      <dd>returns the Matching dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as (c_tf + c_ft) / n, where n is len($u$) and c_ij is the number of
      occurrences of $u$[k]=i and $v$[k]=j for k<n.
    </dl>

    >> MatchingDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 4 / 7
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        return Expression('Divide', c_tf + c_ft, n)


class JaccardDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'JaccardDissimilarity[$u$, $v$]'
      <dd>returns the Jaccard-Needham dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as (c_tf + c_ft) / (c_tt + c_ft + c_tf), where n is len($u$) and c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n.
    </dl>

    >> JaccardDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 2 / 3
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        return Expression('Divide', c_tf + c_ft, c_tt + c_ft + c_tf)


class DiceDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'DiceDissimilarity[$u$, $v$]'
      <dd>returns the Dice dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as (c_tf + c_ft) / (2 * c_tt + c_ft + c_tf), where n is len($u$) and c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n.
    </dl>

    >> DiceDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 1 / 2
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        return Expression('Divide', c_tf + c_ft, 2 * c_tt + c_ft + c_tf)


class YuleDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'YuleDissimilarity[$u$, $v$]'
      <dd>returns the Yule dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as R / (c_tt * c_ff + R / 2) where n is len($u$), c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n, and R = 2 * c_tf * c_ft.
    </dl>

    >> YuleDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 6 / 5
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        r_half = c_tf * c_ft
        return Expression('Divide', 2 * r_half, c_tt * c_ff + r_half)


class SokalSneathDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'SokalSneathDissimilarity[$u$, $v$]'
      <dd>returns the Sokal-Sneath dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as R / (c_tt + R) where n is len($u$), c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n, and R = 2 * (c_tf + c_ft).
    </dl>

    >> SokalSneathDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 4 / 5
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        r = 2 * (c_tf + c_ft)
        return Expression('Divide', r, c_tt + r)


class RussellRaoDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'RussellRaoDissimilarity[$u$, $v$]'
      <dd>returns the Russell-Rao dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as (n - c_tt) / c_tt where n is len($u$) and c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n.
    </dl>

    >> RussellRaoDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 5 / 7
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        return Expression('Divide', n - c_tt, n)


class RogersTanimotoDissimilarity(_BooleanDissimilarity):
    """
    <dl>
    <dt>'RogersTanimotoDissimilarity[$u$, $v$]'
      <dd>returns the Rogers-Tanimoto dissimilarity between the two boolean 1-D lists $u$ and $v$,
      which is defined as R / (c_tt + c_ff + R) where n is len($u$), c_ij is
      the number of occurrences of $u$[k]=i and $v$[k]=j for k<n, and R = 2 * (c_tf + c_ft).
    </dl>

    >> RogersTanimotoDissimilarity[{1, 0, 1, 1, 0, 1, 1}, {0, 1, 1, 0, 0, 0, 1}]
     = 8 / 11
    """

    def _compute(self, n, c_ff, c_ft, c_tf, c_tt):
        r = 2 * (c_tf + c_ft)
        return Expression('Divide', r, c_tt + c_ff + r)


distribsAssumptions = {'BernoulliDistribution[p]':'0 <= p <= 1',
                       'BetaBinomialDistribution[alpha, beta, n]':'n \[Element] Integers && n > 0 && alpha > 0 && beta > 0',
                       'BetaDistribution[alpha, beta]':'alpha > 0 && beta > 0',
                       'BetaNegativeBinomialDistribution[alpha, beta, n]':'alpha > 0 && beta > 0 && n > 0',
                       'CauchyDistribution[a, b]':'a \[Element] Reals && b > 0',
                       'ChiDistribution[nu]':'nu > 0',
                       'ChiSquareDistribution[nu]':'nu > 0',
                       'DiscreteUniformDistribution[{imin, imax}]':'imin \[Element] Integers && imax \[Element] Integers && imax >= imin',
                       'ExponentialDistribution[lambda]':'lambda > 0',
                       'ExtremeValueDistribution[alpha, beta]':'alpha \[Element] Reals && beta > 0',
                       'FRatioDistribution[n, m]':'n > 0 && m > 0',
                       'GammaDistribution[alpha, beta]':'alpha > 0 && beta > 0',
                       'GammaDistribution[alpha, beta, gamma, nu]':'alpha > 0 && beta > 0 && gamma > 0 && nu \[Element] Reals}',
                       'GumbelDistribution[alpha, beta]':'alpha \[Element] Reals && beta > 0',
                       'HalfNormalDistribution[theta]':'theta > 0',
                       'InverseGaussianDistribution[mu, lambda]':'mu > 0 && lambda > 0',
                       'InverseGaussianDistribution[mu, lambda, theta]':'mu > 0 && lambda > 0 && theta \[Element] Reals',
                       'LaplaceDistribution[mu, beta]':'mu \[Element] Reals && beta > 0',
                       'LogisticDistribution[mu, beta]':'mu \[Element] Reals && beta > 0',
                       'LogNormalDistribution[mu, sigma]':'mu \[Element] Reals && sigma > 0',
                       'LogSeriesDistribution[theta]':'theta > 0 && theta < 1',
                       'MaxwellDistribution[sigma]':'sigma > 0',
                       'NegativeBinomialDistribution[n, p]':'n >= 0 && 0 < p <= 1',
                       'NoncentralChiSquareDistribution[nu, lambda]':'nu > 0 && lambda >= 0',
                       'NoncentralFRatioDistribution[n, m, lambda]':'n > 0 && m > 0 && lambda >= 0',
                       'NoncentralFRatioDistribution[n, m, lambda, eta]':'n > 0 && m > 0 && lambda >= 0 && eta >= 0',
                       'NormalDistribution[mu, sigma]':'mu \[Element] Reals && sigma > 0',
                       'NormalDistribution[0, 1]': 'True',
                       'ParetoDistribution[k, alpha]':'k > 0 && alpha > 0',
                       'ParetoDistribution[k, alpha, mu]':'k > 0 && alpha > 0 && mu \[Element] Reals',
                       'ParetoDistribution[k, alpha, gamma, mu]':'k > 0 && alpha > 0 && gamma > 0 && mu \[Element] Reals',
                       'PoissonDistribution[mu]':'mu \[Element] Reals && mu > 0',
                       'RayleighDistribution[sigma]':'sigma > 0',
                       'SkewNormalDistribution[mu, sigma, alpha]':'mu \[Element] Reals && sigma > 0 && alpha \[Element] Reals',
                       'TriangularDistribution[{min, max}]':'min \[Element] Reals && max \[Element] Reals && max > min',
                       'TriangularDistribution[{min, max}, c]':'min \[Element] Reals && max \[Element] Reals && max >= c && min <= c && max > min',
                       'UniformDistribution[{min, max}]':'min \[Element] Reals && max \[Element] Reals && min < max',
                       'UniformDistribution[{0, 1}]':'True','WeibullDistribution[alpha, beta]':'alpha > 0 && beta > 0',
                       'WeibullDistribution[alpha, beta, mu]':'alpha > 0 && beta > 0 && mu \[Element] Reals',
                       'ZipfDistribution[rho]':'rho > 0','ZipfDistribution[n, rho]':'rho > 0 && n \[Element] Integers && n > 0}',
                       'ArcSinDistribution[{xmin, xmax}]':'xmin \[Element] Reals && xmax \[Element] Reals && xmax > xmin',
                       'BatesDistribution[n]':'n \[Element] Integers && n > 0',
                       'BatesDistribution[n, {min, max}]':'n \[Element] Integers && n > 0 && min \[Element] Reals && max \[Element] Reals && min < max',
                       'BeniniDistribution[alpha, beta, sigma]':'alpha >= 0 && beta >= 0 && sigma > 0 && alpha > -beta',
                       'BenktanderGibratDistribution[a, b]':'a > 0 && b > 0 && b <= 1/2 a (1 + a)',
                       'BenktanderWeibullDistribution[a, b]':'a > 0 && b > 0 && b <= 1',
                       'BetaPrimeDistribution[p, q]':'p > 0 && q > 0',
                       'BetaPrimeDistribution[p, q, beta]':'p > 0 && q > 0 && beta > 0',
                       'BetaPrimeDistribution[p, q, alpha, beta]':'p > 0 && q > 0 && alpha > 0 && beta > 0',
                       'BinomialDistribution[n, p]':'n \[Element] Integers && n >= 0 && 0 <= p <= 1',
                       'BirnbaumSaundersDistribution[alpha, lambda]':'alpha > 0 && lambda > 0',
                       'DagumDistribution[p, a, b]':'p > 0 && a > 0 && b > 0',
                       'DavisDistribution[b, n, mu]':'b > 0 && mu >= 0 && n > 1',
                       'ErlangDistribution[k, lambda]':'k \[Element] Integers && k > 0 && lambda > 0',
                       'ExpGammaDistribution[k, theta, mu]':'k > 0 && theta > 0 && mu \[Element] Reals',
                       'ExponentialPowerDistribution[k, mu, sigma]':'k > 0 && mu \[Element] Reals && sigma > 0',
                       'FrechetDistribution[alpha, beta]':'alpha > 0 && beta > 0',
                       'FrechetDistribution[alpha, beta, mu]':'alpha > 0 && beta > 0 && mu \[Element] Reals',
                       'GeometricDistribution[p]':'0 < p <= 1',
                       'GompertzMakehamDistribution[lambda, zeta]':'lambda > 0 && zeta > 0',
                       'HotellingTSquareDistribution[p, m]':'p > 0 && m > -1 + p && m > 0',
                       'HoytDistribution[q, omega]':'0 < q <= 1 && omega > 0',
                       'HyperbolicDistribution[alpha, beta, delta, mu]':'alpha > 0 && beta \[Element] Reals && -alpha < beta < alpha && delta > 0 && mu \[Element] Reals}',
                       'HyperbolicDistribution[lambda, alpha, beta, delta, mu]':'lambda \[Element] Reals && alpha > 0 && beta \[Element] Reals && -alpha < beta < alpha && delta > 0 && mu \[Element] Reals',
                       'HypergeometricDistribution[n, nsucc, ntot]':'n \[Element] Integers && n >= 0 && nsucc \[Element] Integers && nsucc >= 0 && ntot \[Element] Integers && ntot > 0 && ntot >= n && ntot >= nsucc',
                       'InverseChiSquareDistribution[nu]':'nu > 0',
                       'InverseChiSquareDistribution[nu, zeta]':'nu > 0 && zeta > 0',
                       'InverseGammaDistribution[alpha, beta]':'alpha > 0 && beta > 0',
                       'InverseGammaDistribution[alpha, beta, gamma, mu]':'alpha > 0 && beta > 0 && gamma > 0 && mu \[Element] Reals',
                       'KumaraswamyDistribution[alpha, beta]':'alpha > 0 && beta > 0',
                       'LandauDistribution[mu, sigma]':'mu \[Element] Reals && sigma > 0',
                       'LevyDistribution[mu, sigma]':'mu \[Element] Reals && sigma > 0',
                       'LindleyDistribution[delta]':'delta > 0',
                       'LogGammaDistribution[alpha, beta, mu]':'alpha > 0 && beta > 0 && mu >= 0',
                       'LogLogisticDistribution[gamma, sigma]':'gamma > 0 && sigma > 0',
                       'MaxStableDistribution[mu, sigma, zeta]':'mu \[Element] Reals && sigma > 0 && zeta \[Element] Reals',
                       'MeixnerDistribution[a, b, m, d]':'a > 0 && d > 0 && b \[Element] Reals && -\[Pi] < b < \[Pi] && m \[Element] Reals',
                       'MinStableDistribution[mu, sigma, zeta]':'mu \[Element] Reals && sigma > 0 && zeta \[Element] Reals',
                       'MoyalDistribution[mu, sigma]':'mu \[Element] Reals && sigma > 0',
                       'NakagamiDistribution[mu, omega]':'mu > 0 && omega > 0',
                       'NoncentralBetaDistribution[alpha, beta, delta]':'alpha > 0 && beta > 0 && delta > 0',
                       'NoncentralStudentTDistribution[nu, delta]':'nu > 0 && delta \[Element] Reals',
                       'PearsonDistribution[a1, a0, b2, b1, b0]': 'a1 \[Element] Reals && a0 \[Element] Reals && b2 \[Element] Reals && b1 \[Element] Reals && b0 \[Element] Reals && ((b2^2 > 0 && b1^2 - 4 b0 b2 < 0 && a1/b2 > 1) || (b1^2 - 4 b0 b2 > 0 && b2^2 > 0 && (a0 + a1 (-Sqrt[b1^2/(4 b2^2) - b0/b2] - b1/(2 b2)))/(2 Sqrt[b1^2/(4 b2^2) - b0/b2] b2) > -1 && -((a0 + a1 (Sqrt[b1^2/(4 b2^2) - b0/b2] - b1/(2 b2)))/(2 Sqrt[b1^2/(4 b2^2) - b0/b2] b2)) > -1) || (b2^2 > 0 && b1^2 - 4 b0 b2 > 0 && -1 + a1/b2 > 0 && 2 Sqrt[b1^2/(4 b2^2) - b0/b2] - (a0 + a1 (Sqrt[b1^2/(4 b2^2) - b0/b2] - b1/(2 b2)))/b2 > 0) || (b2 == 0 && ((b1^2 > 0 && a1 b1 > 0 && a1 b0 - a0 b1 > -b1^2) || (b1 == 0 && b0^2 > 0 && a1 b0 > 0))) || (b2^2 > 0 && b1^2 == 4 b0 b2 && a1 b1 > 2 a0 b2 && a1/b2 > 1) || (b1^2 - 4 b0 b2 > 0 && b2^2 > 0 && 2 a0 b2 == a1 b1 && (a0 + a1 (-Sqrt[b1^2/(4 b2^2) - b0/b2] - b1/(2 b2)))/(2 Sqrt[b1^2/(4 b2^2) - b0/b2] b2) > -1) || (b2^2 > 0 && b1^2 - 4 b0 b2 < 0 && a1/b2 > 1 && a1 b1 == 2 a0 b2))',
                       'PowerDistribution[k, a]':'k > 0 && a > 0',
                       'RiceDistribution[alpha, beta]':'alpha >= 0 && beta > 0',
                       'RiceDistribution[m, alpha, beta]':'m > 0 && alpha >= 0 && beta > 0',
                       'SechDistribution[mu, sigma]':'mu \[Element] Reals && sigma > 0',
                       'SinghMaddalaDistribution[q, a, b]':'q > 0 && a > 0 && b > 0',
                       'StudentTDistribution[nu]':'nu > 0',
                       'StudentTDistribution[mu, sigma, nu]':'mu \[Element] Reals && sigma > 0 && nu > 0',
                       'SuzukiDistribution[mu, nu]':'mu \[Element] Reals && nu > 0',
                       'TsallisQExponentialDistribution[lambda, q]':'lambda > 0 && q < 2',
                       'TsallisQGaussianDistribution[mu, beta, q]':'mu \[Element] Reals && beta > 0 && q < 3',
                       'TukeyLambdaDistribution[lambda]':'lambda \[Element] Reals',
                       'TukeyLambdaDistribution[lambda, mu, sigma]':'lambda \[Element] Reals && mu \[Element] Reals && sigma > 0',
                       'TukeyLambdaDistribution[{lambda1, lambda2}, mu, {sigma1, sigma2}]':'lambda1 \[Element] Reals && lambda2 \[Element] Reals && mu \[Element] Reals && sigma1 > 0 && sigma2 > 0',
                       'VonMisesDistribution[mu, k]':'mu \[Element] Reals && k >= 0}',
                       'WakebyDistribution[alpha, beta, gamma, delta, mu]':'alpha > 0 && beta > 0 && gamma > 0 && delta > 0 && mu \[Element] Reals',
                       'WignerSemicircleDistribution[r]':'r > 0',
                       'WignerSemicircleDistribution[a, r]':'a \[Element] Reals && r > 0',
                       'BenfordDistribution[b]':'b \[Element] Integers && b >= 2',
                       'BinormalDistribution[{mu1, mu2}, {sigma1, sigma2}, rho]':'(mu1 | mu2) \[Element] Reals && sigma1 > 0 && sigma2 > 0 && -1 < rho < 1',
                       'BinormalDistribution[{sigma1, sigma2}, rho]':'sigma1 > 0 && sigma2 > 0 && -1 < rho < 1',
                       'BinormalDistribution[rho]':'-1 < rho < 1',
                       'BorelTannerDistribution[alpha, n]':'alpha > 0 && alpha < 1 && n \[Element] Integers && n > 0',
                       'FisherHypergeometricDistribution[n, nsucc, ntot, w]':'n \[Element] Integers && n > 0 && nsucc \[Element] Integers && nsucc > 0 && ntot \[Element] Integers && ntot > 0 && w > 0 && n <= ntot && nsucc <= ntot',
                       'FisherZDistribution[n, m]':'n > 0 && m > 0',
                       'KDistribution[nu, w]':'nu > 0 && w > 0',
                       'PascalDistribution[n, p]':'n \[Element] Integers && n > 0 && 0 <= p <= 1',
                       'PERTDistribution[{min, max}, c]':'c \[Element] Reals && max > c && min < c && max > min',
                       'PERTDistribution[{min, max}, c, lambda]':'c \[Element] Reals && lambda \[Element] Reals && max > c && min < c && max > min && lambda >= 0',
                       'PoissonConsulDistribution[mu, lambda]':'mu > 0 && 0 <= lambda < 1',
                       'PolyaAeppliDistribution[theta, p]':'theta > 0 && 0 < p < 1',
                       'SkellamDistribution[mu1, mu2]':'mu1 > 0 && mu2 > 0',
                       'UniformSumDistribution[n, {0, 1}]':'n \[Element] Integers && n > 0',
                       'UniformSumDistribution[n, {min, max}]':'n \[Element] Integers && n > 0 && min \[Element] Reals && max \[Element] Reals && min < max',
                       'WaringYuleDistribution[alpha]':'alpha > 0',
                       'WaringYuleDistribution[alpha, beta]':'alpha > 0 && beta > 0'}

distribs = ['BinomialDistribution[n, p]',
            'PoissonDistribution[mu]',
            'SkellamDistribution[mu1, mu2]',
            'FRatioDistribution[n, m]',
            'MinStableDistribution[mu, sigma, zeta]',
            'BernoulliDistribution[p]',
            'ParetoDistribution[k, alpha, mu]',
            'BetaNegativeBinomialDistribution[alpha, beta, n]',
            'BenktanderWeibullDistribution[a, b]',
            'LogLogisticDistribution[gamma, sigma]',
            'HotellingTSquareDistribution[p, m]',
            'UniformSumDistribution[n, {min, max}]',
            'BorelTannerDistribution[alpha, n]',
            'LogGammaDistribution[alpha, beta, mu]',
            'LogNormalDistribution[mu, sigma]',
            'BatesDistribution[n]',
            'HoytDistribution[q, omega]',
            'MaxwellDistribution[sigma]',
            'NoncentralFRatioDistribution[n, m, lambda]',
            'WaringYuleDistribution[alpha]',
            'ZipfDistribution[rho]',
            'FisherHypergeometricDistribution[n, nsucc, ntot, w]',
            'TukeyLambdaDistribution[{lambda1, lambda2}, mu, {sigma1, sigma2}]',
            'BetaPrimeDistribution[p, q, beta]',
            'FrechetDistribution[alpha, beta, mu]',
            'UniformSumDistribution[n, {0, 1}]',
            'PowerDistribution[k, a]',
            'NoncentralFRatioDistribution[n, m, lambda, eta]',
            'SuzukiDistribution[mu, nu]',
            'NormalDistribution[0, 1]',
            'VonMisesDistribution[mu, k]',
            'MaxStableDistribution[mu, sigma, zeta]',
            'InverseGammaDistribution[alpha, beta]',
            'BetaPrimeDistribution[p, q]',
            'FisherZDistribution[n, m]',
            'InverseGaussianDistribution[mu, lambda, theta]',
            'FrechetDistribution[alpha, beta]',
            'WignerSemicircleDistribution[a, r]',
            'DavisDistribution[b, n, mu]',
            'MoyalDistribution[mu, sigma]',
            'PearsonDistribution[a1, a0, b2, b1, b0]',
            'TsallisQGaussianDistribution[mu, beta, q]',
            'LaplaceDistribution[mu, beta]',
            'KumaraswamyDistribution[alpha, beta]',
            'ArcSinDistribution[{xmin, xmax}]',
            'TsallisQExponentialDistribution[lambda, q]',
            'HalfNormalDistribution[theta]',
            'InverseGammaDistribution[alpha, beta, gamma, mu]',
            'GompertzMakehamDistribution[lambda, zeta]',
            'SechDistribution[mu, sigma]',
            'StudentTDistribution[nu]',
            'HyperbolicDistribution[alpha, beta, delta, mu]',
            'BatesDistribution[n, {min, max}]',
            'BeniniDistribution[alpha, beta, sigma]',
            'ExponentialDistribution[lambda]',
            'WeibullDistribution[alpha, beta]',
            'NoncentralChiSquareDistribution[nu, lambda]',
            'ParetoDistribution[k, alpha, gamma, mu]',
            'GammaDistribution[alpha, beta, gamma, nu]',
            'LandauDistribution[mu, sigma]',
            'PoissonConsulDistribution[mu, lambda]',
            'InverseGaussianDistribution[mu, lambda]',
            'HypergeometricDistribution[n, nsucc, ntot]',
            'SkewNormalDistribution[mu, sigma, alpha]',
            'ParetoDistribution[k, alpha]',
            'NakagamiDistribution[mu, omega]',
            'BenktanderGibratDistribution[a, b]',
            'RiceDistribution[alpha, beta]',
            'NegativeBinomialDistribution[n, p]',
            'LevyDistribution[mu, sigma]',
            'RiceDistribution[m, alpha, beta]',
            'UniformDistribution[{0, 1}]',
            'GammaDistribution[alpha, beta]',
            'NoncentralStudentTDistribution[nu, delta]',
            'BinormalDistribution[rho]',
            'UniformDistribution[{min, max}]',
            'WeibullDistribution[alpha, beta, mu]',
            'HyperbolicDistribution[lambda, alpha, beta, delta, mu]',
            'BetaBinomialDistribution[alpha, beta, n]',
            'BinormalDistribution[{mu1, mu2}, {sigma1, sigma2}, rho]',
            'BirnbaumSaundersDistribution[alpha, lambda]',
            'BetaDistribution[alpha, beta]',
            'StudentTDistribution[mu, sigma, nu]',
            'PERTDistribution[{min, max}, c, lambda]',
            'DiscreteUniformDistribution[{imin, imax}]',
            'ChiDistribution[nu]',
            'PolyaAeppliDistribution[theta, p]',
            'PascalDistribution[n, p]',
            'ChiSquareDistribution[nu]',
            'ExtremeValueDistribution[alpha, beta]',
            'GumbelDistribution[alpha, beta]',
            'ErlangDistribution[k, lambda]',
            'WignerSemicircleDistribution[r]',
            'InverseChiSquareDistribution[nu, zeta]',
            'MeixnerDistribution[a, b, m, d]',
            'LogisticDistribution[mu, beta]',
            'TriangularDistribution[{min, max}]',
            'PERTDistribution[{min, max}, c]',
            'BenfordDistribution[b]',
            'ExponentialPowerDistribution[k, mu, sigma]',
            'LindleyDistribution[delta]',
            'DagumDistribution[p, a, b]',
            'ExpGammaDistribution[k, theta, mu]',
            'TriangularDistribution[{min, max}, c]',
            'CauchyDistribution[a, b]',
            'BinormalDistribution[{sigma1, sigma2}, rho]',
            'LogSeriesDistribution[theta]',
            'ZipfDistribution[n, rho]',
            'KDistribution[nu, w]',
            'BetaPrimeDistribution[p, q, alpha, beta]',
            'TukeyLambdaDistribution[lambda]',
            'SinghMaddalaDistribution[q, a, b]',
            'RayleighDistribution[sigma]',
            'InverseChiSquareDistribution[nu]',
            'WaringYuleDistribution[alpha, beta]',
            'TukeyLambdaDistribution[lambda, mu, sigma]',
            'GeometricDistribution[p]',
            'NoncentralBetaDistribution[alpha, beta, delta]',
            'NormalDistribution[mu, sigma]',
            'WakebyDistribution[alpha, beta, gamma, delta, mu]']


def Quantile(Builtin):
    """
    <dl>
    <dt>'Quantile[$list$, $q$]'
        <dd>gives the qth quantile of list'.      
    </dl>

    >> Quantile[{1, 2, 3, 4, 5, 6, 7}, 1/2]
     = 4
    """
    
    ##TODO: implement other patterns
    
    rules = {'Quantile[list_, q_]':'Sort[list, Less][[Ceiling[q*Length[list]]]]'}
    


class Distribution(Builtin):
    ##todo
        
    def cdf_sym(self, symbol):



    def expectation(self):



    def probability(self, conditions):



class DistributionParameterQ(Builtin):

    def apply(self, expr, evaluation):
        'DistributionParameterQ[expr]'
    ##todo


class DistributionParameterAssumptions(Builtin):

    def apply(self, distribution, evaluation):
        'DistributionParameterAssumptions[distribution]'

        if isinstance(distribution, Distribution):
            return String(distribution.arg_assumptions)


class CDF(Builtin):

    def apply(self, distribution, evaluation):
        'CDF[distribution, symbol]'

        if isinstance(distribution, Distribution):
            return distribution.cdf(str(symbol)))
            

class PDF(Builtin):

    def apply(self, distribution, evaluation):
        'PDF[distribution, symbol]'

        if isinstance(distribution, Distribution):
            return distribution.pdf(str(symbol)))


class InverseCDF(Builtin):



class SurvivalFunction(Builtin):

    def apply(self, distribution, evaluation):
        'SurvivalFunction[distribution]'
        
        return Expression(evaluate('1 - CDF[distribution, x]'))

    
class InverseSurvivalFunction(Builtin):


    
class SurvivalDistribution(Builtin):


    
class BernoulliDistribution(Builtin):



class BetaBinomialDistribution(Distribution):



class BetaDistribution(Distribution):

    'BetaDistribution[alpha, beta]'
    
    parameters = [alpha, beta]
    nargs = 2
    sympy_name = 'Beta'
    arg_assumptions = 'alpha > 0 && beta > 0'


class BetaNegativeBinomialDistribution(Builtin):



class CauchyDistribution(Builtin):



class ChiDistribution(Builtin):



class ChiSquareDistribution(Builtin):



class DiscreteUniformDistribution(Builtin):



class ExponentialDistribution(Builtin):



class ExtremeValueDistribution(Builtin):



class FRatioDistribution(Builtin):



class GammaDistribution(Builtin):



class GumbelDistribution(Builtin):



class HalfNormalDistribution(Builtin):



class InverseGaussianDistribution(Builtin):



class LaplaceDistribution(Builtin):



class LogisticDistribution(Builtin):



class LogNormalDistribution(Builtin):



class LogSeriesDistribution(Builtin):



class MaxwellDistribution(Builtin):



class NegativeBinomialDistribution(Builtin):



class NoncentralChiSquareDistribution(Builtin):



class NoncentralFRatioDistribution(Builtin):



class NormalDistribution(Builtin):



class ParetoDistribution(Builtin):



class PoissonDistribution(Builtin):



class RayleighDistribution(Builtin):



class SkewNormalDistribution(Builtin):



class TriangularDistribution(Builtin):



class UniformDistribution(Builtin):



class WeibullDistribution(Builtin):



class ZipfDistribution(Builtin):



class ArcSinDistribution(Builtin):



class BatesDistribution(Builtin):



class BeckmannDistribution(Builtin):



class BeniniDistribution(Builtin):



class BenktanderGibratDistribution(Builtin):



class BenktanderWeibullDistribution(Builtin):



class BernoulliGraphDistribution(Builtin):



class BetaPrimeDistribution(Builtin):



class BinomialDistribution(Builtin):



class BirnbaumSaundersDistribution(Builtin):



class DagumDistribution(Builtin):



class DavisDistribution(Builtin):



class ErlangDistribution(Builtin):



class ExpGammaDistribution(Builtin):



class ExponentialPowerDistribution(Builtin):



class FrechetDistribution(Builtin):



class GeometricDistribution(Builtin):



class GompertzMakehamDistribution(Builtin):



class HotellingTSquareDistribution(Builtin):



class HoytDistribution(Builtin):



class HyperbolicDistribution(Builtin):



class HypergeometricDistribution(Builtin):



class InverseChiSquareDistribution(Builtin):



class InverseGammaDistribution(Builtin):



class JohnsonDistribution(Builtin):



class KumaraswamyDistribution(Builtin):



class LandauDistribution(Builtin):



class LevyDistribution(Builtin):



class LindleyDistribution(Builtin):



class LogGammaDistribution(Builtin):



class LogLogisticDistribution(Builtin):



class MarginalDistribution(Builtin):



class MaxStableDistribution(Builtin):



class MeixnerDistribution(Builtin):



class MinStableDistribution(Builtin):



class MixtureDistribution(Builtin):



class MoyalDistribution(Builtin):



class MultinomialDistribution(Builtin):



class MultinormalDistribution(Builtin):



class MultivariateHypergeometricDistribution(Builtin):



class NakagamiDistribution(Builtin):



class NoncentralBetaDistribution(Builtin):



class NoncentralStudentTDistribution(Builtin):



class PearsonDistribution(Builtin):



class PowerDistribution(Builtin):



class RiceDistribution(Builtin):



class SechDistribution(Builtin):



class SinghMaddalaDistribution(Builtin):



class StudentTDistribution(Builtin):



class SuzukiDistribution(Builtin):



class TsallisQExponentialDistribution(Builtin):



class TsallisQGaussianDistribution(Builtin):



class TukeyLambdaDistribution(Builtin):



class UniformGraphDistribution(Builtin):



class VonMisesDistribution(Builtin):



class WakebyDistribution(Builtin):



class WignerSemicircleDistribution(Builtin):



class BarabasiAlbertGraphDistribution(Builtin):



class BenfordDistribution(Builtin):



class BinormalDistribution(Builtin):



class BorelTannerDistribution(Builtin):



class CensoredDistribution(Builtin):



class CircularOrthogonalMatrixDistribution(Builtin):



class CircularQuaternionMatrixDistribution(Builtin):



class CircularRealMatrixDistribution(Builtin):

    

class CircularSymplecticMatrixDistribution(Builtin):



class CircularUnitaryMatrixDistribution(Builtin):

    

class CompoundPoissonDistribution(Builtin):



class CoxianDistribution(Builtin):



class DegreeGraphDistribution(Builtin):



class DirichletDistribution(Builtin):



class EmpiricalDistribution(Builtin):



class FirstPassageTimeDistribution(Builtin):



class FisherHypergeometricDistribution(Builtin):



class FisherZDistribution(Builtin):



class GaussianOrthogonalMatrixDistribution(Builtin):



class GaussianSymplecticMatrixDistribution(Builtin):



class GaussianUnitaryMatrixDistribution(Builtin):



class GraphPropertyDistribution(Builtin):



class HistogramDistribution(Builtin):



class HyperexponentialDistribution(Builtin):



class HypoexponentialDistribution(Builtin):



class InverseWishartMatrixDistribution(Builtin):



class KDistribution(Builtin):



class LogMultinormalDistribution(Builtin):



class MatrixNormalDistribution(Builtin):



class MatrixPropertyDistribution(Builtin):



class MatrixTDistribution(Builtin):



class MultivariatePoissonDistribution(Builtin):



class MultivariateTDistribution(Builtin):



class NegativeMultinomialDistribution(Builtin):



class OrderDistribution(Builtin):



class ParameterMixtureDistribution(Builtin):



class PascalDistribution(Builtin):



class PERTDistribution(Builtin):



class PoissonConsulDistribution(Builtin):



class PolyaAeppliDistribution(Builtin):



class PriceGraphDistribution(Builtin):



class QuantityDistribution(Builtin):



class ReliabilityDistribution(Builtin):



class ShiftedGompertzDistribution(Builtin):



class SkellamDistribution(Builtin):



class SliceDistribution(Builtin):



class SmoothKernelDistribution(Builtin):



class SplicedDistribution(Builtin):



class StableDistribution(Builtin):



class StandbyDistribution(Builtin):



class TracyWidomDistribution(Builtin):



class TransformedDistribution(Builtin):



class TruncatedDistribution(Builtin):



class UniformSumDistribution(Builtin):



class WalleniusHypergeometricDistribution(Builtin):



class WaringYuleDistribution(Builtin):



class WattsStrogatzGraphDistribution(Builtin):



class WishartMatrixDistribution(Builtin):
