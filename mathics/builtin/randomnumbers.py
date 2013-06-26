# -*- coding: utf8 -*-

"""
Random number generation

Random numbers are generated using the Mersenne Twister.
"""

from __future__ import with_statement

import random
import cPickle as pickle
import binascii

from mathics.builtin.base import Builtin
from mathics.core.expression import (Integer, String, Symbol, Real, Expression,
                                     Complex)


def get_random_state():
    state = random.getstate()
    state = pickle.dumps(state)
    state = binascii.b2a_hex(state)
    state = int(state, 16)
    return state


def set_random_state(state):
    if state is None:
        random.seed()
    else:
        state = hex(state)[2:]  # drop leading "0x"
        if state.endswith('L'):
            state = state[:-1]
        state = binascii.a2b_hex(state)
        state = pickle.loads(state)
        random.setstate(state)


class RandomEnv:
    def __init__(self, evaluation):
        self.evaluation = evaluation

    def __enter__(self):
        state = self.evaluation.get_config_value('$RandomState')
        set_random_state(state)
        return self

    def __exit__(self, type, value, traceback):
        state = get_random_state()
        self.evaluation.set_config_value('$RandomState', state)

    def randint(self, a, b):
        return random.randint(a, b)

    def randreal(self, a, b):
        return random.uniform(a, b)

    def seed(self, x=None):
        random.seed(x)


class RandomState(Builtin):
    """
    <dl>
    <dt>'$RandomState'
        <dd>is a long number representing the internal state of the pseudorandom number generator.
    </dl>

    >> Mod[$RandomState, 10^100]
     = ...
    >> IntegerLength[$RandomState]
     = ...

    So far, it is not possible to assign values to '$RandomState'.
    >> $RandomState = 42
     : It is not possible to change the random state.
     = 42
    Not even to its own value:
    >> $RandomState = $RandomState;
     : It is not possible to change the random state.
    """

    name = '$RandomState'

    messages = {
        'rndst': "It is not possible to change the random state.",
        #"`1` is not a valid random state.",
    }

    def apply(self, evaluation):
        '$RandomState'

        with RandomEnv(evaluation):
            return Integer(get_random_state())


class SeedRandom(Builtin):
    """
    <dl>
    <dt>'SeedRandom[$x$]'
        <dd>resets the pseudorandom generator with seed $x$.
    <dt>'SeedRandom[]'
        <dd>uses the current date and time as seed.
    </dl>

    'SeedRandom' can be used to get reproducable random numbers:
    >> SeedRandom[42]
    >> RandomInteger[100]
     = 7
    >> RandomInteger[100]
     = 14
    >> SeedRandom[42]
    >> RandomInteger[100]
     = 7
    >> RandomInteger[100]
     = 14

    #> SeedRandom[x]
     : Argument x should be an integer or a string.
     = SeedRandom[x]
    """

    messages = {
        'seed': "Argument `1` should be an integer or a string.",
    }

    def apply(self, x, evaluation):
        'SeedRandom[x_]'

        if not isinstance(x, (Integer, String)):
            return evaluation.message('SeedRandom', 'seed', x)
        with RandomEnv(evaluation) as rand:
            rand.seed(x)
        return Symbol('Null')

    def apply_empty(self, evaluation):
        'SeedRandom[]'

        with RandomEnv(evaluation) as rand:
            rand.seed()
        return Symbol('Null')


class RandomInteger(Builtin):
    """
    <dl>
    <dt>'RandomInteger[{$min$, $max$}]'
        <dd>yields a pseudorandom integer in the range from $min$ to $max$.
    <dt>'RandomInteger[$max$]'
        <dd>yields a pseudorandom integer in the range from 0 to $max$.
    <dt>'RandomInteger[]'
        <dd>gives 0 or 1.
    <dt>'RandomInteger[$range$, $n$]'
        <dd>gives a list of $n$ pseudorandom integers.
    <dt>'RandomInteger[$range$, {$n1$, $n2$, ...}]'
        <dd>gives a nested list of pseudorandom integers.
    </dl>

    >> RandomInteger[{1, 5}]
     = ...
    #> 1 <= % <= 5
     = True

    >> RandomInteger[100, {2, 3}] // TableForm
     = ...   ...   ...
     .
     . ...   ...   ...

    Calling 'RandomInteger' changes '$RandomState':
    >> previousState = $RandomState;
    >> RandomInteger[]
     = ...
    >> $RandomState != previousState
     = True
    """

    messages = {
        'unifr': ("The endpoints specified by `1` for the endpoints of the "
                  "discrete uniform distribution range are not integers."),
    }

    rules = {
        'RandomInteger[]': 'RandomInteger[{0, 1}]',
        'RandomInteger[max_Integer]': 'RandomInteger[{0, max}]',
        'RandomInteger[max_Integer, ns_]': 'RandomInteger[{0, max}, ns]',
        'RandomInteger[spec_, n_Integer]': 'RandomInteger[spec, {n}]',
    }

    def apply(self, rmin, rmax, evaluation):
        'RandomInteger[{rmin_, rmax_}]'

        if not isinstance(rmin, Integer) or not isinstance(rmax, Integer):
            return evaluation.message('RandomInteger', 'unifr',
                                      Expression('List', rmin, rmax))
        rmin, rmax = rmin.value, rmax.value
        with RandomEnv(evaluation) as rand:
            return Integer(rand.randint(rmin, rmax))

    def apply_list(self, rmin, rmax, ns, evaluation):
        'RandomInteger[{rmin_, rmax_}, ns_?ListQ]'
        if not isinstance(rmin, Integer) or not isinstance(rmax, Integer):
            return evaluation.message('RandomInteger', 'unifr',
                                      Expression('List', rmin, rmax))
        rmin, rmax = rmin.value, rmax.value
        result = ns.to_python()

        with RandomEnv(evaluation) as rand:
            def search_product(i):
                if i == len(result) - 1:
                        return Expression('List', *[
                            Integer(rand.randint(rmin, rmax))
                            for j in xrange(result[i])])
                else:
                    return Expression('List', *[
                        search_product(i + 1) for j in xrange(result[i])])
            return search_product(0)


class RandomReal(Builtin):
    """
    <dl>
    <dt>'RandomReal[{$min$, $max$}]'
        <dd>yields a pseudorandom real number in the range from $min$ to $max$.
    <dt>'RandomReal[$max$]'
        <dd>yields a pseudorandom real number in the range from 0 to $max$.
    <dt>'RandomReal[]'
        <dd>yields a pseudorandom real number in the range from 0 to 1.
    <dt>'RandomReal[$range$, $n$]'
        <dd>gives a list of $n$ pseudorandom real numbers.
    <dt>'RandomReal[$range$, {$n1$, $n2$, ...}]'
        <dd>gives a nested list of pseudorandom real numbers.
    </dl>

    >> RandomReal[]
     = ...
    #> 0 <= % <= 1
     = True

    >> RandomReal[{1, 5}]
     = ...

    ## needs too much horizontal space in TeX form
    #> RandomReal[100, {2, 3}] // TableForm
     = ...   ...   ...
     .
     . ...   ...   ...

    #> RandomReal[{0, 1}, {1, -1}]
     : The array dimensions {1, -1} given in position 2 of RandomReal[{0, 1}, {1, -1}] should be a list of non-negative machine-sized integers giving the dimensions for the result.
     = RandomReal[{0, 1}, {1, -1}]
    """

    messages = {
        'unifr': ("The endpoints specified by `1` for the endpoints of the "
                  "discrete uniform distribution range are not real valued."),
        'array': ("The array dimensions `2` given in position 2 of `1` should "
                  "be a list of non-negative machine-sized integers giving "
                  "the dimensions for the result."),
    }

    rules = {
        'RandomReal[]': 'RandomReal[{0, 1}]',
        'RandomReal[max_?NumberQ]': 'RandomReal[{0, max}]',
        'RandomReal[max_?NumberQ, ns_]': 'RandomReal[{0, max}, ns]',
        'RandomReal[spec_, n_Integer]': 'RandomReal[spec, {n}]',
    }

    def apply(self, xmin, xmax, evaluation):
        'RandomReal[{xmin_, xmax_}]'

        if not (isinstance(xmin, (Real, Integer)) and
                isinstance(xmax, (Real, Integer))):
            return evaluation.message('RandomReal', 'unifr',
                                      Expression('List', xmin, xmax))

        min_value, max_value = xmin.to_python(), xmax.to_python()

        with RandomEnv(evaluation) as rand:
            return Real(rand.randreal(min_value, max_value))

    def apply_list(self, xmin, xmax, ns, evaluation):
        'RandomReal[{xmin_, xmax_}, ns_?ListQ]'

        if not (isinstance(xmin, (Real, Integer)) and
                isinstance(xmax, (Real, Integer))):
            return evaluation.message('RandomReal', 'unifr',
                                      Expression('List', xmin, xmax))

        min_value, max_value = xmin.to_python(), xmax.to_python()
        result = ns.to_python()

        if not all([isinstance(i, int) and i >= 0 for i in result]):
            expr = Expression('RandomReal', Expression('List', xmin, xmax), ns)
            return evaluation.message('RandomReal', 'array', expr, ns)

        assert all([isinstance(i, int) for i in result])

        with RandomEnv(evaluation) as rand:
            def search_product(i):
                if i == len(result) - 1:
                        return Expression('List', *[
                            Real(rand.randreal(min_value, max_value))
                            for j in xrange(result[i])])
                else:
                    return Expression('List', *[
                        search_product(i + 1) for j in xrange(result[i])])
            return search_product(0)


class RandomComplex(Builtin):
    """
    <dl>
    <dt>'RandomComplex[{$z_min$, $z_max$}]'
        <dd>yields a pseudorandom complex number in the rectangle with complex corners $z_min$ and $z_max$.
    <dt>'RandomComplex[$z_max$]'
        <dd>yields a pseudorandom complex number in the rectangle with corners at the origin and at $z_max$.
    <dt>'RandomComplex[]'
        <dd>yields a pseudorandom complex number with real and imaginary parts from 0 to 1.
    <dt>'RandomComplex[$range$, $n$]'
        <dd>gives a list of $n$ pseudorandom complex numbers.
    <dt>'RandomComplex[$range$, {$n1$, $n2$, ...}]'
        <dd>gives a nested list of pseudorandom complex numbers.
    </dl>

    >> RandomComplex[]
     = ...
    #> 0 <= Re[%] <= 1 && 0 <= Im[%] <= 1
     = True

    >> RandomComplex[{1+I, 5+5I}]
     = ...
    #> 1 <= Re[%] <= 5 && 1 <= Im[%] <= 5
     = True

    >> RandomComplex[1+I, 5]
     = {..., ..., ..., ..., ...}

    >> RandomComplex[{1+I, 2+2I}, {2, 2}]
     = {{..., ...}, {..., ...}}
    """

    messages = {
        'unifr': (
            "The endpoints specified by `1` for the endpoints of the "
            "discrete uniform distribution range are not complex valued."),
        'array': (
            "The array dimensions `1` given in position 2 of `2` should be a "
            "list of non-negative machine-sized integers giving the "
            "dimensions for the result."),
    }

    rules = {
        'RandomComplex[]': 'RandomComplex[{0, 1+I}]',
        'RandomComplex[zmax_?NumberQ]': 'RandomComplex[{0, zmax}]',
        'RandomComplex[zmax_?NumberQ, ns_]': 'RandomComplex[{0, zmax}, ns]',
    }

    def apply(self, zmin, zmax, evaluation):
        'RandomComplex[{zmin_, zmax_}]'
        if Expression('RealNumberQ', zmin).evaluate(evaluation):
            zmin = Complex(zmin, 0.0)
        if Expression('RealNumberQ', zmax).evaluate(evaluation):
            zmax = Complex(zmax, 0.0)

        if not (isinstance(zmin, Complex) and isinstance(zmax, Complex)):
            return evaluation.message('RandomComplex', 'unifr',
                                      Expression('List', zmin, zmax))

        min_value, max_value = zmin.to_python(), zmax.to_python()

        with RandomEnv(evaluation) as rand:
            return Complex(rand.randreal(min_value.real, max_value.real),
                           rand.randreal(min_value.imag, max_value.imag))

    def apply_list(self, zmin, zmax, ns, evaluation):
        'RandomComplex[{zmin_, zmax_}, ns_]'
        expr = Expression('RandomComplex', Expression('List', zmin, zmax), ns)

        if Expression('RealNumberQ', zmin).evaluate(evaluation):
            zmin = Complex(zmin, 0.0)
        if Expression('RealNumberQ', zmax).evaluate(evaluation):
            zmax = Complex(zmax, 0.0)

        if not (isinstance(zmin, Complex) and isinstance(zmax, Complex)):
            return evaluation.message('RandomComplex', 'unifr',
                                      Expression('List', zmin, zmax))

        min_value, max_value = zmin.to_python(), zmax.to_python()

        py_ns = ns.to_python()
        if not isinstance(py_ns, list):
            py_ns = [py_ns]

        if not all([isinstance(i, int) and i >= 0 for i in py_ns]):
            return evaluation.message('RandomComplex', 'array', ns, expr)

        with RandomEnv(evaluation) as rand:
            def search_product(i):
                if i == len(py_ns) - 1:
                        return Expression('List', *[
                            Complex(
                                rand.randreal(min_value.real, max_value.real),
                                rand.randreal(min_value.imag, max_value.imag)
                            ) for j in xrange(py_ns[i])])
                else:
                    return Expression('List', *[
                        search_product(i + 1) for j in xrange(py_ns[i])])
            return search_product(0)
