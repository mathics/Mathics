#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Random number generation

Random numbers are generated using the Mersenne Twister.
"""

from __future__ import unicode_literals
from __future__ import absolute_import

from six.moves import range
import six.moves.cPickle as pickle

import binascii
import hashlib
import numpy
import time
import os

from mathics.builtin.base import Builtin
from mathics.core.expression import (Integer, String, Symbol, Real, Expression,
                                     Complex)


def get_random_state():
    state = numpy.random.get_state()
    state = pickle.dumps(state)
    state = binascii.b2a_hex(state)
    state.decode('ascii')
    state = int(state, 16)
    return state


def set_random_state(state):
    if state is None:
        numpy.random.seed()
    else:
        state = hex(state)[2:]  # drop leading "0x"
        state = state.rstrip('L')
        state = state.encode('ascii')
        state = binascii.a2b_hex(state)
        state = pickle.loads(state)
        numpy.random.set_state(state)


def _from_numpy(a, new_element, d=1):
    if len(a.shape) == d:
        leaves = [new_element(x) for x in a]
    else:
        leaves = [_from_numpy(a[k], new_element, d) for k in range(a.shape[0])]
    return Expression('List', *leaves)


class RandomEnv:
    def __init__(self, evaluation):
        self.evaluation = evaluation

    def __enter__(self):
        state = self.evaluation.definitions.get_config_value('$RandomState')
        set_random_state(state)
        return self

    def __exit__(self, exit_type, value, traceback):
        state = get_random_state()
        self.evaluation.definitions.set_config_value('$RandomState', state)

    def randint(self, a, b, size=None):
        return numpy.random.random_integers(a, b, size)

    def randreal(self, a, b, size=None):
        # numpy gives us [a, b). we want [a, b].
        return numpy.random.uniform(a, numpy.nextafter(b, 1), size)

    def randchoice(self, n, size, replace, p):
        return numpy.random.choice(n, size=size, replace=replace, p=p)

    def seed(self, x=None):
        if x is None:  # numpy does not know to seed itself randomly
            x = int(time.time() * 1000) ^ hash(os.urandom(16))
        # for numpy, seed must be convertible to 32 bit unsigned integer.
        numpy.random.seed(abs(x) & 0xffffffff)


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
        # "`1` is not a valid random state.",
    }

    def apply(self, evaluation):
        '$RandomState'

        with RandomEnv(evaluation):
            return Integer(get_random_state())


class SeedRandom(Builtin):
    """
    <dl>
    <dt>'SeedRandom[$n$]'
        <dd>resets the pseudorandom generator with seed $n$.
    <dt>'SeedRandom[]'
        <dd>uses the current date and time as the seed.
    </dl>

    'SeedRandom' can be used to get reproducible random numbers:
    >> SeedRandom[42]
    >> RandomInteger[100]
     = ...
    >> RandomInteger[100]
     = ...
    >> SeedRandom[42]
    >> RandomInteger[100]
     = ...
    >> RandomInteger[100]
     = ...

    String seeds are supported as well:
    >> SeedRandom["Mathics"]
    >> RandomInteger[100]
     = ...

    Calling 'SeedRandom' without arguments will seed the random
    number generator to a random state:
    >> SeedRandom[]
    >> RandomInteger[100]
     = ...

    #> SeedRandom[x]
     : Argument x should be an integer or string.
     = SeedRandom[x]
    """

    messages = {
        'seed': "Argument `1` should be an integer or string.",
    }

    def apply(self, x, evaluation):
        'SeedRandom[x_]'

        if isinstance(x, Integer):
            value = x.value
        elif isinstance(x, String):
            # OS/version-independent hash
            value = int(hashlib.md5(x.get_string_value().encode('utf8')).hexdigest(), 16)
        else:
            return evaluation.message('SeedRandom', 'seed', x)
        with RandomEnv(evaluation) as rand:
            rand.seed(value)
        return Symbol('Null')

    def apply_empty(self, evaluation):
        'SeedRandom[]'

        with RandomEnv(evaluation) as rand:
            rand.seed()
        return Symbol('Null')


class _RandomBase(Builtin):
    rules = {
        '%(name)s[spec_]': '%(name)s[spec, {1}]',
        '%(name)s[spec_, n_Integer]': '%(name)s[spec, {n}]'
    }

    messages = {
        'array': (
            "The array dimensions `1` given in position 2 of `2` should be a "
            "list of non-negative machine-sized integers giving the "
            "dimensions for the result."),
    }

    def _size_to_python(self, domain, size, evaluation):
        is_proper_spec = size.get_head_name() == 'System`List' and all(n.is_numeric() for n in size.leaves)

        py_size = size.to_python() if is_proper_spec else None
        if (py_size is None) or (not all(isinstance(i, int) and i >= 0 for i in py_size)):
            expr = Expression(self.get_name(), domain, size)
            return evaluation.message(self.get_name(), 'array', size, expr), None

        return False, py_size


class RandomInteger(Builtin):
    """
    <dl>
    <dt>'RandomInteger[{$min$, $max$}]'
        <dd>yields a pseudorandom integer in the range from $min$ to
        $max$ inclusive.
    <dt>'RandomInteger[$max$]'
        <dd>yields a pseudorandom integer in the range from 0 to $max$
        inclusive.
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
            return _from_numpy(rand.randint(rmin, rmax, result), Integer)


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
            return _from_numpy(rand.randreal(min_value, max_value, result), Real)


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
            return evaluation.message('RandomComplex', 'unifr', Expression('List', zmin, zmax))

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
            return evaluation.message('RandomComplex', 'unifr', Expression('List', zmin, zmax))

        min_value, max_value = zmin.to_python(), zmax.to_python()

        py_ns = ns.to_python()
        if not isinstance(py_ns, list):
            py_ns = [py_ns]

        if not all([isinstance(i, int) and i >= 0 for i in py_ns]):
            return evaluation.message('RandomComplex', 'array', ns, expr)

        with RandomEnv(evaluation) as rand:
            real = rand.randreal(min_value.real, max_value.real, py_ns)
            imag = rand.randreal(min_value.imag, max_value.imag, py_ns)

            # numpy.stack in the following code stacks real and imag along the most inner axis:
            # e.g. numpy.stack([ [1, 2], [3, 4] ], axis=-1)
            # gives: array([ [1, 3], [2, 4] ])
            # e.g. numpy.stack([ [[1, 2], [3, 4]], [[4, 5], [6, 7]] ], axis=-1)
            # gives: array([[[1, 4], [2, 5]], [[3, 6], [4, 7]]])

            return _from_numpy(numpy.stack([real, imag], axis=-1), lambda c: Complex(*c), d=2)


class _RandomSelection(_RandomBase):
    # implementation note: weights are clipped to numpy floats. this might be different from MMA
    # where weights might be handled with full dynamic precision support through the whole computation.
    # we try to limit the error by normalizing weights with full precision, and then clipping to float.
    # since weights are probabilities into a finite set, this should not make a difference.

    messages = {
        'wghtv':
            "The weights on the left-hand side of `1` has to be a list of non-negative numbers " +
            "with the same length as the list of items on the right-hand side.",
        'lrwl': "`1` has to be a list of items or a rule of the form weights -> choices.",
        'smplen':
            "RandomSample cannot choose `1` samples, as this are more samples than there are in `2`. " +
            "Use RandomChoice to choose items from a set with replacing."
    }

    def apply(self, domain, size, evaluation):
        '''%(name)s[domain_, size_]'''
        if domain.get_head_name() == 'System`Rule':  # elements and weights
            err, py_weights = self._weights_to_python(domain.leaves[0], evaluation)
            if py_weights is None:
                return err
            elements = domain.leaves[1].leaves
            if domain.leaves[1].get_head_name() != 'System`List' or len(py_weights) != len(elements):
                return evaluation.message(self.get_name(), 'wghtv', domain)
        elif domain.get_head_name() == 'System`List':  # only elements
            py_weights = None
            elements = domain.leaves
        else:
            return evaluation.message(self.get_name(), 'lrwl', domain)
        err, py_size = self._size_to_python(domain, size, evaluation)
        if py_size is None:
            return err
        if not self._replace:  # i.e. RandomSample?
            n_chosen = 1
            for n in py_size:
                n_chosen *= n
            if len(elements) < n_chosen:
                return evaluation.message('smplen', size, domain), None
        with RandomEnv(evaluation) as rand:
            return _from_numpy(rand.randchoice(len(elements), size=py_size, replace=self._replace,
                                               p=py_weights), lambda i: elements[i])

    def _weights_to_python(self, weights, evaluation):
        # we need to normalize weights as numpy.rand.randchoice expects this and as we can limit
        # accuracy problems with very large or very small weights by normalizing with sympy
        is_proper_spec = weights.get_head_name() == 'System`List' and all(w.is_numeric() for w in weights.leaves)

        if is_proper_spec and len(weights.leaves) > 1:  # normalize before we lose accuracy
            norm_weights = Expression('Divide', weights, Expression('Total', weights)).evaluate(evaluation)
            if norm_weights is None or not all(w.is_numeric() for w in norm_weights.leaves):
                return evaluation.message(self.get_name(), 'wghtv', weights), None
            weights = norm_weights

        py_weights = weights.to_python() if is_proper_spec else None
        if (py_weights is None) or (not all(isinstance(w, (int, float)) and w >= 0 for w in py_weights)):
            return evaluation.message(self.get_name(), 'wghtv', weights), None

        return False, py_weights


class RandomChoice(_RandomSelection):
    """
    <dl>
    <dt>'RandomChoice[$items$]'
        <dd>randomly picks one item from $items$.
    <dt>'RandomChoice[$items$, $n$]'
        <dd>randomly picks $n$ items from $items$. Each pick in the $n$ picks happens from the
        given set of $items$, so each item can be picked any number of times.
    <dt>'RandomChoice[$items$, {$n1$, $n2$, ...}]'
        <dd>randomly picks items from $items$ and arranges the picked items in the nested list
        structure described by {$n1$, $n2$, ...}.
    <dt>'RandomChoice[$weights$ -> $items$, $n$]'
        <dd>randomly picks $n$ items from $items$ and uses the corresponding numeric values in
        $weights$ to determine how probable it is for each item in $items$ to get picked (in the
        long run, items with higher weights will get picked more often than ones with lower weight).
    <dt>'RandomChoice[$weights$ -> $items$]'
        <dd>randomly picks one items from $items$ using weights $weights$.
    <dt>'RandomChoice[$weights$ -> $items$, {$n1$, $n2$, ...}]'
        <dd>randomly picks a structured list of items from $items$ using weights $weights$.
    </dl>

    >> SeedRandom[42]
    >> RandomChoice[{a, b, c}]
     = {c}
    >> SeedRandom[42]
    >> RandomChoice[{a, b, c}, 20]
     = {c, a, c, c, a, a, c, b, c, c, c, c, a, c, b, a, b, b, b, b}
    >> SeedRandom[42]
    >> RandomChoice[{"a", {1, 2}, x, {}}, 10]
     = {x, {}, a, x, x, {}, a, a, x, {1, 2}}
    >> SeedRandom[42]
    >> RandomChoice[{a, b, c}, {5, 2}]
     = {{c, a}, {c, c}, {a, a}, {c, b}, {c, c}}
    >> SeedRandom[42]
    >> RandomChoice[{1, 100, 5} -> {a, b, c}, 20]
     = {b, b, b, b, b, b, b, b, b, b, b, c, b, b, b, b, b, b, b, b}
     """

    _replace = True


class RandomSample(_RandomSelection):
    """
    <dl>
    <dt>'RandomSample[$items$]'
        <dd>randomly picks one item from $items$.
    <dt>'RandomSample[$items$, $n$]'
        <dd>randomly picks $n$ items from $items$. Each pick in the $n$ picks happens after the
        previous items picked have been removed from $items$, so each item can be picked at most
        once.
    <dt>'RandomSample[$items$, {$n1$, $n2$, ...}]'
        <dd>randomly picks items from $items$ and arranges the picked items in the nested list
        structure described by {$n1$, $n2$, ...}. Each item gets picked at most once.
    <dt>'RandomSample[$weights$ -> $items$, $n$]'
        <dd>randomly picks $n$ items from $items$ and uses the corresponding numeric values in
        $weights$ to determine how probable it is for each item in $items$ to get picked (in the
        long run, items with higher weights will get picked more often than ones with lower weight).
        Each item gets picked at most once.
    <dt>'RandomSample[$weights$ -> $items$]'
        <dd>randomly picks one items from $items$ using weights $weights$. Each item gets picked
        at most once.
    <dt>'RandomSample[$weights$ -> $items$, {$n1$, $n2$, ...}]'
        <dd>randomly picks a structured list of items from $items$ using weights $weights$. Each
        item gets picked at most once.
    </dl>

    >> SeedRandom[42]
    >> RandomSample[{a, b, c}]
     = {a}
    >> SeedRandom[42]
    >> RandomSample[{a, b, c, d, e, f, g, h}, 7]
     = {b, f, a, h, c, e, d}
    >> SeedRandom[42]
    >> RandomSample[{"a", {1, 2}, x, {}}, 3]
     = {{1, 2}, {}, a}
    >> SeedRandom[42]
    >> RandomSample[Range[100], {2, 3}]
     = {{84, 54, 71}, {46, 45, 40}}
    >> SeedRandom[42]
    >> RandomSample[Range[100] -> Range[100], 5]
     = {62, 98, 86, 78, 40}
     """

    _replace = False
