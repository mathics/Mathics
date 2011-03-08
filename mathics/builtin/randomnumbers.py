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
from mathics.core.expression import Integer, String, Symbol, Real

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
        print "seed to %s" % x
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
        
        with RandomEnv(evaluation) as rand:
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
     = 18
    >> RandomInteger[100]
     = 95
    >> SeedRandom[42]
    >> RandomInteger[100]
     = 18
    >> RandomInteger[100]
     = 95
    
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
        'unifr': "The endpoints specified by `1` for the endpoints of the discrete uniform distribution range are not integers.",
    }
    
    rules = {
        'RandomInteger[]': 'RandomInteger[{0, 1}]',
        'RandomInteger[max_Integer]': 'RandomInteger[{0, max}]',
        'RandomInteger[spec_, n_]': 'Table[RandomInteger[spec], {n}]',
        'RandomInteger[spec_, ns_List]': 'Table[RandomInteger[spec], Evaluate[Sequence @@ List /@ ns]]',
    }
    
    def apply(self, min, max, evaluation):
        'RandomInteger[{min_, max_}]'
        
        if not isinstance(min, Integer) or not isinstance(max, Integer):
            return evaluation.message('RandomInteger', 'unifr', Expression('List', min, max))
        min, max = min.value, max.value
        with RandomEnv(evaluation) as rand:
            return Integer(rand.randint(min, max))
    
class RandomReal(Builtin):
    """
    <dl>
    <dt>'RandomReal[{$min$, $max$}]'
        <dd>yields a pseudorandom real numbers in the range from $min$ to $max$.
    <dt>'RandomReal[$max$]'
        <dd>yields a pseudorandom real numbers in the range from 0 to $max$.
    <dt>'RandomReal[]'
        <dd>yields a pseudorandom real numbers in the range from 0 to 1.
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
    """
    
    messages = {
        'unifr': "The endpoints specified by `1` for the endpoints of the discrete uniform distribution range are not real valued.",
    }
    
    rules = {
        'RandomReal[]': 'RandomReal[{0, 1}]',
        'RandomReal[max_Integer]': 'RandomReal[{0, max}]',
        'RandomReal[spec_, n_]': 'Table[RandomReal[spec], {n}]',
        'RandomReal[spec_, ns_List]': 'Table[RandomReal[spec], Evaluate[Sequence @@ List /@ ns]]',
    }
    
    def apply(self, min, max, evaluation):
        'RandomReal[{min_, max_}]'
        
        min_value = min.get_real_value()
        max_value = max.get_real_value()
        if min_value is None or max_value is None:
            return evaluation.message('RandomReal', 'unifr', Expression('List', min, max))
        with RandomEnv(evaluation) as rand:
            return Real(rand.randreal(min_value, max_value))
            