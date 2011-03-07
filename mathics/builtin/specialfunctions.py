# -*- coding: utf8 -*-

"""
Special functions
"""

import mpmath

from mathics.builtin.arithmetic import _MPMathFunction

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