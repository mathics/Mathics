# -*- coding: utf8 -*-

"""
Date and Time
"""

from time import clock

from mathics.core.expression import Expression, Real
from mathics.builtin.base import Builtin

class Timing(Builtin):
    """
    <dl>
    <dt>'Timing[$expr$]'
    <dd>measures the time it takes to evaluate $expr$.
    It returns a list containing the measured time in seconds and the result of the evaluation.
    </dl> 
    >> Timing[50!]
     = {..., 30414093201713378043612608166064768844377641568960512000000000000}
    >> Attributes[Timing]
     = {HoldAll, Protected}
    """
    
    attributes = ('HoldAll',)
    
    def apply(self, expr, evaluation):
        'Timing[expr_]'
        
        start = clock()
        result = expr.evaluate(evaluation)
        stop = clock()
        return Expression('List', Real(stop - start), result)