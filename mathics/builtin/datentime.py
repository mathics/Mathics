# -*- coding: utf8 -*-

"""
Date and Time
"""

import time
from mathics.core.expression import Expression, Real, Integer, from_python
from mathics.builtin.base import Builtin, Predefined

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

class DateList(Builtin):
    """
    <dl>
    <dt>'DateList[]'
      <dd>returns the current local time in the form {$year$, $month$, $day$, $hour$, $minute$, $second$}.
    <dt>'DateList[time_]'
      <dd>returns a formatted date for the number of seconds $time$ since epoch Jan 1 1900.
    </dl>

    >> DateList[0]
     = {1900, 1, 1, 0, 0, 0.}

    >> DateList[3155673600]
     = {2000, 1, 1, 0, 0, 0.}
    """

    rules = {
        'DateList[]': 'DateList[AbsoluteTime[]]',
    }

    messages = {
        'arg': 'Argument `1` cannot be intepreted as a date or time input.',
    }

    def apply(self, epochtime, evaluation):
        'DateList[epochtime_]'
        secs = epochtime.to_python()
        if not (isinstance(secs, float) or isinstance(secs, int)):
            evaluation.message('DateList', 'arg', epochtime)
            return

        try:
            timestruct = time.gmtime(secs - 2208988800)
        except ValueError:
            #TODO: Fix arbitarily large times
            return

        datelist = list(timestruct[:5])
        datelist.append(timestruct[5] + secs % 1.)      # Hack to get seconds as float not int.

        return Expression('List', *datelist)
        
class AbsoluteTime(Builtin):
    """
    <dl>
    <dt>'AbsoluteTime[]'
      <dd>Gives the local time in seconds since epoch Jan 1 1900.
    </dl>
    """

    def apply(self, evaluation):
        'AbsoluteTime[]'
        return from_python(time.time() + 2208988800 - time.timezone)


class TimeZone(Predefined):
    """
    """

    name = '$TimeZone'

    def evaluate(self, evaluation):
        return Real(-time.timezone / 3600.)


class TimeUsed(Builtin):
    """
    <dl>
    <dt>'TimeUsed[]'
      <dd>returns the total cpu time used for this session.
    </dl>
    """

    def apply(self, evaluation):
        'TimeUsed[]'
        return Real(time.clock()) #TODO: Check this for windows


