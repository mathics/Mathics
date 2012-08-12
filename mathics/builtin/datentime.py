# -*- coding: utf8 -*-

"""
Date and Time
"""

import time
from datetime import datetime, timedelta
from mathics.core.expression import Expression, Real, Symbol, from_python
from mathics.builtin.base import Builtin, Predefined

START_TIME = time.time()

class Timing(Builtin):
    """
    <dl>
    <dt>'Timing[$expr$]'
    <dd>measures the processor time taken to evaluate $expr$.
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
        
        start = time.clock()
        result = expr.evaluate(evaluation)
        stop = time.clock()
        return Expression('List', Real(stop - start), result)

class AbsoluteTiming(Builtin):
    """
    <dl>
    <dt>'AbsoluteTiming[$expr$]'
    <dd>measures the actual time it takes to evaluate $expr$.
    It returns a list containing the measured time in seconds and the result of the evaluation.
    </dl> 
    >> AbsoluteTiming[50!]
     = {..., 30414093201713378043612608166064768844377641568960512000000000000}
    >> Attributes[AbsoluteTiming]
     = {HoldAll, Protected}
    """
    
    attributes = ('HoldAll',)
    
    def apply(self, expr, evaluation):
        'AbsoluteTiming[expr_]'
        
        start = time.time()
        result = expr.evaluate(evaluation)
        stop = time.time()
        return Expression('List', Real(stop - start), result)

class DateList(Builtin):
    """
    <dl>
    <dt>'DateList[]'
      <dd>returns the current local time in the form {$year$, $month$, $day$, $hour$, $minute$, $second$}.
    <dt>'DateList[time_]'
      <dd>returns a formatted date for the number of seconds $time$ since epoch Jan 1 1900.
    <dt>'DateList[{y, m, d, h, m, s}]'
      <dd>converts an incomplete date list to the standard representation.
    </dl>

    >> DateList[0]
     = {1900, 1, 1, 0, 0, 0.}

    >> DateList[3155673600]
     = {2000, 1, 1, 0, 0, 0.}

    >> DateList[{2003, 5, 0.5, 0.1, 0.767}]
     = {2003, 4, 30, 12, 6, 46.02}

    >> DateList[{2012, 1, 300., 10, 0.}]
     = {2012, 10, 26, 10, 0, 0.}
    """

    rules = {
        'DateList[]': 'DateList[AbsoluteTime[]]',
    }

    messages = {
        'arg': 'Argument `1` cannot be intepreted as a date or time input.',
    }

    def apply(self, epochtime, evaluation):
        'DateList[epochtime_]'
        etime = epochtime.to_python()
        if isinstance(etime, float) or isinstance(etime, int):
            try:
                timestruct = time.gmtime(etime - 2208988800)
            except ValueError:
                #TODO: Fix arbitarily large times
                return

            datelist = list(timestruct[:5])
            datelist.append(timestruct[5] + etime % 1.)      # Hack to get seconds as float not int.

        elif isinstance(etime, list) and 1 <= len(etime) <= 6 \
          and all((isinstance(val, float) and i>1) or isinstance(val, int) for i,val in enumerate(etime)):
            default_date = [1900, 1, 1, 0, 0, 0.]

            datelist = etime + default_date[len(etime):]
            prec_part, imprec_part = datelist[:2], datelist[2:]

            try:
                dtime = datetime(prec_part[0], prec_part[1], 1)
            except ValueError:
                # datetime is fairly easy to overlfow. 1 <= month <= 12 and some bounds on year too.
                # TODO: Make this more resiliant (accept a wider range of years and months)
                evaluation.message('DateList', 'arg', epochtime)
                return

            tdelta = timedelta(days=imprec_part[0]-1, hours=imprec_part[1], minutes=imprec_part[2], seconds=imprec_part[3])
            dtime += tdelta
            datelist = [dtime.year, dtime.month, dtime.day, dtime.hour, dtime.minute, dtime.second + 1e-06 * dtime.microsecond]
            
        else:
            evaluation.message('DateList', 'arg', epochtime)
            return

        #TODO: Other input forms
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

class SessionTime(Builtin):
    """
    <dl>
    <dt>'SessionTime[]'
      <dd>returns the total time since this session started.
    </dl>
    """
    def apply(self, evaluation):
        'SessionTime[]'
        return Real(time.time() - START_TIME)


class Pause(Builtin):
    """
    <dl>
    <dt>'Pause[n]'
      <dd>pauses for $n$ seconds.
    </dl>
    """

    messages = {
        'numnm': 'Non-negative machine-sized number expected at position 1 in `1`.',
    }

    def apply(self, n, evaluation):
        'Pause[n_]'
        sleeptime = n.to_python()
        if not (isinstance(sleeptime, int) or isinstance(sleeptime, float)) or sleeptime < 0:
            evaluation.message('Pause', 'numnm', Expression('Pause', n))
            return

        time.sleep(sleeptime)
        return Symbol('Null')

