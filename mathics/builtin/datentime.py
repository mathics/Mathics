# -*- coding: utf8 -*-

"""
Date and Time
"""

import time
from datetime import datetime, timedelta
from mathics.core.expression import Expression, Real, Symbol, from_python
from mathics.builtin.base import Builtin, Predefined

START_TIME = time.time()

TIME_INCREMENTS = {
    'Year':     (1, 0, 0, 0, 0, 0),
    'Quarter':  (0, 4, 0, 0, 0, 0),
    'Month':    (0, 1, 0, 0, 0, 0),
    'Week':     (0, 0, 7, 0, 0, 0),
    'Day':      (0, 0, 1, 0, 0, 0),
    'Hour':     (0, 0, 0, 1, 0, 0),
    'Minute':   (0, 0, 0, 0, 1, 0),
    'Second':   (0, 0, 0, 0, 0, 1),
}

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

class _Date():
    def __init__(self, datelist = [], absolute=None):
        datelist += [1900, 1, 1, 0, 0, 0.][len(datelist):]
        self.date = datetime(
            datelist[0], datelist[1], datelist[2], datelist[3], datelist[4], 
            int(datelist[5]), int(1e6 * (datelist[5] % 1.)))
        if absolute is not None:
            self.date += timedelta(seconds=absolute)

    def add(self, timevec):
        years = self.date.year + timevec[0] + int((self.date.month + timevec[1]) / 12)
        months = (self.date.month + timevec[1]) % 12
        if months == 0:
            months += 12
            years -= 1
        self.date = datetime(years, months, self.date.day, self.date.hour, self.date.minute, self.date.second)
        tdelta = timedelta(days=timevec[2], hours=timevec[3], minutes=timevec[4], seconds=timevec[5])
        self.date += tdelta

    def to_list(self):
        return [self.date.year, self.date.month, self.date.day, self.date.hour, self.date.minute, self.date.second + 1e-6*self.date.microsecond]

class DatePlus(Builtin):
    """
    <dl>
    <dt>'DatePlus[date, n]'
      <dd>finds the date $n$ days after $date$.
    <dt>'DatePlus[date, {n, "unit"}]'
      <dd>finds the date $n$ units after $date$.
    <dt>'DatePlus[date, {{n1, "unit1"}, {n2, unit2}, ...}]'
      <dd>finds the date which is $n_i$ specified units after $date$.
    <dt>'DatePlus[n]'
      <dd>finds the date $n$ days after the current date.
    <dt>'DatePlus[offset]'
      <dd>finds the date which is offset from the current date.
    </dl>

    Add 73 days to Feb 5, 2010
    >> DatePlus[{2010, 2, 5}, 73]
     = {2010, 4, 19}

    Add 8 Weeks 1 day to March 16, 1999
    >> DatePlus[{2010, 2, 5}, {{8, "Week"}, {1, "Day"}}]
     = {2010, 4, 3}
    """

    rules = {
        'DatePlus[n_]': 'DatePlus[{DateList[][[1]], DateList[][[2]], DateList[][[3]]}, n]',
    }

    messages = {
        'date': 'Argument `1` cannot be interpreted as a date.',
        'inc': 'Argument `1` is not a time increment or a list of time increments.',
    }

    def apply(self, date, off, evaluation):
        'DatePlus[date_, off_]'
        
        # Process date
        pydate = date.to_python()
        if isinstance(pydate, list):        # Date List
            date_prec = len(pydate)
            idate = _Date(datelist = pydate)
        elif isinstance(pydate, float) or isinstance(pydate, int):     # Absolute Time
            date_prec = 'absolute'
            idate = _Date(absolute = pydate)
        elif isinstance(pydate, unicode):
            date_prec = 'string'
            #TODO
            return
        else:
            evaluation.message('DatePlus', 'date', date)        
            return

        # Process offset
        pyoff = off.to_python()
        if isinstance(pyoff, float) or isinstance(pyoff, int):
            pyoff = [[pyoff, u'"Day"']]
        elif isinstance(pyoff, list) and len(pyoff) == 2 and isinstance(pyoff[1], unicode):
            pyoff = [pyoff]

        # Strip " marks
        pyoff = map(lambda x: [x[0], x[1].strip('"')], pyoff)

        if isinstance(pyoff, list) and all(len(o) == 2 and o[1] in TIME_INCREMENTS.keys() and (isinstance(o[0], float) or isinstance(o[0], int)) for o in pyoff):
            for o in pyoff:
                idate.add([o[0] * TIME_INCREMENTS[o[1]][i] for i in range(6)])
        else:
            evaluation.message('DatePlus', 'inc', off) 
            return

        if isinstance(date_prec, int):
            result = Expression('List', *idate.to_list()[:date_prec])
        elif date_prec == 'absolute':
            result = Expression('AbsoluteTime', idate.to_list())
        elif date_prec == 'string':
            result = Expression('DateString', *idate.to_list())

        return result
