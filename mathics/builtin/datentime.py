# -*- coding: utf-8 -*-

"""
Date and Time
"""

from datetime import datetime, timedelta
import dateutil.parser
import re
import sys
import time

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.core.expression import (
    Expression,
    Real,
    Symbol,
    SymbolAborted,
    SymbolInfinity,
    String,
    Integer,
    from_python,
)

from mathics.core.evaluation import TimeoutInterrupt, run_with_timeout_and_stack

from mathics.builtin.base import Builtin, Predefined
from mathics.settings import TIME_12HOUR


START_TIME = time.time()

TIME_INCREMENTS = {
    "Year": (1, 0, 0, 0, 0, 0),
    "Quarter": (0, 3, 0, 0, 0, 0),
    "Month": (0, 1, 0, 0, 0, 0),
    "Week": (0, 0, 7, 0, 0, 0),
    "Day": (0, 0, 1, 0, 0, 0),
    "Hour": (0, 0, 0, 1, 0, 0),
    "Minute": (0, 0, 0, 0, 1, 0),
    "Second": (0, 0, 0, 0, 0, 1),
}

# FIXME: Some of the formats are not supported by strftime/strptime
# (commented out)
DATE_STRING_FORMATS = {
    "Date": "%c",
    "DateShort": "%a %d %b %Y",
    "Time": "%X",
    "DateTime": "%c %X",
    "DateTimeShort": "%a %d %b %Y %X",
    "Year": "%Y",
    "YearShort": "%y",
    # "QuarterName": "Quarter N",
    # "QuarterNameShort": "QN",
    # "Quarter": "",
    "MonthName": "%B",
    "MonthNameShort": "%b",
    # "MonthNameInitial": "%b",
    "Month": "%m",
    "MonthShort": "%m",
    "DayName": "%A",
    "DayNameShort": "%a",
    # "DayNameInitial": "%a",
    "Day": "%d",
    "DayShort": "%d",
    "Hour": "%I" if TIME_12HOUR else "%H",
    "Hour12": "%I",
    "Hour24": "%H",
    "HourShort": "%H",
    "Hour12Short": "%I",
    "Hour24Short": "%H",
    "AMPM": "%p",
    # "AMPMLowerCase": "%p",
    "Minute": "%M",
    "MinuteShort": "%M",
    "Second": "%S",
    "SecondShort": "%S",
    "SecondExact": "%S.%f",
    # "Millisecond": "%f",
    # "MillisecondShort": "",
}

EPOCH_START = datetime(1900, 1, 1)

if not hasattr(timedelta, "total_seconds"):

    def total_seconds(td):
        return (
            float(td.microseconds + (td.seconds + td.days * 24 * 3600) * 10 ** 6)
            / 10 ** 6
        )


else:
    total_seconds = timedelta.total_seconds


class TimeRemaining(Builtin):
    """
    <dl>
    <dt>'TimeRemaining[]'
        <dd>Gives the number of seconds remaining until the earliest enclosing 'TimeConstrained' will request the current computation to stop.
    <dt>'TimeConstrained[$expr$, $t$, $failexpr$]'
        <dd>returns $failexpr$ if the time constraint is not met.
    </dl>

    If TimeConstrained is called out of a TimeConstrained expression, returns `Infinity`
    >> TimeRemaining[]
     = Infinity

    X> TimeConstrained[1+2; Print[TimeRemaining[]], 0.9]
     | 0.899318

    """

    def apply(self, evaluation):
        "TimeRemaining[]"
        if len(evaluation.timeout_queue) > 0:
            t, start_time = evaluation.timeout_queue[-1]
            curr_time = datetime.now().timestamp()
            deltat = t + start_time - curr_time
            return Real(deltat)
        else:
            return SymbolInfinity


if sys.platform != "win32":

    class TimeConstrained(Builtin):
        """
        <dl>
        <dt>'TimeConstrained[$expr$, $t$]'
            <dd>'evaluates $expr$, stopping after $t$ seconds.'
        <dt>'TimeConstrained[$expr$, $t$, $failexpr$]'
            <dd>'returns $failexpr$ if the time constraint is not met.'
        </dl>
        >> TimeConstrained[Integrate[Sin[x]^1000000,x],1]
        = $Aborted

        >> TimeConstrained[Integrate[Sin[x]^1000000,x], 1, Integrate[Cos[x],x]]
        = Sin[x]

        >> s=TimeConstrained[Integrate[Sin[x] ^ 3, x], a]
         : Number of seconds a is not a positive machine-sized number or Infinity.
         = TimeConstrained[Integrate[Sin[x] ^ 3, x], a]

        >> a=1; s
        = -Cos[x] + Cos[x] ^ 3 / 3

        Possible issues: for certain time-consuming functions (like simplify)
        which are based on sympy or other libraries, it is possible that
        the evaluation continues after the timeout. However, at the end of the evaluation, the function will return $\\$Aborted$ and the results will not affect
        the state of the mathics kernel.

        """

        attributes = ("HoldAll",)
        messages = {
            "timc": "Number of seconds `1` is not a positive machine-sized number or Infinity.",
        }

        def apply_2(self, expr, t, evaluation):
            "TimeConstrained[expr_, t_]"
            return self.apply_3(expr, t, SymbolAborted, evaluation)

        def apply_3(self, expr, t, failexpr, evaluation):
            "TimeConstrained[expr_, t_, failexpr_]"
            t = t.evaluate(evaluation)
            if not t.is_numeric():
                evaluation.message("TimeConstrained", "timc", t)
                return
            try:
                t = float(t.to_python())
                evaluation.timeout_queue.append((t, datetime.now().timestamp()))
                request = lambda: expr.evaluate(evaluation)
                res = run_with_timeout_and_stack(request, t, evaluation)
            except TimeoutInterrupt:
                evaluation.timeout_queue.pop()
                return failexpr.evaluate(evaluation)
            except:
                evaluation.timeout_queue.pop()
                raise
            evaluation.timeout_queue.pop()
            return res


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

    attributes = ("HoldAll",)

    def apply(self, expr, evaluation):
        "Timing[expr_]"

        start = time.process_time()
        result = expr.evaluate(evaluation)
        stop = time.process_time()
        return Expression("List", Real(stop - start), result)


class AbsoluteTiming(Builtin):
    """
    <dl>
      <dt>'AbsoluteTiming[$expr$]'
      <dd>evaluates $expr$, returning a list of the absolute number of seconds in real time that have elapsed, together with the result obtained.
    </dl>

    >> AbsoluteTiming[50!]
     = {..., 30414093201713378043612608166064768844377641568960512000000000000}
    >> Attributes[AbsoluteTiming]
     = {HoldAll, Protected}
    """

    attributes = ("HoldAll",)

    def apply(self, expr, evaluation):
        "AbsoluteTiming[expr_]"

        start = time.time()
        result = expr.evaluate(evaluation)
        stop = time.time()
        return Expression("List", Real(stop - start), result)


class DateStringFormat(Predefined):
    """
    <dl>
      <dt>'$DateStringFormat'
      <dd>gives the format used for dates generated by 'DateString'.
    </dl>

    >> $DateStringFormat
     = {DateTimeShort}
    """

    name = "$DateStringFormat"

    value = "DateTimeShort"

    # TODO: Methods to change this

    def evaluate(self, evaluation):
        return Expression("List", String(self.value))


class _DateFormat(Builtin):
    messages = {
        "arg": "Argument `1` cannot be interpreted as a date or time input.",
        "str": "String `1` cannot be interpreted as a date in format `2`.",
        "ambig": "The interpretation of `1` is ambiguous.",
        "fmt": "`1` is not a valid date format.",
    }

    automatic = re.compile(
        r"^([0-9]{1,4})\s*([^0-9]*)\s*([0-9]{1,2})\s*\2\s*([0-9]{1,4})\s*"
    )

    def parse_date_automatic(self, epochtime, etime, evaluation):
        m = _DateFormat.automatic.search(etime)
        if not m:
            return dateutil.parser.parse(etime)

        x1, x2, x3 = tuple(m.group(i) for i in (1, 3, 4))
        i1, i2, i3 = tuple(int(x) for x in (x1, x2, x3))

        if len(x1) <= 2:
            if i1 > 12:
                month_day = "%d %m"
                is_ambiguous = False
            else:
                month_day = "%m %d"
                is_ambiguous = not (i2 > 12 or i1 == i2)  # is i2 not clearly a day?

            if len(x3) <= 2:
                date = datetime.strptime(
                    "%02d %02d %02d" % (i1, i2, i3), month_day + " %y"
                )
            else:
                date = datetime.strptime(
                    "%02d %02d %04d" % (i1, i2, i3), month_day + " %Y"
                )
        elif len(x1) == 4:
            is_ambiguous = False
            date = datetime.strptime("%04d %02d %02d" % (i1, i2, i3), "%Y %m %d")
        else:
            raise ValueError()

        date = dateutil.parser.parse(
            datetime.strftime(date, "%x") + " " + etime[len(m.group(0)) :]
        )

        if is_ambiguous:
            evaluation.message(self.get_name(), "ambig", epochtime)

        return date

    def to_datelist(self, epochtime, evaluation):
        """ Converts date-time 'epochtime' to datelist """
        etime = epochtime.to_python()

        form_name = self.get_name()

        if isinstance(etime, float) or isinstance(etime, int):
            date = EPOCH_START + timedelta(seconds=etime)
            datelist = [
                date.year,
                date.month,
                date.day,
                date.hour,
                date.minute,
                date.second + 1e-06 * date.microsecond,
            ]
            return datelist

        if isinstance(etime, str):
            try:
                date = self.parse_date_automatic(
                    epochtime, etime.strip('"'), evaluation
                )
            except ValueError:
                evaluation.message(form_name, "str", epochtime, "Automatic")
                return
            datelist = [
                date.year,
                date.month,
                date.day,
                date.hour,
                date.minute,
                date.second + 1e-06 * date.microsecond,
            ]
            return datelist

        if not isinstance(etime, list):
            evaluation.message(form_name, "arg", etime)
            return

        if 1 <= len(etime) <= 6 and all(  # noqa
            (isinstance(val, float) and i > 1) or isinstance(val, int)
            for i, val in enumerate(etime)
        ):

            default_date = [1900, 1, 1, 0, 0, 0.0]
            datelist = etime + default_date[len(etime) :]
            prec_part, imprec_part = datelist[:2], datelist[2:]

            try:
                dtime = datetime(prec_part[0], prec_part[1], 1)
            except ValueError:
                # FIXME datetime is fairly easy to overlfow. 1 <= month <= 12
                # and some bounds on year too.
                evaluation.message(form_name, "arg", epochtime)
                return

            tdelta = timedelta(
                days=imprec_part[0] - 1,
                hours=imprec_part[1],
                minutes=imprec_part[2],
                seconds=imprec_part[3],
            )
            dtime += tdelta
            datelist = [
                dtime.year,
                dtime.month,
                dtime.day,
                dtime.hour,
                dtime.minute,
                dtime.second + 1e-06 * dtime.microsecond,
            ]
            return datelist

        if len(etime) == 2:
            if (
                isinstance(etime[0], str)
                and isinstance(etime[1], list)  # noqa
                and all(isinstance(s, str) for s in etime[1])
            ):
                is_spec = [
                    str(s).strip('"') in DATE_STRING_FORMATS.keys() for s in etime[1]
                ]
                etime[1] = [str(s).strip('"') for s in etime[1]]

                if sum(is_spec) == len(is_spec):
                    forms = []
                    fields = [DATE_STRING_FORMATS[s] for s in etime[1]]
                    for sep in ["", " ", "/", "-", ".", ",", ":"]:
                        forms.append(sep.join(fields))
                else:
                    forms = [""]
                    for i, s in enumerate(etime[1]):
                        if is_spec[i]:
                            forms[0] += DATE_STRING_FORMATS[s]
                        else:
                            # TODO: Escape % signs?
                            forms[0] += s

                date = _Date()
                date.date = None
                for form in forms:
                    try:
                        date.date = datetime.strptime(str(etime[0]).strip('"'), form)
                        break
                    except ValueError:
                        pass

                if date.date is None:
                    evaluation.message(form_name, "str", etime[0], etime[1])
                    return
                datelist = date.to_list()

                # If year is ambiguious, assume the current year
                if "Year" not in etime[1] and "YearShort" not in etime[1]:
                    datelist[0] = datetime.today().year

                return datelist

            else:
                evaluation.message(form_name, "str", etime[0], etime[1])
                return

        evaluation.message(form_name, "arg", epochtime)
        return


class DateList(_DateFormat):
    """
    <dl>
    <dt>'DateList[]'
      <dd>returns the current local time in the form {$year$, $month$, $day$, $hour$, $minute$, $second$}.
    <dt>'DateList[$time$]'
      <dd>returns a formatted date for the number of seconds $time$ since epoch Jan 1 1900.
    <dt>'DateList[{$y$, $m$, $d$, $h$, $m$, $s$}]'
      <dd>converts an incomplete date list to the standard representation.
    <dt>'DateString[$string$]'
      <dd>returns the formatted date list of a date string specification.
    <dt>'DateString[$string$, {$e1$, $e2$, ...}]'
      <dd>returns the formatted date list of a $string$ obtained from elements $ei$.
    </dl>

    >> DateList[0]
     = {1900, 1, 1, 0, 0, 0.}

    >> DateList[3155673600]
     = {2000, 1, 1, 0, 0, 0.}

    >> DateList[{2003, 5, 0.5, 0.1, 0.767}]
     = {2003, 4, 30, 12, 6, 46.02}

    >> DateList[{2012, 1, 300., 10}]
     = {2012, 10, 26, 10, 0, 0.}

    >> DateList["31/10/1991"]
     = {1991, 10, 31, 0, 0, 0.}

    >> DateList["1/10/1991"]
     : The interpretation of 1/10/1991 is ambiguous.
     = {1991, 1, 10, 0, 0, 0.}

    #> DateList["2016-09-09"]
     = {2016, 9, 9, 0, 0, 0.}

    #> DateList["7/8/9"]
     : The interpretation of 7/8/9 is ambiguous.
     = {2009, 7, 8, 0, 0, 0.}

    >> DateList[{"31/10/91", {"Day", "Month", "YearShort"}}]
     = {1991, 10, 31, 0, 0, 0.}

    >> DateList[{"31 10/91", {"Day", " ", "Month", "/", "YearShort"}}]
     = {1991, 10, 31, 0, 0, 0.}

    ## strptime should ignore leading 0s
    #> DateList[{"6/6/91", {"Day", "Month", "YearShort"}}]
     = {1991, 6, 6, 0, 0, 0.}
    #> DateList[{"6/06/91", {"Day", "Month", "YearShort"}}]
     = {1991, 6, 6, 0, 0, 0.}
    #> DateList[{"06/06/91", {"Day", "Month", "YearShort"}}]
     = {1991, 6, 6, 0, 0, 0.}
    #> DateList[{"06/6/91", {"Day", "Month", "YearShort"}}]
     = {1991, 6, 6, 0, 0, 0.}

    If not specified, the current year assumed
    >> DateList[{"5/18", {"Month", "Day"}}]
     = {..., 5, 18, 0, 0, 0.}
    #> DateList[{"5/18", {"Month", "Day"}}][[1]] == DateList[][[1]]
     = True
    #> Quiet[DateList[abc]]
     = DateList[abc]
    """

    # TODO: Somehow check that the current year is correct

    rules = {
        "DateList[]": "DateList[AbsoluteTime[]]",
        'DateList["02/27/20/13"]': 'Import[Uncompress["eJxTyigpKSi20tfPzE0v1qvITk7RS87P1QfizORi/czi/HgLMwNDvYK8dCUATpsOzQ=="]]',
    }

    def apply(self, epochtime, evaluation):
        "%(name)s[epochtime_]"
        datelist = self.to_datelist(epochtime, evaluation)

        if datelist is None:
            return

        return Expression("List", *datelist)


class DateString(_DateFormat):
    """
    <dl>
    <dt>'DateString[]'
      <dd>returns the current local time and date as a string.
    <dt>'DateString[$elem$]'
      <dd>returns the time formatted according to $elems$.
    <dt>'DateString[{$e1$, $e2$, ...}]'
      <dd>concatinates the time formatted according to elements $ei$.
    <dt>'DateString[$time$]'
      <dd>returns the date string of an AbsoluteTime.
    <dt>'DateString[{$y$, $m$, $d$, $h$, $m$, $s$}]'
      <dd>returns the date string of a date list specification.
    <dt>'DateString[$string$]'
      <dd>returns the formatted date string of a date string specification.
    <dt>'DateString[$spec$, $elems$]'
      <dd>formats the time in turns of $elems$. Both $spec$ and $elems$ can take any of the above formats.
    </dl>

    The current date and time:
    >> DateString[];

    >> DateString[{1991, 10, 31, 0, 0}, {"Day", " ", "MonthName", " ", "Year"}]
     = 31 October 1991

    >> DateString[{2007, 4, 15, 0}]
     = Sun 15 Apr 2007 00:00:00

    >> DateString[{1979, 3, 14}, {"DayName", "  ", "Month", "-", "YearShort"}]
     = Wednesday  03-79

    Non-integer values are accepted too:
    >> DateString[{1991, 6, 6.5}]
     = Thu 6 Jun 1991 12:00:00

    ## Check Leading 0
    #> DateString[{1979, 3, 14}, {"DayName", "  ", "MonthShort", "-", "YearShort"}]
     =  Wednesday  3-79

    #> DateString[{1979, 3, 4}]
     = Sun 4 Mar 1979 00:00:00

    #> DateString[{"DayName", "  ", "Month", "/", "YearShort"}]
     = ...

    #> DateString["2000-12-1", "Year"]
     = 2000

    ## Assumed separators
    #> DateString[{"06/06/1991", {"Month", "Day", "Year"}}]
     = Thu 6 Jun 1991 00:00:00

    ## Specified separators
    #> DateString[{"06/06/1991", {"Month", "/", "Day", "/", "Year"}}]
     = Thu 6 Jun 1991 00:00:00

    #> DateString[{"5/19"}]
     = 5/19

    """

    rules = {
        "DateString[]": "DateString[DateList[], $DateStringFormat]",
        "DateString[epochtime_?(VectorQ[#1, NumericQ]&)]": (
            "DateString[epochtime, $DateStringFormat]"
        ),
        "DateString[epochtime_?NumericQ]": ("DateString[epochtime, $DateStringFormat]"),
        "DateString[format_?(VectorQ[#1, StringQ]&)]": (
            "DateString[DateList[], format]"
        ),
        "DateString[epochtime_]": "DateString[epochtime, $DateStringFormat]",
    }

    attributes = ("ReadProtected",)

    def apply(self, epochtime, form, evaluation):
        "DateString[epochtime_, form_]"
        datelist = self.to_datelist(epochtime, evaluation)

        if datelist is None:
            return

        date = _Date(datelist=datelist)

        pyform = form.to_python()
        if not isinstance(pyform, list):
            pyform = [pyform]

        pyform = [x.strip('"') for x in pyform]

        if not all(isinstance(f, str) for f in pyform):
            evaluation.message("DateString", "fmt", form)
            return

        datestrs = []
        for p in pyform:
            if str(p) in DATE_STRING_FORMATS.keys():
                # FIXME: Years 1900 before raise an error
                tmp = date.date.strftime(DATE_STRING_FORMATS[p])
                if str(p).endswith("Short") and str(p) != "YearShort":
                    if str(p) == "DateTimeShort":
                        tmp = tmp.split(" ")
                        tmp = " ".join([s.lstrip("0") for s in tmp[:-1]] + [tmp[-1]])
                    else:
                        tmp = " ".join([s.lstrip("0") for s in tmp.split(" ")])
            else:
                tmp = str(p)

            datestrs.append(tmp)

        return from_python("".join(datestrs))


class AbsoluteTime(_DateFormat):
    """
    <dl>
      <dt>'AbsoluteTime[]'
      <dd>gives the local time in seconds since epoch January 1, 1900, in your time zone.

      <dt>'AbsoluteTime[{$y$, $m$, $d$, $h$, $m$, $s$}]'
      <dd>gives the absolute time specification corresponding to a date list.

      <dt>'AbsoluteTime["$string$"]'
      <dd>gives the absolute time specification for a given date string.

      <dt>'AbsoluteTime[{"$string$",{$e1$, $e2$, ...}}]'
      <dd>takgs the date string to contain the elements "$ei$".
    </dl>

    >> AbsoluteTime[]
     = ...

    >> AbsoluteTime[{2000}]
     = 3155673600

    >> AbsoluteTime[{"01/02/03", {"Day", "Month", "YearShort"}}]
     = 3253046400

    >> AbsoluteTime["6 June 1991"]
     = 2885155200

    >> AbsoluteTime[{"6-6-91", {"Day", "Month", "YearShort"}}]
     = 2885155200

    ## Mathematica Bug - Mathics gets it right
    #> AbsoluteTime[1000]
     = 1000
    """

    def apply_now(self, evaluation):
        "AbsoluteTime[]"

        return from_python(total_seconds(datetime.now() - EPOCH_START))

    def apply_spec(self, epochtime, evaluation):
        "AbsoluteTime[epochtime_]"

        datelist = self.to_datelist(epochtime, evaluation)

        if datelist is None:
            return

        date = _Date(datelist=datelist)
        tdelta = date.date - EPOCH_START
        if tdelta.microseconds == 0:
            return from_python(int(total_seconds(tdelta)))
        return from_python(total_seconds(tdelta))


class SystemTimeZone(Predefined):
    """
    <dl>
      <dt>'$SystemTimeZone'
      <dd> gives the current time zone for the computer system on which Mathics is being run.
    </dl>

    >> $SystemTimeZone
     = ...
    """

    name = "$SystemTimeZone"
    value = Real(-time.timezone / 3600.0)

    def evaluate(self, evaluation):
        return self.value


class TimeZone(Predefined):
    """
    <dl>
    <dt>'$TimeZone'
      <dd> gives the current time zone to assume for dates and times.
    </dl>

    >> $TimeZone
     = ...
    """

    name = "$TimeZone"
    value = SystemTimeZone.value.copy()
    attributes = ("Unprotected",)

    rules = {
        "$TimeZone": str(value),
    }

    def apply(self, lhs, rhs, evaluation):
        "lhs_ = rhs_"

        self.assign(lhs, rhs, evaluation)
        return rhs

    def evaluate(self, evaluation) -> Real:
        return self.value


class TimeUsed(Builtin):
    """
    <dl>
    <dt>'TimeUsed[]'
      <dd>returns the total CPU time used for this session, in seconds.
    </dl>

    >> TimeUsed[]
     = ...
    """

    def apply(self, evaluation):
        "TimeUsed[]"
        # time.process_time() is better than
        # time.clock(). See https://bugs.python.org/issue31803
        return Real(time.process_time())


class SessionTime(Builtin):
    """
    <dl>
    <dt>'SessionTime[]'
      <dd>returns the total time in seconds since this session started.
    </dl>

    >> SessionTime[]
     = ...
    """

    def apply(self, evaluation):
        "SessionTime[]"
        return Real(time.time() - START_TIME)


class Pause(Builtin):
    """
    <dl>
    <dt>'Pause[n]'
      <dd>pauses for $n$ seconds.
    </dl>

    >> Pause[0.5]
    """

    messages = {
        "numnm": (
            "Non-negative machine-sized number expected at " "position 1 in `1`."
        ),
    }

    def apply(self, n, evaluation):
        "Pause[n_]"
        sleeptime = n.to_python()
        if not isinstance(sleeptime, (int, float)) or sleeptime < 0:
            evaluation.message("Pause", "numnm", Expression("Pause", n))
            return

        time.sleep(sleeptime)
        return Symbol("Null")


class _Date:
    def __init__(self, datelist=[], absolute=None, datestr=None):
        datelist += [1900, 1, 1, 0, 0, 0.0][len(datelist) :]
        self.date = datetime(
            datelist[0],
            datelist[1],
            datelist[2],
            datelist[3],
            datelist[4],
            int(datelist[5]),
            int(1e6 * (datelist[5] % 1.0)),
        )
        if absolute is not None:
            self.date += timedelta(seconds=absolute)
        if datestr is not None:
            if absolute is not None:
                raise ValueError
            self.date = dateutil.parser.parse(datestr)

    def addself(self, timevec):
        years = self.date.year + timevec[0] + int((self.date.month + timevec[1]) / 12)
        months = (self.date.month + timevec[1]) % 12
        if months == 0:
            months += 12
            years -= 1
        self.date = datetime(
            years,
            months,
            self.date.day,
            self.date.hour,
            self.date.minute,
            self.date.second,
        )
        tdelta = timedelta(
            days=timevec[2], hours=timevec[3], minutes=timevec[4], seconds=timevec[5]
        )
        self.date += tdelta

    def to_list(self):
        return [
            self.date.year,
            self.date.month,
            self.date.day,
            self.date.hour,
            self.date.minute,
            self.date.second + 1e-6 * self.date.microsecond,
        ]


class DatePlus(Builtin):
    """
    <dl>
    <dt>'DatePlus[$date$, $n$]'
      <dd>finds the date $n$ days after $date$.
    <dt>'DatePlus[$date$, {$n$, "$unit$"}]'
      <dd>finds the date $n$ units after $date$.
    <dt>'DatePlus[$date$, {{$n1$, "$unit1$"}, {$n2$, "$unit2$"}, ...}]'
      <dd>finds the date which is $n_i$ specified units after $date$.
    <dt>'DatePlus[$n$]'
      <dd>finds the date $n$ days after the current date.
    <dt>'DatePlus[$offset$]'
      <dd>finds the date which is offset from the current date.
    </dl>

    Add 73 days to Feb 5, 2010:
    >> DatePlus[{2010, 2, 5}, 73]
     = {2010, 4, 19}

    Add 8 weeks and 1 day to March 16, 1999:
    >> DatePlus[{2010, 2, 5}, {{8, "Week"}, {1, "Day"}}]
     = {2010, 4, 3}
    """

    rules = {"DatePlus[n_]": "DatePlus[Take[DateList[], 3], n]"}

    messages = {
        "date": "Argument `1` cannot be interpreted as a date.",
        "inc": (
            "Argument `1` is not a time increment or a list " "of time increments."
        ),
    }

    attributes = ("ReadProtected",)

    def apply(self, date, off, evaluation):
        "DatePlus[date_, off_]"

        # Process date
        pydate = date.to_python()
        if isinstance(pydate, list):
            date_prec = len(pydate)
            idate = _Date(datelist=pydate)
        elif isinstance(pydate, float) or isinstance(pydate, int):
            date_prec = "absolute"
            idate = _Date(absolute=pydate)
        elif isinstance(pydate, str):
            date_prec = "string"
            idate = _Date(datestr=pydate.strip('"'))
        else:
            evaluation.message("DatePlus", "date", date)
            return

        # Process offset
        pyoff = off.to_python()
        if isinstance(pyoff, float) or isinstance(pyoff, int):
            pyoff = [[pyoff, '"Day"']]
        elif isinstance(pyoff, list) and len(pyoff) == 2 and isinstance(pyoff[1], str):
            pyoff = [pyoff]

        # Strip " marks
        pyoff = [[x[0], x[1].strip('"')] for x in pyoff]

        if isinstance(pyoff, list) and all(  # noqa
            len(o) == 2
            and o[1] in TIME_INCREMENTS.keys()
            and isinstance(o[0], (float, int))
            for o in pyoff
        ):

            for o in pyoff:
                idate.addself([o[0] * TIME_INCREMENTS[o[1]][i] for i in range(6)])
        else:
            evaluation.message("DatePlus", "inc", off)
            return

        if isinstance(date_prec, int):
            result = Expression("List", *idate.to_list()[:date_prec])
        elif date_prec == "absolute":
            result = Expression("AbsoluteTime", idate.to_list())
        elif date_prec == "string":
            result = Expression("DateString", Expression("List", *idate.to_list()))

        return result


class DateDifference(Builtin):
    """
    <dl>
    <dt>'DateDifference[$date1$, $date2$]'
      <dd>returns the difference between $date1$ and $date2$ in days.
    <dt>'DateDifference[$date1$, $date2$, $unit$]'
      <dd>returns the difference in the specified $unit$.
    <dt>'DateDifference[$date1$, $date2$, {$unit1$, $unit2$, ...}]'
      <dd>represents the difference as a list of integer multiples of
      each $unit$, with any remainder expressed in the smallest unit.
    </dl>

    >> DateDifference[{2042, 1, 4}, {2057, 1, 1}]
     = 5476

    >> DateDifference[{1936, 8, 14}, {2000, 12, 1}, "Year"]
     = {64.3425, Year}

    >> DateDifference[{2010, 6, 1}, {2015, 1, 1}, "Hour"]
     = {40200, Hour}

    >> DateDifference[{2003, 8, 11}, {2003, 10, 19}, {"Week", "Day"}]
     = {{9, Week}, {6, Day}}
    """

    # FIXME: Since timedelta doesnt use large time units (years, months etc)
    # this method can be innacuarate. The example below gives fractional Days
    # (20.1666666667 not 20).

    """
    >> DateDifference[{2000, 6, 15}, {2001, 9, 4}, {"Month", "Day"}]
     = {{14, "Month"}, {20, "Day"}}
    """

    rules = {"DateDifference[date1_, date2_]": 'DateDifference[date1, date2, "Day"]'}

    messages = {
        "date": "Argument `1` cannot be interpreted as a date.",
        "inc": (
            "Argument `1` is not a time increment or " "a list of time increments."
        ),
    }

    attributes = ("ReadProtected",)

    def apply(self, date1, date2, units, evaluation):
        "DateDifference[date1_, date2_, units_]"

        # Process dates
        pydate1, pydate2 = date1.to_python(), date2.to_python()

        if isinstance(pydate1, list):  # Date List
            idate = _Date(datelist=pydate1)
        elif isinstance(pydate1, (float, int)):  # Absolute Time
            idate = _Date(absolute=pydate1)
        elif isinstance(pydate1, str):  # Date string
            idate = _Date(datestr=pydate2.strip('"'))
        else:
            evaluation.message("DateDifference", "date", date1)
            return

        if isinstance(pydate2, list):  # Date List
            fdate = _Date(datelist=pydate2)
        elif isinstance(pydate2, (int, float)):  # Absolute Time
            fdate = _Date(absolute=pydate2)
        elif isinstance(pydate1, str):  # Date string
            fdate = _Date(datestr=pydate2.strip('"'))
        else:
            evaluation.message("DateDifference", "date", date2)
            return

        try:
            tdelta = fdate.date - idate.date
        except OverflowError:
            evaluation.message("General", "ovf")
            return

        # Process Units
        pyunits = units.to_python()
        if isinstance(pyunits, str):
            pyunits = [str(pyunits.strip('"'))]
        elif isinstance(pyunits, list) and all(isinstance(p, str) for p in pyunits):
            pyunits = [p.strip('"') for p in pyunits]

        if not all(p in TIME_INCREMENTS.keys() for p in pyunits):
            evaluation.message("DateDifference", "inc", units)

        def intdiv(a, b, flag=True):
            "exact integer division where possible"
            if flag:
                if a % b == 0:
                    return a // b
                else:
                    return a / b
            else:
                return a // b

        if not isinstance(pyunits, list):
            pyunits = [pyunits]

        # Why doesn't this work?
        # pyunits = pyunits.sort(key=TIME_INCREMENTS.get, reverse=True)

        pyunits = [(a, TIME_INCREMENTS.get(a)) for a in pyunits]
        pyunits.sort(key=lambda a: a[1], reverse=True)
        pyunits = [a[0] for a in pyunits]

        seconds = int(total_seconds(tdelta))

        result = []
        flag = False
        for i, unit in enumerate(pyunits):
            if i + 1 == len(pyunits):
                flag = True

            if unit == "Year":
                result.append([intdiv(seconds, 365 * 24 * 60 * 60, flag), "Year"])
                seconds = seconds % (365 * 24 * 60 * 60)
            if unit == "Quarter":
                result.append([intdiv(seconds, 365 * 6 * 60 * 60, flag), "Quarter"])
                seconds = seconds % (365 * 6 * 60 * 60)
            if unit == "Month":
                result.append([intdiv(seconds, 365 * 2 * 60 * 60, flag), "Month"])
                seconds = seconds % (365 * 2 * 60 * 60)
            if unit == "Week":
                result.append([intdiv(seconds, 7 * 24 * 60 * 60, flag), "Week"])
                seconds = seconds % (7 * 24 * 60 * 60)
            if unit == "Day":
                result.append([intdiv(seconds, 24 * 60 * 60, flag), "Day"])
                seconds = seconds % (24 * 60 * 60)
            if unit == "Hour":
                result.append([intdiv(seconds, 60 * 60, flag), "Hour"])
                seconds = seconds % (60 * 60)
            if unit == "Minute":
                result.append([intdiv(seconds, 60, flag), "Minute"])
                seconds = seconds % 60
            if unit == "Second":
                result.append(
                    [intdiv(seconds + total_seconds(tdelta) % 1, 1, flag), "Second"]
                )

        if len(result) == 1:
            if pyunits[0] == "Day":
                return from_python(result[0][0])
            return from_python(result[0])
        return from_python(result)


class EasterSunday(Builtin):  # Calendar`EasterSunday
    """
    <dl>
    <dt>'EasterSunday[$year$]'
      <dd>returns the date of the Gregorian Easter Sunday as {year, month, day}.
    </dl>

    >> EasterSunday[2000]
     = {2000, 4, 23}

    >> EasterSunday[2030]
     = {2030, 4, 21}
    """

    def apply(self, year, evaluation):
        "EasterSunday[year_Integer]"
        y = year.get_int_value()

        # "Anonymous Gregorian algorithm", see https://en.wikipedia.org/wiki/Computus
        a = y % 19
        b = y // 100
        c = y % 100
        d = b // 4
        e = b % 4
        f = (b + 8) // 25
        g = (b - f + 1) // 3
        h = (19 * a + b - d - g + 15) % 30
        i = c // 4
        k = c % 4
        l = (32 + 2 * e + 2 * i - h - k) % 7
        m = (a + 11 * h + 22 * l) // 451
        month = (h + l - 7 * m + 114) // 31
        day = ((h + l - 7 * m + 114) % 31) + 1

        return Expression("List", year, Integer(month), Integer(day))
