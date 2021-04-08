#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pickle
from queue import Queue

import os
import sys
from threading import Thread, stack_size as set_thread_stack_size

from typing import Tuple

from mathics_scanner import TranslateError

from mathics import settings
from mathics.core.expression import ensure_context, KeyComparable, SymbolAborted, SymbolList, SymbolNull

FORMATS = [
    "StandardForm",
    "FullForm",
    "TraditionalForm",
    "OutputForm",
    "InputForm",
    "TeXForm",
    "MathMLForm",
    "MatrixForm",
    "TableForm",
]


class EvaluationInterrupt(Exception):
    pass


class AbortInterrupt(EvaluationInterrupt):
    pass


class TimeoutInterrupt(EvaluationInterrupt):
    pass


class ReturnInterrupt(EvaluationInterrupt):
    def __init__(self, expr):
        self.expr = expr


class BreakInterrupt(EvaluationInterrupt):
    pass


class ContinueInterrupt(EvaluationInterrupt):
    pass


class WLThrowInterrupt(EvaluationInterrupt):
    def __init__(self, value, tag=None):
        self.tag = tag
        self.value = value


def _thread_target(request, queue) -> None:
    try:
        result = request()
        queue.put((True, result))
    except BaseException:
        exc_info = sys.exc_info()
        queue.put((False, exc_info))


# MAX_RECURSION_DEPTH gives the maximum value allowed for $RecursionLimit. it's usually set to its
# default settings.DEFAULT_MAX_RECURSION_DEPTH.

MAX_RECURSION_DEPTH = max(
    settings.DEFAULT_MAX_RECURSION_DEPTH,
    int(os.getenv("MATHICS_MAX_RECURSION_DEPTH", settings.DEFAULT_MAX_RECURSION_DEPTH)),
)


def python_recursion_depth(n) -> int:
    # convert Mathics recursion depth to Python recursion depth. this estimates how many Python calls
    # we need at worst to process one Mathics recursion.
    return 200 + 30 * n


def python_stack_size(n) -> int:  # n is a Mathics recursion depth
    # python_stack_frame_size is the (maximum) number of bytes Python needs for one call on the stack.
    python_stack_frame_size = 512  # value estimated experimentally
    return python_recursion_depth(n) * python_stack_frame_size


def set_python_recursion_limit(n) -> None:
    "Sets the required python recursion limit given $RecursionLimit value"
    python_depth = python_recursion_depth(n)
    sys.setrecursionlimit(python_depth)
    if sys.getrecursionlimit() != python_depth:
        raise OverflowError


def run_with_timeout_and_stack(request, timeout, evaluation):
    """
    interrupts evaluation after a given time period. Provides a suitable stack environment.
    """

    # only use set_thread_stack_size if max recursion depth was changed via the environment variable
    # MATHICS_MAX_RECURSION_DEPTH. if it is set, we always use a thread, even if timeout is None, in
    # order to be able to set the thread stack size.

    if MAX_RECURSION_DEPTH > settings.DEFAULT_MAX_RECURSION_DEPTH:
        set_thread_stack_size(python_stack_size(MAX_RECURSION_DEPTH))
    elif timeout is None:
        return request()

    queue = Queue(maxsize=1)  # stores the result or exception
    thread = Thread(target=_thread_target, args=(request, queue))
    thread.start()

    # Thead join(timeout) can leave zombie threads (we are the parent)
    # when a time out occurs, but the thread hasn't terminated.  See
    # https://docs.python.org/3/library/multiprocessing.shared_memory.html
    # for a detailed discussion of this.
    #
    # To reduce this problem, we make use of specific properties of
    # the Mathics evaluator: if we set "evaluation.timeout", the
    # next call to "Expression.evaluate" in the thread will finish it
    # immediately.
    #
    # However this still will not terminate long-running processes
    # in Sympy or or libraries called by Mathics that might hang or run
    # for a long time.
    thread.join(timeout)
    if thread.is_alive():
        evaluation.timeout = True
        while thread.is_alive():
            pass
        evaluation.timeout = False
        evaluation.stopped = False
        raise TimeoutInterrupt()

    success, result = queue.get()
    if success:
        return result
    else:
        raise result[0].with_traceback(result[1], result[2])


class Out(KeyComparable):
    def __init__(self) -> None:
        self.is_message = False
        self.is_print = False
        self.text = ""

    def get_sort_key(self) -> Tuple[bool, bool, str]:
        return (self.is_message, self.is_print, self.text)


class Message(Out):
    def __init__(self, symbol, tag, text: str) -> None:
        super(Message, self).__init__()
        self.is_message = True
        self.symbol = symbol
        self.tag = tag
        self.text = text

    def __str__(self) -> str:
        return "{}::{}: {}".format(self.symbol, self.tag, self.text)

    def __eq__(self, other) -> bool:
        return self.is_message == other.is_message and self.text == other.text

    def get_data(self):
        return {
            "message": True,
            "symbol": self.symbol,
            "tag": self.tag,
            "prefix": "%s::%s" % (self.symbol, self.tag),
            "text": self.text,
        }


class Print(Out):
    def __init__(self, text) -> None:
        super(Print, self).__init__()
        self.is_print = True
        self.text = text

    def __str__(self) -> str:
        return self.text

    def __eq__(self, other) -> bool:
        return self.is_message == other.is_message and self.text == other.text

    def get_data(self):
        return {
            "message": False,
            "text": self.text,
        }


class Result(object):
    def __init__(self, out, result, line_no, last_eval=None) -> None:
        self.out = out
        self.result = result
        self.line_no = line_no
        self.last_eval = last_eval

    def get_data(self):
        return {
            "out": [out.get_data() for out in self.out],
            "result": self.result,
            "line": self.line_no,
        }


class Output(object):
    def max_stored_size(self, settings) -> int:
        return settings.MAX_STORED_SIZE

    def out(self, out):
        pass

    def clear(self, wait):
        raise NotImplementedError

    def display(self, data, metadata):
        raise NotImplementedError


class Evaluation(object):
    def __init__(
        self, definitions=None, output=None, format="text", catch_interrupt=True
    ) -> None:
        from mathics.core.definitions import Definitions
        from mathics.core.expression import Symbol

        if definitions is None:
            definitions = Definitions()
        self.definitions = definitions
        self.recursion_depth = 0
        self.timeout = False
        self.timeout_queue = []
        self.stopped = False
        self.out = []
        self.output = output if output else Output()
        self.listeners = {}
        self.options = None
        self.predetermined_out = None

        self.quiet_all = False
        self.format = format
        self.catch_interrupt = catch_interrupt

        self.SymbolNull = SymbolNull

        # status of last evaluate
        self.exc_result = self.SymbolNull
        self.last_eval = None

    def parse(self, query):
        "Parse a single expression and print the messages."
        from mathics.core.parser import MathicsSingleLineFeeder

        return self.parse_feeder(MathicsSingleLineFeeder(query))

    def parse_evaluate(self, query, timeout=None):
        expr = self.parse(query)
        if expr is not None:
            return self.evaluate(expr, timeout)

    def parse_feeder(self, feeder):
        return self.parse_feeder_returning_code(feeder)[0]

    def parse_feeder_returning_code(self, feeder):
        "Parse a single expression from feeder and print the messages."
        from mathics.core.parser.util import parse_returning_code

        try:
            result, source_code = parse_returning_code(self.definitions, feeder)
        except TranslateError:
            self.recursion_depth = 0
            self.stopped = False
            source_code = ""
            result = None
        feeder.send_messages(self)
        return result, source_code

    def evaluate(self, query, timeout=None, format=None):
        """Evaluate a Mathics expression and return the
        result of evaluation.

        On return self.exc_result will contain status of various
        exception type of result like $Aborted, Overflow, Break, or Continue.
        If none of the above applies self.exc_result is Null
        """
        from mathics.core.expression import Symbol, Expression
        from mathics.core.rules import Rule

        self.recursion_depth = 0
        self.timeout = False
        self.stopped = False
        self.exc_result = self.SymbolNull
        self.last_eval = None
        if format is None:
            format = self.format

        line_no = self.definitions.get_line_no()
        line_no += 1
        self.definitions.set_line_no(line_no)

        history_length = self.definitions.get_history_length()

        result = None

        def check_io_hook(hook):
            return len(self.definitions.get_ownvalues(hook)) > 0

        def evaluate():
            if history_length > 0:
                self.definitions.add_rule("In", Rule(Expression("In", line_no), query))
            if check_io_hook("System`$Pre"):
                self.last_eval = Expression("System`$Pre", query).evaluate(self)
            else:
                self.last_eval = query.evaluate(self)

            if check_io_hook("System`$Post"):
                self.last_eval = Expression("System`$Post", self.last_eval).evaluate(
                    self
                )
            if history_length > 0:
                if self.predetermined_out is not None:
                    out_result = self.predetermined_out
                    self.predetermined_out = None
                else:
                    out_result = self.last_eval

                stored_result = self.get_stored_result(out_result)
                self.definitions.add_rule(
                    "Out", Rule(Expression("Out", line_no), stored_result)
                )
            if self.last_eval != self.SymbolNull:
                if check_io_hook("System`$PrePrint"):
                    self.last_eval = Expression(
                        "System`$PrePrint", self.last_eval
                    ).evaluate(self)
                return self.format_output(self.last_eval, format)
            else:
                self.exec_result = self.SymbolNull
                return None

        try:
            try:
                result = run_with_timeout_and_stack(evaluate, timeout, self)
            except KeyboardInterrupt:
                if self.catch_interrupt:
                    self.exc_result = SymbolAborted
                else:
                    raise
            except ValueError as exc:
                text = str(exc)
                if (
                    text == "mpz.pow outrageous exponent"
                    or text == "mpq.pow outrageous exp num"  # noqa
                ):
                    self.message("General", "ovfl")
                    self.exc_result = Expression("Overflow")
                else:
                    raise
            except WLThrowInterrupt as ti:
                if ti.tag:
                    self.exc_result = Expression(
                        "Hold", Expression("Throw", ti.value, ti.tag)
                    )
                else:
                    self.exc_result = Expression("Hold", Expression("Throw", ti.value))
                self.message("Throw", "nocatch", self.exc_result)
            except OverflowError:
                self.message("General", "ovfl")
                self.exc_result = Expression("Overflow")
            except BreakInterrupt:
                self.message("Break", "nofdw")
                self.exc_result = Expression("Hold", Expression("Break"))
            except ContinueInterrupt:
                self.message("Continue", "nofdw")
                self.exc_result = Expression("Hold", Expression("Continue"))
            except TimeoutInterrupt:
                self.stopped = False
                self.timeout = True
                self.message("General", "timeout")
                self.exc_result = SymbolAborted
            except AbortInterrupt:  # , error:
                self.exc_result = SymbolAborted
            except ReturnInterrupt as ret:
                self.exc_result = ret.expr

            if self.exc_result is not None:
                self.recursion_depth = 0
                if self.exc_result != self.SymbolNull:
                    result = self.format_output(self.exc_result, format)

            result = Result(self.out, result, line_no, self.last_eval)
            self.out = []
        finally:
            self.stop()

        history_length = self.definitions.get_history_length()

        line = line_no - history_length
        while line > 0:
            unset_in = self.definitions.unset("In", Expression("In", line))
            unset_out = self.definitions.unset("Out", Expression("Out", line))
            if not (unset_in or unset_out):
                break
            line -= 1
        return result

    def get_stored_result(self, eval_result):
        """Return `eval_result` stripped of any format, e.g. FullForm, MathML, TeX
        that it might have been wrapped in.
        """
        if eval_result.has_form(FORMATS, 1):
            return eval_result.leaves[0]

        return eval_result

    def stop(self) -> None:
        self.stopped = True

    def format_output(self, expr, format=None):
        if format is None:
            format = self.format

        if isinstance(format, dict):
            return dict((k, self.format_output(expr, f)) for k, f in format.items())

        from mathics.core.expression import Expression, BoxError

        if format == "text":
            result = expr.format(self, "System`OutputForm")
        elif format == "xml":
            result = Expression("StandardForm", expr).format(self, "System`MathMLForm")
        elif format == "tex":
            result = Expression("StandardForm", expr).format(self, "System`TeXForm")
        elif format == "unformatted":
            self.exc_result = None
            return expr
        else:
            raise ValueError

        try:
            boxes = result.boxes_to_text(evaluation=self)
        except BoxError:
            self.message(
                "General", "notboxes", Expression("FullForm", result).evaluate(self)
            )
            boxes = None
        return boxes

    def set_quiet_messages(self, messages) -> None:
        from mathics.core.expression import Expression

        value = Expression(SymbolList, *messages)
        self.definitions.set_ownvalue("Internal`$QuietMessages", value)

    def get_quiet_messages(self):
        from mathics.core.expression import Expression

        value = self.definitions.get_definition("Internal`$QuietMessages").ownvalues
        if value:
            try:
                value = value[0].replace
            except AttributeError:
                return []
        if not isinstance(value, Expression):
            return []
        return value.leaves

    def message(self, symbol, tag, *args) -> None:
        from mathics.core.expression import String, Symbol, Expression, from_python

        # Allow evaluation.message('MyBuiltin', ...) (assume
        # System`MyBuiltin)
        symbol = ensure_context(symbol)
        quiet_messages = set(self.get_quiet_messages())

        pattern = Expression("MessageName", Symbol(symbol), String(tag))

        if pattern in quiet_messages or self.quiet_all:
            return

        # Shorten the symbol's name according to the current context
        # settings. This makes sure we print the context, if it would
        # be necessary to find the symbol that this message is
        # attached to.
        symbol_shortname = self.definitions.shorten_name(symbol)

        if settings.DEBUG_PRINT:
            print("MESSAGE: %s::%s (%s)" % (symbol_shortname, tag, args))

        text = self.definitions.get_value(symbol, "System`Messages", pattern, self)
        if text is None:
            pattern = Expression("MessageName", Symbol("General"), String(tag))
            text = self.definitions.get_value(
                "System`General", "System`Messages", pattern, self
            )

        if text is None:
            text = String("Message %s::%s not found." % (symbol_shortname, tag))

        text = self.format_output(
            Expression("StringForm", text, *(from_python(arg) for arg in args)), "text"
        )

        self.out.append(Message(symbol_shortname, tag, text))
        self.output.out(self.out[-1])

    def print_out(self, text) -> None:
        from mathics.core.expression import from_python

        text = self.format_output(from_python(text), "text")

        self.out.append(Print(text))
        self.output.out(self.out[-1])
        if settings.DEBUG_PRINT:
            print("OUT: " + text)

    def error(self, symbol, tag, *args) -> None:
        # Temporarily reset the recursion limit, to allow the message being
        # formatted
        self.recursion_depth, depth = 0, self.recursion_depth
        try:
            self.message(symbol, tag, *args)
        finally:
            self.recursion_depth = depth
        raise AbortInterrupt

    def error_args(self, symbol, given, *needed) -> None:
        self.message_args(symbol, given, *needed)
        raise AbortInterrupt

    def message_args(self, symbol, given, *needed) -> None:
        from mathics.core.expression import Symbol

        if len(needed) == 1:
            needed = needed[0]
            if given > 1 and needed > 1:
                self.message(symbol, "argrx", Symbol(symbol), given, needed)
            elif given == 1:
                self.message(symbol, "argr", Symbol(symbol), needed)
            elif needed == 1:
                self.message(symbol, "argx", Symbol(symbol), given)
        elif len(needed) == 2:
            if given == 1:
                self.message(symbol, "argtu", Symbol(symbol), *needed)
            else:
                self.message(symbol, "argt", Symbol(symbol), *needed)
        else:
            raise NotImplementedError

    def check_stopped(self) -> None:
        if self.stopped:
            raise TimeoutInterrupt

    def inc_recursion_depth(self) -> None:
        self.check_stopped()
        limit = self.definitions.get_config_value(
            "$RecursionLimit", MAX_RECURSION_DEPTH
        )
        if limit is not None:
            if limit < 20:
                limit = 20
            self.recursion_depth += 1
            if self.recursion_depth > limit:
                self.error("$RecursionLimit", "reclim", limit)

    def dec_recursion_depth(self) -> None:
        self.recursion_depth -= 1

    def add_listener(self, tag, listener) -> None:
        existing = self.listeners.get(tag)
        if existing is None:
            existing = self.listeners[tag] = []
        existing.insert(0, listener)

    def remove_listener(self, tag, listener) -> None:
        self.listeners.get(tag).remove(listener)

    def publish(self, tag, *args, **kwargs) -> None:
        listeners = self.listeners.get(tag, [])
        for listener in listeners:
            if listener(*args, **kwargs):
                break
