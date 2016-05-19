#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import print_function
from __future__ import absolute_import

import six
import six.moves.cPickle as pickle
from six.moves.queue import Queue

import sys
from threading import Thread
import itertools

from mathics import settings
from mathics.core.expression import ensure_context, KeyComparable

FORMATS = ['StandardForm', 'FullForm', 'TraditionalForm',
           'OutputForm', 'InputForm',
           'TeXForm', 'MathMLForm',
           'MatrixForm', 'TableForm']


def _interleave(*gens):  # interleaves over n generators of even or uneven lengths
    active = [gen for gen in gens]
    while len(active) > 0:
        i = 0
        while i < len(active):
            try:
                yield next(active[i])
                i += 1
            except StopIteration:
                del active[i]


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


def _thread_target(request, queue):
    try:
        result = request()
        queue.put((True, result))
    except BaseException:
        exc_info = sys.exc_info()
        queue.put((False, exc_info))


def run_with_timeout(request, timeout):
    '''
    interrupts evaluation after a given time period.
    '''
    if timeout is None:
        return request()

    queue = Queue(maxsize=1)   # stores the result or exception
    thread = Thread(target=_thread_target, args=(request, queue))
    thread.start()

    thread.join(timeout)
    if thread.is_alive():
        raise TimeoutInterrupt()

    success, result = queue.get()
    if success:
        return result
    else:
        six.reraise(*result)


class Out(KeyComparable):
    def __init__(self):
        self.is_message = False
        self.is_print = False
        self.text = ''

    def get_sort_key(self):
        (self.is_message, self.is_print, self.text)


class Message(Out):
    def __init__(self, symbol, tag, text):
        super(Message, self).__init__()
        self.is_message = True
        self.symbol = symbol
        self.tag = tag
        self.text = text

    def __str__(self):
        return '{}::{}: {}'.format(self.symbol, self.tag, self.text)

    def __eq__(self, other):
        return self.is_message == other.is_message and self.text == other.text

    def get_data(self):
        return {
            'message': True,
            'symbol': self.symbol,
            'tag': self.tag,
            'prefix': '%s::%s' % (self.symbol, self.tag),
            'text': self.text,
        }


class Print(Out):
    def __init__(self, text):
        super(Print, self).__init__()
        self.is_print = True
        self.text = text

    def __str__(self):
        return self.text

    def __eq__(self, other):
        return self.is_message == other.is_message and self.text == other.text

    def get_data(self):
        return {
            'message': False,
            'text': self.text,
        }


class Result(object):
    def __init__(self, out, result, line_no):
        self.out = out
        self.result = result
        self.line_no = line_no

    def get_data(self):
        return {
            'out': [out.get_data() for out in self.out],
            'result': self.result,
            'line': self.line_no,
        }


class Output(object):
    def max_stored_size(self, settings):
        return settings.MAX_STORED_SIZE

    def out(self, out):
        pass

    def clear(self, wait):
        raise NotImplementedError

    def display(self, data, metadata):
        raise NotImplementedError


class Evaluation(object):
    def __init__(self, definitions=None,
                 output=None, format='text', catch_interrupt=True):
        from mathics.core.definitions import Definitions

        if definitions is None:
            definitions = Definitions()
        self.definitions = definitions
        self.recursion_depth = 0
        self.timeout = False
        self.stopped = False
        self.out = []
        self.output = output if output else Output()
        self.listeners = {}
        self.options = None
        self.predetermined_out = None

        self.quiet_all = False
        self.format = format
        self.output_size_limit = None
        self.catch_interrupt = catch_interrupt

    def parse(self, query):
        'Parse a single expression and print the messages.'
        from mathics.core.parser import SingleLineFeeder
        return self.parse_feeder(SingleLineFeeder(query))

    def parse_evaluate(self, query, timeout=None):
        expr = self.parse(query)
        if expr is not None:
            return self.evaluate(expr, timeout)

    def parse_feeder(self, feeder):
        'Parse a single expression from feeder and print the messages.'
        from mathics.core.parser import parse, TranslateError
        try:
            result = parse(self.definitions, feeder)
        except TranslateError as exc:
            self.recursion_depth = 0
            self.stopped = False
            result = None
        feeder.send_messages(self)
        return result

    def evaluate(self, query, timeout=None):
        'Evaluate an expression.'
        from mathics.core.expression import Symbol, Expression
        from mathics.core.rules import Rule

        self.recursion_depth = 0
        self.timeout = False
        self.stopped = False

        line_no = self.definitions.get_line_no()
        line_no += 1
        self.definitions.set_line_no(line_no)

        history_length = self.definitions.get_history_length()

        result = None
        exc_result = None

        def evaluate():
            if history_length > 0:
                self.definitions.add_rule('In', Rule(
                    Expression('In', line_no), query))
            result = query.evaluate(self)
            if history_length > 0:
                if self.predetermined_out is not None:
                    out_result = self.predetermined_out
                    self.predetermined_out = None
                else:
                    out_result = result

                stored_result = self.get_stored_result(out_result)
                self.definitions.add_rule('Out', Rule(
                    Expression('Out', line_no), stored_result))
            if result != Symbol('Null'):
                return self.format_output(result, self.format)
            else:
                return None
        try:
            try:
                result = run_with_timeout(evaluate, timeout)
            except KeyboardInterrupt:
                if self.catch_interrupt:
                    exc_result = Symbol('$Aborted')
                else:
                    raise
            except ValueError as exc:
                text = six.text_type(exc)
                if (text == 'mpz.pow outrageous exponent' or    # noqa
                    text == 'mpq.pow outrageous exp num'):
                    self.message('General', 'ovfl')
                    exc_result = Expression('Overflow')
                else:
                    raise
            except OverflowError:
                self.message('General', 'ovfl')
                exc_result = Expression('Overflow')
            except BreakInterrupt:
                self.message('Break', 'nofdw')
                exc_result = Expression('Hold', Expression('Break'))
            except ContinueInterrupt:
                self.message('Continue', 'nofdw')
                exc_result = Expression('Hold', Expression('Continue'))
            except TimeoutInterrupt:
                self.stopped = False
                self.timeout = True
                self.message('General', 'timeout')
                exc_result = Symbol('$Aborted')
            except AbortInterrupt:  # , error:
                exc_result = Symbol('$Aborted')
            except ReturnInterrupt as ret:
                exc_result = ret.expr
            if exc_result is not None:
                self.recursion_depth = 0
                if exc_result != Symbol('Null'):
                    result = self.format_output(exc_result, self.format)

            result = Result(self.out, result, line_no)
            self.out = []
        finally:
            self.stop()

        history_length = self.definitions.get_history_length()

        line = line_no - history_length
        while line > 0:
            unset_in = self.definitions.unset('In', Expression('In', line))
            unset_out = self.definitions.unset('Out', Expression('Out', line))
            if not (unset_in or unset_out):
                break
            line -= 1
        return result

    def get_stored_result(self, result):
        from mathics.core.expression import Symbol

        # Remove outer format
        if result.has_form(FORMATS, 1):
            result = result.leaves[0]

        # Prevent too large results from being stored, as this can exceed the
        # DB's max_allowed_packet size
        max_stored_size = self.output.max_stored_size(settings)
        if max_stored_size is not None:
            data = pickle.dumps(result)
            if len(data) > max_stored_size:
                return Symbol('Null')
        return result

    def stop(self):
        self.stopped = True

    def format_output(self, expr, format=None):
        if format is None:
            format = self.format

        if isinstance(format, dict):
            return dict((k, self.format_output(expr, f)) for k, f in format.items())

        from mathics.core.expression import Expression, BoxError

        orig_output_size_limit = self.output_size_limit
        try:
            self.output_size_limit = self.definitions.get_config_value('System`$OutputSizeLimit')
            options = {}

            if format == 'text':
                result = expr.format(self, 'System`OutputForm')
                # for MathMLForm and TexForm, output size limits are applied in the form's apply
                # methods (e.g. see MathMLForm.apply) and then passed through result.boxes_to_text
                # which must, in these cases, not apply additional clipping, as this would clip
                # already clipped string material. for OutputForm, on the other hand, the call to
                # result.boxes_to_text is the only place we have to apply output size limits.
                options['output_size_limit'] = self.output_size_limit
            elif format == 'xml':
                result = Expression(
                    'StandardForm', expr).format(self, 'System`MathMLForm')
            elif format == 'tex':
                result = Expression('StandardForm', expr).format(
                    self, 'System`TeXForm')
            else:
                raise ValueError

            try:
                boxes = result.boxes_to_text(evaluation=self, **options)
            except BoxError:
                self.message('General', 'notboxes',
                             Expression('FullForm', result).evaluate(self))
                boxes = None
        finally:
            self.output_size_limit = orig_output_size_limit

        return boxes

    def set_quiet_messages(self, messages):
        from mathics.core.expression import Expression, String
        value = Expression('List', *messages)
        self.definitions.set_ownvalue('Internal`$QuietMessages', value)

    def get_quiet_messages(self):
        from mathics.core.expression import Expression
        value = self.definitions.get_definition('Internal`$QuietMessages').ownvalues
        if value:
            try:
                value = value[0].replace
            except AttributeError:
                return []
        if not isinstance(value, Expression):
            return []
        return value.leaves

    def make_boxes(self, items, form, segment=None):
        from mathics.core.expression import Expression, Omitted

        if self.output_size_limit is None or len(items) < 1:
            if segment is not None:
                segment.extend((False, 0, 0))
            return [Expression('MakeBoxes', item, form) for item in items]

        old_capacity = self.output_size_limit
        capacity = old_capacity

        left_leaves = []
        right_leaves = []

        middle = len(items) // 2
        # note that we use generator expressions, not list comprehensions, here, since we
        # might quit early in the loop below, and copying all leaves might prove inefficient.
        from_left = ((leaf, left_leaves.append) for leaf in items[:middle])
        from_right = ((leaf, right_leaves.append) for leaf in reversed(items[middle:]))

        try:
            for item, push in _interleave(from_left, from_right):
                self.output_size_limit = capacity

                # calling evaluate() here is a serious difference to the implementation
                # without $OutputSizeLimit. here, we evaluate MakeBoxes bottom up, i.e.
                # the leaves get evaluated first, since we need to estimate their size
                # here.
                #
                # without $OutputSizeLimit, on the other hand, the expression
                # gets evaluates from the top down, i.e. first MakeBoxes is wrapped around
                # each expression, then we call evaluate on the root node. assuming that
                # there are no rules like MakeBoxes[x_, MakeBoxes[y_]], both approaches
                # should be identical.
                #
                # we could work around this difference by pushing the unevaluated
                # expression here (see "push(box)" below), instead of the evaluated.
                # this would be very inefficient though, since we would get quadratic
                # runtime (quadratic in the depth of the tree).
                box = Expression('MakeBoxes', item, form).evaluate(self)

                cost = len(box.boxes_to_xml(evaluation=self))  # evaluate len as XML
                if capacity < cost:
                    break
                capacity -= cost
                push(box)
        finally:
            self.output_size_limit = old_capacity

        ellipsis_size = len(items) - (len(left_leaves) + len(right_leaves))
        ellipsis = [Omitted('<<%d>>' % ellipsis_size)] if ellipsis_size > 0 else []

        if segment is not None:
            if ellipsis_size > 0:
                segment.extend((True, len(left_leaves), len(items) - len(right_leaves)))
            else:
                segment.extend((False, 0, 0))

        return list(itertools.chain(left_leaves, ellipsis, reversed(right_leaves)))

    def message(self, symbol, tag, *args):
        from mathics.core.expression import (String, Symbol, Expression,
                                             from_python)

        # Allow evaluation.message('MyBuiltin', ...) (assume
        # System`MyBuiltin)
        symbol = ensure_context(symbol)
        quiet_messages = set(self.get_quiet_messages())

        pattern = Expression('MessageName', Symbol(symbol), String(tag))

        if pattern in quiet_messages or self.quiet_all:
            return

        # Shorten the symbol's name according to the current context
        # settings. This makes sure we print the context, if it would
        # be necessary to find the symbol that this message is
        # attached to.
        symbol_shortname = self.definitions.shorten_name(symbol)

        if settings.DEBUG_PRINT:
            print('MESSAGE: %s::%s (%s)' % (symbol_shortname, tag, args))

        text = self.definitions.get_value(
            symbol, 'System`Messages', pattern, self)
        if text is None:
            pattern = Expression('MessageName', Symbol('General'), String(tag))
            text = self.definitions.get_value(
                'System`General', 'System`Messages', pattern, self)

        if text is None:
            text = String("Message %s::%s not found." % (symbol_shortname, tag))

        text = self.format_output(Expression(
            'StringForm', text, *(from_python(arg) for arg in args)), 'text')

        self.out.append(Message(symbol_shortname, tag, text))
        self.output.out(self.out[-1])

    def print_out(self, text):
        from mathics.core.expression import from_python

        text = self.format_output(from_python(text), 'text')

        self.out.append(Print(text))
        self.output.out(self.out[-1])
        if settings.DEBUG_PRINT:
            print('OUT: ' + text)

    def error(self, symbol, tag, *args):
        # Temporarily reset the recursion limit, to allow the message being
        # formatted
        self.recursion_depth, depth = 0, self.recursion_depth
        try:
            self.message(symbol, tag, *args)
        finally:
            self.recursion_depth = depth
        raise AbortInterrupt

    def error_args(self, symbol, given, *needed):
        self.message_args(symbol, given, *needed)
        raise AbortInterrupt

    def message_args(self, symbol, given, *needed):
        from mathics.core.expression import Symbol

        if len(needed) == 1:
            needed = needed[0]
            if given > 1 and needed > 1:
                self.message(symbol, 'argrx', Symbol(symbol), given, needed)
            elif given == 1:
                self.message(symbol, 'argr', Symbol(symbol), needed)
            elif needed == 1:
                self.message(symbol, 'argx', Symbol(symbol), given)
        elif len(needed) == 2:
            if given == 1:
                self.message(symbol, 'argtu', Symbol(symbol), *needed)
            else:
                self.message(symbol, 'argt', Symbol(symbol), *needed)
        else:
            raise NotImplementedError

    def check_stopped(self):
        if self.stopped:
            raise TimeoutInterrupt

    def inc_recursion_depth(self):
        self.check_stopped()
        limit = self.definitions.get_config_value(
            '$RecursionLimit', settings.MAX_RECURSION_DEPTH)
        if limit is not None:
            if limit < 20:
                limit = 20
            self.recursion_depth += 1
            if self.recursion_depth > limit:
                self.error('$RecursionLimit', 'reclim', limit)

    def dec_recursion_depth(self):
        self.recursion_depth -= 1

    def add_listener(self, tag, listener):
        existing = self.listeners.get(tag)
        if existing is None:
            existing = self.listeners[tag] = []
        existing.insert(0, listener)

    def remove_listener(self, tag, listener):
        self.listeners.get(tag).remove(listener)

    def publish(self, tag, *args, **kwargs):
        listeners = self.listeners.get(tag, [])
        for listener in listeners:
            if listener(*args, **kwargs):
                break
