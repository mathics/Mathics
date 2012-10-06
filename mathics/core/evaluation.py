# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011 Jan Pöschko

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

import threading
import sys
#from gmpy import mpz, mpq, mpf
import cPickle as pickle

try:
    # Import settings module via Django, when possible
    from django.conf import settings
    settings.DEBUG  # django.conf imports settings lazily, so we need to access some setting to check
except ImportError:
    from mathics import settings

from mathics.core.numbers import format_float, prec, get_type, dps, prec
from mathics.core.util import subsets, subranges, permutations, interpolate_string

FORMATS = ['StandardForm', 'FullForm', 'TraditionalForm',
    'OutputForm', 'InputForm',
    'TeXForm', 'MathMLForm',
    'MatrixForm', 'TableForm']

def timeout_call(func, stop_func=None, timeout_duration=None, *args, **kwargs):
    if timeout_duration is None or settings.PROPAGATE_EXCEPTIONS:
        return func(*args, **kwargs)
    
    class InterruptableThread(threading.Thread):
        def __init__(self):
            threading.Thread.__init__(self)
            self.result = False
            self.exception = False

        def run(self):
            try:
                self.result = func(*args, **kwargs)
            except BaseException, exception:
                self.exception = True
                self.exc_info = sys.exc_info()

    thread = InterruptableThread()
    thread.start()
    thread.join(timeout_duration)
    if thread.isAlive():
        if stop_func is not None:
            stop_func()
            # Thread should quit quite soon...
            #thread.join()
        raise TimeoutInterrupt
    else:
        if thread.exception:
            #raise thread.exception
            exc_type, exc_value, exc_traceback = thread.exc_info
            raise exc_type, exc_value, exc_traceback
        return thread.result
                
class EvaluationInterrupt(Exception):
    pass

class AbortInterrupt(EvaluationInterrupt):
    pass

class TimeoutInterrupt(EvaluationInterrupt):
    pass

class ReturnInterrupt(EvaluationInterrupt):
    pass

class BreakInterrupt(EvaluationInterrupt):
    pass

class ContinueInterrupt(EvaluationInterrupt):
    pass

class Out(object):
    def __init__(self):
        self.is_message = False
        self.is_print = False
        self.text = ''

class Message(Out):
    def __init__(self, symbol, tag, text):
        super(Message, self).__init__()
        self.is_message = True
        self.symbol = symbol
        self.tag = tag
        self.text = text
    
    def __str__(self):
        return ' : ' + self.text
        
    def __cmp__(self, other):
        if self.is_message == other.is_message and self.text == other.text:
            return 0
        else:
            return 1
    
    def get_data(self):
        return {
            'message': True,
            'symbol': self.symbol,
            'tag': self.tag,
            'prefix': u'%s::%s' % (self.symbol, self.tag),
            'text': self.text,
        }

class Print(Out):
    def __init__(self, text):
        super(Print, self).__init__()
        self.is_print = True
        self.text = text
    
    def __str__(self):
        return ' | ' + self.text
        
    def __cmp__(self, other):
        if self.is_message == other.is_message and self.text == other.text:
            return 0
        else:
            return 1
    
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
        
class Evaluation(object):
    def __init__(self, input=None, definitions=None, timeout=None, out_callback=None, format='text',
        catch_interrupt=True):
        from mathics.core.definitions import Definitions
        
        if definitions is None:
            definitions = Definitions()
        self.definitions = definitions
        self.recursion_depth = 0
        self.timeout = False
        self.stopped = False
        self.out = []
        self.out_callback = out_callback
        self.listeners = {}
        self.options = None
        
        self.quiet_all = False
        self.quiet_messages = set()
        
        self.format = format
        
        queries = []
        last_parse_error = None
        if input is not None:
            from mathics.core.parser import parse, TranslateError
            
            lines = input.splitlines()
            query = ''
            for line in lines:
                if line:
                    query += line
                    try:
                        expression = parse(query)
                        if expression is not None:
                            queries.append(expression)
                        query = ''
                        last_parse_error = None
                    except TranslateError, exc:
                        last_parse_error = exc
                else:
                    query += ' '
            
        self.results = []
        
        for query in queries:
            self.recursion_depth = 0
            self.timeout = False
            self.stopped = False
        
            from mathics.core.expression import Symbol, Expression, Integer
            from mathics.core.rules import Rule
            
            line_no = self.get_config_value('$Line', 0)
            line_no += 1
            self.definitions.set_ownvalue('$Line', Integer(line_no))
            
            history_length = self.get_config_value('$HistoryLength', 100)
            if history_length is None or history_length > 100:
                history_length = 100
            
            def evaluate(self):
                if history_length > 0:
                    self.definitions.add_rule('In', Rule(Expression('In', line_no), query))
                result = query.evaluate(self)
                if history_length > 0:
                    stored_result = self.get_stored_result(result)
                    self.definitions.add_rule('Out', Rule(Expression('Out', line_no), stored_result))
                if result != Symbol('Null'):
                    return self.format_output(result)
                else:
                    return None
                     
            try:
                result = None
                exc_result = None
                try:
                    result = timeout_call(evaluate, self.stop, timeout, *[self])
                except KeyboardInterrupt:
                    if catch_interrupt:
                        exc_result = Symbol('$Aborted')
                    else:
                        raise
                except ValueError, exc:
                    text = unicode(exc)
                    if text == 'mpz.pow outrageous exponent' or text == 'mpq.pow outrageous exp num':
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
                except AbortInterrupt: #, error:
                    exc_result = Symbol('$Aborted')
                if exc_result is not None:
                    self.recursion_depth = 0
                    result = self.format_output(exc_result)
                    
                self.results.append(Result(self.out, result, line_no))
                self.out = []
            finally:
                self.stop()
        
            history_length = self.get_config_value('$HistoryLength', 100)
            if history_length is None or history_length > 100:
                history_length = 100
            line = line_no - history_length
            while line > 0:
                unset_in = self.definitions.unset('In', Expression('In', line))
                unset_out = self.definitions.unset('Out', Expression('Out', line))
                if not (unset_in or unset_out):
                    break
                line -= 1
                
        if last_parse_error is not None:
            self.recursion_depth = 0
            self.stopped = False
            self.message('General', 'syntax', unicode(last_parse_error))
            self.results.append(Result(self.out, None, None))
            
    def get_stored_result(self, result):
        from mathics.core.expression import Symbol
        
        # Remove outer format
        if result.has_form(FORMATS, 1):
            result = result.leaves[0]
        
        # Prevent too large results from being stored, as this can exceed the DB's max_allowed_packet size
        data = pickle.dumps(result)
        if len(data) > 10000:
            return Symbol('Null')
        return result
            
    def stop(self):
        self.stopped = True
        
    def format_output(self, expr):
        from mathics.core.expression import Expression, String, BoxError
        
        if self.format == 'text':
            result = expr.format(self, 'OutputForm')
        elif self.format == 'xml':
            result = Expression('StandardForm', expr).format(self, 'MathMLForm')
        elif self.format == 'tex':
            result = Expression('StandardForm', expr).format(self, 'TeXForm')
        else:
            raise ValueError
        try:
            boxes = result.boxes_to_text(evaluation=self)
        except BoxError:
            self.message('General', 'notboxes', String('%s' % result))
            boxes = None
        return boxes
                
    def message(self, symbol, tag, *args):
        from mathics.core.expression import String, Symbol, Expression, from_python
        
        if (symbol, tag) in self.quiet_messages or self.quiet_all:
            return
        
        if settings.DEBUG_PRINT:
            print 'MESSAGE: %s::%s (%s)' % (symbol, tag, args)
        
        pattern = Expression('MessageName', Symbol(symbol), String(tag))
        text = self.definitions.get_value(symbol, 'Messages', pattern, self)
        if text is None:
            pattern = Expression('MessageName', Symbol('General'), String(tag))
            text = self.definitions.get_value('General', 'Messages', pattern, self)
        
        if text is None:
            text = String("Message %s::%s not found." % (symbol, tag))
            
        text = self.format_output(Expression('StringForm', text, *(from_python(arg) for arg in args)))
        
        self.out.append(Message(symbol, tag, text))
        if self.out_callback:
            self.out_callback(self.out[-1])
    
    def print_out(self, text):
        from mathics.core.expression import from_python
        
        text = self.format_output(from_python(text))
        
        self.out.append(Print(text))
        if self.out_callback:
            self.out_callback(self.out[-1])
        if settings.DEBUG_PRINT:
            print 'OUT: ' + text
        
    def error(self, symbol, tag, *args):
        # Temporarily reset the recursion limit, to allow the message being formatted
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
        
    def get_config_value(self, name, default=None):
        # Infinity -> None, otherwise returns integer
        
        # Temporarily reset the recursion limit, to allow the evaluation of $RecursionLimit
        # and the possible message formatting to use some recursion.
        #self.recursion_depth, depth = 0, self.recursion_depth
        value = self.definitions.get_definition(name).ownvalues
        if value:
            try:
                value = value[0].replace
            except AttributeError:
                return None
            if value.get_name() == 'Infinity':
                return None
            
            return int(value.get_int_value())
        else:
            return default
        
    def set_config_value(self, name, new_value):
        from mathics.core.expression import Integer
        
        self.definitions.set_ownvalue(name, Integer(new_value))
        
    def inc_recursion_depth(self):
        from mathics.core.expression import Symbol
        
        self.check_stopped()
        
        limit = self.get_config_value('$RecursionLimit', settings.MAX_RECURSION_DEPTH)
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
        
