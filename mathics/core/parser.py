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

#from spark.spark import GenericScanner, GenericParser
#from spark import spark

import ply.lex as lex
import ply.yacc as yacc

import re
#from re import compile, escape
#
#import unicodedata
#
from mathics.core.expression import BaseExpression, Expression, Integer, Real, Symbol, String
from mathics.builtin import builtins
         
class TranslateError(Exception):
    pass

class ScanError(TranslateError):
    def __init__(self, pos):
        super(ScanError, self).__init__()
        self.pos = pos
        
    def __unicode__(self):
        return u"Lexical error at position {0}.".format(self.pos)
        
class InvalidCharError(TranslateError):
    def __init__(self, char):
        super(InvalidCharError, self).__init__()
        self.char = char
        
    def __unicode__(self):
        return u"Invalid character at '%s'." % self.char #.decode('utf-8')

class ParseError(TranslateError):
    def __init__(self, token):
        super(ParseError, self).__init__()
        self.token = token
        
    def __unicode__(self):
        return u"Parse error at or near token %s." % str(self.token)
    
operators = {} # operator (e.g. "+") -> list of classes ([Minus, Subtract])
operators_by_prec = {} # precedence (310) -> list of classes ([Plus, Minus])
binary_operators = [] # list of binary operators (['+', '-', ...])
for name, builtin in builtins.iteritems():
    operator = builtin.get_operator()
    if operator:
        if builtin.precedence_parse is not None:
            precedence = builtin.precedence_parse
        else:
            precedence = builtin.precedence
        existing = operators_by_prec.get(precedence)
        if existing is None:
            operators_by_prec[precedence] = [builtin]
        else:
            existing.append(builtin)
        operators[operator] = builtin
        if builtin.is_binary():
            binary_operators.append(operator)
symbols = operators.keys()
symbols.sort(key=lambda s: len(s), reverse=True)

lookup = {}
for symbol in symbols:
    lookup[symbol] = operators[symbol].__class__.__name__ 


#symbol_re = compile(r'[a-zA-Z$][a-zA-Z0-9$]*')
#
#def is_symbol_name(text):
#    return symbol_re.sub('', text) == ''
#
#additional_entities = {
#    'DifferentialD': u'\u2146',
#    'Sum': u'\u2211',
#    'Product': u'\u220f',
#}

tokens = [
    'parenthesis_0',
    'parenthesis_1',
    'parenthesis_2',
    'parenthesis_3',
    'symbol',
    'float',
    'int', 
    'blanks', 
    'blankdefault',
    'string',
    'out',
    'slot',
    'slotseq',
    'span',
    'other',
    'parsedexpr',
] + ['operator_%.4d' % i for i in range(len(operators.keys()))]

literals = ['(', ')', '{', '}', ',']

class MathicsScanner:
    tokens = tokens
    literals = literals

    #t_ignore = ur' [\s\u2062]+ '
    t_ignore = ' \t '

    t_symbol = r' [a-zA-Z$][a-zA-Z0-9$]* '
    #t_comma = r' , '
    t_int = r' \d+ '
    t_blanks = r' ([a-zA-Z$][a-zA-Z0-9$]*)?_(__?)?([a-zA-Z$][a-zA-Z0-9$]*)? '
    t_blankdefault = r' ([a-zA-Z$][a-zA-Z0-9$]*)?_\. '

    t_parenthesis_0 = r' \[\[ '
    t_parenthesis_1 = r' \[ '
    t_parenthesis_2 = r' \]\] '
    t_parenthesis_3 = r' \] '
    #t_parenthesis_4 = r' \( '
    #t_parenthesis_5 = r' \) '
    #t_parenthesis_6 = r' \{ '
    #t_parenthesis_7 = r' \} '

    t_span = r' \;\; '
    t_other = r' \/\: '

    def build(self, **kwargs):
        # add operators
        index = 0
        for symbol in symbols:
            def t_op(t):
                return t

            t_op.__doc__ = re.escape(symbol)
            setattr(self, 't_operator_%.4d' % index, t_op)
            index += 1
            
        self.lexer = lex.lex(debug=0, module=self, **kwargs)

    def tokenize(self, input_string):
        self.tokens = []
        self.lexer.input(input_string)
        while True:
            tok = self.lexer.token()
            if not tok:
                break
            self.tokens.append(tok)
        return self.tokens

    def t_float(self, t):
        r' \d*(?<!\.)\.\d+(\*\^(\+|-)?\d+)? | \d+\.(?!\.) \d*(\*\^(\+|-)?\d+)?'
        s = t.value.split('*^')
        if len(s) == 1:
            s = s[0]
        else:
            assert len(s) == 2
            exp = int(s[1])
            if exp >= 0:
                s = s[0] + '0' * exp
            else:
                s = '0' * -exp + s[0]

            dot = s.find('.')
            s = s[:dot] + s[dot+1:]
            s = s[:exp+dot] + '.' + s[exp+dot:]

        t.value = s
        return t

    def t_string(self, t):
        r' "([^\\"]|\\\\|\\"|\\\[[a-zA-Z]+\]|\\n|\\r|\\r\\n)*" '
        s = t.value[1:-1]
        
        def sub_entity(match):
            name = match.group(1)
            entity = additional_entities.get(name)
            if entity is not None:
                return entity
            uname = ''
            for c in name:
                if 'A' <= c <= 'Z':
                    uname += ' ' + c
                else:
                    uname += c
            try:
                uname = uname.strip()
                return unicodedata.lookup(uname)
            except KeyError:
                return '\\[' + name + ']'
        
        s = re.sub(r'\\\[([a-zA-Z]+)\]', sub_entity, s)
        s = s.replace('\\\\', '\\').replace('\\"', '"')
        s = s.replace('\\r\\n', '\r\n')
        s = s.replace('\\r', '\r')
        s = s.replace('\\n', '\n')

        t.value = s
        return t

    def t_slotseq_1(self, t):
        r' \#\#\d+ '
        (t.type, t.value) = ('slotseq', int(t.values[2:]))
        return t
    
    def t_slotseq_2(self, t):
        r' \#\# '
        s = t.value
        (t.type, t.value) = ('slotseq', 1)
        return t
    
    def t_slotsingle_1(self, t):
        r' \#\d+ '
        (t.type, t.value) = ('slot', int(t.values[2:]))
        return t

    def t_slotsingle_2(self, t):
        r' \# '
        (t.type, t.value) = ('slot', 1)
        return t

    def t_out_1(self, t):
        r' \%\d+ '
        (t.type, t.value) = ('out', int(t.value[1:]))
        return t

    def t_out_2(self, t):
        r' \%+ '
        (t.type, t.value) = ('out', 1)
        return t

    def t_comment(self, t):
        r' (?s) \(\* .*? \*\) '
        return None
    
    def t_error(self, t):
        print t
        raise ScanError(self.lexer.lexpos)

class AbstractToken(object):
    pass

class CompoundToken(AbstractToken):
    def __init__(self, items):
        self.items = items
        
class SequenceToken(CompoundToken):
    pass
        
class ArgsToken(CompoundToken):
    pass
        
class PositionToken(CompoundToken):
    pass

class RestToken(AbstractToken):
    pass

    # Actual expressions in there don't matter - we just use its parse_tokens property!
#        
#def join_parse_tokens(tokens):
#    result = []
#    for token in tokens:
#        result.extend(token.parse_tokens)
#    return result
#
#def parsing(function):
#    def new_function(self, args):
#        result = function(self, args)
#        result.parse_tokens = join_parse_tokens(args)
#        return result
#    new_function.__name__ = function.__name__
#    new_function.__doc__ = function.__doc__
#    return new_function
#
#def parsing_static(function):
#    def new_function(args):
#        result = function(args)
#        result.parse_tokens = join_parse_tokens(args)
#        return result
#    new_function.__name__ = function.__name__
#    new_function.__doc__ = function.__doc__
#    return new_function
#
class MathicsParser:
    tokens = tokens
    literals = literals

    def build(self, **kwargs):
        self.parser = yacc.yacc(debug=1, module=self, **kwargs)

    def p_error(self, p):
        print p
        raise ParseError(p)
    
    def parse(self, string):
        result = self.parser.parse(string)
        print "##", result
        #result = result.post_parse()
        return result

    def p_op_400(self, args):
        'expr : expr expr'
        #args[0] = builtins['Times'].parse([args[0], None, args[1]])
        args[0] = Expression('Times', args[1], args[2])
    
    def p_parenthesis(self, args):
        '''
        expr : '(' expr ')'
        '''
        expr = args[2]
        expr.parenthesized = True
        args[0] = expr
    
    def p_tagset(self, args):
        '''expr : expr other expr operator_0014 expr
                | expr other expr operator_0049 expr
                | expr other expr operator_0022'''
        if args[4] == '=':
            args[0] = Expression('TagSet', args[1], args[3], args[5])
        elif args[4] == ':=':
            args[0] = Expression('TagSetDelayed', args[1], args[3], args[5])
        elif args[4] == '=.':
            args[0] = Expression('TagUnset', args[1], args[3])

    def p_compound(self, args):
        'expr : expr operator_0043'
        if args[2] == ';':
            args[0] = Expression('CompoundExpression', args[1], Symbol('Null'))
    
    def p_parsed_expr(self, args):
        'expr : parsedexpr'
        args[0] = args[1]
    
    def p_op_670_call(self, args):
        'expr : expr args'
        expr = Expression(args[1], *args[1].items)
        expr.parenthesized = True # to handle e.g. Power[a,b]^c correctly
        args[0] = expr
    
    def p_op_670_part(self, args):
        'expr : expr position'
        args[0] = Expression('Part', args[1], *args[2].items)
    
    def p_span_start(self, args):
        '''span_start :
                      | expr'''
        if len(args) == 1:
            args[0] = Integer(1)
        elif len(args) == 2:
            args[0] = args[1]
    
    def p_span_stop(self, args):
        '''span_stop :
                     | expr'''
        if len(args) == 1:
            args[0] = Symbol('All')
        elif len(args) == 2:
            args[0] = args[1]

    def p_span_step(self, args):
        '''span_step :
                     | expr'''
        if len(args) == 1:
            args[0] = Integer(1)
        elif len(args) == 2:
            args[0] = args[1]

    def p_op_305_1(self, args):
        'expr : span_start span span_stop span span_step'
        #'expr : span_start ;; span_stop ;; span_step'
        args[0] = Expression('Span', args[1], args[3], args[5])
    
    def p_op_305_2(self, args):
        'expr : span_start span span_stop'
        #'expr : span_start ;; span_stop'
        args[0] = Expression('Span', args[1], args[3], Integer(1))
    
    def p_args_1(self, args):
        'args : parenthesis_1 sequence parenthesis_3'
        args[0] = ArgsToken(args[1].items)
    
    def p_list(self, args):
        '''
        expr : '{' sequence '}'
        '''
        args[0] = Expression('List', *args[2].items)
    
    def p_position(self, args):
        'position : parenthesis_0 sequence parenthesis_2'
        args[0] = PositionToken(args[2].items)
    
    def p_rest_left(self, args):
        '''rest_left :
                     | expr
                     | expr binary_op'''
        args[0] = RestToken()
    
    def p_rest_right(self, args):
        '''rest_right :
                      | expr
                      | args rest_right
                      | position rest_right
                      | rest_right binary_op expr'''
        args[0] = RestToken()
    
    def p_sequence_0(self, args):
        '''sequence :
                    | expr
                    | ','
                    | sequence ','
                    | sequence ',' expr'''

        if len(args) == 1:
            args[0] = SequenceToken([])
        elif len(args) == 2:
            if args[1] == ',':
                args[0] = SequenceToken([Symbol('Null'), Symbol('Null')])
            else:
                args[0] = SequenceToken([args[1]])
        elif len(args) == 3 and args[2] == ',':
            args[0] = SequenceToken(args[1].items + [Symbol('Null')])
        elif len(args) == 4 and args[2] == ',':
            args[0] = SequenceToken(args[1].items + [args[2]])
        
    def p_symbol(self, args):
        'expr : symbol'
        args[0] = Symbol(args[1])
        
    def p_int(self, args):
        'expr : int'
        args[0] = Integer(args[1])
        
    def p_float(self, args):
        'expr : float'
        args[0] = Real(args[1])
        
    def p_blanks(self, args):
        'expr : blanks'
        pieces = args[1].split('_')
        count = len(pieces) - 1
        if count == 1:
            name = 'Blank'
        elif count == 2:
            name = 'BlankSequence'
        elif count == 3:
            name = 'BlankNullSequence'
        if pieces[-1]:
            blank = Expression(name, Symbol(pieces[-1]))
        else:
            blank = Expression(name)
        if pieces[0]:
            args[0] = Expression('Pattern', Symbol(pieces[0]), blank)
        else:
            args[0] = blank
        
    def p_blankdefault(self, args):
        'expr : blankdefault'
        name = args[1][:-2]
        if name:
            args[0] = Expression('Optional', Expression('Pattern', Symbol(name), Expression('Blank')))
        else:
            args[0] = Expression('Optional', Expression('Blank'))
        
    def p_slot(self, args):
        'expr : slot'
        args[0] = Expression('Slot', Integer(args[1]))

    def p_slotseq(self, args):
        'expr : slotseq'
        args[0] = Expression('SlotSequence', Integer(args[1]))
    
    def p_out(self, args):
        'expr : out'
        args[0] = Expression('Out', Integer(args[1]))
        
    def p_string(self, args):
        'expr : string'
        args[0] = String(args[1])

#    def ambiguity(self, rules):
#        """
#        Don't use length of right-hand side for ordering of rules!
#        (Problem with 'implicit' multiplication!)
#        """
#        #
#        #  XXX - problem here and in collectRules() if the same rule
#        #     appears in >1 method.  Also undefined results if rules
#        #     causing the ambiguity appear in the same method.
#        #
#        
#        sortlist = []
#        name2index = {}
#        for i in range(len(rules)):
#            lhs, rhs = rule = rules[i]
#            name = self.rule2name[self.new2old[rule]]
#            sortlist.append(name)
#            name2index[name] = i
#        sortlist.sort()
#        result = rules[name2index[self.resolve(sortlist)]]
#        return result
#    
#    def collectRules(self):
#        
#        custom_ops = {}
#        for name in dir(self):
#            if name.startswith('op_'):
#                precedence = int(name[3:6])
#                existing = custom_ops.get(precedence)
#                if existing is None:
#                    custom_ops[precedence] = [name]
#                else:
#                    existing.append(name)
#        
#        precedences = set(operators_by_prec.keys() + custom_ops.keys())
#        precedences = sorted(precedences)
#        
#        for precedence in precedences:
#            builtin = operators_by_prec.get(precedence, [])
#            
#            for operator in builtin:
#                def p_op(args, operator=operator):
#                    return operator.parse(args)
#                p_op.__name__ = 'p_zzzz_%.4d' % precedence #(1000-precedence)
#                rule = operator.get_rule()
#                p_op = parsing_static(p_op)
#                self.addRule(rule, p_op)
#            
#            custom = custom_ops.get(precedence, [])
#            for name in custom:
#                func = getattr(self, name)
#                doc = func.__doc__
#                def p_op(args, func=func):
#                    return func(args)
#                p_op.__name__ = 'p_zzzz_%.4d_%s' % (precedence, name)
#                p_op = parsing_static(p_op)
#                self.addRule(doc, p_op)
#        
        
    def p_binary_op(self, args):
        'expr : expr binary_op expr'
        args[0] = Expression(args[2], args[1], args[3])

    def p_op(self, args):
        'binary_op : operator_0037'
        args[0] = 'Plus'

scanner = MathicsScanner()
scanner.build()
parser = MathicsParser()
parser.build()

def parse(string):
    print "#>", string
    return parser.parse(string)

parse('1')
parse('1.4')
parse('xX')
parse('"abc 123"')
parse('1 2 3')
parse('145 (* abf *) 345')
#parse('+')
parse('1 + 2')

quit()
