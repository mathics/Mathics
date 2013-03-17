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
from ply.lex import TOKEN

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

precedence = (
    #('right', 'FORMBOX'),
    #('nonassoc', 'COMPOUNDEXPRESSION'),
    ('nonassoc', 'PUT'),
    #('right', 'SET'),
    #('right', 'POSTFIX'),
    #('right', 'COLON'),
    #('nonassoc', 'FUNCTION'),
    #('right', 'ADDTO'),
    #('left', 'REPLACE'),
    #('right', 'RULE'),
    #('left', 'CONDITION'),
    #('left', 'STRINGEXPRESSION'),
    ('nonassoc', 'PATTERN'),
    ('left', 'ALTERNATIVES'),
    ('nonassoc', 'REPEATED'),
    #('right', 'IMPLIES'),
    #('left', 'EQUIVALENT'),
    ('left', 'OR'),
    ('left', 'XOR'),
    ('left', 'AND'),
    ('right', 'NOT'),
    #('right', 'FORALL', 'EXISTS'),
    #('left', 'ELEMENT')
    ('left', 'SAMEQ'),
    ('left', 'EQUAL'),
    ('left', 'SPAN'),
    ('left', 'PLUS'),
    ('right', 'TIMES'),                     # flat
    ('right', 'BACKSLASH'),
    ('left', 'DIVIDE'),
    ('nonassoc', 'MINUS'),
    ('right', 'NONCOMMUTATIVEMULTIPLY'),    # flat
    #('right', 'INTEGRATION'),
    ('right', 'SQRT'),
    ('right', 'POWER'),
    ('left', 'STRINGJOIN'),                 # flat
    ('nonassoc', 'DERIVATIVE'),
    ('nonassoc', 'CONJUGATE'),
    ('nonassoc', 'FACTORIAL'),
    ('right', 'APPLY'),
    ('left', 'INFIX'),
    ('right', 'PREFIX'),
    ('nonassoc', 'PREINCREMENT'),
    ('nonassoc', 'INCREMENT'),
    ('left', 'PART'),
    ('nonassoc', 'PATTERNTEST'),
    ('right', 'SUBSCRIPT'),
    ('right', 'OVERSCRIPT'),
    ('nonassoc', 'GET'),
    ('nonassoc', 'BLANK'),
    ('nonassoc', 'OUT'),
    ('nonassoc', 'SLOT'),
    ('nonassoc', 'MESSAGENAME'),
    ('nonassoc', 'STRING'),
    ('nonassoc', 'SYMBOL'),
    ('nonassoc', 'NUMBER'),
)

#additional_entities = {
#    'DifferentialD': u'\u2146',
#    'Sum': u'\u2211',
#    'Product': u'\u220f',
#}

tokens = (
    'parenthesis_0',
    'parenthesis_1',
    'parenthesis_2',
    'parenthesis_3',
    'symbol',
    'float',
    'int', 
    'string',
    'blanks', 
    'blankdefault',
    'out',
    'slot',
    'slotseq',
    'span',
    #'parsedexpr',
    'op_Get',
    'op_Put',
    'op_PutAppend',
    'op_MessageName',
    'op_Overscript',
    'op_Underscript',
    'op_Subscript',
    'op_Otherscript',
    'op_PatternTest',
    'op_Increment',
    'op_Decrement',
    'op_Prefix',
    'op_Infix',
    'op_Apply1',
    'op_Apply2',
    'op_Map',
    'op_MapAll',
    'op_Bang',
    'op_DoubleBang',
    'op_Conjugate',
    'op_Transpose',
    'op_ConjugateTranspose',
    'op_Derivative',
    'op_StringJoin',
    'op_Power',
    'op_Power2',
    'op_Sqrt',
    'op_NonCommutativeMultiply',
    'op_Plus',
    'op_Minus',
    'op_PlusMinus',
    'op_MinusPlus',
    'op_Slash',
    'op_Backslash',
    'op_Times',
    'op_Equal',
    'op_Unequal',
    'op_Greater',
    'op_Less',
    'op_GreaterEqual',
    'op_LessEqual',
    'op_SameQ',
    'op_UnsameQ',
    'op_And',
    'op_Xor',
    'op_Or',
    'op_Repeated',
    'op_RepeatedNull',
    'op_Alternatives',
    'op_Colon',
)

literals = ['(', ')', '{', '}', ',']

class MathicsScanner:
    tokens = tokens
    literals = literals
    precedence = precedence

    #t_ignore = ur' [\s \u2062]+ '
    t_ignore = ' \t '

    t_symbol = r' [a-zA-Z$][a-zA-Z0-9$]* '
    t_int = r' \d+ '
    t_blanks = r' ([a-zA-Z$][a-zA-Z0-9$]*)?_(__?)?([a-zA-Z$][a-zA-Z0-9$]*)? '
    t_blankdefault = r' ([a-zA-Z$][a-zA-Z0-9$]*)?_\. '

    t_parenthesis_0 = r' \[\[ '
    t_parenthesis_1 = r' \[ '
    t_parenthesis_2 = r' \]\] '
    t_parenthesis_3 = r' \] '

    t_span = r' \;\; '

    t_op_MessageName = r' \:\: '
    t_op_Get = r' \<\< '
    t_op_Put = r' \>\> '
    t_op_PutAppend = r' \>\>\> '

    t_op_Overscript = r' \\\& '
    t_op_Underscript = r' \\\+ '
    t_op_Subscript = r' \\\_ '
    t_op_Otherscript = r' \\\% '

    t_op_PatternTest = r' \? '
    t_op_Increment = r' \+\+ '
    t_op_Decrement = r' \-\- '

    t_op_Prefix = r' \@ '
    t_op_Infix = r' \~ '
    t_op_Apply1 = r' \@\@ '
    t_op_Apply2 = r' \@\@\@ '
    t_op_Map = r' \/\@ '
    t_op_MapAll = r' \/\/\@ '

    t_op_Bang = r' \! '
    t_op_DoubleBang = r' \!\! '

    #TODO
    #t_op_Conjugate = r''
    #t_op_Transpose = r''
    #t_op_ConjugateTranspose = r''

    t_op_Derivative = r' \'+ '
    t_op_StringJoin = r' \<\> '

    t_op_Power = r' \^ '
    t_op_Power2 = r' \\\^ '
    t_op_Sqrt = r' \\\@ '
    t_op_NonCommutativeMultiply = r' \*\* '

    t_op_Plus = r' \+ '
    t_op_Minus = r' \- '
    t_op_Slash = r' \/ '
    t_op_Backslash = r' \\ '

    t_op_Times = r' \* '

    #TODO
    #t_op_PlusMinus = r''
    #t_op_MinusPlus = r''

    t_op_Equal = r' \=\= '
    t_op_Unequal = r' \!\= '
    t_op_Greater = r' \> '
    t_op_Less = r' \< '
    t_op_GreaterEqual = r' \>\= '
    t_op_LessEqual = r' \<\= '

    t_op_SameQ = r' \=\=\= '
    t_op_UnsameQ = r' \=\!\= '

    t_op_And = r' \&\& '
    #t_op_Xor = r''     #TODO
    t_op_Or = r' \|\|  '

    t_op_Repeated = r' \.\. '
    t_op_RepeatedNull = r' \.\.\. '
    t_op_Alternatives = r' \| '

    t_op_Colon = r' \: '

    def build(self, **kwargs):
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
        (t.type, t.value) = ('slotseq', int(t.value[2:]))
        return t
    
    def t_slotseq_2(self, t):
        r' \#\# '
        s = t.value
        (t.type, t.value) = ('slotseq', 1)
        return t
    
    def t_slotsingle_1(self, t):
        r' \#\d+ '
        (t.type, t.value) = ('slot', int(t.value[1:]))
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
        (t.type, t.value) = ('out', -len(t.value))
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


class MathicsParser:
    tokens = tokens
    literals = literals
    precedence = precedence

    def build(self, **kwargs):
        self.parser = yacc.yacc(debug=1, module=self, **kwargs)

    def p_error(self, p):
        print p
        raise ParseError(p)
    
    def parse(self, string):
        result = self.parser.parse(string)
        #result = result.post_parse()
        return result
        
    def p_parenthesis(self, args):
        '''
        expr : '(' expr ')'
        '''
        expr = args[2]
        expr.parenthesized = True
        args[0] = expr
    
    #def p_tagset(self, args):
    #    '''expr : expr other expr operator_0014 expr
    #            | expr other expr operator_0049 expr
    #            | expr other expr operator_0022'''
    #    if args[4] == '=':
    #        args[0] = Expression('TagSet', args[1], args[3], args[5])
    #    elif args[4] == ':=':
    #        args[0] = Expression('TagSetDelayed', args[1], args[3], args[5])
    #    elif args[4] == '=.':
    #        args[0] = Expression('TagUnset', args[1], args[3])

    #def p_compound(self, args):
    #    'expr : expr operator_0043 %prec COMPOUNDEXPRESSION'
    #    args[0] = Expression('CompoundExpression', args[1], Symbol('Null'))
    
    #def p_parsed_expr(self, args):
    #    'expr : parsedexpr'
    #    args[0] = args[1]
    
    def p_op_670_call(self, args):
        'expr : expr args %prec PART'
        expr = Expression(args[1], *args[2].items)
        expr.parenthesized = True # to handle e.g. Power[a,b]^c correctly
        args[0] = expr
    
    def p_op_670_part(self, args):
        'expr : expr position %prec PART'
        args[0] = Expression('Part', args[1], *args[2].items)
    
    def p_args(self, args):
        'args : parenthesis_1 sequence parenthesis_3'
        args[0] = ArgsToken(args[2].items)
    
    def p_list(self, args):
        '''
        expr : '{' sequence '}'
        '''
        args[0] = Expression('List', *args[2].items)
    
    def p_position(self, args):
        'position : parenthesis_0 sequence parenthesis_2'
        args[0] = PositionToken(args[2].items)
    
    #def p_rest_left(self, args):
    #    '''rest_left :
    #                 | expr
    #                 | expr binary_op'''
    #    args[0] = RestToken()
    #
    #def p_rest_right(self, args):
    #    '''rest_right :
    #                  | expr
    #                  | args rest_right
    #                  | position rest_right
    #                  | rest_right binary_op expr'''
    #    args[0] = RestToken()

    def p_sequence(self, args):
        '''sequence :
                    | expr
                    | ','
                    | sequence ','
                    | sequence ',' expr'''

        if len(args) == 1:
            args[0] = SequenceToken([])
        elif len(args) == 2:
            if isinstance(args[1], BaseExpression):
                args[0] = SequenceToken([args[1]])
            else:
                args[0] = SequenceToken([Symbol('Null'), Symbol('Null')])
        elif len(args) == 3:
            args[0] = SequenceToken(args[1].items + [Symbol('Null')])
        elif len(args) == 4:
            args[0] = SequenceToken(args[1].items + [args[3]])
        
    def p_symbol(self, args):
        'expr : symbol %prec SYMBOL'
        args[0] = Symbol(args[1])
        
    def p_int(self, args):
        'expr : int %prec NUMBER'
        args[0] = Integer(args[1])
        
    def p_float(self, args):
        'expr : float %prec NUMBER'
        args[0] = Real(args[1])
        
    def p_blanks(self, args):
        'pattern : blanks %prec BLANK'
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
        'pattern : blankdefault %prec BLANK'
        name = args[1][:-2]
        if name:
            args[0] = Expression('Optional', Expression('Pattern', Symbol(name), Expression('Blank')))
        else:
            args[0] = Expression('Optional', Expression('Blank'))
        
    def p_slot(self, args):
        'expr : slot %prec SLOT'
        args[0] = Expression('Slot', Integer(args[1]))

    def p_slotseq(self, args):
        'expr : slotseq %prec SLOT'
        args[0] = Expression('SlotSequence', Integer(args[1]))
    
    def p_out(self, args):
        'expr : out %prec OUT'
        if args[1] == -1:
            args[0] = Expression('Out')
        else:
            args[0] = Expression('Out', Integer(args[1]))
        
    def p_string(self, args):
        'expr : string %prec STRING'
        args[0] = String(args[1])

    def p_filename_string(self, args):
        '''filename : string
                    | symbol'''
        args[0] = String(args[1])

    def p_Get(self, args):
        'expr : op_Get filename %prec GET'
        args[0] = Expression('Get', args[2])

    def p_Put(self, args):
        'expr : expr op_Put filename %prec PUT'
        args[0] = Expression('Put', args[1], args[3])

    def p_PutAppend(self, args):
        'expr : expr op_PutAppend filename %prec PUT'
        args[0] = Expression('PutAppend', args[1], args[3])

    def p_MessageName(self, args):
        '''expr : expr op_MessageName string op_MessageName string %prec MESSAGENAME
                | expr op_MessageName string %prec MESSAGENAME'''
        if len(args) == 4:
            args[0] = Expression('MessageName', args[1], String(args[3]))
        elif len(args) == 6:
            args[0] = Expression('MessageName', args[1], String(args[3]), String(args[5]))

    def p_OverScript(self, args):
        '''expr : expr op_Underscript expr op_Otherscript expr %prec OVERSCRIPT
                | expr op_Overscript expr op_Otherscript expr %prec OVERSCRIPT
                | expr op_Overscript expr %prec OVERSCRIPT
                | expr op_Underscript expr %prec OVERSCRIPT'''
        if len(args) == 4:
            if args[2] == '\\+':
                args[0] = Expression('Underscript', args[1], args[3])
            elif args[2] == '\\&':
                args[0] = Expression('Overscript', args[1], args[3])
        elif len(args) == 6:
            if args[2] == '\\+':
                args[0] = Expression('Underoverscript', args[1], args[3], args[5])
            elif args[2] == '\\&':
                args[0] = Expression('Underoverscript', args[1], args[5], args[3])

    def p_Subscript(self, args):
        '''expr : expr op_Subscript expr op_Otherscript expr %prec SUBSCRIPT
                | expr op_Subscript expr %prec SUBSCRIPT'''
        if len(args) == 4:
            args[0] = Expression('Subscript', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('Power', Expression('Subscript', args[1], args[3]), args[5])

    def p_PatternTest(self, args):
        'expr : expr op_PatternTest expr %prec PATTERNTEST'
        args[0] = Expression('PatternTest', args[1], args[3])

    def p_Increment(self, args):
        '''expr : expr op_Increment %prec INCREMENT
                | expr op_Decrement %prec INCREMENT'''
        if args[2] == '++':
            args[0] = Expression('Increment', args[1])
        elif args[2] == '--':
            args[0] = Expression('Decrement', args[1])

    def p_PreIncrement(self, args):
        '''expr : op_Increment expr %prec PREINCREMENT
                | op_Decrement expr %prec PREINCREMENT'''
        if args[1] == '++':
            args[0] = Expression('PreIncrement', args[2])
        elif args[1] == '--':
            args[0] = Expression('PreDecrement', args[2])

    def p_Prefix(self, args):
        'expr : expr op_Prefix expr %prec PREFIX'
        args[0] = Expression(args[1], args[3])

    def p_Infix(self, args):
        'expr : expr op_Infix expr op_Infix expr %prec INFIX'
        args[0] = Expression(args[3], args[1], args[5])

    def p_Apply(self, args):
        '''expr : expr op_Apply1 expr %prec APPLY
                | expr op_Apply2 expr %prec APPLY
                | expr op_Map expr %prec APPLY
                | expr op_MapAll expr %prec APPLY'''
        if args[2] == '@@':
            args[0] = Expression('Apply', args[1], args[3])
        elif args[2] == '@@@':
            args[0] = Expression('Apply', args[1], args[3], Expression('List', Integer(1)))

    def p_Factorial(self, args):
        '''expr : expr op_Bang %prec FACTORIAL
                | expr op_DoubleBang %prec FACTORIAL'''
        if args[2] == '!':
            args[0] = Expression('Factorial', args[1])
        elif args[2] == '!!':
            args[0] = Expression('Factorial2', args[1])

    def p_Conjugate(self, args):
        '''expr : expr op_Conjugate %prec CONJUGATE'''
        args[0] = Expression('Conjugate', args[1])

    def p_Transpose(self, args):
        '''expr : expr op_Transpose %prec CONJUGATE'''
        args[0] = Expression('Transpose', args[1])

    def p_ConjugateTranspose(self, args):
        '''expr : expr op_ConjugateTranspose %prec CONJUGATE'''
        args[0] = Expression('ConjugateTranspose', args[1])

    def p_Derivative(self, args):
        'expr : expr op_Derivative %prec DERIVATIVE'
        args[0] = Expression(Expression('Derivative', Integer(len(args[2]))), args[1])

    def p_StringJoin(self, args):
        'expr : expr op_StringJoin expr %prec STRINGJOIN'
        args[0] = Expression('StringJoin', args[1], args[3])

    def p_Power(self, args):
        '''expr : expr op_Power2 expr op_Otherscript expr %prec POWER
                | expr op_Power expr %prec POWER'''
        if args[2] == '^':
            args[0] = Expression('Power', args[1], args[3])
        elif args[2] == '\\^':
            args[0] = Expression('Power', Expression('Subscript', args[1], args[5]), args[3])

    def p_Sqrt(self, args):
        '''expr : op_Sqrt expr op_Otherscript expr %prec SQRT
                | op_Sqrt expr %prec SQRT'''
        if len(args) == 3:
            args[0] = Expression('Sqrt', args[2])
        elif len(args) == 5:
            args[0] = Expression('Power', args[2], Expression('Times', Integer(1), Expression('Power', args[4], Integer(-1))))

    def p_NonCommutativeMultiply(self, args):
        'expr : expr op_NonCommutativeMultiply expr %prec NONCOMMUTATIVEMULTIPLY'
        args[0] = Expression('NonCommutativeMultiply', args[1], args[3])

    def p_Plus(self, args):
        '''expr : expr op_Plus expr %prec PLUS
                | op_Plus expr %prec MINUS'''
        if len(args) == 3:
            args[0] = args[2]
        elif len(args) == 4:
            args[0] = Expression('Plus', args[1], args[3])

    def p_Minus(self, args):
        '''expr : expr op_Minus expr %prec PLUS
                | op_Minus expr %prec MINUS'''
        if len(args) == 3:
            args[0] = Expression('Times', Integer(-1), args[2])
        elif len(args) == 4:
            args[0] = Expression('Plus', args[1], Expression('Times', Integer(-1), args[3]))

    def p_PlusMinus(self, args):
        '''expr : expr op_PlusMinus expr %prec PLUS
                | op_PlusMinus expr %prec MINUS'''
        if len(args) == 3:
            args[0] = Expression('PlusMinus', args[2])
        elif len(args) == 4:
            args[0] = Expression('PlusMinus', args[1], args[3])

    def p_MinusPlus(self, args):
        '''expr : expr op_MinusPlus expr %prec PLUS
                | op_MinusPlus expr %prec MINUS'''
        if len(args) == 3:
            args[0] = Expression('MinusPlus', args[2])
        elif len(args) == 4:
            args[0] = Expression('MinusPlus', args[1], args[3])

    def p_Slash(self, args):
        'expr : expr op_Slash expr %prec DIVIDE'
        args[0] = Expression('Times', args[1], Expression('Power', args[3], Integer(-1)))

    def p_Backslash(self, args):
        'expr : expr op_Backslash expr %prec BACKSLASH'
        args[0] = Expression('Backslash', args[1], args[3])

    def p_Times(self, args):
        '''expr : expr expr %prec TIMES
                | expr op_Times expr %prec TIMES'''
        if len(args) == 3:
            args[0] = builtins['Times'].parse([args[1], None, args[2]])
        elif len(args) == 4:
            args[0] = builtins['Times'].parse([args[1], None, args[3]])
    
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

    def p_Span(self, args):
        '''expr : span_start span span_stop span span_step %prec SPAN
                | span_start span span_stop %prec SPAN'''
        if len(args) == 4:
            args[0] = Expression('Span', args[1], args[3], Integer(1))
        elif len(args) == 5:
            args[0] = Expression('Span', args[1], args[3], args[5])

    def p_Equal(self, args):
        '''expr : expr op_Equal expr %prec EQUAL
                | expr op_Unequal expr %prec EQUAL
                | expr op_Greater expr %prec EQUAL
                | expr op_Less expr %prec EQUAL
                | expr op_GreaterEqual expr %prec EQUAL
                | expr op_LessEqual expr %prec EQUAL'''
        if args[2] == '==':
            args[0] = Expression('Equal', args[1], args[3])
        elif args[2] == '!=':
            args[0] = Expression('Unequal', args[1], args[3])
        elif args[2] == '>':
            args[0] = Expression('Greater', args[1], args[3])
        elif args[2] == '<':
            args[0] = Expression('Less', args[1], args[3])
        elif args[2] == '>=':
            args[0] = Expression('GreaterEqual', args[1], args[3])
        elif args[2] == '<=':
            args[0] = Expression('LessEqual', args[1], args[3])

    def p_SameQ(self, args):
        '''expr : expr op_SameQ expr %prec SAMEQ
                | expr op_UnsameQ expr %prec SAMEQ'''
        if args[2] == '===':
            args[0] = Expression('SameQ', args[1], args[3])
        elif args[2] == '=!=':
            args[0] = Expression('UnsameQ', args[1], args[3])

    def p_Not(self, args):
        'expr : op_Bang expr %prec NOT'
        args[0] = Expression('Not', args[2])

    def p_And(self, args):
        'expr : expr op_And expr %prec AND'
        args[0] = Expression('And', args[1], args[3])

    def p_Xor(self, args):
        'expr : expr op_Xor expr %prec XOR'
        args[0] = Expression('Xor', args[1], args[3])

    def p_Or(self, args):
        'expr : expr op_Or expr %prec OR'
        args[0] = Expression('Or', args[1], args[3])

    def p_Repeated(self, args):
        '''expr : expr op_Repeated %prec REPEATED
                | expr op_RepeatedNull %prec REPEATED'''
        if args[2] == '..':
            args[0] = Expression('Repeated', args[1])
        elif args[2] == '...':
            args[0] = Expression('RepeatedNull', args[1])

    def p_Alternatives(self, args):
        'expr : expr op_Alternatives expr %prec ALTERNATIVES'
        args[0] = Expression('Alternatives', args[1], args[3])

    def p_Pattern(self, args):
        'expr : symbol op_Colon expr %prec PATTERN'
        args[0] = Expression('Pattern', Symbol(args[1]), args[3])

    def p_Optional(self, args):
        'expr : pattern op_Colon expr %prec PATTERN'
        args[0] = Expression('Optional', args[1], args[3])
        

scanner = MathicsScanner()
scanner.build()
parser = MathicsParser()
parser.build()

def parse(string):
    print "#>", string
    return parser.parse(string)

assert parse('1') == Integer(1)
assert parse('1.4') == Real('1.4')
assert parse('xX') == Symbol('xX')
assert parse('"abc 123"') == String('abc 123')
#assert parse('145 (* abf *) 345')==Expression('Times',Integer(145),Integer(345))

assert parse('1 :: "abc"') == Expression('MessageName', Integer(1), String("abc"))
assert parse('1 :: "abc" :: "123"') == Expression('MessageName', Integer(1), String("abc"), String("123"))

assert parse('<< filename') == Expression('Get', String('filename'))
assert parse('<<"filename"') == Expression('Get', String('filename'))
assert parse('1 >> filename') == Expression('Put', Integer(1), String('filename'))
assert parse('1 >>> filename') == Expression('PutAppend', Integer(1), String('filename'))

assert parse('1 \\& 2') == Expression('Overscript', Integer(1), Integer(2))
assert parse('1 \\+ 2') == Expression('Underscript', Integer(1), Integer(2))
assert parse('1 \\+ 2 \\% 3') == Expression('Underoverscript', Integer(1), Integer(2), Integer(3))
assert parse('1 \\& 2 \\% 3') == Expression('Underoverscript', Integer(1), Integer(3), Integer(2))

assert parse('1 \\_ 2') == Expression('Subscript', Integer(1), Integer(2))
assert parse('1 \\_ 2 \\% 3') == Expression('Power', Expression('Subscript', Integer(1), Integer(2)), Integer(3))

assert parse('1?2') == Expression('PatternTest', Integer(1), Integer(2))

assert parse('expr1[expr2]') == Expression('expr1', Symbol('expr2'))
assert parse('expr1[expr2][expr3]') == Expression(Expression('expr1', Symbol('expr2')), Symbol('expr3'))
assert parse('expr1[[expr2]]') == Expression('Part', Symbol('expr1'), Symbol('expr2'))
assert parse('expr1[[expr2, expr3]]') == Expression('Part', Symbol('expr1'), Symbol('expr2'), Symbol('expr3'))
assert parse('expr1[[expr2]][[expr3]]') == Expression('Part', Expression('Part', Symbol('expr1'), Symbol('expr2')), Symbol('expr3'))

assert parse('a++') == Expression('Increment', Symbol('a'))
assert parse('a--') == Expression('Decrement', Symbol('a'))
assert parse('++a') == Expression('PreIncrement', Symbol('a'))
assert parse('--a') == Expression('PreDecrement', Symbol('a'))

assert parse('expr1 @ expr2') == Expression('expr1', Symbol('expr2'))
assert parse('expr1 ~ expr2 ~ expr3') == Expression('expr2', Symbol('expr1'), Symbol('expr3'))

assert parse('f @@ expr') == Expression('Apply', Symbol('f'), Symbol('expr'))
assert parse('f @@@ expr') == Expression('Apply', Symbol('f'), Symbol('expr'), Expression('List', 1))
assert parse('f /@ expr') == Expression('Map', Symbol('f'), Symbol('expr'))
assert parse('f //@ expr') == Expression('MapAll', Symbol('f'), Symbol('expr'))
#assert parse('a @@ b @@ c') == Expression('Apply', Symbol('a'), Expression('Apply', Symbol('b'), Symbol('c')))

assert parse('5!') == Expression('Factorial', Integer(5))
assert parse('5 !!') == Expression('Factorial2', Integer(5))
assert parse('5 ! !') == Expression('Factorial', Expression('Factorial', Integer(5)))

assert parse("f'") == Expression(Expression('Derivative', Integer(1)), Symbol('f'))
assert parse("f''") == Expression(Expression('Derivative', Integer(2)), Symbol('f'))

assert parse('1 <> 2 ') == Expression('StringJoin', Integer(1), Integer(2))
#assert parse('1 <> 2 <> 3') == Expression('StringJoin', Integer(1), Integer(2), Integer(3))

assert parse('1 ^ 2') == Expression('Power', Integer(1), Integer(2))
assert parse('1 \^ 2 \% 3') == Expression('Power', Expression('Subscript', Integer(1), Integer(3)), Integer(2))
assert parse('\@ 3') == Expression('Sqrt', Integer(3))
assert parse('\@ 3 \% 3') == Expression('Power', Integer(3), Expression('Times', Integer(1), Expression('Power', Integer(3), Integer(-1))))

assert parse('expr1 ** expr2') == Expression('NonCommutativeMultiply', Symbol('expr1'), Symbol('expr2'))
#assert parse('expr1 ** expr2 ** expr3') == Expression('NonCommutativeMultiply', Symbol('expr1'), Symbol('expr2'), Symbol('expr3'))

assert parse('+1') == Integer(1)
assert parse('-1') == Expression('Times', Integer(-1), Integer(1))

assert parse('3/2') == Expression('Times', Integer(3), Expression('Power', Integer(2), Integer(-1)))
assert parse('3\\2') == Expression('Backslash', Integer(3), Integer(2))

assert parse('1 2') == Expression('Times', Integer(1), Integer(2))
assert parse('1*2') == Expression('Times', Integer(1), Integer(2))
assert parse('1 2 3') == Expression('Times', Integer(1), Integer(2), Integer(3))
assert parse('1*2*3') == Expression('Times', Integer(1), Integer(2), Integer(3))

assert parse('1 + 2') == Expression('Plus', Integer(1), Integer(2))
assert parse('1 - 2') == Expression('Plus', Integer(1), Expression('Times', Integer(-1), Integer(2)))
#assert parse('1 + 2 + 3') == Expression('Plus', Integer(1), Integer(2), Integer(3))

#FIXME
#assert parse('1;;2;;3') == Expression('Span', Integer(1), Integer(2), Integer(3))
#assert parse('1;; ;;3') == Expression('Span', Integer(1), Symbol('All'), Integer(3))
#assert parse(' ;;2;;3') == Expression('Span', Integer(1), Integer(2), Integer(3))
 
assert parse('1 == 2') == Expression('Equal', Integer(1), Integer(2))
assert parse('1 != 2') == Expression('Unequal', Integer(1), Integer(2))
#assert parse('1 == 2 == 3') == Expression('Equal', Integer(1), Integer(2), Integer(3))
#assert parse('1 != 2 != 3') == Expression('Unequal', Integer(1), Integer(2), Integer(3))

assert parse('1 > 2') == Expression('Greater', Integer(1), Integer(2))
assert parse('1 >= 2') == Expression('GreaterEqual', Integer(1), Integer(2))
assert parse('1 < 2') == Expression('Less', Integer(1), Integer(2))
assert parse('1 <= 2') == Expression('LessEqual', Integer(1), Integer(2))
#assert parse('1 > 2 > 3') == Expression('Greater', Integer(1), Integer(2), Integer(3))
#assert parse('1 >= 2 >= 3') == Expression('GreaterEqual', Integer(1), Integer(2), Integer(3))
#assert parse('1 < 2 < 3') == Expression('Less', Integer(1), Integer(2), Integer(3))
#assert parse('1 <= 2 <= 3') == Expression('LessEqual', Integer(1), Integer(2), Integer(3))

assert parse('1 === 2') == Expression('SameQ', Integer(1), Integer(2))
assert parse('1 =!= 2') == Expression('UnsameQ', Integer(1), Integer(2))
#assert parse('1 === 2 === 3') == Expression('SameQ', Integer(1), Integer(2), Integer(3))
#assert parse('1 =!= 2 =!= 3') == Expression('UnsameQ', Integer(1), Integer(2), Integer(3))

assert parse('!1') == Expression('Not', Integer(1))
assert parse('1 && 2') == Expression('And', Integer(1), Integer(2))
assert parse('1 || 2') == Expression('Or', Integer(1), Integer(2))

assert parse('1..') == Expression('Repeated', Integer(1))
assert parse('1...') == Expression('RepeatedNull', Integer(1))

assert parse('1 | 2') == Expression('Alternatives', Integer(1), Integer(2))
#assert parse('1 | 2 | 3') == Expression('Alternatives', Integer(1), Integer(2), Integer(3))

assert parse('x:expr') == Expression('Pattern', Symbol('x'), Symbol('expr'))
assert parse('x_:expr') == Expression('Optional', Expression('Pattern', Symbol('x'), Expression('Blank')), Symbol('expr'))

# assert parse('1 ^ 2') == Expression('Power', Integer(1), Integer(2))
# assert parse('{x, y}') == Expression('List', Symbol('x'), Symbol('y'))
# assert parse('{a,}') == Expression('List', Symbol('a'), Symbol('Null'))
# assert parse('{,}') == Expression('List', Symbol('Null'), Symbol('Null'))
# #assert parse('{,a}') == Expression('List', Symbol('Null'), Symbol('a')) #TODO
# 
# assert parse('Sin[x, y]') == Expression('Sin', Symbol('x'), Symbol('y'))
# assert parse('a[[1]]') == Expression('Part', Symbol('a'), Integer(1))
# 
# assert parse('f_') == Expression('Pattern', Symbol('f'), Expression('Blank'))
# assert parse('f__') == Expression('Pattern', Symbol('f'), Expression('BlankSequence'))
# assert parse('f___') == Expression('Pattern', Symbol('f'), Expression('BlankNullSequence'))
# 
# assert parse('#2') == Expression('Slot', Integer(2))
# assert parse('#') == Expression('Slot', Integer(1))
# 
# assert parse('##2') == Expression('SlotSequence', Integer(2))
# assert parse('##') == Expression('SlotSequence', Integer(1))
# 
assert parse('%2') == Expression('Out', Integer(2))
assert parse('%') == Expression('Out')
assert parse('%%') == Expression('Out', Integer(-2))
assert parse('%%%%') == Expression('Out', Integer(-4))

assert parse('x ! y') == Expression('Times', Expression('Factorial', Symbol('x')), Symbol('y'))
assert parse('x ^ 2 y') == Expression('Times', Expression('Power', Symbol('x'), Integer(2)), Symbol('y'))
quit()
