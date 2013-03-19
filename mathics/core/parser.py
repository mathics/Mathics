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
    ('right', 'FORMBOX'),
    ('nonassoc', 'COMPOUNDEXPRESSION'),
    ('nonassoc', 'PUT'),
    ('right', 'SET'),
    ('right', 'POSTFIX'),
    ('right', 'COLON'),
    ('nonassoc', 'FUNCTION'),
    ('right', 'ADDTO'),
    ('left', 'REPLACE'),
    ('right', 'RULE'),
    ('left', 'CONDITION'),
    ('left', 'STRINGEXPRESSION'),
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
    'Get',
    'Put',
    'PutAppend',
    'MessageName',
    'Overscript',
    'Underscript',
    'Subscript',
    'Otherscript',
    'PatternTest',
    'Increment',
    'Decrement',
    'Prefix',
    'Infix',
    'Apply1',
    'Apply2',
    'Map',
    'MapAll',
    'Bang',
    'DoubleBang',
    'Conjugate',
    'Transpose',
    'ConjugateTranspose',
    'Derivative',
    'StringJoin',
    'Power',
    'Power2',
    'Sqrt',
    'NonCommutativeMultiply',
    'Plus',
    'Minus',
    'PlusMinus',
    'MinusPlus',
    'Slash',
    'Backslash',
    'Times',
    'Equal',
    'Unequal',
    'Greater',
    'Less',
    'GreaterEqual',
    'LessEqual',
    'SameQ',
    'UnsameQ',
    'And',
    'Xor',
    'Or',
    'Repeated',
    'RepeatedNull',
    'Alternatives',
    'Colon',
    'StringExpression',
    'Condition',
    'Rule',
    'RuleDelayed',
    'ReplaceAll',
    'ReplaceRepeated',
    'AddTo',
    'SubtractFrom',
    'TimesBy',
    'DivideBy',
    'Function',
    'RawColon',
    'Postfix',
    'Set',
    'SetDelayed',
    'UpSet',
    'UpSetDelayed',
    'TagSet',
    'Unset',
    'Semicolon',
    'FormBox',
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

    t_MessageName = r' \:\: '
    t_Get = r' \<\< '
    t_Put = r' \>\> '
    t_PutAppend = r' \>\>\> '

    t_Overscript = r' \\\& '
    t_Underscript = r' \\\+ '
    t_Subscript = r' \\\_ '
    t_Otherscript = r' \\\% '

    t_PatternTest = r' \? '
    t_Increment = r' \+\+ '
    t_Decrement = r' \-\- '

    t_Prefix = r' \@ '
    t_Infix = r' \~ '
    t_Apply1 = r' \@\@ '
    t_Apply2 = r' \@\@\@ '
    t_Map = r' \/\@ '
    t_MapAll = r' \/\/\@ '

    t_Bang = r' \! '
    t_DoubleBang = r' \!\! '

    #TODO
    #t_Conjugate = r''
    #t_Transpose = r''
    #t_ConjugateTranspose = r''

    t_Derivative = r' \'+ '
    t_StringJoin = r' \<\> '

    t_Power = r' \^ '
    t_Power2 = r' \\\^ '
    t_Sqrt = r' \\\@ '
    t_NonCommutativeMultiply = r' \*\* '

    t_Plus = r' \+ '
    t_Minus = r' \- '
    t_Slash = r' \/ '
    t_Backslash = r' \\ '

    t_Times = r' \* '

    #TODO
    #t_PlusMinus = r''
    #t_MinusPlus = r''

    t_Equal = r' \=\= '
    t_Unequal = r' \!\= '
    t_Greater = r' \> '
    t_Less = r' \< '
    t_GreaterEqual = r' \>\= '
    t_LessEqual = r' \<\= '

    t_SameQ = r' \=\=\= '
    t_UnsameQ = r' \=\!\= '

    t_And = r' \&\& '
    #t_Xor = r''     #TODO
    t_Or = r' \|\|  '

    t_Repeated = r' \.\. '
    t_RepeatedNull = r' \.\.\. '
    t_Alternatives = r' \| '

    t_Colon = r' \: '
    t_StringExpression = r' \~\~ '
    t_Condition = r' \/\; '

    t_Rule = r' \-\> '
    t_RuleDelayed = r' \:\> '
    t_ReplaceAll = r' \/\. '
    t_ReplaceRepeated = r' \/\/\. '

    t_AddTo = r' \+\= '
    t_SubtractFrom = r' \-\=  '
    t_TimesBy = r' \*\= '
    t_DivideBy = r' \/\=  '

    t_Function = r' \& '
    #t_RawColon = r''   #TODO
    t_Postfix = r' \/\/ '

    t_Set = r' \= '
    t_SetDelayed = r' \:\= '
    t_UpSet = r' \^\= '
    t_UpSetDelayed = r' \^\:\= '
    t_TagSet = r' \/\: '
    t_Unset = r' \=\. '

    t_Semicolon = r' \; '
    t_FormBox = r' \\\` '

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

    def p_670_call(self, args):
        'expr : expr args %prec PART'
        expr = Expression(args[1], *args[2].items)
        expr.parenthesized = True # to handle e.g. Power[a,b]^c correctly
        args[0] = expr
    
    def p_670_part(self, args):
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
        'expr : Get filename %prec GET'
        args[0] = Expression('Get', args[2])

    def p_MessageName(self, args):
        '''expr : expr MessageName string MessageName string %prec MESSAGENAME
                | expr MessageName string %prec MESSAGENAME'''
        if len(args) == 4:
            args[0] = Expression('MessageName', args[1], String(args[3]))
        elif len(args) == 6:
            args[0] = Expression('MessageName', args[1], String(args[3]), String(args[5]))

    def p_OverScript(self, args):
        '''expr : expr Underscript expr Otherscript expr %prec OVERSCRIPT
                | expr Overscript expr Otherscript expr %prec OVERSCRIPT
                | expr Overscript expr %prec OVERSCRIPT
                | expr Underscript expr %prec OVERSCRIPT'''
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
        '''expr : expr Subscript expr Otherscript expr %prec SUBSCRIPT
                | expr Subscript expr %prec SUBSCRIPT'''
        if len(args) == 4:
            args[0] = Expression('Subscript', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('Power', Expression('Subscript', args[1], args[3]), args[5])

    def p_PatternTest(self, args):
        'expr : expr PatternTest expr %prec PATTERNTEST'
        args[0] = Expression('PatternTest', args[1], args[3])

    def p_Increment(self, args):
        '''expr : expr Increment %prec INCREMENT
                | expr Decrement %prec INCREMENT'''
        if args[2] == '++':
            args[0] = Expression('Increment', args[1])
        elif args[2] == '--':
            args[0] = Expression('Decrement', args[1])

    def p_PreIncrement(self, args):
        '''expr : Increment expr %prec PREINCREMENT
                | Decrement expr %prec PREINCREMENT'''
        if args[1] == '++':
            args[0] = Expression('PreIncrement', args[2])
        elif args[1] == '--':
            args[0] = Expression('PreDecrement', args[2])

    def p_Prefix(self, args):
        'expr : expr Prefix expr %prec PREFIX'
        args[0] = Expression(args[1], args[3])

    def p_Infix(self, args):
        'expr : expr Infix expr Infix expr %prec INFIX'
        args[0] = Expression(args[3], args[1], args[5])

    def p_Apply(self, args):
        '''expr : expr Apply1 expr %prec APPLY
                | expr Apply2 expr %prec APPLY
                | expr Map expr %prec APPLY
                | expr MapAll expr %prec APPLY'''
        if args[2] == '@@':
            args[0] = Expression('Apply', args[1], args[3])
        elif args[2] == '@@@':
            args[0] = Expression('Apply', args[1], args[3], Expression('List', Integer(1)))

    def p_Factorial(self, args):
        '''expr : expr Bang %prec FACTORIAL
                | expr DoubleBang %prec FACTORIAL'''
        if args[2] == '!':
            args[0] = Expression('Factorial', args[1])
        elif args[2] == '!!':
            args[0] = Expression('Factorial2', args[1])

    def p_Conjugate(self, args):
        '''expr : expr Conjugate %prec CONJUGATE'''
        args[0] = Expression('Conjugate', args[1])

    def p_Transpose(self, args):
        '''expr : expr Transpose %prec CONJUGATE'''
        args[0] = Expression('Transpose', args[1])

    def p_ConjugateTranspose(self, args):
        '''expr : expr ConjugateTranspose %prec CONJUGATE'''
        args[0] = Expression('ConjugateTranspose', args[1])

    def p_Derivative(self, args):
        'expr : expr Derivative %prec DERIVATIVE'
        args[0] = Expression(Expression('Derivative', Integer(len(args[2]))), args[1])

    def p_StringJoin(self, args):
        'expr : expr StringJoin expr %prec STRINGJOIN'
        args[0] = Expression('StringJoin', args[1], args[3])

    def p_Power(self, args):
        '''expr : expr Power2 expr Otherscript expr %prec POWER
                | expr Power expr %prec POWER'''
        if args[2] == '^':
            args[0] = Expression('Power', args[1], args[3])
        elif args[2] == '\\^':
            args[0] = Expression('Power', Expression('Subscript', args[1], args[5]), args[3])

    def p_Sqrt(self, args):
        '''expr : Sqrt expr Otherscript expr %prec SQRT
                | Sqrt expr %prec SQRT'''
        if len(args) == 3:
            args[0] = Expression('Sqrt', args[2])
        elif len(args) == 5:
            args[0] = Expression('Power', args[2], Expression('Times', Integer(1), Expression('Power', args[4], Integer(-1))))

    def p_NonCommutativeMultiply(self, args):
        'expr : expr NonCommutativeMultiply expr %prec NONCOMMUTATIVEMULTIPLY'
        args[0] = Expression('NonCommutativeMultiply', args[1], args[3])

    def p_Plus(self, args):
        '''expr : expr Plus expr %prec PLUS
                | Plus expr %prec MINUS'''
        if len(args) == 3:
            args[0] = args[2]
        elif len(args) == 4:
            args[0] = Expression('Plus', args[1], args[3])

    def p_Minus(self, args):
        '''expr : expr Minus expr %prec PLUS
                | Minus expr %prec MINUS'''
        if len(args) == 3:
            args[0] = Expression('Times', Integer(-1), args[2])
        elif len(args) == 4:
            args[0] = Expression('Plus', args[1], Expression('Times', Integer(-1), args[3]))

    def p_PlusMinus(self, args):
        '''expr : expr PlusMinus expr %prec PLUS
                | PlusMinus expr %prec MINUS'''
        if len(args) == 3:
            args[0] = Expression('PlusMinus', args[2])
        elif len(args) == 4:
            args[0] = Expression('PlusMinus', args[1], args[3])

    def p_MinusPlus(self, args):
        '''expr : expr MinusPlus expr %prec PLUS
                | MinusPlus expr %prec MINUS'''
        if len(args) == 3:
            args[0] = Expression('MinusPlus', args[2])
        elif len(args) == 4:
            args[0] = Expression('MinusPlus', args[1], args[3])

    def p_Slash(self, args):
        'expr : expr Slash expr %prec DIVIDE'
        args[0] = Expression('Times', args[1], Expression('Power', args[3], Integer(-1)))

    def p_Backslash(self, args):
        'expr : expr Backslash expr %prec BACKSLASH'
        args[0] = Expression('Backslash', args[1], args[3])

    def p_Times(self, args):
        '''expr : expr expr %prec TIMES
                | expr Times expr %prec TIMES'''
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
        '''expr : expr Equal expr %prec EQUAL
                | expr Unequal expr %prec EQUAL
                | expr Greater expr %prec EQUAL
                | expr Less expr %prec EQUAL
                | expr GreaterEqual expr %prec EQUAL
                | expr LessEqual expr %prec EQUAL'''
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
        '''expr : expr SameQ expr %prec SAMEQ
                | expr UnsameQ expr %prec SAMEQ'''
        if args[2] == '===':
            args[0] = Expression('SameQ', args[1], args[3])
        elif args[2] == '=!=':
            args[0] = Expression('UnsameQ', args[1], args[3])

    def p_Not(self, args):
        'expr : Bang expr %prec NOT'
        args[0] = Expression('Not', args[2])

    def p_And(self, args):
        'expr : expr And expr %prec AND'
        args[0] = Expression('And', args[1], args[3])

    def p_Xor(self, args):
        'expr : expr Xor expr %prec XOR'
        args[0] = Expression('Xor', args[1], args[3])

    def p_Or(self, args):
        'expr : expr Or expr %prec OR'
        args[0] = Expression('Or', args[1], args[3])

    def p_Repeated(self, args):
        '''expr : expr Repeated %prec REPEATED
                | expr RepeatedNull %prec REPEATED'''
        if args[2] == '..':
            args[0] = Expression('Repeated', args[1])
        elif args[2] == '...':
            args[0] = Expression('RepeatedNull', args[1])

    def p_Alternatives(self, args):
        'expr : expr Alternatives expr %prec ALTERNATIVES'
        args[0] = Expression('Alternatives', args[1], args[3])

    def p_Pattern(self, args):
        'expr : symbol Colon expr %prec PATTERN'
        args[0] = Expression('Pattern', Symbol(args[1]), args[3])

    def p_Optional(self, args):
        'expr : pattern Colon expr %prec PATTERN'
        args[0] = Expression('Optional', args[1], args[3])

    def p_StringExpression(self, args):
        'expr : expr StringExpression expr %prec STRINGEXPRESSION'
        args[0] = Expression('StringExpression', args[1], args[3])

    def p_Condition(self, args):
        'expr : expr Condition expr %prec CONDITION'
        args[0] = Expression('Condition', args[1], args[3])

    def p_Rule(self, args):
        '''expr : expr Rule expr %prec RULE
                | expr RuleDelayed expr %prec RULE'''
        if args[2] == '->':
            args[0] = Expression('Rule', args[1], args[3])
        elif args[2] == ':>':
            args[0] = Expression('RuleDelayed', args[1], args[3])

    def p_Replace(self, args):
        '''expr : expr ReplaceAll expr %prec REPLACE
                | expr ReplaceRepeated expr %prec REPLACE'''
        if args[2] == '/.':
            args[0] = Expression('ReplaceAll', args[1], args[3])
        elif args[2] == '//.':
            args[0] = Expression('ReplaceRepeated', args[1], args[3])

    def p_AddTo(self, args):
        '''expr : expr AddTo expr %prec ADDTO
                | expr SubtractFrom expr %prec ADDTO
                | expr TimesBy expr %prec ADDTO
                | expr DivideBy expr %prec ADDTO'''
        if args[2] == '+=':
            args[0] = Expression('AddTo', args[1], args[3])
        elif args[2] == '-=':
            args[0] = Expression('SubtractFrom', args[1], args[3])
        elif args[2] == '*=':
            args[0] = Expression('TimesBy', args[1], args[3])
        elif args[2] == '/=':
            args[0] = Expression('DivideBy', args[1], args[3])

    def p_Function(self, args):
        'expr : expr Function %prec FUNCTION'
        args[0] = Expression('Function', args[1])

    def p_RawColon(self, args):
        'expr : expr RawColon expr %prec COLON'
        args[0] = Expression('Colon', args[1], args[3])

    def p_Postfix(self, args):
        'expr : expr Postfix expr %prec POSTFIX'
        args[0] = Expression(args[3], args[1])

    def p_Set(self, args):
        '''expr : symbol TagSet expr Set expr %prec SET
                | expr Set expr %prec SET'''
        if len(args) == 4:
            args[0] = Expression('Set', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('TagSet', Symbol(args[1]), args[3], args[5])

    def p_SetDelayed(self, args):
        '''expr : symbol TagSet expr SetDelayed expr %prec SET
                | expr SetDelayed expr %prec SET'''
        if len(args) == 4:
            args[0] = Expression('SetDelayed', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('TagSetDelayed', Symbol(args[1]), args[3], args[5])

    def p_UpSet(self, args):
        'expr : expr UpSet expr %prec SET'
        args[0] = Expression('UpSet', args[1], args[3])

    def p_UpSetDelayed(self, args):
        'expr : expr UpSetDelayed expr %prec SET'
        args[0] = Expression('UpSetDelayed', args[1], args[3])

    def p_Unset(self, args):
        '''expr : symbol TagSet expr Unset %prec SET
                | expr Unset %prec SET'''
        if len(args) == 3:
            args[0] = Expression('Unset', args[1])
        elif len(args) == 4:
            args[0] = Expression('TagUnset', Symbol(args[1]), args[3])

    def p_Put(self, args):
        'expr : expr Put filename %prec PUT'
        args[0] = Expression('Put', args[1], args[3])

    def p_PutAppend(self, args):
        'expr : expr PutAppend filename %prec PUT'
        args[0] = Expression('PutAppend', args[1], args[3])

    def p_Compound(self, args):
        '''expr : expr Semicolon expr %prec COMPOUNDEXPRESSION
                | expr Semicolon %prec COMPOUNDEXPRESSION'''
        if len(args) == 4:
            args[0] = Expression('CompoundExpression', args[1], args[3])
        if len(args) == 3:
            args[0] = Expression('CompoundExpression', args[1], Symbol('Null'))

    def p_FormBox(self, args):
        'expr : expr FormBox expr %prec FORMBOX'
        args[0] = Expression('FormBox', args[3], args[1])

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

assert parse('x ~~ y') == Expression('StringExpression', Symbol('x'), Symbol('y'))
#assert parse('x ~~ y ~~ z') == Expression('StringExpression', Symbol('x'), Symbol('y'), Symbol('z'))

assert parse('x /; y') == Expression('Condition', Symbol('x'), Symbol('y'))
assert parse('x -> y') == Expression('Rule', Symbol('x'), Symbol('y'))

assert parse('x /. y') == Expression('ReplaceAll', Symbol('x'), Symbol('y'))
assert parse('x //. y') == Expression('ReplaceRepeated', Symbol('x'), Symbol('y'))

assert parse('x += y') == Expression('AddTo', Symbol('x'), Symbol('y'))
assert parse('x -= y') == Expression('SubtractFrom', Symbol('x'), Symbol('y'))
assert parse('x *= y') == Expression('TimesBy', Symbol('x'), Symbol('y'))
assert parse('x /= y') == Expression('DivideBy', Symbol('x'), Symbol('y'))

assert parse('x &') == Expression('Function', Symbol('x'))
assert parse('x // y') == Expression('y', Symbol('x'))

assert parse('x = y') == Expression('Set', Symbol('x'), Symbol('y'))
assert parse('x := y') == Expression('SetDelayed', Symbol('x'), Symbol('y'))
assert parse('x ^= y') == Expression('UpSet', Symbol('x'), Symbol('y'))
assert parse('x ^:= y') == Expression('UpSetDelayed', Symbol('x'), Symbol('y'))
assert parse('x =.') == Expression('Unset', Symbol('x'))

assert parse('x/:1=1') == Expression('TagSet', Symbol('x'), Integer(1), Integer(1))
assert parse('x/:1:=1') == Expression('TagSetDelayed', Symbol('x'), Integer(1), Integer(1))
assert parse('x/:1=.') == Expression('TagUnset', Symbol('x'), Integer(1))

assert parse('1 \\` 2') == Expression('FormBox', Integer(2), Integer(1))

#FIXME
#assert parse('1 ; 5') == Expression('CompoundExpression', Integer(1), Integer(5))
#assert parse('1 ;') == Expression('CompoundExpression', Integer(1), Symbol('Null'))

## assert parse('1 ^ 2') == Expression('Power', Integer(1), Integer(2))
## assert parse('{x, y}') == Expression('List', Symbol('x'), Symbol('y'))
## assert parse('{a,}') == Expression('List', Symbol('a'), Symbol('Null'))
## assert parse('{,}') == Expression('List', Symbol('Null'), Symbol('Null'))
## #assert parse('{,a}') == Expression('List', Symbol('Null'), Symbol('a')) #TODO
## assert parse('Sin[x, y]') == Expression('Sin', Symbol('x'), Symbol('y'))
## assert parse('a[[1]]') == Expression('Part', Symbol('a'), Integer(1))
## assert parse('f_') == Expression('Pattern', Symbol('f'), Expression('Blank'))
## assert parse('f__') == Expression('Pattern', Symbol('f'), Expression('BlankSequence'))
## assert parse('f___') == Expression('Pattern', Symbol('f'), Expression('BlankNullSequence'))
## assert parse('#2') == Expression('Slot', Integer(2))
## assert parse('#') == Expression('Slot', Integer(1))
## assert parse('##2') == Expression('SlotSequence', Integer(2))
## assert parse('##') == Expression('SlotSequence', Integer(1))
## assert parse('%2') == Expression('Out', Integer(2))
## assert parse('%') == Expression('Out')
## assert parse('%%') == Expression('Out', Integer(-2))
## assert parse('%%%%') == Expression('Out', Integer(-4))

assert parse('x ! y') == Expression('Times', Expression('Factorial', Symbol('x')), Symbol('y'))
assert parse('x ^ 2 y') == Expression('Times', Expression('Power', Symbol('x'), Integer(2)), Symbol('y'))
quit()
