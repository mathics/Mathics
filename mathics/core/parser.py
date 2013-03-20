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
import unicodedata
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
    ('right', 'COMPOUNDEXPRESSION'),
    ('nonassoc', 'PUT'),
    ('nonassoc', 'SET2'),
    ('right', 'SET'),
    ('left', 'BECAUSE'),
    ('right', 'THEREFORE'),
    ('left', 'VERTICALSEPARATOR'),         # flat
    ('right', 'POSTFIX'),
    ('right', 'COLON'),
    ('nonassoc', 'AMPERSAND'),
    ('right', 'ADDTO'),
    ('left', 'REPLACE'),
    ('right', 'RULE'),
    ('left', 'CONDITION'),
    ('left', 'STRINGEXPRESSION'),
    ('nonassoc', 'PATTERN'),
    ('left', 'ALTERNATIVES'),
    ('nonassoc', 'REPEATED'),
    ('right', 'SUCHTHAT'),
    ('right', 'IMPLIES'),
    ('left', 'EQUIVALENT'),
    ('left', 'OR'),
    ('left', 'XOR'),
    ('left', 'AND'),
    ('right', 'NOT'),
    ('right', 'FORALL'),
    ('left', 'ELEMENT'),
    ('left', 'SAMEQ'),
    ('left', 'EQUAL'),
    ('left', 'SPAN'),
    ('left', 'UNION'),                      # flat
    ('left', 'INTERSECTION'),               # flat
    ('left', 'PLUS'),                       # flat
    #('left', 'SUM'),                       # flat
    ('left', 'CIRCLEPLUS'),                 # flat
    #('left', 'CAP'),                       # flat
    #('left', 'COPRODUCT'),                 # flat
    #('left', 'VERTICALTILDE'),             # flat
    #('left', 'PRODUCT'),
    ('left', 'STAR'),                       # flat
    ('left', 'TIMES'),                      # flat
    ('left', 'DIAMOND'),                    # flat
    ('left', 'WEDGE'),                      # flat
    ('left', 'VEE'),                        # flat
    ('left', 'CIRCLETIMES'),                # flat
    ('left', 'CENTERDOT'),                  # flat
    ('right', 'BACKSLASH'),
    ('left', 'DIVIDE'),
    ('nonassoc', 'MINUS'),
    ('left', 'DOT'),                       # flat
    ('left', 'CROSS'),                     # flat
    ('left', 'NONCOMMUTATIVEMULTIPLY'),    # flat
    ('right', 'CIRCLEDOT'),
    ('right', 'SQUARE'),
    ('right', 'DEL'),
    ('right', 'INTEGRATE'),
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

tokens = (
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
    'RawLeftBracket',
    'RawRightBracket',
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
    'Integral',
    'DifferentialD',
    'PartialD',
    'Del',
    'Square',
    'CircleDot',
    'SmallCircle',
    'NonCommutativeMultiply',
    'Cross',
    'Dot',
    'Plus',
    'Minus',
    'PlusMinus',
    'MinusPlus',
    'Slash',
    'Backslash',
    'Diamond',
    'Wedge',
    'Vee',
    'CircleTimes',
    'CenterDot',
    'Star',
    'Sum',
    'Product',
    'Asterisk',
    'Times',
    'Divide',
    'op_Equal',
    'op_Unequal',
    'Greater',
    'Less',
    'op_GreaterEqual',
    'op_LessEqual',
    'SameQ',
    'UnsameQ',
    'op_And',
    'op_Or',
    'Repeated',
    'RepeatedNull',
    'Alternatives',
    'RawColon',
    'StringExpression',
    'Condition',
    'op_Rule',
    'op_RuleDelayed',
    'ReplaceAll',
    'ReplaceRepeated',
    'AddTo',
    'SubtractFrom',
    'TimesBy',
    'DivideBy',
    'RawAmpersand',
    'Colon',
    'Postfix',
    'Set',
    'SetDelayed',
    'UpSet',
    'UpSetDelayed',
    'TagSet',
    'Unset',
    'Semicolon',
    'FormBox',
    'DiscreteShift',
    'DiscreteRatio',
    'DifferenceDelta',
    'VerticalTilde',
    'Coproduct',
    'Cap',
    'Cup',
    'CirclePlus',
    'CircleMinus',
    'Intersection',
    'Union',
    'Equal',
    'LongEqual',
    'NotEqual',
    'LessEqual',
    'LessSlantEqual',
    'GreaterEqual',
    'GreaterSlantEqual',
    'VerticalBar',
    'NotVerticalBar',
    'DoubleVerticalBar',
    'NotDoubleVerticalBar',
    'Element',
    'NotElement',
    'Subset',
    'Superset',
    'ForAll',
    'Exists',
    'NotExists',
    'Not',
    'And',
    'Nand',
    'Xor',
    'Xnor',
    'Or',
    'Nor',
    'Equivalent',
    'Implies',
    'RightTee',
    'DoubleRightTee',
    'LeftTee',
    'DoubleLeftTee',
    'SuchThat',
    'Rule',
    'RuleDelayed',
    'VerticalSeparator',
    'Therefore',
    'Because',
    'Function',
)

literals = ['(', ')', '{', '}', ',']

class MathicsScanner:
    tokens = tokens
    literals = literals
    precedence = precedence

    #t_ignore = ur' [\s \u2062]+ '
    t_ignore = ' \t\n '

    t_symbol = r' [a-zA-Z$][a-zA-Z0-9$]* '
    t_int = r' \d+ '

    t_RawLeftBracket = r' \[ '
    t_RawRightBracket = r' \] '

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

    t_Transpose = ur' \\\[Transpose\]|\uf3c7 '
    t_Conjugate = ur' \\\[Conjugate\]|\uf3c8 '
    t_ConjugateTranspose = ur' \\\[ConjugateTranspose\]|\uf3c9 '

    t_Derivative = r' \'+ '
    t_StringJoin = r' \<\> '

    t_Power = r' \^ '
    t_Power2 = r' \\\^ '
    t_Sqrt = r' \\\@ '

    t_Integral = ur' \\\[Integral\]|\u222b '
    t_DifferentialD = ur'\\\[DifferentialD\]|\uf74c '
    t_PartialD = ur' \\\[PartialD\]|\u2202 '
    t_Del = ur' \\\[Del\]|\u2207 '
    
    t_Square = ur' \\\[Square\]|\uf520 '
    t_SmallCircle = ur' \\\[SmallCircle\]|\u2218 '
    t_CircleDot = ur' \\\[CircleDot\]|\u2299 '

    t_NonCommutativeMultiply = r' \*\* '

    t_Cross = ur' \\\[Cross\]|\uf4a0 '
    t_Dot = r' \. '

    t_Plus = r' \+ '
    t_Minus = r' \- '
    t_Slash = r' \/ '
    t_Backslash = r' \\ '

    t_Diamond = ur' \\\[Diamond\]|\u22c4 '
    t_Wedge = ur' \\\[Wedge\]|\u22c0 '
    t_Vee = ur' \\\[Vee\]|\u22c1 '
    t_CircleTimes = ur' \\\[CircleTimes\]|\u2297 '
    t_CenterDot = ur' \\\[CenterDot\]|\u00b7 '
    t_Star = ur' \\\[Star\]|\u22c6'

    t_Sum = ur' \\\[Sum\]|\u2211 '
    t_Product = ur' \\\[Product\]|\u220f '

    t_Asterisk = r' \* '
    t_Times = ur'\\\[Times\]|\u00d7 '
    t_Divide = ur' \\\[Divide\]|\u00f7 '

    #TODO
    t_PlusMinus = ur' \\\[PlusMinus\]|\u00b1 '
    t_MinusPlus = ur' \\\[MinusPlus\]|\u2213 '

    t_op_Equal = r' \=\= '
    t_op_Unequal = r' \!\= '
    t_Greater = r' \> '
    t_Less = r' \< '
    t_op_GreaterEqual = r' \>\= '
    t_op_LessEqual = r' \<\= '

    t_SameQ = r' \=\=\= '
    t_UnsameQ = r' \=\!\= '

    t_op_And = r' \&\& '
    t_op_Or = r' \|\|  '

    t_Or = ur' \\\[Or\]|\u2228 '
    t_Nor = ur' \\\[Nor\]|\u22BD '

    t_And = ur' \\\[And\]|\u2227 '
    t_Nand = ur' \\\[Nand\]|\u22BC '

    t_Xor = ur' \\\[Xor\]|\u22BB '
    t_Xnor = ur' \\\[Xnor\]|\uF4A2 '

    t_Repeated = r' \.\. '
    t_RepeatedNull = r' \.\.\. '
    t_Alternatives = r' \| '

    t_RawColon = r' \: '
    t_StringExpression = r' \~\~ '
    t_Condition = r' \/\; '

    t_op_Rule = r' \-\> '
    t_op_RuleDelayed = r' \:\> '
    t_ReplaceAll = r' \/\. '
    t_ReplaceRepeated = r' \/\/\. '

    t_AddTo = r' \+\= '
    t_SubtractFrom = r' \-\=  '
    t_TimesBy = r' \*\= '
    t_DivideBy = r' \/\=  '

    t_RawAmpersand = r' \& '
    t_Colon = ur' \\\[Colon\]|\u2236 ' 
    t_Postfix = r' \/\/ '

    t_Set = r' \= '
    t_SetDelayed = r' \:\= '
    t_UpSet = r' \^\= '
    t_UpSetDelayed = r' \^\:\= '
    t_TagSet = r' \/\: '
    t_Unset = r' \=\. '

    t_Semicolon = r' \; '
    t_FormBox = r' \\\` '

    t_DiscreteShift = ur' \\\[DiscreteShift\]|\uf4a3 '
    t_DiscreteRatio = ur' \\\[DiscreteRatio\]|\uf4a4 '
    t_DifferenceDelta = ur' \\\[DifferenceDelta\]|\u2206 '
    t_VerticalTilde = ur' \\\[VerticalTilde\]|\u2240 '
    t_Coproduct = ur' \\\[Coproduct\]|\u2210 '
    t_Cap = ur' \\\[Cap\]|\u2322 '
    t_Cup = ur' \\\[Cup\]|\u2323 '
    t_CirclePlus = ur' \\\[CirclePlus\]|\u2295 '
    t_CircleMinus = ur' \\\[CircleMinus\]|\u2296 '
    t_Intersection = ur' \\\[Intersection\]|\u22c2 '
    t_Union = ur' \\\[Union\]|\u22c3 '
    t_Equal = ur' \\\[Equal\]|\uf431 '
    t_LongEqual = ur' \\\[LongEqual\]|\uf7d9 '
    t_NotEqual = ur' \\\[NotEqual\]|\u2260 '
    t_LessEqual = ur' \\\[LessEqual\]|\u2264 '
    t_LessSlantEqual = ur' \\\[LessSlantEqual\]|\u2a7d '
    t_GreaterEqual = ur' \\\[GreaterEqual\]|\u2265 '
    t_GreaterSlantEqual = ur' \\\[GreaterSlantEqual\]|\u2a7e '
    t_VerticalBar = ur' \\\[VerticalBar\]|\u2223 '
    t_NotVerticalBar = ur' \\\[NotVerticalBar\]|\u2224 '
    t_DoubleVerticalBar = ur' \\\[DoubleVerticalBar\]|\u2225 '
    t_NotDoubleVerticalBar = ur' \\\[NotDoubleVerticalBar\]|\u2226 '
    t_Element = ur' \\\[Element\]|\u2208 '
    t_NotElement = ur' \\\[NotElement\]|\u2209 '
    t_Subset = ur' \\\[Subset\]|\u2282 '
    t_Superset = ur' \\\[Superset\]|\u2283 '
    t_ForAll = ur' \\\[ForAll\]|\u2200 '
    t_Exists = ur' \\\[Exists\]|\u2203 '
    t_NotExists = ur' \\\[NotExists\]|\u2204 '
    t_Not = ur' \\\[Not\]|\u00AC '
    t_Equivalent = ur' \\\[Equivalent\]|\u29E6 '
    t_Implies = ur' \\\[Implies\]|\uF523 '
    t_RightTee = ur' \\\[RightTee\]|\u22A2 '
    t_DoubleRightTee = ur' \\\[DoubleRightTee\]|\u22A8 '
    t_LeftTee = ur' \\\[LeftTee\]|\u22A3 '
    t_DoubleLeftTee = ur' \\\[DoubleLeftTee\]|\u2AE4 '
    t_SuchThat = ur' \\\[SuchThat\]|\u220D '
    t_Rule = ur' \\\[Rule\]|\uF522 '
    t_RuleDelayed = ur' \\\[RuleDelayed\]|\uF51F '
    t_VerticalSeparator = ur' \\\[VerticalSeparator\]|\uF432 '
    t_Therefore = ur' \\\[Therefore\]|\u2234 '
    t_Because = ur' \\\[Because\]|\u2235 '
    t_Function = ur' \\\[Function\]|\uF4A1 '

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
            #entity = additional_entities.get(name)
            entity = None
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

    def t_blankdefault(self, t):    # this must come before t_blanks
        r' ([a-zA-Z$][a-zA-Z0-9$]*)?_\. '
        return t

    def t_blanks(self, t):
        r' ([a-zA-Z$][a-zA-Z0-9$]*)?_(__?)?([a-zA-Z$][a-zA-Z0-9$]*)? '
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

def Flat(operator, args):
    if len(args) == 2:
        return Expression(operator, *args[1])
    elif isinstance(args[1], list):
        args[1].append(args[3])
        return args[1]
    else:
        return [args[1], args[3]]

def FLAT(op_tokens, prec):
    def set_doc(f, op_tokens=op_tokens, prec=prec):
        if not isinstance(op_tokens, list):
            op_tokens = [op_tokens]
        tokenname = op_tokens[0]+'TOKEN'
        rule1 = ['{0} : expr {1} expr %prec {2}'.format(tokenname, op, prec) for op in op_tokens]
        rule2 = ['{0} : {0} {1} expr %prec {2}'.format(tokenname, op, prec) for op in op_tokens]
        rule3 = ['expr : {0}'.format(tokenname)]
        f.__doc__ = '\n'.join(rule1 + rule2 + rule3)
        return f
    return set_doc

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
        "expr : '(' expr ')'"
        expr = args[2]
        expr.parenthesized = True
        args[0] = expr

    def p_call(self, args):
        'expr : expr args %prec PART'
        expr = Expression(args[1], *args[2].items)
        expr.parenthesized = True # to handle e.g. Power[a,b]^c correctly
        args[0] = expr
    
    def p_part(self, args):
        'expr : expr position %prec PART'
        args[0] = Expression('Part', args[1], *args[2].items)
    
    def p_args(self, args):
        'args : RawLeftBracket sequence RawRightBracket'
        args[0] = ArgsToken(args[2].items)
    
    def p_list(self, args):
        "expr : '{' sequence '}'"
        args[0] = Expression('List', *args[2].items)
    
    def p_position(self, args):
        'position : RawLeftBracket RawLeftBracket sequence RawRightBracket RawRightBracket'
        args[0] = PositionToken(args[3].items)

    def p_sequence(self, args):
        '''sequence :
                    | sequence1'''
        if len(args) == 1:
            args[0] = SequenceToken([])
        else:
            args[0] = args[1]

    def p_sequence1(self, args):
        '''sequence1 : sequence1 ',' expr
                     | sequence1 ','
                     | ',' sequence1
                     | expr
                     | ',' '''
        if len(args) == 4:
            args[1].items.append(args[3])
            args[0] = args[1]
        elif len(args) == 3:
            if args[2] == ',':
                args[1].items.append(Symbol('Null'))
                args[0] = args[1]
            elif args[1] == ',':
                args[2].items.insert(0, Symbol('Null'))
                args[0] = args[2]
        elif len(args) == 2:
            if isinstance(args[1], BaseExpression):
                args[0] = SequenceToken([args[1]])
            elif args[1] == ',':
                args[0] = SequenceToken([Symbol('Null'), Symbol('Null')])

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

    def p_pattern(self, args):
        'expr : pattern'
        args[0] = args[1]
        
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
                | expr MessageName symbol MessageName string %prec MESSAGENAME
                | expr MessageName symbol %prec MESSAGENAME
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
        elif args[2] == '/@':
            args[0] = Expression('Map', args[1], args[3])
        elif args[2] == '//@':
            args[0] = Expression('MapAll', args[1], args[3])

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

    @FLAT('StringJoin', 'STRINGJOIN')
    def p_StringJoin(self, args):
        args[0] = Flat('StringJoin', args)

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

    def p_Integrate(self, args):
        'expr : Integral expr DifferentialD expr %prec INTEGRATE'
        args[0] = Expression('Integrate', args[2], args[4])

    #TODO: p_D(self, args):

    def p_Del(self, args):
        'expr : Del expr %prec DEL'
        expr = Expression('Del', args[2])

    def p_Square(self, args):
        'expr : Square expr %prec SQUARE'
        expr = Expression('Square', args[2])

    @FLAT('SmallCircle', 'SQUARE')
    def p_SmallCircle(self, args):
        args[0] = Flat('SmallCircle', args)

    @FLAT('CircleDot', 'CIRCLEDOT')
    def p_CircleDot(self, args):
        args[0] = Flat('CircleDot', args)

    @FLAT('NonCommutativeMultiply', 'NONCOMMUTATIVEMULTIPLY')
    def p_NonCommutativeMultiply(self, args):
        args[0] = Flat('NonCommutativeMultiply', args)

    @FLAT('Cross', 'CROSS')
    def p_Cross(self, args):
        args[0] = Flat('Cross', args)

    @FLAT('Dot', 'DOT')
    def p_Dot(self, args):
        args[0] = Flat('Dot', args)

    @FLAT('Plus', 'PLUS')
    def p_Plus(self, args):
        args[0] = Flat('Plus', args)

    def p_Minus(self, args):
        'expr : expr Minus expr %prec PLUS'
        args[0] = Expression('Plus', args[1], Expression('Times', Integer(-1), args[3]))

    def p_UPlus(self, args):
        'expr : Plus expr %prec MINUS'
        args[0] = args[2]

    def p_UMinus(self, args):
        'expr : Minus expr %prec MINUS'''
        args[0] = Expression('Times', Integer(-1), args[2])

    def p_PlusMinus(self, args):
        'expr : expr PlusMinus expr %prec PLUS'
        args[0] = Expression('PlusMinus', args[1], args[3])

    def p_UPlusMinus(self, args):
        'expr : PlusMinus expr %prec MINUS'''
        args[0] = Expression('PlusMinus', args[2])

    def p_MinusPlus(self, args):
        'expr : expr MinusPlus expr %prec PLUS'
        args[0] = Expression('MinusPlus', args[1], args[3])

    def p_UMinusPlus(self, args):
        'expr : MinusPlus expr %prec MINUS'
        args[0] = Expression('MinusPlus', args[2])

    @FLAT('Intersection', 'INTERSECTION')
    def p_Intersection(self, args):
        args[0] = Flat('Intersection', args)

    @FLAT('Union', 'UNION')
    def p_Union(self, args):
        args[0] = Flat('Union', args)

    def p_Slash(self, args):
        '''expr : expr Slash expr %prec DIVIDE
                | expr Divide expr %prec DIVIDE'''
        args[0] = Expression('Times', args[1], Expression('Power', args[3], Integer(-1)))

    @FLAT('Diamond', 'DIAMOND')
    def p_Diamond(self, args):
        args[0] = Flat('Diamond', args)

    @FLAT('Wedge', 'WEDGE')
    def p_Wedge(self, args):
        args[0] = Flat('Wedge', args)

    @FLAT('Vee', 'VEE')
    def p_Vee(self, args):
        args[0] = Flat('Vee', args)

    @FLAT('CircleTimes', 'CIRCLETIMES')
    def p_CircleTimes(self, args):
        args[0] = Flat('CircleTimes', args)

    @FLAT('CirclePlus', 'CIRCLEPLUS')
    def p_CirclePlus(self, args):
        args[0] = Flat('CirclePlus', args)

    @FLAT('CircleMinus', 'CIRCLEPLUS')
    def p_CircleMinus(self, args):
        args[0] = Flat('CircleMinus', args)

    @FLAT('CenterDot', 'CENTERDOT')
    def p_CenterDot(self, args):
        args[0] = Flat('CenterDot', args)

    @FLAT('Star', 'STAR')
    def p_Star(self, args):
        args[0] = Flat('Star', args)

    @FLAT('Backslash', 'BACKSLASH')
    def p_Backslash(self, args):
        args[0] = Flat('Backslash', args)
    
    @FLAT(['Times', 'Asterisk', ''], 'TIMES')
    def p_Times(self, args):
        if len(args) == 2:
            args[0] = Expression('Times', *args[1])
        elif isinstance(args[1], list):
            if len(args) == 3:
                args[1].append(args[2])
            elif len(args) == 4:
                args[1].append(args[3])
            args[0] = args[1]
        else:
            if len(args) == 3:
                args[0] = [args[1], args[2]] 
            elif len(args) == 4:
                args[0] = [args[1], args[3]] 

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

    @FLAT(['op_Equal', 'LongEqual', 'Equal'], "EQUAL")
    def p_Equal(self, args):
        args[0] = Flat('Equal', args)

    @FLAT(['op_Unequal', 'NotEqual'], "EQUAL")
    def p_Unequal(self, args):
        args[0] = Flat('Unequal', args)

    @FLAT('Greater', 'EQUAL')
    def p_Greater(self, args):
        args[0] = Flat('Greater', args)

    @FLAT('Less', 'EQUAL')
    def p_Less(self, args):
        args[0] = Flat('Less', args)

    @FLAT(['op_GreaterEqual', 'GreaterEqual', 'GreaterSlantEqual'], 'EQUAL')
    def p_GreaterEqual(self, args):
        args[0] = Flat('GreaterEqual', args)

    @FLAT(['op_LessEqual', 'LessEqual', 'LessSlantEqual'], 'EQUAL')
    def p_LessEqual(self, args):
        args[0] = Flat('LessEqual', args)

    @FLAT('VerticalBar', 'EQUAL')
    def p_VerticalBar(self, args):
        args[0] = Flat('VerticalBar', args)

    @FLAT('NotVerticalBar', 'EQUAL')
    def p_NotVerticalBar(self, args):
        args[0] = Flat('NotVerticalBar', args)

    @FLAT('DoubleVerticalBar', 'EQUAL')
    def p_DoubleVerticalBar(self, args):
        args[0] = Flat('DoubleVerticalBar', args)

    @FLAT('NotDoubleVerticalBar', 'EQUAL')
    def p_NotDoubleVerticalBar(self, args):
        args[0] = Flat('NotDoubleVerticalBar', args)

    @FLAT('SameQ', 'SAMEQ')
    def p_SameQ(self, args):
        args[0] = Flat('SameQ', args)

    @FLAT('UnsameQ', 'SAMEQ')
    def p_Unsame(self, args):
        args[0] = Flat('UnsameQ', args)

    @FLAT('Element', 'ELEMENT')
    def p_Element(self, args):
        args[0] = Flat('Element', args)

    @FLAT('NotElement', 'ELEMENT')
    def p_NotElement(self, args):
        args[0] = Flat('NotElement', args)

    @FLAT('Subset', 'ELEMENT')
    def p_Subset(self, args):
        args[0] = Flat('Subset', args)

    @FLAT('Superset', 'ELEMENT')
    def p_Superset(self, args):
        args[0] = Flat('Superset', args)

    def p_ForAll(self, args):
        'expr : ForAll expr %prec FORALL'
        args[0] = Expression('ForAll', args[2])

    def p_Exists(self, args):
        'expr : Exists expr %prec FORALL'
        args[0] = Expression('Exists', args[2])

    def p_NotExists(self, args):
        'expr : NotExists expr %prec FORALL'
        args[0] = Expression('NotExists', args[2])

    def p_Not(self, args):
        '''expr : Bang expr %prec NOT
                | Not expr %prec NOT'''
        args[0] = Expression('Not', args[2])

    @FLAT(['And', 'op_And'], 'AND')
    def p_And(self, args):
        args[0] = Flat('And', args)

    @FLAT('Nand', 'AND')
    def p_Nand(self, args):
        args[0] = Flat('Nand', args)

    @FLAT('Xor', 'XOR')
    def p_Xor(self, args):
        args[0] = Flat('Xor', args)

    @FLAT('Xnor', 'XOR')
    def p_Xnor(self, args):
        args[0] = Flat('Xnor', args)

    @FLAT(['op_Or', 'Or'], 'OR')
    def p_Or(self, args):
        args[0] = Flat('Or', args)

    @FLAT('Nor', 'OR')
    def p_Nor(self, args):
        args[0] = Flat('Nor', args)

    @FLAT('Equivalent', 'EQUIVALENT')
    def p_Equivalent(self, args):
        args[0] = Flat('Equivalent', args)

    def p_Implies(self, args):
        'expr : expr Implies expr %prec IMPLIES'
        args[0] = Expression('Implies', args[1], args[3])

    def p_SuchThat(self, args):
        'expr : expr SuchThat expr %prec SUCHTHAT'
        args[0] = Expression('SuchThat', args[1], args[3])

    def p_Repeated(self, args):
        '''expr : expr Repeated %prec REPEATED
                | expr RepeatedNull %prec REPEATED'''
        if args[2] == '..':
            args[0] = Expression('Repeated', args[1])
        elif args[2] == '...':
            args[0] = Expression('RepeatedNull', args[1])

    @FLAT('Alternatives', 'ALTERNATIVES')
    def p_Alternatives(self, args):
        args[0] = Flat('Alternatives', args)

    def p_Pattern(self, args):
        'expr : symbol RawColon expr %prec PATTERN'
        args[0] = Expression('Pattern', Symbol(args[1]), args[3])

    def p_Optional(self, args):
        'expr : pattern RawColon expr %prec PATTERN'
        args[0] = Expression('Optional', args[1], args[3])

    @FLAT('StringExpression', 'STRINGEXPRESSION')
    def p_StringExpression(self, args):
        args[0] = Flat('StringExpression', args)

    def p_Condition(self, args):
        'expr : expr Condition expr %prec CONDITION'
        args[0] = Expression('Condition', args[1], args[3])

    def p_Rule(self, args):
        '''expr : expr op_Rule expr %prec RULE
                | expr Rule expr %prec RULE'''
        args[0] = Expression('Rule', args[1], args[3])

    def p_RuleDelayed(self, args):
        '''expr : expr op_RuleDelayed expr %prec RULE
                | expr RuleDelayed expr %prec RULE'''
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

    def p_Ampersand(self, args):
        'expr : expr RawAmpersand %prec AMPERSAND'
        args[0] = Expression('Function', args[1])

    @FLAT('Colon', 'COLON')
    def p_Colon(self, args):
        args[0] = Flat('Colon', args)

    def p_Postfix(self, args):
        'expr : expr Postfix expr %prec POSTFIX'
        args[0] = Expression(args[3], args[1])

    @FLAT('VerticalSeparator', 'VERTICALSEPARATOR')
    def p_VerticalSeparator(self, args):
        args[0] = Flat('VerticalSeparator', args)

    def p_Therefore(self, args):
        'expr : expr Therefore expr %prec THEREFORE'
        args[0] = Expression('Therefore', args[1], args[3])

    def p_Because(self, args):
        'expr : expr Because expr %prec BECAUSE'
        args[0] = Expression('Because', args[1], args[3])

    def p_Set(self, args):
        '''expr : expr TagSet expr Set expr %prec SET2
                | expr Set expr %prec SET'''
        if len(args) == 4:
            args[0] = Expression('Set', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('TagSet', args[1], args[3], args[5])

    def p_SetDelayed(self, args):
        '''expr : expr TagSet expr SetDelayed expr %prec SET2
                | expr SetDelayed expr %prec SET'''
        if len(args) == 4:
            args[0] = Expression('SetDelayed', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('TagSetDelayed', args[1], args[3], args[5])

    def p_UpSet(self, args):
        'expr : expr UpSet expr %prec SET'
        args[0] = Expression('UpSet', args[1], args[3])

    def p_UpSetDelayed(self, args):
        'expr : expr UpSetDelayed expr %prec SET'
        args[0] = Expression('UpSetDelayed', args[1], args[3])

    def p_Unset(self, args):
        '''expr : expr TagSet expr Unset %prec SET2
                | expr Unset %prec SET2'''
        if len(args) == 3:
            args[0] = Expression('Unset', args[1])
        elif len(args) == 4:
            args[0] = Expression('TagUnset', args[1], args[3])

    def p_Function(self, args):
        'expr : expr Function expr %prec SET2'
        args[0] = Expression('Function', Expression('List', args[1]), args[3])

    def p_Put(self, args):
        'expr : expr Put filename %prec PUT'
        args[0] = Expression('Put', args[1], args[3])

    def p_PutAppend(self, args):
        'expr : expr PutAppend filename %prec PUT'
        args[0] = Expression('PutAppend', args[1], args[3])

    def p_Compound(self, args):
        '''CompoundToken : expr Semicolon expr %prec COMPOUNDEXPRESSION
                         | expr Semicolon %prec COMPOUNDEXPRESSION
                         | expr Semicolon CompoundToken %prec COMPOUNDEXPRESSION
                    expr : CompoundToken'''
        if len(args) == 2:
            args[0] = Expression('CompoundExpression', *args[1])
        if len(args) == 3:
            args[0] = [args[1], Symbol('Null')]
        if len(args) == 4:
            if isinstance(args[3], list):
                args[3].append(args[1])
                args[0] = args[3]
            else:
                args[0] = [args[1], args[3]]

    def p_FormBox(self, args):
        'expr : expr FormBox expr %prec FORMBOX'
        args[0] = Expression('FormBox', args[3], args[1])

scanner = MathicsScanner()
scanner.build()
parser = MathicsParser()
parser.build()

def parse(string):
    #print "#>", string
    return parser.parse(string)

assert parse('1') == Integer(1)
assert parse('1.4') == Real('1.4')
assert parse('xX') == Symbol('xX')
assert parse('"abc 123"') == String('abc 123')
assert parse('145 (* abf *) 345')==Expression('Times',Integer(145),Integer(345))

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

assert parse('z \\[Conjugate]') == Expression('Conjugate', Symbol('z'))
assert parse('z \\[Transpose]') == Expression('Transpose', Symbol('z'))
assert parse('z \\[ConjugateTranspose]') == Expression('ConjugateTranspose', Symbol('z'))

assert parse(u'z \uf3c7 ') == Expression('Transpose', Symbol('z'))
assert parse(u'z \uf3c8 ') == Expression('Conjugate', Symbol('z'))
assert parse(u'z \uf3c9 ') == Expression('ConjugateTranspose', Symbol('z'))

assert parse("f'") == Expression(Expression('Derivative', Integer(1)), Symbol('f'))
assert parse("f''") == Expression(Expression('Derivative', Integer(2)), Symbol('f'))

assert parse('1 <> 2 ') == Expression('StringJoin', Integer(1), Integer(2))
assert parse('1 <> 2 <> 3') == Expression('StringJoin', Integer(1), Integer(2), Integer(3))

assert parse('1 ^ 2') == Expression('Power', Integer(1), Integer(2))
assert parse('1 \^ 2 \% 3') == Expression('Power', Expression('Subscript', Integer(1), Integer(3)), Integer(2))
assert parse('\@ 3') == Expression('Sqrt', Integer(3))
assert parse('\@ 3 \% 3') == Expression('Power', Integer(3), Expression('Times', Integer(1), Expression('Power', Integer(3), Integer(-1))))

assert parse('\\[Integral] x \\[DifferentialD] x') == Expression('Integrate', Symbol('x'), Symbol('x'))
assert parse('\\[Del] x') == Expression('Del', Symbol('x'))

assert parse('\\[Square] x') == Expression('Square', Symbol('x'))
assert parse('1 \\[SmallCircle] 2') == Expression('SmallCircle', Integer(1), Integer(2))
assert parse('1 \\[SmallCircle] 2 \\[SmallCircle] 3') == Expression('SmallCircle', Integer(1), Integer(2), Integer(3))
assert parse(u'1 \u2218 2') == Expression('SmallCircle', Integer(1), Integer(2))

assert parse('1 \\[CircleDot] 2') == Expression('CircleDot', Integer(1), Integer(2))
assert parse(u'1 \u2299 2') == Expression('CircleDot', Integer(1), Integer(2))

assert parse('1 \\[Diamond] 2') == Expression('Diamond', Integer(1), Integer(2))
assert parse('1 \\[Wedge] 2') == Expression('Wedge', Integer(1), Integer(2))
assert parse('1 \\[Vee] 2') == Expression('Vee', Integer(1), Integer(2))
assert parse('1 \\[CircleTimes] 2') == Expression('CircleTimes', Integer(1), Integer(2))
assert parse('1 \\[CenterDot] 2') == Expression('CenterDot', Integer(1), Integer(2))
assert parse('1 \\[Star] 2') == Expression('Star', Integer(1), Integer(2))

assert parse(u'1 \u22C4 2') == Expression('Diamond', Integer(1), Integer(2))
assert parse(u'1 \u22C0 2') == Expression('Wedge', Integer(1), Integer(2))
assert parse(u'1 \u22c1 2') == Expression('Vee', Integer(1), Integer(2))
assert parse(u'1 \u2297 2') == Expression('CircleTimes', Integer(1), Integer(2))
assert parse(u'1 \u00B7 2') == Expression('CenterDot', Integer(1), Integer(2))
assert parse(u'1 \u22C6 2') == Expression('Star', Integer(1), Integer(2))

assert parse('expr1 ** expr2') == Expression('NonCommutativeMultiply', Symbol('expr1'), Symbol('expr2'))
assert parse('expr1 ** expr2 ** expr3') == Expression('NonCommutativeMultiply', Symbol('expr1'), Symbol('expr2'), Symbol('expr3'))

assert parse('1 . 2') == Expression('Dot', Integer(1), Integer(2))
assert parse('1 \\[Cross] 2') == Expression('Cross', Integer(1), Integer(2))
assert parse(u'1 \uf4a0 2') == Expression('Cross', Integer(1), Integer(2))

assert parse('+1') == Integer(1)
assert parse('-1') == Expression('Times', Integer(-1), Integer(1))

assert parse('3/2') == Expression('Times', Integer(3), Expression('Power', Integer(2), Integer(-1)))
assert parse('3\\[Divide]2') == Expression('Times', Integer(3), Expression('Power', Integer(2), Integer(-1)))
assert parse(u'3 \u00f7 2') == Expression('Times', Integer(3), Expression('Power', Integer(2), Integer(-1)))

assert parse('3\\2') == Expression('Backslash', Integer(3), Integer(2))

assert parse('1 2') == Expression('Times', Integer(1), Integer(2))
assert parse('1*2') == Expression('Times', Integer(1), Integer(2))
assert parse('1 2 3') == Expression('Times', Integer(1), Integer(2), Integer(3))
assert parse('1*2*3') == Expression('Times', Integer(1), Integer(2), Integer(3))

assert parse('1 \\[Times] 2') == Expression('Times', Integer(1), Integer(2))
assert parse(u'1 \u00d7 2') == Expression('Times', Integer(1), Integer(2))

assert parse('1 + 2') == Expression('Plus', Integer(1), Integer(2))
assert parse('1 - 2') == Expression('Plus', Integer(1), Expression('Times', Integer(-1), Integer(2)))
assert parse('1 + 2 + 3') == Expression('Plus', Integer(1), Integer(2), Integer(3))
assert parse('1 + 2 + 3 + 4') == parse('Plus[1, 2, 3, 4]')

assert parse('1 \[PlusMinus] 2') == Expression('PlusMinus', Integer(1), Integer(2))
assert parse('1 \[MinusPlus] 2') == Expression('MinusPlus', Integer(1), Integer(2))
assert parse('\[PlusMinus] 1') == Expression('PlusMinus', Integer(1))
assert parse('\[MinusPlus] 1') == Expression('MinusPlus', Integer(1))

assert parse(u'\u00b1 1') == Expression('PlusMinus', Integer(1))
assert parse(u'\u2213 1') == Expression('MinusPlus', Integer(1))

#FIXME
#assert parse('1;;2;;3') == Expression('Span', Integer(1), Integer(2), Integer(3))
#assert parse('1;; ;;3') == Expression('Span', Integer(1), Symbol('All'), Integer(3))
#assert parse(' ;;2;;3') == Expression('Span', Integer(1), Integer(2), Integer(3))
 
assert parse('1 == 2') == Expression('Equal', Integer(1), Integer(2))
assert parse('1 != 2') == Expression('Unequal', Integer(1), Integer(2))
assert parse('1 == 2 == 3') == Expression('Equal', Integer(1), Integer(2), Integer(3))
assert parse('1 != 2 != 3') == Expression('Unequal', Integer(1), Integer(2), Integer(3))

assert parse('1 > 2') == Expression('Greater', Integer(1), Integer(2))
assert parse('1 >= 2') == Expression('GreaterEqual', Integer(1), Integer(2))
assert parse('1 < 2') == Expression('Less', Integer(1), Integer(2))
assert parse('1 <= 2') == Expression('LessEqual', Integer(1), Integer(2))

assert parse('1 > 2 > 3') == Expression('Greater', Integer(1), Integer(2), Integer(3))
assert parse('1 >= 2 >= 3') == Expression('GreaterEqual', Integer(1), Integer(2), Integer(3))
assert parse('1 < 2 < 3') == Expression('Less', Integer(1), Integer(2), Integer(3))
assert parse('1 <= 2 <= 3') == Expression('LessEqual', Integer(1), Integer(2), Integer(3))

assert parse('1 === 2') == Expression('SameQ', Integer(1), Integer(2))
assert parse('1 =!= 2') == Expression('UnsameQ', Integer(1), Integer(2))
assert parse('1 === 2 === 3') == Expression('SameQ', Integer(1), Integer(2), Integer(3))
assert parse('1 =!= 2 =!= 3') == Expression('UnsameQ', Integer(1), Integer(2), Integer(3))

assert parse('!1') == Expression('Not', Integer(1))
assert parse('1 && 2') == Expression('And', Integer(1), Integer(2))
assert parse('1 \\[And] 2') == Expression('And', Integer(1), Integer(2))
assert parse(u'1 \u2227 2') == Expression('And', Integer(1), Integer(2))

assert parse('1 || 2') == Expression('Or', Integer(1), Integer(2))
assert parse('1 \\[Or] 2') == Expression('Or', Integer(1), Integer(2))
assert parse(u'1 \u2228 2') == Expression('Or', Integer(1), Integer(2))

assert parse('1..') == Expression('Repeated', Integer(1))
assert parse('1...') == Expression('RepeatedNull', Integer(1))

assert parse('1 | 2') == Expression('Alternatives', Integer(1), Integer(2))
assert parse('1 | 2 | 3') == Expression('Alternatives', Integer(1), Integer(2), Integer(3))

assert parse('x:expr') == Expression('Pattern', Symbol('x'), Symbol('expr'))
assert parse('x_:expr') == Expression('Optional', Expression('Pattern', Symbol('x'), Expression('Blank')), Symbol('expr'))

assert parse('x ~~ y') == Expression('StringExpression', Symbol('x'), Symbol('y'))
assert parse('x ~~ y ~~ z') == Expression('StringExpression', Symbol('x'), Symbol('y'), Symbol('z'))

assert parse('x /; y') == Expression('Condition', Symbol('x'), Symbol('y'))
assert parse('x -> y') == Expression('Rule', Symbol('x'), Symbol('y'))
assert parse('x :> y') == Expression('RuleDelayed', Symbol('x'), Symbol('y'))

assert parse('x /. y') == Expression('ReplaceAll', Symbol('x'), Symbol('y'))
assert parse('x //. y') == Expression('ReplaceRepeated', Symbol('x'), Symbol('y'))

assert parse('x += y') == Expression('AddTo', Symbol('x'), Symbol('y'))
assert parse('x -= y') == Expression('SubtractFrom', Symbol('x'), Symbol('y'))
assert parse('x *= y') == Expression('TimesBy', Symbol('x'), Symbol('y'))
assert parse('x /= y') == Expression('DivideBy', Symbol('x'), Symbol('y'))

assert parse('x &') == Expression('Function', Symbol('x'))

assert parse('a \\[Colon] b') == Expression('Colon', Symbol('a'), Symbol('b'))
assert parse(u'a \u2236 b') == Expression('Colon', Symbol('a'), Symbol('b'))

assert parse('x // y') == Expression('y', Symbol('x'))

assert parse('x = y') == Expression('Set', Symbol('x'), Symbol('y'))
assert parse('x := y') == Expression('SetDelayed', Symbol('x'), Symbol('y'))
assert parse('x ^= y') == Expression('UpSet', Symbol('x'), Symbol('y'))
assert parse('x ^:= y') == Expression('UpSetDelayed', Symbol('x'), Symbol('y'))
assert parse('x =.') == Expression('Unset', Symbol('x'))

assert parse('x/:1=1') == Expression('TagSet', Symbol('x'), Integer(1), Integer(1))
assert parse('x/:1:=1') == Expression('TagSetDelayed', Symbol('x'), Integer(1), Integer(1))
assert parse('x/:1=.') == Expression('TagUnset', Symbol('x'), Integer(1))

assert parse('x \\[Function] y') == parse('Function[{x}, y]')
assert parse(u'x \uf4a1 y') == parse('Function[{x}, y]')

assert parse('1 \\` 2') == Expression('FormBox', Integer(2), Integer(1))

#FIXME
#assert parse('1 ; 5') == Expression('CompoundExpression', Integer(1), Integer(5))
assert parse('1 ;') == Expression('CompoundExpression', Integer(1), Symbol('Null'))

assert parse('1 ^ 2') == Expression('Power', Integer(1), Integer(2))
assert parse('{x, y}') == Expression('List', Symbol('x'), Symbol('y'))

assert parse('{}') == Expression('List')
assert parse('{a,}') == Expression('List', Symbol('a'), Symbol('Null'))
assert parse('{,a}') == Expression('List', Symbol('Null'), Symbol('a'))
assert parse('{,}') == Expression('List', Symbol('Null'), Symbol('Null'))

assert parse('{a, b,}') == Expression('List', Symbol('a'), Symbol('b'), Symbol('Null'))
assert parse('{, a, b}') == Expression('List', Symbol('Null'), Symbol('a'), Symbol('b'))
assert parse('{,a,b,}') == Expression('List', Symbol('Null'), Symbol('a'), Symbol('b'), Symbol('Null'))

assert parse('Sin[x, y]') == Expression('Sin', Symbol('x'), Symbol('y'))
assert parse('a[[1]]') == Expression('Part', Symbol('a'), Integer(1))

assert parse('f_') == Expression('Pattern', Symbol('f'), Expression('Blank'))
assert parse('f__') == Expression('Pattern', Symbol('f'), Expression('BlankSequence'))
assert parse('f___') == Expression('Pattern', Symbol('f'), Expression('BlankNullSequence'))

assert parse('_') == parse('Blank[]')
assert parse('_expr') == parse('Blank[expr]')
assert parse('__') == parse('BlankSequence[]')
assert parse('__expr') == parse('BlankSequence[expr]')
assert parse('___') == parse('BlankNullSequence[]')
assert parse('___expr') == parse('BlankNullSequence[expr]')

assert parse('_.') == parse('Optional[Blank[]]')
assert parse('symb_') == parse('Pattern[symb, Blank[]]')
assert parse('symb_expr') == parse('Pattern[symb, Blank[expr]]')
assert parse('symb__') == parse('Pattern[symb, BlankSequence[]]')
assert parse('symb__expr') == parse('Pattern[symb, BlankSequence[expr]]')
assert parse('symb___') == parse('Pattern[symb, BlankNullSequence[]]')
assert parse('symb___expr') == parse('Pattern[symb, BlankNullSequence[expr]]')
assert parse('symb_.') == parse('Optional[Pattern[symb, Blank[]]]')

assert parse('#2') == Expression('Slot', Integer(2))
assert parse('#') == Expression('Slot', Integer(1))
assert parse('##2') == Expression('SlotSequence', Integer(2))
assert parse('##') == Expression('SlotSequence', Integer(1))
assert parse('%2') == Expression('Out', Integer(2))
assert parse('%') == Expression('Out')
assert parse('%%') == Expression('Out', Integer(-2))
assert parse('%%%%') == Expression('Out', Integer(-4))

assert parse('x ! y') == Expression('Times', Expression('Factorial', Symbol('x')), Symbol('y'))
assert parse('x ^ 2 y') == Expression('Times', Expression('Power', Symbol('x'), Integer(2)), Symbol('y'))

assert parse('Infinity::indet') == parse('MessageName[Infinity, "indet"]')
assert parse('Infinity::indet::"fasd"') == parse('MessageName[Infinity, "indet", "fasd"]')

assert parse('RepeatedNull[item_]') == parse('RepeatedNull[Pattern[item, Blank[]]]')

assert parse('"System`"') == String('System`')

# import time
# instr = '+'.join(map(str, range(10000)))
# #instr = '{' + ', '.join(map(str, range(10000))) + '}'
# stime = time.time()
# a = parse(instr)
# print time.time() - stime

#quit()
