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

import ply.lex as lex
import ply.yacc as yacc

import re
import unicodedata

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

prefix_operators = {
    'Del' : 'Del',
    'Square': 'Square',
    'ForAll': 'ForAll',
    'Exists': 'Exists',
    'NotExists': 'NotExists',
}

infix_operators = {
    'PatternTest' : 'PatternTest',
    'Apply': 'Apply1',
    'Map': 'Map',
    'MapAll': 'MapAll',
    'PlusMinus': 'PlusMinus',
    'MinusPlus': 'MinusPlus',
    'RightTee' : 'RightTee',
    'DoubleRightTee' : 'DoubleRightTee',
    'LeftTee' : 'LeftTee',
    'DoubleLeftTee' : 'DoubleLeftTee',
    'Implies' : 'Implies',
    'SuchThat' : 'SuchThat',
    'Condition' : 'Condition',
    'Rule' : ['op_Rule', 'Rule'],
    'RuleDelayed' : ['op_RuleDelayed', 'RuleDelayed'],
    'ReplaceAll' : 'ReplaceAll',
    'ReplaceRepeated' : 'ReplaceRepeated',
    'AddTo' : 'AddTo',
    'SubtractFrom' : 'SubtractFrom',
    'TimesBy' : 'TimesBy',
    'DivideBy' : 'DivideBy',
    'Therefore': 'Therefore',
    'Because': 'Because',
    'UpSet' : 'UpSet',
    'UpSetDelayed' : 'UpSetDelayed',
}

flat_infix_operators = {
    'StringJoin': 'StringJoin',
    'SmallCircle' : 'SmallCircle',
    'CircleDot' : 'CircleDot',
    'NonCommutativeMultiply' : 'NonCommutativeMultiply',
    'Cross' : 'Cross',
    'Dot' : 'RawDot',
    'Plus' : 'Plus',
    'Intersection' : 'Intersection',
    'Union' : 'Union',
    'Diamond' : 'Diamond',
    'Wedge' : 'Wedge',
    'Vee' : 'Vee',
    'CircleTimes' : 'CircleTimes',
    'CirclePlus' : 'CirclePlus',
    'CircleMinus' : 'CircleMinus',
    'CenterDot' : 'CenterDot',
    'VerticalTilde' : 'VerticalTilde',
    'Coproduct' : 'Coproduct',
    'Cap' : 'Cap',
    'Cup' : 'Cup',
    'Star' : 'Star',
    'Backslash' : 'RawBackslash',
    'Equal' : ['op_Equal', 'LongEqual', 'Equal'],
    'Unequal' : ['op_Unequal', 'NotEqual'],
    'Greater' : 'Greater',
    'Less' : 'Less',
    'GreaterEqual' : ['op_GreaterEqual', 'GreaterEqual', 'GreaterSlantEqual'],
    'LessEqual' : ['op_LessEqual', 'LessEqual', 'LessSlantEqual'],
    'VerticalBar' : 'VerticalBar',
    'NotVerticalBar' : 'NotVerticalBar',
    'DoubleVerticalBar' : 'DoubleVerticalBar',
    'NotDoubleVerticalBar' : 'NotDoubleVerticalBar',
    'SameQ' : 'SameQ',
    'UnsameQ' : 'UnsameQ',
    'Element' : 'Element',
    'NotElement' : 'NotElement',
    'Subset' : 'Subset',
    'Superset' : 'Superset',
    'And' : ['And', 'op_And'],
    'Nand' : 'Nand',
    'Xor' : 'Xor',
    'Xnor' : 'Xnor',
    'Or' : ['op_Or', 'Or'],
    'Nor' : 'Nor',
    'Equivalent' : 'Equivalent',
    'Alternatives' : 'Alternatives',
    'StringExpression' : 'StringExpression',
    'Colon' : 'Colon',
    'VerticalSeparator' : 'VerticalSeparator',
}

postfix_operators = {
    'Increment': 'Increment',
    'Decrement': 'Decrement',
    'Factorial': 'Factorial',
    'Factorial2': 'Factorial2',
    'Conjugate': 'Conjugate',
    'Transpose': 'Transpose',
    'ConjugateTranspose': 'ConjugateTranspose',
    'Repeated' : 'Repeated',
    'RepeatedNull' : 'RepeatedNull',
    'Function' : 'RawAmpersand',
}

precedence = (
    ('right', 'FormBox'),
    ('right', 'Semicolon'),
    ('nonassoc', 'Put', 'PutAppend'),
    ('nonassoc', 'Set', 'SetDelayed', 'Function', 'UpSet', 'UpSetDelayed'),
    ('left', 'Because'),
    ('right', 'Therefore'),
    ('left', 'VerticalSeparator'),         # flat
    ('right', 'Postfix'),
    ('right', 'Colon'),
    ('nonassoc', 'RawAmpersand'),
    ('right', 'AddTo', 'SubtractFrom', 'TimesBy', 'DivideBy'),
    ('left', 'ReplaceAll', 'ReplaceRepeated'),
    ('right', 'Rule', 'RuleDelayed', 'op_RuleDelayed'),
    ('left', 'Condition'),
    ('left', 'StringExpression'),
    ('nonassoc', 'RawColon'),
    ('left', 'Alternatives'),
    ('nonassoc', 'Repeated', 'RepeatedNull'),
    ('right', 'SuchThat'),
    ('left', 'LeftTee', 'DoubleLeftTee'),
    ('right', 'RightTee', 'DoubleRightTee'),
    ('right', 'Implies'),
    ('left', 'Equivalent'),
    ('left', 'Or', 'op_Or', 'Nor'),
    ('left', 'Xor', 'Xnor'),
    ('left', 'And', 'op_And', 'Nand'),
    ('right', 'Not'),
    ('right', 'ForAll', 'Exists', 'NotExists'),
    ('left', 'Element', 'NotElement', 'Subset', 'Superset'),
    ('left', 'SameQ', 'UnsameQ'),
    ('left', 'Equal', 'op_Equal', 'LongEqual', 'op_Unequal', 'NotEqual', 'Greater', 'Less', 'GreaterEqual', 'op_GreaterEqual', 'GreaterSlantEqual', 'LessEqual', 'op_LessEqual', 'LessSlantEqual', 'VerticalBar', 'NotVerticalBar', 'DoubleVerticalBar', 'NotDoubleVerticalBar'),
    ('left', 'Span'),
    ('left', 'Union'),                      # flat
    ('left', 'Intersection'),               # flat
    ('left', 'Plus', 'Minus', 'PlusMinus', 'MinusPlus'),  # flat
    #('left', 'Sum'),                       # flat
    ('left', 'CirclePlus', 'CircleMinus'),  # flat
    ('left', 'Cap', 'Cup'),                 # flat
    ('left', 'Coproduct'),                  # flat
    ('left', 'VerticalTilde'),              # flat
    #('left', 'Product'),
    ('left', 'Star'),                       # flat
    ('left', 'Times', 'RawStar'),           # flat
    ('left', 'CenterDot'),                  # flat
    ('left', 'CircleTimes'),                # flat
    ('left', 'Vee'),                        # flat
    ('left', 'Wedge'),                      # flat
    ('left', 'Diamond'),                    # flat
    ('right', 'RawBackslash'),
    ('left',  'RawSlash', 'Divide'),
    ('nonassoc', 'UPlus', 'UMinus', 'UPlusMinus', 'UMinusPlus'),
    ('left', 'RawDot'),                    # flat
    ('left', 'Cross'),                     # flat
    ('left', 'NonCommutativeMultiply'),    # flat
    ('right', 'CircleDot'),
    ('left', 'SmallCircle'),
    ('right', 'Square'),
    ('right', 'Del'),
    ('right', 'Integral'),
    ('right', 'Sqrt'),
    ('right', 'Power', 'Power2'),
    ('left', 'StringJoin'),                 # flat
    ('left', 'Derivative'),
    ('nonassoc', 'Conjugate'),
    ('left', 'Factorial'),
    ('right', 'Apply1', 'Apply2', 'Map', 'MapAll'),
    ('left', 'Infix'),
    ('right', 'Prefix'),
    ('nonassoc', 'PreIncrement', 'PreDecrement'),
    ('nonassoc', 'Increment', 'Decrement'),
    ('left', 'PART'),
    ('nonassoc', 'PatternTest'),
    ('right', 'Subscript'),
    ('right', 'Overscript', 'Underscript'),
    ('nonassoc', 'Get'),
    ('nonassoc', 'blanks', 'blankdefault'),
    ('nonassoc', 'out'),
    ('nonassoc', 'slot', 'slotseq'),
    ('nonassoc', 'MessageName'),
    ('nonassoc', 'string'),
    ('nonassoc', 'symbol'),
    ('nonassoc', 'int', 'float'),
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
    'Span',
    'RawLeftBracket',
    'RawRightBracket',
    'RawLeftBrace',
    'RawRightBrace',
    'RawLeftParenthesis',
    'RawRightParenthesis',
    'RawComma',
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
    'Factorial',
    'Factorial2',
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
    #'PartialD',
    'Del',
    'Square',
    'CircleDot',
    'SmallCircle',
    'NonCommutativeMultiply',
    'Cross',
    'RawDot',
    'Plus',
    'Minus',
    'PlusMinus',
    'MinusPlus',
    'RawSlash',
    'RawBackslash',
    'Diamond',
    'Wedge',
    'Vee',
    'CircleTimes',
    'CenterDot',
    'Star',
    #'Sum',
    #'Product',
    'RawStar',
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
    #'DiscreteShift',
    #'DiscreteRatio',
    #'DifferenceDelta',
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

class MathicsScanner:
    tokens = tokens
    precedence = precedence

    #t_ignore = ur' [\s \u2062]+ '
    t_ignore = ' \t\n '

    t_symbol = r' [a-zA-Z$][a-zA-Z0-9$]* '
    t_int = r' \d+ '

    t_RawLeftBracket = r' \[ '
    t_RawRightBracket = r' \] '
    t_RawLeftBrace = r' \{ '
    t_RawRightBrace = r' \} '
    t_RawLeftParenthesis = r' \( '
    t_RawRightParenthesis = r' \) '

    t_RawComma = r' \, '

    t_Span = r' \;\; '

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

    t_Factorial = r' \! '
    t_Factorial2 = r' \!\! '

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
    #t_PartialD = ur' \\\[PartialD\]|\u2202 '
    t_Del = ur' \\\[Del\]|\u2207 '
    
    t_Square = ur' \\\[Square\]|\uf520 '
    t_SmallCircle = ur' \\\[SmallCircle\]|\u2218 '
    t_CircleDot = ur' \\\[CircleDot\]|\u2299 '

    t_NonCommutativeMultiply = r' \*\* '

    t_Cross = ur' \\\[Cross\]|\uf4a0 '
    t_RawDot = r' \. '

    t_Plus = r' \+ '
    t_Minus = r' \- '
    t_RawSlash = r' \/ '
    t_RawBackslash = r' \\ '

    t_Diamond = ur' \\\[Diamond\]|\u22c4 '
    t_Wedge = ur' \\\[Wedge\]|\u22c0 '
    t_Vee = ur' \\\[Vee\]|\u22c1 '
    t_CircleTimes = ur' \\\[CircleTimes\]|\u2297 '
    t_CenterDot = ur' \\\[CenterDot\]|\u00b7 '
    t_Star = ur' \\\[Star\]|\u22c6'

    #t_Sum = ur' \\\[Sum\]|\u2211 '
    #t_Product = ur' \\\[Product\]|\u220f '

    t_RawStar = r' \* '
    t_Times = ur'\\\[Times\]|\u00d7 '
    t_Divide = ur' \\\[Divide\]|\u00f7 '

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

    #t_DiscreteShift = ur' \\\[DiscreteShift\]|\uf4a3 '
    #t_DiscreteRatio = ur' \\\[DiscreteRatio\]|\uf4a4 '
    #t_DifferenceDelta = ur' \\\[DifferenceDelta\]|\u2206 '
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

# Decorator hack to convince ply that a parsing rule only accepts one argument
def ONEARG(f):      
    def wrapped(args):
        return f(args)
    return wrapped

class MathicsParser:
    tokens = tokens
    precedence = precedence
    start = 'Expression'

    def __init__(self):
        for prefix_op in prefix_operators:
            @ONEARG
            def tmp(args, op=prefix_op):
                args[0] = Expression(op, args[2])
            tokens = prefix_operators[prefix_op]
            if not isinstance(tokens, list):
                tokens = [tokens]
            tmp.__doc__ = 'expr : ' + '\n     | '.join(['{0} expr'.format(token) for token in tokens])
            setattr(self, 'p_{0}_prefix'.format(prefix_op), tmp)

        for infix_op in infix_operators:
            tokens = infix_operators[infix_op]
            if not isinstance(tokens, list):
                tokens = [tokens]
            @ONEARG
            def tmp(args, op=infix_op):
                args[0] = Expression(op, args[1], args[3])
            tmp.__doc__ = 'expr : ' + '\n     | '.join(['expr {0} expr'.format(token) for token in tokens])
            setattr(self, 'p_{0}_infix'.format(infix_op), tmp)

        for flat_infix_op in flat_infix_operators:
            tokens = flat_infix_operators[flat_infix_op]
            if not isinstance(tokens, list):
                tokens = [tokens]
            @ONEARG
            def tmp(args, op=flat_infix_op):
                if args[1].get_head_name() == op:
                    args[1].leaves.append(args[3])
                    args[0] = args[1]
                else:
                    args[0] = Expression(op, args[1], args[3])
            tmp.__doc__ = 'expr : ' + '\n     | '.join(['expr {0} expr'.format(token) for token in tokens])
            setattr(self, 'p_{0}_infix'.format(flat_infix_op), tmp)

        for postfix_op in postfix_operators:
            @ONEARG
            def tmp(args, op=postfix_op):
                args[0] = Expression(op, args[1])
            tokens = postfix_operators[postfix_op]
            if not isinstance(tokens, list):
                tokens = [tokens]
            tmp.__doc__ = 'expr : ' + '\n     | '.join(['expr {0}'.format(token) for token in tokens])
            setattr(self, 'p_{0}_postfix'.format(postfix_op), tmp)

    def build(self, **kwargs):
        self.parser = yacc.yacc(debug=1, module=self, **kwargs)

    def p_error(self, p):
        if p is not None:
            print p
        raise ParseError(p)
    
    def parse(self, string):
        result = self.parser.parse(string)
        if result is not None:
            result = result.post_parse()
        return result

    def p_Expression(self, args):
        'Expression : expr'
        args[0] = args[1]

    def p_Empty(self, args):
        'Expression :'
        args[0] = None
        
    def p_parenthesis(self, args):
        'expr : RawLeftParenthesis expr RawRightParenthesis'
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
        'expr : RawLeftBrace sequence RawRightBrace'
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
        '''sequence1 : sequence1 RawComma expr
                     | sequence1 RawComma
                     | RawComma sequence1
                     | expr
                     | RawComma '''
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
        'expr : symbol'
        args[0] = Symbol(args[1])
        
    def p_int(self, args):
        'expr : int'
        args[0] = Integer(args[1])
        
    def p_float(self, args):
        'expr : float'
        args[0] = Real(args[1])
        
    def p_blanks(self, args):
        'pattern : blanks'
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
        'pattern : blankdefault'
        name = args[1][:-2]
        if name:
            args[0] = Expression('Optional', Expression('Pattern', Symbol(name), Expression('Blank')))
        else:
            args[0] = Expression('Optional', Expression('Blank'))

    def p_pattern(self, args):
        'expr : pattern'
        args[0] = args[1]
        
    def p_slot(self, args):
        'expr : slot'
        args[0] = Expression('Slot', Integer(args[1]))

    def p_slotseq(self, args):
        'expr : slotseq'
        args[0] = Expression('SlotSequence', Integer(args[1]))
    
    def p_out(self, args):
        'expr : out'
        if args[1] == -1:
            args[0] = Expression('Out')
        else:
            args[0] = Expression('Out', Integer(args[1]))
        
    def p_string(self, args):
        'expr : string'
        args[0] = String(args[1])

    def p_filename_string(self, args):
        '''filename : string
                    | symbol'''
        args[0] = String(args[1])

    def p_Get(self, args):
        'expr : Get filename'
        args[0] = Expression('Get', args[2])

    def p_MessageName(self, args):
        '''expr : expr MessageName string MessageName string
                | expr MessageName symbol MessageName string
                | expr MessageName symbol
                | expr MessageName string'''

        if len(args) == 4:
            args[0] = Expression('MessageName', args[1], String(args[3]))
        elif len(args) == 6:
            args[0] = Expression('MessageName', args[1], String(args[3]), String(args[5]))

    def p_OverScript(self, args):
        '''expr : expr Underscript expr Otherscript expr %prec Underscript
                | expr Overscript expr Otherscript expr %prec Overscript
                | expr Overscript expr
                | expr Underscript expr'''
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
        '''expr : expr Subscript expr Otherscript expr %prec Subscript
                | expr Subscript expr'''
        if len(args) == 4:
            args[0] = Expression('Subscript', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('Power', Expression('Subscript', args[1], args[3]), args[5])

    def p_PreIncrement(self, args):
        'expr : Increment expr %prec PreIncrement'
        args[0] = Expression('PreIncrement', args[2])

    def p_PreDecrement(self, args):
        'expr : Decrement expr %prec PreDecrement'
        args[0] = Expression('PreDecrement', args[2])

    def p_Prefix(self, args):
        'expr : expr Prefix expr'
        args[0] = Expression(args[1], args[3])

    def p_Infix(self, args):
        'expr : expr Infix expr Infix expr'
        args[0] = Expression(args[3], args[1], args[5])

    def p_Apply2(self, args):
        'expr : expr Apply2 expr'
        assert args[2] == '@@@'
        args[0] = Expression('Apply', args[1], args[3], Expression('List', Integer(1)))

    def p_Derivative(self, args):
        'expr : expr Derivative'
        args[0] = Expression(Expression('Derivative', Integer(len(args[2]))), args[1])

    def p_Power(self, args):
        '''expr : expr Power2 expr Otherscript expr %prec Power2
                | expr Power expr'''
        if args[2] == '^':
            args[0] = Expression('Power', args[1], args[3])
        elif args[2] == '\\^':
            args[0] = Expression('Power', Expression('Subscript', args[1], args[5]), args[3])

    def p_Sqrt(self, args):
        '''expr : Sqrt expr Otherscript expr %prec Sqrt
                | Sqrt expr'''
        if len(args) == 3:
            args[0] = Expression('Sqrt', args[2])
        elif len(args) == 5:
            args[0] = Expression('Power', args[2], Expression('Times', Integer(1), Expression('Power', args[4], Integer(-1))))

    def p_Integrate(self, args):
        'expr : Integral expr DifferentialD expr %prec Integral'
        args[0] = Expression('Integrate', args[2], args[4])

    def p_Minus(self, args):
        'expr : expr Minus expr'
        args[0] = Expression('Plus', args[1], Expression('Times', Integer(-1), args[3]))

    def p_UPlus(self, args):
        'expr : Plus expr %prec UPlus'
        args[0] = args[2]

    def p_UMinus(self, args):
        'expr : Minus expr %prec UMinus'''
        args[0] = Expression('Times', Integer(-1), args[2])

    def p_UPlusMinus(self, args):
        'expr : PlusMinus expr %prec UPlusMinus'''
        args[0] = Expression('PlusMinus', args[2])

    def p_UMinusPlus(self, args):
        'expr : MinusPlus expr %prec UMinusPlus'''
        args[0] = Expression('MinusPlus', args[2])

    def p_Slash(self, args):
        '''expr : expr RawSlash expr
                | expr Divide expr'''
        args[0] = Expression('Times', args[1], Expression('Power', args[3], Integer(-1)))

    def p_Times(self, args):
        '''expr : expr Times expr
                | expr RawStar expr
                | expr expr %prec Times'''
        if len(args) == 3:
            if args[2].get_head_name() == 'Times':
                args[2].leaves.insert(0, args[1])
                args[0] = args[2]
            else:
                args[0] = Expression('Times', args[1], args[2])
        elif len(args) == 4:
            if args[1].get_head_name() == 'Times':
                args[1].leaves.append(args[3])
                args[0] = args[1]
            else:
                args[0] = Expression('Times', args[1], args[3])

    def p_Span(self, args):
        '''expr : expr Span expr Span expr
                | expr Span      Span expr
                |      Span expr Span expr
                |      Span      Span expr
                | expr Span expr
                | expr Span
                |      Span expr
                |      Span'''

        if len(args) == 6:
            args[0] = Expression('Span', args[1], args[3], args[5])
        elif len(args) == 5:
            if isinstance(args[1], BaseExpression):
                args[0] = Expression('Span', args[1], Symbol('All'), args[4])
            elif isinstance(args[2], BaseExpression):
                args[0] = Expression('Span', Integer(1), args[2], args[4])
        elif len(args) == 4:
            args[0] = Expression('Span', args[1], args[3])
        elif len(args) == 3:
            if isinstance(args[1], BaseExpression):
                args[0] = Expression('Span', args[1], Symbol('All'))
            elif isinstance(args[2], BaseExpression):
                args[0] = Expression('Span', Integer(1), args[2])
        elif len(args) == 2:
                args[0] = Expression('Span', Integer(1), Symbol('All'))

    def p_Not(self, args):
        '''expr : Not expr
                | Factorial expr %prec Not'''
        args[0] = Expression('Not', args[2])

    def p_Pattern(self, args):
        'expr : symbol RawColon expr'
        args[0] = Expression('Pattern', Symbol(args[1]), args[3])

    def p_Optional(self, args):
        'expr : pattern RawColon expr'
        args[0] = Expression('Optional', args[1], args[3])

    def p_Postfix(self, args):
        'expr : expr Postfix expr'
        args[0] = Expression(args[3], args[1])

    def p_Set(self, args):
        '''expr : expr TagSet expr Set expr
                | expr Set expr'''
        if len(args) == 4:
            args[0] = Expression('Set', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('TagSet', args[1], args[3], args[5])

    def p_SetDelayed(self, args):
        '''expr : expr TagSet expr SetDelayed expr
                | expr SetDelayed expr'''
        if len(args) == 4:
            args[0] = Expression('SetDelayed', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('TagSetDelayed', args[1], args[3], args[5])

    def p_Unset(self, args):
        '''expr : expr TagSet expr Unset
                | expr Unset'''
        if len(args) == 3:
            args[0] = Expression('Unset', args[1])
        elif len(args) == 4:
            args[0] = Expression('TagUnset', args[1], args[3])

    def p_Function(self, args):
        'expr : expr Function expr'
        args[0] = Expression('Function', Expression('List', args[1]), args[3])

    def p_Put(self, args):
        'expr : expr Put filename'
        args[0] = Expression('Put', args[1], args[3])

    def p_PutAppend(self, args):
        'expr : expr PutAppend filename'
        args[0] = Expression('PutAppend', args[1], args[3])

    def p_Compound(self, args):
        '''CompoundToken : expr Semicolon expr
                         | CompoundToken Semicolon expr
                         | expr Semicolon
                         | CompoundToken Semicolon
                    expr : CompoundToken'''
        if len(args) == 2:
            args[0] = Expression('CompoundExpression', *args[1])
        if len(args) == 3:
            if isinstance(args[1], list):
                args[1].append(Symbol('Null'))
                args[0] = args[1]
            else:
                args[0] = [args[1], Symbol('Null')]
        if len(args) == 4:
            if isinstance(args[1], list):
                args[1].append(args[3])
                args[0] = args[1]
            else:
                args[0] = [args[1], args[3]]

    def p_FormBox(self, args):
        'expr : expr FormBox expr'
        args[0] = Expression('FormBox', args[3], args[1])

scanner = MathicsScanner()
scanner.build()
parser = MathicsParser()
parser.build()

def parse(string):
    return parser.parse(string)
