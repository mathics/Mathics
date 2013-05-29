# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011-2013 The Mathics Team

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
from math import log10

from mathics.core.expression import (BaseExpression, Expression, Integer, 
    Real, Symbol, String, Rational)
from mathics.core.numbers import dps
from mathics.core.characters import letters, letterlikes, named_characters

from mathics.builtin.numeric import machine_precision

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

# Symbols can be any letters
base_symb = ur'((?![0-9])([0-9${0}{1}])+)'.format(letters, letterlikes)

symbol_re = re.compile(ur'`?{0}(`{0})*'.format(base_symb))

def is_symbol_name(text):
    return symbol_re.sub('', text) == ''

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
    'Power' : 'Power',
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
    'ConjugateTranspose': ['ConjugateTranspose', 'HermitianConjugate'],
    'Repeated' : 'Repeated',
    'RepeatedNull' : 'RepeatedNull',
    'Function' : 'RawAmpersand',
}

innequality_operators = {
    'Equal' : ['op_Equal', 'LongEqual', 'Equal'],
    'Unequal' : ['op_Unequal', 'NotEqual'],
    'Greater' : 'Greater',
    'Less' : 'Less',
    'GreaterEqual' : ['op_GreaterEqual', 'GreaterEqual', 'GreaterSlantEqual'],
    'LessEqual' : ['op_LessEqual', 'LessEqual', 'LessSlantEqual'],
}

precedence = (
    ('right', 'FormBox'),
    ('left', 'Semicolon'),                  # flat - custom
    ('left', 'Put', 'PutAppend'),
    ('right', 'Set', 'SetDelayed', 'Function', 'UpSet', 'UpSetDelayed', 'TagSet', 'Unset'),
    ('left', 'Because'),
    ('right', 'Therefore'),
    ('left', 'VerticalSeparator'),          # flat
    ('left', 'Postfix'),
    ('right', 'Colon'),                     # flat
    ('left', 'RawAmpersand'),
    ('right', 'AddTo', 'SubtractFrom', 'TimesBy', 'DivideBy'),
    ('left', 'ReplaceAll', 'ReplaceRepeated'),
    ('right', 'Rule', 'op_Rule', 'RuleDelayed', 'op_RuleDelayed'),
    ('left', 'Condition'),
    ('left', 'StringExpression'),           # flat
    ('right', 'RawColon'),
    ('left', 'Alternatives'),               # flat
    ('nonassoc', 'Repeated', 'RepeatedNull'),  
    ('right', 'SuchThat'),
    ('left', 'LeftTee', 'DoubleLeftTee'),
    ('right', 'RightTee', 'DoubleRightTee'),
    ('right', 'Implies'),
    ('left', 'Equivalent'),                 # flat
    ('left', 'Or', 'op_Or', 'Nor'),         # flat
    ('left', 'Xor', 'Xnor'),                # flat
    ('left', 'And', 'op_And', 'Nand'),      # flat
    ('right', 'Not'),
    ('right', 'ForAll', 'Exists', 'NotExists'),
    ('left', 'Element', 'NotElement', 'Subset', 'Superset'),    # flat
    ('left', 'SameQ', 'UnsameQ'),           # flat
    ('left', 'Equal', 'op_Equal', 'LongEqual', 'op_Unequal', 'NotEqual', 'Greater', 'Less', 'GreaterEqual', 'op_GreaterEqual', 'GreaterSlantEqual', 'LessEqual', 'op_LessEqual', 'LessSlantEqual', 'VerticalBar', 'NotVerticalBar', 'DoubleVerticalBar', 'NotDoubleVerticalBar'),
    ('nonassoc', 'Span'),
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
    #This is a hack to get implicit times working properly:
    ('left', 'Times', 'RawStar', 'blanks', 'blankdefault', 'out', 'slot', 'slotseq', 'string', 'symbol', 'number', 'RawLeftBrace', 'RawLeftParenthesis'), # flat, 
    ('left', 'CenterDot'),                  # flat
    ('left', 'CircleTimes'),                # flat
    ('left', 'Vee'),                        # flat
    ('left', 'Wedge'),                      # flat
    ('left', 'Diamond'),                    # flat
    ('nonassoc', 'RawBackslash'),
    ('left',  'RawSlash', 'Divide', 'Fraction'),
    ('right', 'UPlus', 'UMinus', 'UPlusMinus', 'UMinusPlus'),
    ('left', 'RawDot'),                     # flat
    ('left', 'Cross'),                      # flat
    ('left', 'NonCommutativeMultiply'),     # flat
    ('right', 'CircleDot'),
    ('left', 'SmallCircle'),                # flat
    ('right', 'Square'),
    ('right', 'Del'),
    ('right', 'Integral', 'DifferentialD'),
    ('right', 'Sqrt'),
    ('right', 'Power', 'Superscript'),
    ('left', 'StringJoin'),                 # flat
    ('left', 'Derivative'),
    ('left', 'Conjugate', 'Transpose', 'ConjugateTranspose'),
    ('left', 'Factorial', 'Factorial2'),
    ('right', 'Apply1', 'Apply2', 'Map', 'MapAll'),
    ('left', 'Infix'),
    ('right', 'Prefix'),
    ('right', 'PreIncrement', 'PreDecrement'),
    ('left', 'Increment', 'Decrement'),
    ('left', 'PART', 'RawLeftBracket', 'RawRightBracket'),
    ('nonassoc', 'PatternTest'),
    ('nonassoc', 'InterpretedBox'),
    ('right', 'Subscript'),
    ('right', 'Overscript', 'Underscript'),
    ('nonassoc', 'Get'),
    #('nonassoc', 'blanks', 'blankdefault'),
    #('nonassoc', 'out'),
    #('nonassoc', 'slot', 'slotseq'),
    ('nonassoc', 'MessageName'),
    #('nonassoc', 'string'),
    #('nonassoc', 'symbol'),
    #('nonassoc', 'number'),
)

tokens = (
    'symbol',
    'number',
    'string',
    'blanks', 
    'blankdefault',
    'out',
    'slot',
    'slotseq',
    'filename',
    'Span',
    'RawLeftBracket',
    'RawRightBracket',
    'RawLeftBrace',
    'RawRightBrace',
    'RawLeftParenthesis',
    'RawRightParenthesis',
    'LeftBoxParenthesis',
    'RightBoxParenthesis',
    'LeftBoxParenthesisInternal',
    'RightBoxParenthesisInternal',
    'RawComma',
    'Get',
    'Put',
    'PutAppend',
    'MessageName',
    'Superscript',
    'Subscript',
    'Overscript',
    'Underscript',
    'Otherscript',
    'Fraction',
    'Sqrt',
    'FormBox',
    'InterpretedBox',
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
    'HermitianConjugate',
    'Derivative',
    'StringJoin',
    'Power',
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

    states = (
        ('file', 'exclusive'),
        ('boxes', 'inclusive'),
    )

    t_ANY_ignore = ' \t\n '

    t_RawLeftBracket = r' \[ '
    t_RawRightBracket = r' \] '
    t_RawLeftBrace = r' \{ '
    t_RawRightBrace = r' \} '
    t_RawLeftParenthesis = r' \( '
    t_RawRightParenthesis = r' \) '

    t_RawComma = r' \, '

    t_Span = r' \;\; '

    t_MessageName = r' \:\: '

    # Box Constructors
    t_InterpretedBox = r' \\\! '
    t_boxes_Superscript = r' \\\^ '
    t_boxes_Subscript = r' \\\_ '
    t_boxes_Overscript = r' \\\& '
    t_boxes_Underscript = r' \\\+ '
    t_boxes_Otherscript = r' \\\% '
    t_boxes_Fraction = r' \\\/ '
    t_boxes_Sqrt = r' \\\@ '
    t_boxes_FormBox = r' \\\` '

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

    t_Transpose = ur' \uf3c7 '
    t_Conjugate = ur' \uf3c8 '
    t_ConjugateTranspose = ur' \uf3c9 '
    t_HermitianConjugate = ur' \uf3ce '

    t_Derivative = r' \'+ '
    t_StringJoin = r' \<\> '

    t_Power = r' \^ '

    t_Integral = ur' \u222b '
    t_DifferentialD = ur' \uf74c '
    #t_PartialD = ur' \u2202 '
    t_Del = ur' \u2207 '
    
    t_Square = ur' \uf520 '
    t_SmallCircle = ur' \u2218 '
    t_CircleDot = ur' \u2299 '

    t_NonCommutativeMultiply = r' \*\* '

    t_Cross = ur' \uf4a0 '
    t_RawDot = r' \. '

    t_Plus = r' \+ '
    t_Minus = r' \- '
    t_RawSlash = r' \/ '
    t_RawBackslash = r' \\ '

    t_Diamond = ur' \u22c4 '
    t_Wedge = ur' \u22c0 '
    t_Vee = ur' \u22c1 '
    t_CircleTimes = ur' \u2297 '
    t_CenterDot = ur' \u00b7 '
    t_Star = ur' \u22c6'

    #t_Sum = ur' \u2211 '
    #t_Product = ur' \u220f '

    t_RawStar = r' \* '
    t_Times = ur' \u00d7 '
    t_Divide = ur' \u00f7 '

    t_PlusMinus = ur' \u00b1 '
    t_MinusPlus = ur' \u2213 '

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

    t_Or = ur' \u2228 '
    t_Nor = ur' \u22BD '

    t_And = ur' \u2227 '
    t_Nand = ur' \u22BC '

    t_Xor = ur' \u22BB '
    t_Xnor = ur' \uF4A2 '

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
    t_Colon = ur' \u2236 ' 
    t_Postfix = r' \/\/ '

    t_Set = r' \= '
    t_SetDelayed = r' \:\= '
    t_UpSet = r' \^\= '
    t_UpSetDelayed = r' \^\:\= '
    t_TagSet = r' \/\: '
    t_Unset = r' \=\. '

    t_Semicolon = r' \; '

    #t_DiscreteShift = ur' \uf4a3 '
    #t_DiscreteRatio = ur' \uf4a4 '
    #t_DifferenceDelta = ur' \u2206 '
    t_VerticalTilde = ur' \u2240 '
    t_Coproduct = ur' \u2210 '
    t_Cap = ur' \u2322 '
    t_Cup = ur' \u2323 '
    t_CirclePlus = ur' \u2295 '
    t_CircleMinus = ur' \u2296 '
    t_Intersection = ur' \u22c2 '
    t_Union = ur' \u22c3 '
    t_Equal = ur' \uf431 '
    t_LongEqual = ur' \uf7d9 '
    t_NotEqual = ur' \u2260 '
    t_LessEqual = ur' \u2264 '
    t_LessSlantEqual = ur' \u2a7d '
    t_GreaterEqual = ur' \u2265 '
    t_GreaterSlantEqual = ur' \u2a7e '
    t_VerticalBar = ur' \u2223 '
    t_NotVerticalBar = ur' \u2224 '
    t_DoubleVerticalBar = ur' \u2225 '
    t_NotDoubleVerticalBar = ur' \u2226 '
    t_Element = ur' \u2208 '
    t_NotElement = ur' \u2209 '
    t_Subset = ur' \u2282 '
    t_Superset = ur' \u2283 '
    t_ForAll = ur' \u2200 '
    t_Exists = ur' \u2203 '
    t_NotExists = ur' \u2204 '
    t_Not = ur' \u00AC '
    t_Equivalent = ur' \u29E6 '
    t_Implies = ur' \uF523 '
    t_RightTee = ur' \u22A2 '
    t_DoubleRightTee = ur' \u22A8 '
    t_LeftTee = ur' \u22A3 '
    t_DoubleLeftTee = ur' \u2AE4 '
    t_SuchThat = ur' \u220D '
    t_Rule = ur' \uF522 '
    t_RuleDelayed = ur' \uF51F '
    t_VerticalSeparator = ur' \uF432 '
    t_Therefore = ur' \u2234 '
    t_Because = ur' \u2235 '
    t_Function = ur' \uF4A1 '

    def build(self, **kwargs):
        self.lexer = lex.lex(
            debug=False,
            module=self, 
            **kwargs)

        self.precompiled_regex = {
            'longnames': re.compile(r'(?<!\\)(\\\[[a-zA-Z]+\])'),
            'oct': re.compile(r'(?<!\\)(\\[0-7]{3})'),
            'hex': re.compile(r'(?<!\\)(\\\.[0-9a-fA-F]{2}|\\\:[0-9a-fA-F]{4})')
        }


    def tokenize(self, input_string):
        self.tokens = []
        self.lexer.input(input_string)
        while True:
            tok = self.lexer.token()
            if not tok:
                break
            self.tokens.append(tok)
        return self.tokens

    def convert_character_codes(self, s):
        "Converts character codes to characters E.g. \.7A -> z, \:004a -> J"
        def repl_hex_char(match):
            return unichr(int(match.group(0)[2:], 16))

        def repl_oct_char(match):
            return unichr(int(match.group(0)[1:], 8))

        hex_re = self.precompiled_regex['hex']
        oct_re = self.precompiled_regex['oct']

        s = hex_re.sub(repl_hex_char, s)
        s = oct_re.sub(repl_oct_char, s)

        return s

    def convert_unicode_longnames(self, s):
        "Converts unicode longnames to characters. E.g. \[Theta] -> \u03B8"

        def repl_named_char(match):
            name = match.group(0)[2:-1]
            char = named_characters.get(name)
            if char is not None:
                return char
            else:
                #TODO: Syntax::sntufn message
                return '\\[' + name + ']'

        longnames_re = self.precompiled_regex['longnames']

        return longnames_re.sub(repl_named_char, s)

    @staticmethod
    def string_escape(s):
        s = s.replace('\\\\', '\\').replace('\\"', '"')
        s = s.replace('\\r\\n', '\r\n')
        s = s.replace('\\r', '\r')
        s = s.replace('\\n', '\n')
        return s

    def t_comment(self, t):
        r' (?s) \(\* .*? \*\) '
        return None

    # Lex '1..' as [1, RepeatedNull]. MMA fails when base given e.g. '8^^1..'
    def t_intRepeated(self, t): 
        r' (\d+\^\^[a-zA-Z0-9]+|\d+)(?=\.\.) '
        t = self.t_number(t)
        t.type = 'number'
        return t

    def t_number(self, t):
        r'''
        ( (?# Two possible forms depending on whether base is specified)
            (\d+\^\^([a-zA-Z0-9]+\.?[a-zA-Z0-9]*|[a-zA-Z0-9]*\.?[a-zA-Z0-9]+))
            | (\d+\.?\d*|\d*\.?\d+)
        )
        (``?(\+|-)?(\d+\.?\d*|\d*\.?\d+)|`)?        (?# Precision / Accuracy)
        (\*\^(\+|-)?\d+)?                           (?# Exponent)
        '''
        s = t.value

        # Look for base
        s = s.split('^^')
        if len(s) == 1:
            base, s = 10, s[0]
        else:
            assert len(s) == 2
            base, s = int(s[0]), s[1]
            assert 2 <= base <= 36

        # Look for mantissa
        s = s.split('*^')
        if len(s) == 1:
            n, s = 0, s[0]
        else:
            #TODO: modify regex and provide error message if n not an int
            n, s = int(s[1]), s[0]

        # Look at precision ` suffix to get precision/accuracy
        prec, acc = None, None
        s = s.split('`', 1)
        if len(s) == 1:
            suffix, s = None, s[0]
        else:
            suffix, s = s[1], s[0]

            if suffix == '':
                prec = machine_precision
            elif suffix.startswith('`'):
                acc = float(suffix[1:])
            else:
                if re.match('0+$', s) is not None:
                    t.value = Integer(0)
                    return t
                prec = float(suffix)

        # Look for decimal point
        if s.count('.') == 0:
            if suffix is None:
                if n < 0:
                    t.value = Rational(int(s, base), base ** abs(n))
                else:
                    t.value = Integer(int(s, base) * (base ** n))
                return t
            else:
                s = s + '.'

        if base == 10:
            if n != 0:
                s = s + 'E' + str(n)    # sympy handles this

            if acc is not None:
                if float(s) == 0:
                    prec = 0.
                else:
                    prec = acc + log10(float(s)) + n

            #XXX
            if prec is not None:
                prec = dps(prec)
            t.value = Real(s, prec)
            #t.value = Real(s, prec, acc)
        else:
            # Convert the base
            assert isinstance(base, int) and 2 <= base <= 36

            # Put into standard form mantissa * base ^ n
            s = s.split('.')
            if len(s) == 1:
                man = s[0]
            else:
                n -= len(s[1])
                man = s[0] + s[1]

            man = int(man, base)

            if n >= 0:
                result = Integer(man * base ** n)
            else:
                result = Rational(man, base ** -n)

            if acc is None and prec is None:
                acc = len(s[1])
                acc10 = acc * log10(base)
                prec10 = acc10 + log10(result.to_python())
                if prec10 < 18:
                    prec10 = None
            elif acc is not None:
                acc10 = acc * log10(base)
                prec10 = acc10 + log10(result.to_python())
            elif prec is not None:
                if prec == machine_precision:
                    prec10 = machine_precision
                else:
                    prec10 = prec * log10(base)
            #XXX
            if prec10 is None:
                prec10 = machine_precision
            else:
                prec10 = dps(prec10)

            t.value = result.round(prec10)

        return t

    def t_string(self, t):
        r' "([^\\"]|\\\\|\\"|\\n|\\r|\\r\\n)*" '
        t.value = self.string_escape(t.value[1:-1])
        return t

    @lex.TOKEN(ur'{0}?_\.'.format(base_symb))
    def t_blankdefault(self, t):    # this must come before t_blanks
        #r' ([a-zA-Z$][a-zA-Z0-9$]*)?_\. '
        return t

    @lex.TOKEN(ur'{0}?_(__?)?{0}?'.format(base_symb))
    def t_blanks(self, t):
        #r' ([a-zA-Z$][a-zA-Z0-9$]*)?_(__?)?([a-zA-Z$][a-zA-Z0-9$]*)? '
        return t

    @lex.TOKEN(ur'`?{0}(`{0})*'.format(base_symb))
    def t_symbol(self, t):
        #r' `?[a-zA-Z$][a-zA-Z0-9$]*(`[a-zA-Z$][a-zA-Z0-9$]*)* '
        s = t.value
        if s.startswith('`'):
            #FIXME: Replace Global with the current value of $Context
            s = 'Global' + s
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

    def t_INITIAL_LeftBoxParenthesis(self, t):
        r' \\\( '
        t.lexer.level = 1
        t.lexer.begin('boxes')
        return t

    def t_boxes_LeftBoxParenthesis(self, t):
        r' \\\( '
        t.lexer.level += 1
        t.type = 'LeftBoxParenthesisInternal'
        return t

    def t_boxes_RightBoxParenthesis(self, t):
        r' \\\) '
        t.lexer.level -= 1
        if t.lexer.level == 0:
            t.lexer.begin('INITIAL')
            return t
        else:
            t.type = 'RightBoxParenthesisInternal'
            return t

    def t_PutAppend(self, t):
        r' \>\>\> '
        t.lexer.begin('file')
        return t

    def t_Put(self, t):
        r' \>\> '
        t.lexer.begin('file')
        return t

    def t_Get(self, t):
        r' \<\< '
        t.lexer.begin('file')
        return t

    def t_file_filename(self, t):
        r'''
        (?P<quote>\"?)                              (?# Opening quotation mark)
            [a-zA-Z0-9\`/\.\\\!\-\:\_\$\*\~\?]+     (?# Literal characters)
        (?P=quote)                                  (?# Closing quotation mark)
        '''
        s = t.value
        if s.startswith('"'):
            s = s[1:-1]
        s = self.string_escape(s)
        s = s.replace('\\', '\\\\')
        t.value = String(s)
        t.lexer.begin('INITIAL')
        return t

    def t_ANY_error(self, t):
        # print t
        raise ScanError(self.lexer.lexpos)

class AbstractToken(object):
    def __init__(self, items):
        self.items = items

class SequenceToken(AbstractToken):
    pass

class ArgsToken(AbstractToken):
    pass

class PositionToken(AbstractToken):
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

        for innequality_op in innequality_operators:
            @ONEARG
            def tmp(args, op=innequality_op):
                head = args[1].get_head_name()
                if head == op:
                    args[1].leaves.append(args[3])
                    args[0] = args[1]
                elif head == 'Inequality':
                    args[1].leaves.append(Symbol(op))
                    args[1].leaves.append(args[3])
                    args[0] = args[1]
                elif head in innequality_operators.keys():
                    leaves = []
                    for i, leaf in enumerate(args[1].leaves):
                        if i != 0:
                            leaves.append(Symbol(head))
                        leaves.append(leaf)
                    leaves.append(Symbol(op))
                    leaves.append(args[3])
                    args[0] = Expression('Inequality', *leaves)
                else:
                    args[0] = Expression(op, args[1], args[3])
            tokens = innequality_operators[innequality_op]
            if not isinstance(tokens, list):
                tokens = [tokens]
            tmp.__doc__ = 'expr : ' + '\n     | '.join(['expr {0} expr'.format(token) for token in tokens])
            setattr(self, 'p_{0}_innequality'.format(innequality_op), tmp)

    def build(self, **kwargs):
        self.parser = yacc.yacc(
            module=self,
            debug=False,
            tabmodule='mathics.core.parsetab',  # where to look for parsetab
            outputdir='mathics/core/',          # where to store parsetab
            **kwargs)

    def p_error(self, p):
        if p is not None:
            p = p.value
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
        
    def p_number(self, args):
        'expr : number'
        args[0] = args[1]
        
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
        n = len(args[2])
        if isinstance(args[1], Expression) and isinstance(args[1].head,
            Expression) and args[1].head.get_head_name() == 'Derivative' \
            and args[1].head.leaves[0].get_int_value() is not None:
            n += args[1].head.leaves[0].get_int_value()
            args[1] = args[1].leaves[0]
        args[0] = Expression(Expression('Derivative', Integer(n)), args[1])

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
        if args[2].get_head_name() in ['Integer', 'Real']:
            args[2].value = -args[2].value
            args[0] = args[2]
        else:
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
            assert args[2].get_head_name != 'Times'
            if args[1].get_head_name() == 'Times':
                args[1].leaves.append(args[2])
                args[0] = args[1]
            else:
                args[0] = Expression('Times', args[1], args[2])
        elif len(args) == 4:
            assert args[3].get_head_name != 'Times'
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
            if isinstance(args[1], BaseExpression):
                args[0] = Expression('Span', args[1], args[3])
            else:
                args[0] = Expression('Span', Integer(1), Symbol('All'), args[3])
        elif len(args) == 3:
            if isinstance(args[1], BaseExpression):
                args[0] = Expression('Span', args[1], Symbol('All'))
            elif isinstance(args[2], BaseExpression):
                args[0] = Expression('Span', Integer(1), args[2])
        elif len(args) == 2:
                args[0] = Expression('Span', Integer(1), Symbol('All'))

    def p_Not(self, args):
        '''expr : Not expr
                | Factorial expr %prec Not
                | Factorial2 expr %prec Not'''
        args[0] = Expression('Not', args[2])
        if args[1] == '!!':
            args[0] = Expression('Not', args[0])

    def p_Pattern(self, args):
        '''expr : symbol RawColon pattern RawColon expr
                | symbol RawColon expr'''
        args[0] = Expression('Pattern', Symbol(args[1]), args[3])
        if len(args) == 6:
            args[0] = Expression('Optional', args[0], args[5])
        elif args[3].get_head_name() == 'Pattern':
            args[0] = Expression('Optional', Expression('Pattern', Symbol(args[1]), args[3].leaves[0]), args[3].leaves[1])

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
        '''expr : expr Semicolon expr
                | expr Semicolon'''
        if args[1].get_head_name() == 'CompoundExpression':
            pass
        else:
            args[1] = Expression('CompoundExpression', args[1])
        if len(args) == 4:
            args[1].leaves.append(args[3])
        else:
            args[1].leaves.append(Symbol('Null'))
        args[0] = args[1]

    def p_box_to_expr(self, args):
        '''expr : LeftBoxParenthesis boxes RightBoxParenthesis
                | InterpretedBox LeftBoxParenthesis boxes RightBoxParenthesis %prec InterpretedBox'''
        if len(args) == 4:
            args[0] = args[2]
        else:
            result = Expression('MakeExpression', args[3])
            args[0] = Expression('ReleaseHold', result) #remove HoldComplete

    def p_box(self, args):
        'box : expr'
        args[0] = Expression('MakeBoxes', args[1])

    def p_form(self, args):
        'form : expr'
        if args[1].get_head_name() == 'Symbol':
            args[0] = args[1]
        else:
            args[0] = Expression('Removed', String("$$Failure"))

    def p_boxes(self, args):
        '''boxTOKEN : box
                    | boxTOKEN box
              boxes : boxTOKEN
                    |'''
        if len(args) == 1:
            args[0] = String("")
        if len(args) == 2:
            if isinstance(args[1], list):
                if len(args[1]) > 1:
                    args[0] = Expression('RowBox', *args[1])
                else:
                    args[0] = args[1][0]
            else:
                args[0] = [args[1]]
        elif len(args) == 3:
            args[1].append(args[2])
            args[0] = args[1]

    def p_RowBox(self, args):       # used for grouping raw boxes
        'box : LeftBoxParenthesisInternal boxes RightBoxParenthesisInternal'
        args[2].parenthesized = True
        args[0] = args[2]

    def p_SuperscriptBox(self, args):
        '''box : box Superscript box Otherscript box %prec Superscript
               | box Superscript box'''
        if len(args) == 4:
            args[0] = Expression('SuperscriptBox', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('SubsuperscriptBox', args[1], args[5], args[3])

    def p_Subscript(self, args):
        '''box : box Subscript box Otherscript box %prec Subscript
               | box Subscript box'''
        if len(args) == 4:
            args[0] = Expression('SubscriptBox', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('SubsuperscriptBox', args[1], args[3], args[5])

    def p_OverscriptBox(self, args):
        '''box : box Overscript box Otherscript box %prec Overscript
               | box Overscript box'''
        if len(args) == 4:
            args[0] = Expression('OverscriptBox', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('UnderoverscriptBox', args[1], args[5], args[3])

    def p_UnderscriptBox(self, args):
        '''box : box Underscript box Otherscript box %prec Underscript
               | box Underscript box'''
        if len(args) == 4:
            args[0] = Expression('UnderscriptBox', args[1], args[3])
        elif len(args) == 6:
            args[0] = Expression('UnderoverscriptBox', args[1], args[3], args[5])

    def p_FractionBox(self, args):
        'box : box Fraction box'
        args[0] = Expression('FractionBox', args[1], args[3])

    def p_SqrtBox(self, args):
        '''box : Sqrt box Otherscript box %prec Sqrt
               | Sqrt box'''
        if len(args) == 3:
            args[0] = Expression('SqrtBox', args[2])
        elif len(args) == 5:
            args[0] = Expression('RadicalBox', args[2], args[4])

    def p_FormBox(self, args):
        'box : form FormBox box'
        args[0] = Expression('FormBox', args[3], args[1])

scanner = MathicsScanner()
scanner.build()
parser = MathicsParser()
parser.build()

def parse(string):
    scanner.lexer.begin('INITIAL')      # Reset the lexer state (known lex bug)

    string = scanner.convert_unicode_longnames(string)
    string = scanner.convert_character_codes(string)

    return parser.parse(string)
