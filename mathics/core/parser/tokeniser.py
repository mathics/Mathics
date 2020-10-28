#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import re
import string

from mathics.core.parser.errors import ScanError
from mathics.core.parser.prescanner import Prescanner
from mathics.core.characters import letters, letterlikes


# special patterns
number_pattern = r'''
( (?# Two possible forms depending on whether base is specified)
    (\d+\^\^([a-zA-Z0-9]+\.?[a-zA-Z0-9]*|[a-zA-Z0-9]*\.?[a-zA-Z0-9]+))
    | (\d+\.?\d*|\d*\.?\d+)
)
(``?(\+|-)?(\d+\.?\d*|\d*\.?\d+)|`)?        (?# Precision / Accuracy)
(\*\^(\+|-)?\d+)?                           (?# Exponent)
'''
base_symbol_pattern = r'((?![0-9])([0-9${0}{1}])+)'.format(letters, letterlikes)
full_symbol_pattern = r'(`?{0}(`{0})*)'.format(base_symbol_pattern)
pattern_pattern = r'{0}?_(\.|(__?)?{0}?)?'.format(full_symbol_pattern)
slot_pattern = r'\#(\d+|{0})?'.format(base_symbol_pattern)
filename_pattern = r'''
(?P<quote>\"?)                              (?# Opening quotation mark)
    [a-zA-Z0-9\`/\.\\\!\-\:\_\$\*\~\?]+     (?# Literal characters)
(?P=quote)                                  (?# Closing quotation mark)
'''

tokens = [
    ('Definition', r'\? '),
    ('Information', r'\?\? '),

    ('Number', number_pattern),
    ('String', r'"'),
    ('Pattern', pattern_pattern),
    ('Symbol', full_symbol_pattern),
    ('SlotSequence', r'\#\#\d*'),
    ('Slot', slot_pattern),
    ('Out', r'\%(\%+|\d+)?'),
    ('PutAppend', r'\>\>\>'),
    ('Put', r'\>\>'),
    ('Get', r'\<\<'),
    ('RawLeftBracket', r' \[ '),
    ('RawRightBracket', r' \] '),
    ('RawLeftBrace', r' \{ '),
    ('RawRightBrace', r' \} '),
    ('RawLeftParenthesis', r' \( '),
    ('RawRightParenthesis', r' \) '),
    ('RawLeftAssociation', r' \<\| '),
    ('RawRightAssociation', r' \|\> '),     
    
    ('RawComma', r' \, '),

    ('Span', r' \;\; '),

    ('MessageName', r' \:\: '),

    # boxes
    ('LeftRowBox', r' \\\( '),
    ('RightRowBox', r' \\\) '),
    ('InterpretedBox', r' \\\! '),
    ('SuperscriptBox', r' \\\^ '),
    ('SubscriptBox', r' \\\_ '),
    ('OverscriptBox', r' \\\& '),
    ('UnderscriptBox', r' \\\+ '),
    ('OtherscriptBox', r' \\\% '),
    ('FractionBox', r' \\\/ '),
    ('SqrtBox', r' \\\@ '),
    ('FormBox', r' \\\` '),

    ('Information', r'\?\?'),
    ('PatternTest', r' \? '),
    ('Increment', r' \+\+ '),
    ('Decrement', r' \-\- '),

    ('MapAll', r' \/\/\@ '),
    ('Map', r' \/\@ '),
    ('ApplyList', r' \@\@\@ '),
    ('Apply', r' \@\@ '),
    ('Composition', r' \@\* '),
    ('Prefix', r' \@ '),

    ('StringExpression', r' \~\~ '),
    ('Infix', r' \~ '),

    ('Derivative', r' \' '),
    ('StringJoin', r' \<\> '),

    ('NonCommutativeMultiply', r' \*\* '),

    ('AddTo', r' \+\= '),
    ('SubtractFrom', r' \-\=  '),
    ('TimesBy', r' \*\= '),
    ('DivideBy', r' \/\=  '),

    ('Times', r' \*|\u00d7 '),

    ('SameQ', r' \=\=\= '),
    ('UnsameQ', r' \=\!\= '),

    ('Equal', r' (\=\=) | \uf431 | \uf7d9 '),
    ('Unequal', r' (\!\= ) | \u2260 '),
    ('LessEqual', r' (\<\=) | \u2264 '),
    ('LessSlantEqual', r' \u2a7d '),
    ('GreaterEqual', r' (\>\=) | \u2265 '),
    ('GreaterSlantEqual', r' \u2a7e '),

    ('Greater', r' \> '),
    ('Less', r' \< '),

    ('Or', r' (\|\|) | \u2228 '),
    ('And', r' (\&\&) | \u2227 '),

    ('RepeatedNull', r' \.\.\. '),
    ('Repeated', r' \.\. '),
    ('Alternatives', r' \| '),

    ('Rule', r' (\-\>)|\uF522 '),
    ('RuleDelayed', r' (\:\>)|\uF51F '),
    ('UndirectedEdge', r' (\<\-\>)|\u29DF '),
    ('ReplaceRepeated', r' \/\/\. '),
    ('ReplaceAll', r' \/\. '),

    ('Postfix', r' \/\/ '),

    ('UpSetDelayed', r' \^\:\= '),
    ('SetDelayed', r' \:\= '),
    ('UpSet', r' \^\= '),
    ('TagSet', r' \/\: '),
    ('Unset', r' \=\s*\.(?!\d|\.) '),  # allow whitspace but avoid e.g. x=.01
    ('Set', r' \= '),

    ('Condition', r' \/\; '),
    ('Semicolon', r' \; '),

    ('Divide', r' \/|\u00f7 '),
    ('Power', r' \^ '),
    ('Dot', r' \. '),
    ('Minus', r' \-|\u2212 '),
    ('Plus', r' \+ '),
    ('RawBackslash', r' \\ '),
    
    ('Factorial2', r' \!\! '),
    ('Factorial', r' \! '),
    ('Function', r' \& | \uF4A1 '),
    ('RawColon', r' \: '),

    # ('DiscreteShift', r' \uf4a3 '),
    # ('DiscreteRatio', r' \uf4a4 '),
    # ('DifferenceDelta', r' \u2206 '),
    # ('PartialD', r' \u2202 '),

    ('Cross', r' \uf4a0 '),
    ('Colon', r' \u2236 '),
    ('Transpose', r' \uf3c7 '),
    ('Conjugate', r' \uf3c8 '),
    ('ConjugateTranspose', r' \uf3c9 '),
    ('HermitianConjugate', r' \uf3ce '),
    ('Integral', r' \u222b '),
    ('DifferentialD', r' \uf74c '),
    ('Del', r' \u2207 '),
    ('Square', r' \uf520 '),
    ('SmallCircle', r' \u2218 '),
    ('CircleDot', r' \u2299 '),

    # ('Sum', r' \u2211 '),
    # ('Product', r' \u220f '),
    ('PlusMinus', r' \u00b1 '),
    ('MinusPlus', r' \u2213 '),
    ('Nor', r' \u22BD '),
    ('Nand', r' \u22BC '),
    ('Xor', r' \u22BB '),
    ('Xnor', r' \uF4A2 '),
    ('Diamond', r' \u22c4 '),
    ('Wedge', r' \u22c0 '),
    ('Vee', r' \u22c1 '),
    ('CircleTimes', r' \u2297 '),
    ('CenterDot', r' \u00b7 '),
    ('Star', r' \u22c6'),
    ('VerticalTilde', r' \u2240 '),
    ('Coproduct', r' \u2210 '),
    ('Cap', r' \u2322 '),
    ('Cup', r' \u2323 '),
    ('CirclePlus', r' \u2295 '),
    ('CircleMinus', r' \u2296 '),
    ('Intersection', r' \u22c2 '),
    ('Union', r' \u22c3 '),
    ('VerticalBar', r' \u2223 '),
    ('NotVerticalBar', r' \u2224 '),
    ('DoubleVerticalBar', r' \u2225 '),
    ('NotDoubleVerticalBar', r' \u2226 '),
    ('Element', r' \u2208 '),
    ('NotElement', r' \u2209 '),
    ('Subset', r' \u2282 '),
    ('Superset', r' \u2283 '),
    ('ForAll', r' \u2200 '),
    ('Exists', r' \u2203 '),
    ('NotExists', r' \u2204 '),
    ('Not', r' \u00AC '),
    ('Equivalent', r' \u29E6 '),
    ('Implies', r' \uF523 '),
    ('RightTee', r' \u22A2 '),
    ('DoubleRightTee', r' \u22A8 '),
    ('LeftTee', r' \u22A3 '),
    ('DoubleLeftTee', r' \u2AE4 '),
    ('SuchThat', r' \u220D '),
    ('VerticalSeparator', r' \uF432 '),
    ('Therefore', r' \u2234 '),
    ('Because', r' \u2235 '),
    ('Backslash', r' \u2216 '),
]


literal_tokens = {
    '!': ['Unequal', 'Factorial2', 'Factorial'],
    '"': ['String'],
    '#': ['SlotSequence', 'Slot'],
    '%': ['Out'],
    '&': ['And', 'Function'],
    "'": ['Derivative'],
    '(': ['RawLeftParenthesis'],
    ')': ['RawRightParenthesis'],
    '*': ['NonCommutativeMultiply', 'TimesBy', 'Times'],
    '+': ['Increment', 'AddTo', 'Plus'],
    ',': ['RawComma'],
    '-': ['Decrement', 'SubtractFrom', 'Rule', 'Minus'],
    '.': ['Number', 'RepeatedNull', 'Repeated', 'Dot'],
    '/': ['MapAll', 'Map', 'DivideBy', 'ReplaceRepeated', 'ReplaceAll',
          'Postfix', 'TagSet', 'Condition', 'Divide'],
    ':': ['MessageName', 'RuleDelayed', 'SetDelayed', 'RawColon'],
    ';': ['Span', 'Semicolon'],
    '<': ['RawLeftAssociation', 'UndirectedEdge', 'Get', 'StringJoin', 'LessEqual', 'Less'],
    '=': ['SameQ', 'UnsameQ', 'Equal', 'Unset', 'Set'],
    '>': ['PutAppend', 'Put', 'GreaterEqual', 'Greater'],
    '?': ['Information', 'PatternTest'],
    '@': ['ApplyList', 'Apply', 'Composition', 'Prefix'],
    '[': ['RawLeftBracket'],
    '\\': ['LeftRowBox', 'RightRowBox', 'InterpretedBox', 'SuperscriptBox',
           'SubscriptBox', 'OverscriptBox', 'UnderscriptBox', 'OtherscriptBox',
           'FractionBox', 'SqrtBox', 'FormBox', 'RawBackslash'],
    ']': ['RawRightBracket'],
    '^': ['UpSetDelayed', 'UpSet', 'Power'],
    '_': ['Pattern'],
    '`': ['Pattern', 'Symbol'],
    '|': ['RawRightAssociation', 'Or', 'Alternatives'],
    '{': ['RawLeftBrace'],
    '}': ['RawRightBrace'],
    '~': ['StringExpression', 'Infix']
}

for c in string.ascii_letters:
    literal_tokens[c] = ['Pattern', 'Symbol']

for c in string.digits:
    literal_tokens[c] = ['Number']


def find_indices(literals):
    'find indices of literal tokens'
    literal_indices = {}
    for key, tags in literals.items():
        indices = []
        for tag in tags:
            for i, (tag2, pattern) in enumerate(tokens):
                if tag == tag2:
                    indices.append(i)
                    break
        literal_indices[key] = tuple(indices)
        assert len(indices) == len(tags)
    return literal_indices


def compile_pattern(pattern):
    return re.compile(pattern, re.VERBOSE)


def compile_tokens(token_list):
    return [(tag, compile_pattern(pattern)) for tag, pattern in token_list]


filename_tokens = [
    ('Filename', filename_pattern),
]

token_indices = find_indices(literal_tokens)
tokens = compile_tokens(tokens)
filename_tokens = compile_tokens(filename_tokens)
full_symbol_pattern = compile_pattern(full_symbol_pattern)


def is_symbol_name(text):
    return full_symbol_pattern.sub('', text) == ''


class Token(object):
    def __init__(self, tag, text, pos):
        self.tag = tag
        self.text = text
        self.pos = pos

    def __eq__(self, other):
        if not isinstance(other, Token):
            raise TypeError()
        return self.tag == other.tag and self.text == other.text and self.pos == other.pos

    def __repr__(self):
        return 'Token(%s, %s, %i)' % (self.tag, self.text, self.pos)


class Tokeniser(object):
    modes = {
        'expr': (tokens, token_indices),
        'filename': (filename_tokens, {}),
    }

    def __init__(self, feeder):
        self.pos = 0
        self.feeder = feeder
        self.prescanner = Prescanner(feeder)
        self.code = self.prescanner.scan()
        self.change_mode('expr')

    def change_mode(self, mode):
        self.mode = mode
        self.tokens, self.token_indices = self.modes[mode]

    def incomplete(self):
        'get more code from the prescanner and continue'
        self.prescanner.incomplete()
        self.code += self.prescanner.scan()

    def sntx_message(self, pos=None):
        if pos is None:
            pos = self.pos
        pre, post = self.code[:pos], self.code[pos:].rstrip('\n')
        if pos == 0:
            self.feeder.message('Syntax', 'sntxb', post)
        else:
            self.feeder.message('Syntax', 'sntxf', pre, post)

    def next(self):
        'return next token'
        self.skip_blank()
        if self.pos >= len(self.code):
            return Token('END', '', len(self.code))

        # look for a matching pattern
        indices = self.token_indices.get(self.code[self.pos], ())
        if indices:
            for index in indices:
                tag, pattern = self.tokens[index]
                match = pattern.match(self.code, self.pos)
                if match is not None:
                    break
        else:
            for tag, pattern in self.tokens:
                match = pattern.match(self.code, self.pos)
                if match is not None:
                    break

        # no matching pattern found
        if match is None:
            self.sntx_message()
            raise ScanError()

        # custom tokenisation rules defined with t_tag
        override = getattr(self, 't_' + tag, None)
        if override is not None:
            return override(match)
        else:
            text = match.group(0)
            self.pos = match.end(0)
            return Token(tag, text, match.start(0))

    def skip_blank(self):
        'skip whitespace and comments'
        comment = []   # start positions of comments
        while True:
            if self.pos >= len(self.code):
                if comment:
                    self.incomplete()
                else:
                    break
            if comment:
                if self.code.startswith('(*', self.pos):
                    comment.append(self.pos)
                    self.pos += 2
                elif self.code.startswith('*)', self.pos):
                    comment.pop()
                    self.pos += 2
                else:
                    self.pos += 1
            elif self.code.startswith('(*', self.pos):
                comment.append(self.pos)
                self.pos += 2
            elif self.code[self.pos] in ' \r\n\t':
                self.pos += 1
            else:
                break

    def t_String(self, match):
        start, end = self.pos, None
        self.pos += 1   # skip opening '"'
        newlines = []
        while True:
            if self.pos >= len(self.code):
                if end is None:
                    # reached end while still inside string
                    self.incomplete()
                    newlines.append(self.pos)
                else:
                    break
            c = self.code[self.pos]
            if c == '"':
                self.pos += 1
                end = self.pos
                break
            elif c == '\\':
                self.pos += 2
            else:
                self.pos += 1
        indices = [start] + newlines + [end]
        result = ''.join(self.code[indices[i]:indices[i + 1]]
                         for i in range(len(indices) - 1))
        return Token('String', result, start)

    def t_Number(self, match):
        text = match.group(0)
        pos = match.end(0)
        if self.code[pos - 1:pos + 1] == '..':
            # Trailing .. should be ignored. That is, `1..` is `Repeated[1]`.
            text = text[:-1]
            self.pos = pos - 1
        else:
            self.pos = pos
        return Token('Number', text, match.start(0))

    def token_mode(self, match, tag, mode):
        'consume a token and switch mode'
        text = match.group(0)
        self.pos = match.end(0)
        self.change_mode(mode)
        return Token(tag, text, match.start(0))

    def t_Get(self, match):
        return self.token_mode(match, 'Get', 'filename')

    def t_Put(self, match):
        return self.token_mode(match, 'Put', 'filename')

    def t_PutAppend(self, match):
        return self.token_mode(match, 'PutAppend', 'filename')

    def t_Filename(self, match):
        return self.token_mode(match, 'Filename', 'expr')
