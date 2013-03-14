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

from re import escape

#import re
#from re import compile, escape
#
#import unicodedata
#
#from mathics.core.expression import BaseExpression, Expression, Integer, Real, Symbol, String
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

class MathicsScanner:

    tokens = [
        'comment',
        'parenthesis_0',
        'parenthesis_1',
        'parenthesis_2',
        'parenthesis_3',
        'parenthesis_4',
        'parenthesis_5',
        'parenthesis_6',
        'parenthesis_7',
        'comma',
        'symbol',
        'float',
        'int', 
        'blanks', 
        'blankdefault',
        'string',
        'out_1',
        'out_2',
        'slotseq_1',
        'slotseq_2',
        'slotsingle_1',
        'slotsingle_2',
        'span',
        'other',
        'default',
    ] + ['operator_%.4d' % i for i in range(len(operators.keys()))]

    #t_ignore = ur' [\s\u2062]+ '
    t_ignore = ' \t '

    t_comment = r' (?s) \(\* .*? \*\) '
    t_symbol = r' [a-zA-Z$][a-zA-Z0-9$]* '
    t_comma = r' , '
    t_int = r' \d+ '
    t_blanks = r' ([a-zA-Z$][a-zA-Z0-9$]*)?_(__?)?([a-zA-Z$][a-zA-Z0-9$]*)? '
    t_blankdefault = r' ([a-zA-Z$][a-zA-Z0-9$]*)?_\. '

    t_parenthesis_0 = r' \[\[ '
    t_parenthesis_1 = r' \[ '
    t_parenthesis_2 = r' \]\] '
    t_parenthesis_3 = r' \] '
    t_parenthesis_4 = r' \( '
    t_parenthesis_5 = r' \) '
    t_parenthesis_6 = r' \{ '
    t_parenthesis_7 = r' \} '

    t_out_1 = r' \%\d+ '
    t_out_2 = r' \%+ '
    t_slotseq_1 = r' \#\#\d+ '
    t_slotseq_2 = r' \#\# '

    t_slotsingle_1 = r' \#\d+ '
    t_slotsingle_2 = r' \# '
    t_span = r' \;\; '
    t_other = r' \/\: | \=\. '

    def build(self, **kwargs):
        # add operators
        symbols = operators.keys()
        symbols.sort(key=lambda s: len(s), reverse=True)
        index = 1
        for symbol in symbols:
            def t_op(t):
                return t

            t_op.__doc__ = escape(symbol)
            setattr(self, 't_operator_%.4d' % index, t_op)
            index += 1
            
        self.lexer = lex.lex(debug=1, module=self, **kwargs)

    def tokenize(self, input_string):
        self.tokens = []
        self.lexer.input(input_string)
        while True:
            tok = self.lexer.token()
            if not tok:
                break
            print tok
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
    
    #def t_default(self, t):
    #    r'( . | \n )+'        
    #    print t.value, type(t.type)
    #    raise InvalidCharError(t.value)
    
    def t_error(self, t):
        print t
        raise ScanError(self.lexer.lexpos)
    
    #def t_int(self, t):
    #    r' \d+ '
    #    return t
    #    
    #def t_blanks(self, t):
    #    r' ([a-zA-Z$][a-zA-Z0-9$]*)?_(__?)?([a-zA-Z$][a-zA-Z0-9$]*)? '
    #    return t
    #
    #def t_blankdefault(self, t):
    #    r' ([a-zA-Z$][a-zA-Z0-9$]*)?_\. '
    #    return t
        
    #def t_comment(self, t):
    #    r'(?s) \(\* .*? \*\) '
    #    
    #def t_parenthesis_0(self, t):
    #    r' \[\[ '
    #    self.open_square_parenthesizes.append('[[')
    #    
    #def t_parenthesis_1(self, t):
    #    r' \[ '
    #    self.open_square_parenthesizes.append('[')
    #    return t
    #    
    #def t_parenthesis_2(self, s):
    #    r' \]\] '
    #    
    #    last = self.open_square_parenthesizes.pop() if self.open_square_parenthesizes else None
    #    if last == '[[':
    #        pass
    #    else:
    #        if self.open_square_parenthesizes:
    #            self.open_square_parenthesizes.pop()
    #        raise NotImplementedError
    #        #self.tokens.append(Token(type=']'))
    #        #self.tokens.append(Token(type=']'))
    #
    #def t_parenthesis_3(self, t):
    #    r' \] '
    #    if self.open_square_parenthesizes:
    #        self.open_square_parenthesizes.pop()
    #    return t
    #    
    #def t_comma(self, t):
    #    r' , '
    #    t.value = ''
    #    return t
    #    
    #def t_symbol(self, t):
    #    r' [a-zA-Z$][a-zA-Z0-9$]* '
    #    return t
    #       
    #def t_out_1(self, t):
    #    r' \%\d+ '
    #    (t.type, t.value) = ('out', int(t.value[1:]))
    #    return t
    #    
    #def t_out_2(self, t):
    #    r' \%+ '
    #    (t.type, t.value) = ('out', -len(t.value))
    #    return t
    #    
    #def t_slotseq_1(self, t):
    #    r' \#\#\d+ '
    #    (t.type, t.value) = ('slotseq', int(t.value[2:]))
    #    return t
    #    
    #def t_slotseq_2(self, t):
    #    r' \#\# '
    #    (t.type, t.value) = ('slotseq', 1)
    #    return t
    #    
    #def t_slotsingle_1(self, t):
    #    r' \#\d+ '
    #    (t.type, t.value) = ('slot', int(t.value[1:]))
    #    return t
    #    
    #def t_slotsingle_2(self, t):
    #    r' \# '
    #    (t.type, t.value) = ('slot', 1)
    #    return t
    #    
    #def t_span(self, t):
    #    r' \;\; '
    #    #FIXME
    #    #t.value = ''
    #    #return t
    #    return t
    #    
    #def t_other(self, t):
    #    r' \/\: | \=\. '
    #    #FIXME
    #    #t.value = (t.value, '')
    #    #return t
    #    return t

m = MathicsScanner()
m.build()
m.tokenize('1 + 1')
m.tokenize('Sin[x]')
m.tokenize('mysym + x + 1 - 3.4')
m.tokenize('4 / 2')
m.tokenize('1 + (2 - 4)')

quit()

#class CompoundToken(AbstractToken):
#    def __init__(self, items):
#        self.items = items
#        
#class SequenceToken(CompoundToken):
#    pass
#        
#class ArgsToken(CompoundToken):
#    pass
#        
#class PositionToken(CompoundToken):
#    pass
#
#class RestToken(AbstractToken):
#    pass
#
#    # Actual expressions in there don't matter - we just use its parse_tokens property!
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
#class MathicsParser:
#    def __init__(self):
#        self.parser = yacc.yacc(object=self)
#
#    def typestring(self, token):
#        if isinstance(token, BaseExpression):
#            return 'expr'
#        else:
#            return token.type
#    
#    def error(self, token):
#        raise ParseError(token)
#    
#    def parse(self, tokens):
#        result = GenericParser.parse(self, tokens)
#        result = result.post_parse()
#        return result
#    
#    @parsing
#    def op_400(self, args):
#        ' expr ::= expr expr '
#        return builtins['Times'].parse([args[0], None, args[1]])
#    
#    @parsing
#    def p_parenthesis(self, args):
#        ' expr ::= ( expr ) '
#        expr = args[1]
#        expr.parenthesized = True
#        return expr
#    
#    @parsing
#    def op_045_tagset(self, args):
#        ' expr ::= expr /: expr = expr '
#        
#        return Expression('TagSet', args[0], args[2], args[4])
#    
#    @parsing
#    def op_045_tagsetdelayed(self, args):
#        ' expr ::= expr /: expr := expr '
#        
#        return Expression('TagSetDelayed', args[0], args[2], args[4])
#    
#    @parsing
#    def op_045_tagunset(self, args):
#        ' expr ::= expr /: expr =. '
#        
#        return Expression('TagUnset', args[0], args[2])
#    
#    @parsing
#    def op_010(self, args):
#        ' expr ::= expr ; '
#        
#        return Expression('CompoundExpression', args[0], Symbol('Null'))
#    
#    @parsing
#    def p_parsed_expr(self, args):
#        ' expr ::= parsedexpr '
#        return args[0].value
#    
#    @parsing
#    def op_670_call(self, args):
#        ' expr ::= expr args'
#        expr = Expression(args[0], *args[1].items)
#        expr.parenthesized = True # to handle e.g. Power[a,b]^c correctly
#        return expr
#    
#    @parsing
#    def op_670_part(self, args):
#        ' expr ::= expr position'
#        return Expression('Part', args[0], *args[1].items)
#    
#    @parsing
#    def p_span_start_1(self, args):
#        ' span_start ::= expr '
#        return args[0]
#    
#    @parsing
#    def p_span_start_2(self, args):
#        ' span_start ::= '
#        return Integer(1)
#    
#    @parsing
#    def p_span_stop_1(self, args):
#        ' span_stop ::= expr '
#        return args[0]
#    
#    @parsing
#    def p_span_stop_2(self, args):
#        ' span_stop ::= '
#        return Symbol('All')
#    
#    @parsing
#    def p_span_step_1(self, args):
#        ' span_step ::= expr '
#        return args[0]
#    
#    @parsing
#    def p_span_step_2(self, args):
#        ' span_step ::= '
#        return Integer(1)
#    
#    @parsing
#    def op_305_1(self, args):
#        ' expr ::= span_start ;; span_stop ;; span_step '
#        return Expression('Span', args[0], args[2], args[4])
#    
#    @parsing
#    def op_305_2(self, args):
#        ' expr ::= span_start ;; span_stop '
#        return Expression('Span', args[0], args[2], Integer(1))
#    
#    @parsing
#    def p_args_1(self, args):
#        ' args ::= [ sequence ] '
#        return ArgsToken(args[1].items)
#    
#    @parsing
#    def p_list(self, args):
#        ' expr ::= { sequence } '
#        return Expression('List', *args[1].items)
#    
#    @parsing
#    def p_position(self, args):
#        ' position ::= [[ sequence ]]'
#        return PositionToken(args[1].items)
#    
#    @parsing
#    def p_rest_left(self, args):
#        '''
#        rest_left ::=
#        rest_left ::= expr
#        rest_left ::= expr binary_op
#        '''
#        return RestToken()
#    
#    @parsing
#    def p_rest_right(self, args):
#        '''
#        rest_right ::=
#        rest_right ::= expr
#        rest_right ::= args rest_right
#        rest_right ::= position rest_right
#        rest_right ::= rest_right binary_op expr
#        '''
#        return RestToken()
#    
#    @parsing
#    def p_sequence_0(self, args):
#        ' sequence ::= '
#        return SequenceToken([])
#    
#    @parsing
#    def p_sequence_1(self, args):
#        ' sequence ::= sequence , expr '
#        return SequenceToken(args[0].items + [args[2]])
#        
#    @parsing
#    def p_sequence_2(self, args):
#        ' sequence ::= expr '
#        return SequenceToken([args[0]])
#        
#    @parsing
#    def p_sequence_3(self, args):
#        ' sequence ::= , '
#        return SequenceToken([Symbol('Null'), Symbol('Null')])
#        
#    @parsing
#    def p_sequence_4(self, args):
#        ' sequence ::= sequence , '
#        return SequenceToken(args[0].items + [Symbol('Null')])
#        
#    @parsing
#    def p_symbol(self, args):
#        ' expr ::= symbol '
#        return Symbol(args[0].value)
#        
#    @parsing
#    def p_int(self, args):
#        ' expr ::= int '
#        return Integer(args[0].value)
#        
#    @parsing
#    def p_float(self, args):
#        ' expr ::= float '
#        return Real(args[0].value)
#        
#    @parsing
#    def p_blanks(self, args):
#        ' expr ::= blanks '
#        value = args[0].value
#        pieces = value.split('_')
#        count = len(pieces) - 1
#        if count == 1:
#            name = 'Blank'
#        elif count == 2:
#            name = 'BlankSequence'
#        elif count == 3:
#            name = 'BlankNullSequence'
#        if pieces[-1]:
#            blank = Expression(name, Symbol(pieces[-1]))
#        else:
#            blank = Expression(name)
#        if pieces[0]:
#            return Expression('Pattern', Symbol(pieces[0]), blank)
#        else:
#            return blank
#        
#    @parsing
#    def p_blankdefault(self, args):
#        ' expr ::= blankdefault '
#        value = args[0].value
#        name = value[:-2]
#        if name:
#            return Expression('Optional', Expression('Pattern', Symbol(name), Expression('Blank')))
#        else:
#            return Expression('Optional', Expression('Blank'))
#        
#    @parsing
#    def p_slot(self, args):
#        ' expr ::= slot '
#        return Expression('Slot', Integer(args[0].value))
#        
#    @parsing
#    def p_slotseq(self, args):
#        ' expr ::= slotseq '
#        return Expression('SlotSequence', Integer(args[0].value))
#    
#    @parsing
#    def p_out(self, args):
#        ' expr ::= out '
#        return Expression('Out', Integer(args[0].value))
#        
#    @parsing
#    def p_string(self, args):
#        ' expr ::= string '
#        return String(args[0].value)
#    
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
#        @parsing_static
#        def p_binary_op(args):
#            return args[0]
#            
#        rule = ''    
#        for operator in binary_operators:
#            rule += u'binary_op ::= %s\n' % operator
#        
#        self.addRule(rule, p_binary_op)
#                
#        GenericParser.collectRules(self)
#                
scanner = MathicsScanner()
scanner.build()
#parser = MathicsParser()
#
def parse(string):
    tokens = scanner.tokenize(string)
    if tokens:
        result = parser.parse(tokens)
        return result
    else:
        return None
