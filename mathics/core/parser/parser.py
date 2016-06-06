#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import absolute_import
from __future__ import unicode_literals

from mathics.core.parser.ast import Node, Number, Symbol, String, Filename
from mathics.core.parser.tokeniser import Tokeniser
from mathics.core.parser.errors import (
    InvalidSyntaxError, IncompleteSyntaxError, TranslateError)
from mathics.core.parser.operators import (
    prefix_ops, postfix_ops, left_binary_ops, right_binary_ops,
    nonassoc_binary_ops, flat_binary_ops, ternary_ops, binary_ops, all_ops)


class Parser(object):
    def parse(self, code):
        self.tokeniser = Tokeniser(code)
        self.current_token = None
        if self.next().tag == 'END':
            return None
        return self.parse_e()

    def next(self):
        if self.current_token is None:
            self.current_token = self.tokeniser.next()
        return self.current_token

    def consume(self):
        self.current_token = None

    def expect(self, expected_tag):
        token = self.next()
        tag = token.tag
        if tag == expected_tag:
            self.consume()
        else:
            if tag == 'END':
                raise IncompleteSyntaxError(token.pos)
            raise InvalidSyntaxError(token.pos)

    def next_filename(self):
        token = self.tokeniser.next_filename()
        tag = token.tag
        if tag == 'filename':
            return Filename(token.text)
        else:
            if tag == 'END':
                raise IncompleteSyntaxError(token.pos)
            raise InvalidSyntaxError(token.pos)

    def backtrack(self, pos):
        self.tokeniser.pos = pos
        self.current_token = None

    def parse_e(self):
        result = self.parse_exp(0)
        self.expect('END')
        return result

    def parse_exp(self, p):
        result = self.parse_p()
        while True:
            token = self.next()
            tag = token.tag
            method = getattr(self, 'e_' + tag, None)
            if method is not None:
                expr = method(result, token, p)
                if expr is None:
                    break
                result = expr
            elif tag in binary_ops:
                q = binary_ops[tag]
                if q < p:
                    break
                self.consume()
                if tag not in right_binary_ops:
                    q += 1
                child = self.parse_exp(q)
                # flatten or associate
                if tag in nonassoc_binary_ops and result.get_head_name() == tag and not result.parenthesised:
                    raise InvalidSyntaxError(token.pos)
                result = Node(tag, result, child)
                if tag in flat_binary_ops:
                    result.flatten()
            elif tag in ternary_ops:
                if ternary_ops[tag] < p:
                    break
                self.consume()
                q = ternary_ops[tag] + 1
                child1 = self.parse_exp(q)
                self.expect(tag)
                child2 = self.parse_exp(q)
                result = Node(tag, result, child1, child2)
            elif tag in postfix_ops:
                if postfix_ops[tag] < p:
                    break
                self.consume()
                q = postfix_ops[tag]
                result = Node(tag, result)
            elif tag not in ('END', 'RawRightParenthesis', 'RawComma', 'RawRightBrace', 'RawRightBracket', 'RawColon', 'DifferentialD') and flat_binary_ops['Times'] >= p:  # implicit times
                q = flat_binary_ops['Times']
                child = self.parse_exp(q + 1)
                if result.get_head_name() == 'Times' and not result.parenthesised:
                    result.children.append(child)
                else:
                    result = Node('Times', result, child)
            else:
                break
        return result

    def parse_p(self):
        token = self.next()
        tag = token.tag
        method = getattr(self, 'p_' + tag, None)

        if method is not None:
            return method(token)
        elif tag in prefix_ops:
            self.consume()
            q = prefix_ops[tag]
            child = self.parse_exp(q)
            return Node(tag, child)
        else:
            raise InvalidSyntaxError(token.pos)

    def parse_seq(self):
        result = []
        while True:
            token = self.next()
            tag = token.tag
            if tag == 'RawComma':
                # TODO message Syntax:com
                result.append(Symbol('Null'))
                self.consume()
            elif tag in ('END', 'RawRightBrace', 'RawRightBracket'):
                if result:
                    # TODO message Syntax:com
                    result.append(Symbol('Null'))
                break
            else:
                result.append(self.parse_exp(0))
                token = self.next()
                tag = token.tag
                if tag == 'RawComma':
                    self.consume()
                    continue
                elif tag in ('END', 'RawRightBrace', 'RawRightBracket'):
                    break
        return result

    # P methods
    #
    # p_xxx methods are called from parse_p.
    # Called with one Token and return a Node.
    # Used for prefix operators and brackets.

    def p_END(self, token):
        raise IncompleteSyntaxError(token.pos)

    def p_Factorial(self, token):
        self.consume()
        q = prefix_ops['Not']
        child = self.parse_exp(q)
        return Node('Not', child)

    def p_RawLeftParenthesis(self, token):
        self.consume()
        result = self.parse_exp(0)
        self.expect('RawRightParenthesis')
        result.parenthesised = True
        return result

    def p_RawLeftBrace(self, token):
        self.consume()
        seq = self.parse_seq()
        self.expect('RawRightBrace')
        return Node('List', *seq)

    def p_Number(self, token):
        result = Number(token.text)
        self.consume()
        return result

    def p_String(self, token):
        result = String(token.text[1:-1])
        self.consume()
        return result

    def p_Symbol(self, token):
        result = Symbol(token.text)
        self.consume()
        return result

    def p_Span(self, token):
        return self.e_Span(Number('1'), token, 0)

    def p_Integral(self, token):
        self.consume()
        inner_prec, outer_prec = all_ops['Sum'] + 1, all_ops['Power'] - 1
        expr1 = self.parse_exp(inner_prec)
        self.expect('DifferentialD')
        expr2 = self.parse_exp(outer_prec)
        return Node('Integrate', expr1, expr2)

    def p_Get(self, token):
        self.consume()
        return Node('Get', self.next_filename())

    def p_Pattern(self, token):
        self.consume()
        text = token.text
        if '.' in text:
            name = text[:-2]
            if name:
                return Node('Optional', Node('Pattern', Symbol(name), Node('Blank')))
            else:
                return Node('Optional', Node('Blank'))
        pieces = text.split('_')
        count = len(pieces) - 1
        if count == 1:
            name = 'Blank'
        elif count == 2:
            name = 'BlankSequence'
        elif count == 3:
            name = 'BlankNullSequence'
        if pieces[-1]:
            blank = Node(name, Symbol(pieces[-1]))
        else:
            blank = Node(name)
        if pieces[0]:
            return Node('Pattern', Symbol(pieces[0]), blank)
        else:
            return blank

    def p_Minus(self, token):
        self.consume()
        q = prefix_ops['Minus']
        return Node('Times', Number('-1'), self.parse_exp(q)).flatten()

    def p_Plus(self, token):
        self.consume()
        q = prefix_ops['Minus']
        return Node('Plus', self.parse_exp(q))

    def p_PlusMinus(self, token):
        self.consume()
        q = prefix_ops['Minus']
        return Node('PlusMinus', self.parse_exp(q))

    def p_MinusPlus(self, token):
        self.consume()
        q = prefix_ops['Minus']
        return Node('MinusPlus', self.parse_exp(q))

    def p_Out(self, token):
        self.consume()
        text = token.text
        if text == '%':
            return Node('Out')
        if text.endswith('%'):
            n = str(-len(text))
        else:
            n = text[1:]
        return Node('Out', Number(n))

    def p_Slot(self, token):
        self.consume()
        text = token.text
        if len(text) == 1:
            n = '1'
        else:
            n = text[1:]
        return Node('Slot', Number(n))

    def p_SlotSequence(self, token):
        self.consume()
        text = token.text
        if len(text) == 2:
            n = '1'
        else:
            n = text[2:]
        return Node('SlotSequence', Number(n))

    def p_Increment(self, token):
        self.consume()
        q = prefix_ops['PreIncrement']
        return Node('PreIncrement', self.parse_exp(q))

    def p_Decrement(self, token):
        self.consume()
        q = prefix_ops['PreDecrement']
        return Node('PreDecrement', self.parse_exp(q))

    # E methods
    #
    # e_xxx methods are called from parse_e.
    # They expect args (Node, Token precedence) and return Node or None.
    # Used for binary and ternary operators.
    # return None if precedence is too low.

    def e_Span(self, expr1, token, p):
        q = ternary_ops['Span']
        if q < p:
            return None

        self.consume()

        # Span[expr1, expr2]
        pos = self.tokeniser.pos
        if self.next().tag in ('Span', 'END'):
            expr2 = Symbol('All')
        else:
            expr2 = self.parse_exp(q + 1)
        if all_ops[expr2.get_head_name()] < ternary_ops['Span']:
            self.backtrack(pos)
            return Node('Span', expr1, Symbol('All'))
        token = self.next()
        if token.tag != 'Span':
            return Node('Span', expr1, expr2)

        # Span[expr1, expr2, expr3]
        pos = self.tokeniser.pos
        self.consume()
        expr3 = self.parse_exp(q + 1)
        if all_ops[expr3.get_head_name()] < ternary_ops['Span']:
            self.backtrack(pos)
            return Node('Span', expr1, expr2)
        return Node('Span', expr1, expr2, expr3)

    def e_RawLeftBracket(self, expr, token, p):
        self.consume()
        token = self.next()
        if token.tag == 'RawLeftBracket':
            self.consume()
            seq = self.parse_seq()
            self.expect('RawRightBracket')
            self.expect('RawRightBracket')
            return Node('Part', expr, *seq)
        else:
            seq = self.parse_seq()
            self.expect('RawRightBracket')
            result = Node(expr, *seq)
            result.parenthesised = True
            return result

    def e_Infix(self, expr1, token, p):
        q = ternary_ops['Infix']
        if q < p:
            return None
        self.consume()
        expr2 = self.parse_exp(q + 1)
        self.expect('Infix')
        expr3 = self.parse_exp(q + 1)
        return Node(expr2, expr1, expr3)

    def e_Postfix(self, expr1, token, p):
        q = 0
        if q < p:
            return None
        self.consume()
        # postix has lowest prec and is left assoc
        expr2 = self.parse_exp(q + 1)
        return Node(expr2, expr1)

    def e_Prefix(self, expr1, token, p):
        q = 640
        if 640 < p:
            return None
        self.consume()
        expr2 = self.parse_exp(q)
        return Node(expr1, expr2)

    def e_ApplyList(self, expr1, token, p):
        q = right_binary_ops['Apply']
        if q < p:
            return None
        self.consume()
        expr2 = self.parse_exp(q)
        expr3 = Node('List', Number('1'))
        return Node('Apply', expr1, expr2, expr3)

    def e_Function(self, expr1, token, p):
        q = postfix_ops['Function']
        if q < p:
            return None
        # postfix or right-binary determined by symbol
        self.consume()
        if token.text == '&':
            return Node('Function', expr1)
        else:
            expr2 = self.parse_exp(q + 1)
            return Node('Function', expr1, expr2)

    def e_RawColon(self, expr1, token, p):
        '''
        Pattern  : patt <- symb ':' expr
        Optional : patt <- patt ':' expr
        '''
        if expr1.get_head_name() == 'Symbol':
            head = 'Pattern'
        elif expr1.get_head_name() in ('Blank', 'BlankSequence', 'BlankNullSequence', 'Optional', 'Pattern'):
            head = 'Optional'
        else:
            raise InvalidSyntaxError(token.pos)
        q = all_ops[head] + 1000    # HACK: add 1000 to ensure patterns are parsed first
        if q < p:
            return None
        self.consume()
        expr2 = self.parse_exp(q + 1)
        if expr1.get_head_name() == head == 'Optional' and not expr1.parenthesised:
            expr1.children.append(expr2)
            return expr1
        return Node(head, expr1, expr2)

    def e_Semicolon(self, expr1, token, p):
        q = flat_binary_ops['CompoundExpression']
        if q < p:
            return None
        self.consume()

        # XXX look for next expr otherwise backtrack
        pos = self.tokeniser.pos
        try:
            expr2 = self.parse_exp(q + 1)
        except TranslateError:
            self.backtrack(pos)
            expr2 = Symbol('Null')
        return Node('CompoundExpression', expr1, expr2).flatten()

    def e_Put(self, expr1, token, p):
        q = left_binary_ops['Put']
        if q < p:
            return None
        self.consume()
        return Node('Put', expr1, self.next_filename())

    def e_PutAppend(self, expr1, token, p):
        q = left_binary_ops['PutAppend']
        if q < p:
            return None
        self.consume()
        return Node('PutAppend', expr1, self.next_filename())

    def e_Minus(self, expr1, token, p):
        q = left_binary_ops['Subtract']
        if q < p:
            return None
        self.consume()
        expr2 = self.parse_exp(q + 1)
        return Node('Subtract', expr1, expr2)

    def e_TagSet(self, expr1, token, p):
        q = all_ops['TagSet']
        if q < p:
            return None
        self.consume()
        expr2 = self.parse_exp(q)

        # examine next token
        token = self.next()
        tag = token.tag
        if tag == 'Set':
            head = 'TagSet'
        elif tag == 'SetDelayed':
            head = 'TagSetDelayed'
        else:
            if head == 'END':
                raise IncompleteSyntaxError(token.pos)
            raise InvalidSyntaxError(token.pos)
        self.consume()

        expr3 = self.parse_exp(q)
        return Node(head, expr1, expr2, expr3)

    def e_Derivative(self, expr1, token, p):
        q = postfix_ops['Derivative']
        if q < p:
            return None
        n = 0
        while self.next().tag == 'Derivative':
            self.consume()
            n += 1
        head = Node('Derivative', Number(str(n)))
        return Node(head, expr1)

    def e_Divide(self, expr1, token, p):
        q = left_binary_ops['Divide']
        if q < p:
            return None
        self.consume()
        expr2 = self.parse_exp(q + 1)
        return Node('Times', expr1, Node('Power', expr2, Number('-1'))).flatten()
