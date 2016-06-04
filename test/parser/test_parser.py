#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import absolute_import
from __future__ import unicode_literals

import sys
import random
import unittest

from mathics.core.parser.errors import ScanError, IncompleteSyntaxError, InvalidSyntaxError
from mathics.core.parser.ast import Node, Symbol, Number, String
from mathics.core.parser.parser import Parser

import six


class ParserTests(unittest.TestCase):
    def setUp(self):
        self.parser = Parser()

    def parse(self, s):
        return self.parser.parse(s)

    def check(self, expr1, expr2):
        if isinstance(expr1, six.string_types):
            expr1 = self.parse(expr1)
        if isinstance(expr2, six.string_types):
            expr2 = self.parse(expr2)

        if expr1 is None:
            self.assertIsNone(expr2)
        else:
            self.assertEqual(expr1, expr2)

    def lex_error(self, string):
        self.assertRaises(ScanError, self.parse, string)

    def incomplete_error(self, string):
        self.assertRaises(IncompleteSyntaxError, self.parse, string)

    def invalid_error(self, string):
        self.assertRaises(InvalidSyntaxError, self.parse, string)


class PrecedenceTests(ParserTests):
    def test_minuslike(self):
        self.check('a * + b', Node('Times', Symbol('a'), Symbol('b')))
        self.check('- a * b', Node('Times', Number(-1), Node('Dot', Symbol('a'), Symbol('b'))))
        self.check('- a / b', Node('Times', Number(-1), Symbol('a'), Node('Power', Symbol('b'), Number(-1))))


class AtomTests(ParserTests):
    def testReal(self):
        self.check('1.5', Real('1.5'))
        self.check('1.5`', Real('1.5'))
        self.check('0.0', Real(0))
        self.check('-1.5`', Real('-1.5'))

        self.check('0.00000000000000000', '0.')
        self.check('0.000000000000000000`', '0.')
        self.check('0.000000000000000000', '0.``18')

    def testSymbol(self):
        self.check('xX', Symbol('Global`xX'))
        self.check('context`name', Symbol('context`name'))
        self.check('`name', Symbol('Global`name'))
        self.check('`context`name', Symbol('Global`context`name'))

    def testAccuracy(self):
        self.lex_error('1.5``')
        self.check('1.0``20', Real('1.0', p=20))

    @unittest.expectedFailure
    def testLowAccuracy(self):
        self.check('1.4``0', Real(0))
        self.check('1.4``-20', Real(0))

    def testPrecision(self):
        self.check('1.`20', Real(1, p=20))
        self.check('1.00000000000000000000000`', Real(1))
        self.check('1.00000000000000000000000`30', Real(1, p=30))

    @unittest.expectedFailure
    def testLowPrecision(self):
        self.check('1.4`1', Real('1', p=1))
        self.check('1.4`0', Real(0, p=0))
        self.check('1.4`-5', Real(0, p=0))

class GeneralTests(ParserTests):
    def testCompound(self):
        self.check('a ; {b}', Node('CompoundExpression', Symbol('a'), Node('List', Symbol('b'))))
        self.check('1 ;', Node('CompoundExpression', Number('1'), Symbol('Null')))
        self.check('1 ; 5', Node('CompoundExpression', Number('1'), Number('5')))
        self.check('4; 1 ; 5', Node('CompoundExpression', Number('4'), Number('1'), Number('5')))
        self.check('4;1;', Node('CompoundExpression', Number('4'), Number('1'), Symbol('Null')))

    def testMessage(self):
        self.check('1 :: "abc"', Node('MessageName', Number('1'), String("abc")))
        self.check('1 :: "abc" :: "123"', Node('MessageName', Number('1'), String("abc"), String("123")))

    def testGetPut(self):
        self.check('<<"filename"', Node('Get', String('filename')))
        self.check('1 >> filename', Node('Put', Number('1'), String('filename')))
        self.check('1 >>> filename', Node('PutAppend', Number('1'), String('filename')))
        self.check('<< filename', Node('Get', String('filename')))

    def testExpression(self):
        self.check('expr1[expr2]', Node('expr1', Symbol('expr2')))
        self.check('expr1[expr2][expr3]', Node(Node('expr1', Symbol('expr2')), Symbol('expr3')))
        self.check('expr1[[expr2]]', Node('Part', Symbol('expr1'), Symbol('expr2')))
        self.check('expr1[[expr2, expr3]]', Node('Part', Symbol('expr1'), Symbol('expr2'), Symbol('expr3')))
        self.check('expr1[[expr2]][[expr3]]', Node('Part', Node('Part', Symbol('expr1'), Symbol('expr2')), Symbol('expr3')))

        self.check('expr1 ~ expr2 ~ expr3', Node('expr2', Symbol('expr1'), Symbol('expr3')))
        self.check('x~f~y', 'f[x, y]')

    def testFunctional(self):
        self.check('expr1 @ expr2', Node('expr1', Symbol('expr2')))
        self.check('f @@ expr', Node('Apply', Symbol('f'), Symbol('expr')))
        self.check('f /@ expr', Node('Map', Symbol('f'), Symbol('expr')))

        self.check('f @@@ expr', Node('Apply', Symbol('f'), Symbol('expr'), Node('List', 1)))
        self.check('f //@ expr', Node('MapAll', Symbol('f'), Symbol('expr')))
        self.check('a @@ b @@ c', Node('Apply', Symbol('a'), Node('Apply', Symbol('b'), Symbol('c'))))

    def testIncDec(self):
        self.check('a++', Node('Increment', Symbol('a')))
        self.check('a--', Node('Decrement', Symbol('a')))
        self.check('++a', Node('PreIncrement', Symbol('a')))
        self.check('--a', Node('PreDecrement', Symbol('a')))

    def testBang(self):
        self.check('5!', Node('Factorial', Number('5')))
        self.check('5 !', Node('Factorial', Number('5')))
        self.check('5 ! !', Node('Factorial', Node('Factorial', Number('5'))))
        self.check('!1', Node('Not', Number('1')))
        self.check('5 !!', Node('Factorial2', Number('5')))
        self.check('x ! y', Node('Times', Node('Factorial', Symbol('x')), Symbol('y')))

    def testDerivative(self):
        self.check("f'", Node(Node('Derivative',   Number('1')), Symbol('f')))
        self.check("f''", Node(Node('Derivative',  Number('2')), Symbol('f')))
        self.check("f' '", Node(Node('Derivative', Number('2')), Symbol('f')))

    def testPlus(self):
        self.check('+1', Number('1'))
        self.check('1 + 2', Node('Plus', Number('1'), Number('2')))
        self.check('1 + 2 + 3', Node('Plus', Number('1'), Number('2'), Number('3')))
        self.check('1 + 2 + 3 + 4', 'Plus[1, 2, 3, 4]')
        self.check('-a', Node('Times', Number(-1), Symbol('a')))
        self.check('1 - 2', Node('Plus', Number('1'), Node('Times', Number(-1), Number('2'))))

        self.check('a*b+c', Node('Plus', Node('Times', Symbol('a'), Symbol('b')), Symbol('c')))
        self.check('a*+b+c', Node('Plus', Node('Times', Symbol('a'), Symbol('b')), Symbol('c')))
        self.check('a+b*c', 'a+(b*c)')
        self.check('a*b+c', '(a*b) + c')

    def testTimes(self):
        self.check('1 2', Node('Times', Number('1'), Number('2')))
        self.check('1*2', Node('Times', Number('1'), Number('2')))

        self.check('1 2 3', Node('Times', Number('1'), Number('2'), Number('3')))
        self.check('1*2*3', Node('Times', Number('1'), Number('2'), Number('3')))

        self.check('x ^ 2 y', Node('Times', Node('Power', Symbol('x'), Number('2')), Symbol('y')))

    def testSpan(self):
        self.check('1;;2;;3', Node('Span', Number('1'), Number('2'), Number('3')))
        self.check('1;; ;;3', Node('Span', Number('1'), Symbol('All'), Number('3')))
        self.check('1;;;;3', Node('Span', Number('1'), Symbol('All'), Number('3')))
        self.check(' ;;2;;3', Node('Span', Number('1'), Number('2'), Number('3')))
        self.check(' ;;2', Node('Span', Number('1'), Number('2')))
        self.check('1;; ', Node('Span', Number('1'), Symbol('All')))
        self.check(' ;; ', Node('Span', Number('1'), Symbol('All')))

    def testBinOp(self):
        self.check('1 <> 2 ', Node('StringJoin', Number('1'), Number('2')))
        self.check('1 <> 2 <> 3', Node('StringJoin', Number('1'), Number('2'), Number('3')))

        self.check('1 ^ 2', Node('Power', Number('1'), Number('2')))
        self.check('1 . 2', Node('Dot', Number('1'), Number('2')))
        self.check('1 && 2', Node('And', Number('1'), Number('2')))
        self.check('1 || 2', Node('Or', Number('1'), Number('2')))

        self.check('x /; y', Node('Condition', Symbol('x'), Symbol('y')))
        self.check('x -> y', Node('Rule', Symbol('x'), Symbol('y')))
        self.check('x :> y', Node('RuleDelayed', Symbol('x'), Symbol('y')))

        self.check('x /. y', Node('ReplaceAll', Symbol('x'), Symbol('y')))
        self.check('x //. y', Node('ReplaceRepeated', Symbol('x'), Symbol('y')))

        self.check('x += y', Node('AddTo', Symbol('x'), Symbol('y')))
        self.check('x -= y', Node('SubtractFrom', Symbol('x'), Symbol('y')))
        self.check('x *= y', Node('TimesBy', Symbol('x'), Symbol('y')))
        self.check('x /= y', Node('DivideBy', Symbol('x'), Symbol('y')))

        self.check('x &', Node('Function', Symbol('x')))

        self.check('x // y', Node('y', Symbol('x')))

        self.check('1 ^ 2 ^ 3', 'Power[1, Power[2, 3]]')
        self.check('3/2', Node('Times', Number('3'), Node('Power', Number('2'), Number(-1))))

        self.check('x ~~ y', Node('StringExpression', Symbol('x'), Symbol('y')))
        self.check('x ~~ y ~~ z', Node('StringExpression', Symbol('x'), Symbol('y'), Symbol('z')))

    def testCompare(self):
        self.check('1 == 2', Node('Equal', Number('1'), Number('2')))
        self.check('1 != 2', Node('Unequal', Number('1'), Number('2')))
        self.check('1 == 2 == 3', Node('Equal', Number('1'), Number('2'), Number('3')))
        self.check('1 != 2 != 3', Node('Unequal', Number('1'), Number('2'), Number('3')))

        self.check('1 > 2', Node('Greater', Number('1'), Number('2')))
        self.check('1 >= 2', Node('GreaterEqual', Number('1'), Number('2')))
        self.check('1 < 2', Node('Less', Number('1'), Number('2')))
        self.check('1 <= 2', Node('LessEqual', Number('1'), Number('2')))

        self.check('1 > 2 > 3', Node('Greater', Number('1'), Number('2'), Number('3')))
        self.check('1 >= 2 >= 3', Node('GreaterEqual', Number('1'), Number('2'), Number('3')))
        self.check('1 < 2 < 3', Node('Less', Number('1'), Number('2'), Number('3')))
        self.check('1 <= 2 <= 3', Node('LessEqual', Number('1'), Number('2'), Number('3')))

        self.check('1 === 2', Node('SameQ', Number('1'), Number('2')))
        self.check('1 =!= 2', Node('UnsameQ', Number('1'), Number('2')))
        self.check('1 === 2 === 3', Node('SameQ', Number('1'), Number('2'), Number('3')))
        self.check('1 =!= 2 =!= 3', Node('UnsameQ', Number('1'), Number('2'), Number('3')))

    def testRepeated(self):
        self.check('1..', Node('Repeated', Number('1')))
        self.check('1...', Node('RepeatedNull', Number('1')))

    def testAlternatives(self):
        self.check('1 | 2', Node('Alternatives', Number('1'), Number('2')))
        self.check('1 | 2 | 3', Node('Alternatives', Number('1'), Number('2'), Number('3')))

    def testSet(self):
        self.check('x = y', Node('Set', Symbol('x'), Symbol('y')))
        self.check('x := y', Node('SetDelayed', Symbol('x'), Symbol('y')))
        self.check('x ^= y', Node('UpSet', Symbol('x'), Symbol('y')))
        self.check('x ^:= y', Node('UpSetDelayed', Symbol('x'), Symbol('y')))
        self.check('x =.', Node('Unset', Symbol('x')))

        self.check('x/:1=1', Node('TagSet', Symbol('x'), Number('1'), Number('1')))
        self.check('x/:1:=1', Node('TagSetDelayed', Symbol('x'), Number('1'), Number('1')))
        # self.check('x/:1=.', Node('TagUnset', Symbol('x'), Number('1')))

    def testList(self):
        self.check('{x, y}', Node('List', Symbol('x'), Symbol('y')))

        self.check('{}', Node('List'))
        self.check('{a,}', Node('List', Symbol('a'), Symbol('Null')))
        self.check('{,}', Node('List', Symbol('Null'), Symbol('Null')))

        self.check('{a, b,}', Node('List', Symbol('a'), Symbol('b'), Symbol('Null')))

        self.check('{,a}', Node('List', Symbol('Null'), Symbol('a')))
        self.check('{, a, b}', Node('List', Symbol('Null'), Symbol('a'), Symbol('b')))
        self.check('{,a,b,}', Node('List', Symbol('Null'), Symbol('a'), Symbol('b'), Symbol('Null')))

    def testSequence(self):
        self.check('Sin[x, y]', Node('Sin', Symbol('x'), Symbol('y')))

    def testPart(self):
        self.check('a[[1]]', Node('Part', Symbol('a'), Number('1')))

    def testSlot(self):
        self.check('#2', Node('Slot', Number('2')))
        self.check('#', Node('Slot', Number('1')))
        self.check('##2', Node('SlotSequence', Number('2')))
        self.check('##', Node('SlotSequence', Number('1')))
        self.check('%2', Node('Out', Number('2')))

    def testOut(self):
        self.check('%%', Node('Out', Number(-2)))
        self.check('%%%%', Node('Out', Number(-4)))
        self.check('%', Node('Out'))

    def testNonAscii(self):
        self.check('z \\[Conjugate]', Node('Conjugate', Symbol('z')))
        self.check('z \\[Transpose]', Node('Transpose', Symbol('z')))
        self.check('z \\[ConjugateTranspose]', Node('ConjugateTranspose', Symbol('z')))
        self.check('z \uf3c7 ', Node('Transpose', Symbol('z')))
        self.check('z \uf3c8 ', Node('Conjugate', Symbol('z')))
        self.check('z \uf3c9 ', Node('ConjugateTranspose', Symbol('z')))
        self.check('\\[Integral] x \\[DifferentialD] x', Node('Integrate', Symbol('x'), Symbol('x')))
        self.check('\\[Del] x', Node('Del', Symbol('x')))
        self.check('\\[Square] x', Node('Square', Symbol('x')))
        self.check('1 \\[SmallCircle] 2', Node('SmallCircle', Number('1'), Number('2')))
        self.check('1 \\[SmallCircle] 2 \\[SmallCircle] 3', Node('SmallCircle', Number('1'), Number('2'), Number('3')))
        self.check('1 \u2218 2', Node('SmallCircle', Number('1'), Number('2')))
        self.check('1 \\[CircleDot] 2', Node('CircleDot', Number('1'), Number('2')))
        self.check('1 \u2299 2', Node('CircleDot', Number('1'), Number('2')))
        self.check('1 \\[Diamond] 2', Node('Diamond', Number('1'), Number('2')))
        self.check('1 \\[Wedge] 2', Node('Wedge', Number('1'), Number('2')))
        self.check('1 \\[Vee] 2', Node('Vee', Number('1'), Number('2')))
        self.check('1 \\[CircleTimes] 2', Node('CircleTimes', Number('1'), Number('2')))
        self.check('1 \\[CenterDot] 2', Node('CenterDot', Number('1'), Number('2')))
        self.check('1 \\[Star] 2', Node('Star', Number('1'), Number('2')))
        self.check('a \\[Cap] b', 'Cap[a,b]')
        self.check('a \\[Cup] b \\[Cup] c', 'Cup[a,b,c]')
        self.check('a \u2322 b \u2322 c', 'Cap[a,b,c]')
        self.check('a \u2323 b', 'Cup[a, b]')
        self.check('1 \u22C4 2', Node('Diamond', Number('1'), Number('2')))
        self.check('1 \u22C0 2', Node('Wedge', Number('1'), Number('2')))
        self.check('1 \u22c1 2', Node('Vee', Number('1'), Number('2')))
        self.check('1 \u2297 2', Node('CircleTimes', Number('1'), Number('2')))
        self.check('1 \u00B7 2', Node('CenterDot', Number('1'), Number('2')))
        self.check('1 \u22C6 2', Node('Star', Number('1'), Number('2')))
        self.check('expr1 ** expr2', Node('NonCommutativeMultiply', Symbol('expr1'), Symbol('expr2')))
        self.check('expr1 ** expr2 ** expr3', Node('NonCommutativeMultiply', Symbol('expr1'), Symbol('expr2'), Symbol('expr3')))
        self.check('1 \\[Cross] 2', Node('Cross', Number('1'), Number('2')))
        self.check('1 \uf4a0 2', Node('Cross', Number('1'), Number('2')))
        self.check('3\\[Divide]2', Node('Times', Number('3'), Node('Power', Number('2'), Number(-1))))
        self.check('3 \u00f7 2', Node('Times', Number('3'), Node('Power', Number('2'), Number(-1))))
        self.check('3\\2', Node('Backslash', Number('3'), Number('2')))
        self.check('1 \\[Times] 2', Node('Times', Number('1'), Number('2')))
        self.check('1 \u00d7 2', Node('Times', Number('1'), Number('2')))
        self.check('1 \[PlusMinus] 2', Node('PlusMinus', Number('1'), Number('2')))
        self.check('1 \[MinusPlus] 2', Node('MinusPlus', Number('1'), Number('2')))
        self.check('\[PlusMinus] 1', Node('PlusMinus', Number('1')))
        self.check('\[MinusPlus] 1', Node('MinusPlus', Number('1')))
        self.check('\u00b1 1', Node('PlusMinus', Number('1')))
        self.check('\u2213 1', Node('MinusPlus', Number('1')))
        self.check('1 \\[And] 2', Node('And', Number('1'), Number('2')))
        self.check('1 \u2227 2', Node('And', Number('1'), Number('2')))
        self.check('1 \\[Or] 2', Node('Or', Number('1'), Number('2')))
        self.check('1 \u2228 2', Node('Or', Number('1'), Number('2')))

        self.check('a \\[Colon] b', Node('Colon', Symbol('a'), Symbol('b')))
        self.check('a \u2236 b', Node('Colon', Symbol('a'), Symbol('b')))
        self.check('x \\[Function] y', 'Function[{x}, y]')
        self.check('x \uf4a1 y', 'Function[{x}, y]')

        self.check('x1 \\[RightTee] x2', 'RightTee[x1, x2]')
        self.check('x1 \\[DoubleRightTee] x2', 'DoubleRightTee[x1, x2]')
        self.check('x1 \\[LeftTee] x2', 'LeftTee[x1, x2]')
        self.check('x1 \\[DoubleLeftTee] x2', 'DoubleLeftTee[x1, x2]')

    @unittest.expectedFailure
    def testBoxes(self):
        self.check('\\(1 \\^ 2\\)', 'SuperscriptBox["1", "2"]')
        self.check('\\(1 \\^ 2 \\% 3\\)', 'SubsuperscriptBox["1", "3", "2"]')
        self.check('\\(1 \\_ 2\\)', 'SubscriptBox["1", "2"]')
        self.check('\\(1 \\_ 2 \\% 3\\)', 'SubsuperscriptBox["1", "2", "3"]')

        self.check('\\( 1 \\& 2 \\)', 'OverscriptBox["1", "2"]')
        self.check('\\( 1 \\& 2 \\% 3 \\)', 'UnderoverscriptBox["1", "3", "2"]')
        self.check('\\( 1 \\+ 2 \\)', 'UnderscriptBox["1", "2"]')
        self.check('\\( 1 \\+ 2 \\% 3 \\)', 'UnderoverscriptBox["1", "2", "3"]')

        self.check('\\( 1 \\/ 2 \\)', 'FractionBox["1", "2"]')

        self.check('\\( \\@ 2 \\)', 'SqrtBox["2"]')
        self.check('\\( \\@ 2 \\% 3 \\)', 'RadicalBox["2", "3"]')

        self.check('\\( 1 \\` 2 \\)', 'FormBox["2", Removed["$$Failure"]]')
        self.check('\\( FullForm \\` 2 \\)', 'FormBox["2", FullForm]')

        self.check('\\( \\)', String(""))
        self.check('\\( a \\)', String("a"))
        self.check('\\( \\@ 1 \\_ 2 \\)', 'SqrtBox[SubscriptBox["1", "2"]]')
        self.check('\\( a + b \\)', 'RowBox[List["a", "+", "b"]]')
        self.check('\\(1 \\` 2\\)', Node('FormBox', Number('2'), Number('1')))

class PatternTests(ParserTests):
    def testPattern(self):
        self.check('Map[f_, expr_, ls_?LevelQ:{1}, OptionsPattern[Map]]', 'Map[Pattern[f, Blank[]], Pattern[expr, Blank[]], PatternTest[Pattern[ls, Blank[]], Pattern[LevelQ, List[1]]], OptionsPattern[Map]]')
        self.check('_^_?t', 'Power[Blank[], PatternTest[Blank[], t]]')
        self.check('-Sin[x]', 'Times[-1, Sin[x]]')
        self.check('a[x_] := x^2', 'SetDelayed[a[Pattern[x, Blank[]]], Power[x, 2]]')
        self.check('MakeBoxes[expr_, f:TraditionalForm|StandardForm|OutputForm|InputForm|FullForm]', 'MakeBoxes[Pattern[expr, Blank[]], Pattern[f, Alternatives[TraditionalForm, StandardForm, OutputForm, InputForm, FullForm]]]')

        self.check('1?2', Node('PatternTest', Number('1'), Number('2')))
        self.invalid_error('a?b?c')

        self.check('x:expr', Node('Pattern', Symbol('x'), Symbol('expr')))
        self.check('x_:expr', Node('Optional', Node('Pattern', Symbol('x'), Node('Blank')), Symbol('expr')))
        self.check('f:a|b', Node('Pattern', Symbol('f'), Node('Alternatives', Symbol('a'), Symbol('b'))))

    def testBlank(self):
        self.check('f_', Node('Pattern', Symbol('f'), Node('Blank')))
        self.check('f__', Node('Pattern', Symbol('f'), Node('BlankSequence')))
        self.check('f___', Node('Pattern', Symbol('f'), Node('BlankNullSequence')))

        self.check('_', 'Blank[]')
        self.check('_expr', 'Blank[expr]')
        self.check('__', 'BlankSequence[]')
        self.check('__expr', 'BlankSequence[expr]')
        self.check('___', 'BlankNullSequence[]')
        self.check('___expr', 'BlankNullSequence[expr]')

        self.check('_.', 'Optional[Blank[]]')
        self.check('symb_', 'Pattern[symb, Blank[]]')
        self.check('symb_expr', 'Pattern[symb, Blank[expr]]')
        self.check('symb__', 'Pattern[symb, BlankSequence[]]')
        self.check('symb__expr', 'Pattern[symb, BlankSequence[expr]]')
        self.check('symb___', 'Pattern[symb, BlankNullSequence[]]')
        self.check('symb___expr', 'Pattern[symb, BlankNullSequence[expr]]')
        self.check('symb_.', 'Optional[Pattern[symb, Blank[]]]')


class IncompleteTests(ParserTests):
    def testParseError(self):
        self.incomplete_error('1+')

    def testBracketInvalid(self):
        self.invalid_error('x)')      # bktmop
        self.invalid_error('x]')      # bktmop
        self.invalid_error('x}')      # bktmop
        self.invalid_error('x]]')     # bktmop

    def testBracketIncomplete(self):
        self.incomplete_error('(x')     # bktmcp
        self.incomplete_error('f[x')    # bktmcp
        self.incomplete_error('{x')     # bktmcp
        self.incomplete_error('f[[x')   # bktmcp

    def testBracketIncompleteInvalid(self):
        self.invalid_error('(x,')
        self.incomplete_error('(x')
        self.invalid_error('[x')
        self.incomplete_error('{x')
        self.invalid_error('[[x')

    def test_trailing_backslash(self):
        self.incomplete_error('x \\')


class CommentTests(ParserTests):
    def testComment(self):
        self.check('145 (* abf *) 345', Node('Times', Number('145'), Number('345')))
        self.check(r'(*"\"\*)', None)
        self.check(r'(**)', None)
        self.check(r'(*)*)', None)
        self.incomplete_error(r'(*(*(*')
        self.incomplete_error(r'(*(*)')
        self.incomplete_error(r'(*(**)')
        self.invalid_error(r'*)')
        self.invalid_error(r'(**)*)')
        self.invalid_error(r'(*(*(**)*)*)*)')
        self.check(r'(*(*)*) (*)*)*)', None)

    def testNone(self):
        self.assertIs(self.parse(''), None)
        self.assertIs(self.parse('(*fdasf *)'), None)


if __name__ == "__main__":
    unittest.main()
