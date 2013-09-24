import unittest2 as unittest
import sys
import random
from mathics import *


class ParserTests(unittest.TestCase):
    def check(self, expr1, expr2):
        if isinstance(expr1, basestring):
            expr1 = parse(expr1)
        if isinstance(expr2, basestring):
            expr2 = parse(expr2)

        self.assertTrue(expr1.same(expr2))

    def lex_error(self, string):
        self.assertRaises(ScanError, parse, string)

    def parse_error(self, string):
        self.assertRaises(ParseError, parse, string)


class NumberTests(ParserTests):
    def testReal(self):
        self.check('1.5', Real('1.5'))
        self.check('1.5`', Real('1.5'))
        self.check('0.0', Real(0))
        self.check('-1.5`', Real('-1.5'))

        self.check('0.00000000000000000', '0.')
        self.check('0.000000000000000000`', '0.')
        self.check('0.000000000000000000', '0.``18')

    def testSymbol(self):
        self.check('xX', Symbol('xX'))
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

    def testInteger(self):
        self.check('0', Integer(0))
        self.check('1', Integer(1))
        self.check('-1', Integer(-1))

        self.check('8^^23', Integer(19))
        self.check('10*^3', Integer(10000))
        self.check('10*^-3', Rational(1, 100))
        self.check('8^^23*^2', Integer(1216))

        n = random.randint(-sys.maxint, sys.maxint)
        self.check(str(n), Integer(n))

        n = random.randint(sys.maxint, sys.maxint * sys.maxint)
        self.check(str(n), Integer(n))

    def testPattern(self):
        self.check('Map[f_, expr_, ls_?LevelQ:{1}, OptionsPattern[Map]]', 'Map[Pattern[f, Blank[]], Pattern[expr, Blank[]], PatternTest[Pattern[ls, Blank[]], Pattern[LevelQ, List[1]]], OptionsPattern[Map]]')
        self.check('_^_?t', 'Power[Blank[], PatternTest[Blank[], t]]')
        self.check('-Sin[x]', 'Times[-1, Sin[x]]')
        self.check('a[x_] := x^2', 'SetDelayed[a[Pattern[x, Blank[]]], Power[x, 2]]')
        self.check('MakeBoxes[expr_, f:TraditionalForm|StandardForm|OutputForm|InputForm|FullForm]', 'MakeBoxes[Pattern[expr, Blank[]], Pattern[f, Alternatives[TraditionalForm, StandardForm, OutputForm, InputForm, FullForm]]]')

        self.check('1?2', Expression('PatternTest', Integer(1), Integer(2)))

        self.check('x:expr', Expression('Pattern', Symbol('x'), Symbol('expr')))
        self.check('x_:expr', Expression('Optional', Expression('Pattern', Symbol('x'), Expression('Blank')), Symbol('expr')))
        self.check('f:a|b', Expression('Pattern', Symbol('f'), Expression('Alternatives', Symbol('a'), Symbol('b'))))

    def testCompound(self):
        self.check('a ; {b}', Expression('CompoundExpression', Symbol('a'), Expression('List', Symbol('b'))))
        self.check('1 ;', Expression('CompoundExpression', Integer(1), Symbol('Null')))
        self.check('1 ; 5', Expression('CompoundExpression', Integer(1), Integer(5)))
        self.check('4; 1 ; 5', Expression('CompoundExpression', Integer(4), Integer(1), Integer(5)))
        self.check('4;1;', Expression('CompoundExpression', Integer(4), Integer(1), Symbol('Null')))

    def testComment(self):
        self.check('145 (* abf *) 345', Expression('Times', Integer(145), Integer(345)))

    def testNone(self):
        self.assertIs(parse(''), None)
        self.assertIs(parse('(*fdasf *)'), None)

    def testMessage(self):
        self.check('1 :: "abc"', Expression('MessageName', Integer(1), String("abc")))
        # self.check('1 :: "abc" :: "123"', Expression('MessageName', Integer(1), String("abc"), String("123")))

    def testGetPut(self):
        self.check('<<"filename"', Expression('Get', String('filename')))
        self.check('1 >> filename', Expression('Put', Integer(1), String('filename')))
        self.check('1 >>> filename', Expression('PutAppend', Integer(1), String('filename')))
        self.check('<< filename', Expression('Get', String('filename')))

    def testExpression(self):
        self.check('expr1[expr2]', Expression('expr1', Symbol('expr2')))
        self.check('expr1[expr2][expr3]', Expression(Expression('expr1', Symbol('expr2')), Symbol('expr3')))
        self.check('expr1[[expr2]]', Expression('Part', Symbol('expr1'), Symbol('expr2')))
        self.check('expr1[[expr2, expr3]]', Expression('Part', Symbol('expr1'), Symbol('expr2'), Symbol('expr3')))
        self.check('expr1[[expr2]][[expr3]]', Expression('Part', Expression('Part', Symbol('expr1'), Symbol('expr2')), Symbol('expr3')))

        self.check('expr1 ~ expr2 ~ expr3', Expression('expr2', Symbol('expr1'), Symbol('expr3')))
        self.check('x~f~y', 'f[x, y]')

    def testFunctional(self):
        self.check('expr1 @ expr2', Expression('expr1', Symbol('expr2')))
        self.check('f @@ expr', Expression('Apply', Symbol('f'), Symbol('expr')))
        self.check('f /@ expr', Expression('Map', Symbol('f'), Symbol('expr')))

        self.check('f @@@ expr', Expression('Apply', Symbol('f'), Symbol('expr'), Expression('List', 1)))
        self.check('f //@ expr', Expression('MapAll', Symbol('f'), Symbol('expr')))
        self.check('a @@ b @@ c', Expression('Apply', Symbol('a'), Expression('Apply', Symbol('b'), Symbol('c'))))

    def testIncDec(self):
        self.check('a++', Expression('Increment', Symbol('a')))
        self.check('a--', Expression('Decrement', Symbol('a')))
        self.check('++a', Expression('PreIncrement', Symbol('a')))
        self.check('--a', Expression('PreDecrement', Symbol('a')))

    def testBang(self):
        self.check('5!', Expression('Factorial', Integer(5)))
        self.check('5 !', Expression('Factorial', Integer(5)))
        self.check('5 ! !', Expression('Factorial', Expression('Factorial', Integer(5))))
        self.check('!1', Expression('Not', Integer(1)))
        self.check('5 !!', Expression('Factorial2', Integer(5)))
        self.check('x ! y', Expression('Times', Expression('Factorial', Symbol('x')), Symbol('y')))

    def testDerivative(self):
        self.check("f'", Expression(Expression('Derivative', Integer(1)), Symbol('f')))
        self.check("f''", Expression(Expression('Derivative', Integer(2)), Symbol('f')))
        self.check("f' '", Expression(Expression('Derivative', Integer(2)), Symbol('f')))

    def testPlus(self):
        self.check('+1', Integer(1))
        self.check('1 + 2', Expression('Plus', Integer(1), Integer(2)))
        self.check('1 + 2 + 3', Expression('Plus', Integer(1), Integer(2), Integer(3)))
        self.check('1 + 2 + 3 + 4', 'Plus[1, 2, 3, 4]')
        self.check('-a', Expression('Times', Integer(-1), Symbol('a')))
        self.check('1 - 2', Expression('Plus', Integer(1), Expression('Times', Integer(-1), Integer(2))))

        self.check('a*b+c', Expression('Plus', Expression('Times', Symbol('a'), Symbol('b')), Symbol('c')))
        self.check('a*+b+c', Expression('Plus', Expression('Times', Symbol('a'), Symbol('b')), Symbol('c')))
        self.check('a+b*c', 'a+(b*c)')
        self.check('a*b+c', '(a*b) + c')

    def testTimes(self):
        self.check('1 2', Expression('Times', Integer(1), Integer(2)))
        self.check('1*2', Expression('Times', Integer(1), Integer(2)))

        self.check('1 2 3', Expression('Times', Integer(1), Integer(2), Integer(3)))
        self.check('1*2*3', Expression('Times', Integer(1), Integer(2), Integer(3)))

        self.check('x ^ 2 y', Expression('Times', Expression('Power', Symbol('x'), Integer(2)), Symbol('y')))

    def testSpan(self):
        self.check('1;;2;;3', Expression('Span', Integer(1), Integer(2), Integer(3)))
        self.check('1;; ;;3', Expression('Span', Integer(1), Symbol('All'), Integer(3)))
        self.check('1;;;;3', Expression('Span', Integer(1), Symbol('All'), Integer(3)))
        self.check(' ;;2;;3', Expression('Span', Integer(1), Integer(2), Integer(3)))
        self.check(' ;;2', Expression('Span', Integer(1), Integer(2)))
        self.check('1;; ', Expression('Span', Integer(1), Symbol('All')))
        self.check(' ;; ', Expression('Span', Integer(1), Symbol('All')))

    def testBinOp(self):
        self.check('1 <> 2 ', Expression('StringJoin', Integer(1), Integer(2)))
        self.check('1 <> 2 <> 3', Expression('StringJoin', Integer(1), Integer(2), Integer(3)))

        self.check('1 ^ 2', Expression('Power', Integer(1), Integer(2)))
        self.check('1 . 2', Expression('Dot', Integer(1), Integer(2)))
        self.check('1 && 2', Expression('And', Integer(1), Integer(2)))
        self.check('1 || 2', Expression('Or', Integer(1), Integer(2)))

        self.check('x /; y', Expression('Condition', Symbol('x'), Symbol('y')))
        self.check('x -> y', Expression('Rule', Symbol('x'), Symbol('y')))
        self.check('x :> y', Expression('RuleDelayed', Symbol('x'), Symbol('y')))

        self.check('x /. y', Expression('ReplaceAll', Symbol('x'), Symbol('y')))
        self.check('x //. y', Expression('ReplaceRepeated', Symbol('x'), Symbol('y')))

        self.check('x += y', Expression('AddTo', Symbol('x'), Symbol('y')))
        self.check('x -= y', Expression('SubtractFrom', Symbol('x'), Symbol('y')))
        self.check('x *= y', Expression('TimesBy', Symbol('x'), Symbol('y')))
        self.check('x /= y', Expression('DivideBy', Symbol('x'), Symbol('y')))

        self.check('x &', Expression('Function', Symbol('x')))

        self.check('x // y', Expression('y', Symbol('x')))

        self.check('1 ^ 2 ^ 3', 'Power[1, Power[2, 3]]')
        self.check('3/2', Expression('Times', Integer(3), Expression('Power', Integer(2), Integer(-1))))

        self.check('x ~~ y', Expression('StringExpression', Symbol('x'), Symbol('y')))
        self.check('x ~~ y ~~ z', Expression('StringExpression', Symbol('x'), Symbol('y'), Symbol('z')))

    def testCompare(self):
        self.check('1 == 2', Expression('Equal', Integer(1), Integer(2)))
        self.check('1 != 2', Expression('Unequal', Integer(1), Integer(2)))
        self.check('1 == 2 == 3', Expression('Equal', Integer(1), Integer(2), Integer(3)))
        self.check('1 != 2 != 3', Expression('Unequal', Integer(1), Integer(2), Integer(3)))

        self.check('1 > 2', Expression('Greater', Integer(1), Integer(2)))
        self.check('1 >= 2', Expression('GreaterEqual', Integer(1), Integer(2)))
        self.check('1 < 2', Expression('Less', Integer(1), Integer(2)))
        self.check('1 <= 2', Expression('LessEqual', Integer(1), Integer(2)))

        self.check('1 > 2 > 3', Expression('Greater', Integer(1), Integer(2), Integer(3)))
        self.check('1 >= 2 >= 3', Expression('GreaterEqual', Integer(1), Integer(2), Integer(3)))
        self.check('1 < 2 < 3', Expression('Less', Integer(1), Integer(2), Integer(3)))
        self.check('1 <= 2 <= 3', Expression('LessEqual', Integer(1), Integer(2), Integer(3)))

        self.check('1 === 2', Expression('SameQ', Integer(1), Integer(2)))
        self.check('1 =!= 2', Expression('UnsameQ', Integer(1), Integer(2)))
        self.check('1 === 2 === 3', Expression('SameQ', Integer(1), Integer(2), Integer(3)))
        self.check('1 =!= 2 =!= 3', Expression('UnsameQ', Integer(1), Integer(2), Integer(3)))

    def testRepeated(self):
        self.check('1..', Expression('Repeated', Integer(1)))
        self.check('1...', Expression('RepeatedNull', Integer(1)))

    def testAlternatives(self):
        self.check('1 | 2', Expression('Alternatives', Integer(1), Integer(2)))
        self.check('1 | 2 | 3', Expression('Alternatives', Integer(1), Integer(2), Integer(3)))

    def testSet(self):
        self.check('x = y', Expression('Set', Symbol('x'), Symbol('y')))
        self.check('x := y', Expression('SetDelayed', Symbol('x'), Symbol('y')))
        self.check('x ^= y', Expression('UpSet', Symbol('x'), Symbol('y')))
        self.check('x ^:= y', Expression('UpSetDelayed', Symbol('x'), Symbol('y')))
        self.check('x =.', Expression('Unset', Symbol('x')))

        self.check('x/:1=1', Expression('TagSet', Symbol('x'), Integer(1), Integer(1)))
        self.check('x/:1:=1', Expression('TagSetDelayed', Symbol('x'), Integer(1), Integer(1)))
        # self.check('x/:1=.', Expression('TagUnset', Symbol('x'), Integer(1)))

    def testList(self):
        self.check('{x, y}', Expression('List', Symbol('x'), Symbol('y')))

        self.check('{}', Expression('List'))
        self.check('{a,}', Expression('List', Symbol('a'), Symbol('Null')))
        self.check('{,}', Expression('List', Symbol('Null'), Symbol('Null')))

        self.check('{a, b,}', Expression('List', Symbol('a'), Symbol('b'), Symbol('Null')))

        self.check('{,a}', Expression('List', Symbol('Null'), Symbol('a')))
        self.check('{, a, b}', Expression('List', Symbol('Null'), Symbol('a'), Symbol('b')))
        self.check('{,a,b,}', Expression('List', Symbol('Null'), Symbol('a'), Symbol('b'), Symbol('Null')))

    def testSequence(self):
        self.check('Sin[x, y]', Expression('Sin', Symbol('x'), Symbol('y')))

    def testPart(self):
        self.check('a[[1]]', Expression('Part', Symbol('a'), Integer(1)))

    def testBlank(self):
        self.check('f_', Expression('Pattern', Symbol('f'), Expression('Blank')))
        self.check('f__', Expression('Pattern', Symbol('f'), Expression('BlankSequence')))
        self.check('f___', Expression('Pattern', Symbol('f'), Expression('BlankNullSequence')))

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

    def testSlot(self):
        self.check('#2', Expression('Slot', Integer(2)))
        self.check('#', Expression('Slot', Integer(1)))
        self.check('##2', Expression('SlotSequence', Integer(2)))
        self.check('##', Expression('SlotSequence', Integer(1)))
        self.check('%2', Expression('Out', Integer(2)))

    def testOut(self):
        self.check('%%', Expression('Out', Integer(-2)))
        self.check('%%%%', Expression('Out', Integer(-4)))
        self.check('%', Expression('Out'))

    def testNonAscii(self):
        self.check('z \\[Conjugate]', Expression('Conjugate', Symbol('z')))
        self.check('z \\[Transpose]', Expression('Transpose', Symbol('z')))
        self.check('z \\[ConjugateTranspose]', Expression('ConjugateTranspose', Symbol('z')))
        self.check(u'z \uf3c7 ', Expression('Transpose', Symbol('z')))
        self.check(u'z \uf3c8 ', Expression('Conjugate', Symbol('z')))
        self.check(u'z \uf3c9 ', Expression('ConjugateTranspose', Symbol('z')))
        self.check('\\[Integral] x \\[DifferentialD] x', Expression('Integrate', Symbol('x'), Symbol('x')))
        self.check('\\[Del] x', Expression('Del', Symbol('x')))
        self.check('\\[Square] x', Expression('Square', Symbol('x')))
        self.check('1 \\[SmallCircle] 2', Expression('SmallCircle', Integer(1), Integer(2)))
        self.check('1 \\[SmallCircle] 2 \\[SmallCircle] 3', Expression('SmallCircle', Integer(1), Integer(2), Integer(3)))
        self.check(u'1 \u2218 2', Expression('SmallCircle', Integer(1), Integer(2)))
        self.check('1 \\[CircleDot] 2', Expression('CircleDot', Integer(1), Integer(2)))
        self.check(u'1 \u2299 2', Expression('CircleDot', Integer(1), Integer(2)))
        self.check('1 \\[Diamond] 2', Expression('Diamond', Integer(1), Integer(2)))
        self.check('1 \\[Wedge] 2', Expression('Wedge', Integer(1), Integer(2)))
        self.check('1 \\[Vee] 2', Expression('Vee', Integer(1), Integer(2)))
        self.check('1 \\[CircleTimes] 2', Expression('CircleTimes', Integer(1), Integer(2)))
        self.check('1 \\[CenterDot] 2', Expression('CenterDot', Integer(1), Integer(2)))
        self.check('1 \\[Star] 2', Expression('Star', Integer(1), Integer(2)))
        self.check('a \\[Cap] b', 'Cap[a,b]')
        self.check('a \\[Cup] b \\[Cup] c', 'Cup[a,b,c]')
        self.check(u'a \u2322 b \u2322 c', 'Cap[a,b,c]')
        self.check(u'a \u2323 b', 'Cup[a, b]')
        self.check(u'1 \u22C4 2', Expression('Diamond', Integer(1), Integer(2)))
        self.check(u'1 \u22C0 2', Expression('Wedge', Integer(1), Integer(2)))
        self.check(u'1 \u22c1 2', Expression('Vee', Integer(1), Integer(2)))
        self.check(u'1 \u2297 2', Expression('CircleTimes', Integer(1), Integer(2)))
        self.check(u'1 \u00B7 2', Expression('CenterDot', Integer(1), Integer(2)))
        self.check(u'1 \u22C6 2', Expression('Star', Integer(1), Integer(2)))
        self.check('expr1 ** expr2', Expression('NonCommutativeMultiply', Symbol('expr1'), Symbol('expr2')))
        self.check('expr1 ** expr2 ** expr3', Expression('NonCommutativeMultiply', Symbol('expr1'), Symbol('expr2'), Symbol('expr3')))
        self.check('1 \\[Cross] 2', Expression('Cross', Integer(1), Integer(2)))
        self.check(u'1 \uf4a0 2', Expression('Cross', Integer(1), Integer(2)))
        self.check('3\\[Divide]2', Expression('Times', Integer(3), Expression('Power', Integer(2), Integer(-1))))
        self.check(u'3 \u00f7 2', Expression('Times', Integer(3), Expression('Power', Integer(2), Integer(-1))))
        self.check('3\\2', Expression('Backslash', Integer(3), Integer(2)))
        self.check('1 \\[Times] 2', Expression('Times', Integer(1), Integer(2)))
        self.check(u'1 \u00d7 2', Expression('Times', Integer(1), Integer(2)))
        self.check('1 \[PlusMinus] 2', Expression('PlusMinus', Integer(1), Integer(2)))
        self.check('1 \[MinusPlus] 2', Expression('MinusPlus', Integer(1), Integer(2)))
        self.check('\[PlusMinus] 1', Expression('PlusMinus', Integer(1)))
        self.check('\[MinusPlus] 1', Expression('MinusPlus', Integer(1)))
        self.check(u'\u00b1 1', Expression('PlusMinus', Integer(1)))
        self.check(u'\u2213 1', Expression('MinusPlus', Integer(1)))
        self.check('1 \\[And] 2', Expression('And', Integer(1), Integer(2)))
        self.check(u'1 \u2227 2', Expression('And', Integer(1), Integer(2)))
        self.check('1 \\[Or] 2', Expression('Or', Integer(1), Integer(2)))
        self.check(u'1 \u2228 2', Expression('Or', Integer(1), Integer(2)))

        self.check('a \\[Colon] b', Expression('Colon', Symbol('a'), Symbol('b')))
        self.check(u'a \u2236 b', Expression('Colon', Symbol('a'), Symbol('b')))
        self.check('x \\[Function] y', 'Function[{x}, y]')
        self.check(u'x \uf4a1 y', 'Function[{x}, y]')

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
        self.check('\\(1 \\` 2\\)', Expression('FormBox', Integer(2), Integer(1)))

    def testParseError(self):
        self.assertRaises(ParseError, parse, '1+')

    def testBracketMatching(self):
        self.assertRaises(ParseError, parse, 'x)')      # bktmop
        self.assertRaises(ParseError, parse, 'x]')      # bktmop
        self.assertRaises(ParseError, parse, 'x}')      # bktmop
        self.assertRaises(ParseError, parse, 'x]]')     # bktmop
        self.assertRaises(ParseError, parse, '(x,')     # bktmcp
        self.assertRaises(ParseError, parse, '(x')      # bktmcp
        self.assertRaises(ParseError, parse, '[x')      # bktmcp
        self.assertRaises(ParseError, parse, '{x')      # bktmcp
        self.assertRaises(ParseError, parse, '[[x')     # bktmcp


if __name__ == "__main__":
    unittest.main()
