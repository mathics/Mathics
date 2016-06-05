#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals

from collections import defaultdict


prefix_ops = {
    'Get': 720,
    'PreIncrement': 660,
    'PreDecrement': 660,
    'Del': 550,
    'Minus': 480,
    'Square': 540,
    'ForAll': 240,
    'Exists': 240,
    'NotExists': 240,
    'Not': 230,
}

postfix_ops = {
    'Unset': 670,
    'Conjugate': 670,
    'Transpose': 670,
    'ConjugateTranspose': 670,
    'Derivative': 670,
    'Function': 670,
    'Increment': 660,
    'Decrement': 660,
    'Factorial': 610,
    'Factorial2': 610,
    'Repeated': 170,
    'RepeatedNull': 170,
}

left_binary_ops = {
    'Subtract': 310,
    'Divide': 470,
    'LeftTee': 190,
    'DoubleLeftTee': 190,
    'Condition': 130,
    'ReplaceAll': 110,
    'ReplaceRepeated': 110,
    'Because': 50,
    'PutAppend': 30,
    'Put': 30,
}

right_binary_ops = {
    'Apply': 670,
    'Map': 620,
    'MapAll': 620,
    'Power': 590,
    'RightTee': 190,
    'DoubleRightTee': 190,
    'SuchThat': 180,
    'Rule': 120,
    'RuleDelayed': 120,
    'AddTo': 100,
    'SubtractFrom': 100,
    'TimesBy': 100,
    'DivideBy': 100,
    'Therefore': 50,
    'UpSet': 40,
    'Set': 40,
    'SetDelayed': 40,
    'UpSetDelayed': 40,
}

flat_binary_ops = {
    'MessageName': 750,
    'Backslash': 670,
    'StringJoin': 600,
    'SmallCircle': 530,
    'CircleDot': 520,
    'NonCommutativeMultiply': 510,
    'Cross': 500,
    'Union': 300,
    'Dot': 490,
    'Diamond': 450,
    'Wedge': 440,
    'Vee': 430,
    'CircleTimes': 420,
    'CenterDot': 410,
    'Times': 400,
    'VerticalTilde': 370,
    'Coproduct': 360,
    'Cap': 350,
    'Cup': 340,
    'Star': 390,
    'CirclePlus': 330,
    'CircleMinus': 330,
    'Plus': 310,
    'Intersection': 305,
    'VerticalBar': 280,
    'NotVerticalBar': 280,
    'DoubleVerticalBar': 280,
    'NotDoubleVerticalBar': 280,
    'SameQ': 290,
    'UnsameQ': 290,
    'Equal': 290,
    'Unequal': 290,
    'Greater': 290,
    'Less': 290,
    'GreaterEqual': 290,
    'LessEqual': 290,
    'Element': 250,
    'NotElement': 250,
    'Subset': 250,
    'Superset': 250,
    'And': 215,
    'Nand': 215,
    'Xor': 215,
    'Xnor': 215,
    'Or': 215,
    'Nor': 215,
    'Equivalent': 205,
    'Alternatives': 160,
    'StringExpression': 135,
    'Colon': 80,
    'VerticalSeparator': 60,
    'CompoundExpression': 10,
}

nonassoc_binary_ops = {
    'PatternTest': 680,
    'PlusMinus': 310,
    'MinusPlus': 310,
    'Implies': 200,
}

ternary_ops = {
    'Span': 305,
    'Infix': 630,   # TODO
}

misc_ops = {
    'Sum': 320,
    'DifferentialD': 550,
}

# binary_ops = left_binary_ops V right_binary_ops V flat_binary_ops V nonassoc_binary_ops
binary_ops = {}
for ops in (left_binary_ops, right_binary_ops, flat_binary_ops, nonassoc_binary_ops):
    for op, prec in ops.items():
        binary_ops[op] = prec

# all ops - check they're disjoint
all_ops = defaultdict(lambda: 670)
for ops in (prefix_ops, postfix_ops, left_binary_ops, right_binary_ops, flat_binary_ops, ternary_ops, nonassoc_binary_ops, misc_ops):
    for op, prec in ops.items():
        if op in all_ops:
            raise AssertionError
        all_ops[op] = prec
