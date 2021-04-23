#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from collections import defaultdict


prefix_ops = {
    "Get": 720,
    "PreIncrement": 660,
    "PreDecrement": 660,
    "Del": 550,
    "Minus": 480,
    "Square": 540,
    "ForAll": 240,
    "Exists": 240,
    "NotExists": 240,
    "Not": 230,
    "Information": 5001,
    "Definition": 5000,
}

postfix_ops = {
    "Unset": 670,
    "Conjugate": 670,
    "Transpose": 670,
    "ConjugateTranspose": 670,
    "Derivative": 670,
    "Increment": 660,
    "Decrement": 660,
    "Factorial": 610,
    "Factorial2": 610,
    "Repeated": 170,
    "RepeatedNull": 170,
    "Function": 90,
}

left_binary_ops = {
    "Divide": 470,
    "PlusMinus": 310,
    "MinusPlus": 310,
    "Subtract": 310,
    "LeftTee": 190,
    "DoubleLeftTee": 190,
    "Condition": 130,
    "ReplaceAll": 110,
    "ReplaceRepeated": 110,
    "Because": 50,
    "PutAppend": 30,
    "Put": 30,
    "Postfix": 70,
}

right_binary_ops = {
    "Apply": 620,
    "Map": 620,
    "MapAll": 620,
    "Power": 590,
    "Implies": 200,
    "RightTee": 190,
    "DoubleRightTee": 190,
    "SuchThat": 180,
    "Rule": 120,
    "RuleDelayed": 120,
    "AddTo": 100,
    "SubtractFrom": 100,
    "TimesBy": 100,
    "DivideBy": 100,
    "Therefore": 50,
    "UpSet": 40,
    "Set": 40,
    "SetDelayed": 40,
    "UpSetDelayed": 40,
}

flat_binary_ops = {
    "MessageName": 750,
    "Composition": 625,
    "StringJoin": 600,
    "SmallCircle": 530,
    "CircleDot": 520,
    "NonCommutativeMultiply": 510,
    "Cross": 500,
    "Union": 300,
    "Dot": 490,
    "Backslash": 460,
    "Diamond": 450,
    "Wedge": 440,
    "Vee": 430,
    "CircleTimes": 420,
    "CenterDot": 410,
    "Times": 400,
    "VerticalTilde": 370,
    "Coproduct": 360,
    "Cap": 350,
    "Cup": 340,
    "Star": 390,
    "CirclePlus": 330,
    "CircleMinus": 330,
    "Plus": 310,
    "Intersection": 305,
    "VerticalBar": 280,
    "NotVerticalBar": 280,
    "DoubleVerticalBar": 280,
    "NotDoubleVerticalBar": 280,
    "SameQ": 290,
    "UnsameQ": 290,
    "Equal": 290,
    "Unequal": 290,
    "Greater": 290,
    "Less": 290,
    "GreaterEqual": 290,
    "LessEqual": 290,
    "Element": 250,
    "NotElement": 250,
    "Subset": 250,
    "Superset": 250,
    # HACK: although the should be 215 for all boolean_ops we adjust slightly
    # to get the subprecedences correct
    "And": 225,
    "Nand": 225,
    "Xor": 220,
    "Xnor": 220,
    "Or": 215,
    "Nor": 215,
    "Equivalent": 205,
    "Alternatives": 160,
    "StringExpression": 135,
    "Colon": 80,
    "VerticalSeparator": 60,
    "CompoundExpression": 10,
}

nonassoc_binary_ops = {
    "UndirectedEdge": 120,
    "DirectedEdge": 128,
    "PatternTest": 680,
}

ternary_ops = {
    "Span": 305,
    "Infix": 630,
}

misc_ops = {
    "DifferentialD": 550,
    "Sum": 320,
    "Pattern": 150,
    "Optional": 140,
    "SqrtBox": 670,
    "RadicalBox": 670,
    "FractionBox": 670,
    "OverscriptBox": 710,
    "UnderscriptBox": 710,
    "SubscriptBox": 695,
    "FormBox": 670,
    "SuperscriptBox": 590,
    "UnderoverscriptBox": 700,
    "SubsuperscriptBox": 690,
}

inequality_ops = ["Less", "LessEqual", "Greater", "GreaterEqual", "Equal", "Unequal"]

# binary_ops = left_binary_ops V right_binary_ops V flat_binary_ops V nonassoc_binary_ops
binary_ops = {}
for ops in (left_binary_ops, right_binary_ops, flat_binary_ops, nonassoc_binary_ops):
    for op, prec in ops.items():
        binary_ops[op] = prec

all_op_collections = (
    prefix_ops,
    postfix_ops,
    left_binary_ops,
    right_binary_ops,
    flat_binary_ops,
    ternary_ops,
    nonassoc_binary_ops,
    misc_ops,
)

# all ops - check they're disjoint
all_ops = defaultdict(lambda: 670)

for ops in all_op_collections:
    for op, prec in ops.items():
        if op in all_ops:
            raise AssertionError
        all_ops[op] = prec

all_operator_names = list(all_ops.keys())
