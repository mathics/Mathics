#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals


prefix_ops = {
    'Minus': 480,
    'Not': 230,
    'Get': 720,
}

postfix_ops = {
    'Factorial': 610,
    "Derivative": 670,
}

left_binary_ops = {
    'Subtract': 310,
    'Divide': 470,
    'PutAppend': 30,
    'Put': 30,
}

right_binary_ops = {
    'Equal': 40,
}

flat_binary_ops = {
    'Plus': 310,
    'Times': 400,
    'CompoundExpression': 10,
}

nonassoc_binary_ops = {
    'PatternTest': 680,
}

ternary_ops = {
    'Span': 305,
    'Infix': 630,   # TODO
}

# binary_ops = left_binary_ops V right_binary_ops V flat_binary_ops
binary_ops = {}
for ops in (left_binary_ops, right_binary_ops, flat_binary_ops, nonassoc_binary_ops):
    for op, prec in ops.items():
        binary_ops[op] = prec

# all ops - check they're disjoint
all_ops = {}
for ops in (prefix_ops, postfix_ops, left_binary_ops, right_binary_ops, flat_binary_ops, ternary_ops):
    for op, prec in ops.items():
        if op in all_ops:
            raise AssertionError
        all_ops[op] = prec

all_ops['Number'] = 670
all_ops['String'] = 670
all_ops['Symbol'] = 670
