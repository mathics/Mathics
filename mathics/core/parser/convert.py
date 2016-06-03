#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import absolute_import
from __future__ import unicode_literals

import re
from math import log10

import mathics.core.expression as ma
from mathics.core.parser.ast import Symbol, String, Number, Filename
from mathics.core.numbers import dps
from mathics.builtin.numeric import machine_precision


def convert(node, definitions):
    if isinstance(node, Symbol):
        return ma.Symbol(definitions.lookup_name(node.value))
    elif isinstance(node, String):
        return ma.String(string_escape(node.value[1:-1]))
    elif isinstance(node, Number):
        return parse_number(node.value)
    elif isinstance(node, Filename):
        return ma.String(filename_escape(node.value))
    return ma.Expression(convert(node.head, definitions), *[convert(child, definitions) for child in node.children])


def string_escape(s):
    s = s.replace('\\\\', '\\').replace('\\"', '"')
    s = s.replace('\\r\\n', '\r\n')
    s = s.replace('\\r', '\r')
    s = s.replace('\\n', '\n')
    return s


def filename_escape(s):
    if s.startswith('"'):
        assert s.endswith('"')
        s = s[1:-1]
    s = string_escape(s)
    s = s.replace('\\', '\\\\')
    return s


def parse_number(s):
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
        # TODO: modify regex and provide error message if n not an int
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
                return ma.Integer(0)
            prec = float(suffix)

    # Look for decimal point
    if s.count('.') == 0:
        if suffix is None:
            if n < 0:
                return ma.Rational(int(s, base), base ** abs(n))
            else:
                return ma.Integer(int(s, base) * (base ** n))
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
        # XXX
        if prec is not None:
            prec = dps(prec)
        # return ma.Real(s, prec, acc)
        return ma.Real(s, prec)
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
            result = ma.Integer(man * base ** n)
        else:
            result = ma.Rational(man, base ** -n)

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
        # XXX
        if prec10 is None:
            prec10 = machine_precision
        else:
            prec10 = dps(prec10)
        return result.round(prec10)
