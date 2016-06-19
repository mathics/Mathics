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


class Converter(object):
    def __init__(self):
        self.definitions = None

    def convert(self, node, definitions):
        self.definitions = definitions
        result = self.do_convert(node)
        self.definitions = None
        return result

    def do_convert(self, node):
        if isinstance(node, Symbol):
            return self.convert_Symbol(node)
        elif isinstance(node, String):
            return self.convert_String(node)
        elif isinstance(node, Number):
            return self.convert_Number(node)
        elif isinstance(node, Filename):
            return self.convert_Filename(node)
        else:
            head = self.do_convert(node.head)
            children = [self.do_convert(child) for child in node.children]
            return ma.Expression(head, *children)

    @staticmethod
    def string_escape(s):
        s = s.replace('\\\\', '\\').replace('\\"', '"')
        s = s.replace('\\r\\n', '\r\n')
        s = s.replace('\\r', '\r')
        s = s.replace('\\n', '\n')
        return s

    def convert_Symbol(self, node):
        if node.context is not None:
            return ma.Symbol(node.context + '`' + node.value)
        value = self.definitions.lookup_name(node.value)
        return ma.Symbol(value)

    def convert_String(self, node):
        value = self.string_escape(node.value)
        return ma.String(value)

    def convert_Filename(self, node):
        s = node.value
        if s.startswith('"'):
            assert s.endswith('"')
            s = s[1:-1]
        s = self.string_escape(s)
        s = s.replace('\\', '\\\\')
        return ma.String(s)

    def convert_Number(self, node):
        s = node.value
        if s[0] == '-':
            sign_prefix, s = s[0], s[1:]
            sign = -1
        else:
            sign_prefix = ''
            sign = 1

        # fast exit
        if s.isdigit():
            return ma.Integer(sign * int(s))

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
                    return ma.Rational(sign * int(s, base), base ** abs(n))
                else:
                    return ma.Integer(sign * int(s, base) * (base ** n))
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
            return ma.Real(sign_prefix + s, prec)
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

            man = sign * int(man, base)
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


converter = Converter()
convert = converter.convert
