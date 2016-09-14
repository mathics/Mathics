#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import absolute_import
from __future__ import unicode_literals

import re
from math import log10
import sympy

import mathics.core.expression as ma
from mathics.core.parser.ast import Symbol, String, Number, Filename
from mathics.core.numbers import machine_precision, reconstruct_digits
from mathics.core.numbers import prec as _prec


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
            s, suffix = s[0], None
        else:
            s, suffix = s[0], s[1]

        # Look for decimal point
        if '.' not in s:
            if suffix is None:
                if n < 0:
                    return ma.Rational(sign * int(s, base), base ** abs(n))
                else:
                    return ma.Integer(sign * int(s, base) * (base ** n))
            else:
                s = s + '.'

        if base == 10:
            man = s
            if n != 0:
                s = s + 'E' + str(n)

            if suffix is None:
                # MachineReal/PrecisionReal is determined by number of digits
                # in the mantissa
                d = len(man) - 2    # one less for decimal point
                if d < reconstruct_digits(machine_precision):
                    result = float(sign_prefix + s)
                else:
                    result = sympy.Float(str(sign_prefix + s), d)
            elif suffix == '':
                result = float(sign_prefix + s)
            elif suffix.startswith('`'):
                acc = float(suffix[1:])
                x = float(s)
                if x == 0:
                    prec10 = acc
                else:
                    prec10 = acc + log10(x)
                result = sympy.Float(str(sign_prefix + s), prec10)
            else:
                result = sympy.Float(str(sign_prefix + s), float(suffix))

            if isinstance(result, float):
                return ma.MachineReal(result)
            elif isinstance(result, sympy.Float):
                return ma.PrecisionReal(result)

        # Put into standard form mantissa * base ^ n
        s = s.split('.')
        if len(s) == 1:
            man = s[0]
        else:
            n -= len(s[1])
            man = s[0] + s[1]
        man = sign * int(man, base)
        if n >= 0:
            result = sympy.Integer(man * base ** n)
        else:
            result = sympy.Rational(man, base ** -n)

        x = float(result)

        # determine `prec10` the digits of precision in base 10
        if suffix is None:
            acc = len(s[1])
            acc10 = acc * log10(base)
            if x == 0:
                prec10 = acc10
            else:
                prec10 = acc10 + log10(abs(x))
            if prec10 < reconstruct_digits(machine_precision):
                prec10 = None
        elif suffix == '':
            prec10 = None
        elif suffix.startswith('`'):
            acc = float(suffix[1:])
            acc10 = acc * log10(base)
            if x == 0:
                prec10 = acc10
            else:
                prec10 = acc10 + log10(abs(x))
        else:
            prec = float(suffix)
            prec10 = prec * log10(base)

        if prec10 is None:
            return ma.MachineReal(x)
        else:
            result = sympy.Float(result, prec10)
            return ma.PrecisionReal(result)


converter = Converter()
convert = converter.convert
