#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from math import log10
import sympy

import mathics.core.expression as ma
from mathics.core.parser.ast import Symbol, String, Number, Filename
from mathics.core.numbers import machine_precision, reconstruct_digits


class GenericConverter(object):
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
            return "Expression", head, children

    @staticmethod
    def string_escape(s):
        s = s.replace("\\\\", "\\").replace('\\"', '"')
        s = s.replace("\\r\\n", "\r\n")
        s = s.replace("\\r", "\r")
        s = s.replace("\\n", "\n")
        s = s.replace("\\t", "\t")
        return s

    def convert_Symbol(self, node):
        if node.context is not None:
            return "Symbol", node.context + "`" + node.value
        else:
            return "Lookup", node.value

    def convert_String(self, node):
        value = self.string_escape(node.value)
        return "String", value

    def convert_Filename(self, node):
        s = node.value
        if s.startswith('"'):
            assert s.endswith('"')
            s = s[1:-1]
        s = self.string_escape(s)
        s = s.replace("\\", "\\\\")
        return "String", s

    def convert_Number(self, node):
        s = node.value
        sign = node.sign
        base = node.base
        suffix = node.suffix
        n = node.exp

        # Look for decimal point
        if "." not in s:
            if suffix is None:
                if n < 0:
                    return "Rational", sign * int(s, base), base ** abs(n)
                else:
                    return "Integer", sign * int(s, base) * (base ** n)
            else:
                s = s + "."

        if base == 10:
            man = s
            if n != 0:
                s = s + "E" + str(n)

            if suffix is None:
                # MachineReal/PrecisionReal is determined by number of digits
                # in the mantissa
                d = len(man) - 2  # one less for decimal point
                if d < reconstruct_digits(machine_precision):
                    return "MachineReal", sign * float(s)
                else:
                    return (
                        "PrecisionReal",
                        ("DecimalString", str("-" + s if sign == -1 else s)),
                        d,
                    )
            elif suffix == "":
                return "MachineReal", sign * float(s)
            elif suffix.startswith("`"):
                acc = float(suffix[1:])
                x = float(s)
                if x == 0:
                    prec10 = acc
                else:
                    prec10 = acc + log10(x)
                return (
                    "PrecisionReal",
                    ("DecimalString", str("-" + s if sign == -1 else s)),
                    prec10,
                )
            else:
                return (
                    "PrecisionReal",
                    ("DecimalString", str("-" + s if sign == -1 else s)),
                    float(suffix),
                )

        # Put into standard form mantissa * base ^ n
        s = s.split(".")
        if len(s) == 1:
            man = s[0]
        else:
            n -= len(s[1])
            man = s[0] + s[1]
        man = sign * int(man, base)
        if n >= 0:
            p = man * base ** n
            q = 1
        else:
            p = man
            q = base ** -n
        result = "Rational", p, q
        x = float(sympy.Rational(p, q))

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
        elif suffix == "":
            prec10 = None
        elif suffix.startswith("`"):
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
            return "MachineReal", x
        else:
            return "PrecisionReal", result, prec10


class Converter(GenericConverter):
    def __init__(self):
        self.definitions = None

    def convert(self, node, definitions):
        self.definitions = definitions
        result = self.do_convert(node)
        self.definitions = None
        return result

    def do_convert(self, node):
        result = GenericConverter.do_convert(self, node)
        return getattr(self, "_make_" + result[0])(*result[1:])

    def _make_Symbol(self, s):
        return ma.Symbol(s)

    def _make_Lookup(self, s):
        value = self.definitions.lookup_name(s)
        return ma.Symbol(value)

    def _make_String(self, s):
        return ma.String(s)

    def _make_Integer(self, x):
        return ma.Integer(x)

    def _make_Rational(self, x, y):
        return ma.Rational(x, y)

    def _make_MachineReal(self, x):
        return ma.MachineReal(x)

    def _make_PrecisionReal(self, value, prec):
        if value[0] == "Rational":
            assert len(value) == 3
            x = sympy.Rational(*value[1:])
        elif value[0] == "DecimalString":
            assert len(value) == 2
            x = value[1]
        else:
            assert False
        return ma.PrecisionReal(sympy.Float(x, prec))

    def _make_Expression(self, head, children):
        return ma.Expression(head, *children)


converter = Converter()
convert = converter.convert
