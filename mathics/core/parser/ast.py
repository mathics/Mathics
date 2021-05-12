#!/usr/bin/env python3
# -*- coding: utf-8 -*-


class Node(object):
    def __init__(self, head, *children):
        if isinstance(head, Node):
            self.head = head
        else:
            self.head = Symbol(head)
        self.value = None
        self.children = list(children)
        self.parenthesised = False

    def get_head_name(self):
        if isinstance(self.head, Symbol):
            return self.head.value
        else:
            return ""

    def __repr__(self):
        return "%s[%s]" % (self.head, ", ".join(str(child) for child in self.children))

    def __eq__(self, other):
        if not isinstance(other, Node):
            raise TypeError()
        return (
            (self.get_head_name() == other.get_head_name())
            and (len(self.children) == len(other.children))
            and all(cs == co for cs, co in zip(self.children, other.children))
        )

    def flatten(self):
        head_name = self.get_head_name()
        new_children = []
        for child in self.children:
            if child.get_head_name() == head_name and not child.parenthesised:
                new_children.extend(child.children)
            else:
                new_children.append(child)
        self.children = new_children
        return self


class Atom(Node):
    def __init__(self, value):
        self.head = Symbol(self.__class__.__name__)
        self.value = value
        self.children = []
        self.parenthesised = False

    def __repr__(self):
        return "%s[%s]" % (self.head, self.value)

    def __eq__(self, other):
        return self.__class__ == other.__class__ and self.value == other.value


class Number(Atom):
    def __init__(self, value, sign=1, base=10, suffix=None, exp=0):
        assert isinstance(value, str)
        assert sign in (-1, 1)
        assert isinstance(base, int)
        assert 2 <= base <= 36
        assert isinstance(exp, int)
        assert suffix is None or isinstance(suffix, str)
        super(Number, self).__init__(None)
        self.value = value
        self.sign = sign
        self.base = base
        self.suffix = suffix
        self.exp = exp

    def __repr__(self):
        result = self.value
        if self.base != 10:
            result = "%i^^%s" % (self.base, result)
        if self.sign == -1:
            result = "-%s" % result
        if self.suffix is not None:
            result = "%s`%s" % (result, self.suffix)
        if self.exp != 0:
            result = "%s*^%i" % (result, self.exp)
        return result

    def __eq__(self, other):
        return isinstance(other, Number) and repr(self) == repr(other)


class Symbol(Atom):
    def __init__(self, value, context="System"):
        self.context = context
        self.value = value
        self.children = []

    # avoids recursive definition
    @property
    def head(self):
        return Symbol(self.__class__.__name__)

    def __repr__(self):
        return self.value


class String(Atom):
    def __repr__(self):
        return '"' + self.value + '"'


class Filename(Atom):
    def __repr__(self):
        return self.value
