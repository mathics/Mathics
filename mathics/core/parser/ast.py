#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals


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
            return ''

    def __repr__(self):
        return '%s[%s]' % (self.head, ', '.join(str(child) for child in self.children))

    def __eq__(self, other):
        if not isinstance(other, Node):
            raise TypeError()
        return (self.get_head_name() == other.get_head_name()) and (len(self.children) == len(other.children)) and all(cs == co for cs, co in zip(self.children, other.children))

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
        return '%s[%s]' % (self.head, self.value)

    def __eq__(self, other):
        return self.__class__ == other.__class__ and self.value == self.value


class Number(Atom):
    def __repr__(self):
        return self.value


class Symbol(Atom):
    def __init__(self, value):
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
