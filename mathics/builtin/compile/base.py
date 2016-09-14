#!/usr/bin/env python
# -*- coding: utf-8 -*-


class CompilationError(Exception):
    pass


class MathicsArg(object):
    def __init__(self, name, type):
        self.name = name
        self.type = type
