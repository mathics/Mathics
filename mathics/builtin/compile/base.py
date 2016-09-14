#!/usr/bin/env python
# -*- coding: utf-8 -*-


class CompilationError(Exception):
    pass


class CompileArg(object):
    def __init__(self, name, type):
        self.name = name
        self.type = type
