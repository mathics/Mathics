#!/usr/bin/env python
# -*- coding: utf-8 -*-


class ParseError(Exception):
    def __init__(self, pos):
        self.pos = pos


class ScanError(ParseError):
    pass


class InvalidSyntaxError(ParseError):
    pass


class IncompleteSyntaxError(ParseError):
    pass
