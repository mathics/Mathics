#!/usr/bin/env python
# -*- coding: utf-8 -*-


class TranslateError(Exception):
    def __init__(self, pos):
        self.pos = pos


class ScanError(TranslateError):
    pass


class InvalidSyntaxError(TranslateError):
    pass


class IncompleteSyntaxError(TranslateError):
    pass
