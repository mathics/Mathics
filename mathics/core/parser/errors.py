#!/usr/bin/env python
# -*- coding: utf-8 -*-


class TranslateError(Exception):
    pos = None
    msg = None
    args = None


class ScanError(TranslateError):
    def __init__(self, pos):
        self.pos = pos
        self.msg = 'scan'
        self.args = [pos]


class InvalidSyntaxError(TranslateError):
    def __init__(self, token):
        self.token = token
        self.pos = token.pos
        self.msg = 'invalid'
        self.args = [token.text]


class IncompleteSyntaxError(TranslateError):
    def __init__(self):
        self.msg = 'incomplete'
        self.args = []
