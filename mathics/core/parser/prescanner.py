#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import absolute_import
from __future__ import unicode_literals

from six import unichr

from mathics.core.characters import named_characters
from mathics.core.parser.errors import ScanError, IncompleteSyntaxError, InvalidSyntaxError


class PreScanner(object):
    r'''
    Converts:
        character codes to characters:
            \.7A -> z
            \:004a -> J
            \041 -> !
        unicode longnames to characters:
            \[Theta] -> \u03B8
        escape sequences:
            \n -> literal \n

    Also reports trailing \ characters as incomplete.

    PreScanner works by breaking the partitioning code into stubs.
    '''
    def __init__(self):
        self.code = None    # input code
        self.stubs = None   # stubs of code to be joined
        self.pos = None     # current position within code
        self.start = None   # start of current stub

    def scan(self, code):
        # initialise
        self.code = code
        self.stubs = []
        self.pos = 0
        self.start = 0
        # main loop
        while self.pos < len(self.code):
            if self.code[self.pos] == '\\':
                if self.pos + 1 == len(self.code):
                    raise IncompleteSyntaxError(self.pos)
                c = self.code[self.pos + 1]
                if c == '.':
                    self.try_parse_base(2, 4, 16)
                elif c == ':':
                    self.try_parse_base(2, 6, 16)
                elif c == '[':
                    self.try_parse_longname(2)
                elif c in '01234567':
                    self.try_parse_base(1, 4, 8)
                else:
                    self.pos += 1
            else:
                self.pos += 1
        self.stubs.append(self.code[self.start:])   # final stub
        # reduce
        return ''.join(self.stubs)

    def newstub(self, pos):
        self.pos = pos
        self.start = pos

    def try_parse_base(self, start_shift, end_shift, base):
        start, end = self.pos + start_shift, self.pos + end_shift
        result = None
        if end <= len(self.code):
            text = self.code[start:end]
            try:
                result = int(text, base)
            except ValueError:
                pass    # result remains None
        if result is None:
            # TODO Syntax  message
            raise InvalidSyntaxError(self.pos)
        self.stubs.append(self.code[self.start:self.pos])
        self.stubs.append(unichr(result))
        self.newstub(end)

    def try_parse_longname(self, start_shift):
        i = self.pos + start_shift
        while i < len(self.code):
            if self.code[i] == ']':
                break
            i += 1
        else:
            raise IncompleteSyntaxError(i)

        longname = self.code[self.pos + start_shift:i]
        if longname.isalpha():
            char = named_characters.get(longname)
            if char is None:
                # TODO: Syntax::sntufn message
                pass    # stay in same stub
            else:
                self.stubs.append(self.code[self.start:self.pos])
                self.stubs.append(char)
                self.newstub(i + 1)
        self.pos = i + 1    # stay in same stub but skip ahead

prescanner = PreScanner()   # singleton instance


def prescan(code):
    return prescanner.scan(code)
