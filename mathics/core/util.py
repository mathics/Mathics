# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011 Jan Pöschko

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

import re

FORMAT_RE = re.compile(r'\`(\d*)\`')

def interpolate_string(text, get_param):
    index = [1]
    
    def get_item(index):
        if 1 <= index <= len(args):
            return args[index - 1]
        else:
            return ''
        
    if isinstance(get_param, list):
        args = get_param
        get_param = get_item
    
    def repl(match):
        arg = match.group(1)
        if arg == '' or arg == '0':
            arg = index[0]
        else:
            arg = int(arg)
        index[0] += 1
        param = get_param(arg)
        return param
    return FORMAT_RE.sub(repl, text)

"""
NOTE: Maybe see
http://www.cosc.canterbury.ac.nz/tad.takaoka/isaac.pdf
resp.
http://www.cosc.canterbury.ac.nz/tad.takaoka/perm.p
for a permutation generating algorithm for multisets.
"""

def permutations(items, without_duplicates=True):
    if not items:
        yield []
    #already_taken = set()
    # first yield identical permutation without recursion
    yield items
    for index in range(len(items)):
        item = items[index]
        #if item not in already_taken:
        for sub in permutations(items[:index] + items[index+1:]):
            yield [item] + sub
            #already_taken.add(item)
            
def subsets(items, min, max, included=None, less_first=False):
    if max is None:
        max = len(items)
    lengths = range(min, max + 1)
    if not less_first:
        lengths = reversed(lengths)
    lengths = list(lengths)
    if lengths and lengths[0] == 0:
        lengths = lengths[1:] + [0]
        
    def decide(chosen, not_chosen, rest, count):
        if count < 0 or len(rest) < count:
            return
        if count == 0:
            yield chosen, not_chosen + rest     
        elif len(rest) == count:
            if included is None or all(item in included for item in rest):
                yield chosen + rest, not_chosen
        elif rest:
            item = rest[0]
            if included is None or item in included:
                for set in decide(chosen + [item], not_chosen, rest[1:], count - 1):
                    yield set 
            for set in decide(chosen, not_chosen + [item], rest[1:], count):
                yield set
                        
    for length in lengths:
        for chosen, not_chosen in decide([], [], items, length):
            yield chosen, ([], not_chosen)
        
def subsets_2(items, min, max, without_duplicates=True):
    """ max may only be 1 or None (= infinity).
    Respects include property of items
    """
    
    if min <= max == 1:
        for index in range(len(items)):
            if items[index].include:
                yield [items[index]], ([], items[:index] + items[index+1:])
        if min == 0:
            yield [], ([], items)
    else:
        counts = {}
        for item in items:
            if item.include:
                if item in counts:
                    counts[item] += 1
                else:
                    counts[item] = 1
        already = set()
        def decide(chosen, not_chosen, rest):
            if not rest:
                if len(chosen) >= min:
                    """if False and len(chosen) > 1 and (permutate_until is None or len(chosen) <= permutate_until):
                        for perm in permutations(chosen):
                            yield perm, ([], not_chosen)
                    else:"""
                    yield chosen, ([], not_chosen)
            else:
                if rest[0].include:
                    for set in decide(chosen + [rest[0]], not_chosen, rest[1:]):
                        yield set
                for set in decide(chosen, not_chosen + [rest[0]], rest[1:]):
                    yield set
        for subset in decide([], [], counts.keys()):
            t = tuple(subset[0])
            if t not in already:
                yield subset
                already.add(t)
            else:
                print 'already taken'
        
def subranges(items, min_count, max, flexible_start=False, included=None, less_first=False):
    # TODO: take into account included
    
    if max is None:
        max = len(items)
    max = min(max, len(items))
    if flexible_start:
        starts = range(len(items) - max + 1)
    else:
        starts = (0,)
    for start in starts:
        lengths = range(min_count, max + 1)
        if not less_first:
            lengths = reversed(lengths)
        lengths = list(lengths)
        if lengths == [0, 1]:
            lengths = [1, 0]
        for length in lengths:
            yield items[start:start+length], (items[:start], items[start+length:])
            
def unicode_superscript(value):
    def repl_char(c):
        if c == '1':
            value = 185
        elif c == '2':
            value = 178
        elif c == '3':
            value = 179
        elif '0' <= c <= '9':
            value = 8304 + (ord(c) - ord('0'))
        elif c == '-':
            value = 8315
        elif c == '(':
            value = 8317
        elif c == ')':
            value = 8318
        else:
            value = ord(c)
        return unichr(value)
    return u''.join(repl_char(c) for c in value)

            