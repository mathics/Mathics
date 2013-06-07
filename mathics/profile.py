# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011-2013 The Mathics Team

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

import cProfile
import pstats

from mathics.core.definitions import Definitions
from mathics.core.expression import Evaluation

definitions = Definitions(add_builtin=True)


def prepare():
    pass

result = None


def run():
    global result
    # prompt = '(1+a)(1+b)(1+c)(1+d)(1+e)//Expand'
    # prompt = 'f/@Range[20000];'
    # prompt = 'Plus @@ Range[50000]'
    # prompt = 'Range[100000];'
    try:
        # prompt = 'SetAttributes[v, Flat]; v[x_]:={x}; v[a,b]'
        # prompt = """(Plus@@Symbol/@CharacterRange["a","z"])^2//Expand;"""
        # prompt = (
        #     'Plus@@f/@Symbol/@StringJoin/@Tuples[CharacterRange["a","z"],2]')
        # prompt = 'FullForm[Nest[1+Sqrt[1+#]&, x, 20]]'
        # prompt = '1+2'
        prompt = 'DensityPlot[x*y,{x,-1,1},{y,-1,1}]'
        evaluation = Evaluation(prompt, definitions, format='xml')
        if evaluation.results:
            result = evaluation.results[0].result
    except KeyboardInterrupt:
        result = 'INTERRUPTED'


def _profile():
    global result
    prepare()
    cProfile.run('run()', 'profile')
    # print 'Result: %s\n' % result
    p = pstats.Stats('profile')
    p.sort_stats('cumulative').print_stats(50)
    p.print_callees(20)

if __name__ == '__main__':
    _profile()
