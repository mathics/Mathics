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

from mathics.core.expression import Expression, Symbol, Integer, Rational, Real, Number
from mathics.core.util import subsets, subranges, permutations

class StopGenerator(Exception):
    def __init__(self, value=None):
        self.value = value
   
class StopGenerator_ExpressionPattern_match(StopGenerator):
    pass

def match(self, yield_func, expression, vars, evaluation, head=None, leaf_index=None, leaf_count=None, fully=True, wrap_oneid=True):
    evaluation.check_stopped()
    
    attributes = self.head.get_attributes(evaluation.definitions)
    if 'Flat' not in attributes:
        fully = True
    if not expression.is_atom():
        def yield_choice(pre_vars):
            for leaf in self.leaves:
                match_count = leaf.get_match_count()
                candidates = leaf.get_match_candidates_count(expression.leaves, expression, attributes, evaluation, pre_vars)
                if candidates < match_count[0]:
                    raise StopGenerator_ExpressionPattern_match()
            #for new_vars, rest in self.match_leaf(self.leaves[0], self.leaves[1:], ([], expression.leaves), pre_vars,
            #    expression, attributes, evaluation, first=True, fully=fully, leaf_count=len(self.leaves),
            #    wrap_oneid=expression.get_head_name() != 'MakeBoxes'):
            #def yield_leaf(new_vars, rest):
            #    yield_func(new_vars, rest)
            self.match_leaf(yield_func, self.leaves[0], self.leaves[1:], ([], expression.leaves), pre_vars,
                expression, attributes, evaluation, first=True, fully=fully, leaf_count=len(self.leaves),
                wrap_oneid=expression.get_head_name() != 'MakeBoxes')
                    
        #for head_vars, _ in self.head.match(expression.get_head(), vars, evaluation):
        def yield_head(head_vars, _):
            if self.leaves:
                #pre_choices = self.get_pre_choices(expression, attributes, head_vars)
                #for pre_vars in pre_choices:
                
                self.get_pre_choices(yield_choice, expression, attributes, head_vars)
            else:
                if not expression.leaves:
                    yield_func(head_vars, None)
                else:
                    return
        try:
            self.head.match(yield_head, expression.get_head(), vars, evaluation)
        except StopGenerator_ExpressionPattern_match:
            return
    if wrap_oneid and 'OneIdentity' in attributes and expression.get_head() != self.head and expression != self.head: # and 'OneIdentity' not in (expression.get_attributes(evaluation.definitions) | expression.get_head().get_attributes(evaluation.definitions)):
        new_expression = Expression(self.head, expression)
        for leaf in self.leaves:
            leaf.match_count = leaf.get_match_count()
            leaf.candidates = [expression] #leaf.get_match_candidates(new_expression.leaves, new_expression, attributes, evaluation, vars)
            if len(leaf.candidates) < leaf.match_count[0]:
                return
        #for new_vars, rest in self.match_leaf(self.leaves[0], self.leaves[1:], ([], [expression]), vars,
        #    new_expression, attributes, evaluation, first=True, fully=fully, leaf_count=len(self.leaves), wrap_oneid=True):
        #def yield_leaf(new_vars, rest):
        #    yield_func(new_vars, rest)
        self.match_leaf(yield_func, self.leaves[0], self.leaves[1:], ([], [expression]), vars,
            new_expression, attributes, evaluation, first=True, fully=fully, leaf_count=len(self.leaves), wrap_oneid=True)
    