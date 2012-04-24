# -*- coding: utf8 -*-
# cython: profile=False

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

#from mathics.core.pattern_nocython import StopGenerator #, Pattern #, ExpressionPattern
#from mathics.core import pattern_nocython
        
def Pattern_create(expr):
    from mathics.builtin import pattern_objects
    #from mathics.core.pattern import AtomPattern, ExpressionPattern
    
    name = expr.get_head_name()
    pattern_object = pattern_objects.get(name)
    if pattern_object is not None:
        return pattern_object(expr)
    if expr.is_atom():
        return AtomPattern(expr)
    else:
        return ExpressionPattern(expr)

class StopGenerator(Exception):
    def __init__(self, value=None):
        self.value = value
   
class StopGenerator_ExpressionPattern_match(StopGenerator):
    pass
    
class StopGenerator_Pattern(StopGenerator):
    pass
        
class Pattern(object):
    #@staticmethod
    #def create(expr):
    
    create = staticmethod(Pattern_create)
        
    def match(self, yield_func, expression, vars, evaluation, head=None, leaf_index=None, leaf_count=None,
        fully=True, wrap_oneid=True):
        raise NotImplementedError
    
    """def match(self, expression, vars, evaluation, head=None, leaf_index=None, leaf_count=None,
        fully=True, wrap_oneid=True):
        #raise NotImplementedError
        result = []
        def yield_func(vars, rest):
            result.append(vars, rest)
        self._match(yield_func, expression, vars, evaluation, head, leaf_index, leaf_count,
            fully, wrap_oneid)
        return result"""
    
    def does_match(self, expression, evaluation, vars=None, fully=True):
        
        if vars is None:
            vars = {}
        #for sub_vars, rest in self.match(expression, vars, evaluation, fully=fully):
        #    return True
        def yield_match(sub_vars, rest):
            raise StopGenerator_Pattern(True)
        try:
            self.match(yield_match, expression, vars, evaluation, fully=fully)
        except StopGenerator_Pattern, exc:
            return exc.value
        return False
        
    def get_name(self):
        return self.expr.get_name()
    def is_atom(self):
        return self.expr.is_atom()
    def get_head_name(self):
        return self.expr.get_head_name()
    def same(self, other):
        return self.expr.same(other.expr)
    def get_head(self):
        return self.expr.get_head()
    def get_leaves(self):
        return self.expr.get_leaves()
    def get_sort_key(self, pattern_sort=False):
        return self.expr.get_sort_key(pattern_sort=pattern_sort)
    def get_lookup_name(self):
        return self.expr.get_lookup_name()
    def get_attributes(self, definitions):
        return self.expr.get_attributes(definitions)
    def get_sequence(self):
        return self.expr.get_sequence()
    def get_option_values(self):
        return self.expr.get_option_values()
    def has_form(self, *args):
        return self.expr.has_form(*args)
    
    def get_match_candidates(self, leaves, expression, attributes, evaluation, vars={}):
        return []
    def get_match_candidates_count(self, leaves, expression, attributes, evaluation, vars={}):
        return len(self.get_match_candidates(leaves, expression, attributes, evaluation, vars))

class AtomPattern(Pattern):
    def __init__(self, expr):
        self.atom = expr
        self.expr = expr
                
    def __repr__(self):
        return '<AtomPattern: %s>' % self.atom
        
    def match(self, yield_func, expression, vars, evaluation, head=None, leaf_index=None, leaf_count=None,
        fully=True, wrap_oneid=True):
        
        if expression.same(self.atom):
            #yield vars, None
            yield_func(vars, None)
    
    def get_match_candidates(self, leaves, expression, attributes, evaluation, vars={}):
        return [leaf for leaf in leaves if leaf.same(self.atom)]
            
    def get_match_count(self, vars={}):
        return (1, 1)


#class StopGenerator_ExpressionPattern_match(StopGenerator):
#    pass

class ExpressionPattern(Pattern):
    #get_pre_choices = pattern_nocython.get_pre_choices
    #match = pattern_nocython.match
    
    def match(self, yield_func, expression, vars, evaluation, head=None, leaf_index=None, leaf_count=None, fully=True, wrap_oneid=True):
        evaluation.check_stopped()
        
        attributes = self.head.get_attributes(evaluation.definitions)
        if 'Flat' not in attributes:
            fully = True
        if not expression.is_atom():
            # don't do this here, as self.get_pre_choices changes the ordering of the leaves!
            #if self.leaves:
            #    next_leaf = self.leaves[0]
            #    next_leaves = self.leaves[1:]
            
            def yield_choice(pre_vars):
                next_leaf = self.leaves[0]
                next_leaves = self.leaves[1:]
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
                self.match_leaf(yield_func, next_leaf, next_leaves, ([], expression.leaves), pre_vars,
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
        
    
    def get_pre_choices(self, yield_func, expression, attributes, vars):
        if 'Orderless' in attributes:
            self.sort()
            patterns = self.filter_leaves('Pattern')
            groups = {}
            prev_pattern = prev_name = None
            for pattern in patterns:
                name = pattern.leaves[0].get_name()
                existing = vars.get(name, None)
                if existing is None:
                    # There's no need for pre-choices if the variable is already set.
                    if name == prev_name:
                        if name in groups:
                            groups[name].append(pattern)
                        else:
                            groups[name] = [prev_pattern, pattern]
                    prev_pattern = pattern
                    prev_name = name
            expr_groups = {}
            prev_leaf = None
            for leaf in expression.leaves:
                if leaf in expr_groups:
                    expr_groups[leaf] += 1
                else:
                    expr_groups[leaf] = 1
            
            def per_name(yield_name, groups, vars):
                " Yields possible variable settings (dictionaries) for the remaining pattern groups "
                
                if groups:
                    name, patterns = groups[0]
                    
                    match_count = [0, None]
                    for pattern in patterns:
                        sub_match_count = pattern.get_match_count()
                        if sub_match_count[0] > match_count[0]:
                            match_count[0] = sub_match_count[0]
                        if match_count[1] is None or (sub_match_count[1] is not None and sub_match_count[1] < match_count[1]):
                            match_count[1] = sub_match_count[1]
                    possibilities = [{}]
                    sum = 0
                    
                    def per_expr(yield_expr, expr_groups, sum=0):
                        """ Yields possible values (sequence lists) for the current variable (name),
                        taking into account the (expression, count)'s in expr_groups
                        """
                        
                        if expr_groups:
                            expr, count = expr_groups[0]
                            max_per_pattern = count / len(patterns)
                            for per_pattern in range(max_per_pattern, -1, -1):
                                for next in per_expr(expr_groups[1:], sum + per_pattern):
                                    yield_expr([expr] * per_pattern + next)                            
                        else:
                            if sum >= match_count[0]:
                                yield_expr([])
                             
                    #for sequence in per_expr(expr_groups.items()):
                    def yield_expr(sequence):
                        wrappings = self.get_wrappings(sequence, match_count[1], expression, attributes)
                        for wrapping in wrappings:
                            #for next in per_name(groups[1:], vars):
                            def yield_next(next):
                                setting = next.copy()
                                setting[name] = wrapping
                                yield_name(setting)
                            per_name(yield_next, groups[1:], vars)
                    per_expr(yield_expr, expr_groups.items())
                else: # no groups left
                    yield_name(vars)
            
            #for setting in per_name(groups.items(), vars):
            #def yield_name(setting):
            #    yield_func(setting)
            per_name(yield_func, groups.items(), vars)
        else:
            yield_func(vars) 
    
    
    def __init__(self, expr):
        self.head = Pattern.create(expr.head)
        self.leaves = [Pattern.create(leaf) for leaf in expr.leaves]
        self.expr = expr
        
    def filter_leaves(self, head_name):
        return [leaf for leaf in self.leaves if leaf.get_head_name() == head_name]
        
    def __repr__(self):
        return u'<ExpressionPattern: %s>' % self.expr
    
    def get_match_count(self, vars={}):
        return (1, 1)
        
    def get_wrappings(self, yield_func, items, max_count, expression, attributes, include_flattened=True):
        if len(items) == 1:
            yield_func(items[0])
        else:
            if max_count is None or len(items) <= max_count:
                if 'Orderless' in attributes:
                    for perm in permutations(items):
                        sequence = Expression('Sequence', *perm)
                        sequence.pattern_sequence = True
                        yield_func(sequence)
                else:
                    sequence = Expression('Sequence', *items)
                    sequence.pattern_sequence = True
                    yield_func(sequence)
            if 'Flat' in attributes and include_flattened:
                yield_func(Expression(expression.get_head(), *items))
        
    def match_leaf(self, yield_func, leaf, rest_leaves, rest_expression, vars, expression, attributes,
        evaluation, leaf_index=1, leaf_count=None, first=False, fully=True, depth=1, wrap_oneid=True):
        
        if rest_expression is None:
            rest_expression = ([], [])
            
        evaluation.check_stopped()
            
        match_count = leaf.get_match_count(vars)
        leaf_candidates = leaf.get_match_candidates(rest_expression[1], #leaf.candidates,
            expression, attributes, evaluation, vars)
        
        if len(leaf_candidates) < match_count[0]:
            return
        
        # STRANGE: candidate in leaf_candidates causes BusError for Real ^ Integer (e.g. 2.0^3),
        # when not converted to a set!
        leaf_candidates = set(leaf_candidates)
        
        candidates = rest_expression[1]
        
        # "Artificially" only use more leaves than specified for some kind of pattern.
        # TODO: This could be further optimized!
        try_flattened = ('Flat' in attributes) and ( \
            leaf.get_head_name() in ('Pattern', 'PatternTest', 'Condition', 'Optional',
                'Blank', 'BlankSequence', 'BlankNullSequence', 'Alternatives', 'OptionsPattern',
                'Repeated', 'RepeatedNull'))
        
        if try_flattened:
            set_lengths = (match_count[0], None)
        else:
            set_lengths = match_count
            
        # try_flattened is used later to decide whether wrapping of leaves into one operand may occur.
        # This can of course also be when flat and same head.
        try_flattened = try_flattened or (('Flat' in attributes) and leaf.get_head() == expression.head)
            
        less_first = len(rest_leaves) > 0
            
        if 'Orderless' in attributes:
            sets = None
            if leaf.get_head_name() == 'Pattern':
                varname = leaf.leaves[0].get_name()
                existing = vars.get(varname, None)
                if existing is not None:
                    head = existing.get_head()
                    if head.get_name() == 'Sequence' or ('Flat' in attributes and head == expression.get_head()):
                        needed = existing.leaves
                    else:
                        needed = [existing]
                    available = candidates[:]
                    for needed_leaf in needed:
                        if needed_leaf in available and needed_leaf in leaf_candidates:
                            available.remove(needed_leaf)
                        else:
                            return
                    sets = [(needed, ([], available))]
                   
            if sets is None:
                sets = subsets(candidates, included=leaf_candidates, less_first=less_first, *set_lengths)
        else:
            sets = subranges(candidates, flexible_start=first and not fully, included=leaf_candidates, less_first=less_first, *set_lengths)
            
        #print "Match %s in %s" % (leaf, expression)
        
        if rest_leaves:
            next_leaf = rest_leaves[0]
            next_rest_leaves = rest_leaves[1:]
        next_depth = depth + 1
        next_index = leaf_index + 1
            
        for items, items_rest in sets:
            #try:
            #print (u"  " + u", ".join(unicode(item) for item in items)).encode('utf-8')
            #except 
            # Include wrappings like Plus[a, b] only if not all items taken
            # - in that case we would match the same expression over and over again.
            
            include_flattened = try_flattened and 0 < len(items) < len(expression.leaves)
        
            # Don't try flattened when the expression would remain the same!
            
            def leaf_yield(next_vars, next_rest):
                #if next_rest is None:
                #    next_rest = ([], [])
                #yield_func(next_vars, (rest_expression[0] + items_rest[0], next_rest[1]))
                if next_rest is None:
                    yield_func(next_vars, (rest_expression[0] + items_rest[0], []))
                else:
                    yield_func(next_vars, (rest_expression[0] + items_rest[0], next_rest[1]))
                
            def match_yield(new_vars, _):
                if rest_leaves:                    
                    self.match_leaf(leaf_yield, next_leaf, next_rest_leaves, items_rest, new_vars,
                        expression, attributes, evaluation, fully=fully, depth=next_depth,
                        leaf_index=next_index, leaf_count=leaf_count, wrap_oneid=wrap_oneid)
                    #for next_vars, next_rest in recursion:
                else:
                    if not fully or (not items_rest[0] and not items_rest[1]):
                        yield_func(new_vars, items_rest)
                        #yield new_vars, items_rest
            
            #wrappings = self.get_wrappings(items, match_count[1], expression, attributes,
            #    include_flattened=include_flattened)
            #for item in wrappings:
            def yield_wrapping(item):
                
                leaf.match(match_yield, item, vars, evaluation, fully=True,
                    head=expression.head, leaf_index=leaf_index, leaf_count=leaf_count, wrap_oneid=wrap_oneid)
                    
                # Need not fully match, as in g[a+b+c+d,a] against g[x_+y_,x_].
                #for new_vars, _ in leaf.match(item, vars, evaluation, fully=True,
                #    head=expression.head, leaf_index=leaf_index, leaf_count=leaf_count, wrap_oneid=wrap_oneid):
            
            self.get_wrappings(yield_wrapping, items, match_count[1], expression, attributes,
                include_flattened=include_flattened)
                    
        
    def get_match_candidates(self, leaves, expression, attributes, evaluation, vars={}):
        """
        Finds possible leaves that could match the pattern, ignoring future pattern variable definitions,
        but taking into account already fixed variables.
        """
        # TODO: fixed_vars!
        
        return [leaf for leaf in leaves if self.does_match(leaf, evaluation, vars)]
        
    def get_match_candidates_count(self, leaves, expression, attributes, evaluation, vars={}):
        """
        Finds possible leaves that could match the pattern, ignoring future pattern variable definitions,
        but taking into account already fixed variables.
        """
        # TODO: fixed_vars!
        
        count = 0
        for leaf in leaves:
            if self.does_match(leaf, evaluation, vars):
                count += 1
        return count
        
    def sort(self):        
        self.leaves.sort(key=lambda e: e.get_sort_key(pattern_sort=True))
        
    
