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
        
class Pattern(object):
    @staticmethod
    def create(expr):
        from mathics.builtin import pattern_objects
        
        name = expr.get_head_name()
        pattern_object = pattern_objects.get(name)
        if pattern_object is not None:
            return pattern_object(expr)
        if expr.is_atom():
            return AtomPattern(expr)
        else:
            return ExpressionPattern(expr)
    
    def match(self, expression, vars, evaluation, head=None, leaf_index=None, leaf_count=None, fully=True, wrap_oneid=True):
        raise NotImplementedError
    
    def does_match(self, expression, evaluation, vars=None, fully=True):
        if vars is None:
            vars = {}
        for sub_vars, rest in self.match(expression, vars, evaluation, fully=fully):
            return True
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
        
class AtomPattern(Pattern):
    def __init__(self, expr):
        self.atom = expr
        self.expr = expr
                
    def __repr__(self):
        return '<AtomPattern: %s>' % self.atom
        
    def match(self, expression, vars, evaluation, head=None, leaf_index=None, leaf_count=None,
        fully=True, wrap_oneid=True):
        
        if expression.same(self.atom):
            yield vars, None
    
    def get_match_candidates(self, leaves, expression, attributes, evaluation, vars={}):
        result = [leaf for leaf in leaves if leaf.same(self.atom)]
        return result
            
    def get_match_count(self, vars={}):
        return (1, 1)

class ExpressionPattern(Pattern):
    def __init__(self, expr):
        self.head = Pattern.create(expr.head)
        self.leaves = [Pattern.create(leaf) for leaf in expr.leaves]
        self.expr = expr
        
    def filter_leaves(self, head_name):
        return [leaf for leaf in self.leaves if leaf.get_head_name() == head_name]
        
    def __repr__(self):
        return '<ExpressionPattern: %s>' % self.expr
    
    def get_match_count(self, vars={}):
        return (1, 1)
        
    def get_wrappings(self, items, max_count, expression, attributes, include_flattened=True):
        if len(items) == 1:
            yield items[0]
        else:
            if max_count is None or len(items) <= max_count:
                if 'Orderless' in attributes:
                    for perm in permutations(items):
                        sequence = Expression('Sequence', *perm)
                        sequence.pattern_sequence = True
                        yield sequence
                else:
                    sequence = Expression('Sequence', *items)
                    sequence.pattern_sequence = True
                    yield sequence
            if 'Flat' in attributes and include_flattened:
                yield Expression(expression.get_head(), *items)
        
    def match_leaf(self, leaf, rest_leaves, rest_expression, vars, expression, attributes,
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
            
        for items, items_rest in sets:
            # Include wrappings like Plus[a, b] only if not all items taken
            # - in that case we would match the same expression over and over again.
            
            include_flattened = try_flattened and 0 < len(items) < len(expression.leaves)
        
            # Don't try flattened when the expression would remain the same!
            
            wrappings = self.get_wrappings(items, match_count[1], expression, attributes,
                include_flattened=include_flattened)
            for item in wrappings:
                # Need not fully match, as in g[a+b+c+d,a] against g[x_+y_,x_].
                for new_vars, _ in leaf.match(item, vars, evaluation, fully=True,
                    head=expression.head, leaf_index=leaf_index, leaf_count=leaf_count, wrap_oneid=wrap_oneid):
                    if rest_leaves:
                        recursion = self.match_leaf(rest_leaves[0], rest_leaves[1:], items_rest, new_vars,
                            expression, attributes, evaluation, fully=fully, depth=depth+1,
                            leaf_index=leaf_index+1, leaf_count=leaf_count, wrap_oneid=wrap_oneid)
                        for next_vars, next_rest in recursion:
                            if next_rest is None:
                                next_rest = ([], [])
                            yield next_vars, (rest_expression[0] + items_rest[0], next_rest[1])
                    else:
                        if not fully or (not items_rest[0] and not items_rest[1]):
                            yield new_vars, items_rest
        
    def get_match_candidates(self, leaves, expression, attributes, evaluation, vars={}):
        """
        Finds possible leaves that could match the pattern, ignoring future pattern variable definitions,
        but taking into account already fixed variables.
        """
        # TODO: fixed_vars!
        
        return [leaf for leaf in leaves if self.does_match(leaf, evaluation, vars)]
        
    def sort(self):        
        self.leaves.sort(key=lambda e: e.get_sort_key(pattern_sort=True))
        
    def get_pre_choices(self, expression, attributes, vars):
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
            
            def per_name(groups, vars):
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
                    
                    def per_expr(expr_groups, sum=0):
                        """ Yields possible values (sequence lists) for the current variable (name),
                        taking into account the (expression, count)'s in expr_groups
                        """
                        
                        if expr_groups:
                            expr, count = expr_groups[0]
                            max_per_pattern = count / len(patterns)
                            for per_pattern in range(max_per_pattern, -1, -1):
                                for next in per_expr(expr_groups[1:], sum + per_pattern):
                                    yield [expr] * per_pattern + next                            
                        else:
                            if sum >= match_count[0]:
                                yield []
                             
                    for sequence in per_expr(expr_groups.items()):
                        wrappings = self.get_wrappings(sequence, match_count[1], expression, attributes)
                        for wrapping in wrappings:
                            for next in per_name(groups[1:], vars):
                                setting = next.copy()
                                setting[name] = wrapping
                                yield setting
                else: # no groups left
                    yield vars
            
            for setting in per_name(groups.items(), vars):
                yield setting
        else:
            yield vars   
    
    def match(self, expression, vars, evaluation, head=None, leaf_index=None, leaf_count=None, fully=True, wrap_oneid=True):
        evaluation.check_stopped()
        
        attributes = self.head.get_attributes(evaluation.definitions)
        if 'Flat' not in attributes:
            fully = True
        if not expression.is_atom():
            for head_vars, _ in self.head.match(expression.get_head(), vars, evaluation):
                if self.leaves:
                    pre_choices = self.get_pre_choices(expression, attributes, head_vars)
                    for pre_vars in pre_choices:
                        for leaf in self.leaves:
                            match_count = leaf.get_match_count()
                            candidates = leaf.get_match_candidates(expression.leaves, expression, attributes, evaluation, pre_vars)
                            if len(candidates) < match_count[0]:
                                return
                        for new_vars, rest in self.match_leaf(self.leaves[0], self.leaves[1:], ([], expression.leaves), pre_vars,
                            expression, attributes, evaluation, first=True, fully=fully, leaf_count=len(self.leaves),
                            wrap_oneid=expression.get_head_name() != 'MakeBoxes'):
                            yield new_vars, rest
                else:
                    if not expression.leaves:
                        yield head_vars, None
                    else:
                        return
        if wrap_oneid and 'OneIdentity' in attributes and expression.get_head() != self.head and expression != self.head: # and 'OneIdentity' not in (expression.get_attributes(evaluation.definitions) | expression.get_head().get_attributes(evaluation.definitions)):
            new_expression = Expression(self.head, expression)
            for leaf in self.leaves:
                leaf.match_count = leaf.get_match_count()
                leaf.candidates = [expression] #leaf.get_match_candidates(new_expression.leaves, new_expression, attributes, evaluation, vars)
                if len(leaf.candidates) < leaf.match_count[0]:
                    return
            for new_vars, rest in self.match_leaf(self.leaves[0], self.leaves[1:], ([], [expression]), vars,
                new_expression, attributes, evaluation, first=True, fully=fully, leaf_count=len(self.leaves), wrap_oneid=True):
                yield new_vars, rest
                    

        
class BaseRule(object):
    def __init__(self, pattern, system=False):
        super(BaseRule, self).__init__()
        self.pattern = Pattern.create(pattern)
        self.system = system
    
    def apply(self, expression, evaluation, fully=True, return_list=False, max_list=None):
        result_list = []
        count = 0
        
        if return_list and max_list is not None and max_list <= 0:
            return []
        
        for vars, rest in self.pattern.match(expression, {}, evaluation, fully=fully):
            if rest is None:
                rest = ([], [])
            if 0 < len(rest[0]) + len(rest[1]) == len(expression.get_leaves()):
                continue
            options = {}
            for name, value in vars.items():
                if name.startswith('_option_'):
                    options[name[len('_option_'):]] = value
                    del vars[name]
            new_expression = self.do_replace(vars, options, evaluation)
            if new_expression is None:
                new_expression = expression
            if rest[0] or rest[1]:
                result = Expression(expression.get_head(), *(rest[0] + [new_expression] + rest[1]))
            else:
                result = new_expression
            # Flatten out sequences (important for Rule itself!)
            
            def flatten(expr):
                new_expr = expr.flatten(Symbol('Sequence'), pattern_only=True)
                if not new_expr.is_atom():
                    for index, leaf in enumerate(new_expr.leaves):
                        new_expr.leaves[index] = flatten(leaf)
                if hasattr(expr, 'options'):
                    new_expr.options = expr.options
                return new_expr
            
            result = flatten(result)
            if return_list:
                result_list.append(result)
                count += 1
                if max_list is not None and count >= max_list:
                    return result_list
            else:
                return result
            
            # only first possibility counts
            
        if return_list:
            return result_list
        else:
            return None
    
    def __cmp__(self, other):
        if other is None:
            # None is not equal to any rule
            return -1
        return cmp((self.system, self.pattern.get_sort_key(True)),
            (other.system, other.pattern.get_sort_key(True)))

class Rule(BaseRule):
    def __init__(self, pattern, replace, system=False):
        super(Rule, self).__init__(pattern, system=system)
        self.replace = replace
    
    def do_replace(self, vars, options, evaluation):
        new = self.replace.replace_vars(vars)
        new.options = options
        return new
    
    def __repr__(self):
        return '<Rule: %s -> %s>' % (self.pattern, self.replace)
    
class BuiltinRule(BaseRule):
    def __init__(self, pattern, function, system=False):
        super(BuiltinRule, self).__init__(pattern, system=system)
        self.function = function
        
    def do_replace(self, vars, options, evaluation):
        if options:
            return self.function(evaluation=evaluation, options=options, **vars)
        else:
            return self.function(evaluation=evaluation, **vars)
    
    def __repr__(self):
        return '<BuiltinRule: %s -> %s>' % (self.pattern, self.function)
    
    def __getstate__(self):
        odict = self.__dict__.copy()
        del odict['function']
        odict['function_'] = (self.function.im_self.get_name(), self.function.__name__)
        return odict

    def __setstate__(self, dict):
        from mathics.builtin import builtins
        
        self.__dict__.update(dict)   # update attributes
        cls, name = dict['function_']
        
        self.function = getattr(builtins[cls], name)
