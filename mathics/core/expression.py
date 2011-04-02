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

from gmpy import mpz, mpq, mpf
import re
import cython
#import pyximport
#pyximport.install()

from mathics.core.numbers import mpcomplex, format_float, prec, get_type, dps, prec
from mathics.core.evaluation import Evaluation
from mathics.core.util import subsets, subranges, permutations, interpolate_string
from mathics.core.convert import from_sage, from_sympy, ConvertSubstitutions, sage_symbol_prefix, sympy_symbol_prefix, \
    SympyExpression

import operator

try:
    from sage import all as sage
    from sage.calculus.calculus import var as sage_var, function as sage_function
except (ImportError, RuntimeError):
    pass

import sympy

builtin_evaluation = Evaluation()

class BoxError(Exception):
    def __init__(self, box, form):
        super(BoxError, self).__init__('Box %s cannot be formatted as %s' % (box, form))
        self.box = box
        self.form = form

class NumberError(Exception):
    def __init__(self, value):
        super(NumberError, self).__init__()

class ExpressionPointer(object):
    def __init__(self, parent, position):
        self.parent = parent
        self.position = position
        
    def replace(self, new):
        if self.position == 0:
            self.parent.head = new
        else:
            self.parent.leaves[self.position - 1] = new
            
    def __str__(self):
        return u'%s[[%s]]' % (self.parent, self.position)
    
def from_python(arg):
    number_type = get_type(arg)
    if isinstance(arg, int) or isinstance(arg, long) or number_type == 'z':
        return Integer(arg)
    elif isinstance(arg, float) or number_type == 'f':
        return Real(arg)
    elif isinstance(arg, basestring):
        return String(arg)
    elif isinstance(arg, BaseExpression):
        return arg
    else:
        raise NotImplementedError

class BaseExpression(object):
    def __init__(self, *args, **kwargs):
        super(BaseExpression, self).__init__()
        
        self.options = None
        
        # For parsing purposes (could in fact be stored only for parsed expressions):    
        self.parenthesized = False
        
        self.pattern_sequence = False
        
        self.unformatted = self
        
    def get_attributes(self, definitions):
        return set()
    
    def evaluate(self, evaluation=builtin_evaluation):
        evaluation.check_stopped()
        return self
    
    def get_atoms(self, include_heads=True):
        return []
    
    def get_name(self):
        " Returns symbol's name if Symbol instance "
        
        return ''
    
    def is_symbol(self):
        return False
    
    def get_lookup_name(self):
        " Returns symbol name of leftmost head "
        
        return self.get_name()
    
    def get_head(self):
        return None
    
    def get_head_name(self):
        return self.get_head().get_name()
    
    def get_leaves(self):
        return []
    
    def get_int_value(self):
        return None
    
    def get_real_value(self):
        return None
    
    def get_string_value(self):
        return None
    
    def is_atom(self):
        return False
        
    def is_true(self):
        return False
    
    def is_numeric(self):
        # used by NumericQ and expression ordering
        return False
    
    def flatten(self, head, pattern_only=False, callback=None):
        return self
    
    def __hash__(self):
        " To allow usage of expression as dictionary keys, as in Expression.get_pre_choices "
        
        return hash(unicode(self))
    
    def __cmp__(self, other):
        return cmp(self.get_sort_key(), other.get_sort_key())
    
    def same(self, other):
        pass
    
    def is_sequence(self):
        return self.get_head_name() == 'Sequence'
    
    def get_sequence(self):
        if self.get_head().get_name() == 'Sequence':
            return self.leaves
        else:
            return [self]
    
    def evaluate_leaves(self, evaluation):
        return self
                    
    def apply_rules(self, rules, evaluation):
        for rule in rules:
            result = rule.apply(self, evaluation, fully=False)
            if result is not None:
                return result, True
        return self, False
    
    def do_format(self, evaluation, form):
        formats = ('InputForm', 'OutputForm', 'StandardForm', 'FullForm', 'TraditionalForm', 'TeXForm', 'MathMLForm')
            
        evaluation.inc_recursion_depth()
        try:
            expr = self
            head = self.get_head_name()
            include_form = False
            if head in formats and len(self.get_leaves()) == 1:
                expr = self.leaves[0]
                if not (form == 'OutputForm' and head == 'StandardForm'):
                    form = head
                    
                    include_form = True
            unformatted = expr
                
            def format_expr(expr):
                name = expr.get_lookup_name()
                formats = evaluation.definitions.get_formats(name, form)
                for rule in formats:
                    result = rule.apply(expr, evaluation)
                    if result is not None and result != expr:
                        #print rule
                        return result.evaluate(evaluation)
                return None
                
            if form != 'FullForm':
                formatted = format_expr(expr)
                if formatted is not None:
                    result = formatted.do_format(evaluation, form)
                    if include_form:
                        result = Expression(form, result)
                    result.unformatted = unformatted
                    return result
                
                head = expr.get_head_name()
                if head in formats:
                    expr = expr.do_format(evaluation, form)
                elif head != 'FullForm' and not expr.is_atom() and head != 'Graphics':
                    new_leaves = [leaf.do_format(evaluation, form) for leaf in expr.leaves]
                    expr = Expression(expr.head.do_format(evaluation, form), *new_leaves)
                    
            if include_form:
                expr = Expression(form, expr)
            expr.unformatted = unformatted
            return expr
        finally:
            evaluation.dec_recursion_depth()
    
    def format(self, evaluation, form):
        expr = self.do_format(evaluation, form)
        result = Expression('MakeBoxes', expr, Symbol(form)).evaluate(evaluation)
        return result
            
    def is_free(self, form, evaluation):
        from mathics.core.pattern import StopGenerator
        
        class StopGenerator_BaseExpression_is_free(StopGenerator):
            pass
        
        #for vars, rest in form.match(self, {}, evaluation, fully=False):
        def yield_match(vars, rest):
            raise StopGenerator_BaseExpression_is_free(False)
            #return False
        try:
            form.match(yield_match, self, {}, evaluation, fully=False)
        except StopGenerator_BaseExpression_is_free, exc:
            return exc.value
        if self.is_atom():
            return True
        else:
            return self.head.is_free(form, evaluation) and all(leaf.is_free(form, evaluation) for leaf in self.leaves)
    
    def post_parse(self):
        return self
        
    def is_inexact(self):
        return self.get_precision() is not None
    
    def get_precision(self):
        return None
        
    def get_option_values(self, evaluation, allow_symbols=False, stop_on_error=True):
        options = self
        if options.has_form('List', None):
            options = options.flatten(Symbol('List'))
            values = options.leaves
        else:
            values = [options]
        option_values = {}
        for option in values:
            symbol_name = option.get_name()
            if allow_symbols and symbol_name:
                options = evaluation.definitions.get_options(symbol_name)
                option_values.update(options)
            else:
                if not option.has_form(('Rule', 'RuleDelayed'), 2):
                    if stop_on_error:
                        return None
                    else:
                        continue
                name = option.leaves[0].get_name()
                if not name:
                    if stop_on_error:
                        return None
                    else:
                        continue
                option_values[name] = option.leaves[1]
        return option_values
    
    def get_rules_list(self):
        from mathics.core.rules import Rule
        
        list_expr = self.flatten(Symbol('List'))
        list = []
        if list_expr.has_form('List', None):
            list.extend(list_expr.leaves)
        else:
            list.append(list_expr)
        rules = []
        for item in list:
            if not item.has_form(('Rule', 'RuleDelayed'), 2):
                return None
            rule = Rule(item.leaves[0], item.leaves[1])
            rules.append(rule)
        return rules
    
    def to_number(self, min=None, max=None, n_evaluation=None):
        if n_evaluation is not None:
            value = Expression('N', self).evaluate(n_evaluation)
        else:
            value = self
        number = value.get_real_value()
        if number is None:
            raise NumberError(self)
        if min is not None and number < min:
            number = min
        if max is not None and number > max:
            number = max
        return float(number)

class Monomial(object):
    """
    An object to sort monomials, used in Expression.get_sort_key and Symbol.get_sort_key.
    """
    
    def __init__(self, exps_dict):
        self.exps = exps_dict
        
    def __cmp__(self, other):
        return self.__cmp(other)
        
    def __cmp(self, other):
        self_exps = self.exps.copy()
        other_exps = other.exps.copy()
        for var in self.exps:
            if var in other.exps:
                dec = min(self_exps[var], other_exps[var])
                self_exps[var] -= dec
                if not self_exps[var]:
                    del self_exps[var]
                other_exps[var] -= dec
                if not other_exps[var]:
                    del other_exps[var]
        self_exps = sorted((var, exp) for var, exp in self_exps.iteritems())
        other_exps = sorted((var, exp) for var, exp in other_exps.iteritems())
        
        index = 0
        self_len = len(self_exps)
        other_len = len(other_exps)
        while True:
            if index >= self_len and index >= other_len:
                return 0
            if index >= self_len:
                return -1   # self < other
            if index >= other_len:
                return 1    # self > other
            self_var, self_exp = self_exps[index]
            other_var, other_exp = other_exps[index]
            var_cmp = cmp(self_var, other_var)
            if var_cmp != 0:
                return var_cmp
            if self_exp != other_exp:
                if index + 1 == self_len or index + 1 == other_len:
                    return cmp(self_exp, other_exp)    # smaller exponents first
                else:
                    return -cmp(self_exp, other_exp)    # bigger exponents first
            index += 1
        return 0
    
class Expression(BaseExpression):
    def __init__(self, head, *leaves, **kwargs):
        super(Expression, self).__init__(**kwargs)
        if isinstance(head, basestring):
            head = Symbol(head)
        self.head = head
        self.leaves = [from_python(leaf) for leaf in leaves]
        
        self.parse_operator = kwargs.get('parse_operator')
        
    def copy(self):
        result = Expression(self.head.copy(), *[leaf.copy() for leaf in self.leaves])
        result.options = self.options
        result.original = self
        return result
    
    def set_positions(self, position=None):
        self.position = position
        self.head.set_positions(ExpressionPointer(self, 0))
        for index, leaf in enumerate(self.leaves):
            leaf.set_positions(ExpressionPointer(self, index + 1))
        
    def get_head(self):
        return self.head
    
    def get_leaves(self):
        return self.leaves
    
    def get_lookup_name(self):
        return self.head.get_lookup_name()
    
    def has_form(self, heads, *leaf_counts):
        """
        leaf_counts:
            (,):        no leaves allowed
            (None,):    no constraint on number of leaves
            (n, None):  leaf count >= n
            (n1, n2, ...):    leaf count in {n1, n2, ...}
        """
        
        head_name = self.head.get_name()
        if isinstance(heads, (tuple, list, set)):
            if not head_name in heads:
                return False
        else:
            if head_name != heads:
                return False
        if not leaf_counts:
            return False
        if leaf_counts and leaf_counts[0] != None:
            count = len(self.leaves)
            if count not in leaf_counts:
                if len(leaf_counts) == 2 and leaf_counts[1] is None and count >= leaf_counts[0]:
                    return True
                else:
                    return False 
        return True
    
    def has_symbol(self, symbol_name):
        return self.head.has_symbol(symbol_name) or any(leaf.has_symbol(symbol_name) for leaf in self.leaves)
    
    def to_sympy(self):
        from mathics.builtin import mathics_to_sympy
        
        lookup_name = self.get_lookup_name()
        builtin = mathics_to_sympy.get(lookup_name)
        if builtin is not None:
            sympy_expr = builtin.to_sympy(self)
            if sympy_expr is not None:
                return sympy_expr
            
        return SympyExpression(self)
    
    def to_sage(self, definitions, subs):
        from mathics.builtin import mathics_to_sage
        
        lookup_name = self.get_lookup_name()
        builtin = mathics_to_sage.get(lookup_name)
        if builtin is not None:
            sage_expr = builtin.to_sage(self, definitions, subs)
            if sage_expr is not None:
                return sage_expr
                
        head_name = self.head.get_name()
        
        attributes = self.head.get_attributes(definitions)
        if (set(('HoldAll', 'HoldAllComplete', 'HoldFirst', 'HoldRest')) & attributes) or not head_name:
            return subs.substitute(self).to_sage(definitions, subs)
        
        if head_name == 'Plus':
            op = sage.add
        elif head_name == 'Times':
            op = sage.mul
        elif head_name == 'Power' and len(self.leaves) == 2:
            return pow(self.leaves[0].to_sage(definitions, subs), self.leaves[1].to_sage(definitions, subs))
        elif head_name == 'List':
            return [leaf.to_sage(definitions, subs) for leaf in self.leaves]
        else:
            sage_func = sage_function(sage_symbol_prefix + head_name)
            args = [leaf.to_sage(definitions, subs) for leaf in self.leaves]
            return sage_func(*args)
        return op([leaf.to_sage(definitions, subs) for leaf in self.leaves])
    
    def get_sort_key(self, pattern_sort=False):
        
        if pattern_sort:
            """
            Pattern sort key structure:
            0: 0/2:        Atom / Expression
            1: pattern:    0 / 11-31 for blanks / 1 for empty Alternatives / 40 for OptionsPattern
            2: 0/1:        0 for PatternTest
            3: 0/1:        0 for Pattern
            4: 0/1:        1 for Optional
            5: head / 0 for atoms
            6: leaves / 0 for atoms
            7: 0/1:        0 for Condition
            """
            
            name = self.head.get_name()
            pattern = 0
            if name == 'Blank':
                pattern = 1
            elif name == 'BlankSequence':
                pattern = 2
            elif name == 'BlankNullSequence':
                pattern = 3
            if pattern > 0:
                if self.leaves:
                    pattern += 10
                else:
                    pattern += 20
            if pattern > 0:
                return [2, pattern, 1, 1, 0, self.head.get_sort_key(True), [leaf.get_sort_key(True) for leaf in self.leaves], 1]
                    
            if name == 'PatternTest':
                if len(self.leaves) != 2:
                    return [3, 0, 0, 0, 0, self.head, self.leaves, 1]
                sub = self.leaves[0].get_sort_key(True)
                sub[2] = 0
                return sub
            elif name == 'Condition':
                if len(self.leaves) != 2:
                    return [3, 0, 0, 0, 0, self.head, self.leaves, 1]
                sub = self.leaves[0].get_sort_key(True)
                sub[7] = 0
                return sub
            elif name == 'Pattern':
                if len(self.leaves) != 2:
                    return [3, 0, 0, 0, 0, self.head, self.leaves, 1]
                sub = self.leaves[1].get_sort_key(True)
                sub[3] = 0
                return sub
            elif name == 'Optional':
                if len(self.leaves) not in (1, 2):
                    return [3, 0, 0, 0, 0, self.head, self.leaves, 1]
                sub = self.leaves[0].get_sort_key(True)
                sub[4] = 1
                return sub
            elif name == 'Alternatives':
                min_key = [4]
                min = None
                for leaf in self.leaves:
                    key = leaf.get_sort_key(True)
                    if key < min_key:
                        min = leaf
                        min_key = key
                if min is None:
                    return [2, 1]   # empty alternatives -> very restrictive pattern
                return min_key
            elif name == 'Verbatim':
                if len(self.leaves) != 1:
                    return [3, 0, 0, 0, 0, self.head, self.leaves, 1]
                return self.leaves[0].get_sort_key(True)
            elif name == 'OptionsPattern':
                return [2, 40, 0, 1, 1, 0, self.head, self.leaves, 1]
            else:
                # Append (4,) to leaves so that longer expressions have higher precedence
                result = [2, 0, 1, 1, 0, self.head.get_sort_key(True), [leaf.get_sort_key(True) for leaf in self.leaves] + [(4,)], 1]
                return result
        else:
            exps = {}
            head = self.head.get_name()
            if head == 'Times':
                for leaf in self.leaves:
                    name = leaf.get_name()
                    if leaf.has_form('Power', 2):
                        var = leaf.leaves[0].get_name()
                        exp = leaf.leaves[1].get_real_value()
                        if var and exp is not None:
                            exps[var] = exps.get(var, 0) + exp
                    elif name:
                        exps[name] = exps.get(name, 0) + 1
            elif self.has_form('Power', 2):
                var = self.leaves[0].get_name()
                exp = self.leaves[1].get_real_value()
                if var and exp is not None:
                    exps[var] = exps.get(var, 0) + exp
            if exps:
                return [1 if self.is_numeric() else 2, 2, Monomial(exps), 1, self.head, self.leaves, 1]
            else:
                return [1 if self.is_numeric() else 2, 3, self.head, self.leaves, 1]
    
    def same(self, other):
        #print u"Same? %s == %s" % (self, other)
        if self.get_head_name() != other.get_head_name():
            #print "Not same head name"
            return False
        #print "Test head"
        if not self.head.same(other.get_head()):
            #print "Not same head"
            return False
        #print "Test length"
        if len(self.leaves) != len(other.get_leaves()):
            #print "Unequal length"
            return False
        #print "Test leaves"
        for leaf, other in zip(self.leaves, other.get_leaves()):
            if not leaf.same(other):
                return False
        #print "Return"
        return True
    
    def flatten(self, head, pattern_only=False, callback=None, level=None):
        if level is not None and level <= 0:
            return self
        sub_level = None if level is None else level - 1
        do_flatten = False
        for leaf in self.leaves:
            if leaf.get_head() == head and (not pattern_only or leaf.pattern_sequence):
                do_flatten = True
                break
        if do_flatten:
            new_leaves = []
            for leaf in self.leaves:
                if leaf.get_head() == head and (not pattern_only or leaf.pattern_sequence):
                    new_leaf = leaf.flatten(head, pattern_only, callback, level=sub_level)
                    if callback is not None:
                        callback(new_leaf.leaves, leaf)
                    new_leaves.extend(new_leaf.leaves)
                else:
                    new_leaves.append(leaf)
            return Expression(self.head, *new_leaves)
        else:
            return self
        
    def evaluate(self, evaluation=builtin_evaluation):
        #print "Eval %s" % self
        evaluation.inc_recursion_depth()
        old_options = evaluation.options
        if hasattr(self, 'options') and self.options:
            evaluation.options = self.options
        try:
            head = self.head.evaluate(evaluation)
            attributes = head.get_attributes(evaluation.definitions)
            leaves = self.leaves[:]
            if 'HoldAll' in attributes or 'HoldAllComplete' in attributes:
                eval_range = []
            elif 'HoldFirst' in attributes:
                eval_range = range(1, len(leaves))
            elif 'HoldRest' in attributes:
                if len(leaves) > 0:
                    eval_range = [0]
                else:
                    eval_range = []
            else:
                eval_range = range(len(leaves))
                
            if 'HoldAllComplete' not in attributes:
                for index, leaf in enumerate(self.leaves):
                    if leaf.has_form('Evaluate', 1) and index not in eval_range:
                        eval_range.append(index)
            eval_range.sort()
            #print "Eval leaves"
            for index in eval_range:
                if not leaves[index].has_form('Unevaluated', 1):
                    leaves[index] = leaves[index].evaluate(evaluation)
            #print "Leaves evaluated"
            
            new = Expression(head, *leaves)
            if 'SequenceHold' not in attributes and 'HoldAllComplete' not in attributes:
                new = new.flatten(Symbol('Sequence'))
            leaves = new.leaves
            
            #print "New constructed"
                
            for leaf in leaves:
                leaf.unevaluated = False
            if not 'HoldAllComplete' in attributes:
                for index, leaf in enumerate(leaves):
                    if leaf.has_form('Unevaluated', 1):
                        leaves[index] = leaf.leaves[0]
                        leaves[index].unevaluated = True
            
            def flatten_callback(new_leaves, old):
                for leaf in new_leaves:
                    leaf.unevaluated = old.unevaluated
            
            #print "Second new"
            new = Expression(head, *leaves)
            if 'Flat' in attributes:
                new = new.flatten(new.head, callback=flatten_callback)
            if 'Orderless' in attributes:
                new.sort()
                
            if 'Listable' in attributes:
                done, threaded = new.thread(evaluation)
                if done:
                    if not threaded.same(new):
                        threaded = threaded.evaluate(evaluation)
                    return threaded
            rules = []
            rules_names = set()
            if not 'HoldAllComplete' in attributes:
                for leaf in leaves:
                    name = leaf.get_lookup_name()
                    if name not in rules_names:
                        rules_names.add(name)
                        rules.extend(evaluation.definitions.get_upvalues(name))
            lookup_name = new.get_lookup_name()
            if lookup_name == new.get_head_name():
                rules += evaluation.definitions.get_downvalues(lookup_name)
            else:
                rules += evaluation.definitions.get_subvalues(lookup_name)
            #print u"Process rules %s\non %s" % (rules, new)
            for rule in rules:
                #print u"  %s" % rule
                result = rule.apply(new, evaluation, fully=False)
                #print u"applied"
                #print u"  -> %s" % result
                if result is not None:
                    #print "Test if same"
                    if not result.same(new):
                        #print "Re-evaluate"
                        result = result.evaluate(evaluation)
                    #print u"Return applied %s" % result
                    return result
            
            # Expression did not change, re-apply Unevaluated
            for index, leaf in enumerate(new.leaves):
                if leaf.unevaluated:
                    new.leaves[index] = Expression('Unevaluated', leaf)
            
            new.unformatted = self.unformatted
            #print "Return new %s" % new
            return new
            
        finally:
            evaluation.options = old_options
            evaluation.dec_recursion_depth()
            #print "Reset evaluation"
    
    def post_parse(self):
        if self.parse_operator is not None:
            return self.parse_operator.post_parse(self)
        else:
            return Expression(self.head.post_parse(), *[leaf.post_parse() for leaf in self.leaves])
    
    def evaluate_leaves(self, evaluation):
        leaves = [leaf.evaluate(evaluation) for leaf in self.leaves]
        head = self.head.evaluate_leaves(evaluation)
        return Expression(head, *leaves)
    
    def __str__(self):
        return u'%s[%s]' % (self.head, u', '.join([unicode(leaf) for leaf in self.leaves]))
    
    def __repr__(self):
        return u'<Expression: %s>' % self
    
    def process_style_box(self, options):
        if self.has_form('StyleBox', 1, None):
            rules = self.leaves[1:]
            for rule in rules:
                if rule.has_form('Rule', 2):
                    name = rule.leaves[0].get_name()
                    value = rule.leaves[1]
                    if name == 'ShowStringCharacters':
                        value = value.is_true()
                        options = options.copy()
                        options['show_string_characters'] = value
                    elif name == 'ImageSizeMultipliers':
                        if value.has_form('List', 2):
                            m1 = value.leaves[0].get_real_value()
                            m2 = value.leaves[1].get_real_value()
                            if m1 is not None and m2 is not None:
                                options = options.copy()
                                options['image_size_multipliers'] = (m1, m2)
            return True, options
        else:
            return False, options
    
    def boxes_to_text(self, **options):
        from mathics.builtin import box_constructs
        
        is_style, options = self.process_style_box(options)
        if is_style:
            return self.leaves[0].boxes_to_text(**options)
        head = self.head.get_name()
        box_construct = box_constructs.get(head)
        if box_construct is not None:
            return box_construct.boxes_to_text(self.leaves, **options)
        if self.has_form('RowBox', 1) and self.leaves[0].has_form('List', None):
            return ''.join([leaf.boxes_to_text(**options) for leaf in self.leaves[0].leaves])
        else:
            raise BoxError(self, 'text')
            
    def boxes_to_xml(self, **options):
        from mathics.builtin import box_constructs
        from mathics.builtin.base import BoxConstructError
        
        is_style, options = self.process_style_box(options)
        if is_style:
            return self.leaves[0].boxes_to_xml(**options)
        head = self.head.get_name()
        box_construct = box_constructs.get(head)
        if box_construct is not None:
            try:
                return box_construct.boxes_to_xml(self.leaves, **options)
            except BoxConstructError:
                raise BoxError(self, 'xml')
        name = self.head.get_name()
        if name == 'RowBox' and len(self.leaves) == 1 and self.leaves[0].get_head_name() == 'List':
            result = []
            inside_row = options.get('inside_row')
            inside_list = options.get('inside_list')
            options = options.copy()  
            
            def is_list_interior(content):     
                if content.has_form('List', None) and all(leaf.get_string_value() == ',' for leaf in content.leaves[1::2]):
                    return True
                return False
            
            is_list_row = False
            if len(self.leaves[0].leaves) == 3 and self.leaves[0].leaves[0].get_string_value() == '{' and \
                self.leaves[0].leaves[2].get_string_value() == '}' and self.leaves[0].leaves[1].has_form('RowBox', 1):
                content = self.leaves[0].leaves[1].leaves[0]    
                if is_list_interior(content):
                    is_list_row = True
            
            if not inside_row and is_list_interior(self.leaves[0]):
                is_list_row = True
            
            if is_list_row:
                options['inside_list'] = True
            else:
                options['inside_row'] = True
                
            for leaf in self.leaves[0].get_leaves():
                result.append(leaf.boxes_to_xml(**options))
            return '<mrow>%s</mrow>' % ' '.join(result) 
        else:
            options = options.copy()
            options['inside_row'] = True
            if name == 'SuperscriptBox' and len(self.leaves) == 2:
                return '<msup>%s %s</msup>' % (self.leaves[0].boxes_to_xml(**options), self.leaves[1].boxes_to_xml(**options))
            if name == 'SubscriptBox' and len(self.leaves) == 2:
                return '<msub>%s %s</msub>' % (self.leaves[0].boxes_to_xml(**options), self.leaves[1].boxes_to_xml(**options))
            if name == 'SubsuperscriptBox' and len(self.leaves) == 3:
                return '<msubsup>%s %s %s</msubsup>' % (self.leaves[0].boxes_to_xml(**options), 
                    self.leaves[1].boxes_to_xml(**options), self.leaves[2].boxes_to_xml(**options))
            elif name == 'FractionBox' and len(self.leaves) == 2:
                return '<mfrac>%s %s</mfrac>' % (self.leaves[0].boxes_to_xml(**options), self.leaves[1].boxes_to_xml(**options))
            elif name == 'SqrtBox' and len(self.leaves) == 1:
                return '<msqrt>%s</msqrt>' % self.leaves[0].boxes_to_xml(**options)
            else:
                raise BoxError(self, 'xml')
            
    def boxes_to_tex(self, **options):
        from mathics.builtin import box_constructs
        
        def block(tex, only_subsup=False):
            if len(tex) == 1:
                return tex
            else:
                if not only_subsup or '_' in tex or '^' in tex:
                    return '{%s}' % tex
                else:
                    return tex
            
        is_style, options = self.process_style_box(options)
        if is_style:
            return self.leaves[0].boxes_to_tex(**options)
        head = self.head.get_name()
        box_construct = box_constructs.get(head)
        if box_construct is not None:
            return box_construct.boxes_to_tex(self.leaves, **options)
        name = self.head.get_name()
        if name == 'RowBox' and len(self.leaves) == 1 and self.leaves[0].get_head_name() == 'List':
            return ''.join([leaf.boxes_to_tex(**options) for leaf in self.leaves[0].get_leaves()])
        elif name == 'SuperscriptBox' and len(self.leaves) == 2:
            tex1 = self.leaves[0].boxes_to_tex(**options)
            sup_string = self.leaves[1].get_string_value()
            if sup_string == u'\u2032':
                return "%s'" % tex1
            elif sup_string == u'\u2032\u2032':
                return "%s''" % tex1
            else:
                return '%s^%s' % (block(tex1, True), block(self.leaves[1].boxes_to_tex(**options)))
        elif name == 'SubscriptBox' and len(self.leaves) == 2:
            return '%s_%s' % (block(self.leaves[0].boxes_to_tex(**options), True), block(self.leaves[1].boxes_to_tex(**options)))
        elif name == 'SubsuperscriptBox' and len(self.leaves) == 3:
            return '%s_%s^%s' % (block(self.leaves[0].boxes_to_tex(**options), True),
                block(self.leaves[1].boxes_to_tex(**options)),
                block(self.leaves[2].boxes_to_tex(**options)))
        elif name == 'FractionBox' and len(self.leaves) == 2:
            return '\\frac{%s}{%s}' % (self.leaves[0].boxes_to_tex(**options), self.leaves[1].boxes_to_tex(**options))
        elif name == 'SqrtBox' and len(self.leaves) == 1:
            return '\\sqrt{%s}' % self.leaves[0].boxes_to_tex(**options)
        else:
            raise BoxError(self, 'tex')
    
    def default_format(self, evaluation, form):
        return '%s[%s]' % (self.head.default_format(evaluation, form),
            ', '.join([leaf.default_format(evaluation, form) for leaf in self.leaves]))
    
    def sort(self, pattern=False):
        " Sort the leaves according to internal ordering. "
        
        if pattern:
            self.leaves.sort(key=lambda e: e.get_sort_key(pattern_sort=True))
        else:
            self.leaves.sort()
        
    def filter_leaves(self, head_name):
        # TODO: should use sorting
        
        return [leaf for leaf in self.leaves if leaf.get_head_name() == head_name]
                    
    def apply_rules(self, rules, evaluation):
        """for rule in rules:
            result = rule.apply(self, evaluation, fully=False)
            if result is not None:
                return result"""
        result, applied = super(Expression, self).apply_rules(rules, evaluation)
        if applied:
            return result, True
        head, applied = self.head.apply_rules(rules, evaluation)
        
        new_applied = [applied] # to be able to access it inside inner function
        def apply_leaf(leaf):
            new, sub_applied = leaf.apply_rules(rules, evaluation)
            new_applied[0] = new_applied[0] or sub_applied
            return new
        
        return Expression(head, *[apply_leaf(leaf) for leaf in self.leaves]), new_applied[0]
                    
    def replace_vars(self, vars, options=None, in_scoping=True, in_function=True):
        from mathics.builtin.scoping import get_scoping_vars
        
        if not in_scoping:
            if self.head.get_name() in ('Module', 'Block', 'With') and len(self.leaves) > 0:
                scoping_vars = set(name for name, new_def in get_scoping_vars(self.leaves[0]))
                """for var in new_vars:
                    if var in scoping_vars:
                        del new_vars[var]"""
                vars = dict((var, value) for var, value in vars.items() if var not in scoping_vars)
        
        leaves = self.leaves
        if in_function:
            if self.head.get_name() == 'Function' and len(self.leaves) > 1 and (self.leaves[0].has_form('List', None) or self.leaves[0].get_name()):
                if self.leaves[0].get_name():
                    func_params = [self.leaves[0].get_name()]
                else:
                    func_params = [leaf.get_name() for leaf in self.leaves[0].leaves]
                if '' not in func_params:
                    body = self.leaves[1]
                    replacement = dict((name, Symbol(name + '$')) for name in func_params)
                    func_params = [Symbol(name + '$') for name in func_params]
                    body = body.replace_vars(replacement, options, in_scoping)
                    leaves = [Expression('List', *func_params)] + [body] + self.leaves[2:]
                            
        return Expression(self.head.replace_vars(vars, options=options, in_scoping=in_scoping),
            *[leaf.replace_vars(vars, options=options, in_scoping=in_scoping) for leaf in leaves])
    
    def replace_slots(self, slots, evaluation):
        if self.head.get_name() == 'Slot':
            if len(self.leaves) != 1:
                evaluation.message_args('Slot', len(self.leaves), 1)
            else:
                slot = self.leaves[0].get_int_value()
                if slot is None or slot < 1:
                    evaluation.message('Function', 'slot', self.leaves[0])
                elif slot > len(slots):
                    evaluation.message('Function', 'slotn', slot)
                else:
                    return slots[int(slot - 1)]
        elif self.head.get_name() == 'SlotSequence':
            if len(self.leaves) != 1:
                evaluation.message_args('SlotSequence', len(self.leaves), 1)
            else:
                slot = self.leaves[0].get_int_value()
                if slot is None or slot < 1:
                    evaluation.error('Function', 'slot', self.leaves[0])
            return Expression('Sequence', *slots[slot-1:])
        return Expression(self.head.replace_slots(slots, evaluation),
            *[leaf.replace_slots(slots, evaluation) for leaf in self.leaves])

    def thread(self, evaluation, head=None):
        if head is None:
            head = Symbol('List')
            
        items = []
        dim = None
        for leaf in self.leaves:
            if leaf.get_head() == head:
                if dim is None:
                    dim = len(leaf.leaves)
                    items = [(items + [leaf]) for leaf in leaf.leaves]
                elif len(leaf.leaves) != dim:
                    evaluation.message('Thread', 'tdlen')
                    return True, self
                else:
                    for index in range(dim):
                        items[index].append(leaf.leaves[index])
            else:
                if dim is None:
                    items.append(leaf)
                else:
                    for item in items:
                        item.append(leaf)
        if dim is None:
            return False, self
        else:
            leaves = [Expression(self.head, *item) for item in items]
            return True, Expression(head, *leaves)
        
    def is_numeric(self):        
        return self.head.get_name() in ('Sqrt', 'Times', 'Plus', 'Subtract', 'Minus', 'Power', 'Abs', 'Divide', 'Sin') and \
            all(leaf.is_numeric() for leaf in self.leaves)
        # TODO: complete list of numeric functions, or access NumericFunction attribute
        
    def numerify(self, evaluation):
        prec = None
        for leaf in self.leaves:
            if leaf.is_inexact():
                leaf_prec = leaf.get_precision()
                if prec is None or leaf_prec < prec:
                    prec = leaf_prec
        if prec is not None:
            new_leaves = self.leaves[:]
            for index in range(len(self.leaves)):
                leaf = self.leaves[index]
                # Don't "numerify" numbers: they should be numerified automatically by the
                # processing function, and we don't want to lose exactness in e.g. 1.0+I.
                if not isinstance(leaf, Number):
                    n_expr = Expression('N', leaf, Real(dps(prec))) 
                    new_leaves[index] = n_expr.evaluate(evaluation) 
            return Expression(self.head, *new_leaves)
        else:
            return self
        
    def get_atoms(self, include_heads=True):
        if include_heads:
            atoms = self.head.get_atoms()
        else:
            atoms = []
        for leaf in self.leaves:
            atoms.extend(leaf.get_atoms())
        return atoms

class Atom(BaseExpression):    
    
    def is_atom(self):
        return True
    
    def has_form(self, heads, *leaf_counts):
        if leaf_counts:
            return False
        name = self.get_atom_name()
        if isinstance(heads, tuple):
            return name in heads
        else:
            return heads == name
        
    def has_symbol(self, symbol_name):
        return False
    
    def get_head(self):
        return Symbol(self.get_atom_name())
    
    def get_atom_name(self):
        return self.__class__.__name__
    
    def __repr__(self):
        return u'<%s: %s>' % (self.get_atom_name(), self)
    
    def replace_vars(self, vars, options=None, in_scoping=True):
        return self
    
    def replace_slots(self, slots, evaluation):
        return self
    
    def round(self, prec):
        return self
    
    def numerify(self, evaluation):
        return self
    
    def copy(self):
        result = self.do_copy()
        result.original = self
        return result

    def set_positions(self, position=None):
        self.position = position
        
    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return [0, 0, 1, 1, 0, 0, 0, 1]
        else:
            raise NotImplementedError
        
    def get_atoms(self, include_heads=True):
        return [self]
    
class Symbol(Atom):        
    def __init__(self, name, sympy_dummy=None, **kwargs):
        super(Symbol, self).__init__(**kwargs)
        self.name = name
        self.sympy_dummy = sympy_dummy
        
    def __str__(self):
        return self.name
    
    def do_copy(self):
        return Symbol(self.name)
    
    def to_sage(self, defintions, subs):
        from mathics.builtin import mathics_to_sage
        
        builtin = mathics_to_sage.get(self.name)
        if builtin is None or not builtin.sage_name:
            # sage.integrate runs endlessly when called with unicode variables!
            return sage_var(sage_symbol_prefix + self.name.encode('utf8'))
        else:
            return getattr(sage, builtin.sage_name)
        
    def to_sympy(self):
        from mathics.builtin import mathics_to_sympy
        
        if self.sympy_dummy is not None:
            return self.sympy_dummy
        
        builtin = mathics_to_sympy.get(self.name)
        if builtin is None or not builtin.sympy_name:
            return sympy.Symbol(sympy_symbol_prefix + self.name.encode('utf8'))
        else:
            return getattr(sympy, builtin.sympy_name)
    
    def default_format(self, evaluation, form):
        return self.name
    
    def get_attributes(self, definitions):
        return definitions.get_attributes(self.name)
    
    def get_name(self):
        return self.name
    
    def is_symbol(self):
        return True
    
    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Symbol, self).get_sort_key(True)
        else:
            return [1 if self.is_numeric() else 2, 2, Monomial({self.name: 1}), 0, self.name, 1]
    
    def same(self, other):
        return isinstance(other, Symbol) and self.name == other.name
    
    def replace_vars(self, vars, options={}, in_scoping=True):
        var = vars.get(self.name, None)
        if var is None:
            return self
        else:
            return var
        
    def has_symbol(self, symbol_name):
        return self.name == symbol_name
        
    def evaluate(self, evaluation=builtin_evaluation):
        rules = evaluation.definitions.get_ownvalues(self.name)
        for rule in rules:
            result = rule.apply(self, evaluation, fully=True)
            if result is not None and result != self:
                return result.evaluate(evaluation)
        return self
    
    def is_true(self):
        return self.name == 'True'
    
    def is_numeric(self):
        return self.name in ('Pi', 'E', 'EulerGamma', 'GoldenRatio', 'MachinePrecision', 'Catalan')
        
class Number(Atom):
    def __str__(self):
        return str(self.value)
    
    @staticmethod
    def from_string(value):
        if 'I' in value:
            return Complex(value)
        elif '.' in value:
            return Real(value)
        elif '/' in value:
            return Rational(value)
        else:
            return Integer(value)
    
    @staticmethod
    def from_mp(value):
        t = get_type(value)
        if t == 'z':
            return Integer(value)
        elif t == 'q':
            if value.denom() == 1:
                return Integer(value.numer())
            else:
                return Rational(value)
        elif t == 'f':
            return Real(value)
        elif t == 'c':
            return Complex(value)
        else:
            raise TypeError('Unknown number type: %s (type %s)' % (value, type(value)))
        
    def is_numeric(self):
        return True
            
def number_boxes(text):
    assert text is not None
    if text.endswith('.0'):
        text = text[:-1]
    if text.startswith('-'):
        return Expression('RowBox', Expression('List', String('-'), String(text[1:])))
    else:
        return String(text)

class Integer(Number):
    def __init__(self, value, **kwargs):
        super(Integer, self).__init__(**kwargs)
        self.value = mpz(str(value))
        
    def __getstate__(self):
        # pickling of mpz sometimes failes...
        return {'value': str(self.value)}
    
    def __setstate__(self, dict):
        self.value = mpz(dict['value'])
        
    def boxes_to_text(self, **options):
        return str(self.value)
    
    def boxes_to_xml(self, **options):
        return self.make_boxes('MathMLForm').boxes_to_xml(**options)
    
    def boxes_to_tex(self, **options):
        return str(self.value)
        
    def make_boxes(self, form):
        return number_boxes(str(self.value))
        
    def default_format(self):
        return str(self.value)
        
    def to_sage(self, definitions, subs):
        return sage.Integer(str(self.value))
    
    def to_sympy(self):
        return sympy.Integer(int(self.value))
        
    def get_int_value(self):
        return self.value
    
    def same(self, other):
        return isinstance(other, Integer) and self.value == other.value
    
    def evaluate(self, evaluation=builtin_evaluation):
        evaluation.check_stopped()
        return self
    
    def round(self, precision):
        return Real(mpf(self.value, precision))
    
    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Integer, self).get_sort_key(True)
        else:
            # HACK: otherwise "Bus error" when comparing 1==1.
            return [0, 0, mpf(self.value), 0, 1]
    
    def get_real_value(self):
        # BUG in gmpy: mpz(1) < mpq(1, 3)
        # => convert integers to rationals
        return mpq(self.value, 1)
    
    def do_copy(self):
        return Integer(self.value)

class Rational(Number):
    def __init__(self, numerator, denominator=None, **kwargs):
        super(Rational, self).__init__(**kwargs)
        if denominator is None:
            self.value = mpq(numerator)
        else:
            self.value = mpq(numerator, denominator)
        
    def __getstate__(self):
        # pickling of mpz sometimes failes...
        return {'value': str(self.value)}
    
    def __setstate__(self, dict):
        self.value = mpq(dict['value'])
        
    def to_sage(self, definitions, subs):
        return sage.Rational((str(self.value.numer()), str(self.value.denom())))
    
    def to_sympy(self):
        return sympy.Rational(int(self.value.numer()), int(self.value.denom()))
    
    def same(self, other):
        return isinstance(other, Rational) and self.value == other.value
    
    def numerator(self):
        return Number.from_mp(self.value.numer())
    
    def denominator(self):
        return Number.from_mp(self.value.denom())
    
    def do_format(self, evaluation, form):
        if form == 'FullForm':
            return Expression(Expression('HoldForm', Symbol('Rational')), self.numerator(), self.denominator()).do_format(evaluation, form)
        else:
            numerator = self.numerator()
            minus = numerator.value < 0
            if minus:
                numerator = Integer(-numerator.value)
            result = Expression('Divide', numerator, self.denominator())
            if minus:
                result = Expression('Minus', result)
            result = Expression('HoldForm', result)
            return result.do_format(evaluation, form)
    
    def default_format(self):
        return 'Rational[%s, %s]' % (self.value.numer(), self.value.denom())
    
    def evaluate(self, evaluation=builtin_evaluation):
        evaluation.check_stopped()
        return self
    
    def round(self, precision):
        return Real(mpf(self.value, precision))
    
    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Rational, self).get_sort_key(True)
        else:
            # HACK: otherwise "Bus error" when comparing 1==1.
            return [0, 0, mpf(self.value), 0, 1]
    
    def get_real_value(self):
        return self.value
    
    def do_copy(self):
        return Rational(self.value)
        
class Real(Number):
    def __init__(self, value):
        super(Real, self).__init__()
        if isinstance(value, basestring):
            value = str(value)
            p = prec(len(value))
            value = mpf(value, p)
        else:
            type = get_type(value)
            if type == 'q':
                value = mpf(str(value.numer())) / mpf(str(value.denom()))
            elif get_type(value) != 'f':
                value = mpf(str(value))
        self.value = value
        
    def __getstate__(self):
        # pickling of mpz sometimes failes...
        p = self.value.getprec()
        s = self.value.digits(10, dps(p) + 5, -5, 6)
        return {'value': s, 'prec': p}
        #return {'value': str(self.value)}
    
    def __setstate__(self, dict):
        p = dict['prec']
        self.value = mpf(dict['value'], p)
        
    def boxes_to_text(self, **options):
        return self.make_boxes('OutputForm').value
    
    def boxes_to_xml(self, **options):
        return self.make_boxes('MathMLForm').boxes_to_xml(**options)
        
    def boxes_to_tex(self, **options):
        return self.make_boxes('TeXForm').boxes_to_tex(**options)  
        
    def make_boxes(self, form):
        s = self.value.digits(10, dps(self.value.getprec()), -5, 6)
        s = s.split('e')
        if len(s) == 2:
            man, exp = s
            man = Real(man).make_boxes(form) #.get_string_value()
            assert man is not None
            if form in ('InputForm', 'OutputForm', 'FullForm'):
                return Expression('RowBox', Expression('List', man, String('*^'), String(exp)))
            else:
                return Expression('RowBox', Expression('List', man, String(u'\u00d7'),
                    Expression('SuperscriptBox', String('10'), String(exp))))
        else:
            assert len(s) == 1
            return number_boxes(s[0])
        
    def to_sage(self, definitions, subs):
        return sage.RealNumber(self.value.digits(10, dps(self.value.getprec()))) #(str(self.value))
    
    def to_sympy(self):
        return sympy.Real(self.value.digits(10, dps(self.value.getprec())))
    
    def same(self, other):
        return isinstance(other, Real) and self.value == other.value
    
    def evaluate(self, evaluation=builtin_evaluation):
        evaluation.check_stopped()
        return self      
    
    def round(self, precision):
        return Real(self.value.round(precision))
    
    def get_precision(self):
        return self.value.getprec()
    
    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Real, self).get_sort_key(True)
        else:
            digits, exp, prec = self.value.digits(10, 0, 0, 0, 2)
            count = dps(prec)
            digits = digits[:count-3]
            if digits.startswith('-'):
                value = digits[:1] + '.' + digits[1:]
            else:
                value = digits[:0] + '.' + digits[0:]
            sort_value = mpf('%se%d' % (value, exp))
            return [0, 0, sort_value, 0, 1]
    
    def get_real_value(self):
        return self.value
    
    def do_copy(self):
        return Real(self.value)
    
class Complex(Number):
    def __init__(self, real, imag=None, **kwargs):
        super(Complex, self).__init__(**kwargs)
        if imag is None:
            self.value = mpcomplex(real)
        else:
            self.value = mpcomplex(real, imag)
        
    def to_sage(self, definitions, subs):
        real = Number.from_mp(self.value.real)
        imag = Number.from_mp(self.value.imag)
        return real.to_sage(definitions, subs) + imag.to_sage(definitions, subs) * sage.I
    
    def to_sympy(self):
        real = Number.from_mp(self.value.real)
        imag = Number.from_mp(self.value.imag)
        return real.to_sympy() + imag.to_sympy() * sympy.I
    
    def do_format(self, evaluation, form):
        real = Number.from_mp(self.value.real)
        imag = Number.from_mp(self.value.imag)
        
        if form == 'FullForm':
            return Expression(Expression('HoldForm', Symbol('Complex')), real, imag).do_format(evaluation, form)

        sum = []
        if not real.same(Integer(0)):
            sum.append(real)
        if imag.same(Integer(1)):
            sum.append(Symbol('I'))
        else:
            sum.append(Expression('Times', imag, Symbol('I')))
        if len(sum) == 1:
            sum = sum[0]
        else:
            sum = Expression('Plus', *sum)                        
        return Expression('HoldForm', sum).do_format(evaluation, form)
    
    def default_format(self, evaluation, form):
        real = Number.from_mp(self.value.real)
        imag = Number.from_mp(self.value.imag)
        return 'Complex[%s, %s]' % (real.default_format(evaluation, form), imag.default_format(evaluation, form))
    
    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Complex, self).get_sort_key(True)
        else:
            real = Real(self.value.real)
            imag = Real(self.value.imag)
            return [0, 0, real.get_sort_key()[2], imag.get_sort_key()[2], 1]
    
    def same(self, other):
        return isinstance(other, Complex) and self.value == other.value
    
    def evaluate(self, evaluation=builtin_evaluation):
        evaluation.check_stopped()
        return self
        
    def round(self, precision):
        real = Number.from_mp(self.value.real)
        imag = Number.from_mp(self.value.imag)
        return Complex(real.round(precision).value, imag.round(precision).value)
    
    def get_precision(self):
        real = Number.from_mp(self.value.real)
        imag = Number.from_mp(self.value.imag)
        real_prec = real.get_precision()
        imag_prec = imag.get_precision()
        if real_prec is None:
            return imag_prec
        elif imag_prec is None:
            return real_prec
        else:
            return min(imag_prec, real_prec)
    
    def do_copy(self):
        return Complex(self.value)
    
def encode_mathml(text):
    text = text.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;').replace('"', '&quot;')
    #.replace('\n', '&NewLine;')
    text = text.replace(' ', '&nbsp;')
    text = text.replace('\n', '<mspace linebreak="newline" />')
    return text

TEX_REPLACE = {
    #'\\': r'\text{$\backslash$}',
    '{': r'\{',
    '}': r'\}',
    #'^': r'\text{${}^{\wedge}$}',
    '_': r'\_',
    '$': r'\$',
    #'~': r'\text{$\sim$}',
    #'|': r'\text{$\vert$}',
    '%': r'\%',
    #'<': r'\text{$<$}',
    #'>': r'\text{$>$}',
    '#': r'\#',
    '&': r'\&',
    '\\': r'\backslash{}',
    '^': r'{}^{\wedge}',
    '~': r'\sim{}',
    '|': r'\vert{}',
}
TEX_TEXT_REPLACE = TEX_REPLACE.copy()
TEX_TEXT_REPLACE.update({
    '<': r'$<$',
    '>': r'$>$',
    '~': r'$\sim$',
    '|': r'$\vert$',
    '\\': r'$\backslash$',
    '^': r'${}^{\wedge}$',
})  
TEX_REPLACE_RE = re.compile('([' + ''.join([re.escape(c) for c in TEX_REPLACE]) + '])')
        
def encode_tex(text, in_text=False):
    def replace(match):
        c = match.group(1)
        repl = TEX_TEXT_REPLACE if in_text else TEX_REPLACE 
        #return TEX_REPLACE[c]
        return repl.get(c, c)
    
    return TEX_REPLACE_RE.sub(replace, text)

extra_operators = set((',', '(', ')', '[', ']', '{', '}', u'\u301a', u'\u301b', u'\u00d7', u'\u2032',
    u'\u2032\u2032', ' ', u'\u2062', u'\u222b', u'\u2146'))
        
class String(Atom):
    def __init__(self, value, **kwargs):
        super(String, self).__init__(**kwargs)        
        self.value = value
        
    def __str__(self):
        return u'"%s"' % self.value
        
    def boxes_to_text(self, show_string_characters=False, **options):
        value = self.value
        if not show_string_characters and value.startswith('"') and value.endswith('"'):
            value = value[1:-1]
        return value
    
    def boxes_to_xml(self, show_string_characters=False, **options):
        from mathics.core.parser import is_symbol_name
        from mathics.builtin import builtins
        
        operators = set()
        for name, builtin in builtins.iteritems():
            operator = builtin.get_operator_display()
            if operator is not None:
                operators.add(operator)
                
        text = self.value
                
        if text.startswith('"') and text.endswith('"'):
            if show_string_characters:
                return '<ms>%s</ms>' % encode_mathml(text[1:-1])
            else:
                return '<mtext>%s</mtext>' % encode_mathml(text[1:-1])
        elif text and ('0' <= text[0] <= '9' or text[0] == '.'):
            return '<mn>%s</mn>' % encode_mathml(text)
        else:
            if text in operators or text in extra_operators:
                if text == u'\u2146':
                    return '<mo form="prefix" lspace="0.2em" rspace="0">%s</mo>' % encode_mathml(text)
                return '<mo>%s</mo>' % encode_mathml(text)
            elif is_symbol_name(text):
                return '<mi>%s</mi>' % encode_mathml(text)
            else:
                return '<mtext>%s</mtext>' % encode_mathml(text)
    
    def boxes_to_tex(self, show_string_characters=False, **options):
        from mathics.core.parser import is_symbol_name
        from mathics.builtin import builtins
        
        operators = set()
        for name, builtin in builtins.iteritems():
            operator = builtin.get_operator_display()
            if operator is not None:
                operators.add(operator)
                
        text = self.value
                
        if text.startswith('"') and text.endswith('"'):
            if show_string_characters:
                return r'\text{"%s"}' % encode_tex(text[1:-1], in_text=True)
            else:
                return r'\text{%s}' % encode_tex(text[1:-1], in_text=True)
        elif text and ('0' <= text[0] <= '9' or text[0] == '.'):
            return encode_tex(text)
        else:
            if text == u'\u2032':
                return "'"
            elif text == u'\u2032\u2032':
                return "''"
            elif text == u'\u2062':
                return ' '
            elif text == u'\u221e':
                return r'\infty '
            elif text == u'\u00d7':
                return r'\times '
            elif text in ('(', '[', '{'):
                return r'\left%s' % encode_tex(text)
            elif text in (')', ']', '}'):
                return r'\right%s' % encode_tex(text)
            elif text == u'\u301a':
                return r'\left[\left['
            elif text == u'\u301b':
                return r'\right]\right]'
            elif text == ',' or text == ', ':
                return text
            elif text == u'\u222b':
                return r'\int'
            elif text == u'\u2146':
                return r'\, d'
            elif len(text) > 1:
                return r'\text{%s}' % encode_tex(text, in_text=True)
            else:
                return encode_tex(text)
    
    def do_copy(self):
        return String(self.value)
        
    def default_format(self, evaluation, form):
        value = self.value.replace('\\', '\\\\').replace('"', '\\"')
        return u'"%s"' % value
        
    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(String, self).get_sort_key(True)
        else:
            return [0, 1, self.value, 0, 1]
    
    def same(self, other):
        return isinstance(other, String) and self.value == other.value
    
    def get_string_value(self):
        return self.value
    
    def to_sage(self, definitions, subs):
        return subs.substitute(self, definitions)
    
    def to_sympy(self):
        return self.value
    
def get_default_value(name, evaluation, k=None, n=None):
    pos = []
    if k is not None:
        pos.append(k)
    if n is not None:
        pos.append(n)
    for pos_len in reversed(range(len(pos) + 1)):
        # Try patterns from specific to general
        defaultexpr = Expression('Default', Symbol(name),
            *[Integer(index) for index in pos[:pos_len]])
        result = evaluation.definitions.get_value(name, 'DefaultValues', defaultexpr, evaluation)
        if result is not None:
            if result.same(defaultexpr):
                result = result.evaluate(evaluation)
            return result
    return None
    
def print_parenthesizes(precedence, outer_precedence=None, parenthesize_when_equal=False):
    return outer_precedence is not None and (outer_precedence > precedence or (outer_precedence == precedence and parenthesize_when_equal))
