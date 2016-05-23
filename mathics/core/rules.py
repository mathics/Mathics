#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

import six

from mathics.core.expression import Expression, Symbol, strip_context, KeyComparable
from mathics.core.pattern import Pattern, StopGenerator


class StopGenerator_BaseRule(StopGenerator):
    pass


class BaseRule(KeyComparable):
    def __init__(self, pattern, system=False):
        super(BaseRule, self).__init__()
        self.pattern = Pattern.create(pattern)
        self.system = system

    def apply(self, expression, evaluation, fully=True, return_list=False,
              max_list=None):
        result_list = []
        # count = 0

        if return_list and max_list is not None and max_list <= 0:
            return []

        def yield_match(vars, rest):
            if rest is None:
                rest = ([], [])
            if 0 < len(rest[0]) + len(rest[1]) == len(expression.get_leaves()):
                # continue
                return
            options = {}
            for name, value in list(vars.items()):
                if name.startswith('_option_'):
                    options[name[len('_option_'):]] = value
                    del vars[name]
            new_expression = self.do_replace(vars, options, evaluation)
            if new_expression is None:
                new_expression = expression
            if rest[0] or rest[1]:
                result = Expression(expression.get_head(), *(
                    rest[0] + [new_expression] + rest[1]))
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
                # count += 1
                if max_list is not None and len(result_list) >= max_list:
                    # return result_list
                    raise StopGenerator_BaseRule(result_list)
            else:
                raise StopGenerator_BaseRule(result)

                # only first possibility counts

        try:
            self.pattern.match(
                yield_match, expression, {}, evaluation, fully=fully)
        except StopGenerator_BaseRule as exc:
            return exc.value

        if return_list:
            return result_list
        else:
            return None

    def get_sort_key(self):
        return (self.system, self.pattern.get_sort_key(True))


class Rule(BaseRule):
    def __init__(self, pattern, replace, system=False):
        super(Rule, self).__init__(pattern, system=system)
        self.replace = replace

    def do_replace(self, vars, options, evaluation):
        new = self.replace.replace_vars(vars)
        new.options = options
        if len(options) > 0:  # need to trigger (re)evaluation?
            new = new.copy()  # clear is_evaluated for whole tree
        return new

    def __repr__(self):
        return '<Rule: %s -> %s>' % (self.pattern, self.replace)


class BuiltinRule(BaseRule):
    def __init__(self, pattern, function, system=False):
        super(BuiltinRule, self).__init__(pattern, system=system)
        self.function = function

    def do_replace(self, vars, options, evaluation):
        # The Python function implementing this builtin expects
        # argument names corresponding to the symbol names without
        # context marks.
        vars_noctx = dict(((strip_context(s), vars[s]) for s in vars))
        if options:
            return self.function(
                evaluation=evaluation, options=options, **vars_noctx)
        else:
            return self.function(evaluation=evaluation, **vars_noctx)

    def __repr__(self):
        return '<BuiltinRule: %s -> %s>' % (self.pattern, self.function)

    def __getstate__(self):
        odict = self.__dict__.copy()
        del odict['function']
        odict['function_'] = (
            self.function.__self__.get_name(), self.function.__name__)
        return odict

    def __setstate__(self, dict):
        from mathics.builtin import builtins

        self.__dict__.update(dict)   # update attributes
        cls, name = dict['function_']

        self.function = getattr(builtins[cls], name)
