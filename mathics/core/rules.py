#!/usr/bin/env python3
# cython: language_level=3
# -*- coding: utf-8 -*-


from mathics.core.expression import Expression, strip_context, KeyComparable
from mathics.core.pattern import Pattern, StopGenerator
from mathics.core.util import function_arguments

from itertools import chain


class StopGenerator_BaseRule(StopGenerator):
    pass


class BaseRule(KeyComparable):
    def __init__(self, pattern, system=False) -> None:
        self.pattern = Pattern.create(pattern)
        self.system = system

    def apply(
        self, expression, evaluation, fully=True, return_list=False, max_list=None
    ):
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
                if name.startswith("_option_"):
                    options[name[len("_option_") :]] = value
                    del vars[name]
            new_expression = self.do_replace(expression, vars, options, evaluation)
            if new_expression is None:
                new_expression = expression
            if rest[0] or rest[1]:
                result = Expression(
                    expression.get_head(),
                    *list(chain(rest[0], [new_expression], rest[1]))
                )
            else:
                result = new_expression

            # Flatten out sequences (important for Rule itself!)
            result = result.flatten_pattern_sequence(evaluation)
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
            self.pattern.match(yield_match, expression, {}, evaluation, fully=fully)
        except StopGenerator_BaseRule as exc:
            return exc.value

        if return_list:
            return result_list
        else:
            return None

    def get_sort_key(self):
        return (self.system, self.pattern.get_sort_key(True))


class Rule(BaseRule):
    def __init__(self, pattern, replace, system=False) -> None:
        super(Rule, self).__init__(pattern, system=system)
        self.replace = replace

    def do_replace(self, expression, vars, options, evaluation):
        new = self.replace.replace_vars(vars)
        new.options = options

        # if options is a non-empty dict, we need to ensure reevaluation of the whole expression, since 'new' will
        # usually contain one or more matching OptionValue[symbol_] patterns that need to get replaced with the
        # options' values. this is achieved through Expression.evaluate(), which then triggers OptionValue.apply,
        # which in turn consults evaluation.options to return an option value.

        # in order to get there, we copy 'new' using copy(reevaluate=True), as this will ensure that the whole thing
        # will get reevaluated.

        # if the expression contains OptionValue[] patterns, but options is empty here, we don't need to act, as the
        # expression won't change in that case. the Expression.options would be None anyway, so OptionValue.apply
        # would just return the unchanged expression (which is what we have already).

        if options:
            new = new.copy(reevaluate=True)

        return new

    def __repr__(self) -> str:
        return "<Rule: %s -> %s>" % (self.pattern, self.replace)


class BuiltinRule(BaseRule):
    def __init__(self, name, pattern, function, check_options, system=False) -> None:
        super(BuiltinRule, self).__init__(pattern, system=system)
        self.name = name
        self.function = function
        self.check_options = check_options
        self.pass_expression = "expression" in function_arguments(function)

    def do_replace(self, expression, vars, options, evaluation):
        if options and self.check_options:
            if not self.check_options(options, evaluation):
                return None
        # The Python function implementing this builtin expects
        # argument names corresponding to the symbol names without
        # context marks.
        vars_noctx = dict(((strip_context(s), vars[s]) for s in vars))
        if self.pass_expression:
            vars_noctx["expression"] = expression
        if options:
            return self.function(evaluation=evaluation, options=options, **vars_noctx)
        else:
            return self.function(evaluation=evaluation, **vars_noctx)

    def __repr__(self) -> str:
        return "<BuiltinRule: %s -> %s>" % (self.pattern, self.function)

    def __getstate__(self):
        odict = self.__dict__.copy()
        del odict["function"]
        odict["function_"] = (self.function.__self__.get_name(), self.function.__name__)
        return odict

    def __setstate__(self, dict):
        from mathics.builtin import builtins

        self.__dict__.update(dict)  # update attributes
        cls, name = dict["function_"]

        self.function = getattr(builtins[cls], name)
