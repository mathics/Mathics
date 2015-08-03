#!/usr/bin/env python
# -*- coding: utf-8 -*-

from mathics.core.expression import Expression, Symbol, strip_context
# from mathics.core.util import subsets, subranges, permutations
from mathics.core.pattern import Pattern, StopGenerator

"""_tagged_stop_generators = {}

def StopGenerator(tag):
    existing = _tagged_stop_generators.get(tag)
    if existing is not None:
        return existing

    class TaggedStopGenerator(Exception):
        def __init__(self, value=None):
            self.value = value
    TaggedStopGenerator.__name__ = 'TaggedStopGenerator_%s' % tag

    _tagged_stop_generators[tag] = TaggedStopGenerator
    return TaggedStopGenerator"""


class StopGenerator_BaseRule(StopGenerator):
    pass


class BaseRule(object):
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
            # print "Yield match %s" % vars
            if rest is None:
                rest = ([], [])
            if 0 < len(rest[0]) + len(rest[1]) == len(expression.get_leaves()):
                # continue
                return
            options = {}
            for name, value in vars.items():
                if name.startswith('_option_'):
                    options[name[len('_option_'):]] = value
                    del vars[name]
            # print sorted((name.encode('utf-8'),
            # unicode(value).encode('utf-8')) for name, value in
            # vars.iteritems())
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

            # print "Flatten"
            result = flatten(result)
            # print "Flattened"
            if return_list:
                result_list.append(result)
                # count += 1
                if max_list is not None and len(result_list) >= max_list:
                    # return result_list
                    raise StopGenerator_BaseRule(result_list)
            else:
                # return result
                # print "Return %s" % result
                # raise StopGenerator("BaseRule_apply")(result)
                # exc = ValueError()
                # exc.value = result
                # print "raise"
                # raise exc
                raise StopGenerator_BaseRule(result)

                # only first possibility counts

        try:
            self.pattern.match(
                yield_match, expression, {}, evaluation, fully=fully)
        # except StopGenerator("BaseRule_apply"), exc:
        except StopGenerator_BaseRule, exc:
            # print "exc"
            # print "Apply %s -> %s" % (self, exc.value)
            return exc.value

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
        return (u'<Rule: %s -> %s>' % (self.pattern, self.replace)).encode(
            'unicode_escape')


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
        s = u'<BuiltinRule: %s -> %s>' % (self.pattern, self.function)
        return s.encode('unicode_escape')

    def __getstate__(self):
        odict = self.__dict__.copy()
        del odict['function']
        odict['function_'] = (
            self.function.im_self.get_name(), self.function.__name__)
        return odict

    def __setstate__(self, dict):
        from mathics.builtin import builtins

        self.__dict__.update(dict)   # update attributes
        cls, name = dict['function_']

        self.function = getattr(builtins[cls], name)
