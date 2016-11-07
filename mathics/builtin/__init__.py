#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

from mathics.builtin import (
    algebra, arithmetic, assignment, attributes, calculus, combinatorial, compilation,
    comparison, control, datentime, diffeqns, evaluation, exptrig, functional,
    graphics, graphics3d, image, inout, integer, linalg, lists, logic, manipulate, natlang, numbertheory,
    numeric, options, patterns, plot, physchemdata, randomnumbers, recurrence,
    specialfunctions, scoping, strings, structure, system, tensors, xmlformat)

from mathics.builtin.base import (
    Builtin, SympyObject, BoxConstruct, Operator, PatternObject)

from mathics.settings import ENABLE_FILES_MODULE

modules = [
    algebra, arithmetic, assignment, attributes, calculus, combinatorial, compilation,
    comparison, control, datentime, diffeqns, evaluation, exptrig, functional,
    graphics, graphics3d, image, inout, integer, linalg, lists, logic, manipulate, natlang, numbertheory,
    numeric, options, patterns, plot, physchemdata, randomnumbers, recurrence,
    specialfunctions, scoping, strings, structure, system, tensors, xmlformat]

if ENABLE_FILES_MODULE:
    from mathics.builtin import files, importexport
    modules += [files, importexport]

builtins = []
builtins_by_module = {}


def is_builtin(var):
    if var == Builtin:
        return True
    if hasattr(var, '__bases__'):
        return any(is_builtin(base) for base in var.__bases__)
    return False

for module in modules:
    builtins_by_module[module.__name__] = []
    vars = dir(module)
    for name in vars:
        var = getattr(module, name)
        if (hasattr(var, '__module__') and
            var.__module__.startswith('mathics.builtin.') and
            var.__module__ != 'mathics.builtin.base' and
            is_builtin(var) and not name.startswith('_') and
            var.__module__ == module.__name__):     # nopep8

            instance = var(expression=False)

            if isinstance(instance, Builtin):
                builtins.append((instance.get_name(), instance))
                builtins_by_module[module.__name__].append(instance)


# builtins = dict(builtins)

mathics_to_sympy = {}
sympy_to_mathics = {}

box_constructs = {}
pattern_objects = {}
builtins_precedence = {}


def add_builtins(new_builtins):
    for var_name, builtin in new_builtins:
        name = builtin.get_name()
        if isinstance(builtin, SympyObject):
            mathics_to_sympy[name] = builtin
            for sympy_name in builtin.get_sympy_names():
                sympy_to_mathics[sympy_name] = builtin
        if isinstance(builtin, BoxConstruct):
            box_constructs[name] = builtin
        if isinstance(builtin, Operator):
            builtins_precedence[name] = builtin.precedence
        if isinstance(builtin, PatternObject):
            pattern_objects[name] = builtin.__class__
    builtins.update(dict(new_builtins))

new_builtins = builtins
builtins = {}
add_builtins(new_builtins)


def get_module_doc(module):
    doc = module.__doc__
    if doc is not None:
        doc = doc.strip()
    if doc:
        title = doc.splitlines()[0]
        text = '\n'.join(doc.splitlines()[1:])
    else:
        title = module.__name__
        for prefix in ('mathics.builtin.', 'mathics.optional.'):
            if title.startswith(prefix):
                title = title[len(prefix):]
        title = title.capitalize()
        text = ''
    return title, text


inout_hooks = {'$PreRead':"$PreRead is a global variable whose value, if set, is applied to the \
text or box form of every input expression before it is fed to the parser. (Not implemented yet) ",
               '$Pre': "$Pre is a global variable whose value, if set, is applied to every \
input expression.",
               '$Post': "$Post is a global variable whose value, if set, is applied to every \
output expression. ",
               '$PrePrint': "$PrePrint is a global variable whose value, if set, is applied to \
every expression before it is printed. ",
               '$SyntaxHandler': "$SyntaxHandler is a global variable which, if set, is applied to any \
               input string that is found to contain a syntax error. (Not implemented yet) ",
           }

def load_InOut_hooks(definitions):
    from mathics.core.definitions import Definition
    from mathics.core.expression import  Symbol, String
    from mathics.core.rules import Rule
    from mathics.core.pattern import Pattern_create
    from mathics.builtin.inout import MessageName
    for hook in inout_hooks:
        fullname = "System`" + hook
        definitions.builtin[fullname] = Definition(name = fullname )
        usage = Rule(MessageName(Symbol(fullname), String("usage")), String(inout_hooks[hook]))
        definitions.builtin[fullname].set_values_list('messages',[usage])

def contribute(definitions):
    # let MakeBoxes contribute first
    builtins['System`MakeBoxes'].contribute(definitions)
    for name, item in builtins.items():
        if name != 'System`MakeBoxes':
            item.contribute(definitions)

    load_InOut_hooks(definitions)
    
    from mathics.core.expression import ensure_context
    from mathics.core.parser import all_operator_names
    from mathics.core.definitions import Definition

    # All builtins are loaded. Create dummy builtin definitions for
    # any remaining operators that don't have them. This allows
    # operators like \[Cup] to behave correctly.
    for operator in all_operator_names:
        if not definitions.have_definition(ensure_context(operator)):
            op = ensure_context(operator)
            definitions.builtin[op] = Definition(name=op)

    
#    definitions.builtin['System`$PrePrint'].set_values_list('messages',
#                                                            Rule('usage',"$PrePrint is a global variable whose value, if set, is applied to \
#every expression before it is printed. ",True))
    
