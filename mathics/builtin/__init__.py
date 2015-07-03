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

from mathics.builtin import (
    algebra, arithmetic, assignment, attributes, calculus, combinatorial,
    comparison, control, datentime, diffeqns, evaluation, exptrig, functional,
    graphics, graphics3d, inout, integer, linalg, lists, logic, numbertheory,
    numeric, options, patterns, plot, physchemdata, randomnumbers, recurrence,
    specialfunctions, scoping, stats, strings, structure, system, tensors)

from mathics.builtin.base import (
    Builtin, SympyObject, BoxConstruct, Operator, PatternObject)

from mathics.settings import ENABLE_FILES_MODULE

modules = [
    algebra, arithmetic, assignment, attributes, calculus, combinatorial,
    comparison, control, datentime, diffeqns, evaluation, exptrig, functional,
    graphics, graphics3d, inout, integer, linalg, lists, logic, numbertheory,
    numeric, options, patterns, plot, physchemdata, randomnumbers, recurrence,
    specialfunctions, scoping, strings, stats, structure, system, tensors]

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
        if (hasattr(var, '__module__')
                and var.__module__.startswith('mathics.builtin.')
                and var.__module__ != 'mathics.builtin.base'
                and is_builtin(var) and not name.startswith('_')
                and var.__module__ == module.__name__):

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
            if builtin.sympy_name:
                sympy_to_mathics[builtin.sympy_name] = builtin
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


def contribute(definitions):
    # let MakeBoxes contribute first
    builtins['System`MakeBoxes'].contribute(definitions)
    for name, item in builtins.items():
        if name != 'System`MakeBoxes':
            item.contribute(definitions)

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
