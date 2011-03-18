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

modules = []
try:
    from sage import all as sage
    from mathics.optional import calculus, numerical_optimization
    
    modules += [calculus, numerical_optimization]
    
    sage_version = sage.version()
except ImportError:
    # silently ignore when Sage cannot be imported
    sage_version = None

from mathics.builtin import add_builtins, builtins_by_module, is_builtin
from mathics.builtin.base import Builtin

optional_builtins = []
optional_builtins_by_module = {}

for module in modules:
    optional_builtins_by_module[module.__name__] = []
    vars = dir(module)
    for name in vars:
        var = getattr(module, name)
        if hasattr(var, '__module__') and var.__module__.startswith('mathics.optional.') and \
            var.__module__ != 'mathics.optional.base' and is_builtin(var) and not name.startswith('_'):
            instance = var(expression=False)
            if isinstance(instance, Builtin):
                optional_builtins.append((instance.get_name(), instance))
                optional_builtins_by_module[module.__name__].append(instance)

# update existing builtins
add_builtins(optional_builtins)
builtins_by_module.update(optional_builtins_by_module)