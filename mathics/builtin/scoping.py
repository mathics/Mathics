# -*- coding: utf8 -*-

from mathics.builtin.base import Builtin, Predefined
from mathics.core.expression import Expression, String, Symbol, Integer


def get_scoping_vars(var_list, msg_symbol='', evaluation=None):
    def message(tag, *args):
        if msg_symbol and evaluation:
            evaluation.message(msg_symbol, tag, *args)

    if var_list.get_head_name() != 'List':
        message('lvlist', var_list)
        return
    vars = var_list.leaves
    scoping_vars = set()
    for var in vars:
        var_name = None
        if var.has_form('Set', 2):
            var_name = var.leaves[0].get_name()
            new_def = var.leaves[1]
            new_def = new_def.evaluate(evaluation)
        elif var.has_form('Symbol'):
            var_name = var.get_name()
            new_def = None
        if not var_name:
            message('lvsym', var)
            continue
        if var_name in scoping_vars:
            message('dup', var_name)
        else:
            scoping_vars.add(var_name)
            yield var_name, new_def


def dynamic_scoping(func, vars, evaluation):
    original_definitions = {}
    for var_name, new_def in vars.items():
        original_definitions[
            var_name] = evaluation.definitions.get_user_definition(var_name)
        evaluation.definitions.reset_user_definition(var_name)
        if new_def is not None:
            new_def = new_def.evaluate(evaluation)
            evaluation.definitions.set_ownvalue(var_name, new_def)
    try:
        result = func(evaluation)
    finally:
        for name, definition in original_definitions.items():
            evaluation.definitions.add_user_definition(name, definition)
    return result


class Block(Builtin):
    """
    <dl>
    <dt>'Block[{$vars$}, $expr$]'
        <dd>temporarily stores the definitions of certain variables, evaluates
        $expr$ with reset values and restores the original definitions afterwards.
    <dt>'Block[{$x$=$x0$, $y$=$y0$, ...}, $expr$]'
        <dd>assigns initial values to the reset variables.
    </dl>
    >> n = 10
     = 10
    >> Block[{n = 5}, n ^ 2]
     = 25
    >> n
     = 10

    Values assigned to block variables are evaluated at the beginning of the block.
    Keep in mind that the result of 'Block' is evaluated again, so a returned block variable
    will get its original value.
    >> Block[{x = n+2, n}, {x, n}]
     = {12, 10}

    If the variable specification is not of the described form, an error message is raised:
    >> Block[{x + y}, x]
     : Local variable specification contains x + y, which is not a symbol or an assignment to a symbol.
     = x

    Variable names may not appear more than once:
    >> Block[{x, x}, x]
     : Duplicate local variable x found in local variable specification.
     = x
    """

    attributes = ('HoldAll',)

    messages = {
        'lvsym': "Local variable specification contains `1`, which is not a symbol or an assignment to a symbol.",
        'dup': "Duplicate local variable `1` found in local variable specification.",
        'lvlist': "Local variable specification `1` is not a List.",
    }

    def apply(self, vars, expr, evaluation):
        'Block[vars_, expr_]'

        vars = dict(get_scoping_vars(vars, 'Block', evaluation))
        result = dynamic_scoping(expr.evaluate, vars, evaluation)

        # Variables may have changed: must revalute
        result.is_evaluated = False

        return result


class ModuleNumber(Predefined):
    """
    <dl>
    <dt>'$ModuleNumber'
        <dd>is the current "serial number" to be used for local module variables.
    </dl>

    >> Unprotect[$ModuleNumber]
    >> $ModuleNumber = 20;
    >> Module[{x}, x]
     = x$20

    >> $ModuleNumber = x;
     : Cannot set $ModuleNumber to x; value must be a positive integer.
    """

    name = '$ModuleNumber'

    messages = {
        'set': "Cannot set $ModuleNumber to `1`; value must be a positive integer.",
    }

    rules = {
        '$ModuleNumber': '1',
    }


class Module(Builtin):
    """
    <dl>
    <dt>'Module[{$vars$}, $expr$]'
        <dd>localizes variables by giving them a temporary name of the form
    'name$number', where number is the current value of '$ModuleNumber'. Each time a module
    is evaluated, '$ModuleNumber' is incremented.
    </dl>

    >> x = 10;
    >> Module[{x=x}, x=x+1; x]
     = 11
    >> x
     = 10
    >> t === Module[{t}, t]
     = False

    Initial values are evaluated immediately:
    >> Module[{t=x}, x = x + 1; t]
     = 10
    >> x
     = 11

    Variables inside other scoping constructs are not affected by the renaming of 'Module':
    >> Module[{a}, Block[{a}, a]]
     = a
    >> Module[{a}, Block[{}, a]]
     = a$5
    """

    attributes = ('HoldAll',)

    messages = {
        'lvsym': "Local variable specification contains `1`, which is not a symbol or an assignment to a symbol.",
        'dup': "Duplicate local variable `1` found in local variable specification.",
        'lvlist': "Local variable specification `1` is not a List.",
    }

    def apply(self, vars, expr, evaluation):
        'Module[vars_, expr_]'

        scoping_vars = get_scoping_vars(vars, 'Module', evaluation)
        replace = {}
        number = Symbol('$ModuleNumber').evaluate(evaluation).get_int_value()
        if number is None:
            number = 1
        evaluation.definitions.set_ownvalue(
            '$ModuleNumber', Integer(number + 1))
        for name, new_def in scoping_vars:
            new_name = '%s$%d' % (name, number)
            if new_def is not None:
                evaluation.definitions.set_ownvalue(new_name, new_def)
            replace[name] = Symbol(new_name)
        new_expr = expr.replace_vars(replace, in_scoping=False)
        result = new_expr.evaluate(evaluation)
        return result


class Context(Builtin):
    r"""
    <dl>
    <dt>'Context[$symbol$]'
        <dd>yields the name of the context where $symbol$ is defined in.
    </dl>

    Contexts are not really implemented in \Mathics. 'Context' just returns '"System`"'
    for built-in symbols and '"Global`"' for user-defined symbols.

    >> Context[a]
     = Global`
    >> Context[Sin] // InputForm
     = "System`"
    """

    def apply(self, symbol, evaluation):
        'Context[symbol_]'

        name = symbol.get_name()
        if not name:
            evaluation.message('Context', 'normal')
            return
        context = evaluation.definitions.get_definition(name).context
        return String(context)
