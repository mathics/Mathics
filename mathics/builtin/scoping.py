#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

from mathics.builtin.base import Builtin, Predefined
from mathics.core.expression import (Expression, String, Symbol, Integer,
                                     fully_qualified_symbol_name)


def get_scoping_vars(var_list, msg_symbol='', evaluation=None):
    def message(tag, *args):
        if msg_symbol and evaluation:
            evaluation.message(msg_symbol, tag, *args)

    if not var_list.has_form('List', None):
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
            message('dup', Symbol(var_name))
        else:
            scoping_vars.add(var_name)
            yield var_name, new_def


def dynamic_scoping(func, vars, evaluation):
    original_definitions = {}
    for var_name, new_def in vars.items():
        assert fully_qualified_symbol_name(var_name)
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
    <dt>'Block[{$x$, $y$, ...}, $expr$]'
        <dd>temporarily removes the definitions of the given
        variables, evaluates $expr$, and restores the original
        definitions afterwards.
    <dt>'Block[{$x$=$x0$, $y$=$y0$, ...}, $expr$]'
        <dd>assigns temporary values to the variables during the
        evaluation of $expr$.
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
        'lvsym': ("Local variable specification contains `1`, "
                  "which is not a symbol or an assignment to a symbol."),
        'dup': ("Duplicate local variable `1` found in local variable "
                "specification."),
        'lvlist': "Local variable specification `1` is not a List.",
    }

    def apply(self, vars, expr, evaluation):
        'Block[vars_, expr_]'

        vars = dict(get_scoping_vars(vars, 'Block', evaluation))
        result = dynamic_scoping(expr.evaluate, vars, evaluation)
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
        'set': ("Cannot set $ModuleNumber to `1`; "
                "value must be a positive integer."),
    }

    rules = {
        '$ModuleNumber': '1',
    }


class Module(Builtin):
    """
    <dl>
    <dt>'Module[{$vars$}, $expr$]'
        <dd>localizes variables by giving them a temporary name of the
        form 'name$number', where number is the current value of
        '$ModuleNumber'. Each time a module is evaluated,
        '$ModuleNumber' is incremented.
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

    #> Module[{n = 3}, Module[{b = n * 5}, b * 7]]
     = 105

    """

    attributes = ('HoldAll',)

    messages = {
        'lvsym': ("Local variable specification contains `1`, "
                  "which is not a symbol or an assignment to a symbol."),
        'dup': ("Duplicate local variable `1` found in local variable "
                "specification."),
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
        new_expr = expr.replace_vars(replace, evaluation, in_scoping=False)
        result = new_expr.evaluate(evaluation)
        return result


class Context(Builtin):
    r"""
    <dl>
    <dt>'Context[$symbol$]'
        <dd>yields the name of the context where $symbol$ is defined in.
    <dt>'Context[]'
        <dd>returns the value of '$Context'.
    </dl>

    >> Context[a]
     = Global`
    >> Context[b`c]
     = b`
    >> Context[Sin] // InputForm
     = "System`"

    >> InputForm[Context[]]
     = "Global`"

    ## placeholder for general context-related tests
    #> x === Global`x
     = True
    #> `x === Global`x
     = True
    #> a`x === Global`x
     = False
    #> a`x === a`x
     = True
    #> a`x === b`x
     = False
    ## awkward parser cases
    #> FullForm[a`b_]
     = Pattern[a`b, Blank[]]
    """

    attributes = ('HoldFirst',)

    rules = {
        'Context[]': '$Context'
    }

    def apply(self, symbol, evaluation):
        'Context[symbol_]'

        name = symbol.get_name()
        if not name:
            evaluation.message('Context', 'normal')
            return
        assert '`' in name
        context = name[:name.rindex('`') + 1]
        return String(context)


class Contexts(Builtin):
    """
    <dl>
    <dt>'Contexts[]'
        <dd>yields a list of all contexts.
    </dl>

    ## this assignment makes sure that a definition in Global` exists
    >> x = 5;
    >> Contexts[] // InputForm
     = {"Combinatorica`", "Global`", "ImportExport`", "Internal`", "System`", "System`Convert`Image`", "System`Convert`JSONDump`", "System`Convert`TableDump`", "System`Convert`TextDump`", "System`Private`", "XML`", "XML`Parser`"}
    """

    def apply(self, evaluation):
        'Contexts[]'

        contexts = set([])
        for name in evaluation.definitions.get_names():
            contexts.add(String(name[:name.rindex('`') + 1]))

        return Expression('List', *sorted(contexts))


class Context_(Predefined):
    """
    <dl>
    <dt>'$Context'
        <dd>is the current context.
    </dl>

    >> $Context
    = Global`

    #> InputForm[$Context]
    = "Global`"

    ## Test general context behaviour
    #> Plus === Global`Plus
     = False
    #> `Plus === Global`Plus
     = True
    """

    name = '$Context'

    messages = {
        'cxset': "`1` is not a valid context name ending in `."
    }

    rules = {
        '$Context': '"Global`"',
    }


class ContextPath(Predefined):
    """
    <dl>
    <dt>'$ContextPath'
        <dd>is the search path for contexts.
    </dl>

    >> $ContextPath // InputForm
     = {"Global`", "System`"}

    #> $ContextPath = Sin[2]
     : Sin[2] is not a list of valid context names ending in `.
     = Sin[2]

    #> x`x = 1; x
     = x
    #> $ContextPath = {"x`"};
    #> x
     = 1
    #> System`$ContextPath
     = {x`}
    #> $ContextPath = {"Global`", "System`"};
    """

    name = '$ContextPath'

    messages = {
        'cxlist': "`1` is not a list of valid context names ending in `."
    }

    rules = {
        '$ContextPath': '{"Global`", "System`"}',
    }


class Begin(Builtin):
    """
    <dl>
    <dt>'Begin'[$context$]
        <dd>temporarily sets the current context to $context$.
    </dl>

    >> Begin["test`"]
     = test`
    >> {$Context, $ContextPath}
     = {test`, {Global`, System`}}
    >> Context[newsymbol]
     = test`
    >> End[]
     = test`
    >> End[]
     : No previous context defined.
     = Global`

    #> Begin["`test`"]
     = Global`test`
    #> Context[]
     = Global`test`
    #> End[]
     = Global`test`
    """

    rules = {
        'Begin[context_String]': '''
             Unprotect[System`Private`$ContextStack];
             System`Private`$ContextStack = Append[System`Private`$ContextStack, $Context];
             Protect[System`Private`$ContextStack];
             $Context = context;
             $Context
        ''',
    }


class End(Builtin):
    """
    <dl>
    <dt>'End[]'
        <dd>ends a context started by 'Begin'.
    </dl>
    """

    messages = {
        'noctx': "No previous context defined.",
    }

    rules = {
        'End[]': '''
             Block[{System`Private`old=$Context},
                   If[Length[System`Private`$ContextStack] === 0,
                     (* then *) Message[End::noctx]; $Context,
                     (* else *) Unprotect[System`Private`$ContextStack];
                                {$Context, System`Private`$ContextStack} =
                                    {Last[System`Private`$ContextStack],
                                     Most[System`Private`$ContextStack]};
                                Protect[System`Private`$ContextStack];
                                System`Private`old]]
        ''',
    }


class BeginPackage(Builtin):
    """
    <dl>
    <dt>'BeginPackage'[$context$]
        <dd>starts the package given by $context$.
    </dl>

    The $context$ argument must be a valid context name.
    'BeginPackage' changes the values of '$Context' and
    '$ContextPath', setting the current context to $context$.

    >> {$Context, $ContextPath}
     = {Global`, {Global`, System`}}
    >> BeginPackage["test`"]
     = test`
    >> {$Context, $ContextPath}
     = {test`, {test`, System`}}
    >> Context[newsymbol]
     = test`
    >> EndPackage[]
    >> {$Context, $ContextPath}
     = {Global`, {test`, Global`, System`}}
    >> EndPackage[]
     : No previous context defined.
    """

    messages = {
        'unimpl': "The second argument to BeginPackage is not yet implemented."
    }

    rules = {
        'BeginPackage[context_String]': '''
             Unprotect[System`Private`$ContextPathStack];
             Begin[context];
             System`Private`$ContextPathStack =
                 Append[System`Private`$ContextPathStack, $ContextPath];
             $ContextPath = {context, "System`"};
             Protect[System`Private`$ContextPathStack];
             context
        ''',
    }


class EndPackage(Builtin):
    """
    <dl>
    <dt>'EndPackage[]'
        <dd>marks the end of a package, undoing a previous 'BeginPackage'.
    </dl>

    After 'EndPackage', the values of '$Context' and '$ContextPath' at
    the time of the 'BeginPackage' call are restored, with the new
    package\'s context prepended to $ContextPath.
    """

    messages = {
        'noctx': "No previous context defined.",
    }

    rules = {
        'EndPackage[]': '''
             Block[{System`Private`newctx=Quiet[End[], {End::noctx}]},
                   If[Length[System`Private`$ContextPathStack] === 0,
                      (* then *) Message[EndPackage::noctx],
                      (* else *) Unprotect[System`Private`$ContextPathStack];
                                 {$ContextPath, System`Private`$ContextPathStack} =
                                     {Prepend[Last[System`Private`$ContextPathStack],
                                              System`Private`newctx],
                                      Most[System`Private`$ContextPathStack]};
                                 Protect[System`Private`$ContextPathStack];
                                 Null]]
        ''',
    }


class ContextStack(Builtin):
    """
    <dl>
    <dt>'System`Private`$ContextStack'
        <dd>is an internal variable tracking the values of '$Context'
        saved by 'Begin' and 'BeginPackage'.
    </dl>
    """

    context = 'System`Private`'
    name = '$ContextStack'

    rules = {
        'System`Private`$ContextStack': '{}',
    }


class ContextPathStack(Builtin):
    """
    <dl>
    <dt>'System`Private`$ContextPathStack'
        <dd>is an internal variable tracking the values of
        '$ContextPath' saved by 'Begin' and 'BeginPackage'.
    </dl>
    """

    context = 'System`Private`'
    name = '$ContextPathStack'

    rules = {
        'System`Private`$ContextPathStack': '{}',
    }
