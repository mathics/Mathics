# -*- coding: utf8 -*-

"""
Differential equation solver functions
"""

import sympy
from mathics.builtin.base import Builtin, BinaryOperator, Test
from mathics.core.expression import Expression, from_sympy
from mathics.core.convert import SympyExpression, sympy_symbol_prefix

class DSolve(Builtin):
    """
    <dl>
    <dt>'DSolve[$eq$, $y[x]$, $x$]'
        <dd>solves a differential equation for the function $y[x]$.
    </dl>

    >> DSolve[y''[x] == 0, y[x], x]
     = {{y[x] -> x C[2] + C[1]}}

    >> DSolve[y''[x] == y[x], y[x], x]
     = {{y[x] -> C[1] E ^ (-x) + C[2] E ^ x}}

    >> DSolve[y''[x] == y[x], y, x]
     = {{y -> (Function[{x}, C[1] Exp[-x] + C[2] Exp[x]])}}

    #> Attributes[f] = {HoldAll}; 
    #> DSolve[f[x + x] == Sin[f'[x]], f, x]
     : To avoid possible ambiguity, the arguments of the dependent variable in -Sin[f'[x]] + f[x + x] should literally match the independent variables.
     = DSolve[f[x + x] == Sin[f'[x]], f, x]

    #> Attributes[f] = {};
    #> DSolve[f[x + x] == Sin[f'[x]], f, x]
     : To avoid possible ambiguity, the arguments of the dependent variable in -Sin[f'[x]] + f[2 x] should literally match the independent variables.
     = DSolve[f[2 x] == Sin[f'[x]], f, x]
    
    #> DSolve[f'[x] == f[x], f, x] // FullForm
     = List[List[Rule[f, Function[List[x], Times[C[1], Exp[x]]]]]]

    #> DSolve[f'[x] == f[x], f, x] /. {C[1] -> 1}
     = {{f -> (Function[{x}, 1 Exp[x]])}}

    #> DSolve[f'[x] == f[x], f, x] /. {C -> D}
     = {{f -> (Function[{x}, D[1] Exp[x]])}}

    #> DSolve[f'[x] == f[x], f, x] /. {C[1] -> C[0]}
     = {{f -> (Function[{x}, C[0] Exp[x]])}}
    """

    #TODO: GeneratedParameters option

    messages = {
        'deqn': 'Equation or list of equations expected instead of `1` in the first argument `1`.',
        'deqx': 'Supplied equations are not differential equations of the given functions.',
        'dsfun': '`1` cannot be used as a function.',
        'dsvar': '`1` cannot be used as a variable.',
        'litarg': 'To avoid possible ambiguity, the arguments of the dependent variable in `1` should literally match the independent variables.',
        #FIXME: Remove these if sympy changes:
        'symsys': 'Unfortunately SymPy, part of the Mathics backend, does not support solving systems of DEs.',
        'symimp': 'Unfortunately SymPy, part of the Mathics backend, does not support solutions to this form of DE.',
        'symmua': 'Unfortunately SymPy, part of the Mathics backend, does not support functions of multiple variables.',
    }

    def apply(self, eqn, y, x, evaluation):
        'DSolve[eqn_, y_, x_]'

        if eqn.has_form('List', eqn):
            #TODO: Try and solve BVPs using Solve or something analagous OR add this functonality to sympy.
            evaluation.message('DSolve', 'symsys')
            return

        if eqn.get_head_name() != 'Equal':
            evaluation.message('DSolve', 'deqn', eqn)
            return

        if (x.is_atom() and not x.is_symbol()) or \
          x.get_head_name() in ('Plus', 'Times', 'Power') or \
          'Constant' in x.get_attributes(evaluation.definitions):
            evaluation.message('DSolve', 'dsvar')
            return

        # Fixes pathalogical DSolve[y''[x] == y[x], y, x]
        try:
            y.leaves
            function_form = None
            func = y
        except AttributeError:
            func = Expression(y, x)
            function_form = Expression('List', x)

        if func.is_atom():
            evaluation.message('DSolve', 'dsfun', y)
            return

        if len(func.leaves) != 1:
            evaluation.message('DSolve', 'symmua')
            return

        if x not in func.leaves:
            evaluation.message('DSolve', 'deqx')
            return

        left, right = eqn.leaves
        eqn = Expression('Plus', left, Expression('Times', -1, right)).evaluate(evaluation)

        sym_eq = eqn.to_sympy(converted_functions = set([func.get_head_name()]))
        sym_x = sympy.symbols(str(sympy_symbol_prefix + x.name))
        sym_func = sympy.Function(str(sympy_symbol_prefix + func.get_head_name())) (sym_x)

        try:
            sym_result = sympy.dsolve(sym_eq, sym_func)
            if not isinstance(sym_result, list):
                sym_result = [sym_result]
        except ValueError as e:
            evaluation.message('DSolve', 'symimp')
            return
        except NotImplementedError as e:
            evaluation.message('DSolve', 'symimp')
            return
        except AttributeError as e:
            evaluation.message('DSolve', 'litarg', eqn)
            return
        except KeyError:
            evaluation.message('DSolve', 'litarg', eqn)
            return

        if function_form is None:
            return Expression('List', *[Expression('List', 
                Expression('Rule', *from_sympy(soln).leaves)) for soln in sym_result])
        else:
            return Expression('List', *[Expression('List', Expression('Rule', y, 
                Expression('Function', function_form, *from_sympy(soln).leaves[1:]))) for soln in sym_result])

#TODO: NDSolve
