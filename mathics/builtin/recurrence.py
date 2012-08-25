# -*- coding: utf8 -*-

"""
Recurrence relation solvers
"""

import sympy
from mathics.builtin.base import Builtin, BinaryOperator, Test
from mathics.core.expression import Expression, from_sympy
from mathics.core.convert import SympyExpression, sympy_symbol_prefix

class RSolve(Builtin):
    """
    <dl>
    <dt>'RSolve[$eqn$, $a[n]$, $n$]'
        <dd> Solves a recurrence equation for the function $a[n]$.
    </dl>

    >> RSolve[a[n] == a[n+1], a[n], n]
     = {{a[n] -> C[0]}}

    >> RSolve[a[n + 2] == 3 a[n], a, n]
    = {{a -> (Function[{n}, C[1] (-Sqrt[3]) ^ n + C[0] 3 ^ (n / 2)])}}
    """

    messages = {
        'deqn': 'Equation or list of equations expected instead of `1` in the first argument `1`.',
        'deqx': 'Supplied equations are not difference equations of the given functions.',
        'dsfun': '`1` cannot be used as a function.',
        'dsvar': '`1` cannot be used as a variable.',
    }

    def apply(self, eqn, a, n, evaluation):
        'RSolve[eqn_, a_, n_]'

        #TODO: Handle initial conditions
        
        if eqn.get_head_name() != 'Equal':
            evaluation.message('RSolve', 'deqn', eqn)
            return

        if (n.is_atom() and not n.is_symbol()) or \
          n.get_head_name() in ('Plus', 'Times', 'Power') or \
          'Constant' in n.get_attributes(evaluation.definitions):
            evaluation.message('RSolve', 'dsvar')
            return

        try:
            a.leaves
            function_form = None
            func = a
        except AttributeError:
            func = Expression(a, n)
            function_form = Expression('List', n)

        if func.is_atom() or len(func.leaves) != 1:
            evaluation.message('RSolve', 'dsfun', y)

        if n not in func.leaves:
            evaluation.message('DSolve', 'deqx')

        left, right = eqn.leaves
        eqn = Expression('Plus', left, Expression('Times', -1, right)).evaluate(evaluation)

        sym_eq = eqn.to_sympy(converted_functions = set([func.get_head_name()]))
        sym_n = sympy.symbols(str(sympy_symbol_prefix + n.name))
        sym_func = sympy.Function(str(sympy_symbol_prefix + func.get_head_name())) (sym_n)

        try:
            sym_result = sympy.rsolve(sym_eq, sym_func)
            if not isinstance(sym_result, list):
                sym_result = [sym_result]
        except ValueError as ve:
            #print ve
            return

        if function_form is None:
            return Expression('List', *[Expression('List',
                Expression('Rule', a, from_sympy(soln))) for soln in sym_result])
        else:
            return Expression('List', *[Expression('List', Expression('Rule', a,
                Expression('Function', function_form, from_sympy(soln)))) for soln in sym_result])

