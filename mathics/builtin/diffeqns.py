# -*- coding: utf8 -*-

"""
Differential equation solver functions
"""

import sympy
from mathics.builtin.base import Builtin, BinaryOperator, Test
from mathics.core.expression import Expression, from_sympy
from mathics.core.convert import SympyExpression

class DSolve(Builtin):
    """
    <dl>
    <dt>'DSolve[$eq$, $y[x]$, $x$]'
        <dd> Solves a differential equation for the function $y[x]$.
    </dl>


    >> DSolve[y''[x] == 0, y[x], x]
     = {{y[x] -> C[1] + x C[2]}}

    >> DSolve[{y'[x] + y[x] == x, y[0] == 0}, y[x], x]
     = {{y[x] -> E^-x (1 - E^x + E^x x)}}
    """

    messages = {
        'deqn': 'Equation or list of equations expected instead of `1` in the first argument `1`.',
        'deqx': 'Supplied equations are not differential equations of the given functions.',
        'dsfun': '`1` cannot be used as a function.',
        'dsvar': '`1` cannot be used as a variable.',
    }


    def apply(self, eqn, y, x, evaluation):
        'DSolve[eqn_, y_, x_]'

        if eqn.get_head_name() != 'Equal':
            evaluation.message('DSolve', 'deqn', eqn)
            return

        if (x.is_atom() and not x.is_symbol()) or \
          x.get_head_name() in ('Plus', 'Times', 'Power') or \
          'Constant' in x.get_attributes(evaluation.definitions):
            evaluation.message('DSolve', 'dsvar')
            return

        if y.is_atom() or len(y.leaves) != 1:
            evaluation.message('DSolve', 'dsfun', y) 

        if x not in y.leaves:
            #TODO: Handle implied leaves like >> DSolve[y'[x] == y[x] (1 - y[x]/27), y, x]
            evaluation.message('DSolve', 'deqx')

        left, right = eqn.leaves
        eq = Expression('Plus', left, Expression('Times', -1, right)).evaluate(evaluation)
        eq = eq.to_sympy()

        sym_x = sympy.symbols('_Mathics_User_' +x.to_python())
        func = sympy.Function('_Mathics_User_' + y.get_head_name()) (sym_x)

        try:
            result = sympy.dsolve(eq, func)
            if not isinstance(result, list):
                result = [result]
                
            return Expression('List', *(Expression('List', *from_sympy(result))))
        except ValueError as ve:
            print ve
            return

#TODO: NDSolve

