# -*- coding: utf-8 -*-

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Integer, Number, from_python
from mathics.core.convert import sympy_symbol_prefix, SympyExpression, from_sympy
from mathics.core.rules import Pattern

import sympy


class Minimize(Builtin):
    """
    <dl>
    <dt>'Minimize[$f$, $x$]'
        <dd>compute the minimum of $f$ respect $x$ that change between $a$ and $b$
    </dl>

    >> Minimize[2 x^2 - 3 x + 5, x]
     = {{31 / 8, {x -> 3 / 4}}}

    #>> Minimize[(x y - 3)^2 + 1, {x, y}]
     = {{1, {x -> 3, y -> 1}}}

    #>> Minimize[{x - 2 y, x^2 + y^2 <= 1}, {x, y}]
     = {{-Sqrt[5], {x -> -Sqrt[5] / 5, y -> 2 Sqrt[5] / 5}}}
    """

    attributes = ("ReadProtected",)

    def apply_onevariable(self, f, x, evaluation):
        "Minimize[f_?NotListQ, x_?NotListQ]"

        sympy_x = x.to_sympy()
        sympy_f = f.to_sympy()

        derivative = sympy.diff(sympy_f, sympy_x)
        second_derivative = sympy.diff(derivative, sympy_x)
        candidates = sympy.solve(derivative, sympy_x, real=True, dict=True)

        minimum_list = []

        for candidate in candidates:
            value = second_derivative.subs(candidate)
            if value.is_real and value > 0:

                if candidate is not list:
                    candidate = candidate

                minimum_list.append([candidate[sympy_x], sympy_f.subs(candidate)])

        return Expression(
            "List",
            *(
                Expression(
                    "List",
                    from_sympy(minimum[1]),
                    Expression("List", (Expression("Rule", x, from_sympy(minimum[0])))),
                )
                for minimum in minimum_list
            )
        )

    def apply_multiplevariable(self, f, vars, evaluation):
        "Minimize[f_?NotListQ, vars_?ListQ]"

        head_name = vars.get_head_name()
        vars_or = vars
        vars = vars.leaves
        for var in vars:
            if (
                (var.is_atom() and not var.is_symbol())
                or head_name in ("System`Plus", "System`Times", "System`Power")  # noqa
                or "System`Constant" in var.get_attributes(evaluation.definitions)
            ):

                evaluation.message("Minimize", "ivar", vars_or)
                return

        vars_sympy = [var.to_sympy() for var in vars]
        sympy_f = f.to_sympy()

        jacobian = [sympy.diff(sympy_f, x) for x in vars_sympy]
        hessian = sympy.Matrix(
            [[sympy.diff(deriv, x) for x in vars_sympy] for deriv in jacobian]
        )

        candidates_tmp = sympy.solve(jacobian, vars_sympy, dict=True)
        candidates = []

        for candidate in candidates_tmp:
            if len(candidate) != len(vars_sympy):
                for variable in candidate:
                    for i in range(len(candidate), len(vars_sympy)):
                        candidate[variable] = candidate[variable].subs(
                            {vars_sympy[i]: 1}
                        )

                for i in range(len(candidate), len(vars_sympy)):
                    candidate[vars_sympy[i]] = 1

            candidates.append(candidate)

        minimum_list = []

        for candidate in candidates:
            eigenvals = hessian.subs(candidate).eigenvals()

            positives_eigenvalues = 0
            negatives_eigenvalues = 0

            for val in eigenvals:
                if val.is_real:
                    if val < 0:
                        negatives_eigenvalues += 1
                    elif val >= 0:
                        positives_eigenvalues += 1

            if positives_eigenvalues + negatives_eigenvalues != len(eigenvals):
                continue

            if positives_eigenvalues == len(eigenvals):
                minimum_list.append(candidate)

        return Expression(
            "List",
            *(
                Expression(
                    "List",
                    from_sympy(sympy_f.subs(minimum).simplify()),
                    [
                        Expression(
                            "Rule",
                            from_sympy(list(minimum.keys())[i]),
                            from_sympy(list(minimum.values())[i]),
                        )
                        for i in range(len(vars_sympy))
                    ],
                )
                for minimum in minimum_list
            )
        )

    def apply_constraints(self, f, vars, evaluation):
        "Minimize[f_?ListQ, vars_?ListQ]"
        head_name = vars.get_head_name()
        vars_or = vars
        vars = vars.leaves
        for var in vars:
            if (
                (var.is_atom() and not var.is_symbol())
                or head_name in ("System`Plus", "System`Times", "System`Power")  # noqa
                or "System`Constant" in var.get_attributes(evaluation.definitions)
            ):

                evaluation.message("Minimize", "ivar", vars_or)
                return

        vars_sympy = [var.to_sympy() for var in vars]
        constraints = [function for function in f.leaves]
        objective_function = constraints[0].to_sympy()

        constraints = constraints[1:]

        g_functions = []
        h_functions = []

        g_variables = []
        h_variables = []

        for constraint in constraints:
            left, right = constraint.leaves
            head_name = constraint.get_head_name()

            left = left.to_sympy()
            right = right.to_sympy()

            if head_name == "System`LessEqual" or head_name == "System`Less":
                eq = left - right
                eq = sympy.together(eq)
                eq = sympy.cancel(eq)

                g_functions.append(eq)
                g_variables.append(sympy.Symbol("kkt_g" + str(len(g_variables))))

            elif head_name == "System`GreaterEqual" or head_name == "System`Greater":
                eq = -1 * (left - right)
                eq = sympy.together(eq)
                eq = sympy.cancel(eq)

                g_functions.append(eq)
                g_variables.append(sympy.Symbol("kkt_g" + str(len(g_variables))))

            elif head_name == "System`Equal":
                eq = left - right
                eq = sympy.together(eq)
                eq = sympy.cancel(eq)

                h_functions.append(eq)
                h_variables.append(sympy.Symbol("kkt_h" + str(len(h_variables))))

        equations = []

        for variable in vars_sympy:
            equation = sympy.diff(objective_function, variable)

            for i in range(len(g_variables)):
                g_variable = g_variables[i]
                g_function = g_functions[i]

                equation = equation + g_variable * sympy.diff(g_function, variable)

            for i in range(len(h_variables)):
                h_variable = h_variables[i]
                h_function = h_functions[i]

                equation = equation + h_variable * sympy.diff(h_function, variable)

            equations.append(equation)

        for i in range(len(g_variables)):
            g_variable = g_variables[i]
            g_function = g_functions[i]

            equations.append(g_variable * g_function)

        for i in range(len(h_variables)):
            h_variable = h_variables[i]
            h_function = h_functions[i]

            equations.append(h_variable * h_function)

        all_variables = vars_sympy + g_variables + h_variables

        candidates_tmp = sympy.solve(equations, all_variables, dict=True)
        candidates = []

        for candidate in candidates_tmp:
            if len(candidate) != len(vars_sympy):
                for variable in candidate:
                    for i in range(len(candidate), len(vars_sympy)):
                        candidate[variable] = candidate[variable].subs(
                            {vars_sympy[i]: 1}
                        )
                for i in range(len(candidate), len(vars_sympy)):
                    candidate[vars_sympy[i]] = 1

            candidates.append(candidate)

        kkt_candidates = []

        for candidate in candidates:
            kkt_ok = True

            sum_constraints = 0

            for i in range(len(g_variables)):
                g_variable = g_variables[i]
                g_function = g_functions[i]

                if candidate[g_variable] < 0:
                    kkt_ok = False

                if candidate[g_variable] * g_function.subs(candidate) != 0:
                    kkt_ok = False

                sum_constraints = sum_constraints + candidate[g_variable]

            for i in range(len(h_variables)):
                h_variable = h_variables[i]
                h_function = h_functions[i]

                sum_constraints = sum_constraints + abs(candidate[h_variable])

            if sum_constraints <= 0:
                kkt_ok = False

            if not kkt_ok:
                continue

            kkt_candidates.append(candidate)

        hessian = sympy.Matrix(
            [[sympy.diff(deriv, x) for x in all_variables] for deriv in equations]
        )

        for i in range(0, len(all_variables) - len(vars_sympy)):
            hessian.col_del(len(all_variables) - i - 1)
            hessian.row_del(len(all_variables) - i - 1)

        minimum_list = []

        for candidate in kkt_candidates:
            eigenvals = hessian.subs(candidate).eigenvals()

            positives_eigenvalues = 0
            negatives_eigenvalues = 0

            for val in eigenvals:
                val = complex(sympy.N(val, chop=True))

                if val.imag == 0:
                    val = val.real
                    if val < 0:
                        negatives_eigenvalues += 1
                    elif val > 0:
                        positives_eigenvalues += 1

            if positives_eigenvalues + negatives_eigenvalues != len(eigenvals):
                continue

            if positives_eigenvalues == len(eigenvals):
                for g_variable in g_variables:
                    del candidate[g_variable]
                for h_variable in h_variables:
                    del candidate[h_variable]

                minimum_list.append(candidate)

        return Expression(
            "List",
            *(
                Expression(
                    "List",
                    from_sympy(objective_function.subs(minimum).simplify()),
                    [
                        Expression(
                            "Rule",
                            from_sympy(list(minimum.keys())[i]),
                            from_sympy(list(minimum.values())[i]),
                        )
                        for i in range(len(vars_sympy))
                    ],
                )
                for minimum in minimum_list
            )
        )


class Maximize(Builtin):
    """
    <dl>
    <dt>'Maximize[$f$, $x$]'
        <dd>compute the maximum of $f$ respect $x$ that change between $a$ and $b$
    </dl>

    >> Maximize[-2 x^2 - 3 x + 5, x]
     = {{49 / 8, {x -> -3 / 4}}}

    #>> Maximize[1 - (x y - 3)^2, {x, y}]
     = {{1, {x -> 3, y -> 1}}}

    #>> Maximize[{x - 2 y, x^2 + y^2 <= 1}, {x, y}]
     = {{Sqrt[5], {x -> Sqrt[5] / 5, y -> -2 Sqrt[5] / 5}}}
    """

    attributes = ("ReadProtected",)

    def apply(self, f, vars, evaluation):
        "Maximize[f_?NotListQ, vars_]"

        dual_f = f.to_sympy() * -1

        dual_solutions = (
            Expression("Minimize", from_sympy(dual_f), vars).evaluate(evaluation).leaves
        )

        solutions = []
        for dual_solution in dual_solutions:
            solution_leaves = dual_solution.leaves
            solutions.append([solution_leaves[0] * -1, solution_leaves[1]])

        return from_python(solutions)

    def apply_constraints(self, f, vars, evaluation):
        "Maximize[f_?ListQ, vars_]"

        constraints = [function for function in f.leaves]
        constraints[0] = from_sympy(constraints[0].to_sympy() * -1)

        dual_solutions = (
            Expression("Minimize", constraints, vars).evaluate(evaluation).leaves
        )

        solutions = []
        for dual_solution in dual_solutions:
            solution_leaves = dual_solution.leaves
            solutions.append([solution_leaves[0] * -1, solution_leaves[1]])

        return from_python(solutions)
