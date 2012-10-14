# -*- coding: utf8 -*-

"""
Calculus functions
"""

import re

from mathics.builtin.base import Builtin, PostfixOperator, SageFunction
from mathics.core.expression import Expression, String, Integer, Number
from mathics.core.expression import from_sage, ConvertSubstitutions, from_sympy
from mathics.core.convert import sage_symbol_prefix, sympy_symbol_prefix
from mathics.core.util import unicode_superscript
from mathics.core.rules import Pattern
from mathics.builtin.scoping import dynamic_scoping

try:
    from sage.symbolic.operators import FDerivativeOperator
    from sage import all as sage
    from sage.calculus.calculus import var as sage_var, function as sage_function
except (ImportError, RuntimeError):
    pass

import sympy

class D(SageFunction):
    u"""
    <dl>
    <dt>'D[$f$, $x$]'
        <dd>gives the partial derivative of $f$ with respect to $x$.
    <dt>'D[$f$, $x$, $y$, ...]'
        <dd>differentiates successively with respect to $x$, $y$, etc.
    <dt>'D[$f$, {$x$, $n$}]'
        <dd>gives the multiple derivative of order $n$.
    <dt>'D[$f$, {{$x1$, $x2$, ...}}]'
        <dd>gives the vector derivative of $f$ with respect to $x1$, $x2$, etc.
    </dl>
    
    >> D[x^3 + x^2, x]
     = 2 x + 3 x ^ 2
    >> D[y, x]
     = 0
    >> D[x, x]
     = 1
    >> D[x + y, x]
     = 1
    >> D[Sin[Cos[x]], x]
     = -Cos[Cos[x]] Sin[x]
     
    >> D[Sin[x], {x, 2}]
     = -Sin[x]
     
    Unknown functions are derived using 'Derivative':
    >> D[f[x], x]
     = f'[x]
    >> D[f[x, x], x]
     = Derivative[0, 1][f][x, x] + Derivative[1, 0][f][x, x]
    >> D[f[x, x], x] // InputForm
     = Derivative[0, 1][f][x, x] + Derivative[1, 0][f][x, x]
     
    Chain rule:
    >> D[f[2x+1, 2y, x+y], x]
     = 2 Derivative[1, 0, 0][f][1 + 2 x, 2 y, x + y] + Derivative[0, 0, 1][f][1 + 2 x, 2 y, x + y]
    >> D[f[x^2, x, 2y], {x,2}, y] // Expand
     = 8 x Derivative[1, 1, 1][f][x ^ 2, x, 2 y] + 8 x ^ 2 Derivative[2, 0, 1][f][x ^ 2, x, 2 y] + 2 Derivative[0, 2, 1][f][x ^ 2, x, 2 y] + 4 Derivative[1, 0, 1][f][x ^ 2, x, 2 y]
     
    Compute the gradient vector of a function:
    >> D[x ^ 3 * Cos[y], {{x, y}}]
     = {3 x ^ 2 Cos[y], -x ^ 3 Sin[y]}
    Hesse matrix:
    >> D[Sin[x] * Cos[y], {{x,y}, 2}]
     = {{-Cos[y] Sin[x], -Cos[x] Sin[y]}, {-Cos[x] Sin[y], -Cos[y] Sin[x]}}
     
    #> D[2/3 Cos[x] - 1/3 x Cos[x] Sin[x] ^ 2,x]//Expand
     = -2 x Cos[x] ^ 2 Sin[x] / 3 + x Sin[x] ^ 3 / 3 - 2 Sin[x] / 3 - Cos[x] Sin[x] ^ 2 / 3
    
    #> D[f[#1], {#1,2}]
     = f''[#1]
    #> D[(#1&)[t],{t,4}]
     = 0

    #> Attributes[f] ={HoldAll}; Apart[f''[x + x]]
     = f''[2 x]

    #> Attributes[f] = {}; Apart[f''[x + x]]
     = f''[2 x]
    """
    
    # TODO
    """
    >> D[2x, 2x]
     = 0
    """
    
    sympy_name = 'Derivative'
    
    messages = {
        'dvar': "Multiple derivative specifier `1` does not have the form {variable, n}, where n is a non-negative machine integer.",
    }
    
    rules = {
        'D[f_ + g_, x_?NotListQ]': 'D[f, x] + D[g, x]',
        'D[f_ * g_, x_?NotListQ]': 'D[f, x] * g + f * D[g, x]',
        'D[f_ ^ r_, x_?NotListQ] /; FreeQ[r, x]': 'r * f ^ (r-1) * D[f, x]',
        'D[E ^ f_, x_?NotListQ]': 'E ^ f * D[f, x]',
        'D[f_ ^ g_, x_?NotListQ]': 'D[E ^ (Log[f] * g), x]',
        
        'D[f_, x_?NotListQ] /; FreeQ[f, x]': '0',
        #'D[f_[g_], x_?NotListQ]': 'Module[{t}, D[f[t], t] /. t -> g] * D[g, x]',
        #'D[f_[g_], x_?NotListQ]': 'D[f[g], g] * D[g, x]',
        
        'D[f_[left___, x_, right___], x_?NotListQ] /; FreeQ[{left, right}, x]':
            'Derivative[Sequence @@ UnitVector[Length[{left, x, right}], Length[{left, x}]]][f][left, x, right]',
        #'D[f_[args___], x_?NotListQ]':
        #    'Plus @@ MapIndexed[(D[f[Sequence@@ReplacePart[{args}, #2->t]], t] /. t->#) * D[#, x]&, {args}]', 
        
        'D[{items___}, x_?NotListQ]': 'D[#, x]& /@ {items}',
        'D[f_, {list_List}]': 'D[f, #]& /@ list',
        'D[f_, {list_List, n_Integer?Positive}]': 'D[f, Sequence @@ ConstantArray[{list}, n]]',
        'D[f_, x_, rest__]': 'D[D[f, x], rest]',
        
        'D[expr_, {x_, n_Integer?NonNegative}]': 'Module[{t}, Nest[Function[{t}, D[t, x]], expr, n]]',
    }
    
    def apply(self, f, x, evaluation):
        'D[f_, x_?NotListQ]'
        
        if f == x:
            return Integer(1)
        elif not f.is_atom() and len(f.leaves) == 1 and f.leaves[0] == x:
            return Expression(Expression(Expression('Derivative', Integer(1)), f.head), x)
        elif not f.is_atom() and len(f.leaves) == 1:
            g = f.leaves[0]
            return Expression('Times', Expression('D', Expression(f.head, g), g),
                Expression('D', g, x))
        elif not f.is_atom() and len(f.leaves) > 1:
            def summand(leaf, index):
                if leaf.same(x):
                    result = Expression(Expression(Expression('Derivative', *([Integer(0)] * (index) + [Integer(1)] + [Integer(0)] * (len(f.leaves)-index-1))), f.head), *f.leaves)
                else:
                    result = Expression('D', f, leaf)
                return Expression('Times', result, Expression('D', leaf, x))
            x_pattern = Pattern.create(x)
            result = Expression('Plus', *[summand(leaf, index) for index, leaf in enumerate(f.leaves) if not leaf.is_free(x_pattern, evaluation)])
            if len(result.leaves) == 1:
                return result.leaves[0]
            else:
                return result
    
    def apply_wrong(self, expr, x, other, evaluation):
        'D[expr_, {x_, other___}]'
        
        arg = Expression('List', x, *other.get_sequence())
        evaluation.message('D', 'dvar', arg)
        return Expression('D', expr, arg)
    
class Derivative(PostfixOperator, SageFunction):    
    u"""
    <dl>
    <dt>'Derivative[$n$][$f$]'
        <dd>represents the $n$th derivative of the function $f$.
    <dt>'Derivative[$n1$, $n2$, ...][$f$]'
        <dd>represents a multivariate derivative.
    </dl>
    
    >> Derivative[1][Sin]
     = Cos[#1]&
    >> Derivative[3][Sin]
     = -Cos[#1]&
    >> Derivative[2][# ^ 3&]
     = 6 #1&
     
    'Derivative' can be entered using '\\'':
    >> Sin'[x]
     = Cos[x]
    >> (# ^ 4&)''
     = 12 #1 ^ 2&
    >> f'[x] // InputForm
     = Derivative[1][f][x]
     
    >> Derivative[1][#2 Sin[#1]+Cos[#2]&]
     = Cos[#1] #2&
    >> Derivative[1,2][#2^3 Sin[#1]+Cos[#2]&]
     = 6 Cos[#1] #2&
    Deriving with respect to an unknown parameter yields 0:
    >> Derivative[1,2,1][#2^3 Sin[#1]+Cos[#2]&]
     = 0&
    The 0th derivative of any expression is the expression itself:
    >> Derivative[0,0,0][a+b+c]
     = a + b + c
     
    You can calculate the derivative of custom functions:
    >> f[x_] := x ^ 2
    >> f'[x]
     = 2 x
     
    Unknown derivatives:
    >> Derivative[2, 1][h]
     = Derivative[2, 1][h]
    >> Derivative[2, 0, 1, 0][h[g]]
     = Derivative[2, 0, 1, 0][h[g]]
    """
    
    operator = "'"
    precedence = 670
    attributes = ('NHoldAll',)
    
    sage_name = ''
    
    rules = {
        'MakeBoxes[Derivative[n__Integer][f_], form:StandardForm|TraditionalForm]':
            r'SuperscriptBox[MakeBoxes[f, form], If[{n} === {2}, "\[Prime]\[Prime]", If[{n} === {1}, "\[Prime]", RowBox[{"(", Sequence @@ Riffle[{n}, ","], ")"}]]]]',
        'MakeBoxes[Derivative[n:1|2][f_], form:OutputForm]':
            """RowBox[{MakeBoxes[f, form], If[n==1, "'", "''"]}]""",
    
        'Derivative[0...][f_]': 'f',
        'Derivative[n__Integer][Derivative[m__Integer][f_]] /; Length[{m}] == Length[{n}]':
            'Derivative[Sequence @@ ({n} + {m})][f]',
        """Derivative[n__Integer][f_Symbol] /; Module[{t=Sequence@@Slot/@Range[Length[{n}]], result, nothing, ft=f[t]},
            If[Head[ft] === f
            && FreeQ[Join[UpValues[f], DownValues[f], SubValues[f]], Derivative|D]
            && Context[f] != "System`",
                False,
                (* else *)
                ft = f[t];
                Block[{f},
                    Unprotect[f];
                    (*Derivative[1][f] ^= nothing;*)
                    Derivative[n][f] ^= nothing;
                    Derivative[n][nothing] ^= nothing;
                    result = D[ft, Sequence@@Table[{Slot[i], {n}[[i]]}, {i, Length[{n}]}]];
                ];
                FreeQ[result, nothing]
            ]
            ]""":
            """Module[{t=Sequence@@Slot/@Range[Length[{n}]], result, nothing, ft},
                ft = f[t];
                Block[{f},
                    Unprotect[f];
                    Derivative[n][f] ^= nothing;
                    Derivative[n][nothing] ^= nothing;
                    result = D[ft, Sequence@@Table[{Slot[i], {n}[[i]]}, {i, Length[{n}]}]];
                ];
                Function @@ {result}
            ]""",
        'Derivative[n__Integer][f_Function]': 'Evaluate[D[Quiet[f[Sequence @@ Table[Slot[i], {i, 1, Length[{n}]}]], Function::slotn], Sequence @@ Table[{Slot[i], {n}[[i]]}, {i, 1, Length[{n}]}]]]&',
    }
    
    default_formats = False
    
    def __init__(self, *args, **kwargs):
        super(Derivative, self).__init__(*args, **kwargs)
        
    """
    def to_sage(self, expr, definitions, subs):
        try:
            fd = expr.head
            args = expr.leaves
            d = fd.head
        except AttributeError:
            try:
                args = None
                fd = expr
                d = fd.head
            except AttributeError:
                return
        if d.get_head_name() != self.get_name():
            return
        d_params = []
        for index, count in enumerate(d.leaves):
            count = count.get_int_value()
            if count is None:
                return
            count = int(count)
            d_params.extend([index] * count)
        if len(fd.leaves) != 1:
            return
        f = fd.leaves[0]
        f_name = f.get_name()
        if not f_name:
            return
        op = FDerivativeOperator(sage_function(sage_symbol_prefix + f_name), d_params)
        if args is not None:
            op = op(*(arg.to_sage(definitions, subs) for arg in args))
              
        return op
    """
    
    def post_parse(self, expression):
        count = 0
        inner = expression
        while inner.has_form('Derivative', 1):
            inner = inner.leaves[0]
            count += 1
        return Expression(Expression('Derivative', Integer(count)), inner)
    
    def to_sympy(self, expr, **kwargs):
        inner = expr
        exprs = [inner]
        try:
            while True:
                inner = inner.head
                exprs.append(inner)
        except AttributeError:
            pass

        if len(exprs) != 4 or not all(len(expr.leaves) >= 1 for exp in exprs[:3]):
            return 

        sym_x = exprs[0].leaves[0].to_sympy()
        func = exprs[1].leaves[0]
        sym_func = sympy.Function(str(sympy_symbol_prefix + func.__str__())) (sym_x)
        
        count = exprs[2].leaves[0].to_python()
        for i in range(count):
            try:
                sym_func = sympy.Derivative(sym_func)
            except ValueError:
                return None

        return sym_func

class Integrate(SageFunction):
    r"""
    <dl>
    <dt>'Integrate[$f$, $x$]'
        <dd>integrates $f$ with respect to $x$. The result does not contain the additive integration constant.
    <dt>'Integrate[$f$, {$x$, $a$, $b$}]'
        <dd>computes the definite integral of $f$ with respect to $x$ from $a$ to $b$.
    </dl>
    
    Integrate a polynomial:
    >> Integrate[6 x ^ 2 + 3 x ^ 2 - 4 x + 10, x]
     = 10 x - 2 x ^ 2 + 3 x ^ 3
    
    Integrate trigonometric functions:
    >> Integrate[Sin[x] ^ 5, x]
     = -Cos[x] - Cos[x] ^ 5 / 5 + 2 Cos[x] ^ 3 / 3
     
    Definite integrals:
    >> Integrate[x ^ 2 + x, {x, 1, 3}]
     = 38 / 3
    >> Integrate[Sin[x], {x, 0, Pi/2}]
     = 1
     
    Some other integrals:
    >> Integrate[1 / (1 - 4 x + x^2), x]
     = -Sqrt[3] Log[-2 + Sqrt[3] + x] / 6 + Sqrt[3] Log[-2 - Sqrt[3] + x] / 6
    >> Integrate[4 Sin[x] Cos[x], x]
     = 2 Sin[x] ^ 2
     
    ## This should better return -Infinity:
    #> Integrate[-Infinity, {x, 0, Infinity}]
     = Indeterminate
    
     
    Integration in TeX:
    >> Integrate[f[x], {x, a, b}] // TeXForm
     = \int_a^bf\left[x\right] \, dx
     
    #> DownValues[Integrate]
     = {}
    #> Definition[Integrate]
     = Attributes[Integrate] = {Protected, ReadProtected}
    #> Integrate[Hold[x + x], {x, a, b}]
     = Integrate[Hold[x + x], {x, a, b}]
    #> Integrate[sin[x], x]
     = Integrate[sin[x], x]
     
    #> Integrate[x ^ 3.5 + x, x]
     = x ^ 2 / 2 + 0.222222222222222222 x ^ 4.5
     
    #> Integrate[Abs[Sin[phi]],{phi,0,2Pi}]//N
     = 4.
     
    #> Integrate[1/(x^5+1), x]
     = RootSum[625 #1 ^ 4 + 125 #1 ^ 3 + 25 #1 ^ 2 + 5 #1 + 1&, #1 Log[x + 5 #1]&] + Log[1 + x] / 5
    
    #> Integrate[ArcTan(x), x]
     = ArcTan x ^ 2 / 2
    #> Integrate[E[x], x]
     = Integrate[E[x], x]
    """
    
    """
    >> Integrate[ArcSin[x / 3],x]
     = 3 Sqrt[1 - 1/9 x ^ 2] + x ArcSin[1/3 x]
    >> Integrate[Sqrt[Tan[x]], x]
     = 1/4 Log[1 + Tan[x] - Sqrt[2] Sqrt[Tan[x]]] Sqrt[2] + 1/2 ArcTan[-1/2 (Sqrt[2] - 2 Sqrt[Tan[x]]) Sqrt[2]] Sqrt[2] + 1/2 ArcTan[1/2 (Sqrt[2] + 2 Sqrt[Tan[x]]) Sqrt[2]] Sqrt[2] - 1/4 Log[1 + Tan[x] + Sqrt[2] Sqrt[Tan[x]]] Sqrt[2]
    >> Integrate[f'[x], {x, a, b}]
     = f[b] - f[a]    
    """
    
    attributes = ('ReadProtected',)
    
    sage_name = ''
    sympy_name = 'Integral'
    
    messages = {
        'idiv': "Integral of `1` does not converge on `2`.",
        'ilim': "Invalid integration variable or limit(s).",
        
        'iconstraints': "Additional constraints needed: `1`",
    }
    
    rules = {
        'Integrate[list_List, x_]': 'Integrate[#, x]& /@ list',
        
        'MakeBoxes[Integrate[f_, x_], form:StandardForm|TraditionalForm]':
            r'RowBox[{"\[Integral]", MakeBoxes[f, form], "\[InvisibleTimes]", RowBox[{"\[DifferentialD]", MakeBoxes[x, form]}]}]',
        'MakeBoxes[Integrate[f_, {x_, a_, b_}], form:StandardForm|TraditionalForm]':
            r'RowBox[{SubsuperscriptBox["\[Integral]", MakeBoxes[a, form], MakeBoxes[b, form]], MakeBoxes[f, form], "\[InvisibleTimes]", RowBox[{"\[DifferentialD]", MakeBoxes[x, form]}]}]',
    }
    
    def prepare_sage(self, leaves):
        if len(leaves) == 2:
            x = leaves[1]
            if x.has_form('List', 3):
                return ('Integrate', [leaves[0]] + x.leaves)
            else:
                return ('Integrate', leaves)
        return leaves
            
    def from_sage(self, leaves):
        if len(leaves) == 4:
            return (leaves[0], Expression('List', *leaves[1:4]))
        else:
            return leaves
        
    prepare_sympy = prepare_sage
    
    def from_sympy(self, leaves):
        args = []
        for leaf in leaves[1:]:
            if leaf.has_form('List', 1):
                # {x} -> x
                args.append(leaf.leaves[0])
            else:
                args.append(leaf)
        return [leaves[0]] + args
    
    def apply(self, f, xs, evaluation):
        'Integrate[f_, xs__]'
        
        f_sympy = f.to_sympy()
        xs = xs.get_sequence()
        vars = []
        prec = None
        for x in xs:
            if x.has_form('List', 3):
                x, a, b = x.leaves
                prec_a = a.get_precision()
                prec_b = b.get_precision()
                if prec_a is not None and prec_b is not None:
                    prec_new = min(prec_a, prec_b)
                    if prec is None or prec_new < prec:
                        prec = prec_new
                a = a.to_sympy()
                b = b.to_sympy()
            else:
                a = b = None
                a_mathics, b_mathics = a, b
            if not x.get_name():
                evaluation.message('Integrate', 'ilim')
                return
            x = x.to_sympy()
            if a is None or b is None:
                vars.append(x)
            else:
                vars.append((x, a, b))
        try:
            result = sympy.integrate(f_sympy, *vars)
        except sympy.PolynomialError:
            return
            
        if prec is not None:
            result = sympy.N(result)
        result = from_sympy(result)
        return result
                
class Solve(Builtin):
    """
    <dl>
    <dt>'Solve[$equation$, $vars$]'
        <dd>attempts to solve $equation$ for the variables $vars$.
    <dt>'Solve[$equation$, $vars$, $domain$]'
        <dd>restricts variables to $domain$, which can be 'Complexes' or 'Reals'.
    </dl>
    
    >> Solve[x ^ 2 - 3 x == 4, x]
     = {{x -> -1}, {x -> 4}}
    >> Solve[4 y - 8 == 0, y]
     = {{y -> 2}}
     
    Apply the solution:
    >> sol = Solve[2 x^2 - 10 x - 12 == 0, x]
     = {{x -> -1}, {x -> 6}}
    >> x /. sol
     = {-1, 6}
     
    Contradiction:
    >> Solve[x + 1 == x, x]
     = {}
    Tautology:
    >> Solve[x ^ 2 == x ^ 2, x]
     = {{}}
     
    Rational equations:
    >> Solve[x / (x ^ 2 + 1) == 1, x]
     = {{x -> 1 / 2 + I / 2 Sqrt[3]}, {x -> 1 / 2 - I / 2 Sqrt[3]}} 
    >> Solve[(x^2 + 3 x + 2)/(4 x - 2) == 0, x]
     = {{x -> -2}, {x -> -1}}
     
    Transcendental equations:
    >> Solve[Cos[x] == 0, x]
     = {{x -> Pi / 2}}
     
    Solve can only solve equations with respect to symbols or functions:
    >> Solve[f[x + y] == 3, f[x + y]]
     = {{f[x + y] -> 3}}
    >> Solve[a + b == 2, a + b]
     : a + b is not a valid variable.
     = Solve[a + b == 2, a + b]
    This happens when solving with respect to an assigned symbol:
    >> x = 3;
    >> Solve[x == 2, x]
     : 3 is not a valid variable.
     = Solve[False, 3]
    >> Clear[x]
    >> Solve[a < b, a]
     : a < b is not a well-formed equation.
     = Solve[a < b, a]
     
    Solve a system of equations:
    >> eqs = {3 x ^ 2 - 3 y == 0, 3 y ^ 2 - 3 x == 0};
    >> sol = Solve[eqs, {x, y}]
     = {{x -> 0, y -> 0}, {x -> 1, y -> 1}, {x -> (-1 / 2 + I / 2 Sqrt[3]) ^ 2, y -> -1 / 2 + I / 2 Sqrt[3]}, {x -> (-1 / 2 - I / 2 Sqrt[3]) ^ 2, y -> -1 / 2 - I / 2 Sqrt[3]}}
    >> eqs /. sol // Simplify
     = {{True, True}, {True, True}, {True, True}, {True, True}}
     
    An underdetermined system:
    >> Solve[x^2 == 1 && z^2 == -1, {x, y, z}]
     : Equations may not give solutions for all "solve" variables.
     = {{x -> -1, z -> -I}, {x -> -1, z -> I}, {x -> 1, z -> -I}, {x -> 1, z -> I}}
     
    Domain specification:
    >> Solve[x^2 == -1, x, Reals]
     = {}
    >> Solve[x^2 == 1, x, Reals]
     = {{x -> -1}, {x -> 1}}
    >> Solve[x^2 == -1, x, Complexes]
     = {{x -> -I}, {x -> I}}
     
    #> Solve[x^5==x,x]
     = {{x -> -1}, {x -> 0}, {x -> 1}, {x -> -I}, {x -> I}}
     
    #> Solve[g[x] == 0, x]
     = Solve[g[x] == 0, x]
    ## (should use inverse functions, actually!)
    #> Solve[g[x] + h[x] == 0, x]
     = Solve[g[x] + h[x] == 0, x]
     
    #> Solve[Sin(x) == 1, x]
     = {{x -> 1 / Sin}}
     
    #> Solve[E == 1, E]
     : E is not a valid variable.
     = Solve[False, E]
    #> Solve[False, Pi]
     : Pi is not a valid variable.
     = Solve[False, Pi]
    """
    
    messages = {
        'eqf': "`1` is not a well-formed equation.",
        'svars': "Equations may not give solutions for all \"solve\" variables.",
    }
    
    rules = {
        'Solve[eqs_, vars_, Complexes]': 'Solve[eqs, vars]',
        'Solve[eqs_, vars_, Reals]': """Select[Solve[eqs, vars],
            And @@ ((!NumberQ[#] || Im[#] == 0 &) [Chop[N[#], 10^-15]] & /@
            (If[ListQ[vars], vars, {vars}] /. #)) &]"""
    }
    
    def apply(self, eqs, vars, evaluation):
        'Solve[eqs_, vars_]'
        
        vars_original = vars
        head_name = vars.get_head_name()
        if head_name == 'List':
            vars = vars.leaves
        else:
            vars = [vars]
        for var in vars:
            if (var.is_atom() and not var.is_symbol()) or \
                head_name in ('Plus', 'Times', 'Power') or \
                'Constant' in var.get_attributes(evaluation.definitions):
                evaluation.message('Solve', 'ivar', vars_original)
                return
        eqs_original = eqs
        if eqs.get_head_name() in ('List', 'And'):
            eqs = eqs.leaves
        else:
            eqs = [eqs]
        sympy_eqs = []
        sympy_denoms = []
        for eq in eqs:
            symbol_name = eq.get_name()
            if symbol_name == 'True':
                pass
            elif symbol_name == 'False':
                return Expression('List')
            elif not eq.has_form('Equal', 2):
                return evaluation.message('Solve', 'eqf', eqs_original)
            else:
                left, right = eq.leaves
                left = left.to_sympy()
                right = right.to_sympy()
                eq = left - right
                eq = sympy.together(eq)
                eq = sympy.cancel(eq)                
                sympy_eqs.append(eq)
                numer, denom = eq.as_numer_denom()
                sympy_denoms.append(denom)
        
        vars_sympy = [var.to_sympy() for var in vars]
        
        # delete unused variables to avoid SymPy's
        # PolynomialError: Not a zero-dimensional system
        # in e.g. Solve[x^2==1&&z^2==-1,{x,y,z}]
        all_vars = vars[:]
        all_vars_sympy = vars_sympy[:]
        vars = []
        vars_sympy = []   
        for var, var_sympy in zip(all_vars, all_vars_sympy):
            pattern = Pattern.create(var)
            if not eqs_original.is_free(pattern, evaluation):
                vars.append(var)
                vars_sympy.append(var_sympy)
        
        def transform_dict(sols):
            if not sols:
                yield sols
            for var, sol in sols.iteritems():
                rest = sols.copy()
                del rest[var]
                rest = transform_dict(rest)
                if not isinstance(sol, (tuple, list)):
                    sol = [sol]
                if not sol:
                    for r in rest:
                        yield r
                else:
                    for r in rest:
                        for item in sol:
                            new_sols = r.copy()
                            new_sols[var] = item
                            yield new_sols
                break
        
        def transform_solution(sol):
            if not isinstance(sol, dict):
                if not isinstance(sol, (list, tuple)):
                    sol = [sol]
                sol = dict(zip(vars_sympy, sol))
            return transform_dict(sol)
        
        if not sympy_eqs:
            sympy_eqs = True
        elif len(sympy_eqs) == 1:
            sympy_eqs = sympy_eqs[0]
            
        try:
            if isinstance(sympy_eqs, bool):
                result = sympy_eqs
            else:
                result = sympy.solve(sympy_eqs, vars_sympy)
            if not isinstance(result, list):
                result = [result]
            if result == [True]:
                return Expression('List', Expression('List'))
            if result == [None]:
                return Expression('List')
            results = []
            for sol in result:
                results.extend(transform_solution(sol))
            result = results
            if any(sol and any(var not in sol for var in all_vars_sympy) for sol in result):
                evaluation.message('Solve', 'svars')
            result = [sol for sol in result if all(sympy.simplify(denom.subs(sol)) != 0 for denom in sympy_denoms)]   # filter out results for which denominator is 0
                # (SymPy should actually do that itself, but it doesn't!)
            return Expression('List', *(Expression('List', *(Expression('Rule', var, from_sympy(sol[var_sympy]))
                for var, var_sympy in zip(vars, vars_sympy) if var_sympy in sol)) for sol in result))
        except sympy.PolynomialError:
            # raised for e.g. Solve[x^2==1&&z^2==-1,{x,y,z}] when not deleting unused variables beforehand
            pass
        except NotImplementedError:
            pass
        except TypeError, exc:
            if str(exc).startswith("expected Symbol, Function or Derivative"):
                evaluation.message('Solve', 'ivar', vars_original)
        
class Limit(Builtin):
    """
    <dl>
    <dt>'Limit[$expr$, $x$->$x0$]'
        <dd>gives the limit of $expr$ as $x$ approaches $x0$.
    <dt>'Limit[$expr$, $x$->$x0$, Direction->1]'
        <dd>approaches $x0$ from smaller values.
    <dt>'Limit[$expr$, $x$->$x0$, Direction->-1]'
        <dd>approaches $x0$ from larger values.
    </dl>
    
    >> Limit[x, x->2]
     = 2
    >> Limit[Sin[x] / x, x->0]
     = 1
    >> Limit[1/x, x->0, Direction->-1]
     = Infinity
    >> Limit[1/x, x->0, Direction->1]
     = -Infinity
    """
    
    attributes = ('Listable',)
    
    options = {
        'Direction': '1',
    }
    
    messages = {
        'ldir': "Value of Direction -> `1` should be -1 or 1.",
    }
    
    def apply(self, expr, x, x0, evaluation, options={}):
        'Limit[expr_, x_->x0_, OptionsPattern[Limit]]'
        
        expr = expr.to_sympy()
        x = x.to_sympy()
        x0 = x0.to_sympy()
        
        direction = self.get_option(options, 'Direction', evaluation)
        value = direction.get_int_value()
        if value not in (-1, 1):
            evaluation.message('Limit', 'ldir', direction)
        if value > 0:
            dir_sympy = '-'
        else:
            dir_sympy = '+'
        
        try:
            result = sympy.limit(expr, x, x0, dir_sympy)
            return from_sympy(result)
        except sympy.PoleError:
            pass
        except NotImplementedError:
            pass
    
class FindRoot(Builtin):
    r"""
    <dl>
    <dt>'FindRoot[$f$, {$x$, $x0$}]'
        <dd>searches for a numerical root of $f$, starting from '$x$=$x0$'.
    <dt>'FindRoot[$lhs$ == $rhs$, {$x$, $x0$}]'
        <dd>tries to solve the equation '$lhs$ == $rhs$'.
    </dl>
    
    'FindRoot' uses Newton\'s method, so the function of interest should have a first derivative.
    
    >> FindRoot[Cos[x], {x, 1}]
     = {x -> 1.57079632679489662}
    >> FindRoot[Sin[x] + Exp[x],{x, 0}]
     = {x -> -0.588532743981861077}
     
    >> FindRoot[Sin[x] + Exp[x] == Pi,{x, 0}]
     = {x -> 0.866815239911458064}
     
    'FindRoot' has attribute 'HoldAll' and effectively uses 'Block' to localize $x$.
    However, in the result $x$ will eventually still be replaced by its value.
    >> x = 3;
    >> FindRoot[Tan[x] + Sin[x] == Pi, {x, 1}]
     = {3 -> 1.14911295431426855}
    >> Clear[x]
    
    'FindRoot' stops after 100 iterations:
    >> FindRoot[x^2 + x + 1, {x, 1}]
     : The maximum number of iterations was exceeded. The result might be inaccurate.
     = {x -> -1.}
     
    Find complex roots:
    >> FindRoot[x ^ 2 + x + 1, {x, -I}]
     = {x -> -0.5 - 0.866025403784438647 I}
     
    The function has to return numerical values:
    >> FindRoot[f[x] == 0, {x, 0}]
     : The function value is not a number at x = 0..
     = FindRoot[f[x] - 0, {x, 0}]
     
    The derivative must not be 0:
    >> FindRoot[Sin[x] == x, {x, 0}]
     : Encountered a singular derivative at the point x = 0..
     = FindRoot[Sin[x] - x, {x, 0}]
     
    #> FindRoot[2.5==x,{x,0}]
     = {x -> 2.5}
    """
    
    attributes = ('HoldAll',)
    
    messages = {
        'snum': "Value `1` is not a number.",
        'nnum': "The function value is not a number at `1` = `2`.",
        'dsing': "Encountered a singular derivative at the point `1` = `2`.",
        'maxiter': "The maximum number of iterations was exceeded. The result might be inaccurate.",
    }
    
    rules = {
        'FindRoot[lhs_ == rhs_, {x_, xs_}]': 'FindRoot[lhs - rhs, {x, xs}]',
    }
    
    def apply(self, f, x, x0, evaluation):
        'FindRoot[f_, {x_, x0_}]'
        
        x0 = Expression('N', x0).evaluate(evaluation)
        if not isinstance(x0, Number):
            evaluation.message('FindRoot', 'snum', x0)
            return
        x_name = x.get_name()
        if not x_name:
            evaluation.message('FindRoot', 'sym', x, 2)
            return
        count = 0
        
        def diff(evaluation):
            return Expression('D', f, x).evaluate(evaluation)
        
        d = dynamic_scoping(diff, {x_name: None}, evaluation)
        
        def sub(evaluation):
            d_value = d.evaluate(evaluation)
            if d_value == Integer(0):
                return None
            return Expression('Times', f, Expression('Power', d_value, Integer(-1))).evaluate(evaluation)
        
        while count < 100:
            minus = dynamic_scoping(sub, {x_name: x0}, evaluation)
            if minus is None:
                evaluation.message('FindRoot', 'dsing', x_name, x0)
                return
            x1 = Expression('Plus', x0, Expression('Times', Integer(-1), minus)).evaluate(evaluation)
            if not isinstance(x1, Number):
                evaluation.message('FindRoot', 'nnum', x_name, x0)
                return
            if x1 == x0:
                break
            x0 = x1.evaluate(evaluation)
            count += 1
        else:
            evaluation.message('FindRoot', 'maxiter')
            
        return Expression('List', Expression('Rule', x, x0))
                
