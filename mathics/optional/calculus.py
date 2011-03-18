"""
Calculus functions

Advanced calculus functions depending on \Sage.
"""

from sage import all as sage

from mathics.optional.base import OptionalSageFunction
from mathics.core.convert import from_sage, to_sage
from mathics.core.expression import Expression

class SageIntegrate(OptionalSageFunction):
    """
    <dl>
    <dt>'SageIntegrate[$f$, $x$]'
        <dd>integrates $f$ with respect to $x$. The result does not contain the additive integration constant.
    <dt>'SageIntegrate[$f$, {$x$, $a$, $b$}]'
        <dd>computes the definite integral of $f$ with respect to $x$ from $a$ to $b$.
    </dl>
    
    Integrate a polynomial:
    >> SageIntegrate[6 x ^ 2 + 3 x ^ 2 - 4 x + 10, x]
     = 10 x - 2 x ^ 2 + 3 x ^ 3
     
    >> SageIntegrate[ArcSin[x / 3], x]
     = x ArcSin[x / 3] + 3 Sqrt[1 - x ^ 2 / 9]
    
    \Sage can integrate functions that SymPy cannot integrate:
    ##>> Integrate[ArcSin[x / 3], x]
    ## = Integrate[ArcSin[x / 3], x]
    >> Integrate[Sqrt[Tan[x]], x]
     = Integrate[Sqrt[Tan[x]], x]
    >> SageIntegrate[Sqrt[Tan[x]], x]
     = -Sqrt[2] Log[1 + Tan[x] + Sqrt[2] Sqrt[Tan[x]]] / 4 + Sqrt[2] Log[1 + Tan[x] - Sqrt[2] Sqrt[Tan[x]]] / 4 + Sqrt[2] ArcTan[-Sqrt[2] (Sqrt[2] - 2 Sqrt[Tan[x]]) / 2] / 2 + Sqrt[2] ArcTan[Sqrt[2] (Sqrt[2] + 2 Sqrt[Tan[x]]) / 2] / 2
    
    #> Integrate[f[x], {x, a, b}]
     = Integrate[f[x], {x, a, b}]
    #> Integrate[f'[x], {x, a, b}]
     = Integrate[f'[x], {x, a, b}]
     
    #> SageIntegrate[Abs[Sin[phi]], {phi, 0, 2Pi}] // N
     = 4.
    #> SageIntegrate[Hold[x + x], {x, a, b}]
     = SageIntegrate[Hold[x + x], {x, a, b}]
    ## see Sage bug http://trac.sagemath.org/sage_trac/ticket/10914
    """
    
    attributes = ('ReadProtected',)
    
    sage_name = 'integrate'
    sympy_name = ''
    
    # messages are taken from Integrate
    
    rules = {
        'SageIntegrate[list_List, x_]': 'SageIntegrate[#, x]& /@ list',
        
        'MakeBoxes[SageIntegrate[f_, x_], form:StandardForm|TraditionalForm]':
            r'RowBox[{"\[Integral]", MakeBoxes[f, form], "\[InvisibleTimes]", RowBox[{"\[DifferentialD]", MakeBoxes[x, form]}]}]',
    }
    
    def prepare_sage(self, leaves):
        if len(leaves) == 2:
            x = leaves[1]
            if x.has_form('List', 3):
                return ('SageIntegrate', [leaves[0]] + x.leaves)
            else:
                return ('SageIntegrate', leaves)
        return leaves
            
    def from_sage(self, leaves):
        if len(leaves) == 4:
            return (leaves[0], Expression('List', *leaves[1:4]))
        else:
            return leaves
        
    prepare_sympy = prepare_sage
    
    def from_sympy(self, leaves):
        args = []
        if len(leaves) > 1 and leaves[1].has_form('List', None):
            for arg in leaves[1].leaves:
                if arg.has_form('List', 2):
                    if arg.leaves[1].get_name() == 'Null':
                        args.append(arg.leaves[0])
                    elif arg.leaves[1].has_form('List', 2):
                        args.append(Expression('List', arg.leaves[0], arg.leaves[1].leaves[0], arg.leaves[1].leaves[1]))
        return [leaves[0]] + args
    
    def apply(self, f, xs, evaluation):
        'SageIntegrate[f_, xs__]'
        
        xs = xs.get_sequence()
        vars = []
        for x in xs:
            if x.has_form('List', 3):
                x, a, b = x.leaves
            else:
                a = b = None
            if not x.get_name():
                return evaluation.message('Integrate', 'ilim')
            if a is None or b is None:
                vars.append(x)
            else:
                vars.append((x, a, b))
        (f, vars), subs = to_sage((f, vars), evaluation)
        
        try:
            result = sage.integrate(f, *vars)
        except TypeError:
            # SageIntegrate[f[x], x] raises TypeError because maxima can't handle unknown functions, obviously
            return
        if a is not None and b is not None:
            prec_a = a.get_precision()
            prec_b = b.get_precision()
            if prec_a is not None and prec_b is not None:
                #prec = min(prec_a, prec_b)
                result = sage.n(result)
                
        return from_sage(result, subs)
