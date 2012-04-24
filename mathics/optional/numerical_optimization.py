"""
Numerical optimization

Some numerical optimization functions depending on \Sage.
"""

from gmpy import mpf

from sage import all as sage
#from sage.numerical.knapsack import knapsack
#from sage.all import RDF, vector, matrix
#from sage.numerical.optimize import linear_program
from sage.all import vector, matrix, RDF

from mathics.optional.base import OptionalSageFunction, catching
from mathics.core.convert import from_sage, to_sage
from mathics.core.expression import Expression, Symbol

# fix Sage's linear_program to support programs without optimal solution
# (in fact, we don't really need Sage here - the package cvxopt would be enough!)
def linear_program(c,G,h,A=None,b=None,solver=None):
    """
    Solves the dual linear programs:

    - Minimize  `c'x` subject to `Gx + s = h`, `Ax = b`, and `s \geq 0` where
      `'` denotes transpose.

    - Maximize  `-h'z - b'y` subject to `G'z + A'y + c = 0` and `z \geq 0`.


    INPUT:

    - ``c`` -- a vector

    - ``G`` -- a matrix

    - ``h`` -- a vector

    - ``A`` -- a matrix

    - ``b`` --- a vector

    - ``solver`` (optional) --- solver to use. If None, the cvxopt's lp-solver
                                is used. If it is 'glpk', then glpk's solver
                                is used.
 
    These can be over any field that can be turned into a floating point
    number.
    
   
    OUTPUT:

    A dictionary ``sol`` with keys ``x``, ``s``, ``y``, ``z`` corresponding
    to the variables above:

    - ``sol['x']`` -- the solution to the linear program

    - ``sol['s']`` -- the slack variables for the solution

    - ``sol['z']``, ``sol['y']`` -- solutions to the dual program


    EXAMPLES:

    First, we minimize `-4x_1 - 5x_2` subject to `2x_1 + x_2 \leq 3`,
    `x_1 +  2x_2 \leq 3`, `x_1 \geq 0`, and `x_2 \geq 0`::

        sage: c=vector(RDF,[-4,-5])
        sage: G=matrix(RDF,[[2,1],[1,2],[-1,0],[0,-1]])
        sage: h=vector(RDF,[3,3,0,0])
        sage: sol=linear_program(c,G,h) 
        sage: sol['x'] 
        (0.999..., 1.000...)

    Next, we maximize `x+y-50` subject to `50x + 24y \leq 2400`,
    `30x + 33y \leq 2100`, `x \geq 45`, and `y \geq 5`::

        sage: v=vector([-1.0,-1.0,-1.0])
        sage: m=matrix([[50.0,24.0,0.0],[30.0,33.0,0.0],[-1.0,0.0,0.0],[0.0,-1.0,0.0],[0.0,0.0,1.0],[0.0,0.0,-1.0]])
        sage: h=vector([2400.0,2100.0,-45.0,-5.0,1.0,-1.0])
        sage: sol=linear_program(v,m,h)
        sage: sol['x']
        (45.000000..., 6.2499999...3, 1.00000000...)
        sage: sol=linear_program(v,m,h,solver='glpk')
        sage: sol['x']
        (45.0..., 6.25, 1.0...)
    """
    from cvxopt.base import matrix as m
    from cvxopt import solvers
    solvers.options['show_progress']=False
    if solver=='glpk':
        from cvxopt import glpk 
        glpk.options['LPX_K_MSGLEV'] = 0 
    c_=m(c.base_extend(RDF).numpy())
    G_=m(G.base_extend(RDF).numpy())
    h_=m(h.base_extend(RDF).numpy())
    if A!=None and b!=None:
        A_=m(A.base_extend(RDF).numpy())
        b_=m(b.base_extend(RDF).numpy())
        sol=solvers.lp(c_,G_,h_,A_,b_,solver=solver)
    else:
        sol=solvers.lp(c_,G_,h_,solver=solver)
    status=sol['status']
    # CHANGED:
    #if status != 'optimal':
    if status not in ('optimal', 'dual infeasible'):
       return  {'x':None,'s':None,
           #'y':None, 'z':None,
           'status':status}
    x=vector(RDF,list(sol['x']))
    s=vector(RDF,list(sol['s']))
    #y=vector(RDF,list(sol['y']))
    #z=vector(RDF,list(sol['z']))
    #return  {'primal objective':sol['primal objective'],'x':x,'s':s,'y':y,
    #           'z':z,'status':status}
    return {'x': x, 's': s, 'status': status}

def unit_vector(k, n, e=1):
    return [0] * k + [e] + [0] * (n - k - 1)

class LinearProgramming(OptionalSageFunction):
    """
    <dl>
    <dt>'LinearProgramming[$c$, $G$, $h$]'
        <dd>solves the linear program given by 'min $c$.$x$' s.t. '$G$.$x$ >= $h$', '$x$ >= 0' and returns $x$.
    </dl>
    
    >> LinearProgramming[{1, 1}, {{2, 2}}, {4}]
     = {1., 1.}
    >> LinearProgramming[{1}, {{-2}}, {4}]
     : No solution can be found that satisfies the constraints.
     = LinearProgramming[{1}, {{-2}}, {4}]
    >> LinearProgramming[{-1, -1}, {{2, 2}}, {4}]
     : This problem is unbounded.
     = {Indeterminate, Indeterminate}
    """
    
    messages = {
        'lpnn': "Input data contains elements that are empty matrices, invalid vectors or matrices, or not real numbers.",
        'lpsub': "This problem is unbounded.",
        'lpsnf': "No solution can be found that satisfies the constraints.",
    }
    
    #@catching
    def apply(self, c, G, h, evaluation):
        "LinearProgramming[c_, G_, h_]"
        
        (c, G, h), subs = to_sage((c, G, h), evaluation)
        n = len(c)
        c = vector(c)
        G = matrix([[-item for item in row] for row in G] + [unit_vector(k, n, -1) for k in range(n)])
        h = vector([-item for item in h] + [0] * n)
        result = linear_program(c, G, h)
        status = result['status']
        if status == 'dual infeasible':
            evaluation.message('LinearProgramming', 'lpsub')
            return Expression('List', *([Symbol('Indeterminate')] * n))
        elif status == 'primal infeasible':
            return evaluation.message('LinearProgramming', 'lpsnf')
        #print result
        x = result['x']
        x = [round(value, 6) for value in x]  # round result to 6 digits after comma
        return from_sage(x, subs)

# knapsack is only available in Sage >= 4.6
"""
class Knapsack(OptionalSageFunction):
    ""
    <dl>
    <dt>'Knapsack[{{$weight1$, $value1$}, {$weight2$, $value2$}, ...}, $maxweight$]'
        <dd>solves the given binary knapsack problem.
    </dl>
    ""
    
    messages = {
        'empty': "Empty knapsack given.",
        #'invalid': ,
    }
    
    @catching
    def apply(self, items, maxweight, evaluation):
        "Knapsack[{items___}, maxweight_]"
        
        (items, maxweight), subs = to_sage((items.get_sequence(), maxweight), evaluation)
        if not items:
            return evaluation.message('Knapsack', 'empty')
        items = [tuple(item) for item in items]
        result = knapsack(items, max=maxweight)
        return from_sage(result, subs)
"""