# -*- coding: utf8 -*-

"""
Linear algebra
"""

import sympy

from mathics.builtin.base import Builtin
from mathics.core.convert import from_sympy
from mathics.core.expression import Expression, Integer

def matrix_data(m):
    if not m.has_form('List', None):
        return None
    if not all(leaf.has_form('List', None) for leaf in m.leaves):
        return None
    return [[item.to_sympy() for item in row.leaves] for row in m.leaves]

def to_sympy_matrix(data):
    """if not m.has_form('List', None):
        return None
    if not all(leaf.has_form('List', None) for leaf in m.leaves):
        return None
    data = [[item.to_sympy() for item in row.leaves] for row in m.leaves]"""
    if not isinstance(data, list):
        data = matrix_data(data)
    try:
        return sympy.Matrix(data)
    except (TypeError, AssertionError):
        return None

class Det(Builtin):
    u"""
    <dl>
    <dt>'Det[$m$]'
        <dd>computes the determinant of the matrix $m$.
    </dl>
    
    >> Det[{{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}]
     = -2
     
    Symbolic determinant:
    >> Det[{{a, b, c}, {d, e, f}, {g, h, i}}]
     = a e i - a f h - b d i + b f g + c d h - c e g
    """
    
    def apply(self, m, evaluation):
        'Det[m_]'
        
        matrix = to_sympy_matrix(m)
        if matrix is None or matrix.cols != matrix.rows or matrix.cols == 0:
            return evaluation.message('Det', 'matsq', m)
        det = matrix.det()
        return from_sympy(det)

class Inverse(Builtin):
    """
    <dl>
    <dt>'Inverse[$m$]'
        <dd>computes the inverse of the matrix $m$.
    </dl>
    
    >> Inverse[{{1, 2, 0}, {2, 3, 0}, {3, 4, 1}}]
     = {{-3, 2, 0}, {2, -1, 0}, {1, -2, 1}}
    >> Inverse[{{1, 0}, {0, 0}}]
     : The matrix {{1, 0}, {0, 0}} is singular.
     = Inverse[{{1, 0}, {0, 0}}]
    """
    
    messages = {
        'sing': "The matrix `1` is singular.",
    }
    
    def apply(self, m, evaluation):
        'Inverse[m_]'
        
        matrix = to_sympy_matrix(m)
        if matrix is None or matrix.cols != matrix.rows or matrix.cols == 0:
            return evaluation.message('Inverse', 'matsq', m)
        if matrix.det() == 0:
            return evaluation.message('Inverse', 'sing', m)
        inv = matrix.inv()
        return from_sympy(inv)
    
class LinearSolve(Builtin):
    """
    <dl>
    <dt>'LinearSolve[$matrix$, $right$]'
        <dd>solves the linear equation system '$matrix$ . x = $right$' and returns one corresponding solution 'x'.
    </dl>
    
    >> LinearSolve[{{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}, {1, 2, 3}]
     = {0, 1, 2}
    Test the solution:
    >> {{1, 1, 0}, {1, 0, 1}, {0, 1, 1}} . {0, 1, 2}
     = {1, 2, 3}
    If there are several solutions, one arbitrary solution is returned:
    >> LinearSolve[{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}, {1, 1, 1}]
     = {-1, 1, 0}
    Infeasible systems are reported:
    >> LinearSolve[{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}, {1, -2, 3}]
     : Linear equation encountered that has no solution.
     = LinearSolve[{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}, {1, -2, 3}]
    """
    
    messages = {
        'lslc': "Coefficient matrix and target vector(s) or matrix do not have the same dimensions.", 
        'nosol': "Linear equation encountered that has no solution.",
    }
    
    def apply(self, m, b, evaluation):
        'LinearSolve[m_, b_]'
        
        matrix = matrix_data(m)
        if matrix is None:
            return
        if not b.has_form('List', None):
            return
        if len(b.leaves) != len(matrix):
            return evaluation.message('LinearSolve', 'lslc')
        system = [m + [v] for m, v in zip(matrix, b.leaves)]
        system = to_sympy_matrix(system)
        if system is None:
            return
        syms = [sympy.Symbol('LinearSolve_var%d' % k, dummy=True) for k in range(system.cols - 1)]
        sol = sympy.solve_linear_system(system, *syms)
        if sol:
            # substitute 0 for variables that are not in result dictionary 
            free_vars = dict((sym, sympy.Integer(0)) for sym in syms if sym not in sol)
            sol.update(free_vars)
            sol = [(sol[sym] if sym in free_vars else sol[sym].subs(free_vars)) for sym in syms]
            return from_sympy(sol)
        else:
            return evaluation.message('LinearSolve', 'nosol')
        
class NullSpace(Builtin):
    """
    <dl>
    <dt>'NullSpace[$matrix$]'
        <dd>returns a list of vectors that span the nullspace of $matrix$.
    </dl>
    
    >> NullSpace[{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}]
     = {{1, -2, 1}}
     
    >> A = {{1, 1, 0}, {1, 0, 1}, {0, 1, 1}};
    >> NullSpace[A]
     = {}
    >> MatrixRank[A]
     = 3
    """
    
    def apply(self, m, evaluation):
        'NullSpace[m_]'
        
        matrix = to_sympy_matrix(m)
        if matrix is None:
            return
        nullspace = matrix.nullspace()
        # convert n x 1 matrices to vectors
        nullspace = [list(vec) for vec in nullspace]
        return from_sympy(nullspace)
    
class RowReduce(Builtin):
    """
    <dl>
    <dt>'RowReduce[$matrix$]'
        <dd>returns the reduced row-echelon form of $matrix$.
    </dl>
    
    >> RowReduce[{{1, 0, a}, {1, 1, b}}]
     = {{1, 0, a}, {0, 1, -a + b}}
     
    >> RowReduce[{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}] // MatrixForm
     = 1   0   -1
     .
     . 0   1   2
     .
     . 0   0   0
    """
    
    def apply(self, m, evaluation):
        'RowReduce[m_]'
        
        matrix = to_sympy_matrix(m)
        if matrix is None:
            return
        reduced = matrix.rref()[0]
        return from_sympy(reduced)
    
class MatrixRank(Builtin):
    """
    <dl>
    <dt>'MatrixRank[$matrix$]'
        <dd>returns the rank of $matrix$.
    </dl>
    
    >> MatrixRank[{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}]
     = 2
    >> MatrixRank[{{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}]
     = 3
    >> MatrixRank[{{a, b}, {3 a, 3 b}}]
     = 1
    """
    
    def apply(self, m, evaluation):
        'MatrixRank[m_]'
        
        matrix = to_sympy_matrix(m)
        if matrix is None:
            return
        rank = len(matrix.rref()[1])
        return Integer(rank)

class Eigenvalues(Builtin):
    """
    <dl>
    <dt>'Eigenvalues[$m$]'
        <dd>computes the eigenvalues of the matrix $m$.
    </dl>
    
    >> Eigenvalues[{{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}]
     = {2, 1, -1}
    """
    
    def apply(self, m, evaluation):
        'Eigenvalues[m_]'
        
        matrix = to_sympy_matrix(m)
        if matrix is None or matrix.cols != matrix.rows or matrix.cols == 0:
            return evaluation.message('Eigenvalues', 'matsq', m)
        eigenvalues = matrix.eigenvals()
        eigenvalues = sorted(eigenvalues.iteritems(), key=lambda (v, c): abs(v), reverse=True)
        result = []
        for val, count in eigenvalues:
            result.extend([val] * count)
        return from_sympy(result)

class Eigenvectors(Builtin):
    """
    <dl>
    <dt>'Eigenvectors[$m$]'
        <dd>computes the eigenvectors of the matrix $m$.
    </dl>
    
    >> Eigenvectors[{{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}]
     = {{1, 1, 1}, {-1, 0, 1}, {1, -2, 1}}
    >> Eigenvectors[{{1, 0, 0}, {0, 1, 0}, {0, 0, 0}}]
     = {{0, 1, 0}, {1, 0, 0}, {0, 0, 1}}
    >> Eigenvectors[{{2, 0, 0}, {0, -1, 0}, {0, 0, 0}}]
     = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
    >> Eigenvectors[{{0.1, 0.2}, {0.8, 0.5}}]
     : Eigenvectors is not yet implemented for the matrix {{0.1, 0.2}, {0.8, 0.5}}.
     = Eigenvectors[{{0.1, 0.2}, {0.8, 0.5}}]
    """
    
    messages = {
        'eigenvecnotimplemented': "Eigenvectors is not yet implemented for the matrix `1`.",
    }
    
    def apply(self, m, evaluation):
        'Eigenvectors[m_]'
        
        matrix = to_sympy_matrix(m)
        if matrix is None or matrix.cols != matrix.rows or matrix.cols == 0:
            return evaluation.message('Eigenvectors', 'matsq', m)
        # sympy raises an error for some matrices that Mathematica can compute.
        try: 
            eigenvects = matrix.eigenvects()
        except NotImplementedError:
            return evaluation.message('Eigenvectors', 'eigenvecnotimplemented', m)
        # Mathematica seems to sort differently; eivenvalue ordering is first
        # by abs(val), second by val, e.g. {2, -1, 1}, instead of {2, 1, -1}
        # for Eigenvalue[{{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}].
        # The eigenvectors are given in the same order as the eigenvalues.
        eigenvects = sorted(eigenvects, key=lambda (val, c, vect): abs(val), reverse=True)
        result = []
        for val, count, basis in eigenvects:
            # select the i'th basis vector, convert matrix to vector, and convert from sympy
            vects = [from_sympy(list(basis[i])) for i in range(count)]
            # this follows Mathematica convention better; higher indexed pivots are outputted first.
            # E.g. {{0, 1}, {1, 0}} instead of {{1, 0}, {0, 1}}.
            vects.reverse()
            result.extend(vects)
        result.extend([Expression('List', *([0] * matrix.rows))] * (matrix.rows - len(result)))
        return Expression('List', *result)
