# -*- coding: utf8 -*-

"""
Linear algebra
"""

import sympy
from mpmath import mp

from mathics.builtin.base import Builtin
from mathics.core.convert import from_sympy
from mathics.core.expression import Expression, Integer


def matrix_data(m):
    if not m.has_form('List', None):
        return None
    if not all(leaf.has_form('List', None) for leaf in m.leaves):
        return None
    return [[item.to_sympy() for item in row.leaves] for row in m.leaves]


def to_sympy_matrix(data, **kwargs):
    if not isinstance(data, list):
        data = matrix_data(data)
    try:
        return sympy.Matrix(data)
    except (TypeError, AssertionError, ValueError):
        return None

def to_mpmath_matrix(data, **kwargs):
    def mpmath_matrix_data(m):
        if not m.has_form('List', None):
            return None
        if not all(leaf.has_form('List', None) for leaf in m.leaves):
            return None
        return [[str(item) for item in row.leaves] for row in m.leaves]


    if not isinstance(data, list):
        data = mpmath_matrix_data(data)
    try:
        return mp.matrix(data)
    except (TypeError, AssertionError, ValueError):
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

    >> Inverse[{{1, 0, 0}, {0, Sqrt[3]/2, 1/2}, {0,-1 / 2, Sqrt[3]/2}}]
    = {{1, 0, 0}, {0, Sqrt[3] / 2, -1 / 2}, {0, 1 / 2, Sqrt[3] / 2}}
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


class SingularValueDecomposition(Builtin):
    """
    <dl>
    <dt>'SingularValueDecomposition[$m$]'
        <dd>Calculate the singular value decomposition for matrix $m$.
        Returns $u, s, w$ such that $m=u s v$, $u'u=1$, $v'v=1$, $s$ is diagonal.
    </dl>

    >> SingularValueDecomposition[{{1, 2}, {2, 3}, {3, 4}}]
     = {{-11 / 6, -1 / 3, 7 / 6}, {4 / 3, 1 / 3, -2 / 3}}

    >> PseudoInverse[{{1, 2, 0}, {2, 3, 0}, {3, 4, 1}}]
     = {{-3, 2, 0}, {2, -1, 0}, {1, -2, 1}}
    """


    def apply(self, m, evaluation):
        'SingularValueDecomposition[m_]'

        matrix = to_mpmath_matrix(m)
        U, S, V = mp.svd(matrix)
        S = mp.diag(S)
        print(U, S, V)
        U_list = Expression('List', *U.tolist())
        S_list = Expression('List', *S.tolist())
        V_list = Expression('List', *V.tolist())
        return Expression('List', *[U_list, S_list, V_list])


class PseudoInverse(Builtin):
    """
    <dl>
    <dt>'PseudoInverse[$m$]'
        <dd>computes the Moore-Penrose pseudoinverse of the matrix $m$. 
        If $m$ is invertible, the pseudoinverse equals the inverse.
    </dl>

    >> PseudoInverse[{{1, 2}, {2, 3}, {3, 4}}]
     = {{-11 / 6, -1 / 3, 7 / 6}, {4 / 3, 1 / 3, -2 / 3}}

    >> PseudoInverse[{{1, 2, 0}, {2, 3, 0}, {3, 4, 1}}]
     = {{-3, 2, 0}, {2, -1, 0}, {1, -2, 1}}
    """

    def apply(self, m, evaluation):
        'PseudoInverse[m_]'

        matrix = to_sympy_matrix(m)
        pinv = matrix.pinv()
        return from_sympy(pinv) 


class LeastSquares(Builtin):
    """
    <dl>
    <dt>'LeastSquares[$m$, $b$]'
        <dd>Compute the least squares solution to $m x = b$. 
        Finds an x that solves for b optimally.
    </dl>

    >> LeastSquares[{{1, 2}, {2, 3}, {5, 6}}, {1, 5, 3}]
     = {{-28 / 13}, {31 / 13}}

    >> Simplify[LeastSquares[{{1, 2}, {2, 3}, {5, 6}}, {1, x, 3}]]
     = {{12 / 13 - 8 x / 13}, {-4 / 13 + 7 x / 13}}

    """

    messages = {
        'underdetermined': "Solving for underdetermined system not implemented"
    }

    def apply(self, m, b, evaluation):
        'LeastSquares[m_, b_]'

        matrix = to_sympy_matrix(m)
        b_vector = to_sympy_matrix([el.to_sympy() for el in b.leaves])

        try:
            solution = matrix.solve_least_squares(b_vector) # default method = Cholesky
        except NotImplementedError as e:
            return evaluation.message('LeastSquares', 'underdetermined')

        return from_sympy(solution) 


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
        'lslc': ("Coefficient matrix and target vector(s) or matrix "
                 "do not have the same dimensions."),
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
        system = [mm + [v] for mm, v in zip(matrix, b.leaves)]
        system = to_sympy_matrix(system)
        if system is None:
            return
        syms = [sympy.Dummy('LinearSolve_var%d' % k)
                for k in range(system.cols - 1)]
        sol = sympy.solve_linear_system(system, *syms)
        if sol:
            # substitute 0 for variables that are not in result dictionary
            free_vars = dict((sym, sympy.Integer(
                0)) for sym in syms if sym not in sol)
            sol.update(free_vars)
            sol = [(sol[sym] if sym in free_vars else sol[sym].subs(free_vars))
                   for sym in syms]
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

    #> RowReduce[{{1, 0}, {0}}]
     = RowReduce[{{1, 0}, {0}}]
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

    >> Eigenvalues[{{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}] // Sort
     = {-1, 1, 2}

    >> Eigenvalues[{{Cos[theta],Sin[theta],0},{-Sin[theta],Cos[theta],0},{0,0,1}}] // Sort
     = {1, Cos[theta] + Sqrt[-1 + Cos[theta] ^ 2], Cos[theta] - Sqrt[-1 + Cos[theta] ^ 2]}

    >> Eigenvalues[{{7, 1}, {-4, 3}}]
     = {5, 5}
    """

    def apply(self, m, evaluation):
        'Eigenvalues[m_]'

        matrix = to_sympy_matrix(m)
        if matrix is None or matrix.cols != matrix.rows or matrix.cols == 0:
            return evaluation.message('Eigenvalues', 'matsq', m)
        eigenvalues = matrix.eigenvals()
        try:
            eigenvalues = sorted(eigenvalues.iteritems(),
                                 key=lambda (v, c): (abs(v), -v), reverse=True)
        except TypeError as e:
            if not str(e).startswith('cannot determine truth value of'):
                raise e
            eigenvalues = eigenvalues.items()
        return from_sympy([v for (v, c) in eigenvalues for _ in xrange(c)])


class Eigenvectors(Builtin):
    """
    <dl>
    <dt>'Eigenvectors[$m$]'
        <dd>computes the eigenvectors of the matrix $m$.
    </dl>

    >> Eigenvectors[{{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}]
     = {{1, 1, 1}, {1, -2, 1}, {-1, 0, 1}}
    >> Eigenvectors[{{1, 0, 0}, {0, 1, 0}, {0, 0, 0}}]
     = {{0, 1, 0}, {1, 0, 0}, {0, 0, 1}}
    >> Eigenvectors[{{2, 0, 0}, {0, -1, 0}, {0, 0, 0}}]
     = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
    >> Eigenvectors[{{0.1, 0.2}, {0.8, 0.5}}]
     = {{0.309016994374947, 1.}, {-0.809016994374947, 1.}}

    #> Eigenvectors[{{-2, 1, -1}, {-3, 2, 1}, {-1, 1, 0}}]
     = {{1 / 3, 7 / 3, 1}, {1, 1, 0}, {0, 0, 0}}
    """

    messages = {
        'eigenvecnotimplemented': (
            "Eigenvectors is not yet implemented for the matrix `1`."),
    }

    # TODO: Normalise the eigenvectors

    def apply(self, m, evaluation):
        'Eigenvectors[m_]'

        matrix = to_sympy_matrix(m)
        if matrix is None or matrix.cols != matrix.rows or matrix.cols == 0:
            return evaluation.message('Eigenvectors', 'matsq', m)
        # sympy raises an error for some matrices that Mathematica can compute.
        try:
            eigenvects = matrix.eigenvects()
        except NotImplementedError:
            return evaluation.message(
                'Eigenvectors', 'eigenvecnotimplemented', m)

        # The eigenvectors are given in the same order as the eigenvalues.
        eigenvects = sorted(eigenvects, key=lambda (
            val, c, vect): (abs(val), -val), reverse=True)
        result = []
        for val, count, basis in eigenvects:
            # Select the i'th basis vector, convert matrix to vector,
            # and convert from sympy
            vects = [from_sympy(list(b)) for b in basis]

            # This follows Mathematica convention better; higher indexed pivots
            # are outputted first. e.g. {{0,1},{1,0}} instead of {{1,0},{0,1}}
            vects.reverse()

            # Add the vectors to results
            result.extend(vects)
        result.extend([Expression('List', *(
            [0] * matrix.rows))] * (matrix.rows - len(result)))
        return Expression('List', *result)
