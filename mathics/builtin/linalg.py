# -*- coding: utf-8 -*-

"""
Linear algebra
"""

import sympy
from sympy import re, im
from mpmath import mp

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin
from mathics.core.convert import from_sympy
from mathics.core.expression import Expression, Integer, Symbol, Real, from_mpmath


def matrix_data(m):
    if not m.has_form("List", None):
        return None
    if all(leaf.has_form("List", None) for leaf in m.leaves):
        result = [[item.to_sympy() for item in row.leaves] for row in m.leaves]
        if not any(None in row for row in result):
            return result
    elif not any(leaf.has_form("List", None) for leaf in m.leaves):
        result = [item.to_sympy() for item in m.leaves]
        if None not in result:
            return result


def to_sympy_matrix(data, **kwargs):
    """Convert a Mathics matrix to one that can be used by Sympy.
    None is returned if we can't convert to a Sympy matrix.
    """
    if not isinstance(data, list):
        data = matrix_data(data)
    try:
        return sympy.Matrix(data)
    except (TypeError, AssertionError, ValueError):
        return None


def to_mpmath_matrix(data, **kwargs):
    """Convert a Mathics matrix to one that can be used by mpmath.
    None is returned if we can't convert to a mpmath matrix.
    """

    def mpmath_matrix_data(m):
        if not m.has_form("List", None):
            return None
        if not all(leaf.has_form("List", None) for leaf in m.leaves):
            return None
        return [[str(item) for item in row.leaves] for row in m.leaves]

    if not isinstance(data, list):
        data = mpmath_matrix_data(data)
    try:
        return mp.matrix(data)
    except (TypeError, AssertionError, ValueError):
        return None


class Tr(Builtin):
    """
    <dl>
    <dt>'Tr[$m$]'
        <dd>computes the trace of the matrix $m$.
    </dl>

    >> Tr[{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}]
     = 15

    Symbolic trace:
    >> Tr[{{a, b, c}, {d, e, f}, {g, h, i}}]
     = a + e + i
    """

    messages = {"matsq": "The matrix `1` is not square."}

    # TODO: generalize to vectors and higher-rank tensors, and allow function arguments for application

    def apply(self, m, evaluation):
        "Tr[m_]"

        matrix = to_sympy_matrix(m)
        if matrix is None or matrix.cols != matrix.rows or matrix.cols == 0:
            return evaluation.message("Tr", "matsq", m)
        tr = matrix.trace()
        return from_sympy(tr)


class Det(Builtin):
    """
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
        "Det[m_]"

        matrix = to_sympy_matrix(m)
        if matrix is None or matrix.cols != matrix.rows or matrix.cols == 0:
            return evaluation.message("Det", "matsq", m)
        det = matrix.det()
        return from_sympy(det)


class Cross(Builtin):
    """
    <dl>
    <dt>'Cross[$a$, $b$]'
        <dd>computes the vector cross product of $a$ and $b$.
    </dl>

    >> Cross[{x1, y1, z1}, {x2, y2, z2}]
     = {y1 z2 - y2 z1, -x1 z2 + x2 z1, x1 y2 - x2 y1}

    >> Cross[{x, y}]
     = {-y, x}

    >> Cross[{1, 2}, {3, 4, 5}]
     : The arguments are expected to be vectors of equal length, and the number of arguments is expected to be 1 less than their length.
     = Cross[{1, 2}, {3, 4, 5}]
    """

    rules = {"Cross[{x_, y_}]": "{-y, x}"}

    messages = {
        "nonn1": (
            "The arguments are expected to be vectors of equal length, "
            "and the number of arguments is expected to be 1 less than "
            "their length."
        )
    }

    def apply(self, a, b, evaluation):
        "Cross[a_, b_]"
        a = to_sympy_matrix(a)
        b = to_sympy_matrix(b)

        if a is None or b is None:
            return evaluation.message("Cross", "nonn1")

        try:
            res = a.cross(b)
        except sympy.ShapeError:
            return evaluation.message("Cross", "nonn1")
        return from_sympy(res)


class VectorAngle(Builtin):
    """
    <dl>
    <dt>'VectorAngle[$u$, $v$]'
        <dd>gives the angles between vectors $u$ and $v$
    </dl>

    >> VectorAngle[{1, 0}, {0, 1}]
     = Pi / 2

    >> VectorAngle[{1, 2}, {3, 1}]
     = Pi / 4

    >> VectorAngle[{1, 1, 0}, {1, 0, 1}]
     = Pi / 3

    #> VectorAngle[{0, 1}, {0, 1}]
     = 0
    """

    rules = {"VectorAngle[u_, v_]": "ArcCos[u.v / (Norm[u] Norm[v])]"}


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

    messages = {"sing": "The matrix `1` is singular."}

    def apply(self, m, evaluation):
        "Inverse[m_]"

        matrix = to_sympy_matrix(m)
        if matrix is None or matrix.cols != matrix.rows or matrix.cols == 0:
            return evaluation.message("Inverse", "matsq", m)
        if matrix.det() == 0:
            return evaluation.message("Inverse", "sing", m)
        inv = matrix.inv()
        return from_sympy(inv)


class SingularValueDecomposition(Builtin):
    """
    <dl>
    <dt>'SingularValueDecomposition[$m$]'
        <dd>calculates the singular value decomposition for the matrix $m$.
    </dl>

    'SingularValueDecomposition' returns $u$, $s$, $w$ such that $m$=$u$ $s$ $v$,
    $u$\'$u$=1, $v$\'$v$=1, and $s$ is diagonal.

    >> SingularValueDecomposition[{{1.5, 2.0}, {2.5, 3.0}}]
     = {{{0.538954, 0.842335}, {0.842335, -0.538954}}, {{4.63555, 0.}, {0., 0.107862}}, {{0.628678, 0.777666}, {-0.777666, 0.628678}}}


    #> SingularValueDecomposition[{{3/2, 2}, {5/2, 3}}]
     : Symbolic SVD is not implemented, performing numerically.
     = {{{0.538954, 0.842335}, {0.842335, -0.538954}}, {{4.63555, 0.}, {0., 0.107862}}, {{0.628678, 0.777666}, {-0.777666, 0.628678}}}

    #> SingularValueDecomposition[{1, {2}}]
     : Argument {1, {2}} at position 1 is not a non-empty rectangular matrix.
     = SingularValueDecomposition[{1, {2}}]
    """

    # Sympy lacks symbolic SVD
    """
    >> SingularValueDecomposition[{{1, 2}, {2, 3}, {3, 4}}]
     = {{-11 / 6, -1 / 3, 7 / 6}, {4 / 3, 1 / 3, -2 / 3}}

    >> SingularValueDecomposition[{{1, 2, 0}, {2, 3, 0}, {3, 4, 1}}]
     = {{-3, 2, 0}, {2, -1, 0}, {1, -2, 1}}
    """

    messages = {
        "nosymb": "Symbolic SVD is not implemented, performing numerically.",
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix.",
    }

    def apply(self, m, evaluation):
        "SingularValueDecomposition[m_]"

        matrix = to_mpmath_matrix(m)
        if matrix is None:
            return evaluation.message("SingularValueDecomposition", "matrix", m, 1)

        if not any(leaf.is_inexact() for row in m.leaves for leaf in row.leaves):
            # symbolic argument (not implemented)
            evaluation.message("SingularValueDecomposition", "nosymb")

        U, S, V = mp.svd(matrix)
        S = mp.diag(S)
        U_list = Expression("List", *U.tolist())
        S_list = Expression("List", *S.tolist())
        V_list = Expression("List", *V.tolist())
        return Expression("List", *[U_list, S_list, V_list])


class QRDecomposition(Builtin):
    """
    <dl>
    <dt>'QRDecomposition[$m$]'
        <dd>computes the QR decomposition of the matrix $m$.
    </dl>

    >> QRDecomposition[{{1, 2}, {3, 4}, {5, 6}}]
     = {{{Sqrt[35] / 35, 3 Sqrt[35] / 35, Sqrt[35] / 7}, {13 Sqrt[210] / 210, 2 Sqrt[210] / 105, -Sqrt[210] / 42}}, {{Sqrt[35], 44 Sqrt[35] / 35}, {0, 2 Sqrt[210] / 35}}}

    #> QRDecomposition[{1, {2}}]
     : Argument {1, {2}} at position 1 is not a non-empty rectangular matrix.
     = QRDecomposition[{1, {2}}]
    """

    messages = {
        "sympy": "Sympy is unable to perform the QR decomposition.",
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix.",
    }

    def apply(self, m, evaluation):
        "QRDecomposition[m_]"

        matrix = to_sympy_matrix(m)
        if matrix is None:
            return evaluation.message("QRDecomposition", "matrix", m, 1)
        try:
            Q, R = matrix.QRdecomposition()
        except sympy.matrices.MatrixError:
            return evaluation.message("QRDecomposition", "sympy")
        Q = Q.transpose()
        return Expression("List", *[from_sympy(Q), from_sympy(R)])


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

    >> PseudoInverse[{{1.0, 2.5}, {2.5, 1.0}}]
     = {{-0.190476, 0.47619}, {0.47619, -0.190476}}

    #> PseudoInverse[{1, {2}}]
    : Argument {1, {2}} at position 1 is not a non-empty rectangular matrix.
    = PseudoInverse[{1, {2}}]
    """

    messages = {
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix."
    }

    def apply(self, m, evaluation):
        "PseudoInverse[m_]"

        matrix = to_sympy_matrix(m)
        if matrix is None:
            return evaluation.message("PseudoInverse", "matrix", m, 1)
        pinv = matrix.pinv()
        return from_sympy(pinv)


class LeastSquares(Builtin):
    """
    <dl>
    <dt>'LeastSquares[$m$, $b$]'
        <dd>computes the least squares solution to $m$ $x$ = $b$, finding
        an $x$ that solves for $b$ optimally.
    </dl>

    >> LeastSquares[{{1, 2}, {2, 3}, {5, 6}}, {1, 5, 3}]
     = {-28 / 13, 31 / 13}

    >> Simplify[LeastSquares[{{1, 2}, {2, 3}, {5, 6}}, {1, x, 3}]]
     = {12 / 13 - 8 x / 13, -4 / 13 + 7 x / 13}

    >> LeastSquares[{{1, 1, 1}, {1, 1, 2}}, {1, 3}]
     : Solving for underdetermined system not implemented.
     = LeastSquares[{{1, 1, 1}, {1, 1, 2}}, {1, 3}]

    ## Inconsistent system - ideally we'd print a different message
    #> LeastSquares[{{1, 1, 1}, {1, 1, 1}}, {1, 0}]
     : Solving for underdetermined system not implemented.
     = LeastSquares[{{1, 1, 1}, {1, 1, 1}}, {1, 0}]

    #> LeastSquares[{1, {2}}, {1, 2}]
     : Argument {1, {2}} at position 1 is not a non-empty rectangular matrix.
     = LeastSquares[{1, {2}}, {1, 2}]
    #> LeastSquares[{{1, 2}, {3, 4}}, {1, {2}}]
     : Argument {1, {2}} at position 2 is not a non-empty rectangular matrix.
     = LeastSquares[{{1, 2}, {3, 4}}, {1, {2}}]
    """

    messages = {
        "underdetermined": "Solving for underdetermined system not implemented.",
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix.",
    }

    def apply(self, m, b, evaluation):
        "LeastSquares[m_, b_]"

        matrix = to_sympy_matrix(m)
        if matrix is None:
            return evaluation.message("LeastSquares", "matrix", m, 1)

        b_vector = to_sympy_matrix(b)
        if b_vector is None:
            return evaluation.message("LeastSquares", "matrix", b, 2)

        try:
            solution = matrix.solve_least_squares(b_vector)  # default method = Cholesky
        except NotImplementedError:
            return evaluation.message("LeastSquares", "underdetermined")

        return from_sympy(solution)


class LinearSolve(Builtin):
    """
    <dl>
    <dt>'LinearSolve[$matrix$, $right$]'
        <dd>solves the linear equation system '$matrix$ . $x$ = $right$'
        and returns one corresponding solution $x$.
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

    #> LinearSolve[{1, {2}}, {1, 2}]
     : Argument {1, {2}} at position 1 is not a non-empty rectangular matrix.
     = LinearSolve[{1, {2}}, {1, 2}]
    #> LinearSolve[{{1, 2}, {3, 4}}, {1, {2}}]
     : Argument {1, {2}} at position 2 is not a non-empty rectangular matrix.
     = LinearSolve[{{1, 2}, {3, 4}}, {1, {2}}]
    """

    messages = {
        "lslc": (
            "Coefficient matrix and target vector(s) or matrix "
            "do not have the same dimensions."
        ),
        "nosol": "Linear equation encountered that has no solution.",
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix.",
    }

    def apply(self, m, b, evaluation):
        "LinearSolve[m_, b_]"

        matrix = matrix_data(m)
        if matrix is None:
            return evaluation.message("LinearSolve", "matrix", m, 1)
        if not b.has_form("List", None):
            return
        if len(b.leaves) != len(matrix):
            return evaluation.message("LinearSolve", "lslc")

        for leaf in b.leaves:
            if leaf.has_form("List", None):
                return evaluation.message("LinearSolve", "matrix", b, 2)

        system = [mm + [v.to_sympy()] for mm, v in zip(matrix, b.leaves)]
        system = to_sympy_matrix(system)
        if system is None:
            return evaluation.message("LinearSolve", "matrix", b, 2)
        syms = [sympy.Dummy("LinearSolve_var%d" % k) for k in range(system.cols - 1)]
        sol = sympy.solve_linear_system(system, *syms)
        if sol:
            # substitute 0 for variables that are not in result dictionary
            free_vars = dict((sym, sympy.Integer(0)) for sym in syms if sym not in sol)
            sol.update(free_vars)
            sol = [
                (sol[sym] if sym in free_vars else sol[sym].subs(free_vars))
                for sym in syms
            ]
            return from_sympy(sol)
        else:
            return evaluation.message("LinearSolve", "nosol")


class FittedModel(Builtin):
    rules = {
        "FittedModel[x_List][s_String]": "s /. x",
        "FittedModel[x_List][y_]": '("Function" /. x)[y]',
        "MakeBoxes[FittedModel[x_List], f_]": """
            RowBox[{"FittedModel[",
                Replace[Temporary["BestFit" /. x, f], Temporary -> MakeBoxes, 1, Heads -> True],
                "]"}]
            """,
    }


class DesignMatrix(Builtin):
    """
    <dl>
    <dt>'DesignMatrix[$m$, $f$, $x$]'
        <dd>returns the design matrix.
    </dl>

    >> DesignMatrix[{{2, 1}, {3, 4}, {5, 3}, {7, 6}}, x, x]
     = {{1, 2}, {1, 3}, {1, 5}, {1, 7}}

    >> DesignMatrix[{{2, 1}, {3, 4}, {5, 3}, {7, 6}}, f[x], x]
     = {{1, f[2]}, {1, f[3]}, {1, f[5]}, {1, f[7]}}
    """

    rules = {
        "DesignMatrix[m_, f_List, x_?AtomQ]": "DesignMatrix[m, {f}, ConstantArray[x, Length[f]]]",
        "DesignMatrix[m_, f_, x_?AtomQ]": "DesignMatrix[m, {f}, {x}]",
        "DesignMatrix[m_, f_List, x_List]": "Prepend[MapThread[Function[{ff, xx, rr}, ff /. xx -> rr], {f, x, Most[#]}], 1]& /@ m",
    }


class LinearModelFit(Builtin):
    """
    <dl>
    <dt>'LinearModelFit[$m$, $f$, $x$]'
        <dd>returns the design matrix.
    </dl>

    >> m = LinearModelFit[{{2, 1}, {3, 4}, {5, 3}, {7, 6}}, x, x];
    >> m["BasisFunctions"]
     = {1, x}

    >> m["BestFit"]
     = 0.186441 + 0.779661 x

    >> m["BestFitParameters"]
     = {0.186441, 0.779661}

    >> m["DesignMatrix"]
     = {{1, 2}, {1, 3}, {1, 5}, {1, 7}}

    >> m["Function"]
     = 0.186441 + 0.779661 #1&

    >> m["Response"]
     = {1, 4, 3, 6}

    >> m["FitResiduals"]
     = {-0.745763, 1.47458, -1.08475, 0.355932}

    >> m = LinearModelFit[{{2, 2, 1}, {3, 2, 4}, {5, 6, 3}, {7, 9, 6}}, {Sin[x], Cos[y]}, {x, y}];
    >> m["BasisFunctions"]
     = {1, Sin[x], Cos[y]}

    >> m["Function"]
     = 3.33077 - 5.65221 Cos[#2] - 5.01042 Sin[#1]&

    >> m = LinearModelFit[{{{1, 4}, {1, 5}, {1, 7}}, {1, 2, 3}}];
    >> m["BasisFunctions"]
     = {#1, #2}

    >> m["FitResiduals"]
     = {-0.142857, 0.214286, -0.0714286}
    """

    # see the paper "Regression by linear combination of basis functions" by Risi Kondor for a good
    # summary of the math behind this

    rules = {
        "LinearModelFit[data_, f_, x_?AtomQ]": "LinearModelFit[data, {f}, {x}]",
        "LinearModelFit[data_, f_List, x_List] /; Length[f] == Length[x]": """
            LinearModelFit[{DesignMatrix[data, f, x], Part[data, ;;, -1]},
                Prepend[MapThread[#1 /. #2 -> #3&, {f, x, Table[Slot[i], {i, Length[f]}]}], 1],
                "BasisFunctions" -> Prepend[f, 1], "NumberOfSlots" -> Length[f]]
            """,
        "LinearModelFit[{m_?MatrixQ, v_}, f_, options___]": """
            Module[{m1 = N[m], v1 = N[v], bf = "BasisFunctions" /. Join[{options}, {"BasisFunctions" -> f}]},
                Module[{t1 = Transpose[m1], n = "NumberOfSlots" /. Join[{options}, {"NumberOfSlots" -> Length[f]}]},
                    Module[{parameters = Dot[Dot[Inverse[Dot[t1, m1]], t1], v1]},
                        Module[{function = Replace[Temporary[Total[f * parameters]],
                            Temporary -> Function, 1, Heads -> True], (* work around Function's Hold *)},
                            FittedModel[{
                                "BasisFunctions" -> bf,
                                "BestFit" -> Total[bf * parameters],
                                "BestFitParameters" -> parameters,
                                "DesignMatrix" -> m,
                                "Function" -> function,
                                "Response" -> v,
                                "FitResiduals" -> MapThread[#2 - (function @@ Take[#1, -n])&, {m1, v1}]
                            }]
                        ]
                    ]
                ]
            ]
            """,  # f is a Slot[] version of BasisFunctions
        "LinearModelFit[{m_?MatrixQ, v_}]": "LinearModelFit[{m, v}, Table[Slot[i], {i, Length[First[m]]}]]",
    }


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

    #> NullSpace[{1, {2}}]
     : Argument {1, {2}} at position 1 is not a non-empty rectangular matrix.
     = NullSpace[{1, {2}}]
    """

    messages = {
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix."
    }

    def apply(self, m, evaluation):
        "NullSpace[m_]"

        matrix = to_sympy_matrix(m)
        if matrix is None:
            return evaluation.message("NullSpace", "matrix", m, 1)

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
     : Argument {{1, 0}, {0}} at position 1 is not a non-empty rectangular matrix.
     = RowReduce[{{1, 0}, {0}}]
    """

    messages = {
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix."
    }

    def apply(self, m, evaluation):
        "RowReduce[m_]"

        matrix = to_sympy_matrix(m)
        if matrix is None:
            return evaluation.message("RowReduce", "matrix", m, 1)
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

    #> MatrixRank[{{1, 0}, {0}}]
     : Argument {{1, 0}, {0}} at position 1 is not a non-empty rectangular matrix.
     = MatrixRank[{{1, 0}, {0}}]
    """

    messages = {
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix."
    }

    def apply(self, m, evaluation):
        "MatrixRank[m_]"

        matrix = to_sympy_matrix(m)
        if matrix is None:
            return evaluation.message("MatrixRank", "matrix", m, 1)
        rank = len(matrix.rref()[1])
        return Integer(rank)


class Eigenvalues(Builtin):
    """
    <dl>
      <dt>'Eigenvalues[$m$]'
      <dd>computes the eigenvalues of the matrix $m$.
      By default Sympy's routine is used. Sometimes this is slow and
      less good than the corresponding mpmath routine. Use option Method->"mpmath" if you want
      to use mpmath's routine instead.
    </dl>

    Numeric eigenvalues are sorted in order of decreasing absolute value:
    >> Eigenvalues[{{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}]
     = {2, -1, 1}

    Symbolic eigenvalues:
    >> Eigenvalues[{{Cos[theta],Sin[theta],0},{-Sin[theta],Cos[theta],0},{0,0,1}}] // Sort
     = {1, Cos[theta] + Sqrt[(-1 + Cos[theta]) (1 + Cos[theta])], Cos[theta] - Sqrt[(-1 + Cos[theta]) (1 + Cos[theta])]}

    >> Eigenvalues[{{7, 1}, {-4, 3}}]
     = {5, 5}

    >> Eigenvalues[{{7, 1}, {-4, 3}}]
     = {5, 5}

    #> Eigenvalues[{{1, 0}, {0}}]
     : Argument {{1, 0}, {0}} at position 1 is not a non-empty rectangular matrix.
     = Eigenvalues[{{1, 0}, {0}}]
    """

    sympy_name = "eigenvalues"
    mpmath_name = "eig"

    messages = {
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix."
    }

    @staticmethod
    def mp_eig(mp_matrix) -> Expression:
        try:
            _, ER = mp.eig(mp_matrix)
        except:
            return None

        eigenvalues = ER.tolist()
        # Sort the eigenvalues in the Mathematica convention: largest first.
        eigenvalues.sort(
            key=lambda v: (abs(v[0]), -v[0].real, -(v[0].imag)), reverse=True
        )
        eigenvalues = [[from_mpmath(c) for c in row] for row in eigenvalues]
        return Expression("List", *eigenvalues)

    options = {"Method": "sympy"}

    def apply(self, m, evaluation, options={}) -> Expression:
        "Eigenvalues[m_, OptionsPattern[Eigenvalues]]"

        method = self.get_option(options, "Method", evaluation)
        if method and method.get_string_value() == "mpmath":
            mp_matrix = to_mpmath_matrix(m)
            if mp_matrix is not None:
                return self.mp_eig(mp_matrix)

        sympy_matrix = to_sympy_matrix(m)
        if sympy_matrix is None:
            return evaluation.message("Eigenvalues", "matrix", m, 1)

        if sympy_matrix.cols != sympy_matrix.rows or sympy_matrix.cols == 0:
            return evaluation.message("Eigenvalues", "matsq", m)

        eigenvalues = list(sympy_matrix.eigenvals().items())
        if all(v.is_complex for (v, _) in eigenvalues):
            # Try to sort the eigenvalues in the Mathematica convention: largest first.
            try:
                eigenvalues.sort(
                    key=lambda v: (abs(v[0]), -re(v[0]), -im(v[0])), reverse=True
                )

                eigenvalues = [
                    from_sympy(v) for (v, c) in eigenvalues for _ in range(c)
                ]

                return Expression("List", *eigenvalues)
            except TypeError:
                pass

        eigenvalues = [(from_sympy(v), c) for (v, c) in eigenvalues]

        # Sort the eigenvalues by their sort key
        eigenvalues.sort(key=lambda v: v[0].get_sort_key())

        eigenvalues = [v for (v, c) in eigenvalues for _ in range(c)]

        return Expression("List", *eigenvalues)


class Eigensystem(Builtin):
    """
    <dl>
    <dt>'Eigensystem[$m$]'
        <dd>returns the list '{Eigenvalues[$m$], Eigenvectors[$m$]}'.
    </dl>

    >> Eigensystem[{{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}]
     = {{2, -1, 1}, {{1, 1, 1}, {1, -2, 1}, {-1, 0, 1}}}
    """

    rules = {"Eigensystem[m_]": "{Eigenvalues[m], Eigenvectors[m]}"}


class MatrixPower(Builtin):
    """
    <dl>
    <dt>'MatrixPower[$m$, $n$]'
        <dd>computes the $n$th power of a matrix $m$.
    </dl>

    >> MatrixPower[{{1, 2}, {1, 1}}, 10]
     = {{3363, 4756}, {2378, 3363}}

    >> MatrixPower[{{1, 2}, {2, 5}}, -3]
     = {{169, -70}, {-70, 29}}

    #> MatrixPower[{{0, x}, {0, 0}}, n]
     = MatrixPower[{{0, x}, {0, 0}}, n]

    #> MatrixPower[{{1, 0}, {0}}, 2]
     : Argument {{1, 0}, {0}} at position 1 is not a non-empty rectangular matrix.
     = MatrixPower[{{1, 0}, {0}}, 2]
    """

    messages = {
        "matrixpowernotimplemented": "Matrix power not implemented for matrix `1`.",
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix.",
        "matrixpowernotinvertible": "Matrix det == 0; not invertible",
    }

    def apply(self, m, power, evaluation):
        "MatrixPower[m_, power_]"
        sympy_m = to_sympy_matrix(m)
        if sympy_m is None:
            return evaluation.message("MatrixPower", "matrix", m, 1)

        sympy_power = power.to_sympy()
        if sympy_power is None:
            return

        try:
            res = sympy_m ** sympy_power
        except NotImplementedError:
            return evaluation.message("MatrixPower", "matrixpowernotimplemented", m)
        except ValueError:
            return evaluation.message("MatrixPower", "matrixpowernotinvertible", m)
        return from_sympy(res)


class MatrixExp(Builtin):
    """
    <dl>
    <dt>'MatrixExp[$m$]'
        <dd>computes the exponential of the matrix $m$.
    </dl>

    >> MatrixExp[{{0, 2}, {0, 1}}]
     = {{1, -2 + 2 E}, {0, E}}

    >> MatrixExp[{{1.5, 0.5}, {0.5, 2.0}}]
     = {{5.16266, 3.02952}, {3.02952, 8.19218}}

    #> MatrixExp[{{a, 0}, {0, b}}]
     = {{E ^ a, 0}, {0, E ^ b}}

    #> MatrixExp[{{1, 0}, {0}}]
     : Argument {{1, 0}, {0}} at position 1 is not a non-empty rectangular matrix.
     = MatrixExp[{{1, 0}, {0}}]
    """

    messages = {
        "matrixexpnotimplemented": ("Matrix power not implemented for matrix `1`."),
        "matrix": "Argument `1` at position `2` is not a non-empty rectangular matrix.",
    }

    # TODO fix precision

    def apply(self, m, evaluation):
        "MatrixExp[m_]"
        sympy_m = to_sympy_matrix(m)
        if sympy_m is None:
            return evaluation.message("MatrixExp", "matrix", m, 1)

        try:
            res = sympy_m.exp()
        except NotImplementedError:
            return evaluation.message("MatrixExp", "matrixexpnotimplemented", m)
        return from_sympy(res)


class Norm(Builtin):
    """
    <dl>
    <dt>'Norm[$m$, $l$]'</dt>
        <dd>computes the l-norm of matrix m (currently only works for vectors!).</dd>
    <dt>'Norm[$m$]'</dt>
        <dd>computes the 2-norm of matrix m (currently only works for vectors!).</dd>
    </dl>

    >> Norm[{1, 2, 3, 4}, 2]
     = Sqrt[30]

    >> Norm[{10, 100, 200}, 1]
     = 310

    >> Norm[{a, b, c}]
     = Sqrt[Abs[a] ^ 2 + Abs[b] ^ 2 + Abs[c] ^ 2]

    >> Norm[{-100, 2, 3, 4}, Infinity]
     = 100

    >> Norm[1 + I]
     = Sqrt[2]

    #> Norm[{1, {2, 3}}]
     : The first Norm argument should be a number, vector, or matrix.
     = Norm[{1, {2, 3}}]

    #> Norm[{x, y}]
     = Sqrt[Abs[x] ^ 2 + Abs[y] ^ 2]

    #> Norm[{x, y}, p]
     = (Abs[x] ^ p + Abs[y] ^ p) ^ (1 / p)

    #> Norm[{x, y}, 0]
     : The second argument of Norm, 0, should be a symbol, Infinity, or an integer or real number not less than 1 for vector p-norms; or 1, 2, Infinity, or "Frobenius" for matrix norms.
     = Norm[{x, y}, 0]

    #> Norm[{x, y}, 0.5]
     : The second argument of Norm, 0.5, should be a symbol, Infinity, or an integer or real number not less than 1 for vector p-norms; or 1, 2, Infinity, or "Frobenius" for matrix norms.
     = Norm[{x, y}, 0.5]

    #> Norm[{}]
     = Norm[{}]

    #> Norm[0]
     = 0
    """

    rules = {
        "Norm[m_?NumberQ]": "Abs[m]",
        "Norm[m_?VectorQ, DirectedInfinity[1]]": "Max[Abs[m]]",
    }

    messages = {
        "nvm": "The first Norm argument should be a number, vector, or matrix.",
        "ptype": (
            "The second argument of Norm, `1`, should be a symbol, Infinity, "
            "or an integer or real number not less than 1 for vector p-norms; "
            'or 1, 2, Infinity, or "Frobenius" for matrix norms.'
        ),
        "normnotimplemented": "Norm is not yet implemented for matrices.",
    }

    def apply_single(self, m, evaluation):
        "Norm[m_]"
        return self.apply(m, Integer(2), evaluation)

    def apply(self, m, l, evaluation):
        "Norm[m_, l_]"

        if isinstance(l, Symbol):
            pass
        elif isinstance(l, (Real, Integer)) and l.to_python() >= 1:
            pass
        else:
            return evaluation.message("Norm", "ptype", l)

        l = l.to_sympy()
        if l is None:
            return
        matrix = to_sympy_matrix(m)

        if matrix is None:
            return evaluation.message("Norm", "nvm")
        if len(matrix) == 0:
            return

        try:
            res = matrix.norm(l)
        except NotImplementedError:
            return evaluation.message("Norm", "normnotimplemented")

        return from_sympy(res)


class Normalize(Builtin):
    """
    <dl>
    <dt>'Normalize[$v$]'
        <dd>calculates the normalized vector $v$.
    <dt>'Normalize[$z$]'
        <dd>calculates the normalized complex number $z$.
    </dl>

    >> Normalize[{1, 1, 1, 1}]
     = {1 / 2, 1 / 2, 1 / 2, 1 / 2}

    >> Normalize[1 + I]
     = (1 / 2 + I / 2) Sqrt[2]

    #> Normalize[0]
     = 0

    #> Normalize[{0}]
     = {0}

    #> Normalize[{}]
     = {}
    """

    rules = {"Normalize[v_]": "Module[{norm = Norm[v]}, If[norm == 0, v, v / norm, v]]"}


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


    ## There are problems, in $MachinePrecision = UnsignedInteger32 vs UnsignedIntegeret128
    ## in testing. And this is better done in a unit test.
    >> Eigenvectors[{{0.1, 0.2}, {0.8, 0.5}}]
     = ...
    ### = {{-0.355518, -1.15048}, {-0.62896, 0.777438}}

    #> Eigenvectors[{{-2, 1, -1}, {-3, 2, 1}, {-1, 1, 0}}]
     = {{1, 7, 3}, {1, 1, 0}, {0, 0, 0}}
    """

    messages = {
        "eigenvecnotimplemented": (
            "Eigenvectors is not yet implemented for the matrix `1`."
        )
    }

    # TODO: Normalise the eigenvectors

    def apply(self, m, evaluation):
        "Eigenvectors[m_]"

        matrix = to_sympy_matrix(m)
        if matrix is None or matrix.cols != matrix.rows or matrix.cols == 0:
            return evaluation.message("Eigenvectors", "matsq", m)
        # sympy raises an error for some matrices that Mathematica can compute.
        try:
            eigenvects = matrix.eigenvects(simplify=True)
        except NotImplementedError:
            return evaluation.message("Eigenvectors", "eigenvecnotimplemented", m)

        # Try to sort the eigenvectors by their corresponding eigenvalues
        if all(v.is_complex for (v, _, _) in eigenvects):
            try:
                eigenvects.sort(
                    key=lambda v: (abs(v[0]), -re(v[0]), -im(v[0])), reverse=True
                )
            except TypeError:
                eigenvects.sort(key=lambda v: from_sympy(v[0]).get_sort_key())
        else:
            eigenvects.sort(key=lambda v: from_sympy(v[0]).get_sort_key())

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
        result.extend(
            [Expression("List", *([0] * matrix.rows))] * (matrix.rows - len(result))
        )
        return Expression("List", *result)


def _norm_calc(head, u, v, evaluation):
    expr = Expression(head, u, v)
    old_quiet_all = evaluation.quiet_all
    try:
        evaluation.quiet_all = True
        expr_eval = expr.evaluate(evaluation)
    finally:
        evaluation.quiet_all = old_quiet_all
    if expr_eval.same(expr):
        evaluation.message("Norm", "nvm")
        return None
    else:
        return expr_eval


class EuclideanDistance(Builtin):
    """
    <dl>
    <dt>'EuclideanDistance[$u$, $v$]'
        <dd>returns the euclidean distance between $u$ and $v$.
    </dl>

    >> EuclideanDistance[-7, 5]
     = 12

    >> EuclideanDistance[{-1, -1}, {1, 1}]
     = 2 Sqrt[2]

    >> EuclideanDistance[{a, b}, {c, d}]
     = Sqrt[Abs[a - c] ^ 2 + Abs[b - d] ^ 2]
    """

    def apply(self, u, v, evaluation):
        "EuclideanDistance[u_, v_]"
        t = _norm_calc("Subtract", u, v, evaluation)
        if t is not None:
            return Expression("Norm", t)


class SquaredEuclideanDistance(Builtin):
    """
    <dl>
    <dt>'SquaredEuclideanDistance[$u$, $v$]'
        <dd>returns squared the euclidean distance between $u$ and $v$.
    </dl>

    >> SquaredEuclideanDistance[-7, 5]
     = 144

    >> SquaredEuclideanDistance[{-1, -1}, {1, 1}]
     = 8
    """

    def apply(self, u, v, evaluation):
        "SquaredEuclideanDistance[u_, v_]"
        t = _norm_calc("Subtract", u, v, evaluation)
        if t is not None:
            return Expression("Power", Expression("Norm", t), 2)


class ManhattanDistance(Builtin):
    """
    <dl>
    <dt>'ManhattanDistance[$u$, $v$]'
        <dd>returns the Manhattan distance between $u$ and $v$, which is the number of horizontal or vertical
        moves in the gridlike Manhattan city layout to get from $u$ to $v$.
    </dl>

    >> ManhattanDistance[-7, 5]
     = 12

    >> ManhattanDistance[{-1, -1}, {1, 1}]
     = 4
    """

    def apply(self, u, v, evaluation):
        "ManhattanDistance[u_, v_]"
        t = _norm_calc("Subtract", u, v, evaluation)
        if t is not None:
            return Expression("Total", Expression("Abs", t))


class ChessboardDistance(Builtin):
    """
    <dl>
    <dt>'ChessboardDistance[$u$, $v$]'
        <dd>returns the chessboard distance (also known as Chebyshev distance) between $u$ and $v$, which is
        the number of moves a king on a chessboard needs to get from square $u$ to square $v$.
    </dl>

    >> ChessboardDistance[-7, 5]
     = 12

    >> ChessboardDistance[{-1, -1}, {1, 1}]
     = 2
    """

    def apply(self, u, v, evaluation):
        "ChessboardDistance[u_, v_]"
        t = _norm_calc("Subtract", u, v, evaluation)
        if t is not None:
            return Expression("Max", Expression("Abs", t))


class CanberraDistance(Builtin):
    """
    <dl>
    <dt>'CanberraDistance[$u$, $v$]'
        <dd>returns the canberra distance between $u$ and $v$, which is a weighted version of the Manhattan
        distance.
    </dl>

    >> CanberraDistance[-7, 5]
     = 1

    >> CanberraDistance[{-1, -1}, {1, 1}]
     = 2
    """

    def apply(self, u, v, evaluation):
        "CanberraDistance[u_, v_]"
        t = _norm_calc("Subtract", u, v, evaluation)
        if t is not None:
            return Expression(
                "Total",
                Expression(
                    "Divide",
                    Expression("Abs", t),
                    Expression("Plus", Expression("Abs", u), Expression("Abs", v)),
                ),
            )


class BrayCurtisDistance(Builtin):
    """
    <dl>
    <dt>'BrayCurtisDistance[$u$, $v$]'
        <dd>returns the Bray Curtis distance between $u$ and $v$.
    </dl>

    >> BrayCurtisDistance[-7, 5]
     = 6

    >> BrayCurtisDistance[{-1, -1}, {10, 10}]
     = 11 / 9
    """

    def apply(self, u, v, evaluation):
        "BrayCurtisDistance[u_, v_]"
        t = _norm_calc("Subtract", u, v, evaluation)
        if t is not None:
            return Expression(
                "Divide",
                Expression("Total", Expression("Abs", t)),
                Expression("Total", Expression("Abs", Expression("Plus", u, v))),
            )


class CosineDistance(Builtin):
    """
    <dl>
    <dt>'CosineDistance[$u$, $v$]'
      <dd>returns the cosine distance between $u$ and $v$.
    </dl>

    >> N[CosineDistance[{7, 9}, {71, 89}]]
     = 0.0000759646

    >> CosineDistance[{a, b}, {c, d}]
     = 1 + (-a c - b d) / (Sqrt[Abs[a] ^ 2 + Abs[b] ^ 2] Sqrt[Abs[c] ^ 2 + Abs[d] ^ 2])
    """

    def apply(self, u, v, evaluation):
        "CosineDistance[u_, v_]"
        dot = _norm_calc("Dot", u, v, evaluation)
        if dot is not None:
            return Expression(
                "Subtract",
                1,
                Expression(
                    "Divide",
                    dot,
                    Expression("Times", Expression("Norm", u), Expression("Norm", v)),
                ),
            )
