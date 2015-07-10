from mathics.builtin.base import Builtin


class Mean(Builtin):
    """
    <dl>
    <dt>'Mean[$list$]'
        <dd>Mean over all values in list (Total[list] / Length[list])
    </dl>

    >> Mean[{1, 2, 3}]
     = 2

    >> Mean[{a, b, 123}]
     = (123 + a + b) / 3

    >> Mean[{}]
     = Mean[{}]
    """

    rules = {
        'Mean[list_List /; Length[list] > 0]': 'Total[list] / Length[list]',
    }


class Variance(Builtin):
    """
    <dl>
    <dt>'Mean[$list$]'
        <dd>Mean over all values in list (Total[list] / Length[list])
    </dl>

    >> Variance[{1, 2, 3}]
     = 1

    >> Variance[{Pi, 2.0, I}]
     = 2.86213969796992406

    >> Variance[{}]
     = Variance[{}]
    """

    # FIXME precision

    rules = {
        'Variance[list_List /; Length[list] > 0]': (
            'Re[(list - Mean[list]).Conjugate[list - Mean[list]] / (Length[list] - 1)]')
    }


class StandardDeviation(Builtin):
    rules = {
        'StandardDeviation[list_List]': 'Sqrt[Variance[list]]'
    }


class Quantile(Builtin):

    rules = {
        'Quantile[list_List, q_RealValueQ]': 'Sort[list, Less][[Ceiling[q * Length[list]]]]',
        'Quantile[list_List, q_List]': 'Quantile[list, singleq] /@ q'
    }


class Diagonal(Builtin):
    rules = {
        'Diagonal[l_List, k]': 'Module[{k = 0, j = 0, i = 0}, If[n > 0, j = n, k = -n]; Table[l[[i + k, i + j]], {i, 1, Min[Dimensions[l] - {k, j}]}]]',
        'Diagonal[l_List]': 'Module[{i = 0}, Table[l[[i, i]], {i, 1, Min[Dimensions[l]]}]]'
    }


class Tr(Builtin):
    rules = {
        'Tr[m_List] /; Length[Dimensions[m]] == 1': 'Total[m]',
        'Tr[m_List, head_, level_]': 'Apply[head, Diagonal[Flatten[m, level]]]',
        'Tr[m_List, head_]': 'Apply[head, Diagonal[m]]',
        'Tr[m_List]': 'Tr[m, Plus]'

    }


class ListCorrelate(Builtin):       
    rules = {
        'ListCorrelate[ker_List, list_List]': 'Map[ker.# & , Partition[list, Length[ker], 1]]'
    }


class ListConvolve(Builtin):
    rules = {
        'ListConvolve[ker_List, list_List]': 'Map[Reverse[ker].# & , Partition[list, Length[ker], 1]]'
    }


class MovingAverage(Builtin):
    # TODO: Length constraints
    rules = {
        'MovingAverage[list_List, weights_List]': 'ListCorrelate[weights/ Total[weights], list]',
        'MovingAverage[list_List, r_Integer]': 'MovingAverage[list, ConstantArray[1, r]]'
    }


class MovingMedian(Builtin):
    rules = {
        'MovingMedian[list_List]': 'MovingAverage[list, 2]'
    }
