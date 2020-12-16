# -*- coding: utf-8 -*-
from .helper import session, check_evaluation

import sys
from mathics.core.parser import parse, SingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
import pytest

session.evaluate(
    """
Needs["DiscreteMath`CombinatoricaV0.9`"]
     """
)

# A number of examples from Implementing Discrete Mathematics by
# Steven Skiena and
# A number of examples from Computation Discrete Mathematics by
# Sriram Pemmaraju and Steven Skiena.

# Page numbers below come from the first book


def test_combinatorica_permutations_1_1():

    for str_expr, str_expected, message in (
        (
            "Permute[{a, b, c, d}, Range[4]]",
            "{a, b, c, d}",
            "Permute list with simple list; 1.1 Page 3",
        ),
        (
            "Permute[{a, b, c, d}, {1,2,2,4}]",
            "Permute[{a, b, c, d}, {1,2,2,4}]",
            "Incorrect permute: index 2 duplicated; 1.1 Page 3",
        ),
        (
            "LexicographicPermutations[{a,b,c,d}]",
            "{{a, b, c, d}, {a, b, d, c}, {a, c, b, d}, "
            "{a, c, d, b}, {a, d, b, c}, {a, d, c, b}, "
            "{b, a, c, d}, {b, a, d, c}, {b, c, a, d}, "
            "{b, c, d, a}, {b, d, a, c}, {b, d, c, a}, "
            "{c, a, b, d}, {c, a, d, b}, {c, b, a, d}, "
            "{c, b, d, a}, {c, d, a, b}, {c, d, b, a}, "
            "{d, a, b, c}, {d, a, c, b}, {d, b, a, c}, "
            "{d, b, c, a}, {d, c, a, b}, {d, c, b, a}}",
            "LexicographicPermuations, 1.1.1 Page 4",
        ),
        # NthPermutation does not work
        (
            "Map[RankPermutation, Permutations[Range[4]]]",
            "Range[0, 23]",
            "Permutations uses lexographic order; 1.1.2, Page 6",
        ),
        (
            "RandomPermutation1[20] === RandomPermutation2[20]",
            "False",
            "Not likey two of the 20! permutations will be the same, 1.1.3, Page 7",
        ),
        (
            "RandomPermutation1[20] === RandomPermutation1[20]",
            "False",
            "Not likley two of 20! permutations will be the same (same routine)",
        ),
        (
            "MinimumChangePermutations[{a,b,c}]",
            "{{a, b, c}, {b, a, c}, {c, a, b}, {a, c, b}, {b, c, a}, {c, b, a}}",
            "MinimumChangePermuations; 1.1.4, Page 11",
        ),
        (
            "Union[Permutations[{a,a,a,a,a}]]",
            "{{a, a, a, a, a}}",
            "simple but wasteful Permutation duplication elimination, 1.1.5, Page 12",
        ),
        (
            "DistinctPermutations[{1,1,2,2}]",
            "{{1, 1, 2, 2}, {1, 2, 1, 2}, {1, 2, 2, 1}, "
            "{2, 1, 1, 2}, {2, 1, 2, 1}, {2, 2, 1, 1}}",
            "DisctinctPermutations of multiset Binomial[6,3] permutations, 1.1.5, Page 14",
        ),
        ("Multinomial[3,3]", "20", "The built-in function Multinomial, Page 14"),
        (
            "DistinctPermutations[{A,B,C}]",
            "{{A, B, C}, {A, C, B}, {B, A, C}, {B, C, A}, {C, A, B}, {C, B, A}}",
            "DisctinctPermutations all n! permutations, Page 14",
        ),
        (
            "BinarySearch[Table[2i,{i, 30}],40]",
            "20",
            "BinarySearch: 40 is one of the first 30 even numbers; 1.1.6, Page 16",
        ),
        (
            "BinarySearch[Table[2i,{i, 30}],41]",
            "41/2",
            "BinarySearch: BinarySearch: 41 is not even; 1.1.6, Page 16",
        ),
        (
            "Sort[ Subsets [Range[4]],(Apply[Plus, #1]<=Apply[Plus,#2])& ]",
            "{{}, {1}, {2}, {3}, {1, 2}, {4}, {1, 3}, {1, 4}, {2, 3}, {2, 4}, "
            "{1, 2, 3}, {3, 4}, {1, 2, 4}, {1, 3, 4}, {2, 3, 4}, {1, 2, 3, 4}}",
            "Sort to total order subsets, Page 15",
        ),
    ):
        check_evaluation(str_expr, str_expected, message)


def test_combinatorica_permutations_1_2():

    for str_expr, str_expected, message in (
        (
            "MultiplicationTable[Permutations[Range[3]], Permute ]",
            "{{1, 2, 3, 4, 5, 6}, "
            "{2, 1, 5, 6, 3, 4}, "
            "{3, 4, 1, 2, 6, 5}, "
            "{4, 3, 6, 5, 1, 2}, "
            "{5, 6, 2, 1, 4, 3}, "
            "{6, 5, 4, 3, 2, 1}}",
            "Symmetric group S_n. S_n is not commutative. 1.2 Page 17"
        ),
        (
            "InversePermutation[{4,8,5,2,1,3,7,6}]",
            "{5, 4, 6, 1, 3, 8, 7, 2}",
            "InversePermutation: 7 is fixed point. 1.2 Page 18",
        ),
        (
            "star = Automorphisms[Star[5]]",
            "{{1, 2, 3, 4, 5}, {1, 2, 4, 3, 5}, {1, 3, 2, 4, 5}, {1, 3, 4, 2, 5}, "
            "{1, 4, 2, 3, 5}, {1, 4, 3, 2, 5}, {2, 1, 3, 4, 5}, {2, 1, 4, 3, 5}, "
            "{2, 3, 1, 4, 5}, {2, 3, 4, 1, 5}, {2, 4, 1, 3, 5}, {2, 4, 3, 1, 5}, "
            "{3, 1, 2, 4, 5}, {3, 1, 4, 2, 5}, {3, 2, 1, 4, 5}, {3, 2, 4, 1, 5}, "
            "{3, 4, 1, 2, 5}, {3, 4, 2, 1, 5}, {4, 1, 2, 3, 5}, {4, 1, 3, 2, 5}, "
            "{4, 2, 1, 3, 5}, {4, 2, 3, 1, 5}, {4, 3, 1, 2, 5}, {4, 3, 2, 1, 5}}",
            "Automorphisms, 1.2.3 Page 19",
        ),
        (
            "relation = SamenessRelation[star]",
            "{{1, 1, 1, 1, 0}, "
            "{1, 1, 1, 1, 0}, "
            "{1, 1, 1, 1, 0}, "
            "{1, 1, 1, 1, 0}, "
            "{0, 0, 0, 0, 1}}",
            "Sameness, 1.2.3 Page 19",
        ),
        # (
        #     "PermutationGroupQ[Range[4], {4, 2, 3, 1}]",
        #     "True",
        #     "PermutationGroupQ, 1.2.3 Page 20",
        # ),
    ):
        check_evaluation(str_expr, str_expected, message)


def test_combinatorica_rest():

    # Permutation[3] doesn't work
    permutations3 = (
        r"{{1, 2, 3}, {1, 3, 2}, {2, 1, 3}, {2, 3, 1}, {3, 1, 2}, {3, 2, 1}}"
    )
    for str_expr, str_expected, message in (
        (
            "Permute[{A, B, C, D}, %s]" % permutations3,
            "{{A, B, C}, {A, C, B}, {B, A, C}, {B, C, A}, {C, A, B}, {C, B, A}}",
            "Permute",
        ),
        (
            "Subsets[Range[3]]",
            "{{}, {1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3}}",
            "Subsets",
        ),
        (
            "BinarySearch[{2, 3, 9}, 7] // N",
            "2.5",
            "BinarySearch - mid-way insertion point",
        ),
        ("BinarySearch[{3, 4, 10, 100, 123}, 100]", "4", "BinarySearch find item"),
        (
            "BinarySearch[{2, 3, 9}, 7] // N",
            "2.5",
            "BinarySearch - mid-way insertion point",
        ),
        (
            "BinarySearch[{2, 7, 9, 10}, 3] // N",
            "1.5",
            "BinarySearch - insertion point after 1st item",
        ),
        (
            "BinarySearch[{-10, 5, 8, 10}, -100] // N",
            "0.5",
            "BinarySearch find before first item",
        ),
        (
            "BinarySearch[{-10, 5, 8, 10}, 20] // N",
            "4.5",
            "BinarySearch find after last item",
        ),
        (
            "BinarySearch[{{a, 1}, {b, 7}}, 7, #[[2]]&]",
            "2",
            "BinarySearch - find where key is a list",
        ),
        (
            "InversePermutation[{4,8,5,2,1,3,7,6}]",
            "{5, 4, 6, 1, 3, 8, 7, 2}",
            "InversePermutation: 7 is fixed point. Page 18",
        ),
        # (
        #     "SetPartitions[3]",
        #     "{{{1, 2, 3}}, {{1}, {2, 3}}, {{1, 2}, {3}}, {{1, 3}, {2}}, {{1}, {2}, {3}}}",
        #     "SetPartitions"
        # ),
        (
            "TransposePartition[{8, 6, 4, 4, 3, 1}]",
            "{6, 5, 5, 4, 2, 2, 1, 1}",
            "TransposePartition",
        ),
    ):
        check_evaluation(str_expr, str_expected, message)
