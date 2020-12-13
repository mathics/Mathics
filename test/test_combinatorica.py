# -*- coding: utf-8 -*-
from .helper import session, check_evaluation

import sys
from mathics.core.parser import parse, SingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
import pytest


def test_combinatorica():
    session.evaluate(
        """
     Needs["DiscreteMath`CombinatoricaLite`"]
     """
    )

    # A number of examples from Computation Discrete Mathematics by
    # Sriram Pemmaraju and Steven Skiena

    # Permutation[3] doesn't work
    permutations3 = (
        r"{{1, 2, 3}, {1, 3, 2}, {2, 1, 3}, {2, 3, 1}, {3, 1, 2}, {3, 2, 1}}"
    )
    for str_expr, str_expected, message in (
        (
            "Permute[{a, b, c, d}, Range[4]]",
            "{a, b, c, d}",
            "Permute list with simple list",
        ),
        (
            "Permute[{a, b, c, d}, {1,2,2,4}]",
            "Permute[{a, b, c, d}, {1,2,2,4}]",
            "Incorrect permute: index 2 duplicated",
        ),
        (
            "Permute[{A, B, C, D}, %s]" % permutations3,
            "{{A, B, C}, {A, C, B}, {B, A, C}, {B, C, A}, {C, A, B}, {C, B, A}}",
            "Permute",
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
            "LexicographicPermuations"
        ),

        ("Map[RankPermutation, Permutations[Range[4]]]",
         "Range[0, 23]",
         "Permutations uses lexographic order"
         ),

        ("RandomPermutation1[20] === RandomPermutation2[20]",
         "False",
         "Not likey two of 20! permutations will be the same (different routines)"
         ),
        ("RandomPermutation1[20] === RandomPermutation1[20]",
         "False",
         "Not likey two of 20! permutations will be the same (same routine)"
         ),
        ("RankPermutation[{8, 9, 7, 1, 6, 4, 5, 3, 2}]", "321953", "RankPermutation"),
        (
            "Permute[{5,2,4,3,1}, InversePermutation[{5,2,4,3,1}]]",
            "{1, 2, 3, 4, 5}",
            "InversePermute",
        ),
        (
            "MinimumChangePermutations[{a,b,c}]",
            "{{a, b, c}, {b, a, c}, {c, a, b}, {a, c, b}, {b, c, a}, {c, b, a}}",
            "MinimumChangePermuations",
        ),
        (
            "Subsets[Range[3]]",
            "{{}, {1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3}}",
            "Subsets",
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
            "KSubsets[Range[5], 3]",
            "{{1, 2, 3}, {1, 2, 4}, {1, 2, 5}, {1, 3, 4}, {1, 3, 5}, {1, 4, 5}, "
            "{2, 3, 4}, {2, 3, 5}, {2, 4, 5}, {3, 4, 5}}",
            "Ksubsets",
        ),
        (
            "Compositions[5,3]",
            "{{0, 0, 5}, {0, 1, 4}, {0, 2, 3}, {0, 3, 2}, {0, 4, 1}, {0, 5, 0}, "
            "{1, 0, 4}, {1, 1, 3}, {1, 2, 2}, {1, 3, 1}, {1, 4, 0}, {2, 0, 3}, "
            "{2, 1, 2}, {2, 2, 1}, {2, 3, 0}, {3, 0, 2}, {3, 1, 1}, {3, 2, 0}, "
            "{4, 0, 1}, {4, 1, 0}, {5, 0, 0}}",
            "Compositions",
        ),
        (
            "SetPartitions[3]",
            "{{{1, 2, 3}}, {{1}, {2, 3}}, {{1, 2}, {3}}, {{1, 3}, {2}}, {{1}, {2}, {3}}}",
            "SetPartitions"
        ),
        (
            "TransposePartition[{8, 6, 4, 4, 3, 1}]",
            "{6, 5, 5, 4, 2, 2, 1, 1}",
            "TransposePartition"
        ),
    ):
        check_evaluation(str_expr, str_expected, message)
