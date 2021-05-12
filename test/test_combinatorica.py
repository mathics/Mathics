# -*- coding: utf-8 -*-
from .helper import session, check_evaluation

session.evaluate(
    """
    Needs["DiscreteMath`CombinatoricaV0.9`"]
    """
)

# A number of examples from:
#  * Implementing Discrete Mathematics by Steven Skiena and
#  * Computation Discrete Mathematics by Sriram Pemmaraju and Steven Skiena.

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
            " {a, c, d, b}, {a, d, b, c}, {a, d, c, b}, "
            " {b, a, c, d}, {b, a, d, c}, {b, c, a, d}, "
            " {b, c, d, a}, {b, d, a, c}, {b, d, c, a}, "
            " {c, a, b, d}, {c, a, d, b}, {c, b, a, d}, "
            " {c, b, d, a}, {c, d, a, b}, {c, d, b, a}, "
            " {d, a, b, c}, {d, a, c, b}, {d, b, a, c}, "
            " {d, b, c, a}, {d, c, a, b}, {d, c, b, a}}",
            "LexicographicPermuations, 1.1.1 Page 4",
        ),
        (
            "Table[ NthPermutation[n, Range[4]], {n, 0, 23}]",
            "{{1, 2, 3, 4}, {1, 2, 4, 3}, {1, 3, 2, 4}, {1, 3, 4, 2}, "
            " {1, 4, 2, 3}, {1, 4, 3, 2}, {2, 1, 3, 4}, {2, 1, 4, 3}, "
            " {2, 3, 1, 4}, {2, 3, 4, 1}, {2, 4, 1, 3}, {2, 4, 3, 1}, "
            " {3, 1, 2, 4}, {3, 1, 4, 2}, {3, 2, 1, 4}, {3, 2, 4, 1}, "
            " {3, 4, 1, 2}, {3, 4, 2, 1}, {4, 1, 2, 3}, {4, 1, 3, 2}, "
            " {4, 2, 1, 3}, {4, 2, 3, 1}, {4, 3, 1, 2}, {4, 3, 2, 1}} ",
            "slower method for computing permutations in lex order, 1.1.2, Page 6",
        ),
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
            " {2, 1, 1, 2}, {2, 1, 2, 1}, {2, 2, 1, 1}}",
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
            "{{}, {1}, {2}, {3}, {1, 2}, {4}, "
            " {1, 3}, {1, 4}, {2, 3}, {2, 4}, "
            " {1, 2, 3}, {3, 4}, {1, 2, 4}, {1, 3, 4}, {2, 3, 4}, "
            " {1, 2, 3, 4}}",
            "Sort to total order subsets, Page 15",
        ),
    ):
        check_evaluation(str_expr, str_expected, message)


def test_combinatorica_permutations_1_2():

    for str_expr, str_expected, message in (
        (
            "MultiplicationTable[Permutations[Range[3]], Permute ]",
            "{{1, 2, 3, 4, 5, 6}, "
            " {2, 1, 5, 6, 3, 4}, "
            " {3, 4, 1, 2, 6, 5}, "
            " {4, 3, 6, 5, 1, 2}, "
            " {5, 6, 2, 1, 4, 3}, "
            " {6, 5, 4, 3, 2, 1}}",
            "Symmetric group S_n. S_n is not commutative. 1.2 Page 17",
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
            " {1, 1, 1, 1, 0}, "
            " {1, 1, 1, 1, 0}, "
            " {1, 1, 1, 1, 0}, "
            " {0, 0, 0, 0, 1}}",
            "Sameness, 1.2.3 Page 19",
        ),
        (
            "EquivalenceClasses[relation]",
            "{{1, 2, 3, 4}, {5}}",
            "EquivalenceClasses, 1.2.3, Page 19",
        ),
        (
            "PermutationGroupQ[{{1, 2, 3, 4}, {4, 2, 3, 1}}]",
            "True",
            "PermutationGroupQ, 1.2.3 Page 20",
        ),
        (
            "ToCycles[Range[10]]",
            "{{1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}}",
            "ToCycles, 1.2.4, Page 21",
        ),
        (
            "ToCycles[r = RotateLeft[Range[10],1]]",
            "{r}",
            "ToCycles with rotation by 1",
        ),
        (
            "Select[ Permutations[Range[4]], (Length[ToCycles[#]] == 1)&]",
            "{{2, 3, 4, 1}, {2, 4, 1, 3}, {3, 1, 4, 2}, "
            " {3, 4, 2, 1}, {4, 1, 2, 3}, {4, 3, 1, 2}}",
            "ToCycles, 1.2.4, Page 21",
        ),
        (
            "ToCycles[ Reverse[Range[10]] ]",
            "{{10, 1}, {9, 2}, {8, 3}, {7, 4}, {6, 5}}",
            "Reverse ToCycles, 1.2.4, Page 21",
        ),
        (
            "Permute[ Reverse[Range[10]], Reverse[Range[10]] ]",
            "Range[10]",
            "Pemute as involution, 1.2.4, Page 21",
        ),
        (
            "Apply[ And, List[p=RandomPermutation[8]; p===FromCycles[ToCycles[p]]] ]",
            "True",
            "Convert to-and-from cycle structure is identity, 1.2.4, Page 22",
        ),
        (
            "Apply[ And, List[p=RandomPermutation[8]; p===FromCycles[ToCycles[p]]] ]",
            "True",
            "Convert to-and-from cycle structure is identity, 1.2.4, Page 22",
        ),
        (
            "ToCycles[{6,2,1,5,4,3} ]",
            "{{6, 3, 1}, {2}, {5, 4}}",
            "Three permutations, one of each size, 1.2.4, Page 22",
        ),
        (
            "HideCycles[ToCycles[{6,2,1,5,4,3}]]",
            "{4, 5, 2, 1, 6, 3}",
            "Permutations is not what we started with, 1.2.4, Page 23",
        ),
        (
            "RevealCycles[ HideCycles[ToCycles[{6,2,1,5,4,3}]] ]",
            "{{4, 5}, {2}, {1, 6, 3}}",
            "RevealCycles 1.2.4, Page 23",
        ),
        (
            "Apply[Or, Map[(# === HideCycles[ToCycles[#]])&, Permutations[Range[5]] ]]",
            "False",
            "None of the permutations on five elements is identical to its hidden cycle representation 1.2.4, Page 23",
        ),
        (
            "{StirlingFirst[6,3], StirlingS1[6,3]}",
            "{225, -225}",
            "StirlingFirst 1.2.4, Page 24",
        ),
        (
            "Select[ Map[ToCycles, Permutations[Range[4]]], (Length[#]==2)&]",
            "{{{1}, {3, 4, 2}}, {{1}, {4, 3, 2}}, {{2, 1}, {4, 3}}, "
            " {{2, 3, 1}, {4}}, {{2, 4, 1}, {3}}, {{3, 2, 1}, {4}}, "
            " {{3, 4, 1}, {2}}, {{3, 1}, {4, 2}}, {{4, 2, 1}, {3}}, "
            " {{4, 3, 1}, {2}}, {{4, 1}, {3, 2}}}",
            "11 permutations of 4 elements and 2 cycles, Page 24",
        ),
        (
            "NumberOfPermutationsByCycles[4,2]",
            "11",
            "NumberOfPermutationsByCycles 1.2.4, Page 24",
        ),
        (
            "{StirlingSecond[6,3], StirlingS2[6,3]}",
            "{90, 90}",
            "StirlingSecond 1.2.4, Page 24",
        ),
        (
            "SignaturePermutation[{1,3,2,4,5,6,7,8}]",
            "-1",
            "SignaturePermutation 1.2.5, Page 25",
        ),
        (
            "SignaturePermutation[p] == SignaturePermutation[InversePermutation[p]]",
            "True",
            "A particular permutation has the same sign as its inverse 1.2.5, Page 25",
        ),
        (
            "PermutationGroupQ[ Select [ Permutations[Range[4]], (SignaturePermutation[#]==1)&] ]",
            "True",
            "All permutations have the same sign as their inverse 1.2.5, Page 25",
        ),
        (
            "Polya[Table[RotateRight[Range[8],i], {i, 8}], m]",
            "(4 m + 2 m ^ 2 + m ^ 4 + m ^ 8) / 8",
            "Polya counting resulting in polynomial 1.2.6, Page 25",
        ),
        # Automorphism is slow. So we reduce Cycle[8] given as the example in the
        # book to Cycle[3].
        (
            "Polya[Automorphisms[Cycle[3]], m]",
            "(2 m + 3 m ^ 2 + m ^ 3) / 6",
            "Polya counting resulting in polynomial 1.2.6, Page 26",
        ),
        (
            "Factor[(2 m + 3 m ^ 2 + m ^ 3) / 6]",
            "m (1 + m) (2 + m) / 6",
            "Factor Polya polynomial 1.2.6, Page 26",
        ),
        (
            "Factor[(4 m + 2 m^2 + 5m^4 + 4m^5 + m^8)/16]",
            "m (1 + m) (4 - 2 m + 2 m ^ 2 + 3 m ^ 3 + m ^ 4 - m ^ 5 + m ^ 6) / 16",
            "Factor example in Polya polynomial 1.2.6, Page 26",
        ),

    ):
        check_evaluation(str_expr, str_expected, message)


def test_combinatorica_permutations_1_5():

    # We include this earlier since the above in fact rely on KSubsets
    for str_expr, str_expected, message in (
        (
            "KSubsets[Range[5], 3]",
            "{{1, 2, 3}, {1, 2, 4}, {1, 2, 5}, {1, 3, 4}, {1, 3, 5}, "
            "{1, 4, 5},  {2, 3, 4}, {2, 3, 5}, {2, 4, 5}, {3, 4, 5}}",
            "KSubsets 1.5.5, Page 45",
        ),
        (
            "KSubsets[Range[3], 0]",
            "{ {} } ",
            "KSubsets[0] == { {} }",
        ),
        (
            "KSubsets[Range[5], 1]",
            "{{1}, {2}, {3}, {4}, {5}}",
            "KSubsets[Range[n, 1] == Partition[n]",
        ),
        (
            "KSubsets[Range[5], 5]",
            "{Range[5]} ",
            "KSubsets[l, k] == Length(l)",
        ),
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
