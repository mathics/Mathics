(* ::Package:: *)

(* :Title: Combinatorica Lite *)

(* :Summary:
This is a stripped-down version of Combinatorica.

Perhaps one day we'll be support the full thing, as an import.

Until then this is useful for getting us there.

The original contains:

:Copyright: Copyright 2000-2003 by Sriram V. Pemmaraju and Steven S. Skiena

This package may be copied in its entirety for nonprofit purposes only
Sale, other than for the direct cost of the media, is prohibited.  This
copyright notice must accompany all copies.

The authors, Wolfram Research, and Cambridge University Press
make no representations, express or implied, with respect to this
documentation, or the software it describes and contains, including
without limitations, any implied warranties of mechantability or fitness
for a particular purpose, all of which are expressly disclaimed.  The
authors, Wolfram Research, or Cambridge University Press, their licensees,
distributors and dealers shall in no event be liable for any indirect,
incidental, or consequential damages.

And for the 0.6 version:
		Version 0.6   6/11/90   Beta Release
		Copyright (c) 1990 by Steven S. Skiena

*)

BeginPackage["DiscreteMath`CombinatoricaLite`"]

PermutationQ::usage = "PermutationQ[p] yields True if p is a list representing a permutation and False otherwise."
PermutationQ[e_List] := (Sort[e] === Range[Length[e]])

(*
Unprotect[Permutations]
Permutations[n_Integer] := Permutations[Range[n]]
Protect[Permutations]
 *)

Permute::usage = "Permute[l, p] permutes list l according to permutation p."
Permute[l_List,p_?PermutationQ] := l [[ p ]]
Permute[l_List,p_List] := Map[ (Permute[l,#])&, p] /; (Apply[And, Map[PermutationQ, p]])

InversePermutation::usage = "InversePermutation[p] yields the multiplicative inverse of permutation p."
InversePermutation[p_?PermutationQ] :=
	Module[{inverse=p},
		Do[ inverse[[ p[[i]] ]] = i, {i,Length[p]} ];
		inverse
	]

LexicographicPermutations::usage = "LexicographicPermutations[l] constructs all permutations of list l in lexicographic order."
LexicographicPermutations[0] := {{}}
LexicographicPermutations[1] := {{1}}

(*
LexicographicPermutations[n_Integer?Positive] := LP[n]
LexicographicPermutations[l_List] := Permute[l, LexicographicPermutations[Length[l]] ]
LP[{{n, _Integer}}] :=
	Module[{l = Range[n], i, j, t},
               NestList[(i = n-1; While[ #[[i]] > #[[i+1]], i--];
                         j = n; While[ #[[j]] < #[[i]], j--];
                         t = #[[i]]; #[[i]] = #[[j]]; #[[j]] = t;
                         Join[ Take[#,i], Reverse[Drop[#,i]] ])&,
							       l, n!-1
               ]
	]
 *)

MinimumChangePermutations::usage = "MinimumChangePermutations[l] constructs all permutations of list l such that adjacent permutations differ by only one transposition."
MinimumChangePermutations[l_List] := LexicographicPermutations[l] /; (Length[l] < 2)
MinimumChangePermutations[l_List] :=
	Module[{i=1,c,p=l,n=Length[l],k},
		c = Table[1,{n}];
		Join[{l},
                     Table[While [ c[[i]] >= i, c[[i]] = 1; i++];
                           If[OddQ[i], k=1, k=c[[i]] ];
                           {p[[i]],p[[k]]} = {p[[k]],p[[i]]};
                           c[[i]]++; i = 2; p,
                           {n!-1}
                     ]
                ]
        ]

RankPermutation::usage = "RankPermutation[p] gives the rank of permutation p in lexicographic order."
RankPermutation[{1}] := 0
RankPermutation[{}] := 0

RankPermutation[p_?PermutationQ] :=
        Block[{$RecursionLimit = Infinity},
              (p[[1]]-1) (Length[Rest[p]]!) +
              RankPermutation[ Map[(If[#>p[[1]], #-1, #])&, Rest[p]]]
        ]

UnrankPermutation::usage = "UnrankPermutation[r, l] gives the rth permutation in the lexicographic list of permutations of list l. UnrankPermutation[r, n] gives the rth permutation in the lexicographic list of permutations of {1, 2,..., n}."

UnrankPermutation[r_Integer, {}] := {}
UnrankPermutation[r_Integer, l_List] :=
        Module[{s = l, k, t, p = UP[Mod[r, Length[l]!], Length[l]]},
               Table[k = s[[t = p[[i]] ]];
                     s = Delete[s, t];
                     k,
                     {i, Length[ p ]}
               ]
        ]

UnrankPermutation[r_Integer, n_Integer?Positive] :=
        UnrankPermutation[r, Range[n]]

Compositions::usage = "Compositions[n, k] gives a list of all compositions of integer n into k parts."
Compositions[n_Integer,k_Integer] :=
	Map[
		(Map[(#[[2]]-#[[1]]-1)&, Partition[Join[{0},#,{n+k}],2,1] ])&,
		KSubsets[Range[n+k-1],k-1]
	]

Cyclic::usage = "Cyclic is an argument to the Polya-theoretic functions ListNecklaces, NumberOfNecklace, and NecklacePolynomial, which count or enumerate distinct necklaces. Cyclic refers to the cyclic group acting on necklaces to make equivalent necklaces that can be obtained from each other by rotation.";

CyclicGroup::usage = "CyclicGroup[n] returns the cyclic group of permutations on n symbols.";
CyclicGroup[0] := {{}}
CyclicGroup[n_Integer] := Table[RotateRight[Range[n], i], {i, 0, n-1}]

DihedralGroupIndex::usage = "DihedralGroupIndex[n, x] returns the cycle index of the dihedral group on n symbols, expressed as a polynomial in x[1], x[2], ..., x[n].";

DihedralGroupIndex[n_Integer?Positive , x_Symbol] :=
        Expand[Simplify[CyclicGroupIndex[n, x]/2 +
                        If[EvenQ[n],
                           (x[2]^(n/2) + x[1]^2x[2]^(n/2-1))/4,
                           (x[1]x[2]^((n-1)/2))/2
                        ]
               ]
        ]

(*
NecklacePolynomial::usage = "NecklacePolynomial[n, c, Cyclic] returns a polynomial in the colors in c whose coefficients represent numbers of ways of coloring an n-bead necklace with colors chosen from c, assuming that two colorings are equivalent if one can be obtained from the other by a rotation. NecklacePolynomial[n, c, Dihedral] is different in that it considers two colorings equivalent if one can be obtained from the other by a rotation or a flip or both.";

NecklacePolynomial[n_Integer?Positive, c_, Cyclic] :=
        OrbitInventory[CyclicGroupIndex[n, x], x, c]

NecklacePolynomial[n_Integer?Positive, c_, Dihedral] :=
        OrbitInventory[DihedralGroupIndex[n, x], x, c]
 *)

OrbitInventory::usage = "OrbitInventory[ci, x, w] returns the value of the cycle index ci when each formal variable x[i] is replaced by w. OrbitInventory[ci, x, weights] returns the inventory of orbits induced on a set of functions by the action of a group with cycle index ci. It is assumed that each element in the range of the functions is assigned a weight in list weights.";


OrbitInventory[ci_?PolynomialQ, x_Symbol, weights_List] :=
        Expand[ci /. Table[x[i] ->  Apply[Plus, Map[#^i&, weights]],
                                  {i, Exponent[ci, x[1]]}
              ]
        ]

OrbitInventory[ci_?PolynomialQ, x_Symbol, r_] :=
        Expand[ci /. Table[x[i] -> r, {i, Exponent[ci, x[1]]} ]]

(* Not working: always returns the same sorted value.
  We don't support Compile
 *)
(***
RP = Compile[{{n, _Integer}},
             Module[{p = Range[n],i,x,t},
	            Do [x = Random[Integer,{1,i}];
		        t = p[[i]]; p[[i]] = p[[x]]; p[[x]] = t,
		        {i,n,2,-1}
		    ];
	            p
	     ]
        ]

RandomPermutation[n_Integer] := RP[n]
RandomPermutation[l_List] := Permute[l, RP[Length[l]]]
 ***)

GrayCodeSubsets::usage = "GrayCodeSubsets[l] constructs a binary reflected Gray code on set l";
GrayCodeSubsets[n_Integer?Positive] := GrayCodeSubsets[Range[n]]

GrayCodeSubsets[ { } ] := { {} }

GrayCodeSubsets[l_List] :=
       Block[{s, $RecursionLimit = Infinity},
              s = GrayCodeSubsets[Take[l, 1-Length[l]]];
              Join[s,  Map[Prepend[#, First[l]] &, Reverse[s]]]
       ]

(* We have a builtin that does this. Keep that?
Subsets::usage = "Subsets[l] gives all subsets of set l."
Subsets[l_List] := GrayCodeSubsets[l]
Subsets[n_Integer] := GrayCodeSubsets[Range[n]]
*)

KSetPartitions::usage = "KSetPartitions[set, k] returns the list of set partitions of set with k blocks. KSetPartitions[n, k] returns the list of set partitions of {1, 2, ..., n} with k blocks. If all set partitions of a set are needed, use the function SetPartitions."
KSetPartitions[{}, 0] := {{}}
KSetPartitions[s_List, 0] := {}
KSetPartitions[s_List, k_Integer] := {} /; (k > Length[s])
KSetPartitions[s_List, k_Integer] := {Map[{#} &, s]} /; (k === Length[s])
KSetPartitions[s_List, k_Integer] :=
       Block[{$RecursionLimit = Infinity},
             Join[Map[Prepend[#, {First[s]}] &, KSetPartitions[Rest[s], k - 1]],
                  Flatten[
                     Map[Table[Prepend[Delete[#, j], Prepend[#[[j]], s[[1]]]],
                              {j, Length[#]}
                         ]&,
                         KSetPartitions[Rest[s], k]
                     ], 1
                  ]
             ]
       ] /; (k > 0) && (k < Length[s])

KSetPartitions[0, 0] := {{}}
KSetPartitions[0, k_Integer?Positive] := {}
KSetPartitions[n_Integer?Positive, 0] := {}
KSetPartitions[n_Integer?Positive, k_Integer?Positive] := KSetPartitions[Range[n], k]

(*
KSubsets[l_List,0] := { {} }
KSubsets[l_List,1] := Partition[l,1]
KSubsets[l_List,2] := Flatten[Table[{l[[i]], l[[j]]},
                                    {i, Length[l]-1},
                                    {j, i+1, Length[l]}
                              ],
                              1

KSubsets::usage = "KSubsets[l, k] gives all subsets of set l containing exactly k elements, ordered lexicographically.";
KSubsets[l_List,k_Integer?Positive] := {l} /; (k == Length[l])
KSubsets[l_List,k_Integer?Positive] := {}  /; (k > Length[l])

KSubsets[s_List, k_Integer] := Prepend[Map[s[[#]] &, KS[Length[s], k]], s[[Range[k] ]] ]
 *)

(*
BinarySearch::usage = "BinarySearch[l, k] searches sorted list l for key k and gives the position of l containing k, if k is present in l. Otherwise, if k is absent in l, the function returns (p + 1/2) where k falls between the elements of l in positions p and p+1. BinarySearch[l, k, f] gives the position of k in the list obtained from l by applying f to each element in l."

BinarySearch::error = "The input list is non-numeric."
BinarySearch[l_?(Length[#] > 0&), k_?NumericQ, f_:Identity]:=
        With[{res = binarysearchchore[l, k, f]},
             res/; res =!= $Failed
        ]
binarysearchchore[l_, k_, f_]:=
        Module[{lo = 1, mid, hi = Length[l], el},
                    While[lo <= hi,
                        If[(el=f[l[[mid =
                                    Floor[ (lo + hi)/2 ]]]])===k,
                           Return[mid]
                        ];
            If[!NumericQ[el], (Message[BinarySearch::error]; Return[$Failed])];
                        If[el > k, hi = mid-1, lo = mid+1]
                    ];
                    Return[lo-1/2]
        ];

 *)

TransposePartition::usage = "TransposePartition[p] reflects a partition p of k parts along the main diagonal, creating a partition with maximum part k."
TransposePartition[{}] := {}

TransposePartition[p_List] :=
	Module[{s=Select[p,(#>0)&], i, row, r},
		row = Length[s];
		Table [r = row; While [s[[row]]<=i, row--]; r, {i,First[s]}]
	]


(*** FIXME: we run into recursion errors for nontrivial partitions. ***)
(*
Partitions::usage = "Partitions[n] constructs all partitions of integer n in reverse lexicographic order. Partitions[n, k] constructs all partitions of the integer n with maximum part at most k, in reverse lexicographic order."

Partitions[n_Integer] := Partitions[n,n]

Partitions[n_Integer,_] := {} /; (n<0)
Partitions[0,_] := { {} }
Partitions[n_Integer,1] := { Table[1,{n}] }
Partitions[_,0] := {}

Partitions[n_Integer, maxpart_Integer] :=
        Block[{$RecursionLimit = Infinity},
	      Join[Map[(Prepend[#,maxpart])&, Partitions[n-maxpart,maxpart]],
                   Partitions[n,maxpart-1]
              ]
	]
 *)

SetPartitions[{}] := {{}}
SetPartitions[s_List] := Flatten[Table[KSetPartitions[s, i], {i, Length[s]}], 1]

SetPartitions[0] := {{}}
SetPartitions[n_Integer?Positive] := SetPartitions[Range[n]]

LastLexicographicTableau::usage = "LastLexicographicTableau[p] constructs the last Young tableau with shape described by partition p."
LastLexicographicTableau[s_List] :=
	Module[{c=0},
		Map[(c+=#; Range[c-#+1,c])&, s]
	]

(*
NumberOfTableaux::usage = "NumberOfTableaux[p] uses the hook length formula to count the number of Young tableaux with shape defined by partition p."
NumberOfTableaux[{}] := 1
NumberOfTableaux[s_List] :=
	Module[{row,col,transpose=TransposePartition[s]},
		(Apply[Plus,s])! /
		Product [
			(transpose[[col]]-row+s[[row]]-col+1),
			{row,Length[s]}, {col,s[[row]]}
		]
	]

NumberOfTableaux[n_Integer] := Apply[Plus, Map[NumberOfTableaux, Partitions[n]]]
 *)

TransposePartition::usage = "TransposePartition[p] reflects a partition p of k parts along theg main diagonal, creating a partition with maximum part k."
TransposePartition[{}] := {}

(*
TransposePartition[p_List] :=
	Module[{s=Select[p,(#>0)&], i, row, r},
		row = Length[s];
		Table [r = row; While [s[[row]]<=i, row--]; r, {i,First[s]}]
	]
 *)


Tableaux::usage = "Tableaux[p] constructs all tableaux having a shape given by integer partition p."
Tableaux[s_List] :=
	Module[{t = LastLexicographicTableau[s]},
		Table[ t = NextTableau[t], {NumberOfTableaux[s]} ]
	]

Tableaux[n_Integer?Positive] := Apply[ Join, Map[ Tableaux, Partitions[n] ] ]


(****************************************************************************
*** Combinatorica 0.6 versions until we support more modern WL features *****
*****************************************************************************)

(* Note: Until we support With[], this is the Combinatorica 0.6  version of BinarySearch *)
BinarySearch::usage = "BinarySearch[l,k,f] searches sorted list l for key k and returns the the position of l containing k, with f a function which extracts the key from an element of l."
BinarySearch[l_List,k_Integer] := BinarySearch[l,k,1,Length[l],Identity]
BinarySearch[l_List,k_Integer,f_] := BinarySearch[l,k,1,Length[l],f]

BinarySearch[l_List,k_Integer,low_Integer,high_Integer,f_] :=
	Block[{mid = Floor[ (low + high)/2 ]},
		If [low > high, Return[low - 1/2]];
		If [f[ l[[mid]] ] == k, Return[mid]];
		If [f[ l[[mid]] ] > k,
			BinarySearch[l,k,1,mid-1,f],
			BinarySearch[l,k,mid+1,high,f]
		]
	]

KSubsets::usage = "KSubsets[l, k] gives all subsets of set l containing exactly k elements, ordered lexicographically."
KSubsets[l_List,0] := { {} }
KSubsets[l_List,1] := Partition[l,1]
KSubsets[l_List,k_Integer?Positive] := {l} /; (k == Length[l])
KSubsets[l_List,k_Integer?Positive] := {}  /; (k > Length[l])

KSubsets[l_List,k_Integer?Positive] :=
	Join[
		Map[(Prepend[#,First[l]])&, KSubsets[Rest[l],k-1]],
		KSubsets[Rest[l],k]
	]

LexicographicPermutations[{a_,b_}] := {{a,b},{b,a}}

LexicographicPermutations[l_List] :=
	Block[{i,n=Length[l]},
		Apply[
			Join,
			Table[
				Map[
					(Prepend[#,l[[i]]])&,
					LexicographicPermutations[
						Complement[l,{l[[i]]}]
					]
				],
				{i,n}
			]
		]
	]


(* Not working: always returns the same sorted value.
Probably Sort[] below is buggy.
 *)
(*
RandomPermutation::usage = "RandomPermutation[n] returns a random permutation of length n."
RandomPermutation1[n_Integer?Positive] :=
	Map[ Last, Sort[ Map[({Random[],#})&,Range[n]] ] ]

RandomPermutation2[n_Integer?Positive] :=
	Block[{p = Range[n],i,x},
		Do [
			x = Random[Integer,{1,i}];
			{p[[i]],p[[x]]} = {p[[x]],p[[i]]},
			{i,n,2,-1}
		];
		p
	]
RandomPermutation[n_Integer?Positive] := RandomPermutation1[n]
  *)

(* Tableaux stuff not working. Hitting recursion limit....
TransposeTableau::usage = "TransposeTableau[t] reflects a Young tableau t along the main diagonal, creating a different tableau."
TransposeTableau[tb_List] :=
	Block[{t=Select[tb,(Length[#]>=1)&],row},
		Table[
			row = Map[First,t];
			t = Map[ Rest, Select[t,(Length[#]>1)&] ];
			row,
			{Length[First[tb]]}
		]
	]

TableauQ::usage = "TableauQ[t] returns True if and only if t represents a Young tableau."
TableauQ[{}] = True
TableauQ[t_List] :=
	And [
		Apply[ And, Map[(Apply[LessEqual,#])&,t] ],
		Apply[ And, Map[(Apply[LessEqual,#])&,TransposeTableau[t]] ],
		Apply[ GreaterEqual, Map[Length,t] ],
		Apply[ GreaterEqual, Map[Length,TransposeTableau[t]] ]
	]


NextTableau::usage = "NextTableau[t] returns the tableau of shape t which follows t in lexicographic order."
NextTableau[t_?TableauQ] :=
	Block[{s,y,row,j,count=0,tj,i,n=Max[t]},
		y = TableauToYVector[t];
		For [j=2, (j<n)  && (y[[j]]>=y[[j-1]]), j++];
		If [y[[j]] >= y[[j-1]],
			Return[ FirstLexicographicTableau[ ShapeOfTableau[t] ] ]
		];
		s = ShapeOfTableau[ Table[Select[t[[i]],(#<=j)&], {i,Length[t]}] ];
		{row} = Last[ Position[ s, s[[ Position[t,j] [[1,1]] + 1 ]] ] ];
		s[[row]] --;
		tj = FirstLexicographicTableau[s];
		If[ Length[tj] < row,
			tj = Append[tj,{j}],
			tj[[row]] = Append[tj[[row]],j]
		];
		Join[
			Table[
				Join[tj[[i]],Select[t[[i]],(#>j)&]],
				{i,Length[tj]}
			],
			Table[t[[i]],{i,Length[tj]+1,Length[t]}]
		]
	]


NumberOfTableaux::usage = "NumberOfTableaux[p] uses the hook length formula to count the number of Young tableaux with shape defined by partition p."
NumberOfTableaux[{}] := 1
NumberOfTableaux[s_List] :=
	Block[{row,col,transpose=TransposePartition[s]},
		(Apply[Plus,s])! /
		Product [
			(transpose[[col]]-row+s[[row]]-col+1),
			{row,Length[s]}, {col,s[[row]]}
		]
	]

NumberOfTableaux[n_Integer] := Apply[Plus, Map[NumberOfTableaux, Partitions[n]]]
*)

EndPackage[]
