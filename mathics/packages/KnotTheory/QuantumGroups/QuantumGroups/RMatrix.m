(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



BeginPackage["QuantumGroups`RMatrix`",{"QuantumGroups`","QuantumGroups`Utilities`MatrixWrapper`","QuantumGroups`Utilities`Debugging`","QuantumGroups`RootSystems`","QuantumGroups`Algebra`","QuantumGroups`WeylGroups`","QuantumGroups`Representations`","QuantumGroups`QuantumRoots`","QuantumGroups`MatrixPresentations`"}];


RMatrix::usage="";


CheckRMatrixOppositeCommutes::usage="";


Begin["`Private`"];


q=Global`q;


PartialRMatrix[\[CapitalGamma]_][n_]:=PartialRMatrix[\[CapitalGamma]][n]=Module[{p=Length[QuantumPositiveRoots[\[CapitalGamma]]],iterators,r,d=CartanFactors[\[CapitalGamma]],i=LongestWordDecomposition[\[CapitalGamma]],l,t,rmatrix},
DebugPrintHeld["Calculating ",PartialRMatrix[\[CapitalGamma]][n]];
l=QuantumRootHeight[\[CapitalGamma]]/@QuantumPositiveRoots[\[CapitalGamma]];
iterators=Table[{t[r],0,(n-Sum[l[[k]]t[k],{k,r+1,p}])/l[[r]]},{r,p,2,-1}]~Join~{With[{t1=(n-Sum[t[k]l[[k]],{k,2,p}])/l[[1]]},{t[1],t1,t1}]};
rmatrix=Sum[If[p>1,NonCommutativeMultiply,Times]@@Table[(q^d[[i[[r]]]])^(1/2 t[r](t[r]+1)) (1-q^(-2d[[i[[r]]]]))^t[r]/qFactorial[t[r]][q^d[[i[[r]]]]] NonCommutativePower[SuperPlus[Subscript[X, \[CapitalGamma],r]],t[r]]\[CircleTimes]NonCommutativePower[SuperMinus[Subscript[X, \[CapitalGamma],r]],t[r]],{r,1,p}],Evaluate[Sequence@@iterators]];
DebugPrintHeld["Finished calculating ",PartialRMatrix[\[CapitalGamma]][n]];
rmatrix
]


RMatrixAdjunct[\[CapitalGamma]_,V1_,V2_,\[Lambda]_]:=Module[{partialWeightMultiplicities,exponents,d},
partialWeightMultiplicities=QuantumGroups`MatrixPresentations`Private`WeightMultiplicityComponents[\[CapitalGamma],V1,V2,\[Lambda]];
exponents=KillingForm[\[CapitalGamma]][\[Lambda]-#,#]&/@Weights[\[CapitalGamma],V2];
d=Flatten[Table[#[[1]],{#[[2]]}]&/@Transpose[{q^exponents,partialWeightMultiplicities}]];
Matrix[DiagonalMatrix[d]]
]


PartialRMatrixPresentation[\[CapitalGamma]_,n_,V_,W_,\[Beta]_,\[Lambda]_]:=PartialRMatrixPresentation[\[CapitalGamma],n,V,W,\[Beta],\[Lambda]]=FastMatrixPresentation[\[CapitalGamma]][PartialRMatrix[\[CapitalGamma]][n]][V\[CircleTimes]W,\[Beta],\[Lambda]]


CarefulFastMatrixPresentation[\[CapitalGamma]_][X_][V_,\[Beta]_,\[Lambda]_]:=Module[{told,tnew,rold,rnew},
{tnew,rnew}=AbsoluteTiming[FastMatrixPresentation[\[CapitalGamma]][X][V,\[Beta],\[Lambda]]];
{told,rold}=AbsoluteTiming[MatrixPresentation[\[CapitalGamma]][X][V,\[Beta],\[Lambda]]];
If[rold=!=rnew,Print["Achtung, FastMatrixPresentation failed."]];
DebugPrint["FastMatrixPresentation timing: ",{told,tnew}];
rnew
]


FastMatrixPresentation[\[CapitalGamma]_][\[ScriptOne]\[CircleTimes]\[ScriptOne]][V_\[CircleTimes]W_,\[Beta]_,\[Lambda]_]:=MatrixPresentation[\[CapitalGamma]][\[ScriptOne]\[CircleTimes]\[ScriptOne]][V\[CircleTimes]W,\[Beta],\[Lambda]]
FastMatrixPresentation[\[CapitalGamma]_][SuperPlus[Subscript[X, \[CapitalGamma]_,r_]]\[CircleTimes]SuperMinus[Subscript[X, \[CapitalGamma]_,r_]]][V_\[CircleTimes]W_,\[Beta]_,\[Lambda]_]:=MatrixPresentation[\[CapitalGamma]][SuperPlus[Subscript[X, \[CapitalGamma],r]]\[CircleTimes]SuperMinus[Subscript[X, \[CapitalGamma],r]]][V\[CircleTimes]W,\[Beta],\[Lambda]]


FastMatrixPresentation[\[CapitalGamma]_][(X:(NonCommutativeMultiply[(SuperPlus[Subscript[X, \[CapitalGamma]_,_]])..]))\[CircleTimes](Y:(NonCommutativeMultiply[(SuperMinus[Subscript[X, \[CapitalGamma]_,_]])..]))][V_\[CircleTimes]W_,\[Beta]_,\[Lambda]_]:=
Module[{result},
If[WeightMultiplicity[\[CapitalGamma],V\[CircleTimes]W,\[Lambda]+OperatorWeight[\[CapitalGamma]][X]]==0,Return[ZeroesMatrix[WeightMultiplicity[\[CapitalGamma],V\[CircleTimes]W,\[Lambda]+OperatorWeight[\[CapitalGamma]][X\[CircleTimes]Y]],WeightMultiplicity[\[CapitalGamma],V\[CircleTimes]W,\[Lambda]]]]];
If[WeightMultiplicity[\[CapitalGamma],V\[CircleTimes]W,\[Lambda]+OperatorWeight[\[CapitalGamma]][Y]]==0,Return[ZeroesMatrix[WeightMultiplicity[\[CapitalGamma],V\[CircleTimes]W,\[Lambda]+OperatorWeight[\[CapitalGamma]][X\[CircleTimes]Y]],WeightMultiplicity[\[CapitalGamma],V\[CircleTimes]W,\[Lambda]]]]];
result=Simplify[MatrixPresentation[\[CapitalGamma]][X\[CircleTimes]Y][V\[CircleTimes]W,\[Beta],\[Lambda]]];
Return[result]
]


FastMatrixPresentation[\[CapitalGamma]_][A_Plus][V_,\[Beta]_,\[Lambda]_]:=FastMatrixPresentation[\[CapitalGamma]][#][V,\[Beta],\[Lambda]]&/@A


FastMatrixPresentation[\[CapitalGamma]_][\[Alpha]_?qNumberQ A_][V_,\[Beta]_,\[Lambda]_]:=\[Alpha] FastMatrixPresentation[\[CapitalGamma]][A][V,\[Beta],\[Lambda]]


FastMatrixPresentation[\[CapitalGamma]_][X_][V_,\[Beta]_,\[Lambda]_]:=(DebugPrint["FastMatrixPresentation degrading to MatrixPresentation."];MatrixPresentation[\[CapitalGamma]][X][V,\[Beta],\[Lambda]])


RMatrix[\[CapitalGamma]_,V1_,V2_,\[Beta]_,\[Lambda]_]/;MemberQ[Weights[\[CapitalGamma],V1\[CircleTimes]V2],\[Lambda]]:=Module[{n=-1,w,m,data},
w=Weights[\[CapitalGamma],V1\[CircleTimes]V2];
m=Length[w];
data=Simplify[Inner[Dot,
Table[RMatrixAdjunct[\[CapitalGamma],V1,V2,w[[i]]],{i,1,m}],FixedPoint[(n++;#+Table[PartialRMatrixPresentation[\[CapitalGamma],n,V1,V2,\[Beta],w[[i]]],{i,1,m}])&,0],List]
];
Table[RMatrix[\[CapitalGamma],V1,V2,\[Beta],w[[i]]]=data[[i]],{i,1,m}];
RMatrix[\[CapitalGamma],V1,V2,\[Beta],\[Lambda]]
]
RMatrix[\[CapitalGamma]_,V1_,V2_,\[Beta]_,\[Lambda]_]/;!MemberQ[Weights[\[CapitalGamma],V1\[CircleTimes]V2],\[Lambda]]:=Matrix[0,0]


CheckRMatrixOppositeCommutes[\[CapitalGamma]_,Z_][V1_,V2_,\[Beta]_,\[Lambda]_]:=With[{R1=RMatrix[\[CapitalGamma],V1,V2,\[Beta],\[Lambda]],R2=RMatrix[\[CapitalGamma],V1,V2,\[Beta],\[Lambda]+OperatorWeight[\[CapitalGamma]][\[CapitalDelta][Z]]]},
ZeroMatrixQ[Simplify[MatrixPresentation[\[CapitalGamma]][\[CapitalDelta]op[Z]][V1\[CircleTimes]V2,\[Beta],\[Lambda]]-R2.MatrixPresentation[\[CapitalGamma]][\[CapitalDelta][Z]][V1\[CircleTimes]V2,\[Beta],\[Lambda]].Inverse[R1]]
]
]


CheckRMatrixOppositeCommutes[\[CapitalGamma]_][V1_,V2_,\[Beta]_,\[Lambda]_]:=And@@(CheckRMatrixOppositeCommutes[\[CapitalGamma],#][V1,V2,\[Beta],\[Lambda]]&/@PositiveGenerators[\[CapitalGamma]])


CheckRMatrixOppositeCommutes[\[CapitalGamma]_][V1_,V2_,\[Beta]_]:=And@@(CheckRMatrixOppositeCommutes[\[CapitalGamma]][V1,V2,\[Beta],#]&/@Weights[\[CapitalGamma],V1\[CircleTimes]V2])


End[];


EndPackage[];
