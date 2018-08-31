(* ::Package:: *)

(* Mathematica Package *)

(* original by Tudor Jabelean: search Mma-Prolog_Tudor _ 15-Jan-2011.nb *)

BeginPackage["JacquardProlog`"]
(* Exported symbols added here with SymbolName::usage *)  

OccursIn::usage = "QccursIn[elem, expr]: Element occurs in expression.";
Substituted::usage = "Substituted[expr, substitution]: Apply substitution to the expression.";
SubstitutedTerms::usage = "SubstitutedTerms[{var[v] -> term, ...}, substitution]: Apply the substitution to all terms in the list of replacements and drop identity replacements.";
DroppedVars::usage = "DroppedVars[replacements1, replacements2]: Drop from replacements2 every replacement whose variable is also in replacements1.";
Composed::usage = "Composed[subst1, subst2]: Produces the composition of two substitutions.";
MGU::usage = "MGU[expr1, expr2]: Produces the most general unifier of the two expressions as a list of replacement rules.";
FreshVars::usage = "FreshVars[expr]: Replace vars in expr with fresh vars";
FreshRules::usage = "FreshRules[expr]: Scan expr for vars and make replacement rules for them.";
VarToFreshRule::usage = "VarToFreshRule[var[v]]: Produce a rule to replace var[v] with a fresh symbol";
SelectedRules::usage = "SelectedRules[rules, vars]: Filter the rules, keeping only those that contain some var in vars.";
VarsOfSubst::usage = "VarsOfSubst[subst]: Produce a list of vars in the subst (list of replacement rules)";
Vars::usage = "Vars[expr]: Produce the dedup'ed set of vars in the expr";
Transition::usage = "Transition[st[{questions, ...}, substs, vars, depth], rl[{premises, ...}, goal]: Produces a single rule transition recorded in the state st[...].";
MultiMatch::usage = "MultiMatch[st[{questions, ...}, substs, vars, depth], knowledgeBase], Find all matches between the questions and the rules in the given knowledgebase and apply the given state transition.";
MultiTransition::usage = "MultiTransition[st[{questions, ...}, substs, vars, depth], knowledgeBase, maxDepth:5], Find all matches between the questions and the rules in the given knowledgebase with a search depth less than the given maximum and apply the given state transition.";	
Query::usage = "TODO";
StateQuery::usage = "TODO";
Solutions::usage = "Solutions[question, KB, maxDepth:5]";

var::usage = "var: head for prolog variables.";
st::usage  = "st: head for prolog states.";
rl::usage  = "rl: head for prolog rules and facts.";
$Fail::usage = "Prolog-specific unique failure value.";

testtest::usage = "Temporary.";

Begin["`Private`"] (* Begin Private Context *) 



(* ::Section:: *)
(*OccursIn*)


OccursIn[elem_, expr_] := ({} =!= Cases[{expr}, elem, Infinity]);


(* ::Section:: *)
(*Substituted*)


Substituted[expr_, subst_] := expr /. subst;


(* ::Section:: *)
(*SubstitutedTerms*)


SubstitutedTerms[{}, subst_] = {};
SubstitutedTerms[{var[v_] -> term_, rest___}, subst_] :=
    Module[{firstTerm, restTerms},
      firstTerm = Substituted[term, subst];
      restTerms = SubstitutedTerms[{rest}, subst];
      If[ var[v] === firstTerm,
          restTerms,
          Prepend[restTerms, var[v] -> firstTerm]
      ]
    ]


(* ::Section:: *)
(*DroppedVars*)


DroppedVars[{}, subst_] := subst;
DroppedVars[
  {var[v_] -> term1_, repls1rest___},
  {repls2pre___, var[v_] -> term2_, repls2post___} ] :=
    DroppedVars[{repls1rest}, {repls2pre, repls2post}];
DroppedVars[
  {var[v_] -> term1_, repls1rest___}, repls2_ ] :=
    DroppedVars[{repls1rest}, repls2];


(* ::Section:: *)
(*Composed*)


(* ::Text:: *)
(*To compose two substitutions Subscript[\[Sigma], 1] and Subscript[\[Sigma], 2]:*)


(* ::Text:: *)
(*Let Subscript[\[Sigma], 1]={Subscript[x, 1]:>Subscript[T, 1],Subscript[x, 2]:>Subscript[T, 2],...,Subscript[x, m]:>Subscript[T, m]} , Subscript[\[Sigma], 2]={Subscript[y, 1]:>Subscript[S, 1],Subscript[y, 2]:>Subscript[S, 2],...,Subscript[y, n]:>Subscript[S, n]} *)


(* ::Text:: *)
(*Compute Subscript[\[Sigma], 1]\[SmallCircle]Subscript[\[Sigma], 2]. *)


(* ::Text:: *)
(*Apply Subscript[\[Sigma], 2] to every term Subscript[T, i] in Subscript[\[Sigma], 1] and delete every identity replacement (optimization), obtaining substitution Subscript[\[Theta], 1]. *)


(* ::Text:: *)
(*Drop from Subscript[\[Sigma], 2] every replacement Subscript[y, j]:>Subscript[S, j] whose variable Subscript[y, j] is a variable in Subscript[\[Sigma], 1], obtaining Subscript[\[Theta], 2].*)


(* ::Text:: *)
(*Result is Subscript[\[Theta], 1]\[Union]Subscript[\[Theta], 2]*)


Composed[subst1_, subst2_] :=
    Join[
      SubstitutedTerms[subst1, subst2], 
      DroppedVars[subst1, subst2] ];


(* ::Section:: *)
(*MGU: Most General Unifier*)


MGU[expr_, expr_] := {};

MGU[var[v_], expr_] :=
  If[OccursIn[v, expr], $Fail,
    {var[v] -> expr}];
	
MGU[expr_, var[v_]] := MGU[var[v], expr];

MGU[f_[expr1_, exprSeq1___], f_[expr2_, exprSeq2___]] :=
  Module[{subst, newSubst},
    subst = MGU[expr1, expr2]; 
	(* Print["MGU subst: ",subst]; *)
	If[subst === $Fail, $Fail,
	  newSubst = MGU[
	    Substituted[f[exprSeq1], subst], 
		Substituted[f[exprSeq2], subst]];
    If[newSubst === $Fail, $Fail,
      Composed[subst, newSubst]]] ];

MGU[expr1_, expr2_] := $Fail;
MGU[exprSeq___] := (Print["Wrong args to MGU: ", {exprSeq}]; Abort[]);


(* ::Section:: *)
(*FreshVars*)


FreshVars[expr_] := expr /. FreshRules[expr];


(* ::Section:: *)
(*FreshRules*)


FreshRules[expr_] := Map[VarToFreshRule, Vars[expr]];


(* ::Section:: *)
(*VarToFreshRule*)


VarToFreshRule[var[v_]] := (var[v] -> var[Unique[v]]);


(* ::Section:: *)
(*SelectedRules*)


SelectedRules[rules_, vars_] :=
  Cases[rules, (var[v_] -> _) /; OccursIn[var[v], vars]];


(* ::Section:: *)
(*VarsOfSubst*)


VarsOfSubst[substs_] := Cases[substs, (var[v_] -> _) -> var[v]];


(* ::Section:: *)
(*Vars*)


Vars[expr_] := Union[Cases[{expr}, var[v_], Infinity]];


(* ::Section:: *)
(*Transition*)


Transition[
    st[{quest_, quests___}, subst_, vars_, depth_],
    rl[{premises___}, goal_]] :=
  Module[{newSubst = MGU[quest, goal]},
    st[
	  Substituted[{premises, quests}, newSubst],
	  (* Composed[subst, newSubst] *)
	  SelectedRules[Composed[subst, newSubst], vars],
	  (* SelectedRules[Composed[subst, newSubst], Union[vars, VarsOfSubst[newSubst]]] *)
	  vars, 
	  depth + 1
	]
  ];


(* ::Section:: *)
(*MultiMatch*)


MultiMatch[st[{quest_, quests___}, subst_, vars_, depth_], kb_] :=
  Cases[Map[FreshVars, kb],
   rl[{premises___}, goal_] /; ($Fail =!= MGU[quest, goal]) :>
    Transition[
     st[{quest, quests}, subst, vars, depth],
     rl[{premises}, goal]]];


(* ::Section:: *)
(*Multitransition*)


MultiTransition[st[{}, args___], kb_, maxDepth_:5] := st[{}, args];
MultiTransition[st[args__, depth_], kb_, maxDepth_:5] /; (depth > maxDepth) := st[args, depth];
MultiTransition[state_, kb_, maxDepth_:5] := MultiMatch[state, kb]


(* ::Section:: *)
(*Query*)


Query[question_, kb_, maxDepth_:5] := StateQuery[st[{question}, {}, Vars[question], 0], kb, maxDepth]


(* ::Section:: *)
(*StateQuery*)


StateQuery[state_, kb_, maxDepth_:5] := state //. {st[args__] :> MultiTransition[st[args], kb, maxDepth]}


(* ::Section:: *)
(*Solutions*)


Solutions[And[questions__],kb_,maxDepth_:5] :=
  (Print[questions];
   Print[Solutions[#,kb,maxDepth]&/@questions];
    Intersection @@
    Solutions[#, kb, maxDepth]&/@
    questions)


Solutions[question_, kb_, maxDepth_:5] := Union[
  Cases[
    Query[question, kb, maxDepth], 
    st[{}, subst_, __] -> subst,
    Infinity]]


(* ::Section:: *)
(*Auxilliary Definitions*)


testtest[x_] := x + 2;


(* ::Section:: *)
(*The End*)


End[] (* End Private Context *)


EndPackage[]
