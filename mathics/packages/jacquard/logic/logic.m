(* ::Package:: *)

(* :Context: logic` *)

(* :Title: Logic Programming *)

(* :Author: Lic. Ren\[AAcute]n Cabrera *)

(* :Summary: This is package for Logic Programming 
    Tested under Mathematica 3.0 
*)


(* :Package Version: 1.0       December 2000 *)

(*  Mathematica is a registered trademark of Wolfram Research, Inc.   *)

BeginPackage[ "logic`" ]


Question::usage     = "Question[ Statements, Query, Var]" 
AddKnowledge::usage = "AddKnowledge[ Statements, Rule ]" 


Begin["`Private`"]


(* ::Text:: *)
(*Question first makes a fresh variable in the Module expression. The superficial name of the fresh variable is Answer, but, internally, Mathematica makes session-fresh name of the form $479 that is guaranteed not to collide with any other variable in this session. *)


(* ::Text:: *)
(*The first argument of the Cases expression is a ReplaceRepeated expression (the original was "ReplaceAll," but it did not yield sufficient results). It searches the given knowledge base for all terms matching the pattern and rewrites them to Answer[X], where X is the declared variable provided by the user in the third parameter position of the Question expression. This is the input list that Cases will search. Cases then searches the resulting list for terms matching Answer[_], that is, Answer[anything], and rewrites the Answers to Sequences, stripping off the placeholder name Answer, so that the final results just appear in list form.*)


Question[Knowledge_, Patt_ , X_ ] :=
  Module[ {Answer}, 
    Cases[Knowledge //. Patt -> Answer[X],
	  Answer[_]] /. Answer -> Sequence ];


(* ::Text:: *)
(*The second form of Question just yields "True" or "False." Cabrera's original (commented out) was not able to recognize questions of the form Question[Knowledge, Statements[ . . . ]], but the following will do.*)


Question[Knowledge_, Patt_] :=
  Question[Knowledge, Patt, Unique[]] =!= {};
  (*Cases[Knowledge, Patt ] =!= {};*)


(* ::Text:: *)
(*This implementation of AddKnowledge just brute-forces all implications of the rules into the factbase. This is not scaleable. UNDONE: fix*)


AddKnowledge[K_, R_]:= Union[K, K //. R];


End[]


EndPackage[]
