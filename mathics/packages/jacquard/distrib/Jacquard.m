(* ::Package:: *)

(* ::Title:: *)
(*Jacquard and JaqSON*)


(* ::Text:: *)
(*Serialized expressions for remote evaluation.*)


(* ::Subsubtitle:: *)
(*Brian Beckman*)
(*Februrary 2012*)
(*with ideas from Avi Bar-Zeev, Erik Meijer, and Savas Parastiditis*)


(* ::Subsubtitle::RGBColor[0, 0, 1]:: *)
(*Copyright (c) 2012, Microsoft Corporation*)
(**)
(*   Licensed under the Apache License, Version 2.0 (the "License");*)
(*   you may not use this file except in compliance with the License.*)
(*   You may obtain a copy of the License at*)
(**)
(*       http://www.apache.org/licenses/LICENSE-2.0*)
(**)
(*   Unless required by applicable law or agreed to in writing, software*)
(*   distributed under the License is distributed on an "AS IS" BASIS,*)
(*   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.*)
(*   See the License for the specific language governing permissions and*)
(*   limitations under the License.*)


(* ::Section:: *)
(*Public Interface*)


(* Mathematica Package *)

(* Created by the Wolfram Workbench Mar 8, 2012 *)

(*BeginPackage["Jacquard`", {"JLink`", "MiniData`"}]*)
BeginPackage["Jacquard`", {"JLink`"}]
(* Exported symbols added here with SymbolName::usage *) 

gridExpression::usage = "Displays expressions in a hierarchical grid.";
gridCaptive::usage = "Displays captive expressions in a hierarchical grid.";
gridCaptive2::usage = "Displays captive expressions in a hierarchical grid.";
gridRules::usage = "Displays Lists of Rules and Lists of Lists of Rules in a hierarchical grid.";
traceView2::usage = "Presents Cascading OpenerViews of an expression trace.";
traceView4::usage = "Presents interactive, button-driven view of an expression-evaluation trace.";
traceView5::usage = "Presents an abbreviated button-driven view of an expression-evaluation trace.";

SymbolQ::usage = "Tests whether its argument is a Symbol";

KvpQ::usage = "Tests whether its argument is a KVPair or KVP, that is, a list of two values, the first of which is a string or symbol Key.";
KvpsQ::usage = "Tests whether its argument is an association list: that is, a list of key-value pairs where no key appears more than once.";

RulesQ::usage = "Tests whether its argument is a list of Rules, which is the Import/Export preferred representation for key-value pairs (the analog to 'lookup' for rules is just 'Replace.')";
RulessQ::usage = "Tests whether its argument is a list of list of Rules, which is the preferred representation for argument lists.";

Args::usage = "Produces all the arguments of a composite expression; counterpoint to built-in Head.";

rulesFromExpression::usage = "Documentation TODO";
rulesFromHeldExpression::usage = "Documentation TODO";
rulesFromEvaluatableHeldExpression::usage = "Documentation TODO";
expressionFromRules::usage = "Documentation TODO";
stringifiedExpressionFromRules::usage = "Documentation TODO";

jsonStringFromExpression::usage = "jsonStringFromExpression[expr]";
jsonStringFromHeldExpression::usage = "jsonStringFromHeldExpression[expr]";
expressionFromJsonString::usage = "expressionFromJsonString[str]";
stringifiedExpressionFromJsonString::usage = "stringifiedExpressionFromJsonString[str]";

jacquardPostRules::usage = "jacquardPostRules[rules, importer, url]";
jacquardPostExpression::usage = "jacquardPostRules[rules, importer, url]";
jacquardEvalExpression::usage = "jacquardPostExpression[expr, importer, url]";
jacquardEvalHeldExpression::usage = "jacquardEvalHeldExpression[expr, importer, url]";
jacquardEvalEvaluatableHeldExpression::usage = "jacquardEvalHeldExpression[expr, importer, url]";
jacquardStringifiedEvalHeldExpression::usage = "jacquardStringifiedEvalHeldExpression[expr, importer, url]";

(* During Development, keep these symbols public.  Pick which ones
   to make permanently public later. *)
insertCommaSeparators::usage = "Temporary.";
number2::usage = "Temporary.";
string2::usage = "Temporary.";
symbol2::usage = "Temporary.";
dpyCaptive2::usage = "Temporary.";
prefixes::usage = "Temporary.";
postfixes::usage = "Temporary.";
splits::usage = "Temporary.";
flat1::usage = "Temporary.";
Pluck::usage = "Pluck[n] is a Function that produces the n-th Part of its input.";
SelectMany::usage = "SelectMany[list, listFromElement]";
Zip::usage = "Zip[list1, list2, itemFromPair]";
ZipMany::usage = "ZipMany[list1, list2, listFromPair]";
captive::usage = "Temporary.";
captive2::usage = "Temporary.";
RealQ::usage = "Temporary.";
ComplexQ::usage = "Temporary.";
RationalQ::usage = "Temporary.";
numericSubtype::usage = "Temporary.";
pairsQ::usage = "Temporary.";
keys::usage = "Temporary.";
values::usage = "Temporary.";


Begin["`Private`"]
(* Implementation of the package *)


(* ::Section:: *)
(*Visualization and Debugging Definitions*)


(* ::Subsection:: *)
(*gridRules[ rules ]*)


(* ::Text:: *)
(*Display rules generated from expressions and roundtripped through JSON. Update to handle Rule-Delayed.*)


ClearAll[gridRules,fopts];
ClearAll[allAreRulesQ,noneAreAtomsQ,noneAreAtomsOrListsQ];

(* A "HoldAll" version of the following, with "Unevaluated" everywhere, may be required. *)
noneAreAtomsQ[candidates_List]:=And@@(Not[AtomQ@#]&/@candidates);
noneAreAtomsOrListsQ[candidates_List]:=And@@((Not[AtomQ@#||ListQ@#])&/@candidates)
allAreRulesQ[candidates_List]:=And@@(With[{h=Head@#},h===Rule||h===RuleDelayed]&/@candidates);
fopts[fcolor_,bcolor_]:=Sequence[Frame -> All,Alignment -> Left,FrameStyle -> fcolor,
Background -> {{LightOrange, {bcolor}}}]

gridRules[Rule[key_, val_]]:=
Grid[{{Style[gridRules@key,Bold,Black],Style[gridRules @ val,Bold, Blue]}},
fopts[Blue,LightYellow]];

gridRules[RuleDelayed[key_, val_]]:=
Grid[{{Style[gridRules@key,Bold,Black],Style[gridRules @ val,Bold, Red]}},
fopts[Red,LightYellow]];

(* Put lists of non-atoms vertically and joinGrids them up, removing one level of nesting. *)
gridRules[exprs_List?noneAreAtomsOrListsQ]:=
Grid[(joinGrids@Transpose@{gridRules /@ exprs}),
Frame->All,FrameStyle->Darker[Green],Alignment->Left,Background->LightGreen];
(* Put other lists vertically but DON'T joinGrids them up. *)

gridRules[exprs_List]:=
Grid[(Transpose@{gridRules /@ exprs}),
Frame->All,FrameStyle->Darker[Green],Alignment->Left,Background->LightGreen];

gridRules[h_[parts___]]:=
Grid[{{Style[gridRules@h,Bold, Darker[Purple]]}~Join~(gridRules /@ {parts})},
fopts[Black,LightGreen]];

gridRules[any_?AtomQ]:=Style[Grid[{{any}},Alignment->Left],Bold];

gridRules[any_]:=Grid[{{any}},Alignment->Left];

ClearAll[joinGrids];
(* Arbitrarily choose the first of multiple option tails z1, z2, ... . *)
joinGrids[{{Grid[{xs__},z1___]},{Grid[{ys__},z2___]}}]:={{Grid[{xs,ys},z1]}};
joinGrids[{{Grid[{xs__},z1___]},{Grid[{ys__},z2___]}, k__}]:=joinGrids[{{Grid[{xs,ys},z1]},k}];
joinGrids[{{Grid[xs___]}}]:={{Grid[xs]}};

joinGrids[{{Style[Grid[{xs__},z1___],sty1___]},{Style[Grid[{ys__},z2___],sty2___]}}]:={{Style[Grid[{xs,ys},z1],sty1]}};
joinGrids[{{Style[Grid[{xs__},z1___],sty1___]},{Style[Grid[{ys__},z2___],sty2___]}, k__}]:=joinGrids[{{Style[Grid[{xs,ys},z1],sty1]},k}];

joinGrids[{{Style[Grid[{xs__},z1___],sty1___]},{Grid[{ys__},z2___]}}]:={{Style[Grid[{xs,ys},z1],sty1]}};
joinGrids[{{Style[Grid[{xs__},z1___],sty1___]},{Grid[{ys__},z2___]}, k__}]:=joinGrids[{{Style[Grid[{xs,ys},z1],sty1]},k}];

joinGrids[{{Grid[{xs__},z1___]},{Style[Grid[{ys__},z2___],sty2___]}}]:={{Style[Grid[{xs,ys},z1],sty2]}};
joinGrids[{{Grid[{xs__},z1___]},{Style[Grid[{ys__},z2___],sty2___]}, k__}]:=joinGrids[{{Style[Grid[{xs,ys},z1],sty2]},k}];

joinGrids[{{Style[Grid[xs___],sty___]}}]:={{Style[Grid[xs],sty]}};

joinGrids[{}]:={};


(* ::Subsection::Closed:: *)
(*Private Display (dpy) Definitions*)


dpyNullary[ex_] :=
  Grid[{{ex, ""}},
   Frame      -> {All,False},
   Alignment  -> Left,
   Background -> {{LightOrange,{LightYellow}}}];


dpyMultiary[key_, vals_] :=
   With[{c = Length @ vals},
    Module[{
      spans = Table["", {c}],
      slot  = Floor[(1+c)/2]},
     spans[[slot]] = key;
     Grid[MapThread[List,{spans, vals}],
       Frame      -> {All,False},
       Alignment  -> Left,
       Background -> {{LightOrange,{LightGreen}}}]
      ]];


dpyAtom[ex_] := 
  Grid[{{Style[ex,Bold]}},
   Frame      -> All,
   Alignment  -> Left,
   Background ->
     Switch[Head @ ex,
       String,   LightYellow,
       Symbol,   LightPurple,
       Integer,  LightBlue,
       Real,     LightBlue,
       Rational, LightBlue,
       Complex,  LightBlue,
       _,        Red]];


dpyQuotedAtom[number2[ex_]] := 
  Grid[{{Style[ex,Bold,Blue]}},
   Frame      -> All,
   Alignment  -> Left,
   Background -> LightBlue];


dpyQuotedAtom[string2[ex_]] := 
  Grid[{{Style[ex,Bold,Green]}},
   Frame      -> All,
   Alignment  -> Left,
   Background -> LightYellow];


dpyQuotedAtom[symbol2[ex_]] := 
  Grid[{{Style[ex,Bold,Purple]}},
   Frame      -> All,
   Alignment  -> Left,
   Background -> LightPurple];


dpyEmpty[] :=
  Grid[{{}},
   Frame      -> All,
   Alignment  -> Left,
   Background -> White];


(* ::Subsection:: *)
(*gridExpression[ expr ]*)


(* ::Text:: *)
(*Display a gridded representation of an expression.*)


gridExpression[ex_[]]        := dpyNullary[gridExpression @ ex];
gridExpression[ex_[parts__]] := dpyMultiary[gridExpression @ ex, gridExpression /@ {parts}];
gridExpression[ex_?AtomQ]    := dpyAtom[ex];
gridExpression[x___]         := Throw[{x}];


(* ::Subsection:: *)
(*gridCaptive[ expr ]*)


(* ::Text:: *)
(*Display a gridded representation of a captive (quoted) expression.*)


SetAttributes[gridCaptive, HoldAllComplete];

dpyCaptive[{ex_}]      := dpyNullary[dpyCaptive @ ex];
dpyCaptive[{a_, as__}] := dpyMultiary[dpyCaptive @ a, dpyCaptive /@ {as}];
dpyCaptive[ex_?AtomQ]  := dpyAtom[ex];
dpyCaptive[x___]       := Throw[Unevaluated @ {x}];

gridCaptive[expr_]:= dpyCaptive @ captive @ expr;


(* ::Subsection:: *)
(*captive2*)


(* ::Text:: *)
(*TODO: deprecate the old captive*)


SetAttributes[captive2, HoldAllComplete];
captive2[expr_ /; NumberQ @ Unevaluated @ expr]           := number2[ToString[expr]]
captive2[expr_ /; (Head @ Unevaluated @ expr === Symbol)] := symbol2[ToString @ Unevaluated @ expr]
captive2[expr_ /; (Head @ Unevaluated @ expr === String)] := string2["\"" <> expr <> "\""]
captive2[head_[parts___]] := {captive2 @ head, captive2 /@ Unevaluated @ {parts}}
captive2[x___]           := Throw @ {x};


(* ::Subsection:: *)
(*gridCaptive2[ expr ]*)


SetAttributes[gridCaptive2, HoldAllComplete];

dpyCaptive2[{symbol2["List"], {}}] := dpyEmpty[];
dpyCaptive2[{ex_, {}}]             := dpyNullary[dpyCaptive2 @ ex];
dpyCaptive2[{a_, {as__}}]          := dpyMultiary[dpyCaptive2 @ a, dpyCaptive2 /@ {as}];
dpyCaptive2[number2[ex_]]          := dpyQuotedAtom[number2[ex]];
dpyCaptive2[string2[ex_]]          := dpyQuotedAtom[string2[ex]];
dpyCaptive2[symbol2[ex_]]          := dpyQuotedAtom[symbol2[ex]];
dpyCaptive2[x___]                  := Throw[Unevaluated @ {x}];

gridCaptive2[expr_] := dpyCaptive2 @ captive2 @ expr;


(* ::Subsection::Closed:: *)
(*traceView2[ expr ]*)


(* ::Text:: *)
(*Display an interactive visualization of the trace of the evaluation of an expression. *)


traceView2[expr_]:=Module[
  {steps={},stack={},pre,post,show,dynamic},
  pre[e_]:=(stack={steps,stack};steps={});
  post[e_,r_]:=(steps=First@stack~Join~{show[e,HoldForm[r],steps]};stack=stack[[2]]);
  SetAttributes[post,HoldAllComplete];
  show[e_,r_,steps_]:=Grid[
    steps/.{
      {}->{{"Expr  ",Row[{e," ",Style["inert",{Italic,Small}]}]}},
      _->{
        {"Expr  ",e},
        {"Steps",steps/.{
          {}->Style["no definitions apply",Italic],
          _:>OpenerView[{Length@steps,dynamic@Column[steps]}]}},
        {"Result",r}}},
      Alignment->Left,
      Frame->All,
      Background->{{LightCyan},None}];
    TraceScan[pre,expr,___,post];
    Deploy@Pane[steps[[1]]/.dynamic->Dynamic,ImageSize->10000]]
SetAttributes[traceView2,{HoldAllComplete}]


(* ::Subsection::Closed:: *)
(*traceView4[ expr ]*)


(* ::Text:: *)
(*Display a compact interactive visualization of the trace of the evaluation of an expression.*)


traceView4[expr_]:=Module[
  {steps={},stack={},pre,post},
  pre[e_]:=(stack={steps,stack};steps={});
  post[e_,r_]:=(steps=First@stack~Join~{{e,steps,HoldForm[r]}};stack=stack[[2]]);
  SetAttributes[post,HoldAllComplete];
  TraceScan[pre,expr,___,post];
  DynamicModule[
    {focus,show,substep,enter,exit},
    focus=steps;
    substep[{e_,{},_},_]:={Null,e,Style["inert",{Italic,Small}]};
    substep[{e_,_,r_},p_]:={
      Button[Style["show",Small],enter[p]],
      e,
      Style[Row[{"-> ",r}],Small]};
    enter[{p_}]:=PrependTo[focus,focus[[1,2,p]]];
    exit[]:=focus=Drop[focus,1];
    show[{e_,s_,r_}]:=Column[{
      Grid[{
        {"Expression",Column@Reverse@focus[[All,1]]},
        {Column[{
          "Steps",
          focus/.{
            {_}:>Sequence[],
            _:>Button["Back",exit[],ImageSize->Automatic]}}],
         Grid[MapIndexed[substep,s],Alignment->Left]},
		{"Result",Column@focus[[All,3]]}},
       Alignment->Left,
       Frame->All,
       Background->{{LightCyan}}]
     }];
   Dynamic@show@focus[[1]]
  ]]
SetAttributes[traceView4,{HoldAllComplete}]


(* ::Subsection::Closed:: *)
(*traceView5[ expr ]*)


(* ::Text:: *)
(*Display an alternative compact interactive visualization of the trace of the evaluation of an expression.*)


SetAttributes[traceView5,HoldFirst]

traceView5[e_,s___,opts:OptionsPattern[Trace]]:=
  Module[{show2},
  show2[{expr_,steps__}]:=OpenerView[{expr,Column[show2/@{steps}]}];
  show2[{HoldForm[x_]}]:=Row[{Opener[True,Enabled->False],HoldForm[x]}];
  show2[HoldForm[x_]]:=HoldForm[x];
  show2[Trace[Unevaluated[e],s,opts,TraceOriginal->True]]]


(* ::Section:: *)
(*Predicates and Helpers*)


(* ::Subsection:: *)
(*KvpQ[ list ]*)


(* ::Text:: *)
(*Something is a Kvp (key-value pair) if it matches the pattern of a list of two items, the first of which is either a string or a symbol.*)


KvpQ[kvp_List] := MatchQ[kvp, {_Symbol, _} | {_String, _}];
KvpQ[___] = False;


(* ::Subsection:: *)
(*pairsQ[ list ]*)


(* ::Text:: *)
(*Something is a list of pairs if it is a list and every element is a pair.*)


pairsQ[list_List] := And @@ (KvpQ /@ list);
pairsQ[___] = False;


(* ::Text:: *)
(*To fetch the keys from a list, map a function that takes the first part of every element over the list. Here, we don't need the presumably expensive check of ?pairsQ on the argument, just the cheap check of List, since this is an internal function.*)


(* ::Subsection:: *)
(*keys[ list ]*)


keys[list_List] := #[[1]] & /@ list;


keysUniqueQ[list_List] := Length@Union@keys@list === Length@keys@list;
keysUniqueQ[___] = False;


(* ::Text:: *)
(*To fetch the values, map a function that picks the second element from each internal kvp.*)


(* ::Subsection:: *)
(*values[ list ]*)


values[list_List] := #[[2]] & /@ list


(* ::Subsection:: *)
(*KvpsQ[ list ]*)


KvpsQ[list_?pairsQ] := keysUniqueQ@list;
KvpsQ[___] = False;


(* ::Subsection:: *)
(*SymbolQ[ symbol ]*)


SymbolQ[x_Symbol] = True;
SymbolQ[___] = False;


(* ::Subsection:: *)
(*RulesQ[ list ]*)


RulesQ[vs_List] := And @@ (MatchQ[#, Rule[_, _]] & /@ vs);
RulesQ[___] = False;


(* ::Subsection:: *)
(*RulessQ[ lists ]*)


RulessQ[vss_List] := And @@ (RulesQ /@ vss);
RulessQ[___] = False;


(* ::Subsection:: *)
(*Args[ list ]*)


Args[e_] := Level[e, 1];


(* ::Section:: *)
(*LinqSim*)


(* ::Text:: *)
(*TODO: generalize these to arbitrary heads, not just to lists*)


(* ::Subsection:: *)
(*Pluck[ n ]*)


Pluck[n_] := Function[item, item[[n]]];


(* ::Subsection:: *)
(*flat1[ list ]*)


flat1 = Flatten[#, 1] &;


(* ::Subsection:: *)
(*SelectMany[ lists, func ]*)


SelectMany[list_, func_] := Map[func, list] // flat1;


(* ::Subsection:: *)
(*Zip[ list1, list2, itemFromPair ]*)


Zip[list1_List, list2_List, func_:List]:=
With[{len = Min[Length @ list1, Length @ list2]},
  MapThread[func, Take[#,len]& /@ {list1, list2}]]


(* ::Subsection:: *)
(*ZipMany[list1, list2, listFromPair]*)


ZipMany = Composition[flat1, Zip]


(* ::Subsection:: *)
(*prefixes[ hd [ list___ ] ]*)


(*prefixes[acc_, {}] := acc;
prefixes[acc_, {f_, r___}] :=
  Join[acc,
    prefixes[Map[Append[#, f]&, acc], {r}]];
prefixes[list_List] := prefixes[{{}}, list];*)


prefixes[hd_[list___]] := 
  Map[Function[sub, hd @@ Take[{list}, sub]],
  Range[0, Length[{list}]]];


(* ::Subsection:: *)
(*postfixes[ hd [ list___ ] ]*)


(*postfixes[acc_, {}] := acc;
postfixes[acc_, {h___, l_}] :=
  Join[acc,
    postfixes[Map[Prepend[#,l]&, acc], {h}]];
postfixes[list_List] := Reverse @ postfixes[{{}}, list];*)


postfixes[hd_[list___]] :=
  With[{len = Length[{list}]},
    Map[Function[sub, hd @@ Take[{list}, -(len-sub)]],
    Range[0, len]]];


(* ::Subsection:: *)
(*splits[ list ]*)


(* ::Text:: *)
(*TODO: a "cute" implementation is ReplaceList[list_List, {xs___,ys___}->{{xs},{ys}}]. Assess it for perf*)


splits[list_List] := Zip[prefixes @ list, postfixes @ list]


(* ::Subsection:: *)
(*permutations[ list ]*)


(* ::Text:: *)
(*TODO: make this work for arbitrary heads.*)


(* ::Text:: *)
(*TODO: Consider this variation from the Haskell prelude:*)
(**)
(*-- | The 'permutations' function returns the list of all permutations of the argument.*)
(*--*)
(*-- > permutations "abc" == ["abc","bac","cba","bca","cab","acb"]*)
(*permutations            :: [a] -> [[a]]*)
(*permutations xs0        =  xs0 : perms xs0 []*)
(*  where*)
(*    perms []     _  = []*)
(*    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)*)
(*      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs*)
(*            interleave' _ []     r = (ts, r)*)
(*            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r*)
(*                                     in  (y:us, f (t:y:us) : zs)*)


(* ::Text:: *)
(*NOTE: this is for ILLUSTRATION only; the built-in Mathematica Permutations is MUCH more efficient.*)


permutations[{}] := {{}};
permutations[{args__}]:=
(* Flatten-1 of Map is SelectMany *)
 Flatten[Map[Function[pos,
 With[{
   elt={args}[[pos]],
   rest=Delete[{args}, pos]},
  Map[
   Function[perm, Prepend[perm, elt]],
   permutations[rest]]]],
  Range[Length[{args}]]],
 1]


(* ::Section:: *)
(*Quotes and Unquotes*)


(* ::Subsection:: *)
(*Numerical Subtypes*)


(* ::Subsubsection::Closed:: *)
(*RealQ[ x ]*)


RealQ[x_Real] := True;
RealQ[___] := False;


(* ::Subsubsection::Closed:: *)
(*RationalQ[ x ]*)


RationalQ[x_Rational] := True;
RationalQ[___] := False;


(* ::Subsubsection::Closed:: *)
(*ComplexQ[ x ]*)


ComplexQ[x_Complex] := True;
ComplexQ[___] := False;


(* ::Subsubsection::Closed:: *)
(*numericSubtype[ x ]*)


numericSubtype[x_?NumberQ] :=
	Switch[x,
		_Integer,  "Integer",
		_Real,     "Real",
		_Rational, "Rational",
		_Complex,  "Complex"]


(* ::Subsection::Closed:: *)
(*rulesFromExpression[ expr ]*)


rulesFromExpression[expr_?NumberQ] := {
	"number" -> {  "subtype" -> numericSubtype @ expr,
                   (* InputForm reliable for roundtripping Rational *)
                   "value" -> ToString @ InputForm @ expr } }
rulesFromExpression[expr_Symbol] := {"symbol" -> ToString @ expr}
rulesFromExpression[expr_String] := {"string" -> ToString @ expr}
rulesFromExpression[head_[parts___]] := {
	"head"   -> rulesFromExpression  @ head, 
	"parts"  -> rulesFromExpression /@ {parts}}

rulesFromExpression[x___] := Throw[{x}];


(* ::Subsection::Closed:: *)
(*jsonStringFromExpression[ expr ]*)


jsonStringFromExpression[expr_] :=
  ExportString[rulesFromExpression[expr], "JSON"] // 
    StringReplace[#,Whitespace->""]&;


(* ::Subsection::Closed:: *)
(*expressionFromRules[ expr ]*)


expressionFromRules[{___, "symbol" -> sym_, ___}] := ToExpression @ sym;
expressionFromRules[{___, "string" -> str_, ___}] := ToExpression @ str;
expressionFromRules[{___, "number" -> val_, ___}] := ToExpression @ ("value" /. val);
expressionFromRules[rules_] := 
	If[MatchQ[rules,{___,"parts"->_,___}],
      Apply[
        expressionFromRules  @ ("head" /. rules),
        expressionFromRules /@ ("parts" /. rules)],
      Throw[rules]];

expressionFromRules[x___] := Throw[{x}];


(* ::Subsection::Closed:: *)
(*stringifiedExpressionFromRules[ expr ]*)


insertCommaSeparators[ss_List] := StringJoin @@ Riffle[ss, ", "];
(* Module[{result = ""},
		MapIndexed[
			Function[{string, part},
				result = If[part[[1]] > 1,
					result <> ", " <> string,
					result <> string]],
			ss];
		result]; *)

insertCommaSeparators[x___] := Throw[{x}];


stringifiedExpressionFromRules[{___, "symbol" -> sym_, ___}] := sym;
stringifiedExpressionFromRules[{___, "string" -> str_, ___}] := "\"" <> str <> "\"";
stringifiedExpressionFromRules[{___, "number" -> val_, ___}] := 
	Switch[("subtype" /. val),
		"Integer", ("value" /. val),
		"Real",    ("value" /. val) <> "`",
		"Complex", 
			Quiet @ StringReplace[("value" /. val),
				(p:NumberString) ~~ " + " ~~ (q:NumberString) ~~ "*I" ->
				"Complex[" <> p <> ", " <> q <> "]" ] //
			Quiet @ StringReplace[#,
				(p:DigitCharacter...) ~~ "." ~~ (q:DigitCharacter..) -> 
				p <> "." <> q <> "`"] &,
		_,         Throw[{val}]
		]
stringifiedExpressionFromRules[rules_] := 
	If[MatchQ[rules,{___, "parts" -> _, ___}],
      stringifiedExpressionFromRules  @ ("head" /. rules) <> "[" <>
      insertCommaSeparators [
		stringifiedExpressionFromRules /@ ("parts" /. rules) ] <> "]",
      Throw[rules]];

stringifiedExpressionFromRules[x___] := Throw[{x}];


(* ::Subsection:: *)
(*rulesFromEvaluatableHeldExpression[ expr ]*)


(* ::Text:: *)
(*Allows call of Evaluate inside the expression. Differs from "rulesFromHeldExpression" only in the Attributes -- rulesFromEvaluatableHeldExpression is HoldAll; rulesFromHeldExpression is HoldAllComplete. *)


SetAttributes[rulesFromEvaluatableHeldExpression, HoldAll];

rulesFromEvaluatableHeldExpression[expr_ /; NumberQ @ Unevaluated @ expr] := 
	rulesFromExpression[expr]
rulesFromEvaluatableHeldExpression[expr_ /; (Head @ Unevaluated @ expr === Symbol && AtomQ @ Unevaluated @ expr === True)] := {
	"symbol" -> ToString @ Unevaluated @ expr}
rulesFromEvaluatableHeldExpression[expr_ /; (Head @ Unevaluated @ expr === String)] := {
	"string" -> ToString @ Unevaluated @ expr}
rulesFromEvaluatableHeldExpression[head_[parts___]] := {
	"head"  -> rulesFromEvaluatableHeldExpression  @ head, 
	"parts" -> rulesFromEvaluatableHeldExpression /@ Unevaluated @ {parts}}
rulesFromEvaluatableHeldExpression[x___] := Throw @ {x};


(* ::Subsection:: *)
(*rulesFromHeldExpression[ expr ]*)


(* ::Text:: *)
(*Applies maximal quoting to input expressions. Will even quote calls of "Evaluate". *)


SetAttributes[rulesFromHeldExpression, HoldAllComplete];

rulesFromHeldExpression[expr_ /; NumberQ @ Unevaluated @ expr] := 
	rulesFromExpression[expr]
rulesFromHeldExpression[expr_ /; (Head @ Unevaluated @ expr === Symbol && AtomQ @ Unevaluated @ expr === True)] := {
	"symbol" -> ToString @ Unevaluated @ expr}
rulesFromHeldExpression[expr_ /; (Head @ Unevaluated @ expr === String)] := {
	"string" -> ToString @ Unevaluated @ expr}
rulesFromHeldExpression[head_[parts___]] := {
	"head"  -> rulesFromHeldExpression  @ head, 
	"parts" -> rulesFromHeldExpression /@ Unevaluated @ {parts}}
rulesFromHeldExpression[x___] := Throw @ {x};


(* ::Subsection:: *)
(*jsonStringFromHeldExpression[ expr ]*)


SetAttributes[jsonStringFromHeldExpression, HoldAllComplete];

jsonStringFromHeldExpression[expr_] :=
  ExportString[rulesFromHeldExpression[expr], "JSON"] // 
    StringReplace[#,Whitespace->""]&;


(* ::Subsection:: *)
(*expressionFromJsonString[ str ]*)


expressionFromJsonString[str_String] :=
  expressionFromRules @ ImportString[str, "JSON"];


(* ::Subsection:: *)
(*captive[ expr ] -- a direct mapping to S-expressions*)


(* ::Text:: *)
(*Less verbose output*)


SetAttributes[captive, HoldAll]; (* NOT HoldAllComplete *)


(* ::Text:: *)
(*Rules for Atoms*)


captive[expr_ /; AtomQ @ Unevaluated @ expr] := 
  If[Quiet[ValueQ @ expr], captive @ Evaluate @ expr, expr]; 


(* ::Text:: *)
(*Rules for non-Atoms*)


captive[head_[parts___]] := 
  {captive @ head} ~Join~ (captive /@ (Unevaluated @ {parts}))
captive[x___] := Throw[{x}];


(* ::Subsection:: *)
(*jaqCaptive[ expr ] -- TODO: a *)
(*more Jacquard-ish captive*)


(* ::Text:: *)
(*Less verbose output*)


SetAttributes[jaqCaptive, HoldAll]; (* NOT HoldAllComplete *)


(* ::Text:: *)
(*Rules for Atoms*)


jaqCaptive[expr_ /; AtomQ @ Unevaluated @ expr] := 
  If[Quiet[ValueQ @ expr], captive @ Evaluate @ expr, expr]


(* ::Text:: *)
(*Rules for non-Atoms*)


jaqCaptive[head_[parts___]] := 
  {jaqCaptive @ head} ~Join~ (jaqCaptive /@ (Unevaluated @ {parts}))
jaqCaptive[x___] := Throw[{x}];


(* ::Section:: *)
(*HTTP Post Interfaces*)


SetAttributes[jacquardPostExpression, HoldFirst];
jacquardPostExpression[
    request_,
    importer_:expressionFromRules,
    url_: "http://127.0.0.1:5000/jacquard"
] :=
  Module[{
      requestExprString,
      client, method,
      entity, responseCode, response,
      responseRules, responseExpression},
    JavaBlock[
      requestExprString = ToString[Unevaluated[request], InputForm];
      Print[requestExprString];
      client=JavaNew[
        "org.apache.commons.httpclient.HttpClient"];
      method=JavaNew[
        "org.apache.commons.httpclient.methods.PostMethod",url];
      entity=JavaNew[
        "org.apache.commons.httpclient.methods.StringRequestEntity",
        requestExprString];
      method @ setRequestHeader["Content-Type", "text/plain"];
      method @ setRequestEntity[entity];
      responseCode=client @ executeMethod[method];
      If[responseCode === 200,
        (* then *)
        response = method @ getResponseBodyAsString[];
        responseRules = ImportString[response,"JSON"];
        responseExpression = importer @ responseRules,
        (* else *)
        Message[
          jacquardPostRules::httperr,
          responseCode];
          $Failed
        ]]]


jacquardPostRules[
    request_,
    importer_:expressionFromRules,
    url_: "http://127.0.0.1:5000/jacquard"
] :=
  Module[{
      requestJsonString,
      client, method,
      entity, responseCode, response,
      responseRules, responseExpression},
    JavaBlock[
      requestJsonString=ExportString[request,"JSON"];
      client=JavaNew[
        "org.apache.commons.httpclient.HttpClient"];
      method=JavaNew[
        "org.apache.commons.httpclient.methods.PostMethod",url];
      entity=JavaNew[
        "org.apache.commons.httpclient.methods.StringRequestEntity",
        requestJsonString];
      method @ setRequestHeader["Content-Type", "application/json"];
      method @ setRequestEntity[entity];
      responseCode=client @ executeMethod[method];
      If[responseCode === 200,
        (* then *)
        response = method @ getResponseBodyAsString[];
        responseRules = ImportString[response,"JSON"];
        responseExpression = importer @ responseRules,
        (* else *)
        Message[
          jacquardPostRules::httperr,
          responseCode];
          $Failed
        ]]]


jacquardEvalExpression[
    requestExpr_,
    importer_:expressionFromRules,
    url_: "http://127.0.0.1:5000/jacquard"] :=
  jacquardPostRules[rulesFromExpression @ requestExpr, importer, url];


SetAttributes[jacquardEvalHeldExpression,HoldAllComplete];
jacquardEvalHeldExpression[
    requestExpr_,
    importer_:expressionFromRules,
    url_: "http://127.0.0.1:5000/jacquard"] :=
  jacquardPostRules[rulesFromHeldExpression[requestExpr], importer, url];


ClearAll[jacquardEvalEvaluatableHeldExpression];
SetAttributes[jacquardEvalEvaluatableHeldExpression,HoldAllComplete];
jacquardEvalEvaluatableHeldExpression[
    requestExpr_,
    importer_:expressionFromRules,
    url_: "http://127.0.0.1:5000/jacquard"] :=
  jacquardPostRules[rulesFromEvaluatableHeldExpression[requestExpr], importer, url];


SetAttributes[jacquardStringifiedEvalHeldExpression,HoldAllComplete];
jacquardStringifiedEvalHeldExpression[
    requestExpr_,
    url_:"http://127.0.0.1:5000/jacquard"] :=
  jacquardPostRules[rulesFromHeldExpression[requestExpr], stringifiedExpressionFromRules, url];


(* ::Section:: *)
(*The Ends*)


End[];(* Private *)


EndPackage[];
