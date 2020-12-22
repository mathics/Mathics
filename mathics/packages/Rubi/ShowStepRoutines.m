(* ::Package:: *)

SimplifyFlag = True;
$StepCounter = 0;
$RuleList = {};

StepFunction::usage = "StepFunction[func] modifies the rules to display steps when the control variable $ShowSteps is True";
StepFunction[func_] :=
    Module[{lst = DownValues[func]},
      Block[{ShowStep, SimplifyFlag},
        Monitor[Do[
          lst[[i]] = ModifyRule[i, lst[[i]], SimplifyFlag],
          {i, 1, Length[lst]}],
          ProgressIndicator[i, {1, Length[lst] + 1}, ImageSize -> {500, 20}]]];
      ClearDownValues[func];
      SetDownValues[func, lst];
      Unprotect[func]]

StepFunction[func1_, func2_] :=
    Block[{lst, num = 0, ShowStep, SimplifyFlag},
      lst = Map[Function[ModifyRule[num++, #, SimplifyFlag]], DownValues[func1]];
      ClearDownValues[func1];
      SetDownValues[func2, ReplaceAll[lst, {func1 -> func2}]]]


(* rule is an expression of the form RuleDelayed[lhs,rhs].
  flag has the value SimplifyFlag.  ModifyRule[rule,flag]
  formats the rule's left hand side as a string (lhsStrg) in InputForm,
  formats the rule's conditions as a string (condStrg) in StandardForm,
  formats the rule's let statements as a string (letStrg) in StandardForm,
  splices the conditions string and let strings together as a string (condStrg) in StandardForm,
  formats the rule's right hand side as a string (rhsStrg) in InputForm,
  then it returns rule with the body replaced with the expression
    ShowStep[num,condStrg,lhsStrg,rhsStrg,rhs] /; flag. *)
ModifyRule[num_, rule_RuleDelayed, flag_] :=
    Module[{lhsStrg, rhsStrg, condStrg, letStrg},
      If[Not[FreeQ[Hold[rule], ShowStep]] ||
          Not[FreeQ[Hold[rule], Identity]] ||
          Not[FreeQ[Hold[rule], DeactivateTrig]] ||
          Not[FreeQ[Hold[rule], Defer[Int]]] ||
          Not[FreeQ[Hold[rule], Unintegrable]] ||
          Not[FreeQ[Hold[rule], CannotIntegrate]] ||
          Not[FreeQ[Hold[rule], Preprocess]],
        rule,
        lhsStrg = FormatLhs[rule];
        If[rule[[2, 0]] === Condition,
          condStrg = FormatConditions[Extract[rule, {2, 2}, Defer]];
          If[rule[[2, 1, 0]] === With || rule[[2, 1, 0]] === Module || rule[[2, 1, 0]] === Block,
            letStrg = FormatLets[Extract[rule, {2, 1, 1}, Defer]];
            If[rule[[2, 1, 2, 0]] === Condition,
              condStrg = SpliceConditionString[condStrg, letStrg, FormatConditions[Extract[rule, {2, 1, 2, 2}, Defer]]];
              rhsStrg = FormatRhs[Extract[rule, {2, 1, 2, 1}, Defer]];
              WrapCondition[ReplacePart[rule, ShowStep[num, condStrg, lhsStrg, rhsStrg, Extract[rule, {2, 1, 2, 1}, Hold]], {2, 1, 2, 1}], flag],
              condStrg = SpliceConditionString[condStrg, letStrg, ""];
              rhsStrg = FormatRhs[Extract[rule, {2, 1, 2}, Defer]];
              WrapCondition[ReplacePart[rule, ShowStep[num, condStrg, lhsStrg, rhsStrg, Extract[rule, {2, 1, 2}, Hold]], {2, 1, 2}], flag]],
            condStrg = SpliceConditionString[condStrg, "", ""];
            rhsStrg = FormatRhs[Extract[rule, {2, 1}, Defer]];
            WrapCondition[ReplacePart[rule, ShowStep[num, condStrg, lhsStrg, rhsStrg, Extract[rule, {2, 1}, Hold]], {2, 1}], flag]],
          If[rule[[2, 0]] === With || rule[[2, 0]] === Module || rule[[2, 0]] === Block,
            letStrg = FormatLets[Extract[rule, {2, 1}, Defer]];
            If[rule[[2, 2, 0]] === Condition,
              condStrg = FormatConditions[Extract[rule, {2, 2, 2}, Defer]];
              condStrg = SpliceConditionString["", letStrg, condStrg];
              rhsStrg = FormatRhs[Extract[rule, {2, 2, 1}, Defer]];
              WrapCondition[ReplacePart[rule, ShowStep[num, condStrg, lhsStrg, rhsStrg, Extract[rule, {2, 2, 1}, Hold]], {2, 2, 1}], flag],
              condStrg = SpliceConditionString["", letStrg, ""];
              rhsStrg = FormatRhs[Extract[rule, {2, 2}, Defer]];
              WrapCondition[ReplacePart[rule, ShowStep[num, condStrg, lhsStrg, rhsStrg, Extract[rule, {2, 2}, Hold]], {2, 2}], flag]],
            rhsStrg = FormatRhs[Extract[rule, 2, Defer]];
            WrapCondition[ReplacePart[rule, ShowStep[num, "", lhsStrg, rhsStrg, Extract[rule, 2, Hold]], 2], flag]]]]]


WrapCondition[rule_RuleDelayed, condition_] :=
    ReplacePart[ReplacePart[rule, Append[Extract[rule, {2}, Hold], condition], {2}], Condition, {2, 0}]


(* rule is a rule as an expression of the form RuleDelayed[lhs,rhs].
  FormatLhs[rule] returns returns a string for the lhs of the rule in InputForm with the
  pattern tags "_." and "_" removed from the dummy variable names. *)
FormatLhs[rule_] :=
    Module[{lhs = Extract[rule, {1, 1}, Defer], conditions, func, var},
      ( If[rule[[2, 0]] === Condition,
        conditions = Extract[rule, {2, 2}, Defer];
        If[conditions[[1, 0]] === FunctionOfQ,
          func = conditions[[1, 1]];
          var = conditions[[1, 2]];
          lhs = ReplaceVariable[lhs, var, func],
          If[conditions[[1, 0]] === And && MemberQ[conditions, FunctionOfQ, {3}, Heads -> True],
            func = conditions[[1, Position[conditions, FunctionOfQ, {3}, 1][[1, 2]], 1]];
            var = conditions[[1, Position[conditions, FunctionOfQ, {3}, 1][[1, 2]], 2]];
            lhs = ReplaceVariable[lhs, var, func]]]] );
      DropDefer[StringReplace[ToString[lhs, InputForm], {"_Symbol" -> "", "_." -> "", "_" -> ""}]]]

(* ReplaceVariable[lhs,var,func] returns lhs with the var replaced by F[func] *)
ReplaceVariable[lhs_, var_, func_] :=
    Block[{F},
      If[PatternEqualQ[lhs[[1, 1]], var],
        ReplacePart[lhs, F[func], {1, 1}],
        If[PatternEqualQ[lhs[[1, 1, 1]], var],
          ReplacePart[lhs, F[func], {1, 1, 1}],
          If[PatternEqualQ[lhs[[1, 1, 2]], var],
            ReplacePart[lhs, F[func], {1, 1, 2}],
            If[PatternEqualQ[lhs[[1, 1, 3]], var],
              ReplacePart[lhs, F[func], {1, 1, 3}],
              Print["Function of expression variable not found: ", lhs, " ", var, " ", func];
              Abort[]]]]]]

PatternEqualQ[pattern_, var_] :=
    Head[pattern] === Pattern && pattern[[1]] === var


(* rhs is an expression of the form Defer[...] where ... is the right hand side of a rule.
  FormatRhs[rhs] returns a string for the rhs of the rule in InputForm. *)
FormatRhs[rhs_] :=
    Block[{SubstFor, Simp, Rt, F},
      DropDefer[ToString[
        ReplaceAll[
          ReplaceAll[rhs, {
            SubstFor[v_, u_, x_] -> F[x],
            Rt[u_, 2] -> Sqrt[u],
            Rt[u_, n_] -> u^(1 / n)}],
          Simp[u_, x_] -> u],
        InputForm]]]


(* strg is a string of the form "Defer[...]".  DropDefer[strg] returns the string "...", with
  all occurrences of the string "Int[" replaced with "Integrate" and "Dif" with "D" so they will
  display using integral and differential signs when the string is converted to StandardForm. *)
DropDefer[strg_] :=
    StringDrop[StringDrop[StringReplace[strg, {
      "Int[" -> "Integrate[",
      "Dif[" -> "D["
      (*, "sin["->"Sin[",  "cos["->"Cos[",  "tan["->"Tan[",  "cot["->"Cot[",  "sec["->"Sec[",  "csc["->"csc[",
        "sinh["->"Sinh[",  "cosh["->"Cosh[",  "tanh["->"Tanh[",  "coth["->"Coth[",  "sech["->"Sech[",  "csch["->"csch[" *)
    }], 6], -1]


(* conditions is an expression comprising the conditions on a rule.  FormatConditions[conditions]
  replaces expressions of the form Not[FalseQ[u]] with u in conditions,
  deletes expressions of the form FreeQ[u,x], FunctionOfQ[u,v,x] and EasyDQ[u,x] in conditions,
  replaces expressions of the form Rt[u,2] with Sqrt[u] and Rt[u,n] with u^(1/n) in conditions,
  and then returns the conditions as a string formatted in Mathematica's StandardForm. *)
FormatConditions[conditions_] :=
    If[conditions[[1, 0]] === Not && conditions[[1, 1, 0]] === FalseQ,
      FormatConditions[Extract[conditions, {1, 1, 1}, Defer]],
      If[conditions[[1, 0]] === FreeQ,
        "",
        If[conditions[[1, 0]] === And && MemberQ[conditions, FreeQ, {3}, Heads -> True],
          FormatConditions[DeleteCondition[FreeQ, conditions]],
          If[conditions[[1, 0]] === FunctionOfQ,
            "",
            If[conditions[[1, 0]] === And && MemberQ[conditions, FunctionOfQ, {3}, Heads -> True],
              FormatConditions[DeleteCondition[FunctionOfQ, conditions]],
              If[conditions[[1, 0]] === EasyDQ,
                "",
                If[conditions[[1, 0]] === And && MemberQ[conditions, EasyDQ, {3}, Heads -> True],
                  FormatConditions[DeleteCondition[EasyDQ, conditions]],
                  ToConditionString[conditions]]]]]]]]

DeleteCondition[func_, conditions_] :=
    If[Quiet[Head[Extract[conditions, {1, 3}, Defer]] === Extract],
      If[Position[conditions, func, {3}, 1][[1, 2]] == 1,
        Extract[conditions, {1, 2}, Defer],
        Extract[conditions, {1, 1}, Defer]],
      Delete[conditions, {1, Position[conditions, func, {3}, 1][[1, 2]]}]]


(* let is a body of a With, Module or Block statement.  FormatLets[let] returns the assignment as a
  string in the form "var=expression, then" formatted in Mathematica's StandardForm. *)
FormatLets[let_] :=
    If[MatchQ[let, Defer[{u_}]],
      If[let[[1, 1, 0]] === Set &&
          let[[1, 1, 2, 0]] === Block &&
          Extract[let, {1, 1, 2, 1}, Defer] === Defer[{$ShowSteps = False}],
        If[let[[1, 1, 2, 2, 0]] === Simplify,
          ToConditionString[Extract[let, {1, 1, 1}, Defer]] <> "=" <>
              ToConditionString[Extract[let, {1, 1, 2, 2, 1}, Defer]] <> ", then",
          ToConditionString[Extract[let, {1, 1, 1}, Defer]] <> "=" <>
              ToConditionString[Extract[let, {1, 1, 2, 2}, Defer]] <> ", then"],
        ToConditionString[Extract[let, {1, 1}, Defer]] <> ", then"],
      ToConditionString[let] <> ", then"]


(* conditions is an expression comprising the conditions on a rule.  ToConditionString[conditions]
  replace calls in the conditions on the function Rt[u,2] with Sqrt[u] and Rt[u,n] with u^(1/n),
  and then returns the conditions as a string formatted in Mathematica's StandardForm. *)
ToConditionString[conditions_] :=
    ToString[ReplaceAll[conditions, {
      Rt[u_, 2] -> Sqrt[u],
      Rt[u_, n_] -> u^(1 / n)
    }], StandardForm]


(* cond1, lets and cond2 are strings.  SpliceConditionString[cond1,lets,cond2] concatenates and
  returns the condition and let strings along with the "if" and "let" to make a human readable
  condition string. *)
SpliceConditionString[cond1_, lets_, cond2_] :=
    If[cond2 === "",
      If[lets === "",
        If[cond1 === "",
          "",
          "If " <> cond1],
        If[cond1 === "",
          "Let " <> lets,
          "If " <> cond1 <> " let " <> lets]],
      If[lets === "",
        If[cond1 === "",
          "If " <> cond2,
          "If " <> cond1 <> " if " <> cond2],
        If[cond1 === "",
          "Let " <> lets <> " if " <> cond2,
          "If " <> cond1 <> " let " <> lets <> " if " <> cond2]]]


(* condStrg, lhsStrg and rhsStrg are the strings required to display the rule being applied.
  rhs is the expression on the right side of the rule (i.e. the consequent of the rule).
  If $ShowSteps is True, ShowStep[num,condStrg,lhsStrg,rhsStrg,rhs] displays the rule being applied,
  sets SimplifyFlag to False to turn off further simplification, and release the hold on the rhs
  of the rule. *)
ShowStep[condStrg_, lhsStrg_, rhsStrg_, rhs_] := With[
  {
    replaceEllipsis = Function[in, StringReplace[in, "..." -> "\[Ellipsis]"]]
  },
  If[IntegerQ[$StepCounter],
    $StepCounter = $StepCounter + 1];
  If[$ShowSteps === True,
    Sow[RubiRule[condStrg, MakeExpression[replaceEllipsis@lhsStrg], MakeExpression[replaceEllipsis@rhsStrg]]];
    Block[{SimplifyFlag = False},
      ReplaceAll[ReleaseHold[rhs], {Unintegrable -> Defer[Int], CannotIntegrate -> Defer[Int]}]],
    ReleaseHold[rhs]]
]

ShowStep[num_, condStrg_, lhsStrg_, rhsStrg_, rhs_] := (
  If[IntegerQ[$StepCounter],
    $StepCounter = $StepCounter + 1];
  If[Head[$RuleList] === List && Not[MemberQ[$RuleList, num]],
    $RuleList = Append[$RuleList, num]];
  If[$ShowSteps === True,
    Sow[RubiRule[condStrg, MakeExpression[lhsStrg], MakeExpression[rhsStrg], num ]];
    Block[{SimplifyFlag = False},
      ReplaceAll[ReleaseHold[rhs], {Unintegrable -> Defer[Int], CannotIntegrate -> Defer[Int]}]],
    ReleaseHold[rhs]] )


(* Note: Clear[func] also eliminates 2-D display of functions like Integrate. *)
ClearDownValues[func_Symbol] := (
  Unprotect[func];
  DownValues[func] = {};
  Protect[func])


(* Note: A bug in earlier versions of Mathematica prevents setting more than 529 DownValues using a simple assignment! *)
SetDownValues[func_Symbol, lst_List] := (
  Unprotect[func];
  ( If[$VersionNumber >= 8,
    DownValues[func] = lst,
    DownValues[func] = Take[lst, Min[529, Length[lst]]];
    Scan[Function[ReplacePart[ReplacePart[#, #[[1, 1]], 1], SetDelayed, 0]], Drop[lst, Min[529, Length[lst]]]]] );
  Protect[func])
