(* ::Package:: *)

(* ::Title:: *)
(*Load Rubi*)


(* ::Subsubtitle:: *)
(*Global variables:*)


(* ::Subsection:: *)
(*  RulesDirectory is the root directory of the integration rules.  *)


(* ::Subsection:: *)
(*   If $LoadElementaryFunctionRules is True, elementary and special function integration rules are loaded in addition to rational and algebraic function rules.*)


(* ::Subsection:: *)
(*  If $LoadShowSteps is True, rules are modified so integration steps can be displayed.*)


(* ::Subsection:: *)
(*  If $IndexRules is True, displays the range of rule numbers defined in each file as the rule files are loaded.*)


(* ::Section::Closed:: *)
(* Usage Messages*)


Unprotect[Int];  Clear[Int];  Clear[Unintegrable];  Clear[CannotIntegrate];


Int::usage = If[TrueQ[$LoadShowSteps],
"Int[expn, var] returns the antiderivative (indefinite integral) of <expn> with respect to <var>.
Int[expn, var, Step] displays the first step used to integrate <expn> with respect to <var>, and returns the intermediate result.
Int[expn, var, Steps] displays all the steps used to integrate <expn> with respect to <var>, and returns the antiderivative.
Int[expn, var, Stats], before returning the antiderivative of <expn> with respect to <var>, displays a list of statistics of the form {a, b, c, d, e} where
   <a> is the number of steps used to integrate <expn>,
   <b> is the number of distinct rules used to integrate <expn>,
   <c> is the leaf count size of <expn>,
   <d> is the leaf count size of the antiderivative, and
   <e> is the rule-to-size ratio of the integration (i.e. the quotient of <b> and <c>).
Int[{expn1, expn2, ...}, var] returns a list of the antiderivatives of <expn1>, <expn2>, ... each with respect to <var>.
Int[expn, {var, a, b}] returns the limit of the antiderivative of <expn> as <var> approaches <b> minus the limit as <var> approaches <a>.  Note that this difference will NOT always equal the definite integral of <expn> from <a> to <b>.",

"Int[expn, var] returns the antiderivative (indefinite integral) of <expn> with respect to <var>.
Int[{expn1, expn2, ...},var] returns a list of the antiderivatives of <expn1>, <expn2>, ... each with respect to <var>.
Int[expn, {var, a, b}] returns the limit of the antiderivative of <expn> as <var> approaches <b> minus the limit as <var> approaches <a>.  Note that this difference will NOT always equal the definite integral of <expn> from <a> to <b>."];


$RubiVersion::usage = "$RubiVersion shows the currently loaded version of Rubi.";


$RubiVersion = "Rubi 4.17.6";


$UseLog::usage = "If $UseLog is True, antiderivatives will be expressed in terms of logarithms rather than hyperbolic arctangents when convenient.";


$UseGamma::usage = "If $UseGamma is True, antiderivatives will be expressed more compactly in terms of the gamma function rather than a sum of exponentials.";


Star::usage = "Star[expn1,expn2] displays as <expn1>*<expn2>, and returns their product with <expn1> distributed over the terms of <expn2>.";
Subst::usage = "Subst[expn1,var,expn2] substitutes <expn2> for <var> in <expn1>.";
Step::usage = "Int[expn, var, Step] displays the first step in the integration of <expn> with respect to <var> and returns the intermediate result."
Steps::usage = "Int[expn, var, Steps] displays all the steps in the integration of <expn> with respect to <var> and returns the antiderivative."
Stats::usage = "Int[expn, var, Stats] before returning the antiderivative <expn> with respect to <var>, displays a list {a, b, c, d, e} of statistics."


$RuleColor::usage = "$RuleColor is the color used to display rules when showing integration steps. The default rule color is red."
$ConditionColor::usage = "$ConditionColor is the color used to display application conditions when showing integration steps. The default condition color is blue."


Unintegrable::usage = "Unintegrable[expn,var] indicates <expn> is not integrable with respect to <var> in closed-form.";
CannotIntegrate::usage = "CannotIntegrate[expn,var] indicates Rubi is unable to integrate <expn> with respect to <var>.";
$Unintegrable::usage = "If $Unintegrable is True and <expn> is not integrable with respect to <var> in terms of the functions Rubi uses to express antiderivatives, Int[expn,var] returns Unintegrable[expn,var].";
$StepCounter::usage = "If the ShowSteps package has been loaded and $StepCounter is an integer, it is incremented each time an integration rule is applied.";


sin::usage = "Inert sine function";
cos::usage = "Inert cosine function";
tan::usage = "Inert tangent function";
cot::usage = "Inert cotangent function";
sec::usage = "Inert secant function";
csc::usage = "Inert cosecant function";


(* ::Section::Closed:: *)
(*LoadRules Routine*)


LoadRules[filename_String] :=
  If[TrueQ[$IndexRules],
    With[{n0=Length[DownValues[Int]]+1},
    Get[FileNameJoin[{RulesDirectory,filename<>".m"}]];
    If[n0>Length[DownValues[Int]],
      Print["File \""<>filename<>".m\"..."],
    Print["Rules ",n0,"-",Length[DownValues[Int]]," defined in \""<>filename<>".nb\"..."]]],
  StatusBarPrint["Loading "<>filename<>".m..."];
  Get[FileNameJoin[{RulesDirectory,filename<>".m"}]];
  StatusBarPrint[""]]


StatusBarPrint[message_String] := If[$Notebooks, CurrentValue[EvaluationNotebook[], WindowStatusArea] = message]


Print[$RubiVersion<>" running on Mathematica "<>ToString[$VersionNumber]<>" on " <> DateString[]];


(* ::Section:: *)
(*Load Algebraic Function Integration Rules*)


(* ::Item:: *)
(*Rubi's integration rule files in this section must be loaded in the following order:*)


  LoadRules[FileNameJoin[{"IntegrationUtilityFunctions"}]];
  LoadRules[FileNameJoin[{"Debug routines"}]];


(* ::Subsection::Closed:: *)
(*Load Linear/Quadratic Integration Rules*)


  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.1 (a+b x)^m"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.2 (a+b x)^m (c+d x)^n"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.3 (a+b x)^m (c+d x)^n (e+f x)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.4 (a+b x)^m (c+d x)^n (e+f x)^p (g+h x)^q"}]];

(*
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.6 LinearQuadratic", "1.1.6.1 (c+d x)^m (a+b x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.6 LinearQuadratic", "1.1.6.2 (e x)^n (c+d x)^m (a+b x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.6 LinearQuadratic", "1.1.6.3 (c+d x)^m (e+f x) (a+b x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.6 LinearQuadratic", "1.1.6.4 (c+d x)^m (e+f x)^n (a+b x^2)^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.1 (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.2 (d+e x)^m (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.3 (d+e x)^m (f+g x)^n (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.4 (a+b x+c x^2)^p (d+e x+f x^2)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.5 (g+h x)^m (a+b x+c x^2)^p (d+e x+f x^2)^q"}]];
 *)


(* ::Subsection::Closed:: *)
(*Load Binomial Integration Rules*)


  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1 Generic Binomial Integration Rules"}]];


(*
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.1 (a+b x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.2 (c x)^m (a+b x^2)^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.3 (a+b x^2)^p (c+d x^2)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.4 (e x)^m (a+b x^2)^p (c+d x^2)^q"}]];
(*
 *)

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 General", "1.1.4.1 (a+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 General", "1.1.4.2 (c x)^m (a+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 General", "1.1.4.3 (a+b x^n)^p (c+d x^n)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 General", "1.1.4.4 (e x)^m (a+b x^n)^p (c+d x^n)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 General", "1.1.4.5 (a+b x^n)^p (c+d x^n)^q (e+f x^n)^r"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 General", "1.1.4.6 (g x)^m (a+b x^n)^p (c+d x^n)^q (e+f x^n)^r"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.5 Improper", "1.1.5.1 (a x^j+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.5 Improper", "1.1.5.2 (c x)^m (a x^j+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.5 Improper", "1.1.5.3 (e x)^m (a x^j+b x^k)^p (c+d x^n)^q"}]];
   *)

(* ::Subsection:: *)
(*Load Trinomial Integration Rules*)


  (*
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 Quartic", "1.2.2.0 P[x] (a+c x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.1 (a+b x^n+c x^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.2 (d x)^m (a+b x^n+c x^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.3 (d+e x^n)^q (a+b x^n+c x^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.4 (f x)^m (d+e x^n)^q (a+b x^n+c x^(2 n))^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.1 (a x^q+b x^n+c x^(2 n-q))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.2 (d x)^m (a x^q+b x^n+c x^(2 n-q))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.3 (d+e x^(n-q)) (a x^q+b x^n+c x^(2 n-q))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.4 (f x)^m (d+e x^(n-q)) (a x^q+b x^n+c x^(2 n-q))^p"}]];
   *)


(* ::Subsection::Closed:: *)
(*Load Polynomial-Linear/Quadratic Integration Rules*)


  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1 Generic Polynomial-Binomial Integration Rules"}]];

  (*
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.4 Simplification rules", "1.4.1 Generic simplification rules"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.4 Simplification rules", "1.4.2 Algebraic simplification rules"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.4 Simplification rules", "1.4.3 Algebraic normalization rules"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.6 LinearQuadratic", "1.1.6.6 P[x] (e x)^m (c+d x)^n (a+b x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.6 LinearQuadratic", "1.1.6.5 P[x] (c+d x)^n (a+b x^2)^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.8 (c x)^m P[x] (a+b x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.7 P[x] (a+b x^2)^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.8 P[x] (a+b x)^m (c+d x)^n (e+f x)^p (g+h x)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.7 P[x] (a+b x)^m (c+d x)^n (e+f x)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.6 P[x] (a+b x)^m (c+d x)^n"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.5 P[x] (a+b x)^m"}]];
   *)

  (*
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.9 P[x] (a+b x+c x^2)^p (d+e x+f x^2)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.8 P[x] (d+e x)^m (f+g x)^n (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.7 P[x] (d+e x)^m (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.6 P[x] (a+b x+c x^2)^p"}]];
   *)


(* ::Subsection::Closed:: *)
(*Load Polynomial-Trinomial Integration Rules*)


  (*
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.5 LinearQuartic", "1.2.5.1 (d+e x)^q (a+b x^2+c x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.5 LinearQuartic", "1.2.5.2 P[x] (d+e x)^q (a+b x^2+c x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.5 LinearQuartic", "1.2.5.3 F[x] (d+e x)^q (a+b x^2+c x^4)^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.8 (f x)^m P[x] (d+e x^n)^q (a+b x^n+c x^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.7 P[x] (d+e x^n)^q (a+b x^n+c x^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.6 (d x)^m P[x] (a+b x^n+c x^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.5 P[x] (a+b x^n+c x^(2 n))^p"}]];
   *)


(* ::Subsection::Closed:: *)
(*Load Polynomial-Binomial Integration Rules*)


  (*
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 Quartic", "1.1.3.3 P[x] (a+b x^4)^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 General", "1.1.4.8 (c x)^m P[x] (a+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 General", "1.1.4.7 P[x] (a+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.5 Improper", "1.1.5.4 (c x)^m P[x] (a x^j+b x^n)^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.6 LinearQuadratic", "1.1.6.7 P[x] (c+d x)^m (e+f x)^n (a+b x^2)^p"}]];
 *)

(* ::Subsection::Closed:: *)
(*Load Miscellaneous Algebraic Integration Rules*)


  (*
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Miscellaneous rules", "1.3.3 P[x]^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Miscellaneous rules", "1.3.1 F[x] (a+b x+c x^2+d x^3)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Miscellaneous rules", "1.3.2 F[x] (a+b x+c x^2+d x^3+e x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Miscellaneous rules", "1.3.4 P[x] Q[x]^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Miscellaneous rules", "1.3.6 F[x] (c x^2+d Sqrt[a+b x^4])^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Miscellaneous rules", "1.3.7 Miscellaneous algebraic rules"}]];
   *)

  LoadRules[FileNameJoin[{"9 Miscellaneous", "9.2 Piecewise linear functions"}]];


(* ::Section::Closed:: *)
(*Load Exponential Integration Rules*)

(*
If[TrueQ[$LoadElementaryFunctionRules],
  LoadRules[FileNameJoin[{"2 Exponentials", "2.1 (c+d x)^m (a+b (F^(g (e+f x)))^n)^p"}]];
  LoadRules[FileNameJoin[{"2 Exponentials", "2.2 (c+d x)^m (F^(g (e+f x)))^n (a+b (F^(g (e+f x)))^n)^p"}]];
  LoadRules[FileNameJoin[{"2 Exponentials", "2.3 Miscellaneous exponentials"}]];
];

 *)

(* ::Section::Closed:: *)
(*Load Logarithm Function Integration Rules*)

(*
If[TrueQ[$LoadElementaryFunctionRules],
  LoadRules[FileNameJoin[{"3 Logarithms", "3.1.1 (a+b log(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"3 Logarithms", "3.1.2 (d x)^m (a+b log(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"3 Logarithms", "3.1.3 (d+e x^r)^q (a+b log(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"3 Logarithms", "3.1.4 (f x)^m (d+e x^r)^q (a+b log(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"3 Logarithms", "3.1.5 u (a+b log(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"3 Logarithms", "3.3 u (a+b log(c (d+e x)^n))^p"}]];
  LoadRules[FileNameJoin[{"3 Logarithms", "3.4 u (a+b log(c (d+e x^m)^n))^p"}]];
  LoadRules[FileNameJoin[{"3 Logarithms", "3.2.1 (f+g x)^m (A+B log(e ((a+b x) over (c+d x))^n))^p"}]];
  LoadRules[FileNameJoin[{"3 Logarithms", "3.2.2 (f+g x)^m (h+i x)^q (A+B log(e ((a+b x) over (c+d x))^n))^p"}]];
  LoadRules[FileNameJoin[{"3 Logarithms", "3.2.3 u log(e (f (a+b x)^p (c+d x)^q)^r)^s"}]];
  LoadRules[FileNameJoin[{"3 Logarithms", "3.5 Miscellaneous logarithms"}]];
];


(* ::Section::Closed:: *)
(*Load Trig Function Integration Rules*)


If[TrueQ[$LoadElementaryFunctionRules],
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.0.1 (a sin)^m (b trg)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.0.2 (a trg)^m (b tan)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.0.3 (a csc)^m (b sec)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.1.1 (a+b sin)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.1.2 (g cos)^p (a+b sin)^m"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.1.3 (g tan)^p (a+b sin)^m"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.2.1 (a+b sin)^m (c+d sin)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.2.2 (g cos)^p (a+b sin)^m (c+d sin)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.2.3 (g sin)^p (a+b sin)^m (c+d sin)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.3.1 (a+b sin)^m (c+d sin)^n (A+B sin)"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.4.1 (a+b sin)^m (A+B sin+C sin^2)"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.4.2 (a+b sin)^m (c+d sin)^n (A+B sin+C sin^2)"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.5 trig^m (a cos+b sin)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.6 (a+b cos+c sin)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.7.1 (a+b sin^n)^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.7.2 (d trig)^m (a+b sin^n)^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.8 trig^m (a+b cos^p+c sin^q)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.9 trig^m (a+b sin^n+c sin^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.10 (c+d x)^m (a+b sin)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.11 (e x)^m (a+b x^n)^p sin"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.12 (e x)^m (a+b sin(c+d x^n))^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.13 (d+e x)^m sin(a+b x+c x^2)^n"}]];

  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.1.1 (a+b tan)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.1.2 (d sec)^m (a+b tan)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.1.3 (d sin)^m (a+b tan)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.2.1 (a+b tan)^m (c+d tan)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.2.3 (g tan)^p (a+b tan)^m (c+d tan)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.3.1 (a+b tan)^m (c+d tan)^n (A+B tan)"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.4.1 (a+b tan)^m (A+B tan+C tan^2)"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.4.2 (a+b tan)^m (c+d tan)^n (A+B tan+C tan^2)"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.7 (d trig)^m (a+b (c tan)^n)^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.9 trig^m (a+b tan^n+c tan^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.10 (c+d x)^m (a+b tan)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.11 (e x)^m (a+b tan(c+d x^n))^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.3 Tangent", "4.3.12 (d+e x)^m tan(a+b x+c x^2)^n"}]];

  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.1.1 (a+b sec)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.1.2 (d sec)^n (a+b sec)^m"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.1.3 (d sin)^n (a+b sec)^m"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.1.4 (d tan)^n (a+b sec)^m"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.2.1 (a+b sec)^m (c+d sec)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.2.2 (g sec)^p (a+b sec)^m (c+d sec)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.3.1 (a+b sec)^m (d sec)^n (A+B sec)"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.4.1 (a+b sec)^m (A+B sec+C sec^2)"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.4.2 (a+b sec)^m (d sec)^n (A+B sec+C sec^2)"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.7 (d trig)^m (a+b (c sec)^n)^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.9 trig^m (a+b sec^n+c sec^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.10 (c+d x)^m (a+b sec)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.5 Secant", "4.5.11 (e x)^m (a+b sec(c+d x^n))^p"}]];

  LoadRules[FileNameJoin[{"4 Trig functions", "4.7 Miscellaneous", "4.7.1 Sine normalization rules"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.7 Miscellaneous", "4.7.2 Tangent normalization rules"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.7 Miscellaneous", "4.7.3 Secant normalization rules"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.7 Miscellaneous", "4.7.4 (c trig)^m (d trig)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.7 Miscellaneous", "4.7.5 Inert trig functions"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.7 Miscellaneous", "4.7.6 (c+d x)^m trig(a+b x)^n trig(a+b x)^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.7 Miscellaneous", "4.7.7 F^(c (a+b x)) trig(d+e x)^n"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.7 Miscellaneous", "4.7.8 u trig(a+b log(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"4 Trig functions", "4.7 Miscellaneous", "4.7.9 Active trig functions"}]];
];


(* ::Section::Closed:: *)
(*Load Inverse Trig Function Integration Rules*)


If[TrueQ[$LoadElementaryFunctionRules],
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.1 Inverse sine", "5.1.1 (a+b arcsin(c x))^n"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.1 Inverse sine", "5.1.2 (d x)^m (a+b arcsin(c x))^n"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.1 Inverse sine", "5.1.3 (d+e x^2)^p (a+b arcsin(c x))^n"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.1 Inverse sine", "5.1.4 (f x)^m (d+e x^2)^p (a+b arcsin(c x))^n"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.1 Inverse sine", "5.1.5 u (a+b arcsin(c x))^n"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.1 Inverse sine", "5.1.6 Miscellaneous inverse sine"}]];

  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.3 Inverse tangent", "5.3.1 (a+b arctan(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.3 Inverse tangent", "5.3.2 (d x)^m (a+b arctan(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.3 Inverse tangent", "5.3.3 (d+e x)^m (a+b arctan(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.3 Inverse tangent", "5.3.4 u (a+b arctan(c x))^p"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.3 Inverse tangent", "5.3.5 u (a+b arctan(c+d x))^p"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.3 Inverse tangent", "5.3.6 Exponentials of inverse tangent"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.3 Inverse tangent", "5.3.7 Miscellaneous inverse tangent"}]];

  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.5 Inverse secant", "5.5.1 u (a+b arcsec(c x))^n"}]];
  LoadRules[FileNameJoin[{"5 Inverse trig functions", "5.5 Inverse secant", "5.5.2 Miscellaneous inverse secant"}]];
];


(* ::Section::Closed:: *)
(*Load Hyperbolic Function Integration Rules*)


If[TrueQ[$LoadElementaryFunctionRules],
  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.1 Hyperbolic sine", "6.1.10 (c+d x)^m (a+b sinh)^n"}]];
  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.1 Hyperbolic sine", "6.1.11 (e x)^m (a+b x^n)^p sinh"}]];
  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.1 Hyperbolic sine", "6.1.12 (e x)^m (a+b sinh(c+d x^n))^p"}]];
  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.1 Hyperbolic sine", "6.1.13 (d+e x)^m sinh(a+b x+c x^2)^n"}]];

  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.3 Hyperbolic tangent", "6.3.10 (c+d x)^m (a+b tanh)^n"}]];
  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.3 Hyperbolic tangent", "6.3.11 (e x)^m (a+b tanh(c+d x^n))^p"}]];
  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.3 Hyperbolic tangent", "6.3.12 (d+e x)^m tanh(a+b x+c x^2)^n"}]];

  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.5 Hyperbolic secant", "6.5.10 (c+d x)^m (a+b sech)^n"}]];
  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.5 Hyperbolic secant", "6.5.11 (e x)^m (a+b sech(c+d x^n))^p"}]];

  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.7 Miscellaneous", "6.7.6 (c+d x)^m hyper(a+b x)^n hyper(a+b x)^p"}]];
  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.7 Miscellaneous", "6.7.7 F^(c (a+b x)) hyper(d+e x)^n"}]];
  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.7 Miscellaneous", "6.7.8 u hyper(a+b log(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"6 Hyperbolic functions", "6.7 Miscellaneous", "6.7.9 Active hyperbolic functions"}]];
];


(* ::Section::Closed:: *)
(*Load Inverse Hyperbolic Function Integration Rules*)


If[TrueQ[$LoadElementaryFunctionRules],
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.1 Inverse hyperbolic sine", "7.1.1 (a+b arcsinh(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.1 Inverse hyperbolic sine", "7.1.2 (d x)^m (a+b arcsinh(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.1 Inverse hyperbolic sine", "7.1.3 (d+e x^2)^p (a+b arcsinh(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.1 Inverse hyperbolic sine", "7.1.4 (f x)^m (d+e x^2)^p (a+b arcsinh(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.1 Inverse hyperbolic sine", "7.1.5 u (a+b arcsinh(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.1 Inverse hyperbolic sine", "7.1.6 Miscellaneous inverse hyperbolic sine"}]];

  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.2 Inverse hyperbolic cosine", "7.2.1 (a+b arccosh(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.2 Inverse hyperbolic cosine", "7.2.2 (d x)^m (a+b arccosh(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.2 Inverse hyperbolic cosine", "7.2.3 (d+e x^2)^p (a+b arccosh(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.2 Inverse hyperbolic cosine", "7.2.4 (f x)^m (d+e x^2)^p (a+b arccosh(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.2 Inverse hyperbolic cosine", "7.2.5 u (a+b arccosh(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.2 Inverse hyperbolic cosine", "7.2.6 Miscellaneous inverse hyperbolic cosine"}]];

  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.3 Inverse hyperbolic tangent", "7.3.1 (a+b arctanh(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.3 Inverse hyperbolic tangent", "7.3.2 (d x)^m (a+b arctanh(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.3 Inverse hyperbolic tangent", "7.3.3 (d+e x)^m (a+b arctanh(c x^n))^p"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.3 Inverse hyperbolic tangent", "7.3.4 u (a+b arctanh(c x))^p"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.3 Inverse hyperbolic tangent", "7.3.5 u (a+b arctanh(c+d x))^p"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.3 Inverse hyperbolic tangent", "7.3.6 Exponentials of inverse hyperbolic tangent"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.3 Inverse hyperbolic tangent", "7.3.7 Miscellaneous inverse hyperbolic tangent"}]];

  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.5 Inverse hyperbolic secant", "7.5.1 u (a+b arcsech(c x))^n"}]];
  LoadRules[FileNameJoin[{"7 Inverse hyperbolic functions", "7.5 Inverse hyperbolic secant", "7.5.2 Miscellaneous inverse hyperbolic secant"}]];
];
 *)

(* ::Section::Closed:: *)
(*Load Special Function Integration Rules*)

(*
If[TrueQ[$LoadElementaryFunctionRules],
  LoadRules[FileNameJoin[{"8 Special functions", "8.1 Error functions"}]];
  LoadRules[FileNameJoin[{"8 Special functions", "8.2 Fresnel integral functions"}]];
  LoadRules[FileNameJoin[{"8 Special functions", "8.3 Exponential integral functions"}]];
  LoadRules[FileNameJoin[{"8 Special functions", "8.4 Trig integral functions"}]];
  LoadRules[FileNameJoin[{"8 Special functions", "8.5 Hyperbolic integral functions"}]];
  LoadRules[FileNameJoin[{"8 Special functions", "8.6 Gamma functions"}]];
  LoadRules[FileNameJoin[{"8 Special functions", "8.7 Zeta function"}]];
  LoadRules[FileNameJoin[{"8 Special functions", "8.8 Polylogarithm function"}]];
  LoadRules[FileNameJoin[{"8 Special functions", "8.9 Product logarithm function"}]];
(*LoadRules[FileNameJoin[{"8 Special functions", "8.10 Bessel functions"}]]; *)

  LoadRules[FileNameJoin[{"9 Miscellaneous", "9.1 Derivative integration rules"}]]
];
 *)

  LoadRules[FileNameJoin[{"9 Miscellaneous", "9.3 Miscellaneous integration rules"}]];


(* ::Section::Closed:: *)
(* Modify rules to display steps*)


(* Calculate the rule-count directly after all integration rules, because below there are some more rules added that are not integration rules. *)
$RuleCount = Length[DownValues[Int]];


$UseLog = True;


$UseGamma = False;


StatusBarPrint["Modifying "<>ToString[$RuleCount]<>" integration rules to distribute coefficients over sums..."];
FixIntRules[];
StatusBarPrint[""];


If[TrueQ[$LoadShowSteps],
  LoadRules["ShowStep routines"];
  StatusBarPrint["Modifying " <> ToString[$RuleCount] <> " integration rules to display steps..."];
  (*
  StepFunction[Int];
   *)
  StatusBarPrint[""]];


Print["Defined ", $RuleCount," integration rules"]


(* ::Section::Closed:: *)
(* Define Int[u, x, flag]*)


If[TrueQ[$LoadShowSteps],
Int[u_,x_Symbol,flag_] :=
  If[flag===Step,
    Block[{$ShowSteps=True}, Int[u,x]],
  If[flag===Steps,
    Block[{$ShowSteps=True},
    FixedPoint[
      Function[CellPrint[ExpressionCell[#,"Input"]];
      ReplaceAll[#,{Defer[Int]->Int,Defer[Subst]->Subst}]],Int[u,x]]],
  If[flag===Stats,
    Block[{$ShowSteps=False,$StepCounter=0,$RuleList={}},
    With[{result=Int[u,x]},
    Print[{$StepCounter,Length[$RuleList],LeafCount[u],LeafCount[result],N[Length[$RuleList]/LeafCount[u],4]}];
    result]],
  Print["Invalid integration command:"];
  Defer[Int][u,x,flag]]]]
]


(* ::Section::Closed:: *)
(* Define Int[u, {x,a,b}]*)


Int[u_,{x_Symbol,a_,b_}] :=
  If[EqQ[a,b],
    0,
  With[{result=Int[u,x]},
  If[LtQ[a,b],
    Limit[result,x->b,Direction->1]-Limit[result,x->a,Direction->-1],
  If[GtQ[a,b],
    Limit[result,x->b,Direction->-1]-Limit[result,x->a,Direction->1],
  Limit[result,x->b]-Limit[result,x->a]]]]]


Int[{u__},x_Symbol] :=
  Map[Function[Int[#,x]],{u}]


(* Unprotect[Sinc]; Sinc[u_] := Sin[u]/u; Protect[Sinc]; *)
