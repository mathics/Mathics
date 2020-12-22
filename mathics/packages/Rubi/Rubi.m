(* ::Package:: *)

(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: Rubi (Rule-Based Integrator) Package *)
(* :Context: Rubi` *)
(* :Author: Albert Rich, Patrick Scheibe *)

(* :Mathematica Version: 7+ *)
(* :Copyright: (c) 2018 Rule-based Integration Organization (https://rulebasedintegration.org/) *)


(* ::Title:: *)
(*Rubi (Rule-Based Integrator) Package*)


BeginPackage["Rubi`"];


(* ::Section::Closed:: *)
(* Usage Messages *)


Int::usage = "Int[expn, var] returns the antiderivative (indefinite integral) of <expn> with respect to <var>.\n" <>
    "Int[{expn1, expn2, ...},var] returns a list of the antiderivatives of <expn1>, <expn2>, ... each with respect to <var>.\n" <>
    "Int[expn, {var, a, b}] returns the limit of the antiderivative of <expn> as <var> approaches <b> minus the limit as <var> approaches <a>. " <>
    "Note that this difference will NOT always equal the definite integral of <expn> from <a> to <b>.";
Dist::usage = "Dist[expn1,expn2,var] distributes <expn1> over <expn2>.";
Subst::usage = "Subst[expn1,var,expn2] substitutes <expn2> for <var> in <expn1>.";
Step::usage = "Step[Int[expn, var]] displays the first step in the integration of <expn> with respect to <var> and returns the intermediate result.";
Steps::usage = "Steps[Int[expn, var]] displays all the steps in the integration of <expn> with respect to <var> and returns the antiderivative.";
Stats::usage = "Stats[Int[expn, var]] prints statistical information of the integration before returning the antiderivative <expn> with respect to <var>." <>
    "It consists of (a) the number of steps used to integrate, (b) the number of distinct rules used, (c) is the leaf count size of the input," <>
    "(d) the leaf count size of the antiderivative, and (e) the rule-to-size ratio of the integration (i.e. the quotient of (b) and (c)).";

$RubiVersion::usage = "$RubiVersion shows the currently loaded version of Rubi.";
RubiRule::usage = "RubiRule is a symbolic wrapper that is used when displaying integration steps.";
RubiIntermediateResult::usage = "RubiIntermediateResult is a symbolic wrapper that is used when displaying integration steps.";
RubiStats::usage = "RubiStats is a symbolic wrapper that contains statistical information about an integration." <>
    "It consists of (a) the number of steps used to integrate, (b) the number of distinct rules used, (c) is the leaf count size of the input," <>
    "(d) the leaf count size of the antiderivative, and (e) the rule-to-size ratio of the integration (i.e. the quotient of (b) and (c)).";
RubiPrintInformation::usage = "RubiPrintInformation is an option to Steps and Stats that prints information if set to True and returns as a list otherwise.";
RubiClearMemoryImages::usage = "RubiClearMemoryImages[] deletes the memory files that are created for each system to speed-up the loading time of the package. " <>
    "The memory files are recreated during the next loading of the Rubi package.";

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
(* Implementation *)


Begin["`Private`"];

$rubiDir = DirectoryName[System`$InputFileName];
$RubiVersion = StringJoin[
  "Rubi ",
  Version /. List@@Get[FileNameJoin[{$rubiDir, "PacletInfo.m"}]]
	       ];
Print["Loading Rubi..."];
(*PrintTemporary["Loading " <> $RubiVersion <> " will take a minute or two. In the future this will take less than a second."];*)

(* If[Not@ValueQ[Global`$LoadShowSteps], *)
(*   $LoadShowSteps = True, *)
(*   $LoadShowSteps = TrueQ[Global`$LoadShowSteps] *)
(* ]; *)
$LoadShowSteps=True;

If[Not@ValueQ[Global`$LoadElementaryFunctionRules],
  $LoadElementaryFunctionRules = True,
  $LoadElementaryFunctionRules = TrueQ[Global`$LoadElementaryFunctionRules]
]

$ruleDir = FileNameJoin[{$rubiDir, "IntegrationRules"}];
$utilityPackage = FileNameJoin[{$rubiDir, "IntegrationUtilityFunctions.m"}];
$stepRoutines = FileNameJoin[{$rubiDir, "ShowStepRoutines.m"}];
$ruleFormatting = FileNameJoin[{$rubiDir, "ShowStepFormatting.m"}];

RubiClearMemoryImages[] := Module[{files = FileNames["*.mx", {FileNameJoin[{$rubiDir, "Kernel"}]}]},
  DeleteFile /@ files;
];

LoadRules::inv = "Could not load file or section: ``";
LoadRules[fileName_String /; FileExtension[fileName] =!= "m"] := LoadRules[FileNameJoin[{$ruleDir, fileName <> ".m"}]];
LoadRules[fileName_String /; FileExistsQ[fileName]] := (
	(*
	  StatusBarPrint["Loading " <> FileBaseName@fileName <> ".m..."];
	  Get[fileName];
	  StatusBarPrint[""]
	 *)
	 Print["Loading " <> FileNameTake@fileName "..."];
	);
LoadRules[arg___] := Message[LoadRules::inv, {arg}];

StatusBarPrint[message_String] := If[$Notebooks, CurrentValue[EvaluationNotebook[], WindowStatusArea] = message];
ClearStatusBar[] := If[$Notebooks, CurrentValue[EvaluationNotebook[], WindowStatusArea] = ""];


(* ::Section::Closed:: *)
(* Load Integration Rules *)


Unprotect[RealNumberQ]; Uprotect[HoldPattern]; Unprotect[Condition];
Unprotect[Int];  Clear[Int];  Clear[Unintegrable];  Clear[CannotIntegrate];

(* The order of loading the rule-files below is crucial to ensure a functional Rubi integrator! *)
LoadRules[$utilityPackage];
  LoadRules[FileNameJoin[{"9 Miscellaneous", "9.1 Integrand simplification rules"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.1 (a+b x)^m"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.2 (a+b x)^m (c+d x)^n"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.3 (a+b x)^m (c+d x)^n (e+f x)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.4 (a+b x)^m (c+d x)^n (e+f x)^p (g+h x)^q"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.1 (a+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.2 (c x)^m (a+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.3 (a+b x^n)^p (c+d x^n)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.4 (e x)^m (a+b x^n)^p (c+d x^n)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.5 (a+b x^n)^p (c+d x^n)^q (e+f x^n)^r"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.6 (g x)^m (a+b x^n)^p (c+d x^n)^q (e+f x^n)^r"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.1 (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.2 (d+e x)^m (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.3 (d+e x)^m (f+g x) (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.4 (d+e x)^m (f+g x)^n (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.5 (a+b x+c x^2)^p (d+e x+f x^2)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.6 (g+h x)^m (a+b x+c x^2)^p (d+e x+f x^2)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.7 (a+b x+c x^2)^p (d+e x+f x^2)^q (A+B x+C x^2)"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.1 (a+b x^2+c x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.2 (d x)^m (a+b x^2+c x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.3 (d+e x^2)^q (a+b x^2+c x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.4 (f x)^m (d+e x^2)^q (a+b x^2+c x^4)^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.1 (a+b x^n+c x^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.2 (d x)^m (a+b x^n+c x^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.3 (d+e x^n)^q (a+b x^n+c x^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.4 (f x)^m (d+e x^n)^q (a+b x^n+c x^(2 n))^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.7 P(x) (a+b x)^m (c+d x)^n (e+f x)^p (g+h x)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.6 P(x) (a+b x)^m (c+d x)^n (e+f x)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.5 P(x) (a+b x)^m (c+d x)^n"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.9 P(x) (d+e x)^m (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.8 P(x) (a+b x+c x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.6 P(x) (d x)^m (a+b x^2+c x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.5 P(x) (a+b x^2+c x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.7 P(x) (d+e x^2)^q (a+b x^2+c x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.8 P(x) (d+e x)^q (a+b x^2+c x^4)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.6 P(x) (d x)^m (a+b x^n+c x^(2 n))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.5 P(x) (a+b x^n+c x^(2 n))^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.y P(x) (c x)^m (a+b x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.x P(x) (a+b x^2)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.8 P(x) (c x)^m (a+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.7 P(x) (a+b x^n)^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.1 (a x^q+b x^n+c x^(2 n-q))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.2 (d x)^m (a x^q+b x^n+c x^(2 n-q))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.3 (d+e x^(n-q)) (a x^q+b x^n+c x^(2 n-q))^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.4 (f x)^m (d+e x^(n-q)) (a x^q+b x^n+c x^(2 n-q))^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Miscellaneous", "1.3.4 Normalizing algebraic functions"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 Improper", "1.1.4.1 (a x^j+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 Improper", "1.1.4.2 (c x)^m (a x^j+b x^n)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 Improper", "1.1.4.3 (e x)^m (a x^j+b x^k)^p (c+d x^n)^q"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 Improper", "1.1.4.4 P(x) (c x)^m (a x^j+b x^n)^p"}]];

  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Miscellaneous", "1.3.1 P(x)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Miscellaneous", "1.3.2 P(x) Q(x)^p"}]];
  LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Miscellaneous", "1.3.3 Miscellaneous algebraic functions"}]];
  LoadRules[FileNameJoin[{"9 Miscellaneous", "9.3 Piecewise linear functions"}]];

If[$LoadElementaryFunctionRules===True,
  LoadRules[FileNameJoin[{"2 Exponentials", "2.1 (c+d x)^m (a+b (F^(g (e+f x)))^n)^p"}]];
  LoadRules[FileNameJoin[{"2 Exponentials", "2.2 (c+d x)^m (F^(g (e+f x)))^n (a+b (F^(g (e+f x)))^n)^p"}]];
  LoadRules[FileNameJoin[{"2 Exponentials", "2.3 Miscellaneous exponentials"}]];

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
  LoadRules[FileNameJoin[{"4 Trig functions", "4.1 Sine", "4.1.7 (d trig)^m (a+b (c sin)^n)^p"}]];
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

  LoadRules[FileNameJoin[{"9 Miscellaneous", "9.2 Derivative integration rules"}]]
];
LoadRules[FileNameJoin[{"9 Miscellaneous", "9.4 Miscellaneous integration rules"}]];

(* ::Section::Closed:: *)
(* Modify rules to display steps*)


(* Calculate the rule-count directly after all integration rules, because below there are some more rules
 added that are not integration rules*)
$RuleCount = Length[DownValues[Int]];

If[$LoadShowSteps === True,
  LoadRules[$stepRoutines];
];

(*StatusBarPrint["Modifying " <> ToString[$RuleCount] <> " integration rules to distribute coefficients over sums..."];*)
Print["integration rules to distribute coefficients over sums..."]
FixIntRules[];


(*If[$LoadShowSteps === True,
  StatusBarPrint["Modifying " <> ToString[$RuleCount] <> " integration rules to display steps..."];
  StepFunction[Int];
];
 *)
Print["integration rules to display steps..."]
StepFunction[Int];


(* ::Section::Closed:: *)
(* Define Steps, Step and Stats*)


Int::noShowSteps = "To use this function, you need to define $LoadShowSteps=True before loading the Rubi package";
Steps::negSteps = "Number of steps must be a positive integer.";
SetAttributes[Steps, {HoldFirst}];
Options[Steps] = {
  RubiPrintInformation -> True
};
Int::wrngUsage = "Wrong usage of the `1` function. Please use `1`[Int[expr, x]].";
Steps[Int[expr_, x_], opts : OptionsPattern[]] := Steps[Int[expr, x], $IterationLimit, opts];
Steps[Int[expr_, x_], n_Integer, OptionsPattern[]] := Module[{result, steps},
  If[$LoadShowSteps =!= True,
    Message[Int::noShowSteps];
    Return[Int[expr, x]]
  ];
  {result, steps} = Reap@Block[{$ShowSteps = True},
    FixedPoint[
      Function[int,
        With[{held = ReplaceAll[HoldComplete[int], {Defer[Int] -> Int, Defer[Dist] -> Dist, Defer[Subst] -> Subst}]},
          Sow[RubiIntermediateResult[held]];
          ReleaseHold[held]
        ]
      ], Int[expr, x],
      n - 1
    ]
  ];
  If[OptionValue[RubiPrintInformation] === True,
    PrintRubiSteps[steps];
    result,
    {steps, result}
  ]
] /; Head[x] === Symbol && If[TrueQ[n > 0], True, Message[Steps::negSteps]; False];
Steps[___] := (Message[Int::wrngUsage, Steps]; $Failed);

SetAttributes[Step, {HoldFirst}];
Options[Step] = {
  RubiPrintInformation -> True
};
Step[Int[expr_, x_], OptionsPattern[]] := Module[
  {
    result,
    step
  },
  If[$LoadShowSteps =!= True,
    Message[Int::noShowSteps];
    Return[Int[expr, x]]
  ];
  {result, step} = Reap@Block[{$ShowSteps = True}, Int[expr, x]];
  If[OptionValue[RubiPrintInformation] === True,
    PrintRubiSteps[step];
    result,
    {step, result}
  ]
] /; Head[x] === Symbol;
Step[___] := (Message[Int::wrngUsage, Step]; $Failed);


SetAttributes[Stats, {HoldFirst}];
Options[Stats] = {
  RubiPrintInformation -> True
};
Stats[Int[expr_, x_], OptionsPattern[]] := Block[{$ShowSteps = False, $StepCounter = 0, $RuleList = {}},
  With[{result = Int[expr, x]},
    If[$LoadShowSteps =!= True,
      Message[Int::noShowSteps];
      Return[result]
    ];
    If[OptionValue[RubiPrintInformation] === True,
      Print@RubiStats@{$StepCounter, Length[$RuleList], LeafCount[expr], LeafCount[result], N[Length[$RuleList] / LeafCount[expr], 4], $RuleList};
      result,
      {
        RubiStats@{$StepCounter, Length[$RuleList], LeafCount[expr], LeafCount[result], N[Length[$RuleList] / LeafCount[expr], 4], $RuleList},
        result
      }
    ]
  ]] /; Head[x] === Symbol;
Stats[___] := (Message[Int::wrngUsage, Stats]; $Failed);



(* Print a warning when users use the old style routines for showing steps *)
Int::oldFlag = "The usage Int[expr_, x_, `1`] is depreciated. Please use `1`[Int[expr_, x_]].";
Int[e_, x_, flag : (Stats | Step | Steps)] := flag[Int[e, x]] /; (Message[Int::oldFlag, flag]; True);


(* ::Section::Closed:: *)
(* Define Unintegrable and CannotIntegrate*)


Int[u_, {x_Symbol, a_, b_}] := With[{result = Int[u, x]}, Limit[result, x -> b] - Limit[result, x -> a]];
Int[{u__}, x_Symbol] := Map[Function[Int[#, x]], {u}];

Protect[Int];
Protect[RealNumberQ]; Protect[HoldPattern]; Protect[Condition];

$Unintegrable = False;
Unintegrable[u_, x_] :=
    If[$Unintegrable === True,
      Defer[Unintegrable][u, x],
      Defer[Int][u, x]
    ];


CannotIntegrate[u_, x_] := Defer[Int][u, x];
LoadRules[$ruleFormatting];
ClearStatusBar[];


End[];
EndPackage[ ];
