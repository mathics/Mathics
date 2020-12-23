(* ::Package:: *)

(* ::Title:: *)
(*Command Line Rubi*)


(* ::Subsection:: *)
(*To load Rubi's rational and algebraic function integrator with a command line interface, open this file in Mathematica or Mathics and then issue a Evaluate Initialization Cells command.*)


(* ::Section::Closed:: *)
(*Set control variables*)


Integrator = "Rubi";
$LoadElementaryFunctionRules = False;
PrintProblems = False;
MakeTestSuite = False;
HideKnownProblems = True;
ShowSuboptimal = True;
$IndexRules = False;


RulesDirectory = FileNameJoin[{DirectoryName[System`$InputFileName],"Rules"}];
ProblemsDirectory = FileNameJoin[{DirectoryName[System`$InputFileName],"Problems"}]


PercentTested = 100;


$ValidTest = False;
$Kahan = False;


ShowStepInfo = True;


IntegrationFunction = Int;


UseSimpLite = False;


ShowDeficiencies = True;


ProblemTimeLimit = 25;


SimplifyResult=False;
FullTest=True;              (* Set to False when generating Maple/Mathematica/RBI spreadsheet *)
MaximumSteps=Null;
FlagProblemTime=Null;       (* Seconds of problem time for flagging slow problems *)


RationalFunctionTest=True;
AlgebraicFunctionTest=True;
ElementaryFunctionTest=True;
InverseFunctionTest=True;


$LoadShowSteps=True; ShowSteps=$LoadShowSteps; LoadElementary=$LoadElementaryFunctionRules;


(* ::Section:: *)
(*Load Rubi's integration rules and test routines*)


Print[FileNameJoin[{RulesDirectory,"Load Rubi.m"}]]
Get[FileNameJoin[{RulesDirectory,"Load Rubi.m"}], Trace->True];
