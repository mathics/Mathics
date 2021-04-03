(* ::Package:: *)

(* SyntaxAnnotations package

Copyright (c) 2015 - 2019 Jakub Kuczmarski <Jakub.Kuczmarski@gmail.com>
Released under The MIT License
https://github.com/jkuczm/MathematicaSyntaxAnnotations/blob/master/LICENSE *)


(*	Begining of package context with ` is intentional - it allows loading
	SyntaxAnnotations as a sub-package. Apropriate steps to load package with
	SyntaxAnnotations` as "top level" context are taken in Kernel/init.m.
	This is inspired by system used in https://github.com/szhorvat/LTemplate *)
BeginPackage["TeXUtilities`CellsToTeX`SyntaxAnnotations`"]


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Usage messages*)


AnnotateSyntax::usage =
"\
AnnotateSyntax[boxes] \
returns boxes with certain subboxes wrapped with boxes identifying their \
syntactic role."


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"]


ClearAll["`*"]


(* ::Subsection:: *)
(*Private symbols usage*)


undefinedSymbolQ::usage =
"\
undefinedSymbolQ[symbol] \
returns True if symbol is undefined. Returns False otherwise.\

undefinedSymbolQ[\"name\"] \
returns True if symbol, with given \"name\", is undefined. Returns False \
otherwise."


symbolNameQ::usage =
"\
symbolNameQ[\"str\"] \
returns True if given String is valid symbol name, returns False otherwise."


whitespaceQ::usage =
"\
whitespaceQ[\"str1\", \"str2\", ...] \
returns True if given strings are empty or contain only whitespace or \
\\[IndentingNewLine] characters, returns False otherwise."


syntaxBox::usage =
"\
syntaxBox[boxes, {{type1, subtype1, ...}, {type2, subtype2, ...}, ...}] \
represents boxes that in an expression perform sytnax roles of given types."


extractSymbolName::usage =
"\
extractSymbolName[\"str\"] \
returns a List. For \"str\" being valid symbol name returned list conatins \
this name. For box representation of Blank... pattern with head, returned \
list contains this head. For box representation of pattern with name, \
returned list contains this name. If none of above is true returned list is \
empty."


extractLocalVariableNames::usage =
"\
extractLocalVariableNames[\"type\"][boxes1, boxes2, ...] \
returns a List of strings with names of all local symbols extracted from \
given boxes. \
\"type\" can be any of types from \"LocalVariables\" property of \
SyntaxInformation, with addition of: \
\"Scoping\" type, applicable to With, Module and Block; \
\"LowerBound\" applicable to \[Sum] and \[Product]; \
\"IntegrateDifferentialD\" applicable to \[Integral]; \
\"PatternName\" for rules and set expressions. \
boxesi are treated as box representation of function arguments, they don't \
need to represent valid mathematica expressions."


extractArgs::usage =
"\
extractArgs[boxes, {imin, imax}] \
returns List of box representations of function arguments, with positions \
from imin to imax, extracted from boxes treated as representation of sequence \
of arguments.\

extractArgs[boxes, {i}] \
returns List containing one element: box representation of function argument, \
with position i.\

extractArgs[boxes, 0] \
returns List containing given boxes.\

boxes don't need to represent valid mathematica expression. \
Second argument of extractArgs accepts same values as second element of \
\"LocalVariables\" property of SyntaxInformation with addition of 0. \
All occurrences of syntaxBox are stripped."


extendedSyntaxInformation::usage =
"\
extendedSyntaxInformation[\"name\"] \
returns SyntaxInformation for symbol with given \"name\". \
extendedSyntaxInformation for some built-in symbols is enriched with \
properties that are missing from ordinary SyntaxInformation, yet equivalent \
functionality is implemented by other means.\

extendedSyntaxInformation[\"op1\", \"op2\"] \
returns SyntaxInformation for operators composed of two parts."


$inFunction::usage =
"\
$inFunction \
is True if parser is currently inside box expression representing \
one-argument Function, otherwise it's false."


$directlyInScopingRHS::usage =
"\
$directlyInScopingRHS \
is True if parser is currently directly inside box expression representing \
right hand side of assignment in variable specification of With, Module or
Block, otherwise it's false."


patternNameTypes::usage =
"\
patternNameTypes[name] \
returns List of syntax types assigned to pattern with given name. DownValues \
of this function are changed during parsing."


stringBoxTypes::usage =
"\
stringBoxTypes[str] \
returns List of syntax types assigned to given box String str. DownValues \
of this function are changed during parsing."


deafultStringBoxTypes::usage =
"\
deafultStringBoxTypes[str] \
returns List of syntax types assigned to given box String str, excluding \
contex dependend types assigned during parsing."


modifyTypes::usage =
"\
modifyTypes[register, type, symNames] \
adds given type to syntax types of given symbol names symNames in given \
register. Possible registers are patternNameTypes and stringBoxTypes.\

modifyTypes[{register1, register2, ...}, type, symNames] \
registers types in all given registers."


withModifiedTypes::usage =
"\
withModifiedTypes[{type, symNames}, body] \
evaluates body with with given syntax type added to types, of given symbol \
names symNames, in patternNameTypes and stringBoxTypes registers.

withModifiedTypes[{register1, register2, ...}, {type, symNames}, body] \
registers changes in all given registers."


withLocalVariables::usage =
"\
withLocalVariables[{funcName, localVarsSym, argumentBoxes}, body] \
when used on right hand side of SetDelayed assigns to left hand side given \
body wrapped with condition that causes definition to apply only when given \
funcName has defined extendedSyntaxInformation with \"LocalVariables\" \
element. body can contain symbol given as localVarsSym and it will be \
replaced by list of box representations of local variables extracted from \
argumentBoxes."


parseScopingVars::usage =
"\
parseScopingVars[mode, funcName, localVars][boxes] \
returns given boxes with symbol names, given in localVars List, wrapped with \
syntaxBox identifying their role as local variables of scoping construct. \
Appearances of symbol names, from localVars, on right hand side of \
assignments present directly in given boxes are annotated differently than \
other appearances of those symbol names."


parse::usage =
"\
parse[mode][boxes] \
returns given boxes with certain subboxes wrapped with syntaxBox identifying \
their syntactic role, governed by given parsing mode."


posExprPosition::usage =
"\
posExprPosition[posExpr] \
returns position encoded in given \"position expression\" posExpr.\

posExprPosition[posExpr, i] \
returns position encoded in posExpr with last i elements droped."


normalizeAnnotationTypes::usage =
"\
normalizeAnnotationTypes[types] \
returns List of strings with names of syntax types used by Mathematica from \
given List of annotation types used internally by this package."


syntaxStyleBox::usage =
"\
syntaxStyleBox[boxes, annotationTypes] \
returns StyleBox containing given boxes with proper style for given List of \
annotationTypes."


$stringBoxToTypes::usage =
"\
$stringBoxToTypes \
is List of default rules converting string boxes to annotation types."


(* ::Subsection:: *)
(*operator groups*)


$patternOperators = "=" | "^=" | "->" | "\[Rule]"

$patternDelayedOperators = ":=" | "^:=" | ":>" | "\[RuleDelayed]"

$assignmentOperators = "=" | ":=" | "^=" | "^:="

$ruleOperators = "->" | "\[Rule]" | ":>" | "\[RuleDelayed]"


(* ::Subsection:: *)
(*symbolNameQ*)


symbolNameQ["Null"] = True

symbolNameQ[str_String] :=
	And[
		StringFreeQ[str, WhitespaceCharacter],
		MatchQ[
			Quiet @ MakeExpression[str, StandardForm],
			HoldComplete[Except[Null | Symbol[___], _Symbol]]
		]
	]

symbolNameQ[_] = False


(* ::Subsection:: *)
(*whitespaceQ*)


whitespaceQ =
	StringMatchQ[
		StringJoin[##],
		(WhitespaceCharacter | "\[IndentingNewLine]")...
	]&


(* ::Subsection:: *)
(*undefinedSymbolQ*)


SetAttributes[undefinedSymbolQ, HoldFirst]


undefinedSymbolQ[
	s : _String | _Symbol /; Quiet[Context[s], Context::notfound] === "System`"
] = False

undefinedSymbolQ[name_String /; StringFreeQ[name, WhitespaceCharacter]] :=
	With[{heldSym = Quiet @ MakeExpression[name, StandardForm]},
		undefinedSymbolQ @@ heldSym /;
			MatchQ[heldSym, HoldComplete[Except[Null | Symbol[___], _Symbol]]]
	]

undefinedSymbolQ[sym_Symbol] :=
	And @@ (#@sym === {}& /@ {
		OwnValues, SubValues, UpValues, DownValues, NValues, FormatValues,
		DefaultValues, Messages
	})

undefinedSymbolQ[_] = False


(* ::Subsection:: *)
(*extractSymbolName*)


extractSymbolName[str_String] :=
	Select[StringSplit[str, "_"], symbolNameQ, 1]


(* ::Subsection:: *)
(*extractLocalVariableNames*)


extractLocalVariableNames[_][Repeated[_, {0, 1}]] = {}

extractLocalVariableNames[type_][argsBoxes__] :=
	extractLocalVariableNames[type] /@ {argsBoxes} // Flatten

extractLocalVariableNames["Table" | "Plot" | "Integrate"][
	RowBox[{"{", RowBox[{name_String?symbolNameQ, ",", ___}], "}"}]
] :=
	{name}

extractLocalVariableNames["Solve" | "Integrate"][name_String?symbolNameQ] :=
	{name}

extractLocalVariableNames["Solve"][
	RowBox[{"{", RowBox[argBoxes : {__}], "}"}]
] :=
	Cases[argBoxes, _String?symbolNameQ] // Flatten

extractLocalVariableNames["Solve"][
	RowBox[{"{", name_String?symbolNameQ, "}"}]
] :=
	{name}

extractLocalVariableNames["Limit"][
	RowBox[{name_String?symbolNameQ, "\[Rule]" | "->", __}]
] :=
	{name}

extractLocalVariableNames["Manipulate"][
	RowBox[{"{",
		RowBox[{
			name_String?symbolNameQ |
				RowBox[{"{",
					RowBox[{name_String?symbolNameQ, ",", ___}],
				"}"}],
			",",
			___
		}],
	"}"}]
] :=
	{name}

extractLocalVariableNames["LowerBound"][
	RowBox[{name_String?symbolNameQ, $assignmentOperators, ___}]
] :=
	{name}

extractLocalVariableNames["LowerBound"][boxes_] :=
	Cases[boxes, _?symbolNameQ, {2}] // Flatten

extractLocalVariableNames["IntegrateDifferentialD"][
	RowBox[{"\[DifferentialD]", name_String?symbolNameQ}]
] :=
	{name}

(*	\[Function] extracts symbol names and non-named blank heads as local
	variables from everything except:
	* func[...] with func having any kind of local variable specification,
	* LHS of \[Function] constructs,
	* RHS of assignments.

	Function accepts symbol and non-named blank head as single local variable,
	and from a List of potential variables extacts same things as \[Function].

	Scoping functions extract symbol names and non-named blank heads as local
	variables from a List with same exceptions as \[Function]. From LHS of
	assignment, pattern names are also extracted, so we use two "inner modes"
	of extraction for scoping:
	* {"Scoping", "List"} for boxes inside List, but not on LHS of assignment,
	* {"Scoping", "LHS"} for boxes on LHS of assignment inside List.

	Assignment functions extract pattern names, as local variables, from
	everything except RHS of inner assignments.

	Non assignmet patterns, i.e. rules and infix Condition, extract pattern
	names as local variables from everything with same exceptions as
	\[Function]. *)

extractLocalVariableNames["Scoping"][RowBox[{"{", argBoxes_, "}"}]] :=
	extractLocalVariableNames[{"Scoping", "List"}][argBoxes]

extractLocalVariableNames["Function"][RowBox[{"{", argBoxes_, "}"}]] :=
	extractLocalVariableNames["\[Function]"][argBoxes]

extractLocalVariableNames[
	type : {"Scoping", _} | "NonAssigPatt" | "\[Function]"
][
	RowBox[{
		funcName_String /;
			MemberQ[extendedSyntaxInformation[funcName],
				"LocalVariables" -> _
			],
		"[",
		__
	}]
] :=
	extractLocalVariableNames[type][funcName]

extractLocalVariableNames[
	type : {"Scoping", _} | "NonAssigPatt" | "\[Function]"
][
	RowBox[{__, "\[Function]", rhs__}]
] :=
	extractLocalVariableNames[type] /@ {rhs} // Flatten

extractLocalVariableNames[{"Scoping", _}][
	RowBox[{lhs__, $assignmentOperators, __}]
] :=
	extractLocalVariableNames[{"Scoping", "LHS"}] /@ {lhs} // Flatten

extractLocalVariableNames[
	type : "NonAssigPatt" | "Assignment" | "\[Function]"
][
	RowBox[{lhs__, $assignmentOperators, __}]
] :=
	extractLocalVariableNames[type] /@ {lhs} // Flatten

extractLocalVariableNames[type : "NonAssigPatt" | "Assignment"][
	RowBox[{name_String, ":", rest__}]
] :=
	{
		extractSymbolName[name],
		extractLocalVariableNames[type] /@ {rest}
	} // Flatten

extractLocalVariableNames[type : {"Scoping", "List"} | "\[Function]"][
	RowBox[{_String, ":", rest__}]
] :=
	extractLocalVariableNames[type] /@ {rest} // Flatten

extractLocalVariableNames[{"Scoping", "List"} | "Function" | "\[Function]"][
	name_String /; StringMatchQ[name, "_"... ~~ Except["_"]...]
] :=
	extractSymbolName[name]

extractLocalVariableNames[{"Scoping", "LHS"}][name_String] :=
	extractSymbolName[name]

extractLocalVariableNames["NonAssigPatt" | "Assignment"][
	(name_String /; StringMatchQ[name, Except["_"].. ~~ "_" ~~ ___])
] :=
	extractSymbolName[name]

extractLocalVariableNames[
	type : {"Scoping", _} | "NonAssigPatt" | "Assignment" | "\[Function]"
][boxes:(_[__])] :=
	extractLocalVariableNames[type] /@ List @@ boxes // Flatten


(* ::Subsection:: *)
(*extractArgs*)


extractArgs[syntaxBox[arg_, _], spec_] := extractArgs[arg, spec]

extractArgs[boxes_, 0] := {boxes} /. syntaxBox[var_, _] :> var

extractArgs[arg_String, {min_, max_} /; min <= 1 <= max] := {arg}

extractArgs[RowBox[argsBoxes:{___}], {min_Integer, max:_Integer|Infinity}] :=
	Module[{args, $previousComma = True},
		args = argsBoxes /. syntaxBox[var_, _] :> var;
		(*	It's possible that RowBox contains adjacent comma strings, or comma
			string as first or last element. Such boxes are parsed, the same as
			if there was empty or whitespace only String between commas, to
			expression with Null argument betwenn commas, or before/after comma
			if it is first/last in row. To take this into account in arguments
			counting, we put empty strings in relevant places. *)
		args =
			Replace[args, {
				"," :>
					If[$previousComma,
						$previousComma = True;
						""
					(* else *),
						$previousComma = True;
						Unevaluated@Sequence[]
					],
				x_ :> ($previousComma = False; x)
			}, {1}];
		If[$previousComma,
			AppendTo[args, ""]
		];
		Take[args, {Max[1, min], Min[Length[args], max]}]
	]

extractArgs[argsBoxes_, {i_}] := extractArgs[argsBoxes, {i, i}]

extractArgs[_, {_, _}] = {}


(* ::Subsection:: *)
(*extendedSyntaxInformation*)


extendedSyntaxInformation[symName : "Block" | "Module" | "With"] :=
	Append[
		SyntaxInformation[Symbol[symName]],
		"LocalVariables" -> {"Scoping", {1}}
	]

extendedSyntaxInformation[symName : "Function" | "\[Function]"] :=
	Append[
		SyntaxInformation[Function],
		"LocalVariables" -> {symName, {1}}
	]

extendedSyntaxInformation["\[Sum]" | "\[Product]"] :=
	{"LocalVariables" -> {"LowerBound", 0}}

extendedSyntaxInformation["\[Integral]"] :=
	{"LocalVariables" -> {"IntegrateDifferentialD", {2, \[Infinity]}}}

extendedSyntaxInformation[$ruleOperators | "/;"] :=
	{"LocalVariables" -> {"NonAssigPatt", {1}}}

extendedSyntaxInformation[$assignmentOperators] :=
	{"LocalVariables" -> {"Assignment", {1}}}

extendedSyntaxInformation["/:", $assignmentOperators] :=
	{"LocalVariables" -> {"Assignment", {1, 2}}}

extendedSyntaxInformation[symName_String] :=
	SyntaxInformation[Quiet[Symbol[symName], Symbol::symname]]


(* ::Subsection:: *)
(*$inFunction*)


$inFunction = False


(* ::Subsection:: *)
(*$directlyInScopingRHS*)


$directlyInScopingRHS = False


(* ::Subsection:: *)
(*patternNameTypes*)


patternNameTypes[_] = {}


(* ::Subsection:: *)
(*stringBoxTypes*)


stringBoxTypes[str_] :=
	With[{split = StringSplit[str, "_", 2]},
		patternNameTypes @ First[split] /; MatchQ[split, {Except[""], _}]
	]

stringBoxTypes[_] = {}


(* ::Subsection:: *)
(*deafultStringBoxTypes*)


deafultStringBoxTypes[_] = {}


(* ::Subsection:: *)
(*modifyTypes*)


modifyTypes[registers_List, type_, symNames_List] :=
	Scan[modifyTypes[#, type, symNames]&, registers]

modifyTypes[stringBoxTypes, type_, symNames_List] :=
	Scan[
		(
			AppendTo[stringBoxTypes[#], type];

			stringBoxTypes["_" <> #] =
			stringBoxTypes["__" <> #] =
			stringBoxTypes["___" <> #] =
				Append[stringBoxTypes["_" <> #], type];
		)&,
		symNames
	]

modifyTypes[patternNameTypes, type_, symNames_List] :=
	Scan[AppendTo[patternNameTypes[#], type]&, symNames]


(* ::Subsection:: *)
(*withModifiedTypes*)


SetAttributes[withModifiedTypes, HoldRest]

withModifiedTypes[
	heads_List:{patternNameTypes, stringBoxTypes},
	{type_, symNames_List},
	body_
] :=
	Internal`InheritedBlock[heads,
		modifyTypes[heads, type, symNames];

		body
	]


(* ::Subsection:: *)
(*withLocalVariables*)


SetAttributes[withLocalVariables, {HoldAll, SequenceHold}]


withLocalVariables /: Verbatim[SetDelayed][
	lhs_,
	withLocalVariables[{funcName_, localVarsSym_Symbol, argumentBoxes_}, body_]
] := (
	lhs :=
		With[
			{
				localVariables =
					"LocalVariables" /. extendedSyntaxInformation[funcName]
			}
			,
			With[
				{
					localVarsSym =
						DeleteDuplicates[
							extractLocalVariableNames[localVariables[[1]]] @@
								extractArgs[argumentBoxes, localVariables[[2]]]
						]
				}
				,
				body
			] /; Length[localVariables] >= 2
		]
)


(* ::Subsection:: *)
(*parseScopingVars*)


(*	List in variable specificaition can contain comma separated sequence of
	variable specifications. *)
parseScopingVars[mode_, funcName_, localVars_][RowBox[boxes:{_, ",", ___}]] :=
	RowBox[parseScopingVars[mode, funcName, localVars] /@ boxes]

(*	List in variable specificaition can contain an assignment. *)
parseScopingVars[mode_, funcName_, localVars_][
	RowBox[{lhs__, op:$assignmentOperators, rhs___}]
] :=
	RowBox@Join[
		withModifiedTypes[{{"Scoping", funcName}, localVars},
			parse[mode] /@ {lhs, op}
		]
		,
		Block[{$directlyInScopingRHS = True},
			withModifiedTypes[{{"Scoping", funcName, "RHS"}, localVars},
				parse[mode] /@ {rhs}
			]
		]
	]

(*	Anything else is parsed with annotated local variables of this scope. *)
parseScopingVars[mode_, funcName_, localVars_][boxes___] :=
	withModifiedTypes[{{"Scoping", funcName}, localVars},
		Sequence @@ (parse[mode] /@ {boxes})
	]



(* ::Subsection:: *)
(*parse*)


parse[_][str_String] :=
	Module[{types = stringBoxTypes[str]},
		If[types =!= {},
			If[!$directlyInScopingRHS,
				PrependTo[types, "NotDirectlyInScopingRHS"]
			];

			syntaxBox[str, types]
		(* else *),
			str
		]
	]

parse[_][boxes_?AtomQ] := boxes

parse[mode_][RowBox[{name_String, ":", rest__}]] :=
	RowBox@Join[
		{
			Module[
				{
					types =
						Join[
							deafultStringBoxTypes[name],
							patternNameTypes @@ extractSymbolName[name]
						]
				},
				If[types =!= {},
					If[!$directlyInScopingRHS,
						PrependTo[types, "NotDirectlyInScopingRHS"]
					];

					syntaxBox[name, types]
				(* else *),
					name
				]
			],
			":"
		},
		parse[mode] /@ {rest}
	]

parse[_][RowBox[{"::"}]] = RowBox[{"::"}]

parse[mode_][
	RowBox[{sym___, "::", tag___, "::", lang___, "::", excess___}]
] :=
	RowBox@Join[
		parse[mode] /@ {sym},
		{If[{sym} === {}, syntaxBox["::", {"SyntaxError"}], "::"]},
		syntaxBox[#, {"String"}] & /@ {tag},
		{"::"},
		syntaxBox[#, {"String"}] & /@ {lang},
		{syntaxBox["::", {"ExcessArgument"}]},
		syntaxBox[#, {"ExcessArgument"}] & /@ {excess}
	]

parse[mode_][RowBox[{sym___, "::", tag___, "::", lang___}]] :=
	RowBox@Join[
		parse[mode] /@ {sym},
		{If[{sym} === {}, syntaxBox["::", {"SyntaxError"}], "::"]},
		syntaxBox[#, {"String"}] & /@ {tag},
		{If[{lang} === {}, syntaxBox["::", {"SyntaxError"}], "::"]},
		syntaxBox[#, {"String"}] & /@ {lang}
	]

parse[mode_][RowBox[{sym___, "::", tag___}]] :=
	RowBox@Join[
		parse[mode] /@ {sym},
		{
			If[{sym} === {} || {tag} === {},
				syntaxBox["::", {"SyntaxError"}]
			(* else *),
				"::"
			]
		},
		syntaxBox[#, {"String"}] & /@ {tag}
	]

parse[mode:Except["AssignmentLHS"]][
	RowBox[{
		funcName : "With" | "Module" | "Block", "[",
			args:RowBox[{
				RowBox[{"{", varSpecBoxes_ | PatternSequence[], "}"}],
				",",
				restArgs___
			}],
		"]"
	}]
] :=
	withLocalVariables[{funcName, localVars, args},
		RowBox[{
			parse[mode][funcName]
			,
			"["
			,
			(*	Being inside inner scoping construct overrides effects of being
				on RHS of assignment in variable specification of outer scoping
				construct. *)
			Block[{$directlyInScopingRHS = False},
				RowBox[{
					RowBox[{"{",
						(*	Symbols on RHS of assignments in scoping construct
							variable specification are not local variables of
							this scoping construct, switch to special
							sub-parser that takes this into account. *)
						parseScopingVars[mode, funcName, localVars][
							varSpecBoxes
						],
					"}"}],
					",",
					withModifiedTypes[{{"Scoping", funcName}, localVars},
						Sequence @@ parse[mode][{restArgs}]
					]
				}]
			]
			,
			"]"
		}]
	]

parse[mode_][
	RowBox[boxes : (
		{"Function", "[", Except[RowBox[{_, ",", ___}]], "]"} |
		{_, "&"}
	)]
] :=
	Block[{$inFunction = True},
		RowBox[parse[mode] /@ boxes]
	]

parse[mode:Except["AssignmentLHS"]][
	RowBox[{
		funcName:Except["With" | "Module" | "Block", _String], "[", args_, "]"
	}]
] :=
	withLocalVariables[{funcName, localVars, args},
		RowBox[{
			parse[mode][funcName],
			"[",
			(*	Being inside function having local variable specification
				overrides effects of being on RHS of assignment in variable
				specification of outer scoping construct. *)
			Block[{$directlyInScopingRHS = False},
				withModifiedTypes[
					{
						{Replace[funcName, {
							"Function" -> "PatternVariable",
							_ -> "FunctionLocalVariable"
						}], funcName},
						localVars
					}
					,
					parse[mode][args]
				]
			],
			"]"
		}]
	]

parse[mode:Except["AssignmentLHS"]][RowBox[{lhs_, "\[Function]", rhs_}]] :=
	withLocalVariables[{"\[Function]", localVars, RowBox[{lhs, ",", rhs}]},
		RowBox[{
			(*	Being inside LHS of \[Function] overrides effects of being on
				RHS of assignment in variable specification of outer scoping
				construct. *)
			Block[{$directlyInScopingRHS = False},
				withModifiedTypes[
					{{"PatternVariable", "\[Function]"}, localVars},
					parse[mode][lhs]
				]
			],
			parse[mode]["\[Function]"],
			withModifiedTypes[{{"PatternVariable", "\[Function]"}, localVars},
				parse[mode][rhs]
			]
		}]
	]

parse[mode:Except["AssignmentLHS"]][
	RowBox[boxes:(
		{UnderoverscriptBox[funcName:"\[Sum]" | "\[Product]", args_, _], _} |
		{
			SubsuperscriptBox[funcName : "\[Integral]", _, _] |
				funcName : "\[Integral]"
			,
			args_
		}
	)]
] :=
	withLocalVariables[{funcName, localVars, args},
		(*	Being inside function having local variable specification overrides
			effects of being on RHS of assignment in variable specification
			of outer scoping construct. *)
		Block[{$directlyInScopingRHS = False},
			withModifiedTypes[{{"FunctionLocalVariable", funcName}, localVars},
				RowBox[parse[mode] /@ boxes]
			]
		]
	]

parse[mode:Except["AssignmentLHS"]][
	RowBox[{lhs_, funcName:$ruleOperators, rhs_}]
] :=
	withLocalVariables[{funcName, localVars, RowBox[{lhs, ",", rhs}]},
		RowBox[{
			withModifiedTypes[{patternNameTypes},
				{{"PatternVariable", funcName, "LHS"}, localVars},
				parse[mode][lhs]
			]
			,
			parse[mode][funcName]
			,
			If[MatchQ[funcName, $patternDelayedOperators],
				(*	Patterns, on right hand side of delayed rule, with same
					name as one of patterns on left hand side, are marked as
					local scope conflict not as pattern variables. *)
				withModifiedTypes[{patternNameTypes},
					{{"LocalScopeConflict", funcName, "RHS"}, localVars},
					withModifiedTypes[{stringBoxTypes},
						{{"PatternVariable", funcName, "RHS"}, localVars},
						parse[mode][rhs]
					]
				]
			(* else *),
				parse[mode][rhs]
			]
		}]
	]

parse[mode_][RowBox[boxes : {patt_, "/;", test_}]] :=
	withLocalVariables[{"/;", localVars, RowBox[{patt, ",", test}]},
		withModifiedTypes[{{"PatternVariable", "/;"}, localVars},
			RowBox[parse[mode] /@ boxes]
		]
	]

parse[mode_][
	boxes : RowBox[{
		PatternSequence[tag_, tagSep:"/:"] | PatternSequence[],
		lhs_,
		funcName:$assignmentOperators,
		rhs_
	}]
] :=
	withLocalVariables[
		{
			Sequence[tagSep, funcName],
			localVars,
			Replace[boxes, "/:" | $assignmentOperators -> ",", {2}]
		}
		,
		RowBox[{
			withModifiedTypes[{patternNameTypes},
				{{"PatternVariable", funcName, "LHS"}, localVars}
				,
				(*	Constructs inside LHS of assignments don't change syntax
					roles with exception of other assignments, so we switch to
					"AssignmentLHS" parsing mode and set $inFunction to
					False. *)
				Sequence @@ Block[{$inFunction = False},
					parse["AssignmentLHS"] /@ {tag, tagSep, lhs}
				]
			]
			,
			parse[mode][funcName]
			,
			(*	Being on RHS of inner assignment overrides effects of being
				on RHS of assignment in variable specification of outer scoping
				construct. *)
			Block[{$directlyInScopingRHS = False},
				(*	Being on RHS of inner assignment overrides effects of being
					on LHS of outer assignment, so switch to "Main" parsing
					mode. *)
				If[MatchQ[funcName, $patternDelayedOperators],
					(*	Patterns, on right hand side of delayed assignment,
						with same name as one of patterns on left hand side,
						are marked as local scope conflict not as pattern
						variables. *)
					withModifiedTypes[{patternNameTypes},
						{{"LocalScopeConflict", funcName, "RHS"}, localVars},
						withModifiedTypes[{stringBoxTypes},
							{{"PatternVariable", funcName, "RHS"}, localVars},
							parse["Main"][rhs]
						]
					]
				(* else *),
					parse["Main"][rhs]
				]
			]
		}]
	]

parse[mode_][boxes_] := parse[mode] /@ boxes


(* ::Subsection:: *)
(*posExprPosition*)


posExprPosition[expr_] := posExprPosition[expr, 0]

posExprPosition[pos_List, i_] := Drop[pos, -i]

posExprPosition[head_[___], i_] := posExprPosition[head, i + 1]


(* ::Subsection:: *)
(*normalizeAnnotationTypes*)


(*	Local variables of scoping construct that are on RHS of assignment in
	variable specification, but not directly, i.e. they are inside some other
	function with local variables, are treated as local variables of scoping
	construct. *)
normalizeAnnotationTypes[{"NotDirectlyInScopingRHS", types___}] :=
	normalizeAnnotationTypes @
		Replace[{types},
			{"Scoping", funcName_, "RHS"} :> {"Scoping", funcName},
			{1}
		]

normalizeAnnotationTypes[{"UndefinedSymbol", types___}] :=
	Append[normalizeAnnotationTypes[{types}], "UndefinedSymbol"]

normalizeAnnotationTypes[types_List] :=
	Module[
		{
			newTypes = types, reversedTypes = Reverse[types],
			scopingPos, pattVarNonRuleLhsPos, lastPattConfPos, locScopeConfPos,
			locVarPos, funcLocVarPos
		},
		(*	Local scope conflict arises when we have:
			 *	Scoping inside Scoping,
			 *	Scoping, any non-\[Function] and non-/; PatternVariable, or
			 	LocalScopeConflict from using pattern on RHS of delayed rule or
			 	assignment, inside PatternVariable that is not from \[Function]
			 	nor LHS of a rule.
			LocalScopeConflict type appears just before first type involved in
			conflict. *)
		scopingPos =
			Position[newTypes, {"Scoping", __}, {1}, 2, Heads -> False];
		pattVarNonRuleLhsPos =
			Position[newTypes,
				Except[
					{"PatternVariable", $ruleOperators, "LHS"},
					{"PatternVariable", Except["\[Function]" | "/;"], ___}
				],
				{1}, 2, Heads -> False
			];
		lastPattConfPos =
			Length[newTypes] + 1 -
				Position[reversedTypes,
					{
						"PatternVariable" | "LocalScopeConflict",
						Except["\[Function]" | "/;"],
						___
					},
					{1}, 1, Heads -> False
				];
		locScopeConfPos =
			Which[
				(*	<no conflict>,
					first non-Rule-LHS non-\[Function] non-/; PatternVariable,
					...,
					first Scoping,
					... *)
				pattVarNonRuleLhsPos =!= {} && scopingPos =!= {} &&
						pattVarNonRuleLhsPos[[1, 1]] < scopingPos[[1, 1]]
					,
					pattVarNonRuleLhsPos[[1, 1]]
				,
				(*	<no conflict>, first Scoping, ..., second Scoping, .... *)
				Length[scopingPos] >= 2,
					scopingPos[[1, 1]]
				,
				(*	<no conflict>,
					first non-Rule-LHS non-\[Function] non-/; PatternVariable,
					...,
					last Scoping or any PatternVariable,
					... *)
				pattVarNonRuleLhsPos =!= {} &&
						pattVarNonRuleLhsPos[[1, 1]] < lastPattConfPos[[1, 1]]
					,
					pattVarNonRuleLhsPos[[1, 1]]
				,
				True,
					{}
			];
		If[locScopeConfPos =!= {},
			(*	LocalScopeConflict imposes non-iatlic style, even if giving
				italic PatternVariable is present, when last type involved in
				conflict is not of PatternVariable type. Emulate this behavior
				by removing all PatternVariable after LocalScopeConflict. *)
			newTypes =
				If["PatternVariable" =!=
						First @ Cases[reversedTypes,
							{
								type :
									"Scoping" |
									"PatternVariable" |
									"LocalScopeConflict",
								__
							} :> type
							,
							{1},
							1
						]
				(* then *),
					Join[
						Take[newTypes, locScopeConfPos - 1],
						{"LocalScopeConflict"},
						DeleteCases[
							Drop[newTypes, locScopeConfPos - 1],
							{"PatternVariable", __}
						]
					]
				(* else *),
					Insert[newTypes, "LocalScopeConflict", locScopeConfPos]
				];
		];

		(* LocalVariable (With, Module) removes all outer Block types. *)
		locVarPos =
			Position[Reverse[newTypes],
				{"Scoping", "With" | "Module", ___},
				{1},
				1,
				Heads -> False
			];
		If[locVarPos =!= {},
			locVarPos = -locVarPos[[1, 1]];
			newTypes =
				Join[
					DeleteCases[
						Drop[newTypes, locVarPos],
						{"Scoping", "Block", ___}
					],
					Take[newTypes, locVarPos]
				]
		];

		(*	FunctionLocalVariable, coming from anything else than Block,
			removes LocalVariable immediately before it, and is always moved
			to the end. *)
		funcLocVarPos =
			Position[newTypes,
				{"FunctionLocalVariable", __},
				{1},
				Heads -> False
			];
		locVarPos =
			Select[
				Replace[funcLocVarPos, {{1}, ___} :> Rest[funcLocVarPos]] - 1,
				MatchQ[newTypes[[First[#]]],
					{"Scoping", "With" | "Module", ___}
				]&
			];
		If[funcLocVarPos =!= {},
			newTypes =
				Append[
					Delete[newTypes, Join[funcLocVarPos, locVarPos]],
					"FunctionLocalVariable"
				]
		];

		DeleteDuplicates @ Replace[
			newTypes, {
				{"Scoping", _, "RHS"} -> Sequence[],
				{"Scoping", "Block", ___} -> "FunctionLocalVariable",
				{"Scoping", ___} -> "LocalVariable",
				{type_, ___} :> type
			},
			{1}
		]
	]


(* ::Subsection:: *)
(*syntaxStyleBox*)


syntaxStyleBox[boxes_, annotationTypes_] :=
	StyleBox[
		boxes,
		CurrentValue[{AutoStyleOptions, # <> "Style"}]& /@ annotationTypes
	]


(* ::Subsection:: *)
(*$stringBoxToTypes*)


$stringBoxToTypes = {
	str_ /; $inFunction && StringMatchQ[str, "#*"] :>
		{{"PatternVariable", "Function"}}
	,
	str_ /; StringMatchQ[str, "\"*\""] -> {"String"}
	,
	_?undefinedSymbolQ -> {"UndefinedSymbol"}
}


(* ::Subsection:: *)
(*AnnotateSyntax*)


Options[AnnotateSyntax] = {
	"Annotation" -> Automatic,
	"StringBoxToTypes" -> Automatic,
	"AnnotateComments" -> True
}


AnnotateSyntax[boxes_, OptionsPattern[]] :=
	Module[
		{
			annotation =
				Replace[OptionValue["Annotation"], Automatic -> syntaxStyleBox]
			,
			stringBoxToTypes =
				Replace[
					OptionValue["StringBoxToTypes"],
					Automatic -> $stringBoxToTypes,
					{0, 1}
				] // Flatten
			,
			commentPlaceholder, boxesCommRepl, commPos, boxesComm, ignoredPos,
			boxesClean, boxesCleanParsed, syntaxPosClean, syntaxPos
		},

		boxesCommRepl =
			boxes /. RowBox[{"(*", ___, "*)"}] -> commentPlaceholder;
		commPos =
			Position[boxesCommRepl, commentPlaceholder, {-1}, Heads -> False];
		boxesComm =
			If[TrueQ @ OptionValue["AnnotateComments"],
				MapAt[annotation[#, {"Comment"}]&, boxes, commPos]
			(* else *),
				boxes
			];
		ignoredPos =
			Join[
				Position[boxesCommRepl,
					_String?whitespaceQ,
					{-1},
					Heads -> False
				],
				commPos
			];
		boxesClean = Delete[boxesCommRepl, ignoredPos];
		If[{boxesClean} === {}, Return[boxesComm, Module]];

		Internal`InheritedBlock[{deafultStringBoxTypes, stringBoxTypes},
			Function[{lhs, rhs},
				deafultStringBoxTypes[lhs] := rhs;
				stringBoxTypes[lhs] := rhs
				,
				HoldAll
			] @@@
				stringBoxToTypes;
			boxesCleanParsed = parse["Main"][boxesClean]
		];

		syntaxPosClean =
			Position[boxesCleanParsed, _syntaxBox, Heads -> False];
		syntaxPos =
			Extract[
				Delete[
					MapIndexed[#2&, boxes, {-1}, Heads -> True],
					ignoredPos
				],
				syntaxPosClean,
				posExprPosition
			];
		ReplacePart[boxesComm,
			MapThread[
				With[{normalizedTypes = normalizeAnnotationTypes @ Last[#2]},
					If[normalizedTypes === {},
						Unevaluated @ Sequence[]
					(* else *),
						{#1} -> annotation[#3, normalizedTypes]
					]
				] &,
				{
					syntaxPos,
					Extract[boxesCleanParsed, syntaxPosClean],
					Extract[boxes, syntaxPos]
				}
			]
		]
	]


(* ::Section:: *)
(*Package Epilogue*)


End[]

Protect@Evaluate@Names[Context[] ~~ Except["$"] ~~ Except["`"]...]


EndPackage[]
