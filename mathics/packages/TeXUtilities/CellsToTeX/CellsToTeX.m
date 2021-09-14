(* ::Package:: *)

(* ::Section:: *)
(*Usage messages*)


BeginPackage["TeXUtilities`CellsToTeX`CellsToTeX`"]


Unprotect["`*"]
ClearAll["`*"]


(* ::Subsection:: *)
(*Public*)


CellToTeX::usage =
"\
CellToTeX[cell] \
returns String with TeX code representing given cell. Returned TeX code \
contains converted cell contents and data extracted from Cell options."


CellsToTeXPreamble::usage =
"\
CellsToTeXPreamble[] \
returns String with TeX code setting global properties of mmacells package, \
suitable for inclusion in document preamble."


CellsToTeXException::usage =
"\
CellsToTeXException \
is a symbol to which CellsToTeX package exception messages are attached. \

CellsToTeXException[errType, errSubtype, ...] \
is used as Throw tag identifying CellsToTeX package exceptions."


(* ::Subsection:: *)
(*Configuration*)


Begin["`Configuration`"]


Unprotect["`*"]
ClearAll["`*"]


(* ::Subsubsection:: *)
(*Variables*)


$supportedCellStyles::usage =
"\
$supportedCellStyles \
is a pattern matching all cell styles supported by CellToTeX function."


$cellStyleOptions::usage =
"\
$cellStyleOptions \
is a List of rules with left hand sides being Lists of two elements. First \
element is a pattern matching particular cell styles, second element is \
option name. Right hand sides of rules are default values of options that \
will be used for styles matching said pattern."


$commandCharsToTeX::usage =
"\
$commandCharsToTeX \
is a List of three rules. With left hand sides being: eascape character and \
argument delimiters used by TeX formatting commands. Right hand sides are \
strings used to represent escaped commandChars."


$basicBoxes::usage =
"\
$basicBoxes \
is a pattern matching boxes that can  be converted to correct TeX verbatim \
code without any BoxRules."


$linearBoxesToTeX::usage =
"\
$linearBoxesToTeX \
is a List of rules transforming \"linear\" boxes to TeX Verbatim code."


$boxesToFormattedTeX::usage =
"\
$boxesToFormattedTeX \
is a List of rules transforming boxes to formatted TeX verbatim code."


$boxHeadsToTeXCommands::usage =
"\
$boxHeadsToTeXCommands \
is a List of rules assigning TeX command specifications to box heads. \
Right hand side of rules can be a List of two elements with first being a \
String with TeX command name and second being number of arguments of TeX \
command."


$stringsToTeX::usage =
"\
$stringsToTeX \
is a List of rules transforming string fragments to TeX suitable for \
inclusion in formatted TeX verbatim code."


$annotationTypesToTeX::usage =
"\
$annotationTypesToTeX \
is List of rules with left hand sides being String with annotation type and \
right hand sides being lists of two elements. First element of pair is a \
String with key used in TeX mmaCell optional argument, or None if there's no \
key associated with annotation type. Second element is String with TeX \
command used to annotate verbatim code."


$currentValueObj::usage =
"\
$currentValueObj \
is a notebook or front end object used as basis for CurrentValue evaluations \
extracting styles needed for conversion of some boxes."


(* ::Subsubsection:: *)
(*Utilities*)


headRulesToBoxRules::usage =
"\
headRulesToBoxRules[head -> {\"name\", argsNo}] \
returns delayed rule that transforms box expression, with given head, to TeX \
formatting command with given name. Box can contain argsNo arguments and \
options.\

headRulesToBoxRules[{rule1, rule2, ...}] \
returns List of transformed rules."


defaultAnnotationType::usage =
"\
defaultAnnotationType[sym] or defaultAnnotationType[\"name\"] \
returns String with default syntax annotation type of given symbol sym \
or of symbol with given \"name\"."


texMathReplacement::usage =
"\
texMathReplacement[\"str\"] \
returns String with TeX code representing given String \"str\". By default \
DownValues of texMathReplacement are used by CellsToTeXPreamble."


texMathReplacementRegister::usage =
"\
texMathReplacementRegister[\"str\"] \
defines TeX code math replacement for given String \"str\", if it's not \
already defined. Returns  \"str\"."


makeString::usage =
"\
makeString[boxes] \
returns String representation of given boxes. \
This function can be used on right hand side of user provided BoxRules. \
It will apply BoxRules to its argument. \
Definitions for this function are locally created from user provided BoxRules \
on each evaluation of boxesToString function."


makeStringDefault::usage =
"\
makeStringDefault[boxes] \
returns String representation of given boxes. \
This function can be used on right hand side of user provided BoxRules. \
It will not apply BoxRules to its argument. \
This function is called by makeString function, when no user provided BoxRule \
applies."


charToTeX::usage =
"\
charToTeX[\"x\"] \
returns String with TeX code, representing given character, suitable for \
inclusion in formatted TeX verbatim code."


fileNameGenerator::usage =
"\
fileNameGenerator[boxes, exportFormat] \
returns String with file name created from Hash of given boxes with proper \
extension for given exportFormat."


processorDataLookup::usage =
"\
processorDataLookup[processorCall, data, key] \
returns value associated with given key, if key is in given data. Otherwise \
throws CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"].\

processorDataLookup[processorCall, data, {key1, key2, ...}] \
returns List of values associated with given keys, if all of them are in \
given data. Otherwise throws \
CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"]."


syntaxBox::usage =
"\
syntaxBox[boxes, type] \
represents boxes that in an expression perform sytnax role of given type."


(* ::Subsubsection:: *)
(*Processors*)


extractCellOptionsProcessor::usage =
"\
extractCellOptionsProcessor[{\"Boxes\" -> boxes, ...}] \
returns List of given options with following modifications. If boxes is a \
Cell expression, then all it's options are appended to given options. \
Otherwise given options are returned unchanged.

If \"Boxes\" key is not present \
CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"] is thrown."


cellLabelProcessor::usage =
"\
cellLabelProcessor[{\
\"TeXOptions\" -> texOptions, \"CurrentCellIndex\" -> ...,
\"PreviousIntype\" -> ..., \"Indexed\" -> ..., \"Intype\" -> ...,\
\"CellLabel\" -> ..., \"CellIndex\" -> ..., \"CellForm\" -> ..., ...\
}] \
returns List of given options with following modifications. texOptions \
can have appended \"label\", \"index\" and/or \"form\" key. \"Indexed\", \
\"CellIndex\" and \"Intype\" options are added/updated.\

If \"TeXOptions\", \"CurrentCellIndex\" or \"PreviousIntype\" key is not \
present CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"] is \
thrown."


trackCellIndexProcessor::usage =
"\
trackCellIndexProcessor[{\
\"Indexed\" -> indexed, \"CellIndex\" -> cellIndex, \"Intype\" -> intype, ...\
}] \
returns unmodified List of given options. For indexed cells sets \
\"CurrentCellIndex\" and \"PreviousIntype\" options, of CellToTeX function, \
to cellIndex and intype respectively.

If \"Indexed\", \"CellIndex\" or \"Intype\" key is not present \
CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"] is thrown."


annotateSyntaxProcessor::usage =
"\
annotateSyntaxProcessor[{\
\"Boxes\" -> boxes, \"TeXOptions\" -> texOptions, \
\"BoxRules\" -> boxRules, ...\
}] \
returns List of given options with following modifications. boxes have \
annotated syntax. boxRules have appended rules transforming annotated \
expressions. texOptions have appended options representing commonest \
annotations of annotated syntax elements.\

If \"Boxes\" or \"TeXOptions\" key is not present \
CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"] is thrown."


toInputFormProcessor::usage =
"\
toInputFormProcessor[{\"Boxes\" -> boxes, ...}] \
returns List of given options with boxes converted to input form boxes.\

If \"Boxes\" key is not present \
CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"] is thrown.\

If boxes represent syntactically invalid expression \
CellsToTeXException[\"Invalid\", \"Boxes\"] is thrown. \

If parsing of string with InputForm of expression represented by boxes fails \
CellsToTeXException[\"Failed\", \"Parser\"] is thrown."


messageLinkProcessor::usage =
"\
messageLinkProcessor[{\
\"Boxes\" -> boxes, \"TeXOptions\" -> texOptions, ...\
}] \
returns List of given options with following modifications. If boxes contain \
message link, proper messagelink option is appended to texOptions. \
Otherwise options are returned unchanged.\

If \"Boxes\" or \"TeXOptions\" key is not present \
CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"] is thrown."


boxRulesProcessor::usage =
"\
boxRulesProcessor[{\
\"BoxRules\" -> boxRules, \"StringRules\" -> stringRules, \
\"NonASCIIHandler\" -> nonASCIIHandler, ...\
}] \
returns List of given options with following modifications. If \
nonASCIIHandler is not Identity, \"StringRules\" have appended rule \
converting non-ASCII characters using nonASCIIHandler. If after modification \
\"StringRules\" are not empty, \"BoxRules\" have appended rule replaceing \
string using \"StringRules\"."


boxesToTeXProcessor::usage =
"\
boxesToTeXProcessor[{\"Boxes\" -> boxes, \"BoxRules\" -> boxRules,  ...}] \
returns List of given options with \"TeXCode\" option added. This option's \
value is a String with TeX representation of given boxes obtained by applying \
given boxRules.\

If \"Boxes\" key is not present \
CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"] is thrown.\

If boxes contain box not covered by boxRules, then \
CellsToTeXException[\"Unsupported\", \"Box\"] is thrown."


mmaCellProcessor::usage =
"\
mmaCellProcessor[{\
\"TeXCode\" -> texCode, \"Style\" -> style, \"TeXOptions\" -> texOptions, \
\"Indentation\" -> indentation, ...\
}] \
returns List of given options with \"TeXCode\" option modified. Its lines are \
indented using given indentation and whole code is wrapped with TeX mmaCell \
environment with given style and texOptions.\

If \"TeXCode\", \"Style\" or \"TeXOptions\" key is not present \
CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"] is thrown."


exportProcessor::usage =
"\
exportProcessor[{\
\"Boxes\" -> boxes, \"Style\" -> style, \
\"AdditionalCellOptions\" -> additionalCellOptions, \
\"ExportFormat\" -> exportFormat, \"FileNameGenerator\" -> fileNameGenerator, \
...\
}] \
exports given boxes to a file and returns List of given options with \
\"FileName\" option added. boxes, before export, are wrapped, if necessary, \
with BoxData and Cell with given style and additionalCellOptions. boxes are \
exported using given exportFormat. Name of file is a result of evaluation of \
given fileNameGenerator function with boxes as first argument and \
exportFormat as second.\

If \"Boxes\" or \"Style\" key is not present \
CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"] is thrown.\

If Export command fails CellsToTeXException[\"Failed\", \"Export\"] is thrown."


mmaCellGraphicsProcessor::usage =
"\
mmaCellGraphicsProcessor[{\
\"FileName\" -> filename, \"Style\" -> style, \"TeXOptions\" -> texOptions, \
...\
}] \
returns List of given options with \"TeXCode\" option added. This option's \
value is a String with TeX mmaCellGraphics command including file with given \
filename as graphics. TeX command has given style, texOptions and filename as \
arguments.\

If \"FileName\", \"Style\" or \"TeXOptions\" key is not present \
CellsToTeXException[\"Missing\", \"Keys\", \"ProcessorArgument\"] is thrown."


End[]


(* ::Subsection:: *)
(*Internal*)


Begin["`Internal`"]


ClearAll["`*"]


$texEscape::usage =
"\
$texEscape \
is a String used to escape a character in non-verbatim TeX code."


$texSpecialChars::usage =
"\
$texSpecialChars \
is a string pattern matching characters that need to be escaped in \
non-verbatim TeX code."


throwException::usage =
"\
throwException[thrownBy, {errType, errSubtype, ...}, {val1, val2, ...}] or \
\
throwException[\
HoldComplete[thrownBy], {errType, errSubtype, ...}, {val1, val2, ...}\
] or \
\
throwException[\
thrownBy, {errType, errSubtype, ...}, HoldComplete[val1, val2, ...]\
] \
throws Failure object tagged with \
CellsToTeXException[errType, errSubtype, ...]. Failure object contains List \
with thrownBy, CellsToTeXException[errType, errSubtype, ...] and given vali, \
wrapped with HoldForm, associated to \"MessageParameters\" key and message \
name apropriate for given exception types associated to \"MessageTemplate\" \
key.\

throwException[thrownBy, \"errType\", vals] \
is equivalent to throwException[thrownBy, {\"errType\"}, vals].\

throwException[thrownBy, \"errType\"] \
uses empty List of values.\

throwException[thrownBy, errTypes, vals, \"messageName\"] \
uses CellsToTeXException::messageName as \"MessageTemplate\" key."


handleException::usage =
"\
handleException[value, tag] \
if tag is CellsToTeXException[...] expression and value is a Failure object \
prints exception message and returns value, for CellsToTeXException[...] tag \
and non-Failure value returns $Failed and prints generic exception message, \
otherwise throws value with given tag."


catchException::usage =
"\
catchException[body] \
evaluates body. If CellsToTeXException was thrown in process of body \
evaluation prints apropriate exception message and returns Failure object. \
Otherwise returns result of body evalaution."


rethrowException::usage =
"\
rethrowException[rethrownBy][body] \
evaluates body. If CellsToTeXException was thrown in process of body \
evaluation re-throws this exception with rethrownBy as first argument of \
\"MessageParameters\" List. Other throws are not caught. If nothing was \
thrown result of body evalaution is returned."


dataLookup::usage =
"\
dataLookup[data, key] \
returns value associated with given key, if key is in given data. Otherwise \
throws CellsToTeXException[\"Missing\", \"Keys\"].\

dataLookup[data, {key1, key2, ...}] \
returns List of values associated with given keys, if all of them are in \
given data. Otherwise throws CellsToTeXException[\"Missing\", \"Keys\"]."


extractStyleOptions::usage =
"\
extractStyleOptions[style, {{stylePatt1, opt1} -> val1, ...}] \
returns List of options for given style. Returned List contains rules of form \
opti -> vali for which in given list of rules there exist rule
{{stylePatti, opti} -> vali} such that given style matches stylePatti."


boxesToInputFormBoxes::usage =
"\
boxesToInputFormBoxes[boxes] \
returns given boxes converted to InputForm.\

If boxes represent syntactically invalid expression \
CellsToTeXException[\"Invalid\", \"Boxes\"] is thrown.\

If parsing of string with InputForm of expression represented by given boxes \
fails CellsToTeXException[\"Failed\", \"Parser\"] is thrown."


boxesToString::usage =
"\
boxesToString[boxes, boxRules] \
returns String representation of given boxes, obtained by applying given \
boxRules.\

If value of FormatType option is different than InputForm and OutputForm \
CellsToTeXException[\"Unsupported\", \"FormatType\"] is thrown."


defaultOrFirst::usage =
"\
defaultOrFirst[list, default] \
returns default if it's member of given list, otherwise first element of list \
is returned. Given list should have at least one element."


commonestAnnotationTypes::usage =
"\
commonestAnnotationTypes[boxes, allowedTypes, specialChars] \
returns List of rules with left hand sides being strings and right hand sides \
being most common annotation types of those strings in given boxes. \
Only annotation types matching allowedTypes are taken into consideration when \
determining commonest annotation types. If specialChars is True all strings \
in boxes are inspected, if it's False only those composed of ASCII characters \
are inspected."


annotationTypesToKeyVal::usage =
"\
annotationTypesToKeyVal[{sym1 -> type1, ...}, {type1 -> key1, ...}] \
returns List of rules representing key-value pairs for inclusion as optional \
argument of mmaCell TeX environment. Returned List can contain keys \
representing syntax coloring."


annotationRulesToBoxRules::usage =
"\
annotationRulesToBoxRules[{type1 -> {key1, command1}, ...}] \
returns List of delayed rules that transform syntaxBox with given typei to \
TeX commandi."


labelToCellData::usage =
"\
labelToCellData[\"cellLabel\"] \
returns List containing three elements, extracted from given \"cellLabel\": \
cell type (In, Out, or None), \
cell index (Integer or None) and \
cell form (String or None)."


labelToKeyVal::usage =
"\
labelToKeyVal[cellLabel, cellIndex, cellForm, currentCellIndex] \
returns List containing two elements. First element is List of rules \
representing key-value pairs for inclusion as optional argument of mmaCell \
TeX environment, it can contain \"label\", \"index\" and \"form\" keys. \
Second element is new value for of currentCellIndex."


optionValueToTeX::usage =
"\
optionValueToTeX[val] \
returns given TeX option value val suitable for inclusion as value in TeX \
key-value argument."


optionsToTeX::usage =
"\
optionsToTeX[{key1 -> val1, key2 :> val2, ...}] \
returns String with given rules transformed to TeX key-value pairs.\

optionsToTeX[pre, {key1 -> val1, key2 :> val2, ...}, post] \
wraps result with pre and post, if result is not empty."


mergeAdjacentTeXDelims::usage =
"\
mergeAdjacentTeXDelims[startDelim, endDelim][texCode] \
returns String with given texCode in which code fragments, delimited by \
startDelim and endDelim, separated only by tabs or spaces, are merged \
together."


mergeAdjacentTeXCommands::usage =
"\
mergeAdjacentTeXCommands[cmd, argStart, argEnd][texCode] \
returns String with given texCode in which occurences of commands cmd with \
single arguments, separated only by tabs or spaces, are merged into one \
command."


templateBoxDisplayBoxes::usage =
"\
templateBoxDisplayBoxes[templateBox] \
returns boxes representing display form of given templateBox."


extractMessageLink::usage =
"\
extractMessageLink[boxes] \
returns String with URI ending of message link, if such link is in boxes. \
Otherwise returns Missing[\"NotFound\"]."


prettifyPatterns::usage =
"\
prettifyPatterns[expr] \
returns expr with changed Pattern names. Names not used in pattern are \
removed. Names used in pattern are replaced by strings."


formatToExtension::usage =
"\
formatToExtension[\"format\"] \
returns String with file extension for given export format. Extension \
contains leading dot. For unknown formats empty string is returned."


$convertRecursiveOption::usage =
"\
$convertRecursiveOption \
is name of System`Convert`CommonDump`RemoveLinearSyntax function option \
apropriate for used Mathematica version."


End[]


(* ::Subsection:: *)
(*Package*)


Begin["`Package`"]


ClearAll["`*"]


addIncorrectArgsDefinition::usage =
"\
addIncorrectArgsDefinition[sym] \
adds catch-all definition to given symbol sym, that, when matched, throws \
CellsToTeXException[\"Error\", \"IncorrectArguments\"].\

addIncorrectArgsDefinition[\"symName\"] \
adds definition to symbol with given name. If given string is not a symbol \
name, throws CellsToTeXException[\"Error\", \"NotSymbolName\"]. If symbol \
with given name has defined own value, throws \
CellsToTeXException[\"Error\", \"UnwantedEvaluation\"]"


End[]


(* ::Subsection:: *)
(*Backports*)


Begin["`Backports`"]


Unprotect["`*"]
ClearAll["`*"]


If[Not[StringMatchQ[$Version, StartOfString ~~ "Mathics" ~~ __]],
	If[$VersionNumber < 10,
		FirstCase::usage = "\
FirstCase[{e1, e2, ...}, pattern] \
gives the first ei to match pattern, or Missing[\"NotFound\"] if none \
matching pattern is found.\

FirstCase[{e1, e2, ...}, pattern -> rhs] \
gives the value of rhs corresponding to the first ei to match pattern.\

FirstCase[expr, pattern, default] \
gives default if no element matching pattern is found.\

FirstCase[expr, pattern, default, levelspec] \
finds only objects that appear on levels specified by levelspec.\

FirstCase[pattern] \
represents an operator form of FirstCase that can be applied to an expression.\

This is a backport of FirstCase from Mathematica versions >= 10.";


		Association::usage = "\
Association[key1 -> val1, key2 :> val2, ...] \
is a very limited backport of Association from Mathematica version 10.\

Association[key1 -> val1, key2 :> val2, ...][key] \
if key exists in association returns value associated with given key, \
otherwise returns Missing[\"KeyAbsent\", key].";


		AssociationQ::usage = "\
AssociationQ[expr] \
gives True if expr is a valid Association object, and False otherwise. \
This is a backport from Mathematica version 10.";


		Key::usage = "\
Key[key] \
represents a key used to access a value in an association. This is a backport \
from Mathematica version 10.";


		Lookup::usage = "\
Lookup[assoc, key] \
looks up the value associated with key in the association assoc, or \
Missing[\"KeyAbsent\"].\

Lookup[assoc, key, default] \
gives default if key is not present.\

This is a very limited backport of Lookup from Mathematica version 10.";


		Failure::usage = "\
Failure[tag, assoc] \
represents a failure of a type indicated by tag, with details given by the \
association assoc. This is a backport from Mathematica version 10."
	]
]


End[]


(* ::Section:: *)
(*Exception messages*)


CellsToTeXException::unsupported = "\
`3`: `4` is not one of supported: `5`. \
Exception occurred in `1`."

CellsToTeXException::failed = "\
Evaluation of following `3` expression failed: `4`. \
Exception occurred in `1`."

CellsToTeXException::invalid = "\
Following elements of type `3` are invalid: `4`. \
Exception occurred in `1`."

CellsToTeXException::missing = "\
Following elements of type `3` are missing: `4`. \
Available elements of type `3` are: `5`. \
Exception occurred in `1`."

CellsToTeXException::missingProcArg = "\
Processor didn't receive required data with following keys: `4`. \
Passed data keys are: `5`. \
Exception occurred in `1`."

CellsToTeXException::missingProcRes = "\
Required keys: `4` are not present in data returned by processor: `6`. \
Data contained only `5` keys. \
Exception occurred in `1`."

CellsToTeXException::missingCellStyle = "\
Couldn't extract cell style from given boxes. \
Either use \"Style\" option or provide Cell expression with defined style. \
Given boxes are: `3`. \
Exception occurred in `1`."

CellsToTeXException::error = "\
An internal error occurred. \
`1` expression caused `2`. \
Please inform the package maintainer about this problem."

CellsToTeXException::unknownError = "\
An internal error occurred. \
`1` was thrown tagged with `2`. \
Please inform the package maintainer about this problem."


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Dependencies*)


Needs["TeXUtilities`CellsToTeX`SyntaxAnnotations`"]


Begin["`Private`"]


ClearAll["`*"]


$ContextPath =
	Join[
		{
			"TeXUtilities`CellsToTeX`CellsToTeX`Configuration`",
			"TeXUtilities`CellsToTeX`CellsToTeX`Package`",
			"TeXUtilities`CellsToTeX`CellsToTeX`Internal`",
			"TeXUtilities`CellsToTeX`CellsToTeX`Backports`"
		},
		$ContextPath
	]


(* Dummy evaluation to load System`Convert`TeXFormDump` context. *)
Convert`TeX`BoxesToTeX


(* ::Subsection:: *)
(*Package*)


(* ::Subsubsection:: *)
(*addIncorrectArgsDefinition*)


addIncorrectArgsDefinition[sym_Symbol] := (
	functionCall:sym[___] :=
		throwException[functionCall, {"Error", "IncorrectArguments"}]
)

functionCall:addIncorrectArgsDefinition[str_String] := (
	Check[
		Symbol[str],
		throwException[functionCall, {"Error", "NotSymbolName"}, {str}],
		Symbol::symname
	];
	With[{heldSym = ToExpression[str, InputForm, HoldComplete]},
		If[ValueQ @@ heldSym,
			throwException[functionCall, {"Error", "UnwantedEvaluation"},
				heldSym
			]
		];

		addIncorrectArgsDefinition @@ heldSym
	]
)


(* ::Subsubsection:: *)
(*Common operations*)


addIncorrectArgsDefinition /@
	Names["TeXUtilities`CellsToTex`CellsToTeX`Package`" ~~ Except["$"] ~~ Except["`"]...]


(* ::Subsection:: *)
(*Backports*)


(* ::Subsubsection:: *)
(*FirstCase*)


If[Not[StringMatchQ[$Version, StartOfString ~~ "Mathics" ~~ __]],
   If[$VersionNumber < 10,
	SetAttributes[FirstCase, HoldRest];

	Options[FirstCase] = Options[Cases];

	FirstCase[
		expr_, pattOrRule_, Shortest[default_:Missing["NotFound"], 1],
		Shortest[levelspec_:{1}, 2], opts:OptionsPattern[]
	] :=
		Replace[Cases[expr, pattOrRule, levelspec, 1, opts],
			{{} :> default, {match_} :> match}
		];

	FirstCase[pattOrRule_][expr_] := FirstCase[expr, pattOrRule]
   ]


   (* ::Subsubsection:: *)
   (*Association*)


   If[$VersionNumber < 10,
	(assoc_Association)[key_] := Lookup[assoc, key];


	Association /: Extract[
		assoc_Association, fullKey:(Key[key_] | key_String), head_:Identity
	] :=
		FirstCase[assoc,
			_[Verbatim[key], val_] :> head@val,
			head@Missing["KeyAbsent", fullKey]
		];


	Association /: Append[
		Association[rules___],
		{newRules:(_Rule | _RuleDelayed)...} | newRule:(_Rule | _RuleDelayed)
	] :=
		With[{newRulesList = {newRules, newRule}},
			Association @@ Join[
				FilterRules[{rules}, Except[newRulesList]],
				newRulesList
			]
		]
   ]


   (* ::Subsubsection:: *)
   (*AssociationQ*)


   If[$VersionNumber < 10,
	AssociationQ[Association[(_Rule | _RuleDelayed)...]] = True;

	AssociationQ[_] = False
   ]


   (* ::Subsubsection:: *)
   (*Lookup*)


   If[$VersionNumber < 10,
	SetAttributes[Lookup, HoldAllComplete];

	Lookup[assoc_?AssociationQ, key_, default_] :=
		FirstCase[assoc, _[Verbatim[key], val_] :> val, default];

	Lookup[assoc_?AssociationQ, key_] :=
		Lookup[assoc, key, Missing["KeyAbsent", key]]
   ]


   (* ::Subsubsection:: *)
   (*Failure*)


   If[$VersionNumber < 10,
	Failure /: MakeBoxes[Failure[tag_, assoc_Association], StandardForm] :=
		With[
			{
				msg = assoc["MessageTemplate"],
				msgParam = assoc["MessageParameters"],
				type = assoc["Type"]
			}
			,
			ToBoxes @ Interpretation[
				"Failure" @ Panel @ Grid[
					{
						{
							Style["\[WarningSign]", "Message", FontSize -> 35]
							,
							Style["Message:", FontColor->GrayLevel[0.5]]
							,
							ToString[
								StringForm[msg, Sequence @@ msgParam],
								StandardForm
							]
						},
						{
							SpanFromAbove,
							Style["Tag:", FontColor->GrayLevel[0.5]],
							ToString[tag, StandardForm]
						},
						{
							SpanFromAbove,
							Style["Type:", FontColor->GrayLevel[0.5]],
							ToString[type, StandardForm]
						}
					},
					Alignment -> {Left, Top}
				],
				Failure[tag, assoc]
			] /; msg =!= Missing["KeyAbsent", "MessageTemplate"] &&
				msgParam =!= Missing["KeyAbsent", "MessageParameters"] &&
				msgParam =!= Missing["KeyAbsent", "Type"]
		]
   ]


   (* ::Subsubsection:: *)
(*Common operations*)
]


Protect @ Evaluate @ Names[
	"TeXUtilities`CellsToTex`CellsToTeX`Backports`" ~~ Except["$"] ~~ Except["`"]...
]


(* ::Subsection:: *)
(*Internal*)


(* ::Subsubsection:: *)
(*Common operations*)


addIncorrectArgsDefinition /@
	Names["TeXUtilities`CellsToTex`CellsToTeX`Internal`" ~~ Except["$"] ~~ Except["`"]...]


(* ::Subsubsection:: *)
(*$texEscape*)


$texEscape = "\\"


(* ::Subsubsection:: *)
(*$texSpecialChars*)


$texSpecialChars = "#" | "%" | "{" | "}" | "\\"


(* ::Subsubsection:: *)
(*throwException*)


SetAttributes[throwException, HoldFirst]


throwException[
	HoldComplete[thrownBy_] | thrownBy_,
	{"Unsupported", elementType_, subTypes___},
	(List | HoldComplete)[unsupported_, supported_],
	messageName:(_String | Automatic):Automatic
] :=
	With[{supportedPretty = prettifyPatterns /@ supported},
		throwException[
			thrownBy,
			{"Unsupported", elementType, subTypes},
			HoldComplete[elementType, unsupported, supportedPretty],
			messageName
		]
	]

throwException[
	HoldComplete[thrownBy_] | thrownBy_,
	{"Missing", elementType_, subTypes___},
	(List | HoldComplete)[missing_, available_],
	messageName:(_String | Automatic):Automatic
] :=
	throwException[
		thrownBy,
		{"Missing", elementType, subTypes},
		HoldComplete[elementType, missing, available],
		messageName
	]

throwException[
	HoldComplete[thrownBy_] | thrownBy_,
	{"Invalid", elementType_, subTypes___},
	(List | HoldComplete)[boxes_],
	messageName:(_String | Automatic):Automatic
] :=
	throwException[
		thrownBy,
		{"Invalid", elementType, subTypes},
		HoldComplete[elementType, boxes],
		messageName
	]

throwException[
	HoldComplete[thrownBy_] | thrownBy_,
	{types__} | type_String,
	(List | HoldComplete)[vals___] | PatternSequence[],
	messageNameArg:(_String | Automatic):Automatic
] :=
	With[
		{
			tag = CellsToTeXException[types, type],
			messageName =
				If[messageNameArg === Automatic,
					Replace[First[{types, type}], {
						mainType_String :>
							With[{msgName = ToLowerCase[mainType]},
								msgName /; ValueQ @
									MessageName[CellsToTeXException, msgName]
							],
						_ -> "error"
					}]
				(* else *),
					messageNameArg
				]
		}
		,
		Throw[
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :>
						MessageName[CellsToTeXException, messageName],
					"MessageParameters" ->
						List @@ HoldForm /@ HoldComplete[thrownBy, tag, vals],
					"Type" -> {types, type}
				]
			],
			tag
		]
	]


(* ::Subsubsection:: *)
(*handleException*)


handleException[
	failure:Failure[_, assoc_Association],
	tag_CellsToTeXException
] :=
	With[
		{
			heldMsgName =
				Quiet[Extract[assoc, "MessageTemplate", Hold], Extract::keyw]
		}
		,
		(
			Message @@ Join[
				heldMsgName,
				Hold @@ Lookup[
					assoc, "MessageParameters",
					{HoldForm["Unknown"], HoldForm[tag]}
				]
			];
			failure
		) /; heldMsgName =!= Hold@Missing["KeyAbsent", "MessageTemplate"]
	]

handleException[val_, tag_CellsToTeXException] := (
	Message[CellsToTeXException::unknownError, HoldForm[val], HoldForm[tag]];
	If[Head[val] === Failure,
		val
	(* else *),
		$Failed
	]
)

handleException[value_, tag_] := Throw[value, tag]


(* ::Subsubsection:: *)
(*catchException*)


SetAttributes[catchException, HoldFirst]


catchException[body_] := Catch[body, _CellsToTeXException, handleException]


(* ::Subsubsection:: *)
(*rethrowException*)


Options[rethrowException] = {
	"TagPattern" -> _CellsToTeXException,
	"AdditionalExceptionSubtypes" -> {},
	"MessageTemplate" -> Automatic,
	"AdditionalMessageParameters" -> {}
}

SetAttributes[rethrowException, HoldFirst]


rethrowException[rethrownBy_, opts:OptionsPattern[]] :=
	With[
		{
			tagPattern = OptionValue["TagPattern"],
			additionalExceptionSubtypes =
				OptionValue["AdditionalExceptionSubtypes"],
			messageTemplateRule = First @
				Options[{opts, Options[rethrowException]}, "MessageTemplate"],
			additionalMessageParameters =
				OptionValue["AdditionalMessageParameters"]
		}
		,
		Function[body,
			Catch[
				body
				,
				tagPattern
				,
				Function[{value, tag},
					Module[{throwInvValExc, assoc, msgParams},
						throwInvValExc =
							throwException[
								rethrowException[rethrownBy, opts][body],
								{"Error", "InvalidExceptionValue", #},
								HoldComplete[value]
							]&;

						If[
							!MatchQ[value,
								Failure[CellsToTeXException, _Association]
							]
						(* then *),
							throwInvValExc["NonFailureObject"]
						];

						assoc = Last[value];
						msgParams =
							Lookup[assoc, "MessageParameters",
								throwInvValExc["NoMessageParameters"]
							];
						If[!(ListQ[msgParams] && Length[msgParams > 2]) ,
							throwInvValExc["InvalidMessageParameters"]
						];

						If[Last[messageTemplateRule] =!= Automatic,
							assoc = Append[assoc, messageTemplateRule]
						];

						With[
							{
								newTag = Join[
									tag,
									Head[tag] @@ additionalExceptionSubtypes
								]
							}
							,
							assoc = Append[assoc, {
								"MessageParameters" -> {
									HoldForm[rethrownBy],
									HoldForm[newTag],
									Sequence @@ Drop[msgParams, 2],
									Sequence @@ HoldForm /@
										additionalMessageParameters
								},
								"Type" -> List @@ newTag
							}];

							Throw[Failure[CellsToTeXException, assoc], newTag]
						]
					]
				]
			]
			,
			HoldAll
		]
	]


(* ::Subsubsection:: *)
(*dataLookup*)


functionCall:dataLookup[
	data:{___?OptionQ},
	keys:{(_String | _Symbol)...} | _String | _Symbol
] :=
	Quiet[
		Check[
			OptionValue[{data}, keys]
			,
			With[
				{
					available =
						DeleteDuplicates @ Replace[
							Flatten[data][[All, 1]],
							sym_Symbol :> SymbolName[sym],
							{1}
						]
				},
				With[
					{
						missing =
							Complement[
								Replace[
									Flatten[{keys}],
									sym_Symbol :> SymbolName[sym],
									{1}
								],
								available
							]
					},
					throwException[functionCall, {"Missing", "Keys"},
						{missing, available}
					]
				]
			]
			,
			OptionValue::optnf
		]
		,
		OptionValue::optnf
	]


(* ::Subsubsection:: *)
(*extractStyleOptions*)


extractStyleOptions[style_, rules:{(_Rule | _RuleDelayed)...}] :=
	Cases[rules,
		h_[{stylePatt_ /; MatchQ[style, stylePatt], opt_}, val_] :> h[opt, val]
	]


(* ::Subsubsection:: *)
(*boxesToInputFormBoxes*)


SetAttributes[boxesToInputFormBoxes, Listable]


boxesToInputFormBoxes[
	str_String /; MakeExpression[str, StandardForm] === HoldComplete[Null]
] := str

functionCall:boxesToInputFormBoxes[boxes_] :=
	Module[{expr, str, newBoxes},
		expr = MakeExpression[StripBoxes[boxes], StandardForm];
		If[Head[expr] =!= HoldComplete,
			throwException[functionCall, {"Invalid", "Boxes"}, {boxes}]
		];

		str = StringTake[ToString[expr, InputForm], {14, -2}];

		newBoxes =
			FrontEndExecute @
				FrontEnd`UndocumentedTestFEParserPacket[str, False];
		If[newBoxes === $Failed,
			With[{str = str},
				throwException[functionCall, {"Failed", "Parser"},
					HoldComplete @ FrontEndExecute @
						FrontEnd`UndocumentedTestFEParserPacket[str, False]
				]
			]
		];

		Replace[newBoxes, {BoxData[b_], ___} :> b]
	]


(* ::Subsubsection:: *)
(*boxesToString*)


functionCall:boxesToString[
	boxes_, boxRules_List, opts:OptionsPattern[ToString]
] :=
	With[{formatType = OptionValue[{opts, ToString}, "FormatType"]},
		Internal`InheritedBlock[{makeStringDefault, makeString, ToString},
			Unprotect[makeStringDefault, makeString];

			Switch[formatType,
				InputForm,
					makeStringDefault[RowBox[l_List]] := makeString[l];
					makeStringDefault[str_String] :=
						StringReplace[
							StringTake[ToString[str], {2, -2}],
							{
								"\\\"" -> "\"",
								"\\n" -> "\n",
								"\\r" -> "\r",
								"\\t" -> "\t",
								"\\\\" -> "\\"
							}
						];
					makeStringDefault[arg_] := ToString[arg],
				OutputForm,
					makeStringDefault[RowBox[l_List]] :=
						ToString[DisplayForm[RowBox[makeString /@ l]]];
					makeStringDefault[str_String] :=
						ToString[DisplayForm[RowBox[{str}]]];
					makeStringDefault[arg_] := ToString[DisplayForm[arg]],
				_,
					throwException[functionCall, {"Unsupported", "FormatType"},
						{formatType, {InputForm, OutputForm}}
					]
			];
			Function[{lhs, rhs}, makeString[lhs] := rhs, HoldRest] @@@
				boxRules;
			SetOptions[ToString,
				(* In SetOptions if option name is repeated, then last
					option is used, so reverse options order. *)
				Reverse @ Flatten[{opts}]
			];
			makeString[boxes]
		]
	]


(* ::Subsubsection:: *)
(*defaultOrFirst*)


defaultOrFirst[{___, default_, ___}, default_] := default

defaultOrFirst[{first_, ___}, _] := first


(* ::Subsubsection:: *)
(*commonestAnnotationTypes*)


commonestAnnotationTypes[boxes_, allowedTypes_, specialChars : True|False] :=
	(* Get list of string box - syntax type pairs. *)
	Cases[boxes,
		syntaxBox[
			If[specialChars,
				name_String
			(* else *),
				name_String /;
					StringMatchQ[name, RegularExpression["[\\x00-\\x7F]*"]]
			],
			type:allowedTypes,
			___
		] :>
			{name, type}
		,
		{0, Infinity}
	] //
		Tally //
		(* Gather tallied name - type pairs by name. *)
		GatherBy[#, #[[1, 1]]&]& //
		(* Convert each group to name -> commonestType rule. *)
		Map[
			With[{name = #[[1, 1, 1]], typeMult = #[[All, 2]]},
				name ->
					(*	Select default type, if it's among commonest types,
						otherwise take first of commonest types. *)
					defaultOrFirst[
						(* Pick types with maximal number of occurrences.*)
						Pick[#[[All, 1, 2]], typeMult, Max @ typeMult],
						defaultAnnotationType[name]
					]
			]&
			,
			#
		]&



(* ::Subsubsection:: *)
(*annotationTypesToKeyVal*)


annotationTypesToKeyVal[
	symToTypes:{(_Rule | _RuleDelayed)...},
	typesToKeys:{(_Rule | _RuleDelayed)...}
] :=
	(
		Replace[#[[1, 2]], typesToKeys] ->
			StringReplace[#[[All, 1]], c:$texSpecialChars :> $texEscape <> c]
	)& /@
		GatherBy[
			Cases[
				symToTypes,
				(name_ -> type_) /; defaultAnnotationType[name] =!= type
			],
			Last
		]


(* ::Subsubsection:: *)
(*annotationRulesToBoxRules*)


annotationRulesToBoxRules[rules:{_Rule...}] :=
	Replace[
		rules
		,
		(type_ -> {_, command_}) :>
			With[
				{
					start =
						$commandCharsToTeX[[1, 1]] <> command <>
							$commandCharsToTeX[[2, 1]],
					end = $commandCharsToTeX[[3, 1]]
				}
				,
				syntaxBox[boxes_, type, ___] :>
					start <> makeString[boxes] <> end
			]
		,
		{1}
	]


(* ::Subsubsection:: *)
(*labelToCellData*)


labelToCellData[label_String] :=
	Module[{result},
		result =
			StringCases[
				label
				,
				StartOfString ~~ "In[" ~~ i:DigitCharacter.. ~~ "]:=" ~~
					EndOfString :>
						{In, ToExpression[i], None}
				,
				1
			];
		If[result === {},
			result =
				StringCases[
					label
					,
					StartOfString ~~ "Out[" ~~ i:DigitCharacter.. ~~ "]" ~~
						("//" ~~ form__) | "" ~~ "=" ~~ EndOfString :>
							{Out, ToExpression[i], Replace[form, {"" -> None}]}
					,
					1
				]
		];
		result = Flatten[result];
		If[result === {},
			{None, None, None}
		(* else *),
			result
		]
	]


(* ::Subsubsection:: *)
(*labelToKeyVal*)


labelToKeyVal[
	cellLabel:(_String | None),
	cellIndex:(_Integer | Automatic | None),
	cellForm:(_String | Automatic | None),
	currentIndex:(_Integer | Automatic)
] :=
	Module[
		{
			label = cellLabel, index = cellIndex, form = cellForm,
			labelType, labelIndex, labelForm,
			addToIndex = None
		}
		,
		If[label =!= None,
			{labelType, labelIndex, labelForm} = labelToCellData[cellLabel];

			If[labelType =!= None,
				If[index === Automatic, index = labelIndex];
				If[form === Automatic, form = labelForm];

				If[index === labelIndex && form === labelForm,
					label = None
				]
			]
		];

		If[index =!= currentIndex && IntegerQ[index] && IntegerQ[currentIndex],
			addToIndex = index - currentIndex
		];
		{
			DeleteCases[
				{
					"label" -> label,
					"addtoindex" -> addToIndex,
					"form" -> form
				},
				_ -> None | Automatic
			]
			,
			If[IntegerQ[index],
				index
			(* else *),
				currentIndex
			]
		}
	]


(* ::Subsubsection:: *)
(*optionValueToTeX*)


optionValueToTeX[True] = "true"

optionValueToTeX[False] = "false"

optionValueToTeX[subOpts:{OptionsPattern[]}] := optionsToTeX["{", subOpts, "}"]

optionValueToTeX[val_] :=
	With[{str = ToString[val]},
		If[StringMatchQ[str, "{*}"] || StringFreeQ[str, {"[", "]", ",", "="}],
			str
		(* else *),
			"{" <> str <> "}"
		]
	]


(* ::Subsubsection:: *)
(*optionsToTeX*)


optionsToTeX[keyval:{OptionsPattern[]}] :=
	StringJoin @
		Riffle[(ToString[#1] <> "=" <> optionValueToTeX[#2]) & @@@ keyval, ","]

optionsToTeX[_String, {}, _String] := ""

optionsToTeX[
	pre_String, keyval:{OptionsPattern[]}, post_String
] :=
	pre <> optionsToTeX[keyval] <> post


(* ::Subsubsection:: *)
(*mergeAdjacentTeXDelims*)


mergeAdjacentTeXDelims[startDelim_String, endDelim_String, texCode_String] :=
	StringReplace[texCode, {
		c : Except[WordCharacter] ~~ endDelim <> startDelim :> c,
		endDelim <> startDelim ~~ c : Except[WordCharacter] :> c,
		endDelim ~~ ws : (" " | "\t") .. ~~ startDelim :> ws
	}]


(* ::Subsubsection:: *)
(*mergeAdjacentTeXCommands*)


mergeAdjacentTeXCommands[
	cmd_String, argStart_String, argEnd_String, texCode_String
] :=
	StringReplace[texCode,
		cmds : (
			RegularExpression[
				StringReplace[cmd, "\\" -> "\\\\"] <> "(?P<braces>" <>
				argStart <> "(?:[^" <> argStart <> argEnd <>
				"]|(?P>braces))*" <> argEnd <> ")"
			] ~~ (" " | "\t") ...
		) .. :>
			mergeAdjacentTeXDelims[cmd <> argStart, argEnd, cmds]
]


(* ::Subsubsection:: *)
(*templateBoxDisplayBoxes*)


templateBoxDisplayBoxes[TemplateBox[boxes_, tag_, opts___]] :=
	Module[{displayFunction = Replace[DisplayFunction, {opts}]},
		If[displayFunction === DisplayFunction,
			displayFunction =
				CurrentValue[
					$currentValueObj
					,
					{
						StyleDefinitions,
						tag,
						"TemplateBoxOptionsDisplayFunction"
					}
				]
		];
		displayFunction @@ boxes
	]


(* ::Subsubsection:: *)
(*extractMessageLink*)


extractMessageLink[boxes_] :=
	FirstCase[
		System`Convert`CommonDump`RemoveLinearSyntax[
			boxes,
			$convertRecursiveOption -> True
		]
		,
		ButtonBox[
			content_ /;
				MatchQ[
					ToString @ DisplayForm[content],
					">>" | "\[RightSkeleton]"
				]
			,
			___,
			(Rule | RuleDelayed)[
				ButtonData,
				uri_String /; StringMatchQ[uri, "paclet:ref/*"]
			],
			___
		] :>
			StringDrop[uri, 11]
		,
		Missing["NotFound"]
		,
		{0, Infinity}
	]


(* ::Subsubsection:: *)
(*prettifyPatterns*)


prettifyPatterns[expr_] :=
	Module[{pattNames, presentQ, duplicates, result},
		pattNames =
			Alternatives @@ Cases[
				expr,
				Verbatim[Pattern][name_, _] :> name,
				{0, Infinity}
			];
		presentQ[_] = False;
		{result, duplicates} =
			Reap[expr /. name:pattNames :>
				With[{nameStr = SymbolName[name]},
					If[presentQ[name], Sow[nameStr]];
					presentQ[name] = True;
					nameStr /; True
				]
			];
		duplicates = Alternatives @@ Flatten[duplicates];

		result /. Verbatim[Pattern][Except[duplicates], patt_] :> patt
	]


(* ::Subsubsection:: *)
(*formatToExtension*)


formatToExtension[_String] = ""

Scan[
	(formatToExtension[Last[#]] =
		StringDrop[First[#], 1]) &
	,
	GatherBy[System`ConvertersDump`$extensionMappings, Last][[All, 1]]
]
Scan[
	With[{ext = formatToExtension[Last[#]]},
		If[ext =!= "",
			formatToExtension[First[#]] = ext
		]
	] &
	,
	System`ConvertersDump`$formatMappings
]


(* ::Subsubsection:: *)
(*$convertRecursiveOption*)


$convertRecursiveOption =
	Options[System`Convert`CommonDump`RemoveLinearSyntax][[1, 1]]


(* ::Subsection:: *)
(*Configuration*)


(* ::Subsubsection:: *)
(*$supportedCellStyles*)


$supportedCellStyles = "Code" | "Input" | "Output" | "Print" | "Message"


(* ::Subsubsection:: *)
(*$cellStyleOptions*)


$cellStyleOptions = {
	{"Code", "Processor"} ->
		Composition[
			trackCellIndexProcessor, mmaCellProcessor, boxesToTeXProcessor,
			boxRulesProcessor, annotateSyntaxProcessor, toInputFormProcessor,
			cellLabelProcessor, extractCellOptionsProcessor
		],
	{"Input", "Processor"} ->
		Composition[
			trackCellIndexProcessor, mmaCellProcessor, boxesToTeXProcessor,
			boxRulesProcessor, annotateSyntaxProcessor, cellLabelProcessor,
			extractCellOptionsProcessor
		],
	{"Output" | "Print", "Processor"} ->
		Composition[
			trackCellIndexProcessor, mmaCellProcessor, boxesToTeXProcessor,
			boxRulesProcessor, cellLabelProcessor, extractCellOptionsProcessor
		],
	{"Message", "Processor"} ->
		Composition[
			trackCellIndexProcessor, mmaCellProcessor, boxesToTeXProcessor,
			boxRulesProcessor, messageLinkProcessor, cellLabelProcessor,
			extractCellOptionsProcessor
		],
	{"Code", "BoxRules"} :> $linearBoxesToTeX,
	{"Input" | "Output" | "Print" | "Message", "BoxRules"} :>
		Join[
			$linearBoxesToTeX,
			$boxesToFormattedTeX,
			headRulesToBoxRules[$boxHeadsToTeXCommands]
		],
	{"Input" | "Output" | "Print" | "Message", "StringRules"} :>
		Join[$stringsToTeX, $commandCharsToTeX],
	{"Input", "NonASCIIHandler"} -> (charToTeX[#, FontWeight -> Bold]&),
	{"Output" | "Print" | "Message", "NonASCIIHandler"} ->
		(charToTeX[#, FontWeight -> Plain]&),
	{"Code", "CharacterEncoding"} -> "ASCII",
	{"Input" | "Output" | "Print" | "Message", "CharacterEncoding"} ->
		"Unicode",
	{"Code" | "Input", "FormatType"} -> InputForm,
	{"Output" | "Print" | "Message", "FormatType"} -> OutputForm,
	{"Input", "TeXCodeSimplifier"} ->
		(mergeAdjacentTeXCommands[
			$commandCharsToTeX[[1, 1]] <> "pmb",
			$commandCharsToTeX[[2, 1]],
			$commandCharsToTeX[[3, 1]],
			mergeAdjacentTeXDelims[
				$commandCharsToTeX[[1, 1]] <> "(",
				$commandCharsToTeX[[1, 1]] <> ")",
				#
			]
		]&),
	{"Output" | "Print" | "Message", "TeXCodeSimplifier"} ->
		(mergeAdjacentTeXDelims[
			$commandCharsToTeX[[1, 1]] <> "(",
			$commandCharsToTeX[[1, 1]] <> ")",
			#
		]&),
	{"Code" | "Input" | "Output", "Indexed"} -> True,
	{"Print" | "Message", "Indexed"} -> False,
	{"Code" | "Input", "Intype"} -> True,
	{"Output" | "Print" | "Message", "Intype"} -> False,
	{"Print" | "Message", "CellLabel"} -> None
}


(* ::Subsubsection:: *)
(*$commandCharsToTeX*)


$commandCharsToTeX = {
	"\\" -> "\\textbackslash{}",
	"{" -> "\\{",
	"}" -> "\\}"
}


(* ::Subsubsection:: *)
(*$basicBoxes*)


$basicBoxes = _BoxData | _TextData | _RowBox | _String | _List


(* ::Subsubsection:: *)
(*$linearBoxesToTeX*)


$linearBoxesToTeX = {
	RowBox[l_List] :> makeString[l]
	,
	(StyleBox | ButtonBox | InterpretationBox | FormBox | TagBox | TooltipBox)[
		contents_, ___
	] :> makeString[contents]
	,
	tb:TemplateBox[_, _, ___] :> makeString[templateBoxDisplayBoxes[tb]]
	,
	GridBox[grid:{___List}, ___] :>
		StringJoin@Riffle[Riffle[makeString /@ #, "\t"]& /@ grid, "\n"]
	,
	PaneSelectorBox[
		rules:{(_Rule | _RuleDelayed)...},
		HoldPattern[Dynamic][v_, ___] | v_,
		Shortest[def_:" "],
		OptionsPattern[]
	] :>
		makeString@Replace[v, Append[rules, _ :> def]]
}


(* ::Subsubsection:: *)
(*$boxesToFormattedTeX*)


$boxesToFormattedTeX =
	(
		#1["\[Integral]", scr_, OptionsPattern[]] :>
			With[
				{
					escChar = $commandCharsToTeX[[1, 1]],
					argStart = $commandCharsToTeX[[2, 1]],
					argEnd = $commandCharsToTeX[[3, 1]]
				}
				,
				StringJoin[
					escChar, #2,
					argStart, escChar, "int", argEnd,
					argStart, makeString[scr], argEnd
				]
			]
	)& @@@ {
		SubscriptBox -> "mmaSubM",
		SuperscriptBox -> "mmaSupM"
	}

AppendTo[$boxesToFormattedTeX,
	SubsuperscriptBox["\[Integral]", sub_, sup_, OptionsPattern[]] :>
		With[
			{
				escChar = $commandCharsToTeX[[1, 1]],
				argStart = $commandCharsToTeX[[2, 1]],
				argEnd = $commandCharsToTeX[[3, 1]]
			}
			,
			StringJoin[
				escChar, "mmaSubSupM",
				argStart, escChar, "int", argEnd,
				argStart, makeString[sub], argEnd,
				argStart, makeString[sup], argEnd
			]
		]
]


(* ::Subsubsection:: *)
(*$boxHeadsToTeXCommands*)


$boxHeadsToTeXCommands = {
	SubscriptBox -> {"mmaSub", 2},
	SuperscriptBox -> {"mmaSup", 2},
	SubsuperscriptBox -> {"mmaSubSup", 3},
	UnderscriptBox -> {"mmaUnder", 2},
	OverscriptBox -> {"mmaOver", 2},
	UnderoverscriptBox -> {"mmaUnderOver", 3},
	FractionBox -> {"mmaFrac", 2},
	SqrtBox -> {"mmaSqrt", 1},
	RadicalBox -> {"mmaRadical", 2}
}


(* ::Subsubsection:: *)
(*$stringsToTeX*)


$stringsToTeX = {
	"\[LeftSkeleton]" -> "<<",
	"\[RightSkeleton]" -> ">>"
}

If[$VersionNumber >=10,
	$stringsToTeX =
		Join[
			{
				ToExpression["\"\\[LeftAssociation]\""] -> "<|",
				ToExpression["\"\\[RightAssociation]\""] -> "|>"
			},
			$stringsToTeX
		]
]


(* ::Subsubsection:: *)
(*$annotationTypesToTeX*)


$annotationTypesToTeX = {
	"DefinedSymbol" -> {"defined", "mmaDef"},
	"UndefinedSymbol" -> {"undefined", "mmaUnd"},
	"LocalVariable" -> {"local", "mmaLoc"},
	"FunctionLocalVariable" -> {"functionlocal", "mmaFnc"},
	"PatternVariable" -> {"pattern", "mmaPat"},
	"LocalScopeConflict" -> {"localconflict", "mmaLCn"},
	"GlobalToLocalScopeConflict" -> {"globalconflict", "mmaGCn"},
	"ExcessArgument" -> {"excessargument", "mmaExc"},
	"UnknownOption" -> {"unknownoption", "mmaOpt"},
	"UnwantedAssignment" -> {"unwantedassignment", "mmaAsg"},
	"SymbolShadowing" -> {"shadowing", "mmaShd"},
	"SyntaxError" -> {"syntaxerror", "mmaSnt"},
	"EmphasizedSyntaxError" -> {"emphasizedsyntaxerror", "mmaEmp"},
	"FormattingError" -> {"formattingerror", "mmaFmt"},
	"String" -> {None, "mmaStr"},
	"Comment" -> {None, "mmaCmt"}
}


(* ::Subsubsection:: *)
(*$currentValueObj*)


$currentValueObj = Sequence[]


(* ::Subsubsection:: *)
(*makeString*)


makeString[arg_] := makeStringDefault[arg]

makeString[
	str_String?System`Convert`CommonDump`EmbeddedStringWithLinearSyntaxQ
] :=
	makeString @ System`Convert`CommonDump`removeLinearSyntax[
		str,
		$convertRecursiveOption -> True
	]


(* ::Subsubsection:: *)
(*makeStringDefault*)


makeStringDefault["\[IndentingNewLine]"] := "\n"

makeStringDefault[boxes_List] := StringJoin[makeString /@ boxes]

makeStringDefault[BoxData[boxes_]] := makeString[boxes]


(* ::Subsubsection:: *)
(*headRulesToBoxRules*)


SetAttributes[headRulesToBoxRules, Listable]


headRulesToBoxRules[
	boxHead_ -> {texCommandName_String, argsNo_Integer?NonNegative}
] :=
	With[
		{
			comm = $commandCharsToTeX[[1, 1]] <> texCommandName,
			argStart = $commandCharsToTeX[[2, 1]],
			argEnd = $commandCharsToTeX[[3, 1]]
		}
		,
		HoldPattern @ boxHead[boxes:Repeated[_, {argsNo}], OptionsPattern[]] :>
			comm <> (argStart <> makeString[#] <> argEnd& /@ {boxes})
	]


(* ::Subsubsection:: *)
(*defaultAnnotationType*)


defaultAnnotationType[
	sym:(_Symbol | _String) /;
		Quiet[Context[sym], Context::notfound] === "System`"
] := "DefinedSymbol"

defaultAnnotationType[_Symbol | _String] := "UndefinedSymbol"


(* ::Subsubsection:: *)
(*texMathReplacementRegister*)


texMathReplacementRegister[str_String] := (
	If[! StringQ @ texMathReplacement[str],
		With[{texCode = StringTrim @ System`Convert`TeXFormDump`MakeTeX[str]},
			If[texCode =!= "",
				texMathReplacement[str] = texCode
			]
		]
	];
	str
)


(* ::Subsubsection:: *)
(*charToTeX*)


Options[charToTeX] = {FontWeight -> Plain}


functionCall:charToTeX[char_, OptionsPattern[]] :=
	With[
		{
			styleWrapper =
				Replace[OptionValue[FontWeight], {
					Plain -> Identity,
					Bold -> ({"\\pmb{", # , "}"} &),
					_ :>
						throwException[functionCall,
							{"Unsupported", "OptionValue", FontWeight},
							{OptionValue[FontWeight], {Plain, Bold}}
						]
				}]
		}
		,
		StringReplace[
			StringJoin @ Replace[
				System`Convert`TeXFormDump`TextExceptions @
					System`Convert`TeXFormDump`TeXCharacters[char]
				,
				{
					{"$", texStr_, "$"} :>
						If[StringFreeQ[texStr, "\\"],
							texStr
						(* else *),
							{"\\(", styleWrapper[texStr], "\\)"}
						],
					texStr_String /; !StringFreeQ[texStr, "\\"] :>
						styleWrapper[texStr]
				}
			]
			,
			With[{escChar = $commandCharsToTeX[[1, 1]]},
				{
					" " -> "",
					"\\{" -> escChar <> "{", "{" -> $commandCharsToTeX[[2, 1]],
					"\\}" -> escChar <> "}", "}" -> $commandCharsToTeX[[3, 1]],
					"\\\\" -> "\\\\", "\\" -> escChar
				}
			]
		]
	]


(* ::Subsubsection:: *)
(*fileNameGenerator*)


Options[fileNameGenerator] = {
	"CellOptionsFilter" -> Except[{CellLabel, CellChangeTimes}]
}


fileNameGenerator[boxes_, exportFormat_String, OptionsPattern[]] :=
	IntegerString[
		Hash[
			Replace[boxes,
				Cell[contents__, Longest[opts__?OptionQ]] :>
					Cell[contents,
						Sequence @@ FilterRules[
							{opts}, OptionValue["CellOptionsFilter"]
						]
					]
			],
			"MD5"
		],
		16,
		8
	] <>
		formatToExtension[exportFormat]


(* ::Subsubsection:: *)
(*processorDataLookup*)


SetAttributes[processorDataLookup, HoldFirst]


processorDataLookup[
	functionCall_,
	data:{___?OptionQ},
	keys:{(_String | _Symbol)...} | _String | _Symbol
] :=
	rethrowException[functionCall,
		"TagPattern" -> CellsToTeXException["Missing", "Keys"],
		"AdditionalExceptionSubtypes" -> {"ProcessorArgument"},
		"MessageTemplate" :> CellsToTeXException::missingProcArg
	] @ dataLookup[data, keys]


(* ::Subsubsection:: *)
(*extractCellOptionsProcessor*)


functionCall:extractCellOptionsProcessor[data:{___?OptionQ}] :=
	With[
		{
			cellOpts =
				Cases[
					processorDataLookup[functionCall, data, "Boxes"],
					Cell[__, Longest[cellOpts___?OptionQ]] :> cellOpts,
					{0},
					1
				]
		}
		,
		If[cellOpts === {},
			data
		(* else *),
			{data, cellOpts}
		]
	]


(* ::Subsubsection:: *)
(*cellLabelProcessor*)


Options[cellLabelProcessor] = {
	"Indexed" -> False,
	"Intype" -> False,
	"CellLabel" -> None,
	"CellIndex" -> Automatic,
	"CellForm" -> Automatic
}


functionCall:cellLabelProcessor[data:{___?OptionQ}] :=
	Module[
		{
			texOptions, texOptionsFromLabel,
			indexed, intype, cellLabel, cellIndex, cellForm,
			previousIntype, currentCellIndex
		}
		,
		{
			texOptions, currentCellIndex, previousIntype,
			indexed, intype, cellLabel, cellIndex, cellForm
		} =
			processorDataLookup[functionCall,
				{data, Options[cellLabelProcessor]},
				{
					"TeXOptions", "CurrentCellIndex", "PreviousIntype",
					"Indexed", "Intype", "CellLabel", "CellIndex", "CellForm"
				}
			];

		If[indexed && (intype || ! previousIntype) &&
				IntegerQ[currentCellIndex]
			,
			currentCellIndex++
		];

		{texOptionsFromLabel, currentCellIndex} =
			labelToKeyVal[cellLabel, cellIndex, cellForm, currentCellIndex];

		texOptions = Join[texOptionsFromLabel, texOptions];

		{
			"TeXOptions" -> texOptions,
			"Indexed" -> indexed,
			"Intype" -> intype,
			"CellIndex" -> currentCellIndex,
			data
		}
	]


(* ::Subsubsection:: *)
(*trackCellIndexProcessor*)


functionCall:trackCellIndexProcessor[data:{___?OptionQ}] :=
	Module[{indexed, cellIndex, intype},
		{indexed, cellIndex, intype} =
			processorDataLookup[functionCall,
				data, {"Indexed", "CellIndex", "Intype"}
			];

		If[indexed,
			SetOptions[CellToTeX,
				"CurrentCellIndex" -> cellIndex,
				"PreviousIntype" -> intype
			]
		];

		data
	]


(* ::Subsubsection:: *)
(*annotateSyntaxProcessor*)


Options[annotateSyntaxProcessor] = {
	"BoxRules" -> {},
	"AnnotationTypesToTeX" :> $annotationTypesToTeX,
	"CommonestTypesAsTeXOptions" -> "ASCII",
	"StringBoxToTypes" -> Automatic,
	"AnnotateComments" -> Automatic
}


functionCall:annotateSyntaxProcessor[data:{___?OptionQ}] :=
	Module[
		{
			boxes, boxRules, texOptions,
			annotationTypesToTeX, commonestTypesAsTeXOptions,
			stringBoxToTypes, annotateComments,
			preprocessedBoxes, commonestTypes, annotationTypesToTeXKeys
		}
		,
		{
			boxes, boxRules, texOptions,
			annotationTypesToTeX, commonestTypesAsTeXOptions,
			stringBoxToTypes, annotateComments
		} =
			processorDataLookup[functionCall,
				{data, Options[annotateSyntaxProcessor]},
				{
					"Boxes", "BoxRules", "TeXOptions",
					"AnnotationTypesToTeX", "CommonestTypesAsTeXOptions",
					"StringBoxToTypes", "AnnotateComments"
				}
			];

		If[stringBoxToTypes === Automatic,
			stringBoxToTypes =
				{
					If[commonestTypesAsTeXOptions === False,
						Automatic
					(* else *),
						(* Delete String type annotation rule. *)
						Delete[TeXUtilities`CellsToTex`CellsToTeX`SyntaxAnnotations`Private`$stringBoxToTypes, 2]
					]
					,
					_String?TeXUtilities`CellsToTex`CellsToTeX`SyntaxAnnotations`Private`symbolNameQ ->
						{"DefinedSymbol"}
				} // Flatten
		];
		If[annotateComments === Automatic,
			annotateComments = commonestTypesAsTeXOptions === False
		];

		preprocessedBoxes =
			AnnotateSyntax[
				boxes,
				"Annotation" -> (syntaxBox[#1, First@#2]&),
				"StringBoxToTypes" -> stringBoxToTypes,
				"AnnotateComments" -> annotateComments
			];

		Switch[commonestTypesAsTeXOptions,
			"ASCII" | True,
				commonestTypes =
					commonestAnnotationTypes[
						preprocessedBoxes,
						Except[
							Alternatives @@ Cases[annotationTypesToTeX,
								_[type_, {None, _}] :> type
							]
						],
						TrueQ[commonestTypesAsTeXOptions]
					];

				preprocessedBoxes =
					preprocessedBoxes /.
						(syntaxBox[#1, #2] :> #1 & @@@ commonestTypes);

				annotationTypesToTeXKeys =
					Cases[annotationTypesToTeX,
						h_[type_, {key:Except[None], _}] :>
							h[type, "more" <> key]
					];

				texOptions =
					Join[texOptions,
						annotationTypesToKeyVal[
							commonestTypes
							,
							Append[annotationTypesToTeXKeys,
								unsupportedType_ :>
									throwException[functionCall,
										{"Unsupported", "AnnotationType"},
										{
											unsupportedType,
											annotationTypesToTeXKeys[[All, 1]]
										}
									]
							]
						]
					]
			,
			Except[False],
				throwException[functionCall,
					{"Unsupported", "OptionValue", "CommonestTypesAsTeXOptions"},
					{commonestTypesAsTeXOptions, {True, "ASCII", False}}
				]
		];

		{
			"Boxes" -> preprocessedBoxes,
			"BoxRules" ->
				Join[
					annotationRulesToBoxRules[annotationTypesToTeX],
					boxRules
				],
			"TeXOptions" -> texOptions,
			data
		}
	]


(* ::Subsubsection:: *)
(*toInputFormProcessor*)


functionCall:toInputFormProcessor[data:{___?OptionQ}] :=
	Module[{boxes, template, placeholder},
		boxes = processorDataLookup[functionCall, data, "Boxes"];
		template =
			Replace[boxes, {
				Cell[BoxData[b_], cellData___] :> (
					boxes = b;
					Cell[BoxData[placeholder], cellData]
				),
				Cell[b_, cellData___] :> (
					boxes = b;
					Cell[placeholder, cellData]
				),
				BoxData[b_] :> (
					boxes = b;
					BoxData[placeholder]
				),
				_ -> placeholder
			}];

		boxes =
			rethrowException[functionCall,
				"TagPattern" ->
					CellsToTeXException["Invalid", "Boxes"] |
					CellsToTeXException["Failed", "Parser"]
			] @
				boxesToInputFormBoxes[boxes];
		boxes = template /. placeholder -> boxes;

		{"Boxes" -> boxes, data}
	]


(* ::Subsubsection:: *)
(*messageLinkProcessor*)


functionCall:messageLinkProcessor[data:{___?OptionQ}] :=
	Module[{boxes, texOptions, messageLink},
		{boxes, texOptions} =
			processorDataLookup[functionCall, data, {"Boxes", "TeXOptions"}];

		messageLink = extractMessageLink[boxes];

		If[MatchQ[messageLink, _Missing],
			data
		(* else *),
			AppendTo[texOptions, "messagelink" -> messageLink];
			{"TeXOptions" -> texOptions, data}
		]
	]


(* ::Subsubsection:: *)
(*boxRulesProcessor*)


Options[boxRulesProcessor] = {
	"BoxRules" -> {}, "StringRules" -> {}, "NonASCIIHandler" -> Identity
}


functionCall:boxRulesProcessor[data:{___?OptionQ}] :=
	Module[{boxRules, stringRules, nonASCIIHandler},
		{boxRules, stringRules, nonASCIIHandler} =
			processorDataLookup[functionCall,
				{data, Options[boxRulesProcessor]},
				{"BoxRules", "StringRules", "NonASCIIHandler"}
			];

		If[nonASCIIHandler =!= Identity,
			With[{nonASCIIHandler = nonASCIIHandler},
				AppendTo[stringRules,
					char:RegularExpression["[^\\x00-\\x7F]"] :>
						nonASCIIHandler[char]
				]
			]
		];

		If[stringRules =!= {},
			With[{stringRules = stringRules},
				AppendTo[boxRules,
					str_String :>
						StringReplace[makeStringDefault[str], stringRules]
				]
			]
		];

		{"BoxRules" -> boxRules, "StringRules" -> stringRules, data}
	]


(* ::Subsubsection:: *)
(*boxesToTeXProcessor*)


Options[boxesToTeXProcessor] =
	{"BoxRules" -> {}, "TeXCodeSimplifier" -> Identity}


functionCall:boxesToTeXProcessor[data:{___?OptionQ}] :=
	Module[{boxes, boxRules, texCodeSimplifier, texCode},
		{boxes, boxRules, texCodeSimplifier} =
			processorDataLookup[functionCall,
				{data, Options[boxesToTeXProcessor]},
				{"Boxes", "BoxRules", "TeXCodeSimplifier"}
			];
		boxes = Replace[boxes, Cell[contents_, ___] :> contents];
		boxes = Replace[boxes, BoxData[b_] :> b];

		AppendTo[boxRules,
			With[{supportedBoxes = boxRules[[All, 1]]},
				unsupportedBox:Except[$basicBoxes] :>
					throwException[functionCall, {"Unsupported", "Box"},
						{unsupportedBox, supportedBoxes}
					]
			]
		];

		texCode =
			texCodeSimplifier @
				rethrowException[functionCall,
					"TagPattern" ->
						CellsToTeXException["Unsupported", "FormatType"]
				] @ boxesToString[
					boxes, boxRules, FilterRules[data, Options[ToString]]
				];

		{"TeXCode" -> texCode, data}
	]


(* ::Subsubsection:: *)
(*mmaCellProcessor*)


Options[mmaCellProcessor] = {"Indentation" -> "  "}


functionCall:mmaCellProcessor[data:{___?OptionQ}] :=
	Module[{texCode, style, texOptions, indentation},
		{texCode, style, texOptions, indentation} =
			processorDataLookup[functionCall,
				{data, Options[mmaCellProcessor]},
				{"TeXCode", "Style", "TeXOptions", "Indentation"}
			];

		texCode = StringJoin[
			"\\begin{mmaCell}",
			optionsToTeX["[", texOptions, "]"],
			"{", style, "}",
			StringReplace[
				StringJoin["\n", texCode],
				"\n" | "\[IndentingNewLine]" -> "\n" <> indentation
			]
			,
			"\n\\end{mmaCell}"
		];

		{"TeXCode" -> texCode, data}
	]


(* ::Subsubsection:: *)
(*exportProcessor*)


Options[exportProcessor] = {
	"AdditionalCellOptions" -> {ShowSyntaxStyles -> True},
	"ExportFormat" -> "PDF",
	"FileNameGenerator" -> fileNameGenerator
}


functionCall:exportProcessor[data:{___?OptionQ}] :=
	Module[
		{
			boxes, style, additionalCellOptions, exportFormat, nameGenerator,
			filename, exportResult
		}
		,
		{boxes, style, additionalCellOptions, exportFormat, nameGenerator} =
			processorDataLookup[functionCall,
				{data, Options[exportProcessor]},
				{
					"Boxes", "Style", "AdditionalCellOptions", "ExportFormat",
					"FileNameGenerator"
				}
			];

		additionalCellOptions = Sequence @@ additionalCellOptions;
		boxes =
			Replace[boxes, {
				Cell[contents_, rest___, Longest[cellOpts___?OptionQ]] :>
					Cell[
						contents, style, rest, additionalCellOptions, cellOpts
					]
				,
				contents:(_BoxData | _TextData) :>
					Cell[contents, style, additionalCellOptions]
				,
				other_ :> Cell[BoxData[other], style, additionalCellOptions]
			}];

		filename = nameGenerator[boxes, exportFormat];
		exportResult = Export[filename, boxes, exportFormat, data];

		If[exportResult === $Failed,
			With[
				{
					filename = filename, boxes = boxes,
					exportFormat = exportFormat
				}
				,
				throwException[functionCall, {"Failed", "Export"},
					HoldComplete[Export[filename, boxes, exportFormat, data]]
				]
			]
		];

		{"FileName" -> exportResult, data}
	]


(* ::Subsubsection:: *)
(*mmaCellGraphicsProcessor*)


functionCall:mmaCellGraphicsProcessor[data:{___?OptionQ}] :=
	Module[
		{style, texOptions, filename, texCode}
		,
		{filename, style, texOptions} =
			processorDataLookup[functionCall,
				data, {"FileName", "Style", "TeXOptions"}
			];

		texCode = StringJoin[
			"\\mmaCellGraphics", optionsToTeX["[", texOptions, "]"],
			"{", style, "}{", filename, "}"
		];

		{"TeXCode" -> texCode, data}
	]


(* ::Subsubsection:: *)
(*Common operations*)


Protect @ Evaluate @ DeleteCases[
	Names["TeXUtilities`CellsToTex`CellsToTeX`Configuration`" ~~ Except["$"] ~~ Except["`"] ...],
	"texMathReplacement",
	{1},
	1
]


(* ::Subsection:: *)
(*Public*)


(* ::Subsubsection:: *)
(*CellToTeX*)


Options[CellToTeX] = {
	"Style" -> Automatic,
	"SupportedCellStyles" :> $supportedCellStyles,
	"CellStyleOptions" :> $cellStyleOptions,
	"ProcessorOptions" -> {},
	"Processor" -> Composition[mmaCellGraphicsProcessor, exportProcessor],
	"TeXOptions" -> {},
	"CatchExceptions" -> True,
	"CurrentCellIndex" -> Automatic,
	"PreviousIntype" -> False
}


functionCall:CellToTeX[boxes_, opts:OptionsPattern[]] :=
	If[OptionValue["CatchExceptions"],
		catchException
	(* else *),
		Identity
	] @ Module[
		{
			style, supportedCellStyles, processorOpts, cellStyleOptions,
			styleOptions, data, processor
		}
		,
		{style, supportedCellStyles, processorOpts, cellStyleOptions} =
			OptionValue[{
				"Style", "SupportedCellStyles", "ProcessorOptions",
				"CellStyleOptions"
			}];

		If[style === Automatic,
			Replace[boxes,
				Cell[_, cellStyle_String, ___] :> (style = cellStyle;)
			];
			If[style === Automatic,
				throwException[functionCall, {"Missing", "CellStyle"},
					{boxes} , "missingCellStyle"
				]
			]
		];

		If[!MatchQ[style, supportedCellStyles],
			throwException[functionCall, {"Unsupported", "CellStyle"},
				{style, supportedCellStyles}
			]
		];

		styleOptions = extractStyleOptions[style, cellStyleOptions];

		data = {
			"Boxes" -> boxes,
			"Style" -> style,
			opts,
			processorOpts,
			styleOptions,
			Options[CellToTeX]
		};
		processor = dataLookup[data, "Processor"];
		data = processor[data];

		rethrowException[functionCall,
			"TagPattern" -> CellsToTeXException["Missing", "Keys"],
			"AdditionalExceptionSubtypes" -> {"ProcessorResult"},
			"MessageTemplate" :> CellsToTeXException::missingProcRes,
			"AdditionalMessageParameters" -> {processor}
		] @ dataLookup[data, "TeXCode"]
	]


(* ::Subsubsection:: *)
(*CellsToTeXPreamble*)


Options[CellsToTeXPreamble] = {
	"Gobble" -> Automatic,
	"UseListings" -> Automatic,
	"TeXOptions" -> {},
	"TeXMathReplacement" -> texMathReplacement,
	"CatchExceptions" -> True
}


functionCall:CellsToTeXPreamble[OptionsPattern[]] :=
	If[OptionValue["CatchExceptions"],
		catchException
	(* else *),
		Identity
	] @ Module[
		{gobble, useListings, texOptions, mathReplacement}
		,
		{gobble, useListings, texOptions, mathReplacement} =
			OptionValue @
				{"Gobble", "UseListings", "TeXOptions", "TeXMathReplacement"};

		If[!MatchQ[texOptions, {OptionsPattern[]}],
			throwException[functionCall,
				{"Unsupported", "OptionValue", "TeXOptions"},
				{texOptions, {"list of options"}}
			]
		];
		If[!MatchQ[mathReplacement, Except[HoldPattern@Symbol[___], _Symbol]],
			throwException[functionCall,
				{"Unsupported", "OptionValue", "TeXMathReplacement"},
				{mathReplacement, {"a symbol"}}
			]
		];

		(* By default gobble default indentation of mmaCell. *)
		If[gobble === Automatic,
			gobble =
				StringLength @ OptionValue[mmaCellProcessor, "Indentation"]
		];
		Switch[gobble,
			_Integer?NonNegative,
				PrependTo[texOptions, "morefv" -> {"gobble" -> gobble}]
			,
			Except[None],
				throwException[functionCall,
					{"Unsupported", "OptionValue", "Gobble"},
					{gobble, {Automatic, None, "non-negative integer"}}
				]
		];

		(*	Listings are used to highlight non-annotated code using TeX
			environment options. If moving annotations to options is switched
			off then, by default, don't use listings, otherwise use them.
			Since listings are, by default, switched on in mmacells package,
			if we're using them, we can just omit this option. *)
		If[useListings === Automatic,
			useListings =
				Replace[
					OptionValue[
						annotateSyntaxProcessor,
						"CommonestTypesAsTeXOptions"
					],
					Except[False] -> None
				]
		];
		Switch[useListings,
			True | False,
				PrependTo[texOptions, "uselistings" -> useListings]
			,
			Except[None],
				throwException[functionCall,
					{"Unsupported", "OptionValue", "UseListings"},
					{useListings, {Automatic, True, False, None}}
				]
		];
		StringJoin @ Riffle[
			DeleteCases[
				Prepend[
					StringJoin[
						"\\mmaDefineMathReplacement{",
							#1[[1, 1]],
						"}{",
							#2,
						"}"
					]& @@@
						DownValues @ Evaluate[mathReplacement]
					,
					optionsToTeX["\\mmaSet{", texOptions, "}"]
				],
				""
			],
			"\n"
		]
	]


(* ::Subsubsection:: *)
(*Common operations*)


Protect @ Evaluate @ Names[
	"TeXUtilities`CellsToTex`CellsToTeX`" ~~ Except["$"] ~~ Except["`"]...
]


End[]


EndPackage[]
