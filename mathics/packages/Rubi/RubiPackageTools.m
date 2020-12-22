(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA, see http://wlplugin.halirutan.de/ *)

(* :Title: RubiPackageTools *)
(* :Context: RubiPackageTools` *)
(* :Author: patrick *)
(* :Date: 2018-06-08 *)
(* :Discussion: This package is only intended for development purpose! It extracts all integration rules from the
  * notebooks and zips them together in several packages that are then loaded by Rubi. The structure, i.e. which notebooks
  * are packed into one package, is as important as the order in which they are loaded in Rubi.m.
  *
  * The core function of this package imports the "Code" cells from a notebook and converts them to Expressions which
  * are then exported into the .m package file.
  * *)

BeginPackage["Rubi`RubiPackageTools`", "PacletManager`"];

BuildIntegrationRules::usage = "BuildIntegrationRules[] creates the .m package files for the rules from the notebooks";
DeployRubi::usage = "DeployRubi[] creates a paclet and zip of the current release and puts it in the parent folder of the Rubi sources.";

Begin["`Private`"];
$dir = DirectoryName@System`$InputFileName;
$ruleDir = FileNameJoin[{$dir, "..", "IntegrationRuleNotebooks"}];

BuildIntegrationRules[] := BuildIntegrationRules[#, FileNameJoin[{$dir, "IntegrationRules"}]]& /@ FileNames["*.nb", {$ruleDir}, Infinity];
BuildIntegrationRules[file_String /; FileExistsQ[file], outDir_String /; DirectoryQ[outDir]] := Module[
  {
    files,
    outputFile,
    sectionName,
    sourceAsList,
    outDir2 = StringReplace[DirectoryName[file], $ruleDir -> outDir]
  },
  sectionName = FileBaseName[file];
  PrintTemporary["Exporting all notebooks from " <> sectionName];



  sourceAsList = Prepend[
    Map[inputTextToString, NotebookImport[file, "Code" -> "InputText"]],
    subSectionComment[FileBaseName[file]]
  ];
  If[Not@DirectoryQ[outDir2],
    CreateDirectory[outDir2]
  ];
  outputFile = FileNameJoin[{outDir2, sectionName <> ".m"}];
  Export[outputFile, StringRiffle[sourceAsList, {"", "\n", "\n"}], "Text", CharacterEncoding -> "ASCII"]
];

inputTextToString[str_String /; SyntaxQ[str]] := StringReplace[str, {"\\\n" -> " ", Whitespace.. -> " "}];
inputTextToString[args___] := Throw[{args}];

sectionComment[message_String] := TemplateApply["\n(* ::Section:: *)\n(* `` *)", message];
subSectionComment[message_String] := TemplateApply["\n(* ::Subsection::Closed:: *)\n(* `` *)", message];

deleteMXFiles[] := Module[{files = FileNames["*.mx", {FileNameJoin[{$dir, "Kernel"}]}]},
  DeleteFile /@ files;
];

DeployRubi[] := Module[{file, paclet},
  deleteMXFiles[];
  paclet = PackPaclet[$dir];
  file = StringReplace[paclet, ".paclet" -> ".zip"];
  If[FileExistsQ[file],
    DeleteFile[file]
  ];
  CreateArchive[$dir, file];
];

End[]; (* `Private` *)

EndPackage[]
