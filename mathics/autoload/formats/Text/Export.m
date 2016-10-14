(* Text Exporter *)

Begin["System`Convert`TextDump`"]

Options[TextExport] = {
    "CharacterEncoding" :> $CharacterEncoding
};

TextExport[filename_, expr_, opts:OptionsPattern[]] :=
  Module[{strm, data},
    strm = OpenWrite @@ Join[{filename}, FilterRules[{opts}, "CharacterEncoding"]];
    If[strm === $Failed, Return[$Failed]];
    data = ToString[expr];
    WriteString[strm, data];
    Close[strm];
  ]

ImportExport`RegisterExport[
    "Text",
	System`Convert`TextDump`TextExport,
	FunctionChannels -> {"FileNames"},
	Options -> {"CharacterEncoding", "ByteOrderMark"},
	DefaultElement -> "Plaintext",
	BinaryFormat -> True
]


End[]
