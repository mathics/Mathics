(* Text Exporter *)

TextExport[filename_, expr_, opts___] := 
  Module[{strm, data}, 
    strm = OpenWrite[filename];
    If[strm === $Failed, Return[$Failed]];
    data = ToString[expr];
    WriteString[strm, data];
    Close[strm];
  ]

RegisterExport[
    "Text",
	TextExport,
	FunctionChannels -> {"FileNames"},
	Options -> {"ByteOrderMark"},
	DefaultElement -> "Plaintext",
	BinaryFormat -> True
]
