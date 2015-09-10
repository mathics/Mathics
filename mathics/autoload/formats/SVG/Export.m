(* Text Exporter *)

Begin["System`Convert`TextDump`"]


SVGExport[filename_, expr_, opts___] := 
  Module[{strm, data}, 
    strm = OpenWrite[filename];
    If[strm === $Failed, Return[$Failed]];
    data = StringTake[ToString[MathMLForm[expr]],{25,-29}];    
    WriteString[strm, data];
    Close[strm];
  ]

RegisterExport[
    "SVG",
	System`Convert`TextDump`SVGExport,
	FunctionChannels -> {"FileNames"},
	Options -> {"ByteOrderMark"},
	DefaultElement -> "Plaintext",
	BinaryFormat -> False
]


End[]
