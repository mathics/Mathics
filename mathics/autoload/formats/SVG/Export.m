(* Text Exporter *)

Begin["System`Convert`TextDump`"]


SVGExport[filename_, expr_, opts___] := 
  Module[{strm, data}, 
    strm = OpenWrite[filename];
    If[strm === $Failed, Return[$Failed]];
    If[System`$UseSansSerif,
	    data = StringTake[ToString[MathMLForm[expr]],{23,-8}],
	    data = StringTake[ToString[MathMLForm[expr]],{23,-8}]];
    WriteString[strm, "<svg>" <> data <> "</svg>"];
    Close[strm];
  ]

ImportExport`RegisterExport[
    "SVG",
	System`Convert`TextDump`SVGExport,
	FunctionChannels -> {"FileNames"},
	Options -> {"ByteOrderMark"},
	DefaultElement -> "Plaintext",
	BinaryFormat -> False
]


End[]
