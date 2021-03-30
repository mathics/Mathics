(* Text Exporter *)

Begin["System`Convert`TextDump`"]


SVGExport[filename_, expr_, opts___] := 
  Module[{strm, data, p, expr2}, 
    strm = OpenWrite[filename];
    If[strm === $Failed, Return[$Failed]];
    expr2 = If[Head[expr]=!=System`Graphics, System`Graphics[{System`Inset[ToString[expr]]}], expr];
	 expr2= MathMLForm[expr2];
    data=ToString[expr2];
    p = StringPosition[data, "data:image/svg+xml;base64"][[1]][[2]];
    data = StringTake[data  ,{p+2,-19}];
    WriteString[strm, System`Convert`B64Dump`B64Decode[data]];
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
