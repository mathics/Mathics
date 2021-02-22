(* Text Exporter *)

Begin["System`Convert`TextDump`"]


ImportExport`RegisterExport[
    "SVG",
	System`Convert`TextDump`SVGExport,
	FunctionChannels -> {"FileNames"},
	Options -> {"ByteOrderMark"},
	DefaultElement -> "Plaintext",
	BinaryFormat -> False
]


End[]
