(* Text Exporter *)

Begin["System`Convert`TextDump`"]


ImportExport`RegisterExport[
    "SVG",
	System`Convert`TextDump`ExportSVG,
	FunctionChannels -> {"FileNames"},
	Options -> {"ByteOrderMark"},
	DefaultElement -> "Plaintext",
	BinaryFormat -> False
]


End[]
