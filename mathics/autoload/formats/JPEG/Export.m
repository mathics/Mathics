(* Text Exporter *)

Begin["System`Convert`JPEG`"]

RegisterExport[
    "JPEG",
	System`ImageExport,
	Options -> {},
	BinaryFormat -> True
]

End[]
