(* Text Importer *)

RegisterImport[
    "Text",
    {
        "String" :> StringImport, 
        "Plaintext" :> PlaintextImport,
        "Data" :> DataImport,
        PlaintextImport
    } (*,
    "FunctionChannels" -> {"Streams"},
	"AvailableElements" -> {"Data", "Lines", "Plaintext", "String", "Words"},
	"DefaultElement" -> "Plaintext",
	"BinaryFormat" -> True *)
]

