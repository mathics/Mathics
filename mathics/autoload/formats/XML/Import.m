(* XML Importer *)

(* "CDATA", "Comments", "EmbeddedDTD", "Plaintext", "Tags", "XMLObject", "XMLElement" *)

Begin["System`Convert`XML`"]

ImportExport`RegisterImport[
    "XML",
    {
        "XMLObject" :> System`XML`XMLObjectImport,
        "Plaintext" :> System`XML`PlaintextImport,
        "Tags" :> System`XML`TagsImport,
    },
    {},
	AvailableElements -> {"Plaintext", "Tags", "XMLObject"},
	DefaultElement -> "XMLObject",
    FunctionChannels -> {"FileNames"}
]

End[]
