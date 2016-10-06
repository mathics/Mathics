(* XML Importer *)

(* "CDATA", "Comments", "EmbeddedDTD", "Plaintext", "Tags", "XMLObject", "XMLElement" *)

Begin["System`Convert`XML`"]

ImportExport`RegisterImport[
    "XML",
    {
        "XMLObject" :> XML`XMLObjectImport,
        "Plaintext" :> XML`PlaintextImport,
        "Tags" :> XML`TagsImport,
        XML`XMLObjectImport
    },
    {},
	AvailableElements -> {"Plaintext", "Tags", "XMLObject"},
	DefaultElement -> "XMLObject",
    FunctionChannels -> {"FileNames"}
]

End[]
