(* HTML Importer *)

Begin["System`Convert`HTML`"]

ImportExport`RegisterImport[
    "HTML",
    {
        "Data" :> HTML`DataImport,
        "Hyperlinks" :> HTML`HyperlinksImport,
        "ImageLinks" :> HTML`ImageLinksImport,
        "Plaintext" :> HTML`PlaintextImport,
        "Source" :> HTML`SourceImport,
        "XMLObject" :> HTML`XMLObjectImport,
        HTML`PlaintextImport
    },
    {},
	AvailableElements -> {"Data", "Hyperlinks", "ImageLinks", "Plaintext", "Source", "XMLObject"},
	DefaultElement -> "Plaintext",
    FunctionChannels -> {"FileNames"}
]

End[]
