(* HTML Importer *)

Begin["System`Convert`HTML`"]

ImportExport`RegisterImport[
    "HTML",
    {
        "Data" :> HTML`DataImport,
        "FullData" :> HTML`FullDataImport,
        "Hyperlinks" :> HTML`HyperlinksImport,
        "ImageLinks" :> HTML`ImageLinksImport,
        "Plaintext" :> HTML`PlaintextImport,
        "Source" :> HTML`SourceImport,
        "Title" :> HTML`TitleImport,
        "XMLObject" :> HTML`XMLObjectImport,
        HTML`PlaintextImport
    },
    {},
	AvailableElements -> {"Data", "FullData", "Hyperlinks", "ImageLinks", "Plaintext", "Source", "Title", "XMLObject"},
	DefaultElement -> "Plaintext",
    FunctionChannels -> {"FileNames"}
]

End[]
