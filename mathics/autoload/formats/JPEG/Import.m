(* JPEG Importer *)

Begin["System`Convert`JPEG`"]

RegisterImport[
    "JPEG",
    System`ImageImport,
    {},
    AvailableElements -> {"Image"},
    DefaultElement -> "Image",
    FunctionChannels -> {"FileNames"}
]

End[]
