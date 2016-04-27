(* JPEG Importer *)

Begin["System`Convert`JPEG`"]

RegisterImport[
    "JPEG",
    System`ImportImage,
    {},
    AvailableElements -> {"Image"},
    DefaultElement -> "Image",
    FunctionChannels -> {"FileNames"}
]
