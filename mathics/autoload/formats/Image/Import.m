(* Image Importer *)

Begin["System`Convert`Image`"]

RegisterImageImport[type_] := ImportExport`RegisterImport[
    type,
    System`ImageImport,
    {},
    AvailableElements -> {"Image"},
    DefaultElement -> "Image",
    FunctionChannels -> {"FileNames"}
];

RegisterImageImport[#]& /@ {"BMP", "GIF", "JPEG2000", "JPEG", "PCX", "PNG", "PPM", "PBM", "PGM", "TIFF", "ICO", "TGA"};

End[]
