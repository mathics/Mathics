(* Image Exporter *)

Begin["System`Convert`Image`"]

RegisterImageExport[type_] := ImportExport`RegisterExport[
    type,
	System`ImageExport,
        FunctionChannels -> {"FileNames"},
	Options -> {},
	BinaryFormat -> True
];

RegisterImageExport[#]& /@ {"BMP", "GIF", "JPEG2000", "JPEG", "PCX", "PNG", "PPM", "PBM", "PGM", "TIFF"};

End[]
