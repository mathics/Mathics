(* Image Exporter *)

Begin["System`Convert`Image`"]

RegisterImageExport[type_] := RegisterExport[
    type,
	System`ImageExport,
	Options -> {},
	BinaryFormat -> True
];

RegisterImageExport[#]& /@ {"BMP", "GIF", "JPEG2000", "JPEG", "PCX", "PNG", "PPM", "PBM", "PGM", "TIFF"};

End[]
