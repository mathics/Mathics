(* Image Exporter *)

Begin["System`Convert`PDF`"]

RegisterImageExport[type_] := ImportExport`RegisterExport[
    type,
	System`PDFExport,
        FunctionChannels -> {"FileNames"},
	Options -> {},
	BinaryFormat -> True
];

RegisterImageExport[#]& /@ {"PDF"};

End[]
