(* Image Exporter *)

Begin["System`Convert`PDF`"]

RegisterImageExport[type_] := ImportExport`RegisterExport[
    type,
	System`Convert`PDFDump`ExportToPDF,
        FunctionChannels -> {"FileNames"},
	Options -> {},
	BinaryFormat -> True
];

RegisterImageExport[#]& /@ {"PDF"};

End[]
