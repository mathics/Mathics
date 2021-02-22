(* Image Exporter *)

Begin["System`Convert`PDFDump`"]

Print["Registering PDF export"]
RegisterImageExport[type_] := ImportExport`RegisterExport[
    type,
	System`Convert`PDFDump`ExportToPDF,
        FunctionChannels -> {"FileNames"},
	Options -> {},
	BinaryFormat -> True
];

RegisterImageExport[#]& /@ {"PDF"};

End[]
