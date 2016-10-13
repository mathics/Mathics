(* CSV Importer *)

Begin["System`Convert`TableDump`"]


ImportCSV[filename_String, opts:OptionsPattern[]]:=
    Module[{stream, data, grid, sep = FilterRules[{opts}, System`FieldSeparators][[1]]},
        stream = OpenRead @@ Join[{filename}, FilterRules[{opts}, System`CharacterEncoding]];
        data = StringSplit[#, ","]& /@ ReadList[stream, String];
        grid = Grid[data];
        Close[stream];
        {
            "Data" -> data,
            "Grid" -> grid
        }
]

ImportExport`RegisterImport[
    "CSV",
    System`Convert`TableDump`ImportCSV,
    {
        "Data" :> GetData,
	    "Grid" :> GetGrid
    },
    (* Sources -> ImportExport`DefaultSources["Table"], *)
    FunctionChannels -> {"FileNames"},
    AvailableElements -> {"Data", "Grid"},
    DefaultElement -> "Data"
]


End[]
