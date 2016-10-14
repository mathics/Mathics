(* CSV Importer *)

Begin["System`Convert`TableDump`"]


Options[ImportCSV] = {
    "CharacterEncoding" :> $CharacterEncoding,
    "FieldSeparators" -> ","
};

ImportCSV[filename_String, opts:OptionsPattern[]]:=
    Module[{stream, data, grid, sep = "FieldSeparators" /. {opts}},
        stream = OpenRead @@ Join[{filename}, FilterRules[{opts}, "CharacterEncoding"]];
        data = StringSplit[#, sep]& /@ ReadList[stream, String];
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
    DefaultElement -> "Data",
    Options -> {
        "CharacterEncoding",
        "FieldSeparators"
    }
]


End[]
