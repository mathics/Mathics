(* CSV Importer *)

Begin["System`Convert`TableDump`"]


Options[ImportCSV] = {
    "CharacterEncoding" :> $CharacterEncoding,
    "FieldSeparators" -> ","
};

ImportCSV[stream, OptionsPattern[]]:=
    Module[{data, grid, sep = OptionValue["FieldSeparators"]},        
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
    FunctionChannels -> {"Stream"},
    AvailableElements -> {"Data", "Grid"},
    DefaultElement -> "Data",
    Options -> {
        "CharacterEncoding",
        "FieldSeparators"
    }
]


End[]
