(* CSV Importer *)

Begin["System`Convert`TableDump`"]


Options[ImportCSV] = {
    "CharacterEncoding" :> $CharacterEncoding,
    "FieldSeparators" -> ","
};

ImportCSV[stream_InputStream, OptionsPattern[]]:=
    Module[{data, grid, sep = OptionValue["FieldSeparators"]},        
        data = StringSplit[#, sep]& /@ ReadList[stream, String];
        grid = Grid[data];
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
    FunctionChannels -> {"Streams"},
    AvailableElements -> {"Data", "Grid"},
    DefaultElement -> "Data",
    Options -> {
        "CharacterEncoding",
        "FieldSeparators"
    }
]


End[]
