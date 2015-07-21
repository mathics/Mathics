(* CSV Importer *)

Begin["System`Convert`TableDump`"]


ImportCSV[filename_String]:=
    Module[{stream, data, grid},
        stream = OpenRead[filename];
        data = StringSplit[#, ","]& /@ ReadList[stream, String];
        grid = Grid[data];
        Close[stream];
        {
            "Data" -> data,
            "Grid" -> grid
        }
]

RegisterImport[
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
