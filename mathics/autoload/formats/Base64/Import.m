Begin["System`Convert`B64Dump`"]


Options[B64Import] = {
    "CharacterEncoding" :> $CharacterEncoding
};

B64Import[strm_, expr_, OptionsPattern[]] := 
    Module[{data, grid, sep = OptionValue["FieldSeparators"]},
        data = Read[stream, Record, RecordSeparator->{}];
        { "Data" -> B64Decode[data] }
]

ImportExport`RegisterImport[
    "Base64",
    System`Convert`B64Dump`B64Import,
    {  },
    (* Sources -> ImportExport`DefaultSources["Table"], *)
    FunctionChannels -> {"FileNames"},
    AvailableElements -> {"Data"},
    DefaultElement -> "Data",
    Options -> {
        "CharacterEncoding",
        "FieldSeparators"
    }
]



End[]