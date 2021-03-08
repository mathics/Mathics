(* CSV Exporter *)

Begin["System`Convert`TableDump`"]

Options[CSVExport] = {
    "CharacterEncoding" :> $CharacterEncoding,
    "FieldSeparators" -> ","
};

CSVExport[strm_OutputStream, data_, OptionsPattern[]]:=
    Module[{char, wraplist, sep = OptionValue["FieldSeparators"]},        
        If[strm === $Failed, Return[$Failed]];
        wraplist[x_] := If[Head[x] === List, x, {x}];
        char = Map[ToString, wraplist /@ wraplist[data], {2}];
        char = StringJoin[Riffle[Riffle[#, sep] & /@ char, "\n"]];
        WriteString[strm, char];
    ]

ImportExport`RegisterExport[
    "CSV",
    System`Convert`TableDump`CSVExport,
    FunctionChannels -> {"Streams"},
    Options -> {"ByteOrderMark"},
    DefaultElement -> "Plaintext",
    BinaryFormat -> False,
    Options -> {
        "CharacterEncoding",
        "FieldSeparators"
    }
]


End[]
