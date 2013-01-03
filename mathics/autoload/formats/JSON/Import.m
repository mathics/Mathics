(* JSON Importer *)

(* Based on converter at http://stackoverflow.com/questions/2633003/parsing-and-generating-json *)
importJSON[filename_String]:= 
    Module[{data},
        data = Import[filename, {"Text", "String"}];
        data = StringReplace[data, {
            "["     -> "(*MAGIC[*){",
            "]"     -> "(*MAGIC]*)}",
            ":"     -> "(*MAGIC:*)->",
            "true"  -> "(*MAGICt*)True",
           "false" -> "(*MAGICf*)False",
            "null"  -> "(*MAGICn*)Null",
            "e"     -> "(*MAGICe*)*10^",
            "E"     -> "(*MAGICE*)*10^"
        }];
        data = ToString@FullForm@ToExpression[data];
        data = ToExpression@StringReplace[data, {
            "(*MAGIC[*){"     -> "[",
            "(*MAGIC]*)}"     -> "]",
            "(*MAGIC:*)->"    -> ":",
            "(*MAGICt*)True"  -> "true",
            "(*MAGICf*)False" -> "false",
            "(*MAGICn*)Null"  -> "null",
            "(*MAGICe*)*10^"  -> "e",
            "(*MAGICE*)*10^"  -> "E"
        }];
        {"Data" -> data}
    ]

RegisterImport[
    "JSON",
    importJSON,
    {},
    AvailableElements -> {"Data"},
    DefaultElement -> "Data",
    FunctionChannels -> {"FileNames"},
    (* "Sources" -> {"JLink`","Convert`JSON`"} *)
]
