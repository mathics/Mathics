(* JSON Importer *)

Begin["System`Convert`JSONDump`"]


(* Based on converter at http://stackoverflow.com/questions/2633003/parsing-and-generating-json

First, a really quick-and-dirty partial solution to JSON parsing would be this:
ToExpression[StringReplace[json, {"["->"{", "]"->"}", ":"->"->"}]

That is, just replace square brackets with curly braces and colons with
arrows and then eval it. All that remains is to not do those
substitutions inside of strings. (Also need a few more substitutions
for null, true, false, and scientific notation.)

For this we add *MAGIC<something>* before the evaluation and
remove the same after evaluation.
 *)
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

ImportExport`RegisterImport[
    "JSON",
    System`Convert`JSONDump`importJSON,
    {},
    AvailableElements -> {"Data"},
    DefaultElement -> "Data",
    FunctionChannels -> {"FileNames"}
]


End[]
