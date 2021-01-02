(*****************************)
(* Settings for Mathics Core *)
(*****************************)

Settings`$TraceGet::usage = "If this Boolean variable is set True, 'Get' traces the lines it reads that start a new expression";
Settings`$TraceGet = False
Unprotect[Settings`$TraceGet]

System`MathicsVersion::usage = "This string is the version of Mathics we are running."
(* System`MathicsVersion = "xxx" - set inside Mathics *)
