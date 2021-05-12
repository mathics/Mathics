(*****************************)
(* Settings for Mathics Core *)
(*****************************)

Settings`$TraceGet::usage = "If this Boolean variable is set True, 'Get' traces the lines it reads that start a new expression";
Settings`$TraceGet = False
Unprotect[Settings`$TraceGet]


Settings`$PreferredBackendMethod::usage = "Set this do whether to use mpmath, numpy or Sympy for numeric and symbolic constants and methods when there is a choice";
Settings`$PreferredBackendMethod = "sympy"
Unprotect[Settings`$PreferredBackendMethod]

(* Some packages like Feyncalc, test for whether a they are being used
inside a notbook. *)
System`$Notebooks::usage = "Set True if the Mathics is being used with a notebook-based front end.";
System`$Notebooks = False;
Unprotect[System`$Notebooks];
