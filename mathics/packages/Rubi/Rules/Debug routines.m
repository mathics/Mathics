(* ::Package:: *)

(* ::Title:: *)
(*Debug Routines*)


Ints[u_,x_] := ($ShowSteps=True; Int[u,x]);
Intn[u_,x_] := ($ShowSteps=False; Int[u,x]);


int[u_] := Int[u,x];
ints[u_] := ($ShowSteps=True; Int[u,x]);
intn[u_] := ($ShowSteps=False; Int[u,x]);
