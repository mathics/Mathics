(* ::Package:: *)

(* :Title: Three-Dimensional Vector Analysis *)

(* :Author: Angus Griffith *)

(* :Summary: 
Standard vector calculus operators in three dimensions. Includes
coordinate transformations between orthogonal coordinate systems.
*)

BeginPackage["VectorAnalysis`"]

$CoordSysList = {Cartesian};

$CoordinateSystem = Cartesian

IsVecQ[v_] := Length[v] == 3 && VectorQ[v];

DotProduct[v1_?IsVecQ, v2_?IsVecQ, coordsys_:$CoordinateSystem] :=
    Module[{c1, c2},
    c1 = CoordinatesToCartesian[v1, coordsys];
    c2 = CoordinatesToCartesian[v2, coordsys];
    Apply[Plus, c1 * c2]]

CrossProduct[v1_?IsVecQ, v2_?IsVecQ, coordsys_:$CoordinateSystem] :=
    Module[{c1, c2},
    c1 = CoordinatesToCartesian[v1, coordsys];
    c2 = CoordinatesToCartesian[v2, coordsys];
    CoordinatesFromCartesian[Det[{IdentityMatrix[3], c1, c2}]]]

ScalarTripleProduct[v1_?IsVecQ, v2_?IsVecQ, v3_?IsVecQ, 
        coordsys_:$CoordinateSystem] := 
    Module[{c1, c2, c3},
    c1 = CoordinatesToCartesian[v1, coordsys];
    c2 = CoordinatesToCartesian[v2, coordsys];
    c3 = CoordinatesToCartesian[v3, coordsys];
    Det[{c1, c2, c3}]]

(* TODO: Other coordinate systems *)
CoordinatesToCartesian[pt_?IsVecQ, coordsys_:$CoordinateSystem] := pt

CoordinatesFromCartesian[pt_?IsVecQ, coordsys_:$CoordinateSystem] := pt

Attributes[DotProduct] = {ReadProtected, Protected};
Attributes[CrossProduct] = {ReadProtected, Protected};
Attributes[ScalarTripleProduct] = {ReadProtected, Protected};
Attributes[CoordinatesToCartesian] = {ReadProtected, Protected};
Attributes[CoordinatesFromCartesian] = {ReadProtected, Protected};

EndPackage[]
