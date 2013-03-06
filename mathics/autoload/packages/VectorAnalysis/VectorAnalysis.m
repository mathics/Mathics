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
CoordinatesToCartesian[pt_?IsVecQ, coordsys_:$CoordinateSystem] := 
    Module[{v1 = pt[[1]], v2 = pt[[2]], v3 = pt[[3]]}, 
        Switch[coordsys,
            Cartesian, {v1, v2, v3},
            Spherical, {v1 Sin[v2] Cos[v3], v1 Sin[v2] Sin[v3], v1 Cos[v2]},
            Cylindrical, {v1 Cos[v2], v1 Sin[v2], v3}
        ]
    ]

CoordinatesFromCartesian[pt_?IsVecQ, coordsys_:$CoordinateSystem] :=
    Module[{v1 = pt[[1]], v2 = pt[[2]], v3 = pt[[3]]}, 
        Switch[coordsys,
            Cartesian, {v1, v2, v3},
            Spherical, {Sqrt[v1^2 + v2^2 + v3^2], ArcCos[v3/Sqrt[v1^2 + v2^2 + v3^2]], ArcTan[v1, v2]},
            Cylindrical, {Sqrt[v1^2 + v2^2], ArcTan[v1, v2], v3}
        ]
    ]

Attributes[DotProduct] = {ReadProtected, Protected};
Attributes[CrossProduct] = {ReadProtected, Protected};
Attributes[ScalarTripleProduct] = {ReadProtected, Protected};
Attributes[CoordinatesToCartesian] = {ReadProtected, Protected};
Attributes[CoordinatesFromCartesian] = {ReadProtected, Protected};

EndPackage[]
