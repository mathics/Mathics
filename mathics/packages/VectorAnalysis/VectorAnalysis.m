(* ::Package:: *)

(* :Title: Three-Dimensional Vector Analysis *)

(* :Author: Angus Griffith *)

(* :Summary:
Standard vector calculus operators in three dimensions. Includes coordinate
transformations between orthogonal coordinate systems. *)


BeginPackage["VectorAnalysis`"]

$CoordSysList = {Cartesian, Spherical, Cylindrical};

$IsVecQ[v_] := Length[v] == 3 && VectorQ[v];


Unprotect[DotProduct, CrossProduct, ScalarTripleProduct, 
    CoordinatesToCartesian, CoordinatesFromCartesian, Coordinates,
    Parameters, CoordinateRanges, ParameterRanges, SetCoordinates,
    CoordinateSystem, ScaleFactors, JacobianDeterminant, JacobianMatrix];

  (* ============================ Dot Product ============================ *)

DotProduct::usage =
"DotProduct[v1, v2] gives the dot product between v1 and v2 in three spatial
dimensions. DotProduct[v1, v2, coordsys] gives the dot product of vectors v1
and v2 in the specified coodrinate system, coordsys.";

DotProduct[v1_?$IsVecQ, v2_?$IsVecQ, coordsys_:CoordinateSystem] :=
    Module[{c1, c2},
    c1 = CoordinatesToCartesian[v1, coordsys];
    c2 = CoordinatesToCartesian[v2, coordsys];
    Apply[Plus, c1 * c2]]

Attributes[DotProduct] = {ReadProtected, Protected};


  (* =========================== Cross Product =========================== *)

CrossProduct::usage =
"CrossProduct[v1, v2] gives the cross product between v1 and v2 in three
spatial dimensions. DotProduct[v1, v2, coordsys] gives the cross product of
vectors v1 and v2 in the specified coodrinate system, coordsys.";

CrossProduct[v1_?$IsVecQ, v2_?$IsVecQ, coordsys_:CoordinateSystem] :=
    Module[{c1, c2},
    c1 = CoordinatesToCartesian[v1, coordsys];
    c2 = CoordinatesToCartesian[v2, coordsys];
    CoordinatesFromCartesian[Det[{IdentityMatrix[3], c1, c2}]]]

Attributes[CrossProduct] = {ReadProtected, Protected};


  (* ======================= Scalar Triple Product ======================= *)

ScalarTripleProduct::usage =
"ScalarTripleProduct[v1, v2, v3] gives the scalar triple product product
between v1, v2 and v3 in three spatial dimensions.
ScalarTripleProduct[v1, v2, v3, coordsys] gives the scalar triple product of
vectors v1, v2 and v3 in the specified coodrinate system, coordsys.";

ScalarTripleProduct[v1_?$IsVecQ, v2_?$IsVecQ, v3_?$IsVecQ,
        coordsys_:CoordinateSystem] :=
    Module[{c1, c2, c3},
    c1 = CoordinatesToCartesian[v1, coordsys];
    c2 = CoordinatesToCartesian[v2, coordsys];
    c3 = CoordinatesToCartesian[v3, coordsys];
    Det[{c1, c2, c3}]]

Attributes[ScalarTripleProduct] = {ReadProtected, Protected};


  (* ======================= Coordinates To Cartesian ==================== *)

CoordinatesToCartesian::usage =
"CoordinatesToCartesian[pt] converts the given point, pt, from the default
coordinates to Cartesian coordinates. CoordinatesToCartesian[pt, coordsys]
converts the given point, pt, from the specified coordinate system, coordsys,
to Cartesian coordinates.";

CoordinatesToCartesian[pt_?$IsVecQ, coordsys_:CoordinateSystem] :=
    Module[{v1 = pt[[1]], v2 = pt[[2]], v3 = pt[[3]]},
        Switch[coordsys,
            Cartesian, {v1, v2, v3},
            Spherical, {v1 Sin[v2] Cos[v3], v1 Sin[v2] Sin[v3], v1 Cos[v2]},
            Cylindrical, {v1 Cos[v2], v1 Sin[v2], v3}
        ]
    ]

Attributes[CoordinatesToCartesian] = {ReadProtected, Protected};


  (* ====================== Coordinates From Cartesian =================== *)

CoordinatesFromCartesian::usage =
"CoordinatesFromCartesian[pt] converts the given point, pt, from Cartesian
coordinates to the default coordinate system.
CoordinatesFromCartesian[pt, coordsys] converts the given point, pt, from
Cartesian coordinates to the specified coordinate system, coordsys.";

CoordinatesFromCartesian[pt_?$IsVecQ, coordsys_:CoordinateSystem] :=
    Module[{v1 = pt[[1]], v2 = pt[[2]], v3 = pt[[3]]},
        Switch[coordsys,
            Cartesian, {v1, v2, v3},
            Spherical, {Sqrt[v1^2 + v2^2 + v3^2],
                ArcCos[v3/Sqrt[v1^2 + v2^2 + v3^2]], ArcTan[v1, v2]},
            Cylindrical, {Sqrt[v1^2 + v2^2], ArcTan[v1, v2], v3}
        ]
    ]

Attributes[CoordinatesFromCartesian] = {ReadProtected, Protected};


  (* ============================ Coordinates ============================ *)

Coordinates::usage =
"Coordinates[] gives the default cordinate variables of the current coordinate
system. Coordinates[coordsys] gives the default coordinate variables of the
specified coordinate system, coordsys.";

Coordinates::invalid = "`1` is not a valid coordinate system specification.";

Coordinates[] := Coordinates[CoordinateSystem];
Coordinates[Cartesian] ^= {Xx, Yy, Zz};
Coordinates[Spherical] ^= {Rr, Ttheta, Pphi};
Coordinates[Cylindrical] ^= {Rr, Ttheta, Zz};

Attributes[Coordinates] = {ReadProtected, Protected};


  (* ============================= Parameters ============================ *)

Parameters::usage =
"Parameters[] gives the default paramater variables of the current coordinate
system. Parameters[coordsys] gives the default paramater variables for the
specified coordinate system, coordsys.";

Parameters[] := Parameters[CoordinateSystem];
Parameters[Cartesian] ^= {};
Parameters[Spherical] ^= {};
Parameters[Cylindrical] ^= {};

Attributes[Parameters] = {ReadProtected, Protected};


  (* ========================= Coordinate Ranges ========================= *)

CoordinateRanges::usage =
"CoordinateRanges[] gives the acceptable range of coordinates for the current
coordinate system. CoordinateRanges[coordsys] gives the acceptable range of
coordinates for the specified coordinate system, coordsys.";

CoordinateRanges[] := CoordinateRanges[CoordinateSystem];
CoordinateRanges[Cartesian] ^= {-Infinity < Xx < Infinity,
    -Infinity < Yy < Infinity, -Infinity < Zz <Infinity};
CoordinateRanges[Spherical] ^=   {0 <= Rr < Infinity, 0 <= Ttheta <= Pi,
    -Pi < Pphi <= Pi};
CoordinateRanges[Cylindrical] ^= {0 <= Rr < Infinity, -Pi < Ttheta <= Pi,
     -Infinity < Zz <Infinity};

Attributes[CoordinateRanges] = {ReadProtected, Protected};


  (* ========================== Parameter Ranges ========================= *)

ParameterRanges::usage =
"ParameterRanges[] gives the acceptable range of parameters for the current
coordinate system. ParameterRanges[coordsys] gives the acceptable range of
parameters for the specified coordinate system, coordsys.";

ParameterRanges[] := ParametersRanges[CoordinateSystem];
ParameterRanges[Cartesian] ^= Null;
ParameterRanges[Spherical] ^= Null;
ParameterRanges[Cylindrical] ^= Null;

Attributes[ParameterRanges] = {ReadProtected, Protected};


  (* ========================== Set Coordinates ========================== *)

SetCoordinates::usage =
"SetCoordinates[coordsys] sets the current coordinate system";

SetCoordinates[coordsys_Symbol] :=
    If[MemberQ[$CoordSysList, coordsys],
        Unprotect[CoordinateSystem];
        CoordinateSystem = coordsys;
        Protect[CoordinateSystem];
        Apply[coordsys, Coordinates[coordsys]],
        Message[Coordinates::invalid, coordsys];
        $Failed]

Attributes[SetCoordinates] = {ReadProtected, Protected};


  (* ========================= Coordinate System ========================= *)

CoordinateSystem::usage = "CoordinateSystem is the current coordinate system";

CoordinateSystem = Cartesian;

Attributes[CoordinateSystem] = {ReadProtected, Protected};


  (* =========================== Scale Factors =========================== *)

ScaleFactors::usage = 
"ScaleFactors[pt] gives the scale factors at point pt in the current
coordinate system. ScaleFactors[pt, coordsys] gives the scale factors in the
specified coordinate system at point pt.";

ScaleFactors[pt_?$IsVecQ, coordsys_:CoordinateSystem] :=
    Module[{},
        Switch[coordsys,
            Cartesian, {1, 1, 1},
            Spherical, {1, pt[[1]], pt[[1]] Sin[pt[[2]]]},
            Cylindrical, {1, pt[[1]], 1}
        ]
    ]

ScaleFactors[coordsys_Symbol:CoordinateSystem] := 
        ScaleFactors[Coordinates[coordsys], coordsys]
        
Attributes[ScaleFactors] = {ReadProtected, Protected};


  (* ======================== Jacobian Determinant ======================= *)

JacobianDeterminant::usage =
"JacobianDeterminant[pt] gives the determinant of the Jacobian describing
the transformation from the current coordinate system to Cartesian
coordinates at the specified point pt. JacobianDeterminant[pt, coordsys]
gives the determinant of the Jacobian Matrix in the specified coordinates
at the given point pt.";

JacobianDeterminant[pt_?$IsVecQ, coordsys_:CoordinateSystem] :=
    Times @@ ScaleFactors[pt, coordsys]

JacobianDeterminant[coordsys_Symbol:CoordinateSystem] :=
    Times @@ ScaleFactors[coordsys]

Attributes[JacobianDeterminant] = {ReadProtected, Protected};


  (* ========================== Jacobian Matrix ========================== *)

JacobianMatrix::usage =
"JacobianMatrix[pt] gives the Jacobian describing the transformation from 
the current coordinate system to Cartesian coordinates at the specified 
point pt. JacobianMatrix[pt, coordsys] gives the Jacobian Matrix in the 
specified coordinates at the given point pt.";

JacobianMatrix[pt_?$IsVecQ, coordsys_:CoordinateSystem] :=
    Module[{cpt},
        cpt = CoordinatesToCartesian[pt, coordsys];
        Outer[D, cpt, Coordinates[coordsys]]
    ]

JacobianMatrix[coordsys_Symbol:CoordinateSystem] :=
    JacobianMatrix[Coordinates[coordsys], coordsys]

Attributes[JacobianMatrix] = {ReadProtected, Protected};


EndPackage[]
