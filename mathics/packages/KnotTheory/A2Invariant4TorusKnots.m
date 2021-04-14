BeginPackage["KnotTheory`A2Invariant4TorusKnots`", {"KnotTheory`"}]
Message[KnotTheory::loading, "A2Invariant4TorusKnots`"]

Begin["`Private`"]

KOs = {
  TorusKnot[3, 2], TorusKnot[5, 2], TorusKnot[7, 2], TorusKnot[4, 3],
  TorusKnot[9, 2], TorusKnot[5, 3], TorusKnot[11, 2], TorusKnot[13, 2],
  TorusKnot[7, 3], TorusKnot[5, 4], TorusKnot[15, 2]
}

Is = {
 q^2 + q^4 + 2*q^6 + q^8 - q^12 - q^14, q^6 + q^8 + 2*q^10 + q^12 + q^14 - 
  q^18 - q^20 - q^22, q^10 + q^12 + 2*q^14 + q^16 + q^18 - q^26 - q^28 - 
  q^30, q^10 + q^12 + 2*q^14 + 2*q^16 + 2*q^18 - q^22 - 2*q^24 - 2*q^26 - 
  q^28 + q^32, q^14 + q^16 + 2*q^18 + q^20 + q^22 - q^34 - q^36 - q^38, 
 q^14 + q^16 + 2*q^18 + 2*q^20 + 2*q^22 + q^24 - 2*q^28 - 2*q^30 - 2*q^32 - 
  q^34 + q^40, q^18 + q^20 + 2*q^22 + q^24 + q^26 - q^42 - q^44 - q^46, 
 q^22 + q^24 + 2*q^26 + q^28 + q^30 - q^50 - q^52 - q^54, 
 q^22 + q^24 + 2*q^26 + 2*q^28 + 2*q^30 + q^32 + q^34 - q^38 - 2*q^40 - 
  2*q^42 - 2*q^44 - q^46 + q^56, q^22 + q^24 + 2*q^26 + 2*q^28 + 3*q^30 + 
  2*q^32 + q^34 - q^36 - 2*q^38 - 3*q^40 - 3*q^42 - 2*q^44 - q^46 + q^48 + 
  q^50 + q^52, q^26 + q^28 + 2*q^30 + q^32 + q^34 - q^58 - q^60 - q^62
}

Is = Function /@ (Is /. q -> #)

MapThread[(A2Invariant[#1] = #2)&, {KOs, Is}]

Clear[KOs, Is]

End[]; EndPackage[]

