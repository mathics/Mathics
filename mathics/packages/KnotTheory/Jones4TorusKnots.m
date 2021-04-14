BeginPackage["KnotTheory`Jones4TorusKnots`", {"KnotTheory`"}]
Message[KnotTheory::loading, "Jones4TorusKnots`"]

Begin["`Private`"]

Ks = {
  TorusKnot[3, 2], TorusKnot[5, 2], TorusKnot[7, 2], TorusKnot[4, 3],
  TorusKnot[9, 2], TorusKnot[5, 3], TorusKnot[11, 2], TorusKnot[13, 2],
  TorusKnot[7, 3], TorusKnot[5, 4], TorusKnot[15, 2], TorusKnot[8, 3],
  TorusKnot[17, 2], TorusKnot[19, 2], TorusKnot[10, 3], TorusKnot[7, 4],
  TorusKnot[21, 2], TorusKnot[11, 3], TorusKnot[23, 2], TorusKnot[6, 5],
  TorusKnot[25, 2], TorusKnot[13, 3], TorusKnot[9, 4], TorusKnot[27, 2],
  TorusKnot[7, 5], TorusKnot[14, 3], TorusKnot[29, 2], TorusKnot[31, 2],
  TorusKnot[8, 5], TorusKnot[16, 3], TorusKnot[11, 4], TorusKnot[33, 2],
  TorusKnot[17, 3], TorusKnot[7, 6], TorusKnot[35, 2], TorusKnot[9, 5]
}

Js = {q + q^3 - q^4, q^2 + q^4 - q^5 + q^6 - q^7, q^3 + q^5 - q^6 + q^7 - q^8 + 
  q^9 - q^10, q^3 + q^5 - q^8, q^4 + q^6 - q^7 + q^8 - q^9 + q^10 - q^11 + 
  q^12 - q^13, q^4 + q^6 - q^10, q^5 + q^7 - q^8 + q^9 - q^10 + q^11 - q^12 + 
  q^13 - q^14 + q^15 - q^16, q^6 + q^8 - q^9 + q^10 - q^11 + q^12 - q^13 + 
  q^14 - q^15 + q^16 - q^17 + q^18 - q^19, q^6 + q^8 - q^14, 
 q^6 + q^8 + q^10 - q^11 - q^13, q^7 + q^9 - q^10 + q^11 - q^12 + q^13 - 
  q^14 + q^15 - q^16 + q^17 - q^18 + q^19 - q^20 + q^21 - q^22, 
 q^7 + q^9 - q^16, q^8 + q^10 - q^11 + q^12 - q^13 + q^14 - q^15 + q^16 - 
  q^17 + q^18 - q^19 + q^20 - q^21 + q^22 - q^23 + q^24 - q^25, 
 q^9 + q^11 - q^12 + q^13 - q^14 + q^15 - q^16 + q^17 - q^18 + q^19 - q^20 + 
  q^21 - q^22 + q^23 - q^24 + q^25 - q^26 + q^27 - q^28, q^9 + q^11 - q^20, 
 q^9 + q^11 + q^13 - q^14 + q^15 - q^16 - q^18, 
 q^10 + q^12 - q^13 + q^14 - q^15 + q^16 - q^17 + q^18 - q^19 + q^20 - q^21 + 
  q^22 - q^23 + q^24 - q^25 + q^26 - q^27 + q^28 - q^29 + q^30 - q^31, 
 q^10 + q^12 - q^22, q^11 + q^13 - q^14 + q^15 - q^16 + q^17 - q^18 + q^19 - 
  q^20 + q^21 - q^22 + q^23 - q^24 + q^25 - q^26 + q^27 - q^28 + q^29 - 
  q^30 + q^31 - q^32 + q^33 - q^34, q^10 + q^12 + q^14 - q^17 - q^19, 
 q^12 + q^14 - q^15 + q^16 - q^17 + q^18 - q^19 + q^20 - q^21 + q^22 - q^23 + 
  q^24 - q^25 + q^26 - q^27 + q^28 - q^29 + q^30 - q^31 + q^32 - q^33 + 
  q^34 - q^35 + q^36 - q^37, q^12 + q^14 - q^26, 
 q^12 + q^14 + q^16 - q^17 + q^18 - q^19 + q^20 - q^21 - q^23, 
 q^13 + q^15 - q^16 + q^17 - q^18 + q^19 - q^20 + q^21 - q^22 + q^23 - q^24 + 
  q^25 - q^26 + q^27 - q^28 + q^29 - q^30 + q^31 - q^32 + q^33 - q^34 + 
  q^35 - q^36 + q^37 - q^38 + q^39 - q^40, q^12 + q^14 + q^16 - q^20 - q^22, 
 q^13 + q^15 - q^28, q^14 + q^16 - q^17 + q^18 - q^19 + q^20 - q^21 + q^22 - 
  q^23 + q^24 - q^25 + q^26 - q^27 + q^28 - q^29 + q^30 - q^31 + q^32 - 
  q^33 + q^34 - q^35 + q^36 - q^37 + q^38 - q^39 + q^40 - q^41 + q^42 - q^43, 
 q^15 + q^17 - q^18 + q^19 - q^20 + q^21 - q^22 + q^23 - q^24 + q^25 - q^26 + 
  q^27 - q^28 + q^29 - q^30 + q^31 - q^32 + q^33 - q^34 + q^35 - q^36 + 
  q^37 - q^38 + q^39 - q^40 + q^41 - q^42 + q^43 - q^44 + q^45 - q^46, 
 q^14 + q^16 + q^18 - q^23 - q^25, q^15 + q^17 - q^32, 
 q^15 + q^17 + q^19 - q^20 + q^21 - q^22 + q^23 - q^24 + q^25 - q^26 - q^28, 
 q^16 + q^18 - q^19 + q^20 - q^21 + q^22 - q^23 + q^24 - q^25 + q^26 - q^27 + 
  q^28 - q^29 + q^30 - q^31 + q^32 - q^33 + q^34 - q^35 + q^36 - q^37 + 
  q^38 - q^39 + q^40 - q^41 + q^42 - q^43 + q^44 - q^45 + q^46 - q^47 + 
  q^48 - q^49, q^16 + q^18 - q^34, q^15 + q^17 + q^19 + q^21 - q^22 - q^24 - 
  q^26, q^17 + q^19 - q^20 + q^21 - q^22 + q^23 - q^24 + q^25 - q^26 + q^27 - 
  q^28 + q^29 - q^30 + q^31 - q^32 + q^33 - q^34 + q^35 - q^36 + q^37 - 
  q^38 + q^39 - q^40 + q^41 - q^42 + q^43 - q^44 + q^45 - q^46 + q^47 - 
  q^48 + q^49 - q^50 + q^51 - q^52, q^16 + q^18 + q^20 - q^26 - q^28}

Js = Function /@ (Js /. q -> #)

MapThread[(Jones[#1] = #2)&, {Ks, Js}]

Clear[Ks, Js]

End[]; EndPackage[]

