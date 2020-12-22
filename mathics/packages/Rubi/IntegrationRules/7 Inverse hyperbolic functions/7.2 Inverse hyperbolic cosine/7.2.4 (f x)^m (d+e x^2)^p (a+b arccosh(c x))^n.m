(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* ::Code:: *)
Int[(f_.*x_)^m_.*(d1_+e1_.*x_)^p_.*(d2_+e2_.*x_)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  Int[(f*x)^m*(d1*d2+e1*e2*x^2)^p*(a+b*ArcCosh[c*x])^n,x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m,n},x] && EqQ[d2*e1+d1*e2,0] && IntegerQ[p]


(* ::Code:: *)
Int[x_*(a_.+b_.*ArcCosh[c_.*x_])^n_./(d_+e_.*x_^2),x_Symbol] :=
  1/e*Subst[Int[(a+b*x)^n*Coth[x],x],x,ArcCosh[c*x]] /;
FreeQ[{a,b,c,d,e},x] && EqQ[c^2*d+e,0] && IGtQ[n,0]


(* ::Code:: *)
Int[x_*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n/(2*e*(p+1)) - 
  b*n/(2*c*(p+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d,e,p},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && NeQ[p,-1]


(* ::Code:: *)
Int[x_*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (d1+e1*x)^(p+1)*(d2+e2*x)^(p+1)*(a+b*ArcCosh[c*x])^n/(2*e1*e2*(p+1)) - 
  b*n/(2*c*(p+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,p},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[n,0] && NeQ[p,-1]


(* ::Code:: *)
Int[(a_.+b_.*ArcCosh[c_.*x_])^n_./(x_*(d_+e_.*x_^2)),x_Symbol] :=
  -1/d*Subst[Int[(a+b*x)^n/(Cosh[x]*Sinh[x]),x],x,ArcCosh[c*x]] /;
FreeQ[{a,b,c,d,e},x] && EqQ[c^2*d+e,0] && IGtQ[n,0]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*(d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n/(d*f*(m+1)) + 
  b*c*n/(f*(m+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m+1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d,e,f,m,p},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && EqQ[m+2*p+3,0] && NeQ[m,-1]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*(d1+e1*x)^(p+1)*(d2+e2*x)^(p+1)*(a+b*ArcCosh[c*x])^n/(d1*d2*f*(m+1)) + 
  b*c*n/(f*(m+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m+1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m,p},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[n,0] && EqQ[m+2*p+3,0] && NeQ[p,-1]


(* ::Code:: *)
Int[(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])/x_,x_Symbol] :=
  (d+e*x^2)^p*(a+b*ArcCosh[c*x])/(2*p) - 
  b*c*(-d)^p/(2*p)*Int[(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2),x] + 
  d*Int[(d+e*x^2)^(p-1)*(a+b*ArcCosh[c*x])/x,x] /;
FreeQ[{a,b,c,d,e},x] && EqQ[c^2*d+e,0] && IGtQ[p,0]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_]),x_Symbol] :=
  (f*x)^(m+1)*(d+e*x^2)^p*(a+b*ArcCosh[c*x])/(f*(m+1)) - 
  b*c*(-d)^p/(f*(m+1))*Int[(f*x)^(m+1)*(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2),x] - 
  2*e*p/(f^2*(m+1))*Int[(f*x)^(m+2)*(d+e*x^2)^(p-1)*(a+b*ArcCosh[c*x]),x] /;
FreeQ[{a,b,c,d,e,f},x] && EqQ[c^2*d+e,0] && IGtQ[p,0] && ILtQ[(m+1)/2,0]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_]),x_Symbol] :=
  With[{u=IntHide[(f*x)^m*(d+e*x^2)^p,x]},  
  Dist[a+b*ArcCosh[c*x],u,x] - b*c*Int[SimplifyIntegrand[u/(Sqrt[1+c*x]*Sqrt[-1+c*x]),x],x]] /;
FreeQ[{a,b,c,d,e,f,m},x] && EqQ[c^2*d+e,0] && IGtQ[p,0]


(* ::Code:: *)
Int[x_^m_*(d_+e_.*x_^2)^p_*(a_.+b_.*ArcCosh[c_.*x_]),x_Symbol] :=
  With[{u=IntHide[x^m*(d+e*x^2)^p,x]},  
  Dist[a+b*ArcCosh[c*x],u] - 
  b*c*Simp[Sqrt[d+e*x^2]/(Sqrt[1+c*x]*Sqrt[-1+c*x])]*Int[SimplifyIntegrand[u/Sqrt[d+e*x^2],x],x]] /;
FreeQ[{a,b,c,d,e},x] && EqQ[c^2*d+e,0] && IntegerQ[p-1/2] && NeQ[p,-1/2] && (IGtQ[(m+1)/2,0] || ILtQ[(m+2*p+3)/2,0])


(* ::Code:: *)
Int[x_^m_*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_]),x_Symbol] :=
  With[{u=IntHide[x^m*(d1+e1*x)^p*(d2+e2*x)^p,x]},  
  Dist[a+b*ArcCosh[c*x],u] - 
  b*c*Simp[Sqrt[d1+e1*x]*Sqrt[d2+e2*x]/(Sqrt[1+c*x]*Sqrt[-1+c*x])]*Int[SimplifyIntegrand[u/(Sqrt[d1+e1*x]*Sqrt[d2+e2*x]),x],x]] /;
FreeQ[{a,b,c,d1,e1,d2,e2},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && IntegerQ[p-1/2] && NeQ[p,-1/2] && (IGtQ[(m+1)/2,0] || ILtQ[(m+2*p+3)/2,0])


(* ::Code:: *)
Int[(f_.*x_)^m_*Sqrt[d_+e_.*x_^2]*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*Sqrt[d+e*x^2]*(a+b*ArcCosh[c*x])^n/(f*(m+1)) - 
  b*c*n/(f*(m+1))*Simp[Sqrt[d+e*x^2]/(Sqrt[1+c*x]*Sqrt[-1+c*x])]*
    Int[(f*x)^(m+1)*(a+b*ArcCosh[c*x])^(n-1),x] - 
  c^2/(f^2*(m+1))*Simp[Sqrt[d+e*x^2]/(Sqrt[1+c*x]*Sqrt[-1+c*x])]*
    Int[(f*x)^(m+2)*(a+b*ArcCosh[c*x])^n/(Sqrt[1+c*x]*Sqrt[-1+c*x]),x] /;
FreeQ[{a,b,c,d,e,f},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && LtQ[m,-1]


(* ::Code:: *)
Int[(f_.*x_)^m_*Sqrt[d1_+e1_.*x_]*Sqrt[d2_+e2_.*x_]*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*Sqrt[d1+e1*x]*Sqrt[d2+e2*x]*(a+b*ArcCosh[c*x])^n/(f*(m+1)) - 
  b*c*n/(f*(m+1))*Simp[Sqrt[d1+e1*x]/Sqrt[1+c*x]]*Simp[Sqrt[d2+e2*x]/Sqrt[-1+c*x]]*
    Int[(f*x)^(m+1)*(a+b*ArcCosh[c*x])^(n-1),x] - 
  c^2/(f^2*(m+1))*Simp[Sqrt[d1+e1*x]/Sqrt[1+c*x]]*Simp[Sqrt[d2+e2*x]/Sqrt[-1+c*x]]*
    Int[((f*x)^(m+2)*(a+b*ArcCosh[c*x])^n)/(Sqrt[1+c*x]*Sqrt[-1+c*x]),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[n,0] && LtQ[m,-1]


(* ::Code:: *)
Int[(f_.*x_)^m_*Sqrt[d_+e_.*x_^2]*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*Sqrt[d+e*x^2]*(a+b*ArcCosh[c*x])^n/(f*(m+2)) - 
  b*c*n/(f*(m+2))*Simp[Sqrt[d+e*x^2]/(Sqrt[1+c*x]*Sqrt[-1+c*x])]*
    Int[(f*x)^(m+1)*(a+b*ArcCosh[c*x])^(n-1),x] - 
  1/(m+2)*Simp[Sqrt[d+e*x^2]/(Sqrt[1+c*x]*Sqrt[-1+c*x])]*
    Int[(f*x)^m*(a+b*ArcCosh[c*x])^n/(Sqrt[1+c*x]*Sqrt[-1+c*x]),x] /;
FreeQ[{a,b,c,d,e,f,m},x] && EqQ[c^2*d+e,0] && IGtQ[n,0] && (IGtQ[m,-2] || EqQ[n,1])


(* ::Code:: *)
Int[(f_.*x_)^m_*Sqrt[d1_+e1_.*x_]*Sqrt[d2_+e2_.*x_]*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*Sqrt[d1+e1*x]*Sqrt[d2+e2*x]*(a+b*ArcCosh[c*x])^n/(f*(m+2)) - 
  b*c*n/(f*(m+2))*Simp[Sqrt[d1+e1*x]/Sqrt[1+c*x]]*Simp[Sqrt[d2+e2*x]/Sqrt[-1+c*x]]*
    Int[(f*x)^(m+1)*(a+b*ArcCosh[c*x])^(n-1),x] - 
  1/(m+2)*Simp[Sqrt[d1+e1*x]/Sqrt[1+c*x]]*Simp[Sqrt[d2+e2*x]/Sqrt[-1+c*x]]*
    Int[(f*x)^m*(a+b*ArcCosh[c*x])^n/(Sqrt[1+c*x]*Sqrt[-1+c*x]),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && IGtQ[n,0] && (IGtQ[m,-2] || EqQ[n,1])


(* ::Code:: *)
Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*(d+e*x^2)^p*(a+b*ArcCosh[c*x])^n/(f*(m+1)) - 
  2*e*p/(f^2*(m+1))*Int[(f*x)^(m+2)*(d+e*x^2)^(p-1)*(a+b*ArcCosh[c*x])^n,x] - 
  b*c*n/(f*(m+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m+1)*(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d,e,f},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && GtQ[p,0] && LtQ[m,-1]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*(d1+e1*x)^p*(d2+e2*x)^p*(a+b*ArcCosh[c*x])^n/(f*(m+1)) - 
  2*e1*e2*p/(f^2*(m+1))*Int[(f*x)^(m+2)*(d1+e1*x)^(p-1)*(d2+e2*x)^(p-1)*(a+b*ArcCosh[c*x])^n,x] - 
  b*c*n/(f*(m+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m+1)*(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[n,0] && GtQ[p,0] && LtQ[m,-1]


(* ::Code:: *)
(* Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  f*(f*x)^(m-1)*(d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n/(e*(m+2*p+1)) + 
  f^2*(m-1)/(c^2*(m+2*p+1))*Int[(f*x)^(m-2)*(d+e*x^2)^p*(a+b*ArcCosh[c*x])^n,x] - 
  b*f*n/(c*(m+2*p+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m-1)*(-1+c^2*x^2)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d,e,f,p},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && EqQ[n,1] && IGtQ[p+1/2,0] && IGtQ[(m-1)/2,0] *)


(* ::Code:: *)
(* Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  f*(f*x)^(m-1)*(d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n/(2*e*(p+1)) - 
  f^2*(m-1)/(2*e*(p+1))*Int[(f*x)^(m-2)*(d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n,x] - 
  b*f*n/(2*c*(p+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m-1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d,e,f},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && EqQ[n,1] && ILtQ[p-1/2,0] && IGtQ[(m-1)/2,0] *)


(* ::Code:: *)
Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*(d+e*x^2)^p*(a+b*ArcCosh[c*x])^n/(f*(m+2*p+1)) + 
  2*d*p/(m+2*p+1)*Int[(f*x)^m*(d+e*x^2)^(p-1)*(a+b*ArcCosh[c*x])^n,x] - 
  b*c*n/(f*(m+2*p+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m+1)*(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d,e,f,m},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && GtQ[p,0] && Not[LtQ[m,-1]]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*(d1+e1*x)^p*(d2+e2*x)^p*(a+b*ArcCosh[c*x])^n/(f*(m+2*p+1)) + 
  2*d1*d2*p/(m+2*p+1)*Int[(f*x)^m*(d1+e1*x)^(p-1)*(d2+e2*x)^(p-1)*(a+b*ArcCosh[c*x])^n,x] - 
  b*c*n/(f*(m+2*p+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m+1)*(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[n,0] && GtQ[p,0] && Not[LtQ[m,-1]]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*(d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n/(d*f*(m+1)) + 
  c^2*(m+2*p+3)/(f^2*(m+1))*Int[(f*x)^(m+2)*(d+e*x^2)^p*(a+b*ArcCosh[c*x])^n,x] + 
  b*c*n/(f*(m+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m+1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d,e,f,p},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && ILtQ[m,-1]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  (f*x)^(m+1)*(d1+e1*x)^(p+1)*(d2+e2*x)^(p+1)*(a+b*ArcCosh[c*x])^n/(d1*d2*f*(m+1)) + 
  c^2*(m+2*p+3)/(f^2*(m+1))*Int[(f*x)^(m+2)*(d1+e1*x)^p*(d2+e2*x)^p*(a+b*ArcCosh[c*x])^n,x] + 
  b*c*n/(f*(m+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m+1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,p},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[n,0] && ILtQ[m,-1]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  f*(f*x)^(m-1)*(d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n/(2*e*(p+1)) - 
  f^2*(m-1)/(2*e*(p+1))*Int[(f*x)^(m-2)*(d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n,x] - 
  b*f*n/(2*c*(p+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m-1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d,e,f},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && LtQ[p,-1] && IGtQ[m,1]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  f*(f*x)^(m-1)*(d1+e1*x)^(p+1)*(d2+e2*x)^(p+1)*(a+b*ArcCosh[c*x])^n/(2*e1*e2*(p+1)) - 
  f^2*(m-1)/(2*e1*e2*(p+1))*Int[(f*x)^(m-2)*(d1+e1*x)^(p+1)*(d2+e2*x)^(p+1)*(a+b*ArcCosh[c*x])^n,x] - 
  b*f*n/(2*c*(p+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m-1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[n,0] && LtQ[p,-1] && IGtQ[m,1]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  -(f*x)^(m+1)*(d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n/(2*d*f*(p+1)) + 
  (m+2*p+3)/(2*d*(p+1))*Int[(f*x)^m*(d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n,x] - 
  b*c*n/(2*f*(p+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m+1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d,e,f,m},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && LtQ[p,-1] && Not[GtQ[m,1]] && (IntegerQ[m] || IntegerQ[p] || EqQ[n,1])


(* ::Code:: *)
Int[(f_.*x_)^m_*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  -(f*x)^(m+1)*(d1+e1*x)^(p+1)*(d2+e2*x)^(p+1)*(a+b*ArcCosh[c*x])^n/(2*d1*d2*f*(p+1)) + 
  (m+2*p+3)/(2*d1*d2*(p+1))*Int[(f*x)^m*(d1+e1*x)^(p+1)*(d2+e2*x)^(p+1)*(a+b*ArcCosh[c*x])^n,x] - 
  b*c*n/(2*f*(p+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m+1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[n,0] && LtQ[p,-1] && Not[GtQ[m,1]] && (IntegerQ[m] || EqQ[n,1])


(* ::Code:: *)
Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  f*(f*x)^(m-1)*(d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])^n/(e*(m+2*p+1)) + 
  f^2*(m-1)/(c^2*(m+2*p+1))*Int[(f*x)^(m-2)*(d+e*x^2)^p*(a+b*ArcCosh[c*x])^n,x] - 
  b*f*n/(c*(m+2*p+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m-1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d,e,f,p},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && IGtQ[m,1] && NeQ[m+2*p+1,0]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  f*(f*x)^(m-1)*(d1+e1*x)^(p+1)*(d2+e2*x)^(p+1)*(a+b*ArcCosh[c*x])^n/(e1*e2*(m+2*p+1)) + 
  f^2*(m-1)/(c^2*(m+2*p+1))*Int[(f*x)^(m-2)*(d1+e1*x)^p*(d2+e2*x)^p*(a+b*ArcCosh[c*x])^n,x] - 
  b*f*n/(c*(m+2*p+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m-1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n-1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,p},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[n,0] && IGtQ[m,1] && NeQ[m+2*p+1,0]


(* ::Code:: *)
Int[(f_.*x_)^m_.*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_,x_Symbol] :=
  (f*x)^m*Simp[Sqrt[1+c*x]*Sqrt[-1+c*x]*(d+e*x^2)^p]*(a+b*ArcCosh[c*x])^(n+1)/(b*c*(n+1)) + 
  f*m/(b*c*(n+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m-1)*(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2)*(a+b*ArcCosh[c*x])^(n+1),x] /;
FreeQ[{a,b,c,d,e,f,m,p},x] && EqQ[c^2*d+e,0] && LtQ[n,-1] && EqQ[m+2*p+1,0]


(* ::Code:: *)
Int[(f_.*x_)^m_.*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_,x_Symbol] :=
  (f*x)^m*Simp[Sqrt[1+c*x]*Sqrt[-1+c*x]*(d1+e1*x)^p]*(d2+e2*x)^p*(a+b*ArcCosh[c*x])^(n+1)/(b*c*(n+1)) + 
  f*m/(b*c*(n+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m-1)*(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2)*(a+b*ArcCosh[c*x])^(n+1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m,p},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && LtQ[n,-1] && EqQ[m+2*p+1,0]


(* ::Code:: *)
Int[(f_.*x_)^m_.*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_,x_Symbol] :=
  (f*x)^m*Simp[Sqrt[1+c*x]*Sqrt[-1+c*x]*(d+e*x^2)^p]*(a+b*ArcCosh[c*x])^(n+1)/(b*c*(n+1)) + 
  f*m/(b*c*(n+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m-1)*(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2)*(a+b*ArcCosh[c*x])^(n+1),x] - 
  c*(m+2*p+1)/(b*f*(n+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m+1)*(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2)*(a+b*ArcCosh[c*x])^(n+1),x] /;
FreeQ[{a,b,c,d,e,f,m,p},x] && EqQ[c^2*d+e,0] && LtQ[n,-1] && IGtQ[2*p,0] && NeQ[m+2*p+1,0] && IGtQ[m,-3]


(* ::Code:: *)
Int[(f_.*x_)^m_.*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_,x_Symbol] :=
  (f*x)^m*Sqrt[1+c*x]*Sqrt[-1+c*x]*(d1+e1*x)^p*(d2+e2*x)^p*(a+b*ArcCosh[c*x])^(n+1)/(b*c*(n+1)) + 
  f*m/(b*c*(n+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m-1)*(-1+c^2*x^2)^(p-1/2)*(a+b*ArcCosh[c*x])^(n+1),x] - 
  c*(m+2*p+1)/(b*f*(n+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m+1)*(-1+c^2*x^2)^(p-1/2)*(a+b*ArcCosh[c*x])^(n+1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m,p},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && LtQ[n,-1] && IGtQ[p+1/2,0] && NeQ[m+2*p+1,0] && IGtQ[m,-3]


(* ::Code:: *)
(* Int[(f_.*x_)^m_.*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_,x_Symbol] :=
  (f*x)^m*Simp[Sqrt[1+c*x]*Sqrt[-1+c*x]*(d+e*x^2)^p]*(a+b*ArcCosh[c*x])^(n+1)/(b*c*(n+1)) - 
  f*m/(b*c*(n+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m-1)*(1+c*x)^(p+1/2)*(-1+c*x)^(p+1/2)*(a+b*ArcCosh[c*x])^(n+1),x] - 
  c*(2*p+1)/(b*f*(n+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Int[(f*x)^(m+1)*(1+c*x)^(p-1/2)*(-1+c*x)^(p-1/2)*(a+b*ArcCosh[c*x])^(n+1),x] /;
FreeQ[{a,b,c,d,e,f,m,p},x] && EqQ[c^2*d+e,0] && LtQ[n,-1] && NeQ[p,-1/2] && IntegerQ[2*p] && IGtQ[m,-3] *)


(* ::Code:: *)
(* Int[(f_.*x_)^m_.*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_,x_Symbol] :=
  (f*x)^m*Sqrt[1+c*x]*Sqrt[-1+c*x]*(d1+e1*x)^p*(d2+e2*x)^p*(a+b*ArcCosh[c*x])^(n+1)/(b*c*(n+1)) - 
  f*m/(b*c*(n+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m-1)*(-1+c^2*x^2)^(p+1/2)*(a+b*ArcCosh[c*x])^(n+1),x] - 
  c*(2*p+1)/(b*f*(n+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Int[(f*x)^(m+1)*(-1+c^2*x^2)^(p-1/2)*(a+b*ArcCosh[c*x])^(n+1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m,p},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && LtQ[n,-1] && ILtQ[p+1/2,0] && IGtQ[m,-3] *)


(* ::Code:: *)
Int[(f_.*x_)^m_*(a_.+b_.*ArcCosh[c_.*x_])^n_./Sqrt[d_+e_.*x_^2],x_Symbol] :=
  f*(f*x)^(m-1)*Sqrt[d+e*x^2]*(a+b*ArcCosh[c*x])^n/(e*m) - 
  b*f*n/(c*m)*Simp[Sqrt[1+c*x]*Sqrt[-1+c*x]/Sqrt[d+e*x^2]]*Int[(f*x)^(m-1)*(a+b*ArcCosh[c*x])^(n-1),x] + 
  f^2*(m-1)/(c^2*m)*Int[(f*x)^(m-2)*(a+b*ArcCosh[c*x])^n/Sqrt[d+e*x^2],x] /;
FreeQ[{a,b,c,d,e,f},x] && EqQ[c^2*d+e,0] && GtQ[n,0] && IGtQ[m,1]


(* ::Code:: *)
Int[(f_.*x_)^m_*(a_.+b_.*ArcCosh[c_.*x_])^n_./(Sqrt[d1_+e1_.*x_]*Sqrt[d2_+e2_.*x_]),x_Symbol] :=
  f*(f*x)^(m-1)*Sqrt[d1+e1*x]*Sqrt[d2+e2*x]*(a+b*ArcCosh[c*x])^n/(e1*e2*m) - 
  b*f*n/(c*m)*Simp[Sqrt[1+c*x]/Sqrt[d1+e1*x]]*Simp[Sqrt[-1+c*x]/Sqrt[d2+e2*x]]*
    Int[(f*x)^(m-1)*(a+b*ArcCosh[c*x])^(n-1),x] + 
  f^2*(m-1)/(c^2*m)*Int[(f*x)^(m-2)*(a+b*ArcCosh[c*x])^n/(Sqrt[d1+e1*x]*Sqrt[d2+e2*x]),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[n,0] && IGtQ[m,1]


(* ::Code:: *)
Int[x_^m_*(a_.+b_.*ArcCosh[c_.*x_])^n_./Sqrt[d_+e_.*x_^2],x_Symbol] :=
  1/c^(m+1)*Simp[Sqrt[1+c*x]*Sqrt[-1+c*x]/Sqrt[d+e*x^2]]*
    Subst[Int[(a+b*x)^n*Cosh[x]^m,x],x,ArcCosh[c*x]] /;
FreeQ[{a,b,c,d,e},x] && EqQ[c^2*d+e,0] && IGtQ[n,0] && IntegerQ[m]


(* ::Code:: *)
Int[x_^m_*(a_.+b_.*ArcCosh[c_.*x_])^n_./(Sqrt[d1_+e1_.*x_]*Sqrt[d2_+e2_.*x_]),x_Symbol] :=
  1/c^(m+1)*Simp[Sqrt[1+c*x]/Sqrt[d1+e1*x]]*Simp[Sqrt[-1+c*x]/Sqrt[d2+e2*x]]*
    Subst[Int[(a+b*x)^n*Cosh[x]^m,x],x,ArcCosh[c*x]] /;
FreeQ[{a,b,c,d1,e1,d2,e2},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && IGtQ[n,0] && IntegerQ[m]


(* ::Code:: *)
Int[(f_.*x_)^m_*(a_.+b_.*ArcCosh[c_.*x_])/Sqrt[d_+e_.*x_^2],x_Symbol] :=
  (f*x)^(m+1)/(f*(m+1))*Simp[Sqrt[1-c^2*x^2]/Sqrt[d+e*x^2]]*
    (a+b*ArcCosh[c*x])*Hypergeometric2F1[1/2,(1+m)/2,(3+m)/2,c^2*x^2] + 
  b*c*(f*x)^(m+2)/(f^2*(m+1)*(m+2))*Simp[Sqrt[1+c*x]*Sqrt[-1+c*x]/Sqrt[d+e*x^2]]*
    HypergeometricPFQ[{1,1+m/2,1+m/2},{3/2+m/2,2+m/2},c^2*x^2] /;
FreeQ[{a,b,c,d,e,f,m},x] && EqQ[c^2*d+e,0] && Not[IntegerQ[m]]


(* ::Code:: *)
Int[(f_.*x_)^m_*(a_.+b_.*ArcCosh[c_.*x_])/(Sqrt[d1_+e1_.*x_]*Sqrt[d2_+e2_.*x_]),x_Symbol] :=
  (f*x)^(m+1)/(f*(m+1))*Simp[Sqrt[1-c^2*x^2]/(Sqrt[d1+e1*x]*Sqrt[d2+e2*x])]*
    (a+b*ArcCosh[c*x])*Hypergeometric2F1[1/2,(1+m)/2,(3+m)/2,c^2*x^2] + 
  b*c*(f*x)^(m+2)/(f^2*(m+1)*(m+2))*Simp[Sqrt[1+c*x]/Sqrt[d1+e1*x]]*Simp[Sqrt[-1+c*x]/Sqrt[d2+e2*x]]*
    HypergeometricPFQ[{1,1+m/2,1+m/2},{3/2+m/2,2+m/2},c^2*x^2] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && Not[IntegerQ[m]]


(* ::Code:: *)
Int[(f_.*x_)^m_.*(a_.+b_.*ArcCosh[c_.*x_])^n_/Sqrt[d_+e_.*x_^2],x_Symbol] :=
  (f*x)^m*(a+b*ArcCosh[c*x])^(n+1)/(b*c*(n+1))*Simp[Sqrt[1+c*x]*Sqrt[-1+c*x]/Sqrt[d+e*x^2]] - 
  f*m/(b*c*(n+1))*Simp[Sqrt[1+c*x]*Sqrt[-1+c*x]/Sqrt[d+e*x^2]]*Int[(f*x)^(m-1)*(a+b*ArcCosh[c*x])^(n+1),x] /;
FreeQ[{a,b,c,d,e,f,m},x] && EqQ[c^2*d+e,0] && LtQ[n,-1]


(* ::Code:: *)
Int[(f_.*x_)^m_.*(a_.+b_.*ArcCosh[c_.*x_])^n_/(Sqrt[d1_+e1_.*x_]*Sqrt[d2_+e2_.*x_]),x_Symbol] :=
  (f*x)^m*(a+b*ArcCosh[c*x])^(n+1)/(b*c*(n+1))*Simp[Sqrt[1+c*x]/Sqrt[d1+e1*x]]*Simp[Sqrt[-1+c*x]/Sqrt[d2+e2*x]] - 
  f*m/(b*c*(n+1))*Simp[Sqrt[1+c*x]/Sqrt[d1+e1*x]]*Simp[Sqrt[-1+c*x]/Sqrt[d2+e2*x]]*
    Int[(f*x)^(m-1)*(a+b*ArcCosh[c*x])^(n+1),x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && LtQ[n,-1]


(* ::Code:: *)
Int[x_^m_.*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  1/(b*c^(m+1))*Simp[(d+e*x^2)^p/((1+c*x)^p*(-1+c*x)^p)]*
    Subst[Int[x^n*Cosh[-a/b+x/b]^m*Sinh[-a/b+x/b]^(2*p+1),x],x,a+b*ArcCosh[c*x]] /;
FreeQ[{a,b,c,d,e,n},x] && EqQ[c^2*d+e,0] && IGtQ[2*p+2,0] && IGtQ[m,0]


(* ::Code:: *)
Int[x_^m_.*(d1_+e1_.*x_)^p_.*(d2_+e2_.*x_)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  1/(b*c^(m+1))*Simp[(d1+e1*x)^p/(1+c*x)^p]*Simp[(d2+e2*x)^p/(-1+c*x)^p]*
    Subst[Int[x^n*Cosh[-a/b+x/b]^m*Sinh[-a/b+x/b]^(2*p+1),x],x,a+b*ArcCosh[c*x]] /;
FreeQ[{a,b,c,d1,e1,d2,e2,n},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && IGtQ[p+3/2,0] && IGtQ[m,0]


(* ::Code:: *)
Int[(f_.*x_)^m_*(d_+e_.*x_^2)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  Int[ExpandIntegrand[(a+b*ArcCosh[c*x])^n/Sqrt[d+e*x^2],(f*x)^m*(d+e*x^2)^(p+1/2),x],x] /;
FreeQ[{a,b,c,d,e,f,m,n},x] && EqQ[c^2*d+e,0] && IGtQ[p+1/2,0] && Not[IGtQ[(m+1)/2,0]] && (EqQ[m,-1] || EqQ[m,-2])


(* ::Code:: *)
Int[(f_.*x_)^m_*(d1_+e1_.*x_)^p_*(d2_+e2_.*x_)^p_*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  Int[ExpandIntegrand[(a+b*ArcCosh[c*x])^n/(Sqrt[d1+e1*x]*Sqrt[d2+e2*x]),(f*x)^m*(d1+e1*x)^(p+1/2)*(d2+e2*x)^(p+1/2),x],x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m,n},x] && EqQ[e1,c*d1] && EqQ[e2,-c*d2] && GtQ[d1,0] && LtQ[d2,0] && IGtQ[p+1/2,0] && Not[IGtQ[(m+1)/2,0]] && 
  (EqQ[m,-1] || EqQ[m,-2])


(* ::Code:: *)
Int[(f_.*x_)^m_.*(d_+e_.*x_^2)*(a_.+b_.*ArcCosh[c_.*x_]),x_Symbol] :=
  d*(f*x)^(m+1)*(a+b*ArcCosh[c*x])/(f*(m+1)) + 
  e*(f*x)^(m+3)*(a+b*ArcCosh[c*x])/(f^3*(m+3)) - 
  b*c/(f*(m+1)*(m+3))*Int[(f*x)^(m+1)*(d*(m+3)+e*(m+1)*x^2)/(Sqrt[1+c*x]*Sqrt[-1+c*x]),x] /;
FreeQ[{a,b,c,d,e,f,m},x] && NeQ[c^2*d+e,0] && NeQ[m,-1] && NeQ[m,-3]


(* ::Code:: *)
Int[x_*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_]),x_Symbol] :=
  (d+e*x^2)^(p+1)*(a+b*ArcCosh[c*x])/(2*e*(p+1)) - b*c/(2*e*(p+1))*Int[(d+e*x^2)^(p+1)/(Sqrt[1+c*x]*Sqrt[-1+c*x]),x] /;
FreeQ[{a,b,c,d,e,p},x] && NeQ[c^2*d+e,0] && NeQ[p,-1]


(* ::Code:: *)
Int[(f_.*x_)^m_.*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_]),x_Symbol] :=
  With[{u=IntHide[(f*x)^m*(d+e*x^2)^p,x]},  
  Dist[a+b*ArcCosh[c*x],u,x] - b*c*Int[SimplifyIntegrand[u/(Sqrt[1+c*x]*Sqrt[-1+c*x]),x],x]] /;
FreeQ[{a,b,c,d,e,f,m},x] && NeQ[c^2*d+e,0] && IntegerQ[p] && (GtQ[p,0] || IGtQ[(m-1)/2,0] && LeQ[m+p,0])


(* ::Code:: *)
(* Int[x_^m_.*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_,x_Symbol] :=
  1/(b*c^(m+2*p+1))*Subst[Int[x^n*Cosh[-a/b+x/b]^m*(c^2*d+e*Cosh[-a/b+x/b]^2)^p*Sinh[-a/b+x/b],x],x,a+b*ArcCosh[c*x]] /;
FreeQ[{a,b,c,d,e,n},x] && IGtQ[m,0] && IGtQ[p,0] *)


(* ::Code:: *)
Int[(f_.*x_)^m_.*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  Int[ExpandIntegrand[(a+b*ArcCosh[c*x])^n,(f*x)^m*(d+e*x^2)^p,x],x] /;
FreeQ[{a,b,c,d,e,f},x] && NeQ[c^2*d+e,0] && IGtQ[n,0] && IntegerQ[p] && IntegerQ[m]


(* ::Code:: *)
Int[(f_.*x_)^m_.*(d_+e_.*x_^2)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  Unintegrable[(f*x)^m*(d+e*x^2)^p*(a+b*ArcCosh[c*x])^n,x] /;
FreeQ[{a,b,c,d,e,f,m,n,p},x]


(* ::Code:: *)
Int[(f_.*x_)^m_.*(d1_+e1_.*x_)^p_.*(d2_+e2_.*x_)^p_.*(a_.+b_.*ArcCosh[c_.*x_])^n_.,x_Symbol] :=
  Unintegrable[(f*x)^m*(d1+e1*x)^p*(d2+e2*x)^p*(a+b*ArcCosh[c*x])^n,x] /;
FreeQ[{a,b,c,d1,e1,d2,e2,f,m,n,p},x]



