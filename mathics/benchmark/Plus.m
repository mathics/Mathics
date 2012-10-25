(* Addition of Integers *)
l1= Range[10000];
l2= Range[-10000, 10000];
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l2][[1]]
Timing[Plus @@ l2][[1]]
Timing[Plus @@ l2][[1]]
Timing[Plus @@ l2][[1]]
Timing[Plus @@ l2][[1]]

(* Addition of Boolean Integers *)
l1 = RandomInteger[{0,1}, 200];
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]

(* Addition of Small Integers *)
l1 = RandomInteger[{0,100}, 200];
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]

(* Addition of Large Integers *)
l1 = RandomInteger[{0,10^6}, 200];
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]

(* Addition of Small Floats *)
l1 = RandomReal{0, 1}, 200];
l2 = RandomReal{-1, 1}, 200];
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l2][[1]]
Timing[Plus @@ l2][[1]]
Timing[Plus @@ l2][[1]]
Timing[Plus @@ l2][[1]]
Timing[Plus @@ l2][[1]]

(* Addition of Large Floats *)
l1 = RandomReal[{-10^10, 10^10}, 200];
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]
Timing[Plus @@ l1][[1]]

(* Addition of Exact Complex Numbers *)

(* Addition of Inexact Complex Numbers *)

(* Symbolic Additions *)
