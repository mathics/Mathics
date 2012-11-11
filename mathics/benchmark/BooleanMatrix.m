(* Generate Boolean Matricies *)
Timing[m0 = RandomInteger[{0,1},{10, 10}]][[1]]
Timing[m1 = RandomInteger[{0,1},{10, 10}]][[1]]
Timing[m2 = RandomInteger[{0,1},{10, 10}]][[1]]
Timing[m3 = RandomInteger[{0,1},{10, 10}]][[1]]
Timing[m4 = RandomInteger[{0,1},{10, 10}]][[1]]
Timing[m5 = RandomInteger[{0,1},{10, 10}]][[1]]
Timing[m6 = RandomInteger[{0,1},{10, 10}]][[1]]
Timing[m7 = RandomInteger[{0,1},{10, 10}]][[1]]
Timing[m8 = RandomInteger[{0,1},{10, 10}]][[1]]
Timing[m9 = RandomInteger[{0,1},{10, 10}]][[1]]

(* Multiply Boolean Matricies *)
Timing[Mod[m0.m1, 2]][[1]]
Timing[Mod[m2.m3, 2]][[1]]
Timing[Mod[m4.m5, 2]][[1]]
Timing[Mod[m6.m7, 2]][[1]]
Timing[Mod[m8.m9, 2]][[1]]
Timing[Mod[m0.m1.m2.m3, 2]][[1]]
Timing[Mod[m4.m5.m8.m9, 2]][[1]]

(* Add Boolean Matricies *)
Timing[Mod[m0+m1+m2+m3+m4+m5+m6+m7+m8+m9, 2]][[1]]
Timing[Mod[m0+m2+m4+m6+m8+m0+m2+m4+m6+m8, 2]][[1]]
Timing[Mod[m1+m3+m5+m7+m9+m1+m3+m5+m7+m9, 2]][[1]]
Timing[Mod[m0+m2+m4+m6+m8+m0+m2+m4+m6+m8+m1+m3+m5+m7+m9+m1+m3+m5+m7+m9, 2]][[1]]
