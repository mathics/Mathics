(* ~~~> Include the tooling <~~~ *)

<<"../test_driver.m";
<<"c-small.m";
(* Section 1.1.1 *)
expect[{{1, 2, 3}, {1, 3, 2}, {2, 1, 3}, {2, 3, 1}, {3, 1, 2}, {3, 2, 1}},
       Permutations[3]]
expect[{{A, B, C}, {A, C, B}, {B, A, C}, {B, C, A}, {C, A, B}, {C, B, A}},
       Permute[{A, B, C, D}, Permutations[3]]]
expect[{1, 2, 3, 4, 5},
       Permute[{5,2,4,3,1}, InversePermutation[{5,2,4,3,1}] ]]

(* Not working
expect[{{a, b, c}, {b, a, c}, {c, a, b}, {a, c, b}, {b, c, a}, {c, b, a}},
       MinimumChangePermutations[{a,b,c}]]

expect[321953,
       RankPermutation[{8, 9, 7, 1, 6, 4, 5, 3, 2}]]
 *)

(*
expect[4, BinarySearch[{3, 4, 10, 100, 123}, 100]]
expect[2.5, BinarySearch[{2, 3, 9}, 7] // N]
expect[1.5, BinarySearch[{2, 7, 9, 10}, 3] // N]
expect[0.5, BinarySearch[{-10, 5, 8, 10}, -100] // N]
expect[4.5, BinarySearch[{-10, 5, 8, 10}, 20] // N]
expect[2,  BinarySearch[{{a, 1}, {b, 7}}, 7, #[[2]]&]]
 *)
