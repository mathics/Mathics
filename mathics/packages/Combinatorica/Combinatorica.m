(* :Title: Combinatorica *)

(* :Authors: Sriram V. Pemmaraju and Steven S. Skiena*)

(* :Summary:
This package contains all the programs from the book, "Computational 
Discrete Mathematics: Combinatorics and Graph Theory in Mathematica",
by Sriram V. Pemmaraju and Steven S. Skiena, Cambridge University Press,
2003.
*)

(* :Discussion:
The programs from the book "Computational Discrete Mathematics: Combinatorics
and Graph Theory with Mathematica" are available at www.combinatorica.com.

Any comments or bug reports should be forwarded to one of the following:

	Sriram Pemmaraju
	Department of Computer Science
	University of Iowa
	Iowa City, IA 52242

        sriram@cs.uiowa.edu

	(319)-353-2956


	Steven Skiena
	Department of Computer Science
	State University of New York
	Stony Brook, NY 11794-4400

	skiena@cs.sunysb.edu

	(631)-632-9026

*)
(* :Context: DiscreteMath`Combinatorica`  
*)
(* :Package Version: 2.0.0
*)
(* :Copyright: Copyright 2000-2003 by Sriram V. Pemmaraju and Steven S. Skiena
*)

(*
This package may be copied in its entirety for nonprofit purposes only.
Sale, other than for the direct cost of the media, is prohibited.  This
copyright notice must accompany all copies.

The authors, Wolfram Research, and Cambridge University Press
make no representations, express or implied, with respect to this
documentation, or the software it describes and contains, including
without limitations, any implied warranties of mechantability or fitness
for a particular purpose, all of which are expressly disclaimed.  The
authors, Wolfram Research, or Cambridge University Press, their licensees,
distributors and dealers shall in no event be liable for any indirect,
incidental, or consequential damages.
*)
(* :History:
        Version 2.0 most code rewritten Sriram V. Pemmaraju, 2000-2002
                Too many changes to describe here. Read the book!
	Version 1.1 modification by ECM, March 1996.  
		Replaced K with CompleteGraph because K is now the
			default generic name for the summation index in
			symbolic sum.
		Added CombinatorialFunctions.m and Permutations.m to
			BeginPackage, and commented out CatalanNumber,
			PermutationQ, ToCycles, FromCycles, and
			RandomPermutation so there would be no shadowing of
			symbols among the DiscreteMath packages.	
		Replaced old BinarySearch with new code by Paul Abbott
			correctly implementing binary search.
        Version 1.0 by Steven S. Skiena, April 1995.
        Version .9 by Steven S. Skiena, February 1992.
	Version .8 by Steven S. Skiena, July 1991.
	Version .7 by Steven S. Skiena, January 1991. 
	Version .6 by Steven S. Skiena, June 1990.
*)
(*
Acknowledgements: 
        WRI people who helped: John Novak, Eric Weisstein, Arnoud Buzing,
        Shiral Devmal, Anna Pakin, Andy Shiekh, Darren Glosemeyer, Ranjani
        Krishnan, Daniel Lichtblau, 
        Robby Villegas, Stephen Wolfram

	Others who helped: Eugene Curtin, Levon LLoyd, Joan Trias, Kaushal 
        Kurapati, students at Iowa, students at IIT Bombay
*)
(* :Keywords:
	adjacency, automorphism, chromatic, clique, coloring,
	combination, composition, connected components, connectivity, cycle,
	de Bruijn, degree, derangement, Dijkstra, Durfee,
	embedding, equivalence, Eulerian, Ferrers,
	geodesic, graph, Gray code, group, Hamiltonian cycle, Harary, Hasse,
	heap, hypercube, interval, inversion, involution, isomorphism,
	Josephus, network,
	partition, perfect, permutation, planar graph, pseudograph,
	self-loop, sequence, signature, simple, spanning tree,
	stable marriage, star, Stirling,
	transitive closure, traveling salesman tour, tree, Turan,
	vertex cover, wheel, Young tableau
*)
(* :Source:
	Sriram V. Pemmaraju and Steven S. Skiena
        Computational Discrete Mathematics: Combinatorics
	and Graph Theory in Mathematica, 
*)
(* :Mathematica Version: 4.0
*)

BeginPackage["DiscreteMath`Combinatorica`", 
		"DiscreteMath`CombinatorialFunctions`",
 (* needed for CatalanNumber *)
                "Graphics`Colors`",
 (* needed for the enhanced graphics routines such as ShowGraph	 *) 
                "Graphics`Arrow`",
 (* needed for drawing directed edges *) 
                "Statistics`DiscreteDistributions`"
 (* needed for generating random graph*) 
]

Unprotect[
AcyclicQ, 
AddEdge,
AddEdges, 
AddVertex,
AddVertices,
Algorithm,
AllPairsShortestPath, 
AlternatingGroup,
AlternatingGroupIndex,
AlternatingPaths,
AnimateGraph,
AntiSymmetricQ,
Approximate,
ApproximateVertexCover,
ArticulationVertices,
Automorphisms,
Backtrack, 
BellB,
BellmanFord,
BiconnectedComponents, 
BiconnectedQ,
BinarySearch, 
BinarySubsets, 
BipartiteMatching, 
BipartiteMatchingAndCover, 
BipartiteQ,
BooleanAlgebra,
Box, 
BreadthFirstTraversal, 
Brelaz,
BrelazColoring,
Bridges, 
ButterflyGraph,
ToCanonicalSetPartition,
CageGraph,
CartesianProduct, 
Center, 
ChangeEdges, 
ChangeVertices,
ChromaticNumber, 
ChromaticPolynomial,
ChvatalGraph,
CirculantGraph, 
CircularEmbedding, 
CircularVertices, 
CliqueQ, 
CoarserSetPartitionQ,
CodeToLabeledTree, 
Cofactor,
CompleteBinaryTree,
CompleteKaryTree,
CompleteKPartiteGraph,
CompleteGraph,
CompleteQ,
Compositions, 
ConnectedComponents, 
ConnectedQ, 
ConstructTableau,
Contract, 
CostOfPath,
CoxeterGraph,
CubeConnectedCycle,
CubicalGraph,
Cut,
Cycle, 
Cycles, 
CycleIndex,
CycleStructure,
Cyclic,
CyclicGroup,
CyclicGroupIndex,
DeBruijnGraph, 
DeBruijnSequence, 
Degrees,
DegreesOf2Neighborhood,
DegreeSequence,
DeleteCycle, 
DeleteEdge, 
DeleteEdges, 
DeleteFromTableau, 
DeleteVertex,
DeleteVertices, 
DepthFirstTraversal,
DerangementQ, 
Derangements, 
Diameter, 
Dihedral,
DihedralGroup,
DihedralGroupIndex,
Dijkstra, 
DilateVertices,
Directed, 
Distances,
DistinctPermutations, 
Distribution, 
DodecahedralGraph,
DominatingIntegerPartitionQ,
DominationLattice,
DurfeeSquare, 
Eccentricity,
Edge,
EdgeChromaticNumber, 
EdgeColor,
EdgeColoring, 
EdgeConnectivity, 
EdgeDirection, 
EdgeLabel, 
EdgeLabelColor, 
EdgeLabelPosition, 
Edges, 
EdgeStyle, 
EdgeWeight, 
Element,
EmptyGraph, 
EmptyQ, 
EncroachingListSet, 
EquivalenceClasses, 
EquivalenceRelationQ, 
Equivalences, 
Euclidean,
Eulerian,
EulerianCycle,
EulerianQ, 
ExactRandomGraph, 
ExpandGraph, 
ExtractCycles, 
FerrersDiagram, 
FindCycle,
FindSet, 
FiniteGraphs,
FirstLexicographicTableau, 
FolkmanGraph,
FranklinGraph,
FruchtGraph,
FromAdjacencyLists,
FromAdjacencyMatrix,
FromCycles,
FromInversionVector, 
FromOrderedPairs,
FromUnorderedPairs, 
FunctionalGraph,
GeneralizedPetersenGraph,
GetEdgeLabels,
GetEdgeWeights,
GetVertexLabels,
GetVertexWeights,
Girth, 
GraphCenter, 
GraphComplement, 
GraphDifference, 
GraphicQ,
GraphIntersection,
GraphJoin, 
GraphOptions, 
GraphPolynomial,
GraphPower, 
GraphProduct, 
GraphSum, 
GraphUnion, 
GrayCode, 
GrayCodeSubsets, 
GrayCodeKSubsets, 
GrayGraph,
Greedy,
GreedyVertexCover,
GridGraph, 
GrotztschGraph,
HamiltonianCycle, 
HamiltonianPath, 
HamiltonianQ, 
Harary,
HasseDiagram, 
Heapify, 
HeapSort, 
HeawoodGraph,
HerschelGraph,
HideCycles, 
HighlightedEdgeColors,
HighlightedEdgeStyle,
HighlightedVertexColors,
HighlightedVertexStyle,
Highlight,
Hypercube, 
IcosahedralGraph,
IdenticalQ,
IdentityPermutation,
IncidenceMatrix,
InDegree,
IndependentSetQ, 
Index, 
InduceSubgraph,
InitializeUnionFind,
InsertIntoTableau, 
IntervalGraph, 
Invariants,
InversePermutation, 
InversionPoset,
Inversions,
InvolutionQ, 
Involutions, 
IsomorphicQ, 
Isomorphism, 
IsomorphismQ, 
Josephus, 
KnightsTourGraph,
KSetPartitions,
KSubsetGroup,
KSubsetGroupIndex,
KSubsets,
LNorm,
LabeledTreeToCode, 
Large, 
LastLexicographicTableau,
LexicographicPermutations, 
LexicographicSubsets, 
LeviGraph,
LineGraph,
ListGraphs,
ListNecklaces,
LongestIncreasingSubsequence, 
LoopPosition,
LowerLeft, 
LowerRight, 
M,
MakeDirected,
MakeGraph, 
MakeSimple, 
MakeUndirected,
MaximalMatching,
MaximumAntichain, 
MaximumClique, 
MaximumIndependentSet,
MaximumSpanningTree, 
McGeeGraph,
MeredithGraph,
MinimumChainPartition, 
MinimumChangePermutations,
MinimumSpanningTree, 
MinimumVertexColoring, 
MinimumVertexCover, 
MultipleEdgesQ,
MultiplicationTable,
MycielskiGraph,
NecklacePolynomial,
Neighborhood,
NetworkFlow, 
NetworkFlowEdges, 
NextBinarySubset, 
NextComposition, 
NextGrayCodeSubset,
NextKSubset,
NextLexicographicSubset,
NextPartition, 
NextPermutation, 
NextSubset, 
NextTableau, 
NoMultipleEdges, 
NonLineGraphs,
NoPerfectMatchingGraph,
Normal, 
NormalDashed, 
NormalizeVertices,
NoSelfLoops, 
NthPair,
NthPermutation, 
NthSubset, 
NumberOfCompositions,
NumberOfDerangements, 
NumberOfDirectedGraphs, 
NumberOfGraphs,
NumberOfInvolutions, 
NumberOf2Paths,
NumberOfKPaths,
NumberOfNecklaces,
NumberOfPartitions,
NumberOfPermutationsByCycles, 
NumberOfPermutationsByInversions, 
NumberOfPermutationsByType,
NumberOfSpanningTrees, 
NumberOfTableaux,
OctahedralGraph,
OddGraph,
One,
Optimum,
OrbitInventory,
OrbitRepresentatives,
Orbits,
Ordered,
OrientGraph, 
OutDegree,
PairGroup,
PairGroupIndex,
Parent,
ParentsToPaths,
PartialOrderQ, 
PartitionLattice,
PartitionQ, 
Partitions, 
Path, 
PerfectQ,
PermutationGraph, 
PermutationGroupQ, 
PermutationQ,
PermutationToTableaux,
PermutationType,
PermutationWithCycle,
Permute, 
PermuteSubgraph, 
PetersenGraph,
PlanarQ,
PlotRange, 
Polya,
PseudographQ, 
RadialEmbedding, 
Radius,
RandomComposition, 
RandomGraph, 
RandomHeap, 
RandomInteger,
RandomKSetPartition,
RandomKSubset,
RandomPartition, 
RandomPermutation,
RandomRGF,
RandomSetPartition,
RandomSubset, 
RandomTableau, 
RandomTree, 
RandomVertices, 
RankBinarySubset, 
RankedEmbedding, 
RankGraph,
RankGrayCodeSubset, 
RankKSetPartition, 
RankKSubset, 
RankPermutation, 
RankRGF,
RankSetPartition,
RankSubset, 
ReadGraph,
RealizeDegreeSequence, 
ReflexiveQ,
RegularGraph, 
RegularQ, 
RemoveMultipleEdges, 
RemoveSelfLoops, 
ResidualFlowGraph,
RevealCycles, 
ReverseEdges,
RGFQ,
RGFs,
RGFToSetPartition,
RobertsonGraph,
RootedEmbedding, 
RotateVertices, 
Runs, 
SamenessRelation,
SelectionSort, 
SelfComplementaryQ, 
SelfLoopsQ,
SetEdgeWeights,
SetGraphOptions, 
SetPartitions,
SetPartitionListViaRGF,
SetPartitionQ,
SetPartitionToRGF,
SetEdgeLabels,
SetVertexLabels,
SetVertexWeights,
ShakeGraph, 
ShortestPath, 
ShortestPathSpanningTree,
ShowLabeledGraph,
ShowGraph, 
ShowGraphArray, 
ShuffleExchangeGraph,
SignaturePermutation,
Simple, 
SimpleQ,
Small, 
SmallestCyclicGroupGraph,
Spectrum, 
SpringEmbedding, 
StableMarriage, 
Star,
StirlingFirst, 
StirlingSecond,
Strings, 
Strong,
StronglyConnectedComponents,
Subsets, 
SymmetricGroup,
SymmetricGroupIndex,
SymmetricQ,
TableauClasses, 
TableauQ, 
Tableaux,
TableauxToPermutation, 
TetrahedralGraph,
Thick, 
ThickDashed, 
Thin, 
ThinDashed, 
ThomassenGraph,
ToAdjacencyLists,
ToAdjacencyMatrix, 
ToCycles,
ToInversionVector, 
ToOrderedPairs,
TopologicalSort, 
ToUnorderedPairs, 
TransitiveClosure, 
TransitiveQ,
TransitiveReduction, 
TranslateVertices, 
TransposePartition, 
TransposeTableau,
TravelingSalesmanBounds, 
TravelingSalesman, 
TreeIsomorphismQ,
TreeQ, 
TreeToCertificate,
TriangleInequalityQ,
Turan, 
TutteGraph,
TwoColoring, 
Type,
Undirected,
UndirectedQ, 
UnionSet, 
Uniquely3ColorableGraph,
UnitransitiveGraph,
UnrankBinarySubset,
UnrankGrayCodeSubset,
UnrankKSetPartition,
UnrankKSubset,
UnrankPermutation,
UnrankRGF,
UnrankSetPartition,
UnrankSubset,
UnweightedQ,
UpperLeft, 
UpperRight, 
V, 
VertexColor, 
VertexColoring, 
VertexConnectivity, 
VertexConnectivityGraph, 
VertexCover,
VertexCoverQ, 
VertexLabel, 
VertexLabelColor, 
VertexNumber, 
VertexNumberColor,
VertexStyle, 
VertexWeight, 
Vertices,
WaltherGraph,
Weak, 
WeaklyConnectedComponents, 
WeightingFunction,
WeightRange,
Wheel, 
WriteGraph,
Zoom
]

AcyclicQ::usage = "AcyclicQ[g] yields True if graph g is acyclic."

AddEdge::usage = "AddEdge[g, e] returns a graph g with the new edge e added. e can have the form {a, b} or the form {{a, b}, options}."

AddEdges::usage = "AddEdges[g, edgeList] gives graph g with the new edges in edgeList added. edgeList can have the form {a, b} to add a single edge {a, b} or the form {{a, b}, {c, d}, ...}, to add edges {a, b}, {c, d}, ... or the form { {{a, b}, x}, {{c, d}, y}, ...}, where x and y can specify graphics information associated with {a, b} and {c, d}, respectively."

AddVertex::usage = "AddVertex[g] adds one disconnected vertex to graph g. AddVertex[g, v] adds to g a vertex with coordinates specified by v."

AddVertices::usage = "AddVertices[g, n] adds n disconnected vertices to graph g. AddVertices[g, vList] adds vertices in vList to g. vList contains embedding and graphics information and can have the form {x, y} or {{x1, y1}, {x2, y2}...} or the form {{{x1, y1}, g1}, {{x2, y2}, g2},...}, where {x, y}, {x1, y1}, and {x2, y2} are point coordinates and g1 and g2 are graphics information associated with vertices."

Algorithm::usage = "Algorithm is an option that informs functions such as ShortestPath, VertexColoring, and VertexCover about which algorithm to use."

AllPairsShortestPath::usage = "AllPairsShortestPath[g] gives a matrix, where the (i, j)th entry is the length of a shortest path in g between vertices i and j. AllPairsShortestPath[g, Parent] returns a three-dimensional matrix with dimensions 2 * V[g] * V[g], in which the (1, i, j)th entry is the length of a shortest path from i to j and the (2, i, j)th entry is the predecessor of j in a shortest path from i to j."

$NewMessage[ All, "usage"] (* reset the usage of All to the System usage *)
If[StringQ[All::usage], All::usage = StringJoin[ All::usage, " All is also an option to certain Combinatorica functions specifying that all solutions should be returned, instead of just the first one."]]

AlternatingGroup::usage = "AlternatingGroup[n] generates the set of even-size n permutations, the alternating group on n symbols. AlternatingGroup[l] generates the set of even permutations of the list l."

AlternatingGroupIndex::usage = "AlternatingGroupIndex[n, x] gives the cycle index of the alternating group of size n permutations as a polynomial in the symbols x[1], x[2], ..., x[n]."


AlternatingPaths::usage = "AlternatingPaths[g, start, ME] returns the alternating paths in graph g with respect to the matching ME, starting at the vertices in the list start. The paths are returned in the form of a forest containing trees rooted at vertices in start."

AnimateGraph::usage = "AnimateGraph[g, l] displays graph g with each element in the list l successively highlighted. Here l is a list containing vertices and edges of g. An optional flag, which takes on the values All and One, can be used to inform the function about whether objects highlighted earlier will continue to be highlighted or not. The default value of flag is All. All the options allowed by the function Highlight are permitted by AnimateGraph, as well. See the usage message of Highlight for more details."

AntiSymmetricQ::usage = "AntiSymmetricQ[g] yields True if the adjacency matrix of g represents an anti-symmetric binary relation."

Approximate::usage = "Approximate is a value that the option Algorithm can take in calls to functions such as VertexCover, telling it to use an approximation algorithm."

ApproximateVertexCover::usage = "ApproximateVertexCover[g] produces a vertex cover of graph g whose size is guaranteed to be within twice the optimal size."

ArticulationVertices::usage = "ArticulationVertices[g] gives a list of all articulation vertices in graph g. These are vertices whose removal will disconnect the graph."

Automorphisms::usage = "Automorphisms[g] gives the automorphism group of the graph g."

Backtrack::usage = "Backtrack[s, partialQ, solutionQ] performs a backtrack search of the state space s, expanding a partial solution so long as partialQ is True and returning the first complete solution, as identified by solutionQ."

BellB::usage = "BellB[n] returns the nth Bell number."

BellmanFord::usage = "BellmanFord[g, v] gives a shortest-path spanning tree and associated distances from vertex v of graph g. The shortest-path spanning tree is given by a list in which element i is the predecessor of vertex i in the shortest-path spanning tree. BellmanFord works correctly even when the edge weights are negative, provided there are no negative cycles."

BiconnectedComponents::usage = "BiconnectedComponents[g] gives a list of the biconnected components of graph g. If g is directed, the underlying undirected graph is used."

BiconnectedQ::usage = "BiconnectedQ[g] yields True if graph g is biconnected. If g is directed, the underlying undirected graph is used."

BinarySearch::usage = "BinarySearch[l, k] searches sorted list l for key k and gives the position of l containing k, if k is present in l. Otherwise, if k is absent in l, the function returns (p + 1/2) where k falls between the elements of l in positions p and p+1. BinarySearch[l, k, f] gives the position of k in the list obtained from l by applying f to each element in l."

BinarySubsets::usage = "BinarySubsets[l] gives all subsets of l ordered according to the binary string defining each subset. For any positive integer n, BinarySubsets[n] gives all subsets of {1, 2,.., n} ordered according to the binary string defining each subset."

BipartiteMatching::usage = "BipartiteMatching[g] gives the list of edges associated with a maximum matching in bipartite graph g. If the graph is edge weighted, then the function returns a matching with maximum total weight."

BipartiteMatchingAndCover::usage = "BipartiteMatchingAndCover[g] takes a bipartite graph g and returns a matching with maximum weight along with the dual vertex cover. If the graph is not weighted, it is assumed that all edge weights are 1."

BipartiteQ::usage = "BipartiteQ[g] yields True if graph g is bipartite."

BooleanAlgebra::usage = "BooleanAlgebra[n] gives a Hasse diagram for the Boolean algebra on n elements. The function takes two options: Type and VertexLabel, with default values Undirected and False, respectively. When Type is set to Directed, the function produces the underlying directed acyclic graph. When VertexLabel is set to True, labels are produced for the vertices."

Box::usage = "Box is a value that the option VertexStyle, used in ShowGraph, can be set to."

BreadthFirstTraversal::usage = "BreadthFirstTraversal[g, v] performs a breadth-first traversal of graph g starting from vertex v, and gives the breadth-first numbers of the vertices. BreadthFirstTraversal[g, v, Edge] returns the edges of the graph that are traversed by breadth-first traversal. BreadthFirstTraversal[g, v, Tree] returns the breadth-first search tree. BreadthFirstTraversal[g, v, Level] returns the level number of the vertices."

Brelaz::usage = "Brelaz is a value that the option Algorithm can take when used in the function VertexColoring."

BrelazColoring::usage = "BrelazColoring[g] returns a vertex coloring in which vertices are greedily colored with the smallest available color in decreasing order of vertex degree."

Bridges::usage = "Bridges[g] gives a list of the bridges of graph g, where each bridge is an edge whose removal disconnects the graph."

ButterflyGraph::usage = "ButterflyGraph[n] returns the n-dimensional butterfly graph, a directed graph whose vertices are pairs (w, i), where w is a binary string of length n and i is an integer in the range 0 through n and whose edges go from vertex (w, i) to (w', i+1), if w' is identical to w in all bits with the possible exception of the (i+1)th bit. Here bits are counted left to right. An option VertexLabel, with default setting False, is allowed. When this option is set to True, vertices are labeled with strings (w, i)."

CageGraph::usage = "CageGraph[k, r] gives a smallest k-regular graph of girth r for certain small values of k and r. CageGraph[r] gives CageGraph[3, r]. For k = 3, r can be 3, 4, 5, 6, 7, 8, or 10. For k = 4 or 5, r can be 3, 4, 5, or 6."

CartesianProduct::usage = "CartesianProduct[l1, l2] gives the Cartesian product of lists l1 and l2."

Center::usage = "Center is a value that options VertexNumberPosition, VertexLabelPosition, and EdgeLabelPosition can take on in ShowGraph."

ChangeEdges::usage = "ChangeEdges[g, e] replaces the edges of graph g with the edges in e. e can have the form {{s1, t1}, {s2, t2}, ...} or the form { {{s1, t1}, gr1}, {{s2, t2}, gr2}, ...}, where {s1, t1}, {s2, t2}, ... are endpoints of edges and gr1, gr2, ... are graphics information associated with edges."

ChangeVertices::usage = "ChangeVertices[g, v] replaces the vertices of graph g with the vertices in the given list v. v can have the form {{x1, y1}, {x2, y2}, ...} or the form {{{x1, y1}, gr1}, {{x2, y2}, gr2}, ...}, where {x1, y1}, {x2, y2}, ... are coordinates of points and gr1, gr2, ... are graphics information associated with vertices."

ChromaticNumber::usage = "ChromaticNumber[g] gives the chromatic number of the graph, which is the fewest number of colors necessary to color the graph."

ChromaticPolynomial::usage = "ChromaticPolynomial[g, z] gives the chromatic polynomial P(z) of graph g, which counts the number of ways to color g with, at most, z colors."

ChvatalGraph::usage = "ChvatalGraph returns a smallest triangle-free, 4-regular, 4-chromatic graph."

CirculantGraph::usage = "CirculantGraph[n, l] constructs a circulant graph on n vertices, meaning the ith vertex is adjacent to the (i+j)th and (i-j)th vertices, for each j in list l. CirculantGraph[n, l], where l is an integer, returns the graph with n vertices in which each i is adjacent to (i+l) and (i-l)."

CircularEmbedding::usage = "CircularEmbedding[n] constructs a list of n points equally spaced on a circle. CircularEmbedding[g] embeds the vertices of g equally spaced on a circle."

CircularVertices::usage = "CircularVertices[n] constructs a list of n points equally spaced on a circle. CircularVertices[g] embeds the vertices of g equally spaced on a circle. This function is obsolete; use CircularEmbedding instead."

CliqueQ::usage = "CliqueQ[g, c] yields True if the list of vertices c defines a clique in graph g."

CoarserSetPartitionQ::usage = "CoarserSetPartitionQ[a, b] yields True if set partition b is coarser than set partition a, that is, every block in a is contained in some block in b."

CodeToLabeledTree::usage = "CodeToLabeledTree[l] constructs the unique labeled tree on n vertices from the Prufer code l, which consists of a list of n-2 integers between 1 and n."

Cofactor::usage = "Cofactor[m, {i, j}] calculates the (i, j)th cofactor of matrix m."

CompleteBinaryTree::usage = "CompleteBinaryTree[n] returns a complete binary tree on n vertices."

CompleteGraph::usage = "CompleteGraph[n] creates a complete graph on n vertices. An option Type that takes on the values Directed or Undirected is allowed. The default setting for this option is Type -> Undirected. CompleteGraph[a, b, c,...] creates a complete k-partite graph of the prescribed shape. The use of CompleteGraph to create a complete k-partite graph is obsolete; use CompleteKPartiteGraph instead."

CompleteKaryTree::usage = "CompleteKaryTree[n, k] returns a complete k-ary tree on n vertices."

CompleteKPartiteGraph::usage = "CompleteKPartiteGraph[a, b, c, ...] creates a complete k-partite graph of the prescribed shape, provided the k arguments a, b, c, ... are positive integers. An option Type that takes on the values Directed or Undirected is allowed. The default setting for this option is Type -> Undirected."

CompleteQ::usage = "CompleteQ[g] yields True if graph g is complete. This means that between any pair of vertices there is an undirected edge or two directed edges going in opposite directions."

Compositions::usage = "Compositions[n, k] gives a list of all compositions of integer n into k parts."

ConnectedComponents::usage = "ConnectedComponents[g] gives the vertices of graph g partitioned into connected components."

ConnectedQ::usage = "ConnectedQ[g] yields True if undirected graph g is connected. If g is directed, the function returns True if the underlying undirected graph is connected. ConnectedQ[g, Strong] and ConnectedQ[g, Weak] yield True if the directed graph g is strongly or weakly connected, respectively."

ConstructTableau::usage = "ConstructTableau[p] performs the bumping algorithm repeatedly on each element of permutation p, resulting in a distinct Young tableau."

Contract::usage = "Contract[g, {x, y}] gives the graph resulting from contracting the pair of vertices {x, y} of graph g."

CostOfPath::usage = "CostOfPath[g, p] sums up the weights of the edges in graph g defined by the path p."

CoxeterGraph::usage = "CoxeterGraph gives a non-Hamiltonian graph with a high degree of symmetry such that there is a graph automorphism taking any path of length 3 to any other."

CubeConnectedCycle::usage = "CubeConnectedCycle[d] returns the graph obtained by replacing each vertex in a d-dimensional hypercube by a cycle of length d. Cube-connected cycles share many properties with hypercubes but have the additional desirable property that for d > 1 every vertex has degree 3."

CubicalGraph::usage = "CubicalGraph returns the graph corresponding to the cube, a Platonic solid."

Cut::usage = "Cut is a tag that can be used in a call to NetworkFlow to tell it to return the minimum cut."

CycleIndex::usage = "CycleIndex[pg, x] returns the polynomial in x[1], x[2], ..., x[index[g]] that is the cycle index of the permutation group pg. Here index[pg] refers to the length of each permutation in pg."

Cycle::usage = "Cycle[n] constructs the cycle on n vertices, the 2-regular connected graph. An option Type that takes on values Directed or Undirected is allowed. The default setting is Type -> Undirected."

Cycles::usage = "Cycles is an optional argument for the function Involutions."

CycleStructure::usage = "CycleStructure[p, x] returns the monomial in x[1], x[2], ..., x[Length[p]] that is the cycle structure of the permutation p."

Cyclic::usage = "Cyclic is an argument to the Polya-theoretic functions ListNecklaces, NumberOfNecklace, and NecklacePolynomial, which count or enumerate distinct necklaces. Cyclic refers to the cyclic group acting on necklaces to make equivalent necklaces that can be obtained from each other by rotation."

CyclicGroup::usage = "CyclicGroup[n] returns the cyclic group of permutations on n symbols."

CyclicGroupIndex::usage = "CyclicGroupIndex[n, x] returns the cycle index of the cyclic group on n symbols, expressed as a polynomial in x[1], x[2], ..., x[n]."

DeBruijnGraph::usage = "DeBruijnGraph[m, n] constructs the n-dimensional De Bruijn graph with m symbols for integers m > 0 and n > 1. DeBruijnGraph[alph, n] constructs the n-dimensional De Bruijn graph with symbols from alph. Here alph is nonempty and n > 1 is an integer. In the latter form, the function accepts an option VertexLabel, with default value False, which can be set to True, if users want to associate strings on alph to the vertices as labels."

DeBruijnSequence::usage = "DeBruijnSequence[a, n] returns a De Bruijn sequence on the alphabet a, a shortest sequence in which every string of length n on alphabet a occurs as a contiguous subsequence."

DegreeSequence::usage = "DegreeSequence[g] gives the sorted degree sequence of graph g."

Degrees::usage = "Degrees[g] returns the degrees of vertex 1, 2, 3, ... in that order."

DegreesOf2Neighborhood::usage = "DegreesOf2Neighborhood[g, v] returns the sorted list of degrees of vertices of graph g within a distance of 2 from v."

DeleteCycle::usage = "DeleteCycle[g, c] deletes a simple cycle c from graph g. c is specified as a sequence of vertices in which the first and last vertices are identical. g can be directed or undirected. If g does not contain c, it is returned unchanged; otherwise g is returned with c deleted." 

DeleteEdge::usage = "DeleteEdge[g, e] gives graph g minus e. If g is undirected, then e is treated as an undirected edge, otherwise it is treated as a directed edge. If there are multiple edges between the specified vertices, only one edge is deleted. DeleteEdge[g, e, All] will delete all edges between the specified pair of vertices. Using the tag Directed as a third argument in DeleteEdge is now obsolete."

DeleteEdges::usage = "DeleteEdges[g, edgeList] gives graph g minus the list of edges edgeList. If g is undirected, then the edges in edgeList are treated as undirected edges, or otherwise they are treated as directed edges. If there are multiple edges that qualify, then only one edge is deleted. DeleteEdges[g, edgeList, All] will delete all edges that qualify. If only one edge is to be deleted, then edgeList can have the form {s, t}, or otherwise it has the form {{s1, t1}, {s2, t2}, ...}."

DeleteFromTableau::usage = "DeleteFromTableau[t, r] deletes the last element of row r from Young tableaux t."

DeleteVertex::usage = "DeleteVertex[g, v] deletes a single vertex v from graph g. Here v is a vertex number."

DeleteVertices::usage = "DeleteVertices[g, vList] deletes vertices in vList from graph g. vList has the form {i, j, ...}, where i, j, ... are vertex numbers."

DepthFirstTraversal::usage = "DepthFirstTraversal[g, v] performs a depth-first traversal of graph g starting from vertex v, and gives a list of vertices in the order in which they were encountered. DepthFirstTraversal[g, v, Edge] returns the edges of the graph that are traversed by the depth-first traversal in the order in which they are traversed. DepthFirstTraversal[g, v, Tree] returns the depth-first tree of the graph."

DerangementQ::usage = "DerangementQ[p] tests whether permutation p is a derangement, that is, a permutation without a fixed point."

Derangements::usage = "Derangements[p] constructs all derangements of permutation p."

Diameter::usage = "Diameter[g] gives the diameter of graph g, the maximum length, among all pairs of vertices in g, of a shortest path between each pair."

Dihedral::usage = "Dihedral is an argument to the Polya-theoretic functions ListNecklaces, NumberOfNecklace, and NecklacePolynomial, which count or enumerate distinct necklaces. Dihedral refers to the dihedral group acting on necklaces to make equivalent necklaces that can be obtained from each other by a rotation or a flip."

DihedralGroup::usage = "DihedralGroup[n] returns the dihedral group on n symbols. Note that the order of this group is 2n."

DihedralGroupIndex::usage = "DihedralGroupIndex[n, x] returns the cycle index of the dihedral group on n symbols, expressed as a polynomial in x[1], x[2], ..., x[n]."

Dijkstra::usage = "Dijkstra[g, v] gives a shortest-path spanning tree and associated distances from vertex v of graph g. The shortest-path spanning tree is given by a list in which element i is the predecessor of vertex i in the shortest-path spanning tree. Dijkstra does not work correctly when the edge weights are negative; BellmanFord should be used in this case."

DilateVertices::usage = "DilateVertices[v, d] multiplies each coordinate of each vertex position in list v by d, thus dilating the embedding. DilateVertices[g, d] dilates the embedding of graph g by the factor d."

Directed::usage = "Directed is an option value for Type."

$NewMessage[Disk, "usage"] (* reset the usage of Disk to the system usage *)
If[StringQ[Disk::usage], Disk::usage = StringJoin[Disk::usage, " Disk is also a value taken by the VertexStyle option in ShowGraph."]]

Distances::usage = "Distances[g, v] returns the distances in nondecreasing order from vertex v to all vertices in g, treating g as an unweighted graph."

DistinctPermutations::usage = "DistinctPermutations[l] gives all permutations of the multiset described by list l."

Distribution::usage = "Distribution[l, set] lists the frequency of each element of set in list l."

DodecahedralGraph::usage = "DodecahedralGraph returns the graph corresponding to the dodecahedron, a Platonic solid."

DominatingIntegerPartitionQ::usage = "DominatingIntegerPartitionQ[a, b] yields True if integer partition a dominates integer partition b, that is, the sum of a size-t prefix of a is no smaller than the sum of a size-t prefix of b for every t."

DominationLattice::usage = "DominationLattice[n] returns a Hasse diagram of the partially ordered set on integer partitions of n in which p < q if q dominates p. The function takes two options: Type and VertexLabel, with default values Undirected and False, respectively. When Type is set to Directed, the function produces the underlying directed acyclic graph. When VertexLabel is set to True, labels are produced for the vertices."

DurfeeSquare::usage = "DurfeeSquare[p] gives the number of rows involved in the Durfee square of partition p, the side of the largest-sized square contained within the Ferrers diagram of p."

Eccentricity::usage = "Eccentricity[g] gives the eccentricity of each vertex v of graph g, the maximum length among all shortest paths from v."

Edge::usage = "Edge is an optional argument to inform certain functions to work with edges instead of vertices."

EdgeChromaticNumber::usage = "EdgeChromaticNumber[g] gives the fewest number of colors necessary to color each edge of graph g, so that no two edges incident on the same vertex have the same color."

EdgeColor::usage = "EdgeColor is an option that allows the user to associate colors with edges. Black is the default color. EdgeColor can be set as part of the graph data structure or in ShowGraph."

EdgeColoring::usage = "EdgeColoring[g] uses Brelaz's heuristic to find a good, but not necessarily minimal, edge coloring of graph g."

EdgeConnectivity::usage = "EdgeConnectivity[g] gives the minimum number of edges whose deletion from graph g disconnects it. EdgeConnectivity[g, Cut] gives a set of edges of minimum size whose deletion disconnects the graph."

EdgeDirection::usage = "EdgeDirection is an option that takes on values True or False allowing the user to specify whether the graph is directed or not. EdgeDirection can be set as part of the graph data structure or in ShowGraph."

EdgeLabel::usage = "EdgeLabel is an option that can take on values True or False, allowing the user to associate labels to edges. By default, there are no edge labels. The EdgeLabel option can be set as part of the graph data structure or in ShowGraph."

EdgeLabelColor::usage = "EdgeLabelColor is an option that allows the user to associate different colors to edge labels. Black is the default color. EdgeLabelColor can be set as part of the graph data structure or in ShowGraph."

EdgeLabelPosition::usage = "EdgeLabelPosition is an option that allows the user to place an edge label in a certain position relative to the midpoint of the edge. LowerLeft is the default value of this option. EdgeLabelPosition can be set as part of the graph data structure or in ShowGraph."

Edges::usage = "Edges[g] gives the list of edges in g. Edges[g, All] gives the edges of g along with the graphics options associated with each edge. Edges[g, EdgeWeight] returns the list of edges in g along with their edge weights."

EdgeStyle::usage = "EdgeStyle is an option that allows the user to associate different sizes and shapes to edges. A line segment is the default edge. EdgeStyle can be set as part of the graph data structure or in ShowGraph."

EdgeWeight::usage = "EdgeWeight is an option that allows the user to associate weights with edges. 1 is the default weight. EdgeWeight can be set as part of the graph data structure."

$NewMessage[ Element, "usage"] (* reset the usage of Element to the System usage *)
If[StringQ[Element::usage], Element::usage = StringJoin[ Element::usage, " The use of the function Element in Combinatorica is now obsolete, though the function call Element[a, p] still gives the pth element of nested list a, where p is a list of indices."]]

EmptyGraph::usage = "EmptyGraph[n] generates an empty graph on n vertices. An option Type that can take on values Directed or Undirected is provided. The default setting is Type -> Undirected." 

EmptyQ::usage = "EmptyQ[g] yields True if graph g contains no edges."

EncroachingListSet::usage = "EncroachingListSet[p] constructs the encroaching list set associated with permutation p."

EquivalenceClasses::usage = "EquivalenceClasses[r] identifies the equivalence classes among the elements of matrix r."

EquivalenceRelationQ::usage = "EquivalenceRelationQ[r] yields True if the matrix r defines an equivalence relation. EquivalenceRelationQ[g] tests whether the adjacency matrix of graph g defines an equivalence relation."

Equivalences::usage = "Equivalences[g, h] lists the vertex equivalence classes between graphs g and h defined by their vertex degrees. Equivalences[g] lists the vertex equivalences for graph g defined by the vertex degrees. Equivalences[g, h, f1, f2, ...] and Equivalences[g, f1, f2, ...] can also be used, where f1, f2, ... are functions that compute other vertex invariants. It is expected that for each function fi, the call fi[g, v] returns the corresponding invariant at vertex v in graph g. The functions f1, f2, ... are evaluated in order, and the evaluation stops either when all functions have been evaluated or when an empty equivalence class is found. Three vertex invariants, DegreesOf2Neighborhood, NumberOf2Paths, and Distances are Combinatorica functions and can be used to refine the equivalences."

Euclidean::usage = "Euclidean is an option for SetEdgeWeights."

Eulerian::usage = "Eulerian[n, k] gives the number of permutations of length n with k runs."

EulerianCycle::usage = "EulerianCycle[g] finds an Eulerian cycle of g if one exists."

EulerianQ::usage = "EulerianQ[g] yields True if graph g is Eulerian, meaning there exists a tour that includes each edge exactly once."

ExactRandomGraph::usage = "ExactRandomGraph[n, e] constructs a random labeled graph of exactly e edges and n vertices."

ExpandGraph::usage = "ExpandGraph[g, n] expands graph g to n vertices by adding disconnected vertices. This is obsolete; use AddVertices[g, n] instead."

ExtractCycles::usage = "ExtractCycles[g] gives a maximal list of edge-disjoint cycles in graph g."

FerrersDiagram::usage = "FerrersDiagram[p] draws a Ferrers diagram of integer partition p."

FindCycle::usage = "FindCycle[g] finds a list of vertices that define a cycle in graph g."

FindSet::usage = "FindSet[n, s] gives the root of the set containing n in union-find data structure s."

FiniteGraphs::usage = "FiniteGraphs produces a convenient list of all the interesting, finite, parameterless graphs built into Combinatorica."

FirstLexicographicTableau::usage = "FirstLexicographicTableau[p] constructs the first Young tableau with shape described by partition p."

FolkmanGraph::usage = "FolkmanGraph returns a smallest graph that is edge-transitive but not vertex-transitive."

FranklinGraph::usage = "FranklinGraph returns a 12-vertex graph that represents a 6-chromatic map on the Klein bottle. It is the sole counterexample to Heawood's map coloring conjecture."

FromAdjacencyLists::usage = "FromAdjacencyLists[l] constructs an edge list representation for a graph from the given adjacency lists l, using a circular embedding. FromAdjacencyLists[l, v] uses v as the embedding for the resulting graph. An option called Type that takes on the values Directed or Undirected can be used to affect the type of graph produced. The default value of Type is Undirected."

FromAdjacencyMatrix::usage = "FromAdjacencyMatrix[m] constructs a graph from a given adjacency matrix m, using a circular embedding. FromAdjacencyMatrix[m, v] uses v as the embedding for the resulting graph. An option Type that takes on the values Directed or Undirected can be used to affect the type of graph produced. The default value of Type is Undirected. FromAdjacencyMatrix[m, EdgeWeight] interprets the entries in m as edge weights, with infinity representing missing edges, and from this constructs a weighted graph using a circular embedding. FromAdjacencyMatrix[m, v, EdgeWeight] uses v as the embedding for the resulting graph. The option Type can be used along with the EdgeWeight tag."

FromCycles::usage = "FromCycles[{c1, c2, ...}] gives the permutation that has the given cycle structure."

FromInversionVector::usage = "FromInversionVector[v] reconstructs the unique permutation with inversion vector v."

FromOrderedPairs::usage = "FromOrderedPairs[l] constructs an edge list representation from a list of ordered pairs l, using a circular embedding. FromOrderedPairs[l, v] uses v as the embedding for the resulting graph. The option Type that takes on values Undirected or Directed can be used to affect the kind of graph produced. The default value of Type is Directed. Type -> Undirected results in the underlying undirected graph."

FromUnorderedPairs::usage = "FromUnorderedPairs[l] constructs an edge list representation from a list of unordered pairs l, using a circular embedding. FromUnorderedPairs[l, v] uses v as the embedding for the resulting graph. The option Type that takes on values Undirected or Directed can be used to affect the kind of graph produced."

FruchtGraph::usage = "FruchtGraph returns the smallest 3-regular graph whose automorphism group consists of only the identity."

FunctionalGraph::usage = "FunctionalGraph[f, v] takes a set v and a function f from v to v and constructs a directed graph with vertex set v and edges (x, f(x)) for each x in v. FunctionalGraph[f, v], where f is a list of functions, constructs a graph with vertex set v and edge set (x, fi(x)) for every fi in f. An option called Type that takes on the values Directed and Undirected is allowed. Type -> Directed is the default, while Type -> Undirected returns the corresponding underlying undirected graph. FunctionalGraph[f, n] takes a nonnegative integer nand a function f from {0,1,..., n-1} onto itself and produces the directed graphwith vertex set {0, 1,..., n-1} and edge set {x, f(x)} for each vertex x." 

GeneralizedPetersenGraph::usage = "GeneralizedPetersenGraph[n, k] returns the generalized Petersen graph, for integers n > 1 and k > 0, which is the graph with vertices {u1, u2, ..., un} and {v1, v2, ..., vn} and edges {ui, u(i+1)}, {vi, v(i+k)}, and {ui, vi}. The Petersen graph is identical to the generalized Petersen graph with n = 5 and k = 2."

GetEdgeLabels::usage = "GetEdgeLabels[g] returns the list of labels of the edges of g. GetEdgeLabels[g, es] returns the list of labels in graph g of the edges in es." 

GetEdgeWeights::usage = "GetEdgeWeights[g] returns the list of weights of the edges of g. GetEdgeWeights[g, es] returns the list of weights in graph g of the edges in es."

GetVertexLabels::usage = "GetVertexLabels[g] returns the list of labels of vertices of g. GetVertexLabels[g, vs] returns the list of labels in graph g of the vertices specified in list vs."

GetVertexWeights::usage = "GetVertexWeights[g] returns the list of weights of vertices of g. GetVertexWeights[g, vs] returns the list of weights in graph g of the vertices in vs."

Girth::usage = "Girth[g] gives the length of a shortest cycle in a simple graph g."

Graph::usage = "Graph[e, v, opts] represents a graph object where e is the list of edges annotated with graphics options, v is a list of vertices annotated with graphics options, and opts is a set of global graph options. e has the form {{{i1, j1}, opts1}, {{i2, j2}, opts2},...}, where {i1, j1}, {i2, j2},... are edges of the graph and opts1, opts2,... are options that respectively apply to these edges. v has the form {{{x1, y1}, opts1}, {{x2, y2}, opts2},...}, where {x1, y1}, {x2, y2},... respectively denote the coordinates in the plane of vertex 1, vertex 2,... and opts1, opts2,... are options that respectively apply to these vertices. Permitted edge options are EdgeWeight, EdgeColor, EdgeStyle, EdgeLabel, EdgeLabelColor, and EdgeLabelPosition. Permitted vertex options are VertexWeight, VertexColor, VertexStyle, VertexNumber, VertexNumberColor, VertexNumberPosition, VertexLabel, VertexLabelColor, and VertexLabelPosition. The third item in a Graph object is opts, a sequence of zero or more global options that apply to all vertices or all edges or to the graph as a whole. All of the edge options and vertex options can be used as global options also. If a global option and a local edge option or vertex option differ, then the local edge or vertex option is used for that particular edge or vertex. In addition to these options, the following two options can also be specified as part of the global options: LoopPosition and EdgeDirection. Furthermore, all the options of the Mathematica function Plot can be used as global options in a Graph object. These can be used to specify how the graph looks when it is drawn. Also, all options of the graphics primitive Arrow can also be specified as part of global graph options. These can be used to affect the look of arrows that represent directed edges. See the usage message of individual options to find out more about values these options can take on. Whether a graph is undirected or directed is given by the option EdgeDirection. This has default value False. For undirected graphs, the edges {i1, j1}, {i2, j2},... have to satisfy i1 <= j1, i2 <= j2,... and for directed graphs the edges {i1, j1}, {i2, j2},... are treated as ordered pairs, each specifying the direction of the edge as well."

GraphCenter::usage = "GraphCenter[g] gives a list of the vertices of graph g with minimum eccentricity."

GraphComplement::usage = "GraphComplement[g] gives the complement of graph g."

GraphDifference::usage = "GraphDifference[g, h] constructs the graph resulting from subtracting the edges of graph h from the edges of graph g."

GraphicQ::usage = "GraphicQ[s] yields True if the list of integers s is a graphic sequence, and thus represents a degree sequence of some graph."

GraphIntersection::usage = "GraphIntersection[g1, g2, ...] constructs the graph defined by the edges that are in all the graphs g1, g2, ...."

GraphJoin::usage = "GraphJoin[g1, g2, ...] constructs the join of graphs g1, g2, and so on. This is the graph obtained by adding all possible edges between different graphs to the graph union of g1, g2, ...."

GraphOptions::usage = "GraphOptions[g] returns the display options associated with g. GraphOptions[g, v] returns the display options associated with vertex v in g. GraphOptions[g, {u, v}] returns the display options associated with edge {u, v} in g."

GraphPolynomial::usage = "GraphPolynomial[n, x] returns a polynomial in x in which the coefficient of x^m is the number of nonisomorphic graphs with n vertices and m edges. GraphPolynomial[n, x, Directed] returns a polynomial in x in which the coefficient of x^m is the number of nonisomorphic directed graphs with n vertices and m edges."

GraphPower::usage = "GraphPower[g, k] gives the kth power of graph g. This is the graph whose vertex set is identical to the vertex set of g and that contains an edge between vertices i and j if g contains a path between i and j of length, at most, k."

GraphProduct::usage = "GraphProduct[g1, g2, ...] constructs the product of graphs g1, g2, and so forth."

GraphSum::usage = "GraphSum[g1, g2, ...] constructs the graph resulting from joining the edge lists of graphs g1, g2, and so forth."

GraphUnion::usage = "GraphUnion[g1, g2, ...] constructs the union of graphs g1, g2, and so forth. GraphUnion[n, g] constructs n copies of graph g, for any nonnegative integer n."

GrayCode::usage = "GrayCode[l] constructs a binary reflected Gray code on set l. GrayCode is obsolete, so use GrayCodeSubsets instead."

GrayCodeKSubsets::usage = "GrayCodeKSubsets[l, k] generates k-subsets of l in Gray code order."

GrayCodeSubsets::usage = "GrayCodeSubsets[l] constructs a binary reflected Gray code on set l."

GrayGraph::usage = "GrayGraph returns a 3-regular, 54-vertex graph that is edge-transitive but not vertex-transitive; the smallest known such example." 

Greedy::usage = "Greedy is a value that the option Algorithm can take in calls to functions such as VertexCover, telling the function to use a greedy algorithm."

GreedyVertexCover::usage = "GreedyVertexCover[g] returns a vertex cover of graph g constructed using the greedy algorithm. This is a natural heuristic for constructing a vertex cover, but it can produce poor vertex covers."

GridGraph::usage = "GridGraph[n, m] constructs an n*m grid graph, the product of paths on n and m vertices. GridGraph[p, q, r] constructs a p*q*r grid graph, the product of GridGraph[p, q] and a path of length r."

GrotztschGraph::usage = "GrotztschGraph returns the smallest triangle-free graph with chromatic number 4. This is identical to MycielskiGraph[4]."

HamiltonianCycle::usage = "HamiltonianCycle[g] finds a Hamiltonian cycle in graph g if one exists. HamiltonianCycle[g, All] gives all Hamiltonian cycles of graph g."

HamiltonianPath::usage = "HamiltonianPath[g] finds a Hamiltonian path in graph g if one exists. HamiltonianPath[g, All] gives all Hamiltonian paths of graph g."

HamiltonianQ::usage = "HamiltonianQ[g] yields True if there exists a Hamiltonian cycle in graph g, or in other words, if there exists a cycle that visits each vertex exactly once."

Harary::usage = "Harary[k, n] constructs the minimal k-connected graph on n vertices."

HasseDiagram::usage = "HasseDiagram[g] constructs a Hasse diagram of the relation defined by directed acyclic graph g."

Heapify::usage = "Heapify[p] builds a heap from permutation p."

HeapSort::usage = "HeapSort[l] performs a heap sort on the items of list l."

HeawoodGraph::usage = "HeawoodGraph returns a smallest (6, 3)-cage, a 3-regular graph with girth 6."

HerschelGraph::usage = "HerschelGraph returns a graph object that represents a Herschel graph."

HideCycles::usage = "HideCycles[c] canonically encodes the cycle structure c into a unique permutation."

Highlight::usage = "Highlight[g, p] displays g with elements in p highlighted. The second argument p has the form {s1, s2,...}, where the sis are disjoint subsets of vertices and edges of g. The options, HighlightedVertexStyle, HighlightedEdgeStyle, HighlightedVertexColors, and HighlightedEdgeColors are used to determine the appearance of the highlighted elements of the graph. The default settings of the style options are HighlightedVertexStyle->Disk[Large] and HighlightedEdgeStyle->Thick. The options HighlightedVertexColors and HighlightedEdgeColors are both set to {Black, Red, Blue, Green, Yellow, Purple, Brown, Orange, Olive, Pink, DeepPink, DarkGreen, Maroon, Navy}. The colors are chosen from the palette of colors with color 1 used for s1, color 2 used for s2, and so on. If there are more parts than colors, then the colors are used cyclically. The function permits all the options that SetGraphOptions permits, for example, VertexColor, VertexStyle, EdgeColor, and EdgeStyle. These options can be used to control the appearance of the non-highlighted vertices and edges."

HighlightedEdgeColors::usage = "HighlightedEdgeColors is an option to Highlight that determines which colors are used for the highlighted edges."

HighlightedEdgeStyle::usage = "HighlightedEdgeStyle is an option to Highlight that determines how the highlighted edges are drawn."

HighlightedVertexColors::usage = "HighlightedVertexColors is an option to Highlight that determines which colors are used for the highlighted vertices."

HighlightedVertexStyle::usage = "HighlightedVertexStyle is an option to Highlight that determines how the highlighted vertices are drawn."

Hypercube::usage = "Hypercube[n] constructs an n-dimensional hypercube."

IcosahedralGraph::usage = "IcosahedralGraph returns the graph corresponding to the icosahedron, a Platonic solid."

IdenticalQ::usage = "IdenticalQ[g, h] yields True if graphs g and h have identical edge lists, even though the associated graphics information need not be the same."

IdentityPermutation::usage = "IdentityPermutation[n] gives the size-n identity permutation."

IncidenceMatrix::usage = "IncidenceMatrix[g] returns the (0, 1)-matrix of graph g, which has a row for each vertex and a column for each edge and (v, e) = 1 if and only if vertex v is incident upon edge e. For a directed graph, (v, e) = 1 if edge e is outgoing from v."

InDegree::usage = "InDegree[g, n] returns the in-degree of vertex n in directed graph g. InDegree[g] returns the sequence of in-degrees of the vertices in directed graph g."

IndependentSetQ::usage = "IndependentSetQ[g, i] yields True if the vertices in list i define an independent set in graph g."

Index::usage = "Index[p] gives the index of permutation p, the sum of all subscripts j such that p[j] is greater than p[j+1]."

InduceSubgraph::usage = "InduceSubgraph[g, s] constructs the subgraph of graph g induced by the list of vertices s."

InitializeUnionFind::usage = "InitializeUnionFind[n] initializes a union-find data structure for n elements."

InsertIntoTableau::usage = "InsertIntoTableau[e, t] inserts integer e into Young tableau t using the bumping algorithm. InsertIntoTableau[e, t, All] inserts e into Young tableau t and returns the new tableau as well as the row whose size is expanded as a result of the insertion."

IntervalGraph::usage = "IntervalGraph[l] constructs the interval graph defined by the list of intervals l."

Invariants::usage = "Invariants is an option to the functions Isomorphism and IsomorphicQ that informs these functions about which vertex invariants to use in computing equivalences between vertices."

InversePermutation::usage = "InversePermutation[p] yields the multiplicative inverse of permutation p."

InversionPoset::usage = "InversionPoset[n] returns a Hasse diagram of the partially ordered set on size-n permutations in which p < q if q can be obtained from p by an adjacent transposition that places the larger element before the smaller. The function takes two options: Type and VertexLabel, with default values Undirected and False, respectively. When Type is set to Directed, the function produces the underlying directed acyclic graph. When VertexLabel is set to True, labels are produced for the vertices."

Inversions::usage = "Inversions[p] counts the number of inversions in permutation p."

InvolutionQ::usage = "InvolutionQ[p] yields True if permutation p is its own inverse."

Involutions::usage = "Involutions[l] gives the list of involutions of the elements in the list l. Involutions[l, Cycles] gives the involutions in their cycle representation. Involution[n] gives size-n involutions. Involutions[n, Cycles] gives size-n involutions in their cycle representation."

IsomorphicQ::usage = "IsomorphicQ[g, h] yields True if graphs g and h are isomorphic. This function takes an option Invariants -> {f1, f2, ...}, where f1, f2, ... are functions that are used to compute vertex invariants. These functions are used in the order in which they are specified. The default value of Invariants is {DegreesOf2Neighborhood, NumberOf2Paths, Distances}."

Isomorphism::usage = "Isomorphism[g, h] gives an isomorphism between graphs g and h if one exists. Isomorphism[g, h, All] gives all isomorphisms between graphs g and h. Isomorphism[g] gives the automorphism group of g. This function takes an option Invariants -> {f1, f2, ...}, where f1, f2, ... are functions that are used to compute vertex invariants. These functions are used in the order in which they are specified. The default value of Invariants is {DegreesOf2Neighborhood, NumberOf2Paths, Distances}."

IsomorphismQ::usage = "IsomorphismQ[g, h, p] tests if permutation p defines an isomorphism between graphs g and h."

Josephus::usage = "Josephus[n, m] generates the inverse of the permutation defined by executing every mth member in a circle of n members."

KnightsTourGraph::usage = "KnightsTourGraph[m, n] returns a graph with m*n vertices in which each vertex represents a square in an m x n chessboard and each edge corresponds to a legal move by a knight from one square to another."

KSetPartitions::usage = "KSetPartitions[set, k] returns the list of set partitions of set with k blocks. KSetPartitions[n, k] returns the list of set partitions of {1, 2, ..., n} with k blocks. If all set partitions of a set are needed, use the function SetPartitions."

KSubsetGroup::usage = "KSubsetGroup[pg, s] returns the group induced by a permutation group pg on the set s of k-subsets of [n], where n is the index of pg. The optional argument Type can be Ordered or Unordered and depending on the value of Type s is treated as a set of k-subsets or k-tuples."

KSubsetGroupIndex::usage = "KSubsetGroupIndex[g, s, x] returns the cycle index of the k-subset group on s expressed as a polynomial in x[1], x[2], .... This function also takes the optional argument Type that tells the function whether the elements of s should be treated as sets or tuples."

KSubsets::usage = "KSubsets[l, k] gives all subsets of set l containing exactly k elements, ordered lexicographically."

$NewMessage[ K, "usage"] (* reset the usage of K to the System usage *)
If[StringQ[K::usage], K::usage = StringJoin[ K::usage, " The use of K to create a complete graph is obsolete. Use CompleteGraph to create a complete graph."]]

LabeledTreeToCode::usage = "LabeledTreeToCode[g] reduces the tree g to its Prufer code."

Large::usage = "Large is a symbol used to denote the size of the object that represents a vertex. The option VertexStyle can be set to Disk[Large] or Box[Large] either inside the graph data structure or in ShowGraph."

LastLexicographicTableau::usage = "LastLexicographicTableau[p] constructs the last Young tableau with shape described by partition p."

Level::usage = "Level is an option for the function BreadthFirstTraversal that makes the function return levels of vertices."

LeviGraph::usage = "LeviGraph returns the unique (8, 3)-cage, a 3-regular graph whose girth is 8."

LexicographicPermutations::usage = "LexicographicPermutations[l] constructs all permutations of list l in lexicographic order."

LexicographicSubsets::usage = "LexicographicSubsets[l] gives all subsets of set l in lexicographic order. LexicographicSubsets[n] returns all subsets of {1, 2,..., n} in lexicographic order."

LineGraph::usage = "LineGraph[g] constructs the line graph of graph g."

ListGraphs::usage = "ListGraphs[n, m] returns all nonisomorphic undirected graphs with n vertices and m edges. ListGraphs[n, m, Directed] returns all nonisomorphic directed graphs with n vertices and m edges. ListGraphs[n] returns all nonisomorphic undirected graphs with n vertices. ListGraphs[n, Directed] returns all nonisomorphic directed graphs with n vertices."

ListNecklaces::usage = "ListNecklaces[n, c, Cyclic] returns all distinct necklaces whose beads are colored by colors from c. Here c is a list of n, not necessarily distinct colors, and two colored necklaces are considered equivalent if one can be obtained by rotating the other. ListNecklaces[n, c, Dihedral] is similar except that two necklaces are considered equivalent if one can be obtained from the other by a rotation or a flip."

LNorm::usage = "LNorm[p] is a value that the option WeightingFunction, used in the function SetEdgeWeights, can take. Here p can be any integer or Infinity."

LongestIncreasingSubsequence::usage = "LongestIncreasingSubsequence[p] finds the longest increasing scattered subsequence of permutation p."

LoopPosition::usage = "LoopPosition is an option to ShowGraph whose values tell ShowGraph where to position a loop around a vertex. This option can take on values UpperLeft, UpperRight, LowerLeft, and LowerRight."

LowerLeft::usage = "LowerLeft is a value that options VertexNumberPosition, VertexLabelPosition, and EdgeLabelPosition can take on in ShowGraph."

LowerRight::usage = "LowerRight is a value that options VertexNumberPosition, VertexLabelPosition, and EdgeLabelPosition can take on in ShowGraph."

M::usage = "M[g] gives the number of edges in the graph g. M[g, Directed] is obsolete because M[g] works for directed as well as undirected graphs."

MakeDirected::usage = "MakeDirected[g] constructs a directed graph from a given undirected graph g by replacing each undirected edge in g by two directed edges pointing in opposite directions. The local options associated with edges are not inherited by the corresponding directed edges. Calling the function with the tag All, as MakeDirected[g, All], ensures that local options associated with each edge are inherited by both corresponding directed edges."

MakeGraph::usage = "MakeGraph[v, f] constructs the graph whose vertices correspond to v and edges between pairs of vertices x and y in v for which the binary relation defined by the Boolean function f is True. MakeGraph takes two options, Type and VertexLabel. Type can be set to Directed or Undirected and this tells MakeGraph whether to construct a directed or an undirected graph. The default setting is Directed. VertexLabel can be set to True or False, with False being the default setting. Using VertexLabel -> True assigns labels derived from v to the vertices of the graph."

MakeSimple::usage = "MakeSimple[g] gives the undirected graph, free of multiple edges and self-loops derived from graph g."

MakeUndirected::usage = "MakeUndirected[g] gives the underlying undirected graph of the given directed graph g."

MaximalMatching::usage = "MaximalMatching[g] gives the list of edges associated with a maximal matching of graph g."

MaximumAntichain::usage = "MaximumAntichain[g] gives a largest set of unrelated vertices in partial order g."

MaximumClique::usage = "MaximumClique[g] finds a largest clique in graph g. MaximumClique[g, k] returns a k-clique, if such a thing exists in g; otherwise it returns {}."

MaximumIndependentSet::usage = "MaximumIndependentSet[g] finds a largest independent set of graph g."

MaximumSpanningTree::usage = "MaximumSpanningTree[g] uses Kruskal's algorithm to find a maximum spanning tree of graph g."

McGeeGraph::usage = "McGeeGraph returns the unique (7, 3)-cage, a 3-regular graph with girth 7."

MeredithGraph::usage = "MeredithGraph returns a 4-regular, 4-connected graph that is not Hamiltonian, providing a counterexample to a conjecture by C. St. J. A. Nash-Williams."

MinimumChainPartition::usage = "MinimumChainPartition[g] partitions partial order g into a minimum number of chains."

MinimumChangePermutations::usage = "MinimumChangePermutations[l] constructs all permutations of list l such that adjacent permutations differ by only one transposition."

MinimumSpanningTree::usage = "MinimumSpanningTree[g] uses Kruskal's algorithm to find a minimum spanning tree of graph g."

MinimumVertexColoring::usage = "MinimumVertexColoring[g] returns a minimum vertex coloring of g. MinimumVertexColoring[g, k] returns a k-coloring of g, if one exists."

MinimumVertexCover::usage = "MinimumVertexCover[g] finds a minimum vertex cover of graph g. For bipartite graphs, the function uses the polynomial-time Hungarian algorithm. For everything else, the function uses brute force."

MultipleEdgesQ::usage = "MultipleEdgesQ[g] yields True if g has multiple edges between pairs of vertices. It yields False otherwise."

MultiplicationTable::usage = "MultiplicationTable[l, f] constructs the complete transition table defined by the binary relation function f on the elements of list l."

MycielskiGraph::usage = "MycielskiGraph[k] returns a triangle-free graph with chromatic number k, for any positive integer k."

NecklacePolynomial::usage = "NecklacePolynomial[n, c, Cyclic] returns a polynomial in the colors in c whose coefficients represent numbers of ways of coloring an n-bead necklace with colors chosen from c, assuming that two colorings are equivalent if one can be obtained from the other by a rotation. NecklacePolynomial[n, c, Dihedral] is different in that it considers two colorings equivalent if one can be obtained from the other by a rotation or a flip or both."

Neighborhood::usage = "Neighborhood[g, v, k] returns the subset of vertices in g that are at a distance of k or less from vertex v. Neighborhood[al, v, k] behaves identically, except that it takes as input an adjacency list al."

NetworkFlow::usage = "NetworkFlow[g, source, sink] returns the value of a maximum flow through graph g from source to sink. NetworkFlow[g, source, sink, Edge] returns the edges in g that have positive flow along with their flows in a maximum flow from source to sink. NetworkFlow[g, source, sink, Cut] returns a minimum cut between source and sink. NetworkFlow[g, source, sink, All] returns the adjacency list of g along with flows on each edge in a maximum flow from source to sink. g can be a directed or an undirected graph."

NetworkFlowEdges::usage = "NetworkFlowEdges[g, source, sink] returns the edges of the graph with positive flow, showing the distribution of a maximum flow from source to sink in graph g. This is obsolete, and NetworkFlow[g, source, sink, Edge] should be used instead."

NextBinarySubset::usage = "NextBinarySubset[l, s] constructs the subset of l following subset s in the order obtained by interpreting subsets as binary string representations of integers."

NextComposition::usage = "NextComposition[l] constructs the integer composition that follows l in a canonical order."

NextGrayCodeSubset::usage = "NextGrayCodeSubset[l, s] constructs the successor of s in the Gray code of set l."

NextKSubset::usage = "NextKSubset[l, s] gives the k-subset of list l, following the k-subset s in lexicographic order."

NextLexicographicSubset::usage = "NextLexicographicSubset[l, s] gives the lexicographic successor of subset s of set l."

NextPartition::usage = "NextPartition[p] gives the integer partition following p in reverse lexicographic order."

NextPermutation::usage = "NextPermutation[p] gives the permutation following p in lexicographic order."

NextSubset::usage = "NextSubset[l, s] constructs the subset of l following subset s in canonical order."

NextTableau::usage = "NextTableau[t] gives the tableau of shape t, following t in lexicographic order."

NoMultipleEdges::usage = "NoMultipleEdges is an option value for Type."

NonLineGraphs::usage = "NonLineGraphs returns a graph whose connected components are the 9 graphs whose presence as a vertex-induced subgraph in a graph g makes g a nonline graph."

NoPerfectMatchingGraph::usage = "NoPerfectMatchingGraph returns a connected graph with 16 vertices that contains no perfect matching."

Normal::usage = "Normal is a value that options VertexStyle, EdgeStyle, and PlotRange can take on in ShowGraph."

NormalDashed::usage = "NormalDashed is a value that the option EdgeStyle can take on in the graph data structure or in ShowGraph."

NormalizeVertices::usage = "NormalizeVertices[v] gives a list of vertices with a similar embedding as v but with all coordinates of all points scaled to be between 0 and 1."

NoSelfLoops::usage = "NoSelfLoops is an option value for Type."

NthPair::usage = "NthPair[n] returns the nth unordered pair of distinct positive integers, when sequenced to minimize the size of the larger integer. Pairs that have the same larger integer are sequenced in increasing order of their smaller integer."

NthPermutation::usage = "NthPermutation[n, l] gives the nth lexicographic permutation of list l. This function is obsolete; use UnrankPermutation instead."

NthSubset::usage = "NthSubset[n, l] gives the nth subset of list l in canonical order."

NumberOf2Paths::usage = "NumberOf2Paths[g, v] returns a sorted list that contains the number of paths of length 2 to different vertices of g from v."

NumberOfCompositions::usage = "NumberOfCompositions[n, k] counts the number of distinct compositions of integer n into k parts."

NumberOfDerangements::usage = "NumberOfDerangements[n] counts the derangements on n elements, that is, the permutations without any fixed points."

NumberOfDirectedGraphs::usage = "NumberOfDirectedGraphs[n] returns the number of nonisomorphic directed graphs with n vertices. NumberOfDirectedGraphs[n, m] returns the number of nonisomorphic directed graphs with n vertices and m edges."

NumberOfGraphs::usage = "NumberOfGraphs[n] returns the number of nonisomorphic undirected graphs with n vertices. NumberOfGraphs[n, m] returns the number of nonisomorphic undirected graphs with n vertices and m edges."

NumberOfInvolutions::usage = "NumberOfInvolutions[n] counts the number of involutions on n elements."

NumberOfKPaths::usage = "NumberOfKPaths[g, v, k] returns a sorted list that contains the number of paths of length k to different vertices of g from v. NumberOfKPaths[al, v, k] behaves identically, except that it takes an adjacency list al as input."

NumberOfNecklaces::usage = "NumberOfNecklaces[n, nc, Cyclic] returns the number of distinct ways in which an n-bead necklace can be colored with nc colors, assuming that two colorings are equivalent if one can be obtained from the other by a rotation. NumberOfNecklaces[n, nc, Dihedral] returns the number of distinct ways in which an n-bead necklace can be colored with nc colors, assuming that two colorings are equivalent if one can be obtained from the other by a rotation or a flip."

NumberOfPartitions::usage = "NumberOfPartitions[n] counts the number of integer partitions of n."

NumberOfPermutationsByCycles::usage = "NumberOfPermutationsByCycles[n, m] gives the number of permutations of length n with exactly m cycles."

NumberOfPermutationsByInversions::usage = "NumberOfPermutationsByInversions[n, k] gives the number of permutations of length n with exactly k inversions. NumberOfPermutationsByInversions[n] gives a table of the number of length-n permutations with k inversions, for all k."

NumberOfPermutationsByType::usage = "NumberOfPermutationsByTypes[l] gives the number of permutations of type l."

NumberOfSpanningTrees::usage = "NumberOfSpanningTrees[g] gives the number of labeled spanning trees of graph g."

NumberOfTableaux::usage = "NumberOfTableaux[p] uses the hook length formula to count the number of Young tableaux with shape defined by partition p."

OctahedralGraph::usage = "OctahedralGraph returns the graph corresponding to the octahedron, a Platonic solid."

OddGraph::usage = "OddGraph[n] returns the graph whose vertices are the size-(n-1) subsets of a size-(2n-1) set and whose edges connect pairs of vertices that correspond to disjoint subsets. OddGraph[3] is the Petersen graph."

One::usage = "One is a tag used in several functions to inform the functions that only one object need be considered or only one solution be produced, as opposed to all objects or all solutions."

Optimum::usage = "Optimum is a value that the option Algorithm can take on when used in functions VertexColoring and VertexCover."

OrbitInventory::usage = "OrbitInventory[ci, x, w] returns the value of the cycle index ci when each formal variable x[i] is replaced by w. OrbitInventory[ci, x, weights] returns the inventory of orbits induced on a set of functions by the action of a group with cycle index ci. It is assumed that each element in the range of the functions is assigned a weight in list weights."

OrbitRepresentatives::usage = "OrbitRepresentatives[pg, x] returns a representative of each orbit of x induced by the action of the group pg on x. pg is assumed to be a set of permutations on the first n natural numbers and x is a set of functions whose domain is the first n natural numbers. Each function in x is specified as an n-tuple."

Orbits::usage = "Orbits[pg, x] returns the orbits of x induced by the action of the group pg on x. pg is assumed to be a set of permutations on the first n natural numbers and x is a set of functions whose domain is the first n natural numbers. Each function in x is specified as an n-tuple." 

Ordered::usage = "Ordered is an option to the functions KSubsetGroup and KSubsetGroupIndex that tells the functions whether they should treat the input as sets or tuples."

OrientGraph::usage = "OrientGraph[g] assigns a direction to each edge of a bridgeless, undirected graph g, so that the graph is strongly connected."

OutDegree::usage = "OutDegree[g, n] returns the out-degree of vertex n in directed graph g. OutDegree[g] returns the sequence of out-degrees of the vertices in directed graph g."

PairGroup::usage = "PairGroup[g] returns the group induced on 2-sets by the permutation group g. PairGroup[g, Ordered] returns the group induced on ordered pairs with distinct elements by the permutation group g."

PairGroupIndex::usage = "PairGroupIndex[g, x] returns the cycle index of the pair group induced by g as a polynomial in x[1], x[2], .... PairGroupIndex[ci, x] takes the cycle index ci of a group g with formal variables x[1], x[2], ..., and returns the cycle index of the pair group induced by g. PairGroupIndex[g, x, Ordered] returns the cycle index of the ordered pair group induced by g as a polynomial in x[1], x[2], .... PairGroupIndex[ci, x, Ordered] takes the cycle index ci of a group g with formal variables x[1], x[2], ..., and returns the cycle index of the ordered pair group induced by g."

Parent::usage = "Parent is a tag used as an argument to the function AllPairsShortestPath in order to inform this function that information about parents in the shortest paths is also wanted."

ParentsToPaths::usage = "ParentsToPaths[l, i, j] takes a list of parents l and returns the path from i to j encoded in the parent list. ParentsToPaths[l, i] returns the paths from i to all vertices."

PartialOrderQ::usage = "PartialOrderQ[g] yields True if the binary relation defined by edges of the graph g is a partial order, meaning it is transitive, reflexive, and antisymmetric. PartialOrderQ[r] yields True if the binary relation defined by the square matrix r is a partial order."

PartitionLattice::usage = "PartitionLattice[n] returns a Hasse diagram of the partially ordered set on set partitions of 1 through n in which p < q if q is finer than p, that is, each block in q is contained in some block in p. The function takes two options: Type and VertexLabel, with default values Undirected and False, respectively. When Type is set to Directed, the function produces the underlying directed acyclic graph. When VertexLabel is set to True, labels are produced for the vertices."

PartitionQ::usage = "PartitionQ[p] yields True if p is an integer partition. PartitionQ[n, p] yields True if p is a partition of n."

Partitions::usage = "Partitions[n] constructs all partitions of integer n in reverse lexicographic order. Partitions[n, k] constructs all partitions of the integer n with maximum part at most k, in reverse lexicographic order."

If[$VersionNumber < 4, Path::usage=" "];
$NewMessage[Path, "usage"] (* reset the usage of Path to the system usage *)
If[StringQ[Path::usage], Path::usage = StringJoin[Path::usage, " Path[n] constructs a tree consisting only of a path on n vertices. Path[n] permits an option Type that takes on the values Directed and Undirected. The default setting is Type -> Undirected."]]

PathConditionGraph::usage = "The usage of PathConditionGraph is obsolete. This functionality is no longer supported in Combinatorica."

PerfectQ::usage = "PerfectQ[g] yields True if g is a perfect graph, meaning that for every induced subgraph of g the size of a largest clique equals the chromatic number."

PermutationGraph::usage = "PermutationGraph[p] gives the permutation graph for the permutation p."

PermutationGroupQ::usage = "PermutationGroupQ[l] yields True if the list of permutations l forms a permutation group."

PermutationQ::usage = "PermutationQ[p] yields True if p is a list representing a permutation and False otherwise."

PermutationToTableaux::usage = "PermutationToTableaux[p] returns the tableaux pair that can be constructed from p using the Robinson-Schensted-Knuth correspondence."

PermutationType::usage = "PermutationType[p] returns the type of permutation p."

PermutationWithCycle::usage = "PermutationWithCycle[n, {i, j, ...}] gives a size-n permutation in which {i, j, ...} is a cycle and all other elements are fixed points."

Permute::usage = "Permute[l, p] permutes list l according to permutation p."

PermuteSubgraph::usage = "PermuteSubgraph[g, p] permutes the vertices of a subgraph of g induced by p according to p."

PetersenGraph::usage = "PetersenGraph returns the Petersen graph, a graph whose vertices can be viewed as the size-2 subsets of a size-5 set with edges connecting disjoint subsets."

PlanarQ::usage = "PlanarQ[g] yields True if graph g is planar, meaning it can be drawn in the plane so no two edges cross."

PointsAndLines::usage = "PointsAndLines is now obsolete."

Polya::usage = "Polya[g, m] returns the polynomial giving the number of colorings, with m colors, of a structure defined by the permutation group g. Polya is obsolete; use OrbitInventory instead."

PseudographQ::usage = "PseudographQ[g] yields True if graph g is a pseudograph, meaning it contains self-loops."

RadialEmbedding::usage = "RadialEmbedding[g, v] constructs a radial embedding of the graph g in which vertices are placed on concentric circles around v depending on their distance from v. RadialEmbedding[g] constructs a radial embedding of graph g, radiating from the center of the graph."

Radius::usage = "Radius[g] gives the radius of graph g, the minimum eccentricity of any vertex of g."

RandomComposition::usage = "RandomComposition[n, k] constructs a random composition of integer n into k parts."

RandomGraph::usage = "RandomGraph[n, p] constructs a random labeled graph on n vertices with an edge probability of p. An option Type is provided, which can take on values Directed and Undirected, and whose default value is Undirected. Type->Directed produces a corresponding random directed graph. The usages Random[n, p, Directed], Random[n, p, range], and Random[n, p, range, Directed] are all obsolete. Use SetEdgeWeights to set random edge weights."

RandomHeap::usage = "RandomHeap[n] constructs a random heap on n elements."

RandomInteger::usage = "RandomInteger is a value that the WeightingFunction option of the function SetEdgeWeights can take."

RandomKSetPartition::usage = "RandomKSetPartition[set, k] returns a random set partition of set with k blocks. RandomKSetPartition[n, k] returns a random set partition of the first n natural numbers into k blocks."

RandomKSubset::usage = "RandomKSubset[l, k] gives a random subset of set l with exactly k elements."

RandomPartition::usage = "RandomPartition[n] constructs a random partition of integer n."

RandomPermutation::usage = "RandomPermutation[n] generates a random permutation of the first n natural numbers."

RandomPermutation1::usage = "RandomPermutation1 is now obsolete. Use RandomPermutation instead."

RandomPermutation2::usage = "RandomPermutation2 is now obsolete. Use RandomPermutation instead."

RandomRGF::usage = "RandomRGF[n] returns a random restricted growth function (RGF) defined on the first n natural numbers. RandomRGF[n, k] returns a random RGF defined on the first n natural numbers having maximum element equal to k."

RandomSetPartition::usage = "RandomSetPartition[set] returns a random set partition of set. RandomSetPartition[n] returns a random set partition of the first n natural numbers."

RandomSubset::usage = "RandomSubset[l] creates a random subset of set l."

RandomTableau::usage = "RandomTableau[p] constructs a random Young tableau of shape p."

RandomTree::usage = "RandomTree[n] constructs a random labeled tree on n vertices."

RandomVertices::usage = "RandomVertices[g] assigns a random embedding to graph g."

RankBinarySubset::usage = "RankBinarySubset[l, s] gives the rank of subset s of set l in the ordering of subsets of l, obtained by interpreting these subsets as binary string representations of integers."

RankedEmbedding::usage = "RankedEmbedding[l] takes a set partition l of vertices {1, 2,..., n} and returns an embedding of the vertices in the plane such that the vertices in each block occur on a vertical line with block 1 vertices on the leftmost line, block 2 vertices in the next line, and so on. RankedEmbedding[g, l] takes a graph g and a set partition l of the vertices of g and returns the graph g with vertices embedded according to RankedEmbedding[l]. RankedEmbedding[g, s] takes a graph g and a set s of vertices of g and returns a ranked embedding of g in which vertices in s are in block 1, vertices at distance 1 from any vertex in block 1 are in block 2, and so on."

RankGraph::usage = "RankGraph[g, l] partitions the vertices into classes based on the shortest geodesic distance to a member of list l."

RankGrayCodeSubset::usage = "RankGrayCodeSubset[l, s] gives the rank of subset s of set l in the Gray code ordering of the subsets of l." 

RankKSetPartition::usage = "RankKSetPartition[sp, s] ranks sp in the list of all k-block set partitions of s. RankSetPartition[sp] ranks sp in the list of all k-block set partitions of the set of elements that appear in any subset in sp."

RankKSubset::usage = "RankKSubset[s, l] gives the rank of k-subset s of set l in the lexicographic ordering of the k-subsets of l." 

RankPermutation::usage = "RankPermutation[p] gives the rank of permutation p in lexicographic order."

RankRGF::usage = "RankRGF[f] returns the rank of a restricted growth function (RGF) f in the lexicographic order of all RGFs."

RankSetPartition::usage = "RankSetPartition[sp, s] ranks sp in the list of all set partitions of set s. RankSetPartition[sp] ranks sp in the list of all set partitions of the set of elements that appear in any subset in sp."

RankSubset::usage = "RankSubset[l, s] gives the rank, in canonical order, of subset s of set l."

ReadGraph::usage = "ReadGraph[f] reads a graph represented as edge lists from file f and returns a graph object."

RealizeDegreeSequence::usage = "RealizeDegreeSequence[s] constructs a semirandom graph with degree sequence s."

ReflexiveQ::usage = "ReflexiveQ[g] yields True if the adjacency matrix of g represents a reflexive binary relation."

RegularGraph::usage = "RegularGraph[k, n] constructs a semirandom k-regular graph on n vertices, if such a graph exists."

RegularQ::usage = "RegularQ[g] yields True if g is a regular graph."

RemoveMultipleEdges::usage = "RemoveMultipleEdges[g] returns the graph obtained by deleting multiple edges from g."

RemoveSelfLoops::usage = "RemoveSelfLoops[g] returns the graph obtained by deleting self-loops in g."

ResidualFlowGraph::usage = "ResidualFlowGraph[g, flow] returns the directed residual flow graph for graph g with respect to flow."

RevealCycles::usage = "RevealCycles[p] unveils the canonical hidden cycle structure of permutation p."

ReverseEdges::usage = "ReverseEdges[g] flips the directions of all edges in a directed graph."

RGFQ::usage = "RGFQ[l] yields True if l is a restricted growth function. It yields False otherwise."

RGFs::usage = "RGFs[n] lists all restricted growth functions on the first n natural numbers in lexicographic order."

RGFToSetPartition::usage = "RGFToSetPartition[rgf, set] converts the restricted growth function rgf into the corresponding set partition of set. If the optional second argument, set, is not supplied, then rgf is converted into a set partition of {1, 2, ..., Length[rgf]}."

RobertsonGraph::usage = "RobertsonGraph returns a 19-vertex graph that is the unique (4, 5)-cage graph."

RootedEmbedding::usage = "RootedEmbedding[g, v] constructs a rooted embedding of graph g with vertex v as the root. RootedEmbedding[g] constructs a rooted embedding with a center of g as the root."

RotateVertices::usage = "RotateVertices[v, theta] rotates each vertex position in list v by theta radians about the origin (0, 0). RotateVertices[g, theta] rotates the embedding of the graph g by theta radians about the origin (0, 0)."

Runs::usage = "Runs[p] partitions p into contiguous increasing subsequences."

SamenessRelation::usage = "SamenessRelation[l] constructs a binary relation from a list l of permutations, which is an equivalence relation if l is a permutation group."

SelectionSort::usage = "SelectionSort[l, f] sorts list l using ordering function f."

SelfComplementaryQ::usage = "SelfComplementaryQ[g] yields True if graph g is self-complementary, meaning it is isomorphic to its complement."

SelfLoopsQ::usage = "SelfLoopsQ[g] yields True if graph g has self-loops."

SetEdgeLabels::usage = "SetEdgeLabels[g, l] assigns the labels in l to edges of g. If l is shorter than the number of edges in g, then labels get assigned cyclically. If l is longer than the number of edges in g, then the extra labels are ignored."

SetEdgeWeights::usage = "SetEdgeWeights[g] assigns random real weights in the range [0, 1] to edges in g. SetEdgeWeights accepts options WeightingFunction and WeightRange. WeightingFunction can take values Random, RandomInteger, Euclidean, or LNorm[n] for nonnegative n, or any pure function that takes two arguments, each argument having the form {Integer, {Number, Number}}. WeightRange can be an integer range or a real range. The default value for WeightingFunction is Random and the default value for WeightRange is [0, 1]. SetEdgeWeights[g, e] assigns edge weights to the edges in the edge list e. SetEdgeWeights[g, w] assigns the weights in the weight list w to the edges of g. SetEdgeWeights[g, e, w] assigns the weights in the weight list w to the edges in edge list e."

SetGraphOptions::usage = "SetGraphOptions[g, opts] returns g with the options opts set. SetGraphOptions[g, {v1, v2, ..., vopts}, gopts] returns the graph with the options vopts set for vertices v1, v2, ... and the options gopts set for the graph g. SetGraphOptions[g, {e1, e2,..., eopts}, gopts], with edges e1, e2,..., works similarly. SetGraphOptions[g, {{elements1, opts1}, {elements2, opts2},...}, opts] returns g with the options opts1 set for the elements in the sequence elements1, the options opts2 set for the elements in the sequence elements2, and so on. Here, elements can be a sequence of edges or a sequence of vertices. A tag that takes on values One or All can also be passed in as an argument before any options. The default value of the tag is All and it is useful if the graph has multiple edges. It informs the function about whether all edges that connect a pair of vertices are to be affected or only one edge is affected."

SetPartitionListViaRGF::usage = "SetPartitionListViaRGF[n] lists all set partitions of the first n natural numbers, by first listing all restricted growth functions (RGFs) on these and then mapping the RGFs to corresponding set partitions. SetPartitionListViaRGF[n, k] lists all RGFs on the first n natural numbers whose maximum element is k and then maps these RGFs into the corresponding set partitions, all of which contain exactly k blocks." 

SetPartitionQ::usage = "SetPartitionQ[sp, s] determines if sp is a set partition of set s. SetPartitionQ[sp] tests if sp is a set of disjoint sets."

SetPartitions::usage = "SetPartitions[set] returns the list of set partitions of set. SetPartitions[n] returns the list of set partitions of {1, 2, ..., n}. If all set partitions with a fixed number of subsets are needed use KSetPartitions."

SetPartitionToRGF::usage = "SetPartitionToRGF[sp, set] converts the set partition sp of set into the corresponding restricted growth function. If the optional argument set is not specified, then it is assumed that Mathematica knows the underlying order on the set for which sp is a set partition."

SetVertexLabels::usage = "SetVertexLabels[g, l] assigns the labels in l to vertices of g. If l is shorter than the number of vertices in g, then labels get assigned cyclically. If l is longer than the number of vertices in g, then the extra labels are ignored." 

SetVertexWeights::usage = "SetVertexWeights[g] assigns random real weights in the range [0, 1] to vertices in g. SetVertexWeights accepts options WeightingFunction and WeightRange. WeightingFunction can take values Random, RandomInteger, or any pure function that takes two arguments, an integer as the first argument and a pair {number, number} as the second argument. WeightRange can be an integer range or a real range. The default value for WeightingFunction is Random and the default value for WeightRange is [0, 1]. SetVertexWeights[g, w] assigns the weights in the weight list w to the vertices of g. SetVertexWeights[g, vs, w] assigns the weights in the weight list w to the vertices in the vertex list vs."

ShakeGraph::usage = "ShakeGraph[g, d] performs a random perturbation of the vertices of graph g, with each vertex moving, at most, a distance d from its original position."

ShortestPath::usage = "ShortestPath[g, start, end] finds a shortest path between vertices start and end in graph g. An option Algorithm that takes on the values Automatic, Dijkstra, or BellmanFord is provided. This allows a choice between using Dijkstra's algorithm and the Bellman-Ford algorithm. The default is Algorithm -> Automatic. In this case, depending on whether edges have negative weights and depending on the density of the graph, the algorithm chooses between Bellman-Ford and Dijkstra." 

ShortestPathSpanningTree::usage = "ShortestPathSpanningTree[g, v] constructs a shortest-path spanning tree rooted at v, so that a shortest path in graph g from v to any other vertex is a path in the tree. An option Algorithm that takes on the values Automatic, Dijkstra, or BellmanFord is provided. This allows a choice between Dijkstra's algorithm and the Bellman-Ford algorithm. The default is Algorithm -> Automatic. In this case, depending on whether edges have negative weights and depending on the density of the graph, the algorithm chooses between Bellman-Ford and Dijkstra."

ShowGraph::usage = "ShowGraph[g] displays the graph g. ShowGraph[g, options] modifies the display using the given options. ShowGraph[g, Directed] is obsolete and it is currently identical to ShowGraph[g]. All options that affect the look of a graph can be specified as options in ShowGraph. The list of options is: VertexColor, VertexStyle, VertexNumber, VertexNumberColor, VertexNumberPosition, VertexLabel, VertexLabelColor, VertexLabelPosition, EdgeColor, EdgeStyle, EdgeLabel, EdgeLabelColor, EdgeLabelPosition, LoopPosition, and EdgeDirection. In addition, options of the Mathematica function Plot and options of the graphics primitive Arrow can also be specified here. If an option specified in ShowGraph differ from options explicitly set within a graph object, then options specified inside the graph object are used." 

ShowGraphArray::usage = "ShowGraphArray[{g1, g2, ...}] displays a row of graphs. ShowGraphArray[{ {g1, ...}, {g2, ...}, ...}] displays a two-dimensional table of graphs. ShowGraphArray accepts all the options accepted by ShowGraph, and the user can also provide the option GraphicsSpacing -> d."

ShowLabeledGraph::usage = "ShowLabeledGraph[g] displays graph g according to its embedding, with each vertex labeled with its vertex number. ShowLabeledGraph[g, l] uses the ith element of list l as the label for vertex i."

ShuffleExchangeGraph::usage = "ShuffleExchangeGraph[n] returns the n-dimensional shuffle-exchange graph whose vertices are length n binary strings with an edge from w to w' if (i) w' differs from w in its last bit or (ii) w' is obtained from w by a cyclic shift left or a cyclic shift right. An option VertexLabel is provided, with default setting False, which can be set to True, if the user wants to associate the binary strings to the vertices as labels." 

SignaturePermutation::usage = "SignaturePermutation[p] gives the signature of permutation p."

Simple::usage = "Simple is an option value for Type."

SimpleQ::usage = "SimpleQ[g] yields True if g is a simple graph, meaning it has no multiple edges and contains no self-loops."

Small::usage = "Small is a symbol used to denote the size of the object that represents a vertex. The option VertexStyle can be set to Disk[Small] or Box[Small] either inside the graph data structure or in ShowGraph."

SmallestCyclicGroupGraph::usage = "SmallestCyclicGroupGraph returns a smallest nontrivia al graph whose automorphism group is cyclic."

Spectrum::usage = "Spectrum[g] gives the eigenvalues of graph g."

SpringEmbedding::usage = "SpringEmbedding[g] beautifies the embedding of graph g by modeling the embedding as a system of springs. SpringEmbedding[g, step, increment] can be used to refine the algorithm. The value of step tells the function for how many iterations to run the algorithm. The value of increment tells the function the distance to move the vertices at each step. The default values are 10 and 0.15 for step and increment, respectively."

StableMarriage::usage = "StableMarriage[mpref, fpref] finds the male optimal stable marriage defined by lists of permutations describing male and female preferences."

Star::usage = "Star[n] constructs a star on n vertices, which is a tree with one vertex of degree n-1."

StirlingFirst::usage = "StirlingFirst[n, k] returns a Stirling number of the first kind. This is obsolete. Use the built-in Mathematica function StirlingS1 instead."

StirlingSecond::usage = "StirlingSecond[n, k] returns a Stirling number of the second kind."

Strings::usage = "Strings[l, n] constructs all possible strings of length n from the elements of list l."

StronglyConnectedComponents::usage = "StronglyConnectedComponents[g] gives the strongly connected components of directed graph g as lists of vertices."

Strong::usage = "Strong is an option to ConnectedQ that seeks to determine if a directed graph is strongly connected."

Subsets::usage = "Subsets[l] gives all subsets of set l."

SymmetricGroup::usage = "SymmetricGroup[n] returns the symmetric group on n symbols."

SymmetricGroupIndex::usage = "SymmetricGroupIndex[n, x] returns the cycle index of the symmetric group on n symbols, expressed as a polynomial in x[1], x[2], ..., x[n]."

SymmetricQ::usage = "SymmetricQ[r] tests if a given square matrix r represents a symmetric relation. SymmetricQ[g] tests if the edges of a given graph represent a symmetric relation."

TableauClasses::usage = "TableauClasses[p] partitions the elements of permutation p into classes according to their initial columns during Young tableaux construction."

TableauQ::usage = "TableauQ[t] yields True if and only if t represents a Young tableau."

Tableaux::usage = "Tableaux[p] constructs all tableaux having a shape given by integer partition p."

TableauxToPermutation::usage = "TableauxToPermutation[t1, t2] constructs the unique permutation associated with Young tableaux t1 and t2, where both tableaux have the same shape."

TetrahedralGraph::usage = "TetrahedralGraph returns the graph corresponding to the the Tetrahedron, a Platonic solid."

Thick::usage = "Thick is a value that the option EdgeStyle can take on in the graph data structure or in ShowGraph."

ThickDashed::usage = "ThickDashed is a value that the option EdgeStyle can take on in the graph data structure or in ShowGraph."

Thin::usage = "Thin is a value that the option EdgeStyle can take on in the graph data structure or in ShowGraph."

ThinDashed::usage = "ThinDashed is a value that the option EdgeStyle can take on in the graph data structure or in ShowGraph."

ThomassenGraph::usage = "ThomassenGraph returns a hypotraceable graph, a graph G that has no Hamiltonian path but whose subgraph G-v for every vertex v has a Hamiltonian path."

ToAdjacencyLists::usage = "ToAdjacencyLists[g] constructs an adjacency list representation for graph g. It allows an option called Type that takes on values All or Simple. Type -> All is the default setting of the option, and this permits self-loops and multiple edges to be reported in the adjacency lists. Type -> Simple deletes self-loops and multiple edges from the constructed adjacency lists. ToAdjacencyLists[g, EdgeWeight] returns an adjacency list representation along with edge weights."

ToAdjacencyMatrix::usage = "ToAdjacencyMatrix[g] constructs an adjacency matrix representation for graph g. An option Type that takes on values All or Simple can be used to affect the matrix constructed. Type -> All is the default, and Type -> Simple ignores any self-loops or multiple edges g may have. ToAdjacencyMatrix[g, EdgeWeight] returns edge weights as entries of the adjacency matrix with Infinity representing missing edges."

ToCanonicalSetPartition::usage = "ToCanonicalSetPartition[sp, set] reorders sp into a canonical order with respect to set. In the canonical order, the elements of each subset of the set partition are ordered as they appear in set, and the subsets themselves are ordered by their first elements. ToCanonicalSetPartition[sp] reorders sp into canonical order, assuming that Mathematica knows the underlying order on the set for which sp is a set partition."

ToCycles::usage = "ToCycles[p] gives the cycle structure of permutation p as a list of cyclic permutations."

ToInversionVector::usage = "ToInversionVector[p] gives the inversion vector associated with permutation p."

ToOrderedPairs::usage = "ToOrderedPairs[g] constructs a list of ordered pairs representing the edges of the graph g. If g is undirected each edge is interpreted as two ordered pairs. An option called Type that takes on values Simple or All can be used to affect the constructed representation. Type -> Simple forces the removal of multiple edges and self-loops. Type -> All keeps all information and is the default option."

TopologicalSort::usage = "TopologicalSort[g] gives a permutation of the vertices of directed acyclic graph g such that an edge (i, j) implies that vertex i appears before vertex j."

ToUnorderedPairs::usage = "ToUnorderedPairs[g] constructs a list of unordered pairs representing the edges of graph g. Each edge, directed or undirected, results in a pair in which the smaller vertex appears first. An option called Type that takes on values All or Simple can be used, and All is the default value. Type -> Simple ignores multiple edges and self-loops in g."

TransitiveClosure::usage = "TransitiveClosure[g] finds the transitive closure of graph g, the supergraph of g that contains edge {x, y} if and only if there is a path from x to y."

TransitiveQ::usage = "TransitiveQ[g] yields True if graph g defines a transitive relation."

TransitiveReduction::usage = "TransitiveReduction[g] finds a smallest graph that has the same transitive closure as g."

TranslateVertices::usage = "TranslateVertices[v, {x, y}] adds the vector {x, y} to the vertex embedding location of each vertex in list v. TranslateVertices[g, {x, y}] translates the embedding of the graph g by the vector {x, y}."

TransposePartition::usage = "TransposePartition[p] reflects a partition p of k parts along the main diagonal, creating a partition with maximum part k."

TransposeTableau::usage = "TransposeTableau[t] reflects a Young tableau t along the main diagonal, creating a different tableau."

TravelingSalesman::usage = "TravelingSalesman[g] finds an optimal traveling salesman tour in graph g."

TravelingSalesmanBounds::usage = "TravelingSalesmanBounds[g] gives upper and lower bounds on a minimum cost traveling salesman tour of graph g."

Tree::usage = "Tree is an option that informs certain functions for which the user wants the output to be a tree."

TreeIsomorphismQ::usage = "TreeIsomorphismQ[t1, t2] yields True if the trees t1 and t2 are isomorphic. It yields False otherwise."

TreeQ::usage = "TreeQ[g] yields True if graph g is a tree."

TreeToCertificate::usage = "TreeToCertificate[t] returns a binary string that is a certificate for the tree t such that trees have the same certificate if and only if they are isomorphic."

TriangleInequalityQ::usage = "TriangleInequalityQ[g] yields True if the weights assigned to the edges of graph g satisfy the triangle inequality."

Turan::usage = "Turan[n, p] constructs the Turan graph, the extremal graph on n vertices that does not contain CompleteGraph[p]."

TutteGraph::usage = "TutteGraph returns the Tutte graph, the first known example of a 3-connected, 3-regular, planar graph that is non-Hamiltonian."

TwoColoring::usage = "TwoColoring[g] finds a two-coloring of graph g if g is bipartite. It returns a list of the labels 1 and 2 corresponding to the vertices. This labeling is a valid coloring if and only the graph is bipartite."

Type::usage = "Type is an option for many functions that transform graphs. Depending on the functions it is being used in, it can take on values such as Directed, Undirected, Simple, etc."

UndirectedQ::usage = "UndirectedQ[g] yields True if graph g is undirected."

Undirected::usage = "Undirected is an option to inform certain functions that the graph is undirected."

UnionSet::usage = "UnionSet[a, b, s] merges the sets containing a and b in union-find data structure s."

Uniquely3ColorableGraph::usage = "Uniquely3ColorableGraph returns a 12-vertex, triangle-free graph with chromatic number 3 that is uniquely 3-colorable."

UnitransitiveGraph::usage = "UnitransitiveGraph returns a 20-vertex, 3-unitransitive graph discovered by Coxeter, that is not isomorphic to a 4-cage or a 5-cage."

UnrankBinarySubset::usage = "UnrankBinarySubset[n, l] gives the nth subset of list l, listed in increasing order of integers corresponding to the binary representations of the subsets."

UnrankGrayCodeSubset::usage = "UnrankGrayCodeSubset[n, l] gives the nth subset of list l, listed in Gray code order."

UnrankKSetPartition::usage = "UnrankSetPartition[r, s, k] finds a k-block set partition of s with rank r. UnrankSetPartition[r, n, k] finds a k-block set partition of {1, 2,..., n} with rank r." 

UnrankKSubset::usage = "UnrankKSubset[m, k, l] gives the mth k-subset of set l, listed in lexicographic order."

UnrankPermutation::usage = "UnrankPermutation[r, l] gives the rth permutation in the lexicographic list of permutations of list l. UnrankPermutation[r, n] gives the rth permutation in the lexicographic list of permutations of {1, 2,..., n}."

UnrankRGF::usage = "UnrankRGF[r, n] returns a restricted growth function defined on the first n natural numbers whose rank is r."

UnrankSetPartition::usage = "UnrankSetPartition[r, set] finds a set partition of set with rank r. UnrankSetPartition[r, n] finds a set partition of {1, 2, ..., n} with rank r." 

UnrankSubset::usage = "UnrankSubset[n, l] gives the nth subset of list l, listed in some canonical order."

UnweightedQ::usage = "UnweightedQ[g] yields True if all edge weights are 1 and False otherwise."

UpperLeft::usage = "UpperLeft is a value that options VertexNumberPosition, VertexLabelPosition, and EdgeLabelPosition can take on in ShowGraph."

UpperRight::usage = "UpperRight is a value that options VertexNumberPosition, VertexLabelPosition, and EdgeLabelPosition can take on in ShowGraph."

V::usage = "V[g] gives the order or number of vertices of the graph g."

Value::usage = "Value is an option for the function NetworkFlow that makes the function return the value of the maximum flow."

VertexColor::usage = "VertexColor is an option that allows the user to associate colors with vertices. Black is the default color. VertexColor can be set as part of the graph data structure and it can be used in ShowGraph."

VertexColoring::usage = "VertexColoring[g] uses Brelaz's heuristic to find a good, but not necessarily minimal, vertex coloring of graph g. An option Algorithm that can take on the values Brelaz or Optimum is allowed. The setting Algorithm -> Brelaz is the default, while the setting Algorithm -> Optimum forces the algorithm to do an exhaustive search to find an optimum vertex coloring."

VertexConnectivity::usage = "VertexConnectivity[g] gives the minimum number of vertices whose deletion from graph g disconnects it. VertexConnectivity[g, Cut] gives a set of vertices of minimum size, whose removal disconnects the graph."

VertexConnectivityGraph::usage = "VertexConnectivityGraph[g] returns a directed graph that contains an edge corresponding to each vertex in g and in which edge disjoint paths correspond to vertex disjoint paths in g."

VertexCover::usage = "VertexCover[g] returns a vertex cover of the graph g. An option Algorithm that can take on values Greedy, Approximate, or Optimum is allowed. The default setting is Algorithm -> Approximate. Different algorithms are used to compute a vertex cover depending on the setting of the option Algorithm."

VertexCoverQ::usage = "VertexCoverQ[g, c] yields True if the vertices in list c define a vertex cover of graph g."

VertexLabel::usage = "VertexLabel is an option that can take on values True or False, allowing the user to set and display vertex labels. By default, there are no vertex labels. VertexLabel can be set as part of the graph data structure or in ShowGraph."

VertexLabelColor::usage = "VertexLabelColor is an option that allows the user to associate different colors to vertex labels. Black is the default color. VertexLabelColor can be set as part of the graph data structure or in ShowGraph."

VertexLabelPosition::usage = "VertexLabelPosition is an option that allows the user to place a vertex label in a certain position relative to the vertex. The default position is upper right. VertexLabelPosition can be set as part of the graph data structure or in ShowGraph."

VertexNumber::usage = "VertexNumber is an option that can take on values True or False. This can be used in ShowGraph to display or suppress vertex numbers. By default, the vertex numbers are hidden. VertexNumber can be set as part of the graph data structure or in ShowGraph."

VertexNumberColor::usage = "VertexNumberColor is an option that can be used in ShowGraph to associate different colors to vertex numbers. Black is the default color. VertexNumberColor can be set as part of the graph data structure or in ShowGraph."

VertexNumberPosition::usage = "VertexNumberPosition is an option that can be used in ShowGraph to display a vertex number in a certain position relative to the vertex. By default, vertex numbers are positioned to the lower left of vertices. VertexNumberPosition can be set as part of the graph data structure or in ShowGraph."

VertexStyle::usage = "VertexStyle is an option that allows the user to associate different sizes and shapes to vertices. A disk is the default shape. VertexStyle can be set as part of the graph data structure and it can be used in ShowGraph."

VertexWeight::usage = "VertexWeight is an option that allows the user to associate weights with vertices. 0 is the default weight. VertexWeight can be set as part of the graph data structure."

Vertices::usage = "Vertices[g] gives the embedding of graph g, that is, the coordinates of each vertex in the plane. Vertices[g, All] gives the embedding of the graph along with graphics options associated with each vertex."

WaltherGraph::usage = "WaltherGraph returns the Walther graph."

Weak::usage = "Weak is an option to ConnectedQ that seeks to determine if a directed graph is weakly connected."

WeaklyConnectedComponents::usage = "WeaklyConnectedComponents[g] gives the weakly connected components of directed graph g as lists of vertices."

WeightingFunction::usage = "WeightingFunction is an option to the functions SetEdgeWeights and SetVertexWeights and it tells the functions how to compute edge weights and vertex weights, respectively. The default value for this option is Random."

WeightRange::usage = "WeightRange is an option to the functions SetEdgeWeights and SetVertexWeights that gives the range for these weights. The default range is [0, 1] for real as well as integer weights." 

Wheel::usage = "Wheel[n] constructs a wheel on n vertices, which is the join of CompleteGraph[1] and Cycle[n-1]."

WriteGraph::usage = "WriteGraph[g, f] writes graph g to file f using an edge list representation."

Zoom::usage = "Zoom[{i, j, k, ...}] is a value that the PlotRange option can take on in ShowGraph. Setting PlotRange to this value zooms the display to contain the specified subset of vertices, i, j, k, ...."

Begin["`Private`"]
AcyclicQ[g_Graph] := SameQ[FindCycle[g],{}]

AddEdge::obsolete = "Usage of Directed as a second argument to AddEdge is obsolete."
AddEdge[g_Graph, edge:{_Integer,_Integer}, Directed] := (Message[AddEdge::obsolete]; AddEdges[g, {{edge}}])
AddEdge[g_Graph, edge:{_Integer,_Integer}] := AddEdges[g, {{edge}}]
AddEdge[g_Graph, edge:{{_Integer,_Integer}, ___?OptionQ}] := AddEdges[g, {edge}]
AddEdges[g_Graph, edgeList:{{{_Integer, _Integer},___?OptionQ}...}] := 
        Module[{ne = If[UndirectedQ[g], 
                        Map[Prepend[Rest[#], Sort[First[#]]]&, edgeList],
                        edgeList
                     ]
               },
               ChangeEdges[g, Join[Edges[g, All], ne]]
        ] 

AddEdges[g_Graph, edgeList:{{_Integer,_Integer}...}] := 
        AddEdges[g, Map[{#}&, edgeList] ] 

AddEdges[g_Graph, edge:{_Integer,_Integer}] := AddEdges[g, {edge}]
AddToEncroachingLists[k_Integer,{}] := {{k}}

AddToEncroachingLists[k_Integer,l_List] :=
	Append[l,{k}]  /; (k > First[Last[l]]) && (k < Last[Last[l]])

AddToEncroachingLists[k_Integer,l1_List] :=
	Module[{i,l=l1},
		If [k <= First[Last[l]],
			i = Ceiling[ BinarySearch[l,k,First] ];
			PrependTo[l[[i]],k],
			i = Ceiling[ BinarySearch[l,-k,(-Last[#])&] ];
			AppendTo[l[[i]],k]
		];
		l
	]
 
AddVertex[g_Graph] := AddVertices[g, 1]
AddVertex[g_Graph, p:{_?NumericQ, _?NumericQ}] := AddVertices[g, {{p}}]
AddVertices[g_Graph, n_Integer?Positive] := GraphUnion[g, EmptyGraph[n]]

AddVertices[Graph[e_List, v_List, opts___?OptionQ], 
            p:{{{_?NumericQ, _?NumericQ},___?OptionQ}...}] := Graph[e, Join[v, p], opts]

AddVertices[g_Graph, p:{{_?NumericQ, _?NumericQ}...}] := AddVertices[g, Map[{#}&, p]]

AddVertices[g_Graph, p:{_?NumericQ, _?NumericQ}] := AddVertices[g, {p}]
AllPairsShortestPath[g_Graph] := {} /; (V[g] == 0)
AllPairsShortestPath[g_Graph] :=
        Module[{p = ToAdjacencyMatrix[g, EdgeWeight, Type->Simple], m},
               m = V[g]*Ceiling[Max[Cases[Flatten[p], _Real | _Integer]]]+1;
               Zap[DP1[p /. {0 -> 0.0, x_Integer -> 1.0 x, Infinity -> m*1.0}, m]] /. m -> Infinity
        ]

DP1 = Compile[{{p, _Real, 2}, {m, _Integer}},
        Module[{np = p, k, n = Length[p]},
               Do[np = Table[If[(np[[i, k]] == 1.0*m) || (np[[k, j]] == 1.0*m), 
                                np[[i,j]], Min[np[[i,k]]+ np[[k,j]], np[[i,j]]]
                             ], {i,n},{j,n}
                       ], {k, n}];
               np
        ]
      ]
AllPairsShortestPath[g_Graph, Parent] := {} /; (V[g] == 0)
AllPairsShortestPath[g_Graph, Parent] :=
        Module[{q, p = ToAdjacencyMatrix[g,EdgeWeight,Type->Simple], n=V[g], m},
               Do[p[[i, i]] = 0, {i, n}];
               m = V[g]*Ceiling[Max[Cases[Flatten[p], _Real | _Integer]]]+1;
               q = Table[If[(p[[i, j]]===Infinity), j, i], 
                         {i, n}, {j, n}
                   ];
               p = p /. {x_Integer->1.0 x, Infinity -> m*1.0};
               {p, q} = Zap[DP2[p, q, m]];
               {p /. m -> Infinity, q} 
        ]

DP2 =Compile[{{p, _Real, 2}, {q, _Integer, 2}, {m, _Real}}, 
        Module[{np = p, nq = q, k = 0, n = Length[p]}, 
               Do[
                  Do[If[(np[[i, k]] != m*1.0) && (np[[k, j]] != m*1.0),
                        np[[i, j]] = Min[np[[i, k]] + np[[k, j]], np[[i, j]]];
                        If[(np[[i, j]] == np[[i, k]] + np[[k, j]]) && (k != j) && (i != j), 
                           nq[[i, j]] = nq[[k, j]]
                        ]
                     ], 
                     {i, n}, {j, n}
                  ], {k, n}
               ]; 
               {np, nq}
        ]
     ]
AlternatingGroup[l_List] := Select[Permutations[l], (SignaturePermutation[#]===1)&] /; (Length[l] > 0)
AlternatingGroup[n_Integer?Positive] := Select[Permutations[n], (SignaturePermutation[#]===1)&]
AlternatingGroupIndex[n_Integer?Positive, x_Symbol] := 
       Module[{p, y},
              p = SymmetricGroupIndex[n, y]; 
              (p /. Table[y[i] -> x[i], {i, n}]) + (p /. Table[y[i] -> (-1)^(i + 1)x[i], {i, n}])
       ]
AlternatingPaths[g_Graph, start_List, ME_List] := 
       Module[{MV = Table[0, {V[g]}], e = ToAdjacencyLists[g], 
               lvl = Table[Infinity, {V[g]}], cnt = 1, 
               queue = start, parent = Table[i, {i, V[g]}]},
               Scan[(MV[[#[[1]]]] = #[[2]]; MV[[#[[2]]]] = #[[1]]) &, ME]; 
               Scan[(lvl[[#]] = 0) &, start];
               While[queue != {},
                     {v, queue} = {First[queue], Rest[queue]};
                     If[EvenQ[lvl[[v]]],
                        Scan[(If[lvl[[#]] == Infinity, lvl[[#]] = lvl[[v]] + 1;
                              parent[[#]] = v; AppendTo[queue, #]]) &, e[[v]]
                        ],
                        If[MV[[v]] != 0,
                           u = MV[[v]];
                           If[lvl[[u]] == Infinity, 
                              lvl[[u]] = lvl[[v]] + 1; 
                              parent[[u]] = v; 
                              AppendTo[queue, u]
                           ]
                        ]
                     ]
               ];
               parent
       ]
AnimateGraph[g_Graph, l_List, flag_Symbol:All, opts___?OptionQ] := 
       Map[ShowGraph, 
           If[flag === One,
              Table[Highlight[g, {{ l[[ i ]] }}, opts], {i, Length[l]}],
              Table[Highlight[g, {l[[ Range[i] ]]}, opts], {i, Length[l]}]
           ]
       ]
AntiSymmetricQ[r_?SquareMatrixQ] := 
       Apply[And,
             Flatten[Table[r[[i, j]] != r[[j, i]], {i, Length[r]}, {j, i-1}], 1]
       ]

AntiSymmetricQ[g_Graph] := 
	Module[{e = Edges[RemoveSelfLoops[g]]},
		Apply[And, Map[!MemberQ[e, Reverse[#]]&, e] ]
	] /; !UndirectedQ[g]

AntiSymmetricQ[g_Graph] := M[RemoveSelfLoops[g]] == 0 
ApproximateVertexCover[g_Graph] := ApproximateVertexCover[g] /; (!SimpleQ[g] || !UndirectedQ[g])

ApproximateVertexCover[g_Graph] := 
       GreedyVertexCover[g, Apply[Join, MaximalMatching[g]]]
Arctan[{x_,y_}] := Arctan1[Chop[{x,y}]]
Arctan1[{0,0}] := 0
Arctan1[{x_,y_}] := ArcTan[x,y]

Arrows[pointPair_, mel_?NumericQ] :=
          Block[{size, triangle},
               (*size = Min[0.05, mel/3];*)
               size = 0.05;
               triangle={ {0,0}, {-size,size/2}, {-size,-size/2} };
               Polygon[TranslateVertices[
                           RotateVertices[
                               triangle, Arctan[Apply[Subtract, pointPair]]+Pi
                           ], 
                           pointPair[[2]]
                       ]
               ]
          ]
ArticulationVertices[g_Graph]  := Union[Last[FindBiconnectedComponents[g]]];

AugmentFlow[f_List, m_, p_List] :=
        Module[{i, j, pf, pb},
               Scan[({i,j} = {#[[1]], #[[2]]};
                    pf = Position[f[[i]], {j, _}];
                    pb = Position[f[[j]], {i, _}];
                    If[(pb != {}) && (f[[j, pb[[1,1]], 2]] >= m),
                       f[[j, pb[[1,1]],2]]-=m,
                       f[[i, pf[[1,1]],2]]+=m
                    ])&,
                    p
               ]
        ]
AugmentingPath[g_Graph,src_Integer,sink_Integer] :=
	Block[{l={src},lab=Table[0,{V[g]}],v,c=Edges[g,All],e=ToAdjacencyLists[g]},
		lab[[src]] = start;
		While[l != {} && (lab[[sink]]==0),
			{v,l} = {First[l],Rest[l]};
			Scan[ (If[ c[[v,#]] - flow[[v,#]] > 0 && lab[[#]] == 0,
				lab[[#]] = {v,f}; AppendTo[l,#]])&,
				e[[v]]
			];
			Scan[ (If[ flow[[#,v]] > 0 && lab[[#]] == 0,
				lab[[#]] = {v,b}; AppendTo[l,#]] )&,
				Select[Range[V[g]],(c[[#,v]] > 0)&]
			];
		];
		FindPath[lab,src,sink]
	]

Automorphisms[g_Graph] := Isomorphism[g]
BFS[g_Graph, start_Integer] :=
	Module[{e, bfi=Table[0,{V[g]}], cnt=1, queue={start},
                parent=Table[i, {i, V[g]}],lvl=Table[Infinity,{V[g]}]},
		e = ToAdjacencyLists[g]; bfi[[start]] = cnt++; lvl[[start]]=0;
		While[ queue != {},
			{v,queue} = {First[queue],Rest[queue]};
			Scan[(If[bfi[[#]] == 0,
                                 bfi[[#]]=cnt++; parent[[#]]=v; lvl[[#]]=lvl[[v]]+1; AppendTo[queue,#]
                              ])&,
                              e[[v]]
                        ];
		];
		{bfi, parent, lvl}
	] /; (1 <= start) && (start <= V[g])

BFS[g_Graph,s_Integer,t_Integer] :=
	Module[{queue={s}, parent=Table[Infinity, {i, V[g]}], e},
                If[s==t, Return[{s}]];
		e = ToAdjacencyLists[g];
                parent[[s]] = s;
		While[ queue != {},
			{v,queue} = {First[queue],Rest[queue]};
			Scan[(If[parent[[#]] == Infinity,
                                 parent[[#]] = v;
                                 AppendTo[queue,#];
                                 If[# == t, queue={};Return[] ];
                              ])&,
                             e[[v]]
			];
		];
                If[parent[[t]] == Infinity,
                   {},
                   Rest[Reverse[FixedPointList[Function[x, parent[[x]] ], t]]]
                ]
	] /; (1 <= s) && (s <= V[g]) && (1 <= t) && (t <= V[g])
				
Backtrack[space_List,partialQ_,solutionQ_,flag_:One] :=
	Module[{n=Length[space],all={},done,index, v=2, solution},
		index=Prepend[ Table[0,{n-1}],1];
		While[v > 0,
			done = False;
			While[!done && (index[[v]] < Length[space[[v]]]),
				index[[v]]++;
				done = Apply[partialQ,{Solution[space,index,v]}];
			];
			If [done, v++, index[[v--]]=0 ];
			If [v > n,
				solution = Solution[space,index,n];
				If [Apply[solutionQ,{solution}],
					If [SameQ[flag,All],
						AppendTo[all,solution],
						all = solution; v=0
					]
				];
				v--
			]
		];
		all
	]

BeforeQ[l_List,a_,b_] :=
	If [First[l]==a, True, If [First[l]==b, False, BeforeQ[Rest[l],a,b] ] ]
BellB[n_Integer] := BellB1[n]
BellB1[0] := 1
BellB1[n_]:= Block[{$RecursionLimit = Infinity}, BellB1[n] = Sum [Binomial[n-1, k]*BellB1[k], {k, 0, n-1}]]
BellmanFord[g_Graph, s_Integer?Positive] := 
         Module[{p = Table[i, {i, V[g]}], d = Table[Infinity, {V[g]}]},
                d[[s]] = 0;
                {p, d}
         ] /; EmptyQ[g] && (s <= V[g])
BF = Compile[{{n, _Integer}, {s, _Integer}, {e1, _Integer, 2}, 
              {w1, _Real, 1}, {e2, _Integer, 2}, {w2, _Real, 1}}, 
             Module[{d, dist, parent = Range[n], 
                     m = (Length[e1] + Length[e2])*Max[w1, w2] + 1}, 
                     dist = Table[m, {n}]; dist[[s]] = 0; 
                     Do[
                         Do[d = dist[[ e1[[j, 1]] ]] + w1[[j]];
                            If[dist[[ e1[[j, 2]] ]] > d, 
                               dist[[ e1[[j, 2]] ]] = d; parent[[ e1[[j, 2]] ]] = e1[[j, 1]]], 
                            {j, Length[e1]}
                         ]; 
                         Do[d = dist[[ e2[[j, 1]] ]] + w2[[j]];
                            If[dist[[ e2[[j, 2]] ]] > d, 
                               dist[[ e2[[j, 2]] ]] = d; parent[[ e2[[j, 2]] ]] = e2[[j, 1]]], 
                            {j, Length[e2]}
                         ], 
                         {i, Ceiling[n/2]}
                     ]; 
                     {parent, dist}
             ]
     ]

BellmanFord[g_Graph, s_Integer?Positive] := 
         Module[{e = Sort[Edges[g, EdgeWeight]], n = V[g], e1 = {}, w1 = {}, 
                 e2 = {}, w2 = {}, b}, 
                If[UndirectedQ[g], 
                   {e1, w1} = Transpose[e]; 
                   {e2, w2} = Transpose[Reverse[Sort[Map[{Reverse[#[[1]]], #[[2]]} &, e]]]];
                   b = Zap[BF[n, s, e1, 1.0*w1, e2, 1.0*w2]],
                   e1 = Select[e, #[[1, 1]] < #[[1, 2]] &];
                   e2 = Select[e, #[[1, 1]] > #[[1, 2]] &];
                   {e1, w1} = If[e1 != {}, Transpose[e1], {{{1, 1}}, {0.0}}]; 
                   {e2, w2} = If[e2 != {}, Transpose[e2], {{{1, 1}}, {0.0}}];
                   b = Zap[BF[n, s, e1, 1.0*w1, e2, 1.0*w2]]
                ];
                {b[[1]], 
                 Table[If[(i==b[[1, i]]) && (i != s), Infinity, b[[2, i]]], 
                       {i, Length[b[[2]]]}
                 ]}
         ] /; (s <= V[g])
BiconnectedComponents[g_Graph]:= Map[{#}&, Range[V[g]] ]/; (EmptyQ[g])
BiconnectedComponents[g_Graph] := First[FindBiconnectedComponents[g]] /; UndirectedQ[g]
BiconnectedComponents[g_Graph] := First[FindBiconnectedComponents[MakeUndirected[g]]] 

BiconnectedQ[g_Graph] := (Length[ BiconnectedComponents[g] ] == 1)
BinarySearch::error = "The input list is non-numeric."
BinarySearch[l_?(Length[#] > 0&), k_?NumericQ, f_:Identity]:= 
        With[{res = binarysearchchore[l, k, f]},
             res/; res =!= $Failed
        ]
binarysearchchore[l_, k_, f_]:=
        Module[{lo = 1, mid, hi = Length[l], el},
                    While[lo <= hi,
                        If[(el=f[l[[mid =
                                    Floor[ (lo + hi)/2 ]]]])===k,
                           Return[mid]
                        ];
            If[!NumericQ[el], (Message[BinarySearch::error]; Return[$Failed])];
                        If[el > k, hi = mid-1, lo = mid+1]
                    ];
                    Return[lo-1/2]
        ];

BinarySubsets[l_List] := Map[(l[[Flatten[Position[#, 1], 1]]])&, Strings[{0, 1}, Length[l]]]

BinarySubsets[0] := {{}}
BinarySubsets[n_Integer?Positive] := BinarySubsets[Range[n]]
BipartiteMatching[g_Graph] :=
	Module[{p,v1,v2,coloring=TwoColoring[g],n=V[g],flow},
		v1 = Flatten[Position[coloring,1]];
		v2 = Flatten[Position[coloring,2]];
		p = BipartiteMatchingFlowGraph[MakeSimple[g],v1,v2];
		Complement[
                   Map[Sort[First[#]]&, NetworkFlow[p, n+1, n+2, Edge]],
                   Map[{#,n+1}&, v1], 
                   Map[{#,n+2}&, v2]
                ]
	] /; BipartiteQ[g] && UnweightedQ[g]

BipartiteMatching[g_Graph] := First[BipartiteMatchingAndCover[g]] /; BipartiteQ[g]

BipartiteMatchingAndCover[g_Graph] := 
       Module[{ng = g, MV, UV, u , v, cover, diff, MM, WM,
               c = TwoColoring[g], OV1, OV2, V1, V2, r, ip, jp, Tbar},
              V1=OV1=Flatten[Position[c,1]]; V2=OV2=Flatten[Position[c, 2]];
              If[Length[V2] < Length[V1], 
                 {OV1, OV2} = {V1, V2} = {OV2, OV1};
                 ng = AddVertices[g, Length[V2]-Length[V1]];
                 V1 = Join[V1, Range[Length[V1]+Length[V2]+1, 2 Length[V2]]]
              ];
              MM = ToAdjacencyMatrix[ng, EdgeWeight];
              WM = Table[MM[[ V1[[i]], V2[[j]]]], 
                         {i, Length[V1]}, {j, Length[V2]}] /. Infinity -> 0;
              u = Table[Max[WM[[i]]], {i, Length[V1]}];
              v = Table[0, {Length[V2]}];
              While[True,
                    cover = Table[u[[i]] + v[[j]], {i, Length[V1]}, {j, Length[V2]}];
                    ng = ChangeEdges[ng, 
                    Map[Sort[{V1[[#[[1]]]], V2[[#[[2]]]]}]&, Position[diff = cover - WM, 0]]];
                    currentMatching = BipartiteMatching[ng];
                    If[Length[currentMatching]==Length[V1], 
                       Return[{Intersection[currentMatching, Edges[g]], 
                               Transpose[
                                  Sort[Join[Transpose[{OV1, u[[Range[Length[OV1]]]]}],
                                            Transpose[{OV2, v}]
                                       ]
                                  ]
                               ][[2]]}
                       ]
                    ];
                    MV = Apply[Union, currentMatching];
                    U = Complement[V1, MV];
                    r = ReachableVertices[ng, U, currentMatching];
                    S = Complement[r, V2]; T = Complement[r, V1];
                    Tbar = Complement[V2, T];
                    epsilon = Min[Table[ip = Position[V1, S[[i]]][[1, 1]];
                                        jp = Position[V2, Tbar[[j]]][[1, 1]];
                                        diff[[ip, jp]], 
                                        {i, Length[S]}, {j, Length[Tbar]}
                                  ]
                              ];
                    Do[ip=Position[V1, S[[i]]][[1, 1]]; 
                       u[[ip]]=u[[ip]]-epsilon, 
                       {i, Length[S]}
                    ];
                    Do[jp  = Position[V2, T[[j]]][[1, 1]]; 
                       v[[jp]] = v[[jp]]+epsilon, 
                       {j, Length[T]}]
                    ]
              ]
BipartiteMatchingFlowGraph[g_Graph, v1_List, v2_List] := 
       Module[{n = V[g], ng},
              ng = ChangeEdges[
                       SetGraphOptions[g, EdgeDirection -> True],
                       Map[If[MemberQ[v1, #[[1]]], #, Reverse[#]] &, Edges[g]]
                   ];
              AddEdges[AddVertices[ng, 2], 
                       Join[Map[{{n + 1, #}} &, v1], Map[{{#, n + 2}} &, v2]]
              ]
       ]
BipartiteQ[g_Graph] := 
        Module[{c = TwoColoring[g]}, Apply[And, Map[c[[ #[[1]] ]] != c[[ #[[2]] ]]&, Edges[g]]]]
Options[BooleanAlgebra] = {Type -> Undirected, VertexLabel->False}

BooleanAlgebra[n_Integer?Positive, opts___?OptionQ]:=
       Module[{type, label, s = Subsets[n], br},
              {type, label} = {Type, VertexLabel} /. Flatten[{opts, Options[BooleanAlgebra]}];
              br = ((Intersection[#2,#1]===#1)&&(#1!=#2))&;
              If[type === Directed,
                 MakeGraph[s, br, VertexLabel->label],
                 HasseDiagram[MakeGraph[s, br, VertexLabel->label]]
              ]
       ]
BreadthFirstTraversal[g_Graph, s_Integer?Positive] := First[BFS[g,s]] /; (s <= V[g])
BreadthFirstTraversal[g_Graph, s_Integer?Positive, t_Integer?Positive] := 
        BFS[g,s,t] /; (s <= V[g]) && (t <= V[g])

BreadthFirstTraversal[g_Graph, s_Integer?Positive, Edge] := 
        Module[{b  = BFS[g,s], v, p},
               v = InversePermutation[Cases[b[[1]], _?Positive]];
               p = b[[2]];
               Table[{p[[ v[[i]] ]], v[[i]]}, {i, 2, Length[v]}]
        ] /; (s <= V[g])

BreadthFirstTraversal[g_Graph, s_Integer?Positive, Tree] := 
        Module[{p = BFS[g,s][[2]]},
               ChangeEdges[
                   g,
                   Flatten[
                       Table[If[i!=p[[i]], {{{p[[i]],i}}}, {}], 
                             {i, Length[p]}
                       ], 1
                   ]
               ]
        ] /; (s <= V[g])
        

BreadthFirstTraversal[g_Graph, s_Integer?Positive, Level] := 
        Last[BFS[g,s]] /; (s <= V[g])
BrelazColoring[g_Graph] := BrelazColoring[MakeSimple[g]] /; !UndirectedQ[g]

BrelazColoring[g_Graph] := {} /; (V[g] == 0)

BrelazColoring[g_Graph] :=
        Module[{cd = color = Table[0, {V[g]}], m = 0, p, nc,
                e = ToAdjacencyLists[g]},
               While[ m >= 0,
                      p = Position[cd, m][[1, 1]];
                      nc = Append[color[[ e[[p]] ]], 0];
                      color[[ p ]] = Min[Complement[ Range[Max[nc] + 1], nc]];
                      cd[[ p ]] = -2 V[g];
                      Scan[(cd[[ # ]]++)&, e[[ p ]] ];
                      m = Max[cd]
               ];
               color
        ]
Bridges[g_Graph] := Select[BiconnectedComponents[g],(Length[#] == 2)&]

Options[ButterflyGraph] = {VertexLabel->False}

ButterflyGraph[n_Integer?Positive, opts___?OptionQ] := 
        Module[{v = Map[Flatten, CartesianProduct[Strings[{0, 1}, n], Range[0, n]]], label},
               label = VertexLabel /. Flatten[{opts, Options[ButterflyGraph]}];
               RankedEmbedding[ 
                   MakeUndirected[
                       MakeGraph[v, 
                                 (#1[[n+1]]+1 == #2[[n+1]]) && 
                                 (#1[[Range[#2[[n+1]]-1]]] == #2[[Range[#2[[n+1]]-1]]]) && 
                                 (#1[[Range[#2[[n+1]]+1, n]]] == #2[[Range[#2[[n+1]]+1, n]]])&,
                                 VertexLabel -> label
                       ]
                   ],
                   Flatten[Position[v, {__, 0}]]
               ]
        ]
CageGraph[g_Integer?Positive] := CageGraph[3,g]

CageGraph[3,3] := CompleteGraph[4]
CageGraph[3,4] := CompleteGraph[3,3]
CageGraph[3,5] := PetersenGraph
CageGraph[3,6] := HeawoodGraph
CageGraph[3,7] := McGeeGraph
CageGraph[3,8] := LeviGraph

CageGraph[3,10] := 
        Module[{p,i},
               p = GraphUnion[CirculantGraph[20,{6}],Cycle[50]];
               AddEdges[p,
                        Join[Table[{7+5i,32+5i},{i,3,7}],
                             Table[{20+10i,10i+29},{i,4}],
                             Table[{15+10i,10i+24},{i,4}],
                             {{21,20},{1,23},{70,29},{24,65}},
                             Flatten[
                                     Table[{2i+j,21+5i+2j},{j,0,1},{i,9}], 1
                             ]
                         ]
                ]/. Graph[l_List, v_List]:>
                    Graph[l, 
                          Join[Table[{{Cos[#], Sin[#]}}&[2.Pi(i+1/2)/20],
                                     {i,20}
                               ],
                               2 Table[{{Cos[#],Sin[#]}}&[2.Pi (i+1/2)/50],
                                       {i,50}
                                 ]
                          ]
                    ]
        ]

CageGraph[4, 3] := CompleteGraph[5]
CageGraph[4, 4] := CirculantGraph[8,{1,3}]
CageGraph[4, 5] := RobertsonGraph
CageGraph[4,6] := MakeUndirected[
                      MakeGraph[Range[26],
                            Mod[#1-#2, 26] == 1 ||
                                (-1)^#1Mod[#1-#2, 26] == 11 ||
                                (-1)^#1Mod[#1-#2, 26] == 7&,
                            Type -> Directed
                      ]
                  ]

CageGraph[5,3] := CompleteGraph[6]
CageGraph[5,4] := CirculantGraph[10,{1,3,5}]
CageGraph[5,5] := MakeSimple[
                      AddEdges[
                          MakeGraph[Range[30],
                                    (Mod[#1-#2,30]==1 ||
                                    (Mod[#1-#2,30]==15 && Mod[#1,3]==0) ||
                                    (Mod[#1-#2,30]==4 && Mod[#1,2]==0))&
                          ],
                          {{1,9},{1,13},{1,19},{2,16},{3,11},{3,18},{3,25},
                           {4,20},{5,17},{5,23},{5,27},{6,21},{7,15},{7,19},
                           {7,25},{8,22},{9,17},{9,24},{10,26},{11,23},{11,29},
                           {12,27},{13,21},{13,25},{14,28},{15,23},{15,30},
                           {17,29}, {19,27},{21,29}}
                      ]
                  ]
CageGraph[5,6] := MakeUndirected[
                     MakeGraph[Range[42],
                            (Mod[#1-#2,42]==1 ||
                            (MemberQ[{7,27,31},Mod[#1-#2,42]] && Mod[#1,2]==1)||
                            (MemberQ[{11,15,35},Mod[#1-#2,42]]&&Mod[#1,2]==0))&
                     ]
                  ]
CalculateForce[u_Integer,g_Graph,em_List] :=
	Module[{n=V[g],stc=0.25,gr=10.0,e=ToAdjacencyMatrix[g],
                f={0.0,0.0},spl=1.0,v,dsquared},
		Do [
			dsquared = Max[0.001, Apply[Plus,(em[[u]]-em[[v]])^2] ];
			f += (1-e[[u,v]]) (gr/dsquared) (em[[u]]-em[[v]])
				- e[[u,v]] stc Log[dsquared/spl] (em[[u]]-em[[v]]),
			{v,n}
		];
		f
	]

CartesianProduct[a_List, b_List] := Flatten[Outer[List, a, b, 1, 1], 1]
ChangeEdges[Graph[_List, v_List, dopts___?OptionQ], 
            newE:{{{_Integer, _Integer},___?OptionQ}...}] := 
        Graph[newE, v, dopts] /; (Max[Map[First, newE]] <= Length[v]) && (Min[Map[First, newE]] >= 1)

ChangeEdges[Graph[_, v_, dopts___], newE:{{_Integer, _Integer}...}] :=
        Graph[Map[{#}&, newE], v, dopts] /; (Max[newE] <= Length[v]) && (Min[newE] >= 1)
ChangeVertices[Graph[e_List, v_List, dopts___?OptionQ], 
               newV:{{{_?NumericQ, _?NumericQ},___?OptionQ}...}]  := 
        Graph[e, newV, dopts] /; (Length[newV] >= Length[v])

ChangeVertices[Graph[e_List, v_List, dopts___?OptionQ], 
               newV:{{_?NumericQ, _?NumericQ}...}]  := 
        Graph[e, 
              Table[{newV[[i]], Apply[Sequence, Rest[v[[i]]]]},{i, Length[newV]}], 
              dopts
        ] /; (Length[newV] >= Length[v])
ChooseShortestPathAlgorithm[g_Graph, s_Integer, algorithm_Symbol] := 
         If[algorithm === Automatic,
            If[(MemberQ[Negative[GetEdgeWeights[g]],True]) || (M[g] <= 10 V[g]),
               First[BellmanFord[g, s]], First[Dijkstra[g, s]]],
            If[algorithm === BellmanFord, First[BellmanFord[g, s]], First[Dijkstra[g, s]]]
         ]
ChromaticDense[g_Graph, z_] := ChromaticPolynomial[g,z] /; CompleteQ[g]
ChromaticDense[g_Graph, z_] :=
        Block[{el = Edges[GraphComplement[g]]}, 
              ChromaticDense[AddEdges[g,{{First[el]}}], z] + 
              ChromaticDense[MakeSimple[Contract[g,First[el]]], z]
        ]
ChromaticNumber[g_Graph] := 0 /; (V[g] == 0)
ChromaticNumber[g_Graph] := 1 /; EmptyQ[MakeSimple[g]]
ChromaticNumber[g_Graph] := 2 /; BipartiteQ[MakeSimple[g]]
ChromaticNumber[g_Graph] := V[g] /; CompleteQ[MakeSimple[g]]
ChromaticNumber[g_Graph] := Max[MinimumVertexColoring[g]]
ChromaticPolynomial[g_Graph,z_]:= ChromaticPolynomial[MakeSimple[g], z]/; !SimpleQ[g]
ChromaticPolynomial[g_Graph,z_]:=0/;IdenticalQ[g,CompleteGraph[0]]
ChromaticPolynomial[g_Graph,z_] := Module[{i}, Product[z-i, {i,0,V[g]-1}] ] /; CompleteQ[g]
ChromaticPolynomial[g_Graph,z_]:=z ( z - 1 ) ^ (V[g]-1) /; TreeQ[g]
ChromaticPolynomial[g_Graph,z_] := If[M[g]>Binomial[V[g],2]/2, ChromaticDense[g,z], ChromaticSparse[g,z]]
ChromaticSparse[g_Graph,z_] := z^V[g] /; EmptyQ[g]
ChromaticSparse[g_Graph,z_] :=
	Block[{e = Edges[g]},
		ChromaticSparse[ DeleteEdges[g,{First[e]}], z] -
		ChromaticSparse[MakeSimple[Contract[g,First[e]]], z]
	]
ChvatalGraph := 
 Graph[{{{6, 7}}, {{7, 8}}, {{8, 9}}, {{9, 10}}, {{6, 10}}, {{5, 6}},
  {{5, 9}}, {{3, 9}}, {{3, 7}}, {{1, 7}}, {{1, 10}}, {{4, 10}}, {{4, 8}},
  {{2, 8}}, {{2, 6}}, {{2, 11}}, {{5, 11}}, {{5, 12}}, {{3, 12}}, {{1, 11}},
  {{1, 12}}, {{4, 12}}, {{4, 11}}, {{2, 3}}},
 {{{-0.9510565162951535, 0.3090169943749475}},
  {{-0.5877852522924732, -0.8090169943749473}},
  {{0.5877852522924729, -0.8090169943749476}},
  {{0.9510565162951536, 0.3090169943749472}}, {{3.061515884555943*^-16, 1.}},
  {{-1.902113032590307, 0.618033988749895}},
  {{-1.1755705045849465, -1.6180339887498947}},
  {{1.1755705045849458, -1.6180339887498951}},
  {{1.9021130325903073, 0.6180339887498943}}, {{6.123031769111886*^-16, 2.}},
  {{-0.3, 0}}, {{0.3, 0}}}]
CirculantGraph[n_Integer?Positive, l_Integer] := CirculantGraph[n, {l}]

CirculantGraph[n_Integer?Positive, l:{_Integer...}] :=
        Graph[Union[
                  Flatten[Table[Map[{Sort[{i, Mod[i+#, n]}]+1}&, l], {i,0,n-1}], 1],
                  Flatten[Table[Map[{Sort[{i, Mod[i-#, n]}]+1}&, l], {i,0,n-1}], 1]
              ],
	      CircularEmbedding[n] 
        ]
CircularEmbedding[0] := {}

CircularEmbedding[n_Integer] :=
	Module[{i,x = N[2 Pi / n]},
		Chop[ Table[ N[{{ (Cos[x i]), (Sin[x i]) }}], {i,n} ] ]
	]

CircularEmbedding[g_Graph] := 
        ChangeVertices[g, CircularEmbedding[ V[g] ] ]
CircularVertices[0] := {}

CircularVertices[n_Integer] :=
	Module[{i,x = N[2 Pi / n]},
		Chop[ Table[ N[{ (Cos[x i]), (Sin[x i]) }], {i,n} ] ]
	]

CircularVertices[g_Graph] := ChangeVertices[g, CircularVertices[ V[g] ] ]
CliqueQ[g_Graph, clique_List] :=
	IdenticalQ[CompleteGraph[Length[clique]], 
                      InduceSubgraph[MakeSimple[g],clique] 
        ]
CoarserSetPartitionQ[a_?SetPartitionQ, b_?SetPartitionQ] := 
        Apply[And, Map[Apply[Or, Map[Function[x, (Intersection[x, #] === #)], b] ]&, a ]]

CodeToLabeledTree[l_List] :=
	Module[{m=Range[Length[l]+2],x,i},
		FromUnorderedPairs[
			Append[
				Table[
					x = Min[Complement[m,Drop[l,i-1]]];
					m = Complement[m,{x}];
					Sort[{x,l[[i]]}],
					{i,Length[l]}
				],
				Sort[m]
			]
		]
	] /; (Complement[l, Range[Length[l]+2]] == {})
Cofactor[m_?MatrixQ, {i_Integer?Positive , j_Integer?Positive}] :=
	(-1)^(i+j) * Det[ Drop[ Transpose[ Drop[Transpose[m],{j,j}]], {i,i}]] /; (i <= Length[m]) && 
                                                                                 (j <= Length[m[[1]]]) 
CompleteBinaryTree[n_Integer?Positive] := 
       RootedEmbedding[Graph[Join[Table[{{i, 2i}}, {i, Floor[n/2]}], 
                                  Table[{{i, 2i + 1}}, {i, Ceiling[n/2 - 1]}]],
                              CircularEmbedding[n]
                       ], 1
       ]
Options[CompleteGraph] = {Type -> Undirected};

CompleteGraph[n_Integer, opts___?OptionQ] := 
        Module[{type = Type /. Flatten[{opts, Options[CompleteGraph]}]},
              If[type === Undirected, CG[n], CDG[n]]
        ] /; (n >= 0)

CG[0] := Graph[{},{}]
CG[1] := Graph[{},{{{0,0}}}]

CG[n_Integer?Positive] := 
        Graph[
                 Flatten[
                         Table[{{i, j}}, {i, n-1}, {j, i+1, n}], 1
                 ],
                 CircularEmbedding[n]
        ]

CDG[0]  := Graph[{},{}, EdgeDirection -> True]
CDG[1] := Graph[{},{{{0,0}}}, EdgeDirection -> True]
CDG[n_Integer?Positive] := 
        Graph[Map[{#}&, 
                     Double[Flatten[
                                Table[{i, j}, {i, n-1}, {j, i+1, n}],
                                1
                            ]
                     ]
                 ],
                 CircularEmbedding[n],
                 EdgeDirection -> True
        ]


CompleteGraph[l__] :=
        CompleteKPartiteGraph[l] /; TrueQ[Apply[And, Map[Positive,List[l]]]] && (Length[List[l]]>1)
Options[CompleteKPartiteGraph] = {Type -> Undirected};

CompleteKPartiteGraph[l__, opts___?OptionQ] := 
        Module[{type = Type /. Flatten[{opts, Options[CompleteKPartiteGraph]}]},
              If[type === Undirected, 
                 CKPG[l], 
                 SetGraphOptions[CKPG[l], EdgeDirection -> True]
              ]
        ] /; TrueQ[Apply[And, Map[Positive,List[l]]]] && (Length[List[l]] > 0)

CKPG[l__] :=
        Module[{ll=List[l],t,i,x,row,stages=Length[List[l]]},
                t = FoldList[Plus,0,ll];
                AddEdges[Graph[{},
                               Apply[Join,
                                     Table[Table[{{x,i-1+(1-ll[[x]])/2}}//N, 
                                                 {i,ll[[x]]}], 
                                           {x,stages}]
                               ]
                         ],
                         Flatten[
                            Table[
                                CartesianProduct[Range[t[[i-1]]+1, t[[i]]], 
                                                 Range[t[[i]]+1, t[[stages+1]]]],
                                {i, 2, stages}
                            ], 1
                         ]
                ]
        ] 
CompleteKaryTree[n_Integer?Positive, k_Integer?Positive]:=
         RootedEmbedding[Graph[
                            Join[
                               Flatten[
                                  Table[
                                     Table[{{i, j}}, {j,  k i-(k-2), k i+1}], 
                                     {i, Floor[(n-2)/k]} 
                                  ], 1
                               ], 
                               Table[{{Floor[(n-2)/k]+1, j}}, 
                                     {j, k Floor[(n-2)/k]+2 , n}
                               ]
                            ], 
                            CircularEmbedding[n]
                         ], 1
         ]
CompleteQ[g_Graph] := 
        Block[{n = V[g], m = M[g]}, 
              (SimpleQ[g] && ((UndirectedQ[g] && (m == n(n-1)/2)) || (!UndirectedQ[g] && (m == n(n-1))))) 
        ]
Compositions[n_Integer,k_Integer] :=
	Map[
		(Map[(#[[2]]-#[[1]]-1)&, Partition[Join[{0},#,{n+k}],2,1] ])&,
		KSubsets[Range[n+k-1],k-1]
	]
ConnectedComponents[g_Graph] :=
        Block[{untraversed=Range[V[g]], visit, comps={}, e=ToAdjacencyLists[g],
              parent=Table[0,{V[g]}], cnt=1, $RecursionLimit = Infinity, start},
              While[untraversed != {},
                    visit = {}; edges = {};
                    start = First[untraversed];
                    parent[[start]] = start;
                    DFS[start];
                    AppendTo[comps,visit];
                    untraversed = Complement[untraversed,visit]        
              ];
              ToCanonicalSetPartition[comps]
        ] /; UndirectedQ[g]

ConnectedComponents[g_Graph] := ConnectedComponents[MakeUndirected[g]]
ConnectedQ[g_Graph] := True /; (V[g] == 0)
ConnectedQ[g_Graph, _] := True /; (V[g] == 0)

ConnectedQ[g_Graph] := Length[DepthFirstTraversal[g,1]]==V[g] /; UndirectedQ[g]
ConnectedQ[g_Graph] := Length[DepthFirstTraversal[MakeUndirected[g],1]]==V[g]

ConnectedQ[g_Graph, Weak] := 
        Length[ WeaklyConnectedComponents[g] ] == 1 /; !UndirectedQ[g]
 
ConnectedQ[g_Graph, Strong] := 
        Length[ StronglyConnectedComponents[g] ] == 1 /; !UndirectedQ[g]
ConstructTableau[p_List] := Module[{T}, T[a_, b_] := InsertIntoTableau[b, a]; Fold[T, {}, p]]
Contract[g_Graph, l_List]  := 
       Module[{v = Vertices[g, All], t = Table[0, {V[g]}], 
               cnt = 0, last = V[g] - Length[l] + 1}, 
              Do[If[MemberQ[l, k], cnt++; t[[k]] = last, t[[k]] = k - cnt], {k, V[g]}]; 
              Graph[
                 DeleteCases[Edges[g, All] /. {{x_Integer, y_Integer}, opts___?OptionQ} 
                                           :> {Sort[{t[[x]], t[[y]]}], opts}, {{last, last}, opts___?OptionQ}
                 ],
                 Append[v[[Complement[Range[Length[v]], l]]],
                       {Apply[Plus, Map[First, v[[l]]]]/Length[l]}
                 ],
                 Apply[Sequence, GraphOptions[g]]
              ]
       ]
Convert[l_List] := 
	Module[{ch,num,edge={},i=1},
		While[i <= Length[l],
			If[ DigitQ[ l[[i]] ], 
				num = 0;
				While[ ((i <= Length[l]) && (DigitQ[l[[i]]])),
					num = 10 num + Toascii[l[[i++]]] - Toascii["0"]
				];
				AppendTo[edge,num],
				i++
			];
		];
		edge
	]
CostOfPath[g_Graph, p_List] :=
        Block[{w = GetEdgeWeights[g],
               pos = Map[Position[Edges[g], #]&,
                         Map[Sort, Partition[p, 2, 1]]
                     ]
              },
              If[MemberQ[pos, {}], 
                 Infinity,
                 Apply[Plus, w[[ Map[#[[1, 1]]&, pos] ]]
                 ]
              ]
        ]
CoxeterGraph := 
 Graph[{{{1, 2}}, {{1, 3}}, {{1, 8}}, {{2, 5}}, {{2, 14}}, {{3, 4}}, {{3, 9}}, 
  {{4, 7}}, {{4, 10}}, {{5, 6}}, {{5, 13}}, {{6, 7}}, {{6, 12}}, {{7, 11}}, 
  {{8, 20}}, {{8, 25}}, {{9, 21}}, {{9, 24}}, {{10, 15}}, {{10, 23}}, 
  {{11, 16}}, {{11, 22}}, {{12, 17}}, {{12, 28}}, {{13, 18}}, {{13, 27}}, 
  {{14, 19}}, {{14, 26}}, {{15, 18}}, {{15, 19}}, {{16, 19}}, {{16, 20}}, 
  {{17, 20}}, {{17, 21}}, {{18, 21}}, {{22, 24}}, {{22, 27}}, {{23, 25}}, 
  {{23, 28}}, {{24, 26}}, {{25, 27}}, {{26, 28}}}, 
 {{{0.412, 0.984}}, {{0.494, 0.984}}, {{0.366, 0.926}}, {{0.388, 0.862}}, 
  {{0.546, 0.926}}, {{0.518, 0.86}}, {{0.458, 0.818}}, {{0.152, 0.684}}, 
  {{0.264, 0.682}}, {{0.354, 0.68}}, {{0.458, 0.67}}, {{0.554, 0.672}}, 
  {{0.658, 0.668}}, {{0.774, 0.692}}, {{0.164, 0.45}}, {{0.228, 0.448}}, 
  {{0.274, 0.39}}, {{0.242, 0.33}}, {{0.194, 0.278}}, {{0.146, 0.328}}, 
  {{0.102, 0.39}}, {{0.668, 0.472}}, {{0.638, 0.416}}, {{0.656, 0.334}}, 
  {{0.714, 0.27}}, {{0.798, 0.326}}, {{0.83, 0.408}}, {{0.754, 0.466}}}]
CubeConnectedCycle[d_Integer] := 
        Module[{g = Hypercube[d], al, n, v }, 
               al = ToAdjacencyLists[g]; 
               n = V[g]; 
               v = Vertices[g]; 
               InduceSubgraph[
                   AddEdges[
                       AddEdges[
                           AddVertices[g, 
                               Flatten[Table[Map[(.3 #+.7 v[[i]])&,  
                                                 v[[al[[i]]]]
                                             ] , {i, n}
                                       ], 1
                               ]
                           ], 
                           Flatten[
                               Table[Append[
                                         Partition[
                                              Range[n+(i-1)d+1,n+d i],2,1
                                         ],
                                         {n + (i - 1)d + 1, n + d i}
                                     ], 
                                     {i, n}
                               ], 1
                           ]
                       ], 
                       Union[
                           Flatten[
                               Table[
                                   MapIndexed[
                                       Sort[{n+d(i-1)+#2[[1]], 
                                             n+d(#1-1)+
                                             Position[al[[#1]],i][[1,1]]
                                            }
                                       ]&, 
                                       al[[i]] 
                                   ], 
                                   {i, n}
                               ], 1
                           ]
                       ]
                   ], 
                   Range[n+1, n(d+1)]
               ]
        ]
CubicalGraph := 
 Graph[{{{1, 2}}, {{2, 3}}, {{3, 4}}, {{1, 4}}, {{5, 6}}, {{6, 7}}, {{7, 8}}, 
  {{5, 8}}, {{1, 5}}, {{2, 6}}, {{3, 7}}, {{4, 8}}}, 
 {{{0, 1.}}, {{-1., 0}}, {{0, -1.}}, {{1., 0}}, {{0, 2.}}, {{-2., 0}}, 
  {{0, -2.}}, {{2., 0}}}]
Options[Cycle] = {Type -> Undirected};

Cycle[n_Integer, opts___?OptionQ] := 
        Module[{type = Type /. Flatten[{opts, Options[Cycle]}],
                e = Table[{{i, i+1}}, {i, n-1}], c = CircularEmbedding[n]},
               If[type === Undirected,
                  Graph[Append[e, {{1, n}}], c],
                  Graph[Append[e, {{n, 1}}], c, EdgeDirection -> True]
               ]
        ] /; n>=2
CycleIndex[g_List, x_Symbol] := 
        Expand[Apply[Plus, Map[CycleStructure[#, x]&, g]]/Length[g]] /; (Length[g] > 0)
CycleStructure[p_?PermutationQ, x_Symbol] := Apply[Times, Map[x[Length[#]]&, ToCycles[p]]]
CyclicGroup[0] := {{}}
CyclicGroup[n_Integer] := Table[RotateRight[Range[n], i], {i, 0, n-1}]
CyclicGroupIndex[n_Integer?Positive, x_Symbol] :=
        Expand[Apply[Plus, Map[x[#]^(n/#) EulerPhi[#]&, Divisors[n]] ]/n ]
DFS[v_Integer] :=
	( AppendTo[visit,v];
	  Scan[ (If[parent[[#]]==0, AppendTo[edges,{v,#}]; parent[[#]] = v; DFS[#]])&, e[[v]] ])

DValues[0, m_Integer?Positive] := 1
DValues[t_Integer?Positive, m_Integer?Positive] :=
        Block[{$RecursionLimit = Infinity},
              DValues[t, m] = DValues[t - 1, m + 1] + m DValues[t - 1, m]
        ]
Options[DeBruijnGraph] = {VertexLabel->False}

DeBruijnGraph[m_Integer?Positive, n_Integer?Positive] := 
        Block[{alph, states, s, i, j},
              alph = Table[i, {i, 0, m-1}];
              states = Strings[alph, n];
              s = Length[states];
              Graph[
                       Flatten[ 
                               Table[
                                     Table[{{i, 
                                           Position[states, 
                                                    Append[Rest[ states[[i]] ], 
                                                           alph[[j]] 
                                                    ]
                                           ][[1, 1]]
                                           }},
                                           {j, Length[alph]}
                                     ],
                                     {i, s}
                               ], 1
                       ],
                       CircularEmbedding[s],
                       EdgeDirection->True
              ]
        ] 

DeBruijnGraph[alph_List, n_Integer?Positive, opts___?OptionQ] := 
        Module[{label, nalpha = Union[alph]},
               label = VertexLabel /. Flatten[{opts, Options[DeBruijnGraph]}];
               If[label === True || label === On,
                  SetVertexLabels[
                     DeBruijnGraph[Length[nalpha], n], 
                     Map[Apply[StringJoin, Map[ToString, #]] &, Strings[nalpha, n]]
                  ],
                  DeBruijnGraph[Length[nalpha], n]
               ]
        ] /; (alph != {})
DeBruijnSequence[{}, n_Integer?Positive] := {}
DeBruijnSequence[alph_List, 1] := Union[alph]
DeBruijnSequence[alph_List, n_Integer?Positive] := 
              Rest[Strings[Union[alph], n-1]
                   [[ EulerianCycle[DeBruijnGraph[Union[alph], n-1]], 1]]
              ] 
DegreeSequence[g_Graph] := Reverse[ Sort[ Degrees[g] ] ]
Degrees[g_Graph] := Map[Length, ToAdjacencyLists[g]]

DegreesOf2Neighborhood[g_Graph, v_Integer?Positive] := 
        Module[{al = ToAdjacencyLists[g], degrees = Degrees[g]},
               Sort[degrees[[ Neighborhood[al, v, 2] ]]]
        ]
DeleteCycle::obsolete = "Usage of Directed as a second argument to DeleteCycle is obsolete."
DeleteCycle[g_Graph, c_List, Directed] := (Message[DeleteCycle::obsolete]; DeleteCycle[g, c])
DeleteCycle[g_Graph, {}] := g
DeleteCycle[g_Graph, c_List] :=
        Module[{e = If[UndirectedQ[g],
                       Map[Sort, Partition[c, 2, 1] ],
                       Partition[c, 2, 1]
                    ]
               },
               If[Complement[e, Edges[g]] == {}, DeleteEdges[g, e], g]
        ] /; (Last[c] == First[c])
DeleteEdge::obsolete = "Usage of Directed as a second argument to DeleteEdge is obsolete."
DeleteEdge[g_Graph, ne:{_Integer, _Integer}] := DeleteEdges[g, {ne}]
DeleteEdge[g_Graph, ne:{_Integer, _Integer}, All] := DeleteEdges[g, {ne}, All]
DeleteEdge[g_Graph, ne:{_Integer, _Integer}, Directed] := (Message[DeleteEdge::obsolete]; DeleteEdges[g, {ne}])
DeleteEdges[g_Graph, ne:{_Integer, _Integer}, All] := DeleteEdges[g, {ne}, All]

DeleteEdges[g_Graph, ne:{{_Integer, _Integer}...}, All] :=
        Module[{nne},
               nne = If[UndirectedQ[g], Map[Sort, ne ], ne];
               ChangeEdges[g, 
                           Select[Edges[g, All], 
                                  (!MemberQ[nne, First[#]])& 
                           ]
               ]
        ]
DeleteEdges[g_Graph, ne:{_Integer, _Integer}] := DeleteEdges[g, {ne}]

DeleteEdges[g_Graph, ne:{{_Integer, _Integer}...}] :=
        Module[{el = Edges[g, All], nne},
               nne = If[UndirectedQ[g], Map[Sort, ne ], ne];
               ChangeEdges[g, 
                           DeleteCases[
                               Table[If[(p = Position[ nne, el[[i,1]] ]) != {},
                                        nne = MapAt[Infinity&, nne, p[[1]] ];
                                        {},
                                        el[[ i ]]
                                     ],
                                     {i, M[g]}
                               ],
                               {} 
                           ] 
               ] 
        ]
DeleteFromTableau[t1_?TableauQ,r_Integer]:=
	Module [{t=t1, col, row, item=Last[t1[[r]]]},
		col = Length[t[[r]]];
		If[col == 1, t = Drop[t,-1], t[[r]] = Drop[t[[r]],-1]];
		Do [
			While [t[[row,col]]<=item && Length[t[[row]]]>col, col++];
			If [item < t[[row,col]], col--];
			{item,t[[row,col]]} = {t[[row,col]],item},
			{row,r-1,1,-1}
		];
		t
	]
DeleteVertex[g_Graph,v_Integer] := InduceSubgraph[g, Complement[Range[V[g]],{v}]]
DeleteVertices[g_Graph,vl_List] := InduceSubgraph[g, Complement[Range[V[g]],vl]]
DepthFirstTraversal[g_Graph, start_Integer, flag_:Vertex] :=
	Block[{visit={},e=ToAdjacencyLists[g],edges={}, parent=Table[0,{V[g]}], cnt=1, 
               $RecursionLimit = Infinity},
              parent[[start]] = start;
	      DFS[start];
	      Switch[flag, Edge, edges, 
                           Tree, ChangeEdges[g, edges],
                           Vertex, visit
              ]
	] /; (1 <= start) && (start <= V[g])
DerangementQ[p_?PermutationQ] := !(Apply[ Or, Map[( # === p[[#]] )&, Range[Length[p]]] ])
Derangements[0] := { {} }
Derangements[n_Integer] := Derangements[Range[n]]
Derangements[p_?PermutationQ] := Select[ Permutations[p], DerangementQ ]
Diameter[g_Graph] := Max[ Eccentricity[g] ]
DihedralGroup[0] := {{}}
DihedralGroup[1] := {{1}}
DihedralGroup[2] := {{1, 2}, {2, 1}}
DihedralGroup[n_Integer?Positive] := Module[{c = CyclicGroup[n]}, Join[c, Map[Reverse, c]]]
DihedralGroupIndex[n_Integer?Positive , x_Symbol] :=
        Expand[Simplify[CyclicGroupIndex[n, x]/2 + 
                        If[EvenQ[n], 
                           (x[2]^(n/2) + x[1]^2x[2]^(n/2-1))/4,
                           (x[1]x[2]^((n-1)/2))/2
                        ]
               ]
        ]
Dijkstra[al_List, start_Integer] :=
	Module[{dist = Table[Infinity,{i, Length[al]}], parent = Table[i, {i, Length[al]}],
                untraversed = Range[Length[al]], m, v},
               dist[[start]] = 0;
               While[untraversed != {},
                     m = Infinity;
                     Scan[(If[dist[[#]]<=m, v=#;m=dist[[#]]])&, untraversed];
                     untraversed = Complement[untraversed, {v}];
                     n = Table[{al[[v, i, 1]],  m + al[[v, i, 2]]}, {i, Length[ al[[v]] ]}];
                     Scan[If[dist[[ #[[1]] ]] > #[[2]], dist[[ #[[1]] ]] = #[[2]]; parent[[#[[1]]]] = v]&, n];
               ];
               {parent, dist}
        ]

Dijkstra[g_Graph, start_Integer] := Dijkstra[ToAdjacencyLists[g, EdgeWeight], start]

Dijkstra[g_Graph, start_List] :=
        Module[{al = ToAdjacencyLists[g, EdgeWeight]}, 
               Map[Dijkstra[ToAdjacencyLists[g, EdgeWeight], #]&, start]
        ]
DilateVertices[v:{{{_?NumericQ, _?NumericQ},___?OptionQ}...}, d_] := 
       Module[{p = Map[First, v], np},
              np = DilateVertices[p, d];
              Table[{np[[i]], Apply[Sequence, Rest[v[[i]]]]}, {i, Length[np]}]
       ]

DilateVertices[v:{{_?NumericQ, _?NumericQ}...}, d_] := Map[(d * #)&, v]
DilateVertices[g_Graph, d_] := ChangeVertices[g, DilateVertices[Vertices[g, All], d]]
DilateVertices[g_Graph, s_List, t_] :=
       Module[{v = Vertices[g, All]},
              ChangeVertices[g, v[[s]] = DilateVertices[v[[s]], t]; v]
       ]
DilworthGraph[g_Graph] :=
	FromUnorderedPairs[
		Map[
			(#+{0,V[g]})&,
			ToOrderedPairs[RemoveSelfLoops[TransitiveReduction[g]]]
		]
	]
Distance[{p1_List, p2_List}] := 
        Distance[ {p1, p2}, LNorm[2] ]

Distance[{p1_List, p2_List}, Euclidean] := 
        Distance[ {p1, p2}, LNorm[2] ]

Distance[{p1:{(_Integer|_Real), (_Integer|_Real)}, 
          p2:{(_Integer|_Real),(_Integer|_Real)}}, LNorm[Infinity]] := 
        N[Max[ Abs[p1[[1]] - p2[[1]] ], Abs[p1[[2]] - p2[[2]] ] ] ]

Distance[{p1:{(_Integer|_Real), (_Integer|_Real)}, 
          p2:{(_Integer|_Real),(_Integer|_Real)}},LNorm[x_Integer?Positive]] := 
        N[(Abs[p1[[1]] - p2[[1]] ]^x + Abs[p1[[2]] - p2[[2]] ]^x)^(1/x)]
Distances[g_Graph, v_Integer?Positive] := Sort[BreadthFirstTraversal[g, v, Level]]
DistinctPermutations[s_List] := Permutations[s] /; (Length[s] <= 1)

DistinctPermutations[s_List] :=
	Module[{freq,alph=Union[s],n=Length[s]},
		freq = Map[ (Count[s,#])&, alph];
		Map[
			(alph[[#]])&,
			Backtrack[
				Table[Range[Length[alph]],{n}],
				(Count[#,Last[#]] <= freq[[Last[#]]])&,
				(Count[#,Last[#]] <= freq[[Last[#]]])&,
				All
			]
		]
	]
 
Distribution[l_List] := Distribution[l, Union[l]]
Distribution[l_List, set_List] := Map[(Count[l,#])&, set]
DodecahedralGraph :=
 Graph[{{{1, 2}}, {{1, 5}}, {{1, 6}}, {{2, 3}}, {{2, 7}}, {{3, 4}}, {{3, 8}}, 
  {{4, 5}}, {{4, 9}}, {{5, 10}}, {{6, 11}}, {{6, 12}}, {{7, 11}}, {{7, 15}}, 
  {{8, 14}}, {{8, 15}}, {{9, 13}}, {{9, 14}}, {{10, 12}}, {{10, 13}}, 
  {{11, 16}}, {{12, 17}}, {{13, 18}}, {{14, 19}}, {{15, 20}}, {{16, 17}}, 
  {{16, 20}}, {{17, 18}}, {{18, 19}}, {{19, 20}}}, 
 {{{0.546, 0.956}}, {{0.144, 0.65}}, {{0.326, 0.188}}, {{0.796, 0.188}}, 
  {{0.988, 0.646}}, {{0.552, 0.814}}, {{0.264, 0.616}}, {{0.404, 0.296}}, 
  {{0.752, 0.298}}, {{0.846, 0.624}}, {{0.43, 0.692}}, {{0.682, 0.692}}, 
  {{0.758, 0.492}}, {{0.566, 0.358}}, {{0.364, 0.484}}, {{0.504, 0.602}}, 
  {{0.608, 0.602}}, {{0.634, 0.51}}, {{0.566, 0.444}}, {{0.48, 0.51}}}]
DominatingIntegerPartitionQ[a_List, b_List] := 
        Module[{aa  = Table[0, {Length[a]}], 
                bb = Table[0, {Length[b]}]}, 
               (Length[a] <= Length[b]) && 
               (aa[[1]] = a[[1]]; 
                Do[aa[[i]] = aa[[i - 1]] + a[[i]], {i, 2, Length[a]}]; 
                bb[[1]] = b[[1]]; 
                Do[bb[[i]] = bb[[i - 1]] + b[[i]], {i, 2, Length[b]}]; 
                Apply[And, Table[aa[[i]] >= bb[[i]], {i, Length[a]}]]
               )
        ]
Options[DominationLattice] = {Type -> Undirected, VertexLabel->False}

DominationLattice[n_Integer?Positive, opts___?OptionQ] :=
         Module[{type, label, s = Partitions[n], br},
                {type, label} = {Type, VertexLabel} /. Flatten[{opts, Options[DominationLattice]}];
                br = DominatingIntegerPartitionQ[#2, #1]&;
                If[type === Directed,
                   MakeGraph[s, br, VertexLabel->label],
                   HasseDiagram[MakeGraph[s, br, VertexLabel->label]]
                ]
         ]
Double[e_List] := Join[Map[Reverse, Select[e,(#[[1]]!=#[[2]])&]], e ]

Double[e_List, EdgeWeight] :=
        Join[Map[Prepend[Rest[#],Reverse[ #[[1]] ]]&,
                 Select[e,(#[[1,1]]!=#[[1,2]])&]
             ],
             e
        ]

Double[e_List, All] := Double[e, EdgeWeight]
DurfeeSquare[s_List] :=
	Module[{i,max=1},
		Do [
			If [s[[i]] >= i, max=i],
			{i,2,Min[Length[s],First[s]]}
		];
		max
	]

DurfeeSquare[{}] := 0
Eccentricity[g_Graph, start_Integer, NoEdgeWeights] := Max[ BreadthFirstTraversal[g, start, Level] ] 
Eccentricity[g_Graph, start_Integer] := Eccentricity[g, start, NoEdgeWeights] /; UnweightedQ[g]
Eccentricity[g_Graph, start_Integer] := Map[Max, Last[BellmanFord[g, start]]]
Eccentricity[g_Graph] := Table[Eccentricity[g, i, NoEdgeWeights], {i, V[g]}] /; UnweightedQ[g]
Eccentricity[g_Graph] := Map[ Max, AllPairsShortestPath[g] ]
EdgeChromaticNumber[g_Graph] := ChromaticNumber[ LineGraph[g] ]
EdgeColoring[g_Graph] :=
        Module[{c = VertexColoring[LineGraph[g]], e = Edges[g], se},
               se = Sort[ Table[{e[[i]], i}, {i, Length[e]}]];
               Map[Last, Sort[Map[Reverse, Table[Prepend[se[[i]], c[[i]]], {i, Length[se]}]]]]
        ]
EdgeConnectivity[g_Graph] := Module[{i}, Apply[Min, Table[NetworkFlow[g,1,i], {i, 2, V[g]}]]]
EdgeConnectivity[g_Graph, Cut] := 
        Module[{i}, 
               Last[First[Sort[Table[{Length[c = NetworkFlow[g,1,i,Cut]], c}, 
                                     {i, 2, V[g]}
                               ]
                          ]
                    ]
               ]
        ]
EdgeGroup[g_] := KSubsetGroup[g, KSubsets[Range[Max[g[[1]]]], 2]]
EdgeGroupIndex[g_, x_Symbol] := EdgeGroup[CycleIndex[g, x], x] /; PermutationQ[g[[1]]]

EdgeGroupIndex[ci_?PolynomialQ, x_Symbol]:=
        Module[{f1,f2,f3,PairCycles},
               f1[x[i1_]^(j1_)] := 1;
               f1[x[i1_]] := 1;
               f1[x[i1_]*x[(i2_)^(j2_)]] :=
                       x[LCM[i1, i2]]^(j2*GCD[i1, i2]);
               f1[x[i1_]^(j1_)*x[i2_]] :=
                       x[LCM[i1, i2]]^(j1*GCD[i1, i2]);
               f1[x[i1_]*x[i2_]] := x[LCM[i1, i2]]^GCD[i1, i2];
               f1[x[i1_]^(j1_)*x[i2_]^(j2_)] :=
                       x[LCM[i1, i2]]^(j1*j2*GCD[i1, i2]);
               f1[(a_)*(t__)] :=
                       Product[f1[a*{t}[[i]]], {i, Length[{t}]}]*
                       f1[Apply[Times, {t}]];
               f2[x[i1_]^j1_]:=x[i1]^(i1 Binomial[j1,2]);
               f2[x[i1_]]:=1;
               f2[a_  b_ ]:=f2[a] f2[b];
               f3[x[i1_]]:=If[OddQ[i1],x[i1]^( (i1-1)/2),
                       x[i1]^( (i1-2)/2) * x[i1/2]];
               f3[x[i1_]^j1_]:=If[OddQ[i1],x[i1]^(j1 (i1-1)/2),
                       x[i1]^(j1 (i1-2)/2) * x[i1/2]^j1];
               f3[a_ b_]:=f3[a] f3[b];
               PairCycles[u_ + v_]:=PairCycles[u]+ PairCycles[v];
               PairCycles[a_?NumericQ b_]:=a PairCycles[b];
               PairCycles[a_]:=f1[a] f2[a] f3[a];
               Expand[PairCycles[ci]]
        ]

Edges[Graph[e_List, _List, ___?OptionQ]] := Map[First[#]&, e]
Edges[Graph[e_List, _List, ___?OptionQ], All] := e

Edges[g_Graph, EdgeWeight] :=
         Map[{First[#], EdgeWeight} /. 
             Flatten[{Rest[#], GraphOptions[g], Options[Graph]}]&, 
             Edges[g, All]
         ]
Element[a_List,{index___}] := a[[ index ]]
Options[EmptyGraph] = {Type -> Undirected};

EmptyGraph[n_Integer, opts___?OptionQ] := 
        Module[{type = Type /. Flatten[{opts, Options[EmptyGraph]}]},
               If[type === Undirected, EG[n], EDG[n]]
        ] /; (n >= 0)

EG[0] := Graph[{}, {}]
EDG[0] := Graph[{}, {}, EdgeDirection -> True]
EG[n_Integer?Positive] := Graph[{}, CircularEmbedding[n]]
EDG[n_Integer?Positive] := Graph[{}, CircularEmbedding[n], EdgeDirection -> True]
EmptyQ[g_Graph] := (Length[Edges[g]]==0)
EncroachingListSet[l_List?PermutationQ] := EncroachingListSet[l,{}]
EncroachingListSet[{},e_List] := e

EncroachingListSet[l_List,e_List] :=
        Block[{$RecursionLimit = Infinity},
	      EncroachingListSet[Rest[l], AddToEncroachingLists[First[l],e] ]
        ]
EquivalenceClasses[r_List?EquivalenceRelationQ] := ConnectedComponents[ FromAdjacencyMatrix[r] ]
EquivalenceClasses[g_Graph?EquivalenceRelationQ] := ConnectedComponents[g]
EquivalenceRelationQ[r_?SquareMatrixQ] :=
	ReflexiveQ[r] && SymmetricQ[r] && TransitiveQ[r]

EquivalenceRelationQ[g_Graph] := EquivalenceRelationQ[ToAdjacencyMatrix[g]]
Equivalences[g_Graph, h_Graph, f___] := 
        Module[{dg = Degrees[g], dh = Degrees[h], eq}, 
               eq = Table[Flatten[Position[dh, dg[[i]]], 1], {i, Length[dg]}];
               EQ[g, h, eq, f]
        ]

EQ[g_Graph, h_Graph, eq_List] := eq
EQ[g_Graph, h_Graph, eq_List, f1_, f___] := 
               If[Position[eq, {}] == {},
                  EQ[g, h, RefineEquivalences[eq, g, h, f1], f],
                  eq
               ]

Equivalences[g_Graph, f___] := Equivalences[g, g, f]
Eulerian[n_Integer,k_Integer] := Block[{$RecursionLimit = Infinity}, Eulerian1[n, k]]
Eulerian1[0,k_Integer] := If [k==0, 1, 0]
Eulerian1[n_Integer, k_Integer] := 0 /; (k >= n)
Eulerian1[n_Integer, 0] := 1
Eulerian1[n_Integer,k_Integer] := Eulerian1[n,k] = (k+1) Eulerian1[n-1,k] + (n-k) Eulerian1[n-1,k-1] 
EulerianCycle::obsolete = "Usage of Directed as a second argument to EulerianCycle is obsolete."
EulerianCycle[g_Graph, Directed] := (Message[EulerianCycle::obsolete]; EulerianCycle[g])
EulerianCycle[g_Graph] :=
	Module[{euler,c,cycles,v},
		cycles = Map[(Drop[#,-1])&, ExtractCycles[g]];
		{euler, cycles} = {First[cycles], Rest[cycles]};
		Do [
			c = First[ Select[cycles, (Intersection[euler,#]=!={})&] ];
			v = First[Intersection[euler,c]];
			euler = Join[
				RotateLeft[c, Position[c,v] [[1,1]] ],
				RotateLeft[euler, Position[euler,v] [[1,1]] ]
			];
			cycles = Complement[cycles,{c}],
			{Length[cycles]}
		];
		Append[euler, First[euler]]
	] /; EulerianQ[g]
EulerianQ::obsolete = "Usage of Directed as a second argument to EulerianQ is obsolete."
EulerianQ[g_Graph, Directed] := (Message[EulerianQ::obsolete]; EulerianQ[g])
EulerianQ[g_Graph] := ConnectedQ[g] && (InDegree[g] === OutDegree[g]) /; !UndirectedQ[g]
EulerianQ[g_Graph] := ConnectedQ[g] && Apply[And,Map[EvenQ, Degrees[g]]] /; UndirectedQ[g]
ExactRandomGraph[n_Integer,e_Integer] :=
	Graph[Map[{NthPair[#]}&, Take[ RandomPermutation[n(n-1)/2], e] ], CircularEmbedding[n]]
ExpandEdgeOptions[opts_List, i_Integer?Positive, fvp_List, svp_List, aopts_List] :=
        Module[{ec, es, ed, elc, el, elp, lp, nel},
               {ec, es, ed, elc, el, elp, lp} =
               {EdgeColor, EdgeStyle, EdgeDirection,
                EdgeLabelColor, EdgeLabel, EdgeLabelPosition, LoopPosition} /. opts;
               {ec,
                ExpandEdgeOptions[EdgeStyle, es],
                If[SameQ[fvp, svp],
                   ExpandEdgeOptions[Loop, ed, fvp, lp],
                   ExpandEdgeOptions[EdgeDirection, ed, fvp, svp, aopts]
                ],
                If[(el =!= False) && (el =!= Off),
                   {elc, 
                    nel = If[ListQ[el], el[[Mod[i-1, Length[el]]+1]], el];
                    ExpandEdgeOptions[EdgeLabel, nel, elp, fvp, svp]
                   },
                   {Black}
                ]
               }
        ]


ExpandEdgeOptions[EdgeDirection, ed_, fvp_List, svp_List, aopts_List] :=
        Module[{mp},
               Switch[ed, {True|On, _Integer},
                          mp = PathMidpoint[fvp, svp, ed[[2]]*Distance[{fvp,svp}//N]/30];
                          Apply[Sequence, {Line[{fvp, mp}], Arrow[mp, svp, Apply[Sequence, aopts]]}],
                          {False|Off, _Integer},
                          mp = PathMidpoint[fvp, svp, ed[[2]]*Distance[{fvp,svp}//N]/30];
                          Line[{fvp, mp, svp}],
                          True|On, Arrow[fvp, svp, Apply[Sequence, aopts]],
                          False|Off, Line[{fvp, svp}]
               ]
        ]

ExpandEdgeOptions[Loop, ed_, fvp_, lp_] :=
        Module[{offset, radius = 0.02, direction, center},
               If[Length[ed] === 0, offset = 0, offset = ed[[2]]*0.005];
               Switch[lp, UpperRight, direction = {1, 1},
                          UpperLeft, direction = {-1, 1},
                          LowerLeft, direction = {-1, -1},
                          LowerRight, direction = {1, -1}
               ];
               center = fvp + (radius + offset)*direction;
               Circle[center, Sqrt[Apply[Plus,(center-fvp)^2]]]
        ]


ExpandEdgeOptions[EdgeStyle, es_] :=
        Apply[Sequence,
              Switch[es, Thick,        {Thickness[0.02]},
                         Normal,       {Thickness[0.005]},
                         Thin,         {Thickness[0.0005]},
                         ThickDashed,  {Thickness[0.02], Dashing[{0.05, 0.03}]},                         NormalDashed, {Thickness[0.005], Dashing[{0.05,0.03}]},                         ThinDashed,   {Thickness[0.005], Dashing[{0.05, 0.03}]}              ]
        ]

ExpandEdgeOptions[EdgeLabel, el_, elp_, fvp_List, svp_List] :=
        Switch[elp, Center, Text[el, (fvp+svp)/2],
                    LowerLeft, Text[el, Scaled[{-.02,-.02},(fvp+svp)/2],{1,0}],
                    UpperRight, Text[el, Scaled[{.02,.02}, (fvp+svp)/2],{-1,0}],                    LowerRight,Text[el, Scaled[{.02,-.02}, (fvp+svp)/2],{-1,0}],                    UpperLeft, Text[el, Scaled[{-.02,.02}, (fvp+svp)/2],{1,0}],
                    {_, _}, Text[el, Scaled[{elp[[1]], elp[[2]]}, (fvp+svp)/2],{1,0}]
        ]

ExpandGraph[g_Graph, n_] := GraphUnion[g, EmptyGraph[n - V[g]] ] /; V[g] <= n
ExpandPlotOptions[PlotRange -> Normal, v_List] := PlotRange -> FindPlotRange[v]

ExpandPlotOptions[PlotRange -> x_Real, v_List] :=
        Module[{r = FindPlotRange[v], xd, yd},
               xd = (r[[1,2]]-r[[1,1]])Abs[x]/2;
               yd = (r[[2,2]]-r[[2,1]])Abs[x]/2;
               PlotRange -> {{r[[1,1]]-Sign[x] xd, r[[1,2]]+Sign[x] xd},
                             {r[[2,1]]-Sign[x] yd, r[[2,2]]+Sign[x] yd}
                            }
        ]

ExpandPlotOptions[PlotRange -> Zoom[x_], v_List] := PlotRange -> FindPlotRange[ v[[x]] ]
ExpandPlotOptions[PlotRange -> r_, v_List] := PlotRange -> r
ExpandPlotOptions[x_, _] := x
ExpandVertexOptions[vertexOptions_List, p_List, i_Integer?Positive] :=
        Module[{vc, vs, vn, vnc, vnp, vl, vlc, vlp, nvl},
               {vc, vs, vn, vnc, vnp, vl, vlc, vlp} =
                {VertexColor,
                 VertexStyle,
                 VertexNumber,
                 VertexNumberColor,
                 VertexNumberPosition,
                 VertexLabel,
                 VertexLabelColor,
                 VertexLabelPosition
                } /. vertexOptions;
               {vc,
                ExpandVertexOptions[VertexStyle, vs, p],
                If[(vn === True) || (vn === On),
                   {vnc, ExpandVertexOptions[VertexNumber, vnp, p, i]},
                   {Black}
                ],
                If[(vl =!= False) && (vl =!= Off),
                   {vlc, 
                    nvl = If[ListQ[vl], vl[[Mod[i-1, Length[vl]]+1]], vl];
                    ExpandVertexOptions[ VertexLabel, nvl, vlp, p]
                   },
                   {Black}
                ]
               }
        ]      


ExpandVertexOptions[VertexStyle, vs_, p_List] :=
        Module[{x = vs[[1]], y = Head[vs], d},
               Switch[x, Small, d = 0.012,
                         Normal, d = 0.025,
                         Large, d = 0.07,
                         _, d = x
               ];
               Switch[y, Disk, {PointSize[d], Point[p]},
                         Box, Rectangle[p-d/2, p+d/2]
               ]
        ]


ExpandVertexOptions[VertexNumber, vnp_, p_List, i_Integer] :=
        Switch[vnp, Center, Text[i, p],
                    LowerLeft, Text[i, Scaled[{-0.02,-0.02},p], {1, 0}],
                    UpperRight, Text[i, Scaled[{0.02,0.02},p], {-1, 0}],
                    LowerRight, Text[i, Scaled[{0.02,-0.02},p], {-1, 0}],
                    UpperLeft, Text[i, Scaled[{-0.02, 0.02},p], {1, 0}],
                    {_, _}, Text[i, Scaled[{vnp[[1]], vnp[[2]]}, p], {1, 0}]
        ]


ExpandVertexOptions[VertexLabel, vl_, vlp_, p_List] :=
        Switch[vlp, Center, Text[vl, p],
                    LowerLeft, Text[vl,Scaled[{-.02,-.02},p],{1,0}],
                    UpperRight, Text[vl,Scaled[{.02,.02},p],{-1,0}],
                    LowerRight,Text[vl,Scaled[{.02,-.02},p],{-1,0}],
                    UpperLeft, Text[vl,Scaled[{-.02,.02},p],{1,0}],
                    {_, _}, Text[vl,Scaled[{vlp[[1]], vlp[[2]]}, p], {1,0}]
        ]

ExtractCycles[gi_Graph] := 
	Module[{g=gi,cycles={},c},
		While[!SameQ[{}, c=FindCycle[g]],
			PrependTo[cycles,c];
			g = DeleteCycle[g,c];
		];
		cycles
	]

FerrersDiagram[p1_List] :=
	Module[{i,j,n=Length[p1],p=Sort[p1]},
		Show[
			Graphics[
				Join[
					{PointSize[ Min[0.05,1/(2 Max[p])] ]},
					Table[Point[{i,j}], {j,n}, {i,p[[j]]}]
				],
				{AspectRatio -> 1, PlotRange -> All}
			]
		]
	]
FindBackEdge[v_Integer, tag_:True] := 
        (s[[v]] = cnt++; 
         Scan[(If[parent[[#]] == 0, 
                  parent[[#]] = v; FindBackEdge[#, tag], 
                  If[tag === True,
                     If[parent[[v]] != #, Throw[{v, #}]],
                     If[(s[[#]] < s[[v]]) && (f[[#]] == 0), Throw[{v, #}]]
                  ]
               ])&, 
               e[[v]]
         ]; 
         f[[v]] = cnt++;)
FindBiconnectedComponents[g_Graph] := { {}, {} } /; EmptyQ[g]

FindBiconnectedComponents[g_Graph] :=
	Block[{e=ToAdjacencyLists[g],n=V[g],par,c=0,act={},back, dfs,ap=bcc={}},
		back=dfs=Table[0,{n}];
		par = Table[n+1,{n}]; 
                Map[(SearchBiConComp[First[#]]) &, Select[cc = ConnectedComponents[g], Length[#] > 1 &]];
                {Join[bcc, Select[cc, Length[#] == 1 &]], Drop[ap, -1]}
        ]

FindBridge[g_Graph, cycle_List] := 
        Module[{rg = RemoveCycleEdges[g, cycle], b, bridge, j}, 
               b = Map[(IsolateSubgraph[rg, g, cycle, #]) &, 
                       Select[ConnectedComponents[rg], (Intersection[#, cycle] == {})&]
                   ];
               b = Select[b, (! EmptyQ[#]) &];
               j = Join[ 
                      Map[Function[
                             bridge, 
                             Select[cycle, ToAdjacencyLists[bridge][[#]] !={}&]
                          ], b
                      ],
                      Complement[
                         Select[Edges[g], 
                                (Length[Intersection[#, cycle]] == 2)&
                         ], 
                         Join[#, Reverse /@ #]&[Partition[Append[cycle,First[cycle]], 2, 1]]
                      ]
                   ];
               {b, j}
        ]
FindCycle::obsolete = "Usage of Directed as a second argument to FindCycle is obsolete."
FindCycle[g_Graph, Directed] := (Message[FindCycle::obsolete]; FindCycle[g])
FindCycle[g_Graph] := First[Select[Edges[g], #[[1]]==#[[2]]&]] /; SelfLoopsQ[g]

FindCycle[g_Graph] :=
     Module[{e = Cases[Split[Sort[Edges[g]]], {x_List, x_List, ___}][[1, 1]]}, 
             Append[e, e[[1]]]
     ] /; (UndirectedQ[g] && MultipleEdgesQ[g])

FindCycle[g_Graph] := FindCycle[ToAdjacencyLists[g], UndirectedQ[g]]

FindCycle[al_List, tag_:True] := 
       Block[{e = al, parent = Table[0, {Length[al]}], s=Table[0, {Length[al]}],
              f = Table[0, {Length[al]}], cnt = 1, start, edge, $RecursionLimit = Infinity},
              While[Count[parent, 0] > 0, 
                    start = Position[parent, 0][[1, 1]];
                    parent[[start]] = start;
                    edge = Catch[FindBackEdge[start, tag]];
                    If[edge =!= Null,
                       c = Reverse[NestWhileList[parent[[#]] &, parent[[edge[[1]]]], (# != edge[[2]])&]];
                       Return[Prepend[Append[c, edge[[1]]], edge[[1]]]]
                    ]
              ];
              {}
       ]
FindPath[l_List,v1_Integer,v2_Integer] :=
	Block[{x=l[[v2]],y,z=v2,lst={}},
		If[SameQ[x,0], Return[{}]];
		While[!SameQ[x, start],
			If[ SameQ[x[[2]],f],
				PrependTo[lst,{{ x[[1]], z }, f}],
				PrependTo[lst,{{ z, x[[1]] }, b}]
			];
			z = x[[1]]; x = l[[z]];
		];
		lst
	]
FindPlotRange[v_List] :=
        Block[{xmax, xmin, ymax, ymin},
              xmin=Min[Map[First[First[#]]&, v]];
              xmax=Max[Map[First[First[#]]&, v]];
              ymin=Min[Map[Last [First[#]]&, v]]; 
              ymax=Max[Map[Last [First[#]]&, v]];
              {{xmin - 0.05 Max[1,xmax-xmin], xmax + 0.05 Max[1,xmax-xmin]},
               {ymin - 0.05 Max[1,ymax-ymin], ymax + 0.05 Max[1,ymax-ymin]}
              }
        ]
FindSet[n_Integer,s_List] := 
        Block[{$RecursionLimit = Infinity}, If [n == s[[n,1]], n, FindSet[s[[n,1]],s]]]
FiniteGraphs := 
        {ChvatalGraph, CoxeterGraph, CubicalGraph, DodecahedralGraph, 
         FolkmanGraph, FranklinGraph, FruchtGraph, GrotztschGraph, 
         HeawoodGraph, HerschelGraph, LeviGraph, McGeeGraph, MeredithGraph, 
         NonLineGraphs, NoPerfectMatchingGraph, OctahedralGraph, PetersenGraph,
         RobertsonGraph, SmallestCyclicGroupGraph, TetrahedralGraph, 
         ThomassenGraph, TutteGraph, Uniquely3ColorableGraph, 
         UnitransitiveGraph, WaltherGraph}
FirstExample[list_List, predicate_] := Scan[(If [predicate[#],Return[#]])&,list]
FirstLexicographicTableau[s_List] :=
	TransposeTableau[ LastLexicographicTableau[ TransposePartition[s] ] ]
FolkmanGraph := 
 Graph[{{{1, 6}}, {{1, 9}}, {{1, 11}}, {{1, 14}}, {{2, 8}}, {{2, 10}}, 
  {{2, 13}}, {{2, 15}}, {{3, 7}}, {{3, 9}}, {{3, 12}}, {{3, 14}}, {{4, 6}}, 
  {{4, 8}}, {{4, 11}}, {{4, 13}}, {{5, 7}}, {{5, 10}}, {{5, 12}}, {{5, 15}}, 
  {{6, 16}}, {{6, 20}}, {{7, 16}}, {{7, 17}}, {{8, 17}}, {{8, 18}}, 
  {{9, 18}}, {{9, 19}}, {{10, 19}}, {{10, 20}}, {{11, 16}}, {{11, 20}}, 
  {{12, 16}}, {{12, 17}}, {{13, 17}}, {{13, 18}}, {{14, 18}}, {{14, 19}}, 
  {{15, 19}}, {{15, 20}}}, {{{0.474, 0.922}}, {{0.472, 0.844}}, 
  {{0.472, 0.77}}, {{0.478, 0.69}}, {{0.472, 0.998}}, {{0.576, 0.596}}, 
  {{0.68, 0.6}}, {{0.786, 0.596}}, {{0.866, 0.6}}, {{0.946, 0.598}}, 
  {{0.39, 0.598}}, {{0.32, 0.596}}, {{0.214, 0.6}}, {{0.118, 0.598}}, 
  {{0.026, 0.586}}, {{0.484, 0.494}}, {{0.478, 0.388}}, {{0.482, 0.306}}, 
  {{0.478, 0.222}}, {{0.484, 0.15}}}]
Format[Graph[e_, v_, o___]] := 
       SequenceForm["\[SkeletonIndicator]Graph:<", 
                    Length[e], ", ", Length[v], ", ", 
                    If[MemberQ[{o}, EdgeDirection -> On] ||
                       MemberQ[{o}, EdgeDirection -> True], 
                       "Directed", 
                       "Undirected"
                    ], 
                    ">\[SkeletonIndicator]"
       ]
FranklinGraph :=
 Graph[{{{1, 2}}, {{1, 3}}, {{1, 7}}, {{2, 4}}, {{2, 8}}, {{3, 5}}, {{3, 11}}, 
  {{4, 6}}, {{4, 12}}, {{5, 6}}, {{5, 7}}, {{6, 8}}, {{7, 9}}, {{8, 10}}, 
  {{9, 10}}, {{9, 12}}, {{10, 11}}, {{11, 12}}}, 
 {{{0.394, 0.924}}, {{0.562, 0.924}}, {{0.394, 0.762}}, {{0.562, 0.76}}, 
  {{0.256, 0.564}}, {{0.726, 0.564}}, {{0.104, 0.48}}, {{0.872, 0.47}}, 
  {{0.192, 0.332}}, {{0.782, 0.332}}, {{0.65, 0.436}}, {{0.324, 0.436}}}]
Options[FromAdjacencyLists] = {Type -> Undirected}

FromAdjacencyLists[al_List, v_List, opts___?OptionQ] :=
        ChangeVertices[FromAdjacencyLists[al, opts], v]
FromAdjacencyLists[al:{{_Integer...}...}, opts___?OptionQ] := 
        Module[{type, g, nal},
               type = Type /. Flatten[{opts, Options[FromAdjacencyLists]}];
               nal = If[type === Undirected, 
                        Table[Select[al[[i]], (# >= i) &], {i, Length[al]}], 
                        al
                     ];
               g = Graph[Flatten[Table[Map[{{i,#}}&,  nal[[i]] ],
                                          {i, Length[al]}
                                 ], 1
                         ],
                         CircularEmbedding[Length[al]]
                   ];
               If[type === Directed, SetGraphOptions[g, EdgeDirection->True], g]
        ]
FromAdjacencyLists[al:{{{_Integer,(_Integer|_Real)}...}...}, EdgeWeight, 
                   opts___?OptionQ] :=
        Module[{type, g, nal},
               type = Type /. Flatten[{opts,Options[FromAdjacencyLists]}]; 
               nal = If[type === Undirected,
                        Table[Select[al[[i]],(#[[1]] >= i) &],{i, Length[al]}],
                        al
                     ];
               g = Graph[Flatten[
                            Table[Map[{{i,#[[1]]}, EdgeWeight->#[[2]]}&, 
                                      nal[[i]]
                                  ],
                                  {i, Length[al]}
                            ], 1
                         ],
                         CircularEmbedding[Length[al]]
                   ];
               If[type === Directed, SetGraphOptions[g, EdgeDirection->True], g]
        ]
Options[FromAdjacencyMatrix] = {Type -> Undirected};

FromAdjacencyMatrix[m:{{_Integer...}...}, v_List, opts___?OptionQ] := 
        ChangeVertices[FromAdjacencyMatrix[m, opts], v]

FromAdjacencyMatrix[m:{{_Integer...}...}, opts___?OptionQ] := 
        Module[{type, p},
               type = Type /. Flatten[{opts, Options[FromAdjacencyMatrix]}];
               If[type === Undirected, 
                  p=Union[Map[Sort, Position[m, _Integer?Positive]]]; AM[p, m],
                  p=Position[m, _Integer?Positive]; 
                  SetGraphOptions[AM[p, m], EdgeDirection -> True]
               ]
        ]
               
AM[p_List, m_List] := 
        Graph[Flatten[
                  Map[Table[{#}, {i, m[[Apply[Sequence, #]]]}] &, p], 1
              ], 
              CircularEmbedding[Length[m]]
        ]
FromAdjacencyMatrix[m_List, v_List, EdgeWeight, opts___?OptionQ] := 
        ChangeVertices[FromAdjacencyMatrix[m, EdgeWeight, opts], v]

FromAdjacencyMatrix[m_List, EdgeWeight, opts___?OptionQ] := 
        Module[{type}, type = Type /. Flatten[{opts, Options[FromAdjacencyMatrix]}];
               If[type === Undirected, 
                  p = Union[Map[Sort, Position[m, _?NumericQ, 2]]]; 
                  AM[p, m, EdgeWeight], 
                  p = Position[m, _?NumericQ, 2];
                  SetGraphOptions[AM[p, m, EdgeWeight], EdgeDirection -> True]
               ]
        ]

AM[p_List, m_List, EdgeWeight] := 
        Graph[Map[{#, EdgeWeight -> m[[Apply[Sequence, #]]]} &, p], 
              CircularEmbedding[Length[m]]
        ]
FromCycles[cyc_List] := Map[Last, Sort[Transpose[Map[Flatten, {Map[RotateRight, cyc], cyc}]]]]
FromInversionVector[vec_List] :=
	Module[{n=Length[vec]+1,i,p}, p={n}; Do [p = Insert[p, i, vec[[i]]+1], {i,n-1,1,-1}]; p]
Options[FromOrderedPairs] = {Type -> Directed}

FromOrderedPairs[el_List, v_List, opts___?OptionQ] := ChangeVertices[FromOrderedPairs[el, opts], v]
FromOrderedPairs[el_List, opts___?OptionQ] := 
       Module[{type},
              type = Type /. Flatten[{opts, Options[FromOrderedPairs]}];
              If[type === Directed, FOPD[el], FOP[el]]
       ]

FOPD[{}] := Graph[{}, {{0,0}}, EdgeDirection->True]
FOPD[el_List] := Graph[Map[{#}&, el], CircularEmbedding[Max[el]], EdgeDirection -> True]

FOP[{}] := Graph[{}, {{0,0}}]
FOP[el_List] := Graph[Map[{#}&, Union[Map[Sort, el]]], CircularEmbedding[Max[el]]]
FromParent[parent_List,s_Integer] :=
	Module[{i=s,lst={s}},
		While[!MemberQ[lst,(i=parent[[i]])], PrependTo[lst,i] ];
		PrependTo[lst,i];
		Take[lst, Flatten[Position[lst,i]]]
	]
 
Options[FromUnorderedPairs] = {Type -> Undirected};

FromUnorderedPairs[el_List, v_List, opts___?OptionQ] := ChangeVertices[FromUnorderedPairs[el, opts], v]
FromUnorderedPairs[el_List, opts___?OptionQ] := 
       Module[{type},
              type = Type /. Flatten[{opts, Options[FromUnorderedPairs]}];
              If[type === Undirected, FUP[el], FUPD[el]]
       ]

FUP[{}] := Graph[{}, {{0,0}}]
FUP[el_List] := Graph[Map[{Sort[#]}&, el], CircularEmbedding[Max[el]] ]

FUPD[{}] := Graph[{}, {{0,0}}, EdgeDirection->True]
FUPD[el_List] := Graph[Map[{#}&, Double[el]], CircularEmbedding[Max[el]], EdgeDirection -> True]
FruchtGraph := 
 Graph[{{{1, 2}}, {{1, 3}}, {{1, 12}}, {{2, 4}}, {{2, 7}}, {{3, 6}}, 
  {{3, 11}}, {{4, 5}}, {{4, 7}}, {{5, 9}}, {{5, 12}}, {{6, 10}}, {{6, 11}}, 
  {{7, 8}}, {{8, 9}}, {{8, 10}}, {{9, 10}}, {{11, 12}}}, 
 {{{0.474, 0.916}}, {{0.264, 0.832}}, {{0.72, 0.8}}, {{0.388, 0.734}}, 
  {{0.46, 0.7}}, {{0.71, 0.672}}, {{0.246, 0.628}}, {{0.336, 0.46}}, 
  {{0.46, 0.588}}, {{0.656, 0.464}}, {{0.598, 0.686}}, {{0.51, 0.79}}}]
Options[FunctionalGraph] = {Type -> Directed};

FunctionalGraph[f_, n_Integer] := 
        FromOrderedPairs[
            Table[{i, Mod[Apply[f, {i}], n]}+1, {i, 0, n-1}]
        ]

FunctionalGraph[f_List, v_List, opts___?OptionQ] :=
        Module[{type, t},
               type = Type /. Flatten[{opts, Options[FunctionalGraph]}];
               t = Flatten[
                      Table[Table[{i, Position[v, Apply[f[[j]], {v[[i]]}]][[1, 1]]},
                                  {j, Length[f]}
                            ],
                            {i, Length[v]}
                      ], 1
                   ];
               If[type === Directed,
                  FromOrderedPairs[t, Type -> Directed],
                  FromOrderedPairs[t, Type -> Undirected]
               ]
        ]

FunctionalGraph[f_, v_List, opts___?OptionQ] := FunctionalGraph[{f}, v, opts]
GeneralizedPetersenGraph[n_Integer?Positive, k_Integer?Positive] := 
        Module[{c = CircularEmbedding[n]}, 
               AddEdges[
                  ChangeVertices[
                     GraphUnion[CirculantGraph[n, k], Cycle[n]], 
                     Join[c, 2c]
                  ], 
                  Table[{i, n + i}, {i, n}]
               ]
        ] /; (n > 1)
GetEdgeLabels[g_Graph, el:{{{_Integer, _Integer},___?OptionQ}...}] := 
         Map[EdgeLabel/.  
             Flatten[{Rest[#], GraphOptions[g], Options[ShowGraph]}]&,  
             el
         ]

GetEdgeLabels[g_Graph, el:{{_Integer, _Integer}...}] := 
         Module[{nel = If[UndirectedQ[g], Map[Sort, el], el]},
                GetEdgeLabels[g, Select[Edges[g, All], MemberQ[nel, #[[1]]]] ]
         ]
                    
GetEdgeLabels[g_Graph] := GetEdgeLabels[g, Edges[g, All]]
GetEdgeWeights[g_Graph, el:{{{_Integer, _Integer},___?OptionQ}...}] := 
         Map[EdgeWeight /.  
             Flatten[{Rest[#], GraphOptions[g], Options[Graph]}]&,  
             el
         ]

GetEdgeWeights[g_Graph, el:{{_Integer, _Integer}...}] := 
         Module[{nel = If[UndirectedQ[g], Map[Sort, el], el]},
                GetEdgeWeights[g, Select[Edges[g, All], MemberQ[nel, #[[1]]]& ]]
         ]
                    
GetEdgeWeights[g_Graph] := GetEdgeWeights[g, Edges[g, All]]
GetVertexLabels[g_Graph, el:{{{_?NumericQ, _?NumericQ},___?OptionQ}...}] := 
         Map[VertexLabel/.  
             Flatten[{Rest[#], GraphOptions[g], Options[ShowGraph]}]&,  
             el
         ]

GetVertexLabels[g_Graph, el:{_Integer...}] := 
         GetVertexLabels[g, Vertices[g, All][[ el ]] ]
                    
GetVertexLabels[g_Graph] := GetVertexLabels[g, Vertices[g, All]]
GetVertexWeights[g_Graph, el:{{{_?NumericQ, _?NumericQ},___?OptionQ}...}] := 
         Map[VertexWeight /.  
             Flatten[{Rest[#], GraphOptions[g], Options[Graph]}]&,  
             el
         ]

GetVertexWeights[g_Graph, el:{_Integer...}] := 
         GetVertexWeights[g, Vertices[g, All][[ el ]] ]
                    
GetVertexWeights[g_Graph] := GetVertexWeights[g, Vertices[g, All]]
Girth[g_Graph] := 1 /; SelfLoopsQ[g]
Girth[g_Graph] := 2 /; MultipleEdgesQ[g]

Girth[g_Graph] := 
	Module[{v,dist,queue,n=V[g],girth=Infinity,
                parent,e=ToAdjacencyLists[g],x},
		Do [
			dist = parent = Table[Infinity, {n}];
			dist[[v]] = parent[[v]] = 0;
			queue = {v};
			While [queue != {},
				{x,queue} = {First[queue],Rest[queue]};
				Scan[
					(If [ (dist[[#]]+dist[[x]]<girth) &&
				     	      (parent[[x]] != #),
						girth=dist[[#]]+dist[[x]] + 1,
				 	 If [dist[[#]]==Infinity,
						dist[[#]] = dist[[x]] + 1;
						parent[[#]] = x;
						If [2 dist[[#]] < girth-1,
							AppendTo[queue,#] ]
					]])&,
					e[[ x ]]
				];
			],
			{v,n}
		];
		girth
	] 
GraphCenter[g_Graph] := 
	Module[{eccentricity = Eccentricity[g]},
		Flatten[ Position[eccentricity, Min[eccentricity]] ]
	]
GraphComplement[g_Graph] :=
        ChangeVertices[
             DeleteEdges[If[UndirectedQ[g],
                            CompleteGraph[V[g]],
                            CompleteGraph[V[g], Type->Directed]
                         ],
                         Edges[g]
             ],
             Vertices[g, All]
        ]
GraphDifference[g_Graph,h_Graph] :=
        Module[{e = Complement[Edges[g], Edges[h]]}, 
               ChangeEdges[g, Select[Edges[g, All], (MemberQ[e, First[#]]) &]]
	] /; (V[g] == V[h]) && 
             ((UndirectedQ[g] && UndirectedQ[h]) || (!UndirectedQ[g] && !UndirectedQ[h]))
                                                          
GraphIntersection[g_Graph] := g

GraphIntersection[g_Graph,h_Graph] :=
       Module[{e = Intersection[Edges[g], Edges[h]]}, 
              ChangeEdges[g, Select[Edges[g, All], (MemberQ[e, First[#]]) &]]
       ] /; (V[g] == V[h]) && (UndirectedQ[g] == UndirectedQ[h])

GraphIntersection[g_Graph,h_Graph, l__Graph] := GraphIntersection[GraphIntersection[g, h], l]
GraphJoin[g_Graph] := g

GraphJoin[g_Graph,h_Graph] :=
        AddEdges[GraphUnion[g, h], 
                 CartesianProduct[Range[V[g]],Range[V[h]]+V[g]] 
        ] /; (UndirectedQ[g] == UndirectedQ[h])

GraphJoin[g_Graph,h_Graph, l__Graph] := GraphJoin[GraphJoin[g, h], l]

GraphLabels[v_List,l_List] :=
	Module[{i},
		Table[ Text[ l[[i]],v[[i]]-{0.03,0.03},{0,1} ],{i,Length[v]}]
	]
GraphOptions[Graph[e_, v_, opts___?OptionQ]] := {opts}
GraphOptions[Graph[e_, v_, opts___?OptionQ], sv_?NumberQ] :=
        Merge[Rest[v[[sv]]], {opts}]
GraphOptions[Graph[e_, v_, opts___?OptionQ], se_List] :=
        Module[{sse = Sort[se]},
               Merge[Rest[Select[e, First[#]==sse&][[1]]], {opts}]
        ]
GraphPolynomial[0, _] := 1
GraphPolynomial[n_Integer?Positive, x_] :=
        OrbitInventory[PairGroupIndex[SymmetricGroupIndex[n, x], x], x, {1, x}] 

GraphPolynomial[0, _, Directed] := 1
GraphPolynomial[n_Integer?Positive, x_, Directed] :=
        OrbitInventory[PairGroupIndex[SymmetricGroupIndex[n, x], x, Ordered], 
                       x, 
                       {1, x}
        ] 
GraphPower[g_Graph,1] := g
GraphPower[g_Graph, k_Integer] :=
        Module[{prod = power = p = ToAdjacencyMatrix[g]},
               FromAdjacencyMatrix[Do[prod = prod.p; power=power+prod, {k-1}]; power, Vertices[g, All]]
        ]
GraphProduct[g_Graph] := g
GraphProduct[g_Graph, h_Graph] := Graph[{}, {}] /; (V[g] == 0) || (V[h] == 0)
GraphProduct[g_Graph, h_Graph] :=
	Module[{k, eg=Edges[g,All],eh=Edges[h,All],leng=V[g],lenh=V[h]},
               Graph[Flatten[
                         Join[Table[Map[Prepend[
                                           Rest[#], #[[1]] + (i-1)*leng
                                        ]&, eg
                                    ], {i,lenh}
                              ],
                              Map[(Table[Prepend[Rest[#], leng*(#[[1]]-1)+k],
                                         {k, leng}
                                   ])&,
				   eh
                              ]
                         ], 1
                     ],
		     Map[{#}&, ProductVertices[Vertices[g], Vertices[h]]],
                     Apply[Sequence, Options[g]]
               ]
	] /; (UndirectedQ[g] == UndirectedQ[h])

GraphProduct[g_Graph, h_Graph, l__Graph] := GraphProduct[GraphProduct[g, h],l]
GraphSum[g_Graph] := g
GraphSum[g_Graph, h_Graph] :=
	ChangeEdges[g, Join[Edges[g,All], Edges[h,All]]
        ] /; (V[g]==V[h]) && (UndirectedQ[g] == UndirectedQ[h])

GraphSum[g_Graph, h_Graph, l__Graph] := GraphSum[GraphSum[g, h], l]
GraphUnion[g_Graph] := g 

GraphUnion[g_Graph, h_Graph] := g /; (V[h] == 0)
GraphUnion[g_Graph, h_Graph] := h /; (V[g] == 0)

GraphUnion[g_Graph, h_Graph] := 
        If[UndirectedQ[h], 
           GraphUnion[MakeUndirected[g], h], 
           GraphUnion[SetGraphOptions[g, EdgeDirection -> True], h]
        ] /; EmptyQ[g] && (UndirectedQ[g] != UndirectedQ[h])

GraphUnion[g_Graph, h_Graph] := 
        If[UndirectedQ[g], 
           GraphUnion[g, MakeUndirected[h]], 
           GraphUnion[g, SetGraphOptions[h, EdgeDirection -> True]]
        ] /; EmptyQ[h] && (UndirectedQ[g] != UndirectedQ[h])
GraphUnion[lg__Graph] := 
        PutTogether[Map[NormalizeVertices, {lg}]] /; (Count[Map[UndirectedQ, {lg}], True] == 0) ||
                                                     (Count[Map[UndirectedQ, {lg}], True] == Length[{lg}])

PutTogether[{g_Graph,h_Graph}] :=
	Module[{maxg=Max[ Map[First, Vertices[g]]], 
                minh=Min[ Map[First, Vertices[h]]],
                n = V[g], s},
                s = maxg - minh + 1;
                ChangeEdges[
                    ChangeVertices[g,
                                   Join[Vertices[g, All],
                                        Map[Prepend[Rest[#], {s, 0}+ First[#]]&, 
                                            Vertices[h, All]
                                        ]
                                   ]
                    ],
                    Join[Edges[g,All], 
                         Map[Prepend[Rest[#], First[#]+ n ]&,
                             Edges[h,All]
                         ]
                    ] 
                ]
	] 

PutTogether[{g_Graph, h_Graph, l__Graph}] := PutTogether[{PutTogether[{g, h}], l}]

GraphUnion[0,g_Graph] := EmptyGraph[0];
GraphUnion[1,g_Graph] := g
GraphUnion[k_Integer,g_Graph] := GraphUnion[Apply[Sequence, Table[g, {k}]] ]
GraphicQ[s_List] := False /; (Min[s] < 0) || (Max[s] >= Length[s])
GraphicQ[s_List] := (First[s] == 0) /; (Length[s] == 1)
GraphicQ[s_List] :=
	Module[{m,sorted = Reverse[Sort[s]]},
		m = First[sorted];
		GraphicQ[ Join[ Take[sorted,{2,m+1}]-1, Drop[sorted,m+1] ] ]
	]
GrayCode[l_List] := GrayCodeSubsets[l]
GrayCodeKSubsets[n_Integer?Positive, k_Integer] := GrayCodeKSubsets[Range[n], k]

GrayCodeKSubsets[l_List, 0] := {{}}
GrayCodeKSubsets[l_List, 1] := Partition[l, 1]
GrayCodeKSubsets[l_List, k_Integer?Positive] := {l} /; (k == Length[l])
GrayCodeKSubsets[l_List, k_Integer?Positive] := {} /; (k > Length[l])

GrayCodeKSubsets[l_List, k_Integer] := 
       Block[{$RecursionLimit = Infinity},
             Join[GrayCodeKSubsets[Drop[l, -1], k], 
                  Map[Append[#, Last[l]]&, 
                      Reverse[GrayCodeKSubsets[Drop[l,-1], k-1]]
                  ]
             ]
       ]
GrayCodeSubsets[n_Integer?Positive] := GrayCodeSubsets[Range[n]]

GrayCodeSubsets[ { } ] := { {} }

GrayCodeSubsets[l_List] := 
       Block[{s, $RecursionLimit = Infinity}, 
              s = GrayCodeSubsets[Take[l, 1-Length[l]]];
              Join[s,  Map[Prepend[#, First[l]] &, Reverse[s]]]
       ]
GrayGraph := 
    MakeUndirected[
        MakeGraph[Range[54],
            Mod[#2 - #1, 54] == 1 ||
                (Mod[#1, 6] == 1 && Mod[#2 - #1, 54] == 25) ||
                (Mod[#1, 6] == 2 && Mod[#2 - #1, 54] == 29) ||
                (Mod[#1, 6] == 5 && Mod[#2 - #1, 54] == 7) ||
                (Mod[#1, 6] == 3 && Mod[#2 - #1, 54] == 13) &
        ]
    ]
GreedyVertexCover[g_Graph, l_List] := GreedyVertexCover[MakeSimple[g]] /; (!SimpleQ[g] || !UndirectedQ[g])

GreedyVertexCover[g_Graph, l_List] := 
       Module[{ng = g, d, m, s = {}, v}, 
              While[M[ng] != 0,  
                    d = Degrees[ng][[l]]; 
                    m  = Max[d]; 
                    If[m == 0, Return[s]]; 
                    v = l[[Position[d, m][[1, 1]]]]; 
                    AppendTo[s, v]; 
                    al = ToAdjacencyLists[ng]; 
                    ng = DeleteEdges[
                             ng, 
                             Table[{v, al[[v, i]]}, {i, Length[al[[v]]]}]
                         ]
              ]; 
              s
       ]

GreedyVertexCover[g_Graph] := GreedyVertexCover[g, Range[V[g]]]
GridGraph[n_Integer?Positive, m_Integer?Positive] :=
	GraphProduct[
		ChangeVertices[Path[n],Map[({{Max[n,m]*#,0}})&,Range[n]]],
		Path[m]
	]

GridGraph[p_Integer?Positive, q_Integer?Positive, r_Integer?Positive] := 
        GraphProduct[GridGraph[p, q], 
                     DilateVertices[RotateVertices[Path[r], 1], 1/(10*r)]
        ]
GrotztschGraph := 
 Graph[{{{1, 2}}, {{1, 3}}, {{1, 8}}, {{1, 11}}, {{2, 4}}, {{2, 7}}, 
  {{2, 10}}, {{3, 5}}, {{3, 7}}, {{3, 9}}, {{4, 5}}, {{4, 9}}, {{4, 11}}, 
  {{5, 8}}, {{5, 10}}, {{6, 7}}, {{6, 8}}, {{6, 9}}, {{6, 10}}, {{6, 11}}}, 
 {{{0.468, 0.908}}, {{0.242, 0.736}}, {{0.722, 0.728}}, {{0.332, 0.462}}, 
  {{0.612, 0.462}}, {{0.466, 0.684}}, {{0.466, 0.816}}, {{0.604, 0.71}}, 
  {{0.552, 0.556}}, {{0.388, 0.558}}, {{0.336, 0.716}}}]
GroupEdgePositions[e_List, n_Integer] := 
        Map[Map[Last, #]&,
            Split[Union[Transpose[{e, Range[Length[e]]}], Table[{ {i, 0}, 0}, {i, n}]],
                  (#1[[1,1]]=== #2[[1,1]])&
            ]
        ]
HamiltonianCycle[g_Graph,flag_:One] :=
	Module[{s={1},all={},done,adj=Edges[g],
                e=ToAdjacencyLists[g],x,v,ind,n=V[g]},
		ind=Table[1,{n}];
		While[ Length[s] > 0,
			v = Last[s];
			done = False;
			While[ ind[[v]] <= Length[e[[v]]] && !done,
                               x = e[[v,ind[[v]]++]];
                               done = !MemberQ[s, x] && 
                                       (Length[s] == 1 ||
                                        BiconnectedQ[DeleteVertices[AddEdges[g, {{1, x}}], Rest[s]]])
			];
			If[done, AppendTo[s,x], s=Drop[s,-1]; ind[[v]] = 1];
			If[(Length[s] == n),
				If [MemberQ[adj, Sort[{x, 1}]],
                                    AppendTo[all,Append[s,First[s]]];
                                    If [SameQ[flag,All],
                                        s=Drop[s,-1],
					all = Flatten[all]; s={}
			            ],
			            s = Drop[s,-1]
				]
			]
		];
		all
	]
HamiltonianPath[g_Graph] := HamiltonianPath[g, One]
HamiltonianPath[g_Graph, One] := 
        Module[{c = HamiltonianCycle[g], nonEdges, p, q},
               If[c != {}, 
                  Drop[c, -1],
                  nonEdges = Complement[Flatten[Table[{i, j}, {i, V[g]-1}, {j, i+1, V[g]}],1], Edges[g]];
                  Do[h = AddEdges[g, nonEdges[[i]]]; 
                     If[((BiconnectedQ[h]) && ((c = HamiltonianCycle[h]) != {})),
                        p = Position[c = Drop[c,-1], nonEdges[[i, 1]]][[1, 1]];
                        c = RotateLeft[c, p-1];
                        If[nonEdges[[i, 2]] == c[[2]], c = RotateLeft[c, 1]];
                        Break[]
                     ],
                     {i, Length[nonEdges]}
                  ];
                  c
               ]
        ]


HamiltonianPath[g_Graph, All] := 
        Module[{c = HamiltonianCycle[g, All], nonEdges, p, q, h, a, b, 
                al = ToAdjacencyLists[g], s}, 
            Union[
               Flatten[
                   Map[Table[RotateLeft[Drop[#, -1], i - 1], {i, Length[#] - 1}] &, c], 1],
               Flatten[
                   nonEdges = Complement[Flatten[Table[{i, j}, {i, V[g] - 1}, {j, i + 1, V[g]}], 1], Edges[g]];
                   Table[{a, b} = nonEdges[[i]]; 
                         edgesA = Map[{a, #} &, al[[a]]];
                         edgesB = Map[{b, #} &, al[[b]]];
                         h = AddEdges[DeleteEdges[g, Join[edgesA, edgesB]], {a, b}];
                         Table[h = AddEdges[h, {edgesA[[j]], edgesB[[k]]}];
                               If[BiconnectedQ[h],
                                  c = Map[Drop[#, -1] &, HamiltonianCycle[h, All]];
                                  Map[({p, q} = {Position[#, a][[1, 1]], Position[#, b][[1, 1]]};
                                       s = RotateLeft[#, q - 1];
                                       If[s[[2]] == a, RotateLeft[s, 1], s])&, 
                                       c
                                  ], 
                                  {}
                               ], 
                               {j, Length[edgesA]}, {k, Length[edgesB]}
                         ],
                         {i, Length[nonEdges]}
                   ], 3
               ]
            ]
        ]
HamiltonianQ[g_Graph] := False /; !BiconnectedQ[g]
HamiltonianQ[g_Graph] := HamiltonianCycle[g] != {}

Harary[k_?EvenQ, n_Integer] := CirculantGraph[n,Range[k/2]] /; (k > 1) && (n > k)

Harary[k_?OddQ, n_?EvenQ] := CirculantGraph[n,Append[Range[k/2],n/2]] /; (k > 1)&& (n > k)

Harary[k_?OddQ, n_?OddQ] :=
        AddEdges[Harary[k-1, n],
                    Join[{{{1,(n+1)/2}}, {{1,(n+3)/2}}}, Table[{{i,i+(n+1)/2}}, {i,2,(n-1)/2}]]
        ] /; (k > 1) && (n > k)
HasseDiagram[g_Graph, fak_:1] :=
	Module[{r, rank, m, stages, freq=Table[0,{V[g]}], 
                adjm, first},
	       r = TransitiveReduction[RemoveSelfLoops[g]];
               adjm = ToAdjacencyLists[r];
               rank = Table[ 0,{ V[g]} ];                              
               first = Select[ Range[ V[g]], InDegree[r,#]==0& ];
               rank = MakeLevel[ first, 1, adjm, rank];          
               first = Max[rank];
               stages = Distribution[ rank ];                       
               MakeUndirected[
                   ChangeVertices[r,
		          Table[
                            m = ++ freq[[ rank[[i]] ]];
                            {((m-1) + (1-stages[[rank[[i]] ]])/2)*
                            fak^(first-rank[[i]]), rank[[i]]}//N,
                            {i, V[g]}
                          ]
                   ]
               ]
        ] /; AcyclicQ[RemoveSelfLoops[g]] && !UndirectedQ[g]
HeapSort[p_List] :=
	Module[{heap=Heapify[p],min},
		Append[
			Table[
				min = First[heap];
				heap[[1]] = heap[[n]];
				heap = Heapify[Drop[heap,-1],1];
				min,
				{n,Length[p],2,-1}
			],
			Max[heap]
		]
	] /; (Length[p] > 0)
 
HeapSort[{}] := {}
Heapify[p_List] :=
	Module[{j,heap=p},
		Do [
			heap = Heapify[heap,j],
			{j,Quotient[Length[p],2],1,-1}
		];
		heap
	]

Heapify[p_List, k_Integer] :=
	Module[{hp=p, i=k, l, n=Length[p]},
		While[ (l = 2 i) <= n,
			If[ (l < n) && (hp[[l]] > hp[[l+1]]), l++ ];
			If[ hp[[i]] > hp[[l]],
				{hp[[i]],hp[[l]]}={hp[[l]],hp[[i]]};
				i = l,
				i = n+1
			];
		];
		hp
	]

HeawoodGraph := 
 Graph[{{{1, 2}}, {{1, 6}}, {{1, 14}}, {{2, 3}}, {{2, 11}}, {{3, 4}}, 
  {{3, 8}}, {{4, 5}}, {{4, 13}}, {{5, 6}}, {{5, 10}}, {{6, 7}}, {{7, 8}}, 
  {{7, 12}}, {{8, 9}}, {{9, 10}}, {{9, 14}}, {{10, 11}}, {{11, 12}}, 
  {{12, 13}}, {{13, 14}}}, {{{0.8262387743159949, 0.563320058063622}}, 
  {{0.6234898018587336, 0.7818314824680297}}, 
  {{0.0747300935864246, 0.9972037971811801}}, 
  {{-0.22252093395631412, 0.9749279121818236}}, 
  {{-0.7330518718298261, 0.6801727377709197}}, 
  {{-0.9009688679024189, 0.4338837391175586}}, 
  {{-0.9888308262251286, -0.14904226617617403}}, 
  {{-0.9009688679024194, -0.4338837391175576}}, 
  {{-0.5000000000000004, -0.8660254037844384}}, 
  {{-0.22252093395631545, -0.9749279121818234}}, 
  {{0.36534102436639454, -0.9308737486442045}}, 
  {{0.6234898018587327, -0.7818314824680305}}, 
  {{0.9555728057861403, -0.2947551744109056}}, {{1., 0}}}]
HerschelGraph :=
 Graph[{{{1, 3}}, {{1, 4}}, {{1, 5}}, {{1, 6}}, {{2, 3}}, {{2, 4}}, {{2, 7}}, 
  {{2, 8}}, {{3, 11}}, {{4, 10}}, {{5, 9}}, {{5, 10}}, {{6, 9}}, {{6, 11}}, 
  {{7, 9}}, {{7, 10}}, {{8, 9}}, {{8, 11}}}, 
  {{{0.002, 0.244}}, {{0., -0.26}}, {{0.41, 0.008}}, {{-0.396, 0.006}}, 
  {{-0.044, 0.07}}, {{0.042, 0.076}}, {{-0.038, -0.066}}, {{0.048, -0.07}}, 
  {{0., 0.}}, {{-0.204, 0.006}}, {{0.2, 0.008}}}]
HideCycles[c_List] := 
	Flatten[Sort[Map[(RotateLeft[#,Position[#,Min[#]] [[1,1]] - 1])&, c], (#1[[1]] > #2[[1]])& ]]
Options[Highlight] = {HighlightedVertexStyle -> Disk[Large], 
                      HighlightedEdgeStyle -> Thick, 
                      HighlightedVertexColors :> Map[ToExpression, 
                                                     ScreenColorNames],
                      HighlightedEdgeColors :> Map[ToExpression, 
                                                   ScreenColorNames]
                     };

Highlight[g_Graph, {}] := g

Highlight[g_Graph, {}, _] := g

Highlight[g_Graph, l : {_Integer ..}, opts___?OptionQ] := 
        Highlight[g, {l}, opts]

Highlight[g_Graph, l_List, opts___?OptionQ] := 
        Module[{vnc, enc, vs, es, vc, ec, 
                vl = Map[Cases[#, _Integer] &, l], 
                el = Map[Cases[#, {_Integer, _Integer}] &, l]}, 
               {vs, es, vc, ec} = 
               {HighlightedVertexStyle, 
                HighlightedEdgeStyle, 
                HighlightedVertexColors, 
                HighlightedEdgeColors} /. Flatten[{opts, Options[Highlight]}];
               el = If[UndirectedQ[g], Map[Sort, el, 2], el];
               vnc = Length[vc]; 
               enc = Length[ec];
               SetGraphOptions[
                  g, 
                  Join[Table[{Apply[Sequence, vl[[i]]], 
                              VertexStyle -> vs, 
                              VertexColor -> vc[[Mod[i - 1, vnc] + 1]]}, 
                             {i, Length[vl]}
                       ], 
                       Table[{Apply[Sequence, el[[i]]], 
                              EdgeStyle -> es, 
                              EdgeColor -> ec[[Mod[i - 1, enc] + 1]]}, 
                             {i, Length[el]}
                       ]
                  ], opts
               ]
        ]
Hypercube[n_Integer] := Hypercube1[n]

Hypercube1[0] := CompleteGraph[1]
Hypercube1[1] := Path[2]
Hypercube1[2] := Cycle[4]

Hypercube1[n_Integer] := Hypercube1[n] =
	GraphProduct[
		RotateVertices[Hypercube1[Floor[n/2]], 2Pi/5],
		Hypercube1[Ceiling[n/2]]
	]
Hypercube1[0] := CompleteGraph[1]
Hypercube1[1] := Path[2]
Hypercube1[2] := Cycle[4]

Hypercube1[n_Integer] := Hypercube1[n] =
	GraphProduct[
		RotateVertices[ Hypercube1[Floor[n/2]], 2Pi/5],
		Hypercube1[Ceiling[n/2]]
	]
IcosahedralGraph := 
 Graph[{{{1, 2}}, {{1, 3}}, {{1, 4}}, {{1, 5}}, {{1, 9}}, {{2, 3}}, {{2, 7}}, 
  {{2, 8}}, {{2, 9}}, {{3, 5}}, {{3, 6}}, {{3, 7}}, {{4, 5}}, {{4, 9}}, 
  {{4, 10}}, {{4, 12}}, {{5, 6}}, {{5, 12}}, {{6, 7}}, {{6, 11}}, {{6, 12}}, 
  {{7, 8}}, {{7, 11}}, {{8, 9}}, {{8, 10}}, {{8, 11}}, {{9, 10}}, {{10, 11}}, 
  {{10, 12}}, {{11, 12}}}, {{{0.452, 0.956}}, {{0.046, 0.242}}, 
  {{0.832, 0.246}}, {{0.444, 0.67}}, {{0.566, 0.56}}, {{0.544, 0.458}}, 
  {{0.45, 0.396}}, {{0.35, 0.452}}, {{0.32, 0.564}}, {{0.404, 0.558}}, 
  {{0.442, 0.49}}, {{0.48, 0.558}}}]
IdenticalQ[g_Graph, h_Graph] := True /; (EmptyQ[g]) && (EmptyQ[h]) && (V[g] == V[h])
IdenticalQ[g_Graph, h_Graph] := False /; (UndirectedQ[g] != UndirectedQ[h])
IdenticalQ[g_Graph, h_Graph] := (V[g]==V[h]) && (Sort[Edges[g]]===Sort[Edges[h]])
IdentityPermutation[n_Integer] := Range[n]
InDegree[g_Graph, n_Integer] := Length[Select[Edges[g], (Last[#]==n)&]]
InDegree[g_Graph] := OutDegree[ReverseEdges[g]]
IncidenceMatrix[g_Graph] := 
        Module[{e = Edges[g]}, 
               If[UndirectedQ[g], 
                  Table[If[MemberQ[e[[j]], i], 1, 0], {i, V[g]}, {j, M[g]}], 
                  Table[If[i === First[e[[j]]], 1, 0], {i, V[g]}, {j, M[g]}]
               ]
        ]
IndependentSetQ[g_Graph, indep_List] :=
        (Complement[indep, Range[V[g]]] == {}) && VertexCoverQ[ g, Complement[ Range[V[g]], indep] ] 

Index[p_?PermutationQ]:= Module[{i}, Sum[ If [p[[i]] > p[[i+1]], i, 0], {i,Length[p]-1} ]]
InduceSubgraph[g_Graph,{}] := Graph[{},{}]

InduceSubgraph[g_Graph, s_List] := PermuteSubgraph[g, Union[s]] /; Complement[s, Range[V[g]]] == {}
InitializeUnionFind[n_Integer] := Module[{i}, Table[{i,1},{i,n}] ]
InsertIntoTableau[e_Integer, t_?TableauQ] := First[InsertIntoTableau[e, t, All]]

InsertIntoTableau[e_Integer,{}, All] := {{{e}}, 1}
InsertIntoTableau[e_Integer, t1_?TableauQ, All] :=
	Module[{item=e,row=0,col,t=t1},
		While [row < Length[t],
			row++;
			If [Last[t[[row]]] <= item,
				AppendTo[t[[row]],item];
				Return[{t, row}]
			];
			col = Ceiling[ BinarySearch[t[[row]],item] ];
			{item, t[[row,col]]} = {t[[row,col]], item};
		];
		{Append[t, {item}], row+1}
	]
InterlockQ[juncs_, c_] :=
    Module[{pieces = Map[Sort, juncs/.MapThread[Rule, {c, Range[Length[c]]}]]},
            !BipartiteQ[MakeSimple[MakeGraph[pieces, LockQ, Type->Undirected]]]
    ]
IntervalGraph[l_List] :=
        MakeSimple[
	     MakeGraph[l,
		       (((First[#1] <= First[#2]) && (Last[#1] >= First[#2])) ||
		       ((First[#2] <= First[#1]) && (Last[#2] >= First[#1])) )&
	     ]
        ]
InversePermutation[p_?PermutationQ] :=
	Module[{inverse=p},
		Do[ inverse[[ p[[i]] ]] = i, {i,Length[p]} ];
		inverse
	]
Options[InversionPoset] = {Type -> Undirected, VertexLabel->False}

InversionPoset[n_Integer?Positive, opts___?OptionQ] := 
        Module[{type, label, p = Permutations[n], br},
                {type, label} = {Type, VertexLabel} /. Flatten[{opts, Options[InversionPoset]}];
                br = (Count[p = #1 - #2, 0] == n-2) &&
                (Position[p, _?Negative][[1,1]]+1 == Position[p, _?Positive][[1, 1]])&;
                If[type === Directed,
                   MakeGraph[p, br, VertexLabel->label],
                   HasseDiagram[MakeGraph[p, br, VertexLabel->label]]
                ]
        ]
Inversions[{}] := 0
Inversions[p_?PermutationQ] := Apply[Plus,ToInversionVector[p]]
InvolutionQ[p_?PermutationQ] := p[[p]] == Range[Length[p]]
Involutions[l_List, Cycles] := {{ l }} /; (Length[l] === 1)
Involutions[l_List, Cycles] := {{}} /; (Length[l] === 0)

Involutions[l_List, Cycles] := 
       Block[{$RecursionLimit = Infinity},
             Join[Flatten[Table[Map[Append[#, {l[[1]], l[[i]]}] &, 
                                    Involutions[Drop[Rest[l], {i - 1}], Cycles]
                                ], 
                                {i, 2, Length[l]}], 
                          1
                  ],
                  Map[Append[#, {l[[1]]}] & , Involutions[Rest[l], Cycles]]
             ]
       ]

Involutions[l_List] := Map[FromCycles, Involutions[l, Cycles]]
Involutions[n_Integer] := Involutions[Range[n]]
Involutions[n_Integer, Cycles] := Involutions[Range[n], Cycles]
IsolateSubgraph[g_Graph, orig_Graph, cycle_List, cc_List] := 
        ChangeEdges[g,
                    Join[Select[Edges[g], 
                                (Length[Intersection[cc, #]] == 2)&
                         ],
                         Select[Edges[orig], 
                                (Intersection[#, cycle] != {} && 
                                 Intersection[#, cc] != {})&
                         ]
                    ]
        ]
Options[IsomorphicQ] = {Invariants -> {DegreesOf2Neighborhood, NumberOf2Paths, Distances}};

IsomorphicQ[g_Graph, h_Graph, opts___?OptionQ] := True /; IdenticalQ[g,h]
IsomorphicQ[g_Graph, h_Graph, opts___?OptionQ] := 
        Module[{invariants=Invariants /. Flatten[{opts, Options[IsomorphicQ]}]},
	       ! SameQ[ Isomorphism[g, h, Invariants -> invariants], {}]
        ]

Options[Isomorphism] = 
       {Invariants -> {DegreesOf2Neighborhood, NumberOf2Paths, Distances}};

Isomorphism[g_Graph, opts___?OptionQ] := {} /; (V[g] == 0)
Isomorphism[g_Graph, opts___?OptionQ] := {{1}} /; (V[g] == 1)
Isomorphism[g_Graph, opts___?OptionQ] :=
        Module[{invariants=Invariants /. Flatten[{opts, Options[Isomorphism]}]},
               Isomorphism[g, g, Equivalences[g, Apply[Sequence, invariants]], All]
        ] 

Isomorphism[g_Graph, h_Graph, flag_Symbol:One, opts___?OptionQ] := {} /; (V[g] != V[h]) 
Isomorphism[g_Graph, h_Graph, flag_Symbol:One, opts___?OptionQ] := {{1}} /; ((V[g] == 1) && (V[h] == 1) && (M[g] == M[h]))

Isomorphism[g_Graph, h_Graph, flag_Symbol:One, opts___?OptionQ] :=
        Module[{invariants=Invariants /. Flatten[{opts, Options[Isomorphism]}]},
               Isomorphism[g,h,Equivalences[g,h, Apply[Sequence, invariants]], flag]
        ]
Isomorphism[g_Graph, h_Graph, equiv_List, flag_Symbol:One] :=
	If[!MemberQ[equiv,{}],
           Backtrack[equiv,
                     (IdenticalQ[
                          PermuteSubgraph[g,Range[Length[#]]],
		          PermuteSubgraph[h,#] 
                      ] &&
                      !MemberQ[Drop[#,-1],Last[#]]
                     )&,
                     (IsomorphismQ[g,h,#])&,
                     flag
            ],
            {}
        ]
IsomorphismQ[g_Graph, h_Graph, p_List] := False	/;
        (V[g]!= V[h]) || !PermutationQ[p] || (Length[p] != V[g])

IsomorphismQ[g_Graph,h_Graph,p_List] := 
        IdenticalQ[g, PermuteSubgraph[h,p]]
JoinCycle[g1_Graph, cycle_List] :=
	Module[{g=g1},
		Scan[(g = AddEdge[g,#])&, Partition[cycle,2,1] ];
		AddEdge[g,{First[cycle],Last[cycle]}]
	]
Josephus[n_Integer,m_Integer] :=
	Module[{live=Range[n],next},
		InversePermutation[
			Table[
				next = RotateLeft[live,m-1];
				live = Rest[next];
				First[next],
				{n}
			]
		]
	]
KSetPartitions[{}, 0] := {{}}
KSetPartitions[s_List, 0] := {}
KSetPartitions[s_List, k_Integer] := {} /; (k > Length[s])
KSetPartitions[s_List, k_Integer] := {Map[{#} &, s]} /; (k === Length[s])
KSetPartitions[s_List, k_Integer] :=
       Block[{$RecursionLimit = Infinity},
             Join[Map[Prepend[#, {First[s]}] &, KSetPartitions[Rest[s], k - 1]],
                  Flatten[
                     Map[Table[Prepend[Delete[#, j], Prepend[#[[j]], s[[1]]]],
                              {j, Length[#]}
                         ]&, 
                         KSetPartitions[Rest[s], k]
                     ], 1
                  ]
             ]
       ] /; (k > 0) && (k < Length[s])

KSetPartitions[0, 0] := {{}}
KSetPartitions[0, k_Integer?Positive] := {}
KSetPartitions[n_Integer?Positive, 0] := {}
KSetPartitions[n_Integer?Positive, k_Integer?Positive] := KSetPartitions[Range[n], k]
KSubsetGroup[g_List, s:{{_Integer..}...}, type_:Unordered] :=
        Table[Flatten[
              Map[Position[s, If[SameQ[type, Ordered], #,Sort[#]]]&,
                  Map[g[[i]][[#]]&, s, {2}]]
              ],
              {i, Length[g]}
        ]
KSubsetGroupIndex[g_, s_, x_Symbol, type_:Unordered] :=
        CycleIndex[KSubsetGroup[g, s, type],x]
KS = Compile[{{n, _Integer}, {k, _Integer}}, 
             Module[{h, ss = Range[k], x},  
                    Table[(h = Length[ss]; x = n;
                           While[x === ss[[h]], h--; x--];
                           ss = Join[Take[ss, h - 1], 
                                     Range[ss[[h]]+1, ss[[h]]+Length[ss]-h+1] 
                                ]), 
                          {Binomial[n, k]-1}
                    ] 
             ]
     ]

KSubsets[l_List,0] := { {} }
KSubsets[l_List,1] := Partition[l,1]
KSubsets[l_List,2] := Flatten[Table[{l[[i]], l[[j]]}, 
                                    {i, Length[l]-1}, 
                                    {j, i+1, Length[l]}
                              ], 
                              1
                      ]
KSubsets[l_List,k_Integer?Positive] := {l} /; (k == Length[l])
KSubsets[l_List,k_Integer?Positive] := {}  /; (k > Length[l])

KSubsets[s_List, k_Integer] := Prepend[Map[s[[#]] &, KS[Length[s], k]], s[[Range[k] ]] ]
KnightsTourGraph[m_Integer?Positive, n_Integer?Positive] := 
        Module[{p = Flatten[Table[{i, j}, {i, m}, {j, n}], 1]}, 
               Graph[Union[
                         Map[{Sort[{n (#[[1, 1]] - 1) + #[[1, 2]], 
                                n (#[[2, 1]] - 1) + #[[2, 2]]}
                              ]
                             }&, 
                             Select[
                                 Flatten[
                                     Map[{{#, #+{2,1}}, {#, #+{2,-1}}, 
                                          {#, #+{-2,1}},{#, #+{-2,-1}}, 
                                          {#, #+{1,2}}, {#, #+{1,-2}}, 
                                          {#, #+{-1,2}},{#, #+{-1,-2}}}&, 
                                         p
                                     ], 1
                                 ],  
                                 (Min[#] >= 1) && (Max[#[[1,1]],#[[2, 1]]]<= m) && 
                                 (Max[#[[1,2]], #[[2,2]]]<=n)&
                             ]
                         ]
                     ],
                     Map[{#} &, p]
               ]
        ]
LabeledTreeToCode[g_Graph] :=
	Module[{e=ToAdjacencyLists[g],i,code},
		Table [
			{i} = First[ Position[ Map[Length,e], 1 ] ];
			code = e[[i,1]];
			e[[code]] = Complement[ e[[code]], {i} ];
			e[[i]] = {};
			code,
			{V[g]-2}
		]
	]
LastLexicographicTableau[s_List] :=
	Module[{c=0},
		Map[(c+=#; Range[c-#+1,c])&, s]
	]
LeviGraph :=
 Graph[{{{1, 2}}, {{1, 8}}, {{1, 30}}, {{2, 3}}, {{2, 25}}, {{3, 4}}, 
  {{3, 12}}, {{4, 5}}, {{4, 17}}, {{5, 6}}, {{5, 22}}, {{6, 7}}, {{6, 27}}, 
  {{7, 8}}, {{7, 14}}, {{8, 9}}, {{9, 10}}, {{9, 18}}, {{10, 11}}, 
  {{10, 23}}, {{11, 12}}, {{11, 28}}, {{12, 13}}, {{13, 14}}, {{13, 20}}, 
  {{14, 15}}, {{15, 16}}, {{15, 24}}, {{16, 17}}, {{16, 29}}, {{17, 18}}, 
  {{18, 19}}, {{19, 20}}, {{19, 26}}, {{20, 21}}, {{21, 22}}, {{21, 30}}, 
  {{22, 23}}, {{23, 24}}, {{24, 25}}, {{25, 26}}, {{26, 27}}, {{27, 28}}, 
  {{28, 29}}, {{29, 30}}}, {{{0.9781476007338057, 0.20791169081775931}}, 
  {{0.9135454576426009, 0.40673664307580015}}, 
  {{0.8090169943749475, 0.5877852522924731}}, 
  {{0.6691306063588582, 0.7431448254773941}}, 
  {{0.5000000000000001, 0.8660254037844386}}, 
  {{0.30901699437494745, 0.9510565162951535}}, 
  {{0.10452846326765368, 0.9945218953682733}}, 
  {{-0.10452846326765333, 0.9945218953682734}}, 
  {{-0.30901699437494734, 0.9510565162951536}}, 
  {{-0.4999999999999998, 0.8660254037844387}}, 
  {{-0.6691306063588579, 0.7431448254773945}}, 
  {{-0.8090169943749473, 0.5877852522924732}}, 
  {{-0.9135454576426008, 0.40673664307580043}}, 
  {{-0.9781476007338056, 0.20791169081775973}}, {{-1., 0}}, 
  {{-0.9781476007338057, -0.20791169081775907}}, 
  {{-0.9135454576426011, -0.4067366430757998}}, 
  {{-0.8090169943749476, -0.587785252292473}}, 
  {{-0.6691306063588585, -0.743144825477394}}, 
  {{-0.5000000000000004, -0.8660254037844384}}, 
  {{-0.30901699437494756, -0.9510565162951535}}, 
  {{-0.10452846326765423, -0.9945218953682733}}, 
  {{0.10452846326765299, -0.9945218953682734}}, 
  {{0.30901699437494723, -0.9510565162951536}}, 
  {{0.49999999999999933, -0.866025403784439}}, 
  {{0.6691306063588578, -0.7431448254773946}}, 
  {{0.8090169943749473, -0.5877852522924734}}, 
  {{0.9135454576426005, -0.40673664307580093}}, 
  {{0.9781476007338056, -0.20791169081775987}}, {{1., 0}}}]
LexicographicPermutations[0] := {{}}
LexicographicPermutations[1] := {{1}}

LexicographicPermutations[n_Integer?Positive] := LP[n]
LexicographicPermutations[l_List] := Permute[l, LexicographicPermutations[Length[l]] ]
LP = Compile[{{n, _Integer}},
             Module[{l = Range[n], i, j, t},
                    NestList[(i = n-1; While[ #[[i]] > #[[i+1]], i--];
                              j = n; While[ #[[j]] < #[[i]], j--];
                              t = #[[i]]; #[[i]] = #[[j]]; #[[j]] = t;
                              Join[ Take[#,i], Reverse[Drop[#,i]] ])&,
                              l, n!-1
                    ]
             ]
     ]
LexicographicSubsets[{}] := {{ }} 
LexicographicSubsets[l_List] := 
       Block[{$RecursionLimit = Infinity, s = LexicographicSubsets[Rest[l]]}, 
             Join[{{}}, Map[Prepend[#, l[[1]]] &, s], Rest[s]]
       ]

LexicographicSubsets[0] := {{ }} 
LexicographicSubsets[n_Integer] := LexicographicSubsets[Range[n]]
LineGraph[g_Graph] :=
        Module[{e = Sort[Edges[g]], ef, eb, c,
                v = Vertices[g]},
               ef = GroupEdgePositions[e, V[g]];
               eb = GroupEdgePositions[ Map[Reverse,e], V[g]];
               c  = Table[Rest[Union[ef[[i]], eb[[i]]]], {i, V[g]}];
               Graph[Union[
                       Flatten[Map[Table[{{#[[i]], #[[j]]}}, {i, Length[#]-1}, {j, i+1, Length[#]}]&, c], 2]
                     ],
                     Map[({(v[[ #[[1]] ]] + v[[ #[[2]] ]]) / 2})&, e]
               ]
        ]
ListGraphs[n_Integer?Positive, m_Integer] := 
    Module[{allBitVectors, distinctBitVectors, graphs, s = KSubsets[Range[n], 2]}, 
           allBitVectors = Permutations[Join[Table[1, {m}], Table[0, {n(n - 1)/2 - m}]]];
           distinctBitVectors = OrbitRepresentatives[KSubsetGroup[
                                    SymmetricGroup[n], s], allBitVectors
                                ];
           Map[FromUnorderedPairs[#, CircularEmbedding[n]]&,
               Map[s[[#]] &, Map[Flatten[Position[#, 1]] &, distinctBitVectors]]
           ]
    ]

ListGraphs[n_Integer?Positive] :=
        Flatten[Table[ListGraphs[n, m], {m, 0, n(n-1)/2}], 1]

ListGraphs[n_Integer?Positive, m_Integer, Directed] := 
    Module[{allBitVectors, distinctBitVectors, graphs, 
            s = Complement[Flatten[Table[{i, j}, {i, n}, {j, n}], 1], Table[{i, i}, {i, n}]
                ]
           }, 
            allBitVectors = Permutations[Join[Table[1, {m}], Table[0, {n(n - 1) - m}]]];
            distinctBitVectors = OrbitRepresentatives[
                                     KSubsetGroup[SymmetricGroup[n],s,Ordered], 
                                     allBitVectors
                                 ];
            Map[FromOrderedPairs[#, CircularEmbedding[n]]&,
                Map[s[[#]] &, Map[Flatten[Position[#, 1]] &, distinctBitVectors]]
            ]
    ]

ListGraphs[n_Integer?Positive, Directed] :=
        Flatten[Table[ListGraphs[n, m, Directed], {m, 0, n(n-1)}], 1]
ListNecklaces[n_Integer?Positive, c_, Cyclic] :=
        OrbitRepresentatives[CyclicGroup[n], Permutations[c]]

ListNecklaces[n_Integer?Positive, c_, Dihedral] :=
        OrbitRepresentatives[DihedralGroup[n], Permutations[c]]
Lock1Q[a_List,b_List] :=
	Module[{bk, aj},
		bk = Min[ Select[Drop[b,-1], (#>First[a])&] ];
		aj = Min[ Select[a, (# > bk)&] ];
		(aj < Max[b])
	]
LockQ[a_List,b_List] := Lock1Q[a,b] || Lock1Q[b,a]
LockQ[a_List, b_List]/;(a === b && Length[a] > 2) := True
LongestIncreasingSubsequence[{}] := {}
LongestIncreasingSubsequence[p_?PermutationQ] :=
	Module[{c,x,xlast},
		c = TableauClasses[p];
		xlast = x = First[ Last[c] ];
		Append[Reverse[Map[(x = First[Intersection[#, Take[p, Position[p,x][[1,1]]]]])&,
                                   Reverse[Drop[c,-1]]
				]
			],
			xlast
		]
	]
LowerBoundTSP[g_Graph] := Apply[Plus, Map[Min, ToAdjacencyLists[g, EdgeWeight] /. {_Integer, x_Integer} -> x]]
M::obsolete = "Usage of Directed as a second argument to M is obsolete."
M[Graph[e_List, _List, ___?OptionQ]] := Length[e]
M[g_Graph, Directed] := (Message[M::obsolete]; M[g])
MakeDirected[g_Graph?UndirectedQ] := 
        SetGraphOptions[ChangeEdges[g, Double[Edges[g]]], EdgeDirection->True]

MakeDirected[g_Graph?UndirectedQ, All] := 
        SetGraphOptions[ChangeEdges[g, Double[Edges[g, All], All]], EdgeDirection->True]
Options[MakeGraph] = {Type -> Directed, VertexLabel->False}

MakeGraph1[v_List, f_] :=
        Table[If [Apply[f,{v[[i]],v[[j]]}]===True, 1, 0],
                  {i,Length[v]},
                  {j,Length[v]}
        ]

MakeGraph[v_List, f_, opts___?OptionQ] :=
        Module[{type, label, g, l},
               {type, label} = {Type, VertexLabel} /. Flatten[{opts, Options[MakeGraph]}];
               g = If[type === Directed,
                      FromAdjacencyMatrix[MakeGraph1[v, f], Type -> Directed],
                      FromAdjacencyMatrix[MakeGraph1[v, f], Type -> Undirected]
                   ];
               If[(label === On) || (label === True),
                  l = Map[StringJoin[Map[ToString, #]]&, v];
                  SetVertexLabels[g, l],
                  g
               ]
        ]
MakeLevel[{},_,_,rank_] := rank

MakeLevel[l_List,lvl_,adjm_List,r_List] :=
  Module[ {rank=r, v, lst=l }, 
          rank = SetLevel[lst,lvl,rank];  (* make this level ready *)
          While[ lst != {},
                 v = First[lst];
                 rank = MakeLevel[adjm[[v]], lvl+1,adjm,rank]; 
                 lst = Rest[lst];
          ];                            
          rank
  ]
MakeSimple[g_Graph] := MakeUndirected[RemoveMultipleEdges[RemoveSelfLoops[g]]]
MakeUndirected[g_Graph] := 
        If[UndirectedQ[g],
           g,
           RemoveMultipleEdges[
              SetGraphOptions[ 
                   ChangeEdges[g, Edges[g,All] /. {x_Integer, y_Integer} :> Sort[{x, y}]], 
                   EdgeDirection -> False
              ]
           ]
        ]
MaximalMatching[g_Graph] :=
	Module[{match={}},
		Scan[
			(If [Intersection[#,match]=={}, match=Join[match,#]])&,
			Edges[g]
		];
		Partition[match,2]
	]
 
MaximumAntichain[g_Graph] := MaximumIndependentSet[MakeUndirected[TransitiveClosure[g]]]
MaximumClique[g_Graph, k_Integer] := {} /; (V[g] == 0)
MaximumClique[g_Graph, k_Integer] := {1} /; ((V[g] == 1) && (k == 1))
MaximumClique[g_Graph, k_Integer] := {} /; ((V[g] == 1) && (k != 1))
MaximumClique[g_Graph, k_Integer] := MaximumClique[MakeSimple[g], k] /; (!SimpleQ[g] || !UndirectedQ[g])

MaximumClique[g_Graph,k_Integer] :=
	Module[{e = ToAdjacencyLists[g]},
               Flatten[
                  Position[
                     Backtrack[
                        Table[{True,False}, {V[g]}],
                        ((Last[#] == False)||
                         ((Count[#,True]<=k) &&
                          (Length[
                              Intersection[Flatten[Position[#,True]], e[[ Length[#]]]]
                           ] == (Count[#,True]-1)
                          )
                         )
                        )&,
                        ((Count[#,True]==k) && 
                         ((Last[#] == False) ||
                          (Length[Intersection[Flatten[Position[#,True]], e[[ Length[#]]]]
                           ] == (Count[#,True]-1)
                          )
                         )
                        )&,
                        First
                     ],
                     True
                  ]
               ]
	]

MaximumClique[g_Graph] := {} /; (V[g] == 0)
MaximumClique[g_Graph] := {1} /; (V[g] == 1)
MaximumClique[g_Graph] := {1} /; EmptyQ[g]
MaximumClique[g_Graph] :=  Range[V[g]] /; IdenticalQ[MakeSimple[g], CompleteGraph[V[g]]] 
MaximumClique[g_Graph] := MaximumClique[MakeSimple[g]] /; (!SimpleQ[g] || !UndirectedQ[g])

MaximumClique[g_Graph] := 
        Module[{oldClique = First[Edges[g]], c, d = Max[Degrees[g]]}, 
               Do[c = MaximumClique[g, i]; If[c == {}, Break[], oldClique = c ], {i, 3, d+1}];
               oldClique 
        ]
MaximumColorDegreeVertices[e_List,color_List] :=
	Module[{n=Length[color],l,i,x},
		l = Table[ Count[e[[i]], _?(Function[x,color[[x]]!=0])], {i,n}];
		Do [ 
			If [color[[i]]!=0, l[[i]] = -1],
			{i,n}
		];
		Flatten[ Position[ l, Max[l] ] ]
	]
MaximumIndependentSet[g_Graph] := Complement[Range[V[g]], MinimumVertexCover[g]]
MaximumSpanningTree[g_Graph] := MinimumSpanningTree[Map[{First[#], -Last[#]}&, Edges[g, EdgeWeight]], g]
McGeeGraph := 
 Graph[{{{1, 2}}, {{1, 8}}, {{1, 24}}, {{2, 3}}, {{2, 19}}, {{3, 4}}, 
  {{3, 15}}, {{4, 5}}, {{4, 11}}, {{5, 6}}, {{5, 22}}, {{6, 7}}, {{6, 18}}, 
  {{7, 8}}, {{7, 14}}, {{8, 9}}, {{9, 10}}, {{9, 21}}, {{10, 11}}, 
  {{10, 17}}, {{11, 12}}, {{12, 13}}, {{12, 24}}, {{13, 14}}, {{13, 20}}, 
  {{14, 15}}, {{15, 16}}, {{16, 17}}, {{16, 23}}, {{17, 18}}, {{18, 19}}, 
  {{19, 20}}, {{20, 21}}, {{21, 22}}, {{22, 23}}, {{23, 24}}}, 
 {{{0.9659258262890683, 0.25881904510252074}}, 
  {{0.8660254037844387, 0.49999999999999994}}, 
  {{0.7071067811865476, 0.7071067811865475}}, 
  {{0.5000000000000001, 0.8660254037844386}}, 
  {{0.25881904510252096, 0.9659258262890682}}, {{0, 1.}}, 
  {{-0.25881904510252063, 0.9659258262890683}}, 
  {{-0.4999999999999998, 0.8660254037844387}}, 
  {{-0.7071067811865475, 0.7071067811865476}}, 
  {{-0.8660254037844385, 0.5000000000000003}}, 
  {{-0.9659258262890682, 0.258819045102521}}, {{-1., 0}}, 
  {{-0.9659258262890684, -0.25881904510252035}}, 
  {{-0.8660254037844388, -0.4999999999999997}}, 
  {{-0.7071067811865479, -0.7071067811865471}}, 
  {{-0.5000000000000004, -0.8660254037844384}}, 
  {{-0.2588190451025215, -0.9659258262890681}}, {{0, -1.}}, 
  {{0.2588190451025203, -0.9659258262890684}}, 
  {{0.49999999999999933, -0.866025403784439}}, 
  {{0.7071067811865474, -0.7071067811865477}}, 
  {{0.8660254037844384, -0.5000000000000004}}, 
  {{0.9659258262890681, -0.25881904510252157}}, {{1., 0}}}]
MeredithGraph := 
 Graph[{{{1, 5}}, {{1, 6}}, {{1, 7}}, {{2, 5}}, {{2, 6}}, {{2, 7}}, {{3, 5}}, 
  {{3, 6}}, {{3, 7}}, {{4, 5}}, {{4, 6}}, {{4, 7}}, {{8, 12}}, {{8, 13}}, 
  {{8, 14}}, {{9, 12}}, {{9, 13}}, {{9, 14}}, {{10, 12}}, {{10, 13}}, 
  {{10, 14}}, {{11, 12}}, {{11, 13}}, {{11, 14}}, {{15, 19}}, {{15, 20}}, 
  {{15, 21}}, {{16, 19}}, {{16, 20}}, {{16, 21}}, {{17, 19}}, {{17, 20}}, 
  {{17, 21}}, {{18, 19}}, {{18, 20}}, {{18, 21}}, {{22, 26}}, {{22, 27}}, 
  {{22, 28}}, {{23, 26}}, {{23, 27}}, {{23, 28}}, {{24, 26}}, {{24, 27}}, 
  {{24, 28}}, {{25, 26}}, {{25, 27}}, {{25, 28}}, {{29, 33}}, {{29, 34}}, 
  {{29, 35}}, {{30, 33}}, {{30, 34}}, {{30, 35}}, {{31, 33}}, {{31, 34}}, 
  {{31, 35}}, {{32, 33}}, {{32, 34}}, {{32, 35}}, {{36, 40}}, {{36, 41}}, 
  {{36, 42}}, {{37, 40}}, {{37, 41}}, {{37, 42}}, {{38, 40}}, {{38, 41}}, 
  {{38, 42}}, {{39, 40}}, {{39, 41}}, {{39, 42}}, {{43, 47}}, {{43, 48}}, 
  {{43, 49}}, {{44, 47}}, {{44, 48}}, {{44, 49}}, {{45, 47}}, {{45, 48}}, 
  {{45, 49}}, {{46, 47}}, {{46, 48}}, {{46, 49}}, {{50, 54}}, {{50, 55}}, 
  {{50, 56}}, {{51, 54}}, {{51, 55}}, {{51, 56}}, {{52, 54}}, {{52, 55}}, 
  {{52, 56}}, {{53, 54}}, {{53, 55}}, {{53, 56}}, {{57, 61}}, {{57, 62}}, 
  {{57, 63}}, {{58, 61}}, {{58, 62}}, {{58, 63}}, {{59, 61}}, {{59, 62}}, 
  {{59, 63}}, {{60, 61}}, {{60, 62}}, {{60, 63}}, {{64, 68}}, {{64, 69}}, 
  {{64, 70}}, {{65, 68}}, {{65, 69}}, {{65, 70}}, {{66, 68}}, {{66, 69}}, 
  {{66, 70}}, {{67, 68}}, {{67, 69}}, {{67, 70}}, {{3, 51}}, {{2, 52}}, 
  {{10, 58}}, {{9, 59}}, {{17, 65}}, {{16, 66}}, {{24, 37}}, {{23, 38}}, 
  {{31, 44}}, {{30, 45}}, {{4, 22}}, {{8, 25}}, {{15, 32}}, {{1, 18}}, 
  {{11, 29}}, {{39, 43}}, {{36, 67}}, {{60, 64}}, {{53, 57}}, {{46, 50}}}, 
 {{{-0.25093605216412773, -5.626403722878969}}, 
  {{-1.2019925684592812, -5.3173867285040215}}, 
  {{-2.153049084754435, -5.008369734129074}}, 
  {{-3.1041056010495884, -4.699352739754127}}, 
  {{-0.4174473159367571, -4.520838709396341}}, 
  {{-1.3685038322319105, -4.211821715021394}}, 
  {{-2.319560348527064, -3.9028047206464462}}, 
  {{5.273484419331281, -1.9773087351681486}}, 
  {{4.685699167038807, -2.786325729543096}}, 
  {{4.097913914746334, -3.5953427239180433}}, 
  {{3.510128662453861, -4.404359718292991}}, 
  {{4.1705747988100965, -1.794031980063149}}, 
  {{3.5827895465176236, -2.6030489744380962}}, 
  {{2.9950042942251502, -3.4120659688130437}}, 
  {{3.510128662453863, 4.404359718292988}}, 
  {{4.097913914746336, 3.595342723918041}}, 
  {{4.685699167038809, 2.7863257295430937}}, 
  {{5.273484419331282, 1.9773087351681466}}, 
  {{2.995004294225152, 3.4120659688130415}}, 
  {{3.5827895465176254, 2.603048974438094}}, 
  {{4.170574798810099, 1.7940319800631468}}, 
  {{-3.104105601049586, 4.6993527397541275}}, 
  {{-2.1530490847544326, 5.008369734129075}}, 
  {{-1.201992568459279, 5.317386728504022}}, 
  {{-0.2509360521641253, 5.626403722878969}}, 
  {{-2.3195603485270624, 3.9028047206464476}}, 
  {{-1.3685038322319085, 4.211821715021395}}, 
  {{-0.4174473159367549, 4.520838709396342}}, 
  {{-5.428571428571429, -1.4999999999999982}}, 
  {{-5.428571428571429, -0.49999999999999806}}, 
  {{-5.428571428571429, 0.500000000000002}}, 
  {{-5.428571428571428, 1.5000000000000018}}, 
  {{-4.428571428571429, -0.9999999999999983}}, 
  {{-4.428571428571429, 1.6969545188681513*^-15}}, 
  {{-4.428571428571429, 1.0000000000000018}}, 
  {{-1.5311493145746229, 9.566495004673177}}, 
  {{-2.4822058308697765, 9.25747801029823}}, 
  {{-3.43326234716493, 8.948461015923282}}, 
  {{-4.384318863460084, 8.639444021548336}}, 
  {{-2.315694567097147, 10.363043023780858}}, 
  {{-3.266751083392301, 10.05402602940591}}, 
  {{-4.217807599687454, 9.745009035030963}}, 
  {{-9.571428571428571, 1.5000000000000013}}, 
  {{-9.571428571428571, 0.5000000000000012}}, 
  {{-9.571428571428571, -0.4999999999999988}}, 
  {{-9.571428571428571, -1.4999999999999987}}, 
  {{-10.571428571428571, 1.0000000000000013}}, 
  {{-10.571428571428571, 1.2945838597550844*^-15}}, 
  {{-10.571428571428571, -0.9999999999999987}}, 
  {{-4.384318863460085, -8.639444021548334}}, 
  {{-3.433262347164932, -8.94846101592328}}, 
  {{-2.4822058308697783, -9.257478010298229}}, 
  {{-1.5311493145746247, -9.566495004673175}}, 
  {{-4.217807599687456, -9.74500903503096}}, 
  {{-3.2667510833923026, -10.054026029405907}}, 
  {{-2.315694567097149, -10.363043023780856}}, 
  {{6.8617704962929285, -6.839470049218952}}, 
  {{7.449555748585402, -6.0304530548440045}}, 
  {{8.037341000877875, -5.221436060469057}}, 
  {{8.625126253170349, -4.41241906609411}}, 
  {{7.964680116814112, -7.022746804323951}}, 
  {{8.552465369106585, -6.213729809949004}}, 
  {{9.140250621399058, -5.404712815574057}}, 
  {{8.62512625317035, 4.412419066094105}}, 
  {{8.037341000877879, 5.221436060469053}}, 
  {{7.449555748585405, 6.030453054844}}, 
  {{6.861770496292932, 6.8394700492189475}}, 
  {{9.140250621399062, 5.404712815574052}}, 
  {{8.552465369106589, 6.213729809948999}}, 
  {{7.964680116814116, 7.022746804323947}}}]
Merge[gd_List, ld_List] :=
        Join[Table[If[Position[ld,  Rule[gd[[i]][[1]], _]]=={},
                      gd[[i]],
                      {j} = First[Position[ld,  Rule[gd[[i]][[1]], _]]];
                      ld[[j]]
                   ],
                   {i, Length[gd]}
             ],
             Flatten[Table[If[MemberQ[gd, Rule[ld[[i]][[1]], _]],
                              {},
                              {ld[[i]]}
                           ],
                           {i, Length[ld]}
                     ],
                     1
             ]
        ]

Merge[rl1_List, rl2_List, rls__] := Merge[Merge[rl1, rl2], rls]
MinOp[l_List,f_] :=
	Module[{min=First[l]},
		Scan[ (If[ Apply[f,{#,min}], min = #])&, l];
		Return[min];
	]
MinimumChainPartition[g_Graph] :=
	ConnectedComponents[
		FromUnorderedPairs[
			Map[(#-{0,V[g]})&, BipartiteMatching[DilworthGraph[g]]],
			Vertices[g, All]
		]
	]
MinimumChangePermutations[l_List] := LexicographicPermutations[l] /; (Length[l] < 2)
MinimumChangePermutations[l_List] :=
	Module[{i=1,c,p=l,n=Length[l],k},
		c = Table[1,{n}];
		Join[{l},
                     Table[While [ c[[i]] >= i, c[[i]] = 1; i++];
                           If[OddQ[i], k=1, k=c[[i]] ];
                           {p[[i]],p[[k]]} = {p[[k]],p[[i]]};
                           c[[i]]++; i = 2; p,
                           {n!-1}
                     ]
                ]
        ] 
MinimumChangePermutations[n_Integer] := MinimumChangePermutations[Range[n]]
MinimumEdgeLength[v_List,pairs_List] :=
        Max[ Select[
                Chop[ Map[(Sqrt[ N[(v[[#[[1]]]]-v[[#[[2]]]]) .
                        (v[[#[[1]]]]-v[[#[[2]]]])] ])&,pairs] ],
                (# > 0)&
        ], 0.001 ]
MinimumSpanningTree[e_List, g_Graph] :=
	Module[{ne=Sort[e, (#1[[2]] <= #2[[2]])&],
                s=InitializeUnionFind[V[g]]},
		ChangeEdges[g,
			Select[Map[First, ne],
                               (If[FindSet[#[[1]],s]!=FindSet[#[[2]], s],
                                    s=UnionSet[#[[1]],#[[2]], s]; True, False
				])&
			]
		]
	] 
MinimumSpanningTree[g_Graph] := MinimumSpanningTree[ Edges[g, EdgeWeight], g ] /; UndirectedQ[g]
MinimumVertexColoring[g_Graph] := MinimumVertexColoring[MakeSimple[g]] /; !UndirectedQ[g]

MinimumVertexColoring[g_Graph] := Table[1, {V[g]}] /; EmptyQ[g]
MinimumVertexColoring[g_Graph] := TwoColoring[g] /; BipartiteQ[g]
MinimumVertexColoring[g_Graph] := {} /; (V[g] == 0)
MinimumVertexColoring[g_Graph] := {1} /; (V[g] == 1)
MinimumVertexColoring[g_Graph] := 
       Module[{col, oldCol, c},
              c = Max[oldCol = VertexColoring[g, Algorithm->Brelaz]];
              col = oldCol;
              For[i = c-1, i >= 3, i--, 
                  col = MinimumVertexColoring[g, i];
                  If[col == {}, Return[oldCol], oldCol = col]
              ];
              col
       ]

MinimumVertexColoring[g_Graph, k_Integer, number_Symbol:One] := {} /; (V[g] == 0)
MinimumVertexColoring[g_Graph, k_Integer, number_Symbol:One] := {1} /; ((V[g] == 1) && (k == 1))
MinimumVertexColoring[g_Graph, k_Integer, number_Symbol:One] := {} /; ((V[g] == 1) && (k != 1))
MinimumVertexColoring[g_Graph, k_Integer, number_Symbol:One] := 
       Module[{e}, 
              e = ToAdjacencyLists[g, Type->Simple];
              Backtrack[
                  Join[{{1}}, Table[Range[k], {V[g] - 1}]], 
                  (!MemberQ[#[[PriorEdges[e[[Length[#]]], Length[#]]]], Last[#]]) &, 
                  (!MemberQ[#[[PriorEdges[e[[Length[#]]], Length[#]]]], Last[#]]) &, 
                  number
              ]
       ]

PriorEdges[l_List, k_Integer] := Select[l, (# <= k) &]
MinimumVertexCover[g_Graph] := {} /; EmptyQ[g]
MinimumVertexCover[g_Graph] := Flatten[Position[Last[BipartiteMatchingAndCover[g]], 1]]/; BipartiteQ[g]
MinimumVertexCover[g_Graph] := Complement[ Range[V[g]], MaximumClique[ GraphComplement[g] ] ]
MultipleEdgesQ[g_Graph] := Module[{e = Edges[g]}, Length[e] != Length[Union[e]]]
MultiplicationTable[elems_List, op_] :=
        With[{rules = Append[Thread[elems -> Range[Length[elems]]], _ -> 0]},
             Outer[Replace[op[##], rules] &, elems, elems, 1]]
MycielskiGraph[1] := CompleteGraph[1]
MycielskiGraph[2] := CompleteGraph[2]
MycielskiGraph[3] := Cycle[5]
MycielskiGraph[4] := GrotztschGraph
MycielskiGraph[k_Integer] := 
        Module[{g = MycielskiGraph[k - 1], al, n}, 
               n = V[g]; 
               al = ToAdjacencyLists[g]; 
               FromAdjacencyLists[
                   Join[Map[Join[#, n + #] &, al], 
                        Table[Append[al[[i]], 2n + 1], {i, n}], 
                        {Range[n + 1, 2n]}
                   ] 
               ]
        ] /; (k > 4)
Nary[0,b_]:={}
Nary[n_,b_]:=
    Module[{d = IntegerDigits[n,b], p},
           While[(p = Flatten[Position[d,_?NonPositive]]) != {},
                 d[[Last[p]]]+= b; 
                 d[[Last[p]-1]]-= 1; 
                 If[First[d] == 0, d = Rest[d]];
           ];
           d
    ]
NecklacePolynomial[n_Integer?Positive, c_, Cyclic] :=
        OrbitInventory[CyclicGroupIndex[n, x], x, c]

NecklacePolynomial[n_Integer?Positive, c_, Dihedral] :=
        OrbitInventory[DihedralGroupIndex[n, x], x, c]
Neighborhood[g_Graph, v_Integer?Positive, 0] := {v} /; (1 <= v) && (v <= V[g])

Neighborhood[g_Graph, v_Integer?Positive, k_Integer?Positive] := 
       Neighborhood[ToAdjacencyLists[g], v, k] /; (1 <= v) && (v <= V[g])

Neighborhood[al_List, v_Integer?Positive, 0] := {v} /; (1 <= v)&&(v<=Length[al])
Neighborhood[al_List, v_Integer?Positive, k_Integer?Positive] := 
       Module[{n = {v}},
              Do[n = Union[n, Flatten[al[[ n ]], 1]], {i, k}]; 
              n
       ] /; (1 <= v) && (v <= Length[al])
NetworkFlow[g_Graph, s_Integer, t_Integer, All] :=
	Block[{al=ToAdjacencyLists[g,EdgeWeight],f,rg,p,pe,pf,pb,i,j},
              f = Map[ Map[Function[x, {First[x], 0}], #]&, al ];
              rg = ResidualFlowGraph[g,al,f]; 
              While[(p = BreadthFirstTraversal[rg,s,t]) != {},
                    m = Min[ GetEdgeWeights[rg, pe = Partition[p,2,1]] ];
                    Scan[({i,j} = {#[[1]], #[[2]]};
                         pf = Position[f[[i]], {j, _}];
                         pb = Position[f[[j]], {i, _}];
                         If[(pb != {}) && (f[[j, pb[[1,1]], 2]] >= m),
                            f[[j, pb[[1,1]],2]]-=m,
                            f[[i, pf[[1,1]],2]]+=m
                         ])&,
                         pe
                    ];
                    rg = ResidualFlowGraph[g,al,f]; 
              ];
              f
	] /; (1 <= s) && (s <= V[g]) && (1 <= t) && (t <= V[g])
NetworkFlow[g_Graph, s_Integer, t_Integer, Edge] :=
        Module[{f = NetworkFlow[g,s,t, All]},
               Flatten[Table[
                             Map[{{i, First[#]}, Last[#]}&,
                                 Select[f[[i]], (Last[#] > 0)&]
                             ],
                             {i, Length[f]}
                       ], 1
               ]
        ] /; (1 <= s) && (s <= V[g]) && (1 <= t) && (t <= V[g])

NetworkFlow[g_Graph, s_Integer, t_Integer, Cut] := 
        Module[{e = Edges[g], rg = ResidualFlowGraph[g, NetworkFlow[g, s, t, All]]},
               u = DepthFirstTraversal[rg, s]; v = Complement[Range[V[g]], u];
               cut = Select[e, MemberQ[u, #[[1]]] && MemberQ[v, #[[2]]] &];
               If[UndirectedQ[g], 
                  cut = Join[cut, Select[e, MemberQ[v, #[[1]]] && MemberQ[u, #[[2]]] &]]
               ];
               cut
        ]

NetworkFlow[g_Graph, s_Integer, t_Integer] :=
        Module[{f = NetworkFlow[g,s,t, All]},
               Apply[Plus, Map[Last, f[[s]]]] -
               Apply[Plus, Map[Last, Select[Flatten[f, 1], 
                                            (First[#]==s)&
                                     ]
                           ]
               ]
        ] /; (1 <= s) && (s <= V[g]) && (1 <= t) && (t <= V[g])
NetworkFlowEdges[g_Graph, source_Integer, sink_Integer] := 
        Module[{f = NetworkFlow[g, source, sink, Edge], e, v},
               {e, v} = {Map[First, f], Map[Last, f]};
               ToAdjacencyMatrix[SetEdgeWeights[FromUnorderedPairs[e], v], EdgeWeight] /. Infinity -> 0
        ]
NextBinarySubset[set_List,subset_List] := UnrankBinarySubset[RankBinarySubset[set,subset]+1, set]
NextComposition[l_List] :=
	Append[Table[0,{Length[l]-1}], Apply[Plus, l]] /; First[l]==Apply[Plus,l]

NextComposition[l_List] := NC[l]
NC = Compile[{{l, _Integer, 1}},  
             Module[{n = Apply[Plus, l], nl = l, t = Length[l]}, 
                    While[l[[t]] == 0, t--];
                    nl[[t-1]]++;
                    Do[nl[[i]] = 0, {i, t, Length[l]}];
                    nl[[Length[l]]] = Apply[Plus, Take[l, -(Length[l] - t + 1)]] - 1; nl
             ]
     ]
NextGrayCodeSubset[l_List, s_List] := 
        If[ MemberQ[s,l[[1]]], Rest[s], Prepend[s,l[[1]] ] ] /; EvenQ[Length[s]]

NextGrayCodeSubset[l_List, s_List] := 
        Module[{i = 1}, 
               While[ ! MemberQ[s, l[[i]] ], i++]; 
               If[MemberQ[s, l[[i+1]] ], Rest[s], Insert[s, l[[i+1]], 2 ] ]]
NKS = Compile[{{n, _Integer}, {ss, _Integer, 1}}, 
              Module[{h = Length[ss], x = n},
                     While[x === ss[[h]], h--; x--];
                     Join[Take[ss, h - 1], Range[ss[[h]]+1, ss[[h]]+Length[ss]-h+1]]
              ]
      ]

NextKSubset[s_List,ss_List] := Take[s,Length[ss]] /; (Take[s,-Length[ss]] === ss)
NextKSubset[s_List,ss_List] :=
        Map[s[[#]] &, NKS[Length[s], Table[Position[s, ss[[i]]][[1, 1]], {i, Length[ss]}]]]
NextLexicographicSubset[n_Integer, s_List] := NextLexicographicSubset[Range[n], s]

NextLexicographicSubset[l_List, {}] := {First[l]}

NextLexicographicSubset[l_List, s_List] := 
       Module[{elem}, 
              If[Last[s] === Last[l], 
                 (elem = s[[Length[s] - 1]]; 
                 Append[Drop[s,-2], l[[Position[l, elem][[1, 1]] + 1]]]), 
                 Append[s, l[[Position[l, Last[s]][[1, 1]] + 1]] ]
              ]
       ]
NextPartition[p_List] := Join[Drop[p,-1],{Last[p]-1,1}]  /; (Last[p] > 1)

NextPartition[p_List] := {Apply[Plus,p]}  /; (Max[p] == 1)

NextPartition[p_List] := NPT[p];

NPT = Compile[{{p, _Integer, 1}}, 
              Module[{k = Length[p], q = Table[0, {Length[p] + 2}], j, m, r}, 
                     j = Position[p, 1][[1, 1]] - 1;
                     Do[q[[i]] = p[[i]], {i, j - 1}];
                     m = Quotient[p[[j]] + (k - j), p[[j]] - 1];
                     Do[q[[i]] = p[[j]] - 1, {i, j, j + m - 1}];
                     r = Mod[p[[j]] + (k - j), p[[j]] - 1];
                     q[[j + m]] = r;
                     DeleteCases[q, 0]
              ]
      ]
NextPermutation[l_List] := Sort[l] /; (l === Reverse[Sort[l]])

NextPermutation[l_List] := 
        Module[{n = Length[l], i, j, t, nl = l},
               i = n-1; While[ Order[nl[[i]], nl[[i+1]]] == -1, i--];
               j = n; While[ Order[nl[[j]], nl[[i]]] == 1, j--];
               {nl[[i]], nl[[j]]} = {nl[[j]], nl[[i]]};
               Join[ Take[nl,i], Reverse[Drop[nl,i]] ]
        ]
NextSubset[set_List,subset_List] := 
       UnrankBinarySubset[RankBinarySubset[set,subset]+1, set]
NextTableau[t_?TableauQ] :=
	Module[{s,y,row,j,count=0,tj,i,n=Max[t]},
		y = TableauToYVector[t];
		For [j=2, (j<n)  && (y[[j]]>=y[[j-1]]), j++];
		If [y[[j]] >= y[[j-1]],
			Return[ FirstLexicographicTableau[ ShapeOfTableau[t] ] ]
		];
		s = ShapeOfTableau[ Table[Select[t[[i]],(#<=j)&], {i,Length[t]}] ];
		{row} = Last[ Position[ s, s[[ Position[t,j] [[1,1]] + 1 ]] ] ];
		s[[row]] --;
		tj = FirstLexicographicTableau[s];
		If[ Length[tj] < row,
			tj = Append[tj,{j}],
			tj[[row]] = Append[tj[[row]],j]
		];
		Join[
			Table[
				Join[tj[[i]],Select[t[[i]],(#>j)&]],
				{i,Length[tj]}
			],
			Table[t[[i]],{i,Length[tj]+1,Length[t]}]
		]
	]

NoPerfectMatchingGraph :=
 Graph[{{{1, 2}}, {{1, 3}}, {{1, 4}}, {{2, 3}}, {{2, 4}}, {{3, 4}}, {{3, 5}}, 
  {{4, 5}}, {{5, 6}}, {{6, 7}}, {{6, 8}}, {{7, 13}}, {{7, 14}}, {{8, 9}}, 
  {{8, 10}}, {{9, 10}}, {{9, 11}}, {{9, 12}}, {{10, 11}}, {{10, 12}}, 
  {{11, 12}}, {{13, 14}}, {{13, 15}}, {{13, 16}}, {{14, 15}}, {{14, 16}}, 
  {{15, 16}}}, {{{0.3, 0.902}}, {{0.582, 0.902}}, {{0.3, 0.73}}, 
  {{0.582, 0.73}}, {{0.44, 0.664}}, {{0.44, 0.528}}, {{0.534, 0.434}}, 
  {{0.334, 0.434}}, {{0.178, 0.434}}, {{0.342, 0.288}}, {{0.216, 0.182}}, 
  {{0.046, 0.342}}, {{0.716, 0.434}}, {{0.534, 0.28}}, {{0.65, 0.168}}, 
  {{0.834, 0.33}}}]
NonLineGraphs := 
 Graph[{{{1, 2}}, {{1, 3}}, {{1, 4}}, {{5, 7}}, {{5, 8}}, {{6, 7}}, {{6, 8}}, 
  {{7, 8}}, {{8, 9}}, {{10, 12}}, {{10, 13}}, {{10, 14}}, {{11, 12}}, 
  {{11, 13}}, {{11, 14}}, {{12, 13}}, {{12, 14}}, {{13, 14}}, {{15, 16}}, 
  {{16, 17}}, {{16, 18}}, {{17, 18}}, {{17, 19}}, {{18, 19}}, {{19, 20}}, 
  {{21, 22}}, {{21, 23}}, {{21, 24}}, {{22, 23}}, {{22, 24}}, {{22, 25}}, 
  {{23, 24}}, {{23, 25}}, {{25, 26}}, {{27, 28}}, {{27, 29}}, {{27, 30}}, 
  {{28, 29}}, {{28, 30}}, {{28, 31}}, {{28, 32}}, {{29, 30}}, {{29, 31}}, 
  {{29, 32}}, {{31, 32}}, {{33, 35}}, {{33, 36}}, {{33, 37}}, {{34, 35}}, 
  {{34, 36}}, {{34, 38}}, {{35, 36}}, {{37, 38}}, {{39, 40}}, {{39, 41}}, 
  {{39, 44}}, {{40, 41}}, {{40, 42}}, {{40, 43}}, {{40, 44}}, {{41, 42}}, 
  {{42, 43}}, {{43, 44}}, {{45, 46}}, {{45, 47}}, {{46, 47}}, {{46, 48}}, 
  {{47, 48}}, {{47, 49}}, {{48, 49}}, {{48, 50}}, {{49, 50}}}, 
 {{{0.104, 0.906}}, {{0.25, 0.982}}, {{0.25, 0.906}}, {{0.244, 0.83}}, 
  {{0.476, 0.974}}, {{0.474, 0.802}}, {{0.366, 0.894}}, {{0.512, 0.894}}, 
  {{0.582, 0.834}}, {{0.798, 0.986}}, {{0.77, 0.828}}, {{0.664, 0.79}}, 
  {{0.808, 0.908}}, {{0.946, 0.79}}, {{0.248, 0.762}}, {{0.08, 0.762}}, 
  {{0.238, 0.664}}, {{0.144, 0.664}}, {{0.076, 0.554}}, {{0.244, 0.554}}, 
  {{0.444, 0.738}}, {{0.356, 0.63}}, {{0.534, 0.63}}, {{0.444, 0.676}}, 
  {{0.442, 0.54}}, {{0.532, 0.54}}, {{0.968, 0.598}}, {{0.828, 0.718}}, 
  {{0.828, 0.474}}, {{0.89, 0.598}}, {{0.77, 0.606}}, {{0.682, 0.606}}, 
  {{0.158, 0.444}}, {{0.15, 0.206}}, {{0.096, 0.328}}, {{0.216, 0.328}}, 
  {{0.288, 0.444}}, {{0.288, 0.204}}, {{0.838, 0.398}}, {{0.838, 0.282}}, 
  {{0.944, 0.32}}, {{0.902, 0.182}}, {{0.77, 0.182}}, {{0.728, 0.318}}, 
  {{0.51, 0.464}}, {{0.422, 0.376}}, {{0.598, 0.376}}, {{0.422, 0.242}}, 
  {{0.598, 0.242}}, {{0.51, 0.154}}}]
NormalizeVertices[v:{{{_?NumericQ, _?NumericQ},___?OptionQ}...}] := 
        Module[{nv = NormalizeVertices[Map[First, v]]},
                Table[{nv[[i]], Apply[Sequence, Rest[v[[i]]]]}, {i, Length[nv]}]
        ]

NormalizeVertices[v:{{_?NumericQ, _?NumericQ}...}] := 
	Module[{v1 = TranslateVertices[v, {-Min[v], -Min[v]}]},
		DilateVertices[v1, 1/Max[v1, 0.01]]
	]

NormalizeVertices[g_Graph] := 
        ChangeVertices[g, NormalizeVertices[Vertices[g]]]
NthPair = Compile[{{n, _Integer}}, 
             Module[{j}, j = Ceiling[(1 + Sqrt[1 + 8n])/2]; {Round[n - (j-1)(j-2)/2], j}]
          ]
NthPermutation[r_Integer, l_List] := UnrankPermutation[r, l]
NthSubset[x_Integer, y_List] := UnrankGrayCodeSubset[x, y]
NumberOfPermutationsByInversions[n_Integer?Positive] := 
       Module[{p = Expand[Product[Cancel[ (z^i -1)/(z-1)], {i, 1, n}]]}, CoefficientList[p, z]]
NumberOfPermutationsByInversions[n_Integer, k_Integer] := 0 /; (k > Binomial[n,2])
NumberOfPermutationsByInversions[n_Integer, 0] := 1 
NumberOfPermutationsByInversions[n_Integer, k_Integer?Positive] := NumberOfPermutationsByInversions[n][[k+1]]
NumberOf2Paths[g_Graph, v_Integer?Positive] := NumberOfKPaths[g, v, 2]
NumberOfCompositions[n_,k_] := Binomial[ n+k-1, n ]
NumberOfDerangements[0] := 1
NumberOfDerangements[n_Integer?Positive] := 
       Block[{$RecursionLimit = Infinity}, n * NumberOfDerangements[n-1] + (-1)^n]
NumberOfDirectedGraphs[0] := 1
NumberOfDirectedGraphs[n_Integer?Positive] :=
        OrbitInventory[PairGroupIndex[SymmetricGroupIndex[n, x], x, Ordered], 
                       x, 2
        ]

NumberOfDirectedGraphs[n_Integer, 0] := 1 /; (n >= 0)

NumberOfDirectedGraphs[n_Integer?Positive, m_Integer] := 
        Coefficient[GraphPolynomial[n, x, Directed], x^m]
NumberOfGraphs[0] := 1
NumberOfGraphs[n_Integer?Positive] := OrbitInventory[PairGroupIndex[SymmetricGroupIndex[n, x], x], x, 2 ]
NumberOfGraphs[n_Integer, 0] := 1 /; (n >= 0)
NumberOfGraphs[n_Integer?Positive, m_Integer] := Coefficient[GraphPolynomial[n, x], x^m]
NumberOfInvolutions[n_Integer] := Module[{k}, n! Sum[1/((n - 2k)! 2^k k!), {k, 0, Quotient[n, 2]}]]
NumberOfKPaths[g_Graph, v_Integer?Positive, 0] := 1 /; (1 <= v) && (v <= V[g])

NumberOfKPaths[g_Graph, v_Integer?Positive, k_Integer?Positive] := 
       NumberOfKPaths[ToAdjacencyLists[g], v, k] 

NumberOfKPaths[al_List, v_Integer?Positive, 0] := 1 /; (1<=v)&&(v<=Length[al])
NumberOfKPaths[al_List, v_Integer?Positive, k_Integer?Positive] := 
       Module[{n = {v}}, 
              Do[n = Flatten[al[[ n ]], 1]  , {i, k}]; 
              Sort[Map[Length, Split[Sort[n]]]] 
       ] /; (1 <= v) && (v <= Length[al]) 
NumberOfNecklaces[n_Integer?Positive, nc_Integer?Positive, Cyclic] :=
        OrbitInventory[CyclicGroupIndex[n, x], x, nc]

NumberOfNecklaces[n_Integer?Positive, nc_Integer?Positive, Dihedral] :=
        OrbitInventory[DihedralGroupIndex[n, x], x, nc]
NumberOfPartitions[n_Integer] := NumberOfPartitions1[n]
NumberOfPartitions1[n_Integer] := 0  /; (n < 0)
NumberOfPartitions1[n_Integer] := 1  /; (n == 0)

NumberOfPartitions1[n_Integer] := 
	Block[{$RecursionLimit = Infinity, k},
              NumberOfPartitions1[n] =
              Sum[(-1)^(k+1) NumberOfPartitions1[n - k (3k-1)/2] +
                  (-1)^(k+1) NumberOfPartitions1[n - k (3k+1)/2],
                  {k, Ceiling[ (1+Sqrt[1.0 + 24n])/6 ], 1, -1}
              ]
	]
NumberOfPartitions[n_Integer, k_Integer] := NumberOfPartitions2[n, k] /; ((n >= 0) && (k >= 0))

NumberOfPartitions2[n_Integer?Positive, 0] := 0 
NumberOfPartitions2[0, k_Integer] := 1 
NumberOfPartitions2[n_Integer?Positive, 1] := 1
NumberOfPartitions2[n_Integer?Positive, k_Integer?Positive] := NumberOfPartitions[n] /; (k >= n)

NumberOfPartitions2[n_Integer, k_Integer] := 
        Block[{$RecursionLimit = Infinity},
               NumberOfPartitions2[n, k] = NumberOfPartitions2[n, k-1] + NumberOfPartitions2[n-k, k]
        ]
NumberOfPermutationsByCycles[n_Integer,m_Integer] := Abs[StirlingS1[n,m]]
NumberOfPermutationsByType[l_List] := (Length[l]!)/Apply[Times, Table[l[[i]]!i^(l[[i]]), {i, Length[l]}]]
NumberOfSpanningTrees[g_Graph] := 0 /; (V[g] == 0)
NumberOfSpanningTrees[g_Graph] := 1 /; (V[g] == 1)
NumberOfSpanningTrees[g_Graph] :=
        Module[{m = ToAdjacencyMatrix[g]},
	       Cofactor[ DiagonalMatrix[Map[(Apply[Plus,#])&,m]] - m, {1,1}]
        ]
NumberOfTableaux[{}] := 1
NumberOfTableaux[s_List] := 
	Module[{row,col,transpose=TransposePartition[s]},
		(Apply[Plus,s])! /
		Product [
			(transpose[[col]]-row+s[[row]]-col+1),
			{row,Length[s]}, {col,s[[row]]}
		]
	]

NumberOfTableaux[n_Integer] := Apply[Plus, Map[NumberOfTableaux, Partitions[n]]]
OctahedralGraph :=
 Graph[{{{1, 2}}, {{1, 3}}, {{2, 3}}, {{4, 5}}, {{4, 6}}, {{5, 6}}, {{1, 4}}, 
  {{1, 6}}, {{2, 4}}, {{2, 5}}, {{3, 5}}, {{3, 6}}}, 
 {{{-0.4999999999999998, 0.8660254037844387}}, 
  {{-0.5000000000000004, -0.8660254037844384}}, {{1., 0}}, 
  {{-0.3333333333333333, 4.082021179407924*^-17}}, 
  {{0.16666666666666644, -0.28867513459481303}}, 
  {{0.16666666666666666, 0.28867513459481287}}}]
OddGraph[n_Integer] := MakeGraph[KSubsets[Range[2n-1],n-1], 
                                 (SameQ[Intersection[#1,#2],{}])&,
                                 Type -> Undirected
                       ] /; (n > 1)
Options[Graph] := {VertexWeight -> 0, EdgeWeight -> 1}
OrbitInventory[ci_?PolynomialQ, x_Symbol, weights_List] :=
        Expand[ci /. Table[x[i] ->  Apply[Plus, Map[#^i&, weights]],
                                  {i, Exponent[ci, x[1]]}
              ]
        ]

OrbitInventory[ci_?PolynomialQ, x_Symbol, r_] :=
        Expand[ci /. Table[x[i] -> r, {i, Exponent[ci, x[1]]} ]]
OrbitRepresentatives[g_List, x_List, f_:Permute] :=
        Module[{y = Orbits[g, x, f]},
               Table[y[[i]][[1]], {i, Length[y]}]
        ] /; ((Length[g] > 0) && (Length[x] > 0)) && (Length[ g[[1]] ] == Length[ x[[1]] ]) 
Orbits[g_List, x_List, f_:Permute] :=
        Module[{y = x, n = Length[g], orbit, out = {}},
               While[y != {},
                     orbit = Table[Apply[f, {y[[1]], g[[i]]}], {i, 1, n}];
                     y = Complement[y, orbit];
                     AppendTo[out, orbit];
                     Length[y]
               ];
               out
        ]
OrientGraph[g_Graph] :=
	Module[{pairs,newg,rest,cc,c,i,e},
		pairs = Flatten[Map[(Partition[#,2,1])&,ExtractCycles[g]],1];
		newg = FromUnorderedPairs[pairs,Vertices[g, All]];
		rest = ToOrderedPairs[ GraphDifference[ g, newg ] ];
		cc = Sort[ConnectedComponents[newg], 
                          (Length[#1]>=Length[#2])&];
		c = First[cc];
		Do[
			e = Select[rest,(MemberQ[c,#[[1]]] &&
					 MemberQ[cc[[i]],#[[2]]])&];
			rest = Complement[rest,e,Map[Reverse,e]];
			c = Union[c,cc[[i]]];
			pairs = Join[pairs, Prepend[ Rest[e],Reverse[e[[1]]] ] ],
			{i,2,Length[cc]}
		];
		FromOrderedPairs[
			Join[pairs, Select[rest,(#[[1]] > #[[2]])&] ],
			Vertices[g, All]
		]
	] /; SameQ[Bridges[g],{}]
OutDegree[g_Graph, n_Integer] := Length[Select[Edges[g], (First[#]==n)&]]
OutDegree[g_Graph] := Map[Length, ToAdjacencyLists[g]]
PairGroup[g_List] := KSubsetGroup[g, KSubsets[Range[Max[g[[1]]]], 2]] /; (Length[g] > 0)

PairGroup[g_List, Ordered] :=
        KSubsetGroup[g,
                     Complement[Flatten[Table[{i, j}, {i, Max[g[[1]]]}, 
                                              {j, Max[g[[1]]]}
                                        ],
                                        1
                                ],
                                Table[{i, i}, {i, Max[g[[1]]]}]
                     ],
                     Ordered
        ] /; (Length[g] > 0)

PairGroupIndex[g_, x_Symbol] := PairGroupIndex[CycleIndex[g, x], x] /; PermutationQ[g[[1]]]

PairGroupIndex[ci_?PolynomialQ, x_Symbol]:=
        Module[{f1,f2,f3,PairCycles},
               f1[x[i1_]^(j1_)] := 1;
               f1[x[i1_]] := 1;
               f1[x[i1_]*x[(i2_)^(j2_)]] := x[LCM[i1, i2]]^(j2*GCD[i1, i2]);
               f1[x[i1_]^(j1_)*x[i2_]] := x[LCM[i1, i2]]^(j1*GCD[i1, i2]);
               f1[x[i1_]*x[i2_]] := x[LCM[i1, i2]]^GCD[i1, i2];
               f1[x[i1_]^(j1_)*x[i2_]^(j2_)] := x[LCM[i1, i2]]^(j1*j2*GCD[i1, i2]);
               f1[(a_)*(t__)] := Block[{$RecursionLimit = Infinity},
                                       Product[f1[a*{t}[[i]]], {i, Length[{t}]}]*
                                       f1[Apply[Times, {t}]]
                                 ];

               f2[x[i1_]^j1_]:=x[i1]^(i1 Binomial[j1,2]);
               f2[x[i1_]]:=1;
               f2[a_  b_ ]:= Block[{$RecursionLimit = Infinity}, f2[a] f2[b]];

               f3[x[i1_]]:=If[OddQ[i1],x[i1]^( (i1-1)/2),
                       x[i1]^( (i1-2)/2) * x[i1/2]];
               f3[x[i1_]^j1_]:=If[OddQ[i1],x[i1]^(j1 (i1-1)/2),
                       x[i1]^(j1 (i1-2)/2) * x[i1/2]^j1];
               f3[a_ b_]:= Block[{$RecursionLimit = Infinity}, f3[a] f3[b]];

               PairCycles[u_ + v_] := Block[{$RecursionLimit = Infinity},
                                            PairCycles[u] + PairCycles[v]
                                      ];
               PairCycles[a_?NumericQ b_]:= Block[{$RecursionLimit = Infinity},
                                                 a PairCycles[b]
                                           ];
               PairCycles[a_]:=f1[a] f2[a] f3[a];
               Expand[PairCycles[Expand[ci]]]
        ]


PairGroupIndex[g_, x_, Ordered] := PairGroupIndex[CycleIndex[g, x], x, Ordered] /; PermutationQ[g[[1]]]

PairGroupIndex[ci_?PolynomialQ, x_, Ordered]:=
        Module[{f1,f2,f3,PairCycles},
               f1[x[i1_]^(j1_)] := 1;
               f1[x[i1_]] := 1;
               f1[x[i1_]*x[(i2_)^(j2_)]] := x[LCM[i1, i2]]^(2*j2*GCD[i1, i2]);
               f1[x[i1_]^(j1_)*x[i2_]] := x[LCM[i1, i2]]^(2*j1*GCD[i1, i2]);
               f1[x[i1_]*x[i2_]] := x[LCM[i1, i2]]^(2*GCD[i1, i2]);
               f1[x[i1_]^(j1_)*x[i2_]^(j2_)] :=
                       x[LCM[i1, i2]]^(2*j1*j2*GCD[i1, i2]);
               f1[(a_)*(t__)] := Block[{$RecursionLimit = Infinity},
                                        Product[f1[a*{t}[[i]]], {i, Length[{t}]}]*
                                        f1[Apply[Times, {t}]]   
                                 ];


               f2[x[i1_]^j1_]:=x[i1]^(i1 j1 (j1-1));
               f2[x[i1_]]:=1;
               f2[a_  b_ ]:= Block[{$RecursionLimit = Infinity}, f2[a] f2[b]];

               f3[x[i1_]]:= x[i1]^(i1-1);
               f3[x[i1_]^j1_]:= x[i1]^(j1(i1-1));
               f3[a_ b_]:= Block[{$RecursionLimit = Infinity}, f3[a] f3[b]];

               PairCycles[u_ + v_] := Block[{$RecursionLimit = Infinity},
                                           PairCycles[u]+ PairCycles[v]
                                     ];
               PairCycles[a_?NumericQ b_] := Block[{$RecursionLimit = Infinity},
                                                  a PairCycles[b]
                                            ];
               PairCycles[a_]:=f1[a] f2[a] f3[a];

               Expand[PairCycles[Expand[ci]]]
        ]
ParentsToPaths[l_List, i_Integer, j_Integer] := 
        Rest[Reverse[FixedPointList[l[[#]] &, j]]]

ParentsToPaths[l_List, i_Integer] := 
        Table[ParentsToPath[l, i, j], {j, Length[l]}]
PartialOrderQ[r_?SquareMatrix] := ReflexiveQ[r] && AntiSymmetricQ[r] && TransitiveQ[r]
PartialOrderQ[g_Graph] := ReflexiveQ[g] && AntiSymmetricQ[g] && TransitiveQ[g]
Options[PartitionLattice] = {Type -> Undirected, VertexLabel -> False}

PartitionLattice[n_Integer?Positive, opts___?OptionQ] := 
        Module[{type, label, s = SetPartitions[n], br},
               {type, label} = {Type, VertexLabel} /. Flatten[{opts, Options[PartitionLattice]}];
               br = CoarserSetPartitionQ[#2, #1]&;
               g = MakeGraph[s, br];
               If[(label === On) || (label === True),
                  g = SetVertexLabels[MakeGraph[s, br], Map[SetPartitionToLabel, s]];
               ];
               If[type === Undirected, HasseDiagram[g], g]
        ]
PartitionQ[p_List] := (Min[p]>0) && Apply[And, Map[IntegerQ,p]]
PartitionQ[n_Integer, p_List] := (Apply[Plus, p] === n) && (Min[p]>0) && Apply[And, Map[IntegerQ,p]]
Partitions[n_Integer] := Partitions[n,n]

Partitions[n_Integer,_] := {} /; (n<0)
Partitions[0,_] := { {} }
Partitions[n_Integer,1] := { Table[1,{n}] }
Partitions[_,0] := {}

Partitions[n_Integer, maxpart_Integer] :=
        Block[{$RecursionLimit = Infinity},
	      Join[Map[(Prepend[#,maxpart])&, Partitions[n-maxpart,maxpart]],
                   Partitions[n,maxpart-1]
              ]
	]
Options[Path] = {Type->Undirected};

Path[1, opts___?OptionQ] := 
         Module[{type = Type /. Flatten[{opts, Options[Path]}]},
                CompleteGraph[1, Type -> type]
         ]

Path[n_Integer?Positive, opts___?OptionQ] := 
         Module[{type = Type /. Flatten[{opts, Options[Path]}]},
                If[type === Undirected,
	           Graph[Table[{{i, i+1}}, {i, n-1}], Map[({{#,0}})&,Range[n]]],
	           Graph[Table[{{i, i+1}}, {i, n-1}], Map[({{#,0}})&,Range[n]], EdgeDirection -> True]
                ]
         ]

PathMidpoint[p1_List, p2_List, epsilon_] := 
        ((p1 + p2)/2 + {0,epsilon}) /; (p1[[2]] == p2[[2]])

PathMidpoint[p1_List, p2_List, epsilon_] := 
        Block[{pmid = (p1+p2)/2, yvalue, s},
              yvalue  = pmid[[2]] + 
                        ((p1 -p2)[[1]])/((p2 - p1)[[2]])(x - pmid[[1]]);
	      s = Solve[(y-pmid[[2]])^2 + (x - pmid[[1]])^2 == epsilon^2 /.
		         y -> yvalue, x];
              If[epsilon>0, {x, yvalue}/.s[[1]], {x, yvalue}/.s[[2]]]
	]
PathQ[g_Graph] := 
         Module[{d = Degrees[g]}, 
                ConnectedQ[g] && Count[d, 1] == 2 && Count[d, 2] == V[g]-2
         ]
PerfectQ[g_Graph] :=
        Apply[
                And,
                Map[(ChromaticNumber[#] == Length[MaximumClique[#]])&,
                    Map[(InduceSubgraph[g,#])&, Subsets[Range[V[g]]] ] ]
        ]
PermutationGraph[p_?PermutationQ] :=
        Module[{q = InversePermutation[p]},
                MakeGraph[Range[Length[q]], 
                          ((#1 < #2 && q[[#1]] > q[[#2]]) ||
                          (#1 > #2 && q[[#1]] < q[[#2]]))&, 
                          Type -> Undirected
                ]
        ]
PermutationGroupQ[perms_List] :=
	FreeQ[ MultiplicationTable[perms,Permute], 0] &&
		EquivalenceRelationQ[SamenessRelation[perms]]
PermutationQ[e_List] := (Sort[e] === Range[Length[e]])
PermutationToTableaux[{}] := {{}, {}}

PermutationToTableaux[p_?PermutationQ] := 
       Module[{pt = {{p[[1]]}}, qt = {{1}}, r}, 
              Do[{pt, r} = InsertIntoTableau[p[[i]], pt, All];
                 If[r <= Length[qt], AppendTo[qt[[r]], i], AppendTo[qt, {i}]],
                 {i, 2, Length[p]}
              ]; 
              {pt, qt}
       ] 
PermutationType[p_?PermutationQ] := 
        Module[{m = Map[Length, ToCycles[p]], c = Table[0, {Length[p]}]},
               Do[c[[ m[[i]] ]]++, {i, Length[m]}];
               c
        ]
PermutationWithCycle[n_Integer, l_List] := 
        FromCycles[Append[Map[{#} &, Complement[Range[n], l]], l]]
Unprotect[Permutations]
Permutations[n_Integer] := Permutations[Range[n]]
Protect[Permutations]
Permute[l_List,p_?PermutationQ] := l [[ p ]]
Permute[l_List,p_List] := Map[ (Permute[l,#])&, p] /; (Apply[And, Map[PermutationQ, p]])
PermuteEdges[e_List, s_List, n_Integer] :=
        Module[{t = Table[0, {n}]},
               Do[t[[ s[[i]] ]] = i, {i, Length[s]}];
               Map[Prepend[Rest[#], t[[ First[#] ]]]&,
                   Select[e, (MemberQ[s, First[#][[1]]] &&
                             MemberQ[s, First[#][[2]]])&
                   ]
               ]
        ]
PermuteSubgraph[g_Graph,{}] := Graph[{},{}]
PermuteSubgraph[g_Graph, s_List] :=
        Graph[
              If[UndirectedQ[g], 
                 Map[Prepend[Rest[#], Sort[ First[#] ] ]&, 
                     PermuteEdges[Edges[g,All], s, V[g]]
                 ],
                 PermuteEdges[Edges[g,All], s, V[g]]
              ],
              Vertices[g, All][[ Sort[s] ]],
              Apply[Sequence, GraphOptions[g]]
        ] /; (Length[s] <= V[g]) && (Length[Union[s]]==Length[s])

PetersenGraph :=  GeneralizedPetersenGraph[5, 2]
PlanarGivenCycleQ[g_Graph, cycle_List] :=
        Module[{b, j, i},
               {b, j} = FindBridge[g, cycle];
               If[Length[j] === 1,
                  If[b === {}, True, SingleBridgeQ[First[b], First[j]]],
                  If[InterlockQ[j, cycle],
                     False,
                     Apply[And, Table[SingleBridgeQ[b[[i]],j[[i]]], {i,Length[b]}]]
                  ]
               ]
        ]
PlanarQ[g_Graph] :=
        Block[{simpleGraph = MakeSimple[g], 
               $RecursionLimit = Infinity},
          SimplePlanarQ[simpleGraph]
        ]
Polya[g_List, m_] := OrbitInventory[CycleIndex[g, x], x, m]
ProductVertices[vg_List, vh_List] :=
	Flatten[
		Map[(TranslateVertices[
                         DilateVertices[vg, 1/(Max[Length[vg],Length[vh]])], #
                     ])&,
                     RotateVertices[vh,Pi/2]
		],
		1
	] /; (vg != {}) && (vh != {})
PseudographQ[g_Graph] := MemberQ[Edges[g], _?(Function[l, l[[1]] == l[[2]]])]
RGFQ[{}]:= True;
RGFQ[r_]:= 
        Module[{m = Table[1, {Length[r]}]},
               ListQ[r] && (Length[r] > 0) && (Depth[r]==2) && (r[[1]]==1) && 
               (m[[1]] = 1; 
                Do[m[[i]]=Max[m[[i-1]], r[[i]]], {i, 2, Length[r]}];
                Apply[And, Table[r[[i]] <= (m[[i-1]] + 1), {i, 2, Length[r]}]]
               )
        ]
RGFToSetPartition[{}] := {{}}
RGFToSetPartition[rgf_?RGFQ] := RGFToSetPartition[rgf, Range[Length[rgf]]]

RGFToSetPartition[{}, {}] := {{}}
RGFToSetPartition[rgf_?RGFQ, set_List] :=
        Table[set[[Flatten[Position[rgf, i]]]],
              {i, Max[rgf]}
        ] /; (Length[rgf] === Length[set]) 
RGFs[0] := {}
RGFs[1] := {{1}}
RGFs[n_Integer?Positive] := RGFs1[n]

RGFs1 = Compile[{{n, _Integer}}, 
               Module[{r = Table[1, {n}], c = Prepend[Table[2, {n - 1}], 1], i},
                      Transpose[
                         NestList[(i = n;
                                   While[#[[1, i]] === #[[2, i]], i--]; 
                                   {Join[Take[#[[1]], i - 1], {#[[1, i]] + 1}, 
                                         Table[1, {n - i}]
                                    ],
                                    Join[Take[#[[2]], i], 
                                         Table[Max[#[[1, i]] + 2, #[[2, i]]], 
                                               {n - i}
                                         ]
                                    ]
                                   }
                                  )&, 
                                  {r, c},  
                                  BellB[n] - 1
                         ]
                      ][[1]]
               ]
       ]
RadialEmbedding[g_Graph, ct_Integer] := 
        ChangeVertices[g, Vertices[RadialEmbedding[MakeUndirected[g], ct]]] /; !UndirectedQ[g] && (1 <= ct) && (ct <= V[g])

RadialEmbedding[g_Graph, ct_Integer] :=
	Module[{center=ct,ang,i,da,theta,n,v,positioned,done,next,
                e = ToAdjacencyLists[g]},
		ang = Table[{0,2 Pi},{n=V[g]}];
		v = Table[{0,0},{n}];
		positioned = next = done = {center};
		While [next != {},
			center = First[next];
			new = Complement[e[[center]], positioned];
			Do [
				da = (ang[[center,2]]-ang[[center,1]])/Length[new];
				ang[[ new[[i]] ]] = {ang[[center,1]] + (i-1)*da, ang[[center,1]] + i*da};
				theta = Apply[Plus,ang[[ new[[i]] ]] ]/2;
				v[[ new[[i]] ]] = v[[center]] + N[{Cos[theta],Sin[theta]}],
				{i,Length[new]}
			];
			next = Join[Rest[next],new];
			positioned = Union[positioned,new];
			AppendTo[done,center]
		];
		ChangeVertices[g, Map[{#}&,v]]
	] /; (1 <= ct) && (ct <= V[g])

RadialEmbedding[g_Graph] := RadialEmbedding[g, First[GraphCenter[g]]];
Radius[g_Graph] := Min[ Eccentricity[g] ]
RandomComposition[n_Integer,k_Integer] :=
	Map[
		(#[[2]] - #[[1]] - 1)&,
		Partition[Join[{0},RandomKSubset[Range[n+k-1],k-1],{n+k}], 2, 1]
	]
Options[RandomGraph] = {Type -> Undirected};

RandomGraph[n_Integer, p_?NumericQ, {x_Integer, y_Integer}] := 
        SetEdgeWeights[RandomGraph[n, p], 
                       WeightingFunction -> RandomInteger,
                       WeightRange -> {x, y}
        ]

RandomGraph[n_Integer, p_?NumericQ, Directed] := RandomGraph[n, p, Type->Directed]

RandomGraph[0, p_?NumericQ, opts___?OptionQ] := Graph[{}, {}]
RandomGraph[n_Integer?Positive, p_?NumericQ, opts___?OptionQ] :=
       Module[{type},
              type = Type /. Flatten[{opts, Options[RandomGraph]}];
              If[type === Directed, RDG[n, p], RG[n, p] ]
       ]

RG[n_, p_] := 
       Module[{d = BinomialDistribution[Binomial[n, 2], p]}, 
              Graph[Map[{NthPair[#]}&, RandomKSubset[Range[Binomial[n,2]], Random[d]]], CircularEmbedding[n]]
       ]
RDG[n_, p_] :=
        Module[{d = BinomialDistribution[n(n-1), p], i, j}, 
               Graph[Map[(j = Mod[#, n-1]; i = Quotient[#, n-1];
                         If[j!=0, i++, j=n-1]; 
                         If[j >= i, {{i, j+1}}, {{i, j}}])&, 
                         RandomKSubset[Range[n(n-1)], Random[d]]
                     ],
                     CircularEmbedding[n],
                     EdgeDirection -> True
               ]
        ]
RandomHeap[n_Integer] := Heapify[RandomPermutation[n]]
RandomKSetPartition [{}, 0] := {}
RandomKSetPartition [set_List, k_Integer?Positive] :=
        UnrankKSetPartition [
           Random[Integer, {0, StirlingSecond[Length[set], k]-1}], set, k
        ] /; ((Length[set] > 0) && (k <= Length[set]))

RandomKSetPartition [0, 0] := {}
RandomKSetPartition [n_Integer?Positive, k_Integer?Positive] := RandomKSetPartition [Range[n], k] /; (k <= n)
RandomKSubset[n_Integer,k_Integer] := RandomKSubset[Range[n],k]
RandomKSubset[s_List, k_Integer] := s[[Sort[RandomPermutation[Length[s]][[Range[k] ]]]]]
RandomPartition[n_Integer?Positive] :=
  Module[{mult = Table[0, {n}], j, d, r=n, z},
    While[ (r > 0),
      d = 1;  j = 0;
      z = Random[] r PartitionsP[r];
      While [z >= 0, j++; If [r-j*d < 0, {j=1; d++;}]; z -= j*PartitionsP[r-j*d]];
      r -= j d; mult[[j]] += d;
    ];
    Reverse[Flatten[Table[Table[j, {mult[[j]]}], {j, Length[mult]}]]]
  ]
RP = Compile[{{n, _Integer}},
             Module[{p = Range[n],i,x,t},
	            Do [x = Random[Integer,{1,i}];
		        t = p[[i]]; p[[i]] = p[[x]]; p[[x]] = t,
		        {i,n,2,-1}
		    ];
	            p
	     ]
        ]

RandomPermutation[n_Integer] := RP[n]
RandomPermutation[l_List] := Permute[l, RP[Length[l]]]
RandomRGF[0] := {}
RandomRGF[n_Integer?Positive] := UnrankRGF[Random[Integer, {0, BellB[n]-1}], n]
RandomSetPartition[{}] := {}
RandomSetPartition [set_List] :=
        UnrankSetPartition [Random[Integer, {0, BellB[Length[set]]-1}], set] /; (Length[set] > 0)

RandomSetPartition [n_Integer] := RandomSetPartition [ Range[n] ]
RandomSquare[y_List,p_List] :=
	Module[{i=Random[Integer,{1,First[y]}], j=Random[Integer,{1,First[p]}]},
		While[(i > y[[j]]) || (j > p[[i]]), 
			i = Random[Integer,{1,First[y]}];
			j = Random[Integer,{1,First[p]}]
		];
		{i,j}
	]
RandomSubset[set_List] := UnrankSubset[Random[Integer,2^(Length[set])-1],set]

RandomSubset[0] := {}
RandomSubset[n_Integer] := UnrankSubset[Random[Integer,2^(n)-1], Range[n]]
RandomTableau[shape_List] :=
	Module[{i=j=n=Apply[Plus,shape],done,l,m,h=1,k,y,p=shape},
		y= Join[TransposePartition[shape],Table[0,{n - Max[shape]}]];
		Do[
			{i,j} = RandomSquare[y,p]; done = False;
			While [!done,
				h = y[[j]] + p[[i]] - i - j;
				If[ h != 0,
					If[ Random[] < 0.5,
						j = Random[Integer,{j,p[[i]]}],
						i = Random[Integer,{i,y[[j]]}]
					],
					done = True
				];
			];
			p[[i]]--; y[[j]]--;
			y[[m]] = i,
			{m,n,1,-1}
		];
		YVectorToTableau[y]
	]

RandomTree[1] := Graph[{}, {{{0, 0}}}]
RandomTree[n_Integer?Positive] :=
	RadialEmbedding[CodeToLabeledTree[Table[Random[Integer,{1,n}], {n-2}] ], 1]
RandomVertices[n_Integer?Positive] := Table[{{Random[], Random[]}}, {n}]
RandomVertices[g_Graph] := ChangeVertices[g, RandomVertices[V[g]] ]
RankBinarySubset[set_List,subset_List] :=
	Module[{i,n=Length[set]},
		Sum[ 2^(n-i) * If[ MemberQ[subset,set[[i]]], 1, 0], {i,n}]
	]
RankGraph[g_Graph, start_List] :=
	Module[ {rank = Table[0,{V[g]}],edges = ToAdjacencyLists[g],v,
                 queue,new},
		Scan[ (rank[[#]] = 1)&, start];
		queue = start;
		While [queue != {},
			v = First[queue];
			new = Select[ edges[[v]], (rank[[#]] == 0)&];
			Scan[ (rank[[#]] = rank[[v]]+1)&, new];
			queue = Join[ Rest[queue], new];
		];
		rank
	]
 
RankGrayCodeSubset[l_List, s_List] := 
       Module[{c = Table[If[MemberQ[s, l[[i]]], 1, 0], {i, Length[l]}], b = Table[0, {Length[l]}], n = Length[l]}, 
              b[[ 1 ]] = c[[ 1 ]]; 
              Do[b[[i]] = Mod[b[[i - 1]] + c[[i]], 2], {i, 2, n}]; 
              FromDigits[b, 2]
       ]
RankKSetPartition[sp_?SetPartitionQ] := 
        Module[{s = Sort[Flatten[sp, 1]]}, 
               RankKSetPartition1[ToCanonicalSetPartition[sp, s], s]
        ] 

RankKSetPartition[sp_List, s_List] := 
        RankKSetPartition1[ToCanonicalSetPartition[sp, s], s] /; SetPartitionQ[sp, s]
RankKSetPartition1[sp_List, s_List] := 0 /; (Length[sp] === Length[s])
                  
RankKSetPartition1[sp_List, s_List] :=
        Block[{k = Length[sp], n = Length[s], t, orderedT, j,
               $RecursionLimit = Infinity}, 
              If[First[sp] === {s[[1]]}, 
                 RankKSetPartition1[Rest[sp], Rest[s]],
                 (t = Prepend[Rest[sp], Rest[First[sp]]];
                 orderedT = ToCanonicalSetPartition[t, Rest[s]];
                 {j} = First[Position[orderedT, First[t]]];
                 StirlingSecond[n-1, k-1]+k*RankKSetPartition1[orderedT, Rest[s]]+j-1)            
              ]
        ]
RankKSubset[ss_List, s_List] := 0 /; (Length[ss] === Length[s])
RankKSubset[ss_List, s_List] := Position[s, ss[[1]]][[1, 1]] - 1 /; (Length[ss] === 1)
RankKSubset[ss_List, s_List] := 
       Block[{n = Length[s], k = Length[ss], 
              x = Position[s, ss[[1]]][[1, 1]], $RecursionLimit = Infinity},
              Binomial[n, k] - Binomial[n-x+1, k] + RankKSubset[Rest[ss], Drop[s, x]]
       ]
RankPermutation[{1}] := 0
RankPermutation[{}] := 0

RankPermutation[p_?PermutationQ] := 
        Block[{$RecursionLimit = Infinity},
              (p[[1]]-1) (Length[Rest[p]]!) + 
              RankPermutation[ Map[(If[#>p[[1]], #-1, #])&, Rest[p]]]
        ]
RankRGF[r_List] := 
        Module[{u = 1, n = Length[r]}, DValues[n, 1];
               Sum[u = Max[u, r[[i - 1]]]; DValues[n - i, u]*(r[[i]] - 1), 
                   {i, 2, n}
               ]
        ]
RankSetPartition [sp_?SetPartitionQ] :=
       Module[{n = Length[s = Sort[Flatten[sp, 1]]], k = Length[sp]},
              Sum[StirlingSecond[n, i], {i, 1, k-1}] + RankKSetPartition [sp, s]
       ]

RankSetPartition [sp_List, s_List] :=
       Module[{n = Length[s], k = Length[sp]},
              Sum[StirlingSecond[n, i], {i, 1, k-1}] + RankKSetPartition [sp, s]
       ] /; SetPartitionQ[sp, s]
RankSubset[set_List,subset_List] :=  RankGrayCodeSubset[set, subset]
RankedEmbedding[stages_List] := 
        Module[{m, rank, stageSizes, freq = Table[0, {Length[stages]}]}, 
               rank = Table[Position[stages,i][[1,1]], {i, Max[stages]}];
               stageSizes = Distribution[rank]; 
               Table[m = ++freq[[rank[[i]]]];
                     {rank[[i]], (m-1)+(1 - stageSizes[[rank[[i]]]])/2} // N, 
                     {i, Max[stages]}
               ]
        ] 

RankedEmbedding[g_Graph, stages_List] := 
        ChangeVertices[g, RankedEmbedding[stages]] /; SetPartitionQ[stages, Range[ V[g] ]]

RankedEmbedding[g_Graph, start:{_Integer?Positive..}] := 
        Module[{l = RankGraph[g, start]},
               RankedEmbedding[g, Table[Flatten[Position[l, i]], {i, Max[l]}]] 
        ] /; (Max[start] <= V[g])
RankedVertices[g_Graph,start_List] :=
	Module[{i,m,stages,rank,freq = Table[0,{V[g]}]},
		rank = RankGraph[g,start];
		stages = Distribution[ rank ];
		Table[
			m = ++ freq[[ rank[[i]] ]];
			{{rank[[i]], (m-1) + (1 - stages[[ rank[[i]] ]])/2 }}//N,
			{i,V[g]}
		]
	]
ReachableVertices[g_Graph, start_List, ME_List] := 
       Module[{r = AlternatingPaths[g, start, ME]}, 
              Join[start, Flatten[Position[Range[V[g]]-r, _Integer?(# != 0 &)]]]
       ]
ReadGraph[file_] :=
        Module[{edgelist={}, v={},x},
                If[Head[OpenRead[file]] =!= InputStream,
                   EmptyGraph[0], 
                   While[!SameQ[(x = Read[file,Number]), EndOfFile],
                         AppendTo[v,Read[file,{{Number,Number}}]];
                         AppendTo[edgelist,
                                  Convert[Characters[Read[file,String]]]
                         ];
                   ];
                   Close[file];
                   FromAdjacencyLists[edgelist,v]
                ]
        ]
RealizeDegreeSequence[d_List] :=
	Module[{i,j,v,set,seq,n=Length[d],e},
		seq = Reverse[ Sort[ Table[{d[[i]],i},{i,n}]] ];
		Graph[
			Flatten[ Table[
				{{k,v},seq} = {First[seq],Rest[seq]};
				While[ !GraphicQ[
					MapAt[
						(# - 1)&,
						Map[First,seq],
						set = RandomKSubset[Table[{i},{i,n-j}],k] 
					] ]
				];
				e = Map[{Sort[(Prepend[seq[[#,2]],v])]}&,set];
				seq = Reverse[ Sort[
					MapAt[({#[[1]]-1,#[[2]]})&,seq,set]
				] ];
				e,
				{j,Length[d]-1}
			], 1],
			CircularEmbedding[n]
		]
	] /; GraphicQ[d]

RealizeDegreeSequence[d_List,seed_Integer] :=
	(SeedRandom[seed]; RealizeDegreeSequence[d])
 
RefineEquivalences[eq_List, g_Graph, h_Graph, f_] := 
        Module[{dg = Table[Apply[f, {g, i}], {i, V[g]}], 
                dh = Table[Apply[f, {h, i}], {i, V[h]}], eq1}, 
               eq1 = Table[Flatten[Position[dh, dg[[i]]], 1], {i, Length[dg]}]; 
               Table[Intersection[eq[[i]], eq1[[i]]], {i, Length[eq]}]
        ]
ReflexiveQ[r_?SquareMatrixQ] := 
	Module[{i}, Apply[And, Table[(r[[i,i]]!=0), {i, Length[r]}] ] ]

ReflexiveQ[g_Graph] := False /; (V[g] == 0)
ReflexiveQ[g_Graph] := 
	Module[{e=Edges[g]},
		Apply[And, Table[MemberQ[e,{i,i}],{i, V[g]}] ]
	]
RegularGraph[k_Integer, n_Integer] := RealizeDegreeSequence[Table[k,{n}]]
RegularQ[g_Graph] := Apply[ Equal, Degrees[g] ]
RemoveCycleEdges[g_Graph, c_List] := 
        ChangeEdges[g, Select[Edges[g], (Intersection[#, c] == {}) &]]
RemoveMultipleEdges[g_Graph] :=
        ChangeEdges[g,
                       Map[First,
                           Split[
                                 Sort[Edges[g,All], 
                                      OrderedQ[{First[#1], First[#2]}]&
                                 ],
                                 (First[#1] == First[#2])&
                           ]
                       ]
        ]
RemoveSelfLoops[g_Graph] :=
        ChangeEdges[g, Select[Edges[g, All], (First[#][[1]] != First[#][[2]])& ]]
Options[RenderEdges] := Take[Options[ShowGraph], -7]

RenderEdges[v_List, e_List, aopts_List, eopts_List] :=
        Module[{fvp, svp,
                ne = RenderMultipleEdges[e, Cases[eopts, _[EdgeDirection,_]][[1,2]]]
               },
               Table[({fvp, svp} = v[[First[ ne[[i]] ]]];
                     ExpandEdgeOptions[Merge[eopts, Rest[ ne[[i]] ]], i, fvp, svp, aopts]),
                     {i, Length[ne]}                  
               ]
        ]
RenderGraph[Graph[e_, v_, gopts___], PlotRange -> pr_, ropts___] :=
        Block[{defaults = Merge[Options[ShowGraph], {gopts}],
               nv = NormalizeVertices[Map[First[#]&, v]]},
               Graphics[{RenderVertices[nv, defaults],
                         RenderEdges[Map[First[#]&, nv], e, defaults]},
                         ExpandOptions[PlotRange -> pr, nv, defaults],
                         ropts
               ]
        ]
RenderMultipleEdges[e_List, flag_] :=
        Module[{se = Split[Sort[e, OrderedQ[{First[#1], First[#2]}]& ],
                           (First[#1] == First[#2])&
                     ],
                r, nf},
               Flatten[Map[If[(Length[#]==1),
                              #,
                              r = Join[ Range[ Floor[Length[#]/2] ],
                                        -Range[ Floor[Length[#]/2] ],
                                         If[OddQ[Length[#]], {0}, {}]
                                  ];
                              Table[p = Position[#[[i]], EdgeDirection->_];
                                    nf = flag;
                                    If[p != {}, nf = #[[i, p[[1,1]], 2]] ];
                                    Merge[#[[i]],
                                          {EdgeDirection ->{nf, r[[i]]}}
                                    ],
                                    {i, Length[#]}
                              ]
                           ]&,
                           se
                       ], 1
               ]
        ]
Options[RenderVertices] := Take[Options[ShowGraph], 8]

RenderVertices[v_List, opts_List] :=
        Table[ExpandVertexOptions[Merge[opts, Rest[v[[i]]]], First[v[[i]]], i],
              {i, Length[v]}
        ]
ResidualFlowGraph[g_Graph, f_List] := ResidualFlowGraph[g,ToAdjacencyLists[g,EdgeWeight], f]

ResidualFlowGraph[g_Graph, al_List, f_List] := 
        Module[{r, e},
               e = Flatten[
                       Table[Join[
                                  If[(r = f[[i,j,2]])>0, {{{al[[i,j,1]],i}, r}},{}],
                                  If[(r = al[[i,j,2]]-f[[i,j,2]])>0, {{{i,al[[i,j,1]]}, r}},{}]
                             ],
                             {i, Length[f]}, {j, Length[f[[i]]]}
                       ], 2 
                   ];
               e = Map[{#[[1, 1]], EdgeWeight->Apply[Plus, Transpose[#][[2]]]}&,
                       Split[Sort[e], First[#1] == First[#2]&]
                   ];
               SetGraphOptions[ChangeEdges[g, e], EdgeDirection -> True]
        ]
RevealCycles[p_?PermutationQ] := 
      Module[{m = Infinity},
             Map[Take[p, {#[[1]], #[[2]] - 1}]&, 
                 Partition[
                    Join[DeleteCases[Table[If[ p[[i]] < m, m = p[[i]]; i, 0], {i, Length[p]}], 0],  
                         {Length[p] + 1}
                    ], 2, 1
                 ]
             ]
      ]
ReverseEdges[g_Graph] := 
        ChangeEdges[g,
                       Map[Prepend[Rest[#], Reverse[First[#]]]&,
                           Edges[g,All]
                       ]
        ] /; !UndirectedQ[g]
RobertsonGraph :=
 Graph[{{{1, 2}}, {{2, 3}}, {{3, 4}}, {{4, 5}}, {{5, 6}}, {{6, 7}}, {{7, 8}}, 
  {{8, 9}}, {{9, 10}}, {{10, 11}}, {{11, 12}}, {{12, 13}}, {{13, 14}}, 
  {{14, 15}}, {{15, 16}}, {{16, 17}}, {{17, 18}}, {{18, 19}}, {{1, 19}}, 
  {{1, 5}}, {{5, 10}}, {{10, 14}}, {{14, 18}}, {{3, 18}}, {{3, 7}}, 
  {{7, 11}}, {{11, 16}}, {{1, 16}}, {{2, 9}}, {{9, 17}}, {{6, 17}}, 
  {{6, 13}}, {{2, 13}}, {{8, 19}}, {{8, 15}}, {{4, 15}}, {{4, 12}}, 
  {{12, 19}}}, {{{0.9458172417006346, 0.32469946920468346}}, 
  {{0.7891405093963936, 0.6142127126896678}}, 
  {{0.546948158122427, 0.8371664782625285}}, 
  {{0.24548548714079924, 0.9694002659393304}}, 
  {{-0.08257934547233227, 0.9965844930066698}}, 
  {{-0.40169542465296926, 0.9157733266550575}}, 
  {{-0.6772815716257409, 0.7357239106731318}}, 
  {{-0.879473751206489, 0.4759473930370737}}, 
  {{-0.9863613034027223, 0.16459459028073403}}, 
  {{-0.9863613034027224, -0.16459459028073378}}, 
  {{-0.8794737512064891, -0.4759473930370735}}, 
  {{-0.6772815716257414, -0.7357239106731313}}, 
  {{-0.40169542465296987, -0.9157733266550573}}, 
  {{-0.08257934547233274, -0.9965844930066698}}, 
  {{0.2454854871407988, -0.9694002659393305}}, 
  {{0.5469481581224266, -0.8371664782625288}}, 
  {{0.7891405093963934, -0.614212712689668}}, 
  {{0.9458172417006346, -0.32469946920468373}}, {{1., 0}}}]
RootedEmbedding[g_Graph] := RootedEmbedding[g, First[GraphCenter[g]]]

RootedEmbedding[g_Graph,rt_Integer] :=
	Module[{root=rt,pos,i,x,dx,new,n=V[g],v,done,next,
                e=ToAdjacencyLists[g]},
		pos = Table[{-Ceiling[Sqrt[n]],Ceiling[Sqrt[n]]},{n}];
		v = Table[{0,0},{n}];
		next = done = {root};
		While [next != {},
			root = First[next];
			new = Complement[e[[root]], done];
			Do [
				dx = (pos[[root,2]]-pos[[root,1]])/Length[new];
				pos[[ new[[i]] ]] = {pos[[root,1]] + (i-1)*dx,
					pos[[root,1]] + i*dx};
				x = Apply[Plus,pos[[ new[[i]] ]] ]/2;
				v[[ new[[i]] ]] = {x,v[[root,2]]-1},
				{i,Length[new]}
			];
			next = Join[Rest[next],new];
			done = Join[done,new]
		];
		ChangeVertices[g,Map[{#}&, N[v]]]
	]
RotateVertices[v:{{{_?NumericQ, _?NumericQ},___?OptionQ}...}, t_] := 
        Module[{p = Map[First, v], np},
               np = RotateVertices[p, t];
               Table[{np[[i]], Apply[Sequence, Rest[v[[i]]]]}, {i, Length[np]}]
        ]

RotateVertices[v:{{_?NumericQ, _?NumericQ}...}, t_] := 
	Module[{d, theta},
		Map[
			(If[# == {0,0}, 
                            #, 
                            d=Sqrt[#[[1]]^2 + #[[2]]^2];
                            theta = t + Arctan[#];
                            If[!FreeQ[theta, Complex],
                               (* hack to get around new ArcTan behavior *)
                               theta = Chop[theta]
                            ];
                            N[{d Cos[theta], d Sin[theta]}]
			 ])&,
			v
		]
	]

RotateVertices[g_Graph, t_] := 
        ChangeVertices[g, RotateVertices[Vertices[g, All], t]]

RotateVertices[g_Graph, s_List, t_] :=
        Module[{v = Vertices[g, All]},
               ChangeVertices[g, v[[s]] = RotateVertices[v[[s]], t]; v]
        ]
Runs[p_?PermutationQ] :=
	Map[
		(Apply[Take,{p,{#[[1]]+1,#[[2]]}}])&,
		Partition[
			Join[
				{0},
				Select[Range[Length[p]-1], (p[[#]]>p[[#+1]])&],
				{Length[p]}
			],
			2,
			1
		]
	] /; (Length[p] > 0)

Runs[{}] := {}
SamenessRelation[perms_List] :=
	Module[{positions = Transpose[perms], i, j, n=Length[First[perms]]},
		Table[
			If[ MemberQ[positions[[i]],j], 1, 0],
			{i,n}, {j,n}
		]
	] /; perms != {}
ScreenColorNames = {"Black", "Red", "Blue", "Green", "Yellow", "Purple", 
                    "Brown", "Orange", "Olive", "Pink", "DeepPink", 
                    "DarkGreen", "Maroon", "Navy"}
SearchBiConComp[v_Integer] :=
	Block[{r, $RecursionLimit = Infinity},
              back[[v]]=dfs[[v]]=++c;
              Scan[
                   (If[dfs[[#]] == 0, 
                       If[!MemberQ[act,{v,#}], PrependTo[act,{v,#}]];
                       par[[#]] = v;
                       SearchBiConComp[#];
                       If[back[[#]] >= dfs[[v]],
                          {r} = Flatten[Position[act,{v,#}]];
                          AppendTo[bcc,Union[Flatten[Take[act,r]]]];
                          AppendTo[ap,v];
                          act = Drop[act,r]
                       ];
                       back[[v]] = Min[ back[[v]],back[[#]] ],
                       If[# != par[[v]],back[[v]]=Min[dfs[[#]],back[[v]]]]
                    ])&,
                    e[[v]]
              ];
        ]
SearchStrongComp[v_Integer] :=
	Block[{r, $RecursionLimit = Infinity},
		low[[v]]=dfs[[v]]=c++;
		PrependTo[cur,v];
		Scan[
			(If[dfs[[#]] == 0,
				SearchStrongComp[#];
				low[[v]]=Min[low[[v]],low[[#]]],
				If[(dfs[[#]] < dfs[[v]]) && MemberQ[cur,#],
					low[[v]]=Min[low[[v]],dfs[[#]] ]
				];
			])&,
			e[[v]]
		];
		If[low[[v]] == dfs[[v]],
			{r} = Flatten[Position[cur,v]];
			AppendTo[scc,Take[cur,r]];
			cur = Drop[cur,r];
		];
	]

SelectionSort[l_List,f_] :=
	Module[{where,item,unsorted=l},
		Table[
			item = MinOp[unsorted, f];
			{where} = First[ Position[unsorted,item] ];
			unsorted = Drop[unsorted,{where,where}];
			item,
			{Length[l]}
		]
	]
SelfComplementaryQ[g_Graph] := IsomorphicQ[g, GraphComplement[g]]
SelfLoopsQ[g_Graph] := MemberQ[Edges[g], {x_, x_}]
SetEdgeLabels[g_Graph, labels_List] :=
         Module[{el = Edges[g]},
                 SetGraphOptions[g, Table[{el[[i]], 
                                           EdgeLabel -> labels[[Mod[i-1,Length[labels]]+1]]
                                          },
                                          {i, M[g]}
                                    ]
                 ]
         ]
Options[SetEdgeWeights] = {WeightingFunction -> Random, WeightRange -> {0, 1}}

SetEdgeWeights[g_Graph, e : {{_Integer, _Integer} ...}, opts___?OptionQ] := 
        Module[{ v = Vertices[g], myfn, myrange}, 
               {myfn, myrange} = {WeightingFunction, WeightRange} /. 
               Flatten[{opts, Options[SetEdgeWeights]}];
               Switch[myfn, 
                      Random,
                      SetGraphOptions[g, 
                          Map[{#, EdgeWeight->Random[Real, myrange]} &, e]
                      ],
                      RandomInteger,
                      SetGraphOptions[g, 
                          Map[{#, EdgeWeight->Random[Integer, myrange]}&,e]
                      ],
                      Euclidean | LNorm[_],
                      SetGraphOptions[g, 
                          Map[{#, EdgeWeight->Distance[v[[#]], myfn]} &, e]
                      ],
                      _,
                      SetGraphOptions[g, 
                          Map[{#, 
                               EdgeWeight->Apply[myfn,Transpose[{#, v[[#]]}]]}&,
                               e
                          ]
                      ] 
               ]
        ]
  
SetEdgeWeights[g_Graph, opts___?OptionQ] := SetEdgeWeights[g, Edges[g], opts]

SetEdgeWeights[g_Graph, e:{{_Integer, _Integer}...}, weights:{_?NumericQ...}] := 
        SetGraphOptions[g, 
            MapIndexed[{#1, EdgeWeight->weights[[First[#2]]]}&, 
                       If[UndirectedQ[g], Map[Sort, e], e]
            ]
        ] /; (Length[weights] == Length[e])

SetEdgeWeights[g_Graph, weights : {_?NumericQ...}] := 
        SetEdgeWeights[g, Edges[g], weights]
SetGraphOptions[{}, {}] := {}

SetGraphOptions[l_List, {}] := l

SetGraphOptions[l_List, {}, _] := l

SetGraphOptions[vl_List, ol : {{_Integer.., __?OptionQ}..}] := 
        Module[{o=Transpose[ol /. {x__Integer, y__?OptionQ} :> {{x}, {y}}], p},
               Table[p = Position[o[[1]], i]; 
                     If[p == {},
                        vl[[i]],
                        Prepend[Merge[Rest[vl[[i]]], o[[2, p[[1, 1]] ]] ], 
                                First[ vl[[i]] ] 
                        ]
                     ],
                     {i, Length[vl]}
               ]
        ]

SetGraphOptions[el_List, ol_List, All] := 
        Module[{o=Transpose[ol /. {x : {_Integer, _Integer}.., y__?OptionQ} :> 
                                  {{x}, {y}}
                  ], p},
               Table[p = Position[o[[1]], First[ el[[i]] ] ]; 
                     If[p == {}, 
                        el[[i]], 
                        Prepend[Merge[Rest[el[[i]]], o[[2, p[[1, 1]]  ]]  ], 
                                First[ el[[ i ]] ]
                        ]
                     ], 
                     {i, Length[el]}
               ]
        ]

SetGraphOptions[el_List, ol_List, One] := 
        Module[{no=Transpose[ol /. {x:{_Integer, _Integer} .., y__?OptionQ} :> 
                                   {{x}, {y}}
                   ], p},
               Table[p = Position[no[[1]], First[ el[[i]] ] ]; 
                     If[p == {}, 
                        el[[i]], 
                        e = Prepend[Merge[Rest[el[[i]]], no[[2, p[[1, 1]] ]] ], 
                                    First[ el[[i]] ]
                        ];
                        no = MapAt[Infinity &, no, p];
                        e
                     ], 
                     {i, Length[el]}
               ]
        ]

SetGraphOptions[g_Graph, l:{_Integer.., ___?OptionQ}, 
                flag_Symbol:All, opts___?OptionQ] := 
        SetGraphOptions[g, {l}, flag, opts]

SetGraphOptions[g_Graph, l:{{_Integer,_Integer}.., ___?OptionQ}, 
                flag_Symbol:All, opts___?OptionQ] := 
        SetGraphOptions[g, {l}, flag, opts]

SetGraphOptions[Graph[e_, v_, dopts___], l_List:{}, flag_Symbol:All, opts___?OptionQ] :=
        Module[{lv, le, ne, nv},
              lv = Cases[l, {_Integer.., __?OptionQ}];
              If[UndirectedQ[Graph[e, v, dopts]],
                 le = Cases[l, 
                            {{_Integer, _Integer}.., __?OptionQ}
                      ] /. {x : {_Integer, _Integer}.., y___?OptionQ} :> 
                           {Apply[Sequence, Map[Sort[#] &, {x}]], y},
                 le = Cases[l, {{_Integer, _Integer}.., __?OptionQ}]
              ];
              If[flag===One, 
                 ne = SetGraphOptions[e, le, One],
                 ne = SetGraphOptions[e, le, All]
              ];
              nv = SetGraphOptions[v, lv];
              Apply[Graph, Join[{ne, nv}, Merge[{dopts}, {opts}]]]
        ]
SetLevel[l_List,lvl_,rank_List] :=  
    Module[ {r=rank},
            If[ r[[#]] < lvl, r[[#]] = lvl ] & /@ l;
            r
    ]
SetPartitionListViaRGF [n_Integer?Positive] := 
                Map[RGFToSetPartition, RGFs[n]]

SetPartitionListViaRGF [n_Integer?Positive, k_Integer?Positive] :=
                Map[RGFToSetPartition, RGFs[n, k]]
SetPartitionQ[sp_] := (ListQ[sp]) && (Depth[sp] > 2) && SetPartitionQ[sp, Apply[Union, sp]]

SetPartitionQ[sp_, s_List] := (ListQ[sp]) && (Depth[sp] > 2) &&
                              (Apply[And, Map[ListQ, sp]]) && (Sort[Flatten[sp, 1]] === Sort[s])
SetPartitionToLabel[s_?SetPartitionQ] := 
       StringDrop[Apply[StringJoin, 
                        Map[StringJoin[Apply[StringJoin, Map[ToString, #]], "|"] &, s]
                  ], -1
       ]
SetPartitionToRGF[{{}}] := {}
SetPartitionToRGF[sp_?SetPartitionQ] := 
       SetPartitionToRGF[sp, Sort[Flatten[sp, 1]]]

SetPartitionToRGF[sp_?SetPartitionQ, set_List] :=
       Module[{rgf = Table[1, {Length[set]}], 
               nsp = ToCanonicalSetPartition[sp, set]
              },
              Table[rgf[[ Map[Position[set, #][[1, 1]]&, nsp[[i]] ] ]] = i,
                    {i, Length[sp]}
              ];
              rgf
       ]
SetPartitions[{}] := {{}}
SetPartitions[s_List] := Flatten[Table[KSetPartitions[s, i], {i, Length[s]}], 1]

SetPartitions[0] := {{}}
SetPartitions[n_Integer?Positive] := SetPartitions[Range[n]]
SetVertexLabels[g_Graph, labels_List] :=
         SetGraphOptions[g, Table[{i, 
                                   VertexLabel-> labels[[ Mod[i-1,Length[labels]]+1 ]]
                                  },
                                  {i, V[g]}
                            ]
         ]
Options[SetVertexWeights] = {WeightingFunction -> Random, 
                             WeightRange -> {0, 1} }

SetVertexWeights[g_Graph, opts___?OptionQ] :=
        Module[{v = Vertices[g], myfn, myrange},
                {myfn, myrange} = {WeightingFunction, WeightRange} /.
                Flatten[{opts, Options[SetVertexWeights]}];
               Switch[myfn,
                      Random,
                      SetGraphOptions[g,
                          Table[{i, VertexWeight->Random[Real, myrange]}, 
                                {i, V[g]}
                          ]
                      ],
                      RandomInteger,
                      SetGraphOptions[g,
                          Table[{i, VertexWeight->Random[Integer, myrange]}, 
                                {i, V[g]}
                          ]
                      ],
                      _,
                      SetGraphOptions[g,
                          Table[{i, VertexWeight -> Apply[myfn, {i, v[[i]]}]},
                                {i, V[g]}
                          ]
                      ]
               ]
        ]

SetVertexWeights[g_Graph, vs:{_Integer ...}, weights:{_?NumericQ ...}] :=
        SetGraphOptions[g,
            MapIndexed[{#1, VertexWeight->weights[[First[#2]]]}&, vs]
        ] /; (Length[weights] == Length[vs])

SetVertexWeights[g_Graph, weights : {_?NumericQ ...}] :=
        SetVertexWeights[g, Range[V[g]], weights]
ShakeGraph[g_Graph, s_List] := ShakeGraph[g, s, 0.1]
ShakeGraph[g_Graph] := ShakeGraph[g, 0.1]

ShakeGraph[g_Graph, s_List, fract_?NumericQ] :=
        Module[{i, d, a, v = Vertices[g, All]},
               v[[s]] = Map[(d = Random[Real,{0,fract}];
                             a = Random[Real,{0, 2 N[Pi]}];
                             Prepend[Rest[#], 
                                     First[#] + {N[d Cos[a]], N[d Sin[a]]}
                             ])&,
                             v[[s]]
                        ];
               ChangeVertices[g, v]
        ]

ShakeGraph[g_Graph, fract_?NumericQ] := ShakeGraph[g, Range[V[g]], fract]
ShapeOfTableau[t_List] := Map[Length,t]
Options[ShortestPath] = {Algorithm -> Automatic};

ShortestPath[g_Graph, s_Integer, e_Integer, opts___?OptionQ] := 
       Module[{algorithm, parent}, 
              algorithm = Algorithm /. Flatten[{opts, Options[ShortestPath]}];
              parent = ChooseShortestPathAlgorithm[g, s, algorithm];
              Rest[Reverse[FixedPointList[Function[x, parent[[x]]], e, V[g]] ]]
       ] /; (1 <= s) && (s <= V[g]) && (1 <= e) && (e <= V[g])
Options[ShortestPathSpanningTree] = {Algorithm -> Automatic}

ShortestPathSpanningTree[g_Graph, s_Integer, opts___?OptionQ] :=
	Module[{algorithm, parent},
               algorithm = Algorithm /. Flatten[{opts, Options[ShortestPathSpanningTree]}];
               parent = ChooseShortestPathAlgorithm[g, s, algorithm];
               Graph[Map[({Sort[{#,parent[[#]]}]})&, 
                            Complement[Range[V[g]],{s}]
                     ],
                     Vertices[g, All]
	       ]
	]
ShowGraph::obsolete = "Usage of Directed as a second argument to ShowGraph is obsolete."
Options[ShowGraph] := {VertexColor -> Black,
                       VertexStyle -> Disk[Normal],
                       VertexNumber -> False,
                       VertexNumberColor -> Black,
                       VertexNumberPosition -> LowerLeft,
                       VertexLabel -> False,
                       VertexLabelColor -> Black,
                       VertexLabelPosition -> UpperRight,
                       PlotRange -> Normal,
                       AspectRatio -> 1,
                       EdgeColor -> Black,
                       EdgeStyle -> Normal,
                       EdgeLabel -> False,
                       EdgeLabelColor -> Black,
                       EdgeLabelPosition -> LowerLeft,
                       LoopPosition -> UpperRight,
                       EdgeDirection -> False
                      }

Options[MyPlot] := Options[ShowGraph][[{9, 10}]]

SelectOptions[options_List, name_] :=
        Select[options, (MemberQ[Options[name], First[#], 2])&]

ShowGraph[g_Graph, Directed] := (Message[ShowGraph::obsolete]; ShowGraph[g])

ShowGraph[g_Graph, lopts_List, opts___?OptionQ] := 
        ShowGraph[SetGraphOptions[g, lopts], opts] /; (V[g] > 0)

ShowGraph[g_Graph, opts___?OptionQ, Graphics] :=
        Module[{VertexOptions = Merge[Options[RenderVertices],
                                      SelectOptions[{opts}, RenderVertices],
                                      SelectOptions[Options[g], RenderVertices]
                                ],
                EdgeOptions  =  Merge[Options[RenderEdges],
                                      SelectOptions[{opts}, RenderEdges],
                                      SelectOptions[Options[g], RenderEdges]
                                ],
                ArrowOptions =  Merge[SelectOptions[{opts}, Arrow],
                                      SelectOptions[Options[g], Arrow]
                                ],
                PlotOptions =   Merge[Options[MyPlot],
                                      SelectOptions[{opts}, Plot],
                                      SelectOptions[Options[g], Plot]
                                ],
                v = Vertices[g, All],
                nv = NormalizeVertices[Vertices[g]], nnv
               },
               nnv = Table[Prepend[Rest[v[[i]]], nv[[i]] ], {i, Length[v]}];
               Graphics[{RenderEdges[nv, Edges[g, All], ArrowOptions, EdgeOptions],
                         RenderVertices[nnv, VertexOptions]},
                         Apply[Sequence, Map[ExpandPlotOptions[#, nnv]&, PlotOptions]]
               ]
        ] /; (V[g] > 0)


ShowGraph[g_Graph, opts___?OptionQ] := Show[ShowGraph[g, opts, Graphics]] /; (V[g] > 0)
ShowGraphArray[gs_List, opts___] := 
        ShowGraphArray[{gs}, opts] /; (Head[First[gs]] == Graph)

ShowGraphArray[gs_List, opts___] := 
        Block[{p = Position[{opts}, GraphicsSpacing->_], s, nopts},
              If[p == {},
                 s = GraphicsSpacing -> 0.1;
                 nopts = opts,
                 s = {opts}[[ p[[1,1]] ]];
                 nopts = Apply[Sequence, Drop[{opts}, {p[[1,1]]}]]
              ];
              Show[Apply[GraphicsArray, 
                   {Map[ShowGraph[#, nopts, Graphics]&, gs, {2}], s}
                   ]
              ]
        ]
ShowLabeledGraph[g_Graph, o___?OptionQ] := ShowGraph[g, VertexNumber -> True, o]
ShowLabeledGraph[g_Graph, l_List, o___?OptionQ] := ShowGraph[ SetVertexLabels[g, l], o]
Options[ShuffleExchangeGraph] = {VertexLabel -> False}

ShuffleExchangeGraph[n_Integer?Positive, opts___?OptionQ] := 
        Module[{label},
               label = VertexLabel /. Flatten[{opts, Options[MakeGraph]}];
               MakeGraph[Strings[{0, 1}, n],
                        (Last[#1] != Last[#2]) && (Take[#1,(n-1)]==Take[#2,(n-1)]) || 
                        (RotateRight[#1,1] == #2) || (RotateLeft[#1, 1] == #2)&,
                        Type -> Undirected, VertexLabel -> label
               ]
        ]
SignaturePermutation[p_?PermutationQ] := (-1) ^ (Length[p]-Length[ToCycles[p]])
SimplePlanarQ[g_Graph] :=
  Module[{components, $RecursionLimit = Infinity},
        components = BiconnectedComponents[g];
        Apply[  And,
                Map[(PlanarQ[InduceSubgraph[g,#]])&, components]
        ]
  ] /; !(ConnectedQ[g] && BiconnectedQ[g])

SimplePlanarQ[g_Graph] := False /;  (M[g] > 3 V[g]-6) && (V[g] > 2)
SimplePlanarQ[g_Graph] := True /;   (M[g] < V[g] + 3)
SimplePlanarQ[g_Graph] := (PlanarGivenCycleQ[ g, Rest[FindCycle[g]] ])
SimpleQ[g_Graph] := (!PseudographQ[g]) && (!MultipleEdgesQ[g])
SingleBridgeQ[b_Graph, {_}] := PlanarQ[b]
SingleBridgeQ[b_?PathQ, {_,_}] := True

SingleBridgeQ[b_Graph, j_List] :=
    Module[{sp = ShortestPath[b, j[[1]], j[[2]]]},
           If[Intersection[sp, Drop[j, 2]] =!= {},
              SingleBridgeQ[b, RotateLeft[j]],
              PlanarGivenCycleQ[JoinCycle[b,j], Join[sp, Drop[j,2]]]
           ]
    ]
SmallestCyclicGroupGraph := 
 Graph[{{{1, 2}}, {{1, 3}}, {{1, 4}}, {{1, 5}}, {{1, 6}}, {{2, 3}}, {{2, 4}}, 
  {{2, 8}}, {{2, 9}}, {{3, 6}}, {{3, 7}}, {{3, 8}}, {{4, 9}}, {{5, 6}}, 
  {{7, 8}}}, {{{0.508, 0.81}}, {{0.302, 0.44}}, {{0.704, 0.44}}, 
  {{0.426, 0.852}}, {{0.592, 0.854}}, {{0.786, 0.49}}, {{0.704, 0.344}}, 
  {{0.302, 0.344}}, {{0.23, 0.484}}}]
Solution[space_List,index_List,count_Integer] :=
	Module[{i}, Table[space[[ i,index[[i]] ]], {i,count}] ]
Spectrum[g_Graph] := Eigenvalues[ToAdjacencyMatrix[g]]
SpringEmbedding[g_Graph, step_:10, inc_:0.15] := g /; EmptyQ[g]

SpringEmbedding[g_Graph, step_:10, inc_:0.15] := 
       Module[{verts=Vertices[g], new, m=ToAdjacencyMatrix[MakeUndirected[g]]},
              new = UV[step, inc, m, verts];
              ChangeVertices[g, new]
       ]

UV = Compile[{{step, _Real}, {inc, _Real}, {m, _Integer, 2}, 
              {verts, _Real, 2}},
       Module[{u, i, new = old = verts, n = Length[verts]},
              Do[ Do[new[[u]] = old[[u]] + inc*CF[u, m, old], {u, n}];
                  old = new, {i, step}
              ];
              new
       ],
       {{CF[___], _Real, 1}} 
     ]

CF = Compile[{{u, _Integer}, {m, _Integer, 2}, {em, _Real, 2}}, 
       Module[{n = Length[m], stc = 0.25, gr = 10.0, f = {0.0, 0.0}, 
               spl = 1.0, v, dsquared}, 
              Do[dsquared = Max[0.001, Apply[Plus, (em[[u]] - em[[v]])^2]];
                 f += (1 - m[[u, v]]) (gr/dsquared) (em[[u]] - em[[v]]) - 
                      m[[u, v]] stc Log[dsquared/spl] (em[[u]] - em[[v]]), 
                 {v, n}
              ];
              f
       ]
     ]
SquareMatrixQ[{}] = True
SquareMatrixQ[r_] := MatrixQ[r] && (Length[r] == Length[r[[1]]])
StableMarriage[mpref_List,fpref_List] :=
	Module[{n=Length[mpref],freemen,cur,i,w,husband},
		freemen = Range[n];
		cur = Table[1,{n}];
		husband = Table[n+1,{n}];
		While[ freemen != {},
			{i,freemen}={First[freemen],Rest[freemen]};
			w = mpref[[ i,cur[[i]] ]];
			If[BeforeQ[ fpref[[w]], i, husband[[w]] ], 
				If[husband[[w]] != n+1,
					AppendTo[freemen,husband[[w]] ]
				];
				husband[[w]] = i,
				cur[[i]]++;
				AppendTo[freemen,i]
			];
		];
		InversePermutation[ husband ]
	] /; Length[mpref] == Length[fpref]
Star[n_Integer?Positive] :=
        Graph[Table[{{i, n}}, {i, n-1}], 
                 Append[CircularEmbedding[n-1], {{0, 0}}]
        ]
StirlingFirst[n_Integer,m_Integer] := StirlingFirst1[n,m] /; ((n>=0)&&(m>=0))
StirlingFirst1[n_Integer,0] := If [n == 0, 1, 0] 
StirlingFirst1[0,m_Integer] := If [m == 0, 1, 0]

StirlingFirst1[n_Integer,m_Integer] := 
        Block[{$RecursionLimit = Infinity},
               StirlingFirst1[n,m] = (n-1) StirlingFirst1[n-1,m] + StirlingFirst1[n-1, m-1] 
        ]
StirlingSecond[n_Integer,0] := If [n == 0, 1, 0]
StirlingSecond[0,k_Integer] := If [k == 0, 1, 0]

StirlingSecond[n_Integer?Positive, k_Integer?Positive] := 
        Sum [ (-1)^(k-i)*Binomial [k, i]*(i^n), {i, 1, k}]/k!
Strings[l_List,0] := { {} }

Strings[l_List, k_Integer] := Strings[Union[l], k] /; (Length[l] =!= Length[Union[l]])
Strings[l_List,k_Integer] := Distribute[Table[l, {k}], List]
StronglyConnectedComponents[g_Graph] :=
	Block[{e=ToAdjacencyLists[g],s,c=1,i,cur={},
               low=dfs=Table[0,{V[g]}],scc={}
              },
	      While[(s=Select[Range[V[g]],(dfs[[#]]==0)&]) != {},
	            SearchStrongComp[First[s]];
              ];
              ToCanonicalSetPartition[scc]
	] /; !UndirectedQ[g]

Subsets[l_List] := GrayCodeSubsets[l]
Subsets[n_Integer] := GrayCodeSubsets[Range[n]]
SymmetricGroup[n_Integer] := Permutations[n] /; (n >= 0)
SymmetricCoefficient[y_[i_], y_]  := i;
SymmetricCoefficient[y_[i_]^k_, y_]  := i^k k!;
SymmetricCoefficient[u_ v_, y_]  :=
        Block[{$RecursionLimit = Infinity}, SymmetricCoefficient[u, y]*SymmetricCoefficient[v, y]]

SymmetricGroupIndex[n_Integer?Positive, x_Symbol] :=
        Apply[Plus,
              Map[#/SymmetricCoefficient[#, x]&,
                  Map[Apply[Times, #]&, Map[x, Partitions[n], {2}]]
              ]
        ]
SymmetricQ[r_?SquareMatrixQ] := (r === Transpose[r])

SymmetricQ[g_Graph] := 
        Module[{e = Edges[g]},
               Apply[And, Map[MemberQ[e, Reverse[#]]&, e]]
        ]  /; !UndirectedQ[g]

SymmetricQ[g_Graph] := True
TableauClasses[p_?PermutationQ] :=
	Module[{classes=Table[{},{Length[p]}],t={}},
		Scan [(t = InsertIntoTableau[#,t];
                       PrependTo[classes[[Position[First[t],#] [[1,1]] ]], #])&,
                       p
		];
		Select[classes, (# != {})&]
	]
TableauQ[{}] = True
TableauQ[t_List] :=
	And [
		Apply[And, Map[(Apply[LessEqual,#])&, t] ],
		Apply[And, Map[(Apply[LessEqual,#])&, TransposeTableau[t]] ],
		Apply[GreaterEqual, ShapeOfTableau[t] ],
		Apply[GreaterEqual, Map[Length,TransposeTableau[t]] ]
	]
TableauToYVector[t_?TableauQ] :=
	Module[{i,y=Table[1,{Length[Flatten[t]]}]},
		Do [ Scan[ (y[[#]]=i)&, t[[i]] ], {i,2,Length[t]} ];
		y
	]
Tableaux[s_List] :=
	Module[{t = LastLexicographicTableau[s]},
		Table[ t = NextTableau[t], {NumberOfTableaux[s]} ]
	]

Tableaux[n_Integer?Positive] := Apply[ Join, Map[ Tableaux, Partitions[n] ] ]
TableauxToPermutation[p1_?TableauQ,q1_?TableauQ] :=
	Module[{p=p1, q=q1, row, firstrow},
		Reverse[
			Table[
				firstrow = First[p];
				row = Position[q, Max[q]] [[1,1]];
				p = DeleteFromTableau[p,row];
				q[[row]] = Drop[ q[[row]], -1];
				If[ p == {},
					First[firstrow],
					First[Complement[firstrow,First[p]]]
				],
				{Apply[Plus,ShapeOfTableau[p1]]}
			]
		]
	] /; ShapeOfTableau[p1] === ShapeOfTableau[q1]
TetrahedralGraph :=
 Graph[{{{1, 4}}, {{2, 4}}, {{3, 4}}, {{1, 2}}, {{2, 3}}, {{1, 3}}}, 
 {{{-0.4999999999999998, 0.8660254037844387}}, 
  {{-0.5000000000000004, -0.8660254037844384}}, {{1., 0}}, {{0, 0}}}]
ThomassenGraph :=
 Graph[{{{1, 3}}, {{1, 4}}, {{2, 4}}, {{2, 5}}, {{3, 5}}, {{6, 8}}, {{6, 9}}, 
  {{7, 9}}, {{7, 10}}, {{8, 10}}, {{11, 13}}, {{11, 14}}, {{12, 14}}, 
  {{12, 15}}, {{13, 15}}, {{16, 18}}, {{16, 19}}, {{17, 19}}, {{17, 20}}, 
  {{18, 20}}, {{10, 20}}, {{5, 15}}, {{25, 26}}, {{26, 27}}, {{21, 27}}, 
  {{21, 22}}, {{22, 23}}, {{23, 24}}, {{24, 28}}, {{28, 29}}, {{29, 30}}, 
  {{30, 31}}, {{31, 32}}, {{32, 33}}, {{33, 34}}, {{25, 34}}, {{6, 25}}, 
  {{7, 26}}, {{8, 27}}, {{9, 21}}, {{1, 21}}, {{2, 22}}, {{3, 23}}, 
  {{4, 24}}, {{11, 28}}, {{12, 29}}, {{13, 30}}, {{14, 31}}, {{16, 31}}, 
  {{17, 32}}, {{18, 33}}, {{19, 34}}}, 
 {{{-1.6909830056250525, -1.0489434837048464}}, 
  {{-2.8090169943749475, -1.4122147477075266}}, 
  {{-2.8090169943749475, -2.5877852522924734}}, 
  {{-1.6909830056250528, -2.9510565162951536}}, {{-1., -2.0000000000000004}}, 
  {{-1.6909830056250525, 2.9510565162951536}}, 
  {{-2.8090169943749475, 2.5877852522924734}}, 
  {{-2.8090169943749475, 1.4122147477075266}}, 
  {{-1.6909830056250528, 1.0489434837048464}}, {{-1., 1.9999999999999998}}, 
  {{1.6909830056250525, -2.9510565162951536}}, 
  {{2.8090169943749475, -2.5877852522924734}}, 
  {{2.8090169943749475, -1.412214747707527}}, 
  {{1.690983005625053, -1.0489434837048464}}, {{1., -1.9999999999999996}}, 
  {{1.6909830056250525, 1.0489434837048464}}, 
  {{2.8090169943749475, 1.4122147477075266}}, 
  {{2.8090169943749475, 2.587785252292473}}, 
  {{1.690983005625053, 2.9510565162951536}}, {{1., 2.0000000000000004}}, 
  {{-1.3510643118126104, -0.0027813157801774846}}, 
  {{-3.6989356881873894, -0.7656509701858061}}, 
  {{-3.6989356881873894, -3.234349029814194}}, 
  {{-1.3510643118126109, -3.9972186842198227}}, 
  {{-1.3510643118126104, 3.9972186842198223}}, 
  {{-3.6989356881873894, 3.2343490298141937}}, 
  {{-3.6989356881873894, 0.7656509701858059}}, 
  {{1.35106431181261, -3.9972186842198223}}, 
  {{3.6989356881873894, -3.234349029814194}}, 
  {{3.6989356881873903, -0.7656509701858067}}, 
  {{1.3510643118126109, -0.0027813157801772626}}, 
  {{3.6989356881873894, 0.7656509701858059}}, 
  {{3.6989356881873903, 3.2343490298141933}}, 
  {{1.3510643118126109, 3.9972186842198227}}}]
Options[ToAdjacencyLists] = {Type -> All};

ToAdjacencyLists[g_Graph, opts___?OptionQ] :=
       Module[{type, s, al, e = Edges[g], n},
              type = Type /. Flatten[{opts, Options[ToAdjacencyLists]}];
              s = Join[If[UndirectedQ[g], Double[e], e],
                       Table[{i, -1}, {i, V[g]}]
                  ];
              al = Map[Rest,
                       Split[Sort[s], (#1[[1]] === #2[[1]]) &] /.
                       {_Integer, n_Integer} :> n
                   ];
              If[type === Simple,
                 Map[Union, Table[Select[al[[i]], (# != i) &], {i,Length[al]}]],
                 al
              ]
       ]
ToAdjacencyLists[g_Graph, EdgeWeight, opts___?OptionQ] :=
       Module[{type, s, al, e = Edges[g, EdgeWeight]},
              type = Type /. Flatten[{opts, Options[ToAdjacencyLists]}];
              s = Join[If[UndirectedQ[g], Double[e, EdgeWeight], e],
                       Table[{{i, -1}, 1}, {i, V[g]}]
                  ];
              al = Map[Rest[Map[Rest, Partition[Flatten[#], 3, 3]]] &,
                       Split[Sort[s], (#1[[1, 1]] == #2[[1, 1]]) &]
                   ];
              If[type === Simple,
                 Map[Union,
                     Table[Select[al[[i]],(#[[1]] != i) &], {i, Length[al]}]
                 ],
                 al
              ]
       ]
Options[ToAdjacencyMatrix] = {Type -> All};

ToAdjacencyMatrix[g_Graph, opts___?OptionQ] :=
        Module[{e = ToAdjacencyLists[g], blanks = Table[0, {V[g]}], type, am},
               type = Type /. Flatten[{opts, Options[ToAdjacencyMatrix]}]; 
               am = Table[nb = blanks; Scan[nb[[#]]++ &, e[[i]]]; nb, 
                          {i, Length[e]}
                    ];
               If[type === Simple, 
                  Do[am[[i, i]] = 0, {i, V[g]}]; am /. _Integer?Positive -> 1, 
                  am
               ]
        ]
ToAdjacencyMatrix[g_Graph, EdgeWeight, opts___?OptionQ] :=
        Module[{adjList = ToAdjacencyLists[g, EdgeWeight],
                freshRow = Table[Infinity, {V[g]}], row, am, type},
               type = Type /. Flatten[{opts, Options[ToAdjacencyMatrix]}]; 
               am = Map[(row = freshRow;
                        If[#==={}, 
                           row,
                           Apply[Set[Part[row, #1], #2]&, Transpose[#]]; row
                        ])&,
                        adjList
                    ]; 
               If[type === Simple,
                  Do[am[[i, i]] = 0, {i, V[g]}]; am,
                  am
               ]
        ]
ToCanonicalSetPartition[{}] :=  {}
ToCanonicalSetPartition[sp_?SetPartitionQ] := 
      Transpose[Sort[Map[{First[#], #} &, Map[Sort, sp]]]] [[2]]

ToCanonicalSetPartition[sp_List, X_List] := 
      Map[Last, 
          Sort[Map[{First[#], #}&, 
                   Map[Sort[#, 
                            (Position[X,#1][[1,1]]<Position[X,#2][[1,1]])& 
                       ]&,
                       sp
                   ]
               ], 
               (Position[X,#1[[1]]][[1,1]] < Position[X,#2[[1]]][[1,1]])&
          ]
      ] /; SetPartitionQ[sp, X]
ToCycles[p_?PermutationQ] :=
        Module[{k, j, first, np = p, q = Table[0, {Length[p]}]},
               DeleteCases[
                   Table[If[np[[i]] == 0,
                            {},
                            j = 1; first = np[[i]]; np[[i]] = 0; 
                            k = q[[j++]] = first;
                            While[np[[k]] != 0, q[[j++]] = np[[k]]; np[[k]] = 0; k = q[[j-1]]];
                            Take[q, j-1]
                         ],
                         {i, Length[p]}
                   ],
                   _?(#==={}&)
               ]
        ]
ToInversionVector[p_?PermutationQ] :=
	Module[{i,inverse=InversePermutation[p]},
		Table[ Length[ Select[Take[p,inverse[[i]]], (# > i)&] ], {i,Length[p]-1}]
	] /; (Length[p] > 0)
Options[ToOrderedPairs] = {Type -> All}

ToOrderedPairs[g_Graph, opts___?OptionQ] := 
        Module[{type, op},
               type = Type /. Flatten[{opts, Options[ToOrderedPairs]}];
               op = If[UndirectedQ[g], Double[Edges[g]], Edges[g]];
               If[type === Simple, Union[Select[op, (#[[1]] != #[[2]])&]], op] 
        ]
Options[ToUnorderedPairs] = {Type -> All};

ToUnorderedPairs[g_Graph, opts___?OptionQ] := 
        Module[{type, el},
               type = Type /. Flatten[{opts, Options[ToUnorderedPairs]}];
               el = If[UndirectedQ[g], Edges[g], Map[Sort, Edges[g]]];
               If[type === All, el, Union[Select[el, (#[[1]] != #[[2]])&]]]
        ]
Toascii[s_String] := First[ ToCharacterCode[s] ]
TopologicalSort[g_Graph] := Range[V[g]] /; EmptyQ[g]
TopologicalSort[g_Graph] :=
	Module[{g1 = RemoveSelfLoops[g],e,indeg,zeros,v},
		e=ToAdjacencyLists[g1];
		indeg=InDegree[g1];
		zeros = Flatten[ Position[indeg, 0] ];
		Table[{v,zeros}={First[zeros],Rest[zeros]};
                      Scan[(indeg[[#]]--; If[indeg[[#]]==0, AppendTo[zeros,#]])&, e[[v]]];
                      v,
                      {V[g]}
		]
	] /; AcyclicQ[RemoveSelfLoops[g]] && !UndirectedQ[g]
TransitiveClosure[g_Graph] := g /; EmptyQ[g]

TransitiveClosure[g_Graph] := 
        Module[{e = ToAdjacencyMatrix[g]},
               If[UndirectedQ[g], 
                  FromAdjacencyMatrix[TC[e], Vertices[g, All]],
                  FromAdjacencyMatrix[TC[e], Vertices[g, All], Type -> Directed]
               ]
        ]

TC = Compile[{{e, _Integer, 2}},
             Module[{ne = e, n = Length[e], i, j, k},
                    Do[If[ne[[j, i]] != 0, 
                          Do[If[ne[[i, k]] != 0, ne[[j, k]] = 1], {k, n}]
                       ], {i, n}, {j, n}
                    ];
                    ne
             ]
     ]
TransitiveQ[r_?SquareMatrixQ] := 
        TransitiveQ[FromAdjacencyMatrix[r, Type->Directed]]

TransitiveQ[g_Graph] := IdenticalQ[g,TransitiveClosure[g]]
TransitiveReduction[g_Graph] := g /; EmptyQ[g]

TransitiveReduction[g_Graph] :=
	Module[{closure = ToAdjacencyMatrix[g]},
               If[UndirectedQ[g],
                  FromAdjacencyMatrix[TR[closure], Vertices[g, All]],
                  If[AcyclicQ[RemoveSelfLoops[g]],
		     FromAdjacencyMatrix[TRAcyclic[closure], Vertices[g, All], Type->Directed],
		     FromAdjacencyMatrix[TR[closure], Vertices[g, All], Type->Directed]
                  ] 
               ]
	] 

TR = Compile[{{closure, _Integer, 2}},
        Module[{reduction = closure, n = Length[closure], i, j, k},
               Do[
                  If[reduction[[i,j]]!=0 && reduction[[j,k]]!=0 &&
                     reduction[[i,k]]!=0 && (i!=j) && (j!=k) && (i!=k),
                     reduction[[i,k]] = 0
                  ],
                  {i,n},{j,n},{k,n}
                ]; 
                reduction
        ]
     ]
TRAcyclic = Compile[{{closure, _Integer, 2}},
               Module[{n = Length[closure], reduction = closure, i, j, k},
                      Do[
                         If[closure[[i,j]]!=0 && closure[[j,k]]!=0 &&
                            reduction[[i,k]]!=0 && (i!=j) && (j!=k) && (i!=k),
                            reduction[[i,k]] = 0
                         ],
                         {i,n},{j,n},{k,n}
                      ]; 
                      reduction
               ]
            ]
TranslateVertices[v:{{{_?NumericQ, _?NumericQ},___?OptionQ}...}, 
                  {x_?NumericQ, y_?NumericQ}] := 
        Module[{p = Map[First, v], np},
               np = TranslateVertices[p, {x, y}];
               Table[{np[[i]], Apply[Sequence, Rest[v[[i]]]]}, {i, Length[np]}]
        ]

TranslateVertices[v:{{_?NumericQ, _?NumericQ}...}, {x_?NumericQ, y_?NumericQ}] := 
        Map[(# + {x,y})&, v]//N

TranslateVertices[g_Graph, {x_?NumericQ, y_?NumericQ}] := 
        ChangeVertices[g, TranslateVertices[Vertices[g, All], {x, y}] ]

TranslateVertices[g_Graph, s_List, t_] :=
        Module[{v = Vertices[g, All]},
               ChangeVertices[g, v[[s]] = TranslateVertices[v[[s]], t]; v]
        ]
TransposeGraph[Graph[g_List,v_List]] := Graph[ Transpose[g], v ]
TransposePartition[{}] := {}

TransposePartition[p_List] :=
	Module[{s=Select[p,(#>0)&], i, row, r},
		row = Length[s];
		Table [r = row; While [s[[row]]<=i, row--]; r, {i,First[s]}]
	]
TransposeTableau[tb_List] :=
	Module[{t=Select[tb,(Length[#]>=1)&],row},
		Table[
			row = Map[First,t];
			t = Map[ Rest, Select[t,(Length[#]>1)&] ];
			row,
			{Length[First[tb]]}
		]
	]
TravelingSalesman[g_Graph] :=
	Module[{v, s={1}, sol={}, done, cost, e=ToAdjacencyLists[g],
                x, ind, best, n=V[g]},
		ind=Table[1,{n}];
		best = Infinity;
		While[ Length[s] > 0,
			v = Last[s];
			done = False;
			While[ ind[[v]] <= Length[e[[v]]] && !done,
				x = e[[v,ind[[v]]++]];
				done = (best > CostOfPath[g,Append[s,x]]) &&
					!MemberQ[s,x]
			];
			If[done, AppendTo[s,x], s=Drop[s,-1]; ind[[v]] = 1];
			If[(Length[s] == n),
				cost = CostOfPath[g, Append[s,First[s]]];
				If [(cost < best), sol = s; best = cost ];
				s = Drop[s,-1]
			]
		];
		Append[sol,First[sol]]
	] /; HamiltonianQ[g]

TravelingSalesmanBounds[g_Graph] := {LowerBoundTSP[g], UpperBoundTSP[g]}
TreeIsomorphismQ[t1_Graph?TreeQ, t2_Graph?TreeQ] := 
        (V[t1] == V[t2]) &&
        (IdenticalQ[t1, t2] || (TreeToCertificate[t1]==TreeToCertificate[t2]))
TreeQ[g_Graph] := ConnectedQ[g] && (M[g] == V[g]-1)

TreeToCertificate[t_Graph?TreeQ] := 
        Module[{codes = Table["01", {V[t]}], al, leaves, nbrLeaves, nt = t}, 
               While[V[nt] > 2, 
                     al = ToAdjacencyLists[nt]; 
                     leaves  = Flatten[Position[al, _?(Length[#] == 1 &)], 1]; 
                     nbrLeaves = Apply[Union, Map[al[[#]] &, leaves]]; 
                     Do[codes[[ nbrLeaves[[ i ]] ]] = 
                        StringInsert[
                            StringInsert[
                                Apply[StringJoin, 
                                      Sort[Append[codes[[Intersection[al[[nbrLeaves[[i]]]], leaves]]], 
                                                  StringDrop[StringDrop[codes[[nbrLeaves[[i]]]], 1],-1]
                                           ] 
                                      ]
                                ], "0", 1
                            ], "1", -1
                        ],      
                        {i, Length[nbrLeaves]}
                     ]; 
                     codes = codes[[Complement[Range[V[nt]], leaves]]]; 
                     nt = DeleteVertices[nt, leaves]
               ]; 
               Apply[StringJoin, Sort[codes]] 
        ]
TriangleInequalityQ[e_?SquareMatrixQ] :=
	Module[{i,j,k,n=Length[e],flag=True},
		Do [
                        If[(e[[i, k]]!=0)&&(e[[k, j]]!=0)&&(e[[i,j]] !=0),
				If[ e[[i,k]]+ e[[k,j]] < e[[i,j]],
					flag = False;
				]
			],
			{i,n},{j,n},{k,n}
		];
		flag
	]

TriangleInequalityQ[g_Graph] := 
        Block[{e = Edges[g], w = GetEdgeWeights[g], 
               m = Table[0, {V[g]}, {V[g]} ]},
              If[UndirectedQ[g], e = Double[e]; w = Join[w, w]]; 
              Do[m[[ e[[i,1]], e[[i, 2]] ]] = w[[ i ]], {i, Length[e]}
              ];
              TriangleInequalityQ[m]
        ] /; SimpleQ[g] 
 
Turan[n_Integer, 2] := GraphUnion[n, CompleteGraph[1]] /; (n > 0)
Turan[n_Integer,p_Integer] :=
	Module[{k = Floor[ n / (p-1) ], r},
		r = n - k (p-1);
		Apply[CompleteGraph, Join[Table[k,{p-1-r}], Table[k+1,{r}]]]
	] /; (n >= p) && (p > 2)
Turan[n_Integer, p_Integer] := CompleteGraph[n] /; (n < p) && (p > 2)
TutteGraph :=
 Graph[{{{1, 11}}, {{1, 12}}, {{1, 13}}, {{2, 3}}, {{2, 8}}, {{2, 20}}, 
  {{3, 4}}, {{3, 42}}, {{4, 5}}, {{4, 28}}, {{5, 6}}, {{5, 34}}, {{6, 7}}, 
  {{6, 46}}, {{7, 10}}, {{7, 30}}, {{8, 9}}, {{8, 22}}, {{9, 10}}, {{9, 23}}, 
  {{10, 25}}, {{11, 14}}, {{11, 15}}, {{12, 27}}, {{12, 29}}, {{13, 31}}, 
  {{13, 32}}, {{14, 16}}, {{14, 22}}, {{15, 16}}, {{15, 19}}, {{16, 17}}, 
  {{17, 18}}, {{17, 21}}, {{18, 19}}, {{18, 24}}, {{19, 25}}, {{20, 26}}, 
  {{20, 41}}, {{21, 22}}, {{21, 23}}, {{23, 24}}, {{24, 25}}, {{26, 27}}, 
  {{26, 39}}, {{27, 35}}, {{28, 29}}, {{28, 40}}, {{29, 35}}, {{30, 31}}, 
  {{30, 45}}, {{31, 36}}, {{32, 33}}, {{32, 36}}, {{33, 34}}, {{33, 43}}, 
  {{34, 44}}, {{35, 37}}, {{36, 38}}, {{37, 39}}, {{37, 40}}, {{38, 43}}, 
  {{38, 45}}, {{39, 41}}, {{40, 42}}, {{41, 42}}, {{43, 44}}, {{44, 46}}, 
  {{45, 46}}}, {{{0.518, 0.586}}, {{0.294, 0.986}}, {{0.504, 0.99}}, 
  {{0.69, 0.99}}, {{0.998, 0.616}}, {{0.872, 0.374}}, {{0.746, 0.152}}, 
  {{0.024, 0.558}}, {{0.17, 0.382}}, {{0.334, 0.15}}, {{0.454, 0.54}}, 
  {{0.518, 0.67}}, {{0.592, 0.53}}, {{0.35, 0.548}}, {{0.436, 0.484}}, 
  {{0.342, 0.502}}, {{0.296, 0.478}}, {{0.336, 0.418}}, {{0.408, 0.404}}, 
  {{0.332, 0.93}}, {{0.214, 0.502}}, {{0.138, 0.558}}, {{0.226, 0.43}}, 
  {{0.282, 0.38}}, {{0.368, 0.272}}, {{0.394, 0.822}}, {{0.464, 0.732}}, 
  {{0.638, 0.894}}, {{0.55, 0.734}}, {{0.696, 0.274}}, {{0.62, 0.482}}, 
  {{0.658, 0.55}}, {{0.768, 0.568}}, {{0.906, 0.6}}, {{0.508, 0.774}}, 
  {{0.674, 0.5}}, {{0.508, 0.83}}, {{0.728, 0.482}}, {{0.424, 0.864}}, 
  {{0.556, 0.894}}, {{0.414, 0.922}}, {{0.506, 0.934}}, {{0.784, 0.506}}, 
  {{0.842, 0.482}}, {{0.76, 0.376}}, {{0.824, 0.412}}}]
TwoColoring[g_Graph] := {} /; (V[g] == 0)
TwoColoring[g_Graph] := TwoColoring[MakeSimple[g]] /; (!SimpleQ[g]) || (!UndirectedQ[g])
TwoColoring[g_Graph] := 
        Module[{c = ConnectedComponents[g], p, b}, 
                Mod[Flatten[Map[Cases[#, _Integer]&, 
                                Transpose[Map[BreadthFirstTraversal[g, #[[1]], Level]&, c]]
                            ], 1
                    ], 2
                ] + 1
        ]
UndirectedQ[g_Graph] := (!MemberQ[GraphOptions[g], EdgeDirection->On]) &&
                        (!MemberQ[GraphOptions[g], EdgeDirection->True])
UnionSet[a_Integer,b_Integer,s_List] :=
	Module[{sa=FindSet[a,s], sb=FindSet[b,s], set=s},
		If[ set[[sa,2]] < set[[sb,2]], {sa,sb} = {sb,sa} ];
		set[[sa]] = {sa, Max[ set[[sa,2]], set[[sb,2]]+1 ]};
		set[[sb]] = {sa, set[[sb,2]]};
		set
	]
Uniquely3ColorableGraph :=
 Graph[{{{1, 2}}, {{1, 4}}, {{1, 7}}, {{1, 9}}, {{2, 5}}, {{2, 8}}, {{2, 10}}, 
  {{3, 4}}, {{3, 7}}, {{3, 8}}, {{3, 10}}, {{3, 12}}, {{4, 5}}, {{4, 11}}, 
  {{5, 6}}, {{5, 12}}, {{6, 7}}, {{6, 8}}, {{6, 9}}, {{6, 11}}, {{9, 12}}, 
  {{10, 11}}}, {{{0.434, 0.908}}, {{0.536, 0.908}}, {{0.626, 0.712}}, 
  {{0.54, 0.618}}, {{0.432, 0.622}}, {{0.342, 0.708}}, {{0.346, 0.81}}, 
  {{0.63, 0.812}}, {{0.124, 0.766}}, {{0.844, 0.774}}, {{0.43, 0.396}}, 
  {{0.576, 0.404}}}]
UnitransitiveGraph := 
        Module[{i, c = CircularEmbedding[10]}, 
               AddEdges[
                  GraphUnion[
                     Cycle[10], 
                     MakeUndirected[MakeGraph[Range[10], (Mod[#1 - #2, 10] == 3)&]]
                  ], 
                  Table[{i, i + 10}, {i, 10}]
               ] /. Graph[a_List, v_List] :> Graph[a, Join[1.5c, c]]
        ]
UnrankBinarySubset[n_Integer, 0] := {}
UnrankBinarySubset[n_Integer, m_Integer?Positive] := UnrankBinarySubset[Mod[n, 2^m], Range[m]] 
UnrankBinarySubset[n_Integer, l_List] := 
        l[[Flatten[Position[IntegerDigits[Mod[n, 2^Length[l]], 2, Length[l]], 1], 1]]]
UnrankGrayCodeSubset[0, {}] := {}

UnrankGrayCodeSubset[m_Integer, s_List] := 
       Module[{c = Table[0, {Length[s]}], n = Length[s], b, nm}, 
              nm = Mod[m, 2^n];
              b = IntegerDigits[nm, 2, Length[s]];
              c[[ 1 ]] = b[[1]]; 
              Do[c[[i]] = Mod[b[[i]] + b[[i-1]], 2], {i, 2, n}]; 
              s[[ Flatten[Position[c, 1], 1] ]]
       ] 
UnrankKSetPartition[r_Integer, {}, 0] := {} 

UnrankKSetPartition[0, set_List, k_Integer?Positive] :=
       Append[Table[{set[[i]]}, {i, 1, k-1}],
              Take[set, -(Length[set]-k+1)]
       ] /; (k <= Length[set])

UnrankKSetPartition[r_Integer?Positive, set_List, k_Integer?Positive] :=
       Block[{n = Length[set], t, j, $RecursionLimit = Infinity},
             If[r < StirlingSecond[n-1, k-1],
                Prepend[UnrankKSetPartition[r, Rest[set], k-1],
                        {First[set]}
                ],
                t = r - StirlingSecond[n-1, k-1];
                j = 1 + Mod[t, k];
                tempSP = UnrankKSetPartition[Quotient[t, k], Rest[set], k];
                Prepend[Delete[tempSP, j], Prepend[tempSP[[j]], First[set]]]
             ]
       ] /; (k <= Length[set])

UnrankKSetPartition[r_Integer, n_Integer, k_Integer] := 
       UnrankKSetPartition[r, Range[n], k] /; (k <= n) && (k >= 0)
UnrankKSubset[m_Integer, 1, s_List] := {s[[m + 1]]}
UnrankKSubset[0, k_Integer, s_List] := Take[s, k]
UnrankKSubset[m_Integer, k_Integer, s_List] := 
       Block[{i = 1, n = Length[s], x1, u, $RecursionLimit = Infinity}, 
             u = Binomial[n, k]; While[Binomial[i, k] < u - m, i++]; x1 = n - (i - 1); 
             Prepend[UnrankKSubset[m-u+Binomial[n-x1+1, k], k-1, Drop[s, x1]], s[[x1]]]
       ]
UP[r_Integer, n_Integer] := 
        Module[{r1 = r, q = n!}, 
               Table[r1 = Mod[r1, q]; 
                     q = q/(n - i + 1); 
                     Quotient[r1, q] + 1, 
                     {i, n}
               ]
        ]

UnrankPermutation[r_Integer, {}] := {}
UnrankPermutation[r_Integer, l_List] := 
        Module[{s = l, k, t, p = UP[Mod[r, Length[l]!], Length[l]]}, 
               Table[k = s[[t = p[[i]] ]];  
                     s = Delete[s, t]; 
                     k, 
                     {i, Length[ p ]}
               ] 
        ]

UnrankPermutation[r_Integer, n_Integer?Positive] := 
        UnrankPermutation[r, Range[n]] 
UnrankRGF[0, n_Integer?Positive] := Table[1, {n}]

UnrankRGF[r_Integer?Positive, n_Integer?Positive] := 
        Module[{f = Table[1, {n}], m = 1, tr}, tr = r; DValues[n, 1];
               Do[If[tr >= m DValues[n - i, m], 
                     (f[[i]] = m + 1; tr = tr - m DValues[n - i, m]; m++), 
                     (f[[i]] = Quotient[tr, DValues[n - i, m]] + 1; 
                      tr = Mod[tr, DValues[n - i, m]])
                  ], {i, 2, n}
               ]; 
               f
        ]
UnrankSetPartition[0, set_List] := {set}

UnrankSetPartition[r_Integer?Positive, set_List] :=
       Block[{n = Length[set], k = 0, sum = 0, $RecursionLimit = Infinity},
             While[sum <= r, k++; sum = sum + StirlingSecond[n, k]];
             UnrankKSetPartition[r - (sum - StirlingSecond[n, k]), set, k]
       ] /; (r < BellB[ Length[set] ])

UnrankSetPartition[0, 0] = {{}}
UnrankSetPartition[r_Integer, n_Integer?Positive] := UnrankSetPartition[r, Range[n]] /; (r >= 0)
UnrankSubset[m_Integer, l_List] := UnrankGrayCodeSubset[m, l]
UnweightedQ[g_Graph] := (Count[GetEdgeWeights[g],1] === M[g])
UpperBoundTSP[g_Graph] :=
	CostOfPath[g, Append[DepthFirstTraversal[MinimumSpanningTree[g],1],1]]
V[Graph[_List, v_List, ___?OptionQ]] := Length[v]
Options[VertexColoring] = {Algorithm -> Brelaz};

VertexColoring[g_Graph, opts___?OptionQ] :=
        Module[{algo = Algorithm /. Flatten[{opts, Options[VertexColoring]}]},
               If[algo === Brelaz, BrelazColoring[g], MinimumVertexColoring[g] ]
        ]
VertexConnectivity[g_Graph] := V[g] - 1 /; CompleteQ[g]

VertexConnectivity[g_Graph] :=
	Module[{p=VertexConnectivityGraph[g],k=V[g],i=0,notedges},
		notedges = ToUnorderedPairs[ GraphComplement[g] ];
		While[i++ <= k,
		      k=Min[
		            Map[
		                (NetworkFlow[p,2 #[[1]],2 #[[2]]-1])&,
			        Select[notedges,(First[#]==i)&]
                            ],
                            k
                        ]
                ];
                k
        ]
VertexConnectivity[g_Graph, Cut] := Range[V[g]-1] /; CompleteQ[g]

VertexConnectivity[g_Graph, Cut] :=
	Module[{p=VertexConnectivityGraph[g],k=V[g],i=0,notedges,c = {}, tc},
		notedges = ToUnorderedPairs[ GraphComplement[g] ];
		While[i++ <= k,
		      {k, c} =First[Sort[
                                         Append[
		                         Map[({Length[tc = NetworkFlow[p,2 #[[1]],2 #[[2]]-1,Cut]], tc})&,
			                 Select[notedges,(First[#]==i)&]],
                                         {k, c}
                                         ]
                                    ]
                              ]
                ];
               Map[Ceiling[First[#]/2]&, c] 
        ]
VertexConnectivityGraph[g_Graph] :=
	Module[{n=V[g],e=Edges[g,All], v=Vertices[g, All], epsilon=0.05, 
                ne, nv},
               ne = Join[Map[{{2First[#][[1]], 2First[#][[2]]-1}, EdgeWeight->2}& ,e],
                         Table[{{2i-1, 2i}}, {i, n}]
                    ];
               If[UndirectedQ[g], 
                  ne = Join[ne, 
                            Map[{{2First[#][[2]], 2First[#][[1]]-1}, EdgeWeight->2}&, e]
                       ]
               ];
               nv = Flatten[
                             Map[
                                 {Prepend[Rest[#], First[#]-{epsilon, 0}],
                                  Prepend[Rest[#], First[#]+{epsilon, 0}]}&,
                                 v
                             ], 1
                    ];
               SetGraphOptions[ChangeEdges[ChangeVertices[g, nv], ne], 
                               EdgeDirection -> True
               ]
	]
Options[VertexCover] = {Algorithm -> Approximate};

VertexCover[g_Graph, opts___?OptionQ] := 
       Module[{algo = Algorithm /. Flatten[{opts, Options[VertexCover]}]},
              Switch[algo, Approximate, ApproximateVertexCover[g], 
                           Greedy, GreedyVertexCover[g], 
                           Optimum, MinimumVertexCover[g]
              ]
       ]
VertexCoverQ[g_Graph, vc_List] := CliqueQ[ GraphComplement[g], Complement[Range[V[g]], vc] ]

Vertices[Graph[_List, v_List, ___?OptionQ]] := Map[First[#]&, v]
Vertices[Graph[_List, v_List, ___?OptionQ], All] := v
WaltherGraph := 
 Graph[{{{1, 2}}, {{2, 3}}, {{2, 9}}, {{3, 4}}, {{3, 14}}, {{4, 5}}, 
  {{4, 17}}, {{5, 6}}, {{6, 7}}, {{6, 20}}, {{7, 8}}, {{7, 21}}, {{8, 22}}, 
  {{9, 10}}, {{9, 14}}, {{10, 11}}, {{10, 23}}, {{11, 12}}, {{11, 21}}, 
  {{12, 13}}, {{14, 15}}, {{15, 16}}, {{15, 24}}, {{16, 17}}, {{16, 18}}, 
  {{18, 19}}, {{19, 20}}, {{19, 25}}, {{21, 25}}, {{23, 24}}, {{24, 25}}}, 
 {{{0.526, 0.958}}, {{0.526, 0.854}}, {{0.594, 0.768}}, {{0.652, 0.686}}, 
  {{0.724, 0.562}}, {{0.79, 0.44}}, {{0.87, 0.304}}, {{0.91, 0.22}}, 
  {{0.4, 0.704}}, {{0.298, 0.53}}, {{0.162, 0.304}}, {{0.114, 0.24}}, 
  {{0.066, 0.17}}, {{0.508, 0.68}}, {{0.508, 0.552}}, {{0.596, 0.56}}, 
  {{0.628, 0.622}}, {{0.63, 0.488}}, {{0.662, 0.412}}, {{0.726, 0.42}}, 
  {{0.544, 0.304}}, {{0.962, 0.14}}, {{0.37, 0.482}}, {{0.45, 0.422}}, 
  {{0.544, 0.398}}}]
WeaklyConnectedComponents[g_Graph] := ConnectedComponents[MakeUndirected[g]]
Wheel[n_Integer] :=
        Graph[Join[Table[{{i, n}}, {i, n-1}], Table[{{i, i+1}}, {i, n-2}], 
                      {{{1, n-1}}}
                 ], 
                 Append[CircularEmbedding[n-1], {{0, 0}}]
	] /; (n >= 3)
WriteGraph[g_Graph, file_] :=
        Module[{edges=ToAdjacencyLists[g],v=N[NormalizeVertices[Vertices[g]]],i,x,y},
                OpenWrite[file];
                Do[
                        WriteString[file,"      ",ToString[i]];
                        {x,y} = Chop[ v [[i]] ];
                        WriteString[file,"      ",ToString[x]," ",ToString[y]];
                        Scan[
                                (WriteString[file,"     ",ToString[ # ]])&,
                                edges[[i]]
                        ];
                        Write[file],
                        {i,V[g]}
                ];
                Close[file];
        ]
YVectorToTableau[y_List] :=
	Module[{k},
		Table[ Flatten[Position[y,k]], {k,Length[Union[y]]}]
	]
Attributes[Zap] = {Listable}
Zap[n_Integer] := n
Zap[n_Real] := If[Chop[Round[n] - n] == 0, Round[n], n]
End[]

Protect[
AcyclicQ, 
AddEdge,
AddEdges, 
AddVertex,
AddVertices,
Algorithm,
AllPairsShortestPath, 
AlternatingGroup,
AlternatingGroupIndex,
AlternatingPaths,
AnimateGraph,
AntiSymmetricQ,
Approximate,
ApproximateVertexCover,
ArticulationVertices,
Automorphisms,
Backtrack, 
BellB,
BellmanFord,
BiconnectedComponents, 
BiconnectedQ,
BinarySearch, 
BinarySubsets, 
BipartiteMatching, 
BipartiteMatchingAndCover, 
BipartiteQ,
Box, 
BooleanAlgebra,
BreadthFirstTraversal, 
Brelaz,
BrelazColoring,
Bridges, 
ButterflyGraph,
ToCanonicalSetPartition,
CageGraph,
CartesianProduct, 
Center, 
ChangeEdges, 
ChangeVertices,
ChromaticNumber, 
ChromaticPolynomial,
ChvatalGraph,
CirculantGraph, 
CircularEmbedding, 
CircularVertices, 
CliqueQ, 
CoarserSetPartitionQ,
CodeToLabeledTree, 
Cofactor,
CompleteBinaryTree,
CompleteKaryTree,
CompleteKPartiteGraph,
CompleteGraph,
CompleteQ,
Compositions, 
ConnectedComponents, 
ConnectedQ, 
ConstructTableau,
Contract, 
CostOfPath,
CoxeterGraph,
CubeConnectedCycle,
CubicalGraph,
Cut,
Cycle, 
Cycles, 
CycleIndex,
CycleStructure,
Cyclic,
CyclicGroup,
CyclicGroupIndex,
DeBruijnGraph, 
DeBruijnSequence, 
Degrees,
DegreesOf2Neighborhood,
DegreeSequence,
DeleteCycle, 
DeleteEdge, 
DeleteEdges, 
DeleteFromTableau, 
DeleteVertex,
DeleteVertices, 
DepthFirstTraversal,
DerangementQ, 
Derangements, 
Diameter, 
Dihedral,
DihedralGroup,
DihedralGroupIndex,
Dijkstra, 
DilateVertices,
Directed, 
Disk, 
Distances,
DistinctPermutations, 
Distribution, 
DodecahedralGraph,
DominatingIntegerPartitionQ,
DominationLattice,
DurfeeSquare, 
Eccentricity,
Edge,
EdgeChromaticNumber, 
EdgeColor,
EdgeColoring, 
EdgeConnectivity, 
EdgeDirection, 
EdgeLabel, 
EdgeLabelColor, 
EdgeLabelPosition,
LoopPosition, 
Edges, 
EdgeStyle, 
EdgeWeight, 
Element,
EmptyGraph, 
EmptyQ, 
EncroachingListSet, 
EquivalenceClasses, 
EquivalenceRelationQ, 
Equivalences, 
Euclidean,
Eulerian,
EulerianCycle,
EulerianQ, 
ExactRandomGraph, 
ExpandGraph, 
ExtractCycles, 
FerrersDiagram, 
FindCycle,
FindSet, 
FiniteGraphs,
FirstLexicographicTableau, 
FolkmanGraph,
FranklinGraph,
FruchtGraph,
FromAdjacencyLists,
FromAdjacencyMatrix,
FromCycles,
FromInversionVector, 
FromOrderedPairs,
FromUnorderedPairs, 
FunctionalGraph,
GeneralizedPetersenGraph,
GetEdgeLabels,
GetEdgeWeights,
GetVertexLabels,
GetVertexWeights,
Girth, 
GraphCenter, 
GraphComplement, 
GraphDifference, 
GraphicQ,
GraphIntersection,
GraphJoin, 
GraphOptions, 
GraphPolynomial,
GraphPower, 
GraphProduct, 
GraphSum, 
GraphUnion, 
GrayCode,
GrayCodeSubsets, 
GrayCodeKSubsets, 
GrayGraph,
Greedy,
GreedyVertexCover,
GridGraph, 
GrotztschGraph,
HamiltonianCycle, 
HamiltonianPath, 
HamiltonianQ, 
Harary,
HasseDiagram, 
Heapify, 
HeapSort, 
HeawoodGraph,
HerschelGraph,
HideCycles, 
HighlightedEdgeColors,
HighlightedEdgeStyle,
HighlightedVertexColors,
HighlightedVertexStyle,
Highlight,
Hypercube, 
IcosahedralGraph,
IdenticalQ,
IdentityPermutation,
IncidenceMatrix,
InDegree,
IndependentSetQ, 
Index, 
InduceSubgraph,
InitializeUnionFind,
InsertIntoTableau, 
IntervalGraph, 
Invariants,
InversePermutation, 
InversionPoset,
Inversions,
InvolutionQ, 
Involutions, 
IsomorphicQ, 
Isomorphism, 
IsomorphismQ, 
Josephus, 
KnightsTourGraph,
KSetPartitions,
KSubsetGroup,
KSubsetGroupIndex,
KSubsets,
LNorm,
LabeledTreeToCode, 
Large, 
LastLexicographicTableau,
LexicographicPermutations, 
LexicographicSubsets, 
LeviGraph,
LineGraph,
ListGraphs,
ListNecklaces,
LongestIncreasingSubsequence, 
LowerLeft, 
LowerRight, 
M,
MakeDirected,
MakeGraph, 
MakeSimple, 
MakeUndirected,
MaximalMatching,
MaximumAntichain, 
MaximumClique, 
MaximumIndependentSet,
MaximumSpanningTree, 
McGeeGraph,
MeredithGraph,
MinimumChainPartition, 
MinimumChangePermutations,
MinimumSpanningTree, 
MinimumVertexColoring, 
MinimumVertexCover, 
MultipleEdgesQ,
MultiplicationTable,
MycielskiGraph,
NecklacePolynomial,
Neighborhood,
NetworkFlow, 
NetworkFlowEdges, 
NextBinarySubset, 
NextComposition, 
NextGrayCodeSubset, 
NextKSubset,
NextLexicographicSubset,
NextPartition, 
NextPermutation, 
NextSubset, 
NextTableau, 
NoMultipleEdges, 
NonLineGraphs,
Normal, 
NormalDashed, 
NormalizeVertices,
NoPerfectMatchingGraph,
NoSelfLoops, 
NthPair,
NthPermutation, 
NthSubset, 
NumberOfCompositions,
NumberOfDerangements, 
NumberOfDirectedGraphs,
NumberOfGraphs,
NumberOfInvolutions, 
NumberOf2Paths,
NumberOfKPaths,
NumberOfNecklaces,
NumberOfPartitions,
NumberOfPermutationsByCycles, 
NumberOfPermutationsByInversions, 
NumberOfPermutationsByType, 
NumberOfSpanningTrees, 
NumberOfTableaux,
OctahedralGraph,
OddGraph,
One,
Optimum,
OrbitInventory,
OrbitRepresentatives,
Orbits,
Ordered,
OrientGraph, 
OutDegree,
PairGroup,
PairGroupIndex,
Parent,
ParentsToPaths,
PartialOrderQ, 
PartitionLattice,
PartitionQ, 
Partitions, 
Path, 
PerfectQ,
PermutationGraph,
PermutationGroupQ, 
PermutationQ, 
PermutationToTableaux, 
PermutationType, 
PermutationWithCycle,
Permute, 
PermuteSubgraph, 
PetersenGraph,
PlanarQ,
PlotRange, 
Polya,
PseudographQ, 
RadialEmbedding, 
Radius,
RandomComposition, 
RandomGraph, 
RandomHeap, 
RandomInteger,
RandomKSetPartition,
RandomKSubset,
RandomPartition, 
RandomPermutation, 
RandomRGF,
RandomSetPartition,
RandomSubset, 
RandomTableau, 
RandomTree, 
RandomVertices, 
RankBinarySubset, 
RankedEmbedding, 
RankGraph,
RankGrayCodeSubset,
RankKSetPartition,
RankKSubset,
RankPermutation, 
RankRGF,
RankSetPartition,
RankSubset, 
ReadGraph,
RealizeDegreeSequence, 
ReflexiveQ,
RegularGraph, 
RegularQ, 
RemoveMultipleEdges,
RemoveSelfLoops, 
ResidualFlowGraph,
RevealCycles, 
ReverseEdges,
RGFQ,
RGFs,
RGFToSetPartition,
RobertsonGraph,
RootedEmbedding, 
RotateVertices, 
Runs, 
SamenessRelation,
SelectionSort, 
SelfComplementaryQ, 
SelfLoopsQ,
SetEdgeWeights,
SetGraphOptions, 
SetPartitions,
SetPartitionListViaRGF,
SetPartitionQ,
SetPartitionToRGF,
SetEdgeLabels,
SetVertexLabels,
SetVertexWeights,
ShakeGraph, 
ShortestPath, 
ShortestPathSpanningTree,
ShowLabeledGraph,
ShowGraph, 
ShowGraphArray, 
ShuffleExchangeGraph,
SignaturePermutation,
Simple, 
SimpleQ,
Small, 
SmallestCyclicGroupGraph,
Spectrum, 
SpringEmbedding, 
StableMarriage, 
Star,
StirlingFirst, 
StirlingSecond, 
Strings, 
Strong,
StronglyConnectedComponents,
Subsets, 
SymmetricGroup,
SymmetricGroupIndex,
SymmetricQ,
TableauClasses, 
TableauQ, 
Tableaux,
TableauxToPermutation, 
TetrahedralGraph,
Thick, 
ThickDashed, 
Thin, 
ThinDashed, 
ThomassenGraph,
ToAdjacencyLists,
ToAdjacencyMatrix, 
ToCycles,
ToInversionVector, 
ToOrderedPairs,
TopologicalSort, 
ToUnorderedPairs, 
TransitiveClosure, 
TransitiveQ,
TransitiveReduction, 
TranslateVertices, 
TransposePartition, 
TransposeTableau,
TravelingSalesmanBounds, 
TravelingSalesman, 
TreeIsomorphismQ,
TreeQ, 
TreeToCertificate,
TriangleInequalityQ,
Turan, 
TutteGraph,
TwoColoring, 
Type,
Undirected,
UndirectedQ, 
Uniquely3ColorableGraph,
UnionSet, 
UnitransitiveGraph,
UnrankBinarySubset,
UnrankGrayCodeSubset,
UnrankKSetPartition,
UnrankKSubset,
UnrankPermutation,
UnrankRGF,
UnrankSetPartition,
UnrankSubset,
UnweightedQ,
UpperLeft, 
UpperRight, 
V, 
VertexColor, 
VertexColoring, 
VertexConnectivity, 
VertexConnectivityGraph, 
VertexCover,
VertexCoverQ, 
VertexLabel, 
VertexLabelColor, 
VertexNumber, 
VertexNumberColor,
VertexStyle, 
VertexWeight, 
Vertices,
WaltherGraph,
Weak, 
WeaklyConnectedComponents, 
WeightingFunction,
WeightRange,
Wheel, 
WriteGraph,
Zoom
]

EndPackage[ ]
