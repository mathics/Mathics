(* :Title: Combinatorica
*)
(* :Author:
	Steven S. Skiena
*)
(* :Summary:

	Implementing Discrete Mathematics: Combinatorics and Graph Theory
				with Mathematica

This package contains all the programs from the book, "Implementing
Discrete Mathematics: Combinatorics and Graph Theory with Mathematica"
by Steven S. Skiena, Addison-Wesley Publishing Co., Advanced Book Program,
350 Bridge Parkway, Redwood City CA 94065.  ISBN 0-201-50943-1.
For ordering information, call 1-800-447-2226.

These programs can be obtained on Macintosh and MS-DOS disks by sending
$15.00 to Discrete Mathematics Disk, Wolfram Research Inc.,
PO Box 6059, Champaign, IL 61826-9905. (217)-398-0700.

Any comments, bug reports, or requests to get on the Combinatorica
mailing list should be forwarded to:

	Steven Skiena
	Department of Computer Science
	State University of New York
	Stony Brook, NY 11794

	skiena@sbcs.sunysb.edu

	(516)-632-9026 / 8470
*)
(* :Context: DiscreteMath`Combinatorica`
*)
(* :Package Version: .9	(2/29/92 Beta Release)
*)

(**** Note: some very small changes have been made to make this
to work with Mathics 1.1.1 ****)

(* :Copyright: Copyright 1990, 1991, 1992 by Steven S. Skiena

This package may be copied in its entirety for nonprofit purposes only.
Sale, other than for the direct cost of the media, is prohibited.  This
copyright notice must accompany all copies.

The author, Wolfram Research, and Addison-Wesley Publishing Company,
Inc. make no representations, express or implied, with respond to this
documentation, of the software it describes and contains, including
without limitations, any implied warranties of mechantability or fitness
for a particular purpose, all of which are expressly disclaimed.  The
author, Wolfram Research, or Addison-Wesley, their licensees,
distributors and dealers shall in no event be liable for any indirect,
incidental, or consequential damages.
*)
(* :History:
	Version .8 by Steven S. Skiena, July 1991.
	Version .7 by Steven S. Skiena, January 1991.
	Version .6 by Steven S. Skiena, June 1990.
*)
(* :Keywords:
	adjacency, automorphism, chromatic, clique, coloring,
	combination, composition, connected components, connectivity, cycle,
	de Bruijn, degree, derangement, Dijkstra, Durfee,
	embedding, equivalence, Eulerian, Ferrers,
	geodesic, graph, Gray code, group, Hamiltonian cycle, Harary, Hasse,
	heap, hypercube, interval, inversion, involution, isomorphism,
	Josephus, network,
	partition, perfect, permutation, planar graph, Polya, pseudograph,
	self-loop, sequence, signature, simple, spanning tree,
	stable marriage, star, Stirling,
	transitive closure, traveling salesman tour, tree, Turan,
	vertex cover, wheel, Young tableau
*)
(* :Source:
	Steven Skiena: "Implementing Discrete Mathematics: Combinatorics
			and Graph Theory with Mathematica",
			Addison-Wesley Publishing Co.
*)
(* :Mathematica Version: 0.9.0 for Mathics
 This is Mathematica Version 0.9 adapted for Mathics.
*)

BeginPackage["DiscreteMath`CombinatoricaV0.9`"]
Unprotect[All]
Unprotect[Subsets]

Graph::usage = "Graph[g,v] is the header for a graph object where g is an adjacency matrix and v is a list of vertices."

Directed::usage = "Directed is an option to inform certain functions that the graph is directed."

Undirected::usage = "Undirected is an option to inform certain functions that the graph is undirected."

Edge::usage = "Edge is an option to inform certain functions to work with edges instead of vertices."

All::usage = "All is an option to inform certain functions to return all solutions, instead of just the first one."

AcyclicQ::usage = "AcyclicQ[g] returns True if graph g is acyclic. AcyclicQ[g,Directed] returns True if g is a directed acyclic graph."

AddEdge::usage = "AddEdge[g,{x,y}] returns graph g with a new undirected edge {x,y}, while AddEdge[g,{x,y},Directed] returns graph g with a new directed edge {x,y}."

AddVertex::usage = "AddVertex[g] adds a disconnected vertex to graph g."

AllPairsShortestPath::usage = "AllPairsShortestPath[g] returns a matrix, where the (i,j)th entry is the length of the shortest path in g between vertices i and j."

ArticulationVertices::usage = "ArticulationVertices[g] returns a list of all articulation vertices in graph g, vertices whose removal will disconnect the graph."

Automorphisms::usage = "Automorphisms[g] finds the automorphism group of a graph g, the set of isomorphisms of g with itself."

Backtrack::usage = "Backtrack[s,partialQ,solutionQ] performs a backtrack search of the state space s, expanding a partial solution so long as partialQ is True and returning the first complete solution, as identified by solutionQ."

BiconnectedComponents::usage = "BiconnectedComponents[g] returns a list of all the biconnected components of graph g."

BiconnectedComponents::usage = "BiconnectedComponents[g] returns a list of the biconnected components of graph g."

BiconnectedQ::usage = "BiconnectedQ[g] returns True if graph g is biconnected."

BinarySearch::usage = "BinarySearch[l,k,f] searches sorted list l for key k and returns the the position of l containing k, with f a function which extracts the key from an element of l."

BinarySubsets::usage = "BinarySubsets[l] returns all subsets of l ordered according to the binary string defining each subset."

BipartiteMatching::usage = "BipartiteMatching[g] returns the list of edges associated with a maximum matching in bipartite graph g."

BipartiteQ::usage = "BipartiteQ[g] returns True if graph g is bipartite."

BreadthFirstTraversal::usage = "BreadthFirstTraversal[g,v] performs a breadth-first traversal of graph g starting from vertex v, and returns a list of vertices in the order in which they were encountered."

Bridges::usage = "Bridges[g] returns a list of the bridges of graph g, the edges whose removal disconnects the graph."

CartesianProduct::usage = "CartesianProduct[l1,l2] returns the Cartesian product of lists l1 and l2."

CatalanNumber::usage = "CatalanNumber[n] computes the nth Catalan number, for a positive integer n."

ChangeEdges::usage = "ChangeEdges[g,e] constructs a graph with the adjacency matrix e and the embedding of graph g."

ChangeVertices::usage = "ChangeVertices[g,v] constructs a graph with the adjacency matrix of graph g and the list v as its embedding."

ChromaticNumber::usage = "ChromaticNumber[g] computes the chromatic number of the graph, the fewest number of colors necessary to color the graph."

ChromaticPolynomial::usage = "ChromaticPolynomial[g,z] returns the chromatic polynomial P(z) of graph g, which counts the number of ways to color g with exactly z colors."

CirculantGraph::usage = "CirculantGraph[n,l] constructs a circulant graph on n vertices, meaning the ith vertex is adjacent to the (i+j)th and (i-j)th vertex, for each j in list l."

CircularVertices::usage = "CircularVertices[n] constructs a list of n points equally spaced on a circle."

CliqueQ::usage = "CliqueQ[g,c] returns True if the list of vertices c defines a clique in graph g."

CodeToLabeledTree::usage = "CodeToLabeledTree[l] constructs the unique labeled tree on n vertices from the Prufer code l, which consists of a list of n-2 integers from 1 to n."

Cofactor::usage = "Cofactor[m,{i,j}] calculates the (i,j)th cofactor of matrix m."

CompleteQ::usage = "CompleteQ[g] returns True if graph g is complete."

Compositions::usage = "Compositions[n,k] returns a list of all compositions of integer n into k parts."

ConnectedComponents::usage = "ConnectedComponents[g] returns the vertices of graph g partitioned into connected components."

ConnectedQ::usage = "ConnectedQ[g] returns True if undirected graph g is connected. ConnectedQ[g,Directed] and ConnectedQ[g,Undirected] returns True if g is strongly or weakly connected, respectively."

ConstructTableau::usage = "ConstructTableau[p] performs the bumping algorithm repeatedly on each element of permutation p, resulting in a distinct Young tableau."

Contract::usage = "Contract[g,{x,y}] gives the graph resulting from contracting edge {x,y} of graph g."

CostOfPath::usage = "CostOfPath[g,p] sums up the weights of the edges in graph g defined by the path p."

Cycle::usage = "Cycle[n] constructs the cycle on n vertices, a 2-regular connected graph."

DeBruijnSequence::usage = "DeBruijnSequence[a,n] constructs a de Bruijn sequence on the alphabet described by list a, the shortest sequence such that every string of length n on a occurs as a contiguous subrange of the sequence."

DegreeSequence::usage = "DegreeSequence[g] returns the sorted degree sequence of graph g."

DeleteCycle::usage = "DeleteCycle[g,c] deletes undirected cycle c from graph g. DeleteCycle[g,c,Directed] deletes directed cycle c from graph g."

DeleteEdge::usage = "DeleteEdge[g,{x,y}] returns graph g minus undirected edge {x,y}, while DeleteEdge[g,{x,y},Directed] returns graph g minus directed edge {x,y}."

DeleteFromTableau::usage = "DeleteFromTableau[t,r] deletes the last element of row r from Young tableaux t."

DeleteVertex::usage = "DeleteVertex[g,v] deletes vertex v from graph g."

DepthFirstTraversal::usage = "DepthFirstTraversal[g,v] performs a depth-first traversal of graph g starting from vertex v, and returns a list of vertices in the order in which they were encountered."

DerangementQ::usage = "DerangementQ[p] tests whether permutation p is a derangement, a permutation without a fixed point."

Derangements::usage = "Derangements[p] constructs all derangements of permutation p."

Diameter::usage = "Diameter[g] computes the diameter of graph g, the length of the longest shortest path between two vertices of g."

Dijkstra::usage = "Dijkstra[g,v] returns the shortest path spanning tree and associated distances from vertex v of graph g."

DilateVertices::usage = "DilateVertices[v,d] multiplies each coordinate of each vertex position in list l by d, thus dilating the embedding."

DistinctPermutations::usage = "DistinctPermutations[l] returns all permutations of the multiset described by list l."

Distribution::usage = "Distribution[l,set] lists the frequency of occurrence of each element of set in list l."

DurfeeSquare::usage = "DurfeeSquare[p] computes the number of rows involved in the Durfee square of partition p, the side of the largest sized square contained within the Ferrers diagram of p."

Eccentricity::usage = "Eccentricity[g] computes the eccentricity of each vertex v of graph g, the length of the longest shortest path from v."

EdgeChromaticNumber::usage = "EdgeChromaticNumber[g] computes the fewest number of colors necessary to color each edge of graph g, so that no two edges incident on the same vertex have the same color."

EdgeColoring::usage = "EdgeColoring[g] uses Brelaz's heuristic to find a good, but not necessarily minimal, edge coloring of graph g."

EdgeConnectivity::usage = "EdgeConnectivity[g] computes the minimum number of edges whose deletion from graph g disconnects it."

Edges::usage = "Edges[g] returns the adjacency matrix of graph g."

Element::usage = "Element[a,l] returns the lth element of nested list a, where l is a list of indices"

EmptyGraph::usage = "EmptyGraph[n] generates an empty graph on n vertices."

EmptyQ::usage = "EmptyQ[g] returns True if graph g contains no edges."

EncroachingListSet::usage = "EncroachingListSet[p] constructs the encroaching list set associated with permutation p."

EquivalenceClasses::usage = "EquivalenceClasses[r] identifies the equivalence classes among the elements of matrix r."

EquivalenceRelationQ::usage = "EquivalenceRelationQ[r] returns True if the matrix r defines an equivalence relation. EquivalenceRelationQ[g] tests whether the adjacency matrix of graph g defines an equivalence relation."

Equivalences::usage = "Equivalences[g,h] lists the vertex equivalence classes between graphs g and h defined by the all-pairs shortest path heuristic."

EulerianCycle::usage = "EulerianCycle[g] finds an Eulerian circuit of undirected graph g if one exists. EulerianCycle[g,Directed] finds an Eulerian circuit of directed graph g if one exists."

EulerianQ::usage = "EulerianQ[g] returns True if graph g is Eulerian, meaning there exists a tour which includes each edge exactly once. EulerianQ[g,Directed] returns True if directed graph g is Eulerian."

Eulerian::usage = "Eulerian[n,k] computes the number of permutations of length n with k runs."

ExactRandomGraph::usage = "ExactRandomGraph[n,e] constructs a random labeled graph of exactly e edges and n vertices."

ExpandGraph::usage = "ExpandGraph[g,n] expands graph g to n vertices by adding disconnected vertices."

ExtractCycles::usage = "ExtractCycles[g] returns a list of edge disjoint cycles in graph g."

FerrersDiagram::usage = "FerrersDiagram[p] draws a Ferrers diagram of integer partition p."

FindCycle::usage = "FindCycle[g] finds a list of vertices which define an undirected cycle in graph g. FindCycle[g,Directed] finds a directed cycle in graph g."

FindSet::usage = "FindSet[n,s] returns the root of the set containing n in union-find data structure s."

FirstLexicographicTableau::usage = "FirstLexicographicTableau[p] constructs the first Young tableau with shape described by partition p."

FromAdjacencyLists::usage = "FromAdjacencyLists[l] constructs an adjacency matrix representation for a graph with adjacency lists l, using a circular embedding. FromAdjacencyLists[l,v] uses v as the embedding for the resulting graph."

FromCycles::usage = "FromCycles[c] restores a cycle structure c to the original permutation."

FromInversionVector::usage = "FromInversionVector[v] reconstructs the unique permutation with  inversion vector v."

FromOrderedPairs::usage = "FromOrderedPairs[l] constructs an adjacency matrix representation from a list of ordered pairs l, using a circular embedding. FromOrderedPairs[l,v] uses v as the embedding for the resulting graph."

FromUnorderedPairs::usage = "FromUnorderedPairs[l] constructs an adjacency matrix representation from a list of unordered pairs l, using a circular embedding. FromUnorderedPairs[l,v] uses v as the embedding for the resulting graph."

FunctionalGraph::usage = "FunctionalGraph[f,n] constructs the functional digraph on n vertices defined by integer function f."

Girth::usage = "Girth[g] computes the length of the shortest cycle in unweighted graph g."

GraphCenter::usage = "GraphCenter[g] returns a list of the vertices of graph g with minimum eccentricity."

GraphComplement::usage = "GraphComplement[g] returns the complement of graph g."

GraphDifference::usage = "GraphDifference[g,h] constructs the graph resulting from subtracting the adjacency matrix of graph g from that of graph h."

GraphIntersection::usage = "GraphIntersection[g,h] constructs the graph defined by the edges which are in both graph g and graph h."

GraphJoin::usage = "GraphJoin[g,h] constructs the join of graphs g and h."

GraphPower::usage = "GraphPower[g,k] computes the kth power of graph g, meaning there is an edge between any pair of vertices of g with a path between them of length at most k."

GraphProduct::usage = "GraphProduct[g,h] constructs the product of graphs g and h."

GraphSum::usage = "GraphSum[g,h] constructs the graph resulting from adding the adjacency matrices of graphs g and h."

GraphUnion::usage = "GraphUnion[g,h] constructs the union of graphs g and h. GraphUnion[n,g] constructs n copies of graph g, where n is an integer."

GraphicQ::usage = "GraphicQ[s] returns True if the list of integers s is graphic, and thus represents a degree sequence of some graph."

GrayCode::usage = "GrayCode[l] constructs a binary reflected Gray code on set l."

GridGraph::usage = "GridGraph[n,m] constructs an n*m grid graph, the product of paths on n and m vertices."

HamiltonianCycle::usage = "HamiltonianCycle[g] finds a Hamiltonian cycle in graph g if one exists. HamiltonianCycle[g,All] returns all Hamiltonian cycles of graph g."

HamiltonianQ::usage = "HamiltonianQ[g] returns True if there exists a Hamiltonian cycle in graph g, in other words, if there exists a cycle which visits each vertex exactly once."

Harary::usage = "Harary[k,n] constructs the minimal k-connected graph on n vertices."

HasseDiagram::usage = "HasseDiagram[g] constructs a Hasse diagram of the relation defined by directed acyclic graph g."

HeapSort::usage = "HeapSort[l] performs a heap sort on the items of list l."

Heapify::usage = "Heapify[p] builds a heap from permutation p."

HideCycles::usage = "HideCycles[c] canonically encodes the cycle structure c into a unique permutation."

Hypercube::usage = "Hypercube[n] constructs an n-dimensional hypercube."

IdenticalQ::usage = "IdenticalQ[g,h] returns True if graphs g and h have identical adjacency matrices."

IncidenceMatrix::usage = "IncidenceMatrix[g] returns the (0,1) incidence matrix of graph g, which has a row for each vertex and column for each edge and (v,e)=1 if and only if vertex v is incident upon edge e."

IndependentSetQ::usage = "IndependentSetQ[g,i] returns True if the vertices in list i define an independent set in graph g."

Index::usage = "Index[p] returns the index of permutation p, the sum of all subscripts j such that p[j] is greater than p[j+1]."

InduceSubgraph::usage = "InduceSubgraph[g,s] constructs the subgraph of graph g induced by the list of vertices s."

InitializeUnionFind::usage = "InitializeUnionFind[n] initializes a union-find data structure for n elements."

InsertIntoTableau::usage = "InsertIntoTableau[e,t] inserts integer e into Young tableau t using the bumping algorithm."

IntervalGraph::usage = "IntervalGraph[l] constructs the interval graph defined by the list of intervals l."

InversePermutation::usage = "InversePermutation[p] yields the multiplicative inverse of permutation p."

Inversions::usage = "Inversions[p] counts the number of inversions in permutation p."

InvolutionQ::usage = "InvolutionQ[p] returns True if permutation p is its own inverse."

IsomorphicQ::usage = "IsomorphicQ[g,h] returns True if graphs g and h are isomorphic."

IsomorphismQ::usage = "IsomorphismQ[g,h,p] tests if permutation p defines an isomorphism between graphs g and h."

Isomorphism::usage = "Isomorphism[g,h] returns an isomorphism between graphs g and h if one exists."

Josephus::usage = "Josephus[n,m] generates the inverse of the permutation defined by executing every mth member in a circle of n men."

KSubsets::usage = "KSubsets[l,k] returns all subsets of set l containing exactly k elements, ordered lexicographically."

K::usage = "K[n] creates a complete graph on n vertices. K[a,b,c,...,k] creates a complete k-partite graph of the prescribed shape."

LabeledTreeToCode::usage = "LabeledTreeToCode[g] reduces the tree g to its Prufer code."

LastLexicographicTableau::usage = "LastLexicographicTableau[p] constructs the last Young tableau with shape described by partition p."

LexicographicPermutations::usage = "LexicographicPermutations[l] constructs all permutations of list l in lexicographic order."

LexicographicSubsets::usage = "LexicographicSubsets[l] returns all subsets of set l in lexicographic order."

LineGraph::usage = "LineGraph[g] constructs the line graph of graph g."

LongestIncreasingSubsequence::usage = "LongestIncreasingSubsequence[p] find the longest increasing scattered subsequence of permutation p."

M::usage = "M[g] gives the number of edges in undirected graph g."

MakeGraph::usage = "MakeGraph[v,f] constructs the binary relation defined by function f on all pairs of elements of list v."

MakeSimple::usage = "MakeSimple[g] returns an undirected, unweighted graph derived from directed graph g."

MakeUndirected::usage = "MakeUndirected[g] returns a graph with an undirected edge for each directed edge of graph g."

MaximalMatching::usage = "MaximalMatching[g] returns the list of edges associated with a maximal matching of graph g."

MaximumAntichain::usage = "MaximumAntichain[g] returns a largest set of unrelated vertices in partial order g."

MaximumClique::usage = "MaximumClique[g] finds the largest clique in graph g."

MaximumIndependentSet::usage = "MaximumIndependentSet[g] finds the largest independent set of graph g."

MaximumSpanningTree::usage = "MaximumSpanningTree[g] uses Kruskal's algorithm to find a maximum spanning tree of graph g."

MinimumChainPartition::usage = "MinimumChainPartition[g] partitions partial order g into a minimum number of chains."

MinimumChangePermutations::usage = "MinimumChangePermutations[l] constructs all permutations of list l such that adjacent permutations differ by only one transposition."

MinimumSpanningTree::usage = "MinimumSpanningTree[g] uses Kruskal's algorithm to find a minimum spanning tree of graph g."

MinimumVertexCover::usage = "MinimumVertexCover[g] finds the minimum vertex cover of graph g."

MultiplicationTable::usage = "MultiplicationTable[l,f] constructs the complete transition table defined by the binary relation function f on the elements of list l."

NetworkFlowEdges::usage = "NetworkFlowEdges[g,source,sink] returns the adjacency matrix showing the distribution of the maximum flow from source to sink in graph g."

NetworkFlow::usage = "NetworkFlow[g,source,sink] finds the maximum flow through directed graph g from source to sink."

NextComposition::usage = "NextComposition[l] constructs the integer composition which follows l in a canonical order."

NextKSubset::usage = "NextKSubset[l,s] computes the k-subset of list l which appears after k-subsets s in lexicographic order."

NextPartition::usage = "NextPartition[p] returns the integer partition following p in reverse lexicographic order."

NextPermutation::usage = "NextPermutation[p] returns the permutation following p in lexicographic order"

NextSubset::usage = "NextSubset[l,s] constructs the subset of l following subset s in canonical order."

NextTableau::usage = "NextTableau[t] returns the tableau of shape t which follows t in lexicographic order."

NormalizeVertices::usage = "NormalizeVertices[v] returns a list of vertices with the same structure as v but with all coordinates of all points between 0 and 1."

NthPair::usage = "NthPair[n] returns the nth unordered pair of positive integers, when sequenced to minimize the size of the larger integer."

NthPermutation::usage = "NthPermutation[n,l] returns the nth lexicographic permutation of list l."

NthSubset::usage = "NthSubset[n,l] returns the nth subset of list l in canonical order."

NumberOfCompositions::usage = "NumberOfCompositions[n,k] counts the number of distinct compositions of  integer n into k parts."

NumberOfDerangements::usage = "NumberOfDerangements[n] counts the derangements on n elements, the permutations without any fixed points."

NumberOfInvolutions::usage = "NumberOfInvolutions[n] counts the number of involutions on n elements."

NumberOfPartitions::usage = "NumberOfPartitions[n] counts the number of distinct integer partitions of n."

NumberOfPermutationsByCycles::usage = "NumberOfPermutationsByCycles[n,m] returns the number of permutations of length n with exactly m cycles."

NumberOfSpanningTrees::usage = "NumberOfSpanningTrees[g] computes the number of distinct labeled spanning trees of graph g."

NumberOfTableaux::usage = "NumberOfTableaux[p] uses the hook length formula to count the number of Young tableaux with shape defined by partition p."

OrientGraph::usage = "OrientGraph[g] assigns a direction to each edge of a bridgeless, undirected graph g, so that the graph is strongly connected."

PartialOrderQ::usage = "PartialOrderQ[g] returns True if the binary relation defined by the adjacency matrix of graph g is a partial order, meaning it is transitive, reflexive, and anti-symmetric."

PartitionQ::usage = "PartitionQ[p] returns True if p is an integer partition."

Partitions::usage = "Partitions[n] constructs all partitions of integer n in reverse lexicographic order."

PathConditionGraph::usage = "PathConditionGraph[g] replaces each non-edge of a graph by an infinite cost, so shortest path algorithms work correctly"

Path::usage = "Path[n] constructs a tree consisting only of a path on n vertices."

PerfectQ::usage = "PerfectQ[g] returns true is g is a perfect graph, meaning that for every induced subgraph of g the size of the largest clique equals the chromatic number."

PermutationGroupQ::usage = "PermutationGroupQ[l] returns True if the list of permutations l forms a permutation group."

PermutationQ::usage = "PermutationQ[p] returns True if p represents a permutation and False otherwise."

Permute::usage = "Permute[l,p] permutes list l according to permutation p."

PlanarQ::usage = "PlanarQ[g] returns True if graph g is planar, meaning it can be drawn in the plane so no two edges cross."

PointsAndLines::usage = "PointsAndLines[g] constructs a partial graphics representation of a graph g."

Polya::usage = "Polya[g,m] returns the polynomial giving the number of colorings, with m colors, of a structure defined by the permutation group g."

PseudographQ::usage = "PseudographQ[g] returns True if graph g is a pseudograph, meaning it contains self-loops."

RadialEmbedding::usage = "RadialEmbedding[g] constructs a radial embedding of graph g, radiating from the center of the graph."

Radius::usage = "Radius[g] computes the radius of graph g, the minimum eccentricity of any vertex of g."

RandomComposition::usage = "RandomComposition[n,k] constructs a random composition of integer n into k parts."

RandomGraph::usage = "RandomGraph[n,p,{l,h}] constructs a random labeled graph on n vertices with an edge probability of p and edge weights of integers drawn uniformly at random from the range (l,h). RandomGraph[n,p,{l,h},Directed] similarly constructs a random directed graph."

RandomHeap::usage = "RandomHeap[n] constructs a random heap on n elements."

RandomKSubset::usage = "RandomKSubset[l,k] returns a random subset of set l with exactly k elements."

RandomPartition::usage = "RandomPartition[n] constructs a random partition of integer n."

RandomPermutation1::usage = "RandomPermutation1[n] sorts random numbers to generate a random permutation."

RandomPermutation2::usage = "RandomPermutation2[n] uses random transpositions to generate random permutations."

RandomPermutation::usage = "RandomPermutation[n] returns a random permutation of length n."

RandomSubset::usage = "RandomSubset[l] creates a random subset of set l."

RandomTableau::usage = "RandomTableau[p] constructs a random Young tableau of shape p."

RandomTree::usage = "RandomTree[n] constructs a random labeled tree on n vertices."

RandomVertices::usage = "RandomVertices[g] assigns a random embedding to graph g."

RankGraph::usage = "RankGraph[g,l] partitions the vertices into classes based on the shortest geodesic distance to a member of list l."

RankPermutation::usage = "RankPermutation[p] computes the rank of permutation p in lexicographic order."

RankSubset::usage = "RankSubset[l,s] computes the rank, in canonical order, of subset s of set l."

RankedEmbedding::usage = "RankedEmbedding[g,l] performs a ranked embedding of graph g, with the vertices ranked in terms of geodesic distance from a member of list l."

ReadGraph::usage = "ReadGraph[f] reads a graph represented as edge lists from file f, and returns the graph as a graph object."

RealizeDegreeSequence::usage = "RealizeDegreeSequence[s] constructs a semirandom graph with degree sequence s."

RegularGraph::usage = "RegularGraph[k,n] constructs a semirandom k-regular graph on n vertices, if such a graph exists."

RegularQ::usage = "RegularQ[g] returns True if g is a regular graph."

RemoveSelfLoops::usage = "RemoveSelfLoops[g] constructs a graph g with the same edges except for any self-loops."

RevealCycles::usage = "RevealCycles[p] unveils the canonical hidden cycle structure of permutation p."

RootedEmbedding::usage = "RootedEmbedding[g,v] constructs a rooted embedding of graph g with vertex v as the root."

RotateVertices::usage = "RotateVertices[v,theta] rotates each vertex position in list v by theta radians around the origin (0,0)."

Runs::usage = "Runs[p] partitions p into contiguous increasing subsequences."

SamenessRelation::usage = "SamenessRelation[l] constructs a binary relation from a list of permutations l which is an equivalence relation if l is a permutation group."

SelectionSort::usage = "SelectionSort[l,f] sorts list l using ordering function f."

SelfComplementaryQ::usage = "SelfComplementaryQ[g] returns True if graph g is self-complementary, meaning it is isomorphic to its complement."

ShakeGraph::usage = "ShakeGraph[g,d] performs a random perturbation of the vertices of graph g, with each vertex moving at most a distance d from its original position."

ShortestPathSpanningTree::usage = "ShortestPathSpanningTree[g,v] constructs the shortest-path spanning tree originating from v, so that the shortest path in graph g from v to any other vertex is the path in the tree."

ShortestPath::usage = "ShortestPath[g,start,end] finds the shortest path between vertices start and end in graph g."

ShowGraph::usage = "ShowGraph[g] displays graph g according to its embedding. ShowGraph[g,Directed] displays directed graph g according to its embedding, with arrows illustrating the orientation of each edge."

ShowLabeledGraph::usage = "ShowLabeledGraph[g] displays graph g according to its embedding, with each vertex labeled with its vertex number. ShowLabeledGraph[g,l] uses the ith element of list l as the label for vertex i."

SignaturePermutation::usage = "SignaturePermutation[p] gives the signature of permutation p."

SimpleQ::usage = "SimpleQ[g] returns True if g is a simple graph, meaning it is unweighted and contains no self-loops."

Spectrum::usage = "Spectrum[g] gives the eigenvalues of graph g."

SpringEmbedding::usage = "SpringEmbedding[g] beautifies the embedding of graph g by modeling the embedding as a system of springs."

StableMarriage::usage = "StableMarriage[mpref,fpref] finds the male optimal stable marriage defined by lists of permutations describing male and female preferences."

Star::usage = "Star[n] constructs a star on n vertices, which is a tree with one vertex of degree n-1."

StirlingFirst::usage = "StirlingFirst[n,k] computes the Stirling numbers of the first kind."

StirlingSecond::usage = "StirlingSecond[n,k] computes the Stirling numbers of the second kind."

Strings::usage = "Strings[l,n] constructs all possible strings of length n from the elements of list l."

StronglyConnectedComponents::usage = "StronglyConnectedComponents[g] returns the strongly connected components of directed graph g."

Subsets::usage = "Subsets[l] returns all subsets of set l."

TableauClasses::usage = "TableauClasses[p] partitions the elements of permutation p into classes according to their initial columns during Young tableaux construction."

TableauQ::usage = "TableauQ[t] returns True if and only if t represents a Young tableau."

TableauxToPermutation::usage = "TableauxToPermutation[t1,t2] constructs the unique permutation associated with Young tableaux t1 and t2, where both tableaux have the same shape. "

Tableaux::usage = "Tableaux[p] constructs all tableaux whose shape is given by integer partition p."

ToAdjacencyLists::usage = "ToAdjacencyLists[g] constructs an adjacency list representation for graph g."

ToCycles::usage = "ToCycles[p] returns the cycle structure of permutation p."

ToInversionVector::usage = "ToInversionVector[p] computes the inversion vector associated with permutation p."

ToOrderedPairs::usage = "ToOrderedPairs[g] constructs a list of ordered pairs representing the edges of undirected graph g."

ToUnorderedPairs::usage = "ToUnorderedPairs[g] constructs a list of vertex pairs representing graph g, with one pair per undirected edge."

TopologicalSort::usage = "TopologicalSort[g] returns a permutation of the vertices of directed acyclic graph g such that an edge (i,j) implies vertex i appears before vertex j."

TransitiveClosure::usage = "TransitiveClosure[g] finds the transitive closure of graph g, the superset of g which contains edge {x,y} iff there is a path from x to y."

TransitiveQ::usage = "TransitiveQ[g] returns True if graph g defines a transitive relation."

TransitiveReduction::usage = "TransitiveReduction[g] finds the smallest graph which has the same transitive closure as g."

TranslateVertices::usage = "TranslateVertices[v,{x,y}] adds the vector {x,y} to each vertex in list v."

TransposePartition::usage = "TransposePartition[p] reflects a partition p of k parts along the main diagonal, creating a partition with maximum part k."

TransposeTableau::usage = "TransposeTableau[t] reflects a Young tableau t along the main diagonal, creating a different tableau."

TravelingSalesmanBounds::usage = "TravelingSalesmanBounds[g] computes upper and lower bounds on the minimum cost traveling salesman tour of graph g."

TravelingSalesman::usage = "TravelingSalesman[g] finds the optimal traveling salesman tour in graph g."

TreeQ::usage = "TreeQ[g] returns True if graph g is a tree."

TriangleInequalityQ::usage = "TriangleInequalityQ[g] returns True if the weight function defined by the adjacency matrix of graph g satisfies the triangle inequality."

Turan::usage = "Turan[n,p] constructs the Turan graph, the extremal graph on n vertices which does not contain K[p]."

TwoColoring::usage = "TwoColoring[g] finds a two-coloring of graph g if g is bipartite."

UndirectedQ::usage = "UndirectedQ[g] returns True if graph g is undirected."

UnionSet::usage = "UnionSet[a,b,s] merges the sets containing a and b in union-find data structure s."

UnweightedQ::usage = "UnweightedQ[g] returns True if all entries in the adjacency matrix of graph g are zero or one."

V::usage = "V[g] gives the order or number of vertices of graph g."

VertexColoring::usage = "VertexColoring[g] uses Brelaz's heuristic to find a good, but not necessarily minimal, vertex coloring of graph g."

VertexConnectivity::usage = "VertexConnectivity[g] computes the minimum number of vertices whose deletion from graph g disconnects it."

VertexCoverQ::usage = "VertexCoverQ[g,c] returns True if the vertices in list c define a vertex cover of graph g."

Vertices::usage = "Vertices[g] returns the embedding of graph g."

WeaklyConnectedComponents::usage = "WeaklyConnectedComponents[g] returns the weakly connected components of directed graph g."

Wheel::usage = "Wheel[n] constructs a wheel on n vertices, which is the join of K[1] and Cycle[n-1]."

WriteGraph::usage = "WriteGraph[g,f] writes graph g to file f using an edge list representation."

Begin["`private`"]
PermutationQ[p_List] := (Sort[p] == Range[Length[p]])

Permute[l_List,p_?PermutationQ] := l [[ p ]]
Permute[l_List,p_List] := Map[ (Permute[l,#])&, p] /; (Apply[And, Map[PermutationQ, p]])

(* Section 1.1.1 Lexicographically Ordered Permutions, Pages 3-4 *)

LexicographicPermutations[{l_}] := {{l}}

LexicographicPermutations[{a_,b_}] := {{a,b},{b,a}}

LexicographicPermutations[l_List] :=
	Module[{i,n=Length[l]},
		Apply[
			Join,
			Table[
				Map[
					(Prepend[#,l[[i]]])&,
					LexicographicPermutations[
						Complement[l,{l[[i]]}]
					]
				],
				{i,n}
			]
		]
	]

(* Section 1.1.2 Ranking and Unranking Permutations, Pages 5-6 *)

RankPermutation[{1}] = 0

RankPermutation[p_?PermutationQ] := (p[[1]]-1) (Length[Rest[p]]!) +
	RankPermutation[ Map[(If[#>p[[1]], #-1, #])&, Rest[p]] ]

(* UP, and UnrankPermutation come from the V2.1 code.
   There is some problem in the v0.9 code and rather than try to fix that
   we use the newer version
 *)
UP[r_Integer, n_Integer] :=
        Module[{r1 = r, q = n!, i},
               Table[r1 = Mod[r1, q];
                     q = q/(n - i + 1);
                     Quotient[r1, q] + 1,
                     {i, n}
               ]
        ]
UnrankPermutation[r_Integer, {}] := {}
UnrankPermutation[r_Integer, l_List] :=
        Module[{s = l, k, t, p = UP[Mod[r, Length[l]!], Length[l]], i},
               Table[k = s[[t = p[[i]] ]];
                     s = Delete[s, t];
                     k,
                     {i, Length[ p ]}
               ]
        ]
UnrankPermutation[r_Integer, n_Integer?Positive] :=
        UnrankPermutation[r, Range[n]]
NthPermutation[r_Integer, l_List] := UnrankPermutation[r, l]

NextPermutation[p_?PermutationQ] :=
	NthPermutation[ RankPermutation[p]+1, Sort[p] ]

(* Section 1.1.3 RandomPermutations, Pages 6-7 *)

(*** FIXME:
  ListPlot[ RandomPermutation1[30]]
shows that RandomPermutaion1 isn't good. Therefore we use RandomPermutation2
for RandomPermutation.
 ****)

RandomPermutation1[n_Integer?Positive] :=
	Map[ Last, Sort[ Map[({RandomInteger[],#})&,Range[n]] ] ]

RandomPermutation2[n_Integer?Positive] :=
	Module[{p = Range[n],i,x},
		Do [
			x = RandomInteger[{1,i}];
			{p[[i]],p[[x]]} = {p[[x]],p[[i]]},
			{i,n,2,-1}
		];
		p
	]

RandomPermutation[n_Integer?Positive] := RandomPermutation2[n]

(* Section 1.1.4 Permutation from Transpostions, Page 11 *)
MinimumChangePermutations[l_List] :=
	Module[{i=1,c,p=l,n=Length[l],k},
		c = Table[1,{n}];
		Join[
			{l},
			Table[
				While [ c[[i]] >= i, c[[i]] = 1; i++];
				If[OddQ[i], k=1, k=c[[i]] ];
				{p[[i]],p[[k]]} = {p[[k]],p[[i]]};
				c[[i]]++;
				i = 2;
				p,
				{n!-1}
			]
		]
	]

(* Section 1.1.5 Backtracking and Distict Permutations, Page 12-13 *)
Backtrack[space_List,partialQ_,solutionQ_,flag_:One] :=
	Module[{n=Length[space],all={},done,index,v=2,solution},
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

Solution[space_List,index_List,count_Integer] :=
	Module[{i}, Table[space[[ i,index[[i]] ]], {i,count}] ]

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

(* Section 1.1.6 Sorting and Searching, Page 14-16 *)

MinOp[l_List,f_] :=
	Module[{min=First[l]},
		Scan[ (If[ Apply[f,{#,min}], min = #])&, l];
		Return[min];
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

BinarySearch[l_List,k_Integer] := BinarySearch[l,k,1,Length[l],Identity]
BinarySearch[l_List,k_Integer,f_] := BinarySearch[l,k,1,Length[l],f]

BinarySearch[l_List,k_Integer,low_Integer,high_Integer,f_] :=
	Module[{mid = Floor[ (low + high)/2 ]},
		If [low > high, Return[low - 1/2]];
		If [f[ l[[mid]] ] == k, Return[mid]];
		If [f[ l[[mid]] ] > k,
			BinarySearch[l,k,1,mid-1,f],
			BinarySearch[l,k,mid+1,high,f]
		]
	]

(* Section 1.2.1 Multiplying Permutations, Page 17 *)
MultiplicationTable[elems_List,op_] :=
	Module[{i,j,n=Length[elems],p},
		Table[
			p = Position[elems, Apply[op,{elems[[i]],elems[[j]]}]];
			If [p === {}, 0, p[[1,1]]],
			{i,n},{j,n}
		]
	]

(* Section 1.2.2 The Inverse of a Permutation, Page 18 *)
InversePermutation[p_?PermutationQ] :=
	Module[{inverse=p, i},
		Do[ inverse[[ p[[i]] ]] = i, {i,Length[p]} ];
		inverse
	]

(* Section 1.2.3 The Equivalence Relation and Classesn, Page 18-19 *)
EquivalenceRelationQ[r_?SquareMatrixQ] :=
	ReflexiveQ[r] && SymmetricQ[r] && TransitiveQ[r]
EquivalenceRelationQ[g_Graph] := EquivalenceRelationQ[ Edges[g] ]

SquareMatrixQ[{}] = True
SquareMatrixQ[r_] := MatrixQ[r] && (Length[r] == Length[r[[1]]])

ReflexiveQ[r_?SquareMatrixQ] :=
	Module[{i}, Apply[And, Table[(r[[i,i]]!=0),{i,Length[r]}] ] ]

TransitiveQ[r_?SquareMatrixQ] := TransitiveQ[ Graph[v,RandomVertices[Length[r]]] ]
TransitiveQ[r_Graph] := IdenticalQ[r,TransitiveClosure[r]]

SymmetricQ[r_?SquareMatrixQ] := (r === Transpose[r])

EquivalenceClasses[r_List?EquivalenceRelationQ] :=
	ConnectedComponents[ Graph[r,RandomVertices[Length[r]]] ]
EquivalenceClasses[g_Graph?EquivalenceRelationQ] := ConnectedComponents[g]

PermutationGroupQ[perms_List] :=
	FreeQ[ MultiplicationTable[perms,Permute], 0] &&
		EquivalenceRelationQ[SamenessRelation[perms]]

SamenessRelation[perms_List] :=
	Module[{positions = Transpose[perms], i, j, n=Length[First[perms]]},
		Table[
			If[ MemberQ[positions[[i]],j], 1, 0],
			{i,n}, {j,n}
		]
	] /; perms != {}

(* 1.2.4 The Cycle Structure of Permutations; Pages 20-21  *)
ToCycles[p1_?PermutationQ] :=
	Module[{p=p1,m,n,cycle,i},
		Select[
			Table[
				m = n = p[[i]];
				cycle = {};
				While[ p[[n]] != 0,
					AppendTo[cycle,m=n];
					n = p[[n]];
					p[[m]] = 0
				];
				cycle,
				{i,Length[p]}
			],
			(# =!= {})&
		]
	]

FromCycles[cyc_List] :=
	Module[{p=Table[0,{Length[Flatten[cyc]]}], pos},
		Scan[
			(pos = Last[#];
			 Scan[ Function[c, pos = p[[pos]] = c], #])&,
			cyc
		];
		p
	]

(* 1.2.4 The Cycle Structure of Permutations, Hiding Cycles; Page 22  *)
HideCycles[c_List] :=
	Flatten[
		Sort[
			Map[(RotateLeft[#,Position[#,Min[#]] [[1,1]] - 1])&, c],
			(#1[[1]] > #2[[1]])&
		]
	]

RevealCycles[p_?PermutationQ] :=
	Module[{start=end=1, cycles={}},
		While [end <= Length[p],
			If [p[[start]] > p[[end]],
				AppendTo[ cycles, Take[p,{start,end-1}] ];
				start = end,
				end++
			]
		];
		Append[cycles,Take[p,{start,end-1}]]
	]

(* 1.2.4 The Cycle Structure of Permutations, Counting Cycles; Page 23  *)
NumberOfPermutationsByCycles[n_Integer,m_Integer] := (-1)^(n-m) StirlingS1[n,m]

StirlingFirst[n_Integer,m_Integer] := StirlingFirst1[n,m]

StirlingFirst1[n_Integer,0] := If [n == 0, 1, 0]
StirlingFirst1[0,m_Integer] := If [m == 0, 1, 0]

StirlingFirst1[n_Integer,m_Integer] := StirlingFirst1[n,m] =
	(n-1) StirlingFirst1[n-1,m] + StirlingFirst1[n-1, m-1]

StirlingSecond[n_Integer,m_Integer] := StirlingSecond1[n,m]

StirlingSecond1[n_Integer,0] := If [n == 0, 1, 0]
StirlingSecond1[0,m_Integer] := If [m == 0, 1, 0]

StirlingSecond1[n_Integer,m_Integer] := StirlingSecond1[n,m] =
	m StirlingSecond1[n-1,m] + StirlingSecond1[n-1,m-1]

(* 1.2.5 Signatures; Page 24 *)

SignaturePermutation[p_?PermutationQ] := (-1) ^ (Length[p]-Length[ToCycles[p]])

(* 1.2.6 Polya's Theory of Counting; Page 25 *)
Polya[g_List,m_] := Apply[ Plus, Map[(m^Length[ToCycles[#]])&,g] ] / Length[g]

ToInversionVector[p_?PermutationQ] :=
	Module[{i,inverse=InversePermutation[p]},
		Table[
			Length[ Select[Take[p,inverse[[i]]], (# > i)&] ],
			{i,Length[p]-1}
		]
	]

  (* 1.3.1 Inversion Vectors, Page 27 *)
FromInversionVector[vec_List] :=
  Block[{n=Length[vec]+1,i,p={n}},
		Do [
			p = Insert[p, i, vec[[i]]+1],
			{i,n-1,1,-1}
		];
		p
	]

Inversions[p_?PermutationQ] := Apply[Plus,ToInversionVector[p]]

Index[p_?PermutationQ]:=
	Module[{i},
		Sum[ If [p[[i]] > p[[i+1]], i, 0], {i,Length[p]-1} ]
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
	]

Eulerian[n_Integer,k_Integer] := Eulerian1[n,k]

Eulerian1[0,k_Integer] := If [k==1, 1, 0]
Eulerian1[n_Integer,k_Integer] := Eulerian1[n,k] =
	k Eulerian1[n-1,k] + (n-k+1) Eulerian1[n-1,k-1]

InvolutionQ[p_?PermutationQ] := p[[p]] == Range[Length[p]]

NumberOfInvolutions[n_Integer] :=
	Module[{k},
		n! Sum[1/((n - 2k)! 2^k k!), {k, 0, Quotient[n, 2]}]
	]

DerangementQ[p_?PermutationQ] :=
	!(Apply[ Or, Map[( # == p[[#]] )&, Range[Length[p]]] ])

NumberOfDerangements[0] = 1;
NumberOfDerangements[n_] := n * NumberOfDerangements[n-1] + (-1)^n

Derangements[n_Integer] := Derangements[Range[n]]
Derangements[p_?PermutationQ] := Select[ Permutations[p], DerangementQ ]

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

RandomHeap[n_Integer] := Heapify[RandomPermutation[n]]

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
	]

Strings[l_List,0] := { {} }

Strings[l_List,k_Integer?Positive] :=
	Module[{oneless = Strings[l,k-1],i,n=Length[l]},
		Apply[Join, Table[ Map[(Prepend[#,l[[i]]])&, oneless], {i,n}] ]
	]

NthSubset[n_Integer,m_Integer] := NthSubset[n,Range[m]]
NthSubset[n_Integer,l_List] :=
	l[[ Flatten[ Position[Reverse[IntegerDigits[ Mod[n,2^Length[l]],2]],1] ] ]]

BinarySubsets[l_List] :=
	Module[{pos=Reverse[Range[Length[l]]], n=Length[l]},
		Map[(l[[ Reverse[Select[pos*#, Positive]] ]])&, Strings[{0,1},n] ]
	]

NextSubset[set_List,subset_List] := NthSubset[ RankSubset[set,subset], set  ]

RankSubset[set_List,subset_List] :=
	Module[{i,n=Length[set]},
		Sum[ 2^(i-1) * If[ MemberQ[subset,set[[i]]], 1, 0], {i,n}]
	]

RandomSubset[set_List] := NthSubset[RandomInteger[2^(Length[set])-1],set]

GrayCode[l_List] := GrayCode[l,{{}}]

GrayCode[{},prev_List] := prev

GrayCode[l_List,prev_List] :=
	GrayCode[
		Rest[l],
		Join[ prev, Map[(Append[#,First[l]])&,Reverse[prev]] ]
	]

(* We have a builtin that does this.
GrayCode doesn't work?
Subsets[l_List] := GrayCode[l]
Subsets[n_Integer] := GrayCode[Range[n]]
*)

LexicographicSubsets[l_List] := LexicographicSubsets[l,{{}}]

LexicographicSubsets[{},s_List] := s

LexicographicSubsets[l_List,subsets_List] :=
	LexicographicSubsets[
		Rest[l],
		Join[
			subsets,
			Map[(Prepend[#,First[l]])&,LexicographicSubsets[Rest[l],{{}}] ]
		]
	]

(* 1.5.5 Generating k-Subsets *)
KSubsets[l_List,0] := { {} }
KSubsets[l_List,1] := Partition[l,1]
KSubsets[l_List,k_Integer?Positive] := {l} /; (k == Length[l])
KSubsets[l_List,k_Integer?Positive] := {}  /; (k > Length[l])

KSubsets[l_List,k_Integer?Positive] :=
	Join[
		Map[(Prepend[#,First[l]])&, KSubsets[Rest[l],k-1]],
		KSubsets[Rest[l],k]
	]

NextKSubset[set_List,subset_List] :=
	Take[set,Length[subset]] /; (Take[set,-Length[subset]] === subset)

NextKSubset[set_List,subset_List] :=
	Module[{h=1, x=1},
		While [set[[-h]] == subset[[-h]], h++];
		While [set[[x]] =!= subset[[-h]], x++];
		Join[ Drop[subset,-h], Take[set, {x+1,x+h}] ]
	]

RandomKSubset[n_Integer,k_Integer] := RandomKSubset[Range[n],k]

RandomKSubset[set_List,k_Integer] :=
	Module[{s=Range[Length[set]],i,n=Length[set],x},
		set [[
			Sort[
				Table[
					x=RandomInteger[{1,i}];
					{s[[i]],s[[x]]} = {s[[x]],s[[i]]};
					s[[i]],
					{i,n,n-k+1,-1}
				]
			]
		]]
	]

PartitionQ[p_List] := (Min[p]>0) && Apply[And, Map[IntegerQ,p]]

Partitions[n_Integer] := Partitions[n,n]

Partitions[n_Integer,_] := {} /; (n<0)
Partitions[0,_] := { {} }
Partitions[n_Integer,1] := { Table[1,{n}] }
Partitions[_,0] := {}

Partitions[n_Integer,maxpart_Integer] :=
	Join[
		Map[(Prepend[#,maxpart])&, Partitions[n-maxpart,maxpart]],
		Partitions[n,maxpart-1]
	]

NextPartition[p_List] := Join[Drop[p,-1],{Last[p]-1,1}]  /; (Last[p] > 1)

NextPartition[p_List] := {Apply[Plus,p]}  /; (Max[p] == 1)

NextPartition[p_List] :=
	Module[{index,k,m},
		{index} = First[ Position[p,1] ];
		k = p[[index-1]] - 1;
		m = Apply[Plus,Drop[p,index-1]] + k + 1;
		Join[
			Take[p,index-2],
			Table[k,{Quotient[m,k]}],
			If [Mod[m,k] == 0, {}, {Mod[m,k]}]
		]
	]

FerrersDiagram[p1_List] :=
	Module[{i,j,n=Length[p1],p=Sort[p1]},
		Show[
			Graphics[
				Join[
					{PointSize[ Min[0.04,1/(2 Max[p])] ]},
					Table[Point[{i,j}], {j,n}, {i,p[[j]]}]
				],
				{AspectRatio -> 1, PlotRange -> All}
			]
		]
	]

TransposePartition[p_List] :=
	Module[{s=Select[p,(#>0)&], i, row, r},
		row = Length[s];
		Table [
			r = row;
			While [s[[row]]<=i, row--];
			r,
			{i,First[s]}
		]
	]

DurfeeSquare[s_List] :=
	Module[{i,max=1},
		Do [
			If [s[[i]] >= i, max=i],
			{i,2,Min[Length[s],First[s]]}
		];
		max
	]

DurfeeSquare[{}] := 0

NumberOfPartitions[n_Integer] := NumberOfPartitions1[n]

NumberOfPartitions1[n_Integer] := 0  /; (n < 0)
NumberOfPartitions1[n_Integer] := 1  /; (n == 0)

NumberOfPartitions1[n_Integer] := NumberOfPartitions1[n] =
	Module[{m},
		Sum[ (-1)^(m+1) NumberOfPartitions1[n - m (3m-1)/2] +
			(-1)^(m+1) NumberOfPartitions1[n - m (3m+1)/2],
			{m, Ceiling[ (1+Sqrt[1.0 + 24n])/6 ], 1, -1}
		]
	]

RandomPartition[n_Integer?Positive] :=
	Module[{mult = Table[0,{n}],j,d,m = n},
		While[ m != 0,
			{j,d} = NextPartitionElement[m];
			m -= j d;
			mult[[d]] += j;
		];
		Flatten[Map[(Table[#,{mult[[#]]}])&,Reverse[Range[n]]]]
	]

NextPartitionElement[n_Integer] :=
	Module[{d=0,j,m,z=RandomInteger[] n PartitionsP[n],done=False,flag},
		While[!done,
			d++; m = n; j = 0; flag = False;
			While[ !flag,
				j++; m -=d;
				If[ m > 0,
					z -= d PartitionsP[m];
					If[ z <= 0, flag=done=True],
					flag = True;
					If[m==0, z -=d; If[z <= 0, done = True]]
				];
			];
		];
		{j,d}
	]

NumberOfCompositions[n_,k_] := Binomial[ n+k-1, n ]

RandomComposition[n_Integer,k_Integer] :=
	Map[
		(#[[2]] - #[[1]] - 1)&,
		Partition[Join[{0},RandomKSubset[Range[n+k-1],k-1],{n+k}], 2, 1]
	]

Compositions[n_Integer,k_Integer] :=
	Map[
		(Map[(#[[2]]-#[[1]]-1)&, Partition[Join[{0},#,{n+k}],2,1] ])&,
		KSubsets[Range[n+k-1],k-1]
	]

NextComposition[l_List] :=
	Module[{c=l, h=1, t},
		While[c[[h]] == 0, h++];
		{t,c[[h]]} = {c[[h]],0};
		c[[1]] = t - 1;
		c[[h+1]]++;
		c
	]

NextComposition[l_List] :=
	Join[{Apply[Plus,l]},Table[0,{Length[l]-1}]] /; Last[l]==Apply[Plus,l]

TableauQ[{}] = True
TableauQ[t_List] :=
	And [
		Apply[ And, Map[(Apply[LessEqual,#])&,t] ],
		Apply[ And, Map[(Apply[LessEqual,#])&,TransposeTableau[t]] ],
		Apply[ GreaterEqual, Map[Length,t] ],
		Apply[ GreaterEqual, Map[Length,TransposeTableau[t]] ]
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

ShapeOfTableau[t_List] := Map[Length,t]

InsertIntoTableau[e_Integer,{}] := { {e} }

InsertIntoTableau[e_Integer, t1_?TableauQ] :=
	Module[{item=e,row=0,col,t=t1},
		While [row < Length[t],
			row++;
			If [Last[t[[row]]] <= item,
				AppendTo[t[[row]],item];
				Return[t]
			];
			col = Ceiling[ BinarySearch[t[[row]],item] ];
			{item, t[[row,col]]} = {t[[row,col]], item};
		];
		Append[t, {item}]
	]

ConstructTableau[p_List] := ConstructTableau[p,{}]

ConstructTableau[{},t_List] := t

ConstructTableau[p_List,t_List] :=
	ConstructTableau[Rest[p], InsertIntoTableau[First[p],t]]

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

LastLexicographicTableau[s_List] :=
	Module[{c=0},
		Map[(c+=#; Range[c-#+1,c])&, s]
	]

FirstLexicographicTableau[s_List] :=
	TransposeTableau[ LastLexicographicTableau[ TransposePartition[s] ] ]

NextTableau[t_?TableauQ] :=
	Module[{s,y,row,j,count=0,tj,i,n=Max[t]},
		y = TableauToYVector[t];
		For [j=2, (j<n)  && (y[[j]]>=y[[j-1]]), j++ ];
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

Tableaux[s_List] :=
	Module[{t = LastLexicographicTableau[s]},
		Table[ t = NextTableau[t], {NumberOfTableaux[s]} ]
	]

Tableaux[n_Integer?Positive] := Apply[ Join, Map[ Tableaux, Partitions[n] ] ]

YVectorToTableau[y_List] :=
	Module[{k},
		Table[ Flatten[Position[y,k]], {k,Length[Union[y]]}]
	]

TableauToYVector[t_?TableauQ] :=
	Module[{i,y=Table[1,{Length[Flatten[t]]}]},
		Do [ Scan[ (y[[#]]=i)&, t[[i]] ], {i,2,Length[t]} ];
		y
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

CatalanNumber[n_] := Binomial[2n,n]/(n+1)	/; (n>=0)

RandomTableau[shape_List] :=
	Module[{i=j=n=Apply[Plus,shape],done,l,m,h=1,k,y,p=shape},
		y= Join[TransposePartition[shape],Table[0,{n - Max[shape]}]];
		Do[
			{i,j} = RandomSquare[y,p]; done = False;
			While [!done,
				h = y[[j]] + p[[i]] - i - j;
				If[ h != 0,
					If[ RandomInteger[] < 0.5,
						j = RandomInteger[{j,p[[i]]}],
						i = RandomInteger[{i,y[[j]]}]
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

RandomSquare[y_List,p_List] :=
	Module[{i=RandomInteger[{1,First[y]}], j=RandomInteger[{1,First[p]}]},
		While[(i > y[[j]]) || (j > p[[i]]),
			i = RandomInteger[{1,First[y]}];
			j = RandomInteger[{1,First[p]}]
		];
		{i,j}
	]

TableauClasses[p_?PermutationQ] :=
	Module[{classes=Table[{},{Length[p]}],t={}},
		Scan [
			(t = InsertIntoTableau[#,t];
			 PrependTo[classes[[Position[First[t],#] [[1,1]] ]], #])&,
			p
		];
		Select[classes, (# != {})&]
	]

LongestIncreasingSubsequence[p_?PermutationQ] :=
	Module[{c,x,xlast},
		c = TableauClasses[p];
		xlast = x = First[ Last[c] ];
		Append[
			Reverse[
				Map[
					(x = First[ Intersection[#,
					       Take[p, Position[p,x][[1,1]] ] ] ])&,
					Reverse[ Drop[c,-1] ]
				]
			],
			xlast
		]
	]

LongestIncreasingSubsequence[{}] := {}

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

EncroachingListSet[l_List] := EncroachingListSet[l,{}]
EncroachingListSet[{},e_List] := e

EncroachingListSet[l_List,e_List] :=
	EncroachingListSet[Rest[l], AddToEncroachingLists[First[l],e] ]

Edges[Graph[e_,_]] := e

Vertices[Graph[_,v_]] := v

V[Graph[e_,_]] := Length[e]

M[Graph[g_,_],___] := Apply[Plus, Map[(Apply[Plus,#])&,g] ] / 2
M[Graph[g_,_],Directed] := Apply[Plus, Map[(Apply[Plus,#])&,g] ]

ChangeVertices[g_Graph,v_List] := Graph[ Edges[g], v ]

ChangeEdges[g_Graph,e_List] := Graph[ e, Vertices[g] ]

AddEdge[Graph[g_,v_],{x_,y_},Directed] :=
	Module[ {gnew=g},
		gnew[[x,y]] ++;
		Graph[gnew,v]
	]

AddEdge[g_Graph,{x_,y_},flag_:Undirected] :=
	AddEdge[ AddEdge[g, {x,y}, Directed], {y,x}, Directed]

DeleteEdge[Graph[g_,v_],{x_,y_},Directed] :=
	Module[ {gnew=g},
		If [ g[[x,y]] > 1, gnew[[x,y]]--, gnew[[x,y]] = 0];
		Graph[gnew,v]
	]

DeleteEdge[g_Graph,{x_,y_},flag_:Undirected] :=
	DeleteEdge[ DeleteEdge[g, {x,y}, Directed], {y,x}, Directed]

AddVertex[g_Graph] := GraphUnion[g, K[1]]

DeleteVertex[g_Graph,v_Integer] := InduceSubgraph[g,Complement[Range[V[g]],{v}]]

Spectrum[Graph[g_,_]] := Eigenvalues[g]

ToAdjacencyLists[Graph[g_,_]] :=
	Map[ (Flatten[ Position[ #, _?(Function[n, n!=0])] ])&, g ]

FromAdjacencyLists[e_List] :=
	Module[{blanks = Table[0,{Length[e]}] },
		Graph[
			Map [ (MapAt[ 1&,blanks,Partition[#,1]])&, e ],
			CircularVertices[Length[e]]
		]
	]

FromAdjacencyLists[e_List,v_List] := ChangeVertices[FromAdjacencyLists[e], v]

ToOrderedPairs[g_Graph] := Position[ Edges[g], _?(Function[n,n != 0]) ]

ToUnorderedPairs[g_Graph] := Select[ ToOrderedPairs[g], (#[[1]] < #[[2]])& ]

FromOrderedPairs[l_List] :=
	Module[{n=Max[l]},
		Graph[
			MapAt[1&, Table[0,{n},{n}],l],
			CircularVertices[n]
		]
	]
FromOrderedPairs[{}] := Graph[{},{}]
FromOrderedPairs[l_List,v_List] :=
	Graph[ MapAt[1&, Table[0,{Length[v]},{Length[v]}], l], v]

FromUnorderedPairs[l_List] := MakeUndirected[ FromOrderedPairs[l] ]
FromUnorderedPairs[l_List,v_List] := MakeUndirected[ FromOrderedPairs[l,v] ]

PseudographQ[Graph[g_,_]] :=
	Module[{i},
		Apply[Or, Table[ g[[i,i]]!=0, {i,Length[g]} ]]
	]

UnweightedQ[Graph[g_,_]] := Apply[ And, Map[(#==0 || #==1)&, Flatten[g] ] ]

SimpleQ[g_Graph] := (!PseudographQ[g]) && (UnweightedQ[g])

RemoveSelfLoops[g_Graph] :=
	Module[{i,e=Edges[g]},
		Do [ e[[i,i]]=0, {i,V[g]} ];
		Graph[e, Vertices[g]]
	]

EmptyQ[g_Graph] := Edges[g] == Table[0, {V[g]}, {V[g]}]

CompleteQ[g_Graph] := Edges[RemoveSelfLoops[g]] == Edges[ K[V[g]] ]

InduceSubgraph[g_Graph,{}] := Graph[{},{}]

InduceSubgraph[Graph[g_,v_],s_List] :=
	Graph[Transpose[Transpose[g[[s]]] [[s]] ],v[[s]]] /; (Length[s]<=Length[g])

Contract[g_Graph,{u_Integer,v_Integer}] :=
	Module[{o,e,i,n=V[g],newg,range=Complement[Range[V[g]],{u,v}]},
		newg = InduceSubgraph[g,range];
		e = Edges[newg]; o = Edges[g];
		Graph[
			Append[
				Table[
					Append[e[[i]],
						If[o[[range[[i]],u]]>0 ||
							o[[range[[i]],v]]>0,1,0] ],
					{i,n-2}
				],
				Append[
					Map[(If[o[[u,#]]>0||o[[v,#]]>0,1,0])&,range],
					0
				]
			],
			Join[Vertices[newg], {(Vertices[g][[u]]+Vertices[g][[v]])/2}]
		]
	] /; V[g] > 2

Contract[g_Graph,_] := K[1]	/; V[g] == 2

GraphComplement[Graph[g_,v_]] :=
	RemoveSelfLoops[ Graph[ Map[ (Map[ (If [#==0,1,0])&, #])&, g], v ] ]

MakeUndirected[Graph[g_,v_]] :=
	Module[{i,j,n=Length[g]},
		Graph[ Table[If [g[[i,j]]!=0 || g[[j,i]]!=0,1,0],{i,n},{j,n}], v ]
	]

UndirectedQ[Graph[g_,_]] := (Apply[Plus,Apply[Plus,Abs[g-Transpose[g]]]] == 0)

MakeSimple[g_Graph] := MakeUndirected[RemoveSelfLoops[g]]

BFS[g_Graph,start_Integer] :=
	Module[{e,bfi=Table[0,{V[g]}],cnt=1,edges={},queue={start}},
		e = ToAdjacencyLists[g];
		bfi[[start]] = cnt++;
		While[ queue != {},
			{v,queue} = {First[queue],Rest[queue]};
			Scan[
				(If[ bfi[[#]] == 0,
					bfi[[#]] = cnt++;
					AppendTo[edges,{v,#}];
					AppendTo[queue,#]
				])&,
				e[[v]]
			];
		];
		{edges,bfi}
	]

BreadthFirstTraversal[g_Graph,s_Integer,Edge] := First[BFS[g,s]]

BreadthFirstTraversal[g_Graph,s_Integer,___] := InversePermutation[Last[BFS[g,s]]]

DFS[v_Integer] :=
	( dfi[[v]] = cnt++;
	  AppendTo[visit,v];
	  Scan[ (If[dfi[[#]]==0,AppendTo[edges,{v,#}];DFS[#] ])&, e[[v]] ] )

DepthFirstTraversal[g_Graph,start_Integer,flag_:Vertex] :=
	Block[{visit={},e=ToAdjacencyLists[g],edges={},dfi=Table[0,{V[g]}],cnt=1},
		DFS[start];
		If[ flag===Edge, edges, visit]
	]

ShowGraph[g1_Graph,type_:Undirected] :=
	Module[{g=NormalizeVertices[g1]},
		Show[
			Graphics[
				Join[
					PointsAndLines[g],
					If[SameQ[type,Directed],Arrows[g],{}]
				]
			],
			{AspectRatio->1, PlotRange->FindPlotRange[Vertices[g]]}
		]
	]

MinimumEdgeLength[v_List,pairs_List] :=
	Max[ Select[
		Chop[ Map[(Sqrt[ N[(v[[#[[1]]]]-v[[#[[2]]]]) .
			(v[[#[[1]]]]-v[[#[[2]]]])] ])&,pairs] ],
		(# > 0)&
	], 0.001 ]

FindPlotRange[v_List] :=
	Module[{xmin=Min[Map[First,v]], xmax=Max[Map[First,v]],
			ymin=Min[Map[Last,v]], ymax=Max[Map[Last,v]]},
		{ {xmin - 0.05 Max[1,xmax-xmin], xmax + 0.05 Max[1,xmax-xmin]},
		  {ymin - 0.05 Max[1,ymax-ymin], ymax + 0.05 Max[1,ymax-ymin]} }
	]

PointsAndLines[Graph[e_List,v_List]] :=
	Module[{pairs=ToOrderedPairs[Graph[e,v]]},
		Join[
			{PointSize[ 0.025 ]},
			Map[Point,Chop[v]],
			Map[(Line[Chop[ v[[#]] ]])&,pairs]
		]
	]

Arrows[Graph[e_,v_]] :=
	Module[{pairs=ToOrderedPairs[Graph[e,v]], size, triangle},
		size = Min[0.05, MinimumEdgeLength[v,pairs]/3];
		triangle={ {0,0}, {-size,size/2}, {-size,-size/2} };
		Map[
			(Polygon[
				TranslateVertices[
					RotateVertices[
						triangle,
						Arctan[Apply[Subtract,v[[#]]]]+Pi
					],
					v[[ #[[2]] ]]
				]
			])&,
			pairs
		]
	]

ShowLabeledGraph[g_Graph] := ShowLabeledGraph[g,Range[V[g]]]
ShowLabeledGraph[g1_Graph,labels_List] :=
	Module[{pairs=ToOrderedPairs[g1], g=NormalizeVertices[g1], v},
		v = Vertices[g];
		Show[
			Graphics[
				Join[
					PointsAndLines[g],
					Map[(Line[Chop[ v[[#]] ]])&, pairs],
					GraphLabels[v,labels]
				]
			],
			{AspectRatio->1, PlotRange->FindPlotRange[v]}
		]
	]

GraphLabels[v_List,l_List] :=
	Module[{i},
		Table[ Text[ l[[i]],v[[i]]-{0.03,0.03},{0,1} ],{i,Length[v]}]
	]

CircularVertices[0] := {}

CircularVertices[n_Integer] :=
	Module[{i,x = N[2 Pi / n]},
		Chop[ Table[ N[{ (Cos[x i]), (Sin[x i]) }], {i,n} ] ]
	]

CircularVertices[Graph[g_,_]] := Graph[ g, CircularVertices[ Length[g] ] ]

RankGraph[g_Graph, start_List] :=
	Module[ {rank = Table[0,{V[g]}],edges = ToAdjacencyLists[g],v,queue,new},
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

RankedEmbedding[g_Graph,start_List] := Graph[ Edges[g],RankedVertices[g,start] ]

RankedVertices[g_Graph,start_List] :=
	Module[{i,m,stages,rank,freq = Table[0,{V[g]}]},
		rank = RankGraph[g,start];
		stages = Distribution[ rank ];
		Table[
			m = ++ freq[[ rank[[i]] ]];
			{rank[[i]], (m-1) + (1 - stages[[ rank[[i]] ]])/2 },
			{i,V[g]}
		]
	]

Distribution[l_List] := Distribution[l, Union[l]]
Distribution[l_List, set_List] := Map[(Count[l,#])&, set]

Eccentricity[g_Graph] := Map[ Max, AllPairsShortestPath[g] ]
Eccentricity[g_Graph,start_Integer] := Map[ Max, Last[Dijkstra[g,start]] ]

Diameter[g_Graph] := Max[ Eccentricity[g] ]

Radius[g_Graph] := Min[ Eccentricity[g] ]

GraphCenter[g_Graph] :=
	Module[{eccentricity = Eccentricity[g]},
		Flatten[ Position[eccentricity, Min[eccentricity]] ]
	]

RadialEmbedding[g_Graph,ct_Integer] :=
	Module[{center=ct,ang,i,da,theta,n,v,positioned,done,next,e=ToAdjacencyLists[g]},
		ang = Table[{0,2 Pi},{n=V[g]}];
		v = Table[{0,0},{n}];
		positioned = next = done = {center};
		While [next != {},
			center = First[next];
			new = Complement[e[[center]], positioned];
			Do [
				da = (ang[[center,2]]-ang[[center,1]])/Length[new];
				ang[[ new[[i]] ]] = {ang[[center,1]] + (i-1)*da,
					ang[[center,1]] + i*da};
				theta = Apply[Plus,ang[[ new[[i]] ]] ]/2;
				v[[ new[[i]] ]] = v[[center]] +
					N[{Cos[theta],Sin[theta]}],
				{i,Length[new]}
			];
			next = Join[Rest[next],new];
			positioned = Union[positioned,new];
			AppendTo[done,center]
		];
		Graph[Edges[g],v]
	]

RadialEmbedding[g_Graph] := RadialEmbedding[g,First[GraphCenter[g]]];

RootedEmbedding[g_Graph,rt_Integer] :=
	Module[{root=rt,pos,i,x,dx,new,n=V[g],v,done,next,e=ToAdjacencyLists[g]},
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
		Graph[Edges[g],N[v]]
	]

TranslateVertices[v_List,{x_,y_}] := Map[ (# + {x,y})&, v ]
TranslateVertices[Graph[g_,v_],{x_,y_}] := Graph[g, TranslateVertices[v,{x,y}] ]

DilateVertices[v_List,d_] := (d * v)
DilateVertices[Graph[e_,v_],d_] := Graph[e, DilateVertices[v,d]]

RotateVertices[v_List,t_] :=
	Module[{d,theta},
		Map[
			(If[# == {0,0}, {0,0},
				d=Sqrt[#[[1]]^2 + #[[2]]^2];
			 	theta = t + Arctan[#];
			 	N[{d Cos[theta], d Sin[theta]}]
			])&,
			v
		]
	]
RotateVertices[Graph[g_,v_],t_] := Graph[g, RotateVertices[v,t]]

Arctan[{x_,y_}] := Arctan1[Chop[{x,y}]]
Arctan1[{0,0}] := 0
Arctan1[{x_,y_}] := ArcTan[x,y]

NormalizeVertices[v_List] :=
	Module[{v1},
		v1 = TranslateVertices[v,{-Min[v],-Min[v]}];
		DilateVertices[v1, 1/Max[v1,0.01]]
	]

NormalizeVertices[Graph[g_,v_]] := Graph[g, NormalizeVertices[v]]

ShakeGraph[Graph[e_List,v_List], fract_:0.1] :=
	Module[{i,d,a},
		Graph[
			e,
			Table[
				d = RandomReal[Real,{0,fract}];
				a = RandomReal[Real,{0, 2 N[Pi]}];
				{N[v[[i,1]] + d Cos[a]], N[v[[i,2]] + d Sin[a]]},
				{i,Length[e]}
			]
		]
	]

CalculateForce[u_Integer,g_Graph,em_List] :=
	Module[{n=V[g],stc=0.25,gr=10.0,e=Edges[g],f={0.0,0.0},spl=1.0,v,dsquared},
		Do [
			dsquared = Max[0.001, Apply[Plus,(em[[u]]-em[[v]])^2] ];
			f += (1-e[[u,v]]) (gr/dsquared) (em[[u]]-em[[v]])
				- e[[u,v]] stc Log[dsquared/spl] (em[[u]]-em[[v]]),
			{v,n}
		];
		f
	]

SpringEmbedding[g_Graph,step_:10,inc_:0.15] :=
	Module[{new=old=Vertices[g],n=V[g],i,u,g1=MakeUndirected[g]},
		Do [
			Do [
				new[[u]] = old[[u]]+inc*CalculateForce[u,g1,old],
				{u,n}
			];
			old = new,
			{i,step}
		];
		Graph[Edges[g],new]
	]

(*	Rewritten for Version 2.0	*)

ReadGraph[file_] :=
	Module[{edgelist={}, v={},x},
		OpenRead[file];
		While[!SameQ[(x = Read[file,Number]), EndOfFile],
			AppendTo[v,Read[file,{Number,Number}]];
			AppendTo[edgelist,
				Convert[Characters[Read[file,String]]]
			];
		];
		Close[file];
		FromAdjacencyLists[edgelist,v]
	]

Toascii[s_String] := First[ ToCharacterCode[s] ]

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

WriteGraph[g_Graph,file_] :=
	Module[{edges=ToAdjacencyLists[g],v=N[NormalizeVertices[Vertices[g]]],i,x,y},
		OpenWrite[file];
		Do[
			WriteString[file,"	",ToString[i]];
			{x,y} = Chop[ v [[i]] ];
			WriteString[file,"	",ToString[x],"	",ToString[y]];
			Scan[
				(WriteString[file,"	",ToString[ # ]])&,
				edges[[i]]
			];
			Write[file],
			{i,V[g]}
		];
		Close[file];
	]

GraphUnion[g_Graph,h_Graph] :=
	Module[{maxg=Max[ Map[First,Vertices[g]] ], minh=Min[ Map[First,Vertices[h]] ]},
		FromOrderedPairs[
			Join[ ToOrderedPairs[g], (ToOrderedPairs[h] + V[g])],
			Join[ Vertices[g], Map[({maxg-minh+1,0}+#)&, Vertices[h] ] ]
		]
	]

GraphUnion[1,g_Graph] := g
GraphUnion[0,g_Graph] := EmptyGraph[0];
GraphUnion[k_Integer,g_Graph] := GraphUnion[ GraphUnion[k-1,g], g]

ExpandGraph[g_Graph,n_] := GraphUnion[ g, EmptyGraph[n - V[g]] ] /; V[g] <= n

GraphIntersection[g_Graph,h_Graph] :=
	FromOrderedPairs[
		Intersection[ToOrderedPairs[g],ToOrderedPairs[h]],
		Vertices[g]
	] /; (V[g] == V[h])

GraphDifference[g1_Graph,g2_Graph] :=
	Graph[Edges[g1] - Edges[g2], Vertices[g1]] /; V[g1]==V[g2]

GraphSum[g1_Graph,g2_Graph] :=
	Graph[Edges[g1] + Edges[g2], Vertices[g1]] /; V[g1]==V[g2]

GraphJoin[g_Graph,h_Graph] :=
	Module[{maxg=Max[ Abs[ Map[First,Vertices[g]] ] ]},
		FromUnorderedPairs[
			Join[
				ToUnorderedPairs[g],
				ToUnorderedPairs[h] + V[g],
				CartesianProduct[Range[V[g]],Range[V[h]]+V[g]]
			],
			Join[ Vertices[g], Map[({maxg+1,0}+#)&, Vertices[h]]]
		]
	]

CartesianProduct[a_List,b_List] :=
	Module[{i,j},
		Flatten[ Table[{a[[i]],b[[j]]},{i,Length[a]},{j,Length[b]}], 1]
	]

GraphProduct[g_Graph,h_Graph] :=
	Module[{k,eg=ToOrderedPairs[g],eh=ToOrderedPairs[h],leng=V[g],lenh=V[h]},
		FromOrderedPairs[
			Flatten[
				Join[
					Table[eg+(i-1)*leng, {i,lenh}],
					Map[ (Table[
						{leng*(#[[1]]-1)+k, leng*(#[[2]]-1)+k},
						{k,1,leng}
					      ])&,
					      eh
					]
				],
				1
			],
			ProductVertices[Vertices[g],Vertices[h]]
		]
	]

ProductVertices[vg_,vh_] :=
	Flatten[
		Map[
			(TranslateVertices[
				DilateVertices[vg, 1/(Max[Length[vg],Length[vh]])],
			#])&,
			 RotateVertices[vh,Pi/2]
		],
		1
	]

IncidenceMatrix[g_Graph] :=
	Map[
		( Join[
			Table[0,{First[#]-1}], {1},
			Table[0,{Last[#]-First[#]-1}], {1},
			Table[0,{V[g]-Last[#]}]
		] )&,
		ToUnorderedPairs[g]
	]

LineGraph[g_Graph] :=
	Module[{b=IncidenceMatrix[g], edges=ToUnorderedPairs[g], v=Vertices[g]},
		Graph[
			b . Transpose[b] - 2 IdentityMatrix[Length[edges]],
			Map[ ( (v[[ #[[1]] ]] + v[[ #[[2]] ]]) / 2 )&, edges]
		]
	]

K[0] := Graph[{},{}]
K[1] := Graph[{{0}},{{0,0}}]

K[n_Integer?Positive] := CirculantGraph[n,Range[1,Floor[(n+1)/2]]]

CirculantGraph[n_Integer?Positive,l_List] :=
	Module[{i,r},
		r = Prepend[MapAt[1&,Table[0,{n-1}], Map[List,Join[l,n-l]]], 0];
		Graph[ Table[RotateRight[r,i], {i,0,n-1}], CircularVertices[n] ]
	]

EmptyGraph[n_Integer?Positive] :=
	Module[{i},
		Graph[ Table[0,{n},{n}], Table[{0,i},{i,(1-n)/2,(n-1)/2}] ]
	]

K[l__] :=
	Module[{ll=List[l],t,i,x,row,stages=Length[List[l]]},
		t = FoldList[Plus,0,ll];
		Graph[
			Apply[
				Join,
				Table [
					row = Join[
						Table[1, {t[[i-1]]}],
						Table[0, {t[[i]]-t[[i-1]]}],
						Table[1, {t[[stages+1]]-t[[i]]}]
					];
					Table[row, {ll[[i-1]]}],
					{i,2,stages+1}
				]

			],
			Apply [
				Join,
				Table[
					Table[{x,i-1+(1-ll[[x]])/2},{i,ll[[x]]}],
					{x,stages}
				]
			]
		]
	] /; TrueQ[Apply[And, Map[Positive,List[l]]]] && (Length[List[l]]>1)

Turan[n_Integer,p_Integer] :=
	Module[{k = Floor[ n / (p-1) ], r},
		r = n - k (p-1);
		Apply[K, Join[ Table[k,{p-1-r}], Table[k+1,{r}] ] ]
	] /; (n > 0 && p > 1)

Cycle[n_Integer] := CirculantGraph[n,{1}]  /; n>=3

Star[n_Integer?Positive] :=
	Module[{g},
		g = Append [ Table[0,{n-1},{n}], Append[ Table[1,{n-1}], 0] ];
		Graph[
			g + Transpose[g],
			Append[ CircularVertices[n-1], {0,0}]
		]
	]

Wheel[n_Integer] :=
	Module[{i,row = Join[{0,1}, Table[0,{n-4}], {1}]},
		Graph[
			Append[
				Table[ Append[RotateRight[row,i-1],1], {i,n-1}],
				Append[ Table[1,{n-1}], 0]
			],
			Append[ CircularVertices[n-1], {0,0} ]
		]
	] /; n >= 3

Path[1] := K[1]
Path[n_Integer?Positive] :=
	FromUnorderedPairs[ Partition[Range[n],2,1], Map[({#,0})&,Range[n]] ]

GridGraph[n_Integer?Positive,m_Integer?Positive] :=
	GraphProduct[
		ChangeVertices[Path[n], Map[({Max[n,m]*#,0})&,Range[n]]],
		Path[m]
	]

Hypercube[n_Integer] := Hypercube1[n]

Hypercube1[0] := K[1]
Hypercube1[1] := Path[2]
Hypercube1[2] := Cycle[4]

Hypercube1[n_Integer] := Hypercube1[n] =
	GraphProduct[
		RotateVertices[ Hypercube1[Floor[n/2]], 2Pi/5],
		Hypercube1[Ceiling[n/2]]
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

CodeToLabeledTree[l_List] :=
	Module[{m=Range[Length[l]+2],x,i},
		FromUnorderedPairs[
			Append[
				Table[
					x = Min[Complement[m,Drop[l,i-1]]];
					m = Complement[m,{x}];
					{x,l[[i]]},
					{i,Length[l]}
				],
				m
			]
		]
	]

RandomTree[n_Integer?Positive] :=
	RadialEmbedding[CodeToLabeledTree[ Table[Random[Integer,{1,n}],{n-2}] ], 1]

RandomGraph[n_Integer,p_] := RandomGraph[n,p,{1,1}]

RandomGraph[n_Integer,p_,range_List] :=
	Module[{i,g},
		g = Table[
			Join[
				Table[0,{i}],
				Table[
					If[Random[Real]<p, Random[Integer,range], 0],
					{n-i}
				]
			],
			{i,n}
		];
		Graph[ g + Transpose[g], CircularVertices[n] ]
	]

ExactRandomGraph[n_Integer,e_Integer] :=
	FromUnorderedPairs[
		Map[ NthPair, Take[ RandomPermutation[n(n-1)/2], e] ],
		CircularVertices[n]
	]

NthPair[0] := {}
NthPair[n_Integer] :=
	Module[{i=2},
		While[ Binomial[i,2] < n, i++];
		{n - Binomial[i-1,2], i}
	]

RandomVertices[n_Integer] := Table[{Random[], Random[]}, {n}]
RandomVertices[g_Graph] := Graph[ Edges[g], RandomVertices[V[g]] ]

RandomGraph[n_Integer,p_,range_List,Directed] :=
	RemoveSelfLoops[
		Graph[
			Table[If[Random[Real]<p,Random[Integer,range],0],{n},{n}],
			CircularVertices[n]
		]
	]

RandomGraph[n_Integer,p_,Directed] := RandomGraph[n,p,{1,1},Directed]

DegreeSequence[g_Graph] := Reverse[ Sort[ Degrees[g] ] ]

Degrees[Graph[g_,_]] := Map[(Apply[Plus,#])&, g]

GraphicQ[s_List] := False /; (Min[s] < 0) || (Max[s] >= Length[s])
GraphicQ[s_List] := (First[s] == 0) /; (Length[s] == 1)
GraphicQ[s_List] :=
	Module[{m,sorted = Reverse[Sort[s]]},
		m = First[sorted];
		GraphicQ[ Join[ Take[sorted,{2,m+1}]-1, Drop[sorted,m+1] ] ]
	]

RealizeDegreeSequence[d_List] :=
	Module[{i,j,v,set,seq,n=Length[d],e},
		seq = Reverse[ Sort[ Table[{d[[i]],i},{i,n}]] ];
		FromUnorderedPairs[
			Flatten[ Table[
				{{k,v},seq} = {First[seq],Rest[seq]};
				While[ !GraphicQ[
					MapAt[
						(# - 1)&,
						Map[First,seq],
						set = RandomKSubset[Table[{i},{i,n-j}],k]
					] ]
				];
				e = Map[(Prepend[seq[[#,2]],v])&,set];
				seq = Reverse[ Sort[
					MapAt[({#[[1]]-1,#[[2]]})&,seq,set]
				] ];
				e,
				{j,Length[d]-1}
			], 1],
			CircularVertices[n]
		]
	] /; GraphicQ[d]

RealizeDegreeSequence[d_List,seed_Integer] :=
	(SeedRandom[seed]; RealizeDegreeSequence[d])

RegularQ[Graph[g_,_]] := Apply[ Equal, Map[(Apply[Plus,#])& , g] ]

RegularGraph[k_Integer,n_Integer] := RealizeDegreeSequence[Table[k,{n}]]

MakeGraph[v_List,f_] :=
	Module[{n=Length[v],i,j},
		Graph [
			Table[If [Apply[f,{v[[i]],v[[j]]}], 1, 0],{i,n},{j,n}],
			CircularVertices[n]
		]
	]

IntervalGraph[l_List] :=
	MakeGraph[
		l,
		( ((First[#1] <= First[#2]) && (Last[#1] >= First[#2])) ||
		  ((First[#2] <= First[#1]) && (Last[#2] >= First[#1])) )&
	]

FunctionalGraph[f_,n_] :=
	Module[{i,x},
		FromOrderedPairs[
			Table[{i, x=Mod[Apply[f,{i}],n]; If[x!=0,x,n]}, {i,n} ],
			CircularVertices[n]
		]
	]

ConnectedComponents[g_Graph] :=
	Module[{untraversed=Range[V[g]],traversed,comps={}},
		While[untraversed != {},
			traversed = DepthFirstTraversal[g,First[untraversed]];
			AppendTo[comps,traversed];
			untraversed = Complement[untraversed,traversed]
		];
		comps
	]

ConnectedQ[g_Graph] := Length[ DepthFirstTraversal[g,1] ] == V[g]

WeaklyConnectedComponents[g_Graph] := ConnectedComponents[ MakeUndirected[g] ]

ConnectedQ[g_Graph,Undirected] := Length[ WeaklyConnectedComponents[g] ] == 1

StronglyConnectedComponents[g_Graph] :=
	Block[{e=ToAdjacencyLists[g],s,c=1,i,cur={},low=dfs=Table[0,{V[g]}],scc={}},
		While[(s=Select[Range[V[g]],(dfs[[#]]==0)&]) != {},
			SearchStrongComp[First[s]];
		];
		scc
	]

SearchStrongComp[v_Integer] :=
	Block[{r},
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

ConnectedQ[g_Graph,Directed] := Length[ StronglyConnectedComponents[g] ] == 1

OrientGraph[g_Graph] :=
	Module[{pairs,newg,rest,cc,c,i,e},
		pairs = Flatten[Map[(Partition[#,2,1])&,ExtractCycles[g]],1];
		newg = FromUnorderedPairs[pairs,Vertices[g]];
		rest = ToOrderedPairs[ GraphDifference[ g, newg ] ];
		cc = Sort[ConnectedComponents[newg], (Length[#1]>=Length[#2])&];
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
			Vertices[g]
		]
	] /; SameQ[Bridges[g],{}]

FindBiconnectedComponents[g_Graph] :=
	Block[{e=ToAdjacencyLists[g],n=V[g],par,c=0,act={},back,dfs,ap=bcc={}},
		back=dfs=Table[0,{n}];
		par = Table[n+1,{n}];
		Map[(SearchBiConComp[First[#]])&, ConnectedComponents[g]];
		{bcc,Drop[ap, -1]}
	]

SearchBiConComp[v_Integer] :=
	Block[{r},
		back[[v]]=dfs[[v]]=++c;
		Scan[
			(If[ dfs[[#]] == 0,
				If[!MemberQ[act,{v,#}], PrependTo[act,{v,#}]];
				par[[#]] = v;
				SearchBiConComp[#];
				If[ back[[#]] >= dfs[[v]],
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

ArticulationVertices[g_Graph]  := Union[Last[FindBiconnectedComponents[g]]];

Bridges[g_Graph] := Select[BiconnectedComponents[g],(Length[#] == 2)&]

BiconnectedComponents[g_Graph] := First[FindBiconnectedComponents[g]];

BiconnectedQ[g_Graph] := Length[ BiconnectedComponents[g] ] == 1

EdgeConnectivity[g_Graph] :=
	Module[{i},
		Apply[Min, Table[NetworkFlow[g,1,i], {i,2,V[g]}]]
	]

VertexConnectivityGraph[g_Graph] :=
	Module[{n=V[g],e},
		e=Table[0,{2 n},{2 n}];
		Scan[ (e[[#-1,#]] = 1)&, 2 Range[n] ];
		Scan[
			(e[[#[[1]], #[[2]]-1]] = e[[#[[2]],#[[1]]-1]] = Infinity)&,
			2 ToUnorderedPairs[g]
		];
		Graph[e,Apply[Join,Map[({#,#})&,Vertices[g]]]]
	]

VertexConnectivity[g_Graph] :=
	Module[{p=VertexConnectivityGraph[g],k=V[g],i=0,notedges},
		notedges = ToUnorderedPairs[ GraphComplement[g] ];
		While[ i++ <= k,
			k = Min[
				Map[
					(NetworkFlow[p,2 #[[1]],2 #[[2]]-1])&,
					Select[notedges,(First[#]==i)&]
				],
				k
			]
		];
		k
	]

Harary[k_?EvenQ, n_Integer] := CirculantGraph[n,Range[k/2]]

Harary[k_?OddQ, n_?EvenQ] := CirculantGraph[n,Append[Range[k/2],n/2]]

Harary[k_?OddQ, n_?OddQ] :=
	Module[{g=Harary[k-1,n],i},
		FromUnorderedPairs[
			Join[
				ToUnorderedPairs[g],
				{ {1,(n+1)/2}, {1,(n+3)/2} },
				Table [ {i,i+(n+1)/2}, {i,2,(n-1)/2} ]
			],
			Vertices[g]
		]
	]

IdenticalQ[g_Graph,h_Graph] := Edges[g] === Edges[h]

IsomorphismQ[g_Graph,h_Graph,p_List] := False	/;
		(V[g]!=V[h]) || !PermutationQ[p] || (Length[p] != V[g])

IsomorphismQ[g_Graph,h_Graph,p_List] := IdenticalQ[g, InduceSubgraph[h,p] ]

Isomorphism[g_Graph,h_Graph,flag_:One] := {}	/; (V[g] != V[h])

Isomorphism[g_Graph,h_Graph,flag_:One] :=
	Module[{eg=Edges[g],eh=Edges[h],equiv=Equivalences[g,h]},
		If [!MemberQ[equiv,{}],
			Backtrack[
				equiv,
				(IdenticalQ[InduceSubgraph[g,Range[Length[#]]],
				    	InduceSubgraph[h,#] ] &&
			 	!MemberQ[Drop[#,-1],Last[#]])&,
				(IsomorphismQ[g,h,#])&,
				flag
			],
			{}
		]
	]

IsomorphicQ[g_Graph,h_Graph] := True /; IdenticalQ[g,h]
IsomorphicQ[g_Graph,h_Graph] := ! SameQ[ Isomorphism[g,h], {}]

Equivalences[g_Graph,h_Graph] :=
	Equivalences[ AllPairsShortestPath[g], AllPairsShortestPath[h]]

Equivalences[g_List,h_List] :=
	Module[{dg=Map[Sort,g],dh=Map[Sort,h],s,i},
		Table[
			Flatten[Position[dh,_?(Function[s,SameQ[s,dg[[i]] ]])]],
			{i,Length[dg]}
		]
	] /; Length[g] == Length[h]

Automorphisms[g_Graph,flag_:All] :=
	Module[{s=AllPairsShortestPath[g]},
		Backtrack[
			Equivalences[s,s],
			(IdenticalQ[InduceSubgraph[g,Range[Length[#]]],
				    InduceSubgraph[g,#] ] &&
			 !MemberQ[Drop[#,-1],Last[#]])&,
			(IsomorphismQ[g,g,#])&,
			flag
		]
	]

SelfComplementaryQ[g_Graph] := IsomorphicQ[g, GraphComplement[g]]

FindCycle[g_Graph,flag_:Undirected] :=
     Module[{edge,n=V[g],x,queue,v,seen,parent},
       edge=ToAdjacencyLists[g];
       For[ v = 1, v <= n, v++,
           parent=Table[n+1,{n}]; parent[[v]] = 0;
           seen = {}; queue = {v};
           While[ queue != {},
               {x,queue} = {First[queue], Rest[queue]};
               AppendTo[seen,x];
               If[ SameQ[ flag, Undirected],
                   Scan[ (If[ parent[[x]] != #, parent[[#]]=x])&, edge[[x]] ],
                   Scan[ (parent[[#]]=x)&, edge[[x]]]
               ];
               If[ SameQ[flag,Undirected],
                   If[ MemberQ[ edge[[x]],v ] && parent[[x]] != v,
                       Return[ FromParent[parent,x] ]
                   ],
                   If[ MemberQ[ edge[[x]],v ],
                       Return[ FromParent[parent,x] ]
                   ]
               ];
               queue = Join[ Complement[ edge[[x]], seen], queue]
           ]
       ];
     {}
     ]

FromParent[parent_List,s_Integer] :=
	Module[{i=s,lst={s}},
		While[!MemberQ[lst,(i=parent[[i]])], PrependTo[lst,i] ];
		PrependTo[lst,i];
		Take[lst, Flatten[Position[lst,i]]]
	]

AcyclicQ[g_Graph,flag_:Undirected] := SameQ[FindCycle[g,flag],{}]

TreeQ[g_Graph] := ConnectedQ[g] && (M[g] == V[g]-1)

ExtractCycles[gi_Graph,flag_:Undirected] :=
	Module[{g=gi,cycles={},c},
		While[!SameQ[{}, c=FindCycle[g,flag]],
			PrependTo[cycles,c];
			g = DeleteCycle[g,c,flag];
		];
		cycles
	]

DeleteCycle[g_Graph,cycle_List,flag_:Undirected] :=
	Module[{newg=g},
		Scan[(newg=DeleteEdge[newg,#,flag])&, Partition[cycle,2,1] ];
		newg
	]

Girth[g_Graph] :=
	Module[{v,dist,queue,n=V[g],girth=Infinity,parent,e=ToAdjacencyLists[g],x},
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
	] /; SimpleQ[g]

EulerianQ[g_Graph,Directed] :=
	ConnectedQ[g,Undirected] && (InDegree[g] === OutDegree[g])

EulerianQ[g_Graph,flag_:Undirected] := ConnectedQ[g,Undirected] &&
	UndirectedQ[g] && Apply[And,Map[EvenQ,DegreeSequence[g]]]

OutDegree[Graph[e_List,_],n_Integer] := Length[ Select[ e[[n]], (# != 0)& ] ]
OutDegree[g_Graph] := Map[ (OutDegree[g,#])&, Range[V[g]] ]

InDegree[g_Graph,n_Integer] := OutDegree[ TransposeGraph[g], n ];
InDegree[g_Graph] := Map[ (InDegree[g,#])&, Range[V[g]] ]

TransposeGraph[Graph[g_List,v_List]] := Graph[ Transpose[g], v ]

EulerianCycle[g_Graph,flag_:Undirected] :=
	Module[{euler,c,cycles,v},
		cycles = Map[(Drop[#,-1])&, ExtractCycles[g,flag]];
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
	] /; EulerianQ[g,flag]

DeBruijnSequence[alph_List,n_Integer] :=
        Module[{states = Strings[alph,n-1]},
                Rest[ Map[
                        (First[ states[[#]] ])&,
                        EulerianCycle[
                                MakeGraph[
                                        states,
                                        (Module[{i},
                                         MemberQ[
                                                Table[
                                                        Append[Rest[#1],alph[[i]]],
                                                        {i,Length[alph]}
                                                ],
                                                #2
                                         ]
                                        ])&
                                ],
                                Directed
                        ]
                ] ]
        ] /; n>=2

DeBruijnSequence[alph_List,n_Integer] := alph /; n==1

HamiltonianQ[g_Graph] := False /; !BiconnectedQ[g]
HamiltonianQ[g_Graph] := HamiltonianCycle[g] != {}

HamiltonianCycle[g_Graph,flag_:One] :=
	Module[{s={1},all={},done,adj=Edges[g],e=ToAdjacencyLists[g],x,v,ind,n=V[g]},
		ind=Table[1,{n}];
		While[ Length[s] > 0,
			v = Last[s];
			done = False;
			While[ ind[[v]] <= Length[e[[v]]] && !done,
				If[!MemberQ[s,(x = e[[v,ind[[v]]++]])], done=True]
			];
			If[ done, AppendTo[s,x], s=Drop[s,-1]; ind[[v]] = 1];
			If[(Length[s] == n),
				If [(adj[[x,1]]>0),
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

TravelingSalesman[g_Graph] :=
	Module[{v,s={1},sol={},done,cost,g1,e=ToAdjacencyLists[g],x,ind,best,n=V[g]},
		ind=Table[1,{n}];
		g1 = PathConditionGraph[g];
		best = Infinity;
		While[ Length[s] > 0,
			v = Last[s];
			done = False;
			While[ ind[[v]] <= Length[e[[v]]] && !done,
				x = e[[v,ind[[v]]++]];
				done = (best > CostOfPath[g1,Append[s,x]]) &&
					!MemberQ[s,x]
			];
			If[done, AppendTo[s,x], s=Drop[s,-1]; ind[[v]] = 1];
			If[(Length[s] == n),
				cost = CostOfPath[g1, Append[s,First[s]]];
				If [(cost < best), sol = s; best = cost ];
				s = Drop[s,-1]
			]
		];
		Append[sol,First[sol]]
	]

CostOfPath[Graph[g_,_],p_List] := Apply[Plus, Map[(Element[g,#])&,Partition[p,2,1]] ]

Element[a_List,{index___}] := a[[ index ]]

TriangleInequalityQ[e_?SquareMatrixQ] :=
	Module[{i,j,k,n=Length[e],flag=True},
		Do [

			If[(e[[i,k]]!=0) && (e[[k,j]]!=0) && (e[[i,j]]!=0),
				If[e[[i,k]]+e[[k,j]]<e[[i,j]],
					flag = False;
				]
			],
			{i,n},{j,n},{k,n}
		];
		flag
	]

TriangleInequalityQ[g_Graph] := TriangleInequalityQ[Edges[g]]

TravelingSalesmanBounds[g_Graph] := {LowerBoundTSP[g], UpperBoundTSP[g]}

UpperBoundTSP[g_Graph] :=
	CostOfPath[g, Append[DepthFirstTraversal[MinimumSpanningTree[g],1],1]]

LowerBoundTSP[g_Graph] := Apply[Plus, Map[Min,ReplaceAll[Edges[g],0->Infinity]]]

PartialOrderQ[g_Graph] := ReflexiveQ[g] && AntiSymmetricQ[g] && TransitiveQ[g]

TransitiveQ[g_Graph] := IdenticalQ[g,TransitiveClosure[g]]

ReflexiveQ[Graph[g_List,_]] :=
	Module[{i},
		Apply[And, Table[(g[[i,i]]!=0),{i,Length[g]}] ]
	]

AntiSymmetricQ[g_Graph] :=
	Module[{e = Edges[g], g1 = RemoveSelfLoops[g]},
		Apply[And, Map[(Element[e,Reverse[#]]==0)&,ToOrderedPairs[g1]] ]
	]

TransitiveClosure[g_Graph] :=
	Module[{i,j,k,e=Edges[g],n=V[g]},
		Do [
			If[ e[[j,i]] != 0,
				Do [
					If[ e[[i,k]] != 0, e[[j,k]]=1],
					{k,n}
				]
			],
			{i,n},{j,n}
		];
		Graph[e,Vertices[g]]
	]

TransitiveReduction[g_Graph] :=
	Module[{closure=reduction=Edges[g],i,j,k,n=V[g]},
		Do[
			If[ closure[[i,j]]!=0 && closure[[j,k]]!=0 &&
				 reduction[[i,k]]!=0 && (i!=j) && (j!=k) && (i!=k),
					reduction[[i,k]] = 0
			],
			{i,n},{j,n},{k,n}
		];
		Graph[reduction,Vertices[g]]
	] /; AcyclicQ[RemoveSelfLoops[g],Directed]

TransitiveReduction[g_Graph] :=
	Module[{reduction=Edges[g],i,j,k,n=V[g]},
		Do[
			If[ reduction[[i,j]]!=0 && reduction[[j,k]]!=0 &&
				 reduction[[i,k]]!=0 && (i!=j) && (j!=k) && (i!=k),
					reduction[[i,k]] = 0
			],
			{i,n},{j,n},{k,n}
		];
		Graph[reduction,Vertices[g]]
	]

HasseDiagram[g_Graph] :=
	Module[{r,rank,m,stages,freq=Table[0,{V[g]}]},
		r = TransitiveReduction[ RemoveSelfLoops[g] ];
		rank = RankGraph[
				MakeUndirected[r],
				Select[Range[V[g]],(InDegree[r,#]==0)&]
		];
		m = Max[rank];
		rank = MapAt[(m)&,rank,Position[OutDegree[r],0]];
		stages = Distribution[ rank ];
		Graph[
			Edges[r],
			Table[
				m = ++ freq[[ rank[[i]] ]];
				{(m-1) + (1-stages[[rank[[i]] ]])/2, rank[[i]]},
				{i,V[g]}
			]
		]
	] /; AcyclicQ[RemoveSelfLoops[g],Directed]

TopologicalSort[g_Graph] :=
	Module[{g1 = RemoveSelfLoops[g],e,indeg,zeros,v},
		e=ToAdjacencyLists[g1];
		indeg=InDegree[g1];
		zeros = Flatten[ Position[indeg, 0] ];
		Table [
			{v,zeros}={First[zeros],Rest[zeros]};
			Scan[
				( indeg[[#]]--;
				  If[indeg[[#]]==0, AppendTo[zeros,#]] )&,
				e[[ v ]]
			];
			v,
			{V[g]}
		]
	] /; AcyclicQ[RemoveSelfLoops[g],Directed]

ChromaticPolynomial[g_Graph,z_] := 0 /; Identical[g,K[0]]

ChromaticPolynomial[g_Graph,z_] :=
	Module[{i}, Product[z-i, {i,0,V[g]-1}] ] /; CompleteQ[g]

ChromaticPolynomial[g_Graph,z_] := z ( z - 1 ) ^ (V[g]-1) /; TreeQ[g]

ChromaticPolynomial[g_Graph,z_] :=
	If [M[g]>Binomial[V[g],2]/2, ChromaticDense[g,z], ChromaticSparse[g,z]]

ChromaticSparse[g_Graph,z_] := z^V[g] /; EmptyQ[g]
ChromaticSparse[g_Graph,z_] :=
	Module[{i=1, v, e=Edges[g], none=Table[0,{V[g]}]},
        	While[e[[i]] === none, i++];
        	v = Position[e[[i]],1] [[1,1]];
		ChromaticSparse[ DeleteEdge[g,{i,v}], z ] -
			ChromaticSparse[ Contract[g,{i,v}], z ]
	]

ChromaticDense[g_Graph,z_] := ChromaticPolynomial[g,z] /; CompleteQ[g]
ChromaticDense[g_Graph,z_] :=
	Module[
		{i=1, v, e=Edges[g], all=Join[Table[1,{V[g]-1}],{0}] },
		While[e[[i]] === RotateRight[all,i], i++];
		v = Last[ Position[e[[i]],0] ] [[1]];
		ChromaticDense[ AddEdge[g,{i,v}], z ] +
			ChromaticDense[ Contract[g,{i,v}], z ]
	]

ChromaticNumber[g_Graph] :=
	Block[{ways, z},
		ways[z_] = ChromaticPolynomial[g,z];
		For [z=0, z<=V[g], z++,
			If [ways[z] > 0, Return[z]]
		]
	]

TwoColoring[g_Graph] :=
	Module[{queue,elem,edges,col,flag=True,colored=Table[0,{V[g]}]},
		edges = ToAdjacencyLists[g];
		While[ MemberQ[colored,0],
			queue = First[ Position[colored,0] ];
			colored[[ First[queue] ]] = 1;
			While[ queue != {},
				elem = First[queue];
				col = colored[[elem]];
				Scan[
					(Switch[colored[[ # ]],
						col, flag = False,
						0, AppendTo[queue, # ];
						   colored[[#]] = Mod[col,2]+1
					])&,
					edges[[elem]]
				];
				queue = Rest[queue];
			]
		];
		If [!flag, colored[[1]] = 0];
		colored
	]

BipartiteQ[g_Graph] := ! MemberQ[ TwoColoring[g], 0 ]

VertexColoring[g_Graph] :=
	Module[{v,l,n=V[g],e=ToAdjacencyLists[g],x,color=Table[0,{V[g]}]},
		v = Map[(Apply[Plus,#])&, Edges[g]];
		Do[
			l = MaximumColorDegreeVertices[e,color];
			x = First[l];
			Scan[(If[ v[[#]] > v[[x]], x = #])&, l];
			color[[x]] = Min[
				Complement[ Range[n], color[[ e[[x]] ]] ]
			],
			{V[g]}
		];
		color
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

EdgeColoring[g_Graph] := VertexColoring[ LineGraph[g] ]

EdgeChromaticNumber[g_Graph] := ChromaticNumber[ LineGraph[g] ]

CliqueQ[g_Graph,clique_List] :=
	IdenticalQ[ K[Length[clique]], InduceSubgraph[g,clique] ] /; SimpleQ[g]

MaximumClique[g_Graph] := {} /; g === K[0]

MaximumClique[g_Graph] :=
	Module[{d = Degrees[g],i,clique=Null,k},
		i = Max[d];
		While[(SameQ[clique,Null]),
			k = K[i+1];
			clique = FirstExample[
				KSubsets[Flatten[Position[d,_?((#>=i)&)]], i+1],
				(IdenticalQ[k,InduceSubgraph[g,#]])&
			];
			i--;
		];
		clique
	]

FirstExample[list_List, predicate_] := Scan[(If [predicate[#],Return[#]])&,list]

VertexCoverQ[g_Graph,vc_List] :=
	CliqueQ[ GraphComplement[g], Complement[Range[V[g]], vc] ]

MinimumVertexCover[g_Graph] :=
	Complement[ Range[V[g]], MaximumClique[ GraphComplement[g] ] ]

IndependentSetQ[g_Graph,indep_List] :=
	VertexCoverQ[ g, Complement[ Range[V[g]], indep] ]

MaximumIndependentSet[g_Graph] := Complement[Range[V[g]], MinimumVertexCover[g]]

PerfectQ[g_Graph] :=
	Apply[
		And,
		Map[(ChromaticNumber[#] == Length[MaximumClique[#]])&,
			Map[(InduceSubgraph[g,#])&, Subsets[Range[V[g]]] ] ]
	]

Dijkstra[g_Graph,start_Integer] := First[ Dijkstra[g,{start}] ]

Dijkstra[g_Graph, l_List] :=
	Module[{x,start,e=ToAdjacencyLists[g],i,p,parent,untraversed},
		p=Edges[PathConditionGraph[g]];
		Table[
			start = l[[i]];
			parent=untraversed=Range[V[g]];
			dist = p[[start]]; dist[[start]] = 0;
			Scan[ (parent[[#]] = start)&, e[[start]] ];
			While[ untraversed != {} ,
				x = First[untraversed];
				Scan[(If [dist[[#]]<dist[[x]],x=#])&, untraversed];
				untraversed = Complement[untraversed,{x}];
				Scan[
					(If[dist[[#]] > dist[[x]]+p[[x,#]],
						dist[[#]] = dist[[x]]+p[[x,#]];
						parent[[#]] = x ])&,
					e[[x]]
				];
			];
			{parent, dist},
			{i,Length[l]}
		]
	]

ShortestPath[g_Graph,s_Integer,e_Integer] :=
	Module[{parent=First[Dijkstra[g,s]],i=e,lst={e}},
		While[ (i != s) && (i != parent[[i]]),
			PrependTo[lst,parent[[i]]];
			i = parent[[i]]
		];
		If[ i == s, lst, {}]
	]

ShortestPathSpanningTree[g_Graph,s_Integer] :=
	Module[{parent=First[Dijkstra[g,s]],i},
		FromUnorderedPairs[
			Map[({#,parent[[#]]})&, Complement[Range[V[g]],{s}]],
			Vertices[g]
		]
	]

AllPairsShortestPath[g_Graph] :=
	Module[{p=Edges[ PathConditionGraph[g] ],i,j,k,n=V[g]},
		Do [
			p = Table[Min[p[[i,k]]+p[[k,j]],p[[i,j]]],{i,n},{j,n}],
			{k,n}
		];
		p
	] /; Min[Edges[g]] < 0

AllPairsShortestPath[g_Graph] := Map[ Last, Dijkstra[g, Range[V[g]]]]

PathConditionGraph[Graph[e_,v_]] := RemoveSelfLoops[Graph[ReplaceAll[e,0->Infinity],v]]

GraphPower[g_Graph,1] := g

GraphPower[g_Graph,n_Integer] :=
	Module[{prod=power=p=Edges[g]},
		Do [
			prod = prod . p;
			power = prod + power,
			{n-1}
		];
		Graph[power, Vertices[g]]
	]

InitializeUnionFind[n_Integer] := Module[{i}, Table[{i,1},{i,n}] ]

FindSet[n_Integer,s_List] := If [n == s[[n,1]], n, FindSet[s[[n,1]],s] ]

UnionSet[a_Integer,b_Integer,s_List] :=
	Module[{sa=FindSet[a,s], sb=FindSet[b,s], set=s},
		If[ set[[sa,2]] < set[[sb,2]], {sa,sb} = {sb,sa} ];
		set[[sa]] = {sa, Max[ set[[sa,2]], set[[sb,2]]+1 ]};
		set[[sb]] = {sa, set[[sb,2]]};
		set
	]

MinimumSpanningTree[g_Graph] :=
	Module[{edges=Edges[g],set=InitializeUnionFind[V[g]]},
		FromUnorderedPairs[
			Select [
				Sort[
					ToUnorderedPairs[g],
					(Element[edges,#1]<=Element[edges,#2])&
				],
				(If [FindSet[#[[1]],set] != FindSet[#[[2]],set],
					set=UnionSet[#[[1]],#[[2]],set]; True,
					False
				])&
			],
			Vertices[g]
		]
	] /; UndirectedQ[g]

MaximumSpanningTree[g_Graph] := MinimumSpanningTree[Graph[-Edges[g],Vertices[g]]]

Cofactor[m_List,{i_Integer,j_Integer}] :=
	(-1)^(i+j) * Det[ Drop[ Transpose[ Drop[Transpose[m],{j,j}] ], {i,i}] ]

NumberOfSpanningTrees[Graph[g_List,_]] :=
	Cofactor[ DiagonalMatrix[Map[(Apply[Plus,#])&,g]] - g, {1,1}]

NetworkFlow[g_Graph,source_Integer,sink_Integer] :=
	Block[{flow=NetworkFlowEdges[g,source,sink], i},
		Sum[flow[[i,sink]], {i,V[g]}]
	]


NetworkFlowEdges[g_Graph,source_Integer,sink_Integer] :=
	Block[{e=Edges[g], x, y, flow=Table[0,{V[g]},{V[g]}], p, m},
		While[ !SameQ[p=AugmentingPath[g,source,sink], {}],
			m = Min[Map[({x,y}=#[[1]];
				 If[SameQ[#[[2]],f],e[[x,y]]-flow[[x,y]],
					flow[[x,y]]])&,p]];
			Scan[
				({x,y}=#[[1]];
				 If[ SameQ[#[[2]],f],
					flow[[x,y]]+=m,flow[[x,y]]-=m])&,
				 p
			]
		];
		flow
	]

AugmentingPath[g_Graph,src_Integer,sink_Integer] :=
	Block[{l={src},lab=Table[0,{V[g]}],v,c=Edges[g],e=ToAdjacencyLists[g]},
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

BipartiteMatching[g_Graph] :=
	Module[{p,v1,v2,coloring=TwoColoring[g],n=V[g]},
		v1 = Flatten[Position[coloring,1]];
		v2 = Flatten[Position[coloring,2]];
		p = BipartiteMatchingFlowGraph[g,v1,v2];
		flow = NetworkFlowEdges[p,V[g]+1,V[g]+2];
		Select[ToOrderedPairs[Graph[flow,Vertices[p]]], (Max[#]<=n)&]
	] /; BipartiteQ[g]

BipartiteMatchingFlowGraph[g_Graph,v1_List,v2_List] :=
	Module[{edges = Table[0,{V[g]+2},{V[g]+2}],i,e=ToAdjacencyLists[g]},
		Do[
	    		Scan[ (edges[[v1[[i]],#]] = 1)&, e[[ v1[[i]] ]] ],
			{i,Length[v1]}
		];
		Scan[(edges[[V[g] + 1, #]] = 1)&, v1];
		Scan[(edges[[#, V[g] + 2]] = 1)&, v2];
		Graph[edges,RandomVertices[V[g] + 2] ]
	]

MinimumChainPartition[g_Graph] :=
	ConnectedComponents[
		FromUnorderedPairs[
			Map[(#-{0,V[g]})&, BipartiteMatching[DilworthGraph[g]]],
			Vertices[g]
		]
	]

MaximumAntichain[g_Graph] := MaximumIndependentSet[TransitiveClosure[g]]

DilworthGraph[g_Graph] :=
	FromUnorderedPairs[
		Map[
			(#+{0,V[g]})&,
			ToOrderedPairs[RemoveSelfLoops[TransitiveReduction[g]]]
		]
	]

MaximalMatching[g_Graph] :=
	Module[{match={}},
		Scan[
			(If [Intersection[#,match]=={}, match=Join[match,#]])&,
			ToUnorderedPairs[g]
		];
		Partition[match,2]
	]

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

BeforeQ[l_List,a_,b_] :=
	If [First[l]==a, True, If [First[l]==b, False, BeforeQ[Rest[l],a,b] ] ]

PlanarQ[g_Graph] :=
	Apply[
		And,
		Map[(PlanarQ[InduceSubgraph[g,#]])&, ConnectedComponents[g]]
	] /; !ConnectedQ[g]

PlanarQ[g_Graph] := False /;  (M[g] > 3 V[g]-6) && (V[g] > 2)
PlanarQ[g_Graph] := True /;   (M[g] < V[g] + 3)
PlanarQ[g_Graph] := PlanarGivenCycle[ g, Rest[FindCycle[g]] ]

PlanarGivenCycle[g_Graph, cycle_List] :=
	Module[{b, j, i},
		{b, j} = FindBridge[g, cycle];
		If[ InterlockQ[j, cycle],
			False,
			Apply[And, Table[SingleBridgeQ[b[[i]],j[[i]]], {i,Length[b]}]]
		]
	]

SingleBridgeQ[b_Graph, {_}] := PlanarQ[b]

SingleBridgeQ[b_Graph, j_List] :=
	PlanarGivenCycle[ JoinCycle[b,j],
		Join[ ShortestPath[b,j[[1]],j[[2]]], Drop[j,2]] ]

JoinCycle[g1_Graph, cycle_List] :=
	Module[{g=g1},
		Scan[(g = AddEdge[g,#])&, Partition[cycle,2,1] ];
		AddEdge[g,{First[cycle],Last[cycle]}]
	]

FindBridge[g_Graph, cycle_List] :=
    Module[{rg = RemoveCycleEdges[g, cycle], b, bridge, j},
	b = Map[
		(IsolateSubgraph[rg,g,cycle,#])&,
		Select[ConnectedComponents[rg], (Intersection[#,cycle]=={})&]
	];
	b = Select[b, (!EmptyQ[#])&];
	j = Join[
		Map[Function[bridge,Select[cycle, MemberQ[Edges[bridge][[#]],1]&] ], b],
		Complement[
			Select[ToOrderedPairs[g],
				(Length[Intersection[#,cycle]] == 2)&],
			Partition[Append[cycle,First[cycle]],2,1]
		]
	];
	{b, j}
    ]

RemoveCycleEdges[g_Graph, c_List] :=
	FromOrderedPairs[
		Select[ ToOrderedPairs[g], (Intersection[c,#] === {})&],
		Vertices[g]
	]

IsolateSubgraph[g_Graph,orig_Graph,cycle_List,cc_List] :=
	Module[{eg=ToOrderedPairs[g], og=ToOrderedPairs[orig]},
		FromOrderedPairs[
			Join[
				Select[eg, (Length[Intersection[cc,#]] == 2)&],
				Select[og, (Intersection[#,cycle]!={} &&
					Intersection[#,cc]!={})&]
			],
			Vertices[g]
		]
	]

InterlockQ[ bl_List, c_List ] :=
	Module[{in = out = {}, code, jp, bridgelist = bl },
		While [ bridgelist != {},
			{jp, bridgelist} = {First[bridgelist],Rest[bridgelist]};
			code = Sort[ Map[(Position[c, #][[1,1]])&, jp] ];
			If[ Apply[ Or, Map[(LockQ[#,code])&, in] ],
				If [ Apply[Or, Map[(LockQ[#,code])&, out] ],
					Return[True],
					AppendTo[out,code]
				],
				AppendTo[in,code]
			]
		];
		False
	]

LockQ[a_List,b_List] := Lock1Q[a,b] || Lock1Q[b,a]

Lock1Q[a_List,b_List] :=
	Module[{bk, aj},
		bk = Min[ Select[Drop[b,-1], (#>First[a])&] ];
		aj = Min[ Select[a, (# > bk)&] ];
		(aj < Max[b])
	]

KSetPartitions::usage = "KSetPartitions[set, k] returns the list of set partitions of set with k blocks. KSetPartitions[n, k] returns the list of set partitions of {1, 2, ..., n} with k blocks. If all set partitions of a set are needed, use the function SetPartitions."
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

SetPartitions::usage = "SetPartitions[set] returns the list of set partitions of set. SetPartitions[n] returns the list of set partitions of {1, 2, ..., n}. If all set partitions with a fixed number of subsets are needed use KSetPartitions."

SetPartitions[{}] := {{}}
SetPartitions[s_List] := Flatten[Table[KSetPartitions[s, i], {i, Length[s]}], 1]

SetPartitions[0] := {{}}
SetPartitions[n_Integer?Positive] := SetPartitions[Range[n]]


End[]

Protect[
AcyclicQ,
AddEdge,
AddVertex,
AllPairsShortestPath,
ArticulationVertices,
Automorphisms,
Backtrack,
BiconnectedComponents,
BiconnectedComponents,
BiconnectedQ,
BinarySearch,
BinarySubsets,
BipartiteMatching,
BipartiteQ,
BreadthFirstTraversal,
Bridges,
CartesianProduct,
CatalanNumber,
ChangeEdges,
ChangeVertices,
ChromaticNumber,
ChromaticPolynomial,
CirculantGraph,
CircularVertices,
CliqueQ,
CodeToLabeledTree,
Cofactor,
CompleteQ,
Compositions,
ConnectedComponents,
ConnectedQ,
ConstructTableau,
Contract,
CostOfPath,
Cycle,
DeBruijnSequence,
DegreeSequence,
DeleteCycle,
DeleteEdge,
DeleteFromTableau,
DeleteVertex,
DepthFirstTraversal,
DerangementQ,
Derangements,
Diameter,
Dijkstra,
DilateVertices,
DistinctPermutations,
Distribution,
DurfeeSquare,
Eccentricity,
EdgeChromaticNumber,
EdgeColoring,
EdgeConnectivity,
Edges,
Element,
EmptyGraph,
EmptyQ,
EncroachingListSet,
EquivalenceClasses,
EquivalenceRelationQ,
Equivalences,
EulerianCycle,
EulerianQ,
Eulerian,
ExactRandomGraph,
ExpandGraph,
ExtractCycles,
FerrersDiagram,
FindCycle,
FindSet,
FirstLexicographicTableau,
FromAdjacencyLists,
FromCycles,
FromInversionVector,
FromOrderedPairs,
FromUnorderedPairs,
FunctionalGraph,
Girth,
GraphCenter,
GraphComplement,
GraphDifference,
GraphIntersection,
GraphJoin,
GraphPower,
GraphProduct,
GraphSum,
GraphUnion,
GraphicQ,
GrayCode,
GridGraph,
HamiltonianCycle,
HamiltonianQ,
Harary,
HasseDiagram,
HeapSort,
Heapify,
HideCycles,
Hypercube,
IdenticalQ,
IncidenceMatrix,
IndependentSetQ,
Index,
InduceSubgraph,
InitializeUnionFind,
InsertIntoTableau,
IntervalGraph,
InversePermutation,
Inversions,
InvolutionQ,
IsomorphicQ,
IsomorphismQ,
Isomorphism,
Josephus,
KSubsets,
K,
LabeledTreeToCode,
LastLexicographicTableau,
LexicographicPermutations,
LexicographicSubsets,
LineGraph,
LongestIncreasingSubsequence,
M,
MakeGraph,
MakeSimple,
MakeUndirected,
MaximalMatching,
MaximumAntichain,
MaximumClique,
MaximumIndependentSet,
MaximumSpanningTree,
MinimumChainPartition,
MinimumChangePermutations,
MinimumSpanningTree,
MinimumVertexCover,
MultiplicationTable,
NetworkFlowEdges,
NetworkFlow,
NextComposition,
NextKSubset,
NextPartition,
NextPermutation,
NextSubset,
NextTableau,
NormalizeVertices,
NthPair,
NthPermutation,
NthSubset,
NumberOfCompositions,
NumberOfDerangements,
NumberOfInvolutions,
NumberOfPartitions,
NumberOfPermutationsByCycles,
NumberOfSpanningTrees,
NumberOfTableaux,
OrientGraph,
PartialOrderQ,
PartitionQ,
Partitions,
PathConditionGraph,
Path,
PerfectQ,
PermutationGroupQ,
PermutationQ,
Permute,
PlanarQ,
PointsAndLines,
Polya,
PseudographQ,
RadialEmbedding,
Radius,
RandomComposition,
RandomGraph,
RandomHeap,
RandomKSubset,
RandomPartition,
RandomPermutation1,
RandomPermutation2,
RandomPermutation,
RandomSubset,
RandomTableau,
RandomTree,
RandomVertices,
RankGraph,
RankPermutation,
RankSubset,
RankedEmbedding,
ReadGraph,
RealizeDegreeSequence,
RegularGraph,
RegularQ,
RemoveSelfLoops,
RevealCycles,
RootedEmbedding,
RotateVertices,
Runs,
SamenessRelation,
SelectionSort,
SelfComplementaryQ,
ShakeGraph,
ShortestPathSpanningTree,
ShortestPath,
ShowGraph,
ShowLabeledGraph,
SignaturePermutation,
SimpleQ,
Spectrum,
SpringEmbedding,
StableMarriage,
Star,
StirlingFirst,
StirlingSecond,
Strings,
StronglyConnectedComponents,
Subsets,
TableauClasses,
TableauQ,
TableauxToPermutation,
Tableaux,
ToAdjacencyLists,
ToCycles,
ToInversionVector,
ToOrderedPairs,
ToUnorderedPairs,
TopologicalSort,
TransitiveClosure,
TransitiveQ,
TransitiveReduction,
TranslateVertices,
TransposePartition,
TransposeTableau,
TravelingSalesmanBounds,
TravelingSalesman,
TreeQ,
TriangleInequalityQ,
Turan,
TwoColoring,
UndirectedQ,
UnionSet,
UnweightedQ,
V,
VertexColoring,
VertexConnectivity,
VertexCoverQ,
Vertices,
WeaklyConnectedComponents,
Wheel,
WriteGraph,
DilworthGraph ]

EndPackage[ ]
