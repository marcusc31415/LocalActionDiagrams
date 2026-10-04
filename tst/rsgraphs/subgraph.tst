# Testing the subgraph function. 
gap> START_TEST("subgraph.tst");

#
gap> graph_1 := RSGraphByAdjacencyList([[1, 1], [1, 2], [2, 1], [1, 1]], (2,3));;
gap> subgraph_1 := RSGraphSubgraph(graph_1, [2, 3, 4]);
<RSGraph with 2 vertices and 3 arcs>
gap> Print(subgraph_1);
Vertices = { 1, 2 }
Arcs = {
	2 = ( origin = 1, terminus = 2, inverse = 3 )
	3 = ( origin = 2, terminus = 1, inverse = 2 )
	4 = ( origin = 1, terminus = 1, inverse = 4 )
}
Reverse Map = (2,3)

#
gap> subgraph_1_2 := RSGraphSubgraph(subgraph_1, [2, 3]);
<RSGraph with 2 vertices and 2 arcs>
gap> Print(subgraph_1_2);
Vertices = { 1, 2 }
Arcs = {
	2 = ( origin = 1, terminus = 2, inverse = 3 )
	3 = ( origin = 2, terminus = 1, inverse = 2 )
}
Reverse Map = (2,3)

#
gap> graph_2 := RSGraphByAdjacencyList([[1, 1], [1, 2], [2, 1], [2, 2]], (2,3));;
gap> subgraph_2 := RSGraphSubgraph(graph_2, [1, 3, 4]);
Error, Reverse of arc 3 is not in the list of arc ids.

#
gap> subgraph_2 := RSGraphSubgraph(graph_2, [1, 4]);
Error, Graph must be connected.

#
gap> subgraph_2 := RSGraphSubgraph(graph_2, [1]);
<RSGraph with 1 vertex and 1 arc>

#
gap> subgraph_2_2 := RSGraphSubgraph(graph_2, [4]);
<RSGraph with 1 vertex and 1 arc>
gap> Print(subgraph_2_2);
Vertices = { 2 }
Arcs = {
	4 = ( origin = 2, terminus = 2, inverse = 4 )
}
Reverse Map = (2,3)

# Test iterator when there's a gap in the arcs. 
gap> iter := RSGraphArcIterator(subgraph_2_2);;
gap> NextIterator(iter);
[ 4, rec( inverse := 4, origin := 2, terminus := 2 ) ]
gap> NextIterator(iter);
Error, Iterator is exhausted.

#
gap> STOP_TEST("subgraph.tst");
