# Tests the adjacency matrix construction. 
gap> START_TEST("adj_mat_construction.tst");

# 
gap> graph_1 := RSGraphByAdjacencyMatrix([[1, 1], [1, 0]], (2,3));;
gap> graph_2 := RSGraphByAdjacencyMatrix([[3]], ());;
gap> graph_3 := RSGraphByAdjacencyMatrix([[0]], ());;
gap> Print(graph_1);
Vertices = { 1, 2 }
Arcs = {
	1 = ( origin = 1, terminus = 1, inverse = 1 )
	2 = ( origin = 1, terminus = 2, inverse = 3 )
	3 = ( origin = 2, terminus = 1, inverse = 2 )
}
Reverse Map = (2,3)
gap> Print(graph_2);
Vertices = { 1 }
Arcs = {
	1 = ( origin = 1, terminus = 1, inverse = 1 )
	2 = ( origin = 1, terminus = 1, inverse = 2 )
	3 = ( origin = 1, terminus = 1, inverse = 3 )
}
Reverse Map = ()
gap> Print(graph_3);
Vertices = { 1 }
Arcs = {
}
Reverse Map = ()

#
gap> STOP_TEST("adj_mat_construction.tst", 1);
