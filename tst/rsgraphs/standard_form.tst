# Testing the standard form function. 
gap> START_TEST("standard_form.tst");

#
gap> graph_1 := RSGraphByAdjacencyList([[2, 2], [752, 2], [2, 752], [2, 2]], (2,3), [2, 752]);;
gap> standard_1 := RSGraphToStandardForm(graph_1);;
gap> 4^standard_1.arc_id_map;
2
gap> 752^standard_1.vertex_id_map;
2
gap> Print(standard_1.graph);
Vertices = { 1, 2 }
Arcs = {
	1 = ( origin = 1, terminus = 1, inverse = 1 )
	2 = ( origin = 1, terminus = 1, inverse = 2 )
	3 = ( origin = 1, terminus = 2, inverse = 4 )
	4 = ( origin = 2, terminus = 1, inverse = 3 )
}
Reverse Map = (3,4)

#
gap> subgraph_1 := RSGraphSubgraph(graph_1, [2, 3, 4]);;
gap> standard_1_2 := RSGraphToStandardForm(subgraph_1);;
gap> Print(standard_1_2.graph);
Vertices = { 1, 2 }
Arcs = {
	1 = ( origin = 1, terminus = 1, inverse = 1 )
	2 = ( origin = 1, terminus = 2, inverse = 3 )
	3 = ( origin = 2, terminus = 1, inverse = 2 )
}
Reverse Map = (2,3)

#
gap> graph_2 := RSGraphByAdjacencyList([[1, 2], [1, 1], [2, 1]], (1,3));;
gap> standard_2 := RSGraphToStandardForm(graph_2);;
gap> Print(standard_2.graph);
Vertices = { 1, 2 }
Arcs = {
	1 = ( origin = 1, terminus = 1, inverse = 1 )
	2 = ( origin = 1, terminus = 2, inverse = 3 )
	3 = ( origin = 2, terminus = 1, inverse = 2 )
}
Reverse Map = (2,3)

#
gap> STOP_TEST("standard_form.tst");
