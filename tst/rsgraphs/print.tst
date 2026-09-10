# Testing what is printed when the Print function is used. Special cases for 1 vertex and 1 arc graphs. 
gap> START_TEST("print.tst");

#
gap> graph := RSGraphByAdjacencyList([[1, 2], [2, 1], [1, 1]], (1,2));
<RSGraph with 2 vertices and 3 arcs>
gap> Print(graph);
Vertices = { 1, 2 }
Arcs = {
	1 = ( origin = 1, terminus = 2, inverse = 2 )
	2 = ( origin = 2, terminus = 1, inverse = 1 )
	3 = ( origin = 1, terminus = 1, inverse = 3 )
}
Reverse Map = (1,2)

# Empty graph. 
gap> graph_2 := RSGraphByAdjacencyList([], ());
<RSGraph with 0 vertices and 0 arcs>
gap> Print(graph_2);
Vertices = {  }
Arcs = {
}
Reverse Map = ()

# Single vertex graph.
gap> graph_3 := RSGraphByAdjacencyList([], (), [1]);
<RSGraph with 1 vertex and 0 arcs>
gap> Print(graph_3);
Vertices = { 1 }
Arcs = {
}
Reverse Map = ()

#
gap> STOP_TEST("print.tst");
