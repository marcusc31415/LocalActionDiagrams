# Testing what is printed when the View function is used. Special cases for 1 vertex and 1 arc graphs. 
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

# TODO: Fix printing empty graph. 

#
gap> STOP_TEST("print.tst");
