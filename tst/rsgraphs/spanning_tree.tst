# Testing the spanning tree function.
gap> START_TEST("spanning_tree.tst");

#
gap> graph_1 := RSGraphByAdjacencyList([[1, 1], [1, 2], [2, 1], [2, 2], [1, 1]], (2,3));;
gap> tree_1 := RSGraphSpanningTree(graph_1);
<RSGraph with 2 vertices and 2 arcs>
gap> Print(tree_1);
Vertices = { 1, 2 }
Arcs = {
	2 = ( origin = 1, terminus = 2, inverse = 3 )
	3 = ( origin = 2, terminus = 1, inverse = 2 )
}
Reverse Map = (2,3)

#
gap> graph_2 := RSGraphByAdjacencyList([[1, 1]], ());;
gap> tree_2 := RSGraphSpanningTree(graph_2);
<RSGraph with 1 vertex and 0 arcs>

#
gap> graph_3 := RSGraphByAdjacencyList([], ());;
gap> tree_3 := RSGraphSpanningTree(graph_3);
<RSGraph with 0 vertices and 0 arcs>

# Graph with star in the middle.
gap> graph_4 := RSGraphByAdjacencyList([[1, 2], [2, 1], [1, 3], [3, 1], [1, 4], [4, 1], [2, 3], [3, 2], [3, 4], [4, 3]], (1,2)(3,4)(5,6)(7,8)(9,10));;
gap> tree_4_bfs := RSGraphSpanningTree(graph_4);;
gap> tree_4_dfs := RSGraphSpanningTree(graph_4, "dfs");;
gap> Print(tree_4_bfs);
Vertices = { 1, 2, 3, 4 }
Arcs = {
	1 = ( origin = 1, terminus = 2, inverse = 2 )
	2 = ( origin = 2, terminus = 1, inverse = 1 )
	3 = ( origin = 1, terminus = 3, inverse = 4 )
	4 = ( origin = 3, terminus = 1, inverse = 3 )
	5 = ( origin = 1, terminus = 4, inverse = 6 )
	6 = ( origin = 4, terminus = 1, inverse = 5 )
}
Reverse Map = (1,2)(3,4)(5,6)(7,8)(9,10)
gap> Print(tree_4_dfs);
Vertices = { 1, 2, 3, 4 }
Arcs = {
	1 = ( origin = 1, terminus = 2, inverse = 2 )
	2 = ( origin = 2, terminus = 1, inverse = 1 )
	7 = ( origin = 2, terminus = 3, inverse = 8 )
	8 = ( origin = 3, terminus = 2, inverse = 7 )
	9 = ( origin = 3, terminus = 4, inverse = 10 )
	10 = ( origin = 4, terminus = 3, inverse = 9 )
}
Reverse Map = (1,2)(3,4)(5,6)(7,8)(9,10)

#
gap> STOP_TEST("spanning_tree.tst");
