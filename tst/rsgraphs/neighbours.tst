# Tests the adjacency matrix construction. 
gap> START_TEST("neighbours.tst");

# 
gap> graph_1 := RSGraphByAdjacencyList([[1, 2], [2, 1], [1, 1]], (1,2));;
gap> RSGraphOutNeighbours(graph_1);
rec( 1 := [ 1, 2 ], 2 := [ 1 ] )
gap> RSGraphOutArcs(graph_1);
rec( 1 := [ 1, 3 ], 2 := [ 2 ] )
gap> RSGraphInNeighbours(graph_1);
rec( 1 := [ 1, 2 ], 2 := [ 1 ] )
gap> RSGraphInArcs(graph_1);
rec( 1 := [ 2, 3 ], 2 := [ 1 ] )
gap> graph_2 := RSGraphByAdjacencyList([], ());;
gap> RSGraphOutNeighbours(graph_2);
rec(  )
gap> RSGraphOutArcs(graph_2);
rec(  )
gap> RSGraphInNeighbours(graph_2);
rec(  )
gap> RSGraphInArcs(graph_2);
rec(  )

#
gap> STOP_TEST("neighbours.tst", 1);
