# Tests the bipartition function. 
gap> START_TEST("neighbours.tst");

# 
# Fail due to loop.
gap> graph_1 := RSGraphByAdjacencyList([[1, 2], [2, 1], [1, 1]], (1,2));;
gap> RSGraphBipartition(graph_1);
fail

#
# Fail due to parallel arcs. 
gap> graph_2 := RSGraphByAdjacencyList([[1, 2], [1, 2], [2, 1], [2, 1]], (1,3)(2,4));;
gap> RSGraphBipartition(graph_2);
fail

#
# Succeed.  
gap> graph_3 := RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 3], [3, 2]], (1,2)(3,4));;
gap> RSGraphBipartition(graph_3);
[ [ 1, 3 ], [ 2 ] ]

#
# Fail due to no vertices. 
gap> graph_4 := RSGraphByAdjacencyList([], ());;
gap> RSGraphBipartition(graph_4);
fail

#
# Fail due to only one vertex. 
gap> graph_5 := RSGraphByAdjacencyList([[1, 1]], ());;
gap> RSGraphBipartition(graph_5);
fail

#
gap> STOP_TEST("neighbours.tst", 1);
