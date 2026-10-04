# Tests the cycle function. 
gap> START_TEST("cycle.tst");

#
gap> graph_1 := RSGraphByAdjacencyList([[1, 1]], ());;
gap> graph_2 := RSGraphByAdjacencyList([[1, 1], [1, 1]], ());;
gap> graph_3 := RSGraphByAdjacencyList([[1, 1], [1, 1]], (1,2));;
gap> graph_4 := RSGraphByAdjacencyList([[1, 2], [2, 1]], (1,2));;
gap> graph_5 := RSGraphByAdjacencyList([[1, 2], [1, 2], [2, 1], [2, 1]], (1,3)(2,4));;
gap> graph_6 := RSGraphByAdjacencyList([[1, 2], [1, 2], [2, 1], [2, 1], [2, 2]], (1,3)(2,4));;
gap> graph_7 := RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 3], [3, 2], [3, 1], [1, 3]], (1,2)(3,4)(5,6));;
gap> graph_8 := RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 3], [3, 2], [3, 2], [2, 3]], (1,2)(3,4)(5,6));;

# 
gap> RSGraphIsCycle(graph_1);
false

#
gap> RSGraphIsCycle(graph_2);
false

#
gap> RSGraphIsCycle(graph_3);
true

#
gap> RSGraphIsCycle(graph_4);
false

#
gap> RSGraphIsCycle(graph_5);
true

#
gap> RSGraphIsCycle(graph_6);
false

#
gap> RSGraphIsCycle(graph_7);
true

#
gap> RSGraphIsCycle(graph_8);
false

#
gap> STOP_TEST("cycle.tst", 1);
