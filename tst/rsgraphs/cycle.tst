# Tests the bipartition function. 
gap> START_TEST("cycle.tst");

#
gap> graph_list := [RSGraphByAdjacencyList([[1, 1]], ())];;
gap> Add(graph_list, RSGraphByAdjacencyList([[1, 1], [1, 1]], ()));;
gap> Add(graph_list, RSGraphByAdjacencyList([[1, 1], [1, 1]], (1,2)));;
gap> Add(graph_list, RSGraphByAdjacencyList([[1, 2], [2, 1]], (1,2)));;
gap> Add(graph_list, RSGraphByAdjacencyList([[1, 2], [1, 2], [2, 1], [2, 1]], (1,3)(2,4)));;
gap> Add(graph_list, RSGraphByAdjacencyList([[1, 2], [1, 2], [2, 1], [2, 1], [2, 2]], (1,3)(2,4)));;
gap> Add(graph_list, RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 3], [3, 2], [3, 1], [1, 3]], (1,2)(3,4)(5,6)));;
gap> Add(graph_list, RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 3], [3, 2], [3, 2], [2, 3]], (1,2)(3,4)(5,6)));;

#
gap> output_list := [false, false, true, false, true, false, true, false];;

#
gap> for idx in [1..Size(graph_list)] do Print(RSGraphIsCycle(graph_list[idx]) = output_list[idx], "\n"); od;
true
true
true
true
true
true
true
true

#
gap> STOP_TEST("cycle.tst", 1);
