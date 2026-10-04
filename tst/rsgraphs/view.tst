# Testing what is printed when the View function is used. Special cases for 1 vertex and 1 arc graphs. 
gap> START_TEST("short_view.tst");

#
gap> graph_1 := RSGraphByAdjacencyList([], ());
<RSGraph with 0 vertices and 0 arcs>
gap> graph_2 := RSGraphByAdjacencyList([[1, 1]], ());
<RSGraph with 1 vertex and 1 arc>
gap> graph_3 := RSGraphByAdjacencyList([[1, 1], [1, 1]], ());
<RSGraph with 1 vertex and 2 arcs>
gap> graph_4 := RSGraphByAdjacencyList([[1, 2], [2, 1]], (1,2));
<RSGraph with 2 vertices and 2 arcs>

#
gap> STOP_TEST("short_view.tst");
