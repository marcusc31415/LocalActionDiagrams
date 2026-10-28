# Testing all possible errors leading to an invalid local action diagram construction. 
gap> START_TEST("read_write.tst");

#
# Write to string (various edges cases included).
gap> graph_1 := RSGraphByAdjacencyList([], ());;
gap> string_1 := RSGraphToWritableString(graph_1);
"||P"
gap> graph_2 := RSGraphByAdjacencyMatrix([[0]], ());;
gap> string_2 := RSGraphToWritableString(graph_2);
"1||P"
gap> graph_3 := RSGraphByAdjacencyMatrix([[3]], (1,3));;
gap> string_3 := RSGraphToWritableString(graph_3);
"1|1,1,1,2,1,1,3,1,1|P3,2,1"
gap> graph_4 := RSGraphByAdjacencyMatrix([[3]], ());;
gap> string_4 := RSGraphToWritableString(graph_4);
"1|1,1,1,2,1,1,3,1,1|P"
gap> graph_5 := RSGraphByAdjacencyList([[1, 3], [3, 1]], (1, 2), [1, 3]);;
gap> string_5 := RSGraphToWritableString(graph_5);
"1,3|1,1,3,2,3,1|P2,1"
gap> graph_6 := RSGraphByAdjacencyList([[1, 1], [1, 3], [3, 1]], (2, 3), [1, 3]);;
gap> string_6 := RSGraphToWritableString(graph_6);
"1,3|1,1,1,2,1,3,3,3,1|P1,3,2"

#
# Read from string
gap> String(graph_1) = String(RSGraphFromWritableString(string_1));
true
gap> String(graph_2) = String(RSGraphFromWritableString(string_2));
true
gap> String(graph_3) = String(RSGraphFromWritableString(string_3));
true
gap> String(graph_4) = String(RSGraphFromWritableString(string_4));
true
gap> String(graph_5) = String(RSGraphFromWritableString(string_5));
true
gap> String(graph_6) = String(RSGraphFromWritableString(string_6));
true

#
gap> STOP_TEST("read_write.tst");
