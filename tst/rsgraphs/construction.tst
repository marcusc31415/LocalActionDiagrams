# Testing all possible errors leading to an invalid RSGraph construction. These are errors due to not meeting the definition of an RSGraph not errors in a GAP program. 
gap> START_TEST("construction.tst");

#
# Error if reverse map doesn't work.
gap> graph := RSGraphByAdjacencyList([[1, 2], [2, 1], [1, 1]], (1,3));
Error, Reversal mapping must send the terminal vertex of an arc to the origin \
vertex of the arc.

# Error if graph is not connected. 
gap> graph := RSGraphByAdjacencyList([[1, 1], [2, 2]], ());
Error, Graph must be connected.

# Error if reverse map is not an involution. 
gap> graph := RSGraphByAdjacencyList([[1, 1], [2, 2]], (1,2,3));
Error, Reverse map must be an involution.

# Error if vertex IDs are not integers.
gap> graph := RSGraphByAdjacencyList([["a", "a"]], (), ["a"]);
Error, Vertex IDs must be integers.

# Error if reverse map is not valid.
gap> graph := RSGraphByAdjacencyList([[1, 2], [2, 1], [1, 3], [3, 1]], (1,3)(2,4));
Error, Reversal mapping must send the terminal vertex of an arc to the origin \
vertex of the arc.

# Error if arcs use ids that are not vertex ids. 
gap> graph := RSGraphByAdjacencyList([[2, 2]], (), [1]);
Error, Arc 1 origin and terminus vertices do not match vertex ids.

#
gap> STOP_TEST("construction.tst");
