# Testing all possible errors leading to an invalid local action diagram construction. 
gap> START_TEST("construction.tst");

#
# Error if arc labels are not orbits of vertex labels. 
gap> graph := RSGraphByAdjacencyList([[1, 2], [2, 1]], (1,2));;
gap> vert_labels := [Group((1,2)), Group((1,2,3))];;
gap> arc_labels := [[1, 2], [1, 2]];;
gap> LocalActionDiagramFromData(graph, vert_labels, arc_labels);
Error, Arc labels must be orbits of the group labels

#
# Check error occurs if the orbit is a subset.  
gap> arc_labels := [[1, 2, 3], [1, 2]];;
gap> LocalActionDiagramFromData(graph, vert_labels, arc_labels);
Error, Arc labels must be orbits of the group labels

#
# Error if more orbits than arcs. 
gap> vert_labels := [Group((1,2)(3,4)), Group((1,2,3))];;
gap> arc_labels := [[1, 2], [1, 2, 3]];;
gap> LocalActionDiagramFromData(graph, vert_labels, arc_labels);
Error, Must have an arc label for each orbit of the vertex labels.

#
# Error if duplicate arc labels at the same vertex. 
gap> graph := RSGraphByAdjacencyList([[1, 1], [1, 2], [2, 1]], (2,3));;
gap> vert_labels := [Group((1,2)), Group((1,2,3))];;
gap> arc_labels := [[1, 2], [1, 2], [1, 2, 3]];;
gap> LocalActionDiagramFromData(graph, vert_labels, arc_labels);
Error, Must have exactly one arc label for each orbit of a vertex label.

#
# Error if number of arc labels does not match the number of arcs. 
gap> arc_labels := [[1, 2], [1, 2]];;
gap> LocalActionDiagramFromData(graph, vert_labels, arc_labels);
Error, Must have an arc label for each arc in the graph.
gap> arc_labels := [[1, 2], [1, 2], [1, 2, 3], [1, 2]];;
gap> LocalActionDiagramFromData(graph, vert_labels, arc_labels);
Error, Must have an arc label for each arc in the graph.

#
# Error if number of vertex labels does not match the number of vertices. 
gap> graph := RSGraphByAdjacencyList([[1, 1], [1, 2], [2, 1]], (2,3));;
gap> vert_labels := [Group((1,2))];;
gap> arc_labels := [[1, 2], [1, 2], [1, 2, 3]];;
gap> LocalActionDiagramFromData(graph, vert_labels, arc_labels);
Error, Must have a vertex label for each vertex in the graph.
gap> vert_labels := [Group((1,2)), Group((1,2,3)), Group((1, 2, 7))];;
gap> LocalActionDiagramFromData(graph, vert_labels, arc_labels);
Error, Must have a vertex label for each vertex in the graph.

# 
# Error if number of vertices or arcs is 0.
gap> graph := RSGraphByAdjacencyList([], ());;
gap> LocalActionDiagramFromData(graph, [], []);
Error, Local action diagrams must have at least one vertex and one arc.
gap> graph := RSGraphByAdjacencyList([], (), [1]);;
gap> LocalActionDiagramFromData(graph, [Group(())], []);
Error, Local action diagrams must have at least one vertex and one arc.

#
# Check that the construction works. 
gap> graph := RSGraphByAdjacencyList([[1, 1], [1, 2], [2, 1]], (2,3));;
gap> vert_labels := [Group((1,2)(3,4)), Group((1,7))];;
gap> arc_labels := [[1, 2], [3, 4], [1, 7]];;
gap> LocalActionDiagramFromData(graph, vert_labels, arc_labels);
<LocalActionDiagram with 2 vertices and 3 arcs>
gap> vert_labels := rec(1 := Group((1,2)(3,4)), 2 := Group((1,7)));;
gap> arc_labels := rec(2 := [1, 2], 1 := [3, 4], 3 := [1, 7]);;
gap> LocalActionDiagramFromData(graph, vert_labels, arc_labels);
<LocalActionDiagram with 2 vertices and 3 arcs>

#
gap> STOP_TEST("construction.tst");
