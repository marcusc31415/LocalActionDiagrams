# Testing the group name functionality. 
gap> START_TEST("group_name.tst");

#
gap> graph := RSGraphByAdjacencyMatrix([[1]], ());;
gap> lad := LocalActionDiagramFromData(graph, [Group((1,2))], [[1,2]]);
<LocalActionDiagram with 1 vertex and 1 arc>
gap> LocalActionDiagramGroupName(lad); 
""
gap> SetLocalActionDiagramGroupName(lad, "test name");
gap> lad;
<test name (as a Local Action Diagram)>

#
gap> STOP_TEST("group_name.tst");
