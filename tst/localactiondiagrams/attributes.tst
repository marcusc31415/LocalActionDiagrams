# Testing attributes. 
gap> START_TEST("attributes.tst");

#
# Regular tree. 
gap> graph_1 := RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 2]], (1,2));;
gap> lad_1 := LocalActionDiagramFromData(graph_1, [Group((1,2,3,4)), Group((1,2)(3,4))], [[1,2,3,4], [1,2], [3,4]]);;
gap> LocalActionDiagramRegularTree(lad_1);
4
gap> lad_1 := LocalActionDiagramFromData(graph_1, [Group((1,2,3,4,5)), Group((1,2)(3,4))], [[1,2,3,4,5], [1,2], [3,4]]);;
gap> LocalActionDiagramRegularTree(lad_1);
fail

#
gap> STOP_TEST("attributes.tst");
