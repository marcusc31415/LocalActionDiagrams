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
# Scopos. (A diagram with non-trivial scopo is tested in the manual). 
# More scopos are implicitly tested in the group type tests. 
gap> graph_2 := RSGraphByAdjacencyList([[1, 1]], ());;
gap> lad_2 := LocalActionDiagramFromData(graph_2, [Group((1,2,3))], [[1,2,3]]);;
gap> LocalActionDiagramScopos(lad_2);
[ [  ] ]
gap> graph_3 := RSGraphByAdjacencyMatrix([[10]], ());;
gap> g_3 := Group(());;
gap> SetPermGroupDomain(g_3, [1..10]);;
gap> lad_3 := LocalActionDiagramFromData(graph_3, [g_3], List([1..10], x -> [x]));;
gap> LocalActionDiagramScopos(lad_3);
[ [  ] ]

#
gap> STOP_TEST("attributes.tst");
