# Testing attributes. 
gap> START_TEST("group_type.tst");

#
# Fixed vertex. 
gap> graph_4 := RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 3], [3, 2], [2, 4], [4, 2]], (1,2)(3,4)(5,6));;
gap> g := Group(());;
gap> SetPermGroupDomain(g, [1,2, 3]);;
gap> g_2 := Group(());;
gap> SetPermGroupDomain(g_2, [1]);;
gap> lad_4 := LocalActionDiagramFromData(graph_4, [SymmetricGroup(10), g, g_2, g_2], \
>             [[1..10], [1], [2], [1], [3], [1]]);;
gap> LocalActionDiagramGroupType(lad_4);
"Fixed Vertex"

# Inversion. 
gap> graph_5 := RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 3], [3, 2], [2, 4], [4, 2], [1, 1]], (1,2)(3,4)(5,6));;
gap> g := Group(());;
gap> SetPermGroupDomain(g, [1,2, 3]);;
gap> g_2 := Group(());;
gap> SetPermGroupDomain(g_2, [1]);;
gap> g_3 := SymmetricGroup(10);;
gap> SetPermGroupDomain(g_3, [1..11]);;
gap> lad_5 := LocalActionDiagramFromData(graph_5, [g_3, g, g_2, g_2], \
>             [[1..10], [1], [2], [1], [3], [1], [11]]);;
gap> LocalActionDiagramGroupType(lad_5);
"Edge Inversion"

# Lineal. 
gap> graph_6 := RSGraphByAdjacencyList([[1, 2], [2, 1], [1, 2], [2, 1]], (1,2)(3,4));;
gap> g := Group(());;
gap> SetPermGroupDomain(g, [1,2]);;
gap> lad_6 := LocalActionDiagramFromData(graph_6, [g, g], [[1], [1], [2], [2]]);;
gap> LocalActionDiagramGroupType(lad_6);
"Lineal"

# Focal.
gap> graph_7 := RSGraphByAdjacencyList([[1, 1], [1, 1]], (1,2));;
gap> g := Group((1,2), (1,2,3));;
gap> SetPermGroupDomain(g, [1, 2, 3, 4]);;
gap> lad_7 := LocalActionDiagramFromData(graph_7, [g], [[1,2,3], [4]]);;
gap> LocalActionDiagramGroupType(lad_7);
"Focal"

# Focal (non edge case cycle). 
gap> graph_8 := RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 3], [3, 2], [3, 1], [1, 3]], (1,2)(3,4)(5,6));;
gap> g := Group((1,2), (1,2,3));;
gap> SetPermGroupDomain(g, [1, 2, 3, 4]);;
gap> lad_8 := LocalActionDiagramFromData(graph_8, [g, g, g], [[1,2,3], [4], [1,2,3], [4], [1,2,3], [4]]);;
gap> LocalActionDiagramGroupType(lad_8);
"Focal"

# General. 
gap> g_2 := Group((1,2), (3,4));;
gap> lad_9 := LocalActionDiagramFromData(graph_8, [g, g, g_2], [[1,2,3], [4], [1,2,3], [3,4], [1,2], [4]]);;
gap> LocalActionDiagramGroupType(lad_9);
"General"
gap> lad_10 := LocalActionDiagramFromBurgerMozesUniversalGroup(Group((1,2), (3,4,5), (6,7,8), (9,10,11,12,13)));;
gap> LocalActionDiagramGroupType(lad_10);
"General"

#
gap> STOP_TEST("group_type.tst");
