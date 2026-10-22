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
# Checking discrete, uniscalar, and unimodular properties. 
# Also checking that they are set correctly when others are known
# (e.g. true discrete means the other two are also true). 
# Discrete. 
gap> graph_3 := RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 3], [3, 2], [3, 4], [4, 3]], (1,2)(3,4)(5,6));;
gap> g := Group(());;
gap> SetPermGroupDomain(g, [1,2]);;
gap> g_2 := Group(());;
gap> SetPermGroupDomain(g_2, [1]);
gap> lad_3 := LocalActionDiagramFromData(graph_3, [Group((1,2)), Group((1,2)(3,4)), g, g_2], \
>             [[1,2], [1,2], [3,4], [1], [2], [1]]);;
gap> LocalActionDiagramIsDiscrete(lad_3);
true
gap> "LocalActionDiagramIsUniscalar" in KnownPropertiesOfObject(lad_3);
true
gap> "LocalActionDiagramIsUnimodular" in KnownPropertiesOfObject(lad_3);
true

# 
# Uniscalar. 
gap> g := Group((1,2));;
gap> SetPermGroupDomain(g, [1,2,3]);;
gap> lad_3 := LocalActionDiagramFromData(graph_3, [Group((1,2)), Group((1,2)(3,4)), g, g_2], \
>             [[1,2], [1,2], [3,4], [1,2], [3], [1]]);;
gap> LocalActionDiagramIsDiscrete(lad_3);
false
gap> "LocalActionDiagramIsUniscalar" in KnownPropertiesOfObject(lad_3);
false
gap> "LocalActionDiagramIsUnimodular" in KnownPropertiesOfObject(lad_3);
false
gap> LocalActionDiagramIsUniscalar(lad_3);
true
gap> "LocalActionDiagramIsUnimodular" in KnownPropertiesOfObject(lad_3);
true

#
# Unimodular
gap> graph_4 := RSGraphByAdjacencyList([[1, 2], [2, 1], [2, 3], [3, 2], [1, 3], [3, 1], [1, 4], [4, 1]], \
>                                        (1,2)(3,4)(5,6)(7,8));;
gap> lad_4 := LocalActionDiagramFromData(graph_4, [Group((1,2)(3,4,5), (6,7)), Group((1,2), (3,4,5)), Group((3,5,6), (1,2,7)), Group((1,2))], \
>             [[1, 2], [1, 2], [3, 4, 5], [1, 2, 7], [3,4,5], [3,5,6], [6,7], [1,2]]);;
gap> LocalActionDiagramIsDiscrete(lad_4);
false
gap> "LocalActionDiagramIsUniscalar" in KnownPropertiesOfObject(lad_4);
false
gap> "LocalActionDiagramIsUnimodular" in KnownPropertiesOfObject(lad_4);
false
gap> LocalActionDiagramIsUniscalar(lad_4);
false
gap> "LocalActionDiagramIsUnimodular" in KnownPropertiesOfObject(lad_4);
false
gap> LocalActionDiagramIsUnimodular(lad_4);
true

# 
# Unimodular (length 2 cycle)
gap> graph_5 := RSGraphByAdjacencyList([[1, 2], [2, 1], [1, 2], [2, 1]], (1,2)(3,4));;
gap> lad_5 := LocalActionDiagramFromData(graph_5, [Group((1,2), (3,4,5)), Group((1,2)(3,4,5))], \
>             [[1,2], [1,2], [3,4,5], [3,4,5]]);;
gap> LocalActionDiagramIsUnimodular(lad_5);
true

# 
# Unimodular (length 1 cycle)
gap> graph_6 := RSGraphByAdjacencyList([[1, 1], [1, 1]], (1,2));;
gap> lad_6 := LocalActionDiagramFromData(graph_6, [Group((1,2,3,4,5), (6,7,8,9,10))], \
>             [[1,2,3,4,5], [6,7,8,9,10]]);;
gap> LocalActionDiagramIsUnimodular(lad_6);
true

# 
# None of them
gap> graph_7 := RSGraphByAdjacencyList([[1, 1], [1, 1]], (1,2));;
gap> lad_7 := LocalActionDiagramFromData(graph_6, [Group((1,2,3,4,5), (6,7,8,9))], \
>             [[1,2,3,4,5], [6,7,8,9]]);;
gap> LocalActionDiagramIsUnimodular(lad_7);
false

#
gap> STOP_TEST("attributes.tst");
