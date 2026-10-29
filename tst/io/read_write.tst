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
# Write local action diagrams.
gap> lad_1 := LocalActionDiagramFromData(RSGraphByAdjacencyList([[1, 1]], ()), [Group((1,2,3))], [[1,2,3]]);;
gap> string_1 := LocalActionDiagramToWritableString(lad_1);
"1|1,1,1|P/1:P2,3,1:1,2,3|1:1,2,3"
gap> lad_2 := LocalActionDiagramFromData(RSGraphByAdjacencyList([[1, 2], [2, 1]], (1,2)), [Group((1,2,3)), SymmetricGroup(6)], [[1,2,3], [1..6]]);;
gap> string_2 := LocalActionDiagramToWritableString(lad_2);
"1,2|1,1,2,2,2,1|P2,1/1:P2,3,1:1,2,3;2:P2,3,4,5,6,1_P2,1:1,2,3,4,5,6|1:1,2,3;2\
:1,2,3,4,5,6"
gap> g := Group(());;
gap> SetPermGroupDomain(g, [67]);;
gap> lad_3 := LocalActionDiagramFromBurgerMozesUniversalGroup(g);;
gap> string_3 := LocalActionDiagramToWritableString(lad_3);
"1|1,1,1|P/1:P:67|1:67|LocalActionDiagramGroupName!U(Group( [ () ] ))"
gap> g := Group(());;
gap> SetPermGroupDomain(g, [4, 2, 1, 3]);;
gap> lad_4 := LocalActionDiagramFromBurgerMozesUniversalGroup(g);;
gap> LocalActionDiagramIsDiscrete(lad_4);;
gap> string_4 := LocalActionDiagramToWritableString(lad_4);
"1|1,1,1,2,1,1,3,1,1,4,1,1|P/1:P:4,2,1,3|1:4;4:3;2:2;3:1|LocalActionDiagramGro\
upName!U(Group( [ () ] ))|LocalActionDiagramGroupType!General|LocalActionDiagr\
amScopos! |LocalActionDiagramIsDiscrete!true|LocalActionDiagramIsUniscalar!tru\
e|LocalActionDiagramIsUnimodular!true"

#
# Read from string
gap> String(lad_1) = String(LocalActionDiagramFromWritableString(string_1));
true
gap> lad_2_r := LocalActionDiagramFromWritableString(string_2);;
gap> String(LocalActionDiagramRSGraph(lad_2_r)) = String(LocalActionDiagramRSGraph(lad_2));
true
gap> LocalActionDiagramVertexLabels(lad_2_r).1 = LocalActionDiagramVertexLabels(lad_2).1;
true
gap> LocalActionDiagramVertexLabels(lad_2_r).2 = LocalActionDiagramVertexLabels(lad_2).2;
true
gap> LocalActionDiagramArcLabels(lad_2_r).1 = LocalActionDiagramArcLabels(lad_2).1;
true
gap> LocalActionDiagramArcLabels(lad_2_r).2 = LocalActionDiagramArcLabels(lad_2).2;
true

#
gap> lad_3_r := LocalActionDiagramFromWritableString(string_3);;
gap> String(lad_3) = String(lad_3_r);
true
gap> lad_4_r := LocalActionDiagramFromWritableString(string_4);;
gap> String(lad_4) = String(lad_4_r);
true
gap> KnownAttributesOfObject(lad_3) = KnownAttributesOfObject(lad_3_r);
true
gap> KnownPropertiesOfObject(lad_3) = KnownPropertiesOfObject(lad_3_r);
true
gap> "LocalActionDiagramGroupType" in KnownAttributesOfObject(lad_4_r);
true
gap> "LocalActionDiagramScopos" in KnownAttributesOfObject(lad_4_r);
true
gap> "LocalActionDiagramGroupName" in KnownAttributesOfObject(lad_4_r);
true
gap> KnownPropertiesOfObject(lad_4) = KnownPropertiesOfObject(lad_4_r);
true

#
gap> STOP_TEST("read_write.tst");
