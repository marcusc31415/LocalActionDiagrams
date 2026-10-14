# Testing special cases (burger Mozes, bipartition preserving, etc.)
gap> START_TEST("special_cases.tst");

#
# Burger Mozes group.
gap> LocalActionDiagramFromBurgerMozesUniversalGroup(Group((1,2,3)));
<U(Group( [ (1,2,3) ] )) (as a Local Action Diagram)>
gap> g := Group((1,2));;
gap> SetPermGroupDomain(g, [1, 2, 3]);;
gap> LocalActionDiagramFromBurgerMozesUniversalGroup(g);
<U(Group( [ (1,2) ] )) (as a Local Action Diagram)>

#
# Bipartition preserving. 
gap> graph := RSGraphByAdjacencyMatrix([[2]], (1,2));;
gap> lad := LocalActionDiagramFromData(graph, [Group((1,2)(3,4))], [[1,2], [3,4]]);;
gap> lad_2 := LocalActionDiagramConstructBipartitionPreserving(lad);
<LocalActionDiagram with 2 vertices and 4 arcs>
gap> Print(lad_2);
Vertices = { 1, 2 }
Arcs = {
	1 = ( origin = 1, terminus = 2, inverse = 3 )
	2 = ( origin = 1, terminus = 2, inverse = 4 )
	3 = ( origin = 2, terminus = 1, inverse = 1 )
	4 = ( origin = 2, terminus = 1, inverse = 2 )
}
Reverse Map = (1,3)(2,4)
Vertex Labels = {
	1 = Group( [ (1,2)(3,4) ] )
	2 = Group( [ (1,2)(3,4) ] )
}
Arc Labels = {
	1 = [ 1, 2 ]
	2 = [ 3, 4 ]
	3 = [ 3, 4 ]
	4 = [ 1, 2 ]
}

#
# Identifying Burger Mozes
gap> graph_3 := RSGraphByAdjacencyMatrix([[2]], ());;
gap> lad_3 := LocalActionDiagramFromData(graph_3, [Group((1,2)(3,4))], [[1,2], [3,4]]);
<LocalActionDiagram with 1 vertex and 2 arcs>
gap> LocalActionDiagramIsBurgerMozesUniversalGroup(lad_3);
true
gap> lad_3;
<U(Group( [ (1,2)(3,4) ] )) (as a Local Action Diagram)>
gap> LocalActionDiagramIsBurgerMozesUniversalGroup(lad);
false

#
# Bipartition preserving name is correct. 
gap> LocalActionDiagramConstructBipartitionPreserving(lad_3);
<(U(Group( [ (1,2)(3,4) ] )))^o (as a Local Action Diagram)>
gap> Print(last);
Vertices = { 1, 2 }
Arcs = {
	1 = ( origin = 1, terminus = 2, inverse = 3 )
	2 = ( origin = 1, terminus = 2, inverse = 4 )
	3 = ( origin = 2, terminus = 1, inverse = 1 )
	4 = ( origin = 2, terminus = 1, inverse = 2 )
}
Reverse Map = (1,3)(2,4)
Vertex Labels = {
	1 = Group( [ (1,2)(3,4) ] )
	2 = Group( [ (1,2)(3,4) ] )
}
Arc Labels = {
	1 = [ 1, 2 ]
	2 = [ 3, 4 ]
	3 = [ 1, 2 ]
	4 = [ 3, 4 ]
}

#
gap> STOP_TEST("special_cases.tst");
