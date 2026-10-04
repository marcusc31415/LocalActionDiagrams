# Can't install the functions as methods because *IsGraph* is not a category. This file is blank apart from the documentation. 


#! @Chapter Extensions
#! @Section GRAPE Conversion
#! 
#! For the sake of clarity, if an object in the following section is a graph from the <Package>GRAPE</Package> package then it is referred to as a GRAPE graph. 
#! <ManSection>
#!     <Oper Name="RSGraphToGRAPE" Arg="rsgraph"/>
#!     <Returns>A record containing the GRAPE graph and all data needed to reconstruct the RSGraph.</Returns>
#!     <Description>
#!			Given an RSGraph this function converts is to a GRAPE graph. A record is returned containing both
#!			the GRAPE graph and all data needed to reconstruct the original RSGraph. The
#!			record returned contains the elements:
#!			 - <C>graph</C>: a GRAPE graph representing the graph structure (an object for which <C>IsGraph</C> returns true.)
#!			 - <C>reverse_map</C>: the reverse map of the original RSGraph.
#!			 - <C>vertex_id_map</C>: a bijective map from the vertices of the original RSGraph to the vertices of the GRAPE graph which determines the vertex correspondence between the RSGraph and GRAPE graph. 
#!			 - <C>arc_id_map</C>: a bijective map from the arcs of the original RSGraph to the edges of the GRAPE graph which determines the arc correspondence between the RSGraph and GRAPE graph. 
#!			
#!			As an example, if <C>2</C> is a vertex in the RSGraph then <C>2^vertex_id_map</C> is the corresponding vertex in the GRAPE graph. All GRAPE graph returned by this function have vertices labelled from <M>1</M> to <M>n</M> where <M>n</M> is the number of vertices in the RSGraph. Arc <C>i</C> in the digraph is the arc in the <C>i</C>th position of <C>DirectedEdges(graph)</C> --- i.e. <C>DirectedEdges(graph)[i]</C>. 
#! 		</Description>
#! </ManSection>
#!
#! @BeginLogSession
#! gap> graph := RSGraphByAdjacencyList([[2, 3], [3, 2]], (1,2), [2, 3]);;
#! gap> graph_rec := RSGraphToGRAPE(graph);;
#! gap> graph_rec.graph;;
#! gap> 2^graph_rec.vertex_id_map;
#! 1
#! @EndLogSession

#! <ManSection>
#!     <Oper Name="RSGraphFromGRAPE" Arg="graph_rec"/>
#!     <Returns>An RSGraph corresponding to the data in <A>graph_rec</A>.</Returns>
#!     <Description>
#!         Given the output from the function <Ref Func="RSGraphToGRAPE"/> (<A>graph_rec</A>) this function returns the corresponding RSGraph. This returns an RSGraph that is exactly the same as the original RSGraph. If the data input to this function is not fro the <C>RSGraphToGRAPE</C> function then there is no guarantee that this function will run correctly. 
#!     </Description>
#! </ManSection>
#!
#! @BeginLogSession
#! gap> graph := RSGraphByAdjacencyList([[2, 3], [3, 2]], (1,2), [2, 3]);;
#! gap> graph_rec := RSGraphToGRAPE(graph);;
#! gap> new_graph := RSGraphFromGRAPE(graph_rec);;
#! gap> Print(graph);
#! Vertices = { 2, 3 }
#! Arcs = {
#! 	1 = ( origin = 2, terminus = 3, inverse = 2 )
#! 	2 = ( origin = 3, terminus = 2, inverse = 1 )
#! }
#! Reverse Map = (1,2)
#! gap> Print(new_graph);
#! Vertices = { 2, 3 }
#! Arcs = {
#! 	1 = ( origin = 2, terminus = 3, inverse = 2 )
#! 	2 = ( origin = 3, terminus = 2, inverse = 1 )
#! }
#! Reverse Map = (1,2)
#! @EndLogSession
#!

#! <ManSection>
#!     <Oper Name="RSGraphFromGRAPE" Arg="graph, reverse_map"/>
#!     <Returns>An RSGraph.</Returns>
#!     <Description>
#!         Given a GRAPE <A>graph</A> and a <A>reverse_map</A> defined on the
#!         edges of the GRAPE graph this function returns an RSGraph with the
#!         same structure as the GRAPE graph and with the reverse map provided.
#!         This function works by using the adjacency listing of the GRAPE
#!         graph given by <C>DirectedEdges(<A>graph</A>)</C>. This means that
#!         the reverse map must be compatible with the order of the edges given
#!         by this adjacency listing. 
#!
#!         All normal checks for an RSGraph construction are applied when using
#!         this function.  In particular, this means that the GRAPE graph must
#!         be connected and must have a reverse arc for every arc in the GRAPE
#!         graph. 
#!     </Description>
#! </ManSection>


#! @BeginLogSession
#! gap> adj_mat := [[0, 1], [1, 0]];
#! gap> grape_graph := Graph(Group(()), [1, 2], OnPoints, \
#! >                         {x, y} -> adj_mat[x][y] = 1, true);;
#! gap> graph := RSGraphFromGRAPE(grape_graph, (1, 2));;
#! gap> Print(graph);
#! Vertices = { 1, 2 }
#! Arcs = {
#! 	1 = ( origin = 1, terminus = 2, inverse = 2 )
#! 	2 = ( origin = 2, terminus = 1, inverse = 1 )
#! }
#! Reverse Map = (1,2)
#! @EndLogSession
