#! @Chapter Extensions
#! @Section Digraphs Conversion

#! @Returns A record containing the digraph and all data needed to reconstruct the
#! RSGraph.
#! @Arguments rsgraph
#! @Label
#! @Description Given an RSGraph this function converts is to a digraph from the
#! <Package>Digraphs</Package> package. A record is returned containing both the
#! digraph and all data needed to reconstruct the original RSGraph. The record
#! returned contains the elements:
#! - <C>digraph</C>: a digraph representing the graph structure (an object in the category <C>IsDigraph</C>.)
#! - <C>reverse_map</C>: the reverse map of the original RSGraph.
#! - <C>vertex_id_map</C>: a bijective map from the vertices of the original RSGraph to the vertices of the digraph which determines the vertex correspondence between the RSGraph and digraph. 
#! - <C>arc_id_map</C>: a bijective map from the arcs of the original RSGraph to the edges of the digraph which determines the arc correspondence between the RSGraph and digraph. 
#!
#! As an example, if <C>2</C> is a vertex in the RSGraph then <C>2^vertex_id_map</C> is the corresponding vertex in the digraph. All digraphs returned by this function have vertices labelled from <M>1</M> to <M>n</M> where <M>n</M> is the number of vertices in the RSGraph. Arc <C>i</C> in the digraph is the arc in the <C>i</C>th position of <C>DigraphEdges(digraph)</C> --- i.e. <C>DigraphEdges(digraph)[i]</C>. 
DeclareAttribute("RSGraphToDigraph", IsRSGraph);

#! @BeginLogSession
#! gap> graph := RSGraphByAdjacencyList([[2, 3], [3, 2]], (1,2), [2, 3]);;
#! gap> digraph_rec := RSGraphToDigraph(graph);;
#! gap> digraph_rec.digraph;
#! <immutable digraph with 2 vertices, 2 edges>
#! gap> 2^digraph_rec.vertex_id_map;
#! 1
#! @EndLogSession

#! @Returns An RSGraph corresponding to the data in the input record.
#! @Arguments digraph_rec
#! @Label
#! @Description Given the output from the function <Ref Func="RSGraphToDigraph"/> (<A>digraph_rec</A>) this function returns the corresponding RSGraph. This returns an RSGraph that is exactly the same as the original RSGraph. If the data input to this function is not fro the <C>RSGraphToDigraph</C> function then there is no guarantee that this function will run correctly. 
DeclareOperation("RSGraphFromDigraph", [IsRecord]);

#! @BeginLogSession
#! gap> graph := RSGraphByAdjacencyList([[2, 3], [3, 2]], (1,2), [2, 3]);;
#! gap> digraph_rec := RSGraphToDigraph(graph);;
#! gap> new_graph := RSGraphFromDigraph(digraph_rec);;
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


#! @Returns An RSGraph.
#! @Arguments digraph, reverse_map
#! @Label
#! @Description Given a <A>digraph</A> and a <A>reverse_map</A> defined on the edges
#! of the digraph this function returns an RSGraph with the same structure as the 
#! digraph and with the reverse map provided. This function works by using the 
#! adjacency listing of the digraph given by <C>DigraphEdges(<A>digraph</A>)</C>. This
#! means that the reverse map must be compatible with the order of the edges given
#! by this adjacency listing. 
#!
#! All normal checks for an RSGraph construction are applied when using this function.
#! In particular, this means that the digraph must be connected and must have a reverse
#! arc for every arc in the digraph. 
DeclareOperation("RSGraphFromDigraph", [IsDigraph, IsPerm]);

#! @BeginLogSession
#! gap> digraph := DigraphByEdges([[1, 1], [1, 2], [2, 1]]);;
#! gap> graph := RSGraphFromDigraph(digraph, (2, 3));;
#! gap> Print(graph);
#! Vertices = { 1, 2 }
#! Arcs = {
#! 	1 = ( origin = 1, terminus = 1, inverse = 1 )
#! 	2 = ( origin = 1, terminus = 2, inverse = 3 )
#! 	3 = ( origin = 2, terminus = 1, inverse = 2 )
#! }
#! Reverse Map = (2,3)
#! @EndLogSession

