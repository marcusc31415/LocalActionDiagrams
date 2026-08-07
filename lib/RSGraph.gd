# This is loaded before the extension isomorphism files.
# The library can still be used if the isomorphism functions
# aren't available. If the isomorphism files are loaded
# then they set this variable to true. 
BindGlobal("LAD_IsomorphismAvailable@", false);

#! @Chapter RSGraphs

#! @Section Creating RSGraphs
#!
#! @Returns <K>true</K> if <A>graph</A> is of the category <C>IsRSGraph</C> and
#! <K>false</K> otherwise.
#! @Arguments obj
#! @Label for an object
#! @Description
#! Every RSGraph object belongs to the category <C>IsRSGraph</C>. Furthermore, 
#! every RSGraph is an immutable attribute storing object. 
DeclareCategory("IsRSGraph", IsObject);

DeclareRepresentation("IsRSGraphRep", IsRSGraph and IsAttributeStoringRep);

# Constructor
DeclareConstructor("RSGraphConsNC", [IsRSGraph, IsRecord]);

# User Level Construction Operations

#! @BeginGroup AdjacencyList
#! @Returns An RSGraph. 
#! @Arguments adjacency_list, reverse_map[, vertex_ids]
#! @Label for a list, permutation[, and list]
#! @Description
#! This function creates an RSGraph described by an adjacency list. 
#! The argument <A>adjacnecy_list</A> is a list of the from 
#! <C>[[origin, terminus], ...]</C>. Each arc in the graph is given an
#! id which corresponds to its position in the list <A>adjacency_list</A>. 
#!
#! The reverse map must be a permutation that is an involution 
#! --- i.e. <C>reverse_map*reverse_map = ()</C>.  Furthermore, 
#! if element <A>idx</A> of <A>adjacency_list</A> is <C>[u, v]</C>
#! then <A>adjacency_list[idx^reverse_map]</A> must equal <C>[v, u]</C>. 
#!
#! By default, the vertices are labelled with ids <M>\{1, 2, \dots, n\}</M> where
#! <M>n</M> is the maximal element of <C>Flat(adjacency_list)</C>. If the optional
#! third argument <A>vertex_ids</A> is given then these are taken to be the vertex
#! ids. This argument must be a dense list of integers. 
#!
#! Finally, the underlying graph must be connected. 
DeclareOperation("RSGraphByAdjacencyList", [IsList, IsPerm]);
DeclareOperation("RSGraphByAdjacencyList", [IsList, IsPerm, IsList]);

#! @Arguments adjacency_list, reverse_map[, vertex_ids]
#! @Label for a list, permutation[, and list]
#! @Description
#!
#! The NC variant of the function does not check that the graph is connected,
#! the reverse map is valid, and that each arc has an associated reverse arc. 
DeclareOperation("RSGraphByAdjacencyListNC", [IsList, IsPerm]);
DeclareOperation("RSGraphByAdjacencyListNC", [IsList, IsPerm, IsList]);

#! @BeginExampleSession
#! gap> adj_list := [[1, 2], [2, 1], [2, 2], [2, 2], [1, 1]];;
#! gap> rev_map := (1,2)(3,4);;
#! gap> graph := RSGraphByAdjacencyList(adj_list, rev_map);
#! <RSGraph with 2 vertices and 5 arcs>
#!
#! gap> adj_list2 := [[2, 2]];;
#! gap> rev_map2 := ();;
#! gap> vertex_ids := [2];;
#! gap> graph := RSGraphByAdjacencyList(adj_list, rev_map, vertex_ids);
#! <RSGraph with 1 vertex and 1 arc>
#! @EndExampleSession



#! @EndGroup

#! @BeginGroup AdjacencyMatrix 
#! @Returns An RSGraph. 
#! @Arguments adjacency_matrix, reverse_map[, vertex_ids]
#! @Label for a matrix, permutation[, and list]
#! @Description
#! This function creates an RSGraph described by an adjacency matrix. 
#! The argument <A>adjacnecy_matrix</A> is an <M>n \times n</M> matrix 
#! represented by a list of lists. This means that there are <M>m</M>
#! arcs between vertices <M>u</M> and <M>v</M> of the graph if
#! <C>adjacency_matrix[u][v] = m</C>. 
#!
#! The matrix is read in row-major order to determine the arc ids of the
#! graph. This is important for determining the reverse map of the graph.
#! The reverse map must be a permutation that is an involution 
#! --- i.e. <C>reverse_map*reverse_map = ()</C>.  
#!
#! By default, the vertices are labelled with ids <M>\{1, 2, \dots, n\}</M> where
#! <M>n</M> <C>Size(adjacency_matrix)</C> --- i.e. the number of rows (and columns)
#! of the matrix. If the optional third argument <A>vertex_ids</A> is given then 
#! these are taken to be the vertex ids. This argument must be a dense list of 
#! integers and element <C>idx</C> of the list will be the label of the vertex in
#! row and column <C>idx</C> of <A>adjacency_matrix</A>. 
#!
#! Finally, the underlying graph must be connected. 
#!
#! Note that the first argument must be a list of lists with each sublist containing
#! integers. In particular, it can not be an object in the category <K>IsMatrixObj</K>
#! constructed by the function <C>Matrix</C>. 
DeclareOperation("RSGraphByAdjacencyMatrix", [IsMatrix, IsPerm]);
DeclareOperation("RSGraphByAdjacencyMatrix", [IsMatrix, IsPerm, IsList]);

#! @Returns An RSGraph. 
#! @Arguments adjacency_matrix, reverse_map[, vertex_ids]
#! @Label for a matrix, permutation[, and list]
#! @Description
#!
#! The NC variant of the function does not check that the graph is connected,
#! the reverse map is valid, and that each arc has an associated reverse arc. 
DeclareOperation("RSGraphByAdjacencyMatrixNC", [IsMatrix, IsPerm]);
DeclareOperation("RSGraphByAdjacencyMatrixNC", [IsMatrix, IsPerm, IsList]);


#! @BeginExampleSession
#! gap> adj_mat := [[1, 2], [2, 0]];;
#! gap> rev_map := (2, 4)(3,5);;
#! gap> vertex_ids := [2,3];;
#! gap> graph := RSGraphByAdjacencyMatrix(adj_list, rev_map, vertex_ids);
#! <RSGraph with 2 vertices and 5 arcs>
#!
#! gap> Print(graph);
#! Vertices = { 2, 3 }
#! Arcs = {
#!         1 = ( origin = 2, terminus = 2, inverse = 1 )
#!         2 = ( origin = 2, terminus = 3, inverse = 4 )
#!         3 = ( origin = 2, terminus = 3, inverse = 5 )
#!         4 = ( origin = 3, terminus = 2, inverse = 2 )
#!         5 = ( origin = 3, terminus = 2, inverse = 3 )
#! }
#! Reverse Map = (2,4)(3,5)
#! @EndExampleSession

#! @EndGroup




# Graph Attributes

#! @Section RSGraph Attributes and Properties
#!
#! @Returns The list of vertex ids of <A>graph</A>. 
#! @Arguments graph
#! @Label 
#! @Description
DeclareAttribute("RSGraphVertices", IsRSGraph);


#! @Returns The number of vertices of <A>graph</A>. 
#! @Arguments graph
#! @Label 
#! @Description
DeclareAttribute("RSGraphNumberVertices", IsRSGraph);


#! @Returns The record of arcs of <A>graph</A>. 
#! @Arguments graph
#! @Label 
#! @Description The name of each component of the record is the
#! id of the arc. Each component it itself a record with three 
#! components: origin, terminus, and inverse. These store the 
#! origin vertex, terminus vertex, and inverse arc id respectively. 
#!
#! For iterating over the arcs of an RSGraph see <Ref Func="RSGraphArcIterator"/>. 
DeclareAttribute("RSGraphArcs", IsRSGraph);

#! @BeginExampleSession
#! gap> adj_list := [[1, 2], [2, 1], [2, 2], [2, 2], [1, 1]];;
#! gap> rev_map := (1,2)(3,4);;
#! gap> graph := RSGraphByAdjacencyList(adj_list, rev_map);
#! <RSGraph with 2 vertices and 5 arcs>
#! gap> RSGraphArcs(graph).1;
#! rec( inverse := 2, origin := 1, terminus := 2 )
#! @EndExampleSession


#! @Returns The number of arcs of <A>graph</A>. 
#! @Arguments graph
#! @Label 
#! @Description
DeclareAttribute("RSGraphNumberArcs", IsRSGraph);

#! @Returns The list of arc ids of <A>graph</A>. 
#! @Arguments graph
#! @Label 
#! @Description
DeclareAttribute("RSGraphArcIDs", IsRSGraph);

#! @Returns The reverse map of <A>graph</A>. 
#! @Arguments graph
#! @Label 
#! @Description
DeclareAttribute("RSGraphReverseMap", IsRSGraph);

#! @Returns The adjacency matrix of <A>graph</A>. 
#! @Arguments graph
#! @Label 
#! @Description
DeclareAttribute("RSGraphAdjacencyMatrix", IsRSGraph);

#! @Returns The record of vertex neighbours of <A>graph</A>. 
#! @Arguments graph
#! @Label 
#! @Description The name of each component is a vertex id of the graph.
#! Each element is a list of vertex ids corresponding to the neighbours
#! of the vertex. A vertex id <C>v_id</C> is in list <C>idx</C> if and 
#! only if there is an arc connecting the vertices <C>idx</C> and <C>v_id</C>. 
#! For example, if there is an arc from vertex 1 to 3 of the graph 
#! then 3 is an element of <C>RSGraphOutNeighbours(graph).1</C>. 
DeclareAttribute("RSGraphOutNeighbours", IsRSGraph);

#! @Returns The record of vertex neighbours of <A>graph</A>. 
#! @Arguments graph
#! @Label 
#! @Description Since every arc has an inverse arc this returns the same
#! result as <C>RSGraphOutNeighbours(graph)</C>. 
DeclareAttribute("RSGraphInNeighbours", IsRSGraph);

#! @Returns The record of arcs originating at each vertex. 
#! @Arguments graph
#! @Label 
#! @Description The name of each component is a vertex id of the graph. 
#! Each element is a list of arc ids. An arc id is in list <C>idx</C>
#! if and only if the origin of that arc is <C>idx</C>. For example, 
#! if an arc with id <C>2</C> originates at vertex 3 then <C>2</C> is
#! an element of <C>RSGraphInNeighbours(graph).3</C>. 
DeclareAttribute("RSGraphOutArcs", IsRSGraph);

#! @Returns The record of arcs originating at each vertex. 
#! @Arguments graph
#! @Label 
#! @Description The name of each component is a vertex id of the graph. 
#! Each element is a list of arc ids. An arc id is in list <C>idx</C>
#! if and only if the terminus of that arc is <C>idx</C>. For example, 
#! if an arc with id <C>2</C> terminates at vertex 1 then <C>2</C> is 
#! an element of <C>RSGraphInNeighbours(graph).1</C>. 
DeclareAttribute("RSGraphInArcs", IsRSGraph);

#! @BeginExampleSession
#! gap> adj_list := [[1, 2], [2, 1], [2, 2], [2, 2]];;
#! gap> rev_map := (1,2)(3,4);;
#! gap> graph := RSGraphByAdjacencyList(adj_list, rev_map);
#! <RSGraph with 2 vertices and 4 arcs>
#! gap> RSGraphOutNeighbours(graph);
#! rec( 1 := [ 2 ], 2 := [ 1, 2 ] )
#! gap> RSGraphInNeighbours(graph);
#! rec( 1 := [ 2 ], 2 := [ 1, 2 ] )
#! gap> RSGraphOutArcs(graph);
#! rec( 1 := [ 1 ], 2 := [ 2, 3, 4 ] )
#! gap> RSGraphInArcs(graph);
#! rec( 1 := [ 2 ], 2 := [ 1, 3, 4 ] )
#! @EndExampleSession

#! @Returns The bipartition of <C>graph</C> if it is bipartite and
#! <K>fail</K> otherwise. 
#! @Arguments graph
#! @Label 
#! @Description If the graph is bipartite then this returns a list 
#! containing two lists. These two lists contain the vertex ids in 
#! each bipartition. 
#!
#! Note that for an RSGraph to be bipartite it must not contain 
#! parallel edges or loops. 
DeclareAttribute("RSGraphBipartition", IsRSGraph);

#! @BeginExampleSession
#! gap> adj_list := [[1, 2], [2, 1], [1, 3], [3, 1]];;
#! gap> rev_map := (1,2)(3,4);;
#! gap> graph := RSGraphByAdjacencyList(adj_list, rev_map);
#! <RSGraph with 3 vertices and 4 arcs>
#! gap> RSGraphBipartition(graph);
#! [ [ 1 ], [ 2, 3 ] ]
#!
#! gap> adj_list := [[1, 2], [2, 1], [2, 2], [2, 2]];;
#! gap> rev_map := (1,2)(3,4);;
#! gap> graph := RSGraphByAdjacencyList(adj_list, rev_map);
#! <RSGraph with 2 vertices and 4 arcs>
#! gap> RSGraphBipartition(graph);
#! fail
#! @EndExampleSession

#! @Returns The maximum number of arcs originating/terminating
#! at any vertex in the graph. 
#! @Arguments graph
#! @Label 
#! @Description 
DeclareAttribute("RSGraphDegree", IsRSGraph);

#! @Returns <K>true</K> if <A>graph</A> is a cycle graph and <K>false</K>
#! otherwise. 
#! @Arguments graph
#! @Label 
#! @Description This is a cycle in the sense of !!!cite RS paper here!!!. 
#! A graph is a cycle if:
#! - It has a single vertex, two arcs, and a non-trivial reverse map.
#! - It has two vertices and two edges (i.e. four arcs) between them.
#! - It has three or more vertices, no loops, and all vertices have degree two. 
DeclareProperty("RSGraphIsCycle", IsRSGraph);

#! @Returns <K>true</K> if <A>graph</A> is a bipartite graph and
#! <K>false</K> otherwise. 
#! @Arguments graph
#! @Label 
#! @Description This is equivalent to <C>RSGraphBipartition(graph) &lt;&gt; fail</C>. 
DeclareProperty("RSGraphIsBipartite", IsRSGraph);

#! @Returns <K>true</K> if <A>graph</A> has multiple arcs in the
#! same direction between any two pairs of vertices. 
#! @Arguments graph
#! @Label 
#! @Description 
DeclareProperty("RSGraphHasParallelArcs", IsRSGraph);


#! @Section RSGraph Operations
#!
#! @Returns An iterator which iterates over the arcs of <A>graph</A>. 
#! @Arguments graph
#! @Label 
#! @Description This functions returns an iterator object which iterates
#! over the arcs of <A>graph</A>. Each element of the iterator is of the
#! form <C>[arc_id, arc_record]</C> where <C>arc_record</C> is in the 
#! form described in <Ref Func="RSGraphArcs"/>. 
DeclareOperation("RSGraphArcIterator", [IsRSGraph]);

#! @BeginExampleSession
#! gap> adj_list := [[1, 2], [2, 1], [2, 2]];;
#! gap> rev_map := (1,2);;
#! gap> graph := RSGraphByAdjacencyList(adj_list, rev_map);;
#! gap> for arc in RSGraphArcIterator(graph) do
#! gap>     View(arc); 
#! gap>     Print("\n");
#! gap> od; 
#! [ 1, rec( inverse := 2, origin := 1, terminus := 2 ) ]
#! [ 2, rec( inverse := 1, origin := 2, terminus := 1 ) ]
#! [ 3, rec( inverse := 3, origin := 2, terminus := 2 ) ]
#! @EndExampleSession

#! @BeginGroup Subgraph
#!
#! @Returns An arc induced subgraph of <A>graph</A>. 
#! @Arguments graph, arc_list
#! @Label for an RSGraph and List of arcs
#! @Description Given a graph and list of arcs in this graph,
#! this function returns the subgraph consisting exactly of
#! those arcs and the vertices connecting them. The vertex ids,
#! arc ids, and reverse map are preserved. Note that the reverse
#! map is not changed and so may act on a larger set of integers
#! than is strictly necessary.  
#!
#! If an arc is in <A>arc_list</A> then its reverse must also
#! be in it. Furthermore, the resulting subgraph must be connected.
#!
DeclareOperation("RSGraphSubgraph", [IsRSGraph, IsList]);
#! @Arguments graph, arc_list
#! @Label for an RSGraph and List of arcs
#! @Description The NC variant of this function does not check for connectivity
#! or for the inclusion of arc reversals. 
DeclareOperation("RSGraphSubgraphNC", [IsRSGraph, IsList]);

#! @BeginExampleSession
#! gap> adj_list := [[1, 2], [2, 1], [1, 2], [2, 1], [2, 2]];;
#! gap> rev_map := (1,2)(3,4);;
#! gap> graph := RSGraphByAdjacencyList(adj_list, rev_map);;
#! gap> Print(graph);
#! Vertices = { 1, 2 }
#! Arcs = {
#!         1 = ( origin = 1, terminus = 2, inverse = 2 )
#!         2 = ( origin = 2, terminus = 1, inverse = 1 )
#!         3 = ( origin = 1, terminus = 2, inverse = 4 )
#!         4 = ( origin = 2, terminus = 1, inverse = 3 )
#!         5 = ( origin = 2, terminus = 2, inverse = 5 )
#! }
#! Reverse Map = (1,2)(3,4)
#! gap> subgraph := RSGraphSubgraph(graph, [1,2,5]);
#! Print(subgraph);
#! Vertices = { 1, 2 }
#! Arcs = {
#!         1 = ( origin = 1, terminus = 2, inverse = 2 )
#!         2 = ( origin = 2, terminus = 1, inverse = 1 )
#!         5 = ( origin = 2, terminus = 2, inverse = 5 )
#! }
#! Reverse Map = (1,2)(3,4)
#! @EndExampleSession
#!
#! @EndGroup

#! @Returns A record containing the graph in a standard range and
#! the maps from the old graph to the new graph. 
#! @Arguments graph
#! @Label 
#! @Description Given an arbitrary RSGraph with <M>N</M> vertices
#! and <M>M</M> arcs this function changes the vertex ids to be in
#! <M>\{1, 2, \dots, N\}</M> and arc ids to be in the range 
#! <M>\{1, 2, \dots, M\}</M>. It also ensures that the arcs are 
#! sorted in lexicographical order with respect to the origin and
#! terminus vertices. 
#!
#! If <M>\Gamma</M> is the <A>graph</A> then the vertex mapping
#! is defined by <M>V(\Gamma)_i \mapsto i</M>. The arcs origin and 
#! terminus vertices then have this 
#! mapping applied to them. The resulting arcs are then sorted in
#! lexicographical order so that an arc with a smaller arc id will
#! have an origin vertex with a smaller id than one with a larger 
#! arc id. If the origin vertices are equal then the arc with the
#! smaller terminus id will have a smaller arc id. The reverse map
#! of the graph is changed to have the same action on the new arc ids. 
#!
#! This function returns a record containing three components. The
#! <C>graph</C> component contains the new graph constructed from it,
#! the <C>vertex_id_map</C> component contains the map from the vertex
#! ids of the original graph to the new graph, and the <C>arc_id_map</C> 
#! component contains the map from the arc ids of the original graph 
#! to the new graph. 
#!
#! The <C>vertex_id_map</C> and <C>arc_id_map</C> are both in the category
#! <C>IsConstantTimeAccessGeneralMapping</C>. 
DeclareOperation("RSGraphToStandardForm", [IsRSGraph]);

#! @BeginExampleSession
#! gap> adj_list := [[2, 3], [3, 2], [2, 3], [3, 2], [3, 3]];;
#! gap> rev_map := (1,2)(3,4);;
#! gap> verrtex_ids := [2, 3];;
#! gap> graph := RSGraphByAdjacencyList(adj_list, rev_map, vertex_ids);;
#! gap> Print(graph);
#! Vertices = { 2, 3 }
#! Arcs = {
#!         1 = ( origin = 2, terminus = 3, inverse = 2 )
#!         2 = ( origin = 3, terminus = 2, inverse = 1 )
#!         3 = ( origin = 2, terminus = 3, inverse = 4 )
#!         4 = ( origin = 3, terminus = 2, inverse = 3 )
#!         5 = ( origin = 3, terminus = 2, inverse = 5 )
#! }
#! Reverse Map = (1,2)(3,4)
#! gap> standard_rec := RSGraphToStandardForm(graph);;
#! gap> Print(standard_rec.graph);
#! Vertices = { 1, 2 }
#! Arcs = {
#!         1 = ( origin = 1, terminus = 2, inverse = 3 )
#!         2 = ( origin = 1, terminus = 2, inverse = 4 )
#!         3 = ( origin = 2, terminus = 1, inverse = 1 )
#!         4 = ( origin = 2, terminus = 1, inverse = 2 )
#!         5 = ( origin = 2, terminus = 2, inverse = 5 )
#! }
#! Reverse Map = (1,3)(2,4)
#! gap> Print(2^standard_rec.arc_id_map);
#! 3
#! @EndExampleSession


#! @Returns A spanning tree of the graph. 
#! @Arguments graph[, search_type]
#! @Label 
#! @Description This function calculates a spanning tree of
#! the graph based on the edges. This means it does not include
#! and loops or cycles (see <Ref Func="RSGraphIsCycle"/> for
#! the definition of a cycle in an RSGraph). By default it uses
#! a breadth first search. 
#!
#! If the optional parameter <A>search_type</A>
#! is equal to the string <C>"dfs"</C> then it will use a depth first
#! search instead. If it is equal to the string <C>"bfs"</C> then it
#! will use a breadth first search. If it is equal to any other string
#! then a warning will be raised and the function will use a breadth
#! first search. 
DeclareOperation("RSGraphSpanningTree", [IsRSGraph]);
DeclareOperation("RSGraphSpanningTree", [IsRSGraph, IsString]);

#! @BeginExampleSession
#! gap> adj_list := [[1, 2], [2, 1], [1, 3], [3, 1], [1, 4], [4, 1], \
#! gap>              [2, 3], [3, 2], [3, 4], [4, 3]];;
#! gap> rev_map := (1,2)(3,4)(5,6)(7,8)(9,10);;
#! gap> graph := RSGraphByAdjacencyList(adj_list, rev_map);;
#! gap> tree_bfs := RSGraphSpanningTree(graph);;
#! gap> Print(tree_bfs);
#! Vertices = { 1, 2, 3, 4 }
#! Arcs = {
#!         1 = ( origin = 1, terminus = 2, inverse = 2 )
#!         2 = ( origin = 2, terminus = 1, inverse = 1 )
#!         3 = ( origin = 1, terminus = 3, inverse = 4 )
#!         4 = ( origin = 3, terminus = 1, inverse = 3 )
#!         5 = ( origin = 1, terminus = 4, inverse = 6 )
#!         6 = ( origin = 4, terminus = 1, inverse = 5 )
#! }
#! Reverse Map = (1,2)(3,4)(5,6)(7,8)(9,10)
#! gap> tree_dfs := RSGraphSpanningTree(graph, "dfs");;
#! gap> Print(tree_dfs);
#! Vertices = { 1, 2, 3, 4 }
#! Arcs = {
#!         1 = ( origin = 1, terminus = 2, inverse = 2 )
#!         2 = ( origin = 2, terminus = 1, inverse = 1 )
#!         7 = ( origin = 2, terminus = 3, inverse = 8 )
#!         8 = ( origin = 3, terminus = 2, inverse = 7 )
#!         9 = ( origin = 3, terminus = 4, inverse = 10 )
#!         10 = ( origin = 4, terminus = 3, inverse = 9 )
#! }
#! Reverse Map = (1,2)(3,4)(5,6)(7,8)(9,10)
#! @EndExampleSession


DeclareAttribute("AutomorphismGroup", IsRSGraph);
DeclareAttribute("RSGraphCanonicalLabelling", IsRSGraph);

DeclareOperation("LAD_Internal_RSGraphsEnumerate@", [IsInt, IsInt]);
