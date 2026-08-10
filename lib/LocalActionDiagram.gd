#! @Chapter Local Action Diagrams

#! @Section Creating Local Action Diagrams
#!
#! @Returns <K>true</K> if the object is of the category 
#! <C>IsLocalActionDiagram</C> and false otherwise. 
#! @Arguments obj
#! @Label for an object
#! @Description Every local action diagram belongs to the category <C>IsLocalActionDiagram</C>. Furthermore, every local
#! action diagram is an immutable attribute storing object. 
DeclareCategory("IsLocalActionDiagram", IsObject);

DeclareRepresentation("IsLocalActionDiagramRep", IsLocalActionDiagram and IsAttributeStoringRep);

# Constructor
DeclareConstructor("LocalActionDiagramConsNC", [IsLocalActionDiagram, IsRecord]);

# User Level Construction Operations

#! @BeginGroup LADFromData
#!
#! @Returns A local action diagram object. 
#! @Arguments graph, vertex_labels, arc_labels
#! @Label for an RSGraph, list or record of vertex labels, and list or record of arc labels
#! @Description This function constructs a local action diagram by having all of the diagrams data given to it. This is
#! done by providing the <C>RSGraph</C> of the diagram and either lists of vertex and arc labels or records of vertex
#! and arc labels. 
#!
#! If two lists are provided then the vertex and arc labels are assumed to be in the same order as
#! <C>RSGraphVertices(<A>graph</A>)</C> and <C>RSGraphArcIDs(<A>graph</A>)</C> respectively. This means, for example,
#! that element <C>idx</C> of <A>vertex_labels</A> is the label of the vertex with id
#! <C>RSGraphVertexIDs(<A>graph</A>)[idx]</C>. If two records are provided then the names of the record elements are the
#! vertex and arc ids respectively and the components are their labels. 
#!
#! The vertex labels are permutation groups. The domain each label acts on is stored in the mutable attribute
#! <C>PermGroupDomain</C> (see <Ref Func="PermGroupDomain"/>). By default, this is equal to the <C>MovedPoints</C> of the
#! group but can be changed to allow for the group to have fixed points. 
#!
#! The arc labels are lists of integers. Each arc must be labelled by by an orbit of the vertex label at the vertex that
#! arc originates at. Furthermore, every vertex must have an arc originating at it for each orbit of its vertex label.
#! Note that the orbits of a vertex label <C>G</C> are determined by <C>Orbits(G, PermGroupDomain(G))</C>. 
#!
#! Unlike the formal definition of a local action diagram, we allow for overlap between the domains of different vertex 
#! labels. For example, a two vertex local action diagram could be labelled by <M>C_2</M> and <M>S_3</M> and they can
#! have domains <M>\{1, 2\}</M> and <M>\{1, 2, 3\}</M> respectively. 
#!
DeclareOperation("LocalActionDiagramFromData", [IsRSGraph, IsList, IsList]);

#! @Arguments graph, vertex_labels, arc_labels
#! @Label for an RSGraph, list or record of vertex labels, and list or record of arc labels
#! @Description The NC variant of the function does not check that there is a label for each vertex and arc (and exactly this many
#! labels), that the domains of the groups match up with the arc labels, or that the orbits of the vertex labels match
#! up with the arc labels. 
DeclareOperation("LocalActionDiagramFromDataNC", [IsRSGraph, IsList, IsList]);

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList([[1, 1], [1, 2], [2, 1]], (2,3));;
#! gap> label_1 := Group((1,2));;
#! gap> SetPermGroupDomain(label_1, [1, 2, 3]);;
#! gap> label_2 := Group((1,2,3));;
#! gap> vertex_labels := [label_1, label_2];
#! gap> arc_labels := [[1, 2], [3], [1,2,3]];
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);
#! <LocalActionDiagram with 2 vertices and 3 arcs>
#! gap> Print(lad);
#! Vertices = { 1, 2 }
#! Arcs = {
#!         1 = ( origin = 1, terminus = 1, inverse = 1 )
#!         2 = ( origin = 1, terminus = 2, inverse = 3 )
#!         3 = ( origin = 2, terminus = 1, inverse = 2 )
#! }
#! Reverse Map = (2,3)
#! Vertex Labels = {
#!         1 = Group( [ (1,2) ] )
#!         2 = Group( [ (1,2,3) ] )
#! }
#! Arc Labels = {
#!         1 = [ 1, 2 ]
#!         2 = [ 3 ]
#!         3 = [ 1, 2, 3 ]
#! }
#! @EndExampleSession

#! @EndGroup

DeclareOperation("LocalActionDiagramFromData", [IsRSGraph, IsRecord, IsRecord]);
DeclareOperation("LocalActionDiagramFromDataNC", [IsRSGraph, IsRecord, IsRecord]);


#! @Returns A local action diagram object. 
#! @Arguments perm_group
#! @Label 
#! @Description Given a permutation group <M>F</M> (<A>perm_group</A>) this function returns the local action diagram
#! corresponding to the Burger-Mozes universal group <M>U(F)</M> (see [cite Burger-Mozes and Colin and Simon for
#! construction?]). This is a local action diagram with one vertex labelled by <M>F</M> and a self-reverse loop for each
#! orbit of <M>F</M>. Note that <M>F</M> acts on the domain <C>PermGroupDomain(<A>perm_group</A>)</C>. 
DeclareOperation("LocalActionDiagramFromBurgerMozesUniversalGroup", [IsPermGroup]);

#! @BeginExampleSession
#! gap> lad := LocalActionDiagramFromBurgerMozesUniversalGroup(Group((1,2)(3,4)));
#! <U(Group( [ (1,2)(3,4) ] )) (as a Local Action Diagram)>
#! @EndExampleSession

#! @Returns A local action diagram object. 
#! @Arguments lad
#! @Label 
#! @Description Given a single vertex local action diagram <M>\Delta</M> (<A>lad</A>) corresponding to the group
#! <M>U(\Delta)</M> acting on a tree <M>T</M> this function returns the local action diagram corresponding to the group 
#! <M>U(\Delta)^{\circ} = \{g \in U(\Delta)\, |\, \forall\, v \in V(T),\, d(v, gv) \equiv 0 \pmod{2}\}</M>. Intuitively this is
#! the group of all automorphisms the preserve the bipartition of <M>T</M> which always exists since a single vertex
#! local action diagram is vertex transitive. 
#!
#! Given a group <M>G</M> labelling the single vertex of <M>\Delta</M> and arc labels <M>X_a</M> <M>r</M> this local
#! action diagram  is constructed as follows: 
#! - Make a graph with two vertices and an edge for each loop of the graph for <M>\Delta</M>. 
#! - Label both vertices by <M>G</M>. 
#! - For each arc <M>a</M> in the graph of <M>\Delta</M> label the corresponding arc in the new graph by <M>X_{a}</M>
#!   and the reverse of this arc by <M>X_{\overline{a}}</M>. 
DeclareOperation("LocalActionDiagramConstructBipartitionPreserving", [IsLocalActionDiagram]);

#! @Section Local Action Diagram Attributes and Properties

#! @Returns RSGraph associated with the local action diagram. 
#! @Arguments lad
#! @Label 
#! @Description 
DeclareAttribute("LocalActionDiagramRSGraph", IsLocalActionDiagram);

#! @Returns Vertex labels of the local action diagram. 
#! @Arguments lad
#! @Label 
#! @Description 
DeclareAttribute("LocalActionDiagramVertexLabels", IsLocalActionDiagram);

#! @Returns Arc labels of the local action diagram. 
#! @Arguments lad
#! @Label 
#! @Description 
DeclareAttribute("LocalActionDiagramArcLabels", IsLocalActionDiagram);

#! @Returns The vertices of the local action diagrams graph. 
#! @Arguments lad
#! @Label
#! @Description Shorthand for <C>RSGraphVertices(LocalActionDiagramRSGraph(<A>lad</A>))</C>. 
DeclareAttribute("LocalActionDiagramVertices", IsLocalActionDiagram);

#! @Returns The arcs of the local action diagrams graph. 
#! @Arguments lad
#! @Label
#! @Description Shorthand for <C>RSGraphArcs(LocalActionDiagramRSGraph(<A>lad</A>))</C>. 
DeclareAttribute("LocalActionDiagramArcs", IsLocalActionDiagram);

#! @Returns The arc ids of the local action diagrams graph. 
#! @Arguments lad
#! @Label
#! @Description Shorthand for <C>RSGraphArcIDs(LocalActionDiagramRSGraph(<A>lad</A>))</C>. 
DeclareAttribute("LocalActionDiagramArcIDs", IsLocalActionDiagram);

#! @Returns The number of vertices in the local action diagrams graph.  
#! @Arguments lad
#! @Label
#! @Description Shorthand for <C>RSGraphNumberVertices(LocalActionDiagramRSGraph(<A>lad</A>))</C>. 
DeclareAttribute("LocalActionDiagramNumberVertices", IsLocalActionDiagram);

#! @Returns The number of arcs in the local action diagrams graph.  
#! @Arguments lad
#! @Label
#! @Description Shorthand for <C>RSGraphNumberArcs(LocalActionDiagramRSGraph(<A>lad</A>))</C>. 
DeclareAttribute("LocalActionDiagramNumberArcs", IsLocalActionDiagram);

#! @Returns The reverse map of the local action diagrams graph. 
#! @Arguments lad
#! @Label
#! @Description Shorthand for <C>RSGraphReverseMap(LocalActionDiagramRSGraph(<A>lad</A>))</C>. 
DeclareAttribute("LocalActionDiagramReverseMap", IsLocalActionDiagram);

#! @Returns The record of out neighbours of the local action diagrams graph. 
#! @Arguments lad
#! @Label
#! @Description Shorthand for <C>RSGraphOutNeighbours(LocalActionDiagramRSGraph(<A>lad</A>))</C>. 
DeclareAttribute("LocalActionDiagramOutNeighbours", IsLocalActionDiagram);

#! @Returns The record of in neighbours of the local action diagrams graph. 
#! @Arguments lad
#! @Label
#! @Description Shorthand for <C>RSGraphInNeighbours(LocalActionDiagramRSGraph(<A>lad</A>))</C>. 
DeclareAttribute("LocalActionDiagramInNeighbours", IsLocalActionDiagram);

#! @Returns The record of out arcs of the local action diagrams graph. 
#! @Arguments lad
#! @Label
#! @Description Shorthand for <C>RSGraphOutArcs(LocalActionDiagramRSGraph(<A>lad</A>))</C>. 
DeclareAttribute("LocalActionDiagramOutArcs", IsLocalActionDiagram);

#! @Returns The record of in arcs of the local action diagrams graph. 
#! @Arguments lad
#! @Label
#! @Description Shorthand for <C>RSGraphInArcs(LocalActionDiagramRSGraph(<A>lad</A>))</C>. 
DeclareAttribute("LocalActionDiagramInArcs", IsLocalActionDiagram);



#! @Returns A name for the associated universal group of the local action diagram. 
#! @Arguments lad
#! @Label
#! @Description This is a mutable attribute which stores a human readable name for the associated universal group. By
#! default it is equal to the empty string. In this case the <C>View</C> function will print <C>&lt;LocalActionDiagram
#! with n vertices and m arcs&gt;</C> where <C>n</C> is the number of vertices and <C>m</C> is the number of arcs. If
#! this attribute is set to to any other string then the <C>View</C> function will print <C>&lt;{group_name} (as a Local
#! Action Diagram)&gt;</C>. 
DeclareAttribute("LocalActionDiagramGroupName", IsLocalActionDiagram, "mutable");

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList([[1, 1]], ());;
#! gap> vertex_labels := [SymmetricGroup(3)];;
#! gap> arc_labels := [[1,2,3]];;
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);
#! <LocalActionDiagram with 1 vertex and 1 arc>
#! gap> SetLocalActionDiagramGroupName(lad, "Aut(T_3)");
#! gap> View(lad);
#! <Aut(T_3) (as a Local Action Diagram)>
#! @EndExampleSession

#! @Returns The degree of the regular tree the universal group acts on or <K>fail</K> if it does not act on one. 
#! @Arguments lad
#! @Label
#! @Description If every vertex label of the local action diagram has a domain of size <M>d</M> then the associated
#! universal group acts on a regular tree of degree <M>d</M>. This functions returns <M>d</M> if this is the case and
#! <K>fail</K> otherwise. 
DeclareAttribute("LocalActionDiagramRegularTree", IsLocalActionDiagram);

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList([[1, 2], [2, 1]], (1,2));;
#! gap> vertex_labels := [SymmetricGroup(3), Group((1,2,3))];;
#! gap> arc_labels := [[1,2,3], [1,2,3]];;
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);;
#! gap> LocalActionDiagramRegularTree(lad);
#! 3
#! gap> vertex_labels_2 := [SymmetricGroup(3), Group((1,2))];;
#! gap> arc_labels_2 := [[1,2,3], [1,2]];;
#! gap> lad_2 := LocalActionDiagramFromData(graph, vertex_labels_2, arc_labels_2);;
#! gap> LocalActionDiagramRegularTree(lad_2);
#! fail
#! @EndExampleSession

# Attributes needing some calculation

#! @Returns The list of scopos of the local action diagram. 
#! @Arguments lad
#! @Label
#! @Description (CITE REID SMITH HERE) Let <M>\Delta = (\Gamma, (X_a), (G(v)))</M> be a local action diagram. A strongly
#! confluent partial orientation (<E>scopo</E>) of <M>\Delta</M> is a subset <M>O</M> of <M>A(\Gamma)</M> such that: 
#! - if <M>a \in O</M> then <M>\overline{a} \notin O</M>, 
#! - if <M>a \in O</M> then <M>\left|X_{a}\right| = 1</M>, and 
#! - for all <M>v \in V(\Gamma)</M> if <M>O</M> contains an arc <M>a</M> originating at <M>v</M> then <M>O</M> contains
#!   all arcs that terminate at <M>v</M> except for <M>\overline{a}</M>. 
#!
#! This function performs an iterative search for all scopos of a local action diagram. It stores each scopo as a list
#! of arc ids in the scopo. Note that the empty set is a scopo and this is represented as the empty list. 
DeclareAttribute("LocalActionDiagramScopos", IsLocalActionDiagram);

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList ([[1, 2], [2, 1]], (1,2));;
#! gap> group := Group(());;
#! gap> SetPermGroupDomain(group, [1]);;
#! gap> vertex_labels := [group, group];;
#! gap> arc_labels := [[1], [1]];;
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);;
#! gap> LocalActionDiagramScopos(lad);
#! [ [ ], [ 1 ], [ 2 ]]
#! @EndExampleSession

#! @Returns The type of the corresponding universal group. 
#! @Arguments lad
#! @Label
#! @Description (CITE REID SMITH HERE) Every group acting on a tree can be split into one of six mutually exclusive 
#! types: <E>fixed vertex</E>, <E>edge inversion</E>, <E>lineal</E>, <E>horocyclic</E>, <E>focal</E>, and
#! <E>general</E>. This types can be recognised from the groups corresponding local action diagram by analysing the
#! scopos of the local action diagram (see CITE REID SMITH). 
#!
#! This function first calculates the scopos of the local action diagram (see <Ref Func="LocalActionDiagramScopos"/>) and
#! then uses this to determine the type of the corresponding group. It outputs this types as a string. 
DeclareAttribute("LocalActionDiagramGroupType", IsLocalActionDiagram);

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList ([[1, 2], [2, 1]], (1,2));;
#! gap> group := Group(());;
#! gap> SetPermGroupDomain(group, [1]);;
#! gap> vertex_labels := [group, group];;
#! gap> arc_labels := [[1], [1]];;
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);;
#! gap> LocalActionDiagramGroupType(lad);
#! "General"
#! @EndExampleSession

#! @Returns <K>true</K> if the corresponding universal group is discrete and false otherwise.  
#! @Arguments lad 
#! @Label
#! @Description This function implements (CITE THEOREM HERE) to determine if the corresponding universal group is
#! discrete. This requires calculation of the diagrams group type (see <Ref Func="LocalActionDiagramGroupType"/>).
#! Furthermore, if the group is discrete then it is also uniscalar and unimodular and so if this function returns
#! <K>true</K> then it also sets <Ref Func="LocalActionDiagramIsUniscalar"/> and <Ref
#! Func="LocalActionDiagramIsUnimodular"/> to true. 
DeclareProperty("LocalActionDiagramIsDiscrete", IsLocalActionDiagram);

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList ([[1, 2], [2, 1]], (1,2));;
#! gap> group := Group(());;
#! gap> SetPermGroupDomain(group, [1]);;
#! gap> vertex_labels := [group, group];;
#! gap> arc_labels := [[1], [1]];;
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);;
#! gap> LocalActionDiagramIsDiscrete(lad);
#! true
#! @EndExampleSession


#! @Returns <K>true</K> if the corresponding universal group is uniscalar and false otherwise.  
#! @Arguments lad 
#! @Label
#! @Description This function implements (CITE THEOREM HERE) to determine if the corresponding universal group is
#! uniscalar. This requires calculation of the diagrams group type (see <Ref Func="LocalActionDiagramGroupType"/>).
#! Furthermore, if the group is uniscalar then it is also unimodular so if this function returns <K>true</K> then it
#! also sets  <Ref Func="LocalActionDiagramIsUnimodular"/> to true. 
DeclareProperty("LocalActionDiagramIsUniscalar", IsLocalActionDiagram);

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList ([[1, 2], [2, 1], [2, 2]], (1,2));;
#! gap> group := Group(());;
#! gap> group_2 := Group((1, 2), (1,2,3), (4,5));;
#! gap> SetPermGroupDomain(group, [1]);;
#! gap> vertex_labels := [group, group_2];;
#! gap> arc_labels := [[1], [1, 2, 3], [4, 5]];;
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);;
#! gap> LocalActionDiagramIsDiscrete(lad);
#! false
#! gap> LocalActionDiagramIsUniscalar(lad);
#! true
#! @EndExampleSession


#! @Returns <K>true</K> if the corresponding universal group is unimodular and false otherwise.  
#! @Arguments lad 
#! @Label
#! @Description This function implements (CITE THEOREM HERE both us and BK) to determine if the corresponding universal
#! group is unimodular. This requires calculation of a spanning tree of the graph (see <Ref
#! Func="RSGraphSpanningTree"/>).
DeclareProperty("LocalActionDiagramIsUnimodular", IsLocalActionDiagram);

#! @BeginExampleSession
#! gap> adj_list := [[1, 2], [2, 1], [2, 3], [3, 2], [3, 1], [1, 3], [1, 1]];;
#! gap> graph := RSGraphByAdjacencyList(adj_list, (1,2)(3,4)(5,6));;
#! gap> vertex_labels := [];;
#! gap> Add(vertex_labels, Group((1, 2, 3), (4,5), (6,7)));;
#! gap> Add(vertex_labels, Group((1, 2), (3, 4, 5)));;
#! gap> Add(vertex_labels, Group((1, 2), (3, 4)));;
#! gap> arc_labels := [[1, 2, 3], [3, 4, 5], [1, 2], [1, 2], [3,4], [4,5], [6,7]];;
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);;
#! gap> LocalActionDiagramIsDiscrete(lad);
#! false
#! gap> LocalActionDiagramIsUniscalar(lad);
#! false
#! gap> LocalActionDiagramIsUnimodular(lad);
#! true
#! @EndExampleSession

#! @Returns <K>true</K> if the corresponding universal group is a Burger-Mozes Universal Group and <K>false</K>
#! otherwise. 
#! @Arguments lad 
#! @Label
#! @Description If a local action diagram has a single vertex and all loops at the vertex are self-reverse then the
#! diagram corresponds to a Burger-Mozes group (see PROBABLY SECOND COLIN SIMON PAPER). If the given diagram has such a
#! structure then this function returns true. It also sets <Ref Func="LocalActionDiagramGroupName"/> to
#! <C>"U({vertex_label})"</C> where <C>{vertex_label}</C> is the name of the group labelling the vertex. 
DeclareProperty("LocalActionDiagramIsBurgerMozesUniversalGroup", IsLocalActionDiagram);

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList([[1, 1], [1, 1]], ());;
#! gap> vertex_labels := [Group((1, 2), (3, 4))];;
#! gap> SetName(vertex_labels[1], "C2 x C2");
#! gap> arc_labels := [[1, 2], [3, 4]];;
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);;
#! gap> View(lad);
#! <LocalActionDiagram with 1 vertex and 2 arcs>
#! gap> LocalActionDiagramIsBurgerMozesUniversalGroup(lad);
#! true
#! gap> View(lad);
#! <U(C2 x C2) (as a Local Action Diagram)>
#! @EndExampleSession



# Not implemented in this file. Requires digraphs. Here for the library to access this data without needing digraphs. 
DeclareProperty("LocalActionDiagramIsSmithUniversalGroup", IsLocalActionDiagram);

#! @Returns <K>true</K> if the corresponding universal group is the stabiliser of a single end in the full automorphism
#! group of a regular tree and <K>false</K> otherwise. 
#! otherwise. 
#! @Arguments lad 
#! @Label
#! @Description If a local action diagram has a single vertex with two mutually reverse loops and the vertex is labelled
#! by <M>S_{d-1}</M> acting on <M>\{1, 2, \dots, d\}</M> then the universal group is isomorphic to
#! <M>\operatorname{Aut}(T_{d})_{\omega}</M> where <M>\omega</M> is an end of <M>T_{d}</M>. This function returns <K>true</K> if the group
#! satisfies this structure and <K>false</K> otherwise. It also sets <Ref Func="LocalActionDiagramGroupName"/> to <C>Aut(Td)_omega</C>.
DeclareProperty("LocalActionDiagramIsEndStabiliser", IsLocalActionDiagram);

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList([[1, 1], [1, 1]], (1, 2));;
#! gap> vertex_labels := [SymmetricGroup(2)];;
#! gap> SetPermGroupDomain(vertex_labels[1], [1,2,3]);;
#! gap> arc_labels := [[1, 2], [3]];;
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);;
#! gap> View(lad);
#! <LocalActionDiagram with 1 vertex and 2 arcs>
#! gap> LocalActionDiagramIsEndStabiliser(lad);
#! true
#! gap> View(lad);
#! <Aut(T3)_omega (as a Local Action Diagram)>
#! @EndExampleSession

#! @Returns <K>true</K> if the corresponding universal group is the full automorphism group of a regular tree and
#! <K>false</K> otherwise. 
#! @Arguments lad 
#! @Label
#! @Description If a local action diagram has a single vertex with one self-reverse loop and the vertex is labelled by
#! <M>S_d</M> then the universal group is isomorphic to <M>\operatorname{Aut}(T_d)</M>. This function returns
#! <K>true</K> if the group satisfies this structure and <K>false</K> otherwise. It also sets <Ref
#! Func="LocalActionDiagramGroupName"/> to <C>Aut(Td)</C>.
DeclareProperty("LocalActionDiagramIsRegularTreeAutomorphismGroup", IsLocalActionDiagram);

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList([[1, 1]], ());;
#! gap> vertex_labels := [SymmetricGroup(5)];;
#! gap> arc_labels := [[1..5]];;
#! gap> lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);;
#! gap> View(lad);
#! <LocalActionDiagram with 1 vertex and 1 arc>
#! gap> LocalActionDiagramIsRegularTreeAutomorphismGroup(lad);
#! true
#! gap> View(lad);
#! <Aut(T5) (as a Local Action Diagram)>
#! @EndExampleSession



# Not implemented in this file. Requires digraphs. Here for the library to access this data without needing digraphs. 
DeclareProperty("LocalActionDiagramIsBipartitionPreservingGroup", IsLocalActionDiagram);

# Arc iterator? 
# --- Option for [arc_id, arc_rec, origin_label, terminus_label]
# Maybe functions like LocalActionDiagramVertexLabel(id) (return the vert label)
# --- Stop the need for accessing it via record notation? 

DeclareOperation("LAD_Internal_LocalActionDiagramsEnumerate@", [IsInt, IsInt]); 
