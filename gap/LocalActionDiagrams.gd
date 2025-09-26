#########################
# Local Action Diagrams #
#########################

#! @Chapter Introduction 
#!
#! A local action diagram <M>\Delta = (\Gamma, (G(v)), (X_a))</M> consists of: 
#! - a directed graph <M>\Gamma</M>, 
#! - a closed permutation group <M>G(v)</M> for each <M>v \in V(\Gamma)</M>, and 
#! - a set <M>X_{a}</M> for each <M>a \in A(\Gamma)</M> such that each <M>X_{a}</M> is disjoint and <M>X_{a}</M> is an orbit of the action of <M>G(o(a))</M> on <M>X_{v} := \bigsqcup_{a \in o^{-1}(v)}X_{a}</M>. 
#!
#! The definition of digraph used is different to that of the <Package>Digraphs</Package> package. For our purposes, a digraph <M>\Gamma</M> consists of:
#! - a vertex set <M>V</M>,
#! - an arc set <M>A</M>,
#! - a map <M>o : A \to V</M> assigning each arc to an <E>origin</E> vertex, and
#! - a bijection <M>r : A \to A</M> (denoted by <M>a \mapsto \overline{a}</M>) such that <M>r^{2} =  \mathrm{id}</M>. 
#! The bijection <M>r</M> defines a reverse arc for each arc of the graph. This is more specific than the definition in the <A>digraphs</A> package which does not require a reverse mapping. 
#!
#! The local action diagrams package provides a category for local action diagrams. It is built in the <A>IsDigraph</A> category from the <Package>Digraphs</Package> package. 
#!

#! @Chapter Creating Local Action Diagrams
#! @Section Creating Local Action Diagrams 

#! @Returns <K>true</K> if <A>lad</A> is of the category <K>IsLocalActionDiagram</K> and <K>false</K> otherwise. 
#! @Arguments lad
#! @Description
#! Every local action diagram belongs to the <C>IsLocalActionDiagram</C> category. Every local action diagram is immutable. 
DeclareCategory("IsLocalActionDiagram", IsDigraph);


DeclareAttribute("LocalActionDiagramVertices", IsLocalActionDiagram);

#! @Chapter Local Action Diagram Attributes and Operations
#! 
#! @Section Local Action Diagram Attributes

#! @Returns A list of groups.
#! @Arguments lad
#! @Description
#! Returns the groups labelling the vertices of <A>lad</A>. Entry <C>i</C> of the list corresponds to the group labelling vertex <C>i</C> of the digraph. 
DeclareAttribute("LocalActionDiagramVertexLabels", IsLocalActionDiagram);


#! @Returns A list of lists.
#! @Arguments lad
#! @Description  
#! Returns a list of edges of <A>lad</A>. Each edge is stored as a list 
#! <C>[i, j]</C> where <C>i</C> is the origin vertex and <C>j</C> is the
#! terminus vertex. The list is stored in lexicographical order. 
DeclareAttribute("LocalActionDiagramEdges", IsLocalActionDiagram);

#! @Returns A list of lists.
#! @Arguments lad
#! @Description  
#! Returns the edge labels of <A>lad</A>. Entry <C>i</C> of the list corresponds
#! to the label of edge <C>i</C> of the digraph. 
DeclareAttribute("LocalActionDiagramEdgeLabels", IsLocalActionDiagram);

#! @Returns A permutation.
#! @Arguments lad
#! @Description  
#! Returns the reversal mapping of the local action digram <A>lad</A>. 
DeclareAttribute("LocalActionDiagramEdgeReversal", IsLocalActionDiagram);

#! @Section Local Action Diagram Operations 

#! @Returns A list of lists.
#! @Arguments lad
#! @Description  
#! Returns a list of all scopos of <A>lad</A>. Each entry of the list is a list
#! of edges in the scopo. This list will always contain the empty scopo <C>[]</C>.
DeclareAttribute("LocalActionDiagramScopos", IsLocalActionDiagram);

#! @Returns A string.
#! @Arguments lad
#! @Description  
#! Returns the group type the local action digram corresponds to. This is either
#! "Fixed Vertex", "Edge Inversion", "Lineal", "Focal", or "General". Note that
#! all "Horocyclic" local action diagrams have infinitely many vertices and so
#! can never be the type returned by this function. 
DeclareAttribute("LocalActionDiagramGroupType", IsLocalActionDiagram);

#! @Returns <K>true</K> or <K>false</K>.
#! @Arguments lad
#! @Description  
#! Returns <K>true</K> if <A>lad</A> corresponds to a discrete group and
#! <K>false</K> otherwise. 
DeclareAttribute("LocalActionDiagramIsDiscrete", IsLocalActionDiagram);

#! @Chapter Creating Local Action Diagrams
#! @Section Creating Local Action Diagrams 


#! @BeginGroup Creating Local Action Diagrams
#! @GroupTitle Constructing From Data

#! @Arguments D, vertex_labels, edge_labels, rev
#! @Returns A local action diagram.
#! @Description
#! 	Constructs a local action diagram, checking that the arguments given are a valid local action diagram. The argument <A>D</A> is a digraph and <A>rev</A> must be a compatible involution on the edges of <A>D</A>. The argument <A>vertex_labels</A> is a list of vertex labels such that <C><A>vertex_labels</A>[i]</C> is the group labelling vertex <C>i</C> of <A>D</A>. 
#!
#! The argument <A>edge_labels</A> is a list of edge labels. The edges of <A>D</A> are stored in lexicographical order and <C><A>edge_labels</A>[i]</C> is the set labelling edge <C>i</C> of <A>D</A> (when sorted in lexicographical order). 
DeclareOperation("LocalActionDiagramFromData", [IsDigraph, IsList, IsList, IsPerm]);

#! @Returns A local action diagram.
#! @Arguments D, vertex_labels, edge_labels, rev
#! @Description
#! 
#!	The NC variant of the operation does not check that the arguments given are a valid local action diagram.
DeclareOperation("LocalActionDiagramFromDataNC", [IsDigraph, IsList, IsList, IsPerm]);

#! @EndGroup

#! @Returns A local action diagram. 
#! @Arguments F
#! @Description
#! Constructs a local action diagram corresponding to the Burger-Mozes group <M>U(<A>F</A>)</M> where <A>F</A> is a permutation group. 
#! This diagram has a single vertex labelled by the group <A>F</A> and a self-reverse loop for each orbit of the action of <A>F</A>. 
DeclareOperation("LocalActionDiagramUniversalGroup", [IsPermGroup]);



DeclareOperation("IsomorphismLocalActionDiagrams", [IsLocalActionDiagram, IsLocalActionDiagram]);
DeclareOperation("CotreeFromScopo", [IsLocalActionDiagram, IsList]);
DeclareOperation("CotreeFromScopoNC", [IsLocalActionDiagram, IsList]);
DeclareOperation("RandomLocalActionDiagram", [IsPosInt, IsPosInt]);


DeclareOperation("WriteLocalActionDiagram", [IsLocalActionDiagram, IsString]);
DeclareOperation("ReadLocalActionDiagram", [IsString]);


############################
# Permutation Group Domain #
############################

DeclareAttribute("PermGroupDomain", IsPermGroup, "mutable"); 

###################
# Other Functions #
###################

DeclareOperation("AllLocalActionDiagrams", [IsPosInt, IsPosInt]);
