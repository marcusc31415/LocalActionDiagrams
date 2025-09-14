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

## Category For Local Action Diagrams ##
#! @Returns <K>true</K> if <A>lad</A> is of the category <K>IsLocalActionDiagram</K> and <K>false</K> otherwise. 
#! @Arguments lad
#! @Description
#! Every local action diagram belongs to the <C>IsLocalActionDiagram</C> category. Every local action diagram is immutable. 
DeclareCategory("IsLocalActionDiagram", IsDigraph);

# Local Action Diagram Attributes
DeclareAttribute("LocalActionDiagramVertexLabels", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramEdges", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramEdgeLabels", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramEdgeReversal", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramScopos", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramGroupType", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramIsDiscrete", IsLocalActionDiagram);

#! @BeginGroup Creating Local Action Diagrams
#! @GroupTitle Constructing From Data

#! @Arguments D, v_labels, e_labels, rev
#! @Returns A local action diagram.
#! @Description
#! @Description
#! 	Constructs a local action diagram, checking that the arguments given are a valid local action diagram. The argument <A>D</A> is a digraph and <A>rev</A> must be a compatible involution on the edges of <A>D</A>. The argument <A>v_labels</A> is a list of vertex labels such that <C><A>v_labels</A>[i]</C> is the group labelling vertex <C>i</C> of <A>D</A>. 
#!
#! The argument <A>e_labels</A> is a list of edge labels. The edges of <A>D</A> are stored in lexicographical order and <C><A>edge_labels</A>[i]</C> is the set labelling edge <C>i</C> of <A>D</A> (when sorted in lexicographical order). 
DeclareOperation("LocalActionDiagramFromData", [IsDigraph, IsList, IsList, IsPerm]);

#! @Arguments D, v_labels, e_labels, rev
#! @Description
#! 
#!	The NC variant of the operation does not check that the arguments given are a valid local action diagram.
DeclareOperation("LocalActionDiagramFromDataNC", [IsDigraph, IsList, IsList, IsPerm]);

#! @EndGroup


#! @Description
#!	Constructs a local action diagram corresponding to the Burger-Moses group U(F). 
DeclareOperation("LocalActionDiagramUniversalGroup", [IsPermGroup]);



DeclareOperation("IsomorphismLocalActionDiagrams", [IsLocalActionDiagram, IsLocalActionDiagram]);
DeclareOperation("CotreeFromScopo", [IsLocalActionDiagram, IsList]);
DeclareOperation("CotreeFromScopoNC", [IsLocalActionDiagram, IsList]);
DeclareOperation("RandomLocalActionDiagram", [IsPosInt, IsPosInt]);


############################
# Permutation Group Domain #
############################

DeclareAttribute("PermGroupDomain", IsPermGroup, "mutable"); 

###################
# Other Functions #
###################

DeclareOperation("AllLocalActionDiagrams", [IsPosInt, IsPosInt]);
