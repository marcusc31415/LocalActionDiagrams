#########################
# Local Action Diagrams #
#########################

#! @Chapter Introduction
#! @Section Constructing Local Action Diagrams

# Category For Local Action Diagrams
DeclareCategory("IsLocalActionDiagram", IsDigraph);

# Local Action Diagram Attributes
DeclareAttribute("LocalActionDiagramVertexLabels", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramEdges", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramEdgeLabels", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramEdgeReversal", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramScopos", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramGroupType", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramIsDiscrete", IsLocalActionDiagram);

#! @BeginGroup Construction
#! @GroupTitle Constructing From Data

#! @Arguments D, v_labels, e_labels, rev
#! @Returns A local action diagram.
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
