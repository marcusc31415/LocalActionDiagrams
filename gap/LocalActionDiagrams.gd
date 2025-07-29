#########################
# Local Action Diagrams #
#########################

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

# Local Action Diagram Functions
DeclareOperation("LocalActionDiagramFromData", [IsDigraph, IsList, IsList, IsPerm]);
DeclareOperation("LocalActionDiagramFromDataNC", [IsDigraph, IsList, IsList, IsPerm]);
DeclareOperation("LocalActionDiagramUniversalGroup", [IsPermGroup]);
DeclareOperation("IsomorphismLocalActionDiagrams", [IsLocalActionDiagram, IsLocalActionDiagram]);
DeclareOperation("CotreeFromScopo", [IsLocalActionDiagram, IsList]);
DeclareOperation("CotreeFromScopoNC", [IsLocalActionDiagram, IsList]);


############################
# Permutation Group Domain #
############################

DeclareAttribute("PermGroupDomain", IsPermGroup, "mutable"); 

###################
# Other Functions #
###################

DeclareOperation("AllLocalActionDiagrams", [IsPosInt, IsPosInt]);
