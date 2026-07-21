# Category
DeclareCategory("IsLocalActionDiagram", IsObject);

DeclareRepresentation("IsLocalActionDiagramRep", IsLocalActionDiagram and IsAttributeStoringRep);

# Constructor
DeclareConstructor("LocalActionDiagramConsNC", [IsLocalActionDiagram, IsRecord]);

# User Level Construction Operations

DeclareOperation("LocalActionDiagramFromData", [IsRSGraph, IsList, IsList]);
DeclareOperation("LocalActionDiagramFromData", [IsRSGraph, IsRecord, IsRecord]);
DeclareOperation("LocalActionDiagramFromDataNC", [IsRSGraph, IsList, IsList]);
DeclareOperation("LocalActionDiagramFromDataNC", [IsRSGraph, IsRecord, IsRecord]);
DeclareOperation("LocalActionDiagramFromBurgerMozesUniversalGroup", [IsPermGroup]);
DeclareOperation("LocalActionDiagramConstructBipartitionPreserving", [IsLocalActionDiagram]);

# Attributes
DeclareAttribute("LocalActionDiagramRSGraph", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramVertexLabels", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramArcLabels", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramVertices", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramArcs", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramArcIDs", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramReverseMap", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramOutNeighbours", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramInNeighbours", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramOutArcs", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramInArcs", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramGroupName", IsLocalActionDiagram, "mutable");
DeclareAttribute("LocalActionDiagramRegularTree", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramNumberVertices", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramNumberArcs", IsLocalActionDiagram);

# Attributes needing some calculation
DeclareAttribute("LocalActionDiagramScopos", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramGroupType", IsLocalActionDiagram);
DeclareProperty("LocalActionDiagramIsDiscrete", IsLocalActionDiagram);
DeclareProperty("LocalActionDiagramIsUniscalar", IsLocalActionDiagram);
DeclareProperty("LocalActionDiagramIsUnimodular", IsLocalActionDiagram);
DeclareProperty("LocalActionDiagramIsBurgerMozesUniversalGroup", IsLocalActionDiagram);
DeclareProperty("LocalActionDiagramIsSmithUniversalGroup", IsLocalActionDiagram);
DeclareProperty("LocalActionDiagramIsEndStabiliser", IsLocalActionDiagram);
DeclareProperty("LocalActionDiagramIsFullAutomorphismGroup", IsLocalActionDiagram);
DeclareProperty("LocalActionDiagramIsBipartitionPreservingGroup", IsLocalActionDiagram);

# Arc iterator? 
# --- Option for [arc_id, arc_rec, origin_label, terminus_label]
# Maybe functions like LocalActionDiagramVertexLabel(id) (return the vert label)
# --- Stop the need for accessing it via record notation? 

DeclareOperation("LAD_Internal_LocalActionDiagramsEnumerate@", [IsInt, IsInt]); 
