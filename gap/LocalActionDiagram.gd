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
DeclareOperation("LocalActionDiagramFromUniversalGroup", [IsPermGroup]);

# Attributes
DeclareAttribute("LocalActionDiagramRSGraph", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramVertexLabels", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramArcLabels", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramVertices", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramArcs", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramArcIDs", IsLocalActionDiagram);
DeclareAttribute("LocalActionDiagramReverseMap", IsLocalActionDiagram);
