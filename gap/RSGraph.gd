# Category
DeclareCategory("IsRSGraph", IsObject);

DeclareRepresentation("IsRSGraphRep", IsRSGraph and IsAttributeStoringRep);

# Constructor
DeclareConstructor("RSGraphConsNC", [IsRSGraph, IsRecord]);

# User Level Construction Operations

# Adjacency Matrix or List, Reverse Map[, Vertex IDs]. 
DeclareOperation("RSGraphByAdjacencyListNC", [IsList, IsPerm]);
DeclareOperation("RSGraphByAdjacencyListNC", [IsList, IsPerm, IsList]);
DeclareOperation("RSGraphByAdjacencyList", [IsList, IsPerm]);
DeclareOperation("RSGraphByAdjacencyList", [IsList, IsPerm, IsList]);

DeclareOperation("RSGraphByAdjacencyMatrixNC", [IsMatrix, IsPerm]);
DeclareOperation("RSGraphByAdjacencyMatrixNC", [IsMatrix, IsPerm, IsList]);
DeclareOperation("RSGraphByAdjacencyMatrix", [IsMatrix, IsPerm]);
DeclareOperation("RSGraphByAdjacencyMatrix", [IsMatrix, IsPerm, IsList]);

# Graph Attributes
DeclareAttribute("RSGraphVertices", IsRSGraph);
DeclareAttribute("RSGraphNumberVertices", IsRSGraph);
DeclareAttribute("RSGraphArcs", IsRSGraph);
DeclareAttribute("RSGraphNumberArcs", IsRSGraph);
DeclareAttribute("RSGraphArcIds", IsRSGraph);
DeclareAttribute("RSGraphReverseMap", IsRSGraph);
DeclareAttribute("RSGraphAdjacencyMatrix", IsRSGraph);
DeclareAttribute("RSGraphCannonicalForm", IsRSGraph);
DeclareAttribute("RSGraphMG5String", IsRSGraph);

# Input/Output Format
DeclareOperation("RSGraphFromCannonicalFrom", [IsString]);
DeclareOperation("RSGraphFromMG5String", [IsString]);

# Make iterator for graph arcs. Returns [arc id, arc record]. 
