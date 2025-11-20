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
DeclareAttribute("RSGraphMG5String", IsRSGraph);
DeclareAttribute("RSGraphOutNeighbours", IsRSGraph);

# Input/Output Format
DeclareOperation("RSGraphFromMG5String", [IsString]);

# [id, record]
DeclareOperation("RSGraphArcIterator", [IsRSGraph]);

DeclareOperation("RSGraphSubgraph", [IsRSGraph, IsList]);
DeclareOperation("RSGraphSubgraphNC", [IsRSGraph, IsList]);

# Optional argument for bfs/dfs switch. 
DeclareOperation("RSGraphSpanningTree", [IsRSGraph]);
DeclareOperation("RSGraphSpanningTree", [IsRSGraph, IsString]);

