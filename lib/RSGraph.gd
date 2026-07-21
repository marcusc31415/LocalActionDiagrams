# This is loaded before the extension isomorphism files.
# The library can still be used if the isomorphism functions
# aren't available. If the isomorphism files are loaded
# then they set this variable to true. 
BindGlobal("LAD_IsomorphismAvailable@", false);

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
DeclareAttribute("RSGraphArcIDs", IsRSGraph);
DeclareAttribute("RSGraphReverseMap", IsRSGraph);
DeclareAttribute("RSGraphAdjacencyMatrix", IsRSGraph);
DeclareAttribute("RSGraphMG5String", IsRSGraph);
DeclareAttribute("RSGraphOutNeighbours", IsRSGraph);
DeclareAttribute("RSGraphInNeighbours", IsRSGraph);
DeclareAttribute("RSGraphOutArcs", IsRSGraph);
DeclareAttribute("RSGraphInArcs", IsRSGraph);
DeclareAttribute("RSGraphBipartition", IsRSGraph);
DeclareAttribute("RSGraphMaximumDegree", IsRSGraph);
DeclareProperty("RSGraphIsCycle", IsRSGraph);
DeclareProperty("RSGraphIsBipartite", IsRSGraph);
DeclareProperty("RSGraphHasParallelArcs", IsRSGraph);
DeclareAttribute("RSGraphMaximumDegree", IsRSGraph);

# Input/Output Format
DeclareOperation("RSGraphFromMG5String", [IsString]);

# [id, record]
DeclareOperation("RSGraphArcIterator", [IsRSGraph]);

DeclareOperation("RSGraphSubgraph", [IsRSGraph, IsList]);
DeclareOperation("RSGraphSubgraphNC", [IsRSGraph, IsList]);

DeclareOperation("RSGraphToStandardForm", [IsRSGraph]);

# Optional argument for bfs/dfs switch. 
DeclareOperation("RSGraphSpanningTree", [IsRSGraph]);
DeclareOperation("RSGraphSpanningTree", [IsRSGraph, IsString]);

DeclareAttribute("AutomorphismGroup", IsRSGraph);
DeclareAttribute("RSGraphCanonicalLabelling", IsRSGraph);

DeclareOperation("LAD_Internal_RSGraphsEnumerate@", [IsInt, IsInt]);
