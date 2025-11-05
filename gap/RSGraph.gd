# Category
DeclareCategory("IsRSGraph", IsObject);

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

