DeclareAttribute("AutomorphismGroup", IsRSGraph);

DeclareOperation("IsomorphismRSGraphs", [IsRSGraph, IsRSGraph]);

DeclareOperation("RSGraphVertexAutomorphism", [IsRSGraph, IsPerm]);

DeclareOperation("RSGraphsVertexIsomorphism", [IsRSGraph, IsRSGraph, IsPerm]);

DeclareOperation("RSGraphsVertexIsomorphism", [IsRSGraph, IsRSGraph, IsGeneralMapping]);

DeclareOperation("RSGraphsIsomorphismsIterator", [IsRSGraph, IsRSGraph]);

DeclareAttribute("RSGraphCanonicalLabellingMap", IsRSGraph);

DeclareAttribute("RSGraphCanonicalCertificate", IsRSGraph);

DeclareOperation("IsIsomorphicRSGraphs", [IsRSGraph, IsRSGraph]);

DeclareOperation("IsomorphismLocalActionDiagrams", [IsLocalActionDiagram, IsLocalActionDiagram]);

DeclareOperation("AllLocalActionDiagrams", [IsInt, IsInt]);

DeclareOperation("AllRSGraphs", [IsInt, IsInt]);
