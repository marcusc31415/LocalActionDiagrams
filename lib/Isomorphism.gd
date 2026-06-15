#DeclareAttribute("AutomorphismGroup", IsRSGraph);

DeclareAttribute("LAD_RSGraphNonReverseAutomorphisms@", IsRSGraph);

DeclareOperation("IsomorphismRSGraphs", [IsRSGraph, IsRSGraph]);

DeclareOperation("RSGraphVertexAutomorphism", [IsRSGraph, IsPerm]);

DeclareOperation("RSGraphsVertexIsomorphism", [IsRSGraph, IsRSGraph, IsPerm]);

DeclareOperation("RSGraphsVertexIsomorphism", [IsRSGraph, IsRSGraph, IsGeneralMapping]);

DeclareOperation("RSGraphsIsomorphismsIterator", [IsRSGraph, IsRSGraph]);

#DeclareAttribute("RSGraphCanonicalLabelling", IsRSGraph);

DeclareAttribute("RSGraphCanonicalCertificate", IsRSGraph);

DeclareOperation("IsIsomorphicRSGraphs", [IsRSGraph, IsRSGraph]);

DeclareOperation("IsomorphismLocalActionDiagrams", [IsLocalActionDiagram, IsLocalActionDiagram]);

#DeclareOperation("LAD_Internal_AllLocalActionDiagrams@", [IsInt, IsInt]);

#DeclareOperation("LAD_Internal_AllRSGraphs@", [IsInt, IsInt]);
