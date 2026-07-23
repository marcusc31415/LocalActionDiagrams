# This is loaded before the extension isomorphism files.
# The library can still be used if the isomorphism functions
# aren't available. If the isomorphism files are loaded
# then they set this variable to true. 
BindGlobal("LAD_IsomorphismAvailable@", false);

#! @Chapter RSGraphs
#!
#! An RSGraph is a graph <M>\Gamma = (V, A, o, r)</M> that consists of
#! a vertex set <M>V</M>, an arc set <M>A</M>, a map <M>o : A \to V</M>
#! which assigns each vertex to an <E>origin</E>, and a bijection 
#! <M>r : A \to A</M> which assigns each arc to a <E>reverse</E> arc. 
#! There is also a map <M>t := o \circ r</M> which assigns each
#! arc to a <E>terminus</E> vertex. Note the RSGraphs are allowed 
#! to contain loops and parallel arcs. 
#!
#! We created the RSGraph object for this package due to our use of graphs 
#! having a specialised definition. For example, every arc in an RSGraph
#! must have a reverse arc associated with it while a <E>Digraph</E> from
#! the <Package>Digraphs</Package> allows for arcs that do not have an
#! associated reverse arc. Creating a new object category for RSGraphs
#! allows for both better error checking when using RSGraphs and to avoid
#! conflicting definitions, such as the definition of a cycle for a 
#! Digraph and an RSGraph. We provide functionality to convert between an 
#! RSGraph and a <E>Graph</E> from the <Package>GRAPE</Package> package or 
#! a <E>Digraph</E> from the <Package>Digraphs</Package> package. 
#!
#! The name "RSGraph" arises from Reid and Smith, the authors of the local
#! action diagram paper. This name was chosen to avoid conflicting with the
#! <Package>GRAPE</Package> package. 


#! @Section Creating RSGraphs
#!
#! @Returns <K>true</K> if <A>graph</A> is of the category <C>IsRSGraph</C> and
#! false otherwise.
#! @Arguments graph
#! @Description
#! Every RSGraph object belongs to the category <C>IsRSGraph</C>. Furthermore, 
#! every RSGraph is an immutable attribute storing object. 
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
