#! @Chapter IO Operations and Visualisation
#! @Section IO Operations

#! @BeginGroup 
#! @GroupTitle Writing To A String
#!
#! @Returns A String representation of the object. 
#! @Arguments graph
DeclareOperation("RSGraphToWritableString", [IsRSGraph]);

#! @Arguments lad
#! @Description These two functions turn an RSGraph or local action diagram into a string representation. The exact
#! format of the representations are described in (APPENDIX REFERENCE). These functions are not designed to store the
#! objects in the most space-efficient format. The formats are designed so that the objects can be read into a GAP
#! session with very little overhead and so that the objects and known attributes can be recreated in a new GAP session.
#!
#! Note that these functions do not perform any disk IO operations themselves. They return a string in the GAP session and
#! it is up to the user to chose how disk IO operations with the string are done. 
#!
#! For an RSGraph the data that is always stored is:
#! - the vertex ids,
#! - the arc ids,
#! - the adjacency list, and
#! - the reverse map.
#! 
#! If the canonical labelling or automorphism group (REF FUNCTIONS) of the RSGraph are known then they are also stored. 
#!
#! For a local action diagram that data that is always stored is:
#! - the diagrams RSGraph (and all data known about it),
#! - the vertex labels, and
#! - the arc labels.
#!
#! The data stored if it is known is:
#! - the diagrams scopos, 
#! - the corresponding group type,
#! - if the corresponding group is discrete,
#! - if the corresponding group is uniscalar,
#! - if the corresponding group is unimodular, and
#! - the name of the corresponding group. 
DeclareOperation("LocalActionDiagramToWritableString", [IsLocalActionDiagram]);

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList([[1, 1]], ());;
#! gap> RSGraphToWritableString(graph);
#! "1|1,1,1|P"
#!
#! gap> lad := LocalActionDiagramFromData(graph, [Group((1,2))], [[1,2]]);;
#! gap> LocalActionDiagramGroupType(lad);;
#! gap> LocalActionDiagramToWritableString(lad);
#! "1|1,1,1|P/1:P2,1:1,2|1:1,2|LocalActionDiagramGroupType!General|LocalActionDiagramScopos! "
#! @EndExampleSession

#! @EndGroup


#! @BeginGroup 
#! @GroupTitle Reading From A String

#! @Returns The object(s) the string represents.
#! @Arguments string
DeclareOperation("RSGraphFromWritableString", [IsString]);

#! @Arguments string[, return_graph]
#! @Description These function take the output from the <C>RSGraphToWritableString</C> and
#! <C>LocalActionDiagramToWritableString"</C> methods as input and returns the objects those strings represents. While
#! it is possible to edit these strings outside of GAP these functions do not perform error checking and so this can
#! potentially lead to invalid RSGraph or local action diagram objects. If the optional <A>return_graph</A> parameter is
#! set to true then the <C>LocalActionDiagramFromWritableString</C> function will return a list whose first entry is the
#! local action diagram object and second intro is the RSGraph for this local action diagram. 
#!
#! Any optional attributes known about the objects at the time of writing will be stored in the objects returned by
#! these functions without needing to be recomputed. Note that the (REF canon and aut functions) attributes can be
#! stored in the objects created by these functions without needing the <Package>Digraphs</Package> to be loaded even
#! though computing these attributes required the <Package>Digraphs</Package> to be loaded. 
DeclareOperation("LocalActionDiagramFromWritableString", [IsString, IsBool]);
DeclareOperation("LocalActionDiagramFromWritableString", [IsString]);


#! @BeginExampleSession
#! gap> graph_string := "1|1,1,1|P";;
#! gap> graph := RSGraphFromWritableString(graph_string);;
#! gap> Print(graph);
#! Vertices = { 1 }
#! Arcs = {
#!     1 = ( origin = 1, terminus = 1, inverse = 1 )
#! }
#! Reverse Map = ()
#!
#! gap> lad_string := "1|1,1,1|P/1:P2,1:1,2|1:1,2|LocalActionDiagramGroupType!General|LocalActionDiagramScopos! ";;
#! gap> lad := LocalActionDiagramFromWritableString(lad_string);
#! gap> Print(lad);
#! Vertices = { 1 }
#! Arcs = {
#!     1 = ( origin = 1, terminus = 1, inverse = 1 )
#! }
#! Reverse Map = ()
#! Vertex Labels = {
#!     1 = Group( [ (1,2) ] )
#! }
#! Arc Labels = {
#!     1 = [ 1, 2 ]
#! }
#! gap> Print("LocalActionDiagramScopos" in KnownAttributesOfObject(lad));
#! true
#! gap> Print("LocalActionDiagramIsDiscrete" in KnownAttributesOfObject(lad));
#! false
#! @EndExampleSession


#! @EndGroup


#! <ManSection>
#!     <Heading>IO Pickling/Unpickling</Heading>
#!     <Oper Name="IO_Pickle" Arg="[file, ]graph" Label="for [an IO File and] an RSGraph"/>
#!     <Oper Name="IO_Pickle" Arg="[file, ]lad" Label="for [an IO File and] a Local Action Diagram"/>
#!     <Returns><K>IO_OK</K> or the pickle string if the pickling was successful or <K>IO_ERROR</K> if there was an error.</Returns>
#!     <Oper Name="IO_Unpickle" Arg="file" Label="for an IO File or String"/>
#!     <Returns>An RSGraph or Local Action Diagram object if successful or <K>IO_ERROR</K> if there was an error.</Returns>
#!     <Description>
#!         The IO package supports serialising data through the <C>IO_Pickle</C> function. Unlike the
#!         <C>RSGraphToWritableString</C> and <C>LocalActionDiagramToWritableString</C> functions these functions
#!         support recreating an arbitrary structure within the memory of a GAP session (assuming that the pickling
#!         functions have been defined for each object). In particular, it can easily recreate data structures storing
#!         objects. 
#!
#!         For example, if <C>l</C> is a list of RSGraphs then you can write this list to the disk with a single call to
#!         <C>IO_Pickle</C>. This generality comes at the cost of performance. For this reason, if you have a large
#!         collection of RSGraph and local action diagram objects that you need writing to the disk we recommend using
#!         the <C>RSGraphToWritableString</C> and <C>LocalActionDiagramToWritableString</C> functions. 
#!
#!		   In the one argument version for <C>IO_Pickle</C>, the pickle string for the object is returned if the
#!		   pickling was successful. In the two argument version, an IO file opened to write mode is supplied and the
#!		   pickle string is written to this file. 
#!
#!		   The <C>IO_Unpickle</C> function accepts either the pickle string as input or an IO file opened to read mode
#!		   which stores pickled objects. It recreates the objects in memory exactly. 
#!
#!		   As a note for developers, the RSGraph and local action diagram objects are pickled with the "magic values"
#!		   <C>RSGO</C> and <C>LADO</C> respectively.  
#!     </Description>
#! </ManSection>

#! @BeginExampleSession
#! gap> graph := RSGraphByAdjacencyList([[1, 1]], ());;
#! gap> lad := LocalActionDiagramFromData(graph, [Group((1,2))], [[1,2]]);;
#! gap> list := [graph, lad];
#! [ <RSGraph with 1 vertex and 1 arc>, <LocalActionDiagram with 1 vertex and 1 arc> ]
#! gap> file := IO_File("test.pickle", "w");;
#! gap> IO_Pickle(file, list);
#! IO_OK
#! gap> IO_Close(file);;
#! gap> file := IO_File("test.pickle", "r");;
#! gap> IO_Unpickle(file);
#! [ <RSGraph with 1 vertex and 1 arc>, <LocalActionDiagram with 1 vertex and 1 arc> ]
#! @EndExampleSession



#! @Section Visualisation
