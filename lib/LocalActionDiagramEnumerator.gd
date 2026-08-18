LAD_AvailableData@ := [];

# The files are stored as "{degree}_{no_verts}_library.txt".
# This stores a list of available ones in memory in the form
# [[degree, no_verts], ...]. 

for LAD_filename@ in DirectoryContents(DirectoriesPackageLibrary("localactiondiagrams", "data")[1]) do
	if LAD_filename@ = "." or LAD_filename@ = ".." then
		continue;
	fi;

	LAD_filename_split@ := SplitString(LAD_filename@, "_");
	Add(LAD_AvailableData@, [Int(LAD_filename_split@[1]), Int(LAD_filename_split@[2])]);
od;

LAD_AvailableData@ := Set(LAD_AvailableData@);

# Make the LAD_AvailableData@ variable constant and
# immutable so the sublists can't be changed. 
MakeImmutable(LAD_AvailableData@);
MakeConstantGlobal("LAD_AvailableData@");
Unbind(LAD_filename@);
Unbind(LAD_filename_split@);

BindGlobal("LAD_RSGraphsRecord@", rec());
BindGlobal("LAD_LocalActionDiagramsRecord@", rec());


#! @Chapter Enumeration
#! @Section Enumerating RSGraphs and Local Action Diagrams

#! @BeginGroup
#! @Returns A list of RSGraphs or a specific element from this list.
#! @Arguments degree, no_verts[, idx]
#! @Label
#! @Description This function returns a list of RSGraphs such that each vertex has degree less than or equal to
#! <A>degree</A> and the graph has exactly <A>no_verts</A> vertices. Each graph in the list is unique up to isomorphism.
#! If the optional argument <A>idx</A> is provided then it will return the RSGraph in that position of the list. This is
#! analogous to the <C>TransitiveGroup</C> function from the <Package>Transitive Groups</Package> package. 
#!
#! There are two ways this function can get the list of RSGraphs. If the data for the given degree and number of
#! vertices is stored on the disk then it will be read from the disk. If the data is not stored on the disk then the
#! RSGraph enumeration algorithm will be run instead. An information message is displayed if the enumeration algorithm
#! is used.
#!
#! Note that the <Package>Digraphs</Package> is required for the enumeration algorithm to run. If it is not loaded then
#! the function will return an error if it needs to run the enumeration algorithm. The <Package>Digraphs</Package> is 
#! NOT needed if the data is stored on the disk. 
DeclareOperation("RSGraphFromLibrary", [IsInt, IsInt]);
DeclareOperation("RSGraphFromLibrary", [IsInt, IsInt, IsInt]);
#! @EndGroup

#! @BeginExampleSession
#! gap> graph_list := RSGraphFromLibrary(3, 2);;
#! gap> graph := graph_list[4];
#! <RSGraph with 2 vertices and 3 arcs>
#! gap> graph_2 := RSGraphFromLibrary(3, 2, 4);;
#! gap> IsomorphismRSGraphs(graph, graph_2);
#! ()
#! @EndExampleSession

#! @BeginGroup
#! @Returns A list of local action diagrams or a specific element from this list.
#! @Arguments degree, no_verts[, idx]
#! @Label
#! @Description This function returns a list of local action diagrams such that each vertex label has degree equal to
#! <A>degree</A> and the graph has exactly <A>no_verts</A> vertices. Each local action diagram in the list is unique up
#! to isomorphism. If the optional argument <A>idx</A> is provided then it will return the local action diagram in that
#! position of the list. This is analogous to the <C>TransitiveGroup</C> function from the <Package>Transitive
#! Groups</Package> package. 
#!
#! There are two ways this function can get the list of local action diagrams. If the data for the given degree and
#! number of vertices is stored on the disk then it will be read from the disk. If the data is not stored on the disk
#! then the local action diagram enumeration algorithm will be run instead. An information message is displayed if the
#! enumeration algorithm is used.
#!
#! Note that the <Package>Digraphs</Package> is required for the enumeration algorithm to run. If it is not loaded then
#! the function will return an error if it needs to run the enumeration algorithm. The <Package>Digraphs</Package> is 
#! NOT needed if the data is stored on the disk. 
DeclareOperation("LocalActionDiagramFromLibrary", [IsInt, IsInt]);
DeclareOperation("LocalActionDiagramFromLibrary", [IsInt, IsInt, IsInt]);
#! @EndGroup

#! @BeginExampleSession
#! gap> lad_list := LocalActionDiagramFromLibrary(3, 2);;
#! gap> lad := lad_list[4];
#! <LocalActionDiagram with 2 vertices and 4 arcs>
#! gap> lad_2 := LocalActionDiagramFromLibrary(3, 2, 4);;
#! gap> IsomorphismLocalActionDiagrams(lad, lad_2);
#! [ (), (), rec( 1 := <mapping: Domain([ 1, 2, 3 ]) -> Domain([ 1, 2, 3 ]) >, 
#!       2 := <mapping: Domain([ 4, 5, 6 ]) -> Domain([ 4, 5, 6 ]) > ) ]
#! @EndExampleSession


#! @BeginGroup
#! @GroupTitle Number of RSGraphs/Local Action Diagrams
#! @Returns The number of RSGraphs or Local Action Diagrams with a specified degree and number of vertices. 
#! @Arguments degree, no_verts
#! @Label
#! @Description These functions returns the number of RSGraphs or Local Action Diagrams with degree <A>degree</A> and
#! <A>no_verts</A> vertices. They are a shorthand for <C>Size(RSGraphFromLibrary(<A>degree</A>, <A>no_verts</A>)</C> and
#! <C>Size(LocalActionDiagramFromLibrary(<A>degree</A>, <A>no_verts</A>)</C>. They are included to be analogous to the
#! <C>NrTransitiveGroups</C> function from th <Package>Transitive Groups</Package> library. 
DeclareOperation("NumberRSGraphs", [IsInt, IsInt]);
#! @Arguments degree, no_verts
DeclareOperation("NumberLocalActionDiagrams", [IsInt, IsInt]);
#! @EndGroup

#! @BeginExampleSession
#! gap> lad_list := LocalActionDiagramFromLibrary(3, 3);;
#! gap> Size(lad_list);
#! 78
#! gap> NumberLocalActionDiagrams(3, 3);
#! 78
#! @EndExampleSession

#! @Section Searching The Library

#! <ManSection>
#!     <Heading>AllRSGraphs/AllLocalActionDiagrams</Heading>
#!     <Oper Name="AllRSGraphs" Arg="[function_1, value_1, ...]" Label=""/>
#!     <Oper Name="AllLocalActionDiagrams" Arg="[function_1, value_1, ...]" Label=""/>
#!     <Returns>All RSGraphs or Local Action Diagrams satisfying the conditions given as arguments.</Returns>
#!     <Description>
#!         These functions return a list of RSGraphs or Local Action Diagrams satisfying the conditions specified. The
#!         functions take an even number of inputs which alternate between functions and values. Each function must have
#!         a single argument as input which is either an RSGraph or local action diagram. The output of <A>function_i</A>
#!         must be comparable with the equality operation to <A>value_i</A>. The functions are analogous to the
#!         <C>AllTransitiveGroups</C> function from the <Package>Transitive Groups</Package> package. 
#!
#!         These functions work by first starting with a list of every RSGraph or Local Action Diagram stored on the
#!         disk. Each function supplied as an argument is applied to every element of this list and the result of
#!         applying this function is compared with the provided value. If it matches the value then the element is kept
#!         in the list; otherwise it is discarded. After this process is done for every argument the final list is
#!         returned. Note that it can be an empty list if there are no elements satisfying all conditions. 
#!
#!         If any of the functions provided are properties or attribute of RSGraphs or Local Action Diagrams then these
#!         functions are prioritised for evaluation. This means that any function which is not a property or attribute
#!         will be evaluated after each property and attribute is. The reason for this is that properties and attributes
#!         Are known the by "easy" to calculate or are stored in the library data and so this can speed up the search by
#!         reducing the search space for the potentially "harder" functions. 
#!
#!         The first time one of these functions is called the entire library is read into the GAP sessions memory.
#!         Information messages are displayed during this process as this can take some time. Subsequent calls to this
#!         function do not need to read the library files. 
#!
#!         If no arguments are provided then a complete list of RSGraphs or local action diagrams is returned.
#!         Furthermore, every degree two RSGraph and local action diagram can be quickly manually constructed. By
#!         default, this function only includes those with up to ten vertices. This can be changed with the <Ref
#!         Func="SetLocalActionDiagramDegreeTwoVertexBound" Label=""/> function. 
#!     </Description>
#! </ManSection>
#!
#! @BeginLogSession
#! gap> TestFunction := function(lad)
#! >    	return LocalActionDiagramVertexLabels(lad).(1);
#! >    end;
#! function( lad ) ... end
#! gap> lad_list := AllLocalActionDiagrams(TestFunction, Group((1,2)), \ 
#! >                                       LocalActionDiagramIsUnimodular, \ 
#! >                                       true);
#! [ <LocalActionDiagram with 1 vertex and 1 arc>, 
#!   <LocalActionDiagram with 1 vertex and 6 arcs>, 
#!   <LocalActionDiagram with 1 vertex and 6 arcs>, 
#!   <LocalActionDiagram with 1 vertex and 6 arcs>, 
#!   <LocalActionDiagram with 1 vertex and 1 arc>, 
#!   <LocalActionDiagram with 2 vertices and 2 arcs>, 
#!   <LocalActionDiagram with 3 vertices and 4 arcs>, 
#!   <LocalActionDiagram with 4 vertices and 6 arcs>, 
#!   <LocalActionDiagram with 5 vertices and 8 arcs>, 
#!   <LocalActionDiagram with 6 vertices and 10 arcs>, 
#!   <LocalActionDiagram with 7 vertices and 12 arcs>, 
#!   <LocalActionDiagram with 8 vertices and 14 arcs>, 
#!   <LocalActionDiagram with 9 vertices and 16 arcs>, 
#!   <LocalActionDiagram with 10 vertices and 18 arcs> ]
#! gap> Print(lad_list[3]);
#! Vertices = { 1 }
#! Arcs = {
#! 	1 = ( origin = 1, terminus = 1, inverse = 1 )
#! 	2 = ( origin = 1, terminus = 1, inverse = 2 )
#! 	3 = ( origin = 1, terminus = 1, inverse = 3 )
#! 	4 = ( origin = 1, terminus = 1, inverse = 4 )
#! 	5 = ( origin = 1, terminus = 1, inverse = 6 )
#! 	6 = ( origin = 1, terminus = 1, inverse = 5 )
#! }
#! Reverse Map = (5,6)
#! Vertex Labels = {
#! 	1 = Group( [ (1,2) ] )
#! }
#! Arc Labels = {
#! 	1 = [ 1, 2 ]
#! 	2 = [ 3 ]
#! 	3 = [ 4 ]
#! 	4 = [ 5 ]
#! 	5 = [ 6 ]
#! 	6 = [ 7 ]
#! }
#! @EndLogSession

#! <ManSection>
#!     <Heading>OneRSGraph/OneLocalActionDiagram</Heading>
#!     <Oper Name="OneRSGraph" Arg="[function_1, value_1, ...]" Label=""/>
#!     <Oper Name="OneLocalActionDiagram" Arg="[function_1, value_1, ...]" Label=""/>
#!     <Returns>One RSGraphs or Local Action Diagrams satisfying the conditions given as arguments or <K>fail</K> if
#!     there are none satisfying them.</Returns>
#!     <Description>
#!         These functions are a shorthand for <C>AllRSGraphs([<A>function_1</A>, <A>function_2</A>, ...])[1]</C> and
#!         <C>AllLocalActionDiagrams([<A>function_1</A>, <A>function_2</A>, ...])[1]</C>. They are analogous to the
#!         <C>OneTransitiveGroup</C> function from the <Package>Transitive Groups</Package> package. Note that they do
#!         not offer any speed improvements to running the "<C>All</C>" variants of the function. 
#!     </Description>
#! </ManSection>


#! <ManSection>
#!     <Oper Name="SetLocalActionDiagramDegreeTwoVertexBound" Arg="bound" Label=""/>
#!     <Description>
#!         Since any RSGraph and Local action diagram of degree two can be quickly manually constructed there needs to
#!         be a bound on the amount to use for the search functions (<Ref Oper="AllRSGraphs" Label=""/>, <Ref
#!         Oper="AllLocalActionDiagrams" Label=""/>). By default this value is <C>10</C>. This can be changed using this
#!         function to <A>bound</A>. 
#!     </Description>
#! </ManSection>


#! <ManSection>
#!     <Oper Name="LocalActionDiagramDebugSearch" Arg="debug" Label=""/>
#!     <Description>
#!         If <A>debug</A> is <K>true</K> then if there is an error in the search functions (<Ref Oper="AllRSGraphs"
#!         Label=""/>, <Ref Oper="AllLocalActionDiagrams" Label=""/>) this will cause the error message to say which
#!         function and argument caused this error. It will not say what the error is. 
#!     </Description>
#! </ManSection>
