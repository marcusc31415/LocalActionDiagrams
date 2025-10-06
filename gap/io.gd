###############################
### Input/Output Operations ###
###############################

#! @Chapter Introduction 
#! @Chapter Creating Local Action Diagrams 
#! @Chapter Local Action Diagram Attributes and Operations
#! @Chapter Input/Output and Visualisation 
#! @Section Reading and Writing Local Action Diagrams To Files
#!
#! Local action diagrams made in GAP can be written to a file. They can also be read from the file into GAP. The format of these files are as follows:
#!
#! @BeginExampleSession
#! <Digraph String>
#! <Vertex Labels>
#! <Edge Labels>
#! <Edge Reversal Map>
#! <List of Other Attributes>
#! @EndExampleSession
#!
#! Multiple local action diagrams can be stored in the same file.

#! @BeginGroup Writing Local Action Diagrams
#! @GroupTitle Writing Local Action Diagrams
#!
#! @Returns <K>true</K> if writing to the file was successful and <K>false</K> otherwise. 
#! @Arguments lad, filename[, directory]
DeclareOperation("WriteLocalActionDiagram", [IsLocalActionDiagram, IsString]);

#! @Returns <K>true</K> if writing to the file was successful and raises an error and returns <K>fail</K> otherwise. 
#! @Arguments lad_list, filename[, directory]
#! @Description
#! Writes the local action diagram <A>lad</A> or every local action diagram in the list <A>last_list</A> to the file <C><A>filename</A>.lad</C>. The list <A>lad_list</A> must be a dense list of local action diagrams. 
#!
#! If the optional argument <A>directory</A> is specified then the file is written to that directory. Otherwise the file is written to the current directory of the GAP session --- i.e. <C>DirectoryCurrent()</C>. The argument <A>directory</A> can either be in the category <C>IsDirectory</C> or <C>IsString</C>.
#!
#! If the file <C><A>filename</A>.lad</C> does not exist in the directory then it is create; otherwise the file is appended to. 
DeclareOperation("WriteLocalActionDiagram", [IsList, IsString]);

#! @EndGroup

DeclareOperation("WriteLocalActionDiagram", [IsLocalActionDiagram, IsString, IsDirectory]);
DeclareOperation("WriteLocalActionDiagram", [IsLocalActionDiagram, IsString, IsString]);
DeclareOperation("WriteLocalActionDiagram", [IsList, IsString, IsDirectory]);
DeclareOperation("WriteLocalActionDiagram", [IsList, IsString, IsString]);

#! @BeginGroup Reading Local Action Diagrams
#! @GroupTitle Reading Local Action Diagrams
#!
#! @Returns A list of local action diagrams if reading the file was successful or raises an error and returns <K>fail</K> otherwise.
#! @Arguments filename[, directory]
#! @Description
#! Reads the file <A>filename</A>. If the optional argument <A>directory</A> is specified then the file is read from that directory. Otherwise the file is read from the current directory of the GAP session --- i.e. <C>DirectoryCurrent()</C>. The argument <A>directory</A> can either be in the category <C>IsDirectory</C> or <C>IsString</C>.
#!
#! It is expected that the file was written with the <C>WriteLocalActionDiagram</C> function. Some basic error checking of the file is implemented as it is read but this will not catch all errors in the file. 
DeclareOperation("ReadLocalActionDiagram", [IsString]);

#! @EndGroup

DeclareOperation("ReadLocalActionDiagram", [IsString, IsDirectory]);
DeclareOperation("ReadLocalActionDiagram", [IsString, IsString]);

#! @Section Visualising Local Action Diagrams and <M>\Delta</M>-trees. 
#! Stuff goes here soon. 

