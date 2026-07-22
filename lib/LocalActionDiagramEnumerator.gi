BindGlobal("LAD_ReadLibraryData@",
function(degree, no_verts)
	local file_data, filename, file_path, file, data, string_list;

	filename := StringFormatted("{1}_{2}_library.txt.gz", degree, no_verts);
	#Info(InfoPerformance, 1, "Reading library data from disk.");
	file_path := Filename(DirectoriesPackageLibrary("localactiondiagrams", "data")[1], filename);

	#file := IO_FilteredFile([["gzip", ["-dc"]]], file_path, "r");
	file := IO_CompressedFile(file_path, "r");

	if file = fail then
		ErrorNoReturn(StringFormatted("Error reading file {1}.", filename));
	fi;

	data := IO_ReadUntilEOF(file);

	if data = fail then
		ErrorNoReturn(StringFormatted("Error reading file {1}.", filename));
	fi;

	IO_Close(file);

	string_list := SplitString(data, "\n");

	#Info(InfoPerformance, 1, "Finished reading library data from disk.");

	return string_list;

end);

BindGlobal("LAD_LibraryDataToMemory@",
function(degree, no_verts)
	local lad_list, data_list, rs_graph_list, rec_string, rs_graphs, seen_data, graph, graph_data;

	rec_string := StringFormatted("{1},{2}", degree, no_verts);

	data_list := LAD_ReadLibraryData@(degree, no_verts);

	# List of [lad, graph] pairs. 
	data_list := List(data_list, x -> LocalActionDiagramFromWritableString(x, true));

	rs_graph_list := List(data_list, x -> x[2]);
	lad_list := List(data_list, x -> x[1]);

	# Reduce the graph data up to isomorphism. Since graphs
	# are in canonical form we can check this with
	# equality. 
	rs_graphs := [];
	seen_data := [];

	for graph in rs_graph_list do
		graph_data := RSGraphAdjacencyMatrix(graph);

		if graph_data in seen_data then
			continue;
		fi;

		Add(seen_data, graph_data);
		Add(rs_graphs, graph);
	od;

	LAD_RSGraphsRecord@.(rec_string) := rs_graphs;
	LAD_LocalActionDiagramsRecord@.(rec_string) := lad_list;
end);


InstallMethod(RSGraphFromLibrary, "Return list of RSGraphs up to isomorphism.", [IsInt, IsInt],
function(degree, no_verts)
	local file_data, rs_graphs, rec_string, rs_graph_data_pairs, data_pair, idx, aut_groups, canon_labellings, seen_data, graph_data, graph;

	# Deal with special known cases first. 
	
	# There are no graphs with more than two vertices where
	# each vertex has degree 1. 
	if degree = 1 and no_verts > 2 then
		return [];
	fi;

	# There are exactly four in this case. We can construct them
	# manually.
	if degree = 2 and no_verts > 1 then
		rs_graphs := [];
		### Loop at one end and not the other. 
		arc_list := [[1, 1]];
		arc_no := 2;
		rev_map := ();
		for idx in [1..no_verts-1] do
			Add(arc_list, [idx, idx+1]);
			Add(arc_list, [idx+1, idx]);
			rev_map := rev_map * (arc_no, arc_no+1);
			arc_no := arc_no + 2;
		od;

		Add(rs_graphs, RSGraphByAdjacencyList(arc_list, rev_map));

		### Loop at both ends. 
		arc_list := StructuralCopy(arc_list);
		rev_map := StructuralCopy(rev_map);
		Add(arc_list, [no_verts, no_verts]);
		Add(rs_graphs, RSGraphByAdjacencyList(arc_list, rev_map));


		### Cycle. 
		arc_list := StructuralCopy(arc_list);
		rev_map := StructuralCopy(rev_map);

		# Remove the first and last arc (the two loops). 
		Remove(arc_list, 1);
		Remove(arc_list);
		# Connect the first and last vertices. 
		Add(arc_list, [1, no_verts], 2); # Put this in the second position. 
		Add(arc_list, [no_verts, 1]); 
		# Easier to reconstruct the reverse map. 
		rev_map := (1,3)*(2,Size(arc_list)); # Start with the ones in weird positions. 

		Assert(1, IsInt(Size(arc_list)/2));

		idx := 4;
		while idx < Size(arc_list) do
			rev_map := rev_map * (idx, idx+1);
			idx := idx + 2;
		od;



		Add(rs_graphs, RSGraphByAdjacencyList(arc_list, rev_map));

		### No loops and no cycle.
		arc_list := StructuralCopy(arc_list);
		rev_map := StructuralCopy(rev_map);

		# The second and last arcs connect the first and last vertex. 
		Remove(arc_list, 2);
		Remove(arc_list);
		# Remove the one between the two arcs we removed. 
		rev_map := rev_map * (2, Size(arc_list)+2); 

		# Conjugate the move every number greater than 1 down by 1. 
		shift_perm := CycleFromList(Reversed([2..Size(arc_list)+1]));
		rev_map := Inverse(shift_perm)*rev_map*shift_perm;

		Add(rs_graphs, RSGraphByAdjacencyList(arc_list, rev_map));


		return rs_graphs;

	fi;



	rec_string := StringFormatted("{1},{2}", degree, no_verts);

	if not [degree, no_verts] in LAD_AvailableData@ and not rec_string in RecNames(LAD_RSGraphsRecord@) then
		if not LAD_IsomorphismAvailable@ then
			ErrorNoReturn(StringFormatted("Data for degree={1} and number of vertices={2} not stored and isomorphism functions not available.\n", degree, no_verts), "Consider loading the digraphs package to run the enumeration algorithms.");
		fi;

		Info(InfoWarning, 1, StringFormatted("Data for degree={1} and number of vertices={2} not stored.", degree, no_verts));
		Info(InfoWarning, 1, "Runing the enumeration algorithms.");
		Info(InfoWarning, 1, "This could be slow to run.");

		rs_graphs := LAD_Internal_RSGraphsEnumerate@(degree, no_verts);

		LAD_RSGraphsRecord@.(rec_string) := rs_graphs;
	elif rec_string in RecNames(LAD_RSGraphsRecord@) then
		rs_graphs := LAD_RSGraphsRecord@.(rec_string);
	else
		LAD_LibraryDataToMemory@(degree, no_verts);
		rs_graphs := LAD_RSGraphsRecord@.(rec_string);
	fi;

	return rs_graphs;
end);

InstallMethod(RSGraphFromLibrary, "Return the nth RSGraph up to isomorphism.", [IsInt, IsInt, IsInt],
function(degree, no_verts, graph_number)
	local rs_graphs;

	rs_graphs := RSGraphFromLibrary(degree, no_verts);

	if graph_number < 1 or graph_number > Size(rs_graphs) then
		ErrorNoReturn(StringFormatted("Graph number must be between 1 and {1}.", Size(rs_graphs)));
	fi; 

	return rs_graphs[graph_number];
end);

InstallMethod(LocalActionDiagramFromLibrary, "Return list of Local Action Diagrams up to isomorphism.", [IsInt, IsInt],
function(degree, no_verts)
	local file_data, lad_list, rec_string, idx_x, idx_y, vertex_labels, arc_labels, graph, v_id, lad;

	# Deal with special known cases first. 
	
	# There are no graphs with more than two vertices where
	# each vertex has degree 1. 
	if degree = 1 and no_verts > 2 then
		return [];
	fi;

	# There are exactly four in this case. We can construct them
	# manually.
	if degree = 2 and no_verts > 1 then
		graph_list := RSGraphFromLibrary(degree, no_verts);	
		lad_list := [];

		# One loop. 

		vert_labels := List([1..no_verts-1], x -> Group(()));
		arc_labels := [];
		for idx in [1..Size(vert_labels)] do
			G := vert_labels[idx];
			domain := [1+2*(idx-1), 2+2*(idx-1)];
			SetPermGroupDomain(G, domain);

			# Single vertex sets for arc labels. 
			Add(arc_labels, [domain[1]]);
			Add(arc_labels, [domain[2]]);
		od;
		domain := domain+2; # For the last group.
		Add(vert_labels, Group((domain[1], domain[2])));
		Add(arc_labels, domain); # A single arc originating here. 

		Add(lad_list, LocalActionDiagramFromData(graph_list[1], vert_labels, arc_labels));


		# Two loops. 
		vert_labels := StructuralCopy(vert_labels);
		arc_labels := StructuralCopy(arc_labels);
		Remove(vert_labels); # Remove the last vertex and arc label. 
		Remove(arc_labels); 

		Add(vert_labels, Group(()));
		SetPermGroupDomain(Last(vert_labels), domain);
		Add(arc_labels, [domain[1]]);
		Add(arc_labels, [domain[2]]);

		Add(lad_list, LocalActionDiagramFromData(graph_list[2], vert_labels, arc_labels));

		# Cycle. 
		vert_labels := StructuralCopy(vert_labels);
		arc_labels := StructuralCopy(arc_labels);
		Add(lad_list, LocalActionDiagramFromData(graph_list[3], vert_labels, arc_labels));

		# No loops. 
		vert_labels := StructuralCopy(vert_labels);
		arc_labels := StructuralCopy(arc_labels);

		Remove(vert_labels, 1);
		Remove(vert_labels);
		Remove(arc_labels, 1);
		Remove(arc_labels, 1);
		Remove(arc_labels);
		Remove(arc_labels);

		Add(vert_labels, Group((1,2)), 1);
		Add(vert_labels, Group((domain[1], domain[2])));
		Add(arc_labels, [1, 2], 1);
		Add(arc_labels, domain);

		Add(lad_list, LocalActionDiagramFromData(graph_list[4], vert_labels, arc_labels));


		return lad_list;




	fi;


	rec_string := StringFormatted("{1},{2}", degree, no_verts);

	if not [degree, no_verts] in LAD_AvailableData@ and not rec_string in RecNames(LAD_LocalActionDiagramsRecord@) then
		if not LAD_IsomorphismAvailable@ then
			ErrorNoReturn(StringFormatted("Data for degree={1} and number of vertices={2} not stored and isomorphism functions not available.\n", degree, no_verts), "Consider loading the digraphs package to run the enumeration algorithms.");
		fi;

		Info(InfoWarning, 1, StringFormatted("Data for degree={1} and number of vertices={2} not stored.", degree, no_verts));
		Info(InfoWarning, 1, "Runing the enumeration algorithms.");
		Info(InfoWarning, 1, "This could be slow to run.");

		lad_list := LAD_Internal_LocalActionDiagramsEnumerate@(degree, no_verts);

		LAD_LocalActionDiagramsRecord@.(rec_string) := lad_list;
	elif rec_string in RecNames(LAD_LocalActionDiagramsRecord@) then
		lad_list := LAD_LocalActionDiagramsRecord@.(rec_string);
	else
		LAD_LibraryDataToMemory@(degree, no_verts);
		lad_list := LAD_LocalActionDiagramsRecord@.(rec_string);
	fi;

	return lad_list;
end);

InstallMethod(LocalActionDiagramFromLibrary, "Return the nth Local Action Diagram up to isomorphism.", [IsInt, IsInt, IsInt],
function(degree, no_verts, lad_number)
	local lad_list;

	lad_list := LocalActionDiagramFromLibrary(degree, no_verts);

	if lad_number < 1 or lad_number > Size(lad_list) then
		ErrorNoReturn(StringFormatted("Local action diagram number must be between 1 and {1}.", Size(lad_list)));
	fi; 

	return lad_list[lad_number];
end);

InstallMethod(NumberRSGraphs, "Returns the nubmer of RSGraphs with specified degree and vertex count.", [IsInt, IsInt],
function(degree, no_verts)
	return Size(RSGraphFromLibrary(degree, no_verts));
end);

InstallMethod(NumberLocalActionDiagrams, "Returns the nubmer of local action diagrams with specified degree and vertex count.", [IsInt, IsInt],
function(degree, no_verts)
	return Size(LocalActionDiagramFromLibrary(degree, no_verts));
end);

# In order from least to most. Search through the smaller ones first. 
BindGlobal("LAD_CheckOrder@", [[1, 1], [1, 2], [2, 1], [2, 2], [2, 3], [2, 4], [2, 5], [2, 6], [2, 7], [3, 1], [4, 1], [3, 2], [5, 1], [3, 3], [6, 1],
                               [4, 2], [7, 1], [3, 4], [8, 1], [5, 2], [3, 5], [9, 1], [4, 3], [10, 1], [11, 1], [12, 1], [13, 1]]);

BindGlobal("LAD_DebugSearch@", false);

BindGlobal("LocalActionDiagramsDebugSearch",
function(val)
	if val = true then
		MakeReadWriteGlobal("LAD_DebugSearch@LocalActionDiagrams");
		LAD_DebugSearch@ := true;
		MakeReadOnlyGlobal("LAD_DebugSearch@LocalActionDiagrams");
	elif val = false then
		MakeReadWriteGlobal("LAD_DebugSearch@LocalActionDiagrams");
		LAD_DebugSearch@ := false;
		MakeReadOnlyGlobal("LAD_DebugSearch@LocalActionDiagrams");
	else
		ErrorNoReturn("Argument must be \"true\" or \"false\".");
	fi;
end);


BindGlobal("AllRSGraphs", 
function(args...)
	local candidate_graphs, idx, new_graphs, Func, val, graph, new_args_order, easy_args, hard_args, ret, degree, no_verts, rec_string, read_from_disc, positions, unique_certs, canon_certs;

	if Length(args) mod 2 <> 0 then
		ErrorNoReturn("Arugments must be of the form [function1, output1, ...].");
	fi;

	for idx in [1..Length(args)/2] do
		if not IsFunction(args[2*idx-1]) then
			ErrorNoReturn(StringFormatted("Argument {1} must be a function.", idx));
		fi;
	od;

	# Order the easy (pre-computed) functions first.
	easy_args := [];
	hard_args := [];

	for idx in [1..Length(args)/2] do
		if args[2*idx-1] in [RSGraphVertices, RSGraphNumberVertices, RSGraphArcs, RSGraphArcIDs, RSGraphNumberArcs, RSGraphReverseMap, RSGraphAdjacencyMatrix, \
		                     RSGraphOutNeighbours, RSGraphInNeighbours, RSGraphOutArcs, RSGraphInArcs, RSGraphIsCycle, AutomorphismGroup, RSGraphCanonicalLabelling, \
							 RSGraphMaximumDegree] then
			Add(easy_args, args[2*idx-1]);
			Add(easy_args, args[2*idx]);
		else
			Add(hard_args, args[2*idx-1]);
			Add(hard_args, args[2*idx]);
		fi;
	od;

	new_args_order := Concatenation(easy_args, hard_args);
	
	
	candidate_graphs := [];
	for idx in [1..Length(LAD_CheckOrder@)] do
		degree := LAD_CheckOrder@[idx][1];
		no_verts := LAD_CheckOrder@[idx][2];
		rec_string := StringFormatted("{1},{2}", degree, no_verts);

		read_from_disc := false;

		if not rec_string in RecNames(LAD_RSGraphsRecord@) then
			Info(InfoPerformance, 1, StringFormatted("Reading library data for degree={1}, number vertices={2}.", degree, no_verts));
			read_from_disc := true;
		fi;
		candidate_graphs := Concatenation(candidate_graphs, RSGraphFromLibrary(LAD_CheckOrder@[idx][1], LAD_CheckOrder@[idx][2]));
		if read_from_disc then
			Info(InfoPerformance, 1, StringFormatted("Finished reading library data for degree={1}, number vertices={2}.", degree, no_verts));
		fi;
	od;

	# Reduce candidate graphs up to isomorphism (based on the canonical certificates). 
	canon_certs := List(candidate_graphs, graph -> RSGraphCanonicalLabelling(graph).canon_certificate);
	unique_certs := Set(canon_certs);
	positions := List(unique_certs, cert -> Position(canon_certs, cert)); # Get first occurrence of each certificate. 

	candidate_graphs := candidate_graphs{positions}; # Reduces up to isomorphism. 

	new_graphs := [];

	for idx in [1..Length(new_args_order)/2] do
		Func := new_args_order[2*idx-1];
		val := new_args_order[2*idx];

		for graph in candidate_graphs do
			if LAD_DebugSearch@ = true then
				BreakOnError := false;
				SilentNonInteractiveErrors := true;

				ret := CALL_WITH_CATCH(Func, [graph]);

				BreakOnError := true;
				SilentNonInteractiveErrors := false;
			else
				ret := [true, Func(graph)];
			fi; 

			if ret[1] = false then
				ErrorNoReturn(StringFormatted("Error in function \"{1}\" (argument {2}).", NameFunction(Func), 2*idx-1));
			fi;

			if ret[2] = val then
				Add(new_graphs, graph);
			fi;
		od;

		candidate_graphs := new_graphs;
		new_graphs := [];
	od;

	return candidate_graphs;
end);

BindGlobal("OneRSGraph", 
function(args...)
	local all_graphs;

	all_graphs := CallFuncList(AllRSGraphs, args);

	if Size(all_graphs) = 0 then
		return fail;
	else
		return all_graphs[1];
	fi;
end);

BindGlobal("AllLocalActionDiagrams", 
function(args...)
	local candidate_lads, idx, new_lads, Func, val, lad, new_args_order, easy_args, hard_args, ret, degree, no_verts, rec_string, read_from_disc;

	if Length(args) mod 2 <> 0 then
		ErrorNoReturn("Arugments must be of the form [function1, output1, ...].");
	fi;

	for idx in [1..Length(args)/2] do
		if not IsFunction(args[2*idx-1]) then
			ErrorNoReturn(StringFormatted("Argument {1} must be a function.", idx));
		fi;
	od;

	# Order the easy (pre-computed) functions first.
	easy_args := [];
	hard_args := [];

	# Force discrete before uniscalar before unimodular? 
	for idx in [1..Length(args)/2] do
		if args[2*idx-1] in [LocalActionDiagramArcLabels, LocalActionDiagramArcs, LocalActionDiagramGroupName, LocalActionDiagramGroupType, LocalActionDiagramInArcs, \
		                     LocalActionDiagramInNeighbours, LocalActionDiagramOutArcs, LocalActionDiagramOutNeighbours, LocalActionDiagramIsDiscrete, \
							 LocalActionDiagramIsUnimodular, LocalActionDiagramIsUniscalar, LocalActionDiagramRSGraph, LocalActionDiagramReverseMap, \
							 LocalActionDiagramScopos, LocalActionDiagramVertexLabels, LocalActionDiagramVertices, LocalActionDiagramRegularTree, \
							 LocalActionDiagramNumberVertices, LocalActionDiagramNumberArcs] then
			Add(easy_args, args[2*idx-1]);
			Add(easy_args, args[2*idx]);
		else
			Add(hard_args, args[2*idx-1]);
			Add(hard_args, args[2*idx]);
		fi;
	od;

	new_args_order := Concatenation(easy_args, hard_args);
	
	candidate_lads := [];
	for idx in [1..Length(LAD_CheckOrder@)] do
		degree := LAD_CheckOrder@[idx][1];
		no_verts := LAD_CheckOrder@[idx][2];
		rec_string := StringFormatted("{1},{2}", degree, no_verts);

		read_from_disc := false;

		if not rec_string in RecNames(LAD_RSGraphsRecord@) then
			Info(InfoPerformance, 1, StringFormatted("Reading library data for degree={1}, number vertices={2}.", degree, no_verts));
			read_from_disc := true;
		fi;
		candidate_lads := Concatenation(candidate_lads, LocalActionDiagramFromLibrary(LAD_CheckOrder@[idx][1], LAD_CheckOrder@[idx][2]));
		if read_from_disc then
			Info(InfoPerformance, 1, StringFormatted("Finished reading library data for degree={1}, number vertices={2}.", degree, no_verts));
		fi;
	od;

	new_lads := [];

	for idx in [1..Length(new_args_order)/2] do
		Func := new_args_order[2*idx-1];
		val := new_args_order[2*idx];

		for lad in candidate_lads do
			if LAD_DebugSearch@ = true then
				BreakOnError := false;
				SilentNonInteractiveErrors := true;

				ret := CALL_WITH_CATCH(Func, [lad]);

				BreakOnError := true;
				SilentNonInteractiveErrors := false;
			else
				ret := [true, Func(lad)];
			fi; 

			if ret[1] = false then
				ErrorNoReturn(StringFormatted("Error in function \"{1}\" (argument {2}).", NameFunction(Func), 2*idx-1));
			fi;

			if ret[2] = val then
				Add(new_lads, lad);
			fi;
		od;

		candidate_lads := new_lads;
		new_lads := [];
	od;

	return candidate_lads;
end);

BindGlobal("OneLocalActionDiagram", 
function(args...)
	local all_lads;

	all_lads := CallFuncList(AllLocalActionDiagrams, args);

	if Size(all_lads) = 0 then
		return fail;
	else
		return all_lads[1];
	fi;
end);

# Add special "domain" and "degree" functions? 

# AllLocalActionDiagrams(func list)
# OneLocalActionDiagram(func list)
