BindGlobal("LAD_ReadLibraryData@",
function(degree, no_verts)
	local file_data, filename, file_path, file, data, string_list;

	filename := StringFormatted("{1}_{2}_library.txt.gz", degree, no_verts);
	Info(InfoPerformance, 1, "Reading library data from disk.");
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

	Info(InfoPerformance, 1, "Finished reading library data from disk.");

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


InstallMethod(AllRSGraphs, "Return list of RSGraphs up to isomorphism.", [IsInt, IsInt],
function(degree, no_verts)
	local file_data, rs_graphs, rec_string, rs_graph_data_pairs, data_pair, idx, aut_groups, canon_labellings, seen_data, graph_data, graph;

	# Deal with special known cases first. 
	
	# There are no graphs with more than two vertices where
	# each vertex has degree 1. 
	if degree = 1 and no_verts > 2 then
		return [];
	fi;


	rec_string := StringFormatted("{1},{2}", degree, no_verts);

	if not [degree, no_verts] in LAD_AvailableData@ and not rec_string in RecNames(LAD_RSGraphsRecord@) then
		if not LAD_IsomorphismAvailable@ then
			ErrorNoReturn(StringFormatted("Data for degree={1} and number of vertices={2} not stored and isomorphism functions not available.\n", degree, no_verts), "Consider loading the digraphs package to run the enumeration algorithms.");
		fi;

		Info(InfoWarning, 1, StringFormatted("Data for degree={1} and number of vertices={2} not stored.", degree, no_verts));
		Info(InfoWarning, 1, "Runing the enumeration algorithms.");
		Info(InfoWarning, 1, "This could be slow to run.");

		rs_graphs := LAD_Internal_AllRSGraphs@(degree, no_verts);

		LAD_RSGraphsRecord@.(rec_string) := rs_graphs;
	elif rec_string in RecNames(LAD_RSGraphsRecord@) then
		rs_graphs := LAD_RSGraphsRecord@.(rec_string);
	else
		LAD_LibraryDataToMemory@(degree, no_verts);
		rs_graphs := LAD_RSGraphsRecord@.(rec_string);
	fi;

	return rs_graphs;
end);

InstallMethod(AllRSGraphs, "Return the nth RSGraph up to isomorphism.", [IsInt, IsInt, IsInt],
function(degree, no_verts, graph_number)
	local rs_graphs;

	rs_graphs := AllRSGraphs(degree, no_verts);

	if graph_number < 1 or graph_number > Size(rs_graphs) then
		ErrorNoReturn(StringFormatted("Graph number must be between 1 and {1}.", Size(rs_graphs)));
	fi; 

	return rs_graphs[graph_number];
end);

InstallMethod(AllLocalActionDiagrams, "Return list of Local Action Diagrams up to isomorphism.", [IsInt, IsInt],
function(degree, no_verts)
	local file_data, lad_list, rec_string, idx_x, idx_y, vertex_labels, arc_labels, graph, v_id, lad;

	# Deal with special known cases first. 
	
	# There are no graphs with more than two vertices where
	# each vertex has degree 1. 
	if degree = 1 and no_verts > 2 then
		return [];
	fi;


	rec_string := StringFormatted("{1},{2}", degree, no_verts);

	if not [degree, no_verts] in LAD_AvailableData@ and not rec_string in RecNames(LAD_LocalActionDiagramsRecord@) then
		if not LAD_IsomorphismAvailable@ then
			ErrorNoReturn(StringFormatted("Data for degree={1} and number of vertices={2} not stored and isomorphism functions not available.\n", degree, no_verts), "Consider loading the digraphs package to run the enumeration algorithms.");
		fi;

		Info(InfoWarning, 1, StringFormatted("Data for degree={1} and number of vertices={2} not stored.", degree, no_verts));
		Info(InfoWarning, 1, "Runing the enumeration algorithms.");
		Info(InfoWarning, 1, "This could be slow to run.");

		lad_list := LAD_Internal_AllLocalActionDiagrams@(degree, no_verts);

		LAD_LocalActionDiagramsRecord@.(rec_string) := lad_list;
	elif rec_string in RecNames(LAD_LocalActionDiagramsRecord@) then
		lad_list := LAD_LocalActionDiagramsRecord@.(rec_string);
	else
		LAD_LibraryDataToMemory@(degree, no_verts);
		lad_list := LAD_LocalActionDiagramsRecord@.(rec_string);
	fi;

	return lad_list;
end);

InstallMethod(AllLocalActionDiagrams, "Return the nth Local Action Diagram up to isomorphism.", [IsInt, IsInt, IsInt],
function(degree, no_verts, lad_number)
	local lad_list;

	lad_list := AllLocalActionDiagrams(degree, no_verts);

	if lad_number < 1 or lad_number > Size(lad_list) then
		ErrorNoReturn(StringFormatted("Local action diagram number must be between 1 and {1}.", Size(lad_list)));
	fi; 

	return lad_list[lad_number];
end);

InstallMethod(NumberRSGraphs, "Returns the nubmer of RSGraphs with specified degree and vertex count.", [IsInt, IsInt],
function(degree, no_verts)
	return Size(AllRSGraphs(degree, no_verts));
end);

InstallMethod(NumberLocalActionDiagrams, "Returns the nubmer of local action diagrams with specified degree and vertex count.", [IsInt, IsInt],
function(degree, no_verts)
	return Size(AllLocalActionDiagrams(degree, no_verts));
end);

