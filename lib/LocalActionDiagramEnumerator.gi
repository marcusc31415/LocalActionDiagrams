BindGlobal("LAD_ReadLibraryData@",
function(degree, no_verts)
	local file_data, filename, file_path, file, data;

	filename := StringFormatted("{1}_{2}_library.pickle", degree, no_verts);
	file_path := Filename(DirectoriesPackageLibrary("localactiondiagrams", "data")[1], filename);

	file := IO_File(file_path);

	if file = fail then
		ErrorNoReturn(StringFormatted("Error reading file {1}.", filename));
	fi;

	data := IO_Unpickle(file);

	if data = IO_Error then
		ErrorNoReturn(StringFormatted("Error unpickling file {1}.", filename));
	fi;

	return data;

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
		file_data := LAD_ReadLibraryData@(degree, no_verts);

		rs_graph_data_pairs := file_data.("graphs");
		aut_groups := file_data.("graph_automorphism_group");
		canon_labellings := file_data.("graph_canonical_labelling");

		# Reduce the data up to isomorphism. Since graphs
		# are in canonical form we can check this with
		# equality. 
		rs_graphs := [];
		seen_data := [];

		for idx in [1..Size(rs_graph_data_pairs)] do
			graph_data := rs_graph_data_pairs[idx];

			if graph_data in seen_data then
				continue;
			fi;

			Add(seen_data, graph_data);

			graph := RSGraphByAdjacencyMatrix(graph_data[1], graph_data[2]);
			SetAutomorphismGroup(graph, aut_groups[idx]);
			SetRSGraphCanonicalLabelling(graph, canon_labellings[idx]);

			Add(rs_graphs, graph);
		od;

		LAD_RSGraphsRecord@.(rec_string) := rs_graphs;
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
		file_data := LAD_ReadLibraryData@(degree, no_verts);
		lad_list := [];

		for idx_x in [1..file_data.("number_lads")] do
			graph := RSGraphByAdjacencyMatrix(file_data.("graphs")[idx_x][1], file_data.("graphs")[idx_x][2]);
			for v_id in RecNames(file_data.("vertex_labels")[idx_x]) do
				SetPermGroupDomain(file_data.("vertex_labels")[idx_x].(Int(v_id)), file_data.("perm_group_domains")[idx_x].(Int(v_id)));
			od;
			vertex_labels := file_data.("vertex_labels")[idx_x];
			arc_labels := file_data.("arc_labels")[idx_x];

			lad := LocalActionDiagramFromData(graph, vertex_labels, arc_labels);

			SetLocalActionDiagramScopos(lad, file_data.("scopos")[idx_x]);
			SetLocalActionDiagramGroupType(lad, file_data.("group_type")[idx_x]);
			SetLocalActionDiagramIsDiscrete(lad, file_data.("is_discrete")[idx_x]);
			SetLocalActionDiagramIsUniscalar(lad, file_data.("is_uniscalar")[idx_x]);
			SetLocalActionDiagramIsUnimodular(lad, file_data.("is_unimodular")[idx_x]);

			Add(lad_list, lad);
		od;

		LAD_LocalActionDiagramsRecord@.(rec_string) := lad_list;
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

