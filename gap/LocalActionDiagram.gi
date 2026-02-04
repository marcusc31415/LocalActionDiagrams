# Family
BindGlobal("LocalActionDiagramFamily", NewFamily("LocalActionDiagramFamily", IsLocalActionDiagram));

# Type
BindGlobal("LocalActionDiagramType", NewType(LocalActionDiagramFamily, IsLocalActionDiagramRep));

InstallMethod(LocalActionDiagramConsNC, "Construction Local Action Diagram Object", [IsLocalActionDiagram, IsRecord],
function(_, lad_data)
	Assert(1, IsBound(lad_data.graph));
	Assert(1, IsBound(lad_data.vert_labels));
	Assert(1, IsBound(lad_data.arc_labels));

	return ObjectifyWithAttributes(rec(), LocalActionDiagramType,
		LocalActionDiagramRSGraph, lad_data.graph,
		LocalActionDiagramVertexLabels, lad_data.vert_labels,
		LocalActionDiagramArcLabels, lad_data.arc_labels
	);
end);

BindGlobal("LAD_LocalActionDiagramConsCheck@", 
function(graph, vert_labels, arc_labels)
	local orbits, arc, arc_label_combined, idx, name;
	# Check vertex label orbit condition. 
	
	orbits := rec();

	for idx in Set(RecNames(vert_labels)) do
		orbits.(idx) := Orbits(vert_labels.(idx), PermGroupDomain(vert_labels.(idx)));
	od;

	arc_label_combined := rec();

	for name in RecNames(vert_labels) do
		arc_label_combined.(name) := [];
	od;


	for arc in RSGraphArcIterator(graph) do
		if not arc_labels.(arc[1]) in orbits.(arc[2].origin) then
			ErrorNoReturn("Arc labels must be orbits of the group labels");
		fi;
		Add(arc_label_combined.(arc[2].origin), arc_labels.(arc[1]));
	od;


	for idx in Set(RecNames(vert_labels)) do
		if Set(orbits.(idx)) <> Set(arc_label_combined.(idx)) then
			ErrorNoReturn("Must have an arc label for each orbit of the vertex labels.");
		fi;
		if not IsDuplicateFree(arc_label_combined.(idx)) then
			ErrorNoReturn("Must have exactly one arc label for each orbit of a vertex label.");
		fi;
	od;
end);

InstallMethod(LocalActionDiagramFromData, "Constructing Local Action Diagram (With Checks)", [IsRSGraph, IsList, IsList],
function(graph, _vert_labels, _arc_labels)
	local vert_labels, arc_labels, idx;

	if Length(_vert_labels) <> RSGraphNumberVertices(graph) then
		ErrorNoReturn("Must have a vertex label for each vertex in the graph.");
	fi;

	if Length(_arc_labels) <> Length(RSGraphArcIDs(graph)) then
		ErrorNoReturn("Must have an arc label for each arc in the graph.");
	fi;

	vert_labels := rec();
	arc_labels := rec();

	for idx in [1..Length(_vert_labels)] do
		vert_labels.(RSGraphVertices(graph)[idx]) := _vert_labels[idx];
	od;

	for idx in [1..Length(_arc_labels)] do
		arc_labels.(RSGraphArcIDs(graph)[idx]) := _arc_labels[idx];
	od;

	LAD_LocalActionDiagramConsCheck@(graph, vert_labels, arc_labels);

	return LocalActionDiagramFromDataNC(graph, vert_labels, arc_labels);
end);

InstallMethod(LocalActionDiagramFromData, "Constructing Local Action Diagram (With Checks)", [IsRSGraph, IsRecord, IsRecord],
function(graph, vert_labels, arc_labels)
	if Length(RecNames(vert_labels)) <> RSGraphNumberVertices(graph) then
		ErrorNoReturn("Must have a vertex label for each vertex in the graph.");
	fi;

	if Length(RecNames(arc_labels)) <> Length(RSGraphArcIDs(graph)) then
		ErrorNoReturn("Must have an arc label for each arc in the graph.");
	fi;

	LAD_LocalActionDiagramConsCheck@(graph, vert_labels, arc_labels);

	return LocalActionDiagramFromDataNC(graph, vert_labels, arc_labels);
end);

InstallMethod(LocalActionDiagramFromDataNC, "Construction Local Action Diagram (Without Checks)", [IsRSGraph, IsList, IsList],
function(graph, _vert_labels, _arc_labels)
	local vert_labels, arc_labels, idx;

	vert_labels := rec();
	arc_labels := rec();

	for idx in [1..Length(_vert_labels)] do
		vert_labels.(RSGraphVertices(graph)[idx]) := _vert_labels[idx];
	od;

	for idx in [1..Length(_arc_labels)] do
		arc_labels.(RSGraphArcIDs(graph)[idx]) := _arc_labels[idx];
	od;

	return LocalActionDiagramFromDataNC(graph, vert_labels, arc_labels);
end);

InstallMethod(LocalActionDiagramFromDataNC, "Construction Local Action Diagram (Without Checks)", [IsRSGraph, IsRecord, IsRecord],
function(graph, vert_labels, arc_labels)
	local lad_data;

	lad_data := rec();

	lad_data.graph := graph;
	lad_data.vert_labels := vert_labels;
	lad_data.arc_labels := arc_labels;

	return LocalActionDiagramConsNC(IsLocalActionDiagram, lad_data);
end);

InstallMethod(LocalActionDiagramFromUniversalGroup, "Construct Local Action Diagram Corresponding To Universal Group U(F)", [IsPermGroup],
function(perm_group)
	local orbits, graph;

	orbits := Orbits(perm_group, PermGroupDomain(perm_group));
	graph := RSGraphByAdjacencyMatrix([[Length(orbits)]], ());

	return LocalActionDiagramFromDataNC(graph, [perm_group], orbits);

end);

# Visualising Local Action Diagrams
InstallMethod(ViewString, "for a local action diagram", [IsLocalActionDiagram],
function(lad)
	local graph;

	graph := LocalActionDiagramRSGraph(lad);
	return StringFormatted("<LocalActionDiagram with {1} vertices and {2} arcs>", RSGraphNumberVertices(graph), RSGraphNumberArcs(graph));
end);

InstallMethod(PrintString, "for a local action diagram", [IsLocalActionDiagram], String);

InstallMethod(String, "for a local action diagram", [IsLocalActionDiagram],
function(lad)
	local print_string, graph, vert_ids, arc_ids, vert_labels, arc_labels, id;

	graph := LocalActionDiagramRSGraph(lad);
	vert_labels := LocalActionDiagramVertexLabels(lad);
	arc_labels := LocalActionDiagramArcLabels(lad);
	vert_ids := SortedList(RecNames(vert_labels));
	arc_ids := SortedList(RecNames(arc_labels));

	# First Print The Graph Details
	print_string := String(graph);

	print_string := Concatenation(print_string, "Vertex Labels = {\n");

	for id in vert_ids do
		print_string := Concatenation(print_string, StringFormatted("\t{1} = {2}\n", id, vert_labels.(id)));
	od;

	print_string := Concatenation(print_string, "}\nArc Labels = {\n");

	for id in arc_ids do
		print_string := Concatenation(print_string, StringFormatted("\t{1} = {2}\n", id, arc_labels.(id)));
	od;

	print_string := Concatenation(print_string, "}\n");
	
	return print_string;

end);

InstallMethod(LocalActionDiagramVertices, "Local Action Diagram Vertex IDs", [IsLocalActionDiagram], lad -> RSGraphVertices(LocalActionDiagramRSGraph(lad)));

InstallMethod(LocalActionDiagramArcs, "Local Action Diagram Vertex IDs", [IsLocalActionDiagram], lad -> RSGraphArcs(LocalActionDiagramRSGraph(lad)));

InstallMethod(LocalActionDiagramArcIDs, "Local Action Diagram Vertex IDs", [IsLocalActionDiagram], lad -> RSGraphArcIDs(LocalActionDiagramRSGraph(lad)));

InstallMethod(LocalActionDiagramReverseMap, "Local Action Diagram Vertex IDs", [IsLocalActionDiagram], lad -> RSGraphReverseMap(LocalActionDiagramRSGraph(lad)));

InstallMethod(LocalActionDiagramOutNeighbours, "Local Action Diagram Vertex IDs", [IsLocalActionDiagram], lad -> RSGraphOutNeighbours(LocalActionDiagramRSGraph(lad)));

InstallMethod(LocalActionDiagramInNeighbours, "Local Action Diagram Vertex IDs", [IsLocalActionDiagram], lad -> RSGraphInNeighbours(LocalActionDiagramRSGraph(lad)));

InstallMethod(LocalActionDiagramOutArcs, "Local Action Diagram Vertex IDs", [IsLocalActionDiagram], lad -> RSGraphOutArcs(LocalActionDiagramRSGraph(lad)));

InstallMethod(LocalActionDiagramInArcs, "Local Action Diagram Vertex IDs", [IsLocalActionDiagram], lad -> RSGraphInArcs(LocalActionDiagramRSGraph(lad)));


InstallMethod(LocalActionDiagramScopos, "List of Scopos of Local Action Diagram", [IsLocalActionDiagram], 
function(lad)
	local candidate_arcs, candidate, candidate_scopos, scopo_list, arc, new_arcs, combined_new_arcs, candidates_to_remove, rev, idx, arc_labels, CheckScopoInArcCondition, ScopoInArcs;

	rev := LocalActionDiagramReverseMap(lad);
	arc_labels := LocalActionDiagramArcLabels(lad);
	scopo_list := [];

	# Start by finding all arcs that have a label of size 1. 
	# These are the arcs that could be part of a scopo. 
	candidate_arcs := [];
	for idx in RecNames(arc_labels) do
		if Size(arc_labels.(idx)) = 1 then
			Add(candidate_arcs, Int(idx)); # Cast the string to integer. 
		fi;
	od;


	# Get all arcs that terminate at the origin of the arc labelled by *arc_id*. 
	ScopoInArcs := function(lad, arc_id)
		local in_arcs, arc;

		arc := LocalActionDiagramArcs(lad).(arc_id);
		# Get all the arcs that terminate at *arc.origin*. 
		in_arcs := LocalActionDiagramInArcs(lad).(arc.origin);
		# Exclude the reverse arc. 
		in_arcs := Difference(in_arcs, [arc_id^LocalActionDiagramReverseMap(lad)]);

		return in_arcs;
	end;

	# Returns false if an incoming arc has a size greater than one. Otherwise returns true. 
	CheckScopoInArcCondition := function(lad, in_arcs)
		local arc_id;

		for arc_id in in_arcs do
			if Size(LocalActionDiagramArcLabels(lad).(arc_id)) <> 1 then
				return false;
			fi;
		od;

		return true;
	end;



	# Returns all candidate arcs whose incoming arcs are all labelled with size one. 
	candidate_arcs := ListBlist(candidate_arcs, List(candidate_arcs, x -> CheckScopoInArcCondition(lad, ScopoInArcs(lad, x))));
	candidate_scopos := List(candidate_arcs, x -> [[x]]); # Scopos will be lists of arc ids. 


	scopo_list := [ [] ]; # The empty scopo is always there. 
	candidates_to_remove := [];

	while true do
		# Leave loop if there are no scopos. 
		candidate_scopos := Difference(candidate_scopos, candidates_to_remove);
		if candidate_scopos = [] then
			break;
		fi;
		for candidate in candidate_scopos do
			combined_new_arcs := [];
			for arc in Last(candidate) do
				# Get all the new arcs needed for a scopo.
				new_arcs := ScopoInArcs(lad, arc);
				if arc = arc^LocalActionDiagramReverseMap(lad) then 
					# Scopo can't contain a self-reverse arc. 
					Add(candidates_to_remove, candidate);
					break;
				elif CheckScopoInArcCondition(lad, new_arcs) then
					# All the in arcs have a label of size one so they can be
					# part of the candidate scopo. 
					combined_new_arcs := Concatenation(combined_new_arcs, new_arcs);
				else
					# Not all the in arcs have a label of size one so this
					# can't be a scopo. 
					Add(candidates_to_remove, candidate);
					break;
				fi;
			od;
			if candidate in candidates_to_remove then
				# Not a scopo so can skip the rest of the function. 
				continue;
			fi;
			# If this is empty then it's a scopo because there are no new arcs to add.
			if combined_new_arcs = [] then
				Add(scopo_list, Concatenation(candidate));
				# Remove the known scopo so we can eventually terminate the function. 
				Add(candidates_to_remove, candidate);
				continue;
			fi;
			# If all arcs are already in the scopo then we are looping so it's a scopo.
			if ForAll(combined_new_arcs, x -> ForAny(candidate, y -> x in y)) then
				Add(scopo_list, Concatenation(candidate));
				Add(candidates_to_remove, candidate);
				continue;
			fi;
			# This is the reverse of an arc in then candidate so it isn't a scopo. 
			if ForAny(combined_new_arcs, x -> ForAny(candidate, y -> x^rev in y)) then
				Add(candidates_to_remove, candidate);
				continue;
			fi;
			Add(candidate, combined_new_arcs); # Hope this works.
		od;
	od;


	return Set(List(scopo_list, x -> Set(x)));
end);

InstallMethod(LocalActionDiagramGroupType, "Type of the corresponding group", [IsLocalActionDiagram], 
function(lad)
	local scopos, CotreeFromScopo, cotrees, cotree, labels, start_vertex, orientation, rev_orientation, current_vertex, out_arc_ids, arc_id, idx;

	CotreeFromScopo := function(scopo)
		local exclude_edges;
		exclude_edges := Concatenation(scopo, List(scopo, x -> x^LocalActionDiagramReverseMap(lad)));
		return RSGraphSubgraph(LocalActionDiagramRSGraph(lad), Difference(LocalActionDiagramArcIDs(lad), exclude_edges));
	end;

	scopos := LocalActionDiagramScopos(lad);

	cotrees := List(scopos, x -> CotreeFromScopo(x));

	for cotree in cotrees do
		# Single vertex cotree with no arcs. 
		if RSGraphNumberVertices(cotree) = 1 and RSGraphNumberArcs(cotree) = 0 then
			return "Fixed Vertex";
		fi;

		# Single vertex cotree with one (self-reverse) arc. 
		if RSGraphNumberVertices(cotree) = 1 and RSGraphNumberArcs(cotree) = 1 then
			# The arc is labelled by a set of size 1. 
			if Size(LocalActionDiagramArcLabels(lad).(RSGraphArcIDs(cotree)[1])) = 1 then
				return "Edge Inversion";
			fi;
		fi;

		# This cotree corresponds to a "full scopo" (an arc from every
		# edge of the local action diagram belonging to the scopo). 
		# There's nothing to consider with this cotree. 
		if RSGraphNumberVertices(cotree) = 0 then
			continue;
		fi;

		# Check for a cycle graph cotree. 
		if RSGraphIsCycle(cotree) then
			labels := List(RSGraphArcIDs(cotree), x -> LocalActionDiagramArcLabels(lad).(x));
			# Every arc label in the cycle has a size of one. 
			if ForAll(labels, x -> Size(x) = 1) then
				return "Lineal";
			fi;

			# Get cyclic orientation. 
			start_vertex := RSGraphVertices(cotree)[1];
			current_vertex := RSGraphVertices(cotree)[1];
			orientation := [];
			rev_orientation := [];
			# The cotree is a cycle so this is the number of times
			# needed to iterate to walk the cyclic path. 
			for idx in [1..RSGraphNumberVertices(cotree)] do
				out_arc_ids := RSGraphOutArcs(cotree).(current_vertex);
				if out_arc_ids[1] in rev_orientation then
					arc_id := out_arc_ids[2];
				else
					arc_id := out_arc_ids[1];
				fi;
				Add(orientation, arc_id);
				Add(rev_orientation, arc_id^LocalActionDiagramReverseMap(lad));
				current_vertex := RSGraphArcs(cotree).(arc_id).terminus;
			od;

			# Will be true if it's a cyclic orientation. 
			Assert(1, current_vertex = start_vertex);
			Assert(1, Size(orientation) = RSGraphNumberVertices(cotree));
			Assert(1, Size(rev_orientation) = RSGraphNumberVertices(cotree));

			labels := List(orientation, x -> LocalActionDiagramArcLabels(lad).(x));
			# Every label in this orientation has a size of 1. 
			if ForAll(labels, x -> Size(x) = 1) then
				return "Focal";
			fi;

			labels := List(rev_orientation, x -> LocalActionDiagramArcLabels(lad).(x));
			# Every label in this orientation has a size of 1. 
			if ForAll(labels, x -> Size(x) = 1) then
				return "Focal";
			fi;

		fi;
	od;
	return "General";

	# TODO: Use the scopo to get the cyclic orientation instead of calculating it.
end);

InstallMethod(LocalActionDiagramIsDiscrete, "Check if corresponding group is discrete.", [IsLocalActionDiagram], 
function(lad)
	local group_type, v_label, RecIter, scopos, max_scopo, v_in_scopo, arc_id, vertex_id;

	# Helper function for iterating records. 
	RecIter := x -> List(RecNames(x), y -> x.(y));

	group_type := LocalActionDiagramGroupType(lad);

	# Fixed Vertex and Edge Inversion are always discrete. 
	if group_type = "Fixed Vertex" or group_type = "Edge Inversion" then
		SetLocalActionDiagramIsUniscalar(lad, true);
		SetLocalActionDiagramIsUnimodular(lad, true);
		return true;
	# Focal is never discrete. 
	elif group_type = "Focal" then
		return false;
	elif group_type = "Lineal" then
		# Discrete iff each vertex label is trivial. 
		for v_label in RecIter(LocalActionDiagramVertexLabels(lad)) do
			if v_label <> Group(()) then
				return false;
			fi;
		od;
		SetLocalActionDiagramIsUniscalar(lad, true);
		SetLocalActionDiagramIsUnimodular(lad, true);
		return true;
	elif group_type = "General" then
		# Get the unique maximal scopo. 
		scopos := LocalActionDiagramScopos(lad);
		max_scopo := Position(List(scopos, Length), Maximum(List(scopos, Length)));
		max_scopo := scopos[max_scopo];

		# Get every vertex in the scopo (origin vertices of edges in scopo).
		# These are the vertices not in the cotree.
		v_in_scopo := [];
		for arc_id in max_scopo do
			Add(v_in_scopo, LocalActionDiagramArcs(lad).(arc_id).origin);
		od;

		for vertex_id in LocalActionDiagramVertices(lad) do
			v_label := LocalActionDiagramVertexLabels(lad).(vertex_id);
			# Vertex label not in cotree and not trivial.
			if vertex_id in v_in_scopo then
				if v_label <> Group(()) then
					return false;
				fi;
			# Vertex label in cotree and not semi-regular. 
			elif not vertex_id in v_in_scopo then
				if not IsSemiRegular(v_label, PermGroupDomain(v_label)) then
					return false;
				fi;
			else
				Error("Something wrong with general type discrete check. Vertex not in scopo and not in cotree...");
			fi;
		od;
		
		# General type is discrete if above loop finished. 
		SetLocalActionDiagramIsUniscalar(lad, true);
		SetLocalActionDiagramIsUnimodular(lad, true);
		return true;
	else
		Error("Something really bad happend with the group type check.");
	fi;

end);

InstallMethod(LocalActionDiagramIsUniscalar, "Check if corresponding group is uniscalar.", [IsLocalActionDiagram], 
function(lad)
	local group_type, v_label, max_cotree, max_scopo, scopos, arc_id, v_in_scopo, vertex_id, scopo, arc_list, arc_label_rec, arc, remove_domain, max_scopo_with_rev, RecIterName;

	# Helper function for iterating records. 
	RecIterName := x -> List(RecNames(x), y -> [y, x.(y)]);

	group_type := LocalActionDiagramGroupType(lad);

	# These types are always uniscalar for finite local action diagrams . 
	if group_type = "Fixed Vertex" or group_type = "Edge Inversion" or group_type = "Lineal" then
		SetLocalActionDiagramIsUnimodular(lad, true); # Uniscalar implies unimodular. 
		return true;
	# Focal is never uniscalar. 
	elif group_type = "Focal" then
		return false;
	elif group_type = "General" then
		# Get the unique maximum scopo.
		scopos := LocalActionDiagramScopos(lad);
		max_scopo := Position(List(scopos, Length), Maximum(List(scopos, Length)));
		max_scopo := scopos[max_scopo];
		max_scopo_with_rev := Concatenation(max_scopo, List(max_scopo, x -> x^LocalActionDiagramReverseMap(lad)));



		# Get every vertex in the scopo (origin vertices of arcs in scopo).
		# These are the vertices not in the cotree.
		v_in_scopo := [];
		for arc_id in max_scopo do
			Add(v_in_scopo, LocalActionDiagramArcs(lad).(arc_id).origin);
		od;


		arc_list := LocalActionDiagramArcs(lad);
		arc_label_rec := LocalActionDiagramArcLabels(lad);

		for vertex_id in LocalActionDiagramVertices(lad) do
			v_label := LocalActionDiagramVertexLabels(lad).(vertex_id);
			# Vertex in the cotree. Need to check if the vertex label
			# is semi-regular when restricted to the cotree. 
			if not vertex_id in v_in_scopo then
				remove_domain := [];
				for arc in RecIterName(arc_list) do
					if arc[2].origin = vertex_id and arc[1] in max_scopo_with_rev then
						remove_domain := Concatenation(remove_domain, arc_label_rec.(arc[1]));
					fi;
				od;
				if not IsSemiRegular(v_label, Difference(PermGroupDomain(v_label), remove_domain)) then
					return false;
				fi;
			fi;
		od;
		
		# General type is discrete if above loop finished. 
		SetLocalActionDiagramIsUnimodular(lad, true);
		return true;
	else
		Error("Something really bad happend with the group type check.");
	fi;
end);

InstallMethod(LocalActionDiagramIsUnimodular, "Check if corresponding group is unimodular.", [IsLocalActionDiagram], 
function(lad)
	local spanning_tree, sp_arc_ids, cycle_arcs, cycle_basis, cycle_arc, is_unimodular, cycle, arc_id, prod, prod_rev, i, CycleFinder;

	# Find spanning tree by breadth first search to reduce the size of 
	# each cycle in the basis. 
	spanning_tree := RSGraphSpanningTree(LocalActionDiagramRSGraph(lad), "bfs");

	sp_arc_ids := RSGraphArcIDs(spanning_tree);

	# Get each arc not in the spanning tree. 
	cycle_arcs := Difference(LocalActionDiagramArcIDs(lad), sp_arc_ids);

	# Cycle finding function. 
	CycleFinder := function(added_arc_id)
		local cycle, arc_list, visited_verts, dfs, rev_map, arc_id, dead_end, found_cycle, current_vertex, start_vertex, visited_arcs, neighbours;

		arc_list := RSGraphArcs(spanning_tree);
		rev_map := LocalActionDiagramReverseMap(lad);
		cycle := [added_arc_id];
		visited_arcs := [added_arc_id, added_arc_id^rev_map];
		# This arc isn't in the spanning tree so we need to check it 
		# from the whole local action diagram. 
		start_vertex := LocalActionDiagramArcs(lad).(added_arc_id).origin;
		current_vertex := LocalActionDiagramArcs(lad).(added_arc_id).terminus;

		# The added arc is a loop. 
		if start_vertex = current_vertex then
			if added_arc_id^rev_map <> added_arc_id then
				return [added_arc_id, added_arc_id^rev_map];
			else
				return fail;
			fi;
		fi;

		found_cycle := false;

		while true do
			neighbours := RSGraphOutArcs(spanning_tree).(current_vertex);
			dead_end := true;
			for arc_id in neighbours do
				# Haven't searched on this arc yet. 
				if not arc_id in visited_arcs then
					Add(visited_arcs, arc_id);
					Add(visited_arcs, arc_id^rev_map);
					current_vertex := arc_list.(arc_id).terminus;
					Add(cycle, arc_id);
					dead_end := false;
					if current_vertex = start_vertex then
						found_cycle := true;
					fi;
					break;
				fi;
			od;

			if found_cycle = true then
				break;
			fi;

			if dead_end = true then
				Remove(cycle);
				current_vertex := arc_list.(Last(cycle)).terminus;
			fi;
		od;

		return cycle;

	end;

	cycle_basis := [];
	#checked_arcs := [];

	for cycle_arc in cycle_arcs do
		cycle := CycleFinder(cycle_arc);
		if cycle <> fail then
			Add(cycle_basis, cycle);
		fi;
	od;


	is_unimodular := true;

	for cycle in cycle_basis do
		i := 1;
		prod := 1;
		prod_rev := 1;
		for arc_id in cycle do
			if i mod 2 = 1 then
				prod := prod * Size(LocalActionDiagramArcLabels(lad).(arc_id));
				i := i + 1;
			else
				prod_rev := prod_rev * Size(LocalActionDiagramArcLabels(lad).(arc_id));
				i := i + 1;
			fi;
		od;
		if prod <> prod_rev then
			is_unimodular := false;
			break;
		fi;
	od;

	return is_unimodular;
end);
