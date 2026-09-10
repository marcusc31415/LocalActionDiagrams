# Family
BindGlobal("RSGraphFamily", NewFamily("RSGraphFamily", IsRSGraph));

# Type
BindGlobal("RSGraphType", NewType(RSGraphFamily, IsRSGraphRep));

InstallMethod(RSGraphConsNC, "Constructing RSGraph Object", [IsRSGraph, IsRecord],
function(_, graph_data)
	Assert(1, IsBound(graph_data.vertices));
	Assert(1, IsBound(graph_data.arcs));
	Assert(1, IsBound(graph_data.arc_ids));
	Assert(1, IsBound(graph_data.reverse_map));
	Assert(1, IsBound(graph_data.has_parallel));

	return ObjectifyWithAttributes(rec(), RSGraphType, 
		RSGraphVertices, graph_data.vertices,
		RSGraphArcs, graph_data.arcs,
		RSGraphArcIDs, graph_data.arc_ids,
		RSGraphReverseMap, graph_data.reverse_map,
		RSGraphHasParallelArcs, graph_data.has_parallel
	);
end);

# Input Checking For RSGraph Construction.

BindGlobal("LAD_RSGraphConsCheck@", 
function(_arc_list, rev_map, vertex_ids, arc_ids)
	local idx, visited_verts, dfs, arc_list, arc;

	if rev_map*rev_map <> () then
		ErrorNoReturn("Reverse map must be an involution.");
	fi;

	if not ForAll(vertex_ids, IsInt) then
		ErrorNoReturn("Vertex IDs must be integers.");
	fi;

	# Supports either an adjacency listing or an RSGraph 
	# record of arcs (e.g. for subgraph checks).  
	if IsList(_arc_list) then
		arc_list := _arc_list;
	elif IsRecord(_arc_list) then
		arc_list := [];
		for arc in Set(RecNames(_arc_list)) do
			arc_list[Int(arc)] :=  [_arc_list.(arc).origin, _arc_list.(arc).terminus];
		od;
	else
		ErrorNoReturn("Something went wrong with the construction check");
	fi;

	for idx in arc_ids do
		if arc_list[idx^rev_map][1] <> arc_list[idx][2] or arc_list[idx^rev_map][2] <> arc_list[idx][1] then
			ErrorNoReturn("Reversal mapping must send the terminal vertex of an arc to the origin vertex of the arc.");
		fi;

		if not arc_list[idx][1] in vertex_ids or not arc_list[idx][2] in vertex_ids then
			ErrorNoReturn(StringFormatted("Arc {1} origin and terminus vertices do not match vertex ids.", idx));
		fi;
	od;

	# Check that the graph is connected. 
	visited_verts := [];

	dfs := function(vert_id)
		local arc, neighbours, vert;

		AddSet(visited_verts, vert_id);


		# Get a list of neighbours. Only need
		# for each vertex connection and ignores 
		# loops.  
		neighbours := [];
		for arc in arc_list do
			if arc[1] = vert_id and arc[2] <> vert_id then
				AddSet(neighbours, arc[2]);
			fi;
		od;


		for vert in neighbours do
			if not vert in visited_verts then
				dfs(vert);
			fi;
		od;
	end;

	if not IsEmpty(vertex_ids) then
		dfs(vertex_ids[1]);
	fi;


	if Length(visited_verts) <> Length(vertex_ids) then
		Error("Graph must be connected.");
	fi;

end);

BindGlobal("LAD_RSGraphAdjMatToAdjList@",
function(adj_mat, vertex_ids)
	local adj_list, idx_x, idx_y, _;

	adj_list := [];

	for idx_x in [1..NumberRows(adj_mat)] do
		for idx_y in [1..NumberColumns(adj_mat)] do
			for _ in [1..adj_mat[idx_x][idx_y]] do
				Add(adj_list, [vertex_ids[idx_x], vertex_ids[idx_y]]);
			od;
		od;
	od;

	return adj_list;
end);

InstallMethod(RSGraphByAdjacencyList, [IsList, IsPerm],
function(arc_list, rev_map)
	local vertex_ids;

	if IsEmpty(arc_list) then
		vertex_ids := [];
	else
		vertex_ids := [1..Maximum(Flat(arc_list))];
	fi;

	LAD_RSGraphConsCheck@(arc_list, rev_map, vertex_ids, [1..Length(arc_list)]);
	
	return RSGraphByAdjacencyListNC(arc_list, rev_map, vertex_ids);
end);

InstallMethod(RSGraphByAdjacencyList, [IsList, IsPerm, IsList],
function(arc_list, rev_map, vertex_ids)

	LAD_RSGraphConsCheck@(arc_list, rev_map, vertex_ids, [1..Length(arc_list)]);
	
	return RSGraphByAdjacencyListNC(arc_list, rev_map, vertex_ids);
end);

InstallMethod(RSGraphByAdjacencyListNC, [IsList, IsPerm],
function(arc_list, rev_map)
	local vertex_ids;

	if IsEmpty(arc_list) then
		vertex_ids := [];
	else
		vertex_ids := [1..Maximum(Flat(arc_list))];
	fi;
	
	return RSGraphByAdjacencyListNC(arc_list, rev_map, vertex_ids);
end);

InstallMethod(RSGraphByAdjacencyListNC, [IsList, IsPerm, IsList],
function(arc_list, rev_map, vertex_ids)
	local graph_data, arc_records, arc, idx, print_string, vert, arc_directions, has_parallel, arc_dir;

	graph_data := rec();

	arc_directions := [];

	graph_data.vertices := vertex_ids;
	graph_data.arc_ids := [1..Size(arc_list)];

	arc_records := rec();

	has_parallel := false;

	for idx in graph_data.arc_ids do
		arc := rec();
		arc.origin := arc_list[idx][1];
		arc.terminus := arc_list[idx][2];
		arc.inverse := idx^rev_map;
		
		arc_records.(idx) := arc;

		arc_dir := [arc.origin, arc.terminus];

		if not has_parallel and arc_dir in arc_directions then 
			has_parallel := true;
		else
			Add(arc_directions, arc_dir);
		fi;
	od;

	graph_data.arcs := arc_records;
	graph_data.reverse_map := rev_map;
	graph_data.has_parallel := has_parallel;
	
	return RSGraphConsNC(IsRSGraph, graph_data);
end);

InstallMethod(RSGraphByAdjacencyMatrix, [IsRectangularTable, IsPerm],
function(adj_mat, rev_map)
	local adj_list, vertex_ids, graph;

	vertex_ids := [1..NumberRows(adj_mat)];

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	graph := RSGraphByAdjacencyList(adj_list, rev_map, vertex_ids);
	SetRSGraphAdjacencyMatrix(graph, adj_mat);
	return graph;
end);

InstallMethod(RSGraphByAdjacencyMatrix, [IsRectangularTable, IsPerm, IsList],
function(adj_mat, rev_map, vertex_ids)
	local adj_list, graph;

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	graph := RSGraphByAdjacencyList(adj_list, rev_map, vertex_ids);
	SetRSGraphAdjacencyMatrix(graph, adj_mat);
	return graph;
end);

InstallMethod(RSGraphByAdjacencyMatrixNC, [IsRectangularTable, IsPerm],
function(adj_mat, rev_map)
	local adj_list, vertex_ids, graph;

	vertex_ids := [1..NumberRows(adj_mat)];

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	graph := RSGraphByAdjacencyListNC(adj_list, rev_map, vertex_ids);
	SetRSGraphAdjacencyMatrix(graph, adj_mat);
	return graph;
end);

InstallMethod(RSGraphByAdjacencyMatrixNC, [IsRectangularTable, IsPerm, IsList],
function(adj_mat, rev_map, vertex_ids)
	local adj_list, graph;

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	graph := RSGraphByAdjacencyListNC(adj_list, rev_map, vertex_ids);
	SetRSGraphAdjacencyMatrix(graph, adj_mat);
	return graph;
end);

InstallMethod(PrintString, "for an RSGraph", [IsRSGraph], String);

InstallMethod(String, "for an RSGraph", [IsRSGraph], 
function(graph)
	local print_string, vertex_ids, arc_rec, rev_map, idx, vert;

	vertex_ids := RSGraphVertices(graph);
	arc_rec := RSGraphArcs(graph);
	rev_map := RSGraphReverseMap(graph);

	print_string := "Vertices = { ";
	if Size(vertex_ids) = 1 then
		print_string := Concatenation(print_string, String(vertex_ids[1]));
	elif Size(vertex_ids) = 0 then
		;
	else
		for vert in vertex_ids do
			print_string := Concatenation(print_string, StringFormatted("{1}, ", vert));
		od;
		Remove(print_string);
		Remove(print_string);
	fi;

	print_string := Concatenation(print_string, " }\nArcs = {\n");

	for idx in RSGraphArcIDs(graph) do
		print_string := Concatenation(print_string, StringFormatted("\t{1} = ( origin = {2}, terminus = {3}, inverse = {4} )\n", idx, arc_rec.(idx).origin, arc_rec.(idx).terminus, idx^rev_map));
	od;

	print_string := Concatenation(print_string, "}");
	print_string := Concatenation(print_string, StringFormatted("\nReverse Map = {1}\n", rev_map));

	return print_string;
end);

InstallMethod(ViewString, "for an RSGraph", [IsRSGraph],
function(graph)
	local v_string, a_string;

	if Size(RSGraphVertices(graph)) = 1 then
		v_string := StringFormatted("{1} vertex", Size(RSGraphVertices(graph)));
	else
		v_string := StringFormatted("{1} vertices", Size(RSGraphVertices(graph)));
	fi;

	if Size(RSGraphArcIDs(graph)) = 1 then
		a_string := StringFormatted("{1} arc", Size(RSGraphArcIDs(graph)));
	else
		a_string := StringFormatted("{1} arcs", Size(RSGraphArcIDs(graph)));
	fi;

	return StringFormatted("<RSGraph with {1} and {2}>", v_string, a_string);
end);

InstallMethod(RSGraphNumberVertices, "for an RSGraph", [IsRSGraph], graph -> Size(RSGraphVertices(graph)));

InstallMethod(RSGraphNumberArcs, "for an RSGraph", [IsRSGraph], graph -> Size(RSGraphArcIDs(graph)));

InstallMethod(RSGraphAdjacencyMatrix, "for an RSGraph", [IsRSGraph],
function(graph)
	local vertex_ids, arc_ids, arc_rec, standard_range, vert_id_map, adj_mat, id;

	vertex_ids := SortedList(RSGraphVertices(graph));
	standard_range := [1..RSGraphNumberVertices(graph)];
	arc_ids := RSGraphArcIDs(graph);
	arc_rec := RSGraphArcs(graph);

	vert_id_map := MappingPermListList(vertex_ids, standard_range);

	adj_mat := List(standard_range, x -> List(standard_range, x -> 0));

	for id in arc_ids do
		adj_mat[arc_rec.(id).origin^vert_id_map][arc_rec.(id).terminus^vert_id_map] := adj_mat[arc_rec.(id).origin^vert_id_map][arc_rec.(id).terminus^vert_id_map] + 1;
	od;

	return adj_mat;
end);



InstallMethod(RSGraphOutNeighbours, [IsRSGraph],
function(graph)
	local out_rec, id, arc_rec;

	out_rec := rec();

	for id in RSGraphVertices(graph) do
		out_rec.(id) := [];
	od;

	for id in RSGraphArcIDs(graph) do
		arc_rec := RSGraphArcs(graph).(id);
		AddSet(out_rec.(arc_rec.origin), arc_rec.terminus);
	od;

	return out_rec;
end);

InstallMethod(RSGraphInNeighbours, [IsRSGraph],
function(graph)
	local in_rec, id, arc_rec;

	in_rec := rec();

	for id in RSGraphVertices(graph) do
		in_rec.(id) := [];
	od;

	for id in RSGraphArcIDs(graph) do
		arc_rec := RSGraphArcs(graph).(id);
		AddSet(in_rec.(arc_rec.terminus), arc_rec.origin);
	od;

	return in_rec;
end);

InstallMethod(RSGraphOutArcs, [IsRSGraph],
function(graph)
	local out_rec, id, arc_rec;

	out_rec := rec();

	for id in RSGraphVertices(graph) do
		out_rec.(id) := [];
	od;

	for id in RSGraphArcIDs(graph) do
		arc_rec := RSGraphArcs(graph).(id);
		AddSet(out_rec.(arc_rec.origin), id);
	od;

	return out_rec;
end);

InstallMethod(RSGraphInArcs, [IsRSGraph],
function(graph)
	local in_rec, id, arc_rec;

	in_rec := rec();

	for id in RSGraphVertices(graph) do
		in_rec.(id) := [];
	od;

	for id in RSGraphArcIDs(graph) do
		arc_rec := RSGraphArcs(graph).(id);
		AddSet(in_rec.(arc_rec.terminus), id);
	od;

	return in_rec;
end);

InstallMethod(RSGraphArcIterator, "for RSGraphs", [IsRSGraph],
function(graph)
	local NextIterator, IsDoneIterator, ShallowCopy;

	NextIterator := function(iter)
		local count;

		# Find the next arc id. Skip over any holes from subgraphs. 
		count := iter!.counter;
		if count > Maximum(RSGraphArcIDs(iter!.graph)) then
			ErrorNoReturn("Iterator is exhausted.");
		fi;
		while not IsBound(RSGraphArcs(iter!.graph).(count)) do
			count := count + 1;
		od;

		# Store the next arc id to check from. 
		iter!.counter := count + 1; 

		# Return [*id*, *arc record*]
		return [count, RSGraphArcs(iter!.graph).(count)];
	end;

	IsDoneIterator := function(iter)
		return iter!.counter > Maximum(RSGraphArcIDs(iter!.graph));
	end;

	ShallowCopy := function(iter)
		return rec(
			graph := iter!.graph,
			counter := iter!.counter);
	end;

	return IteratorByFunctions(rec(
		NextIterator := NextIterator,
		IsDoneIterator := IsDoneIterator,
		ShallowCopy := ShallowCopy,
		counter := 1,
		graph := graph));
end);

BindGlobal("LAD_Subgraph_Cons@", 
function(graph, arc_ids)
	local vertex_ids, arc_records, subgraph_data, id, has_parallel, arc_directions, arc_dir;

	vertex_ids := [];
	arc_records := rec();

	has_parallel := false;

	arc_directions := [];

	for id in arc_ids do
		arc_records.(id) := RSGraphArcs(graph).(id);
		AddSet(vertex_ids, arc_records.(id).origin);
		AddSet(vertex_ids, arc_records.(id).terminus);
	od;

	for id in arc_ids do
		arc_dir := [arc_records.(id).origin, arc_records.(id).terminus];
		if not has_parallel and arc_dir in arc_directions then
			has_parallel := true;
		else
			Add(arc_directions, arc_dir);
		fi;
	od;

	subgraph_data := rec();

	subgraph_data.vertices := vertex_ids;
	subgraph_data.arc_ids := arc_ids;

	subgraph_data.arcs := arc_records;
	subgraph_data.reverse_map := RSGraphReverseMap(graph);
	subgraph_data.has_parallel := has_parallel;

	return subgraph_data;
end);

InstallMethod(RSGraphSubgraph, "for RSGraphs", [IsRSGraph, IsList],
function(graph, arc_ids)
	local subgraph_data, id;

	if not ForAll(arc_ids, IsInt) then
		ErrorNoReturn("The list of arc ids must be integers.");
	fi;

	for id in arc_ids do
		if not id^RSGraphReverseMap(graph) in arc_ids then
			ErrorNoReturn(StringFormatted("Reverse of arc {1} is not in the list of arc ids.", id));
		fi;
	od;

	subgraph_data := LAD_Subgraph_Cons@(graph, arc_ids);

	LAD_RSGraphConsCheck@(subgraph_data.arcs, subgraph_data.reverse_map, subgraph_data.vertices, subgraph_data.arc_ids);

	return RSGraphConsNC(IsRSGraph, subgraph_data);
end);

InstallMethod(RSGraphSubgraphNC, "for RSGraphs", [IsRSGraph, IsList],
function(graph, arc_ids)
	local subgraph_data;

	subgraph_data := LAD_Subgraph_Cons@(graph, arc_ids);

	return RSGraphConsNC(IsRSGraph, subgraph_data);
end);


BindGlobal("LAD_BFS_Tree@", 
function(graph)
	local seen_verts, arc_id_subgraph, queue, current, arc;

	seen_verts := [];
	arc_id_subgraph := [];

	# Start at the first vertex id. 
	queue := [RSGraphVertices(graph)[1]];

	while Size(queue) > 0 do
		current := Remove(queue, 1); # Pop left. 
		Add(seen_verts, current);

		for arc in RSGraphArcIterator(graph) do
			# The arc originates at the current vertex, 
			# haven't already visited the terminus vertex,
			# and the terminus is not already queued. 
			if arc[2].origin = current and not arc[2].terminus in seen_verts and not arc[2].terminus in queue then
				Add(queue, arc[2].terminus); # Push right. 

				# Add the arc and its reverse. 
				Add(arc_id_subgraph, arc[1]);
				Add(arc_id_subgraph, arc[1]^RSGraphReverseMap(graph));
			fi;
		od;
	od;

	return arc_id_subgraph;

end);

BindGlobal("LAD_DFS_Tree@", 
function(graph)
	local seen_verts, arc_id_subgraph, dfs;

	seen_verts := [];
	arc_id_subgraph := [];

	# Start at the first vertex id. 
	dfs := function(vertex_id)
		local neighbours, arc; 

		Add(seen_verts, vertex_id);

		for arc in RSGraphArcIterator(graph) do
			# Haven't already traversed the arc, the arc originates at
			# the current vertex, and haven't already visited the 
			# terminus vertex. 
			if not arc[1] in arc_id_subgraph and arc[2].origin = vertex_id and not arc[2].terminus in seen_verts  then
				# Add the arc and its reverse. 
				Add(arc_id_subgraph, arc[1]);
				Add(arc_id_subgraph, arc[1]^RSGraphReverseMap(graph));

				dfs(arc[2].terminus); 
			fi;
		od;
	end;

	dfs(RSGraphVertices(graph)[1]);

	return arc_id_subgraph;
end);

InstallMethod(RSGraphSpanningTree, "for and RSGraph", [IsRSGraph],
graph -> RSGraphSpanningTree(graph, "bfs"));

InstallMethod(RSGraphSpanningTree, "for and RSGraph", [IsRSGraph, IsString],
function(graph, type)
	local arc_ids, vertex_ids, arc_records, id, subgraph_data;

	# Deal with the zero vertex case separately.
	if RSGraphNumberVertices(graph) = 0 then
		return graph; # Empty graph is its own spanning tree. 
	fi;

	# Deal with the one vertex case separately. 
	if RSGraphNumberVertices(graph) = 1 then
		vertex_ids := RSGraphVertices(graph);
		arc_records := rec();

		subgraph_data := rec();

		subgraph_data.vertices := vertex_ids;
		subgraph_data.arc_ids := [];

		subgraph_data.arcs := arc_records;
		subgraph_data.reverse_map := RSGraphReverseMap(graph);
		subgraph_data.has_parallel := false;

		LAD_RSGraphConsCheck@(subgraph_data.arcs, subgraph_data.reverse_map, subgraph_data.vertices, subgraph_data.arc_ids);

		return RSGraphConsNC(IsRSGraph, subgraph_data);
	fi;

	if type = "dfs" then
		arc_ids := LAD_DFS_Tree@(graph);
	elif type = "bfs" then
		arc_ids := LAD_BFS_Tree@(graph);
	else
		Info(InfoWarning, 1, StringFormatted("\"{1}\" is an invalid parameter. Defaulting to breadth first search.", type));
		arc_ids := LAD_BFS_Tree@(graph);
	fi;

	return RSGraphSubgraph(graph, arc_ids);
end);

InstallMethod(RSGraphIsCycle, "for an RSGraph", [IsRSGraph],
function(graph)

	# Need to check if the graph is a cycle graph in the sense of
	# Reid-Smith. 
	# Check the 1 and 2 cycle cases separately. 
	# Then check if number of arcs = 2*number vertices.
	# The follow the path to see if it's a cycle. 
	
	local arc1, out_arcs, current_vertex, start_vertex, iter, RecIter;
	
	# 0 vertex case.
	if RSGraphNumberVertices(graph) = 0 then
		return false;
	fi;

	# 1 vertex case. 
	if RSGraphNumberVertices(graph) = 1 then
		if RSGraphNumberArcs(graph) = 2 then
			arc1 := RSGraphArcs(graph).(RSGraphArcIDs(graph)[1]);
			if arc1.inverse = RSGraphArcIDs(graph)[2] then
				return true;
			else
				return false;
			fi;
		else
			return false;
		fi;
	fi;
		
	# Must have 2*NumberVertices arcs for it to be a cycle graph. 
	if RSGraphNumberArcs(graph) <> 2*RSGraphNumberVertices(graph) then
		return false;
	fi;

	# Record iteration function. 
	RecIter := x -> List(RecNames(x), y -> x.(y));

	# Check if each vertex has exactly two arcs originating at it. 
	for out_arcs in RecIter(RSGraphOutArcs(graph)) do
		if Size(out_arcs) <> 2 then
			return false;
		fi;
	od;

	# Check there are no self-reverse loops. 
	# The Filtered list on the left is the number of arcs that are 
	# fixed by the reverse map (i.e. self-reverse arcs). If it is 
	# not of length 0 then there is a self-reverse arc. 
	if Size(Filtered(RSGraphArcIDs(graph), x -> x = x^RSGraphReverseMap(graph))) <> 0 then
		return false;
	fi;

	# The graph has two arcs originating at each vertex and there are
	# no self-reverse arcs. The only way this can happen is if the graph
	# is a cycle graph. 
	return true;

end);

# Converts an RSGraph with *N* vertices and *M* edges to have
# vertex ids [1..N] and arc ids [1..M]. 
InstallMethod(RSGraphToStandardForm, "for an RSGraph", [IsRSGraph],
function(graph)
	local new_vertex_ids, vertex_id_map, new_arc_ids, arc_id_map, new_arcs, arc, new_arc, new_rev_map, MappingCreator, new_graph, checked_ids, arcs, ret, cycle;

	new_vertex_ids := [1..RSGraphNumberVertices(graph)];


	# Returns the function mapping original[i] to new[i]. 
	MappingCreator := function(original, new)
		local tuple_list, idx, map;
		
		tuple_list := [];
		# Create a list of DirectProductElements of the form [original[i], new[i]]. 
		for idx in [1..Size(original)] do
			Add(tuple_list, DirectProductElement([original[idx], new[idx]]));
		od;

		return GeneralMappingByElements(Domain(original), Domain(new), tuple_list);
	end;

	# These map the original vertex/arc id to one in the standard range. 
	# Can't use the built in MappingPermListList as it's not guaranteed 
	# to also work in the other direction. 
	vertex_id_map := MappingCreator(RSGraphVertices(graph), new_vertex_ids);

	arcs := [];

	# List of the form [[id, [origin, terminus]], ...] where origin and 
	# terminus are in terms of the new vertex ids. 
	for arc in RSGraphArcIterator(graph) do
		Add(arcs, [arc[1], [arc[2].origin^vertex_id_map, arc[2].terminus^vertex_id_map]]);
	od;

	# Sort the arc list in lexicographical order --- i.e. [1, 1], [1, 2], [2, 1], [2, 2], etc. 
	SortBy(arcs, x -> x[2]);

	new_arc_ids := [1..RSGraphNumberArcs(graph)];

	# Map from the old arc ids to the standard range. The arc ids have
	# been sorted so this will be in lexicographical order. 
	arc_id_map := MappingCreator(List(arcs, x -> x[1]), new_arc_ids);



	new_arcs := rec();
	new_rev_map := ();
	checked_ids := [];

	for arc in RSGraphArcIterator(graph) do
		new_arc := rec();
		new_arc.origin := arc[2].origin^vertex_id_map;
		new_arc.terminus := arc[2].terminus^vertex_id_map;
		new_arc.inverse := arc[2].inverse^arc_id_map;

		new_arcs.(arc[1]^arc_id_map) := new_arc;

		# Construct the new reverse map. 
		# The checked_ids check ensures we only multiply 
		# by each cycle once. 
		if not arc[1] in checked_ids then
			# If it's a self-reverse loop then CycleFromList will fail. 
			if arc[1] = arc[2].inverse then
				cycle := ();
			else
				cycle := CycleFromList([arc[1]^arc_id_map, arc[2].inverse^arc_id_map]);
			fi;
			new_rev_map := new_rev_map*cycle;
			Add(checked_ids, arc[1]);
			Add(checked_ids, arc[2].inverse);
		fi;
	od;


	new_graph := rec();

	new_graph.vertices := new_vertex_ids;
	new_graph.arc_ids := new_arc_ids;

	new_graph.arcs := new_arcs;
	new_graph.reverse_map := new_rev_map;
	new_graph.has_parallel := RSGraphHasParallelArcs(graph);

	LAD_RSGraphConsCheck@(new_graph.arcs, new_graph.reverse_map, new_graph.vertices, new_graph.arc_ids);

	ret := rec();
	ret.graph := RSGraphConsNC(IsRSGraph, new_graph);
	ret.vertex_id_map := vertex_id_map;
	ret.arc_id_map := arc_id_map;

	return ret;
end);

InstallMethod(AutomorphismGroup, "For an RS Graph", [IsRSGraph],
function(graph)
	ErrorNoReturn("Digraphs package needs to be loaded for this function.");
end);

InstallMethod(RSGraphCanonicalLabelling, "For an RS Graph", [IsRSGraph],
function(graph)
	ErrorNoReturn("Digraphs package needs to be loaded for this function.");
end);

InstallMethod(LAD_Internal_RSGraphsEnumerate@, "For degree and number of vertices. ", [IsInt, IsInt],
function(degree, no_verts)
	ErrorNoReturn("Digraphs package needs to be loaded for this function.");
end);

InstallMethod(RSGraphBipartition, "Find a bipartition of the graph (if it exists).", [IsRSGraph],
function(graph)
	local vertex_queue, colours, current_vert, vert_id, neighbour_vert, vert_sets;

	# Single vertex with no arcs or empty graph edge cases. 
	if RSGraphNumberArcs(graph) = 0 then
		return fail;
	fi;

	# Parallel arcs are not allowed for the bipartition. 
	if RSGraphHasParallelArcs(graph) then
		return fail;
	fi;

	# Use BFS search. 
	# No need to worry about loops because then a vertex 
	# will neighbour itself (same colour). 
	colours := rec();
	for vert_id in RSGraphVertices(graph) do
		colours.(vert_id) := -1;
	od;

	vert_sets := [[], []]; # [colour 0, colour 1]

	for vert_id in RSGraphVertices(graph) do
		if colours.(vert_id) = -1 then
			colours.(vert_id) := 0;

			Add(vert_sets[1], vert_id);

			vertex_queue := [vert_id];

			while Size(vertex_queue) > 0 do
				current_vert := Remove(vertex_queue, 1);

				for neighbour_vert in RSGraphOutNeighbours(graph).(current_vert) do
					if colours.(neighbour_vert) = -1 then
						colours.(neighbour_vert) := 1 - colours.(current_vert);
						Add(vertex_queue, neighbour_vert);

						if colours.(neighbour_vert) = 0 then
							Add(vert_sets[1], neighbour_vert);
						else
							Add(vert_sets[2], neighbour_vert);
						fi;
					elif colours.(neighbour_vert) = colours.(current_vert) then
						return fail;
					fi;
				od;
			od;
		fi;
	od;

	return vert_sets;

end);

InstallMethod(RSGraphIsBipartite, "Check if the graph is a bipartite graph.", [IsRSGraph], graph -> RSGraphBipartition <> fail);

InstallMethod(RSGraphDegree, "Maximum degree of any vertex.", [IsRSGraph], 
function(graph)
	local degree_list, v_id;

	degree_list := [];

	for v_id in RSGraphVertices(graph) do
		Add(degree_list, Size(RSGraphOutArcs(graph).(v_id)));
	od;

	return Maximum(degree_list);
end);






