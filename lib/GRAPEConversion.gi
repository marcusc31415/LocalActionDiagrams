BindGlobal("RSGraphToGRAPE", 
function(graph)
	local standard_graph, adj_mat, ret, idx;

	if RSGraphHasParallelArcs(graph) then
		ErrorNoReturn("GRAPE does not support graphs with parallel arcs.");
	fi;

	standard_graph := RSGraphToStandardForm(graph);

	adj_mat := RSGraphAdjacencyMatrix(standard_graph.graph);

	ret := rec();

	# Construct the graph form the adjacency matrix. 
	# Construction from 2.1 in GRAPE manual. 
	ret.graph := Graph(Group(()), RSGraphVertices(standard_graph.graph), OnPoints, 
	                   function(x, y) return adj_mat[x][y] = 1; end, true);
	ret.reverse_map := RSGraphReverseMap(standard_graph.graph);
	ret.vertex_id_map := standard_graph.vertex_id_map;
	ret.arc_id_map := standard_graph.arc_id_map;

	return ret;
end);

BindGlobal("LAD_GraphFromGRAPEAndPerm",
function(graph, rev_map)
	return RSGraphByAdjacencyList(DirectedEdges(graph), rev_map);
end);

BindGlobal("LAD_GraphFromGRAPERec",
function(graph_rec)
	local vert_ids, graph, arc_rec, graph_arcs, seen_arcs, new_rev, arc_directions, has_parallel, idx, arc_id, rev_id, arc_dir, graph_data;

	graph := graph_rec.graph;

	vert_ids := Vertices(graph);
	vert_ids := List(vert_ids, x -> x^InverseGeneralMapping(graph_rec.vertex_id_map));

	arc_rec := rec();

	graph_arcs := DirectedEdges(graph);
	seen_arcs := [];

	new_rev := ();

	arc_directions := [];
	has_parallel := false;

	for idx in Range(graph_rec.arc_id_map) do
		arc_id := idx^InverseGeneralMapping(graph_rec.arc_id_map);
		rev_id := (idx^graph_rec.reverse_map)^InverseGeneralMapping(graph_rec.arc_id_map);
		arc_dir := [graph_arcs[idx][1]^InverseGeneralMapping(graph_rec.vertex_id_map), graph_arcs[idx][2]^InverseGeneralMapping(graph_rec.vertex_id_map)];

		arc_rec.(arc_id) := rec(origin := arc_dir[1], terminus := arc_dir[2], inverse := rev_id);
		if not arc_id in seen_arcs and arc_id <> rev_id then
			new_rev := new_rev * (arc_id, rev_id);
			Append(seen_arcs, [arc_id, rev_id]);
		fi;

		if not has_parallel and arc_dir in arc_directions then 
			has_parallel := true;
		else
			Add(arc_directions, arc_dir);
		fi;
	od;

	graph_data := rec();

	graph_data.vertices := vert_ids;
	graph_data.arc_ids := List(Source(graph_rec.arc_id_map));
	graph_data.arcs := arc_rec;
	graph_data.reverse_map := new_rev;
	graph_data.has_parallel := has_parallel; 
	
	LAD_RSGraphConsCheck@(graph_data.arcs, graph_data.reverse_map, graph_data.vertices, graph_data.arc_ids);

	return RSGraphConsNC(IsRSGraph, graph_data);
end);

BindGlobal("RSGraphFromGRAPE",
function(args...)
	local graph, error_message;

	error_message := Concatenation("Input must either be a record from RSGraphToGRAPE function ",
	                               "or a GRAPE graph and a reverse map for it.");
	                  
	if Size(args) = 1 then
		if not IsRecord(args[1]) then
			ErrorNoReturn(error_message);
		fi;
		
		graph := LAD_GraphFromGRAPERec(args[1]);
	elif Size(args) = 2 then
		if not (IsGraph(args[1]) and IsPerm(args[2])) then
			ErrorNoReturn(error_message);
		fi;
		
		graph := LAD_GraphFromGRAPEAndPerm(args[1], args[2]);
	else
		ErrorNoReturn(error_message);
	fi;

	return graph;
end);


