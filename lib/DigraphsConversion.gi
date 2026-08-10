InstallMethod(RSGraphToDigraph, "Converts an RSGraph to a Digraph from the Digraphs package", [IsRSGraph],
function(graph)
	local standard_graph, digraph, adj_list, ret, arc;

	standard_graph := RSGraphToStandardForm(graph);

	adj_list := [];

	for arc in RSGraphArcIterator(standard_graph.graph) do
		Add(adj_list, [arc[2].origin, arc[2].terminus]);
	od;

	digraph := DigraphByEdges(adj_list);

	ret := rec();

	ret.digraph := digraph;
	ret.reverse_map := RSGraphReverseMap(standard_graph.graph);
	ret.vertex_id_map := standard_graph.vertex_id_map;
	ret.arc_id_map := standard_graph.arc_id_map;

	return ret;
end);

InstallMethod(RSGraphFromDigraph, "Converts a digraph from the Digraphs package to an RSGraph", [IsDigraph, IsPerm],
function(digraph, reverse_map)
	return RSGraphByAdjacencyList(DigraphEdges(digraph), reverse_map);
end);

InstallMethod(RSGraphFromDigraph, "Converts a digraph from the Digraphs package to an RSGraph", [IsRecord],
function(digraph_rec)
	local vert_ids, digraph, arc_rec, digraph_arcs, seen_arcs, new_rev, arc_directions, has_parallel, idx, arc_id, rev_id, arc_dir, graph_data;

	digraph := digraph_rec.digraph;

	vert_ids := [1..DigraphNrVertices(digraph)];
	vert_ids := List(vert_ids, x -> x^InverseGeneralMapping(digraph_rec.vertex_id_map));

	arc_rec := rec();

	digraph_arcs := DigraphEdges(digraph);
	seen_arcs := [];

	new_rev := ();

	arc_directions := [];
	has_parallel := false;

	for idx in Range(digraph_rec.arc_id_map) do
		arc_id := idx^InverseGeneralMapping(digraph_rec.arc_id_map);
		rev_id := (idx^digraph_rec.reverse_map)^InverseGeneralMapping(digraph_rec.arc_id_map);
		arc_dir := [digraph_arcs[idx][1]^InverseGeneralMapping(digraph_rec.vertex_id_map), digraph_arcs[idx][2]^InverseGeneralMapping(digraph_rec.vertex_id_map)];

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
	graph_data.arc_ids := List(Source(digraph_rec.arc_id_map));
	graph_data.arcs := arc_rec;
	graph_data.reverse_map := new_rev;
	graph_data.has_parallel := has_parallel; 
	
	LAD_RSGraphConsCheck@(graph_data.arcs, graph_data.reverse_map, graph_data.vertices, graph_data.arc_ids);

	return RSGraphConsNC(IsRSGraph, graph_data);
end);

