# Fix it so it's in terms of the RSGraph IDs. Right now it's in terms of
# the digraph IDs. 
InstallMethod(AutomorphismGroup, "Automorphism Group of RSGraph", [IsRSGraph],
function(graph)
	local digraph_rec, digraph, aut_grp, proj_1, proj_2, aut_grp_verts, img_arc_grp, aut_grp_arcs, MapGroup, arc_gen_list, gen, arc_list, arc_mapped, sort_perm, rs_aut_grp;

	digraph_rec := RSGraphToDigraph(graph);

	digraph := digraph_rec.digraph;

	# Function to conjugate a group by a
	# bijection (for the vertex and arc maps). 
	MapGroup := function(group, map1, map2)
		local gens, gen_maps, conj_gen_maps, conj_gens, conj_gen_map, src_list, dst_list, inv_map1, inv_map2, perm_list, idx, gen, cycles, cycle, map_cycles, gen1, gen2, new_gen;
		
		gens := GeneratorsOfGroup(group);
		inv_map1 := InverseGeneralMapping(map1);
		inv_map2 := InverseGeneralMapping(map2);

		conj_gens := [];

		for gen in gens do
			gen1 := gen[1];
			gen2 := gen[2];
			new_gen := [];

			cycles := Cycles(gen1, MovedPoints(gen1));
			map_cycles := [];
			for cycle in cycles do
				Add(map_cycles, CycleFromList(List(cycle, x -> x^inv_map1)));
			od;
			if IsEmpty(map_cycles) then
				Add(new_gen, ());
			else
				Add(new_gen, Product(map_cycles));
			fi;

			cycles := Cycles(gen2, MovedPoints(gen2));
			map_cycles := [];
			for cycle in cycles do
				Add(map_cycles, CycleFromList(List(cycle, x -> x^inv_map2)));
			od;
			if IsEmpty(map_cycles) then
				Add(new_gen, ());
			else
				Add(new_gen, Product(map_cycles));
			fi;

			Add(conj_gens, DirectProductElement(new_gen));
		od;

		return GroupByGenerators(conj_gens);
	end;

	# Get the automorphism group of the vertices. 
	if IsMultiDigraph(digraph) then
		aut_grp := AutomorphismGroup(digraph);
		proj_1 := Projection(aut_grp, 1); # Vertex maps. 
		proj_2 := Projection(aut_grp, 2); # Arc maps. 

		# Automorphism group of vertices. 
		aut_grp_verts := Image(proj_1, aut_grp); 

		# Automorphism group of the arcs (the extra mapping
		# due to the multiple arcs).
		aut_grp_arcs := Image(proj_2, aut_grp); 
	else
		aut_grp_verts := AutomorphismGroup(digraph);

		# No extra mappings due to multiple arcs. 
		aut_grp_arcs := Group(());
	fi;

	# For each generator of *aut_grp_verts* get an associated mapping
	# on the arcs. 
	arc_gen_list := [];

	arc_list := DigraphEdges(digraph);  

	for gen in GeneratorsOfGroup(aut_grp_verts) do
		# Apply the automorphism to each edge. 
		arc_mapped := List(arc_list, x -> [x[1]^gen, x[2]^gen]); #
		# Get the permutation that sorts the edges. 
		# This is the automorphism on the arcs. 
		sort_perm := SortingPerm(arc_mapped); 
		Add(arc_gen_list, DirectProductElement([gen, sort_perm]));
	od;

	arc_gen_list := Concatenation(arc_gen_list, List(GeneratorsOfGroup(aut_grp_arcs), x -> DirectProductElement([(), x])));

	rs_aut_grp := Centraliser(GroupByGenerators(arc_gen_list), DirectProductElement([(), digraph_rec.reverse_map]));

	return MapGroup(rs_aut_grp, digraph_rec.vertex_id_map, digraph_rec.arc_id_map);
end);

# Use the "flatten edges" Digrpahs function to get all possible edge permutations. 
# Then find the one compatible with the isomorphism and reverse map. 
InstallMethod(IsomorphismRSGraphs, "Isormorphiism between two RSGraphs", [IsRSGraph, IsRSGraph],
function(graph1, graph2)
	local digraph_rec1, digraph_rec2, digraph1, digraph2, base_iso, MapPerm, iso, aut, proj_1, proj_2, aut_grp, aut_vertex, aut_arc, vert_iso, arc_iso, edge_gp, edge, mults, start_counter, counter, current_item, digraph1_edges, perm, mapped_arcs, arcs_with_id;

	# Take the permutation *perm* on set [1..N] (= Range(map1) = Range(map2))
	# and make it the same map from Source(map1) to Source(map2). 
	MapPerm := function(perm, map1, map2)
		local inv_map1, inv_map2, dp_elm_list, i, elm, first_elm, second_elm;

		inv_map1 := InverseGeneralMapping(map1);
		inv_map2 := InverseGeneralMapping(map2);

		dp_elm_list := [];

		for i in Range(map1) do
			first_elm := i^inv_map1;
			second_elm := (i^perm)^inv_map2;
			Add(dp_elm_list, DirectProductElement([first_elm, second_elm]));
		od;

		return GeneralMappingByElements(Source(map1), Source(map2), dp_elm_list);
	end;

	digraph_rec1 := RSGraphToDigraph(graph1);
	digraph1 := digraph_rec1.digraph;
	digraph_rec2 := RSGraphToDigraph(graph2);
	digraph2 := digraph_rec2.digraph;

	base_iso := IsomorphismDigraphs(digraph1, digraph2);

	if base_iso = fail then
		return fail;
	fi;

	# If it's not a multigraph then there's no compatibility condition
	# with the reverse map to check. 
	if not IsList(base_iso) then
		vert_iso := MapPerm(base_iso, digraph_rec1.vertex_id_map, digraph_rec2.vertex_id_map);
		# If they are both permutations then return them as permutations. 
		if Source(vert_iso) = Range(vert_iso) then
			vert_iso := MappingPermListList(List(Source(vert_iso)), List(Source(vert_iso), x -> x^vert_iso));
		fi;
		return vert_iso;
	fi;

	# If it is a multigraph then the arc isomorphism from Digraphs 
	# may not be correct for our graphs. The vertex isomorphism 
	# will be correct. We need to calculate a correct "base" arc
	# isomorphism. 
	mapped_arcs := List(DigraphEdges(digraph1), x -> [x[1]^base_iso[1], x[2]^base_iso[1]]);
	base_iso[2] := SortingPerm(mapped_arcs);

	# Check if the base isomorphism is compatible with the reverse mappings (only needed for
	# multigraphs). 
	if base_iso[2]*digraph_rec1.reverse_map = digraph_rec2.reverse_map*base_iso[2] then
		vert_iso := MapPerm(base_iso[1], digraph_rec1.vertex_id_map, digraph_rec2.vertex_id_map);
		arc_iso := MapPerm(base_iso[2], digraph_rec1.arc_id_map, digraph_rec2.arc_id_map);
		# If they are both permutations then return them as permutations. 
		if Source(vert_iso) = Range(vert_iso) and Source(arc_iso) = Range(arc_iso) then
			vert_iso := MappingPermListList(List(Source(vert_iso)), List(Source(vert_iso), x -> x^vert_iso));
			arc_iso := MappingPermListList(List(Source(arc_iso)), List(Source(arc_iso), x -> x^arc_iso));
		fi;
		return [vert_iso, arc_iso];
	fi;

	digraph1_edges := DigraphEdges(digraph1); # 
	current_item := digraph1_edges[1]; # 
	counter := 1; # 
	start_counter := 1; # 
	mults := []; # 

	for edge in digraph1_edges do # 
		if edge <> current_item then
			current_item := edge;
			# If not only one edge. 
			if start_counter + 1 <> counter then
				Add(mults, [start_counter .. counter-1]);
			fi;
			start_counter := counter;
		fi;
		counter := counter + 1;
	od;

	edge_gp := Group(Flat(List(mults, x -> GeneratorsOfGroup(SymmetricGroup(x)))));

	for perm in edge_gp do
		iso := perm*base_iso[2];

		if iso*digraph_rec1.reverse_map = digraph_rec2.reverse_map*iso then
			vert_iso := MapPerm(base_iso[1], digraph_rec1.vertex_id_map, digraph_rec2.vertex_id_map);
			arc_iso := MapPerm(iso, digraph_rec1.arc_id_map, digraph_rec2.arc_id_map);

			# If they are both permutations then return them as permutations. 
			if Source(vert_iso) = Range(vert_iso) and Source(arc_iso) = Range(arc_iso) then
				vert_iso := MappingPermListList(List(Source(vert_iso)), List(Source(vert_iso), x -> x^vert_iso));
				arc_iso := MappingPermListList(List(Source(arc_iso)), List(Source(arc_iso), x -> x^arc_iso));
			fi;
			return [vert_iso, arc_iso];
		fi;
	od; 

	## If it's not then we need to find a compatible one. 
	#aut_grp := AutomorphismGroup(digraph1); 
	#proj_1 := Projection(aut, 1);
	#proj_2 := Projection(aut, 2);

	#for aut in aut_grp do
	#	aut_vertex := Image(proj_1, aut);
	#	aut_arc := Image(proj_2, aut);

	#	# Get a new isomorphism by first applying the automorphism
	#	# to digraph1 then the base isomorphism. 
	#	iso := [aut_vertex*base_iso[1], aut_arc*base_iso[2]];

	#	if iso[2]*digraph_rec1.reverse_map = digraph_rec2.reverse_map*iso[2] then
	#		vert_iso := MapPerm(iso[1], digraph_rec1.vertex_id_map, digraph_rec2.vertex_id_map);
	#		arc_iso := MapPerm(iso[2], digraph_rec1.arc_id_map, digraph_rec2.arc_id_map);
	#		return [vert_iso, arc_iso];
	#	fi;

	#od;

	# There's no isomorphism that respects the reverse map. 
	return fail;

end);












