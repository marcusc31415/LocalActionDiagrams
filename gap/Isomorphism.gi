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

		if Size(conj_gens) = 0 then
			conj_gens := [DirectProductElement([(), ()])];
		fi;

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
		# Can use this one as digraphs have arc labels [1..N]. 
		sort_perm := SortingPerm(arc_mapped); 
		Add(arc_gen_list, DirectProductElement([gen, sort_perm]));
	od;

	arc_gen_list := Concatenation(arc_gen_list, List(GeneratorsOfGroup(aut_grp_arcs), x -> DirectProductElement([(), x])));


	rs_aut_grp := Centraliser(GroupByGenerators(arc_gen_list), DirectProductElement([(), digraph_rec.reverse_map]));

	return MapGroup(rs_aut_grp, digraph_rec.vertex_id_map, digraph_rec.arc_id_map);
end);

# Use the "flatten edges" Digrpahs function to get all possible edge permutations. 
# Then find the one compatible with the isomorphism and reverse map. 
InstallMethod(IsomorphismRSGraphs, "Isormorphism between two RSGraphs", [IsRSGraph, IsRSGraph],
function(graph1, graph2)
	local digraph_rec1, digraph_rec2, digraph1, digraph2, base_iso, MapPerm, iso, aut, proj_1, proj_2, aut_grp, aut_vertex, aut_arc, vert_iso, arc_iso, edge_gp, edge, mults, start_counter, counter, current_item, digraph1_edges, perm, mapped_arcs, arcs_with_id, arc_list, arc_mapped, sort_perm;

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
		
		# Get an arc isomorphism from the vertex isomorphism. 
		arc_list := DigraphEdges(digraph1);
		arc_mapped := List(arc_list, x -> [x[1]^vert_iso, x[2]^vert_iso]); #
		sort_perm := SortingPerm(arc_mapped); # Sorting this list gives the arc isomorphism. 
		arc_iso := MapPerm(sort_perm, digraph_rec1.arc_id_map, digraph_rec2.arc_id_map);

		# If they are both permutations then return them as permutations. 
		if Source(vert_iso) = Range(vert_iso) and Source(arc_iso) = Range(arc_iso) then
			vert_iso := MappingPermListList(List(Source(vert_iso)), List(Source(vert_iso), x -> x^vert_iso));
			arc_iso := MappingPermListList(List(Source(arc_iso)), List(Source(arc_iso), x -> x^arc_iso));
		fi;
		return [vert_iso, arc_iso];
	fi;

	# If it is a multigraph then the arc isomorphism from Digraphs 
	# may not be correct for our graphs. The vertex isomorphism 
	# will be correct. We need to calculate a correct "base" arc
	# isomorphism. 
	mapped_arcs := List(DigraphEdges(digraph1), x -> [x[1]^base_iso[1], x[2]^base_iso[1]]);
	# Can use this one as digraphs have arc labels [1..N]. 
	base_iso[2] := SortingPerm(mapped_arcs);

	# Check if the base isomorphism is compatible with the reverse mappings (only needed for
	# multigraphs). 
	if base_iso[2]*digraph_rec2.reverse_map = digraph_rec1.reverse_map*base_iso[2] then
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

	# There's no isomorphism that respects the reverse map. 
	return fail;

end);

InstallMethod(RSGraphsIsomorphismsIterator, "Iterator of all isomorphisms between two RSGraphs", [IsRSGraph, IsRSGraph],
function(graph1, graph2)
	local NextIterator, IsDoneIterator, ShallowCopy;

	ShallowCopy := function(iter)
		return rec(
			graph1 := iter!.graph1,
			graph2 := iter!.graph2,
			iso := iter!.iso,
			aut_iter := iter!.aut_iter);
	end;

	IsDoneIterator := function(iter)
		return (iter!.iso = fail) or (iter!.aut_iter!.IsDoneIterator(iter!.aut_iter));
	end;

	NextIterator := function(iter)
		local aut, aut_vert_map, aut_arc_map;
		aut := iter!.aut_iter!.NextIterator(iter!.aut_iter);
		if IsPerm(iter!.iso[1]) and IsPerm(iter!.iso[2]) then
			return DirectProductElement([aut[1]*iter!.iso[1], aut[2]*iter!.iso[2]]);
		elif IsGeneralMapping(iter!.iso[1]) and IsGeneralMapping(iter!.iso[2]) then
			aut_vert_map := GeneralMappingByElements(Source(iter!.iso[1]), Source(iter!.iso[1]), List(RSGraphVertices(iter!.graph1), x -> DirectProductElement([x, x^aut[1]])));
			aut_arc_map := GeneralMappingByElements(Source(iter!.iso[2]), Source(iter!.iso[2]), List(RSGraphArcIDs(iter!.graph1), x -> DirectProductElement([x, x^aut[2]])));
			return DirectProductElement([ aut_vert_map * iter!.iso[1], aut_arc_map * iter!.iso[2]]);
		else
			ErrorNoReturn("Problem with isomorphism function output");
		fi;
	end;

	return IteratorByFunctions(rec(
		NextIterator := NextIterator,
		IsDoneIterator := IsDoneIterator,
		ShallowCopy := ShallowCopy,
		iso := IsomorphismRSGraphs(graph1, graph2),
		aut_iter := Iterator(AutomorphismGroup(graph1)),
		graph1 := graph1,
		graph2 := graph2));
end);


InstallMethod(IsomorphismLocalActionDiagrams, "Isormorphism between two LocalActionDiagrams", [IsLocalActionDiagram, IsLocalActionDiagram],
function(lad1, lad2)
	local iso, lad_iso, graph1, graph2, vert_id, arc_id, bijections, out_arc_ids, labels_original, labels_mapped, labels_original_flat, labels_mapped_flat, perm, perms, labels_mapped_flat_perm, bijection, BijectionMap, ConjugateBijection, bad_iso, G1, G2, S, norm, all_conj, found_perm, labels_bijection_mapped;

	# Return the general mapping which maps list1[i] to list2[i]. 
	BijectionMap := function(list1, list2)
		local dp_elms;

		dp_elms := List([1..Size(list1)], idx -> DirectProductElement([list1[idx], list2[idx]]));
		return GeneralMappingByElements(Domain(list1), Domain(list2), dp_elms);
	end;

	ConjugateBijection := function(map, group)
		local generator_maps, gen, new_generator_maps, new_gens, range_list, gen_map, dp_list;

		generator_maps := [];

		for gen in GeneratorsOfGroup(group) do
			dp_list := List(PermGroupDomain(group), x -> DirectProductElement([x, x^gen]));
			gen_map := GeneralMappingByElements(Domain(PermGroupDomain(group)), Domain(PermGroupDomain(group)), dp_list);
			Add(generator_maps, gen_map);
		od;


		# Conjugate by *map*. (left to right composition). 
		new_generator_maps := List(generator_maps, x -> InverseGeneralMapping(map)*x*map);

		new_gens := [];
		for gen_map in new_generator_maps do
			range_list := SortedList(List(Range(map)));
			# Apply gen_map to to the (sorted) list that map can map to
			# (i.e. the elements that the second group acts on).
			gen := MappingPermListList(range_list, List(range_list, x -> x^gen_map));
			Add(new_gens, gen);
		od;



		return GroupByGenerators(new_gens);
	end;

	graph1 := LocalActionDiagramRSGraph(lad1);
	graph2 := LocalActionDiagramRSGraph(lad2);


	for iso in RSGraphsIsomorphismsIterator(graph1, graph2) do
		# First check the arc maps sets of the same size to each other. 
		# If not then this isomorphism doesn't work. 
		bad_iso := false;
		for arc_id in LocalActionDiagramArcIDs(lad1) do
			if Size(LocalActionDiagramArcLabels(lad1).(arc_id)) <> Size(LocalActionDiagramArcLabels(lad2).(arc_id^iso[2])) then 
				bad_iso := true;
				break;
			fi;
		od;

		if bad_iso then
			continue;
		fi;

		# Now check that the group labels are isomorphic to each other.  
		# If not then this isomorphism doesn't work.  
		for vert_id in LocalActionDiagramVertices(lad1) do
			if IsomorphismGroups(LocalActionDiagramVertexLabels(lad1).(vert_id), LocalActionDiagramVertexLabels(lad2).(vert_id^iso[1])) = fail then
				bad_iso := true;
				break;
			fi;
		od;

		if bad_iso then
			continue;
		fi;


		# Now search for conjugate bijections for the vertex labels. 
		bijections := rec();

		for vert_id in LocalActionDiagramVertices(lad1) do
			out_arc_ids := LocalActionDiagramOutArcs(lad1).(vert_id);
			labels_original := [];
			labels_mapped := [];

			# labels_original[i] must be mapped to labels_mapped[i] (from isomorphism 
			# definition to Reid-Smith. This reduces the search space for the conjugate 
			# bijections. 
			for arc_id in out_arc_ids do
				Add(labels_original, LocalActionDiagramArcLabels(lad1).(arc_id));
				Add(labels_mapped, LocalActionDiagramArcLabels(lad2).(arc_id^iso[2]));
			od;

			#### Might be slower than the "brute force" method? ####

			####G1 := LocalActionDiagramVertexLabels(lad1).(vert_id);
			####G2 := LocalActionDiagramVertexLabels(lad2).(vert_id^iso[1]);
			####S := SymmetricGroup(Maximum(Union(MovedPoints(G1), MovedPoints(G2))));

			##### Find a permutation that conjugates G1 into G2. 
			####bijection := RepresentativeAction(S, G1, G2);

			####if bijection = fail then
			####	bijections := fail;
			####	break;
			####fi;

			##### Get every such permutation to find one with the right 
			##### orbit structure. 
			####norm := Normaliser(S, G1); 
			####all_conj := RightCoset(norm, bijection);

			####found_perm := false;
			####for perm in all_conj do
			####	if OnTuplesTuples(labels_original, perm) = labels_mapped then
			####		bijection := perm;
			####		found_perm := true;
			####		break;
			####	fi;
			####od;

			####if not found_perm then
			####	bijections := fail;
			####	break;
			####fi;

			####labels_original_flat := Flat(labels_original);
			####labels_bijection_mapped := List(labels_original_flat, x -> x^bijection);
	

			####bijections.(vert_id) := BijectionMap(labels_original_flat, labels_bijection_mapped);

			# Get every permutation of labels_mapped that respects the partition. 
			perms := DirectProduct(List(labels_mapped, x -> SymmetricGroup(Size(x))));


			labels_original_flat := Flat(labels_original);
			labels_mapped_flat := Flat(labels_mapped);

			bijection := fail;
			for perm in perms do
				labels_mapped_flat_perm := Permuted(Flat(labels_mapped_flat), perm);


				bijection := BijectionMap(labels_original_flat, labels_mapped_flat_perm);
				
				# If this is a conjugate bijection add it to the bijection list and
				# break out of this loop. 
				if ConjugateBijection(bijection, LocalActionDiagramVertexLabels(lad1).(vert_id)) = LocalActionDiagramVertexLabels(lad2).(vert_id^iso[1]) then
					bijections.(vert_id) := bijection;
					break;
				fi;

				bijection := fail;
			od;

			# This vertex map doesn't work so we break out of the loop and continue 
			# to the next isomorphism. 
			if bijection = fail then
				bijections := fail;
				break;
			fi;
		od;

		# A conjugate bijection was found for each vertex under this isomorphism. 
		# This is an RSGraph isomorphism. 
		if bijections <> fail then
			return [iso[1], iso[2], bijections];
		fi;
	od;

	return fail; 

end);




