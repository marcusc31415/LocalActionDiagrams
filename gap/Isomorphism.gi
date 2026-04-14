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

			cycles := Cycles(gen, MovedPoints(gen));
			map_cycles := [];
			for cycle in cycles do
				Add(map_cycles, CycleFromList(List(cycle, x -> x^inv_map2)));
			od;
			if IsEmpty(map_cycles) then
				new_gen := ();
			else
				new_gen := Product(map_cycles);
			fi;

			Add(conj_gens, new_gen);
		od;

		if Size(conj_gens) = 0 then
			conj_gens := [()];
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
		Add(arc_gen_list, sort_perm);
	od;

	arc_gen_list := Concatenation(arc_gen_list, GeneratorsOfGroup(aut_grp_arcs));


	rs_aut_grp := Centraliser(GroupByGenerators(arc_gen_list), digraph_rec.reverse_map);

	return MapGroup(rs_aut_grp, digraph_rec.vertex_id_map, digraph_rec.arc_id_map);
end);

# Use the "flatten edges" Digrpahs function to get all possible edge permutations. 
# Then find the one compatible with the isomorphism and reverse map. 
InstallMethod(IsomorphismRSGraphs, "Isormorphism between two RSGraphs", [IsRSGraph, IsRSGraph],
function(graph1, graph2)
	local digraph_rec1, digraph_rec2, digraph1, digraph2, base_iso, MapPerm, iso, aut, proj_1, proj_2, aut_grp, aut_vertex, aut_arc, vert_iso, arc_iso, edge_gp, edge, mults, start_counter, counter, current_item, digraph1_edges, perm, mapped_arcs, arcs_with_id, arc_list, arc_mapped, sort_perm, graph1_v_ids, graph2_v_ids, g1_canon_cert, g2_canon_cert, g1_canon_label, g2_canon_label;;

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

	g1_canon_cert := RSGraphCanonicalCertificate(graph1);
	g2_canon_cert := RSGraphCanonicalCertificate(graph2);

	if g1_canon_cert <> g2_canon_cert then
		return fail;
	fi;

	g1_canon_label := RSGraphCanonicalLabelling(graph1);
	g2_canon_label := RSGraphCanonicalLabelling(graph2);


end);

# For an automorphism. 
InstallMethod(RSGraphVertexAutomorphism, "Returns associated vertex automorphism given an arc automorphism", [IsRSGraph, IsPerm],
function(graph, arc_aut)
	local v_perm, arc, v_ids_map, arc_id, arc_rec, v_id, id_list, id_list_map;

	v_ids_map := rec();

	for arc in RSGraphArcIterator(graph) do
		arc_id := arc[1];
		arc_rec := arc[2];
		if not String(arc_rec.origin) in RecNames(v_ids_map) then
			v_ids_map.(arc_rec.origin) := RSGraphArcs(graph).(arc_id^arc_aut).origin;
		fi;
	od;

	id_list := [];
	id_list_map := [];

	for v_id in RecNames(v_ids_map) do
		Add(id_list, Int(v_id));
		Add(id_list_map, v_ids_map.(v_id));
	od;

	return MappingPermListList(id_list, id_list_map);
	
end);

InstallMethod(RSGraphsVertexIsomorphism, "Returns associated vertex isomorphism given an arc isomorphism", [IsRSGraph, IsRSGraph, IsPerm],
function(graph1, graph2, arc_iso_perm)
	local dp_elms, arc_iso_map;

	dp_elms := List(RSGraphArcIDs(graph1), x -> DirectProductElement([x, x^arc_iso_perm]));

	arc_iso_map := GeneralMappingByElements(Domain(RSGraphArcIDs(graph1)), Domain(RSGraphArcIDs(graph2)), dp_elms);

	return RSGraphsVertexIsomorphism(graph1, graph2, arc_iso_map);
end);

InstallMethod(RSGraphsVertexIsomorphism, "Returns associated vertex isomorphism given an arc isomorphism", [IsRSGraph, IsRSGraph, IsGeneralMapping],
function(graph1, graph2, arc_iso_map)
	local vert_iso_map, v_ids_map, arc, arc_id, arc_rec, dp_elms;

	v_ids_map := rec();

	for arc in RSGraphArcIterator(graph1) do
		arc_id := arc[1];
		arc_rec := arc[2];
		if not String(arc_rec.origin) in RecNames(v_ids_map) then
			v_ids_map.(arc_rec.origin) := RSGraphArcs(graph2).(arc_id^arc_iso_map).origin;
		fi;
	od;

	dp_elms := List(RSGraphVertices(graph1), x -> DirectProductElement([x, v_ids_map.(x)]));

	vert_iso_map := GeneralMappingByElements(Domain(RSGraphVertices(graph1)), Domain(RSGraphVertices(graph2)), dp_elms);

	# It's a permutation. 
	if Source(vert_iso_map) = Range(vert_iso_map) and Source(arc_iso_map) = Range(arc_iso_map) then
		vert_iso_map := MappingPermListList(List(Source(vert_iso_map)), List(Source(vert_iso_map), x -> x^vert_iso_map));
	fi;

	return vert_iso_map;
end);

InstallMethod(RSGraphsIsomorphismsIterator, "Iterator of all isomorphisms between two RSGraphs", [IsRSGraph, IsRSGraph],
function(graph1, graph2)
	local NextIterator, IsDoneIterator, ShallowCopy, base_iso;

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
		local arc_aut, aut_vert_map, aut_arc_map, v_aut;
		arc_aut := iter!.aut_iter!.NextIterator(iter!.aut_iter);
		v_aut := RSGraphVertexAutomorphism(graph1, arc_aut);
		if IsPerm(iter!.iso[1]) and IsPerm(iter!.iso[2]) then
			return [v_aut*iter!.iso[1], arc_aut*iter!.iso[2]];
		elif IsGeneralMapping(iter!.iso[1]) and IsGeneralMapping(iter!.iso[2]) then
			aut_vert_map := GeneralMappingByElements(Source(iter!.iso[1]), Source(iter!.iso[1]), List(RSGraphVertices(iter!.graph1), x -> DirectProductElement([x, x^v_aut])));
			aut_arc_map := GeneralMappingByElements(Source(iter!.iso[2]), Source(iter!.iso[2]), List(RSGraphArcIDs(iter!.graph1), x -> DirectProductElement([x, x^arc_aut])));
			return [ aut_vert_map * iter!.iso[1], aut_arc_map * iter!.iso[2]];
		else
			ErrorNoReturn("Problem with isomorphism function output");
		fi;
	end;

	base_iso := IsomorphismRSGraphs(graph1, graph2);

	if base_iso <> fail then
		base_iso := [RSGraphsVertexIsomorphism(graph1, graph2, base_iso), base_iso];
	fi;

	return IteratorByFunctions(rec(
		NextIterator := NextIterator,
		IsDoneIterator := IsDoneIterator,
		ShallowCopy := ShallowCopy,
		iso := base_iso,
		aut_iter := Iterator(AutomorphismGroup(graph1)),
		graph1 := graph1,
		graph2 := graph2));
end);

InstallMethod(RSGraphCanonicalLabelling, "Returns the map that sends RSGraph to its canonical RSGraph.", [IsRSGraph],
function(graph)
	local digraph_rec, digraph_canon_vert, digraph_canon_arc, digraph_arcs_mapped, digraph_arcs, PermToMap, new_rev_map, canon_labelling, adj_mat, v_labels_mapped, vert_perm, perm_mat, canon_adj_mat, canon_cert, arc_id_rec, arc_id, idx_x, idx_y, idx, arc_string, moved_arcs, moved_arc_list, moved_rev_points, arc_id_list, lex_arc_id_list, current_arc_id, seen_arcs, current_self_rev_id, current_non_self_id, lex_perm, current_rev, arc_ids;

	PermToMap := function(perm, domain)
		local dp_elms;

		dp_elms := List(domain, x -> DirectProductElement([x, x^perm]));

		return GeneralMappingByElements(domain, domain, dp_elms);
	end;
	
	digraph_rec := RSGraphToDigraph(graph);

	# Get the vertex mapping. 
	digraph_canon_vert := BlissCanonicalLabelling(digraph_rec.digraph);
	if IsList(digraph_canon_vert) then
		digraph_canon_vert := digraph_canon_vert[1];
	fi;

	# Get the arc mapping. 
	digraph_arcs := DigraphEdges(digraph_rec.digraph);
	digraph_arcs_mapped := List(digraph_arcs, x -> [x[1]^digraph_canon_vert, x[2]^digraph_canon_vert]);
	digraph_canon_arc := SortingPerm(digraph_arcs_mapped);

	# Have the reverse map work on the new arc mapping. 
	new_rev_map := Inverse(digraph_canon_arc)*digraph_rec.reverse_map*digraph_canon_arc;


	# Make the mappings from the RSGraph to the "standard range" [1..N]. 
	digraph_canon_vert := digraph_rec.vertex_id_map*PermToMap(digraph_canon_vert, Range(digraph_rec.vertex_id_map));
	digraph_canon_arc := digraph_rec.arc_id_map*PermToMap(digraph_canon_arc, Range(digraph_rec.arc_id_map));

	if Source(digraph_canon_vert) = Range(digraph_canon_vert) and Source(digraph_canon_arc) = Range(digraph_canon_arc) then
		digraph_canon_vert := MappingPermListList(List(Source(digraph_canon_vert)), List(Source(digraph_canon_vert), x -> x^digraph_canon_vert));
		digraph_canon_arc := MappingPermListList(List(Source(digraph_canon_arc)), List(Source(digraph_canon_arc), x -> x^digraph_canon_arc));
	fi;

	canon_labelling := [digraph_canon_vert, digraph_canon_arc, new_rev_map];


	# Calculate the canonical certificate. 
	adj_mat := RSGraphAdjacencyMatrix(graph);

	if not IsPerm(canon_labelling[1]) then
		v_labels_mapped := List(RSGraphVertices(graph), x -> x^canon_labelling[1]);
		vert_perm := SortingPerm(v_labels_mapped);
	else
		vert_perm := canon_labelling[1];
	fi;

	perm_mat := PermutationMat(vert_perm, Size(adj_mat), 1);
	canon_adj_mat := Inverse(perm_mat)*adj_mat*perm_mat;

	canon_cert := String(canon_adj_mat);

	arc_id_rec := rec();

	arc_id := 0;

	for idx_x in [1..Size(canon_adj_mat)] do
		for idx_y in [1..Size(canon_adj_mat)] do
			arc_string := StringFormatted("{1},{2}", idx_x, idx_y);

			# Get the arc ids for arc from idx_x to idx_y. 
			arc_id_rec.(arc_string) := List([1..canon_adj_mat[idx_x][idx_y]], x -> arc_id + x);
			arc_id := arc_id + canon_adj_mat[idx_x][idx_y];
		od;
	od;

	moved_arc_list := List([1..Size(adj_mat)], x -> 0);
	moved_rev_points := MovedPoints(canon_labelling[3]);

	# For each vertex find the number of non-self-reverse loops.  
	for idx in [1..Size(adj_mat)] do
		arc_string := StringFormatted("{1},{2}", idx, idx);
		moved_arcs := Intersection(moved_rev_points, arc_id_rec.(arc_string));
		moved_arc_list[idx] := moved_arcs;
		canon_cert := Concatenation(canon_cert, StringFormatted("{1};", Size(moved_arcs)));
	od;

	# Calculate the "lexicographically standard" arc map. 
	arc_id_list := [1..RSGraphNumberArcs(graph)];
	lex_arc_id_list := List(arc_id_list, x -> 0);

	current_arc_id := 1;
	seen_arcs := [];
	for idx_x in [1..Size(canon_adj_mat)] do
		for idx_y in [1..Size(canon_adj_mat)] do
			arc_string := StringFormatted("{1},{2}", idx_x, idx_y);
			arc_ids := arc_id_rec.(arc_string);
			# If there are arcs between the two vertices. 
			if Size(arc_id_rec.(arc_string)) > 0 then 
				if idx_x = idx_y then
					# If it's a loop. 
					moved_arcs := moved_arc_list[idx_x];
					current_self_rev_id := current_arc_id+Size(moved_arcs);
					current_non_self_id := current_arc_id;

					for arc_id in arc_ids do
						if arc_id in moved_arcs and not arc_id in seen_arcs then
							# If it's not self reverse then make it and its reverse
							# the next at the "start" of the loop arc ids. Add both
							# to the seen arcs list to stop this from happening when
							# the reverse is reached. 
							lex_arc_id_list[current_non_self_id] := arc_id;
							lex_arc_id_list[current_non_self_id+1] := arc_id^canon_labelling[3];
							current_non_self_id := current_non_self_id + 2;
							Add(seen_arcs, arc_id);
							Add(seen_arcs, arc_id^canon_labelling[3]);
							current_arc_id := current_arc_id + 1;
						elif not arc_id in seen_arcs then
							# Otherwise add it to the "middle to end" of the loop. 
							lex_arc_id_list[current_self_rev_id] := arc_id;
							current_self_rev_id := current_self_rev_id + 1;
							Add(seen_arcs, arc_id);
							current_arc_id := current_arc_id + 1;
						else
							current_arc_id := current_arc_id + 1;
						fi;
					od;

				else
					current_rev := Minimum(arc_id_rec.(Reversed(arc_string)));
					for arc_id in arc_ids do
						if not arc_id in seen_arcs then
							lex_arc_id_list[current_arc_id] := arc_id;
							lex_arc_id_list[current_rev] := arc_id^canon_labelling[3];
							Add(seen_arcs, arc_id);
							Add(seen_arcs, arc_id^canon_labelling[3]);
						fi;
						current_rev := current_rev + 1;
						current_arc_id := current_arc_id+1;
					od;
				fi;
			fi;
		od;
	od;

	lex_perm := MappingPermListList(lex_arc_id_list, arc_id_list);

	canon_labelling := Concatenation(canon_labelling, [lex_perm, canon_cert]);

	return canon_labelling;
end);

InstallMethod(RSGraphCanonicalCertificate, "two graphs have the same certificate if and only if they are isomorphic", [IsRSGraph],
function(graph)
	local canon_labelling, canon_cert, adj_mat, perm_mat, standard_form, out_arcs, idx, v_out_arcs, moved_arcs, v_loops, arc_id, canon_adj_mat, idx_x, idx_y, arc_id_rec, arc_string, vert_perm, v_labels_mapped;

	canon_labelling := RSGraphCanonicalLabelling(graph);

	return canon_labelling[5];
end);


InstallMethod(IsomorphismLocalActionDiagrams, "Isormorphism between two LocalActionDiagrams", [IsLocalActionDiagram, IsLocalActionDiagram],
function(lad1, lad2)
	local iso, lad_iso, graph1, graph2, vert_id, arc_id, bijections, out_arc_ids, labels_original, labels_mapped, labels_original_flat, labels_mapped_flat, perm, perms, labels_mapped_flat_perm, bijection, BijectionMap, ConjugateBijection, bad_iso, G1, G2, S, norm, all_conj, found_perm, labels_bijection_mapped, labels_bijection_flat, idx;

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

		if Size(new_gens) = 0 then
			return Group(());
		else
			return GroupByGenerators(new_gens);
		fi;
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
			G1 := LocalActionDiagramVertexLabels(lad1).(vert_id);
			G2 := LocalActionDiagramVertexLabels(lad2).(vert_id^iso[1]);

			if G1 = Group(()) and G2 = Group(()) then
				bijection := ();
			else
				S := SymmetricGroup(Maximum(Union(MovedPoints(G1), MovedPoints(G2))));

				# Find a permutation that conjugates G1 into G2. 
				bijection := RepresentativeAction(S, G1, G2);
			fi;

			if bijection = fail then
				bijections := fail;
				break;
			fi;


			# The bijection found does not take into account fixed points 
			# from PermGroupDomain (using RepresentativeAction with that 
			# causes a significant slowdown). This corrects the fixed point
			# mapping in the bijection by first "undoing" the movement of it
			# and then multiplying by the transposition which is the correct
			# movement of it. Since the orbits are disjoint this works. 
			for idx in [1..Size(labels_original)] do
				if Size(labels_original[idx]) = 1 then
					if labels_original[idx][1] <> labels_original[idx][1]^bijection then
						bijection := bijection * (labels_original[idx][1], labels_original[idx][1]^bijection);
					fi;

					if labels_original[idx][1] <> labels_mapped[idx][1] then
						bijection := bijection * (labels_original[idx][1], labels_mapped[idx][1]);
					fi;
				fi;
			od;

			labels_original_flat := Flat(labels_original);
			labels_mapped_flat := Flat(labels_mapped);
			labels_bijection_flat := List(labels_original_flat, x -> x^bijection);

			# If the bijection found does not map the orbits correctly then we multiply
			# by the transpositions that swap what the bijection maps with something
			# in the correct orbit. Because these swap orbit the product of these 
			# transpositions commute with the bijection so this is still a conjugation
			# of G1 into G2. 
			#
			if labels_bijection_flat <> labels_mapped_flat then
				#for idx in [1..Size(labels_original_flat)] do
				#	if labels_bijection_flat[idx] <> labels_mapped_flat[idx] then
				#		bijection := bijection * (labels_bijection_flat[idx], labels_mapped_flat[idx]);
				#	fi;
				#od;
				bijection := bijection * MappingPermListList(labels_bijection_flat, labels_mapped_flat);
			fi;

			if G1^bijection <> G2 then
				bijections := fail;
				break;
			fi;

			bijections.(vert_id) := BijectionMap(labels_original_flat, List(labels_original_flat, x -> x^bijection));

			#####  #######  # Get every permutation of labels_mapped that respects the partition. 
			#####  #perms := DirectProduct(List(labels_mapped, x -> SymmetricGroup(Size(x))));


			#####  #labels_original_flat := Flat(labels_original);
			#####  #labels_mapped_flat := Flat(labels_mapped);

			#####  #bijection := fail;
			#####  #for perm in perms do
			#####  #	labels_mapped_flat_perm := Permuted(Flat(labels_mapped_flat), perm);


			#####  #	bijection := BijectionMap(labels_original_flat, labels_mapped_flat_perm);
			#####  #	
			#####  #	# If this is a conjugate bijection add it to the bijection list and
			#####  #	# break out of this loop. 
			#####  #	if ConjugateBijection(bijection, LocalActionDiagramVertexLabels(lad1).(vert_id)) = LocalActionDiagramVertexLabels(lad2).(vert_id^iso[1]) then
			#####  #		bijections.(vert_id) := bijection;
			#####  #		break;
			#####  #	fi;

			#####  #	bijection := fail;
			#####  #od;

			#####  ## This vertex map doesn't work so we break out of the loop and continue 
			#####  ## to the next isomorphism. 
			#####  #if bijection = fail then
			#####  #	bijections := fail;
			#####  #	break;
			#####  #fi;
		od;

		# A conjugate bijection was found for each vertex under this isomorphism. 
		# This is an RSGraph isomorphism. 
		if bijections <> fail then
			return [iso[1], iso[2], bijections];
		fi;
	od;

	return fail; 

end);

InstallMethod(AllLocalActionDiagrams, "enumerates local action diagrams up to isomorphism", [IsInt, IsInt],
function(no_vert, degree)
	local lad_list, CSubG, rev_maps, G, arc_labels, rev_map, graph, lad, iso_lad, lad2, Order2Perm;

	Order2Perm := function(domain)
		local tr, elms, t, points, elm;

		if Size(domain) in [0,1] then 
			return [()]; 
		fi;
		
		tr := List(Combinations(domain,2), c->(c[1],c[2]));	
		elms := tr;
		for t in Difference(tr,[()]) do
			points := MovedPoints(t);
			for elm in Order2Perm(Difference(domain, Union(points,[1..Minimum(points)]))) do
				Add(elms, t*elm);
			od;
		od;
		return Union(elms, [()]);
	end;

	lad_list := [];

	# Get all subgroups of Sym(degree) up the conjugacy. 
	CSubG := List(ConjugacyClassesSubgroups(SymmetricGroup(degree)), Representative);


	for G in CSubG do
		SetPermGroupDomain(G, [1..degree]);
		arc_labels := List(Orbits(G, PermGroupDomain(G)), Set);
		rev_maps := Order2Perm([1..Size(arc_labels)]);
		
		for rev_map in rev_maps do
			graph := RSGraphByAdjacencyList(List(arc_labels, x -> [1, 1]), rev_map);

			# Permuting of arc labels is taken care of by going through each possible
			# reverse map. 
			lad := LocalActionDiagramFromData(graph, [G], arc_labels);

			iso_lad := false;
			for lad2 in lad_list do
				if IsomorphismLocalActionDiagrams(lad2, lad) <> fail then
					iso_lad := true;
					break;
				fi;
			od;

			if not iso_lad then
				Add(lad_list, lad);
			fi;
		od;
	od;

	return lad_list;
end);

InstallMethod(AllRSGraphs, "Enumerate RS Graphs with valency <= d and n vertices", [IsInt, IsInt],
function(degree, no_verts)
	local AllRSGraphsRecursive, start_graph, connected_components, start_vertex, list_of_graphs, MergeConnectedComponents, ShallowCopyLists, arc_list, list_of_rsgraphs, graph, arcs, v_adjacency, v_adjacency_idx, idx, rev_map, loop_ids, vert, arc, loop_ids_by_vertex, current_vertex, loop_perms, perm, loop_id, list_of_rsgraphs_isomorphism, graph2, iso_graphs, Order2Perm;

	list_of_graphs := [];

	ShallowCopyLists := x -> List(x, y -> ShallowCopy(y));

	Order2Perm := function(domain)
		local tr, elms, t, points, elm;

		if Size(domain) in [0,1] then 
			return [()]; 
		fi;
		
		tr := List(Combinations(domain,2), c->(c[1],c[2]));	
		elms := tr;
		for t in Difference(tr,[()]) do
			points := MovedPoints(t);
			for elm in Order2Perm(Difference(domain, Union(points,[1..Minimum(points)]))) do
				Add(elms, t*elm);
			od;
		od;
		return Union(elms, [()]);
	end;


	MergeConnectedComponents := function(connected_components, vertex1, vertex2)
		local new_connected_components, first_component_found, component, new_component;

		new_connected_components := [];
		first_component_found := false;

		for component in connected_components do
			if vertex1 in component and vertex2 in component then
				new_connected_components := ShallowCopyLists(connected_components);
				break;
			elif not (vertex1 in component or vertex2 in component) then
				Add(new_connected_components, component);
			elif not first_component_found then
				new_component := component;
				first_component_found := true;
			else
				new_component := Union(new_component, component);
				Add(new_connected_components, new_component);
			fi;
		od;

		return new_connected_components;

	end;



	AllRSGraphsRecursive := function(graph, current_vertex, connected_components)
		local new_vertex, new_graph, new_conn_components, component;


		if current_vertex = no_verts+1 then
			if Size(connected_components) = 1 then
				# Sort each of the sub-lists. 
				for arc_list in graph do 
					Sort(arc_list);
				od;

				if String(graph) = "[ [ 1, 2, 2 ], [ 1, 1 ], [ 3 ] ]" then
					Error("HUH?");
				fi;
				Add(list_of_graphs, graph);
			fi;
			return;
		fi;

		for component in connected_components do
			if Maximum(component) < current_vertex then
				return;
			fi;
		od;


		if Size(graph[current_vertex]) = 0 then
			for new_vertex in [current_vertex..no_verts] do 
				new_graph := ShallowCopyLists(graph);
				Add(new_graph[current_vertex], new_vertex);
				if new_vertex <> current_vertex then
					Add(new_graph[new_vertex], current_vertex);
				fi;

				new_conn_components := MergeConnectedComponents(connected_components, current_vertex, new_vertex);

				AllRSGraphsRecursive(new_graph, current_vertex, new_conn_components);
			od;
		elif Size(graph[current_vertex]) > degree then
			return;
		elif Size(graph[current_vertex]) < degree then
			AllRSGraphsRecursive(ShallowCopyLists(graph), current_vertex+1, connected_components);

			for new_vertex in [current_vertex..no_verts] do 
				new_graph := ShallowCopyLists(graph);
				Add(new_graph[current_vertex], new_vertex);
				if new_vertex <> current_vertex then
					Add(new_graph[new_vertex], current_vertex);
				fi;

				new_conn_components := MergeConnectedComponents(connected_components, current_vertex, new_vertex);

				AllRSGraphsRecursive(new_graph, current_vertex, new_conn_components);
			od;

		else
			AllRSGraphsRecursive(ShallowCopyLists(graph), current_vertex+1, connected_components);
		fi;
	end;


	start_graph := List([1..no_verts], x -> []);
	connected_components := List([1..no_verts], x -> [x]);
	start_vertex := 1;

	AllRSGraphsRecursive(start_graph, start_vertex, connected_components);

	# Remove all duplicates (the sub-lists are sorted). 
	list_of_graphs := Set(list_of_graphs);

	# Construct all the RSGraphs from the list.
	
	list_of_rsgraphs := []; 

	for graph in list_of_graphs do
		arcs := [];

		# Create an adjacency list from the list of vertex adjacencies. 
		for idx in [1..Size(graph)] do
			v_adjacency := graph[idx]; 
			arcs := Concatenation(arcs, List(v_adjacency, x -> [idx, x]));
		od;

		# Reverse each arc (so [i, j] becomes [j, i]) then find the
		# permutation that sorts the list. This is the reverse map 
		# which doesn't touch the loops. This is because arc [j, i]
		# is in the position of arc [i, j] from the first list and
		# arc [i, j] is in the position of arc [j, i]. The sorting
		# perm will move arc [j, i] to the position [i, j] is in. 
		#
		# We only need one not taking into account the loops because
		# any other compatible maps on arcs between two vertices will
		# create an isomorphic RSGraph. 
		rev_map := SortingPerm(List(arcs, x -> Reversed(x)));

		# The loops are the fixed points of the sorting. 
		loop_ids := Difference([1..Size(arcs)], MovedPoints(rev_map));

		loop_ids_by_vertex := [];

		current_vertex := 0;

		for loop_id in loop_ids do
			Assert(1, arcs[loop_id][1] = arcs[loop_id][2]);

			if current_vertex <> arcs[loop_id][1] then
				Add(loop_ids_by_vertex, []);
				current_vertex := arcs[loop_id][1];
			fi;

			Add(Last(loop_ids_by_vertex), loop_id);
		od;

		if Size(loop_ids_by_vertex) = 0 then
			loop_perms := Group(());
		else
			# For each "loop_ids_by_vertex" get the list of every order 2 
			# permutation on that set. 
			#
			# Take the Cartesian product of these sets obtained. Then take the
			# product of these cycles. This will be every order 2 permutation 
			# on the lists. 
			loop_perms := Cartesian(List(loop_ids_by_vertex, x -> Order2Perm(x)));
			loop_perms := List(loop_perms, x -> Product(x));
		fi;

		for perm in loop_perms do
			Assert(1, Size(Intersection(MovedPoints(rev_map), MovedPoints(perm))) = 0);
			Add(list_of_rsgraphs, RSGraphByAdjacencyList(arcs, rev_map*perm));
		od;
	od;

	list_of_rsgraphs_isomorphism := [];
	iso_graphs := false;

	for graph in list_of_rsgraphs do
		for graph2 in list_of_rsgraphs_isomorphism do
			#if IsomorphismRSGraphs(graph, graph2) <> fail then
			#	iso_graphs := true;
			#	break;
			#elif RSGraphCanonicalCertificate(graph) = RSGraphCanonicalCertificate(graph2) then
			#	Error("WHYYYYY?????");
			#fi;
			if RSGraphCanonicalCertificate(graph) = RSGraphCanonicalCertificate(graph2) then
				iso_graphs := true;
				break;
			fi;

		od;
		if not iso_graphs then # iso_graphs = false
			Add(list_of_rsgraphs_isomorphism, graph);
		else # iso_graphs = true
			iso_graphs := false;
		fi;
	od;

	return list_of_rsgraphs_isomorphism;
end);
