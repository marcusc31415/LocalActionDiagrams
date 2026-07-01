MakeReadWriteGlobal("LAD_IsomorphismAvailable@");
LAD_IsomorphismAvailable@ := true;
MakeReadOnlyGlobal("LAD_IsomorphismAvailable@");

###
# Make a new filter for IsObject to be able to use RedispatchOnCondition thing. 
# Or have the method installed previously call a global function and have this
# extension overwrite the global function.  
###

# Fix it so it's in terms of the RSGraph IDs. Right now it's in terms of
# the digraph IDs. 
InstallMethod(AutomorphismGroup, "Automorphism Group of RSGraph", [IsRSGraph], SUM_FLAGS + 100,
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

	SetLAD_RSGraphNonReverseAutomorphisms@(graph, MapGroup(GroupByGenerators(arc_gen_list), digraph_rec.vertex_id_map, digraph_rec.arc_id_map));

	rs_aut_grp := Centraliser(GroupByGenerators(arc_gen_list), digraph_rec.reverse_map);

	return MapGroup(rs_aut_grp, digraph_rec.vertex_id_map, digraph_rec.arc_id_map);
end);

# Use the "flatten edges" Digrpahs function to get all possible edge permutations. 
# Then find the one compatible with the isomorphism and reverse map. 
InstallMethod(IsomorphismRSGraphs, "Isormorphism between two RSGraphs", [IsRSGraph, IsRSGraph],
function(graph1, graph2)
	local MapPerm, g1_canon_label, g2_canon_label, g1_canon_cert, g2_canon_cert, ism, dp_elms; 

	if RSGraphNumberVertices(graph1) <> RSGraphNumberVertices(graph2) then
		return fail;
	fi;

	if RSGraphNumberArcs(graph1) <> RSGraphNumberArcs(graph2) then
		return fail;
	fi;

	g1_canon_label := RSGraphCanonicalLabelling(graph1);
	g2_canon_label := RSGraphCanonicalLabelling(graph2);

	g1_canon_cert := g1_canon_label.canon_certificate;
	g2_canon_cert := g2_canon_label.canon_certificate;

	if g1_canon_cert <> g2_canon_cert then
		return fail;
	fi;

	ism := g1_canon_label.arc_isomorphism;
	ism := ism*g1_canon_label.arc_standard_map;
	if IsPerm(g2_canon_label.arc_standard_map) then
		ism := ism*Inverse(g2_canon_label.arc_standard_map);
	else
		ism := ism*InverseGeneralMapping(g2_canon_label.arc_standard_map);
	fi;

	if IsPerm(g2_canon_label.arc_isomorphism) then
		ism := ism*Inverse(g2_canon_label.arc_isomorphism);
	else
		ism := ism*InverseGeneralMapping(g2_canon_label.arc_isomorphism);
	fi;

	if IsGeneralMapping(ism) then
		if Source(ism) = Range(ism) then
			ism := MappingPermListList(List(Source(ism)), List(Source(ism), x -> x^ism));
		else
			dp_elms := List(Source(ism), x -> DirectProductElement([x, x^ism]));
			ism := GeneralMappingByElements(Source(ism), Range(ism), dp_elms);
		fi;
	fi;

	return ism;
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
	local PermToMap, digraph_rec, digraph_canon_vert, digraph_arcs, digraph_arcs_mapped, digraph_canon_arc, new_rev_map, adj_mat, v_labels_mapped, vert_perm, perm_mat, canon_adj_mat, canon_cert, arc_id_rec, arc_id, moved_rev_points, standard_rev_map, idx_x, idx_y, arc_string, moved_arcs, idx, prev_arcs, current_arcs, lex_perm, all_moved_arcs, arc_id_list, aut_g, current_max, current_v_perm, current_a_perm, g_arc, g_vert, permed_arcs, max;

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

	# Calculate the canonical certificate. 
	adj_mat := RSGraphAdjacencyMatrix(graph);

	if not IsPerm(digraph_canon_vert) then
		v_labels_mapped := List(RSGraphVertices(graph), x -> x^digraph_canon_vert);
		vert_perm := SortingPerm(v_labels_mapped);
	else
		vert_perm := digraph_canon_vert;
	fi;

	perm_mat := PermutationMat(vert_perm, Size(adj_mat), 1);
	canon_adj_mat := Inverse(perm_mat)*adj_mat*perm_mat;

	moved_rev_points := MovedPoints(new_rev_map);

	# Find the number of non-self-reverse loops at each vertex.
	all_moved_arcs := [];
	arc_id := 0;
	for idx_x in [1..Size(canon_adj_mat)] do
		for idx_y in [1..Size(canon_adj_mat)] do
			if idx_x = idx_y then
				arc_id_list := List([1..canon_adj_mat[idx_x][idx_y]], x -> arc_id + x);
				moved_arcs := Intersection(moved_rev_points, arc_id_list);
				Add(all_moved_arcs, Size(moved_arcs));
			fi;

			arc_id := arc_id + canon_adj_mat[idx_x][idx_y];
		od;
	od;

	current_max := ShallowCopy(all_moved_arcs);
	max := SortedList(current_max, {x, y} -> x > y);

	# If the current arrangement of the graph is not already in the 
	# "maximal loop position" then find a mapping that puts it in
	# this position. Otherwise don't do this to save time. 
	if current_max <> max then 
		aut_g := AutomorphismGroup(graph);
		aut_g := LAD_RSGraphNonReverseAutomorphisms@(graph);

		# Now find the automorphism that puts the "largest" vertices
		# (in terms of moved arcs) first. 
		current_v_perm := ();
		current_a_perm := ();

		for g_arc in aut_g do
			g_vert := RSGraphVertexAutomorphism(graph, g_arc);

			permed_arcs := Permuted(all_moved_arcs, Inverse(vert_perm)*g_vert*vert_perm);


			if permed_arcs > current_max then
				current_max := ShallowCopy(permed_arcs);
				current_v_perm := g_vert;
				current_a_perm := g_arc;
				if current_max = max then
					break;
				fi;
			fi;
		od;
		vert_perm := current_v_perm * vert_perm;

		digraph_canon_arc := current_a_perm * digraph_canon_arc;


		new_rev_map := Inverse(digraph_canon_arc)*digraph_rec.reverse_map*digraph_canon_arc;

		moved_rev_points := MovedPoints(new_rev_map);

		perm_mat := PermutationMat(vert_perm, Size(adj_mat), 1);
		canon_adj_mat := Inverse(perm_mat)*adj_mat*perm_mat;
	fi;



	# Calculate the "lexicographically standard" arc map. 
	canon_cert := String(canon_adj_mat);
	arc_id_rec := rec();
	arc_id := 0;
	standard_rev_map := ();
	for idx_x in [1..Size(canon_adj_mat)] do
		for idx_y in [1..Size(canon_adj_mat)] do
			# There are no arcs between the two vertices. 
			if canon_adj_mat[idx_x][idx_y] = 0 then
				if idx_x = idx_y then # Indicate that there's zero non self-reverse loops.
					canon_cert := Concatenation(canon_cert, "0;");
				fi;
				continue;
			fi;

			arc_string := StringFormatted("{1},{2}", idx_x, idx_y);
			# Get the arc ids for arcs from idx_x to idx_y. 
			arc_id_rec.(arc_string) := List([1..canon_adj_mat[idx_x][idx_y]], x -> arc_id + x);

			if idx_x = idx_y then
				# The arcs are loops.
				moved_arcs := Intersection(moved_rev_points, arc_id_rec.(arc_string));
				canon_cert := Concatenation(canon_cert, StringFormatted("{1};", Size(moved_arcs)));

				# For every second moved arc. 
				for idx in [1..Int(Size(moved_arcs)/2)]*2-1 do
					# Add a transposition between the arc and the next one. 
					standard_rev_map := standard_rev_map*(arc_id+idx, arc_id+idx+1);
				od;
			elif idx_x > idx_y then 
				# Not loops and have already seen arcs in the reverse direction. 
				prev_arcs := arc_id_rec.(Reversed(arc_string));
				current_arcs := arc_id_rec.(arc_string);
				for idx in [1..Size(current_arcs)] do
					standard_rev_map := standard_rev_map*(prev_arcs[idx], current_arcs[idx]);
				od;
			fi;

			arc_id := arc_id + canon_adj_mat[idx_x][idx_y];
		od;
	od;

	lex_perm := RepresentativeAction(SymmetricGroup(RSGraphNumberArcs(graph)), new_rev_map, standard_rev_map);

	if not IsPerm(digraph_canon_arc) then
		lex_perm := PermToMap(lex_perm, Domain([1..RSGraphNumberArcs(graph)]));
	fi;

	return rec(
		vertex_isomorphism := digraph_canon_vert,
		arc_isomorphism := digraph_canon_arc,
		reverse_map := new_rev_map,
		arc_standard_map := lex_perm,
		canon_certificate := canon_cert
	);
end);


InstallMethod(IsomorphismLocalActionDiagrams, "Isormorphism between two LocalActionDiagrams", [IsLocalActionDiagram, IsLocalActionDiagram],
function(lad1, lad2)
	local iso, lad_iso, graph1, graph2, vert_id, arc_id, bijections, out_arc_ids, labels_original, labels_mapped, labels_original_flat, labels_mapped_flat, perm, perms, labels_mapped_flat_perm, bijection, BijectionMap, ConjugateBijection, bad_iso, G1, G2, S, norm, all_conj, found_perm, labels_bijection_mapped, labels_bijection_flat, idx, count, new_count, mults, gens;

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
			## #G1 := LocalActionDiagramVertexLabels(lad1).(vert_id);
			## #G2 := LocalActionDiagramVertexLabels(lad2).(vert_id^iso[1]);

			## #if G1 = Group(()) and G2 = Group(()) then
			## #	bijection := ();
			## #else
			## #	S := SymmetricGroup(Maximum(Union(MovedPoints(G1), MovedPoints(G2))));

			## #	# Find a permutation that conjugates G1 into G2. 
			## #	bijection := RepresentativeAction(S, G1, G2);
			## #fi;

			## #if bijection = fail then
			## #	bijections := fail;
			## #	break;
			## #fi;


			## ## The bijection found does not take into account fixed points 
			## ## from PermGroupDomain (using RepresentativeAction with that 
			## ## causes a significant slowdown). This corrects the fixed point
			## ## mapping in the bijection by first "undoing" the movement of it
			## ## and then multiplying by the transposition which is the correct
			## ## movement of it. Since the orbits are disjoint this works. 
			## #for idx in [1..Size(labels_original)] do
			## #	if Size(labels_original[idx]) = 1 then
			## #		if labels_original[idx][1] <> labels_original[idx][1]^bijection then
			## #			bijection := bijection * (labels_original[idx][1], labels_original[idx][1]^bijection);
			## #		fi;

			## #		if labels_original[idx][1] <> labels_mapped[idx][1] then
			## #			bijection := bijection * (labels_original[idx][1], labels_mapped[idx][1]);
			## #		fi;
			## #	fi;
			## #od;

			## #labels_original_flat := Flat(labels_original);
			## #labels_mapped_flat := Flat(labels_mapped);
			## #labels_bijection_flat := List(labels_original_flat, x -> x^bijection);

			## ## If the bijection found does not map the orbits correctly then we multiply
			## ## by the transpositions that swap what the bijection maps with something
			## ## in the correct orbit. Because these swap orbit the product of these 
			## ## transpositions commute with the bijection so this is still a conjugation
			## ## of G1 into G2. 
			## ##
			## #if labels_bijection_flat <> labels_mapped_flat then
			## #	#for idx in [1..Size(labels_original_flat)] do
			## #	#	if labels_bijection_flat[idx] <> labels_mapped_flat[idx] then
			## #	#		bijection := bijection * (labels_bijection_flat[idx], labels_mapped_flat[idx]);
			## #	#	fi;
			## #	#od;
			## #	bijection := bijection * MappingPermListList(labels_bijection_flat, labels_mapped_flat);
			## #fi;

			## #if G1^bijection <> G2 then
			## #	bijections := fail;
			## #	break;
			## #fi;

			## #bijections.(vert_id) := BijectionMap(labels_original_flat, List(labels_original_flat, x -> x^bijection));

			# Get every permutation of labels_mapped that respects the partition. 
			mults := List(labels_mapped, x -> Size(x));
			count := 1;
			new_count := 1;

			# If there are *n* elments in the label then make the list [count..count+n-1].
			# The generators of the symmetric group on that list will give all permutations
			# of that list. 
			for idx in [1..Size(mults)] do
				new_count := count+mults[idx];
				mults[idx] := [count..count+mults[idx]-1];
				count := new_count;
			od;

			# Get the permutation group consisting of all permutations on the mapped labels. 
			gens := Flat(List(List(mults, x -> SymmetricGroup(x)), GeneratorsOfGroup));

			if Size(gens) = 0 then
				perms := Group(());
			else
				perms := Group(gens);
			fi;



			labels_original_flat := Flat(labels_original);
			labels_mapped_flat := Flat(labels_mapped);

			bijection := fail;
			for perm in perms do
				labels_mapped_flat_perm := Permuted(Flat(labels_mapped_flat), perm);


				bijection := BijectionMap(labels_original_flat, labels_mapped_flat_perm);
				
				#if iso[2] = (1,2) then Error("huH"); fi; 
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

InstallMethod(LAD_Internal_AllLocalActionDiagrams@, "enumerates local action diagrams up to isomorphism", [IsInt, IsInt],
function(degree, no_verts)
	local lad_list, CSubG, rev_maps, G, arc_labels, rev_map, lad, iso_lad, lad2, Order2Perm, full_lad_list, subg_orbits, idx, SubG, orb, rs_graphs, graph, rev, no_out_arcs, vert, all_labels, labels, vert_labels, base_arc_labels, all_arc_perms, arc_perms, arc_perm, temp_list, lab, all_arc_perms_gens, gens, v_id, arc_perms_orbits, arc_labels_orbits, OnSetsTuplesSets, lad_orbits, N, shift_bij, current_arc_labels, idx_y, new_arc_labels, orbit_shift_bij, gen_list, v_group, N_v, reduced_lad_list, iso_found, vert_labels_orbit_rec, arc_labels_flat, v_label, rearanged_vert_labels, positions, new_domain, aut_v_gen, gen, aut_v, OnList, all_labels_orbs, bad_gen, gen_group_bij, checked_verts;

	full_lad_list := [];

	# Get all subgroups of Sym(degree) up the conjugacy. 
	CSubG := List(ConjugacyClassesSubgroups(SymmetricGroup(degree)), Representative);

	# Put the groups in this record indexed by the number of orbits. 
	subg_orbits := rec();
	for idx in [1..degree] do
		subg_orbits.(idx) := [];
	od;

	for SubG in CSubG do
		orb := Orbits(SubG, [1..degree]);
		SetPermGroupDomain(SubG, [1..degree]);
		Add(subg_orbits.(Size(orb)), [SubG, orb]);
	od;

	# Suppress the information messages if this function
	# needs to call the enumeration algorithm. Call the 
	# AllRSGraphs function to use pre-computed data
	# this data is already known. 
	SetInfoLevel(InfoWarning, 0);
	rs_graphs := AllRSGraphs(degree, no_verts);
	SetInfoLevel(InfoWarning, 1);

	for graph in rs_graphs do
		rev := RSGraphReverseMap(graph);
		# Get the number of arcs originating at each vertex. 
		no_out_arcs := [];
		for vert in RSGraphVertices(graph) do
			Add(no_out_arcs, Size(RSGraphOutArcs(graph).(vert)));
		od;

		# Each element of all_labels is a valid labelling of the local 
		# action diagram. 
		# Each element is a list where the first element is the vertex
		# label and the second element is the arc labels at that vertex. 
		#
		# Need the if statement for when there is only one vertex. 
		if Size(no_out_arcs) <> 1 then
			all_labels := Cartesian(List(no_out_arcs, x -> subg_orbits.(x)));
		else
			all_labels := Cartesian([subg_orbits.(no_out_arcs[1])]);
		fi;

		# Reduce labels that are the same up to a graph isomorphism. Do
		# this by finding the orbits of *all_labels* under permutation
		# by the vertex automorphism group.  
		aut_v_gen := [];

		for gen in GeneratorsOfGroup(AutomorphismGroup(graph)) do
			Add(aut_v_gen, RSGraphVertexAutomorphism(graph, gen));
		od;

		if Size(aut_v_gen) = 0 then
			aut_v := Group(());
		else
			aut_v := Group(aut_v_gen);
		fi;

		OnList := function(l, g) return Permuted(l, g); end;

		all_labels_orbs := OrbitsDomain(aut_v, all_labels, OnList);

		all_labels := List(all_labels_orbs, x -> x[1]);

		lad_list := [];


		for labels in all_labels do

			vert_labels := List(labels, x -> ShallowCopy(x[1]));
			base_arc_labels := List(labels, x -> x[2]);

			vert_labels_orbit_rec := rec();

			# Shift the elements the groups act on so that they
			# are disjoint. This makes the normaliser stuff
			# later in the code far simpler. 
			for idx in [1..Size(base_arc_labels)] do
				new_domain := [(idx-1)*degree+1..(idx)*degree];
				shift_bij := MappingPermListList([1..degree], new_domain);
				# Get the arc labels for this vertex. 
				current_arc_labels := List(base_arc_labels[idx], ShallowCopy); 
				new_arc_labels := List(base_arc_labels[idx], ShallowCopy); 
				for idx_y in [1..Size(new_arc_labels)] do
					new_arc_labels[idx_y] := List(new_arc_labels[idx_y], x -> x^shift_bij);
				od;
				
				orbit_shift_bij := MappingPermListList(Flat(current_arc_labels), Flat(new_arc_labels));

				base_arc_labels[idx] := new_arc_labels;
				vert_labels[idx] := vert_labels[idx]^orbit_shift_bij;
				SetPermGroupDomain(vert_labels[idx], [(idx-1)*degree+1..idx*degree]);

				vert_labels_orbit_rec.(String(new_domain[1])) := vert_labels[idx];
			od;


			temp_list := [];
			for lab in base_arc_labels do
				temp_list := Concatenation(temp_list, lab);
			od;
			base_arc_labels := temp_list;

			# Reduce every permutation of the arc labels to just one from
			# each of these orbits. The orbits correspond to local action
			# diagram isomorphisms that are just an isomorphism of the
			# RSGraph. 
			#
			# It's not the whole symmetric group. Make it one for each
			# of the arcs ("mults" thing). 
			all_arc_perms_gens := [];
			
			for v_id in RSGraphVertices(graph) do
				gens := GeneratorsOfGroup(SymmetricGroup(RSGraphOutArcs(graph).(v_id)));
				all_arc_perms_gens := Union(all_arc_perms_gens, gens);
			od;

			# This takes into account the arc mappings from automorphisms that move the vertices.  
			all_arc_perms_gens := Union(all_arc_perms_gens, GeneratorsOfGroup(AutomorphismGroup(graph)));

			if Size(all_arc_perms_gens) = 0 then
				all_arc_perms := Group(());
			else
				all_arc_perms := Group(all_arc_perms_gens);
			fi;

			#arc_perms := Centraliser(all_arc_perms, rev);
			arc_perms := AutomorphismGroup(graph);
			arc_perms_orbits := OrbitsDomain(arc_perms, all_arc_perms, OnRight);

			# Get the arc labellings associated with the permutations. Keep them
			# separated by orbits. 
			arc_labels_orbits := List(arc_perms_orbits, x -> Set(List(x, perm -> List(Permuted(base_arc_labels, perm), Set))));
			
			OnSetsTuplesSets := function(e, g) return Set(e, function(i) return OnTuplesSets(i, g); end); end;

			gen_list := [];
			for v_group in vert_labels do 
				N_v := Normaliser(SymmetricGroup(PermGroupDomain(v_group)), v_group);
				gen_list := Concatenation(gen_list, GeneratorsOfGroup(N_v));
			od;

			# Now need to add to the normaliser (all possible bijections) the 
			# bijections between vertices that can map to each other. 


			#v_orbits := OrbitsDomain(aut_v, [1..Size(vert_labels)], OnPoints);

			#for v_orbit in v_orbits do
			#	base_v := v_orbit[1];

			#	for v in v_orbit{[2..Size(v_orbit)]} do
			#		Add(gen_list, MappingPermListList(PermGroupDomain(vert_labels[base_v]), PermGroupDomain(vert_labels[v])));
			#	od;
			#od;


			####################################################
			# Find bug in this or isomorphism code. 
			# With this enabled there is one less in (3, 4). 
			# Find out why!!!
			# ##################################################

			for gen in GeneratorsOfGroup(aut_v) do
				bad_gen := false;
				gen_group_bij := ();

				checked_verts := [];

				for vert in [1..Size(vert_labels)] do
					# Need to make sure the groups conjugate? 
					if not vert in checked_verts then
						gen_group_bij := gen_group_bij * MappingPermListList(PermGroupDomain(vert_labels[vert]), PermGroupDomain(vert_labels[vert^gen]));
						Add(checked_verts, vert);
						Add(checked_verts, vert^gen);
					fi;

					if vert_labels[vert]^gen_group_bij <> vert_labels[vert^gen] then
						bad_gen := true;
						break;
					fi;
				od;

				if bad_gen = false then
					Add(gen_list, gen_group_bij);
				else
					bad_gen := false;
				fi;
			od;

			if Size(gen_list) = 0 then
				N := Group(());
			else
				N := Group(gen_list);
			fi;


			lad_orbits := OrbitsDomain(N, arc_labels_orbits, OnSetsTuplesSets);

			for arc_labels in lad_orbits do


				arc_labels_flat := Flat(arc_labels[1][1]);

				rearanged_vert_labels := [];

				for idx in [1..Size(vert_labels)] do
					positions := [(idx-1)*degree+1..(idx)*degree];
					v_label := vert_labels_orbit_rec.(String(Minimum(arc_labels_flat{positions})));
					Add(rearanged_vert_labels, v_label);
				od;

				lad := LocalActionDiagramFromData(graph, rearanged_vert_labels, arc_labels[1][1]);
				Add(lad_list, lad);

			od;



			#arc_perms := OrbitsDomain(arc_perms, all_arc_perms, OnRight);
			#arc_perms := List(arc_perms, x -> x[1]);

			#iso_lad := false;
			#for arc_perm in arc_perms do
			#	arc_labels := Permuted(base_arc_labels, arc_perm);
			#	lad := LocalActionDiagramFromData(graph, vert_labels, arc_labels);
			#	for lad2 in lad_list do
			#		if IsomorphismLocalActionDiagrams(lad, lad2) <> fail then
			#			iso_lad := true;
			#			break;
			#		fi;
			#	od;

			#	if iso_lad = false then
			#		Add(lad_list, lad);
			#	else
			#		iso_lad := false;
			#	fi;
			#od;
			
		od;

		full_lad_list := Concatenation(full_lad_list, lad_list);
	od;


	return full_lad_list;

	#for G in CSubG do
	#	SetPermGroupDomain(G, [1..degree]);
	#	arc_labels := List(Orbits(G, PermGroupDomain(G)), Set);
	#	rev_maps := Order2Perm([1..Size(arc_labels)]);
	#	
	#	for rev_map in rev_maps do
	#		graph := RSGraphByAdjacencyList(List(arc_labels, x -> [1, 1]), rev_map);

	#		# Permuting of arc labels is taken care of by going through each possible
	#		# reverse map. 
	#		lad := LocalActionDiagramFromData(graph, [G], arc_labels);

	#		iso_lad := false;
	#		for lad2 in lad_list do
	#			if IsomorphismLocalActionDiagrams(lad2, lad) <> fail then
	#				iso_lad := true;
	#				break;
	#			fi;
	#		od;

	#		if not iso_lad then
	#			Add(lad_list, lad);
	#		fi;
	#	od;
	#od;

	return lad_list;
end);

InstallMethod(LAD_Internal_AllRSGraphs@, "Enumerate RS Graphs with valency <= d and n vertices", [IsInt, IsInt],
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
			if IsomorphismRSGraphs(graph, graph2) <> fail then
				iso_graphs := true;
				break;
			fi;

		od;
		if iso_graphs = false then 
			Add(list_of_rsgraphs_isomorphism, graph);
		else 
			iso_graphs := false;
		fi;
	od;

	return list_of_rsgraphs_isomorphism;
end);



