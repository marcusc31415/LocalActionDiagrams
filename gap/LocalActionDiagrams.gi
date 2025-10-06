DrawLADAddToTree := function(LAD, depth, tree, lad_vertex, prev_arc_no, current_depth)
	local tree_vertex_no, rev_arc_no, rev_arc_label, arcs_to_check, lad_next_vert, arc_label, arc_no, arc_labels;
	Add(tree, []);
	if current_depth = depth then
		return;
	fi;
	tree_vertex_no := Size(tree); # The index of this vertex in the tree list. 
	rev_arc_no := prev_arc_no^LocalActionDiagramEdgeReversal(LAD);
	rev_arc_label := LocalActionDiagramEdgeLabels(LAD)[rev_arc_no][1]; # Pick the first label on the reverse arc. 
	arcs_to_check := Positions(DigraphSource(LAD), lad_vertex);

	for arc_no in arcs_to_check do
		arc_labels := LocalActionDiagramEdgeLabels(LAD)[arc_no];
		for arc_label in arc_labels do
			if not (arc_no = rev_arc_no and arc_label = rev_arc_label) then
				Add(tree[tree_vertex_no], Size(tree)+1);
				lad_next_vert := LocalActionDiagramEdges(LAD)[arc_no][2];
				DrawLADAddToTree(LAD, depth, tree, lad_next_vert, arc_no, current_depth+1);
			fi;
		od;
	od;
end;

# *__start_vertex* is an optional argument. 
# If nothing is put there then it will default to 1.
# If any other argument after are provided they will be silently ignored. 
DrawLAD := function(LAD, depth, __start_vertex...)
	local tree, arcs_to_check, arc_no, arc_labels, lad_next_vert, current_depth, arc_label, start_vertex;
	
	if Size(__start_vertex) = 0 then
		start_vertex := 1;
	else
		start_vertex := __start_vertex[1];
	fi;

	if not 1 <= start_vertex and start_vertex <= DigraphNrVertices(LAD) then
		Error("Start vertex must be a vertex in the local action diagram.");
	fi;

	if depth < 1 then
		Error("Depth must be at least 1.");
	fi;


	tree := [[]]; # Create adjacency list and make diagraph at the end of function.
	
	arcs_to_check := Positions(DigraphSource(LAD), start_vertex); # All the edges going out of *start_vertex*. 

	current_depth := 0;

	for arc_no in arcs_to_check do
		arc_labels := LocalActionDiagramEdgeLabels(LAD)[arc_no];
		for arc_label in arc_labels do
			Add(tree[1], Size(tree)+1); # Connect the first vertex to the new vertex to be added. 
			lad_next_vert := LocalActionDiagramEdges(LAD)[arc_no][2];
			DrawLADAddToTree(LAD, depth, tree, lad_next_vert, arc_no, current_depth+1);
		od;
	od;

	return Digraph(tree); 
end;


# Set the domain of a permutation group. 
InstallMethod(PermGroupDomain, [IsPermGroup], MovedPoints);

# Create LAD from data (digraph, vertex labels, edge labels, and reversal mapping.) 
InstallMethod(LocalActionDiagramFromData, "test label 123", [IsDigraph, IsList, IsList, IsPerm],
function(D, vl, el, rev)
	
	local lad, vertex, v_origin, v_orbits, label_subset, i, edges, edge_origin, edge_end, edges_used;

	if not IsImmutableDigraph(D) then
		Error("Must use an immutable Digraph for construction.");
	fi;
	
	if DigraphNrVertices(D) <> Length(vl) then
		Error("Must have label for each vertex in digraph. Number of digraph vertices=",DigraphNrVertices(D));
	fi;

	if DigraphNrEdges(D) <> Length(el) then
		Error("Must have label for each vertex in digraph. Number of digraph edges=",DigraphNrEdges(D));
	fi;
	
	### Check that the reversal permutations are correct. ###

	# Check if the permutation is an involution. 
	if not rev*rev = One(rev) then
		Error("Reversal mapping must be an involution.");
	fi;

	# Check reversal properly?
	# Make a copy of the edges and sort them lexicographically. 
	edges := ShallowCopy(DigraphEdges(D));
	Sort(edges);
	for i in [1..Length(el)] do
		edge_origin := edges[i][1];
		edge_end := edges[i][2];
		if edges[i^rev][1] <> edge_end and edges[i^rev][2] <> edge_origin then
			Error("Reversal mapping must send edges back to their origin vertex.");
		fi;
	od;

	### Check if there are the same number of "in" and "out" edges. ###
	### Not needed due to above reversal check? 
	#if AdjacencyMatrix(D) <> TransposedMat(AdjacencyMatrix(D)) then
	#	Error("Reversal mapping must have an \"in\" edges for every \"out\" edge.");
	#fi;

	### Check the edge labelling is valid. ###

	v_origin := List(edges, x -> x[1]);
	v_orbits := List(vl, G -> Orbits(G, PermGroupDomain(G)));

	# Check if the "out" edge labels are all the orbits of the vertex label. 
	for vertex in [1..DigraphNrVertices(D)] do
		label_subset := el{Positions(v_origin, vertex)};
		if Set(List(label_subset, x -> Set(x))) <> Set(List(v_orbits[vertex], x -> Set(x))) then
			Error("Group at vertex ",vertex," (",vl[vertex],") doesn't have the correct orbits on outgoing edges.");
		fi;
	od;

	### All checks have passed. Now make the object. ###

	#lad := DigraphImmutableCopy(D);
	#SetFilterObj(lad, IsLocalActionDiagram);
	#Setter(LocalActionDiagramVertexLabels)(lad, vl);
	#Setter(LocalActionDiagramEdgeLabels)(lad, el);
	#Setter(LocalActionDiagramEdgeReversal)(lad, rev);

	#SetName(lad, StringFormatted("<immutable Local Action Diagram with {1} vertices and {2} edges>", Length(vl), Length(el)));

	lad := LocalActionDiagramFromDataNC(D, vl, el, rev);

	return lad;

end );

# Make a local action diagram "object" without checking. 
# Thanks GAP for not providing Try-Catch functionality...
InstallMethod(LocalActionDiagramFromDataNC, "No Check test label 123", [IsDigraph, IsList, IsList, IsPerm],
function(D, vl, el, rev)
	local lad, edges;

	lad := DigraphImmutableCopy(D);
	# Make a copy of the edges and sort them lexicographically. 
	edges := ShallowCopy(DigraphEdges(D));
	Sort(edges);

	SetFilterObj(lad, IsLocalActionDiagram);
	Setter(LocalActionDiagramVertices)(lad, DigraphVertices(lad));
	Setter(LocalActionDiagramVertexLabels)(lad, vl);
	Setter(LocalActionDiagramEdges)(lad, edges);
	Setter(LocalActionDiagramEdgeLabels)(lad, el);
	Setter(LocalActionDiagramEdgeReversal)(lad, rev);

	SetName(lad, StringFormatted("<immutable Local Action Diagram with {1} vertices and {2} edges>", Length(vl), Length(el)));

	return lad;
end );

# Make a local action diagram corresponding to the Universal group G. 
InstallMethod(LocalActionDiagramUniversalGroup, [IsPermGroup],
function(G)
	local orbits, digraph, lad;

	orbits := Orbits(G, PermGroupDomain(G));
	digraph := DigraphByAdjacencyMatrix(IsImmutableDigraph, [[Length(orbits)]]);
	lad := LocalActionDiagramFromDataNC(digraph, [G], orbits, ());
	return lad;
end );

# Check if *lad1* and *lad2* are isomorphic. 
InstallMethod(IsomorphismLocalActionDiagrams, [IsLocalActionDiagram, IsLocalActionDiagram],
function(lad1, lad2)
	local every_digraph_iso, iso, bijection_list, G1, G2, i, lad1_v, lad2_v, lad1_e, lad2_e, lad1_el, lad2_el, orbits1, orbits2, j, edges_origin_v1, edges_origin_v2, bijection, found_iso, non_multi_edge_pos;

	# Need to check through every digraph isomorphism because we need to check
	# through every valid "potential conjugate" vertex labels. 
	every_digraph_iso := FindEveryDigraphIsomorphism@(lad1, lad2);
	if every_digraph_iso = fail then
		return fail;
	fi;

	lad1_v := LocalActionDiagramVertexLabels(lad1);
	lad2_v := LocalActionDiagramVertexLabels(lad2);
	lad1_e := LocalActionDiagramEdges(lad1);
	lad2_e := LocalActionDiagramEdges(lad2);
	lad1_el := LocalActionDiagramEdgeLabels(lad1);
	lad2_el := LocalActionDiagramEdgeLabels(lad2);

	for iso in every_digraph_iso do
		bijection_list := [];
		# Check each vertex label for a bijection. 
		found_iso := true;
		for i in [1..Length(lad1_v)] do
			G1 := lad1_v[i];

			# Need to check multidigraph and non-multidigraph cases separately
			# as the isomorphism format returned is different. 
			###if IsMultiDigraph(lad1) then
				G2 := lad2_v[i^iso[1]]; # Vertex the isomorphism maps this one to. 
			###else
			###	G2 := lad2_v[i^iso];
			###fi;

			orbits1 := [];
			orbits2 := [];

			# Find all edges with origin at vertex *i*. 
			edges_origin_v1 := Positions(List(lad1_e, x -> x[1]), i);

			# Find their orbits from the edge labels. 
			orbits1 := lad1_el{edges_origin_v1};
			
			###if IsMultiDigraph(lad1) then
			###	# Get the edges with origin where the isomorphism maps vertex i to. 
			###	edges_origin_v2 := Positions(List(lad2_e, x -> x[1]), i^iso[1]);

			###	# Find where the orbits map to be following the edge isomorphism. 
			###	for j in edges_origin_v2 do
			###		Add(orbits2, lad2_el[j^iso[2]]);
			###	od;
			###else
			###	for j in edges_origin_v1 do
			###		# Find where the edge maps to under the isomorphism. 
			###		non_multi_edge_pos := Position(lad2_e, [lad1_e[j][1]^iso, lad1_e[j][2]^iso]);
			###		# Get that edge label. 
			###		Add(orbits2, lad2_el[non_multi_edge_pos]);
			###	od;
			###fi;

			edges_origin_v2 := List(edges_origin_v1, x -> x^iso[2]);
			orbits2 := lad2_el{edges_origin_v2};
			
			# Find if there's a valid bijection between the two that make the conjugates. 
			bijection := FindConjugateBijectionBetweenGroups@(G1, G2, orbits1, orbits2);
			

			if bijection = fail then
				# No conjugate bijection exists for these vertices so break the loop
				# for this isomorphism and try the next one. .  
				found_iso := false;
				break;
			else
				# Valid bijection found. 
				Add(bijection_list, bijection);
			fi;
		od;
		if found_iso then
			return [iso, bijection_list];
		fi;
	od;
	return fail;
end);

# Group type "variable". It first runs the computation and then will return the computation.
InstallMethod(LocalActionDiagramGroupType, [IsLocalActionDiagram], GroupType@);

# Is Discrete "variable". It first runs the computation and then will return the computation.
InstallMethod(LocalActionDiagramIsDiscrete, [IsLocalActionDiagram], IsDiscrete@);

# Scopo "variable". It first runs the computation and then will return the computation.
InstallMethod(LocalActionDiagramScopos, [IsLocalActionDiagram], FindAllScopos@);

# Find all local action diagrams with vertex labels that are subgroups of Sym(d)
# and *no_vertex_orbits* vertices. 
# These correspond to (P)-closed subgroups of Aut(T_d) with *no_vertex_orbits*
# vertex orbits. 
InstallMethod(AllLocalActionDiagrams, [IsPosInt, IsPosInt],
function(d, no_vertex_orbits)
	local lads, G, SubG, Gv, arcs, i, NGv, S, actNGv, R, cr, r, D, c_group_pairs, group_pair, no_orbits, arcs_between, arc_label_order_one, arc_label_order_two, arc_labels, edges, temp_lad, lad, already_contained, bad_rev, order_two_permutations, arc_label_len, loop_exit, lone_orbits, H;
	if no_vertex_orbits <> 1 and no_vertex_orbits <> 2 then
		Error("Curently only supports one or two vertex orbits (i.e. vertex-transitive graphs.)");
	fi;

	if no_vertex_orbits = 1 then
		# List with all local action diagrams found (will return). 
		lads := [];

		G := SymmetricGroup(d);
		SubG := List(ConjugacyClassesSubgroups(G), Representative);

		for Gv in SubG do
			arcs := List(Orbits(Gv, [1..d]), Set);

			NGv := Normalizer(G, Gv);

			S := SymmetricGroup(Size(arcs));
			actNGv := ActionHomomorphism(NGv, arcs, OnSets);
			R := Image(actNGv);

			for cr in OrbitsDomain(R, S) do
				r := Representative(cr);
				if r*r = () then
					D := DigraphByAdjacencyMatrix(IsImmutableDigraph, [[Length(arcs)]]);
					Add(lads, LocalActionDiagramFromDataNC(D, [Gv], arcs, r));
				fi;
			od;
		od;
	elif no_vertex_orbits = 2 then
		lads := [];

		# Find all vertex labels. We use UnorderedTuples because in the two vertex
		# case swapping vertex labels produces an isomorphic local action diagram. 
		G := SymmetricGroup(d);
		SubG := List(ConjugacyClassesSubgroups(G), Representative);
		for H in SubG do
			SetPermGroupDomain(H, [1..d]);
		od;
		c_group_pairs := UnorderedTuples(SubG, 2);

		# Will store calculated order two permutations to speed up the function. 
		order_two_permutations := [];
		
		for group_pair in c_group_pairs do
			# Find the number of arcs from the orbits of the vertex labels. 
			arcs := List(group_pair, x -> ShallowCopy(Orbits(x, PermGroupDomain(x))));
			no_orbits := List(arcs, Length);

			# There must be at least one arc between the two vertices. There can
			# be at most the minimum number of arcs originating at the two vertices. 
			for arcs_between in [1..Minimum(no_orbits)] do
				D := DigraphByAdjacencyMatrix([[no_orbits[1]-arcs_between, arcs_between], [arcs_between, no_orbits[2]-arcs_between]]);
				edges := LocalActionDiagramEdges(D);

				# For all arrangements of the arc labels on the edges. 
				for arc_label_order_one in Arrangements(arcs[1], Length(arcs[1])) do
					for arc_label_order_two in Arrangements(arcs[2], Length(arcs[2])) do
						arc_labels := Concatenation(arc_label_order_one, arc_label_order_two);
						arc_label_len := Length(arc_labels);

						# Calculate the order two permutations on [1..n] where n is
						# the number of arcs labels if it's not been calculated
						# already. 
						if not IsBound(order_two_permutations[arc_label_len]) then
							order_two_permutations[arc_label_len] := Order2Elements@([1..arc_label_len]);
						fi;
						
						loop_exit := false;
						lone_orbits := false;
						#d_test := DigraphByAdjacencyMatrix([[2, 1], [1, 2]]);
						for r in order_two_permutations[arc_label_len] do
							bad_rev := false;
							# Check that it's a valid reversal mapping. 
							for i in [1..Length(arc_labels)] do
								if edges[i^r][1] <> edges[i][2] or edges[i^r][2] <> edges[i][1] then
									bad_rev := true;
									break;
								fi;
							od;
							if bad_rev then
								continue;
							fi;
							# Already done the checking so use the "no check" version. 
							temp_lad := LocalActionDiagramFromDataNC(D, group_pair, arc_labels, r);
							already_contained := false;
							# Check it's not isomorphic to any of the other local
							# action diagrams found. 
							for lad in lads do
								if IsomorphismLocalActionDiagrams(temp_lad, lad) <> fail then
									already_contained := true;
									break;
								fi;
							od;
							if not already_contained then
								Add(lads, temp_lad);
							fi;

							# If it doesn't have loops then there will be a digraph
							# isomorphism that swaps the edges around so we don't
							# need to check all the arc label arrangements. 
							#if not DigraphHasLoops(temp_lad) then
							#	break;
							#fi;

							# If the labels are all of size one then the vertex
							# labels are the trivial groups and we can construct
							# a bijection that will swap any arrangement of the 
							# arc labels.  
							#if Set(List(arc_labels, Size)) = [1] then
							#	break;
							#fi;
						od;
					od;
				od;
			od;
		od;
	fi;
	return lads;
end );


InstallMethod(CotreeFromScopo, "test label 123", [IsLocalActionDiagram, IsList],
function(lad, scopo)
	if not scopo in LocalActionDiagramScopos(lad) then
		Error("Input list is not a scopo of the local action diagram.");
	fi;
	return CotreeFromScopo@(lad, scopo);
end);

InstallMethod(CotreeFromScopoNC, [IsLocalActionDiagram, IsList], CotreeFromScopo@);

InstallMethod(RandomLocalActionDiagram, "test label 123", [IsPosInt, IsPosInt],
function(no_vert, edge_label_max)
	local graph, edge_labels, vert_labels, rev, edge, edges_to_add, sorted_edges, vert, out_edges, last, el_temp, el, pos, pos_2, vl, rand_groups, grp, gens, gen_list, new_gen_list, n, g_list, p, i, used_p;

	edge_labels := [];
	vert_labels := [];
	edges_to_add := [];
	# Make a random connected, symmetric digraph.
	graph := DigraphSymmetricClosure(RandomDigraph(IsConnectedDigraph, no_vert)); 


	# Make the digraph symmetric. 
	#for edge in DigraphEdges(graph) do
	#	if edge[1] <> edge[2] and not ([edge[2], edge[1]] in DigraphEdges(graph)) then
	#		Add(edges_to_add, [edge[2], edge[1]]);
	#	fi;
	#od;
	#graph := DigraphAddEdges(graph, edges_to_add);

	# Create the edge and vertex labels. 
	sorted_edges := ShallowCopy(DigraphEdges(graph));
	Sort(sorted_edges);

	for vert in [1..no_vert] do
		# Find all the edges that start at vertex *vert*. 
		out_edges := sorted_edges{Positions(List(sorted_edges, x -> x[1]), vert)};
		last := 0;
		el_temp := [];
		# Creates random length edge labels and ensures they do not overlap. 
		for edge in out_edges do
			el := [1..Random(1, edge_label_max)] + last;
			Add(el_temp, el);
			last := Maximum(el);
		od;
		Append(edge_labels, el_temp);
		# The vertex label is is the group generated by all generators of the
		# symmetric groups on each element *el* of *el_temp*. They don't overlap
		# and so the orbits are preserved. 
		#gen_list := Flat(List(el_temp, x -> GeneratorsOfGroup(SymmetricGroup(x))));
		gen_list := [];


		for el in el_temp do
			if Size(el) <> 1 then
				grp := Random(AllTransitiveGroups(NrMovedPoints, Size(el)));
				gens := GeneratorsOfGroup(grp);
				gens := List(gens, x -> PermList(Concatenation([1..Minimum(el)-1],ListPerm(x)+Minimum(el)-1)));
				Add(gen_list, gens);
			fi;
		od;

		new_gen_list := [];


		while Length(gen_list) > 0 do
			g_list := [Random(gen_list[1])];
			n := Random(1, Length(gen_list));
			used_p := [1];
			for i in [2..n] do
				p := Random(2, n);
				if not (p in used_p) then
					Add(g_list, Random(gen_list[p]));
				fi;
				Add(used_p, p);
			od;
			Add(new_gen_list, Product(g_list));
			gen_list := List(gen_list, x -> Difference(x, g_list));
			gen_list := Difference(gen_list, [[]]);
		od;
		
		gen_list := Flat(new_gen_list);

		if Length(gen_list) = 0 then
			gen_list := [()];
		fi;
		vl := Group(gen_list);
		SetPermGroupDomain(vl, Flat(el_temp));
		Add(vert_labels, vl);
	od;

	rev := [];
	for pos in [1..Length(sorted_edges)] do
		edge := sorted_edges[pos];
		# Find first occurrence of a reverse edge not already in the list *rev*
		# i.e. not already assigned a reverse. 
		pos_2 := Position(sorted_edges, [edge[2], edge[1]]);
		while pos_2 in rev do
			pos_2 := Position(sorted_edges, [edge[2], edge[1]], pos_2);
		od;
		Add(rev, pos_2);
	od;
	rev := PermList(rev);

	return LocalActionDiagramFromData(graph, vert_labels, edge_labels, rev);
end);



x := LocalActionDiagramUniversalGroup(Group((1,2),(3,4)));

D := DigraphByAdjacencyMatrix([[0, 1, 1, 1], [1, 0, 0, 0], [1, 0, 0, 1], [1, 0, 1, 0]]);
g := Group(());
g2 := Group(());
g3 := Group(());
SetPermGroupDomain(g, [1, 2]);
SetPermGroupDomain(g2, [1, 2, 3]);
SetPermGroupDomain(g3, [1]);
y := LocalActionDiagramFromData(D, [g2, g3, g, g], [[1], [2], [3], [1], [1], [2], [1], [2]], (1,4)(2,5)(3,7)(6,8));
D := DigraphByAdjacencyMatrix([[1, 1, 1, 0], [1, 0, 1, 0], [1, 1, 0, 1], [0, 0, 1, 0]]);
g1 := Group((1,2));
g2 := Group((1,2));
g := Group(());
g3 := Group(());
g4 := Group(());
SetPermGroupDomain(g1, [1, 2, 3, 4]);
SetPermGroupDomain(g2, [1, 2, 3]);
SetPermGroupDomain(g3, [1, 2, 3]);
SetPermGroupDomain(g4, [1]);
e_l := [[3], [1,2], [4], [1, 2], [3], [1], [2], [3], [1]];
v_l := [g1, g2, g3, g4];
rev := (2, 4)(3, 6)(5, 7)(8, 9);
test := LocalActionDiagramFromData(D, v_l, e_l, rev);
