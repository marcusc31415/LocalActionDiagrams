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
	edges := DigraphEdges(D);
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

	v_origin := List(DigraphEdges(D), x -> x[1]);
	v_orbits := List(vl, G -> Orbits(G, PermGroupDomain(G)));

	# Check if the "out" edge labels are all the orbits of the vertex label. 
	for vertex in [1..DigraphNrVertices(D)] do
		label_subset := el{Positions(v_origin, vertex)};
		if Set(label_subset) <> Set(v_orbits[vertex]) then
			Error("Group at vertex ",vertex," (",vl[vertex],") doesn't have the correct orbits on outgoing edges.");
		fi;
	od;

	### All checks have passed. Now make the object. ###

	lad := DigraphImmutableCopy(D);
	SetFilterObj(lad, IsLocalActionDiagram);
	Setter(LocalActionDiagramVertexLabels)(lad, vl);
	Setter(LocalActionDiagramEdgeLabels)(lad, el);
	Setter(LocalActionDiagramEdgeReversal)(lad, rev);

	SetName(lad, StringFormatted("<immutable Local Action Diagram with {1} vertices and {2} edges>", Length(vl), Length(el)));

	return lad;
end );

# Make a local action diagram "object" without checking. 
# Thanks GAP for not providing Try-Catch functionality...
InstallMethod(LocalActionDiagramFromDataNC, "No Check test label 123", [IsDigraph, IsList, IsList, IsPerm],
function(D, vl, el, rev)
	local lad;

	lad := StructuralCopy(D);
	SetFilterObj(lad, IsLocalActionDiagram);
	Setter(LocalActionDiagramVertexLabels)(lad, vl);
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
	lad1_e := DigraphEdges(lad1);
	lad2_e := DigraphEdges(lad2);
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
			if IsMultiDigraph(lad1) then
				G2 := lad2_v[i^iso[1]]; # Vertex the isomorphism maps this one to. 
			else
				G2 := lad2_v[i^iso];
			fi;

			orbits1 := [];
			orbits2 := [];

			# Find all edges with origin at vertex *i*. 
			edges_origin_v1 := Positions(List(lad1_e, x -> x[1]), i);

			# Find their orbits from the edge labels. 
			orbits1 := lad1_el{edges_origin_v1};
			
			if IsMultiDigraph(lad1) then
				# Get the edges with origin where the isomorphism maps vertex i to. 
				edges_origin_v2 := Positions(List(lad2_e, x -> x[1]), i^iso[1]);

				# Find where the orbits map to be following the edge isomorphism. 
				for j in edges_origin_v2 do
					Add(orbits2, lad2_el[j^iso[2]]);
				od;
			else
				for j in edges_origin_v1 do
					# Find where the edge maps to under the isomorphism. 
					non_multi_edge_pos := Position(lad2_e, [lad1_e[j][1]^iso, lad1_e[j][2]^iso]);
					# Get that edge label. 
					Add(orbits2, lad2_el[non_multi_edge_pos]);
				od;
			fi;
			
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
				edges := DigraphEdges(D);

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
							if not DigraphHasLoops(temp_lad) then
								break;
							fi;

							# If the labels are all of size one then the vertex
							# labels are the trivial groups and we can construct
							# a bijection that will swap any arrangement of the 
							# arc labels.  
							if Set(List(arc_labels, Size)) = [1] then
								break;
							fi;
						od;
					od;
				od;
			od;
		od;
	fi;
	return lads;
end );
