_LAD_WriteSingleLADStringFormat@ := function(lad)
	local output_string, vl, el, attr, attr_list, attr_output_list;

	output_string := "";

	# First write the digraph using the digraph package IO. 
	output_string := Concatenation(output_string, PrintString(DigraphImmutableCopy(lad))); 
	output_string := Concatenation(output_string, "\n");
	
	# Store the vertex labels.
	for vl in LocalActionDiagramVertexLabels(lad) do
		output_string := Concatenation(output_string, String(vl));
		output_string := Concatenation(output_string, "|");
	od;
	Remove(output_string); # Remove the trailing separator. 
	output_string := Concatenation(output_string, "\n");

	# Store the edge labels.
	for el in LocalActionDiagramEdgeLabels(lad) do
		output_string := Concatenation(output_string, String(el));
		output_string := Concatenation(output_string, "|");
	od;
	Remove(output_string); # Remove the trailing separator. 
	output_string := Concatenation(output_string, "\n");

	# Store the edge reversal.
	output_string := Concatenation(output_string, String(LocalActionDiagramEdgeReversal(lad)));
	output_string := Concatenation(output_string, "\n");
	
	# Store all known attributes of the object. 
	# Does not compute any unknown attributes. 
	attr_list := ["LocalActionDiagramScopos", "LocalActionDiagramGroupType", "LocalActionDiagramIsDiscrete"];
	attr_output_list := [];

	for attr in attr_list do
		if attr in KnownAttributesOfObject(x) then
			Add(attr_output_list, EvalString(attr)(x));
		fi;
	od;

	output_string := Concatenation(output_string, String(attr_output_list));
	output_string := Concatenation(output_string, "\n");

	return output_string;
end;

_LAD_ReadLADString@ := function(lad_string)
	local input_lines, lad_list, idx_x, line, digraph, v_labels, v_label, e_labels, e_label, rev, lad, attr_list, idx_y, lad_attrs, input_line;

	input_lines := SplitString(lad_string, "\n");

	lad_list := [];
	idx_x := 1; 

	attr_list := ["LocalActionDiagramScopos", "LocalActionDiagramGroupType", "LocalActionDiagramIsDiscrete"];

	input_line := 0;
	for line in input_lines do
		input_line := input_line+1;
		if idx_x = 1 then
			# Read the digraph. 
			digraph := EvalString(line);
			if IsDigraph(digraph) = false then
				return input_line;
			fi;
			idx_x := idx_x+1;
		elif idx_x = 2 then
			# Read the vertex labels. 
			v_labels := [];
			for v_label in SplitString(line, "|") do
				Add(v_labels, EvalString(v_label));
			od;
			if ForAll(v_labels, IsGroup) = false then
				return input_line;
			fi;
			idx_x := idx_x+1;
		elif idx_x = 3 then
			# Read the edge labels. 
			e_labels := [];
			for e_label in SplitString(line, "|") do
				Add(e_labels, EvalString(e_label));
			od;
			if ForAll(e_labels, IsList) = false then
				return input_line;
			fi;
			idx_x := idx_x+1;
		elif idx_x = 4 then
			# Read the edge reversal and create the local action diagram. 
			rev := EvalString(line);
			if IsPerm(rev) = false then
				return input_line;
			fi;
			idx_x := idx_x+1;
			lad := LocalActionDiagramFromData(digraph, v_labels, e_labels, rev);
		else
			# Read the stored attributes of the local action diagram and set them. 
			lad_attrs := EvalString(line);
			if IsList(lad_attrs) = false then
				return input_line;
			fi;
			for idx_y in [1..Length(attr_list)] do
				if IsBound(lad_attrs[idx_y]) then
					EvalString(Concatenation("Set", attr_list[idx_y]))(lad, lad_attrs[idx_y]);
				fi;
			od;
			# Add the local action diagram to the list and reset the index. 
			Add(lad_list, lad);
			idx_x := 1;
		fi;
	od;

	return lad_list;
end;

_LAD_COLOURS@ := [["blue"], ["blue", "red"], ["blue", "green", "red"], ["blue", "green", "yellow", "orange"], ["blue", "green", "yellow", "orange", "red"], ["blue", "cyan", "green", "yellow", "orange", "red"]];

_LAD_GAP_NAMES@ := ["1", "C2", "C3", "S3", "C2 x C2", "C4", "D8", "A4", "S4", "C5", "C6", "D10", "D12", "C5 : C4", "A5", "S5", "C6", "C2 x C2 x C2", "C4 x C2", "C3 x C3", "C2 x D8", "(C3 x C3) : C2", "C3 x S3", "C2 x A4", "S3 x S3", "(C3 x C3) : C4", "C2 x S4", "(S3 x S3) : C2", "A6", "S6"];
_LAD_LATEX_NAMES@ := ["\\langle\\mathrm{id}\\rangle", "C_{2}", "C_{3}", "S_{3}", "C_{2}\\times C_{2}", "C_{4}", "D_{4}", "A_{4}", "S_{4}", "C_{5}", "C_{6}", "D_{5}", "D_{6}", "\\mathrm{AGL}(1,5)", "A_{5}", "S_{5}", "C_{6}", "C_{2}^{3}", "C_{4}\\times C_{2}", "C_{3}\\times C_{3}", "C_{2}\\times D_{4}", "C_{3}^{2}\\rtimes C_{2}", "C_{3}\\times S_{3}", "C_{2}\\times A_{4}", "S_{3}\\times S_{3}", "C_{3}^{2}\\rtimes C_{4}", "C_{2}\\times S_{4}", "S_{3} \\wr C_{2}", "A_{6}", "S_{6}"];

_LAD_TikzOutputString@ := function(D)
	local grp, deg, grp_name, nr, grps, degs, grp_names, M, arc_labels, arc_reversal, arcs, arc_colours, i, j, domain, orbits, nr_colours, k, l, f, output_string;

	if DigraphNrVertices(D) > 2 then
		Error("Drawing is currently only implemented for local action diagrams on up to two vertices. Try Splash(DotDigraph(D)) for more vertices to get an indication.");
	elif DigraphNrVertices(D) = 1 then
		# Setup
		grp := LocalActionDiagramVertexLabels(D)[1];
		deg := Maximum(PermGroupDomain(grp));
		grp_name := _LAD_LATEX_NAMES@[Position(_LAD_GAP_NAMES@, StructureDescription(grp))];
		arc_labels := LocalActionDiagramEdgeLabels(D);
		arc_reversal := LocalActionDiagramEdgeReversal(D);
		# rainbow-y arc colours
		arcs := DigraphEdges(D);
		arc_colours := [];

		domain := Positions(arcs, [1,1]);
		orbits := AsSortedList(Orbits(Group(arc_reversal), domain));
		nr_colours := Size(orbits);
		for k in [1..Length(orbits)] do
			for l in orbits[k] do
				arc_colours[l] := _LAD_COLOURS@[nr_colours][k];
			od;
		od;
#		return arc_colours;
		
		# Drawing
#		f := Filename(DirectoryCurrent(), Concatenation("local_action_diagram", ".tex"));

		
		output_string := "\\documentclass{standalone}";
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usepackage{tikz}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usepackage{bm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usetikzlibrary{decorations.markings}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\begin{document}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		# Picture
		output_string := Concatenation(output_string, "\\begin{tikzpicture}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\begin{scope}[xscale=1, yscale=1, thick]");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\loopangle{10}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\loopsize{2}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\looplabelsep{0}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\vertexlabelsep{1mm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");	
		
		# Loops
		domain := Positions(arcs, [1,1]);
		output_string := Concatenation(output_string, Concatenation("\\def\\nrloops{", String(Size(arcs)), "}"));
		output_string := Concatenation(output_string, "\n");
		for j in [1..Size(domain)] do
			output_string := Concatenation(output_string, Concatenation("\\draw [", arc_colours[domain[j]], "] (0:0) .. controls ({\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) and ({\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) .. (0:0);"));
			output_string := Concatenation(output_string, "\n");
			output_string := Concatenation(output_string, Concatenation("\\node at ({\\loopangle+", String(j-1),"*(180-2*\\loopangle)/\\nrloops+(180-2*\\loopangle)/\\nrloops/2}:\\loopsize-0.3+\\looplabelsep) {\\scriptsize{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]},"\\}$}};"));
			output_string := Concatenation(output_string, "\n");
			output_string := Concatenation(output_string, "\n");
		od;
		
		# Vertices and their labels
		output_string := Concatenation(output_string, "\\node (0) at (0:0) {};");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\draw [fill] (0) circle [radius=1pt];");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, Concatenation("\\node [below=\\vertexlabelsep] at (0) {\\scriptsize{$", String(grp_name), "$}};"));
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		
		# End
		output_string := Concatenation(output_string, "\\end{scope}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\end{tikzpicture}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\end{document}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
	else
		# Setup
		nr := DigraphNrVertices(D);
		grps := LocalActionDiagramVertexLabels(D);
		degs := List(grps, grp -> Maximum(PermGroupDomain(grp)));
		grp_names := List(grps, grp -> _LAD_LATEX_NAMES@[Position(_LAD_GAP_NAMES@, StructureDescription(grp))]);
		M := AdjacencyMatrix(D);
		arc_labels := LocalActionDiagramEdgeLabels(D);
		arc_reversal := LocalActionDiagramEdgeReversal(D);
		# rainbow-y arc colours
		arcs := DigraphEdges(D);
		arc_colours := [];
		for i in [1..nr] do
			for j in [i..nr] do
				domain := Union(Positions(arcs, [i,j]), Positions(arcs, [j,i]));
				orbits := AsSortedList(Orbits(Group(arc_reversal), domain));
				nr_colours := Size(orbits);
				for k in [1..Length(orbits)] do
					for l in orbits[k] do
						arc_colours[l] := _LAD_COLOURS@[nr_colours][k];
					od;
				od;
			od;				
		od;	
#		return arc_colours;
		
		# Drawing
#		f := Filename(DirectoryCurrent(), Concatenation("local_action_diagram", ".tex"));
		#f := OutputTextFile( Concatenation("local_action_diagram", ".tex"), false );;
		#SetPrintFormattingStatus(f, false);
		
		
		output_string :=  "\\documentclass{standalone}";
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usepackage{tikz}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usepackage{bm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usetikzlibrary{decorations.markings}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\begin{document}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		# Picture
		output_string := Concatenation(output_string, "\\begin{tikzpicture}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\begin{scope}[xscale=2, yscale=2, thick]");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, Concatenation("\\def\\nr{", String(nr), "}"));
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\foreach \\n in {0,...,\\nr} {\\node (\\n) at (\\n*360/\\nr:1) {};}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\loopangle{45}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\loopsize{1.3}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\looplabelsep{-2mm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\edgestart{0.15}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\edgesep{0.3}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\edgelabelsep{-0.3mm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\vertexlabelsep{3mm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");	
		
		# Loops
		for i in [1..nr] do
			domain := Positions(arcs, [i,i]);
			if domain = [] then continue; else
				output_string := Concatenation(output_string, Concatenation("\\def\\nrloops", String(i), "{", String(Size(domain)), "}"));
				output_string := Concatenation(output_string, "\n");
				for j in [1..Size(domain)] do
					output_string := Concatenation(output_string, Concatenation("\\begin{scope}[rotate={", String(i-1), "*360/\\nr}, xshift=1cm, decoration={markings, mark=at position 0.5 with {\\node[label={[black, label distance=\\looplabelsep]{", String(i-1), "*360/\\nr-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "+(180-2*\\loopangle)/\\nrloops", String(i), "/2}:\\tiny{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]}, "\\}$}}]{};}}] \\draw [", arc_colours[domain[j]], ", postaction=decorate] (0:0) .. controls ({-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) and ({-90+\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) .. (0:0); \\end{scope}"));
					output_string := Concatenation(output_string, "\n");
					output_string := Concatenation(output_string, "\n");
				od;
			fi;
		od;
		
		# Arcs (only works for one or two vertices at the moment, TODO: improve this to work for any number of vertices, the rest of the code should be mostly fine)
		# 1 to 2
		domain := Positions(arcs, [1,2]);
		if not domain = [] then
			output_string := Concatenation(output_string, Concatenation("\\def\\nrarcs_", String(i), "_", String(j), "{", String(Size(domain)), "}"));
			output_string := Concatenation(output_string, "\n");
			for k in [1..Size(domain)] do
				# arc
				output_string := Concatenation(output_string, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{>}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, above=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [", arc_colours[domain[k]], ", postaction=decorate] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				output_string := Concatenation(output_string, "\n");
				output_string := Concatenation(output_string, "\n");
			od;
		fi;
		
		
		# 2 to 1
		domain := Positions(arcs, [2,1]);
		if not domain = [] then
			output_string := Concatenation(output_string, Concatenation("\\def\\nrarcs_", String(i), "_", String(j), "{", String(Size(domain)), "}"));
			output_string := Concatenation(output_string, "\n");
			for k in [1..Size(domain)] do
				# arc
				output_string := Concatenation(output_string, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{<}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, below=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [", arc_colours[domain[k]], ", postaction=decorate, xshift=1cm, rotate=360/\\nr/2, xscale=-1, rotate=-360/\\nr/2, xshift=-1cm] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				output_string := Concatenation(output_string, "\n");
				output_string := Concatenation(output_string, "\n");
			od;
		fi;		
		
		# Vertices and their labels
		for i in [1..nr] do
			output_string := Concatenation(output_string, Concatenation("\\node (", String(i), ") at (", String(i), "*360/\\nr:1) {};"));
			output_string := Concatenation(output_string, "\n");
			output_string := Concatenation(output_string, Concatenation("\\draw [fill] (", String(i), ") circle [radius=1pt];"));
			output_string := Concatenation(output_string, "\n");
			output_string := Concatenation(output_string, Concatenation("\\node [below=\\vertexlabelsep] at (", String(i), ") {\\tiny{$", String(grp_names[i]), "$}};"));
			output_string := Concatenation(output_string, "\n");

		od;
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		
		# End
		output_string := Concatenation(output_string, "\\end{scope}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\end{tikzpicture}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\end{document}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");			
	fi;

	return output_string;
end;

_LAD_TikzOutputStringScopo@ := function(D, scopo)
	local grp, deg, grp_name, nr, grps, degs, grp_names, M, arc_labels, arc_reversal, arcs, arc_colours, i, j, domain, orbits, nr_colours, k, l, f, output_string;

	if not ForAll(scopo, i -> i in [1..DigraphNrEdges(D)]) then
		Error("The scopo must be set of indices of list of edges of D");
	elif DigraphNrVertices(D) > 2 then
		Error("Drawing is currently only implemented for local action diagrams on up to two vertices. Try Splash(DotDigraph(D)) for more vertices to get an indication.");
	elif DigraphNrVertices(D) = 1 then
		# Setup
		grp := LocalActionDiagramVertexLabels(D)[1];
		deg := Maximum(PermGroupDomain(grp));
		grp_name := _LAD_LATEX_NAMES@[Position(_LAD_GAP_NAMES@, StructureDescription(grp))];
		arc_labels := LocalActionDiagramEdgeLabels(D);
		arc_reversal := LocalActionDiagramEdgeReversal(D);
		# rainbow-y arc colours
		arcs := DigraphEdges(D);
		arc_colours := [];

		domain := Positions(arcs, [1,1]);
		orbits := AsSortedList(Orbits(Group(arc_reversal), domain));
		nr_colours := Size(orbits);
		for k in [1..Length(orbits)] do
			for l in orbits[k] do
				arc_colours[l] := _LAD_COLOURS@[nr_colours][k];
			od;
		od;
#		return arc_colours;
		
		# Drawing
#		f := Filename(DirectoryCurrent(), Concatenation("local_action_diagram", ".tex"));
		
		output_string :=  "\\documentclass{standalone}";
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usepackage{tikz}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usepackage{bm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usetikzlibrary{decorations.markings}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\begin{document}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		# Picture
		output_string := Concatenation(output_string, "\\begin{tikzpicture}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\begin{scope}[xscale=1, yscale=1, thick]");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\loopangle{10}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\loopsize{2}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\looplabelsep{0}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\vertexlabelsep{1mm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");	
		
		# Loops
		domain := Positions(arcs, [1,1]);
		output_string := Concatenation(output_string, Concatenation("\\def\\nrloops{", String(Size(arcs)), "}"));
		output_string := Concatenation(output_string, "\n");
		for j in [1..Size(domain)] do
			if domain[j] in scopo then
				output_string := Concatenation(output_string, Concatenation("\\draw [dashed, ", arc_colours[domain[j]], "] (0:0) .. controls ({\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) and ({\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) .. (0:0);"));
				output_string := Concatenation(output_string, "\n");
				output_string := Concatenation(output_string, Concatenation("\\node at ({\\loopangle+", String(j-1),"*(180-2*\\loopangle)/\\nrloops+(180-2*\\loopangle)/\\nrloops/2}:\\loopsize-0.3+\\looplabelsep) {\\scriptsize{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]},"\\}$}};"));
				output_string := Concatenation(output_string, "\n");
				output_string := Concatenation(output_string, "\n");
			else
				output_string := Concatenation(output_string, Concatenation("\\draw [", arc_colours[domain[j]], "] (0:0) .. controls ({\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) and ({\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) .. (0:0);"));
				output_string := Concatenation(output_string, "\n");
				output_string := Concatenation(output_string, Concatenation("\\node at ({\\loopangle+", String(j-1),"*(180-2*\\loopangle)/\\nrloops+(180-2*\\loopangle)/\\nrloops/2}:\\loopsize-0.3+\\looplabelsep) {\\scriptsize{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]},"\\}$}};"));
				output_string := Concatenation(output_string, "\n");
				output_string := Concatenation(output_string, "\n");
			fi;
		od;
		
		# Vertices and their labels
		output_string := Concatenation(output_string, "\\node (0) at (0:0) {};");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\draw [fill] (0) circle [radius=1pt];");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, Concatenation("\\node [below=\\vertexlabelsep] at (0) {\\scriptsize{$", String(grp_name), "$}};"));
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		
		# End
		output_string := Concatenation(output_string, "\\end{scope}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\end{tikzpicture}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\end{document}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
	else
		# Setup
		nr := DigraphNrVertices(D);
		grps := LocalActionDiagramVertexLabels(D);
		degs := List(grps, grp -> Maximum(PermGroupDomain((grp))));
		grp_names := List(grps, grp -> _LAD_LATEX_NAMES@[Position(_LAD_GAP_NAMES@, StructureDescription(grp))]);
		M := AdjacencyMatrix(D);
		arc_labels := LocalActionDiagramEdgeLabels(D);
		arc_reversal := LocalActionDiagramEdgeReversal(D);
		# rainbow-y arc colours
		arcs := DigraphEdges(D);
		arc_colours := [];
		for i in [1..nr] do
			for j in [i..nr] do
				domain := Union(Positions(arcs, [i,j]), Positions(arcs, [j,i]));
				orbits := AsSortedList(Orbits(Group(arc_reversal), domain));
				nr_colours := Size(orbits);
				for k in [1..Length(orbits)] do
					for l in orbits[k] do
						arc_colours[l] := _LAD_COLOURS@[nr_colours][k];
					od;
				od;
			od;				
		od;	
#		return arc_colours;
		
		# Drawing
#		f := Filename(DirectoryCurrent(), Concatenation("local_action_diagram", ".tex"));
		
		
		output_string := "\\documentclass{standalone}";
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usepackage{tikz}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usepackage{bm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\usetikzlibrary{decorations.markings}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\begin{document}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		# Picture
		output_string := Concatenation(output_string, "\\begin{tikzpicture}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\begin{scope}[xscale=2, yscale=2, thick]");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, Concatenation("\\def\\nr{", String(nr), "}"));
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\foreach \\n in {0,...,\\nr} {\\node (\\n) at (\\n*360/\\nr:1) {};}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\loopangle{45}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\loopsize{1.3}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\looplabelsep{-2mm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\edgestart{0.15}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\edgesep{0.3}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\edgelabelsep{-0.3mm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\def\\vertexlabelsep{3mm}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");	
		
		# Loops
		for i in [1..nr] do
			domain := Positions(arcs, [i,i]);
			if domain = [] then continue; else
				output_string := Concatenation(output_string, Concatenation("\\def\\nrloops", String(i), "{", String(Size(domain)), "}"));
				output_string := Concatenation(output_string, "\n");
				for j in [1..Size(domain)] do
					if domain[j] in scopo then
						output_string := Concatenation(output_string, Concatenation("\\begin{scope}[rotate={", String(i-1), "*360/\\nr}, xshift=1cm, decoration={markings, mark=at position 0.5 with {\\node[label={[black, label distance=\\looplabelsep]{", String(i-1), "*360/\\nr-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "+(180-2*\\loopangle)/\\nrloops", String(i), "/2}:\\tiny{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]}, "\\}$}}]{};}}] \\draw [dashed,", arc_colours[domain[j]], ", postaction=decorate] (0:0) .. controls ({-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) and ({-90+\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) .. (0:0); \\end{scope}"));
					else
						output_string := Concatenation(output_string, Concatenation("\\begin{scope}[rotate={", String(i-1), "*360/\\nr}, xshift=1cm, decoration={markings, mark=at position 0.5 with {\\node[label={[black, label distance=\\looplabelsep]{", String(i-1), "*360/\\nr-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "+(180-2*\\loopangle)/\\nrloops", String(i), "/2}:\\tiny{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]}, "\\}$}}]{};}}] \\draw [", arc_colours[domain[j]], ", postaction=decorate] (0:0) .. controls ({-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) and ({-90+\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) .. (0:0); \\end{scope}"));
					fi;
					output_string := Concatenation(output_string, "\n");
					output_string := Concatenation(output_string, "\n");
				od;
			fi;
		od;
		
		# Arcs (only works for one or two vertices at the moment, TODO: improve this to work for any number of vertices, the rest of the code should be mostly fine)
		# 1 to 2
		domain := Positions(arcs, [1,2]);
		if not domain = [] then
			output_string := Concatenation(output_string, Concatenation("\\def\\nrarcs_", String(i), "_", String(j), "{", String(Size(domain)), "}"));
			output_string := Concatenation(output_string, "\n");
			for k in [1..Size(domain)] do
				# arc
				if domain[k] in scopo then
					output_string := Concatenation(output_string, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{>}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, above=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [dashed,", arc_colours[domain[k]], ", postaction=decorate] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				else
					output_string := Concatenation(output_string, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{>}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, above=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [", arc_colours[domain[k]], ", postaction=decorate] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				fi;
				output_string := Concatenation(output_string, "\n");
				output_string := Concatenation(output_string, "\n");
			od;
		fi;
		
		
		# 2 to 1
		domain := Positions(arcs, [2,1]);
		if not domain = [] then
			output_string := Concatenation(output_string, Concatenation("\\def\\nrarcs_", String(i), "_", String(j), "{", String(Size(domain)), "}"));
			output_string := Concatenation(output_string, "\n");
			for k in [1..Size(domain)] do
				# arc
				if domain[k] in scopo then
					output_string := Concatenation(output_string, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{<}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, below=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [dashed,", arc_colours[domain[k]], ", postaction=decorate, xshift=1cm, rotate=360/\\nr/2, xscale=-1, rotate=-360/\\nr/2, xshift=-1cm] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				else
					output_string := Concatenation(output_string, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{<}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, below=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [", arc_colours[domain[k]], ", postaction=decorate, xshift=1cm, rotate=360/\\nr/2, xscale=-1, rotate=-360/\\nr/2, xshift=-1cm] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				fi;
				output_string := Concatenation(output_string, "\n");
				output_string := Concatenation(output_string, "\n");
			od;
		fi;

		
		
		
		
		
		
		# Vertices and their labels
		for i in [1..nr] do
			output_string := Concatenation(output_string, Concatenation("\\node (", String(i), ") at (", String(i), "*360/\\nr:1) {};"));
			output_string := Concatenation(output_string, "\n");
			output_string := Concatenation(output_string, Concatenation("\\draw [fill] (", String(i), ") circle [radius=1pt];"));
			output_string := Concatenation(output_string, "\n");
			output_string := Concatenation(output_string, Concatenation("\\node [below=\\vertexlabelsep] at (", String(i), ") {\\tiny{$", String(grp_names[i]), "$}};"));
			output_string := Concatenation(output_string, "\n");

		od;
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");

		
		# End
		output_string := Concatenation(output_string, "\\end{scope}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\end{tikzpicture}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\\end{document}");
		output_string := Concatenation(output_string, "\n");
		output_string := Concatenation(output_string, "\n");
	
		
	fi;

	return output_string;
end;

#######################
### Install Methods ###
#######################

InstallMethod(WriteLocalActionDiagram, "Label This", [IsLocalActionDiagram, IsString],
function(lad, file_name)
	local output_string, output_stream, vl, el, fl_name;
	
	output_string := _LAD_WriteSingleLADStringFormat@(lad);

	fl_name := Filename(DirectoryCurrent(), StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(WriteLocalActionDiagram, "Label This", [IsLocalActionDiagram, IsString, IsDirectory],
function(lad, file_name, dir)
	local output_string, output_stream, vl, el, fl_name;

	# dir![1] is the string of the directory dir. Yes... I looked through the source code
	# to find this out. 
	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir![1]));
		return fail;
	fi;
	
	output_string := _LAD_WriteSingleLADStringFormat@(lad);

	fl_name := Filename(dir, StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(WriteLocalActionDiagram, "Label This", [IsLocalActionDiagram, IsString, IsString],
function(lad, file_name, dir_string)
	local dir, output_string, output_stream, vl, el, fl_name;

	dir := Directory(dir_string);

	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir_string));
		return fail;
	fi;
	
	output_string := _LAD_WriteSingleLADStringFormat@(lad);

	fl_name := Filename(dir, StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);


InstallMethod(WriteLocalActionDiagram, "Label This", [IsList, IsString],
function(lad_list, file_name)
	local lad, output_string, output_stream, vl, el, fl_name;


	if not ForAll(lad_list, IsLocalActionDiagram) then
		ErrorNoReturn("The argument <lad_list> must be a dense list of local action diagrams.");
		return fail;
	fi;
	
	output_string := "";
	for lad in lad_list do
		output_string := Concatenation(output_string, _LAD_WriteSingleLADStringFormat@(lad));
	od;


	fl_name := Filename(DirectoryCurrent(), StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(WriteLocalActionDiagram, "Label This", [IsList, IsString, IsDirectory],
function(lad_list, file_name, dir)
	local lad, output_string, output_stream, vl, el, fl_name;
	
	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir![1]));
		return fail;
	fi;

	if not ForAll(lad_list, IsLocalActionDiagram) then
		ErrorNoReturn("The argument <lad_list> must be a dense list of local action diagrams.");
		return fail;
	fi;
	
	output_string := "";
	for lad in lad_list do
		output_string := Concatenation(output_string, _LAD_WriteSingleLADStringFormat@(lad));
	od;

	fl_name := Filename(dir, StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(WriteLocalActionDiagram, "Label This", [IsList, IsString, IsString],
function(lad_list, file_name, dir_string)
	local lad, output_string, output_stream, vl, el, fl_name, dir;

	dir := Directory(dir_string);
	
	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir_string));
		return fail;
	fi;

	if not ForAll(lad_list, IsLocalActionDiagram) then
		ErrorNoReturn("The argument <lad_list> must be a dense list of local action diagrams.");
		return fail;
	fi;
	
	output_string := "";
	for lad in lad_list do
		output_string := Concatenation(output_string, _LAD_WriteSingleLADStringFormat@(lad));
	od;

	fl_name := Filename(dir, StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);



InstallMethod(ReadLocalActionDiagram, "Label This", [IsString],
function(file_name)
	local lad_list, input_string, fl_name, input_stream;

	# Read the file *file_name* and write its contents to *input_string*.
	fl_name := Filename(DirectoryCurrent(), file_name);
	input_stream := InputTextFile(fl_name);
	if input_stream = fail then
		ErrorNoReturn(StringFormatted("File <{1}> does not exist.", file_name));
		return fail;
	fi;
	input_string := ReadAll(input_stream);
	CloseStream(input_stream);

	lad_list := _LAD_ReadLADString@(input_string);

	if IsInt(lad_list) then
		ErrorNoReturn(StringFormatted("Error on line {1} of {2}", lad_list, file_name));
		return fail;
	fi;

	return lad_list;
end);

InstallMethod(ReadLocalActionDiagram, "Label This", [IsString, IsDirectory],
function(file_name, dir)
	local lad_list, input_string, fl_name, input_stream;

	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir![1]));
		return fail;
	fi;

	# Read the file *file_name* and write its contents to *input_string*.
	fl_name := Filename(dir, file_name);
	input_stream := InputTextFile(fl_name);
	if input_stream = fail then
		ErrorNoReturn(StringFormatted("File <{1}> does not exist.", file_name));
		return fail;
	fi;
	input_string := ReadAll(input_stream);
	CloseStream(input_stream);

	lad_list := _LAD_ReadLADString@(input_string);

	if IsInt(lad_list) then
		ErrorNoReturn(StringFormatted("Error on line {1} of {2}", lad_list, file_name));
		return fail;
	fi;

	return lad_list;
end);

InstallMethod(ReadLocalActionDiagram, "Label This", [IsString, IsString],
function(file_name, dir_string)
	local lad_list, input_string, fl_name, input_stream, dir;

	dir := Directory(dir_string);

	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir_string));
		return fail;
	fi;

	# Read the file *file_name* and write its contents to *input_string*.
	fl_name := Filename(dir, file_name);
	input_stream := InputTextFile(fl_name);
	if input_stream = fail then
		ErrorNoReturn(StringFormatted("File <{1}> does not exist.", file_name));
		return fail;
	fi;
	input_string := ReadAll(input_stream);
	CloseStream(input_stream);

	lad_list := _LAD_ReadLADString@(input_string);

	if IsInt(lad_list) then
		ErrorNoReturn(StringFormatted("Error on line {1} of {2}", lad_list, file_name));
		return fail;
	fi;

	return lad_list;
end);

InstallMethod(GenerateTikZCode, "Label This", [IsLocalActionDiagram, IsString],
function(lad, file_name)
	local fl_name, output_stream, output_string;

	output_string := _LAD_TikzOutputString@(lad); 

	fl_name := Filename(DirectoryCurrent(), StringFormatted("{1}.tex", file_name));
	output_stream := OutputTextFile(fl_name, true);
	SetPrintFormattingStatus(output_stream, false);		
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(GenerateTikZCode, "Label This", [IsLocalActionDiagram, IsString, IsList],
function(lad, file_name, scopo)
	local fl_name, output_stream, output_string, dir;

	# Function could have the last parameter be the output directory string or a scopo. 
	# Need to check because IsList is a sub-category of IsString. GAP complains if 
	# you have two methods for both. 
	if IsString(scopo) then
		dir := Directory(scopo);
		output_string := _LAD_TikzOutputStringScopo@(lad); 
	else
		dir := DirectoryCurrent();
		output_string := _LAD_TikzOutputStringScopo@(lad, scopo); 
	fi;


	fl_name := Filename(dir, StringFormatted("{1}.tex", file_name));
	output_stream := OutputTextFile(fl_name, true);
	SetPrintFormattingStatus(output_stream, false);		
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(GenerateTikZCode, "Label This", [IsLocalActionDiagram, IsString, IsDirectory],
function(lad, file_name, dir)
	local fl_name, output_stream, output_string;

	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir![1]));
		return fail;
	fi;

	output_string := _LAD_TikzOutputString@(lad); 

	fl_name := Filename(dir, StringFormatted("{1}.tex", file_name));
	output_stream := OutputTextFile(fl_name, true);
	SetPrintFormattingStatus(output_stream, false);		
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

#InstallMethod(GenerateTikZCode, "Label This", [IsLocalActionDiagram, IsString, IsString],
#function(lad, file_name, dir_name)
#	local fl_name, output_stream, output_string, dir;
#
#	dir := Directory(dir_name);
#
#	if DirectoryContents(dir) = fail then
#		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir![1]));
#		return fail;
#	fi;
#
#	output_string := _LAD_TikzOutputString@(lad); 
#
#	fl_name := Filename(dir, StringFormatted("{1}.tex", file_name));
#	output_stream := OutputTextFile(fl_name, true);
#	SetPrintFormattingStatus(output_stream, false);		
#	AppendTo(output_stream, output_string);
#	CloseStream(output_stream);
#
#	return true;
#end);

InstallMethod(GenerateTikZCode, "Label This", [IsLocalActionDiagram, IsString, IsList, IsDirectory],
function(lad, file_name, scopo, dir)
	local fl_name, output_stream, output_string;

	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir![1]));
		return fail;
	fi;

	output_string := _LAD_TikzOutputStringScopo@(lad, scopo); 

	fl_name := Filename(dir, StringFormatted("{1}.tex", file_name));
	output_stream := OutputTextFile(fl_name, true);
	SetPrintFormattingStatus(output_stream, false);		
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(GenerateTikZCode, "Label This", [IsLocalActionDiagram, IsString, IsList, IsString],
function(lad, file_name, scopo, dir_name)
	local fl_name, output_stream, output_string, dir;

	dir := Directory(dir_name);

	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir![1]));
		return fail;
	fi;

	output_string := _LAD_TikzOutputStringScopo@(lad, scopo); 

	fl_name := Filename(dir, StringFormatted("{1}.tex", file_name));
	output_stream := OutputTextFile(fl_name, true);
	SetPrintFormattingStatus(output_stream, false);		
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

