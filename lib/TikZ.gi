# Example
# D:=LocalActionDiagramFromLibrary(3,1,6);
# GenerateTikZCode(D);

InstallMethod(GenerateTikZCode, [IsLocalActionDiagram],
function(D)
	local COLOURS, GROUP_NAMES, grp, deg, grp_name, arc_labels, arc_reversal, graph, arcs,  orbits, sorted_orbits, nr_colours, arc_order, nr_loops, arc_colours, a, label, j, k, l, f, vertices, grp1, grp2, deg1, deg2, grp_name1, grp_name2, arc_rec, loops1, loops2, non_loops, orbits1, orbits2, orbits_non_loops, sorted_orbits1, sorted_orbits2, sorted_orbits_non_loops, nr_colours1, nr_colours2, nr_colours_non_loops, arc_order1, arc_order2, arc_order_non_loops, nr_loops1, nr_loops2, nr_non_loops, arc_colours1, arc_colours2, arc_colours_non_loops, a1, a2, label1, label2;
	
	COLOURS := [
	["blue"],
	["blue", "red"],
	["blue", "green", "red"],
	["blue", "green", "yellow", "orange"],
	["blue", "green", "yellow", "orange", "red"],
	["blue", "cyan", "green", "yellow", "orange", "red"] ];
	
	GROUP_NAMES := rec(
	("1") := "\\langle\\mathrm{id}\\rangle",
	("C2") := "C_{2}",
	("C3") := "C_{3}",
	("S3") := "S_{3}",
	("C2 x C2") := "C_{2}\\times C_{2}",
	("C4") := "C_{4}",
	("D8") := "D_{4}",
	("A4") := "A_{4}",
	("S4") := "S_{4}",
	("C5") := "C_{5}",
	("C6") := "C_{6}",
	("D10") := "D_{5}",
	("D12") := "D_{6}",
	("C5 : C4") := "\\mathrm{AGL}(1,5)",
	("A5") := "A_{5}",
	("S5") := "S_{5}",
	("C6") := "C_{6}",
	("C2 x C2 x C2") := "C_{2}^{3}",
	("C4 x C2") := "C_{4}\\times C_{2}",
	("C3 x C3") := "C_{3}\\times C_{3}",
	("C2 x D8") := "C_{2}\\times D_{4}",
	("(C3 x C3) : C2") := "C_{3}^{2}\\rtimes C_{2}",
	("C3 x S3") := "C_{3}\\times S_{3}",
	("C2 x A4") := "C_{2}\\times A_{4}",
	("S3 x S3") := "S_{3}\\times S_{3}",
	("(C3 x C3) : C4") := "C_{3}^{2}\\rtimes C_{4}",
	("C2 x S4") := "C_{2}\\times S_{4}",
	("(S3 x S3) : C2") := "S_{3} \\wr C_{2}",
	("A6") := "A_{6}",
	("S6") := "S_{6}" );	

	if Length(LocalActionDiagramVertices(D)) = 1 then
		# Setup
		grp := LocalActionDiagramVertexLabels(D).(LocalActionDiagramVertices(D)[1]);
		deg := Size(PermGroupDomain(grp));
		if deg > 6 then
			Error("TikZ code is currently only implemented for local action diagrams with permutation groups of degree at most 6.");
		fi;
		
		grp_name := GROUP_NAMES.(StructureDescription(grp));
		arc_labels := LocalActionDiagramArcLabels(D);
		arc_reversal := LocalActionDiagramReverseMap(D);

		graph := LocalActionDiagramRSGraph(D);
		arcs := SortedList(List(RecNames(RSGraphArcs(graph)), x -> Int(x)));

		orbits := Orbits(Group(arc_reversal), arcs);
		sorted_orbits := SortedList(List(orbits, SortedList));
		nr_colours := Size(orbits); # =Size(sorted_orbits)

		arc_order := Concatenation(sorted_orbits);
		nr_loops := Size(arc_order);

		# rainbow-y arc colours
		arc_colours := [];
		for k in [1..nr_colours] do
			for l in sorted_orbits[k] do
				arc_colours[l] := COLOURS[nr_colours][k];
			od;
		od;

		# Drawing
#		f := Filename(DirectoryCurrent(), Concatenation("local_action_diagram", ".tex"));
		f := OutputTextFile( Concatenation("local_action_diagram", ".tex"), false );;
		SetPrintFormattingStatus(f, false);		
		
		PrintTo(f, "\\documentclass{standalone}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{tikz}");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{bm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usetikzlibrary{decorations.markings}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		# Picture
		AppendTo(f, "\\begin{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{scope}[xscale=1, yscale=1, thick]");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopangle{10}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopsize{2}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\looplabelsep{0}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\vertexlabelsep{1mm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");	
		
		# Loops
		AppendTo(f, Concatenation("\\def\\nrloops{", String(nr_loops), "}"));
		AppendTo(f, "\n");
		for j in [1..nr_loops] do
			a := arc_order[j];
			label := String(arc_labels.(String(a)));
			label := label{[2..Length(label)-1]};
			AppendTo(f, Concatenation("\\draw [", arc_colours[a], "] (0:0) .. controls ({\\loopangle+", String(nr_loops-j), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) and ({\\loopangle+", String(nr_loops-j+1), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) .. (0:0);"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\node at ({\\loopangle+", String(nr_loops-j),"*(180-2*\\loopangle)/\\nrloops+(180-2*\\loopangle)/\\nrloops/2}:\\loopsize-0.3+\\looplabelsep) {\\scriptsize{$\\{", label,"\\}$}};"));
			AppendTo(f, "\n");
			AppendTo(f, "\n");
		od;

		# Vertex and label
		AppendTo(f, "\\node (0) at (0:0) {};");
		AppendTo(f, "\n");
		AppendTo(f, "\\draw [fill] (0) circle [radius=1pt];");
		AppendTo(f, "\n");
		AppendTo(f, Concatenation("\\node [below=\\vertexlabelsep] at (0) {\\scriptsize{$", String(grp_name), "$}};"));
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		# End
		AppendTo(f, "\\end{scope}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		Print("File local_action_diagram.tex generated in the current working directory.");
		
	elif Length(LocalActionDiagramVertices(D)) = 2 then
		# Setup		
		vertices := LocalActionDiagramVertices(D);
		grp1 := LocalActionDiagramVertexLabels(D).(vertices[1]);
		grp2 := LocalActionDiagramVertexLabels(D).(vertices[2]);
		deg1 := Size(PermGroupDomain(grp1));
		deg2 := Size(PermGroupDomain(grp2));
		if deg1 > 6 or deg2 > 6 then
			Error("TikZ code is currently only implemented for local action diagrams with permutation groups of degree at most 6.");
		fi;
		
		grp_name1 := GROUP_NAMES.(StructureDescription(grp1));
		grp_name2 := GROUP_NAMES.(StructureDescription(grp2));
		
		arc_labels := LocalActionDiagramArcLabels(D);
		arc_reversal := LocalActionDiagramReverseMap(D);

		graph := LocalActionDiagramRSGraph(D);
		arc_rec := RSGraphArcs(graph);
		arcs := SortedList(List(RecNames(arc_rec), x -> Int(x)));
		loops1 := Filtered(arcs, a -> arc_rec.(a).origin=vertices[1] and arc_rec.(a).terminus=vertices[1]);
		loops2 := Filtered(arcs, a -> arc_rec.(a).origin=vertices[2] and arc_rec.(a).terminus=vertices[2]);
		non_loops := Filtered(arcs, a -> arc_rec.(a).origin <> arc_rec.(a).terminus);
						
		orbits1 := Orbits(Group(arc_reversal), loops1);
		orbits2 := Orbits(Group(arc_reversal), loops2);
		orbits_non_loops := Orbits(Group(arc_reversal), non_loops);
		
		sorted_orbits1 := SortedList(List(orbits1, SortedList));
		sorted_orbits2 := SortedList(List(orbits2, SortedList));
		sorted_orbits_non_loops := SortedList(List(orbits_non_loops, SortedList));		
				
		nr_colours1 := Size(orbits1);
		nr_colours2 := Size(orbits2);
		nr_colours_non_loops := Size(orbits_non_loops);

		arc_order1 := Concatenation(sorted_orbits1);
		arc_order2 := Concatenation(sorted_orbits2);
		arc_order_non_loops := Concatenation(sorted_orbits_non_loops);
		
		nr_loops1 := Size(arc_order1);
		nr_loops2 := Size(arc_order2);
		nr_non_loops := Size(arc_order_non_loops);

		# rainbow-y arc colours
		arc_colours1 := [];
		arc_colours2 := [];
		arc_colours_non_loops := [];
		
		for k in [1..nr_colours1] do
			for l in sorted_orbits1[k] do
				arc_colours1[l] := COLOURS[nr_colours1][k];
			od;
		od;

		for k in [1..nr_colours2] do
			for l in sorted_orbits2[k] do
				arc_colours2[l] := COLOURS[nr_colours2][k];
			od;
		od;

		for k in [1..nr_colours_non_loops] do
			for l in sorted_orbits_non_loops[k] do
				arc_colours_non_loops[l] := COLOURS[nr_colours_non_loops][k];
			od;
		od;
		
		# Drawing
#		f := Filename(DirectoryCurrent(), Concatenation("local_action_diagram", ".tex"));
		f := OutputTextFile( Concatenation("local_action_diagram", ".tex"), false );;
		SetPrintFormattingStatus(f, false);		
		
		PrintTo(f, "\\documentclass{standalone}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{tikz}");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{bm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usetikzlibrary{decorations.markings}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		# Picture
		AppendTo(f, "\\begin{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{scope}[xscale=1, yscale=1, thick]");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopangle{35}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopsize{2}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\looplabelsep{0}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\nonlooplabelsep{0.2}");		
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\vertexlabelsep{2mm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		# Vertices and labels
		AppendTo(f, "\\node (1) at (180:1) {};");
		AppendTo(f, "\n");
		AppendTo(f, "\\draw [fill] (1) circle [radius=1pt];");
		AppendTo(f, "\n");
		AppendTo(f, Concatenation("\\node [below=\\vertexlabelsep] at (1) {\\scriptsize{$", String(grp_name1), "$}};"));
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		AppendTo(f, "\\node (2) at (0:1) {};");
		AppendTo(f, "\n");
		AppendTo(f, "\\draw [fill] (2) circle [radius=1pt];");
		AppendTo(f, "\n");
		AppendTo(f, Concatenation("\\node [below=\\vertexlabelsep] at (2) {\\scriptsize{$", String(grp_name2), "$}};"));
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		# Loops1
		AppendTo(f, Concatenation("\\def\\nrloops1{", String(nr_loops1), "}"));
		AppendTo(f, "\n");
				
		AppendTo(f, "\\begin{scope}[shift={(-1,0)}]");
		AppendTo(f, "\n");	
		AppendTo(f, "\\begin{scope}[rotate=90]");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		for j in [1..nr_loops1] do
			a := arc_order1[j];
			label := String(arc_labels.(String(a)));
			label := label{[2..Length(label)-1]};
			AppendTo(f, Concatenation("\\draw [", arc_colours1[a], "] (0:0) .. controls ({\\loopangle+", String(nr_loops1-j), "*(180-2*\\loopangle)/\\nrloops1}:\\loopsize) and ({\\loopangle+", String(nr_loops1-j+1), "*(180-2*\\loopangle)/\\nrloops1}:\\loopsize) .. (0:0);"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\node at ({\\loopangle+", String(nr_loops1-j),"*(180-2*\\loopangle)/\\nrloops1+(180-2*\\loopangle)/\\nrloops1/2}:\\loopsize-0.3+\\looplabelsep) {\\scriptsize{$\\{", label,"\\}$}};"));
			AppendTo(f, "\n");
			AppendTo(f, "\n");
		od;
		
		AppendTo(f, "\\end{scope}");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{scope}");
		AppendTo(f, "\n");
		
		# Loops2
		AppendTo(f, Concatenation("\\def\\nrloops2{", String(nr_loops2), "}"));
		AppendTo(f, "\n");
				
		AppendTo(f, "\\begin{scope}[shift={(1,0)}]");
		AppendTo(f, "\n");	
		AppendTo(f, "\\begin{scope}[rotate=-90]");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		for j in [1..nr_loops2] do
			a := arc_order2[j];
			label := String(arc_labels.(String(a)));
			label := label{[2..Length(label)-1]};
			AppendTo(f, Concatenation("\\draw [", arc_colours2[a], "] (0:0) .. controls ({\\loopangle+", String(nr_loops2-j), "*(180-2*\\loopangle)/\\nrloops2}:\\loopsize) and ({\\loopangle+", String(nr_loops2-j+1), "*(180-2*\\loopangle)/\\nrloops2}:\\loopsize) .. (0:0);"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\node at ({\\loopangle+", String(nr_loops2-j),"*(180-2*\\loopangle)/\\nrloops2+(180-2*\\loopangle)/\\nrloops2/2}:\\loopsize-0.3+\\looplabelsep) {\\scriptsize{$\\{", label,"\\}$}};"));
			AppendTo(f, "\n");
			AppendTo(f, "\n");
		od;
		
		AppendTo(f, "\\end{scope}");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{scope}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		# Non-loops
		for j in [1..nr_non_loops/2] do
			a1 := arc_order_non_loops[2*j-1];
			a2 := arc_order_non_loops[2*j];
			label1 := String(arc_labels.(String(a1)));
			label1 := label1{[2..Length(label1)-1]};
			label2 := String(arc_labels.(String(a2)));
			label2 := label2{[2..Length(label2)-1]};
			AppendTo(f, Concatenation("\\draw [", arc_colours_non_loops[a1], "] (-1,0) .. controls (-0.5, {0.25+(", String(j), "-1)*0.5}) and (0.5, {0.25+(", String(j), "-1)*0.5}) .. (1,0);"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\node at (0, {0.25+(", String(j), "-1)*0.5+\\nonlooplabelsep}) {\\scriptsize{$\\{", label1, "\\}$}};"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\draw [", arc_colours_non_loops[a2], "] (-1,0) .. controls (-0.5, {-0.25-(", String(j), "-1)*0.5}) and (0.5, {-0.25-(", String(j), "-1)*0.5}) .. (1,0);"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\node at (0, {-0.25-(", String(j), "-1)*0.5-\\nonlooplabelsep}) {\\scriptsize{$\\{", label2, "\\}$}};"));
			AppendTo(f, "\n");
		od;
		
		# End
		AppendTo(f, "\\end{scope}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");

		Print("File local_action_diagram.tex generated in the current working directory.");

	elif Length(LocalActionDiagramVertices(D)) > 2 then	
		Error("TiKZ code is currently only implemented for local action diagrams on up to two vertices.");
	fi;
	
end);
