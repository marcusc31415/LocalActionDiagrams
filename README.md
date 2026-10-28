# The GAP package LocalActionDiagrams

This package implements a category for Local Action Diagrams, which were first introduced by C. Reid and S. Smith in [[1]](#1). To install it, placed it in the `pkg` folder of a GAP root directories. GAP root directories can be found from the `GAPInfo.RootPaths` variable in a GAP session.

To load the package in GAP, use `LoadPackage("localactiondiagrams")`. Note that this package requires the Digraphs package.

To make a local action diagram in GAP, use the `LocalActionDiagramFromData(D, vl, el, r)` function, where `D` is an immutable digraph, `vl` is a list of groups labelling the vertices of `D`, `el` is a list of sets labelling the edges of `D`, and `r` is a reversal mapping of the edges of `D`. The `LocalActionDiagramFromData` function will check that the data forms a valid local action diagram while the `LocalActionDiagramFromDataNC` won't.

Currently the package supports finding local action diagrams isomorphisms through the `IsomorphismLocalActionDiagrams` function, finding strongly confluent partial orientations through the `LocalActionDiagramScopos` function, and enumerating all local action diagrams with one or two vertices with labels that are subgroups of Aut(T<sub>d</sub>) with the `AllLocalActionDiagrams` function. 

## References

<a id="1">[1]</a> C.  D.  Reid  and  S.  M.  Smith, _Groups  acting  on  trees  with  tits’  independence  property  (p)_, 2020. DOI:10.48550/ARXIV.2002.11766. [Online]. Available:c https://arxiv.org/abs/2002.11766
