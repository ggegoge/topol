# topol
topological sorting of a graph. written in ocaml. for a university
course actually

On this branch I've implemented the __dfs algorithm__ for topological
sorting. For further info you can
consult
[wikipedia](https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search) and
the `topol.ml` file itself. 

It differs from the `dfs` branch in how the permanent/temporary mark
system is implemented. Here it is an information stored in every
vertex of the graph with a variant type consisting of variants `Perm |
Temp | None` and in the `dfs` branch it is implemented with a "_set_"
– I use the `pMap` module but I store vertex-unit pairs in it as
bindings and I never really acces the units (why would i) so it is
just a set of vertices really. And I have two of them – a `perm` set
and a `temp` set. It may seem quite doubious but I gather it's
okay. But perhaps here on `dfs2` it is better with the variants etc.


### tests
You can find tests for this task (with GNUPL or WFTPL licences)
in
[this gitlab repository](https://gitlab.com/MIMUW-wpf/testy-topol/-/tree/master/)
