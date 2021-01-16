# topol
topological sorting of a graph. written in ocaml. for my university
course actually

_For further details regarding this project as a whole see
the [notes](notes.org) file written in Polish with a more thourough description._

On this branch I've implemented the __dfs algorithm__ for topological
sorting. For further info you can
consult
[wikipedia](https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search) and
the `topol.ml` file itself. It differs from the `dfs` branch in how
the permanent/temporary mark system is implemented. Here it is an
information stored in every vertex of the graph with a variant type
consisting of variants `Perm | Temp | None` and in the `dfs` branch it
is implemented with a "_set_" – I use the `pMap` module but I store
there vertex-unit pairs as bindings and I never acces the units (why
would i) so it is just a set of vertices really. And I have two of
them – a `perm` set and a `temp` set. It may seem quite doubious but
I gather it's okay. But perhaps here on `dfs2` it is better with the
variants etc.


### tests
all tests from the `tests` folder are written by other people and i
just use them to test whether my implementation is indeed working
properly. They are all on the GNUPL or WFTPL licenses. The same
applies to the bash file `testuj.sh`.

Source: [this gitlab repository](https://gitlab.com/MIMUW-wpf/testy-topol/-/tree/master/)
