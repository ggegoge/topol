# topol
topological sorting of a graph. written in ocaml. for my university
course actually

_For further details regarding this project as a whole see
the [notes](notes.org) file written in Polish with a more thourough description._

within this branch I've used __Kahn's algorithm__ for topological
sorting. It may be slightly slower but on the other hand it is a
simpler concept perhaps. Or maybe I've simply couldn't manage to
properly implement this and that's why it is slower. You can ask
wikipedia for  more information
about Kahn's algorithm -- [here](https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm)


### tests
all tests from the `tests` folder are written by other people and i
just use them to test whether my implementation is indeed working
properly. They are all on the GNUPL or WFTPL licenses. The same
applies to the bash file `testuj.sh`.

Source: [this gitlab repository](https://gitlab.com/MIMUW-wpf/testy-topol/-/tree/master/)
