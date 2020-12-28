(* topol implementacja -- wersja z algorytmem DFS *)

(* v - vertex, vs - vertices
 * e - edge, es - edges
 * g - graph
 * s - stack 
 * c - count 
 * a - accumulator *)

open PMap
(* graf będzie reprezentowany przez taki słownik -- kluczami są wierzchołki 
 * i są połączone z innymi wierzchołkami, z którymi mają krawędzie. prócz tego
 * przechowywana jest informacja z ile krawędzi wchodzi w dany wierzchołek *)
type 'a graph = ('a, 'a list) PMap.t

exception Cykliczne


(* z listy na wejściu typu [(a_1,[a_11;...;a_1n]); ...] tworzy graf *)
let make_graph ls : ('a graph) =
  let grph g (v, es) =
    let old_es =
      try find v g with Not_found -> []
    in
    (add v (es @ old_es) g)
  in
  List.fold_left grph (create compare) ls

let rec dfsort g =
  let rec visit perm temp a v =
    if mem v perm then perm, a
    else if mem v temp then raise Cykliczne
    else
      let temp = add v () temp in
      let es =
        try find v g with Not_found -> []
      in
      let perm, a = List.fold_left (fun (perm, a) v -> visit perm temp a v) (perm, a) es in
      (* let temp = remove v temp in *)
      let perm = add v () perm in
      perm, v::a
  in
  let perm = create compare in
  foldi (fun v vs (perm, a) -> visit perm (create compare) a v) g (perm, [])

let topol ls =
  let g = make_graph ls in
  dfsort g |> snd
