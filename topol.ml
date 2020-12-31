(* topol implementacja -- wersja z algorytmem DFS *)

(* v - vertex, vs - vertices
 * e - edge, es - edges
 * g - graph
 * s - stack 
 * a - accumulator *)

open PMap
(* graf będzie reprezentowany przez taki słownik -- kluczami są wierzchołki 
 * i są połączone z innymi wierzchołkami, z którymi mają krawędzie. *)
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
  List.fold_left grph empty ls

(* sortuję metodą dfs-ową. oznaczam każdy wierzchołek by wiedzieć, czy już go
 * nie przechodziłem i w ten sposób wychwytuję cykle itp. używam do tego pMapu, ale
 * jako de facto pSet -- mam tam słownik, gdzie klucze to wierzchołki, a wartości to
 * unity. temp - tymczasowe oznaczenia, perm - permanentne. gdy wierzchołek nie jest
 * ani w temp ani w perm, to znaczy, że jeszcze visit tam nie był *)
(* dfsort zwraca posortowany graf.
 * procedura rekurencyjna visit służy temu dfsowemu przechodzeniu po grafie *)
let dfsort g =
  let rec visit perm temp a v =
    if mem v perm then perm, a
    else if mem v temp then raise Cykliczne
    else
      let temp = add v () temp in
      let es =
        try find v g with Not_found -> []
      in
      let perm, a =
        List.fold_left (fun (perm, a) v -> visit perm temp a v) (perm, a) es
      in
      let perm = add v () perm in perm, v::a
  in
  let perm = empty in
  foldi (fun v _ (perm, a) -> visit perm empty a v) g (perm, []) |> snd

let topol ls = dfsort (make_graph ls)
