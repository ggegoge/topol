(* topol implementacja -- wersja z algorytmem Kahna *)

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
type 'a graph = ('a, 'a list * int) PMap.t

exception Cykliczne

(* dla listy wierzchołków każdemu zwiększa licznik wchodzących krawędzi *)
let in_edges g vs =
  let incr g v =
    let es, c =
      try find v g with Not_found -> [], 0
    in
    add v (es, c+1) g
  in
  List.fold_left incr g vs

(* z listy na wejściu typu [(a_1,[a_11;...;a_1n]); ...] tworzy graf *)
let make_graph ls : ('a graph) =
  let grph g (v, es) =
    let g = in_edges g es in
    let old_es, c =
      try find v g with Not_found -> [], 0
    in
    (add v (es @ old_es, c) g)
  in
  List.fold_left grph (create compare) ls

(* znajduje na początku te wierzchołki, w które nie wchodzi żadna krawędź *)
let get_zeros g =
  foldi (fun v (_, c) a -> if c = 0 then v::a else a) g []

(* decr usuwa krawędzie i wyłapuje wierzchołki, do których nic nie wchodzi
 * rmadd usuwa wierzchołki z grafu i dodaje do wyniku
 * sort zwraca posortowany topologicznie graf (w zlej kolejnosci) *)
let rec sort g s a =
  let decr (g, ns) v =
    let es, c = find v g in
    let g = add v (es, c-1) g in
    if c-1 = 0 then (g, v::ns) else (g, ns)
  in
  let rmadd (g, ns, a) v =
    let es, _ = find v g in
    let g, ns = List.fold_left decr (g, ns) es in
    (remove v g, ns, v::a)    
  in
  match s with
  | [] -> if is_empty g then a else raise Cykliczne
  | _ ->
     let g, s, a = List.fold_left rmadd (g, [], a) s in
     sort g s a

let topol ls =
  let g = make_graph ls in
  let s = get_zeros g in
  sort g s [] |> List.rev
