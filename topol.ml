(* +------------------- * -------------------+ *)
(* |----------------- topol -----------------| *)
(* |-----------------------------------------| *)
(* |- autor: Grzegorz Cichosz ---------------| *)
(* |- recenzentka: Katarzyna Mielnik --------| *)
(* +------------------- * -------------------+ *)



(* oznaczenia parametrów: *)
(* v - vertex, vs - vertices
 * e - edge, es - edges
 * g - graph
 * a - accumulator
 * m - mark *)

open PMap
(* graf będzie reprezentowany przez taki słownik -- kluczami są wierzchołki 
 * i są połączone z tymi wierzchołkami, z którymi mają krawędzie. *)
(* przechowuję też znacznik (reprezentowany przez typ wariantowy [mark]) 
 * mówiący, czy dany wierzchołek był: 
 *  + już odwiedzony [Perm],
 *  + czy jestem w trakcie odwiedzania jego gałęzi [Temp],
 *  + lub [Nil] -- jeszcze tam w ogóle nie byłem *)
type mark = Nil | Temp | Perm
type 'a graph = ('a, 'a list * mark) PMap.t

exception Cykliczne

(* z listy na wejściu typu [(a_1,[a_11;...;a_1n]); ...] tworzy graf *)
let make_graph ls : ('a graph) =
  let grph g (v, es) =
    let (old_es, m) =
      try find v g with Not_found -> [], Nil
    in
    add v (es @ old_es, m) g
  in
  List.fold_left grph empty ls

(* sortuję metodą dfsową. podprocedura visit oznacza [mark] każdy wierzchołek
 * by wiedzieć, czy już go nie przeszła (lub czy w tej gałęzi właśnie nie jest,
 * znacznik [Temp] -- wtedy podnoszony jest wyjątek [Cykliczne]).
 * na koniec dodaje wierzchołek do akumulatora i zwraca zmieniony graf oraz
 * tenże akumulator, który jest końcowym wynikiem sortowania topologicznego *)
let dfsort g =
  let rec visit a v g =
    let (es, m) =
      try find v g with Not_found -> [], Nil
    in
    match m with
    | Perm -> a, g
    | Temp -> raise Cykliczne
    | Nil ->
       let g = add v (es, Temp) g in
       let (a, g) =
         List.fold_left (fun (a, g) v -> visit a v g) (a, g) es
       in
       let g = add v (es, Perm) g in (v :: a, g)
  in
  foldi (fun v _ (a, g) -> visit a v g) g ([], g) |> fst

let topol ls = dfsort (make_graph ls)
