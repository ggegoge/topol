open Topol

let verbose = (Array.length Sys.argv) >= 2 && Sys.argv.(1) = "-v"
let max_n = if (Array.length Sys.argv) = 3 then int_of_string (Sys.argv.(2))
            else 13

(* grafy typu liczby od 1 do 5, kolejne warianty z powtórkami
 * różnych krawędzi *)
let ex_g1 = [(1, [2; 3; 4]); (2, [3; 4]); (3, [4; 5]); (4, [5])];;
assert (topol ex_g1 = [1; 2; 3; 4; 5])

let ex_g2 =
  [(1, [2; 3; 4]); (2, [3; 4]); (3, [4; 5]); (4, [5]); (4, [5])];;

assert (topol ex_g2 = [1; 2; 3; 4; 5])
let ex_g3 =
  [
    (1, [2; 3; 4]);
    (2, [3; 4]);
    (2, [3; 4]);
    (3, [4; 5]);
    (4, [5]);
    (4, [5]);
    (1, [2; 3; 4]);
  ];;

assert (topol ex_g3 = [1; 2; 3; 4; 5])

let ex_g4 =
  [
    (1, [3; 4]);
    (2, [3; 4]);
    (2, [3; 4]);
    (3, [4; 5]);
    (4, [5]);
    (4, [5]);
    (1, [2; 3; 4]);
  ];;

assert (topol ex_g4 = [1; 2; 3; 4; 5])

let ex_g5 =
  [
    (1, [3; 4]);
    (2, [3; 4]);
    (2, [3; 4]);
    (3, [4; 5]);
    (4, [5]);
    (4, [5]);
    (1, [2; 3; 4]);
    (5, [1]);
  ];;

(* cykl *)
assert ((try topol ex_g5 with Cykliczne -> []) = [])

let ex_g6 =
  [
    (1, [3; 4]);
    (2, [3; 4]);
    (2, [3; 4]);
    (3, [4; 5]);
    (4, [5]);
    (4, [5]);
    (1, [2; 3; 4]);
    (5, [5]);
  ];;

(* samocykl *)
assert ((try topol ex_g6 with Cykliczne -> []) = [])

;;
print_endline "all correctness tests passed";;

print_endline "efficiency test time";;

(* idea: a random list of 'a-s is generated first and then each gets
element gets hashed with its index so that we can be sure that in the 
results from topol everything is correct *)


let () = Random.init 2137;;

let rnd_int n () = Random.int (n / 2);;

(* let rnd_char n () = Char.chr (97 + Random.int 26);; *)

let rnd_str n () =
  let gen_passwd length =
    let gen () =
      match Random.int (26 + 26 + 10) with
      | n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26
    in
    let gen _ = String.make 1 (char_of_int (gen ())) in
    String.concat "" (Array.to_list (Array.init length gen))
  in
  gen_passwd (Random.int n + 1)


(* losowo buduje graf z ciaglej listy *)
let rec graph seq =
  let c1 = Random.bool () in
  match seq with
  | [] -> []
  | [ x ] -> [ (x, []) ]
  | [ a; b ] -> [ (a, [ b ]) ]
  | a :: b :: c :: t ->
      let next = if c1 then [ (a, [ b; c ]) ] else [ (a, [ b ]); (b, [ c ]) ] in
      next @ graph (c :: t)

let gen_seq n rnd =
  let usd = Hashtbl.create n in
  let rec aux i =
    let new_elt = ref (rnd ()) in
    while Hashtbl.mem usd !new_elt do
      new_elt := rnd ()
    done;
    Hashtbl.add usd !new_elt ();
    if i = n then [] else !new_elt :: aux (i+1)
  in aux 0;;

(* test skopiowany z mgienieczko xd *)
exception WA;;
let test n g res =
  let dict = Hashtbl.create n in
  List.iteri (fun i x -> Hashtbl.add dict x i) res;
  let check_one (v, l) =
    List.iter (fun u ->
        if (Hashtbl.find dict v) > (Hashtbl.find dict u)
        then raise WA;) l
  in
  try (List.iter check_one g; true)
  with WA -> false


let inttopol n =
  let rnd = rnd_int (3 * n) in
  let seq = gen_seq n rnd in
  let g = graph seq in
  let start = Sys.time () in  
  let topolised = topol g in
  let czas = Sys.time () -. start in
  let f = float_of_int n in
  let t' = czas *. 100000. /. f (* (f *. log f) *) in
  if verbose then
    Printf.printf "int test: size n=%d, t=%2.4f, t'=%f\n" n czas t';
  test n g topolised

let strtopol n =
  let rnd = rnd_str (3 * n) in
  let seq = gen_seq n rnd in
  let g = graph seq in
  let start = Sys.time () in  
  let topolised = topol g in
  let czas = Sys.time () -. start in
  let f = float_of_int n in
  let t' = czas *. 100000. /. f (* (f *. log f) *) in
  if verbose then
    Printf.printf "string test: size n=%d, t=%2.4f, t'=%f\n" n czas t';
  test n g topolised

;;
let _  = inttopol 10000 in ()
;;
let _ = strtopol 1000 in ()
;;
let powers a n =
  let rec aux curr k =
    if k = 0 then [] else curr :: aux (curr * a) (k-1)
  in aux 1 n;;

let () = List.iter (fun p -> (fun _ -> ()) (strtopol p)) (powers 2 max_n)
