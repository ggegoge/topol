open Topol;;

(* grafy typu liczby od 1 do 5, kolejne warianty z powtórkami
 * różnych krawędzi *)
let ex_g1 = [(1, [2;3;4]); (2, [3; 4]); (3, [4; 5]); (4, [5])];;
assert (topol ex_g1 = [1;2;3;4;5]);;
let ex_g2 = [(1, [2;3;4]); (2, [3; 4]); (3, [4; 5]); (4, [5]); (4, [5])];;
assert (topol ex_g2 = [1;2;3;4;5]);;
let ex_g3 = [(1, [2;3;4]); (2, [3; 4]); (2, [3; 4]); (3, [4; 5]);
             (4, [5]); (4, [5]); (1, [2;3;4])];;
assert (topol ex_g3 = [1;2;3;4;5]);;
let ex_g4 = [(1, [3;4]); (2, [3; 4]); (2, [3; 4]); (3, [4; 5]);
             (4, [5]); (4, [5]); (1, [2;3;4])];;
assert (topol ex_g4 = [1;2;3;4;5]);;
let ex_g5 = [(1, [3;4]); (2, [3; 4]); (2, [3; 4]); (3, [4; 5]);
             (4, [5]); (4, [5]); (1, [2;3;4]); (5, [1])];; (* cykl *)
assert ((try topol ex_g5 with Cykliczne -> []) = []);;
let ex_g6 = [(1, [3;4]); (2, [3; 4]); (2, [3; 4]); (3, [4; 5]);
             (4, [5]); (4, [5]); (1, [2;3;4]); (5, [5])];; (* samocykl *)
assert ((try topol ex_g6 with Cykliczne -> []) = []);;


(* let ex_g6 =
 *   [("woda", [""] *)


(* dostaje graf, stos i akumulator. wszystkie elementy stosu wrzuca
 * do akumulatora i usuwa stosowe wierzchołki oraz idące z nich krawędzie z grafu,
 * a do tego tworzy nowy stos zawierający te wierzchołki, które mają 0 wchodzących 
 * w nie krawędzi. na koniec zwraca akumulator -- posortowany topologicznie graf. 
 * rekurentne ogonowo *)
(* rmadd usuwa wierzchołek z grafu, dodaje do akumulatora. decr usuwa wychodzące 
 * zeń krawędzie i wyłapuje te wierzchołki, które po tym procesie mają licznik 
 * wchodzących krawędzi równy 0, dodaje je do stosu *)


(* PROBLEM DUZY - co jesli w inpucie powtorzy sie
jakas krawedz?... w sumie to nie ma problemu, ponieważ jeśli
nawet jakaś krawędź pojawi się dwukrotnie to dodanie jej do mapy 
sprawi, że najwyżej będzie jakiś wierch połączony dwa razy do swoich
synów, ale to nie powinno być problemem, ponieważ najwyżej licznik dla
danego wierzchołka spadnie poniżej 0, ale już go dodałem wcześniej gdy ten
był =0, więc mając go na stosie usunę go bez kłopotów i tyle żem go widział *)

