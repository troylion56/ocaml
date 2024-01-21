
type 'a graph = ('a * 'a) list 
    
let g = [(1, 2); (1, 3); (2, 3); (3, 4); (4, 1)];;

let es = [1;2;3;4];;

    
(******************************************************************************ESERCIZIO 1*******************************************************************************************) 
(*Il collegamento ordinato di due grafi (𝑉1, 𝐸1) e (𝑉2, 𝐸2) e definito come 𝐺1∇𝐺2 = (𝑉1 ∪ 𝑉2, 𝐸1 ∪ 𝐸2 ∪ {(𝑥1, 𝑥2)|𝑥1 ∈ 𝑉1, 𝑥2 ∈ 𝑉2 }). Vogliamo implementare l’o
perazione di collegamento;*)
(*1. Definire una funzione gen_edges : ’a -> ’a list -> ’a graph che prende un elemento x e una lista lst e restituisce la lista dei archi (x,a) per ogni elemento a della lista lst
Si puo usare List.map e una funzione f:’a -> ’a -> (’a * ’a) *)

let gen_edges x lst = 
  let rec aux lst acc =
    match lst with
    | [] -> acc 
    | y::rest ->aux rest ((x,y)::acc)
  in aux lst [];;

(*2. Definire una funzione joinSingle : ’a -> ’a graph -> ’a graph che prende un elemento x e un grafo grafo e restituisce grafo in cui sono stati aggiunti archi della forma (x,a) 
  per ogni nodo a del grafo. Si puo usare la funzione nodes ’a graph -> ’a list e una funzione ausiliaria*)

let rec list_to_set lst acc = 
  match lst with 
  |[] -> acc
  | x::rest when (List.mem x acc) -> list_to_set rest acc 
  | x::rest -> list_to_set rest (acc@[x]);; 

let rec sons x g = 
  match g with 
  |(a,b)::rest when (a=x) -> b ::(sons x rest)
  |(a,b)::rest -> sons x rest
  |[]-> [];;

let nodes g = list_to_set ((List.map (function (a,b)->a)g) @  (List.map (function (a,b)->b)g)) [];;

let joinSingle x g = list_to_set (g @ (gen_edges x (nodes g)));;
  
(*3. Definire una funzione flatten : ’a list list -> ’a list che prende una lista di liste [l1;. . . ;ln] e restituisce la lista l1 @ . . . @ ln*)

let rec flatten = function 
  | []->[]
  |x::rest -> x @ (flatten rest);;

(*4. Definire una versione generalizzata di gen_edges. Definire generate: ’a list -> ’a list -> ’a graph che prende una lista [x1;. . . ;xn] e una lista [a1;. . . ;ak] e restituisce 
la lista dei archi della forma (xi,aj) *)

let gen_edges_2 la lb= 
  List.map (function x -> gen_edges x lb) la;;

(*5. Definire una funzione joinOrd : ’a graph -> ’a graph -> ’a graph che prende due grafi g1 e g2 e restituisce il collegamento ordinato di g1 con g2*)

(*mi esce diverso perche ho fatto diverso gen_edges dal prof è giusto vedere gen_edges dal prof*)
let joinOrd ga gb =list_to_set (ga @ gb @ (gen_edges (nodes ga) (nodes gb)))



































