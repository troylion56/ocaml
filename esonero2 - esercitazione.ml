
exception NotFound;;
(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*Vogliamo implementare una funzione che aggiunge dei figli a un nodo di un albero n-ario*)
(*1. Definire il tipo ’a ntree degli alberi n-ari*)

type direction = Left | Right
type 'a ntree = Tr of 'a *('a ntree list);;
let t = Tr ( 2 , [Tr (3 , [Tr(1,[])]) ; Tr(3,[]) ; Tr (1 , [Tr(1,[]) ; Tr(2,[])] )])  ;;

(*2. Definire una funzione apply: (’a -> ’b) -> ’a ntree -> ’b ntree che prende una funzione f e la applica a tutti i nodi di un albero t*) 

let rec apply f = function
  |Tr(a,[]) -> Tr(f(a),[]) 
  |Tr(a,tl) -> Tr(f(a), List.map (apply f) tl);;

(*3. Definire una funzione applysubtree: (’a ntree-> ’a ntree) -> ’a ntree -> ’a ntree che prende una funzione f:’a ntree -> ’a ntree e un albero n-ario t e applica f a tutti i 
  sottoalberi di t. Ad esempio apply (function Tr(a,tl) -> Tr(a , Tr(1,[])::tl) ) Tr(1,[Tr(2,[]) ; Tr(5,[Tr(1,[])])]) restituisce Tr(1,[Tr(1,[]); Tr(2,[Tr(1,[])]) ; Tr(5,[Tr(1,[]) ; 
  Tr(1,[Tr(1,[])])])]), cioè corrisponde all’albero in entrata in cui è stato aggiunto il figlio Tr(1,[]) a tutti i suoi sotto–alberi *)

let rec applysubtree f = function
    Tr(a,tl) -> f( Tr(a, List.map (applysubtree f) tl));;

(*4. Definire un funzione addsonsat : ’a -> ’a ntree -> ’a ntree -> ’a ntree che prende un elemento x un albero t0 e un albero t e aggiunge a tutti i sotto–alberi di t che hanno 
  come radice x l’albero t0 come nuovo figlio. Ad esempio addsonsat 5 t0 Tr(1,[Tr(2,[]) ; Tr(5,[Tr(1,[])])]) restituisce Tr(1,[Tr(2,[]) ; Tr(5,[t0 ; Tr(1,[])])])*)

let addsoncnd x t0 t = 
  match t with
  |Tr(a,tl) when (x=a) -> Tr(a, t0::tl) 
  |Tr(a,tl) -> Tr(a,tl);;

let addsonsat x t0 t = applysubtree (addsoncnd x t0) t;;

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    
  
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 2*******************************************************************************************)
(*Vogliamo implementare diverse funzione di ricerca di un cammino in un albero binario*)
(*1. Definire il tipo ’a btree degli alberi binari.*)

type 'a btree = Empty | Tr of 'a * 'a btree * 'a btree;;
let bt = Tr(1, Tr(2, Tr(4,Empty,Empty), Empty), Tr(3, Tr(5, Tr(6,Empty,Empty), Tr(7,Empty,Empty)),Empty)) ;;
(*2. Definire una funzione search: ’a -> ’a btree -> ’a list che prende un elemento x e un albero binario t e restituisce un cammino dalla radice di t a x sotto forma di ’a list.
Definando let leaf a = Tr(a,Empty, Empty) Ad esempio search 3 Tr(2, leaf 1 , Tr(5,leaf 3 ,Empty)) restituisce [2;5;3]*)

let search x t = 
  let rec aux x t acc = 
    match t with 
    | Tr(v,l,r) when (x=v) -> List.rev (v::acc)
    | Tr(v,Empty,Empty) -> raise NotFound
    | Empty -> raise NotFound
    | Tr(v,l,r) -> (try aux x l (v::acc) 
                    with NotFound -> aux x r (v::acc))
  in aux x t [];; 

(*3. Modificare la funzione precedente per definire una funzione searchcnd: int -> int -> int btree -> int list che prende un elemento x un intero n e un albero binario t e 
  restituisce un cammino dalla radice di t a x sotto forma di int list tale che la somma degli elementi del cammino valga n*)

let rec sumlist = function 
  |[] -> 0 
  | x::l -> x + (sumlist l);;

let reject n = function 
  |[] -> false 
  | l -> (sumlist l > n);; 

let searchcnd target n t =
  let rec aux target n t acc = 
    match (t,acc) with
    |(Tr(a,l,r),acc) when (a=target && (sumlist (acc@[a]) = n ) )-> acc@[a] 
    |(Empty,acc) -> raise NotFound 
    |(t,acc) when (reject n acc) -> raise NotFound 
    |(Tr(a,l,r),acc) -> try aux target n l (acc@[a]) with NotFound -> aux target n r (acc@[a])
  in aux target n t [];; 

(*4. Assumiamo di aver dichiarato type direction = Left | Right. Modificare la funzione precedente per definire una funzione searchdir: int -> int -> int btree -> direction list 
  che prende un elemento x un intero n e un albero binario t e restituisce un cammino dalla radice di t a x sotto forma di direction list tale che la somma degli elementi del 
  cammino valga n.
Ad esempio searchdir 3 12 Tr(2, leaf 1 , Tr(5, leaf 3 ,Tr(2,leaf 3, Empty))) restituisce [Right;Right;Left] *)

let searchdir target n t =
  let rec aux target n t acc = 
    match (t,acc) with
    |(Tr(a,l,r),acc) when (a=target && (n-a)=0 )-> acc 
    |(Empty,acc) -> raise NotFound 
    |(t,acc) when (n < 0) -> raise NotFound 
    |(Tr(a,l,r),acc) -> try aux target (n-a) l (acc@[Left]) with NotFound -> aux target (n-a) r (acc@[Right])
  in aux target n t [];;

(***********************************************************************************************************************************************************************************)




(*****************************************************************************ESERCIZIO 3*******************************************************************************************)
(*Denotiamo K3 un grafo con 3 nodi tutti connessi l’un l’altro. Ad esempio se i nodi sono {1, 2, 3} allore l’insieme degli archi è {(1, 2), (2, 1), (1, 3), (3, 1), (2, 3), (3, 2)}. 
  Vogliamo definire una funzione che verifica se un grafo corrisponde a un grafo K3 *)
(*1. Definire il tipo dei grafi ’a graph*)

type 'a graph = ('a * 'a) list;; 
let k3 = [(1,2); (2,1); (1,3); (3,1); (2,3); (3,2)];;

(*2. Definire una funzione nodes : ’a graph -> ’a list che prende un grafo e restituisce la lista dei suoi nodi*)

let rec list_to_set lst acc = match lst with
    [] -> acc |
    x::l when (List.mem x acc) -> list_to_set l acc |
    x::l -> list_to_set l (acc@[x]) ;;

let nodes g =
  list_to_set ((List.map (function (a,b)-> a) g) @ (List.map (function (a,b)-> b) g) );;

(*3. Definire un funzione nodeseq3 : ’a graph -> bool che verificà che un grafo contiene esattamente 3 nodi*)

let rec length = function 
  |[] -> 0 
  | x::l -> 1 + (length l);;

let nodeseq3 g = (length (nodes g)) = 3;;

(*4. Definire una funzione noloop : ’a graph -> bool che verificà che un grafo non contiene archi della forma (𝑥, 𝑥)*)

let rec noloop = function
  |[] -> false 
  |(a,b)::l when (a=b) -> true 
  |(a,b)::l -> noloop l;;

(***********************************************************************************************************************************************************************************)

                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(******************************************************************************ESERCIZIO 4*******************************************************************************************)
(*La lunghezza di un cammino in un grafo 𝐺 è il numero di archi che attraversa. Dato un grafo 𝐺 vogliamo definire il suo grafo quadrato 𝐺2. 𝐺2 ha gli stessi nodi di 𝐺 e due nodi 
  𝑥 e 𝑦 di 𝐺2 sono collegati con un arco se e solo se esiste un cammino in 𝐺 da 𝑥 a 𝑦 di lunghezza minore o uguale a 2 *)
  
let t2 = [(1,2); (2,3); (4,2)];;

(*1. Definire una funzione sons:’a -> ’a graph -> ’a list che prende un nodo x e restituisce la lista dei suoi figli cioè i nodi y tali che esiste un arco (x,y)*)
  
let rec sons x g=
  match g with 
  |[]->[]
  |(x1, y1)::g when(x=x1)->[y1]@(sons x g)
  |(x1, y1)::g->sons x g;; 

(*2. Definire una funzione list_to_set : ’a list -> ’a list che toglie le ripetizioni da una lista*)

let list_to_set lst = 
  let rec aux lst acc=
    match lst with 
    |[] -> []
    |x::rest -> (if List.mem x acc then aux rest acc
                 else aux rest (x::acc))
  in aux lst [];; 

(*3. Definire una funzione sons2 : ’a -> ’a graph -> ’a list che prende un elemento x e un grafo g e restituisce la lista dei figli dei figli di x*)

let rec sons2 x g = 
  match g with 
  | [] -> []
  | (x1,y1)::rest when (x=x1) -> (sons y1 g) @ (sons2 x g)
  | (x1,y1)::rest -> sons2 x g;;


(*4. Definire una funzione newedge : ’a -> ’a graph -> ’a graph che prende un elemento x e un grafo g e restituisce una lista di archi della forma (x,y) quando y appartiene 
a sons2 x g *)

let newedge x g = 
  let rec aux x lst = 
    match lst with 
    | [] -> [] 
    | y::rest -> (x,y)::aux x lst 
  in aux x (sons2 x g) ;;

(*5. Definire una funzione nodes: ’a graph -> ’a list che restituisce la lista dei nodi di un grafo*)

let nodes g =
  let rec aux g acc = 
    match g with 
    | [] -> List.rev acc
    | (x,y)::rest -> (x,y)::acc
  in aux g [] ;;

(*6. Usando le funzioni precedenti definire una funzione square : ’a graph -> ’a graph che prende un grafo g e restituisce il grafo quadrato di g*)

let square g=
  let rec aux lst=match lst with
      []->[] 
    |x::lst->(auxedge (sons2 x g) x)@aux lst
  and
    auxedge lst x=match lst with 
      []->[]
    |y::lst->[(x,y)]@auxedge lst x 
  in (aux (list_to_set (nodes g)));;

let ris = square t2;;




















