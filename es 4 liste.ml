
(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*Definire la funzione split di tipo (’a *’b) list -> (’a list*’b list) che associa ad ogni lista [(𝑎1,𝑏1);...; (an,bn)] la coppia di liste ([𝑎1;...; 𝑎𝑛 ], [𝑏1; ...;bn]) *)

let rec split lst =
  match lst with
  | [] -> ([], [])
  | (a, b) :: rest -> (let (lst_a, lst_b) = split rest in
                       (a :: lst_a, b :: lst_b));;

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    

                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 2*******************************************************************************************) 
(*Definire la funzione lgt che calcola la lunghezza di una lista. Definire la funzione merge che data due liste ([𝑎1;...;an], [b1;...;bk]) restituisce la lista [(a1,b1);...; (an,bn)]
quando 𝑛 = 𝑘. Se invece 𝑛 ≠ 𝑘 la funzione sollevera un eccezione *)

let rec lgt lst = 
  match lst with 
  | [] -> 0
  | x::rest -> 1+lgt rest;;

let rec merge lst1 lst2 =
  if lgt lst1 = lgt lst2 then 
    match lst1,lst2 with 
    | [],[] -> []
    | x::res1,y::res2 -> (x,y)::(merge res1 res2)
  else raise Not_found;;

(***********************************************************************************************************************************************************************************)
  

                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 3*******************************************************************************************) 
(*Definire la funzione invert di tipo ′a list →′ a list che inverta una lista. Definire la funzione concat che prende due liste [𝑎1;..;an] e [𝑏1;..;bk] e restituisce [𝑎1;..;an;b1;..; bk *) 

let invert lst =
  let rec aux lst acc=
    match lst with 
    |[] -> acc
    |x::rest -> aux rest (x::acc)
  in aux lst [];;

let concat lst1 lst2 = 
  lst1@lst2;;

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    


                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 4*******************************************************************************************) 
(*Una lista di tipo ’a * ’b list puo essere vista come una funzione parziale dal tipo ’a verso il tipo ’b. Definire la funzione comp : (’a * ’b) list * (’b * ’c) list -> (’a * ’c) list 
  che corrisponde alla composizione delle funzioni parziale. La funzione sollevera un eccezione se il dominio della prima e il codominio della secondo lista non coincidono *)

exception MissMatch;; 
let rec elemComp = function 
  |((a,b),[]) -> raise MissMatch
  |((a,b),(x,y)::l) -> (if (b=x) then ((a,y))
                        else (elemComp((a,b),l)));; 

let rec comp =function 
  |([],[]) -> []
  |([],l) -> []
  |(x::l,l1)-> elemComp(x,l1)::comp(l,l1);;

(***********************************************************************************************************************************************************************************)

  


(*****************************************************************************ESERCIZIO 5*******************************************************************************************) 
(*Una sottolista–prefisso di una lista [a1;...;an] a una lista della forma [a1;...;ak] dove 𝑘 ≤ 𝑛. Unasottolista di una lista [a1;...;an] a una lista della forma [ai;...;ak] 
dove 1 ≤ 𝑖, 𝑘 ≤ 𝑛 *)
(*1. Definire una funzione prefix che data una lista 𝑙 restituisce la lista di tutte le sottoliste prefisso di 𝑙.*)

let rec prefix lst =
  match lst with
  | [] -> [[]]
  | x :: rest -> (let rest_prefixes = prefix rest in
                  List.fold_right (fun sublist acc -> (x :: sublist) :: sublist :: acc) rest_prefixes []);; 

(*2. Definire una funzione sublist che data una lista 𝑙 restituisce la lista di tutte le sue sottoliste.*)

let rec sublist lst =
  match lst with
  | [] -> [[]]
  | x :: rest -> (let rest_sublists = sublist rest in
                  rest_sublists @ List.map (fun sublist -> x :: sublist) rest_sublists);;

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 6*******************************************************************************************) 
(*Una lista corrisponde a una sequenza finita (𝑎1, . . . , 𝑎𝑛 ) di elementi di un insieme 𝐴. Formalmente, list(𝐴) = Ð𝑛∈N {(𝑎1, . . . , 𝑎𝑛 ) | 𝑎𝑖 ∈ 𝐴}. L
  a funzione contains e definita nel modo seguente: contains : 𝐿𝑖𝑠𝑡 (𝐴) × 𝐴 → {𝑡𝑟𝑢𝑒, 𝑓 𝑎𝑙𝑠𝑒}, ((𝑎1, . . . , 𝑎𝑛 ), 𝑎) ↦ →(𝑡𝑟𝑢𝑒 if there exists 𝑎𝑖 = 𝑎.
𝑓 𝑎𝑙𝑠𝑒 otherwise. *) 
(*1. Definire in ocaml la funzione contains per le liste di interi *)

let rec contains lst x=
  match lst with
  |[]->false
  |a::lst when(a=x)->true
  |a::lst->contains lst x;; 

(*3. Definire la funzione witness che prende una lista di interi 𝑙 e un intero 𝑛 e restituisce una coppia di bool × int; (true,i) se 𝑙 contiene 𝑛 dove 𝑖 e la posizione nella lista 
di 𝑛. (false,0) se 𝑙 non contiene 𝑛. *)

let witness lst n = 
  let rec aux lst n indice = 
    match lst with 
    | [] -> (false,0)
    | x::rest when (x=n) -> (true,indice)
    | x::rest -> aux rest n (indice+1)
  in aux lst n 1;;

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    


(*****************************************************************************ESERCIZIO 6*******************************************************************************************) 
(*Un occorrenza in una lista (𝑎1, . . . , 𝑎𝑛 ) di un elemento 𝑎 e un intero 𝑖 tale che 𝑎𝑖 = 𝑎*)
(*1. Definire la funzione occurence che prende una lista di interi 𝑙 e un intero 𝑛 e restituisce una lista di interi che corrisponde a le occorrenze di 𝑛 in 𝑙.*)  

let occurence l n = 
  let rec aux l n indice acc= 
    match l with 
    | [] -> List.rev acc
    | x::rest when (x=n) -> aux rest n (indice+1) (indice::acc)
    | x::rest -> aux rest n (indice+1) (acc)
  in aux l n 1 [];;

(*2. Definire la funzione remove che prende una lista di interi 𝑙 e un intero 𝑛 e toglie tutte le occorrenze 𝑛 della lista𝑙*) 

let remove l n =
  let rec aux l n acc =
    match l with
    | [] -> List.rev acc
    | x :: rest when x = n -> aux rest n acc
    | x :: rest -> aux rest n (x :: acc)
  in aux l n [];;

(*3. Definire la funzione contract che prende una lista di interi 𝑙 e un intero 𝑛 e toglie tutte le occorrenze di 𝑛 meno la prima occorrenza di 𝑛 della lista 𝑙. *)

let contract l n =
  let rec aux l n found acc =
    match l with
    | [] -> List.rev acc
    | x :: rest when x = n && not found -> aux rest n true (x :: acc)
    | x :: rest when x = n -> aux rest n found acc
    | x :: rest -> aux rest n found (x :: acc)
  in aux l n false [] ;; 

(***********************************************************************************************************************************************************************************) 
