(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*Ricordiamo che per un albero binario un indirizzo un indirizzo e una lista di tipo direction list avendo definito il tipo direction come type direction = Left | Right;; 
L'obiettivo di questo esercizio è di modificare il sottoalbero di un albero binario che si trova in un indirizzo dato.*)

type direction = Left | Right;; 
    
(*1. Definire il tipo dei alberi binari 'a btree. *)
 
type 'a btree = Empty | Tr of 'a * 'a btree * 'a btree;;

let t =Tr(0,Tr(2,Tr(1,Empty,Empty), Tr(0,Empty,Empty)), Tr(5,Empty,Empty) );;
let lst = [Left;Right];;
let funzione = function Tr (a,v,l)-> Tr(a+1,v,l);;

(*2. Definire una funzione find : direction list -> 'a btree -> 'a btree che prende un indirizzo ind un albero binario t e restituisce il sotto-albero t0 di t che si trova in ind.
Se nessun sotto-albero è trovato solleverà un eccezione. Ad esempio find [] Tr(0,Empty, Tr(1,Empty,Empty)) restituisce Tr(0,Empty,Empty)) invece 
  find [Left;Right] Tr(0,Tr(2,Tr(1,Empty,Empty), Tr(0,Empty,Empty)), Tr(5,Empty,Empty) ) restituisce Tr(0,Empty,Empty)*)

exception TroppoLunga;;

let rec find lst t = 
  match lst, t with 
  | [], Tr(v, l, r) -> Tr(v, l, r)
  | Left::rest, Tr(v, l, r) -> find rest l
  | Right::rest, Tr(v, l, r) -> find rest r
  | _, Empty -> raise TroppoLunga;;


(*3 Modificando la funzione find definire una funzione apply : ('a btree -> 'a btree) -> direction list -> 'a btree -> 'a btree che prende una funzione sui alberi binari f , 
  un indirizzo direction list e un albero binario t. Attenzione l'albero non deve essere distrutto. 
  Ad esempio apply (function Tr(a,l,r) -> Tr(a+1,l,r)) [Left;Right] Tr(0,Tr(2,Tr(1,Empty,Empty), Tr(0,Empty,Empty)),Tr (5,Empty,Empty)).*)

let rec apply f lst t = 
  match lst,t with
  | [], Tr(v, l, r) -> Tr(f v, l, r) 
  | Left::rest, Tr(v, l, r) -> apply f rest l
  | Right::rest, Tr(v, l, r) -> apply f rest r
  | _, Empty -> raise TroppoLunga;; 

(*4 Usando la funzione aplly definire substri: 'a btree -> ind -> 'a btree -> 'a btree che prende un albero t0 un indirizzo ind e un albero t e sostituisce il sotto albero di t 
  in ind con t0 substi t0 [Left;Rigth] Tr(0,Tr(2,Tr(1,Empty,Empty), Tr(0,Empty,Empty)),Tr (5,Empty,Empty)). restituisce Tr(0,Tr(2,Tr(1,Empty,Empty), t0),Tr (5,Empty,Empty)).*)

let rec substri t0 ind t =
  match lst,t with 
  | [], Tr(v,l,r) -> t0
  | Left::rest, Tr(v, l, r) -> substri t0 rest l
  | Right::rest, Tr(v, l, r) -> substri t0 rest r
  | _, Empty -> raise TroppoLunga;; 

(***********************************************************************************************************************************************************************************)


  
  
(*****************************************************************************ESERCIZIO 2*******************************************************************************************) 
(*vogliamo implementare la funzione di ricerca di un cammino in un albero n-ario, poi vogliamo definire una funzione che toglie a tutti i sotto alberi di un albero t con la radice x 
  il loro primo figlio*)
(*1. Defininire il tipo 'a ntree dei alberi n-ari*)

type 'a ntree = Tr of 'a * ('a ntree list);;

let t1 =Tr(2,[Tr(3,[]); Tr(1,[Tr(5,[]) ; Tr(3,[Tr(1,[])])])]);; 
let funzione1 = function Tr(a,tl)-> Tr(0,tl);;

(*2 Definire una funzione search : 'a -> 'a ntree -> a'list che prende un elemento x e un albero n-ario t e restituisce un cammino sotto forma di 'a list dalla radice di t al primo 
nodo trovato con il nome x *)

exception Notfound

let search n t = 
  let rec aux t acc = 
    match t with 
    | Tr(a,tlist) when (a=n) -> a::acc
    | Tr(a,[]) -> raise Notfound
    | Tr (a,x::rest) -> (try aux x (a::acc) 
                         with Notfound -> aux (Tr(a,rest)) acc)
  in aux t [];;

(*3 Definire una funzione applyat: ('a ntree -> 'a ntree) -> 'a ntree -> 'a ntree che prende una funzione f sui alberi un elemnto x e un albero n-ario t e applica la funzione f a tutti
  i sotto alberi di t con radice x. Ad esempio  applyat (function Tr(a,tl)-> Tr(0,tl)) 3 Tr(2,[Tr(3,[]); Tr(1,[Tr(5,[]) ; Tr(3,[Tr(1,[])])])]) restituisce 
  Tr(2,[Tr(3,[]); Tr(1,[Tr(5,[]) ; Tr(3,[Tr(1,[])])])]) *)

let rec applyat f x t = 
  match t with 
  | Tr(a,[]) -> (if (a=x) then Tr(x,[])
                 else Tr(a,[]))
  | Tr(a,tlist) when (a=x)-> Tr(x,tlist)
  | Tr (a,y::rest) -> (try applyat f x y
                       with  Notfound -> applyat f x (Tr(a,rest)));;

(*4. Usando la funzione applyat definire una funzione removeaonsat :: 'a -> 'a ntree -> 'a ntree che prende un elemento x e un albero t e toglie il primo figlio di tutti i nodi x nell'albero t 
  Ad esmpio  removesonsat 3 Tr(2,[Tr(3,[]); Tr(1,[Tr(5,[]) ; Tr(3,[Tr(1,[])])])]) restituisce  Tr(2,[Tr(3,[]); Tr(1,[Tr(5,[]) ; Tr(3,[])])]) *)

let rec remove n t = 
  match t with 
  | Tr(a,[]) -> raise Notfound 
  | Tr(a,tlist) when (a=n) -> Tr(a,[])
  | Tr(a,y::rest) -> (try remove n y 
                      with Notfound -> remove n (Tr(a,rest)));;
                           
(***********************************************************************************************************************************************************************************)
  
  
  
  
(*****************************************************************************ESERCIZIO 3*******************************************************************************************) 
(*Un insime di nodi di un grafo è chiamato stabile quando nessun nodo è conesso a un altro. Un insime di nodi stabile è massimale se per qualsiasi nodo aggiunto l'insime non è piu
  stabile. Vogliamo implementare una funzione che completa un insieme di vertici stabile in un insime piu grande che sia ancora stabile, se è possibile*)

type 'a graph = ('a * 'a) list 
let g = [(1, 2); (1, 3); (2, 3); (3, 4); (4, 1)];; 

(*1. Definire una funzione list_to_set: 'a list -> 'a list che toglie le ripetizioni di una lista*)

let list_to_set g = 
  let rec aux g acc=
    match g with 
    | [] -> List.rev acc
    | x::rest -> (if( List.mem x acc) then aux rest acc 
                  else aux rest (x::acc))
  in aux g [];;

(*2. Definire una funzione nodes: 'a graph -> 'a list che prende un grafo e restituisce la lista dei suoi nodi *)

let nodes g = 
  let rec aux g acc=
    match g with 
    | [] -> List.rev acc
    | (x,y)::rest -> (if( List.mem x acc && List.mem x acc ) then aux rest acc 
                      else if ( List.mem x acc ) then aux rest (y::acc)
                      else if ( List.mem y acc ) then aux rest (x::acc)
                      else aux rest (x::y::acc))
  in aux g [];;

(*3. Definire una funzione stabile : 'a list -> 'a graph -> bool che prende una lista di elementi lst e un grafo g e restituisce true s ee solo se lst contine solo nodi g e 
l'insime di nodi lst e stabile nel grafo g *) 

let rec verificaTuttiLstIng lst g = 
  match lst with 
  | [] -> true 
  | x::rest -> (if  (List.mem x (nodes g)) then verificaTuttiLstIng rest g 
                else false );;

(*
  let rec un solo elemnto lst g =
    let primo = (List.hd lst) in 
    match lst with 
    | x::rest -> if 
    
      match 
    | x::rest -> 
               
        let stabile lst g = 
          let verificato= verificaTuttiLstIng lst g in *)
  
  
 

(***********************************************************************************************************************************************************************************) 



  
(*****************************************************************************ESERCIZIO 4*******************************************************************************************) 
(*Un grafo è connesso quando tutti i nodi sono collegati con dei cammini, cioè per qualsiasi coppia di nodi distinti (x,y) esiste un cammino da x a y. L'obiettivo di quest'esercizio
  è di definire una funzione che verifica la connessione di un grafo *)
(*1. Definire una funzione search : 'a -> 'a -> 'a graph -> 'a list che prende due nodi start e target e un grafo g e restituisce se esiste un cammino da start a target nel grafo g.
  Se non esiste nessun cammino la funzione solleverà un eccezione*)

let rec sons g n=
  match g with
  |[]->[]
  |(x,y)::g when(x=n)->[y]@(sons g n)
  |(x,y)::g->sons g n;; 

let search start target g = 
  let rec aux acc nodiConnessi = 
    match nodiConnessi with 
    | [] -> raise Notfound 
    | x::rest when (x=target)-> acc@[x]
    | x::rest -> (try (aux  (acc@[x]) (sons g x)) 
                  with Notfound -> aux acc rest)
  in aux [] [start];; 

(*2. Definire una funzione nodes: 'a graph -> a' list che prende un grafo e restituisce la lista dei suoi nodi*)

let nodes g = 
  let rec aux g acc=
    match g with 
    | [] -> List.rev acc
    | (x,y)::rest -> (if( List.mem x acc && List.mem x acc ) then aux rest acc 
                      else if ( List.mem x acc ) then aux rest (y::acc)
                      else if ( List.mem y acc ) then aux rest (x::acc)
                      else aux rest (x::y::acc))
  in aux g [];;

(*3. Definire una funzione remove : 'a -> a' list -> a' list che rpende ul elemento x e una lista lst e restituisce la lista lst da cui è stata tolta la prima occorrenza dix trovata*)

let remove x lst = 
  let rec aux lst acc = 
    match lst with 
    | [] -> List.rev acc
    | x::rest -> (if (List.mem x acc) then aux rest acc
                  else aux rest (x::acc))
  in aux lst [];;

(*4. Usando la funzione search e noedes definire una funzione connectednode: 'a -> 'a graph -> bool che prende un nodo x e un grafo g e restituisce true se esiste un cammino in g da x 
  a qualsiasi altro nodo y del grafo g. *)



  


        
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  




    