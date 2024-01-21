type 'a graph = ('a * 'a) list 
    
let g = [(1, 2); (1, 3); (2, 3); (3, 4); (4, 1)];; 
  
(******************************************************************************ESERCIZIO 1*******************************************************************************************) 
(*Definiamo funzioni di base su un grafo*) 
(*1. Definire add: (’a * ’a) -> ’a graph -> ’a graph che aggiunge un arco a un grafo*)

let add (x, y) g = (x, y) :: g;; 

(*2. Definire remove : ’a -> ’a graph -> ’a graph che toglie un nodo da un grafo – quindi toglie tutti i archi che contengono quel nodo*)

let rec remove x g = 
  match (x,g) with 
  |(x,[])-> []
  |(x,(x1,y1)::rest ) when (x=x1 || x=y1) -> remove x rest 
  |(x,(x1,y1)::rest) -> (x1,y1)::(remove x rest);; 

(*3. Definire transpose : ’a graph -> ’a graph che inverte tutti i archi di un grafo*)

let rec transpose g = 
  match g with 
  | [] -> []
  | (x,y)::rest -> (y,x)::transpose rest ;;

(*4. Definire una funzione makeUndirected : ’a graph -> ’a graph che rende tutti i archi del grafo simmetrici. Cioè se l’arco (1, 2) occorre nel grafo g allora i archi (1, 2) e (2, 1) 
  occorrono in makeUndirected g. Potete usare;
• la funzione transpose.
• una funzione setadd : ’a -> ’a list -> ’a list che aggiunge a una lista lst un elemento x se e solo se x non occorre in lst*)

let rec setadd x lst = 
  if List.mem x lst then lst 
  else x::lst;;

let rec makeUndirected g = 
  match g with 
  | []-> []
  | (x,y)::rest -> (x,y)::(y,x)::makeUndirected rest;;
  
(***********************************************************************************************************************************************************************************)
  
                                                                                                                                                                                    
                                                                                                                                                                                    

(******************************************************************************ESERCIZIO 2*******************************************************************************************)
(*Vogliamo definire una funzione che restituisce la lista dei nodi di un grafo*)
(*1. Definire nodes : ’a graph -> ’a list una funzione che restituisce la lista – senza ripetizioni – dei nodi di un grafo*)

let nodes g =
  let rec aux g acc=
    match g with 
    | [] -> acc
    | (x,y)::rest ->( if (List.mem x acc && List.mem x acc) then aux rest acc
                      else if (List.mem x acc) then aux rest (y::acc)
                      else if (List.mem y acc)then aux rest (x::acc)
                      else aux rest (x::y::acc))
  in aux g [];; 
  
(*2. Definire un funzione leftelems : (’a * ’b ) list -> ’a list che prende una lista di coppie e restituisce
la lista dei elementi a sinistra di queste coppie.
Ad esempio leftelems [(2,3) ; (1,5) ; (3,3)] restituisce [2;1;3]*)

let rec leftelems lst =
  match lst with 
  | [] -> []
  | (x,y)::rest -> x::leftelems rest;; 

(*3. Definire un funzione rightelems : (’a * ’b ) list -> ’a list che prende una lista di coppie e restituisce la lista dei elementi a destra di queste coppie.
Ad esempio rightelems [(2,3) ; (1,5) ; (3,3)] restituisce [3;5;3] *)

let rec rightelems lst =
  match lst with 
  | [] -> []
  | (x,y)::rest -> x::rightelems rest;;

(*4. Definire una funzione list_to_set : ’a list -> ’a list che prende un lista lst e restituisce lst in cui ogni elemento di lst occorre un unica volta*)

let list_to_set lst = 
  let rec aux lst acc=
    match lst with
    | []-> List.rev acc
    | x::rest -> (if (List.mem x acc) then aux rest acc 
                  else aux rest (x::acc))
  in aux lst [];;
  
(*5. Mediante le funzione leftelems,rightelems, e list_to_set definire una funzione getnodes : ’a graph -> ’a list che prende un grafo e restituisce l’insieme dei nodi del grafo*)

let getnodes g = 
  match g with 
  | [] -> []
  | (x,y)::rest ->  list_to_set (leftelems((x,y)::rest)@ (rightelems ((x,y)::rest) ) );;

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(******************************************************************************ESERCIZIO 3*******************************************************************************************)
(*Vogliamo definire diversi algoritmi di ricerca di un cammino in un grafo*)
(*1. Definire una funzione search : ’a graph -> ’a -> ’a -> ’a list che prende un grafo g due elementi start e target e restituisce un cammino sotto forma di lista di tipo ’a list 
  nel grafo dal nodo start al nodo target *)

exception Notfound;;

let rec sons g n=
  match g with
  |[]->[]
  |(x,y)::g when(x=n)->[y]@(sons g n)
  |(x,y)::g->sons g n;;

let search g start target=
  let rec aux acc connNodes=
    match connNodes with
    |[]->raise Notfound
    |x::rest when(x=target)->acc@[x]
    |x::rest->try(aux (acc@[x]) (sons g x)) with Notfound->aux acc rest
  in aux [] [start];;
                      
(*2. Modificare la funzione search per definire la funzione searchCond: int graph -> int -> int -> int -> int list che prende un grafo di interi grafo due elementi start e target e un 
  intero value e restituisce un cammino sotto la forma int list da start a value tale che la somma dei interi della lista vale value Se nessun cammino e trovato la funzione solleva un eccezione.
Si puo usare altri funzioni: 
• Una funzione accept : int list -> int -> bool che prende una lista e un interi value che restituiscetrue quando la somma dei interi vale value.
• Una funzione reject : int list -> int -> bool.
• Definire la funzione searchCond con le funzioni reject e accept mediante backtracking*)

let accept l v=
  let rec aux l v suml=match l with
      []->suml=v
    |x::l when (suml+x<(v+1))->aux l v (suml+x)
    |_->false
  in aux l v 0;;

let reject l v=
  let rec aux l v suml=match l with
      []->suml>v
    |x::l->aux l v (suml+x)
  in aux l v 0;;

let searchCond g start target v=
  let rec aux acc connNodes=
    match connNodes with
    |[]->raise Notfound
    |x::rest when(x=target && accept (acc@[x]) v)->acc@[x]
    |x::rest when(reject (acc@[x]) v)->aux acc rest
    |x::rest->try (aux (acc@[x]) (sons g x)) with Notfound->aux acc rest
  in aux [] [start];;
  
(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    
  
                                                                                                                                                                                    
(******************************************************************************ESERCIZIO 4*******************************************************************************************)
(*Usando la funzione che ricerca un cammino in un grafo, vogliamo definire una funzione che verifica se un grafo e ciclico*)
(*1. Definire una funzione search : ’a graph -> ’a -> ’a -> ’a list che prende un grafo g due elementi start e target e restituisce un cammino sotto forma di lista di tipo ’a list 
  nel grafo dal nodo start al nodo target. Se nessun cammino e trovato la funzione solleva un eccezione*)

let search g start target=
  let rec aux acc connNodes=
    match connNodes with
    |[]->raise Notfound
    |x::rest when(x=target)->acc@[x]
    |x::rest->try(aux (acc@[x]) (sons g x)) with Notfound->aux acc rest
  in aux [] [start];;

(*2. Modificare la funzione search per definire una funzione cycleat : ’a graph -> ’a -> bool che prende un grafo e un elemento start e restituisce true se trova un cammino da start.*)

let cycleat g start =
  let rec aux acc tovisit =
    match (tovisit, acc) with 
    | (x::rest, acc) when ((List.mem x acc) && (x=start) ) -> acc @ [x] 
    | ([],acc) -> raise Notfound
    | (x::rest,acc) when (List.mem x acc) -> aux acc rest
    | (x::rest, acc) -> (try aux (acc @ [x]) (sons g x) 
                         with Notfound -> aux acc rest)
  in aux [] [start];;

(*3. Usando la funzione cycleat definire una funzione cycle : ’a graph -> bool che restituisce true se e solo se il grafo contiene un ciclo *)

let cycle g start =
  let rec aux acc tovisit =
    match (tovisit, acc) with 
    | (x::rest, acc) when ((List.mem x acc) && (x=start) ) -> true 
    | ([],acc) -> raise Notfound
    | (x::rest,acc) when (List.mem x acc) -> aux acc rest
    | (x::rest, acc) -> (try aux (acc @ [x]) (sons g x) 
                         with Notfound -> aux acc rest)
  in aux [] [start];;

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(******************************************************************************ESERCIZIO 5*******************************************************************************************)
(*Vogliamo definire una funzione che verificà la connetività di un grafo. Ricordiamo che un grafo e connesso quando per ogni nodo 𝑥 e 𝑦 esiste un cammino da 𝑥 a 𝑦. N.B. La funzione
che definiremo in questo esercizio non fa una ricerca esplicita di tutti i cammini possibili usando backtracking*)
(*1. Definire una funzione sons : ’a graph -> ’a -> ’a list che prende un grafo e un elemento x e restituisce la lista dei elementi a tale che (x,a) e un arco del grafo*)

let sons g x = 
  let rec aux g x acc=
    match g with 
    | [] -> acc
    | (x1,y1)::rest when x1=x -> aux rest x (y1::acc)
    | (x1,y1)::rest -> aux rest x acc
  in aux g x [];; 
  
(*2. Usando la funzione sons, definire la funzione reach : ’a graph -> ’a -> ’a list che prende un grafo e un elemento x e restituisce la lista dei elementi che sono raggiungibili da x*) 

let rec merge lst acc= 
  match lst with 
  | []-> acc
  | x::rest when List.mem x lst -> merge rest acc
  | x::rest -> merge rest (x::acc);;

let reach g a = 
  let rec aux g a acc = 
    match acc with
    |[] -> acc
    | x::rest -> aux g a (merge (sons g x) acc)
  in aux g a (sons g a);; 

(*3. Definire la funzione nodes : ’a graph -> ’a list che prende un grafo e restituisce la lista dei suoi nodi.*)

let nodes g = 
  let rec aux g acc=
    match g with 
    | [] -> acc
    | (x,y)::rest -> (if (List.mem x acc && List.mem y acc) then aux rest acc 
                      else if (List.mem x acc) then aux rest (y::acc)
                      else if (List.mem y acc) then aux rest (x::acc)
                      else aux rest (x::y::acc))
  in aux g [];;

(*4. Un nodo 𝑥 in un grafo e conesso quando e collegato a tutti i altri nodi del grafo. Mediande le funzioni reach e nodes definire una funzione connectedNode : ’a -> ’a graph -> bool 
  che restituisce true se l’elemento in entrata e ben collegato nel grafo, altrimenti la fuzione restituisce false*)  

let connectedNode x g = 
  let tutiNodi = nodes g in
  let nodiConnessi = reach g x in 
  List.length (List.sort compare nodiConnessi) = List.length (List.sort compare tutiNodi);;
  
(*5. Mediande le funzioni connectedNode e nodes, definire una funzione connected : ’a graph -> bool che restituisce true se e solo se il grafo in entrata e connesso*)

let connected g = 
  let tuttiNodi = nodes g in 
  List.for_all (fun x -> connectedNode x g)  tuttiNodi;; 

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(******************************************************************************ESERCIZIO 6*******************************************************************************************)
(*Usiamo l’esercizio precedente. Vogliamo definire una altra funzione che verificà la connetività di un grafo*) 
(*1. Definire una funzione undirected: ’a graph -> bool che restituisce true se e solo se il grafo in entrata non e ordinato. Ricordiamo che questo significa che quando un arco (a,b)
  occorre nel grafo allora l’arco (b,a) occorre anche *)

let nodesArchi g = 
  let rec aux g acc=
    match g with
    | [] -> List.rev acc
    | (x,y)::rest -> aux rest ((x,y)::acc)
  in aux g [];;

let ris = nodesArchi g;;
  

let undirected g = 
  let nodiOrietati = nodesArchi g in 
  let aux nodiOrietati =
    match nodiOrietati with 
    | [] -> true 
    | (x,y)::rest -> (if (List.mem (y,x) nodiOrietati) then true 
                      else false )
  in aux nodiOrietati;;

(*2. Definire une funzione connect : ’a -> ’a graph -> bool che prende un grafo e un nodo a e restituisce true se e solo se il grafo non e ordinato e l’elemento a e connesso *)

let rec connect x g = 
  let nodi = nodes g in 
  let notundirected = not(undirected g) in 
  if (List.mem x nodi) && notundirected then true 
  else false ;; 

(*3. Per qualsiasi grafo grafo e ogni nodo x del grafo le funzioni connect x grafo e connected grafo (esercizio precedente) restituiscono lo stesso valore.
Argomentare perché è cosi, cioè perché connect x grafo verificà la connettività del grafo*)

(*perche sono gay *)
(***********************************************************************************************************************************************************************************) 