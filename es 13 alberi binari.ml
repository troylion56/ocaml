type 'a btree = Empty | Tr of 'a * 'a btree * 'a btree;;
type direction = Left | Right;;

let tree = Tr(1, Tr(2, Tr(4,Empty,Empty), Empty), Tr(3, Tr(5, Tr(6,Empty,Empty), Tr(7,Empty,Empty)),Empty)) ;; 
let lista_di_liste = [[Right;Left]];;
let listaDirezione = [Right];;

(*    1
     / \
    2   3
    |    |
    4    5
         /\ 
         6 7*) 
(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(* Vogliamo definire diverse funzione di base su i alberi binari.*)
(*1. Definire size : ’a btree -> int una funzione che restituisce la quantità di elementi che contiene un albero binario*)

let rec size =function 
  | Empty -> 0 
  | Tr(v,l,r) -> 1+ size l + size r;;

(*2. Ricordiamo che una foglia e un albero della forma Tr(a , Empty, Empty). Definire isLeaf : ’a btree -> bool una funzione che prende un albero binario e 
  restituisce true se l’albero in entrata e una foglia, altrimenti la funzione restituisce false. Dare due versioni della funzione
• Una senza usare size.
• Una che usa la funzione size*)

(*versione senza size*)
let isLeaf = function 
  |Tr(v,Empty,Empty) -> true 
  |Tr(v,l,r) -> false 
  | Empty -> false ;;

(*versione con size*)
let isLeaf2 t = 
  if size t = 1 then true 
  else false ;; 

(*3. Definire leaves : ’a btree -> ’a btree list una funzione che prende un albero binario e restituisce la lista delle sue foglie *)

let rec leaves t = 
  match t with 
  | Empty -> [] 
  | Tr(v,Empty,Empty) -> [v]
  | Tr(v,l,r) -> leaves l @ leaves r;; 

(*4. Definire una funzione mirror che prende un albero binario t e inverte i figli destro e sinistro di ogni sotto albero di t*)

let rec mirror t = 
  match t with 
  | Empty -> t 
  | Tr(v,l,r) -> Tr(v,mirror r, mirror l);; 
  
(***********************************************************************************************************************************************************************************)




(*****************************************************************************ESERCIZIO 2*******************************************************************************************)
(*Dato un albero binario vogliamo fare diverse cose;*)
(*1. Definire la funzione delete: ’a btree -> ’a -> ’a btree che prende un albero e un elemento 𝑥 e sostituisce tutti i sotto alberi di radice 𝑥 trovati con l’albero vuoto Empty *)

let rec delete t valore = 
  match t with 
  |Empty -> Empty
  |Tr(v,l,r) when valore = v -> Empty 
  |Tr (v,l,r) -> Tr(v, delete l valore, delete r valore);; 

(*2. Definire deleteFirst: ’a btree -> ’a -> ’a btree un funzione definita con il backtracking che prende un albero un elemento 𝑥 e sostituisce il primo sotto albero di radice 𝑥 
trovato con l’albero vuoto Empty*) 

exception NotFound;;
let rec deleteFirst t x=
  match (t) with
  |Empty->raise NotFound
  |Tr(a,l,r) when (a=x)->Empty
  |Tr(a,l,r)->(try(Tr(a,deleteFirst l x,r)) 
               with NotFound->Tr(a,l,deleteFirst r x));;

(*3. Usando liste di elementi di tipo direction possiamo trovare dei sottoalberi senza ambiguita. Definire find: direction list -> ’a btree -> ’a che prende una lista di direzioni 
  e un albero binario e restituisce il sottoalbero che si trova in quella posizione. Se la posizione non e raggiungibile la funzione sollevera un eccezione*)

let rec direction direzione t= 
  match (direzione,t) with 
  |([],Tr(v,l,r)) -> v
  |(Left::rest, Tr(v,l,r)) -> direction rest l
  |(Right::rest, Tr(v,l,r)) -> direction rest r
  |(_,Empty) -> failwith "listaTroppoLunga";;

(*4. Pendendo spunto dalla funzione find definire deleteAtpos: direction list -> ’a btree -> ’a btree una funzione che prende una posizione e un albero binario t e cancella il 
sottoalbero di t che si trova in quella posizione.*)

let rec deleteAtpos direzione t =
  match (direzione,t) with 
  |([],Tr(v,l,r)) -> Empty
  |(Left::rest, Tr(v,l,r)) -> Tr(v, deleteAtpos rest l, r)
  |(Right::rest, Tr(v,l,r)) -> Tr(v,l,deleteAtpos rest r)
  |(_,Empty) -> failwith "listaTroppoLunga";; 

(***********************************************************************************************************************************************************************************)




(*****************************************************************************ESERCIZIO 3*******************************************************************************************)
(*Vogliamo implementare una funzione che dato un albero binario e una lista di direzione (e.g. posizione) restituisce l’elemento trovato in quella positione nel albero. Vogliamo 
  poi generalizzare questa funzione per funzione su una lista di posizioni.*)
(*1. Implementare una funzione find: direction list -> ’a btree -> ’a che prende una lista di direzioni e un albero binario e restituisce l’elemento che si trova in quella posizione*)

let rec find direzione t =
  match direzione, t with
  |[],Tr(v,l,r) -> v
  |Left::rest,Tr(v,l,r) -> find rest l
  |Right::rest,Tr(v,l,r) -> find rest r
  |(_,Empty) -> failwith "listaTroppoLunga";; 

(*2. Vogliamo generalizzare la funzione precedente; definire una funzione findList : (direction list) list -> ’a btree -> ’a che prende una lista di posizione e un albero binario e 
restituisce la lista degli elementi trovati in quelle posizione.
• Definire una versione senza usare List.map.
• Definire una versione mediante List.map *)

let rec findList posLst t=
  match posLst with 
  |[]->[]
  |pos::posLst->[(find pos t)]@(findList posLst t);;

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 4*******************************************************************************************)
(*Vogliamo definire una funzione che trova le posizione in un albero binario in cui occorono l’elemento dato x*)
(*1. Definire una funzione findFirst : ’a -> ’a btree -> direction list che dato un elemento x e un albero t restituisce l’indirizzo della prima occorenza trovata di x in t.
Se nessuna occorenza e trovata la funzione sollevera un eccezione*)

exception ElementNotfound;;
let findFirst x t = 
  let rec aux stradaCorrente nodo = 
    match nodo with 
    | Empty -> raise ElementNotfound
    | Tr (v,l,r)-> if v = x then List.rev stradaCorrente
        else try aux (Left::stradaCorrente) l 
          with ElementNotfound ->aux (Right::stradaCorrente) r
  in aux [] t;; 

(*2. Vogliamo modificare findFirst con accumulatore per definire una funzione findAll : ’a -> ’a btree -> direction list list che prende un elemento x e un albero t e restituisce 
  la lista delle posizione in cui occore x in t. findAll non deve mai sollevare un eccezione.*)

let findAll x t=
  let rec aux x t acc=
    match t  with
    |(Empty)->raise NotFound
    |(Tr(a,l,r))  when (x=a)->[acc]@(try aux x l (acc@[Left]) with NotFound->[])@(try aux x r (acc@[Right]) with NotFound->[])
    |(Tr(a,l,r))->(try aux x l (acc@[Left]) with NotFound->[])@(try aux x r (acc@[Right]) with NotFound->[])
  in aux x t [];; 

(*3. Definire substi : direction list -> ’a -> ’a btree -> ’a btree che prende una posizione dl : direction list un elemento x e un albero binario t e restituisce t in cui 
  l’elemento in posizione dl e stato sostituito con x*)

let rec substi direzione x t = 
  match (direzione,x,t) with 
  | ([],x,Tr(v,l,r)) -> Tr(x,l,r) 
  | (Left::rest,x,Tr(v,l,r)) -> Tr(v,substi rest x l,r)
  | (Right::rest,x,Tr(v,l,r)) -> Tr(v,l,substi rest x r)
  | (_,x,Empty) -> failwith "listaTroppoLunga";; 

(*4. Definire una versione generalizzata della funzione precedenete. Definire substi_list : (direction list) list -> ’a -> ’a btree -> ’a btree che prende una lista di 
  posizione plist un elemento x e un albero binario t e sostituisce tutti i elementi trovati in una posizione di plist con l’elemento x*)

let rec substi dl x t=
  match (dl,x,t)with
  |([ ],x,Tr(a,l,r))->Tr(x,l,r)
  |(Left::rest,x,Tr(a,l,r))->Tr(a,substi rest x l, r)
  |(Right::rest,x,Tr(a,l,r))->Tr(a, l, substi rest x r);;

(*5. Definire una funzione di sostituzione substitution : ’a -> ’a -> ’a btree -> ’a btree che prende un elemento a, un elemento x e un albero t e sostituisce tutte le occorenze 
  di a in t con x. Usare le funzione findAll e substi_list non li sto usando*)

let rec substitution a x alb = 
  match alb with 
  | Empty -> Empty 
  | Tr(v,l,r) when v =a -> Tr(x,substitution a x l ,substitution a x r)
  | Tr(v,l,r)-> Tr(v,substitution a x l ,substitution a x r);; 

(***********************************************************************************************************************************************************************************)




(*****************************************************************************ESERCIZIO 5*******************************************************************************************)

type espr = I of int | Plus of espr * espr | Mult of espr * espr;;

(*1. Definire una funzione translate : espr -> string btree che prende un espressione e restituisce l’albero binario corrispondente*)

let rec translate espr = 
  match espr with 
  | I a -> Tr(string_of_int a, Empty, Empty)
  | Plus (a, b) -> Tr("Plus", translate a, translate b)
  | Mult (a, b) -> Tr("Mult", translate a, translate b);;

(*2. Definire la funzione inversa, cotranslate : string btree -> espr che prende un albero di stringhe e restituisce un espressione se e possibile. Se non e possibile la funzione 
sollevera un eccezione *)

let rec cotranslate t=match (t)  with 
  |(Tr("Plus",l, r))->Plus ((cotranslate l), (cotranslate r))
  |(Tr("Mult", l, r))->Mult ((cotranslate l), (cotranslate r))
  |(Tr(a, Empty, Empty))->I (int_of_string a);;

  