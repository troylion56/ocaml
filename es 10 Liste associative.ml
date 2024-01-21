
(*Ricordiamo che una lista associativa e una lista di tipo di tipo (’a*’b) list. Ad esempio [(2,true) ; (1;false) ; (2;false)] e [(false, [13;2]) ; (true, []) , (true, [3;1;0])] 
  sono liste associative respettivamente di tipo (int*bool) list e (bool * (int list)) list. Una lista associativa puo essere pensata come un dizionario in C# o Java. Dato una 
  lista associativa lst: (’a *’b) list i elementi di tipo ’a (cioè quelli a sinistra) sono chiamati chiave della lista lst invece i elementi di tipo ’b (cioè quelli a destra) 
sono chiamati valori*)

let listaAss = [(1, 2); (1, 3); (2, 3); (3, 4); (4, 1)];;

(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*Dato una lista associativa vogliamo ottenere la sua lista di chiave e la sua lista di valori*)
(*1. Senzare usare List.split, definire la funzione chiave : (’a * ’b) list -> ’a list che prende una lista associativa e restituisce la lista delle sue chiave*)

let chiave lst = 
  let rec aux lst acc= 
    match lst with 
    | [] -> List.rev acc
    | (x,y)::rest -> aux rest (x::acc)
  in aux lst [];;
    
(*2. Senzare usare List.split, definire la funzione valori : (’a * ’b) list -> ’b list che prende una lista associativa e restituisce la lista dei suoi valori*)

let valori lst = 
  let rec aux lst acc= 
    match lst with 
    |[]-> List.rev acc
    | (x,y)::rest -> aux rest (y::acc)
  in aux lst [];;

let ri = valori listaAss;;

let chiave lst =
  let keys, _ = List.split lst 
  in keys ;;

let valori lst =
  let _, values = List.split lst 
  in values;;

let chiaveValori lst = 
  List.split (lst);; 

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 2*******************************************************************************************)
(*Definire le funzioni del modulo List segente*)
(*1. List.assoc : ’a -> (’a * ’b list) -> ’b che prende un element x e una lista associativa lst e restituisce – se esiste – il primo elemento della forma (x,y) che occore in lst.*)

let rec assoc x lst = 
  match lst with  
  | [] -> raise Not_found 
  | (z,y)::rest -> (if (z=x) then (z,y)
                    else assoc x rest);;
                                 
(*2. List.mem_assoc : ’a -> (’a * ’b list) -> bool. che prende un element x e una lista associativa lst e restituisce true se e solo se se un elemento della forma (x,y) occore in lst *)

let rec mem_assoc x lst = 
  match  lst with 
  | [] -> false 
  | (z,y)::rest -> (if z=x then true
                    else mem_assoc x rest);;

(*3. List.remove_assoc : ’a -> (’a * ’b list) -> bool. che prende un element x e una lista associativa lst e restituisce lst da cui e stato tolto la prima occorenza di un elemento di 
chiave x, cioè della forma (x,y)*) 

let remove_assoc x lst =
  let rec aux acc lst =
    match lst with
    | [] -> List.rev acc
    | (k, v) :: rest -> (if k = x then aux acc rest
                         else aux ((k, v) :: acc) rest)
  in aux [] lst ;;

(***********************************************************************************************************************************************************************************)


                                                                                                                                                                                    
  
(*****************************************************************************ESERCIZIO 3*******************************************************************************************) 
(*Dato una lista associativa vogliamo trovare dato un valore una chiave associata a quel valore*)
(*1. Definire la funzione findkey: ’b -> (’a * ’b list) -> ’a che prende un elemento x di tipo ’b e una lista lst e restituisce la prima occorenza della forma (y,x) in lst*)

let rec findkey x lst = 
  match lst with
  | [] -> raise Not_found
  | (y,z)::rest -> (if x=y then (y,z) 
                    else findkey x rest) ;;

(*2. Definire swap : (’a * ’b) -> (’b * ’a) che prende una coppia (x,y) e restituisce (y,x)*)

let swap = function (x,y) -> (y,x);;

(*3. Mediante swap e List.map definire una funzione swapList: (’a * ’b list) -> (’b * ’a) list che prende una lista associative e inverta le chiave e i valori *)

let swapList lst = 
  List.map (fun (x,y) -> (y,x)) lst ;;

(*4. Definire swapList senza usare List.map*)

let swapList lst = 
  match lst with 
  | [] -> [] 
  | (x,y)::rest -> (y,x)::swapList rest;;

(*5. Definire findkey mediante swapList e List.assoc*)

let findkey2 x lst =
  match List.assoc x (swapList lst) with
  | exception Not_found -> raise Not_found
  | (key, value) -> key ;;

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    

(*****************************************************************************ESERCIZIO 4*******************************************************************************************) 
(*Una lista lst : ’a list soddisfa una coppia (a,n) fatta di un elemento a : ’a e un intero n : int se e solo se l’elemento a occore esatamente n volte in lst *) 
(*• Definire una funzione soddisfa: (’a * int) -> ’a list -> bool che prende una lista lst un coppia (a,n) : ’a * int e restituisce true se l’elemento a occore esatamente n 
volte in lst e false altrimenti*)

let rec soddisfa (a,n) lst =
  if n= 0 then true 
  else 
    match lst with 
    |[] -> false 
    |x::rest -> (if (a=x) then soddisfa (a, n-1) rest 
                 else soddisfa (a, n) rest ) ;;

  

                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    