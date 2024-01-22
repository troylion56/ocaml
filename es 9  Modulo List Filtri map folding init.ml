

(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*Vogliamo definire funzione che creano diverse liste*)
(*1. Vogliamo definire la funzione init che prende due interi i e lung e restituisce la lista [i;. . . ;i+lung].
(a) Definire init senza usare il commando List.init.
(b) Definire init mediante il commando List.init.*)

let rec init i lung =
  if lung < 0 then []
  else i :: init (i + 1) (lung - 1);; 

let init2 i lung = 
  List.init (lung+1) (fun ind -> ind+i);;

let ris = init 5 3;;
let ris2= init2 5 3;;

(*2. Vogliamo definire una funzione multiples che prende due interi n e k e restituisce i primi k multipli di n cioè la lista [0*k;. . . ;n*k].
(a) Definire multiples senza usare il commando List.init.
(b) Definire multiples mediante il commando List.init*)

let rec multiples n k =
  let rec aux k acc = 
    match k with 
    | 0 -> ((0*k)::acc)
    | _ -> aux (k-1) ((n*k)::acc)
  in aux (k-1) [];;

let multiples2 n k =
  List.init k (fun index -> n * index);; 

(***********************************************************************************************************************************************************************************)




(*****************************************************************************ESERCIZIO 2*******************************************************************************************) 
(*La funzione List.init del modulo List puo essere definita mediante la funzione List.map; *) 
(*1. Definire una funzione firstn:int -> int list che prende un intero n e restituisce la lista [0;1;. . . ;n-1]*)

let firstn n = 
  let rec aux n acc= 
    match n with
    | 0 -> 0::acc
    | _ -> aux (n-1) (n::acc)
  in aux (n-1) [];;

(*2. Mediante la funzione firstn e la funzione List.map definire una funzione init: int -> ’a list che si comporta come List.init *)

let init3 n = 
  List.map (fun x -> firstn x) (firstn n);;


(***********************************************************************************************************************************************************************************)
  
                                                                                                                                                                                    
  
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 3*******************************************************************************************) 
(*Dato un intero k la 𝑘–traslazione di una lista di interi [a1;. . . ;an] e la lista [a1+k;. . . ;an+k]*) 
(*1. Senza usare la funzione List.map definire una funzione trasla che prende un intero k e una lista lst e restituisce la k–traslazione di lst*)

let trasla k lst =
  let rec aux k lst acc = 
    match lst with 
    | [] -> List.rev acc 
    | x::rest -> aux k rest ((x*k)::acc)
  in aux k lst [];;

(*2. Definire trasla mediante List.map*)

let trasla2 k lst = 
  List.map (fun x -> x*k) lst;;

(*3. Implementare una funzione mymap che si comporta come la funzione List.map*)

let rec mymap f lst = 
  match lst with
  | [] -> []
  | x::rest -> (f x)::mymap f rest ;;

(***********************************************************************************************************************************************************************************)  




(*****************************************************************************ESERCIZIO 4*******************************************************************************************) 
(*Per calcore il massimo comune divisore di due interi 𝑛 e 𝑚 un metodo naturale puo essere di generare le liste i divisori di 𝑛 e 𝑚 e di costruire poi la loro intersezione, 
  ottenando la lista dei divisori communi di 𝑛 e 𝑚*)
(*1. Senza usare la funzione List.filter definire la funzione intersec che prende due liste la e lb e costruisce la loro intersezione cioe la lista dei elementi di la che occorono in lb*)

let intersec la lb = 
  let rec aux la acc =
    match la with
    | [] -> List.rev acc
    | x::rest -> (if (List.mem x lb) then aux rest (x::acc)
                  else aux rest acc)
  in aux la [];;

(*2. Definire intersec mediante List.filter*)

let intersec2 la lb = List.filter (fun x -> List.mem x lb) la;;
  
(*3. Senza usare List.filter, definire una funzione divisors:int -> int list che restituisce la lista dei divisori di n*)

let divisors n = 
  let rec aux d acc=
    match d with 
    | 0 -> acc
    | _ -> (if n mod d =0 then aux (d-1) (d::acc)
            else aux (d-1) (acc))
  in aux n [];;

let divisors2 n = 
  let rec aux x n acc = 
    if x= n then  List.rev acc
    else if n mod x = 0 then aux (x+1) (n) (x::acc) 
    else aux (x+1) (n) (acc)
  in aux 1 n [];;

(*4. Mediante List.filter e List.init definire la funzione divisors*) 

let divisors3 n = 
  List.filter (fun x -> n mod x = 0) (List.init n (fun x -> x+1)) ;;
  
(*5. Definire la funzione divisors2:int -> int -> int list che prende due interi e restituisce la liste dei loro divisori comuni *)

let divisors3 n1 n2 = 
  let divisorN1 = divisors2 n1 in 
  let divisoriN2 = divisors2 n2 in 
  let rec aux divisorN1 acc = 
    match divisorN1 with 
    | [] -> acc
    | x::rest -> (if (List.mem x divisoriN2 ) then aux rest (x::acc) 
                  else aux rest acc)
  in aux divisorN1 [];;

(***********************************************************************************************************************************************************************************)  


  

(*****************************************************************************ESERCIZIO 5*******************************************************************************************) 
(*Vogliamo definire un metodo per filtrare una lista tale che ne teniamo solo i elementi pari, positivi, e inferiore o uguale a 10*)
(*1. Definire una funzione pari:int -> bool che restituisce true se un intero e pari e false altrimenti*)

let pari n = if n mod 2 = 0 then true else false ;;

(*2. Definire una funzione dieci: int -> bool che prende in entrata un intero n che restituisce true se e solo se 0 ≤ n ≤ 10 *)

let dieci n = if n>=0 && n<=10 then true else false;;

(*3. Definire una funzione pariList che prende una lista di interi e restituisce questa lista tenando solo i elementi pari.*)

let rec pariList lst = 
  match lst with 
  |[] -> true 
  | x::rest -> (if (x mod 2 = 0 ) then pariList rest 
                else false );;

(*4. Definire una funzione dieciList che prende una lista di interi e restituisce questa lista tenando solo i interi 𝑛 tale che 0 ≤ 𝑖 ≤ 10 *)

let rec dieciList lst = 
  match lst with 
  |[] -> true 
  | x::rest -> (if (x >=0 && x<=10) then dieciList rest 
                else false );;

(*5. Definire una funzione filtro che prende una lista di interi e restituisce questa lista tenando solo i interi 𝑛 che sono pari e compreso fra 0 e 𝑛.*)

let filtro lst = 
  let rec aux lst acc=
    match lst with
    | [] -> List.rev acc
    | x::rest -> (if (pari x && dieci x) then aux rest (x::acc)
                  else aux rest acc)
  in aux lst [];;

(*6. Definire meet:(a’ -> bool) -> (a’ -> bool) -> (a’ -> bool) che prende due funzione f e g e restituisce la una funzione di tipo ’a -> bool che applicata a un elemento a di 
tipo ’a restituisce true se e solo se f(a)=true e g(a)=true.*)

let meet f g a =
  if f a = true && g a = true then true else false ;;

(*7. Usando la funzione meet, definire la funzione filtro usando un unica volta la funzione List.filter.*) 

let filtro2 lst = 
  List.filter (fun x-> meet pari dieci x  ) lst ;;
(***********************************************************************************************************************************************************************************)  



                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 6*******************************************************************************************) 
(*Mediante le funzioni List.fold_left e List.init quando e utile, definire le funzione seguente:*)
(*1. sumTo: int -> int che prende un intero 𝑛 e ritorna la somma Í1≤𝑖 ≤𝑛 𝑖.*)

let sumTo n = 
  let rec aux s acc =
    match s with 
    | 0 -> acc
    | _ -> aux (s-1) (s+acc)
  in aux n 0;;

(*2. sumFromTo: int -> int -> int che prende due interi 𝑛 e 𝑚 e ritorna la somma Í𝑛≤𝑖 ≤𝑚 𝑖.*)

let sumFromTo n m = 
  let rec aux s acc = 
    match s with 
    | s when s < n -> acc
    | _ -> aux (s - 1) (s + acc)
  in aux m 0;;

(*3. la funzione fattoriale fact:int -> int che a un intero 𝑛 restituisce Î1≤𝑖 ≤𝑛 𝑖.*)

let fattoriale n = 
  let rec aux b acc= 
    match b with
    | 0 -> acc
    | _ -> aux (b-1) (b*acc)
  in aux n 1;;

(***********************************************************************************************************************************************************************************)  



                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 7*******************************************************************************************) 
(*Vogliamo La funzione listconcat : (’a list) list -> list che prende una lista di liste e restituisce la loro concatenazione.*)
(*1. Definire listconcat senza usa la funzione List.fold_left. *)

let rec listconcat lst =
  match lst with
  | [] -> []
  | hd :: tl -> hd @ listconcat tl;;

(*2. Definire binaryconcat: ’a list -> ’a list -> ’a list che prende due liste l1 e l2 e restituisce la loro concatenazione. *)

let binaryconcat l1 l2 = l1@l2;; 

(*3. Mediante binaryconcat e List.fold_left definire la funzione listconcat *)

let listconcat lst = List.fold_left binaryconcat [] lst;;

(***********************************************************************************************************************************************************************************)  
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 8*******************************************************************************************) 
(*Vogliamo definire un concatenazione distributiva cioè una funzione concatDist :’a -> (’a list) list -> (’a list) list che prende un elemente x e una lista di liste [l1;. . . ;ln] 
e restituisce [x::l1;. . . ;x::ln] *)                     
(*1. Definire concatDist senza usare la funzione List.fold_left *)

let rec concatDist x lst =
  match lst with
  | [] -> []  
  | y::rest -> (x::y) :: concatDist x rest ;;

(*2. Definire la funzione add : ’a -> ’a list -> ’a list che prende un elemento x e una lista lst e restituisce x::lst.*)

let add x lst = x::lst;;

(*3. Mediante add e List.fold_left definire concatDist *)

let concatDist x lst =
  List.rev (List.fold_left (fun acc y -> add x y :: acc) [] lst);;

(***********************************************************************************************************************************************************************************)  
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 9*******************************************************************************************) 
(*Definire la vostra versione delle funzioni del modulo List;*)
(*List.init: int -> (int -> ’a) -> ’a list *)

let myInit n f = 
  let rec aux d acc= 
    match d with 
    | 0 -> acc
    | _ -> aux (d-1) (f d::acc)
  in aux n [];;

(*2. List.map: (’a -> ’b) -> (’a list) -> ’b list.*)

(***********************************************************************************************************************************************************************************) 