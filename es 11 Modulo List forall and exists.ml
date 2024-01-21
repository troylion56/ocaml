
let l= [1;3;5;7;10];;

(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*Implementare le funzione del modulo list;*)
(*1. Una funzione che si comporta come List.for_all*)

let verifica f x= if f x =true then true else false ;;

let rec for_all f lst = 
  match lst with 
  |[]-> true 
  |x::rest -> (if (f x =true ) then for_all f rest 
               else false ) ;;

(*2. Una funzione che si comporta come List.exists*)

let rec exists f lst = 
  match lst with 
  | [] -> false 
  |x::rest -> (if (f x = true) then true 
               else exists f rest);;

(*3. Definire List.exists mediante List.for_all e senza usare List.exists.*)

let exists2 f lst =
  not (for_all (fun x -> not (f x)) lst) ;; 

(*4. Definire List.for_all mediante List.exists e senza usare List.exists*)

let rec for_all f lst =
  not (exists2 (fun x -> not (f x)) lst);;


(***********************************************************************************************************************************************************************************)

                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
  
(*****************************************************************************ESERCIZIO 3*******************************************************************************************) 
  (*Definire una funzione witness : (’a -> bool) -> ’a list -> ’a une versione List.exists che prende una funzione f e una lista lst e invece di restituire un booleano restituisce 
  il primo elemento della lista cherestituisce true per f altrimenti sollevera un eccezione.*)

let witness f lst =
  let rec aux lst =
    match lst with 
    |[] -> raise Not_found 
    |x::rest -> (if (f x ) then x 
                 else aux rest)
  in aux lst;;

let ris = witness (fun x -> x mod 2 = 0) l;;
                                                                                                                                                                                    
(***********************************************************************************************************************************************************************************) 
  
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 4*******************************************************************************************)
(*Un intero 𝑑 e un divisore commune di due interi 𝑛 e 𝑚 se tutti i due interi sono divisibili per 𝑑. Mediante le funzione forall e exists del modulo list cerchiamo di testare 
se un intero divide due interi. *)
(*1. Definire une funzione che divide : int -> int -> bool che prende un intero 𝑑 e un intero 𝑛 e restituisce true se e solo se 𝑑 divide 𝑛 *)

let divide d n = 
  if n mod d = 0 then true else false ;;

(*2. Definire una funzione divisors che prende un intero 𝑛 e restituisce la lista dei suoi divisori*)

let divisors n = 
  let rec aux nbis acc=
    if nbis =n then List.rev acc
    else if n mod nbis = 0 then aux (nbis+1) (nbis::acc) else aux (nbis+1) acc
  in aux 1 [];;
      
(*3. Mediante List.exists definire un funzione occur: ’a -> ’a list -> bool che prende un elemento a e una lista lst e restituisce true se e solo se a occore in lst*)

let rec occur n lst = 
  match lst with 
  | [] -> false 
  | x::rest -> (if x=n then true 
                else occur n rest) ;; 
let occur n lst =
  List.exists (fun x -> x =n) lst;;
  
(*4. Mediante occur e divisors definire una funzione common: int -> int -> int -> bool che prende tre interi 𝑑, 𝑛 e 𝑚 e restituisce true se e solo se 𝑑 divide 𝑛 e 𝑚 *)

let common d n m = 
  if divide d n && divide d m then true else false ;;
    
  



(***********************************************************************************************************************************************************************************)