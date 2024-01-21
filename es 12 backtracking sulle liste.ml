
let l= [1;2;3;4;5];;

(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*Cerchiamo in una lista una sottolista che verifica la proprietà 𝑃, tale 𝑃 (lst) vale se e solo se il suo primo elemento e il l’ultimo elemento di lst e 1 *)
(*1. Definire una funzione safetest : int -> ’a -> ’a list -> bool che prende un intero n un elemento a e una lista lst e restituisce true se la lista contiene al n–esimo posto 
  l’elemento a e false altrimenti, la funzione non solleva mai eccezione *)

let rec safetest n a lst = 
  match lst with 
  | [] -> false 
  | x::rest -> (if (n=0 && a =x) then true 
                else safetest (n-1) a rest );;
        
(*2. Definire la funzione accept : ’a list -> bool che restituisce true se e solo se la lista rispetta la proprietà.*)

let accept lst = 
  match lst with 
  | [] -> false 
  | x::rest ->( if( (x=1)  && (1= List.hd (List.rev lst ) )) then true 
                else false );;

(*3. Definire la funzione reject : ’a list -> bool che prende lst erestituisce true se e solo se ogni lista che contiene lst non verifica la propietà 𝑃*)

let reject lst = not(accept lst);;

(*4. Usando le funzioni reject e accept definire una funzione backtrack : ’a list -> ’a list che prende una lista e restituisce una sua sottolista che verifica la proprietà 𝑃. 
  Si può usare una versione con un accumulatore. Se non e trovata nessuna sottolista la funzione sollevera un eccezione*) 

let backtrack lst =
  let rec aux acc lst =
    if accept acc then acc
    else if reject acc then raise Not_found
    else 
      match lst with
      | [] -> raise Not_found
      | x :: rest -> aux (acc @ [x]) rest
  in aux [] lst ;;

(*5. Usando backtrack e una dichiarazione try ... with ... Definire una funzione resolvable :’a list -> bool che prende una lista lst e restituisce true se e solo se backtrack 
  ha trovato una sottolista con la proprietà 𝑃 *)

let resolvable lst =
  try let _ = backtrack lst in true  (* backtrack non ha sollevato un'eccezione, quindi la lista è risolvibile *)
  with  | Not_found -> false;;  (* backtrack ha sollevato un'eccezione, quindi la lista non è risolvibile *)
                                
  
(*6. Definire resolvable senza usare backtrack, cioè quandè che una lista può contenire una sottolista che ha come primo e ultimo elemento 1?*)

let resolvable lst =
  match lst with
  | [] -> false
  | x :: rest -> x = 1 && List.mem 1 rest;; 

(***********************************************************************************************************************************************************************************)

                                                                                                                                                                                    

  
(*****************************************************************************ESERCIZIO 2*******************************************************************************************) 
(*Cerchiamo in una lista una sottolista con ripetizioni che rispetta la proprietà 𝑃 tale che; 𝑃 (lst) vale see solo se la somma dei elementi di lst vale 5*)
(*1. Definire la funzione accept : ’a list -> bool che restituisce true se e solo se la lista rispetta la proprietà*)

let rec sum lst = 
  match lst with
  |[]-> 0
  |x::rest -> x + sum rest ;;
  
let accept lst = 
  if (sum lst =5) then true else false ;;

(*2. Definire la funzione reject : ’a list -> bool che prende lst erestituisce true se e solo se ogni lista che contiene lst non verifica la propietà 𝑃 *)

let reject lst = 
  if (sum lst = 5 ) then false else true ;;

(*3. Usando le funzioni reject e accept definire una funzione backtrack : ’a list -> ’a list che prende una lista e restituisce una sua sottolista con ripetizioni che verifica 
la proprietà 𝑃 *)

let backtrack lst = 
  let rec aux acc lst = 
    if accept acc then acc 
    else if reject acc then raise Not_found
    else 
      match lst with 
      |[] -> raise Not_found
      |x::rest -> aux (acc@[x]) rest 
  in aux [] lst;;

(*4. Adattare il problema aggiungendo un parametro 𝑛 cioè 𝑃 (lst, 𝑛) vale se e solo se la somma dei elementi di lst vale 𝑛, definire accept, reject e backtrack con un 
parametro in piu*) 

let accept lst n= 
  if (sum lst =n) then true else false ;;

let reject lst n= 
  if (sum lst = n) then false else true ;;

let backtrack n lst = 
  let rec aux acc lst = 
    if accept acc n then acc 
    else if reject acc n then raise Not_found
    else 
      match lst with 
      |[] -> raise Not_found
      | x::rest -> aux (acc@[x]) rest 
  in aux [] lst;; 
  
(***********************************************************************************************************************************************************************************) 
  
  
  

(*****************************************************************************ESERCIZIO 3*******************************************************************************************)
(*Immaginiamo il gioco seguente con un giocatore; il giocatore lancia un dado un numero finito di volte e inserisce in una lista il risultato dei suoi lanci. Ad esempio la lista 
  [2;3;5] significa che il giocatore ha fatto tre tiri, ha tirato il dado e ha fatto 2 poi ha fatto 3 e poi 5. La condizione di vincita e la seguente; il giocatore deve aver 
  fatto un 1 seguito da un 2 nei prossimi due tiri. Ad esempio [1;4;3;2;3] non e vincente perche il valore 2 non occore nei due prossimi tiri, invece [1;3;2;8] e vincente perche 
  l’evento 2 occore al secondo tiro dopo 1. Possiamo tradure questa condizione per vincere come una condizione 𝑃 sulle liste che vale se e solo se il primo elemento della lista e 1, 
l’ultimo elemento e 2 e la lunghezza della lista e inferiore o uguale a 3*)

let lst = [1;2;4;2;1;3;2;6];;

(*1. Definire la funzione accept : ’a list -> bool che restituisce true se e solo se la lista rispetta la proprietà*)

let verificaUno lst =
  match lst with
  | [] -> false 
  | primo :: _ -> primo = 1 ;;
                  
let verificaTerzo lst =
  match lst with
  | _ :: _ :: terzo :: _ -> terzo = 2
  | _ -> false;;
    
(*let verifica lst = 
  if (List.hd lst = 1) && (List.hd (List.rev lst) = 2) then true else false;;*)

let verifica lst =
  if verificaUno lst && verificaTerzo lst then true else false ;;

let accept lst = 
  if verifica lst then true else false ;;

(*2. Definire la funzione reject : ’a list -> bool che prende lst erestituisce true se e solo se ogni lista che contiene lst non verifica la propietà 𝑃 *)

let reject lst = 
  if  not(verifica lst) || (List.length lst > 3) then true else false ;; 

(* 3. Usando le funzioni reject e accept definire una funzione backtrack : ’a list -> ’a list che prende una lista e restituisce una sua sottolista (senza ripetizioni) che 
   verifica la proprietà 𝑃. Si può usare una versione con un accumulatore. Se non e trovata nessuna sottolista la funzione sollevera un eccezione. *)

let backtrack lst = 
  let rec aux acc lst= 
    if accept acc then acc 
    else if reject acc then raise Not_found 
    else 
      match lst with 
      | []-> raise Not_found 
      | x::rest -> aux (List.rev(acc@[x])) rest 
  in aux [] lst;;
      

let backtrack1 lst = 
  let rec aux acc lst= 
    if accept acc then acc 
    else if reject acc then raise Not_found 
    else 
      match lst with 
      | []-> raise Not_found 
      | x::rest -> aux (List.rev(x::acc)) rest 
  in aux [] lst;;

(***********************************************************************************************************************************************************************************)