(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*(Liste Palindrome). Un palindrome e una parola che si legge uguale sia dalla sinistra alla destra che dalla destra alla sinistra. Questa nozione puo essere definita anche per le 
  liste. Ad esempio le liste [1; 1] e [1; 2; 3; 3; 2; 1] sono palindrome, invece [1; 2] e [2; 3; 3] non sono palindrome*)
(*1. Definire la funzione invert : a′ list− > a′ list che prende una lista [a1; . . . ; an] e ritorna la lista [an; . . . ; a1]*)

let invert lst = 
  let rec aux lst acc= 
    match lst with
    | [] -> acc
    | x::rest -> aux rest (x::acc)
  in aux lst [];; 

(*2. Definire la funzione pali : a’ list -> bool che ritorna true se la lista e un palindrome e false altrimenti*)

let pali lst = 
  if lst = invert lst then true else false ;; 

(*1. Definire la funzione length che prende una lista e restituisce la sua lunghezza.*)

let rec length lst = 
  match lst with 
  | [] -> 0
  | x::rest -> 1 + length rest;;

(*2. Definire una funzione invertWithControl di tipo a’ list * a’ list * int che prende una tripla (𝑙1, 𝑙2, 𝑛) e restituisce la coppia (𝑙′ 1, 𝑙′ 2) dove 𝑙′ 1 corrisponde a 
  𝑙1 da cui sono stati tolti i primi 𝑛 elementi, e 𝑙′ 2 corrisponde concatenazione 𝑙𝑝 · 𝑙2 di 𝑙𝑝 la lista dei 𝑛 primi elementi di 𝑙 invertita, con 𝑙2 *) 

let rec invertWithControl = function
  |(l1,l2,0) -> (l1,l2) 
  |([],l2,n) -> ([],l2) 
  |(x1::l1,l2,n) -> invertWithControl(l1,x1::l2,n-1);; 

(*3. Definire la funzione divide che prende una lista [a1; . . . ; a2n] di lunghezza pari e ritorna la coppia di liste [a1; . . . ; an] e [an+1; . . . ; a2n]. Se la lunghezza e 
dispari la funzione sollevera un eccezione Dispari *)

exception Dispari;;

let divide_aux l = if (length(l) mod 2 = 0)
  then (invertWithControl(l,[] , length(l)/2))
  else (raise Dispari);;

let divide l =
  let (l1,l2) = divide_aux(l)
  in (l1,invert(l2));;

(*4. Mediante le funzione invert, length, divide definire una funzione pali1 che verifica se una lista e un palindrome. Cioè, per ogni liste l, pali(l) e pali1(l) sono uguali *)


(***********************************************************************************************************************************************************************************)

   
                                                                                                                                                                                    
    
(*****************************************************************************ESERCIZIO 2*******************************************************************************************) 
(*1. Definire la funzione andList : bool list -> bool che prende una lista di booleani [b1;. . . ;bn] e ritorna true se tutti i booleani bi sono veri. Se la lista e vuota restituisce true.*)  

let rec andList lst = 
  match lst with 
  | [] -> true 
  | true::rest -> andList rest
  | false::rest -> false;;

(*. Definire la funzione orList : bool list -> bool che prende una lista di booleani [b1;. . . ;bn] e ritorna true se un booleano bi e vero. Se la lista e vuota restituisce false*)

let rec orList lst = 
  match lst with 
  | [] -> false 
  | true::rest -> true 
  | false::rest -> orList rest;;

(*3. Definire la funzione clausola : bool list * bool list -> bool che dato una coppia di liste ([p1;. . . ;pk],[q1;. . . ;qk]) restituisce true se la clausola p1 ∧ · · · ∧ 
  pn ⇒ q1 ∨ · · · ∨ qk e vera e false altri-menti *)

let clausola l1 l2 = not( andList(l1) ) || orList(l2);;

(***********************************************************************************************************************************************************************************)

                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 3*******************************************************************************************) 
(*(Massimo Comune Divisore). Un intero 𝑛 divide un intero 𝑚 se esiste un intero 𝑘 tale che 𝑚 = 𝑛 ×𝑘. Il massimo comune divisore di due interi 𝑛 e 𝑚 e il piu grande intero 
  che divide sia 𝑛 che 𝑚. Ad esempio, 3 divide 12 perche 12 = 3 × 4, pero 5 non divide 12. Il massimo comune divisore di 12 e 18 e 6 *)
(*1. Definire una funzione divide : int ∗ int− > int che prende una coppia di interi (n, m) e restituisce true se 𝑛 divide 𝑚 e false altrimenti. *)

let divide (n,m ) =
  if m mod n = 0 then true else false ;;

(*2. La funzione divisors : int− > intlist che prende un intero 𝑛 e restituisce la lista dei divisori di 𝑛. Ad esempio, possiamo usare una funzione ausiliaria e ricorsiva di 
coda divTailAux di tipo int*int* (int list) *)

let divisors n = 
  let rec aux d acc = 
    match d with 
    | 1-> 1::acc
    | x -> if n mod x = 0 then aux (x-1) (d::acc)
        else aux (d-1) (acc) 
  in aux n [];;

(*3. Definire la funzione mcd : int ∗ int− > int che prende due interi 𝑛 e 𝑚 e restituisce il loro massimo comune divisore. Per esempio possiamo definire due funzione ausiliaria 
  max: int list -> int che restituisce il piu grande elemento di una lista di interi e divisors2:int *int -> int list che restituisce la lista dei interi che dividono i due interi
di una coppia (𝑛, 𝑚). *)

let max lst = 
  let rec aux lst acc = 
    match lst with
    | [] -> acc
    | x:: rest -> if x>acc then aux rest x
        else aux rest acc
  in aux lst 0;;

let divisors2 n m = 
  let divN = divisors n in 
  let divM = divisors m in 
  max (List.filter (fun x -> List.mem x divM) divN);;

let mcd n m = 
  divisors2 n m;;

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 4*******************************************************************************************) 
(*(Iterazione e Interi di Church). Dato un intero 𝑛 l’iterazione di una funzione 𝑓 si scrive 𝑓 𝑛 e applicata a un intero 𝑘 restituisce 𝑓 (𝑓 (. . . (𝑓 (𝑘)) . . . ) 
  dove 𝑓 occore 𝑛 volte. Piu formalmente: 𝑓 0 (𝑘) = 𝑘 e 𝑓 𝑛 (𝑘) = 𝑓 (𝑓 𝑛−1 (𝑘)) *)
(*1. Definire la funzione cur che prende una funzione f : A*B -> C e ritorna la sua versione curryficata cioé di tipo A -> (B -> C) *)

let cur f a b = f(a,b);; 

(*2. Definire una funzione iter che prende una funzione 𝑓 e un intero 𝑛 e restituisce la funzione 𝑓 𝑛 . Si puo per esempio, usare una funzione ausiliaria che prende tre argomenti 
  (𝑓 , 𝑛, 𝑘) e ritorna 𝑓 𝑛 (𝑘) e poi usare una forma curryficata. Ad esempio, dato f = function x -> x+2 allora iter(f,3) = function x -> (((x+2)+2)+2).
Gli interi di church sono una rappresentazione degli interi basata sul costruttore function e l’applicazione iterata dellefunzioni;
𝑛 ↦ → 𝜆𝑥 .𝜆𝑓 .𝑓 𝑛 (𝑥) cioè 𝑛 ↦ → function x -> function f -> fn (x).  *)

let iter f n =
  let rec iter_helper n k =
    if n = 0 then k
    else iter_helper (n - 1) (cur f k)
  in iter_helper n (fun x -> x) ;;

(*3. Definire la funzione church che prende un intero 𝑛 e restituisce la sua rappresentazione di Church*)

let church n = fun x f -> iter (fun y -> f (y x)) n;;

(***********************************************************************************************************************************************************************************)



                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 5*******************************************************************************************) 
(*(Espressione ricorsive e stringhe). Consideriamo di aver dichiarato il tipo ricorsivo seguente: type espr = Name of String | Space of espr | Concat of espr*espr
Un elemento di tipo espr e della forma Name s, Space e o Concat (e1,e2) dove 𝑠 e una stringha, 𝑒, 𝑒1, 𝑒2 sono espressione. La sostituzione in un espressione E di un nome Name 
  s con un espressione E’ si scrive E [ Name s <- E’], e corrisponde a l’espression E dove tutte le occorrenze di Name s vengono sostituite con l’espressione E’ *)
(* 1. Definire la funzione substitution : espr * string * espr -> espr che implementa la sostituzione, cioè il valore di substitution(e,s,e’) corrisponde a E [ Name s <- E’].
Ad esempio,
- substitution(Name "a", "a" , Space(Name "c")) restituisce Space(Name c).
- substitution(Name "b", "a" , Space(Name "c")) restituisce Name "b".
- substitution(Concat( Space(Name "a") , Name "cd"), "a" , Space(Name "c")) restituisce Concat(Space(Space((Name "c"))) , Name "cd") *)

type espr = Name of string | Space of espr | Concat of espr * espr

let rec substitution e s e' =
  match e with
  | Name n -> if n = s then e' else e
  | Space e1 -> Space (substitution e1 s e')
  | Concat (e1, e2) -> Concat (substitution e1 s e', substitution e2 s e');;

(*2. Definire la funzione eval: espr -> string che prende un espressione 𝑒 e ritorna una stringa interpretando Concat come la concatenazione e Space come la concatenazione con " ".
Ad esempio,
- eval Name "frase" restituisce "frase".
- eval Space(Name "frase") restituisce "frase ".
- eval Concat (Space(Name "una"), Name "frase") restituisce "una frase". *)
    
let rec eval e =
  match e with
  | Name n -> n
  | Space e' -> eval e' ^ " "
  | Concat (e1, e2) -> eval e1 ^ eval e2 

(***********************************************************************************************************************************************************************************)




(*****************************************************************************ESERCIZIO 6*******************************************************************************************) 
(*(Ordinamento di liste). Una lista di interi [a1; . . . ; an] e crescente se [a1 ≤ · · · ≤ an]. Ordinare una lista corrisponde a trasformare una lista qualsiasi in una lista 
  crescente. Ad esempio [2;1;7;8;4] diventa [1;2;4;7;8]. L’obiettivo del esercizio e di implementare un modo per ordinare le liste di interi *)
(*1. Definire un funzione transition che prende due liste (l1, l2) e se l’elemento in testa di l2 e piu piccolo dell’elemento in testa h1 di l1 restituisce h :: l2, altrimenti 
  restituisce l2. Se l1 = [] allora restituisce l2. Se l1 non e vuota e l2 = [] restituisce [x]. *)

let transition lst1 lst2 = 
  match lst1,lst2 with 
  | [],lst2 -> lst2
  | x::rest,[] -> [x]
  | x::rest1,y::rest2 ->( if x>y then x::y::rest2
                          else y::rest2 );; 
  
(*2. Definire una funzione suborder che prende una lista l una lista che corrisponde alla lista l ottenuta togliendo i elementi non ordinati in l. Si puo usare la funzione invert 
e la funzione transition *)

let suborder lst =
  let rec suborder_helper acc = function
    | [] -> acc
    | x :: [] -> x :: acc
    | x :: rest -> (let new_acc = transition acc [x] 
                    in suborder_helper new_acc rest)
  in suborder_helper [] lst;;
  
(*3. Definire una funzione orderSplit che prende una lista 𝑙 e restituisce la coppia di liste (l1,l2) dove l1 corrisponde a suborder(l) e l2 corrisponde ai elementi di l 
che non occorono in l1 *)

let orderSplit lst =
  let ordered = suborder lst 
  in let not_ordered = List.filter (fun x -> not (List.mem x ordered)) lst 
  in (ordered, not_ordered);;

(*4. definire una funzione orderAdd che prende una lista di interi [a1;. . . ;an] e un intero n restituisce la lista [a1;. . . ;ai;n;ai+1;. . . ;an] tale che per tutti 
1 ≤ 𝑘 ≤ 𝑖 l’intero n maggiora ak *)

let rec split_at i lst =
  if i = 0 then
    ([], lst)
  else
    match lst with
    | [] -> ([], [])
    | x :: rest ->
        let front, back = split_at (i - 1) rest in
        (x :: front, back)
        
let orderAdd lst n i =
  let (front, back) = split_at i lst in
  front @ [n] @ back;;

(*5. Definire la funzione order che prende una lista e restituisce la sua versione ordinata *)

let rec order lst =
  match lst with
  | [] -> []
  | x :: rest -> (let (ordered, not_ordered) = orderSplit rest 
                  in let new_ordered = orderAdd ordered x (List.length ordered) 
                  in new_ordered @ order not_ordered);; 
  
(***********************************************************************************************************************************************************************************)