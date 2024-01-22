

(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*Vogliamo implementare un modo di determinare se due liste sono uguale dopo riordinamento. Cioé se contengono i stessi elementi ma in un ordine diverso*)
(*1. Definire una funzione remove : a’ -> a’ list -> a’ list che prende un elemente elem di tipo a’ e una lista lst di elementi di tipo a’ e restituisce una lista lstOut tale che:
• Se elem occore in lst allora lstOut corrisponde alla lista in entrata lst da cui e stato tolta la prima occorenza di elem.
• Se elem non occore in lst allora lstOut corrisponde a lst.
Ad esempio, remove 2 [1;2;3;2] restituisce [1;3;2] invece remove 0 [1;2;3;2] restituisce [1;2;3;2]. *)

let remove n lst =
  let rec aux lst acc =
    match lst with
    | [] -> List.rev acc 
    | x :: rest -> (if x = n then List.rev_append acc rest
                    else aux rest (x :: acc))
  in aux lst [];;

(*2. Definire una funzione removers : a’ list -> (a’ list -> a’ list) list che prende una lista [a1;. . . ;an] e restituisce la lista di funzione [remove a1 ; . . . ; remove an]*)

let removers lst = List.map (fun n lstBis -> remove n lstBis) lst;;

(*3. Definire una funzione apply : ((a-> b) list * a) -> b che prende una lista di funzioni [f1;. . . ;fn] e  une elemento elem di tipo a e restituisce fn(...(f1(elem))...)*)

let rec apply (functions, elem) =
  match functions with
  | [] -> elem
  | f :: rest -> apply (rest, f elem);;

(*4. Usando removers e apply definire una funzione ordEqual: a’ list * a’ list -> bool che prende due liste e determina se contengono i stessi elementi*)

let ordEqual (la, lb) =
  let functions = removers la in
  let result = apply (functions, lb) in
  result = [];; 

(***********************************************************************************************************************************************************************************)
(*Dato una lista lst e 𝐴 l’insieme che rappresenta vogliamo costruire la funzione che rappresenta l’insieme 𝐴 a partire di lst *)
(*1. Qual l’insieme rappresentato dalle seguente liste; [0;1;2;2;2], [3;4;1], [3;3;3;3;3], [2;0;0;1] *)

                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 2*******************************************************************************************)
         




















                                                                                                                                                                           

                                                                                                                                                                                    