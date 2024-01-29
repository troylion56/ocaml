type espr = Zero | Succ of espr | Plus of espr * espr
(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*Consideriamo di aver dichiarato i seguenti tipi ricorsivi: type espr = Zero | Succ of espr | Plus of espr * espr;; *)
(*1. Definire la funzione eval : espr -> int che calcola il valore di un elemento di espr, interpretando Plus come la somma degli interi e succ come la funzione successore *)

let rec eval espr =
  match espr with
  | Zero -> 0
  | Succ e -> 1 + eval e
  | Plus (e1, e2) -> eval e1 + eval e2;;

(*2. Definire la funzione opposta repr : int -> espr che associa a un intero la sua representazione nel tipo intero come l’iterazione del costruttore Succ. *)

let rec repr n =
  match n with
  | 0 -> Zero
  | _ -> Succ (repr (n - 1));;

(*3. Definire la funzione mult : espr * espr -> espr che calcola la moltiplicazione di due espressioni mediante repr *)

let rec mult (e1, e2) =
  match e1 with
  | Zero -> Zero
  | Succ Zero -> e2
  | Succ rest -> Plus (mult (rest, e2), e2)
  | Plus (e1a, e1b) -> Plus (mult (e1a, e2), mult (e1b, e2));;

(***********************************************************************************************************************************************************************************) 
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 2*******************************************************************************************)
(*Consideriamo di aver dichiarato il tipo seguente type espr = Int of int | Plus of espr * espr | Var of string;;*)   
type espr = Int of int | Plus of espr * espr | Var of string;;
(*Un ambiente di esecuzione 𝑎𝑚𝑏 sugli interi e una lista di tipo (string * int) list dove il fatto che una coppia ("nome", n) occorre nell’ambiente 𝑎𝑚𝑏 significa che la 
variabile "nome" ha il valore 𝑛 *)
(*1. Definire una funzione eval che prende un espressione e un ambiente di esecuzione sugli interi e restituisce il valore dell’espressione. Se una variabile occorre nell’espressione 
  ma non e assegnato a nessun intero dall’ambiente allora eval restituira un eccezione *)

let rec eval expr env =
  match expr with
  | Int n -> n
  | Plus (e1, e2) -> eval e1 env + eval e2 env
  | Var name -> (try List.assoc name env
                 with Not_found -> failwith ("not assigned in the environment"));;

(*2. Definire una funzione dependency : espr -> string list che applicata a un espressione 𝑒 restituisce la lista dei nomi delle variabile contenute nel espressione 𝑒. *) 

let rec dependency expr =
  match expr with
  | Int _ -> []
  | Plus (e1, e2) -> dependency e1 @ dependency e2
  | Var name -> [name];;

(*3. Una stringa 𝑠 occorre in un espressione 𝑒 se Var s e una sottoespressione di 𝑒, diciamo che occorre linearmente se Var s occorre un unica volta in 𝑒.
Un espressione e lineare se tutte le stringe che contiene occorrono in modo lineare. Definire una funzione linear : espr -> espr che prende un espressione 𝑒 e la trasforma in 
    un espressione lineare, cambiando solamente il nome delle stringe che occorrono non linearmente con nuovi nomi *)

(*da fare *) 

(***********************************************************************************************************************************************************************************) 

                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 3*******************************************************************************************)
(*Consideriamo di aver dichiarato il seguente tipo type espr = Int of int | Plus of espr * espr ;; Ricordiamo che 𝑒1 e una sottoespressione di 𝑒2 quando:
• Le due espressioni sono uguali 𝑒1 = 𝑒2. •l’espressione 𝑒2 e delle forma 𝑃𝑙𝑢𝑠 (𝑒3, 𝑒4) e 𝑒1 e una sottoespressione di 𝑒3 o di 𝑒4. *)  
(*1. Definire una funzione isequal: espr * espr -> bool che restituisce true se le due espressioni sono uguali e false altrimenti*)

let rec isequal e1 e2 =
  match (e1, e2) with
  | (Int n1, Int n2) -> n1 = n2
  | (Plus (e1a, e1b), Plus (e2a, e2b)) -> isequal e1a e2a && isequal e1b e2b
  | _ -> false ;;

(*2. Definire la funzione subexpr : espr * espr -> bool che applicata a (𝑒1, 𝑒2) restituisce true quando 𝑒1 e una sottoespressione di 𝑒2 e false altrimenti *)

let rec subexpr e1 e2 =
  if isequal e1 e2 then true
  else 
    match e2 with
    | Int _ -> false
    | Plus (e2a, e2b) -> subexpr e1 e2a || subexpr e1 e2b;;

(***********************************************************************************************************************************************************************************) 
                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
  
(*****************************************************************************ESERCIZIO 4*******************************************************************************************) 
(*Consideriamo di aver dichiarato il tipo seguente type espr = Int of int | Plus of espr * espr | Mult of espr*espr;; type direction = Left | Right;;
Una posizione su un espressione e una sequenza (𝑎1, . . . , 𝑎𝑛 ) dove tutti gli 𝑎𝑖 corrispondo a ’left’ o ’right’. In un espressione 𝑒, la sottoespressione in posizione 
  𝑙 = (𝑎1, . . . , 𝑎𝑛 ) è la sottoespressione 𝑝𝑜𝑠 (𝑒, 𝑙) di 𝑒 ottenuta per induzione;
• 𝑝𝑜𝑠 (𝑒, []) = 𝑒.
• 𝑝𝑜𝑠 (𝑂𝑃 (𝑒1, 𝑒2), (𝑙𝑒 𝑓 𝑡, 𝑎1, . . . , 𝑎𝑛 )) = 𝑝𝑜𝑠 (𝑒1, (𝑎1, . . . , 𝑎𝑛 )) dove 𝑂𝑃 corrisponde a Mult o Plus.
• 𝑝𝑜𝑠 (𝑂𝑃 (𝑒1, 𝑒2), (𝑟𝑖𝑔ℎ𝑡, 𝑎1, . . . , 𝑎𝑛 )) = 𝑝𝑜𝑠 (𝑒2, (𝑎1, . . . , 𝑎𝑛 )) dove 𝑂𝑃 corrisponde a Mult o Plus. 
• In qualsiasi altro caso la funzione non e definita *) 
type espr = Int of int | Plus of espr * espr | Mult of espr*espr;;
type direction = Left | Right;;
(*1. Definire la funzione di concatenazione di due liste *)

let rec concatenazione l1 l2 =
  match l1 with
  | [] -> l2
  | hd :: tl -> hd :: concatenazione tl l2;;

(*2. Definire la funzione concatHO : ’a * (’a list) list -> (’a list) list che applicata a un elemento 𝑎 e una lista di liste 𝑙 = [𝑙1;... ;ln] concatena 𝑎 con tutte le liste li*)

let concatHO a lists =
  List.map (fun l -> concatenazione [a] l) lists;;

(*3. Una posizione 𝑙 è valida su un espressione 𝑒 quando 𝑝𝑜𝑠 (𝑒, 𝑙) e definito. Definire la funzione positions : espr -> direction list list che prende un espressione e 
restituisce la lista delle sue posizione valide *)

let rec positions expr =
  match expr with
  | Int _ -> [[]]  (* Una posizione valida per un valore Int è la lista vuota *)
  | Plus (e1, e2) | Mult (e1, e2) -> (let positions_e1 = List.map (fun dirs -> Left :: dirs) (positions e1) in
                                      let positions_e2 = List.map (fun dirs -> Right :: dirs) (positions e2) in
                                      [] :: (positions_e1 @ positions_e2)) ;;

(*4. Definire la funzione 𝑝𝑜𝑠 (·, ·) in ocaml*)

let rec pos expr dirs =
  match (expr, dirs) with
  | _, [] -> expr
  | Int _, _ -> failwith "Invalid position for Int value"
  | Plus (e1, e2), Left :: rest -> pos e1 rest
  | Plus (e1, e2), Right :: rest -> pos e2 rest
  | Mult (e1, e2), Left :: rest -> pos e1 rest
  | Mult (e1, e2), Right :: rest -> pos e2 rest
  | _, _ -> failwith "Invalid position" ;;

(*5. Definire una funzione subexprpos : espr * espr -> direction list che applicata a (𝑒, 𝑒′) restituisce la lista delle posizione delle sottoespressione di 𝑒 uguale a 𝑒′ *)

(*let rec subexprpos_helper expr target pos acc =
   match (expr, target) with
   | _, _ when isequal expr target -> List.rev pos :: acc
   | Plus (e1, e2), _ -> (let acc' = subexprpos_helper e1 target (Left :: pos) acc in
                          subexprpos_helper e2 target (Right :: pos) acc')
   | Mult (e1, e2), _ -> (let acc' = subexprpos_helper e1 target (Left :: pos) acc in
                          subexprpos_helper e2 target (Right :: pos) acc')
   | _, _ -> acc;;*)

(*5. Definire una funzione subexprpos : espr * espr -> direction list che applicata a (𝑒, 𝑒′) restituisce la lista delle posizione delle sottoespressione di 𝑒 uguale a 𝑒′ *)

(*let subexprpos expr target =
   match subexprpos_helper expr target [] [] with
   | [] -> failwith "No matching subexpression found"
   | lst -> lst ;;*)


(***********************************************************************************************************************************************************************************) 

                                                                                                                                                                                    
                                                                                                                                                                                    
                                                                                                                                                                                    
(*****************************************************************************ESERCIZIO 5*******************************************************************************************) 
(*Consideriamo di aver dichiarato il seguente tipo type espr = Const of int | Ident of ident | Lambda of ident * espr | App of espr * espr ;; and ident = string;;
Un ambiente di sostituzione e una lista di tipo (ident*espr) list *)  
type espr = Const of int | Ident of ident | Lambda of ident * espr | App of espr * espr
and ident = string;;
(*1. Definire una funzione sost : espr * (ident*espr) list -> espr che quando applicata a E,l se ("nome",E’) occorre nella lista sostituice ogni occorrenza de "nome" con l’espressione E’ *)

let rec sost (expr, substitution) =
  match expr with
  | Const _ -> expr
  | Ident name -> (try List.assoc name substitution
                   with Not_found -> expr)
  | Lambda (param, body) -> Lambda (param, sost (body, substitution))
  | App (e1, e2) -> App (sost (e1, substitution), sost (e2, substitution)) ;;


let rec break (expr, condition, replacement) =
  if condition expr then (Ident replacement, expr)
  else
    match expr with
    | Const _ | Ident _ -> (expr, Ident "null")
    | Lambda (param, body) -> (let (newBody, subExpr) = break (body, condition, replacement) in
                               (Lambda (param, newBody), subExpr))
    | App (e1, e2) -> (let (newE1, subExpr1) = break (e1, condition, replacement) in
                       let (newE2, subExpr2) = break (e2, condition, replacement) in
                       (App (newE1, newE2), if subExpr1 = Ident "null" then subExpr2 else subExpr1));;







(***********************************************************************************************************************************************************************************) 

                                                                                                                                                                                    
                                                                                                                                                                                 