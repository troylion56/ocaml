type 'a tree = Tr of 'a * ('a tree list);;

let t = Tr ( 2 , [Tr (3 , [Tr(1,[])]) ; Tr(3,[]) ; Tr (1 , [Tr(1,[]) ; Tr(2,[])] )])  ;;

(*  per fare questo albero devo copiare quello sotto e al posto di leaf devo mettere es leaf 4 = Tr(4,[])*) 
(*per farlo funzioanre con leaf 
  let leaf x = Tr(x,[])
(* Esempio: *)
let t =Tr ( 2 , [Tr (3 , [leaf(1)]) ; leaf(3) ; Tr (1 , [leaf(1) ; leaf(2)])])*)

(*****************************************************************************ESERCIZIO 1*******************************************************************************************)
(*1. Definire una funzione size : ’a tree -> int che prende un albero e restituisce la quantità di elementi che contiene*)

let rec size = function 
  |Tr(a,[]) -> 1
  |Tr(a,x::rest) -> size x+ size (Tr(a,rest));; 

(*2. Definire una funzione sumTree : int tree -> int che prende un albero di interi e restituisce la somma dei suoi elementi*)

let rec sumTree = function 
  |Tr(a,[]) -> a
  |Tr(a,x::rest) -> sumTree x + sumTree (Tr(a,rest));;
  
(*3. Definire leaf : ’a -> ’a tree che prende une elemento x e restituisce l’albero fatto di solo l’elemento x(quest’albero è una foglia)*)

let leaf x= Tr(x,[]);;

(*4. Definire isLeaf: ’a -> ’a tree una funzione che prende un albero t e restituisce true se t e una foglia, altrimenti la funzione restituisce false*)

let isLeaf = function 
  |Tr(_,[]) ->true 
  |Tr(_,tlist)-> false;; 

(***********************************************************************************************************************************************************************************)
                                                                                                                                                                                    

      
                                                                                                                                                                                                                                                                                                                                                                  
(*****************************************************************************ESERCIZIO 2*******************************************************************************************) 
(*Vogliamo implementare algoritmi di ricerca di un cammino in un albero, anche cerchando cammini con una certa proprietà.*)
(*1. Mediante backtracking definire una funzione find : ’a -> ’a tree -> ’a list che prende un elemento a e un albero t e restituisce un cammino sotto forma di lista di tipo 
  ’a list della radice di t a l’elemento a. Se nessun cammino esiste la funzione restituisce un eccezione. 
  Ad esempio find 1 Tr ( 2 , [Tr (3 , [leaf(1)]) ; leaf(3) ; Tr (1 , [leaf(1) ; leaf(2)] )]) restituisce [2;3;1] *)

exception NotFound 
let find x t = 
  let rec aux x t acc= 
    match t with 
    |Tr(a,tlist) when a=x -> List.rev (a::acc)
    |Tr(a,[]) -> raise NotFound
    |Tr(a,hd::rest)-> (try aux x hd (a::acc)
                       with NotFound -> aux x (Tr(a,rest)) acc )
  in aux x t [];; 

(*2. La funzione precedente restituisce una lista ’a list per identificare il cammino, questo crea ambiguita. Vogliamo modificare la funzione precedente tale che restituisce una 
  lista int list di interi. Definire findPos : ’a -> ’a tree -> int list che prende un elemento a e un albero t e restituisce un cammino sotto forma di lista di tipo int list della 
    radice di t a l’elemento a. Se nessun cammino esiste la funzione restituisce un eccezione. 
  Ad esempio findPos 1 Tr ( 2 , [Tr (3 , [leaf(1)]) ; leaf(3) ; Tr (1 , [leaf(1) ; leaf(2)])]) restituisce [1;1].    primo figlio secondo figlio e cosi via *)  

let findPos x t = 
  let rec aux x t acc counter = 
    match (x,t,counter) with 
    | (x,Tr(a,tlist),0) when (x=a)-> acc
    | (x,Tr(a,tlist), counter) when (x=a) -> acc@ [counter]
    | (x,Tr(a,[]),counter) -> raise NotFound
    | (x,Tr(a,hd::rest), 0 ) -> (try aux x hd (acc) 1 
                                 with NotFound -> aux x (Tr(a,rest)) (acc) (counter+1))
    | (x,Tr(a,hd::rest),counter) -> (try aux x hd (acc@[counter]) 1 
                                     with NotFound -> aux x (Tr(a,rest)) (acc) (counter+1))
  in aux x t [] 0 ;; 

(*3. Definire una funzione findPosd : ’a -> ’a tree -> (int * ’a) list che prende un elemento a e un albero t e restituisce un cammino sotto forma di lista di tipo (int * ’a) list 
  della radice di t a l’elemento a. Se nessun cammino esiste la funzione restituisce un eccezione. 
  Ad esempio findPos 1 Tr ( 2 , [Tr (3 , [leaf(1)]) ; leaf(3) ; Tr (2 , [leaf(1) ; leaf(2)] )]) restituisce [(1,3);(1,1)]*) 

let findPosd x t=
  let rec aux x t acc check counter=
    match (x,t, check, acc) with
    |(x,Tr(a,tList), check, (x1,y1)::acc) when (x=a)->acc@[(counter, a)]
    |(x,Tr(a, []), check, acc)->raise NotFound
    |(x,Tr(a,t::tList), false, acc)->(try aux x t (acc@[(counter, a)]) false 1 
                                      with NotFound->aux x (Tr(a,tList)) (acc@[(counter, a)]) true (counter+1))
    |(x,Tr(a,t::tList), true, acc)->(try aux x t acc false 1 
                                     with NotFound->aux x (Tr(a,tList)) acc true (counter+1))
  in aux x t [] false 1;;

(*4. Vogliamo ora definire una funzione che trova un cammino sotto forma di lista (int * ’a) list tale che la somma dei elementi attraversati vale 5. Adattendo la funzione findPosd 
definire una funzione findCond : ’a -> ’a tree -> (int * ’a) list che prende un elemento a e un albero t e restituisce un cammino sotto forma di lista di tipo (int * ’a) list della 
  radice di t a l’elemento a tale che la somma dei elementi attraversati vale 5.
  • Definire una funzione accept : (int * ’a list) -> bool.
  • Definire una funzione reject : (int * ’a list) -> bool.
  • Implementare findCond usando il backtracking.
Ad esempio findCond 1 Tr ( 2 , [Tr (3 , [leaf(1)]) ; leaf(3) ; Tr (2 , [leaf(1) ; leaf(2)])]) restituisce [(3,2);(1,1)]*)

(*controllare la specifica*)
let accept l =
  let rec aux l acc=
    match l with
    |[]->acc=5
    |x::l when(acc+x<=5)->aux l (acc+x)
    |_->false
  in aux l 0;;

(*controllare la specifica pure qua*)
let rejcet l= 
  let rec aux l acc= 
    match l with
    |[]-> acc>5
    |x::l when (acc+x<5) -> aux l (acc+x)
    | _ -> true 
  in aux l 0;;
  
let findCond a t= 
  let rec aux t1 acc=
    match t1 with 
    |Tr(x, []) when (x=a && accept (acc))-> acc@[x] 
    |Tr(x, lst) when (rejcet (acc@[x]))->raise NotFound 
    |Tr(x, lst)->treelst lst (acc@[x]) 
  and 
    treelst lst acc=
    match lst with 
    |[]->[] 
    |t1::lst->(try (aux t1 acc) with NotFound->treelst lst acc) 
  in aux t [];; 

(***********************************************************************************************************************************************************************************)




(*****************************************************************************ESERCIZIO 3*******************************************************************************************) 
(*Vogliamo implementare un algoritmo che ricerca un elemento e restituisce i possibili cammini.*)
(* Definire findPos : ’a -> ’a tree -> int list che prende un elemento a e un albero t e restituisce un cammino sotto forma di lista di tipo int list della radice di t a l’elemento a. 
   Se nessun cammino esiste la funzione restituisce un eccezione.
Ad esempio findPos 1 Tr ( 2 , [Tr (3 , [leaf(1)]) ; leaf(3) ; Tr (1 , [leaf(1) ; leaf(2)])]) restituisce [1;1]*)

let findPos a t = 
  let rec aux a t acc counter =
    match (a,t,counter) with 
    | (a,Tr(v,tlist),0) when a=v -> acc
    | (a,Tr(v,tlist),counter) when a=v -> acc@[counter]
    | (a,Tr(v,[]),counter) -> raise NotFound
    | (a,Tr(v,hd::rest),0)-> (try aux a hd (acc)  1
                              with NotFound -> aux a (Tr(v,rest)) (acc) (counter+1))
    | (a,Tr(v,hd::rest),counter) -> (try aux a hd (acc@[counter] ) 1
                                     with NotFound -> aux a (Tr(v,rest)) (acc) (counter+1))
  in aux a t [] 0;; 

(*2. Definire findAll : ’a -> ’a tree -> int list list che prende un elemento a e un albero t e restituisce un lista di cammini int list list della radice di t a l’elemento a. 
  La funzione non solleva mai eccezione.
Ad esempio findAll 1 Tr ( 2 , [Tr (3 , [leaf(1)]) ; leaf(3) ; Tr (1 , [leaf(1) ; leaf(2)])]) restituisce [[1;1] ; [3;1]]*)

let findAll x t=
  let rec aux x t acc counter=
    match (t, counter) with
    |(Tr(a, lst), 0) when(a=x)->[acc]@(auxlst lst (acc) 0)
    |(Tr(a, lst), counter) when(a=x)->[acc@[counter]]@(auxlst lst (acc@[counter]) 0)
    |(Tr(a, []), counter)->[]
    |(Tr(a, lst), 0)->(auxlst lst (acc) 0)
    |(Tr(a, lst), counter)->(auxlst lst (acc@[counter]) 0)
  and
    auxlst lst acc counter=
    match lst with 
    |[]->[]
    |tl::lst->(aux x tl acc (counter+1))@(auxlst lst acc (counter+1))
  in aux x t [] 0;;

(***********************************************************************************************************************************************************************************)




(*****************************************************************************ESERCIZIO 4*******************************************************************************************) 
(*Vogliamo definire una funzione che aggiunge a un sotto–albero dei figli, poi vogliamo definire un operazione di sostituzione sui alberi*)
(*1. Definire una funzione find : int list -> ’a tree -> ’a tree che prende una posizione p: int list e un albero t e restituisce il sotto albero in posizione p di t. 
Se non esiste la funzione sollevera un eccezione*)

let rec find lst t =
  match (lst,t ) with 
  | ([],t) -> t
  | ((x::l), Tr(a,[])) -> raise NotFound
  | (1::l, Tr(a,t1::tl)) -> find l t1
  | (x::l, Tr(a,t1::tl)) -> find ( (x-1)::l)   (Tr(a,tl));;

(*2. Definire una funzione add : int list -> ’a tree list -> ’a tree -> ’a tree che prende una posizione p un lista di alberi tlist e un albero t e restituisce l’albero t in cui 
  il sottoalbero in position p di t sono venuti aggiunti come figli i alberi di tlist.
Ad esempio add [2;1] [leaf(1) ; leaf(3)] Tr(1 , [leaf(3) ; Tr (1 , [ leaf(8)] ) ] ) restituisce Tr(1 , [leaf(3) ; Tr (1 , [ Tr (8 , [leaf(1) ; leaf(3)])])]) *)

let add lst lstTree t=
  let rec aux lst lstTree t temptList=
    match (lst, t) with
    |(lst, Tr(a, []))->raise NotFound
    |([1], Tr(a, tList))->Tr(a, tList@lstTree)
    |(1::lst, Tr(a, t::tList))->Tr(a, temptList@[(aux lst lstTree t [])]@tList)
    |(x::lst, Tr(a, t::tList))->aux ((x-1)::lst) lstTree (Tr(a, tList)) (temptList@[t])
  in aux lst lstTree t [];;

