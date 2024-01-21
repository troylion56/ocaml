
type 'a btree = Empty | Tr of 'a * 'a btree * 'a btree;;
type direction = Left | Right;; 

let rec size = function 
  | Empty -> 0
  | Tr(v,l,r) -> 1+ size l + size r;;

let rec sommaTuttiNodi = function 
  | Empty -> 0 
  | Tr(v,l,r) -> v + sommaTuttiNodi l + sommaTuttiNodi r;;

let rec foglieAlberoBinario = function 
  | Empty -> []
  |Tr(v,Empty,Empty) -> [v]
  |Tr(v,l,r) -> foglieAlberoBinario l @ foglieAlberoBinario r;;


let sonoFoglia = function 
  | Tr(v,Empty,Empty) -> true 
  | Tr(v,l,r) -> false 
  |Empty -> false ;;

    
(*indirizzo è una sequenza di lef e right che ti portano all'elemento dove arrivi sopra definito come direction*)
exception AlberoVuoto;;

(*restituisce il valore in quella direzione *)
let rec direzione  = function
  |(dirs, Empty) -> raise AlberoVuoto
  |([], Tr(v,l,r)) -> v
  |(Left::rest, Tr(v,l,r)) -> direzione (rest,l)
  |(Right::rest,Tr(v,l,r)) -> direzione (rest,r);;



exception Notfound;;
(*trovare l'indirizzo del nodo con backtracking*)
let rec indirizzo = function 
  | (_,Empty) -> raise Notfound
  | (n,Tr(x, l, r)) when x = n -> []
  | (n,Tr(x, l, r)) -> try Left :: (indirizzo (n,l))
      with Notfound -> Right::(indirizzo (n,r)) ;;


(*trovare un nodo con un certo valore con backtracking*)


let tree =
  Tr(1, Tr(2, Tr(4,Empty,Empty), Empty),
     Tr(3, Tr(5, Tr(6,Empty,Empty), Tr(7,Empty,Empty)),Empty));; 
let ris4 = indirizzo (7 ,tree);;
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    