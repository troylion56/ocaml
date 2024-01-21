type 'a tree = Tr of 'a * ('a tree list);; 

let tree_example = Tr(1, [Tr(2, []); Tr(3, [Tr(4, []); Tr(5, [])]); Tr(6, [])]);;

(*la size di un albero ennario*)
let rec size = function 
  | Tr(x,[])-> 1 
  | Tr(x,t::rest) -> size t + size (Tr(x,rest))
  
let ris1 = size tree_example;;

exception Notfound;;

(*trovare il percorso che percorre per arrivare a quel targert non vuole le direzioni ma i valori*)
let rec search t target acc = 
  match t with 
  |Tr(a,tlist) when (a=target)-> List.rev (a::acc)
  |Tr(a,[]) ->raise Notfound 
  |Tr(a,x::rest) ->try search x target (a::acc)
      with Notfound -> search (Tr(a, rest)) target acc  ;; 

let result1 = search tree_example 6 []
    