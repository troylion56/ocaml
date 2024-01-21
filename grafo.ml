type 'a graph = ('a * 'a) list 
    
let grafo = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6); (5,4); (6,5); (6,7)];;

            
let rec figliNodo nodo = function 
  |[] -> []
  | (x,y)::rest when (x=nodo)-> y::(figliNodo nodo rest)
  | (x,y)::rest -> figliNodo nodo rest;;

let ris= figliNodo 5 grafo;;

(*da vedere*)
(*let rec nodi acc = function 
    | [] -> acc
    | (x, y) :: rest when (List.mem y acc) && (List.mem x acc) -> nodi rest acc 
    | (x, y) :: rest when List.mem x acc -> nodi rest (y :: acc) 
    | (x, y) :: rest when List.mem y acc -> nodi rest (x :: acc)
    | (x, y) :: rest -> nodi (x :: y :: acc) rest *)
                        
(*trovare un cammino di un grafo *)

exception Notfound;;

let accept node = function 
  |[] -> false 
  | l-> node =List.hd l ;;

let rec pathfind node grafo acc figli =
  match figli with 
  |x::l when (accept node acc)-> acc
  |[]-> raise Notfound
  |x::l when (List.mem x acc) -> pathfind node grafo acc l 
  |x::l -> try pathfind node grafo (x::acc)((succ x grafo)@l) 
      with Notfound -> pathfind node grafo (x::acc)list.tl((succ x grafo)@l);;