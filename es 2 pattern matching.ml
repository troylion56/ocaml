

(*Esercizio 5. Mediante pattern matching definire una funzione di tipo 𝑖𝑛𝑡 × 𝑖𝑛𝑡 → 𝑖𝑛𝑡 che applicata a una coppia di interi (𝑛, 𝑚) restituisce
 • true per qualsiasi coppia (0, 𝑚) quando 𝑚 ≠ 0.
 • true per qualsiasi coppia (𝑛, 𝑚) quando 𝑚 ∈ {3, 4}.
 • true per la coppia (1, 0).
 • false in qualsiasi altro caso. *)

let es5 (n,m) =
  match (n,m) with
  | (0,_) when (not(m=0))-> true 
  | (_,_) when (m>=3 && m<=4) -> true 
  | (1,0) -> true 
  | (_,_) -> false ;;