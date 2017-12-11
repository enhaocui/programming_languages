(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

(* assoc : int * string * (string * int) list -> int
Simply check if the key of the head of current list is what we want. If the list is empty, 
we know that key does not exist in the list, so return []
*)
let rec assoc (d,k,l) = 
  match l with
  | [] -> d
  | (key, value)::t -> if key = k then value else assoc (d,k,t);;

(* removeDuplicates : int list -> int list 
use helper function to check if the remaining part of the list has elements equal to h, 
if it does, we simply remove the head, otherwise we know the h is a unique value in the list and we should keep it.
*)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l));;


(* wwhile : (int -> int * bool) * int -> int
we execute f b, and when f returns false, we stop, otherwise, we continue to execute f(f(b)) 
*)
let rec wwhile (f,b) = 
  let res = f b in
    match res with
    | (b', c') -> if c' then wwhile (f, b') else b';;

(* fixpoint : (int -> int) * int -> int
We use a helper function g to check if f b = b, if it does, return a tuple with false, else return the tuple with true. 
And wwhile can use this helper function to loop until we reach a fix point
*)
let fixpoint (f,b) = wwhile ((
  let g b' = 
    if b' = f b' then (b', false) else (f b', true)
  in g),b);;


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
