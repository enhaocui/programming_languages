(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

let rec assoc (d,k,l) = 
  match l with
  | [] -> d
  | (key, value)::t -> if key = k then value else assoc (d,k,t);;

(* fill in the code wherever it says : failwith "to be written" *)
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


(* Small hint: see how ffor is implemented below *)
let rec wwhile (f,b) = 
  let res = f b in
    match res with
    | (b', c') -> if c' then wwhile (f, b') else b';;

(* fill in the code wherever it says : failwith "to be written" *)
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
