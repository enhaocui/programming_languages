(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* val sqsum: int list -> int
    val f: int -> int list -> int
*)
let sqsum xs = 
  let f a x = a + x * x in
  let base = 0 in
    List.fold_left f base xs;;

(* val pipe: ('a -> 'a) list -> 'a -> 'a
*)
let pipe fs = 
  let f a x = fun x' -> x (a x') in
  let base = fun x -> x in
    List.fold_left f base fs;;

let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a^sep^x in
      let base = h in
      let l = t in
        List.fold_left f base l;;

let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]";;

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

let rec clone x n = 
  let rec helper x n a = 
    if n <= 0 then a
    else helper x (n - 1) (x::a)
    in helper x n [];;

let rec padZero l1 l2 = 
  let len1 = List.length l1 in
  let len2 = List.length l2 in
  if len1 < len2 then (clone 0 (len2 - len1) @ l1, l2)
  else (l1, clone 0 (len1 - len2) @ l2);;

let rec removeZero l = 
  match l with
  | 0::t -> removeZero t
  | _ -> l;;

(*
    let a = (carry, sum) in
    let x = (x1, x2) in
*)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 
      match a with 
      | (carry, sum) ->
      match x with 
      | (x1, x2) ->
      let temp = x1 + x2 + carry in 
      (temp / 10, temp mod 10 :: sum) in 
    let base = (0, []) in
    let args = List.combine (List.rev(0::l1)) (List.rev(0::l2)) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2));;

let rec mulByDigit i l = 
  if i = 0 then []
  else if i = 1 then l
  else bigAdd (bigAdd l l) (mulByDigit (i - 2) l);;

let bigMul l1 l2 = 
  let f a x = 
    match a with 
    | (zeros, acc) ->
      (zeros + 1, (bigAdd ((mulByDigit x l2) @ (clone 0 zeros)) acc)) in
  let base = (0, []) in
  let args = List.rev l1 in
  let (_, res) = List.fold_left f base args in
    res;;
