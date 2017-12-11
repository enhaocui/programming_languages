(*
Enhao Cui
A53202267
*)

exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(* 
lookup: string * env -> value
finds the most recent binding of a variable in the list representing the enviroment
*)
let lookup (x,evn) = 
  let t = listAssoc (x,evn) in
  match t with
  | None -> raise (MLFailure("Variable not bound: " ^ x))
  | Some y -> y

(* 
eval : env * expr -> value
evaluates e and raises expection for an unbound type
 *)
let rec eval (evn,e) = 
  match e with
  | Const c -> Int c
  | Var v -> lookup (v, evn)
  | True -> Bool true
  | False -> Bool false
  | Bin(e1, op, e2) -> 
    (match ((eval(evn, e1)), op, (eval(evn, e2))) with
     | Int i1, Plus, Int i2 -> Int (i1 + i2) 
     | Int i1, Minus, Int i2 -> Int (i1 - i2) 
     | Int i1, Mul, Int i2 -> Int(i1 * i2)
     | Int i1, Div, Int i2 -> Int (i1 / i2)
     | Int i1, Eq, Int i2 -> Bool (i1 = i2)
     | Int i1, Ne, Int i2 -> Bool (i1 != i2)
     | Int i1, Lt, Int i2 -> Bool (i1 < i2)
     | Int i1, Le, Int i2 -> Bool (i1 <= i2)
     | Bool b1, And, Bool b2 -> Bool (b1 && b2)
     | Bool b1, Or, Bool b2 -> Bool (b1 || b2)
     | Bool b1, Ne, Bool b2 -> Bool (b1 != b2)
     | Bool b1, Eq, Bool b2 -> Bool (b1 = b2)
     | _ -> raise (MLFailure ("Invalid binop")))
  | If (e1, e2, e3) -> 
    (match eval(evn, e1) with
     | Bool b -> if b then eval (evn, e2) else eval (evn, e3)
     | _  -> raise (MLFailure ("Invalid if")))
  | Let (e1, e2, e3) -> eval ((e1, eval(evn, e2))::evn, e3)
  | Letrec (e1, e2, e3) -> eval ((e1, eval(evn, e2))::evn, e3) 
  | App (e1, e2) -> 
    (let arg = eval(evn, e2) in
      (match eval(evn,e1) with
      | Closure(evn', None, x, e) -> 
        eval((x, arg)::evn'@evn, e)
      | Closure(evn', Some y, x, e) as f ->
        eval((y, arg)::(x,f)::evn'@evn, e)))
  | Fun (e1, e2) -> Closure(evn, None, e1, e2)
  | _ -> raise (MLFailure("invalid input"))


(**********************     Testing Code  ******************************)
