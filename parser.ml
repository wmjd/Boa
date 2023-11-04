open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr

let boa_max = Int64.of_int (int_of_float(2.**62.) - 1);;
let boa_min = Int64.of_int (-int_of_float(2.**62.));;

(* Defines rules for what ids are valid -- ids must match the regex and not
 * be a reserved word *)
let valid_id_regex = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let number_regex = Str.regexp "^[-]?[0-9]+"
let reserved_words = ["let"; "add1"; "sub1"; "isNum"; "isBool"; "if"]
let reserved_constants = ["true"; "false"; ]

let check_reserved word = 
  if (List.fold_left (fun acc rw -> (rw = word) || acc) false reserved_words)
  then failwith ("Syntax error: " ^ word ^ " is a reserved word")
  else word    

let int_of_string_opt s =
  if Str.string_match valid_id_regex s 0 then None else
  if Str.string_match number_regex s 0 then
    match Int64.of_string_opt s with 
    | Some(x) when x <= boa_max && x >= boa_min -> Some(x)
    | _ -> failwith "Non-representable number"
  else failwith "Invalid identifier"

let rec parse (sexp : Sexp.t) : Expr.expr =
  match sexp with
    | Atom("true") -> EBool(true)
    | Atom("false") -> EBool(false)	
    | Atom(s) ->
      (match int_of_string_opt s with
        | None -> EId(s) 
        | Some(i) -> ENumber(i))
    | List(sexps) ->
      match sexps with
        | [Atom("add1"); arg] -> EPrim1(Add1, parse arg)
        | [Atom("sub1"); arg] -> EPrim1(Sub1, parse arg)
        | [Atom("isNum"); arg] -> EPrim1(IsNum, parse arg)
        | [Atom("isBool"); arg] -> EPrim1(IsBool, parse arg)
        | [Atom("+"); arg1; arg2] -> EPrim2(Plus, parse arg1, parse arg2)
        | [Atom("-"); arg1; arg2] -> EPrim2(Minus, parse arg1, parse arg2)
        | [Atom("*"); arg1; arg2] -> EPrim2(Times, parse arg1, parse arg2)
        | [Atom("<"); arg1; arg2] -> EPrim2(Less, parse arg1, parse arg2)
        | [Atom(">"); arg1; arg2] -> EPrim2(Greater, parse arg1, parse arg2)
        | [Atom("=="); arg1; arg2] -> EPrim2(Equal, parse arg1, parse arg2)
        | [Atom("let"); binding; body] ->
          ELet(parse_binding binding, parse body)
        | [Atom("if"); predicate; if_branch; else_branch] -> EIf(parse predicate, parse if_branch, parse else_branch)
        | _ -> failwith "Parse error"

and parse_binding (binding : Sexp.t) : (string * Expr.expr) list =
  match binding with
    | List([List([Atom(name); value])]) -> [(check_reserved name, parse value)]
    | List((List([Atom(name); value]))::more) -> ((check_reserved name, parse value)::(parse_binding (List more)))
	| _ -> failwith "Parse bindings error"
