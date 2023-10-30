open Printf
open Expr
open Asm

let rec find ls x =
  match ls with
  | [] -> None
  | (y,v)::rest ->
    if y = x then Some(v) else find rest x

let stackloc si = RegOffset(-8 * si, RSP)

let true_const  = HexConst(0x0000000000000002L)
let false_const = HexConst(0x0000000000000000L)
                
let rec well_formed_e (e : expr) (env : (string * int) list) : string list =
  let rec ext_env b = 
    match b with
    | [] -> []
    | (x, _)::more -> (x, 0)::(ext_env more)  
  in let check_duplicates b =
    let rec dup b x =
      match b with
      | [] -> []
      | (x_prime, v)::more -> if x_prime = x then ["Multiple bindings for variable identifier " ^ x ^ " "] else dup more x  
    in let rec walk b = 
      match b with 
      | [] -> []
      | (x, v)::more -> (dup more x) @ (well_formed_e v env) @ (walk more) 
    in walk b
  in match e with
  | ENumber(_)
  | EBool(_) -> []
  | ELet(binding, body) -> (check_duplicates binding) @ (well_formed_e body ((ext_env binding) @ env))
  | EId(x) -> (
    match find env x with
    | None -> ["Variable identifier " ^ x ^ " unbound"] 
    | Some(_) -> [] )

 (* TODO *)  
  | _ -> failwith "Not yet implemented: well_formed_e"

let check (e : expr) : string list =
  match well_formed_e e [("input", -1)] with
  | [] -> []
  | errs -> failwith (String.concat "\n" errs)

let rec compile_expr (e : expr) (si : int) (env : (string * int) list) : instruction list =
  match e with
  | EPrim1(op, e) -> compile_prim1 op e si env
  | EPrim2(op, e1, e2) -> compile_prim2 op e1 e2 si env
  | ELet(binding, body) -> 
    let vis, ext_env = compile_binding binding si env in
	let bis = compile_expr body (si + List.length binding) ext_env in
	vis @ bis
  | ENumber(i) -> [IMov(Reg RAX, Const i)]
  | EId(x) -> (
    match find env x with
    | None -> failwith ("compile_expr: Unbound variable identifier " ^ x) (* this should be caught before compilation in check and should never execute here *)
    | Some(i) -> [IMov(Reg RAX, stackloc i)] )

(* Tail Recursive implementation needs to *reverse* the instruction list as usual trick. 
In this case, it is a little tricky because ins is built hierarchically in sections and subsections which must remain ordered *)
and compile_binding b si env = 
  let rec iter b si env ins =
    match b with
    | [] -> (List.rev ins, env)
    | (x,v)::more -> 
      let vis = compile_expr v si env in 
      let sis = [IMov(stackloc si, Reg RAX)] in
      iter more (si+1) ((x,si)::env) (sis @ (List.rev vis) @ ins)  
  in iter b si env []

and compile_prim1 op e si env =
  (* TODO *)
  failwith "Not yet implemented: compile_prim1"

and compile_prim2 op e1 e2 si env =
  (* TODO *)
  failwith "Not yet implemented: compile_prim2"

let compile_to_string prog =
  let _ = check prog in
  let prelude = "  section .text\n" ^
                "  extern error\n" ^
                "  global our_code_starts_here\n" ^
                "our_code_starts_here:\n" ^
                "  mov [rsp - 8], rdi\n" in
  let postlude = [IRet]
    (* TODO *) in
  let compiled = (compile_expr prog 2 [("input", 1)]) in
  let as_assembly_string = (to_asm (compiled @ postlude)) in
  sprintf "%s%s\n" prelude as_assembly_string
