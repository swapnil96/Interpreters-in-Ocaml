
(*type variable = Var of string ;;
type const = Cons of string;;*)

(*type environment = variable * const list;;*)

(*type stack = *)
type expression = 
  | Integer of int
  | Var of string
  | Add of expression * expression
  | Lambda of string * expression
  | App of expression * expression

and instruction = 
  | IConst of int
  | IAdd
  | IAccess of int
  | IClosure of code
  | IApply
  | ITailApply
  | IReturn
  | IVarFree of int

and value = 
  | VVarFree of int
  | VInt of int
  | VClosure of code * env
  | VCode of code
  | VEnv of env

and env = value list
and code = instruction list
;;
(* and machine = int list * instr list *)

type dvar = 
  | Free of int
  | Bounded of int
;;
type dterm = 
  | DInt of int
  | DVar of dvar
  | DAbs of dterm
  | DApp of dterm * dterm
  | DAdd of dterm * dterm
  (*| DSub of dterm * dterm*)
  (*| DMul of dterm * dterm*)
;;

let rec tcompile = function
  | DApp (e1, e2) -> (compile e1) @ (compile e2) @ [ITailApply]
  | _ as a -> (compile a) @ [IReturn]

and compile = function
  | DInt n -> [IConst n]
  | DVar (Bounded n) -> [IAccess n]
  | DVar (Free n) -> [IVarFree n]
  | DAbs e -> [IClosure ((tcompile e))]
  | DAdd (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
  | DApp (e1, e2) -> (compile e1) @ (compile e2) @ [IApply]


module Env = Map.Make(struct type t = string let compare = String.compare end)

let dterm_of_term envb term =
    let envf = ref Env.empty in
    let rec dterm_of_term envb = function
        | Integer n     ->  DInt n
        | Add (e1, e2)  ->  DAdd (dterm_of_term envb e1, dterm_of_term envb e2)
        | Var s         ->  if Env.mem s envb then
                                (DVar (Bounded (Env.find s envb)))
                            else
                                if Env.mem s !envf then
                                    (DVar (Free (Env.find s !envf)))
                                else
                                    begin
                                    let n = Env.cardinal !envf in
                                    envf := Env.add s n !envf ;
                                    (DVar (Free n))
                                end 
        | Lambda (s, t) ->  DAbs (dterm_of_term (Env.add s 0 (Env.map succ envb)) t)
        | App (t1, t2)  ->  DApp (dterm_of_term envb t1, dterm_of_term envb t2)
    in
    dterm_of_term envb term
;;

let rec eval (instructions, env, stack) = 
  match instructions with
    | []                ->  print_endline "(Empty)" ;
                            List.hd stack
    | IConst n :: rest  ->  print_endline "IConst" ;
                            eval (rest, env, VInt(n) :: stack)
    | IAdd :: rest      ->  print_endline "IAdd" ;
                            begin
                            match stack with
                                | VInt(n2) :: VInt(n1) :: stack ->  eval (rest, env, VInt(n1 + n2) :: stack)
                                | _                             -> failwith "Add needs two arguments"
                            end
    | IClosure c' :: c  ->  print_endline "IClosure" ;
	                        eval (c, env, VClosure (c', env) :: stack)
    | IApply :: c       ->  print_endline "IApply" ;
                            begin
                            match stack with
                                v :: VClosure (c', env') :: stack   ->    eval (c', v :: env', stack)
                                | _                                 -> failwith "IApply"
                            end
    | ITailApply :: c   ->  print_endline "ITailApply" ;
                            begin
                                match stack with
                                v :: VClosure (c', env') :: stack   ->  eval (c', v :: env', VCode(c) :: VEnv(env) :: stack)
                                | _                                 -> failwith "IApply"
                            end
    | IReturn :: _      ->  print_endline "IReturn" ;
                            begin
                            match stack with
                                | v :: VCode(c') :: VEnv(e') :: s   ->  eval (c', e', v :: s)
                                | _                                 -> failwith "IReturn"
                            end
    | IVarFree n::rest  ->  print_endline "IVarFree" ;
                            eval (rest, env, VVarFree n :: stack)
    | IAccess n :: rest ->  print_endline "IAccess" ;
	                        eval (rest, env, (List.nth env n) :: stack)


let i1 = [ Add(Integer(10), Integer(11))]
