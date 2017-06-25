
type variable = Var of string;;

type expression = V of variable | Lambda of (string * expression) | N of expression * expression | Add of expression * expression | Cons of int ;;

type opcode = ACCESS of variable | CLOS of variable * opcode list | ADD | MUL | SUB | APPLY | CONST of int | V of variable | RETURN | LET of variable | ENDLET ;;

type value =  Vint of int | Vclosure of variable * environment * (opcode list) | Vcode of opcode list | Venv of environment 

and environment = (variable * value) list
;;

type code = opcode list ;;

let push e s =
    e :: s
;;

exception Error;;

let rec compile e = match e with
    | Lambda (x, e1)    ->  [CLOS( Var(x), compile e1 @ [RETURN]) ]
    | N(e1, e2)         ->  compile (e1) @ compile (e2) @ [APPLY]
    | V(v1)             ->  [ACCESS(v1)] 
    | Add(e1, e2)       ->  compile(e1) @ compile(e2) @ [ADD]
    | Cons(i)           ->  [CONST(i)]
;;

let rec find t e n = match e with 
    |   []          ->  raise Error
    |   (a, b)::c   ->  if t = a then 
                            n
                        else
                            find t c (n+1)
;;

let rec eval c e s = match c with
    | []                    ->  print_string "Stack empty, calculation done"; List.hd s
    | ACCESS(v1)::c'        ->  let t1 = find v1 e 0 in
                                (*begin
                                match t1 with
                                    | Vint(i)   ->  let t1 = List.nth e i in
                                                    begin
                                                    match t1 with 
                                                        | (a, b)    ->  eval c' e (push (b) s)
                                                    end
                                    | _         ->  raise Error
                                end*)
                                let t2 = List.nth e t1 in
                                begin
                                match t2 with 
                                    | (a, b)    ->  eval c' e (push (b) s)
                                end
                                (*eval c' e (push (t1) s)*)
    | LET(v1)::c'           ->  eval c' (push (v1, (List.hd s)) e) (List.tl s)
    | ENDLET::c'            ->  eval c' (List.tl e) s
    | CLOS(x, c')::c''      ->  eval c'' e (push (Vclosure(x, e, c')) s)
    | APPLY::c'             ->  begin   
                                    match s with 
                                        | v :: Vclosure(x, e', c'') :: s'   ->  eval c'' (push (x, v) e') (push (Vcode(c')) (push (Venv(e)) s'))
                                        | _                                 ->  raise Error
                                end
    | RETURN::c'            ->  begin   
                                    match s with 
                                        | v :: Vcode(c'') :: Venv(e') :: s' ->  eval c'' e' (push v s')
                                        | _                                 ->  raise Error
                                end
    | ADD::c'               ->  begin 
                                    match s with 
                                        | Vint(v1) :: Vint(v2) :: s'    ->  eval c' e (push (Vint(v1+v2)) s')
                                        | _                             ->  raise Error
                                end
    | CONST(a)::c'          ->  eval c' e (push (Vint(a)) s) 
    | MUL::c'               ->  begin 
                                    match s with 
                                        | Vint(v1) :: Vint(v2) :: s'    ->  eval c' e (push (Vint(v1*v2)) s')
                                        | _                             ->  raise Error
                                end
    | SUB::c'               ->  begin 
                                    match s with 
                                        | Vint(v1) :: Vint(v2) :: s'    ->  eval c' e (push (Vint(v1 - v2)) s')
                                        | _                             ->  raise Error
                                end

    | _                     ->  raise Error
;;     

(*fun x -> x + 2 *)
(*let c1 = [CLOS(Var("x"), [ACCESS(0); CONST(2); ADD; RETURN]); CONST(5); APPLY];;*)
let c1 = [CLOS(Var("x"), [ACCESS(Var("x")); CONST(2); ADD; RETURN]); CONST(5); APPLY];;

(*fun x -> x * 2 *)
let c2 = [CLOS(Var("x"), [ACCESS(Var("x")); ACCESS(Var("x")); MUL; RETURN]); CONST(5); APPLY];;

(*fun x -> x + 2 *)
let c3 = [CLOS(Var("x"), [ACCESS(Var("x")); CONST(2); SUB; RETURN]); CONST(5); APPLY];;
(*let c4 = [LET; CLOS(Var("x"), [ACCESS(0); CONST(2); SUB; RETURN]); CONST(5); APPLY];;*)

 (*let c4 = [LET(Var("y")); CLOS(Var("x"), [ACCESS(Var("x")); CONST(2); ADD; RETURN]); ENDLET; LET(Var"y"); V(Var("x")); ENDLET; APPLY];;*)


let t1 = eval c1 [] [];;
let t2 = eval c2 [] [];;
let t3 = eval c3 [] [];;
(*let t4 = eval c4 [] [];;*)


let e1 = Lambda("x", Add(V(Var("x")), Cons(2)));;
let e2 = N(Lambda("x", Add(V(Var("x")), Cons(2))), Cons(5));;

let c1 = compile e1;;
let c2 = compile e2;;
let e4 = eval c2 [] [];;
