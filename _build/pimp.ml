(**
   PIMP = IMP + pointeurs

   Partie I. Syntaxe abstraite
 *)

(* Opérations unaires : lecture en mémoire, et allocation *)
type unop = Read | Alloc
type binop = Add | Mul | Lt | Eq
                       
type expression =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Unop  of unop * expression
  | Binop of binop * expression * expression
  (* Appel de fonction *)
  | Call  of func * expression list
  (* Récupération de l'adresse d'une fonction *)
  | Addr  of string
  (* Inclusion d'une séquence d'instructions dans une expression *)
  | Seq   of sequence * expression

(* Une fonction est désignée soit par un identifiant, comme dans IMP,
   soit par un pointeur. *)
and func =
  | FName of string
  | FPointer of expression

and instruction =
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | Return  of expression
  | Expr    of expression
  (* Écriture en mémoire *)
  | Write   of expression * expression
             
and sequence = instruction list
             
type function_def = {
  name: string;
  code: sequence;
  params: string list;
  locals: string list;
}
    
type program = {
  functions: function_def list;
  globals: string list;
}

(**
   Partie II. Interprète
 *)
             
type value =
  | VInt of int
  | VBool of bool
  (* Pointeur sur le tas *)
  | VPointer of int
  (* Pointeur de fonction *)
  | FPointer of string
  | Undef

let vint_or_pointer = function
  | VInt n -> n
  | VPointer p -> p
  | _ -> assert false
let vbool = function
  | VBool b -> b
  | _ -> assert false
let fpointer = function
  | FPointer id -> id
  | _ -> assert false
              
exception EReturn of value
             
let exec_prog prog arg =

  (* Variables globales *)
  let global_env = Hashtbl.create 16 in
  List.iter (fun id -> Hashtbl.add global_env id Undef) prog.globals;

  (* Tas, représenté par une table de pages mémoire de 4096 octets chacune *)
  let memory = Hashtbl.create 16 in
  (* Première adresse libre du tas *)
  let mem_brk = ref 0 in
  (* Numéro de page d'une adresse *)
  let page a = a lsr 10 in
  (* Position d'une adresse à l'intérieur de sa page *)
  let index a = a land 0x3ff in
  (* Lecture en mémoire, on récupère la page puis on consulte la position *)
  let read a = match Hashtbl.find_opt memory (page a) with
    | Some p ->
       let v = p.(index a) in
       v
    (* Résultat [Undef] si la page n'a pas été créée *)
    | None -> Printf.printf "read undef at %i\n" a; Undef
  in
  (* Écriture en mémoire, on récupère la page puis on écrit à la position *)
  let write a v =
    let p = match Hashtbl.find_opt memory (page a) with
      | Some p -> p
      (* On crée la page si elle n'existait pas encore *)
      | None ->
         let p = Array.make 1024 Undef in
         Hashtbl.add memory (page a) p;
         p
    in
    p.(index a) <- v;
  in

  let rec exec_call f args =
    let fdef = List.find (fun fdef -> fdef.name = f) prog.functions in
    let local_env = Hashtbl.create 16 in
    List.iter2 (fun id arg -> Hashtbl.add local_env id arg) fdef.params args;
    List.iter (fun id -> Hashtbl.add local_env id Undef) fdef.locals;

    let rec exec_seq s =
      List.iter exec_instr s

    and exec_instr = function
      | Putchar e -> print_char (char_of_int (vint_or_pointer (eval_expr e)))
      | Set(id, e) ->
         let v = eval_expr e in
         if Hashtbl.mem local_env id then
           Hashtbl.replace local_env id v
         else
           Hashtbl.replace global_env id v
      | If(e, s1, s2) ->
         if vbool(eval_expr e) then
           exec_seq s1
         else
           exec_seq s2
      | While(e, s) as i ->
         if vbool(eval_expr e) then
           (exec_seq s; exec_instr i)
      | Return e -> raise (EReturn(eval_expr e))
      | Expr e -> ignore (eval_expr e)
      (* Écriture en mémoire, directement avec la fonction auxiliaire dédiée, en
         supposant que l'argument [e1] est un pointeur *)
      | Write(e1, e2) ->
         write (vint_or_pointer(eval_expr e1)) (eval_expr e2)

    and eval_expr = function
      | Cst n -> VInt n
      | Bool b -> VBool b
      | Var id -> begin
          match Hashtbl.find_opt local_env id with
          | Some v -> v
          | None -> Hashtbl.find global_env id
        end
      (* Lecture en mémoire, directement avec la fonction auxiliaire dédiée, en
         supposant que l'argument [e] est un pointeur *)
      | Unop(Read, e) -> read (vint_or_pointer (eval_expr e))
      (* Allocation d'un nouveau bloc en incrémentant [mem_brk]. Le résultat produit
         est l'adresse du bloc alloué. Ceci ne permet pas de désallocation. *)
      | Unop(Alloc, e) ->
         let n = vint_or_pointer(eval_expr e) in
         let brk = !mem_brk in
         mem_brk := brk + n;
         VPointer brk
      | Binop(op, e1, e2) ->
         let op = match op with
           | Add -> fun a b -> VInt (vint_or_pointer a + vint_or_pointer b)
           | Mul -> fun a b -> VInt (vint_or_pointer a * vint_or_pointer b)
           | Lt  -> fun a b -> VBool (vint_or_pointer a < vint_or_pointer b)
           | Eq  -> fun a b -> VBool (a = b)
         in
         op (eval_expr e1) (eval_expr e2)
      | Call(f, args) ->
         let fname = match f with
           | FName id -> id
           (* Dans le cas d'un pointeur de fonction, on récupère le nom correspondant *)
           | FPointer e -> (fpointer (eval_expr e))
         in
         exec_call fname (List.map eval_expr args)
      (* On garde le nom [id] pour identifier un pointeur de fonction *)
      | Addr id -> FPointer id
      (* Inclusion d'instructions dans une expression : on exécute d'abord les
         instructions, puis on calcule la valeur de l'expression *)
      | Seq(s, e) ->
         exec_seq s; eval_expr e
                 
    in

    try
      exec_seq fdef.code; Undef
    with
      EReturn v -> v
    
  in

  exec_call "main" [arg]
  
