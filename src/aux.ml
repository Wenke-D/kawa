(** Fonctions auxiliaires *)
open Aux_type

(** shortcut of sprintf *)
let spf = Format.sprintf

(** meta data of a class descriptor (without parent class) *)
type init_class = {
  descr_name: string;
  offsets: (int * string) list; (** function offset and its name*)
  size: int; (** size of class descriptor *)
}


let make_descriptor_name class_name = spf "%s_descriptor" class_name

let make_constructor_name class_name = spf "%s_constructor" class_name


let fill iterable putter =
  let table = Hashtbl.create 32 in
  let putter' = putter table in
  begin
    List.iteri 
    putter'
    iterable;
    table
  end 

(** 把method list 转换为对应offset 表 *)
let fill_method (methods:Kawa.method_def list) =
  fill 
    methods
    (fun table i e -> Hashtbl.add table (e.Kawa.method_name) (i * Kawa.mem_offset))

(** attr list 转换为对应offset 表 *)
let fill_attr (attrs:(string * Kawa.typ) list) =
  fill
    attrs
    (fun table i e -> let (name, _) = e in
      Hashtbl.add table name (i * Kawa.mem_offset))

let tr_binop (op:Kawa.binop) (e1:Pimp.expression) (e2:Pimp.expression)
:Pimp.expression = 
let pimp_op = match op with
|Add -> Pimp.Add
|Mul -> Pimp.Mul
|Lt -> Pimp.Lt
|Eq -> Pimp.Eq
in
Binop(pimp_op, e1, e2)

(** 
  Compute metadata of class decriptor from class definition.
  Assmue the first method of class is constructor and we will skip it.
*)
let make_descriptor (clazz:Kawa.class_def):init_class =
  let name = spf "%s_descriptor" clazz.class_name
  and offsets =
    List.mapi 
      (fun i m ->
        Format.printf "method: %s\n" m.Kawa.method_name;
        let fullname = spf "%s_%s" clazz.class_name m.Kawa.method_name in
        ((i+1) * Kawa.mem_offset, fullname)
      )
      (List.tl clazz.methods) 
    in
      {
        descr_name = name;
        offsets = offsets;
        size = (List.length clazz.methods) * Kawa.mem_offset
      }


(** Generate the function that initializes all class descriptors *)
let descripteur_generation_function (meta_descrs: init_class list):Pimp.function_def =
    let tr_descr (meta: init_class) =
      let alloc = Pimp.Set(meta.descr_name, Unop(Alloc, Cst meta.size))
      and write_functions = 
        List.map
          (
            fun offset_name -> let offset, name = offset_name in
              Pimp.Write(Binop(Add, Var meta.descr_name, Cst offset), Addr name);
          )
          meta.offsets
      in alloc::write_functions
    in let seq = List.concat (List.map tr_descr meta_descrs)
  in
  {
    name = init_function_name;
    code = seq;
    params = [];
    locals = [];
  }

let varnameOfExpr var = match var with
|Kawa.Get(Var varname) -> varname
|Kawa.This -> "self"
|_ -> fatal_error "target expr is not a variable"

let rec classNameOfObj obj vars =
  (* suppose no chain style of field access, such as a.b.c *)
    match obj with
    |Kawa.Get(Var var) ->
      let var_type = Hashtbl.find_opt vars var in
        (
          match var_type with
          |None -> fatal_error (spf "use undefined variable '%s'" var)
          |Some(Kawa.Typ_Class(className))  -> className
          |_ -> fatal_error "access to filed of non obj"
        )
    |Kawa.This -> classNameOfObj (Get(Var "self")) vars
    |_ -> fatal_error " access to field invalid"

let objSizeOfClass (className:string) (obj_layouts:(string, (string, int) Hashtbl.t)Hashtbl.t) =
  let obj_layout = Hashtbl.find_opt obj_layouts className  in
  match obj_layout with
  |None -> fatal_error (spf "class: %s undefined !" className)
  |Some(layout) -> (Hashtbl.length layout + 1) * Kawa.mem_offset


let varDeclrListToTable (varsDeclrs:varDclr list) (target_table:existe_vars) =
  List.iter (fun (name, typ) -> Hashtbl.add target_table name typ) varsDeclrs


let print_ht k_s v_s ht = 
  print_string "=============\n";
  Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" (k_s x) (v_s y)) ht;
  print_string "=============\n"


let print_existe_var =
  print_ht
  (fun x -> x)
  (
    fun (t:Kawa.typ) -> 
    (match t with
    | Typ_Void -> "void"
    | Typ_Int -> "int"
    | Typ_Bool -> "bool"
    | Typ_Class c -> c)
  )


let read_mem (mem:Pimp.expression) :Pimp.expression = Unop(Read, mem)

  




    