open Pimp
open Aux_type

let fail name = failwith (Format.sprintf "%s not implemented" name)



(* Global variables *)
(** layouts of all descriptor  *)
let all_descr:(string, descr_layout) Hashtbl.t = Hashtbl.create 32
(** obj layouts of all class  *)
let all_obj:(string, obj_layout) Hashtbl.t = Hashtbl.create 32
(** definition of all class *)
let all_class:(string, Kawa.class_def) Hashtbl.t = Hashtbl.create 32

let local_var = "local_99"


(** Find type of an abitray expr  *)
let rec typeOfExpr (expr:Kawa.expr) (context:existe_vars):Kawa.typ =
  match expr with
  |Cst n -> Typ_Int
  |Bool b -> Typ_Bool
  |Binop (op, e1, e2) ->
    begin
      match op with
      |Add -> Typ_Int
      |Mul -> Typ_Int
      |_ -> Typ_Bool
    end
  |Get mem ->
    (
      match mem with
      |Var var -> Hashtbl.find context var
      |Field(expr', attr) ->
        (
          match typeOfExpr expr' context with
          |Typ_Class clazz -> 
          (
            match Hashtbl.find_opt all_class clazz with
            |None -> fatal_error (Aux.spf "class %s not defined" clazz)
            |Some res -> Typ_Class res.class_name
          )
          |_ -> fatal_error (Aux.spf "access attr %s from a non-obj" attr)
        ) 
    )
  |This ->  Hashtbl.find context "self"
  |New(clazz, args) -> Typ_Class clazz
  |MethCall(obj, name, args) ->
    (
      match typeOfExpr obj context with
      |Typ_Class clazz -> 
        (
          match Hashtbl.find_opt all_class clazz with
          |None -> fatal_error (Aux.spf "class %s not defined" clazz)
          |Some res -> Typ_Class res.class_name
            
        )
      |_ -> fatal_error (Aux.spf "Call method %s from a non-obj" name)
    )
    

let classOfExpr expr context =
  match typeOfExpr expr context with
  |Typ_Class clazz -> clazz
  |_ -> fatal_error "not a class"


(** Calculating memory access, simple variable or pointer offset *)
let access_mem (acc:Kawa.mem_access) (context:existe_vars):var_typ =
  match acc with
  |Var var ->
    begin
    match Hashtbl.find_opt context var with
    |None -> fatal_error (Aux.spf "Can't find %s when access mem" var)
    |Some(Typ_Class _) -> Pointor(Var var)
    |Some _ -> SimpleVar var
    end

  |Field (obj, field) -> let obj_className = Aux.classNameOfObj obj context
  in
  let obj_layout = 
    match Hashtbl.find_opt all_obj obj_className with
    |None -> fatal_error (Aux.spf "Can't find obj layout for class %s" obj_className)
    |Some res -> res      
    in
    let attr_offset =
      match Hashtbl.find_opt obj_layout field with
      |None -> fatal_error (Aux.spf "Can't find offset of attribute:%s" field)
      |Some res -> res
      in 
      let name = Var (Aux.varnameOfExpr obj)
        in
        Pointor(Binop(Add, name, Cst attr_offset))


(** traduire les exprssions *)
let rec tr_expr
  (** all existe var, params and locals *)
  (vars:(string, Kawa.typ) Hashtbl.t)
  (** expression to be translated *)
  (expr:Kawa.expr)
  :Pimp.expression =
    (* partiel eval as a shortcut *)
    let tr_expr' = tr_expr vars
    in
    match expr with
      |Cst n                      -> Cst n
      |Bool n                     -> Bool n
      |Binop(binop, e1, e2)       -> Aux.tr_binop binop (tr_expr' e1) (tr_expr' e2) 
      (* Accès à une variable ou un attribut
        need to check simple variable or obj variable      
      *)
      |Get(mem)                   ->  
          (match access_mem mem vars with
          |Pointor p -> Unop(Read, p)
          |SimpleVar v -> Var v)

      |This                       -> Var ("self")
      
      |New(className, args)       -> 
        let obj_size = Aux.objSizeOfClass className all_obj
        and class_descr_name = Aux.make_descriptor_name className
        and class_constructor_name = Aux.make_constructor_name className
        in
        let new_mem = Unop(Alloc, Cst obj_size) 
        in
        let alloc_new_mem = Set(local_var, new_mem)
        and write_descr = Write(Var local_var, Var class_descr_name)
        and call_constructor = 
          Expr(Call(FName(class_constructor_name), Var local_var :: List.map tr_expr' args)) 
        in
          Seq([alloc_new_mem; write_descr; call_constructor], Var local_var)

      |MethCall(obj, name, args) ->
        let className = Aux.classNameOfObj obj vars 
        in
        let descr_layout = 
          match Hashtbl.find_opt all_descr className with
          |None -> fatal_error (Aux.spf "Can't find descr layout of class %s" className)
          |Some r -> r
        in
        let func_offset =
          match Hashtbl.find_opt descr_layout name with
          |None -> fatal_error (Aux.spf "Can't find offset of func %s" name)
          |Some r -> r
        in
        let func_pointor = Unop(Read, Binop(Add, Cst func_offset, (tr_expr' obj)))
        in
        Call(FPointer(func_pointor), List.map tr_expr' (obj::args))



let rec tr_instr (vars: existe_vars) (instr:Kawa.instr) :Pimp.instruction =
  let tr_expr' = tr_expr vars
  and tr_instr' = tr_instr vars
  in
  match instr with
    |Putchar expr -> Putchar(tr_expr' expr)

    |If (expr, seq1, seq2) ->
      If(tr_expr' expr, List.map tr_instr' seq1, List.map tr_instr' seq2)
    
    |While (expr, seq) -> While(tr_expr' expr, List.map tr_instr' seq)

    |Return expr -> Return(tr_expr' expr)

    |Expr expr -> Expr(tr_expr' expr)

    (* Écriture dans une mémoire *)
    |Set (mem, expr) -> 
      let dst = access_mem mem vars
      and src = tr_expr' expr
      in
      match dst with
      |SimpleVar var -> Set(var, src)
      |Pointor p -> Write(p, src)
      
    
    (* Écriture dans un attribut, l_expr.field = r_expr *)
    (* |Set (Field(l_expr, field), r_expr) -> 
      let objClassName = Aux.classNameOfObj l_expr vars
      in
      let obj_layout =
        match Hashtbl.find_opt all_obj objClassName with
        |None -> fatal_error (Aux.spf "Can't find obj layout for class '%s' when setting" objClassName)
        |Some res -> res
      in
      let attr_offset =
        match Hashtbl.find_opt obj_layout field with
        |None -> fatal_error (Aux.spf "Can't find offset for attr '%s' when setting" field)
        |Some res -> res
      in
      Write(Binop(Add, Cst attr_offset, (tr_expr' l_expr)), tr_expr' r_expr) *)
  
let tr_class (cdef:Kawa.class_def) =
    let attrs = Aux.fill_attr cdef.attributes
    (* Suppose all class only have simple attr *)
    (* TODO supresse supposion *)
    and obj_size = (List.length cdef.attributes + 1) * Kawa.mem_offset
    and methods = Aux.fill_method cdef.methods 
    and class_size = (List.length cdef.methods + 1) * Kawa.mem_offset
    in
      let tr_method (m_def:Kawa.method_def):Pimp.function_def =
        let vars = Hashtbl.create 32
        in
        begin
          Aux.varDeclrListToTable m_def.params vars;
          Aux.varDeclrListToTable m_def.locals vars;
          Hashtbl.add vars "self" (Typ_Class(cdef.class_name));
          Aux.print_existe_var vars;
          let tr_code (code:Kawa.seq):(Pimp.sequence) =
            List.map (fun instr -> tr_instr vars instr ) code
          in (* end of tr_code *)
          {
            name = Aux.spf "%s_%s" cdef.class_name m_def.Kawa.method_name;
            code = tr_code m_def.code;
            params= "self" :: List.map (fun p -> let (n, _) = p in n) m_def.params;
            locals= List.map (fun l -> let (n, _) = l in n) m_def.locals;
          }
        end
      in (* end of tr_method *)
  (* begin of tr_class body *)
    let cur_class_obj_layout = Hashtbl.create 32
    in
    begin
      (* 1. record obj layout*)
      List.iteri 
        (fun i (attr, _) -> Hashtbl.add cur_class_obj_layout attr ((i + 1) * Kawa.mem_offset) )
        cdef.attributes
      ;
      Hashtbl.add all_obj cdef.class_name cur_class_obj_layout;
      (* 2. TODOs *)
      let
        (* TODO create class descriptor *)
        class_descr_data = Aux.make_descriptor cdef
        in
        (** compiled method + descritor data*)
        (List.map tr_method cdef.methods, class_descr_data)
        (* end of tr_class *)
    end
    

let tr_main main globals =
  let globalVars = Hashtbl.create 32 in
  let compiled_main_seq =
  begin 
    Aux.varDeclrListToTable globals globalVars;
    List.map (fun instr_main -> tr_instr globalVars instr_main ) main 
  end
  in
  let call_init = Expr(Call(FName init_function_name, []))
  in
  {
    name = "main";
    code = call_init :: compiled_main_seq;
    params = [];
    locals = ["local_99"];
  }



let tr_prog (prog:Kawa.program):Pimp.program =
  
  (* Corp of tr_prog begins here *)
  (* compilation of classes *)
  let compiled_classes = List.map tr_class prog.classes
  in
  let methods = List.concat (List.map (fun cc -> let methods, _ = cc in methods) compiled_classes)
  and descr_metas = List.map (fun cc -> let _, meta = cc in meta) compiled_classes
  in
  let descriptors = (List.map (fun meta -> meta.Aux.descr_name) descr_metas)
  and init_descr_func = Aux.descripteur_generation_function descr_metas
  in
  (* compilation of main *)
  let mainMethod = tr_main prog.main prog.globals
  in
  {
    functions = methods @ [init_descr_func; mainMethod];
    globals= descriptors @ List.map (fun (name,typ) -> name) prog.globals
  }