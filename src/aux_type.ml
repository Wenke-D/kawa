(* auxiliary declarations and shorcuts *)

(** function name -> offset *)
type descr_layout = (string, int) Hashtbl.t
(** attribute name -> offset *)
type obj_layout = (string, int) Hashtbl.t

type varDclr = (string * Kawa.typ)

type existe_vars = (string, Kawa.typ) Hashtbl.t

type var_typ =
  |SimpleVar of string (** name of variable *)
  |Pointor of Pimp.expression (** pointor to the expr *)

type value_typ =
  |SimpleVal
  |Addr

let fatal_error cause = failwith (Format.sprintf "Fatal error: %s" cause)


let init_function_name = "__initClasses"