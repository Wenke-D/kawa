open Pimp



let tr_prog (prog: Kawa.program): Pimp.program =

  (* Dans la version minimale, [fields] contient tous les attributs de toutes
     les classes, et [methods] toutes les méthodes de toutes les classes. *)
  let fields = Hashtbl.create 32 in
  let methods = Hashtbl.create 32 in

  (**
     Fonction principale de traduction d'un bloc de code.

     Comme certaines constructions peuvent avoir besoin d'introduire une 
     variable locale, je propose une structure où une fonction principale
     [tr_code] effectue la traduction du code d'une méthode (ou du bloc de
     code [main]) et renvoie la liste des variables locales introduites à la 
     volée. Cette liste est étendue à chaque fois qu'une nouvelle variable 
     locale est introduite avec un appel à la fonction auxiliaire [new_local()].

     Dans cette structure, les fonctions [tr_expr], [tr_instr] et [tr_seq]
     faisant le cœur du travail de traduction sont définies à l'intérieur
     de [tr_code], et ont donc bien accès à [new_local].
   *)
  let tr_code code =
    let new_locals = ref [] in
    let new_local =
      let cpt = ref 0 in
      fun () -> incr cpt;
                let s = Printf.sprintf "local_%i" !cpt in
                new_locals := s :: !new_locals;
                s
    in

    let rec tr_expr e = failwith "not implemented"
    and tr_instr i = failwith "not implemented"
    and tr_seq s = failwith "not implemented"
    in

    tr_seq code, !new_locals
  in
  
  (** 
     Fonction de traduction d'une définition de classe. Il faut, au minimum :
     - ajouter les attributs et les méthodes aux tables [fields] et [methods],
       chacun ou chacune avec son décalage
     - traduire chacune des méthodes en une fonction PIMP
     - produire le code PIMP de création du descripteur de classe
     Ces trois tâches peuvent être réalisées par cette seule fonction, ou
     être séparées en trois fonctions.
   *)
  let tr_class cdef =
    failwith "not implemented"
  in

  {
    (* Remplacer par de vraies valeurs ! *)
    functions = [];
    globals = [];
  }
                
