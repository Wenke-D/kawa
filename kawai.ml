let () = Printexc.record_backtrace true

open Format

let () =
  let file = Sys.argv.(1) in
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Kawaparser.program Kawalexer.token lb in
  close_in c;
  let pimp = Kawa2pimp.tr_prog prog in
  ignore(
    Pimp.(
      exec_prog 
        pimp 
        Undef
        (* (VInt (int_of_string Sys.argv.(2))) *)
    )
  );
  exit 0
