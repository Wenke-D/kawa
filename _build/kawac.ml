open Format

let () =
  let file = Sys.argv.(1) in
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Kawaparser.program Kawalexer.token lb in
  close_in c;
  let pimp = Kawa2pimp.tr_prog prog in
  let output_file = (Filename.chop_suffix file ".kwa") ^ ".pmp" in
  let out = open_out output_file in
  Pimppp.pp_program pimp out;
  close_out out;
  exit 0
