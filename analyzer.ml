let parse_file f =
  let sin =
    let fi = open_in f in
    let flen = in_channel_length fi in
    let buf = String.create flen in
    really_input fi buf 0 flen;
    close_in fi;
    buf
  in
  let lexbuf = Lexing.from_string sin in
  try
    Parser.program Lexer.token lexbuf
  with
  | Failure "lexing: empty token" ->
     let pos = (Lexing.lexeme_end_p lexbuf) in
     Printf.eprintf
       "Lexing error at line %d, character %d.%!"
       pos.Lexing.pos_lnum
       (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
     exit 1
  | Parsing.Parse_error ->
     let pos = (Lexing.lexeme_end_p lexbuf) in
     Printf.eprintf
       "Parse error at word \"%s\", line %d, character %d."
       (Lexing.lexeme lexbuf)
       pos.Lexing.pos_lnum
       (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
     exit 1

let () =
  if Array.length Sys.argv < 2 then
    (* NO FILE NAME *)
    failwith "Please enter a file name."

  else if Array.length Sys.argv = 2 then 
    (* SIMULATE *)
     let prog = parse_file Sys.argv.(1) in
     print_string ("Program:\n\n" ^ Language.string_of_prog prog ^ "\n");
     Simulator.sim(prog)

  else if Array.length Sys.argv = 3 then
     (* WITH FLAGS *)
     print_string ("Flag : ");
     let flag = Sys.argv.(1) in
     let prog = parse_file Sys.argv.(2) in     
     match flag with
      |"--sign" -> 
         print_string ("sign\n");
         Sign.sign(prog)
      |"--interval" -> 
         print_string ("interval\n")
         (*Interval.sim(prog)*)
      |"--set" -> 
         print_string ("set\n")
         (*Set.sim(prog)*)
      | x -> failwith " not recognized\n"
