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
  if Array.length Sys.argv < 2 then begin
    (* NO FILE NAME *)
    failwith "Please enter a file name."
    end

  else if Array.length Sys.argv = 2 then begin
    (* SIMULATE *)
     let prog = parse_file Sys.argv.(1) in
     print_string ("Program:\n\n" ^ Language.string_of_prog prog ^ "\n");
     print_string("\nx = ");
     Simulator.sim(prog);
     print_string("\n")
     end

  else if Array.length Sys.argv = 3 then begin
     (* WITH FLAGS *)
     let flag = Sys.argv.(1) in
     let prog = parse_file Sys.argv.(2) in     
     print_string ("Program:\n\n" ^ Language.string_of_prog prog ^ "\n");
     print_string ("\n\nFlag : ");
     match flag with
      |"--sign" -> 
         print_string ("sign\n");
         Sign.sign(prog)
      |"--interval" -> 
         print_string ("interval\n");
         Interval.intv(prog)
      | x -> failwith " not recognized\n"
      end
