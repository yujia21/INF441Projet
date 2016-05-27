Random.self_init ();
open Language

let variables = Hashtbl.create 100;;
let error = Hashtbl.create 1;;

let rec sim_add (e1, e2) = e1+.e2
   
and sim_sub (e1, e2) = e1-.e2

and sim_mul (e1, e2) = e1*.e2
   
and sim_div (e1, e2) = 
   if (e2 = 0.0) then begin
      Hashtbl.replace error "error" "ERROR";
      e1/.e2 
      (* OCAML handles division by 0 properly*)
   end
   else e1/.e2

and sim_expr = function
   |Var x -> Hashtbl.find variables x
   |Float x -> x
   |Add (e1,e2) -> sim_add (sim_expr e1, sim_expr e2)
   |Sub (e1,e2) -> sim_sub (sim_expr e1, sim_expr e2)
   |Mul (e1,e2) -> sim_mul (sim_expr e1, sim_expr e2)
   |Div (e1,e2) -> sim_div (sim_expr e1, sim_expr e2)
   |Rand(e1, e2) -> (Random.float (sim_expr e2)) *. ((sim_expr e2) -. (sim_expr e1))
   +. (sim_expr e1)
;;
  
let rec sim_prog = function
   |Assign(v,e) -> Hashtbl.replace variables v (sim_expr e);
   |Seq(p1,p2) -> sim_prog p1; sim_prog p2;
   |If(e, p1, p2) -> if (sim_expr e = 0.0) then sim_prog p2 else sim_prog p1;
   |While(v,p) -> if (Hashtbl.find variables v <> 0.0) then begin
                  sim_prog p; 
                  sim_prog (While(v, p));
                  end
                  else begin end
;;

let sim p = 
   sim_prog(p);
   if (Hashtbl.mem error "error") then begin
      print_string(Hashtbl.find error "error");
      print_string("\n")
   end
   else begin
      print_float(Hashtbl.find variables "x");
      print_string("\n")
   end
;;
