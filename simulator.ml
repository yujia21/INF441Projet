Random.self_init ();
open Language

let variables = Hashtbl.create 100;;

let rec sim_add = function
   |Float e1,Float e2->e1+.e2
   |Float e1,e2-> e1+.(sim_expr e2)
   |e1, Float e2 -> (sim_expr e1)+.e2
   |e1, e2 -> (sim_expr e1)+.(sim_expr e2)
   
and sim_sub = function
   |Float e1,Float e2->e1-.e2
   |Float e1,e2-> e1-.(sim_expr e2)
   |e1, Float e2 -> (sim_expr e1)-.e2
   |e1, e2 -> (sim_expr e1)-.(sim_expr e2)

and sim_mul = function
   |Float e1,Float e2->e1*.e2
   |Float e1,e2-> e1*.(sim_expr e2)
   |e1, Float e2 -> (sim_expr e1)*.e2
   |e1, e2 -> (sim_expr e1)*.(sim_expr e2)
   
and sim_div = function
   |Float e1,Float e2-> if e2 = float 0 then failwith "ERROR" else e1/.e2
   |Float e1,e2-> if (sim_expr e2) = float 0 then failwith "ERROR" else e1/.(sim_expr e2)
   |e1, Float e2 -> if e2 = float 0 then failwith "ERROR" else (sim_expr e1)/.e2
   |e1, e2 -> if (sim_expr e2) = float 0 then failwith "ERROR" else (sim_expr e1)/.(sim_expr e2)

and sim_expr = function
   |Var x -> Hashtbl.find variables x
   |Float x -> x
   |Add (e1,e2) -> sim_add (e1, e2)
   |Sub (e1,e2) -> sim_sub (e1, e2)
   |Mul (e1,e2) -> sim_mul (e1, e2)
   |Div (e1,e2) -> sim_div (e1, e2)
   |Rand(e1, e2) -> (Random.float (sim_expr e2)) *. ((sim_expr e2) -. (sim_expr e1))
   +. (sim_expr e1)
;;
  
let rec sim_prog = function
   |Assign(v,e) -> Hashtbl.replace variables v (sim_expr e);
   |Seq(p1,p2) -> sim_prog p1; sim_prog p2;
   |If(e, p1, p2) -> if (sim_expr e = 0.0) then sim_prog p2 else sim_prog p1;
;;

let sim p = 
   sim_prog(p);
   print_float(Hashtbl.find variables "x");
   print_string("\n")
;;
(*
let () = 
   let e1 = Rand(Float 1.0, Float 2.0) in
   let p1 = Assign("y",e1) in
   let e2 = Rand(Float 0.0, Float 1.0) in
   let p2 = Assign("z", e2) in
   let e3 = Div(Var "y", Var "z") in
   let e4 = Sub(e3, Float 1.0) in
   let p3 = Assign("x", e4) in
   let p4 = Seq(Seq(p1, p2), p3) in
   sim(p4)
*)
