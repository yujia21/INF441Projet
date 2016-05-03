open Language

(* Hashtable : links variable to string of "P" etc etc *)

let variables = Hashtbl.create 100;;

let rec sign_add (e1, e2) =
   if ((sign_expr e1) = "B" || (sign_expr e2) = "B")  then "B"
   else if ((sign_expr e1)=(sign_expr e2)) then (sign_expr e2)
   else if ((sign_expr e1)="Z") then (sign_expr e2)
   else if ((sign_expr e2)="Z") then (sign_expr e1)
   else if ((sign_expr e1)="ZP" & (sign_expr e2)="P") then "ZP"
   else if ((sign_expr e1)="P" & (sign_expr e2)="ZP") then "ZP"
   else if ((sign_expr e1)="ZN" & (sign_expr e2)="N") then "ZN"
   else if ((sign_expr e1)="N" & (sign_expr e2)="ZN") then "ZN"
   else "NZP"
         
and sign_sub (e1, e2) = 
   if ((sign_expr e1) = "B" || (sign_expr e2) = "B")  then "B"
   else if ((sign_expr e2)="Z") then (sign_expr e1)
   else if ((sign_expr e1)="Z" & (sign_expr e1)="N") then "P"
   else if ((sign_expr e1)="Z" & (sign_expr e1)="P") then "N"
   else if ((sign_expr e1)="Z" & (sign_expr e1)="ZN") then "ZP"
   else if ((sign_expr e1)="Z" & (sign_expr e1)="ZP") then "ZN"
   else if ((String.contains (sign_expr e1) 'P') & (String.contains (sign_expr e2)
   'N')) then "ZP"
   else if ((String.contains (sign_expr e1) 'N') & (String.contains (sign_expr e2)
   'P')) then "ZN"
   else "NZP"

and sign_mul (e1, e2) = 
   if ((sign_expr e1) = "B" || (sign_expr e2) = "B")  then "B"
   else if ((String.contains (sign_expr e1) 'Z') || (String.contains (sign_expr e2)
   'Z')) then "Z"
   else if ((sign_expr e1)="P" & (sign_expr e2)="P") then "P"
   else if ((sign_expr e1)="N" & (sign_expr e2)="N") then "P"
   else if ((sign_expr e1)="N" & (sign_expr e2)="P") then "N"
   else if ((sign_expr e1)="P" & (sign_expr e2)="N") then "N"
   else if ((String.contains (sign_expr e1) 'P') & (String.contains (sign_expr e2)
   'N')) then "ZN"
   else if ((String.contains (sign_expr e1) 'N') & (String.contains (sign_expr e2)
   'P')) then "ZN"
   else if ((String.contains (sign_expr e1) 'P') & (String.contains (sign_expr e2)
   'P')) then "ZP"
   else if ((String.contains (sign_expr e1) 'N') & (String.contains (sign_expr e2)
   'N')) then "ZP"
   else "NZP"
      
and sign_div (e1, e2) = 
   if ((sign_expr e2) = "Z") then begin
      Hashtbl.replace variables "error" "ERROR";
      "B"
   end
   else begin
      if ((sign_expr e1) = "B" || (sign_expr e2) = "B")  then "B"
      else begin 
         if (String.contains (sign_expr e2) 'Z') then Hashtbl.replace variables "error"
         "MAYBE ERROR";
         if (String.contains (sign_expr e2) 'P') then sign_expr e1
         else if (String.contains (sign_expr e2) 'N') then begin
            if ((sign_expr e1)="P") then "N"
            else if ((sign_expr e1)="N") then "P"
            else if ((sign_expr e1)="ZN") then "ZP"
            else if ((sign_expr e1)="ZP") then "ZN"
            else "ZN"
         end
         else "NZP"
      end
   end

and sign_expr = function
   |Var x -> Hashtbl.find variables x
   |Float x -> 
      if x>0.0 then "P"
      else if x = 0.0 then "Z"
      else "N"
   |Add (e1,e2) -> sign_add (e1, e2)
   |Sub (e1,e2) -> sign_sub (e1, e2)
   |Mul (e1,e2) -> sign_mul (e1, e2)
   |Div (e1,e2) -> sign_div (e1, e2)
   |Rand(e1, e2) -> 
      if ((sign_expr e2)=(sign_expr e1)) then (sign_expr e1)
      else if ((sign_expr e2)="P" & (sign_expr e1)="Z") then "ZP"
      else if ((sign_expr e2)="P" & (sign_expr e1)="ZP") then "ZP"
      else if ((sign_expr e2)="Z" & (sign_expr e1)="N") then "NZ"
      else if ((sign_expr e2)="Z" & (sign_expr e1)="NZ") then "NZ"
      else "NZP"

;;
  
let rec sign_prog = function
   |Assign(v,e) -> Hashtbl.replace variables v (sign_expr e);
   |Seq(p1,p2) -> sign_prog p1; sign_prog p2;
   |If(e, p1, p2) -> if (sign_expr e = "Z") then sign_prog p2 else sign_prog p1;
;;

let sign p = 
   Hashtbl.replace variables "error" "NO ERROR";
   sign_prog(p);
   print_string(Hashtbl.find variables "x");
   print_string("\n");
   print_string(Hashtbl.find variables "error");
   print_string("\n")
;;

(*
let () = 
   let e1 = Rand(Float 1.0, Float 2.0) in (*P*)
   let p1 = Assign("y",e1) in
   let e2 = Rand(Float 0.0, Float 1.0) in (*ZP*)
   let p2 = Assign("z", e2) in
   let e3 = Div(Var "y", Var "z") in (*P / ZP = P maybe error*)
   let e4 = Sub(e3, Float (-1.0)) in
   let p3 = Assign("x", e4) in
   let p4 = Seq(Seq(p1, p2), p3) in
   sign(p4)
*)
