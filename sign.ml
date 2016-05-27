open Language

(* Hashtable : links variable to string of "P" etc etc *)

let variables = Hashtbl.create 100;;

let rec sign_add (s1, s2) = match (s1, s2) with
   |("B", _)
   |(_, "B") -> "B"
   |("Z", x)
   |(x, "Z") -> x
   |("ZP","P")
   |("P","ZP")->"ZP"
   |("NZ","N")
   |("N","NZ")->"NZ"
   |(s1,s2)-> if (s1=s2) then s2 else "NZP"
         
and sign_sub (s1, s2) = match (s1, s2) with
   |("B", _)
   |(_, "B") -> "B"
   |(x, "Z") -> x
   |("Z", "N") -> "P"
   |("Z", "P") -> "N"
   |("Z", "ZP") -> "NZ"
   |("Z", "NZ") -> "ZP"
   |(s1, s2) -> 
      if ((String.contains s1 'P') && (String.contains s2 'N')) then "ZP"
      else if ((String.contains s1 'N') && (String.contains s2 'P')) then "NZ"
      else "NZP"

and sign_mul (s1, s2) = match (s1, s2) with
   |("B", _)
   |(_, "B") -> "B"
   |("P","N")
   |("N","P")->"N"
   |(s1,s2) ->
      if (s1=s2) then s1
      else if ((String.contains s1 'Z') || (String.contains s2 'Z')) then "Z"
      else if ((String.contains s1 'P') && (String.contains s2 'N')) then "NZ"
      else if ((String.contains s1 'N') && (String.contains s2 'P')) then "NZ"
      else if ((String.contains s1 'P') && (String.contains s2 'P')) then "ZP"
      else if ((String.contains s1 'N') && (String.contains s2 'N')) then "ZP"
      else "NZP"
      
and sign_div (s1, s2) = 
   if (s2 = "Z") then begin
      Hashtbl.replace variables "error" "ERROR";
      "B"
   end
   else begin
      if (s1 = "B" || s2 = "B")  then "B"
      else begin 
         if (String.contains s2 'Z') then begin
            Hashtbl.replace variables "error" "MAYBE ERROR";
            "NZP" (*or b?*)
            end
         else if (s2 = "P" || s2 = "ZP") then s1
         else if (s2 = "N" || s2 = "NZ") then begin
            if (s1="P") then "N"
            else if (s1="N") then "P"
            else if (s1="NZ") then "ZP"
            else if (s1="ZP") then "NZ"
            else "Z"
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
   |Add (e1,e2) -> sign_add (sign_expr e1, sign_expr e2)
   |Sub (e1,e2) -> sign_sub (sign_expr e1, sign_expr e2)
   |Mul (e1,e2) -> sign_mul (sign_expr e1, sign_expr e2)
   |Div (e1,e2) -> sign_div (sign_expr e1, sign_expr e2)
   |Rand(e1, e2) -> 
      if ((sign_expr e2)=(sign_expr e1)) then (sign_expr e1)
      else if ((sign_expr e2)="P" && (sign_expr e1)="Z") then "ZP"
      else if ((sign_expr e2)="P" && (sign_expr e1)="ZP") then "ZP"
      else if ((sign_expr e2)="Z" && (sign_expr e1)="N") then "NZ"
      else if ((sign_expr e2)="Z" && (sign_expr e1)="NZ") then "NZ"
      else "NZP"

;;

let combine_err e1 e2 = 
   if (e1 = e2) then Hashtbl.replace variables "error" e1
   else Hashtbl.replace variables "error" "MAYBE ERROR" 
   (*only NO_ERROR, NO_ERROR gives NO_ERROR, only ERROR, ERROR gives ERROR, all else is MAYBE ERROR*)

and combine_val x1 x2 = match (x1, x2) with
   |("ZP", "P")
   |("P","ZP") -> Hashtbl.replace variables "x" "ZP"
   |("NZ", "N")
   |("N","NZ") -> Hashtbl.replace variables "x" "NZ"
   |(x1, x2) -> 
      if (x1 = x2) then Hashtbl.replace variables "x" x1
      else Hashtbl.replace variables "x" "NZP"
   (* all NZP, except when two the same, or ZP, P or NZ, N  *)

let rec combine_prog p1 p2 = (*run p1, check x and error, run p2, check x and error*)
   sign_prog p1;
   let e1 = Hashtbl.find variables "error" in
   let x1 = Hashtbl.find variables "x" in
   sign_prog p2;
   let e2 = Hashtbl.find variables "error" in
   let x2 = Hashtbl.find variables "x" in   
   combine_err e1 e2;
   combine_val x1 x2
  
and sign_prog = function
   |Assign(v,e) -> Hashtbl.replace variables v (sign_expr e);
   |Seq(p1,p2) -> sign_prog p1; sign_prog p2;
   |If(e, p1, p2) -> 
      (*definitely 0*)
      if (sign_expr e = "Z") then sign_prog p2
      (*definitely not 0*)
      else if ((sign_expr e) = "P" || (sign_expr e) = "N") then sign_prog p1
      (*could be 0*)
      else combine_prog p1 p2
   |While(v,p) -> print_string ("Untreated case!\n");sign_prog p(*TO DO*)     
;;

let sign p = 
   Hashtbl.replace variables "error" "NO ERROR";
   sign_prog(p);
   print_string(Hashtbl.find variables "x");
   print_string("\n");
   print_string(Hashtbl.find variables "error");
   print_string("\n")
;;
