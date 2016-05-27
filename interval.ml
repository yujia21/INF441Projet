open Language
open Floatinterval

(* Hashtable : links variable to string of "P" etc etc *)

let variables = Hashtbl.create 100;;
let error = Hashtbl.create 1;;

let rec int_add (e1, e2) = match (e1, e2) with
   |([], _)
   |(_, []) -> [] (*Empty set is for nondefined*)
   |(e1::[], e2::[]) ->
      [{low=(e1.low+.e2.low); high=(e1.high+.e2.high)}]
   |(e2::[],e1::es)
   |(e1::es,e2::[]) ->
      combine (int_add ([e1],[e2])) (int_add (es,
      [e2]))
   |(e1, e2::es) ->  
   combine (int_add (e1,[e2])) (int_add (e1, es))
   
         
and int_sub (e1, e2) = match (e1, e2) with
   |([], _)
   |(_, []) -> [] (*Empty set is for nondefined*)
   |(e1::[], e2::[]) ->
      [{low=(e1.low-.e2.high); high=(e1.high-.e2.low)}]
   |(e1::[],e2::es) ->
   combine (int_sub ([e1],[e2])) (int_sub ([e1],es))
   |(e1::es,e2::[]) ->
      combine (int_sub ([e1],[e2])) (int_sub (es,
      [e2]))
   |(e1, e2::es) ->  
   combine (int_sub (e1,[e2])) (int_sub (e1, es))   
   
and int_mul (e1, e2) = match (e1, e2) with
   |([], _)
   |(_, []) -> [] (*Empty set is for nondefined*)
   |(e1::[], e2::[]) ->
      let a1 = e1.low*.e2.low in
      let a2 = e1.low*.e2.high in
      let a3 = e1.high*.e2.low in
      let a4 = e1.high*.e2.high in
      [{low=(min (min a1 a2) (min a3 a4));
      high=(max (max a1 a2) (max a3 a4))}]
   |(e2::[],e1::es)
   |(e1::es,e2::[]) ->
      combine (int_mul ([e1],[e2])) (int_mul (es,
      [e2]))
   |(e1, e2::es) ->  
   combine (int_mul (e1,[e2])) (int_mul (e1, es))
   
and int_div (e1, e2) = match (e1, e2) with
   |([], _)
   |(_, []) -> [] (*Empty set is for nondefined*)
   |(e1::[], e2::[]) ->
   (*[x1, y1] [x2, y2]*)
   let x1 = e1.low in
   let y1 = e1.high in
   let x2 = e2.low in
   let y2 = e2.high in
   
   (* divide by 0, sure error*)
   if (x2 = 0.0 && y2 = 0.0) then begin
      Hashtbl.replace error "error" "ERROR";
      [{low=neg_infinity;high= infinity}]
      end
      
   (* e2 neg inc 0, maybe divide by 0, maybe error*)            
   else if (y2=0.0) then begin
      Hashtbl.replace error "error" "MAYBE ERROR";
      (*e1 positive*)
      if (x1 >= 0.0) then 
      [{low=neg_infinity;high= x1/.x2}]
      
      (*e1 negative*)
      else if (y1 <= 0.0) then
      [{low=x1/.x2;high=infinity}]
      
      (*e1 both*)
      else [{low=neg_infinity;high= infinity}]
   end
   
   (* e2 pos inc 0, maybe divide by 0, maybe error*)               
   else if (x2=0.0) then begin
      Hashtbl.replace error "error" "MAYBE ERROR";
      (*e1 positive*)
      if (x1 >= 0.0) then 
      [{low=x1/.y2;high=infinity}]
      
      (*e1 negative*)
      else if (y1 <= 0.0) then 
      [{low=neg_infinity;high= x1/.y2}]
      
      (*e1 both*)
      else [{low=neg_infinity;high= infinity}]
   end  

       
   (* e2 neg to pos, maybe divide by 0, maybe error*)      
   else if (x2<=0.0 && y2>=0.0) then begin
      Hashtbl.replace error "error" "MAYBE ERROR";
      (*TO DO how to handle intermediate disjoint unions? Create type called interval?*)
      (*e1 positive*)
      if (x1 >= 0.0) then 
      {low=neg_infinity;high= x1/.x2}::[{low=x1/.y2;high= infinity}]

      (*e1 negative*)
      else if (y1 <= 0.0) then 
      {low=neg_infinity;high= y1/.y2}::[{low=x1/.x2;high= infinity}]      

      (*e1 both*)
      else [{low=neg_infinity;high= infinity}]
      end
      
   else begin
      let a1 = x1/.x2 in
      let a2 = x1/.y2 in
      let a3 = y1/.x2 in
      let a4 = y1/.y2 in
      [{low=(min (min a1 a2) (min a3 a4));high= (max (max a1 a2) (max a3 a4))}]
   end
   |(e1::[],e2::es) ->
   combine (int_div ([e1],[e2])) (int_div ([e1],es))
   |(e1::es,e2::[]) ->
      combine (int_div ([e1],[e2])) (int_div (es,
      [e2]))
   |(e1, e2::es) ->  
   combine (int_div (e1,[e2])) (int_div (e1, es))
      
and int_expr = function 
(*Language.expr -> Floatinterval.interval list*)
   |Var x -> Hashtbl.find variables x
   |Float x -> [{low=x;high=x}]
   (*something wrong here*)
   |Add (e1,e2) -> int_add (int_expr e1, int_expr e2)
   |Sub (e1,e2) -> int_sub (int_expr e1, int_expr e2)
   |Mul (e1,e2) -> int_mul (int_expr e1, int_expr e2)
   |Div (e1,e2) -> int_div (int_expr e1, int_expr e2)
   |Rand(e1, e2) -> 
      let a = int_expr e1 in
      let b = int_expr e2 in
      [{low = min (List.hd(a)).low (List.hd(b)).low; 
      high=max (List.hd(a)).high (List.hd(b)).high}]
   (*assume rand only called on floats*)
;;

let combine_err e1 e2 = 
   if (e1 = e2) then Hashtbl.replace error "error" e1
   else Hashtbl.replace error "error" "MAYBE ERROR" 
   (*only NO_ERROR, NO_ERROR gives NO_ERROR, only ERROR, ERROR gives ERROR, all else is MAYBE ERROR*)
;;

let rec combine_prog p1 p2 = (*run p1, check x and error, run p2, check x and error*)
   int_prog p1;
   let e1 = Hashtbl.find error "error" in
   let x1 = Hashtbl.find variables "x" in
   int_prog p2;
   let e2 = Hashtbl.find error "error" in
   let x2 = Hashtbl.find variables "x" in
   combine_err e1 e2;
   Hashtbl.replace variables "x" (combine x1 x2)
   (*distinguish between two if branches two
   options*)
  
and int_prog = function
   |Assign(v,e) -> Hashtbl.replace variables v (int_expr e);
   |Seq(p1,p2) -> int_prog p1; int_prog p2;
   |If(e, p1, p2) -> 
      let x = (int_expr e) in
      (*definitely 0*)   
      let x1 = (List.hd(x)).low in
      let y1 = (List.hd(x)).high in      
      if (x1=0.0 && y1 = 0.0) then int_prog p2 
      (*definitely not 0*)      
      else if (x1>0.0 || y1 < 0.0) then int_prog p1
      (*could be 0*) 
      else combine_prog p1 p2
;;

let int p = 
   Hashtbl.replace error "error" "NO ERROR";
   int_prog(p);
   let t = (Hashtbl.find variables "x") in
   (*print t;*)(*Interval without extension*)
   print_string("{");
   print_set t;
   print_string("\n");
   print_string(Hashtbl.find error "error");
   print_string("\n")
;;
