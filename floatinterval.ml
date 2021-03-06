type interval = {low:float;high:float}
	   
let rec print t = match t with
   |[] -> print_string("Empty set \n")
   |t::ts -> 
      print_string("[");
      print_float(t.low);
      print_string(",");
      print_float(t.high);
      print_string("]");
      if (List.length(ts) = 0) then print_string("\n")
      else begin
         print_string(" ou ");
         print ts
      end
;;

let rec print_set t = match t with
   |[] -> print_string("Empty set \n")
   |t::ts ->
      if (t.low = t.high) then begin
         print_float(t.low);
         if (List.length(ts) = 0) then
         print_string("")
         else begin
            print_string(", ");
            print_set ts
         end
      end
      else begin
         print_string("[");
         print_float(t.low);
         print_string(",");
         print_float(t.high);
         print_string("]");
         if (List.length(ts) = 0) then
         print_string("")
         else begin
            print_string(", ");
            print_set ts
         end
      end

;;

let rec combine t1 t2 = match (t1,t2) with
   |([], x)
   |(x, []) -> []
   |(t1::[],t2::[])->
      if (t1.low <= t2.low && t1.high >= t2.high)
      then [t1]
      else if (t2.low <= t1.low && t2.high >= t1.high)
      then [t2]
      else if (t1.high >= t2.low && t2.high >=
      t1.high) then 
         [{low=t1.low;high=t2.high}]
      else if (t2.high >= t1.low && t1.high >=
      t2.high) then 
         [{low=t2.low;high=t1.high}]
      else begin
         let a1 = min t1.low t2.low in
         let a2 = min t1.high t2.high in
         let c1 = {low = a1;high=a2} in
         let b1 = max t1.low t2.low in
         let b2 = max t1.high t2.high in
         let c2 = {low = b1;high=b2} in         
         c1::[c2]
      end
   |(t1,t2)-> List.append t1 t2 
   (*assume no overlaps : two if branches*)
