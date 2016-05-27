type expr =
  | Var of string (** variable *)
  | Float of float (** float *)
  | Add of expr * expr (** addition *)
  | Sub of expr * expr (** subtraction *)
  | Mul of expr * expr (** multiplication *)
  | Div of expr * expr (** division *)
  | Rand of expr * expr (** random *)

type prog =
  | Assign of string * expr (** x = e *)
  | Seq of prog * prog (** p1 ; p2 *)
  | If of expr * prog * prog (** if e then p1 else p2 *)
  (*| While of string * prog (** while e do p done *)*)

let rec string_of_expr = function
  | Var x -> x
  | Float x -> string_of_float x
  | Add (e1, e2) -> string_of_expr e1 ^ " + " ^ string_of_expr e2
  | Sub (e1, e2) -> string_of_expr e1 ^ " - " ^ string_of_expr e2
  | Mul (e1, e2) -> string_of_expr e1 ^ " * " ^ string_of_expr e2
  | Div (e1, e2) -> string_of_expr e1 ^ " / " ^ string_of_expr e2
  | Rand (e1, e2) -> "rand(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"

let rec string_of_prog = function
  | Assign (x, e) -> x ^ " = " ^ string_of_expr e
  | Seq (p1, p2) -> string_of_prog p1 ^ ";\n" ^ string_of_prog p2
  | If (e, p1, p2) -> "if " ^ string_of_expr e ^ " then " ^ string_of_prog p1 ^ " else " ^ string_of_prog p2
  (*| While(x, p1) -> "while" ^ x ^ " do " ^ string_of_prog p1 ^ " done "*)
