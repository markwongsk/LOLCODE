(* L1 Compiler
 * IR Trees
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *)

signature TREE =
sig
  type label = Label.label
  type const = Ast.const
  type binop = Ast.binop
  type unop = Ast.unop

  (* pure expressions *)
  datatype exp =
      CONST of const
    | TEMP of Temp.temp
    | PBINOP of binop * exp * exp

  (* commands *)
  and cmd =
      MOVE of Temp.temp * exp
    | EBINOP of Temp.temp * binop * exp * exp
    | GOTO of label
    | IFGOTO of exp * label
    | LABEL of label
    | RETURN of exp
  (* unimplemented!!! not needed for lab2
   *| FMOVE of Function * exp list
   *)

  (* programs *)
  type program = cmd list

  structure Print :
  sig
    val pp_exp : exp -> string
    val pp_stmt : cmd -> string
    val pp_program : program -> string
  end
end

structure Tree : TREE =
struct
  type label = Label.label
  type const = Ast.const
  type binop = Ast.binop
  type unop = Ast.unop

  (* pure expressions *)
  datatype exp =
      CONST of const
    | TEMP of Temp.temp
    | PBINOP of binop * exp * exp

  (* commands *)
  and cmd =
      MOVE of Temp.temp * exp
    | EBINOP of Temp.temp * binop * exp * exp
    | GOTO of label
    | IFGOTO of exp * label
    | LABEL of label
    | RETURN of exp
  (* unimplemented! not needed for lab2
   *| FMOVE of Function * exp list
   *)

  (* programs *)
  type program = cmd list

  structure Print =
  struct

    val pp_binop = Ast.Print.pp_binop_oper
    val pp_unop = Ast.Print.pp_unop_oper

    fun pp_const (Ast.IntConst(x)) = Word32Signed.toString x
      | pp_const (Ast.TRUE) = "true"
      | pp_const (Ast.FALSE) = "false"

    fun pp_exp (CONST const) = pp_const const
      | pp_exp (TEMP t) = Temp.name t
      | pp_exp (PBINOP (binop, e1, e2)) =
      "(" ^ pp_exp e1 ^ " " ^ pp_binop binop ^ " " ^ pp_exp e2 ^ ")"

    fun pp_stmt (MOVE (t,e)) = pp_exp (TEMP(t)) ^ "  <--  " ^ pp_exp e
      | pp_stmt (EBINOP (t, binop, e1, e2)) = pp_exp (TEMP(t)) ^ "  <-- " ^
          "(" ^ pp_exp e1 ^ " " ^ pp_binop binop ^ " " ^ pp_exp e2 ^ ")"
      | pp_stmt (GOTO (name,label)) = "goto: c0" ^ name ^ Int.toString label
      | pp_stmt (IFGOTO (e,(name,label))) =
          "if (" ^ pp_exp e ^ ") goto: c0" ^ name ^ Int.toString label
      | pp_stmt (LABEL (name,label)) = "c0" ^ name ^ Int.toString label ^ ":"
      | pp_stmt (RETURN e) = "return " ^ pp_exp e

    fun pp_program (nil) = ""
      | pp_program (stmt::stmts) = pp_stmt stmt ^ "\n" ^ pp_program stmts
  end
end
