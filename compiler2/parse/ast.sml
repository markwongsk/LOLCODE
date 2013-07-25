(* L1 Compiler
 * Abstract Syntax Trees
 * Author: Alex Vaynberg
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Forward compatible fragment of C0
 *
 * Consider using smlnj's pretty printing library --
 * you might find it useful to deal with indentation, spacing, etc.
 * This is especially useful for large programs when string concatenation
 * may get very slow.
 *)

signature AST =
sig
  type ident = Symbol.symbol
  datatype c0type = INT | BOOL

  datatype const =
     IntConst of Word32.word
   | TRUE
   | FALSE

  datatype binop =
     PLUS
   | MINUS
   | TIMES
   | DIVIDEDBY
   | MODULO
   | LT
   | LEQ
   | GT
   | GEQ
   | EQ
   | NEQ
   | AND
   | OR
   | BAND
   | BOR
   | BXOR
   | LSHIFT
   | RSHIFT

  datatype unop =
     NOT
   | BNOT
   | NEGATIVE

  datatype exp =
     Var of ident
   | ConstExp of const
   | UnopExp of unop * exp
   | BinopExp of binop * exp * exp
   | Tern of exp * exp * exp
   | Marked of exp Mark.marked
  and stmt =
     Assign of ident * exp
   | IfThenElse of exp * stmt * stmt
   | While of exp * stmt
   | For of stmt * exp * stmt * stmt
   | Continue
   | Break
   | Return of exp
   | Nop
   | Seq of stmt list (* need a way to say that stmt can be stmt list *)
   | Declare of ident * c0type * exp option
   | Markeds of stmt Mark.marked

  type program = stmt list

  (* print as source, with redundant parentheses *)
  structure Print :
  sig
    val pp_exp : exp -> string
    val pp_stmt : stmt -> string
    val pp_program : program -> string
    val pp_unop_oper : unop -> string
    val pp_binop_oper : binop -> string
    val pp_type : c0type -> string
  end

end

structure Ast :> AST =
struct
  exception ParserError
  type ident = Symbol.symbol
  datatype c0type = INT | BOOL

  datatype const =
     IntConst of Word32.word
   | TRUE
   | FALSE

  datatype binop =
     PLUS
   | MINUS
   | TIMES
   | DIVIDEDBY
   | MODULO
   | LT
   | LEQ
   | GT
   | GEQ
   | EQ
   | NEQ
   | AND
   | OR
   | BAND
   | BOR
   | BXOR
   | LSHIFT
   | RSHIFT

  datatype unop =
     NOT
   | BNOT
   | NEGATIVE

  datatype exp =
     Var of ident
   | ConstExp of const
   | UnopExp of unop * exp
   | BinopExp of binop * exp * exp
   | Marked of exp Mark.marked
   | Tern of exp * exp * exp
  and stmt =
     Assign of ident * exp
   | IfThenElse of exp * stmt * stmt
   | While of exp * stmt
   | For of stmt * exp * stmt * stmt
   | Continue
   | Break
   | Return of exp
   | Nop
   | Seq of stmt list
   | Declare of ident * c0type * exp option
   | Markeds of stmt Mark.marked

  type program = stmt list

  (* print programs and expressions in source form
   * using redundant parentheses to clarify precedence
   *)
  structure Print =
  struct
    fun pp_ident id = Symbol.name id
    fun pp_type INT = "int"
      | pp_type BOOL = "bool"

    fun pp_binop_oper PLUS = "+"
      | pp_binop_oper MINUS = "-"
      | pp_binop_oper TIMES = "*"
      | pp_binop_oper DIVIDEDBY = "/"
      | pp_binop_oper MODULO = "%"
      | pp_binop_oper LT = "<"
      | pp_binop_oper LEQ = "<="
      | pp_binop_oper GT = ">"
      | pp_binop_oper GEQ = ">="
      | pp_binop_oper EQ = "=="
      | pp_binop_oper NEQ = "!="
      | pp_binop_oper AND = "&&"
      | pp_binop_oper OR = "||"
      | pp_binop_oper BAND = "&"
      | pp_binop_oper BOR = "|"
      | pp_binop_oper BXOR = "^"
      | pp_binop_oper LSHIFT = "<<"
      | pp_binop_oper RSHIFT = ">>"

    fun pp_unop_oper NOT = "!"
      | pp_unop_oper BNOT = "~"
      | pp_unop_oper NEGATIVE = "-"

    fun pp_const (IntConst(i)) = Word32Signed.toString i
      | pp_const (TRUE) = "true"
      | pp_const (FALSE) = "false"

    fun pp_exp (Var(id)) = pp_ident id
      | pp_exp (ConstExp(constExp)) = pp_const constExp
      | pp_exp (UnopExp(unop, e)) = pp_unop_oper unop ^ "(" ^ pp_exp e ^ ")"
      | pp_exp (BinopExp(binop, e1, e2)) =
          "(" ^ pp_exp e1 ^ " " ^ pp_binop_oper binop
          ^ " " ^ pp_exp e2 ^ ")"
      | pp_exp (Marked(marked_exp)) = pp_exp (Mark.data marked_exp)
      | pp_exp (Tern(e1,e2,e3)) = "(" ^ pp_exp e1 ^ ")?" ^ pp_exp e2 ^ " : " ^
                                  pp_exp e3

    fun pp_stmt (Assign(id,exp)) = pp_ident id ^ " = " ^ pp_exp exp
      | pp_stmt (IfThenElse(exp,s1,s2)) =
          "if (" ^ pp_exp exp ^ ")\n" ^ pp_stmt s1 ^ "\n" ^ "else\n" ^ pp_stmt s2
      | pp_stmt (While(exp,s)) = "while (" ^ pp_exp exp ^ ")\n" ^ pp_stmt s
      | pp_stmt (For(s1,exp,s2,s3)) =
          "for (" ^ pp_stmt s1 ^ "; " ^ pp_exp exp ^ "; " ^ pp_stmt s2 ^ ")\n" ^
             pp_stmt s3
      | pp_stmt Continue = "continue"
      | pp_stmt Break = "break"
      | pp_stmt (Return exp) = "return " ^ pp_exp exp
      | pp_stmt Nop = "__NOP__"
      | pp_stmt (Seq stmts) = "\n{\n" ^ pp_stmts stmts ^ "}\n"
      | pp_stmt (Declare(id,t,NONE)) = pp_type t ^ " " ^ pp_ident id ^ ";"
      | pp_stmt (Declare(id,t,SOME e)) = pp_type t ^ " " ^ pp_ident id ^ " = " ^
                                         pp_exp e
      | pp_stmt (Markeds (marked_stmt)) = pp_stmt (Mark.data marked_stmt)

    and pp_stmts nil = ""
      | pp_stmts (s::ss) = pp_stmt s ^ "\n" ^ pp_stmts ss

    fun pp_program stms = "{\n" ^ pp_stmts stms ^ "}"
  end

end
