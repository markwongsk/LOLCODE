(* L2 Compiler
 * TypeChecker
 * Author: Andrew Audibert <aaudiber@andrew.cmu.edu>
 *
 * Typechecker which makes sure all variables are declared before
 * initialization. It also checks that everything is type safe
 * as an added bonus.
 *)

signature TYPE_CHECK =
sig
  (* prints error message and raises ErrorMsg.error if error found *)
  val typecheck : Ast.program -> unit
end;

structure TypeChecker :> TYPE_CHECK =
struct
  structure A = Ast

  exception ImpossibleStateException

  (* tc_exp : unit Symbol.table -> Ast.exp -> Mark.ext option -> unit *)
  fun tc_exp table (A.Var id) ext =
      (case Symbol.look table id
	    of NONE => ( ErrorMsg.error ext ("undeclared variable `" ^
                                         Symbol.name id ^ "'") ;
		             raise ErrorMsg.Error )
	     | SOME _ => ())
    | tc_exp table (A.ConstExp(A.IntConst(_))) ext = ()
    | tc_exp table (A.ConstExp(_)) ext = ()
    | tc_exp table (A.UnopExp(unop, e)) ext = tc_exp table e ext
    | tc_exp table (A.BinopExp(binop, e1, e2)) ext = (tc_exp table e1 ext; tc_exp table e2 ext)
    | tc_exp table (A.Tern(cond, e1, e2)) ext = (tc_exp table cond ext; tc_exp table e1 ext; tc_exp table e2 ext)
    | tc_exp table (A.Marked(marked_exp)) ext =
        tc_exp table (Mark.data marked_exp) (Mark.ext marked_exp)

  (* tc_stmts : A.stmt list -> unit Symbol.table -> Mark.ext option ->
   *            unit Symbol.table *)
  and tc_stmts [] table ext = table
    | tc_stmts (stmt::stmts) table ext =
      case stmt
       of A.Markeds(marked_stmt) =>
          (tc_stmts ((Mark.data marked_stmt)::stmts)
                    table (Mark.ext marked_stmt))
        | _ =>
      let
        fun bind table id ext = Symbol.bind table (id, ())

        (* Guard an addition to the symbol table. *)
        fun checkAssign table (id, _) ext =
            if Symbol.is_bogus id then table
            else
              case Symbol.look table id of
                NONE => ( ErrorMsg.error ext ("undeclared variable `" ^
                                              Symbol.name id ^ "'") ;
		                  raise ErrorMsg.Error )
              | SOME _ => table
        val newTable =
            case stmt of
              A.Assign(id, e) =>
                (checkAssign table (id, tc_exp table e ext) ext; table)
            | A.IfThenElse(e, s1, s2) =>
                (tc_exp table e ext; tc_stmts [s1] table ext; tc_stmts [s2] table ext; table)
            | A.While(e, s) =>
                (tc_exp table e ext; tc_stmts [s] table ext; table)
            | A.For(s1, e, s2, s3) =>
                let val table' = tc_stmts [s1] table ext
                in tc_exp table' e ext; tc_stmts [s2] table' ext; tc_stmts [s3] table' ext; table
                end
            | A.Continue => table
            | A.Break => table
            | A.Return(e) => (tc_exp table e ext; table)
            | A.Nop => table
            | A.Seq(stmts') => (tc_stmts stmts' table ext; table)
            | A.Declare(id, e) =>
                (if isSome e then tc_exp table (valOf e) ext
                 else (); bind table id ext)
            | A.Markeds(marked_stmt) => raise ImpossibleStateException
      in
        tc_stmts stmts newTable ext
      end

  fun typecheck prog = (tc_stmts prog (Symbol.empty) NONE; ())

end
