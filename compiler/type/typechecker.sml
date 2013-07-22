(* L1 Compiler
 * TypeChecker
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Simple typechecker that is based on a unit Symbol.table
 * This is all that is needed since there is only an integer type present
 * Also, since only straightline code is accepted, we hack our way
 * around initialization checks here.
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now distinguishes between declarations and initialization
 *)

signature TYPE_CHECK =
sig
  (* prints error message and raises ErrorMsg.error if error found *)
  val typecheck : Ast.program -> unit
end;

structure TypeChecker :> TYPE_CHECK =
struct
  structure A = Ast

  (* tc_exp : bool Symbol.table -> Ast.exp -> Mark.ext option -> unit *)
  fun tc_exp env (A.Var(id)) ext =
      (case Symbol.look env id
	of NONE => ( ErrorMsg.error ext ("undeclared variable `" ^
                                     Symbol.name id ^ "'") ;
		     raise ErrorMsg.Error )
	 | SOME (false) => ( ErrorMsg.error ext ("uninitialized variable `" ^
                                             Symbol.name id ^ "'") ;
		           raise ErrorMsg.Error )
         | SOME (true) => ())
    | tc_exp env (A.ConstExp(c)) ext = ()
    | tc_exp env (A.OpExp(oper,es)) ext =
      (* Note: it is syntactically impossible in this language to
       * apply an operator to an incorrect number of arguments
       * so we only check each of the arguments
       *)
	List.app (fn e => tc_exp env e ext) es
    | tc_exp env (A.Marked(marked_exp)) ext =
        tc_exp env (Mark.data marked_exp) (Mark.ext marked_exp)

  (* tc_stms : unit Symbol.table -> Ast.stm -> Mark.ext option -> unit *)
  fun tc_stms [] env ext ret = ret
    | tc_stms (stm::stms) env ext ret =
      let
        fun checkBind env (d, init) =
            case Symbol.look env d of
              NONE => Symbol.bind env (d, init)
            | SOME _ => ( ErrorMsg.error NONE ("redeclared variable `" ^
                                               Symbol.name d ^ "'") ;
		                  raise ErrorMsg.Error )
      in
        case stm of
          A.Decl(d) => tc_stms stms (checkBind env (d, false)) ext ret
        | A.DeclAssn(d, e) =>
            (tc_exp env e ext;
             tc_stms stms (checkBind env (d, true)) ext ret)
        | A.Assign(id,e) => (
            tc_exp env e ext;
            (case Symbol.look env id
	          of NONE => ( ErrorMsg.error ext ("undeclared variable `" ^
                                               Symbol.name id ^ "'") ;
		                   raise ErrorMsg.Error )
               (* just got initialized *)
	           | SOME (false) =>
                   tc_stms stms (Symbol.bind env (id, true)) ext ret
               (* already initialized *)
	           | SOME (true) => tc_stms stms env ext ret
            ))
        | A.Markeds(marked_stm) =>
            (tc_stms ((Mark.data marked_stm)
                      ::stms) env (Mark.ext marked_stm) ret)
        | A.Return(e) => (tc_exp env e ext; tc_stms stms env ext true)
      end

  fun typecheck prog =
      if tc_stms prog Symbol.empty NONE false then ()
      else (ErrorMsg.error NONE "main does not return\n"; raise ErrorMsg.Error)

end
