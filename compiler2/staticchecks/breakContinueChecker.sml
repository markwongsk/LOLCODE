(* L2 Compiler
 * Break and Continue Checker
 * Author: Andrew Audibert <aaudiber@andrew.cmu.edu>
 *
 * Checks that all break and continue statements occur within
 * the scopes of while and for loops. Also make sure the step
 * statements in for loops are not declarations.
 *)

signature BCC_CHECK =
sig
  (* prints error message and raises ErrorMsg.error if error found *)
  val bcccheck : Ast.program -> unit
end;

structure BCCChecker :> BCC_CHECK =
struct
  structure A = Ast

  (* bcc_stmts : Ast.stmt -> bool -> Mark.ext option -> () *)
  fun bcc_stmt isInner stmt ext =
      let fun check cond str = if cond then () else
                               ( ErrorMsg.error ext (
                                 str ^ " must be placed within a " ^
                                 "for loop or a while loop") ;
                                 raise ErrorMsg.Error )
          fun checkStep stmt ext =
              case stmt of
                A.Declare(_, _) => ( ErrorMsg.error ext (
                                        "The step statement of a for loop " ^
                                        "must not be a declaration.") ;
                                        raise ErrorMsg.Error )
              | A.Markeds(marked_stmt) =>
                  checkStep (Mark.data marked_stmt) (Mark.ext marked_stmt)
              | A.Seq(_) => ( ErrorMsg.error ext (
                              "The step statement must be a single statement") ;
                              raise ErrorMsg.Error )
              | _ => ()
      in
        case stmt of
          A.IfThenElse(e, s1, s2) => (bcc_stmts isInner [s1] ext;
                                      bcc_stmts isInner [s2] ext)
        | A.While(id, e, s) => bcc_stmt true s ext
        | A.For(id, s1, e, s2, s3) => (checkStep s2 ext;
                                       bcc_stmt false s1 ext;
                                       bcc_stmt false s2 ext;
                                       bcc_stmt true s3 ext)
        | A.Continue => check isInner "Continue"
        | A.Break => check isInner "Break"
        | A.Seq(stmts) => bcc_stmts isInner stmts ext
        | A.Markeds(marked_stmt) =>
            bcc_stmt isInner (Mark.data marked_stmt) (Mark.ext marked_stmt)
        | _ => ()
      end

  (* bcc_stmts : Ast.stmt list -> bool -> Mark.ext option -> bool *)
  and bcc_stmts isInner [] ext = ()
    | bcc_stmts isInner (stmt::stmts) ext =
        (bcc_stmt isInner stmt ext; bcc_stmts isInner stmts ext)

  fun bcccheck prog = bcc_stmts false prog NONE

end
