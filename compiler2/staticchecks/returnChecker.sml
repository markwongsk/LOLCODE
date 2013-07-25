(* L2 Compiler
 * ReturnChecker
 * Author: Andrew Audibert <aaudiber@andrew.cmu.edu>
 *
 * Checks that every control path through the program will either
 * loop forever, raise an exception, or end in an explicit return
 * statement. We also prune dead code following return statements.
 *)

signature RETURN_CHECK =
sig
  (* prints error message and raises ErrorMsg.error if error found *)
  val returncheck : Ast.program -> Ast.program
end;

structure ReturnChecker :> RETURN_CHECK =
struct
  structure A = Ast

  (* rt_stmts : Ast.stmt list -> (Ast.stmt list, bool) *)
  fun rt_stmts [] = ([], false)
    | rt_stmts (stmt::stmts) =
        case stmt of
          A.IfThenElse(e, s1, s2) =>
            let val ([s1'], s1ret) = rt_stmts [s1]
                val ([s2'], s2ret) = rt_stmts [s2]
            in if s1ret andalso s2ret then
                 ([A.IfThenElse(e, s1', s2')], true)
               else let val (stmts', stmtsret) = rt_stmts stmts
                    in (A.IfThenElse(e, s1', s2')::stmts', stmtsret)
                    end
            end
        | A.Return(e) => ([A.Return(e)], true)
        | A.Seq(s) =>
            let val (s', sret) = rt_stmts s
            in if sret then ([A.Seq(s')], true)
               else let val (stmts', stmtsret) = rt_stmts stmts
                    in (A.Seq(s')::stmts', stmtsret)
                    end
            end
        | _ => let val (stmts', stmtsret) = rt_stmts stmts
               in (stmt::stmts', stmtsret)
               end

  fun returncheck prog =
      case rt_stmts prog of
        (newAst, true) => newAst
      | (_, false) =>
          ( ErrorMsg.error NONE
            "Not all paths terminate in return" ;
            raise ErrorMsg.Error
          )

end
