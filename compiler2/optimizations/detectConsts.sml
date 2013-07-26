(* L2 Compiler
 * Contant Checker
 * Author: Andrew Audibert <aaudiber@andrew.cmu.edu>
 *
 * Detect whether a variable is a constant, and if it is replace
 * it with a constant.
 *)

signature CONSTANT_CHECK =
sig
  (* prints error message and raises ErrorMsg.error if error found *)
  val constantcheck : Ast.program -> Ast.program
end;

structure ConstantChecker :> CONSTANT_CHECK =
struct
  structure A = Ast

  datatype VarStatus =
           UNSET
         | SET of A.const
         | NONCONST

  fun getConst e =
      case e of
        A.ConstExp(const) => SOME const
      | A.Marked(marked_exp) => getConst(Mark.data marked_exp)
      | _ => NONE

  fun updateVarStatus id table e =
      let
        val vStat = (case Symbol.look table id of
                       NONE => UNSET
                     | SOME v => v)
        val eConst = getConst e
        val newVStat = case (vStat, eConst) of
                         (UNSET, SOME const) => SET const
                       | (SET(oldConst), SOME const) =>
                           if oldConst = const then vStat else NONCONST
                       | (_, _) => NONCONST
      in
        Symbol.bind table (id, newVStat)
      end

  (* Return a table saying whether each encountered symbol is
   * constant so far.
   * cc_stmts : Ast.stmt list -> VarStat Symbol.table -> VarStat Symbol.table *)
  fun cc_stmts [] vars = vars
    | cc_stmts (stmt::stmts) vars =
        let
          val nextStmts : A.stmt list =
              case stmt of
                A.IfThenElse(e, s1, s2) => [s1, s2]
              | A.While(e, s) => [s]
              | A.For(s1, e, s2, s3) => [s1, s2, s3]
              | A.Seq(s) => s
              | _ => []

          val newVars =
              case stmt of
                A.Assign(id, e) => updateVarStatus id vars e
              | A.Declare(id, eOpt) =>
                  if isSome eOpt then updateVarStatus id vars (valOf eOpt)
                  else vars
              | _ => vars
        in
          case stmt of
            A.Markeds(marked_stmt) =>
              cc_stmts (Mark.data marked_stmt::stmts) vars
          | _ => cc_stmts (nextStmts @ stmts) newVars
        end

  (* Replace constant variables with constants.
   * mapConsts : VarStat Symbol.table -> Ast.exp -> Ast.exp *)
  fun mapConsts_exp varMap e =
      case e of
        A.Var(id) => (case Symbol.look varMap id of
                      SOME(SET const) => A.ConstExp const
                    | _ => A.Var(id))
      | A.UnopExp(oper, exp) => A.UnopExp(oper, mapConsts_exp varMap exp)
      | A.BinopExp(oper, exp1, exp2) =>
          A.BinopExp(oper, mapConsts_exp varMap exp1,
                         mapConsts_exp varMap exp2)
      | A.Tern(exp1, exp2, exp3) =>
          A.Tern(mapConsts_exp varMap exp1, mapConsts_exp varMap exp2,
               mapConsts_exp varMap exp3)
      | A.Marked(marked_exp) =>
          let val newExp = mapConsts_exp varMap (Mark.data marked_exp)
          in A.Marked(Mark.mark'(newExp, Mark.ext marked_exp))
          end
      | otherExp => otherExp

  (* Replace constant variables with constants.
   * mapConsts : VarStat Symbol.table -> Ast.stmt list -> Ast.stmt list *)
  fun mapConsts_stmts varMap [] = []
    | mapConsts_stmts varMap (stmt::stmts) =
        (case stmt of
           A.Assign(id, e) => (case (Symbol.look varMap id) of
                                 SOME(SET(_)) => A.Nop
                               | _ => A.Assign(id, mapConsts_exp varMap e))
         | A.IfThenElse(e, s1, s2)=> A.IfThenElse(mapConsts_exp varMap e,
                                                  hd(mapConsts_stmts varMap [s1]),
                                                  hd(mapConsts_stmts varMap [s2]))
         | A.While(e, s) => A.While(mapConsts_exp varMap e,
                                    hd(mapConsts_stmts varMap [s]))
         | A.For(s1, e, s2, s3) => A.For(hd(mapConsts_stmts varMap [s1]),
                                         mapConsts_exp varMap e,
                                         hd(mapConsts_stmts varMap [s2]),
                                         hd(mapConsts_stmts varMap [s3]))
         | A.Return(e) => A.Return(mapConsts_exp varMap e)
         | A.Seq(stmts') => A.Seq(mapConsts_stmts varMap stmts')
         | A.Declare(id, eOpt) =>
             if isSome eOpt
             then A.Declare (id, SOME(mapConsts_exp varMap (valOf eOpt)))
             else A.Declare (id, eOpt)
         | A.Markeds(marked_stmt) =>
             let val newStmt = hd(mapConsts_stmts varMap [Mark.data marked_stmt])
             in A.Markeds(Mark.mark'(newStmt, Mark.ext marked_stmt))
             end
         | otherStmt => otherStmt
        )
        :: (mapConsts_stmts varMap stmts)

  fun constantcheck prog =
      let val varTable = cc_stmts prog (Symbol.empty)
      in mapConsts_stmts varTable prog
      end

end
