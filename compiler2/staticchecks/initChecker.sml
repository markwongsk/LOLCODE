(* L2 Compiler
 * Initialization Checker
 * Author: Andrew Audibert <aaudiber@andrew.cmu.edu>
 *
 * Here we make sure that no uninitialized variables will ever be used.
 *)

signature INIT_CHECK =
sig
  (* prints error message and raises ErrorMsg.error if error found *)
  val initcheck : Ast.program -> unit
end;

structure InitChecker :> INIT_CHECK =
struct
  structure A = Ast

  datatype End = JUMP | NORMAL

  (* ic_exp : bool Symbol.set -> Ast.exp -> Mark.ext option -> unit *)
  fun ic_exp initSet (A.Var(id)) ext =
        if Symbol.member initSet id then ()
        else ( ErrorMsg.error ext ("uninitialized variable `" ^
                                   Symbol.name id ^ "'") ;
		       raise ErrorMsg.Error
             )
    | ic_exp initSet (A.ConstExp(_)) ext = ()
    | ic_exp initSet (A.UnopExp(unop, e)) ext = ic_exp initSet e ext
    | ic_exp initSet (A.BinopExp(binop, e1, e2)) ext =
        (ic_exp initSet e1 ext; ic_exp initSet e2 ext)
    | ic_exp initSet (A.Tern(cond, e1, e2)) ext =
        (ic_exp initSet cond ext;
         ic_exp initSet e1 ext;
         ic_exp initSet e2 ext)
    | ic_exp initSet (A.Marked(marked_exp)) ext =
        ic_exp initSet (Mark.data marked_exp) (Mark.ext marked_exp)

  fun bogusGuard set id =
      if Symbol.is_bogus id then set
      else Symbol.add set id

  (* checkStep : A.stmt -> Symbol.set -> Mark.ext option -> unit *)
  fun checkStep step env ext =
      (case step of
         NONE => ()
       | SOME stmt => (ic_stmts [stmt] NONE env ext; ()))

  (* ic_stmts : A.stmt list -> A.stmt -> (Symbol.set * Symbol.set) ->
   *            Mark.ext option -> (Symbol.set * Symbol.set * End) *)
  and ic_stmts [] step (declSet, initSet) ext = (declSet, initSet, NORMAL)
    | ic_stmts (stmt::stmts) step (env as (declSet, initSet)) ext =
      let
(*        val _ = print ("Considering statement : " ^ A.Print.pp_stmt stmt ^ "\n")
        val _ = print ("Symbols : " ^ Symbol.showmems initSet ^ "\n")*)
      in
        case stmt of
          A.Assign(id, e) =>
            ( ic_exp initSet e ext;
              ic_stmts stmts step (declSet, bogusGuard initSet id)
                       ext
            )
        | A.IfThenElse(e, s1, s2) =>
            let val _ = ic_exp initSet e ext
                val (_, initSet1, ending1) = ic_stmts s1 step env ext
                val (_, initSet2, ending2) = ic_stmts s2 step env ext
            in
              case (ending1, ending2) of
                (JUMP, JUMP) =>
                  (declSet, Symbol.intersection (initSet1, initSet2), JUMP)
              | (NORMAL, JUMP) => ic_stmts stmts step (declSet, initSet1) ext
              | (JUMP, NORMAL) => ic_stmts stmts step (declSet, initSet2) ext
              | (NORMAL, NORMAL) =>
                  ic_stmts stmts step
                           (declSet, Symbol.intersection (initSet1, initSet2))
                           ext
            end
        | A.While(e, s) =>
            (ic_exp initSet e ext;
             ic_stmts [s] NONE env ext;
             ic_stmts stmts step env ext)
        | A.For(s1, e, s2, s3) =>
            let val (declSet', initSet', _) = ic_stmts [s1] NONE env ext
                val _ = ic_exp initSet' e ext
                val (declSet'', initSet'', ending) =
                    ic_stmts [s3] (SOME s2) (declSet', initSet') ext
                val _ = case ending of JUMP => () | NORMAL =>
                          checkStep (SOME s2) (declSet, initSet'') ext;
            in
               ic_stmts stmts step
                        (declSet, Symbol.intersection (declSet, initSet'))
                        ext
            end
        | A.Continue => (checkStep step env ext; (declSet, initSet, JUMP))
        | A.Break => (declSet, initSet, JUMP)
        | A.Return(e) => (ic_exp initSet e ext; (declSet, initSet, JUMP))
        | A.Nop => ic_stmts stmts step env ext
        | A.Seq(stmts') =>
            let val (declSet', initSet', ending1) = ic_stmts stmts' step env ext
            in
              case ending1 of
                JUMP => (declSet, Symbol.intersection(declSet, initSet'), JUMP)
              | NORMAL =>
                  ic_stmts stmts step
                           (declSet, Symbol.intersection(declSet, initSet'))
                           ext
            end
        | A.Declare(id, eOpt) =>
            (case eOpt of
               NONE => ic_stmts stmts step (Symbol.add declSet id, initSet) ext
             | SOME e => ( ic_exp initSet e ext;
                           ic_stmts stmts step (Symbol.add declSet id,
                                                Symbol.add initSet id)
                                    ext
                         ))
        | A.Markeds(marked_stmt) =>
            (ic_stmts ((Mark.data marked_stmt)::stmts) step
                      env (Mark.ext marked_stmt))
      end

  fun initcheck prog = (ic_stmts prog NONE (Symbol.null, Symbol.null) NONE; ())

end
