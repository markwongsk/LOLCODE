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

  (* Make sure the unary operation matches the type, then return the
   * result type.
   * unopCheck : A.unop -> A.c0type -> Mark.ext option -> c0type *)
  fun unopCheck unop t ext =
      let fun check cond = if cond then () else
                           ( ErrorMsg.error ext (
                             "type mismatch: " ^ A.Print.pp_unop_oper unop ^
                             " doesn't apply to type " ^ A.Print.pp_type t) ;
                             raise ErrorMsg.Error )
      in case unop of
           A.NOT => (check (t = A.BOOL); A.BOOL)
         | A.BNOT => (check (t = A.INT); A.INT)
         | A.NEGATIVE => (check (t = A.INT); A.INT)
      end

  (* Make sure the binary operation matches the types, then return the
   * result type.
   * binopCheck : A.binop -> (A.c0type * A.c0type) -> Mark.ext option -> c0type
   *)
  fun binopCheck binop (t1, t2) ext =
      let fun check cond = if cond then () else
                           ( ErrorMsg.error ext (
                             "type mismatch: " ^ A.Print.pp_binop_oper binop ^
                             " doesn't apply to types " ^ A.Print.pp_type t1 ^
                             " and " ^ A.Print.pp_type t2) ;
                             raise ErrorMsg.Error )
          val bothBool = (t1 = A.BOOL andalso t2 = A.BOOL)
          val bothInt = (t1 = A.INT andalso t2 = A.INT)
      in case binop of
           A.PLUS => (check bothInt; A.INT)
         | A.MINUS => (check bothInt; A.INT)
         | A.TIMES => (check bothInt; A.INT)
         | A.DIVIDEDBY => (check bothInt; A.INT)
         | A.MODULO => (check bothInt; A.INT)
         | A.LT => (check bothInt; A.BOOL)
         | A.LEQ => (check bothInt; A.BOOL)
         | A.GT => (check bothInt; A.BOOL)
         | A.GEQ => (check bothInt; A.BOOL)
         | A.EQ => (check (bothBool orelse bothInt); A.BOOL)
         | A.NEQ => (check (bothBool orelse bothInt); A.BOOL)
         | A.AND => (check bothBool; A.BOOL)
         | A.OR => (check bothBool; A.BOOL)
         | A.BAND => (check bothInt; A.INT)
         | A.BOR => (check bothInt; A.INT)
         | A.BXOR => (check bothInt; A.INT)
         | A.LSHIFT => (check bothInt; A.INT)
         | A.RSHIFT => (check bothInt; A.INT)
      end

  (* ternCheck (c0type * c0type * c0type) -> Mark.ext option -> c0type *)
  fun ternCheck (t1, t2, t3) ext =
      if (t1 <> A.BOOL) then ( ErrorMsg.error ext (
                               "Condition to the ternary expression " ^
                               "must be a boolean");
                               raise ErrorMsg.Error )
      else if (t2 <> t3) then ( ErrorMsg.error ext (
                                "The types of the branches in a ternary " ^
                                "expression must have the same type");
                                raise ErrorMsg.Error )
      else t2 (* same as t3. *)

  fun checkMatch expect got name ext =
      if (expect = got) then ()
      else ( ErrorMsg.error ext (name ^ " expects " ^ A.Print.pp_type expect ^
                                 ", but got " ^ A.Print.pp_type got);
           raise ErrorMsg.Error)

  (* tc_exp : A.c0type Symbol.table -> Ast.exp -> Mark.ext option -> A.c0type *)
  fun tc_exp table (A.Var id) ext =
      (case Symbol.look table id
	    of NONE => ( ErrorMsg.error ext ("undeclared variable `" ^
                                         Symbol.name id ^ "'") ;
		             raise ErrorMsg.Error )
	     | SOME t => t)
    | tc_exp table (A.ConstExp(A.IntConst(_))) ext = A.INT
    | tc_exp table (A.ConstExp(_)) ext = A.BOOL
    | tc_exp table (A.UnopExp(unop, e)) ext =
        unopCheck unop (tc_exp table e ext) ext
    | tc_exp table (A.BinopExp(binop, e1, e2)) ext =
        binopCheck binop ((tc_exp table e1 ext), (tc_exp table e2 ext)) ext
    | tc_exp table (A.Tern(cond, e1, e2)) ext =
        ternCheck (tc_exp table cond ext,
                   tc_exp table e1 ext,
                   tc_exp table e2 ext) ext
    | tc_exp table (A.Marked(marked_exp)) ext =
        tc_exp table (Mark.data marked_exp) (Mark.ext marked_exp)

  (* tc_stmts : A.stmt list -> A.c0type Symbol.table -> Mark.ext option ->
   *            A.c0type Symbol.table *)
  and tc_stmts [] table ext = table
    | tc_stmts (stmt::stmts) table ext =
      case stmt
       of A.Markeds(marked_stmt) =>
          (tc_stmts ((Mark.data marked_stmt)::stmts)
                    table (Mark.ext marked_stmt))
        | _ =>
      let
(*        val _ = print ("considering statement : " ^ A.Print.pp_stmt stmt ^ "\n")*)
        (* Guard an addition to the symbol table. *)
        fun checkBind table (id, t) ext =
            case Symbol.look table id of
              NONE => Symbol.bind table (id, t)
            | SOME _ => (
                ErrorMsg.error ext ("redeclared variable `" ^
                                     Symbol.name id ^ "'") ;
		        raise ErrorMsg.Error
              )
        fun checkAssign table (id, t) ext =
            if Symbol.is_bogus id then table
            else
              case Symbol.look table id of
                NONE => ( ErrorMsg.error ext ("undeclared variable `" ^
                                              Symbol.name id ^ "'") ;
		                  raise ErrorMsg.Error )
              | SOME (t') => (checkMatch t' t "Assignment rvalue" ext; table)
        val newTable =
            case stmt of
              A.Assign(id, e) =>
                checkAssign table (id, tc_exp table e ext) ext
            | A.IfThenElse(e, s1, s2) =>
                (checkMatch A.BOOL (tc_exp table e ext) "If" ext;
                 tc_stmts [s1] table ext; tc_stmts [s2] table ext; table)
            | A.While(e, s) =>
                (checkMatch A.BOOL (tc_exp table e ext) "While" ext;
                 tc_stmts [s] table ext; table)
            | A.For(s1, e, s2, s3) =>
                let val table' = tc_stmts [s1] table ext
                in (checkMatch A.BOOL (tc_exp table' e ext) "For" ext;
                    tc_stmts [s2] table' ext; tc_stmts [s3] table' ext; table)
                end
            | A.Continue => table
            | A.Break => table
            | A.Return(e) =>
                (checkMatch A.INT (tc_exp table e ext) "Return" ext; table)
            | A.Nop => table
            | A.Seq(stmts') => (tc_stmts stmts' table ext; table)
            | A.Declare(id, t, e) =>
                (if isSome e then
                   checkMatch t (tc_exp table (valOf e) ext) "Declaration" ext
                 else () ;
                 checkBind table (id, t) ext)
            | A.Markeds(marked_stmt) => raise ImpossibleStateException
      in
        tc_stmts stmts newTable ext
      end

  fun typecheck prog = (tc_stmts prog (Symbol.empty) NONE; ())

end
