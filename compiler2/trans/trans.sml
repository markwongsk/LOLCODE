(* L1 Compiler
 * AST -> IR Translator
 *
 * author: Andrew Aaudiber and Mark Wong Siang Kai
 *        \/                /\
 * cr == caron and cf == circumflex
 *)

signature TRANS =
sig
  (* translate abstract syntax tree to IR tree *)
  val translate : Ast.program -> Tree.program
end

structure Trans :> TRANS =
struct
  exception IllegalStateException

  structure A = Ast
  structure T = Tree

  val env : Temp.temp Symbol.table ref = ref Symbol.empty

local
  fun var_helper id =
    case Symbol.look (!env) id of
      NONE => let
                val t = Temp.new ()
                val env' = Symbol.bind (!env) (id,t)
                val _ = env := env'
              in
                t
              end
    | SOME t => t

  local
    fun toSmlTruth A.TRUE = true | toSmlTruth A.FALSE = false
    fun fromSmlTruth b = if b then A.TRUE else A.FALSE
  in
  (* Fold constants using the given operation. It is an error to call
   * fold_consts with an impure binop. *)
  fun fold_consts (A.PLUS, A.IntConst(c1), A.IntConst(c2)) =
        T.CONST(A.IntConst(Word32.+(c1, c2)))
    | fold_consts (A.MINUS, A.IntConst(c1), A.IntConst(c2)) =
        T.CONST(A.IntConst(Word32.-(c1, c2)))
    | fold_consts (A.TIMES, A.IntConst(c1), A.IntConst(c2)) =
        T.CONST(A.IntConst(Word32.*(c1, c2)))
    | fold_consts (A.LT, A.IntConst(c1), A.IntConst(c2)) =
        T.CONST(fromSmlTruth (Word32.<(c1, c2)))
    | fold_consts (A.LEQ, A.IntConst(c1), A.IntConst(c2)) =
        T.CONST(fromSmlTruth (Word32.<=(c1, c2)))
    | fold_consts (A.GT, A.IntConst(c1), A.IntConst(c2)) =
        T.CONST(fromSmlTruth (Word32.>(c1, c2)))
    | fold_consts (A.GEQ, A.IntConst(c1), A.IntConst(c2)) =
        T.CONST(fromSmlTruth (Word32.>=(c1, c2)))
    | fold_consts (A.EQ, c1, c2) =
        if c1 = c2 then T.CONST(A.TRUE) else T.CONST(A.FALSE)
    | fold_consts (A.NEQ, c1, c2) =
        if c1 <> c2 then T.CONST(A.TRUE) else T.CONST(A.FALSE)
    | fold_consts (A.AND, c1, c2) =
        if toSmlTruth c1 andalso toSmlTruth c2 then T.CONST(A.TRUE)
        else T.CONST(A.FALSE)
    | fold_consts (A.OR, c1, c2) =
        if toSmlTruth c1 orelse toSmlTruth c2 then T.CONST(A.TRUE)
        else T.CONST(A.FALSE)
    | fold_consts (A.BAND, A.IntConst(c1), A.IntConst(c2)) =
        T.CONST(A.IntConst(Word32.andb(c1, c2)))
    | fold_consts (A.BOR, A.IntConst(c1), A.IntConst(c2)) =
        T.CONST(A.IntConst(Word32.orb(c1, c2)))
    | fold_consts (A.BXOR, A.IntConst(c1), A.IntConst(c2)) =
        T.CONST(A.IntConst(Word32.xorb(c1, c2)))
    | fold_consts (A.LSHIFT, A.IntConst(c1), A.IntConst(c2)) =
        let val shift = Word.andb(Word.fromInt(Word32.toInt c2),
                                  Word.fromInt(0x1F))
        in T.CONST(A.IntConst(Word32.<<(c1, shift)))
        end
    | fold_consts (A.RSHIFT, A.IntConst(c1), A.IntConst(c2)) =
        let val shift = Word.andb(Word.fromInt(Word32.toInt c2),
                                  Word.fromInt(0x1F))
        in T.CONST(A.IntConst(Word32.~>>(c1, shift)))
        end
  end
in
  fun not_helper e =
    T.PBINOP(Ast.PLUS, T.PBINOP(Ast.MINUS, Consts.zero, e), Consts.one)

  and unop_compressor (oper, e) =
    case e of
      A.UnopExp(unop, e') => if unop = oper then trans_exp e'
                             else trans_unop_exp (oper, e)
    | A.Marked(marked_exp) => unop_compressor (oper, Mark.data marked_exp)
    | _ => trans_unop_exp (oper, e)

  (* translate the unary operators *)
  and trans_unop_exp (A.NEGATIVE, e) =
        let
          val (ecr, ecf) = trans_exp e
        in
          (ecr, T.PBINOP(Ast.MINUS, Consts.zero, ecf))
        end
    | trans_unop_exp (A.NOT, e) =
        let
          val (ecr, ecf) = trans_exp e
        in
          (ecr, not_helper ecf)
        end
    | trans_unop_exp (A.BNOT, e) =
        let
          val (ecr, ecf) = trans_exp e
        in
          (ecr, T.PBINOP(Ast.MINUS, T.PBINOP(Ast.MINUS, Consts.zero, ecf),
                         Consts.one))
        end

  (* translate the impure binary operators *)
  and trans_ebinop_exp (oper, e1, e2) =
    let
      val temp = Temp.new ()
      val (e1cr, e1cf) = trans_exp e1
      val (e2cr, e2cf) = trans_exp e2
    in
      (e1cr @ e2cr @ [T.EBINOP(temp, oper, e1cf, e2cf)], T.TEMP temp)
    end

  (* translate the pure binary operators *)
  and trans_pbinop_exp (oper, e1, e2) =
    let
      val (e1cr, e1cf) = trans_exp e1
      val (e2cr, e2cf) = trans_exp e2
    in
      (* If both sides of the binop are constants, fold them *)
      case (e1cf, e2cf) of
        (T.CONST(c1), T.CONST(c2)) => ([], fold_consts(oper, c1, c2))
      | _ => (e1cr @ e2cr, T.PBINOP(oper, e1cf, e2cf))
    end

  (* translate all binary operators *)
  and trans_binop_exp (A.DIVIDEDBY : A.binop, e1 : A.exp, e2 : A.exp) =
        trans_ebinop_exp (A.DIVIDEDBY, e1, e2)
    | trans_binop_exp (A.MODULO, e1, e2) =
        trans_ebinop_exp (A.MODULO, e1, e2)
    | trans_binop_exp (A.AND, e1, e2) = trans_tern_exp (e1, e2, A.ConstExp(A.FALSE))
    | trans_binop_exp (A.OR, e1, e2) = trans_tern_exp (e1, A.ConstExp(A.TRUE), e2)
    | trans_binop_exp (oper, e1, e2) = trans_pbinop_exp (oper, e1, e2)

  and trans_tern_exp (A.ConstExp(A.TRUE), e1, e2) = trans_exp  e1
    | trans_tern_exp (A.ConstExp(A.FALSE), e1, e2) = trans_exp  e2
    | trans_tern_exp (e1, e2, e3) =
        let
          val label1 = ("false_clause_",Label.new ())
          val label2 = ("end_of_ternary_",Label.new ())
          val temp = Temp.new ()
          val (e1cr, e1cf) = trans_exp e1
          val (e2cr, e2cf) = trans_exp e2
          val (e3cr, e3cf) = trans_exp e3
          val not_e1cf = not_helper e1cf
        in
          (e1cr @
           [T.IFGOTO(not_e1cf, label1)] @
           e2cr @
           [T.MOVE(temp, e2cf),
            T.GOTO(label2),
            T.LABEL(label1)] @
           e3cr @
           [T.MOVE(temp, e3cf),
            T.LABEL(label2)], T.TEMP temp)
        end

  and trans_exp (A.Var(id)) = ([], T.TEMP (var_helper id))
    | trans_exp (A.ConstExp const) = ([], T.CONST(const))
    | trans_exp (A.UnopExp(unop,e)) = unop_compressor (unop,e)
    | trans_exp (A.BinopExp(binop,e1,e2)) = trans_binop_exp (binop,e1,e2)
    | trans_exp (A.Marked(marked_exp)) = trans_exp (Mark.data marked_exp)
    | trans_exp (A.Tern(e1,e2,e3)) = trans_tern_exp (e1,e2,e3)

  (* translate the statement given the ironment of temps, the Ast statement
   * and the break label and continue label
   *)
  and trans_stmt (A.Assign(id,e)) _ _ =
        let
          val (ecr,ecf) = trans_exp e
        in
          ecr @ [T.MOVE(var_helper id, ecf)]
        end
    | trans_stmt (A.IfThenElse(e,s1,s2)) br c =
        let
          val label1 = ("true_clause_",Label.new ())
          val label2 = ("end_of_conditional_",Label.new ())
          val (ecr, ecf) = trans_exp  e
          val s1cr = trans_stmt  s1 br c
          val s2cr = trans_stmt  s2 br c
        in
          (ecr @
           [T.IFGOTO(ecf, label1)] @
           s2cr @
           [T.GOTO(label2),
            T.LABEL(label1)] @
           s1cr @
           [T.LABEL(label2)])
        end
    | trans_stmt (A.While(e,s)) br c =
        trans_stmt (A.For(A.Nop,e,A.Nop,s)) br c
    | trans_stmt (A.For(s1,e,s2,s3)) br c =
        let
          val label1 = ("for_body_",Label.new ()) (* main body *)
          val label2 = ("for_step_",Label.new ()) (* continue label *)
          val label3 = ("end_of_for_",Label.new ()) (* break label *)
          val s1cr = trans_stmt  s1 br c
          val (ecr, ecf) = trans_exp  e
          val s2cr = trans_stmt  s2 br c
          val s3cr = trans_stmt  s3 (SOME(label3)) (SOME(label2))
          val not_ecf = not_helper ecf
        in
          (s1cr @
           [T.LABEL(label1)] @
           ecr @
           [T.IFGOTO(not_ecf, label3)] @
           s3cr @
           [T.LABEL(label2)] @
           s2cr @
           [T.GOTO(label1),
            T.LABEL(label3)])
        end
    | trans_stmt (A.Continue) _ (SOME(c)) = [T.GOTO(c)]
    | trans_stmt (A.Break) (SOME(br)) _ = [T.GOTO(br)]
    | trans_stmt (A.Return e) _ _ =
        let
          val (ecr, ecf) = trans_exp e
        in
          ecr @ [T.RETURN(ecf)]
        end
    | trans_stmt (A.Nop) _ _ = []
    | trans_stmt (A.Seq (stms)) br c =
        trans_stmts stms br c
    | trans_stmt (A.Declare (id,t,NONE)) _ _ = []
    | trans_stmt (A.Declare (id,t,SOME e)) br c =
        trans_stmt (A.Assign(id,e)) br c
    | trans_stmt (A.Markeds (marked_stm)) br c =
        trans_stmt  (Mark.data marked_stm) br c
    | trans_stmt _ _ _ = raise IllegalStateException

  and trans_stmts (stm::stms) br c =
        (trans_stmt  stm br c) @ trans_stmts stms br c
    | trans_stmts  _ _ _ = []

  fun translate stms = trans_stmts stms NONE NONE
end

end
