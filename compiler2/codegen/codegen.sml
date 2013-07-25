(* L1 Compiler
 * Assembly Code Generator for FAKE assembly
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Based on code by: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Implements a "convenient munch" algorithm
 *)

signature CODEGEN =
sig
  val codegen : Tree.program -> Assem.instr list
end

structure Codegen :> CODEGEN =
struct
  structure T = Tree
  structure AS = Assem
  type instr = Assem.instr

  exception IllegalMunch

  fun munch_bool Ast.LT = AS.JL
    | munch_bool Ast.LEQ = AS.JLE
    | munch_bool Ast.GT = AS.JG
    | munch_bool Ast.GEQ = AS.JGE
    | munch_bool Ast.EQ = AS.JE
    | munch_bool Ast.NEQ = AS.JNE
    | munch_bool _ = raise IllegalMunch

  (* return (Assem symbol for this op, whether this op is int or bool) *)
  fun munch_op Ast.PLUS = SOME(AS.ADD)
    | munch_op Ast.MINUS = SOME(AS.SUB)
    | munch_op Ast.TIMES = SOME(AS.MUL)
    | munch_op Ast.DIVIDEDBY = SOME(AS.DIV)
    | munch_op Ast.MODULO = SOME(AS.MOD)
    | munch_op Ast.BAND = SOME(AS.AND)
    | munch_op Ast.BOR = SOME(AS.OR)
    | munch_op Ast.BXOR = SOME(AS.XOR)
    | munch_op Ast.LSHIFT = SOME(AS.SAL)
    | munch_op Ast.RSHIFT = SOME(AS.SAR)
    | munch_op _ = NONE

  (* for div and mod, we have to keep eax, edx live as
   * they are used in the x86-64 instructions *)
  fun translate AS.DIV d t1 t2 =
      [AS.MOV(AS.REG(AS.EAX), t1),
       AS.CDQ,
       AS.BINOP(AS.DIV, d, AS.REG(AS.EAX), t2),
       AS.MOV(d, AS.REG(AS.EAX))]
    | translate AS.MOD d t1 t2 =
      [AS.MOV(AS.REG(AS.EAX), t1),
       AS.CDQ,
       AS.BINOP(AS.DIV, d, AS.REG(AS.EAX), t2),
       AS.MOV(d, AS.REG(AS.EDX))]
  (* for sal and sar, we have to keep ecx live as they
   * are used in the x86-64 instruction *)
    | translate AS.SAL d t1 t2 =
      [AS.MOV(AS.REG(AS.ECX), t2),
       AS.MOV(d, t1),
       AS.BINOP(AS.SAL, d, d, AS.REG(AS.ECX))]
    | translate AS.SAR d t1 t2 =
      [AS.MOV(AS.REG(AS.ECX), t2),
       AS.MOV(d, t1),
       AS.BINOP(AS.SAR, d, d, AS.REG(AS.ECX))]
    | translate oper d t1 t2 =
      [AS.MOV(d, t1),
       AS.BINOP(oper, d, d, t2)]

  (* munch_exp : AS.operand -> T.exp -> AS.instr list *)
  (* munch_exp d e
   * generates instructions to achieve d <- e
   * d must be TEMP(t) or REG(r)
   *)
  fun munch_exp d (T.CONST(n)) =
        (case n of Ast.IntConst(w) => [AS.MOV(d, AS.IMM(w))]
                 | Ast.TRUE => [AS.MOV(d, AS.IMM(Word32.fromInt(1)))]
                 | Ast.FALSE => [AS.MOV(d, AS.IMM(Word32.fromInt(0)))])
    | munch_exp d (T.TEMP(t)) = [AS.MOV(d, AS.TEMP(t))]
    | munch_exp d (T.PBINOP(binop, e1, e2)) =
        munch_binop d (binop, e1, e2)

  (* munch_binop : AS.operand -> T.binop * T.exp * T.exp -> AS.instr list *)
  (* munch_binop d (binop, e1, e2)
   * generates instruction to achieve d <- e1 binop e2
   * d must be TEMP(t) or REG(r)
   *)
  and munch_binop d (binop, e1, e2) =
      let
        val operator = munch_op binop
        val jmp = if not (isSome operator) then munch_bool binop
                  else AS.JE (* Trash value - won't be used *)
        val t1 = AS.TEMP(Temp.new())
        val t2 = AS.TEMP(Temp.new())
      in
        case operator of
          SOME(oper) =>
            munch_exp t1 e1
            @ munch_exp t2 e2
            @ translate oper d t1 t2
        | NONE =>
            let val label = ("bool_label_",Label.new ())
            in
              munch_exp t1 e1
              @ munch_exp t2 e2
              @ [AS.MOV(d, AS.IMM(Word32.fromInt 1)),
                 AS.SETF(AS.CMP(t2, t1)),
                 AS.JMP(jmp, label),
                 AS.MOV(d, AS.IMM(Word32.fromInt 0)),
                 AS.LABEL(label)]
            end
      end

  fun munch_ifgoto e label =
    let
      val t1 = AS.TEMP(Temp.new())
    in
      munch_exp t1 e
      @ [AS.SETF(AS.TEST(t1)),
         AS.JMP(AS.JNE, label)]
    end

  (* munch_cmd : T.cmd -> AS.instr list *)
  (* munch_cmd cmd generates code to execute cmd *)
  fun munch_cmd (T.MOVE(t, e)) = munch_exp (AS.TEMP(t)) e
    | munch_cmd (T.EBINOP(t, oper, e1, e2)) = munch_binop (AS.TEMP(t)) (oper, e1, e2)
    | munch_cmd (T.GOTO(label)) = [AS.GOTO(label)]
    | munch_cmd (T.IFGOTO(e, label)) = munch_ifgoto e label
    | munch_cmd (T.LABEL(label)) = [AS.LABEL(label)]
    | munch_cmd (T.RETURN(e)) =
        (* return e is implemented as %eax <- e *)
        munch_exp (AS.REG(AS.EAX)) e @ [AS.DIRECTIVE "RET"]

  fun codegen nil = nil
    | codegen (cmd::cmds) = munch_cmd cmd @ codegen cmds

end
