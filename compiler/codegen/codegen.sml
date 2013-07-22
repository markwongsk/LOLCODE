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
  val codegen : Tree.stm list -> Assem.instr list
end

structure Codegen :> CODEGEN =
struct
  structure T = Tree
  structure AS = Assem
  type instr = Assem.instr

  fun munch_op T.ADD = AS.ADD
    | munch_op T.SUB = AS.SUB
    | munch_op T.MUL = AS.MUL
    | munch_op T.DIV = AS.DIV
    | munch_op T.MOD = AS.MOD

  (* for div and mod, we have to keep eax, edx live as
   * they are used in the x86-64 instructions *)
  fun translate AS.DIV d t1 t2 =
    (* MOV e1, %eax
     * IDIV e2
     * MOV %eax, d
     *)
    [AS.MOV(AS.REG(AS.EAX), t1),
     AS.CDQ,
     AS.BINOP(AS.DIV, d, AS.REG(AS.EAX), t2),
     AS.MOV(d, AS.REG(AS.EAX))]

    | translate AS.MOD d t1 t2 =
    (* MOV e1, %eax
     * IDIV e2
     * MOV %edx, d *)
    [AS.MOV(AS.REG(AS.EAX), t1),
     AS.CDQ,
     AS.BINOP(AS.DIV, d, AS.REG(AS.EAX), t2),
     AS.MOV(d, AS.REG(AS.EDX))]

    | translate oper d t1 t2 =
    (* MOV e1, d
     * BINOP e2, d
     *)
    [AS.MOV(d, t1),
     AS.BINOP(oper, d, d, t2)]

  (* munch_exp : AS.operand -> T.exp -> AS.instr list *)
  (* munch_exp d e
   * generates instructions to achieve d <- e
   * d must be TEMP(t) or REG(r)
   *)
  fun munch_exp d (T.CONST(n)) = [AS.MOV(d, AS.IMM(n))]
    | munch_exp d (T.TEMP(t)) = [AS.MOV(d, AS.TEMP(t))]
    | munch_exp d (T.BINOP(binop, e1, e2)) =
        munch_binop d (binop, e1, e2)

  (* munch_binop : AS.operand -> T.binop * T.exp * T.exp -> AS.instr list *)
  (* munch_binop d (binop, e1, e2)
   * generates instruction to achieve d <- e1 binop e2
   * d must be TEMP(t) or REG(r)
   *)
  and munch_binop d (binop, e1, e2) =
    let
      val operator = munch_op binop
      val t1 = AS.TEMP(Temp.new())
      val t2 = AS.TEMP(Temp.new())
    in
      munch_exp t1 e1
      @ munch_exp t2 e2
      @ translate operator d t1 t2
    end

  (* munch_stm : T.stm -> AS.instr list *)
  (* munch_stm stm generates code to execute stm *)
  fun munch_stm (T.MOVE(T.TEMP(t1), e2)) =
        munch_exp (AS.TEMP(t1)) e2
    | munch_stm (T.RETURN(e)) =
        (* return e is implemented as %eax <- e *)
        munch_exp (AS.REG(AS.EAX)) e

  fun codegen nil = nil
    | codegen (stm::stms) = munch_stm stm @ codegen stms

end
