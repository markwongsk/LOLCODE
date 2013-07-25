(* L1 Compiler
 * Assembly language
 * Author: Kaustuv Chaudhuri <kaustuv+@andrew.cmu.edu>
 * Modified By: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Currently just a pseudo language with 3-operand
 * instructions and arbitrarily many temps
 *
 * We write
 *
 * BINOP  operand1 <- operand2,operand3
 * MOV    operand1 <- operand2
 *)

signature ASSEM =
sig
  exception ElemNotFoundException

  datatype reg = EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP |
                 R8D | R9D | R10D | R12D | R13D | R14D | R15D | SPILL

  (* convenient map from indices to registers *)
  val registers : reg list

  val usedSpecialRegs : reg list

  datatype operand =
       IMM of Word32.word
     | REG of reg
     | TEMP of Temp.temp
     | STACK of int

  datatype operation = ADD | SUB| MUL | DIV | MOD | AND | OR | XOR |
                       SAL | SAR

  datatype jmp = JE | JNE | JGE | JLE | JG | JL

  datatype setf = TEST of operand
                | CMP of (operand * operand)

  datatype instr =
       BINOP of operation * operand * operand * operand
     | MOV of operand * operand
     | CDQ
     | GOTO of Label.label
     | SETF of setf
     | JMP of jmp * Label.label
     | LABEL of Label.label
     | DIRECTIVE of string
     | COMMENT of string

  (* check whether two operands are equal *)
  val equals : operand -> operand -> bool

  (* formats the instruction into a printable string such that
   * it conforms to the format of x86-64 *)
  val format : instr -> string

  (* formats the given operand to its x86-64 name. It is up to
   * the implementor to decide what to do with TEMPs *)
  val formatOperand : operand -> string

  (* converts the given instruction into a string such that
   * it conveys all information in the instruction *)
  val toString : instr -> string

  (* remaps the given color to the appropriate register or stack *)
  val remapColor : int -> operand

  (* gets the color of the register *)
  val getColor : reg -> int
end

structure Assem : ASSEM =
struct
  exception IllegalStateException
  exception ElemNotFoundException

  (* we are in x86-64, but we are dealing with Word32 *)
  datatype reg = EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP |
                 R8D | R9D | R10D | R12D | R13D | R14D | R15D | SPILL

  (* convenient map from indices to registers.
   * we exclude esp, r11d since they are special-purpose. *)
  val registers = [EAX, EDX, ECX, EBX, ESI, EDI, EBP,
                   R8D, R9D, R10D, R12D, R13D, R14D, R15D]

  (* the special registers for pre-coloring *)
  val usedSpecialRegs = List.take (registers,3)


  (* stack offset for the next 'register'
   * NOTE: this value is kept as a positive offset, but will be
   * printed out as negative. For example, if the offset is
   * 20 the output will be -20(%rsp)
   *)
  val offset = ref 0

  datatype operand =
       IMM of Word32.word
     | REG of reg
     | TEMP of Temp.temp
     | STACK of int

  datatype operation = ADD | SUB| MUL | DIV | MOD | AND | OR | XOR |
                       SAL | SAR

  datatype jmp = JE | JNE | JGE | JLE | JG | JL

  datatype setf = TEST of operand
                | CMP of (operand * operand)

  (* maps ints to REG reg or STACK offset *)
  val colorMap : operand IntHashTable.hash_table =
    IntHashTable.mkTable (10, ElemNotFoundException)

  val _ = List.app (fn i => IntHashTable.insert colorMap
            (i, REG (List.nth (registers, i))))
            (List.tabulate (List.length registers, (fn j => j)))

  datatype instr =
       BINOP of operation * operand * operand * operand
     | MOV of operand * operand
     | CDQ
     | GOTO of Label.label
     | SETF of setf
     | JMP of jmp * Label.label
     | LABEL of Tree.label
     | DIRECTIVE of string
     | COMMENT of string

  fun equals op1 op2 =
    case (op1,op2) of
      (IMM w1, IMM w2) => w1 = w2
    | (REG r1, REG r2) => r1 = r2
    | (TEMP t1, TEMP t2) => Temp.compare (t1,t2) = EQUAL
    | (_, _) => false

  fun formatReg EAX = "%eax"
    | formatReg EBX = "%ebx"
    | formatReg ECX = "%ecx"
    | formatReg EDX = "%edx"
    | formatReg ESI = "%esi"
    | formatReg EDI = "%edi"
    | formatReg EBP = "%ebp"
    | formatReg ESP = "%esp"
    | formatReg R8D = "%r8d"
    | formatReg R9D = "%r9d"
    | formatReg R10D = "%r10d"
    | formatReg R12D = "%r12d"
    | formatReg R13D = "%r13d"
    | formatReg R14D = "%r14d"
    | formatReg R15D = "%r15d"
    | formatReg SPILL = "%r11d"

  fun formatBinop ADD = "ADD"
    | formatBinop SUB = "SUB"
    | formatBinop MUL = "IMUL"
    | formatBinop DIV = "IDIV"
    | formatBinop MOD = "IDIV"
    | formatBinop AND = "AND"
    | formatBinop OR = "OR"
    | formatBinop XOR = "XOR"
    | formatBinop SAL = "SAL"
    | formatBinop SAR = "SAR"

  fun formatOperand (IMM(n)) = "$" ^ Word32Signed.toString(n)
    | formatOperand (TEMP(t)) = Temp.name(t)
    | formatOperand (REG(r)) = formatReg r
    | formatOperand (STACK(i)) = "-" ^ (Int.toString i) ^ "(%rsp)"

  fun formatJmp (JE) = "JE"
    | formatJmp (JNE) = "JNE"
    | formatJmp (JGE) = "JGE"
    | formatJmp (JLE) = "JLE"
    | formatJmp (JG) = "JG"
    | formatJmp (JL) = "JL"

  fun toString (BINOP(oper, d, s1, s2)) =
        "\t" ^ formatBinop oper ^ "\t" ^ formatOperand d ^ " <- " ^
        formatOperand s1 ^ "," ^ (formatOperand s2) ^ "\n"
    | toString (MOV(d, s)) =
        "\t" ^ "MOV" ^ "\t" ^ formatOperand d ^ " <- " ^ formatOperand s ^ "\n"
    | toString (CDQ) = "\tCDQ\n"
    | toString (GOTO((name,label))) = "\tGOTO\tc0" ^ name ^ Int.toString label ^ "\n"
    | toString (SETF(TEST(oper))) =
       "\tTEST\t" ^ formatOperand oper ^ "," ^ formatOperand oper ^ "\n"
    | toString (SETF(CMP(oper1, oper2))) =
        "\tCMP\t" ^ formatOperand oper1 ^ "," ^ formatOperand oper2 ^ "\n"
    | toString (JMP(jmp, (name,label))) =
      "\t" ^ formatJmp jmp ^ "\tc0" ^ name ^ Int.toString label ^ "\n"
    | toString (LABEL(name,label)) =
      "c0" ^ name ^ Int.toString label ^ ":\n"
    | toString (DIRECTIVE(str)) =
        str ^ "\n"
    | toString (COMMENT(str)) =
        "/* " ^ str ^ "*/\n"

local
  (* defaultFormat : instr -> string *)
  fun defaultFormat (BINOP(oper, d, s1, s2)) =
        if oper = DIV orelse oper = MOD then
          "\t" ^ formatBinop oper ^ "\t" ^ formatOperand s2 ^ "\n"
        else
          "\t" ^ formatBinop oper ^ "\t" ^ formatOperand s2 ^ "," ^
        formatOperand d ^ "\n"
    | defaultFormat (MOV(d, s)) =
        "\tMOV\t" ^ formatOperand s ^ "," ^ formatOperand d ^ "\n"
    | defaultFormat (CDQ) = "\tCDQ\n"
    | defaultFormat (GOTO (name,label)) = "\tJMP\tc0" ^ name ^ Int.toString label ^ "\n"
    | defaultFormat (SETF setf) = toString (SETF(setf))
    | defaultFormat (JMP(jmp, label)) = toString (JMP(jmp, label))
    | defaultFormat (LABEL label) = toString (LABEL(label))
    | defaultFormat (DIRECTIVE str) = str ^ "\n"
    | defaultFormat (COMMENT str) = "/*" ^ str ^ "*/\n"

  (* d1 happens to be s1 because of codegen preprocessing *)
  fun formatSpill (BINOP(oper, d, s1, s2)) =
        defaultFormat(MOV(REG SPILL, d)) ^
        defaultFormat(BINOP(oper, REG SPILL, REG SPILL, s2)) ^
        defaultFormat(MOV(d, REG SPILL))
    | formatSpill(MOV(d, s)) =
        defaultFormat(MOV(REG SPILL, s)) ^
        defaultFormat(MOV(d,REG SPILL))
    | formatSpill (SETF(CMP(opr1, opr2))) =
        defaultFormat(MOV(REG SPILL, opr2)) ^
        defaultFormat(SETF(CMP(opr1, REG SPILL)))
    | formatSpill (SETF(TEST(opr1))) =
        defaultFormat(MOV(REG SPILL, opr1)) ^
        defaultFormat (SETF(TEST(REG SPILL)))
    | formatSpill _ = raise IllegalStateException

  fun formatDivSpill (BINOP(_, _, _, d as STACK os)) =
        "\tMOV\t" ^ formatOperand d ^ "," ^ formatReg SPILL ^ "\n" ^
        "\t" ^ formatBinop DIV ^ "\t" ^ formatReg SPILL ^ "\n"
    | formatDivSpill (instr) = defaultFormat instr

  fun formatShift (BINOP(oper, d, _, REG RCX)) =
        (case d of
          STACK os => defaultFormat(MOV(REG SPILL, d)) ^
                      formatShift(BINOP(oper, REG SPILL, REG SPILL, REG RCX)) ^
                      defaultFormat(MOV(d, REG SPILL))
        | _ => "\t" ^ formatBinop oper ^ "\t%cl," ^ formatOperand d ^ "\n")
    | formatShift (BINOP(oper, d, _, _)) = raise IllegalStateException

in
  (* invariant: the binop given was previously preprocessed to
   * have the correct conditions. For example, DIV will have
   * eax as the destination and source1 operand.
   *)
  fun format instr =
    case instr of
      BINOP(oper, d, s1, s2) =>
        (* DIV, MOD will always have %eax in as destination *)
        if oper = DIV orelse oper = MOD then formatDivSpill instr

        (* for SAL, SAR, if s2 can be an immediate or a register.
         * If it is a register and it's not %ecx we will have to spill it
         *)
        else if oper = SAL orelse oper = SAR then formatShift instr

        (* as long as one of them is a register we are fine *)
        else (
          case (d,s2) of
            (REG r, _) => defaultFormat instr
          (* d must be a stack location, transfer it to r11 *)
          | (_,_) => formatSpill instr
        )
    | MOV(d, s) => (
        case (d,s) of
          (REG r, _) => defaultFormat instr
        | (_, _) => formatSpill instr
      )

    | SETF(CMP(opr1, STACK _)) => formatSpill instr
    | SETF(TEST(STACK _)) => formatSpill instr
    | _ => defaultFormat instr

end

  (* given an integer, gets an appropriate register or stack space to
   * store a word *)
  fun remapColor i =
    case IntHashTable.find colorMap i of
      SOME color => color
    | NONE =>
        let
          val _ = offset := !offset + 8
          val _ = IntHashTable.insert colorMap (i, STACK (!offset))
        in
          STACK (!offset)
        end

  fun getColor reg =
    case reg of
      EAX => 0
    | EDX => 1
    | ECX => 2
    | _ => raise IllegalStateException

end
