(* L1 Compiler
 * Liveness analyzer
 * Author: Andrew Audibert <aaudiber@andrew.cmu.edu>
 *
 * Work backward to determine liveness.
 *)

(* key for Temp registers *)
structure OperandKey =
struct
  type hash_key = Assem.operand

  (* the vertices are unique by name *)
  fun hashVal operand =
      HashString.hashString (Assem.formatOperand operand)

  fun sameKey (key1 : hash_key, key2 : hash_key) = Assem.equals key1 key2
end

structure OperandHashSet = HashSetFn(OperandKey)

signature LIVENESS =
sig
  type annotations = (Assem.instr * OperandHashSet.set) IntHashTable.hash_table

  (* format the annotations into a human-readable string *)
  val format : annotations -> string

  (* analyze the variables' liveness for a given set of assembly
   * instructions *)
  val annotateLiveness : Assem.instr list -> annotations

  (* gets the interference graph for the given annotations *)
  val getInterferenceGraph : annotations -> ChordalGraph.graph
end

structure Liveness :> LIVENESS =
struct
  exception NotFound
  exception MalformedAnnotations
  exception NYI

  structure AS = Assem
  structure OHS = OperandHashSet
  structure IHT = IntHashTable
  structure CG = ChordalGraph

  type set = OperandHashSet.set
  type 'b hash_table = 'b IHT.hash_table
  type annotations = (AS.instr * OHS.set) hash_table
  type graph = ChordalGraph.graph

  datatype status = PROGRESS | SATURATED

  fun format notes =
      let
        fun rmNewline str = substring(str, 0, size str - 1)
        fun formatSet vars = "\tLive Vars: " ^
                             List.foldr (fn (a, b) => AS.formatOperand a ^
                                                      ", " ^ b) ""
                                        (OHS.listItems vars)
        fun formatEntry NONE = ""(*raise MalformedAnnotations*)
          | formatEntry (SOME(instr, liveVars)) =
              (rmNewline (AS.toString instr))  ^ " : " ^ (formatSet liveVars)
        val lines = List.tabulate
                        (IHT.numItems notes, fn i => Int.toString i ^ ": " ^
                                                     formatEntry (IHT.find notes i))
      in List.foldr (fn (a, b) => a ^ "\n" ^ b) "Annotations:" lines
      end

  local

    (* Get the successors for the given instr on the given line. *)
    fun succs labelDict instr lineNum =
        case instr of
          AS.GOTO((name,label)) => [IHT.lookup labelDict label]
        | AS.JMP(c, (name,label)) => [IHT.lookup labelDict label, lineNum + 1]
        | _ => [lineNum + 1]

    fun appUnlessImm f operand =
        case operand of
          AS.IMM(_) => ()
        | _ => (f operand; ())

    fun addUnlessImm table operand =
        appUnlessImm (fn x => OHS.add (table, x)) operand
    fun addAllUnlessImm table operands =
        List.app (addUnlessImm table) operands

    fun removeUnlessImm table operand =
        appUnlessImm (fn x => OHS.without (table, x)) operand
    fun removeAllUnlessImm table operands =
        List.app (removeUnlessImm table) operands

  in

  fun analyze notes labelDict (instr, lineNum) =
    let
      val successors = succs labelDict instr lineNum
      val S1 = case IHT.find notes (List.nth (successors, 0)) of
                 NONE => OHS.mkEmpty 2
               | SOME (_, S') => S'
      val S2 = if length successors <= 1 then OHS.mkEmpty 2
               else case IHT.find notes (List.nth (successors, 1)) of
                      NONE => OHS.mkEmpty 2
                    | SOME (_, S') => S'
      val S = case IHT.find notes lineNum of
                NONE => OHS.mkEmpty 8
              | SOME (_, S') => S'
      val _ = IHT.insert notes (lineNum, (instr, S))

      val origSize = OHS.numItems S

      (* Copy in the successors' live variables *)
      val _ = OHS.addList (S, OHS.listItems S1)
      val _ = OHS.addList (S, OHS.listItems S2)

      (* if the oper wasn't live, we should add it to the successors. *)
      fun checkLife oper = if OHS.member (S, oper) then oper
                           else (OHS.add (S1, oper);
                                 OHS.add (S2, oper);
                                 oper)

      (* Compute which operands should be removed/added from live set *)
      val (remove, add) =
          case instr of
            AS.MOV(td, ts) => ([checkLife td], [ts])
          | AS.BINOP(AS.DIV, _, ts1, ts2) =>
            ([AS.REG(AS.EAX)], [ts1, ts2, AS.REG(AS.EAX), AS.REG(AS.EDX)])
          | AS.BINOP(_, td, ts1, ts2) => ([checkLife td], [ts1, ts2])
          | AS.CDQ => ([AS.REG(AS.EDX)], [AS.REG(AS.EAX)])
          | AS.SETF(AS.CMP(ts1, ts2)) => ([], [ts1, ts2])
          | AS.SETF(AS.TEST(ts)) => ([], [ts])
          | _ => ([], [])
    in
      (* Update the set based on the line's own instruction. *)
      (removeAllUnlessImm S remove;
       addAllUnlessImm S add;
       if origSize = OHS.numItems S then SATURATED else PROGRESS
      )
    end
  end

  local
  (* enumerate the input list so that [a, b, c] => [(a, 0), (b, 1), (c, 2)] *)
  fun enum l =
      let fun enum' l i = case l of [] => [] | x::xs => (x, i)::(enum' xs (i+1))
      in enum' l 0 end

  (* return a dictionary mapping label names to line numbers *)
  fun mkLabelDict labelDict [] = ()
    | mkLabelDict labelDict ((AS.LABEL((name,label)), line)::instrs) =
        (IHT.insert labelDict (label, line);
         mkLabelDict labelDict instrs)
    | mkLabelDict labelDict (_::instrs) = mkLabelDict labelDict instrs

  fun isProgress (PROGRESS, _) = PROGRESS
    | isProgress (_, PROGRESS) = PROGRESS
    | isProgress _ = SATURATED

  fun saturateAnalysis notes labelDict enumInstrs =
    let val rev = List.rev enumInstrs
    in
      case List.foldl isProgress SATURATED
                       (List.map (analyze notes labelDict) rev) of
        PROGRESS => saturateAnalysis notes labelDict (List.rev enumInstrs)
      | SATURATED => ()
    end

  in

  fun annotateLiveness instrs =
    let
      val enumInstrs = enum instrs
      val labelDict = IHT.mkTable(16, NotFound)
      val _ = mkLabelDict labelDict enumInstrs
      val notes : annotations = IHT.mkTable((*sizeHint=*)16, NotFound)
    in
      ( if Temp.numTemps() >= Consts.seoThreshold then ()
        else saturateAnalysis notes labelDict enumInstrs ;
        notes
      )
    end
  end

  fun getInterferenceGraph notes =
      let val G = CG.emptyGraph ()
          fun addVertices list_ =
              List.app (fn elem => (CG.addVertex G elem)) list_
          fun addInterferences [] = ()
            | addInterferences (x::xs) =
              (List.app (fn elem => CG.addEdge G (x, elem)) xs;
               addInterferences xs)
          val notes' = IHT.listItems notes
          val _ = addVertices (List.map (fn t => AS.TEMP(t)) (Temp.allTemps()))
          val _ = addVertices [AS.REG(AS.EAX), AS.REG(AS.EDX), AS.REG(AS.ECX)]
      in
        (if Temp.numTemps() >= Consts.seoThreshold then ()
         else (List.app (fn (_, set) => addInterferences (OHS.listItems set))
                        notes') ;
         G
        )
      end
end
