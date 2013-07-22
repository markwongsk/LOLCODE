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

  (* Get the successor for the given instr on the given line. *)
  fun succs(instr, lineNum) = lineNum + 1

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

  fun analyze (notes : annotations) (instr : AS.instr, lineNum : int) =
      let
        val S' = case IHT.find notes (succs(instr, lineNum)) of
                   NONE => OHS.mkEmpty 32
                 | SOME (_, S') => S'
      in
        let
          fun checkLife oper = (if OHS.member (S', oper) then () else
                                OHS.add (S', oper); oper)
          (* Copy in the successor's live variables *)
          val S = OHS.mkFromList (OHS.listItems S')
          val _ = IHT.insert notes (lineNum, (instr, S))
          (* Compute which operands should be removed/added from live set *)
          val (remove, add) =
              case instr of
                AS.MOV(td, ts) => ([checkLife td], [ts])
              | AS.BINOP(AS.DIV, _, ts1, ts2) =>
                ([AS.REG(AS.EAX)], [ts1, ts2, AS.REG(AS.EAX), AS.REG(AS.EDX)])
              | AS.BINOP(_, td, ts1, ts2) => ([checkLife td], [ts1, ts2])
              | AS.CDQ => ([AS.REG(AS.EDX)], [AS.REG(AS.EAX)])
              | _ => ([], [])
        in
        (* Update the set based on the line's own instruction. *)
          (removeAllUnlessImm S remove; addAllUnlessImm S add)
        end
      end
  end

  local
  (* enumerate the input list so that [a, b, c] => [(a, 0), (b, 1), (c, 2)] *)
  fun enum l =
      let fun enum' l i = case l of [] => [] | x::xs => (x, i)::(enum' xs (i+1))
      in enum' l 0 end

  in

  fun annotateLiveness instrs =
      let val notes : annotations = IHT.mkTable((*sizeHint=*)16, NotFound)
      in
        ( if Temp.numTemps() >= RegisterAllocator.seoThreshold then ()
          else List.app (analyze notes) (List.rev (enum instrs)) ;
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
          val _ = addVertices [AS.REG(AS.EAX), AS.REG(AS.EDX)]
      in
        (if Temp.numTemps() >= RegisterAllocator.seoThreshold then (print "here")
         else (List.app (fn (_, set) => addInterferences (OHS.listItems set))
                        notes') ;
         G
        )
      end
end
