signature REGISTER_ALLOCATOR =
sig
  (* magic number beyond which we will not use the seo algorithm
   * and will just put everything on the stack
   *)
  val seoThreshold : int

  (* given a graph, produces a coloring mapping each vertex to
   * an int (which represents a color
   *)
  val colorGraph : ChordalGraph.graph -> int CGVHashTable.hash_table

  (* given a list of assembly instructions and a mapping from temps to
   * registers, remaps them so that there are no temps.
   *)
  val remap : int CGVHashTable.hash_table -> Assem.instr list ->
    Assem.instr list
end

structure RegisterAllocator : REGISTER_ALLOCATOR =
struct
  exception ElemNotFoundException

  structure Assem = Assem
  structure CG = ChordalGraph
  structure CGVHashTable = CGVHashTable
  structure IntHashSet = HashSetFn(IntHashTable.Key)

  open CG

  val seoThreshold = 30

  (* initial size for hashtables *)
  val initSize = 10

  fun filterPreOrder (Assem.REG reg) = true
    | filterPreOrder _ = false

local
  (* findLargestBucket : CGVHashTable.hash_table -> int ref -> CGVHashSet.set *)
  (* finds the largest bucket given a ref to the max *)
  fun findLargestBucket ht max =
    let
      val hs = IntHashTable.lookup ht (!max)
    in
      if CGVHashSet.isEmpty hs then
        let val _ = max := !max - 1 in findLargestBucket ht max end
      else hs
    end

  (* increaseLevel : CGVHashTable.hash_table -> CG.graph -> CG.vertex -> int ref *)
  (* updates the ref to the max and the table appropriately *)
  fun increaseLevel ht g max v =
    let
      val weight = getWeight g v

      (* find the hash set you are in *)
      val hs = IntHashTable.lookup ht weight

      (* remove yourself from the hashset *)
      val true = CGVHashSet.delete (hs, v)

      (* update your weight by one and put yourself in the right place *)
      val _ = incrementWeight g v

      (* find the new hash set that you are in *)
      val hs' = IntHashTable.lookup ht (weight + 1)

      (* add yourself to it *)
      val _ = CGVHashSet.add (hs', v)

    in
      (* update the max if necessary *)
      max := Int.max (!max, weight + 1)
    end
in
  fun seo g =
    let
      val n = numVertices g
      val vertices = CGVHashSet.listItems (getVertices g)
      val specialVertices = CGVHashSet.mkFromList (List.filter filterPreOrder vertices)
      val normalVertices = CGVHashSet.mkFromList (List.filter (not o filterPreOrder) vertices)
      val vwTable : CGVHashSet.set IntHashTable.hash_table  = IntHashTable.mkTable (n, ElemNotFoundException)
      val max = ref 0

      (* updates only temp registers *)
      fun updateNonSpecial (Assem.TEMP temp) =
            increaseLevel vwTable g max (Assem.TEMP temp)
        | updateNonSpecial _ = ()

      (* initializes the hash sets *)
      val _ = List.app (fn i => IntHashTable.insert vwTable
                (if i = 0 then (i, CGVHashSet.mkFromList vertices)
                 else (i, CGVHashSet.mkEmpty initSize)))
                (List.tabulate (n, fn j => j))

      (* preOrder : CG.graph -> CG.vertex list *)
      fun preOrder vertices =
        if CGVHashSet.isEmpty vertices then []
        else (
          let
            (* the special registers must have weight zero *)
            val hs = IntHashTable.lookup vwTable 0
            val SOME v = CGVHashSet.takeElem vertices
            val true = CGVHashSet.delete (hs, v)
            val _ = updateWeight g v (~n)
            val _ = CGVHashSet.app updateNonSpecial (neighbor g v)
          in
            v :: (preOrder vertices)
          end
        )

      (* orderVertices : CG.graph -> CG.vertex list *)
      fun orderVertices vertices =
        if CGVHashSet.isEmpty vertices then []
        else (
          let
            (* finds the largest bucket based on the int ref *)
            val hs = findLargestBucket vwTable max
            val SOME v = CGVHashSet.takeElem hs
            val true = CGVHashSet.delete (vertices, v)

            (* update your weight so that when you neighbor tries to
             * update your weight he doesn't
             *)
            val _ = updateWeight g v (~n)

            fun increaseLevelIfNecessary ht g max v =
              if getWeight g v >= 0 then increaseLevel vwTable g max v
              else ()

            val _ = CGVHashSet.app (increaseLevelIfNecessary vwTable g max) (neighbor g v)
          in
            v :: (orderVertices vertices)
          end
        )
    in
      (preOrder specialVertices) @ (orderVertices normalVertices)
    end
end

local
  (* findMEX : IntHashSet.set -> int *)
  (* finds the minimum excluded element *)
  fun findMEX hs =
  let
    fun findMex' i =
      if IntHashSet.member (hs,i) = true then findMex' (i+1)
      else i
  in
    findMex' 0
  end
in
  fun colorGraph g =
    (* optimization hack because graph coloring takes a while *)
    if numVertices g < seoThreshold then (
      let
        val coloring = CGVHashTable.mkTable (initSize, ElemNotFoundException)

        (* neighborVals : vertex -> IntHashSet.set *)
        (* given a vertex, finds the list of values its neighbors have *)
        fun neighborVals v =
          let
            val hs = IntHashSet.mkEmpty initSize
            val _ = CGVHashSet.app (fn v' =>
                      case CGVHashTable.find coloring v' of
                        SOME i => IntHashSet.add (hs, i)
                      | NONE => ()) (neighbor g v)
          in
            hs
          end

        (* colorVertex : operand -> int *)
        (* finds the 'color', represented by an int, of this register *)
        fun colorVertex (Assem.REG reg) = Assem.getColor reg
          | colorVertex v = findMEX (neighborVals v)

        val ordering = seo g
        val _ = List.app (fn v => CGVHashTable.insert coloring
                            (v, colorVertex v)) ordering;
      in
        coloring
      end
    )
    else (
      let
        val coloring = CGVHashTable.mkTable (initSize, ElemNotFoundException)
        val max = ref (List.length Assem.registers)

        (* blindlyColorVertex : operand -> int *)
        (* blindly colors the vertex by assigning it to the stack *)
        fun blindlyColorVertex (Assem.REG reg) = Assem.getColor reg
          | blindlyColorVertex _ = (max := !max + 1; !max)

        val _ = List.app (fn v => CGVHashTable.insert coloring
                            (v, blindlyColorVertex v))
                            (CGVHashSet.listItems (getVertices g))
      in
        coloring
      end
    )
end

local
  fun allocate regMap reg =
    let
      val len = List.length Assem.registers

      (* lookup : Assem.operand -> Assem.operand *)
      fun lookup reg =
        let
          val i = CGVHashTable.lookup regMap reg
        in
          Assem.remapColor i
        end
    in
      case reg of
        Assem.TEMP temp => lookup reg
      | _ => reg
    end

  (* redundant allocates for now *)
  fun mapInstr regMap instr =
    let
      val allocate' = allocate regMap
    in
      case instr of
        Assem.BINOP(oper, d, s1, s2) =>
          Assem.BINOP(oper, allocate' d, allocate' s1, allocate' s2)
      | Assem.MOV(d, s) => Assem.MOV(allocate' d, allocate' s)
      | _ => instr
    end
in
  fun remap regMap instrs =
    List.map (mapInstr regMap) instrs
end

end
