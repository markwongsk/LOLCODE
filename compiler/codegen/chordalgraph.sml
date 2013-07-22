(* L1 Compiler
 * Chordal graph implementation of a graph with weighted vertices
 * Author: Mark Wong Siang Kai <msiangka@andrew.cmu.edu>
 *)

signature CHORDALGRAPH =
sig
  include GRAPH

  (* finds a simplicial elimination ordering for the given graph *)
  val seo : graph -> vertex list
end

structure CGV =
struct
  structure Assem = Assem

  type vertex = (Assem.operand * int ref)

  fun vertexToString (operand, weight) =
    "(" ^ (Assem.formatOperand operand) ^ "=" ^ (Int.toString (!weight)) ^ ")"
end

structure CGVKey : HASH_KEY =
struct
  structure Assem = CGV.Assem
  type hash_key = CGV.vertex

  (* the vertices are unique by name *)
  fun hashVal (operand, weightp) =
    HashString.hashString (Assem.formatOperand operand)

  fun sameKey (key1 : hash_key, key2 : hash_key) =
    Assem.equals (#1(key1)) (#1(key2))
end

structure CGVHashSet = HashSetFn(CGVKey)
structure CGVHashTable = HashTableFn(CGVKey)

(* Chordal graph *)
structure ChordalGraph : CHORDALGRAPH
where type vertex = CGVHashSet.Key.hash_key =
struct
  exception ElemNotFoundException
  exception IllegalArgumentException of string
  exception NYI

  open PrintUtil
  open CGV

  type set = CGVHashSet.set
  type 'b hashTable = 'b CGVHashTable.hash_table
  type vertex = CGVHashSet.Key.hash_key
  type edge = (vertex * vertex)

  datatype graph =
    GRAPH of {n : int ref,
              vertices : set,
              adjTable : set hashTable}

  fun containsVertex (GRAPH {vertices,...}) v =
    CGVHashSet.member (vertices, v)

  fun containsEdge (GRAPH (g as {adjTable,...})) (e as (v1,v2)) =
    containsVertex (GRAPH g) v1 andalso containsVertex (GRAPH g) v2 andalso
    CGVHashSet.member ((CGVHashTable.lookup (adjTable) v1), v2)

  val initSize = 10

  (* edgeToString : edge -> string *)
  (* converts an edge to its string representation *)
  fun edgeToString (e as (v1,v2)) =
    "(" ^ (vertexToString v1) ^ "," ^ (vertexToString v2) ^ ")"

  (* checkEndpoints : graph -> edge -> unit *)
  (* checks if an edge contains a vertex not in the graph. Raises
     IllegalArgumentException with a message containing the violating
     edge *)
  fun checkEndpoints (GRAPH g) (e as (v1,v2)) =
    if containsVertex (GRAPH g) v1 andalso containsVertex (GRAPH g) v2 then ()
    else raise IllegalArgumentException (edgeToString e)

  fun emptyGraph () =
    GRAPH ({
      n = ref 0,
      vertices = CGVHashSet.mkEmpty (initSize),
      adjTable = CGVHashTable.mkTable (initSize, ElemNotFoundException)
    })

  (* invariant: initializes the adjacency table's set for the added vertex *)
  fun addVertex (GRAPH {n,vertices,adjTable}) v =
    if CGVHashSet.member (vertices, v) then ()
    else (
      n := !n + 1;
      CGVHashSet.add (vertices, v);
      CGVHashTable.insert adjTable (v, CGVHashSet.mkEmpty initSize)
    )

  (* raises IllegalArgumentException if e contains a vertex not in vlist *)
  fun addEdge (GRAPH (g as {adjTable,...})) (e as (v1,v2)) = (
    checkEndpoints (GRAPH g) e;
    CGVHashSet.add ((CGVHashTable.lookup adjTable  v1), v2);
    CGVHashSet.add ((CGVHashTable.lookup adjTable  v2), v1)
  )

  fun addVertices g vlist =
    let val _ = List.map (addVertex g) vlist in () end

  fun addEdges g elist =
    let val _ = List.map (addEdge g) elist in () end

  (* raises IllegalArgumentException if elist contains an edge that contains
     a vertex not in vlist *)
  fun newGraph vlist elist =
    let
      val g = emptyGraph ()
      val _ = addVertices g vlist
      val _ = addEdges g elist
    in
      g
    end

  fun numVertices (GRAPH {n,...}) = !n
  fun getVertices (GRAPH {vertices,...}) = CGVHashSet.listItems(vertices)

  (* raises IllegalArgumentException if v is not in the vertex set *)
  fun neighbor (GRAPH (g as {adjTable,...})) v = (
    if containsVertex (GRAPH g) v then
      CGVHashSet.listItems(CGVHashTable.lookup adjTable v)
    else (print ("Illegal Argument: " ^ vertexToString v); raise
      IllegalArgumentException (vertexToString v))
  )

  fun toString (g as (GRAPH {n, vertices, adjTable})) =
    let
      fun mkAdjString v =
        vertexToString v ^ "\t: \t" ^ (listToString vertexToString (neighbor g v))
      val adjStrings = List.map mkAdjString (getVertices g)
    in
      "Graph of " ^ (Int.toString (!n)) ^ " vertices:" ^
      (List.foldl (fn (a, b) => b ^ "\n" ^ a) "" adjStrings)
    end

  fun seo g =
    let
      (* represents one iteration of the max cardinality search algorithm *)
      fun getNext g k n =
        if numVertices g = 0 then []
        else
        let
          (* compares two vertices and returns the "larger" one *)
          fun compareVertex (v1 : vertex, v2 : vertex) =
            if (!(#2 v1)) > (!(#2 v2)) then v1 else v2

          (* increments the vertex's weight *)
          fun incrementVertexWeight (v : vertex) =
            (#2 v) := !(#2 v) + 1

          (* find the maximum vertex *)
          val x::xs = getVertices g
          val maxVertex = List.foldl compareVertex x xs

          (* update the neighbors' weights *)
          val _ = List.map incrementVertexWeight
                  (neighbor g maxVertex)

          (* "remove" the vertex *)
          val _ = (#2 maxVertex) := ~n
        in
          if k = n then []
          else maxVertex :: (getNext g (k+1) n)
        end
    in
      getNext g 0 (numVertices g)
    end
end
