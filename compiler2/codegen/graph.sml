(* L1 Compiler
 * Chordal graph implementation of a graph with weighted vertices
 * Author: Mark Wong Siang Kai <msiangka@andrew.cmu.edu>
 *)

signature GRAPH =
sig
  type set
  type vertex
  type edge = (vertex * vertex)
  type graph

  (* constructs an empty graph *)
  val emptyGraph : unit -> graph

  (* constructs a new graph given a vertex list and an edge list *)
  val newGraph : vertex list -> edge list -> graph

  (* adds a vertex to the given graph *)
  val addVertex : graph -> vertex -> unit

  (* adds an edge to the given graph *)
  val addEdge : graph -> edge -> unit

  (* adds a list of vertices to the given graph *)
  val addVertices : graph -> vertex list -> unit

  (* adds a list of edges to the given graph *)
  val addEdges : graph -> edge list -> unit

  (* updates the weight of the given vertex *)
  val getWeight : graph -> vertex -> int

  (* increases the weight of the edge by one *)
  val incrementWeight : graph -> vertex -> unit

  (* updates the weight of the given vertex *)
  val updateWeight : graph -> vertex -> int -> unit

  (* checks if the graph contains this vertex *)
  val containsVertex : graph -> vertex -> bool

  (* checks if the graph contains this edge *)
  val containsEdge : graph -> edge -> bool

  (* returns the number of vertices in the graph *)
  val numVertices : graph -> int

  (* returns the vertices of a graph *)
  val getVertices : graph -> set

  (* returns neighbors of a vertex *)
  val neighbor : graph -> vertex -> set

  (* returns a string representation of a graph *)
  val toString : graph -> string
end

structure CGV =
struct
  structure Assem = Assem

  type vertex = Assem.operand

  fun vertexToString vertex =
    "(" ^ (Assem.formatOperand vertex) ^ ")"
end

structure CGVKey : HASH_KEY =
struct
  structure Assem = CGV.Assem
  type hash_key = CGV.vertex

  (* the vertices are unique by name *)
  fun hashVal operand =
    HashString.hashString (Assem.formatOperand operand)

  fun sameKey (key1, key2) =
    Assem.equals key1 key2
end

structure CGVHashSet = HashSetFn(CGVKey)
structure CGVHashTable = HashTableFn(CGVKey)

structure ChordalGraph : GRAPH
where type vertex = CGVHashSet.Key.hash_key =
struct
  exception ElemNotFoundException
  exception IllegalArgumentException

  open PrintUtil
  open CGV

  type set = CGVHashSet.set
  type 'b hashTable = 'b CGVHashTable.hash_table
  type vertex = CGVHashSet.Key.hash_key
  type edge = (vertex * vertex)

  datatype graph =
    GRAPH of {n : int ref,
              vertices : set,
              weightTable : int hashTable,
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

  fun emptyGraph () =
    GRAPH ({
      n = ref 0,
      vertices = CGVHashSet.mkEmpty (initSize),
      weightTable = CGVHashTable.mkTable (initSize, ElemNotFoundException),
      adjTable = CGVHashTable.mkTable (initSize, ElemNotFoundException)
    })

  (* invariant: initializes the adjacency table's set for the added vertex.
   *            Also initializes the weight of the vertex to zero
   *)
  fun addVertex (GRAPH {n,vertices,weightTable,adjTable}) v =
    if CGVHashSet.member (vertices, v) then ()
    else (
      n := !n + 1;
      CGVHashSet.add (vertices, v);
      CGVHashTable.insert weightTable (v, 0);
      CGVHashTable.insert adjTable (v, CGVHashSet.mkEmpty initSize)
    )

  fun addEdge (GRAPH (g as {adjTable,...})) (e as (v1,v2)) = (
    CGVHashSet.add ((CGVHashTable.lookup adjTable  v1), v2);
    CGVHashSet.add ((CGVHashTable.lookup adjTable  v2), v1)
  )

  fun addVertices g vlist =
    List.app (addVertex g) vlist

  fun addEdges g elist =
    List.app (addEdge g) elist

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
  fun getVertices (GRAPH {vertices,...}) = vertices
  fun getWeight (GRAPH {weightTable,...}) v =
    CGVHashTable.lookup weightTable v
  fun updateWeight (GRAPH {weightTable,...}) v newWeight =
    CGVHashTable.insert weightTable (v, newWeight)
  fun incrementWeight (GRAPH {weightTable,...}) v =
    CGVHashTable.insert weightTable (v, (CGVHashTable.lookup weightTable v) + 1)

  fun neighbor (GRAPH (g as {adjTable,...})) v =
    CGVHashTable.lookup adjTable v

  fun toString (g as (GRAPH {n, vertices, weightTable, adjTable})) =
    let
      fun mkVertexInfoString v =
        vertexToString v ^ ": \tweight = " ^ (Int.toString (getWeight g v)) ^
        ": \t" ^ (listToString vertexToString (CGVHashSet.listItems (neighbor g v)))
      val vertexInfoStrings =
        List.map mkVertexInfoString (CGVHashSet.listItems (getVertices g))
    in
      "Graph of " ^ (Int.toString (!n)) ^ " vertices:" ^
      (List.foldl (fn (a, b) => b ^ "\n" ^ a) "" vertexInfoStrings)
    end

end
