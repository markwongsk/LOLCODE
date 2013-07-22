(* L1 Compiler
 * Simple graph implementation of a graph
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

  (* checks if the graph contains this vertex *)
  val containsVertex : graph -> vertex -> bool

  (* checks if the graph contains this edge *)
  val containsEdge : graph -> edge -> bool

  (* returns the number of vertices in the graph *)
  val numVertices : graph -> int

  (* returns the vertices of a graph *)
  val getVertices : graph -> vertex list

  (* returns neighbors of a vertex *)
  val neighbor : graph -> vertex -> vertex list

  (* returns a string representation of a graph *)
  val toString : graph -> string
end
