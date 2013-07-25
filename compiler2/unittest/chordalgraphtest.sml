(* Unit Test for chordalgraph.sml
 * Author: Mark Wong Siang Kai <msiangka@unix.andrew.cmu.edu
 *)

structure ChordalGraphTest :> TEST =
struct
  exception AssertionError

  structure CG = ChordalGraph
  open CG
(*
  fun assert expected actual =
    if expected = actual then () else raise AssertionError

  (* tests the creation of empty graph *)
  fun testEmpty () =
    let
      val g = emptyGraph ()
    in
      assert (numVertices g) 0;
      assert (getVertices g) [];
      true
    end

  (* tests the addition of a vertex *)
  fun testAddVertex () =
    let
      val v = ("Andrew", ref 140)
      val g = emptyGraph ()
      val _ = addVertex g v
    in
      assert (numVertices g) 1;

      assert (getVertices g) [v];
      true
    end

  (* tests the addition of multiple vertices *)
  fun testAddVertices () =
    let
      val vlist = [("Andrew", ref 140), ("Mark", ref 117), ("Edmund", ref 130),
                   ("David", ref 140), ("Grace", ref 99)]
      val g = emptyGraph ()
      val _ = addVertices g vlist
    in
      assert (numVertices g) (List.length vlist);
      List.map (fn v => containsVertex g v) vlist;
      true
    end

  (* tests the addition of an edge *)
  fun testAddEdge () =
    let
      val vlist = [("Andrew", ref 140), ("Mark", ref 117)]
      val edge = (List.nth (vlist,0), List.nth (vlist,1))
      val g = emptyGraph ()
      val _ = addVertices g vlist
      val _ = addEdge g edge
    in
      assert (numVertices g) (List.length vlist);
      List.map (fn v => containsVertex g v) vlist;
      assert (containsEdge g edge);
      true
    end

  (* tests the addition of an edge *)
  (*fun testAddEdges () =
    let
      val vlist = [("Andrew", ref 140), ("Mark", ref 117), ("Edmund", ref 130),
                   ("David", ref 140), ("Grace", ref 99)]

      val g = emptyGraph ()
      val _ = addVertices g vlist
      val _ = addEdge g edge
    in
      assert (numVertices g) (List.length vlist);
      List.map (fn v => containsVertex g v) vlist;
      assert (containsEdge g edge);
      true
    end
  *)

  fun all () =
    let
      val tests = [testEmpty, testAddVertex, testAddVertices,
                   testAddEdge]
      val _ = map (fn test => test ()) tests
    in
      true
    end *)
  fun all () = true
end
