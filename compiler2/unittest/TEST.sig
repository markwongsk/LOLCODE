(* signature for all tests *)
signature TEST =
sig
  (* returns true if and only if the unit tests passed all tests *)
  val all : unit -> bool
end
