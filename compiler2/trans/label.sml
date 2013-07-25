(* L2 Compiler
 * Labels
 * Author: Andrew Audibert <aaudiber@andrew.cmu.edu>
 * Author: Mark Wong Siang Kai <msiangka@andrew.cmu.edu>
 *)

signature LABEL =
sig
  type label = string * int

  val new : unit -> int	(* returns a unique new label *)
end

structure Label :> LABEL =
struct
  type label = string * int

  local
    val counter = ref 1
  in
    fun new () = (!counter before ( counter := !counter + 1 ))
  end

end
