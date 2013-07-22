(* L1 Compiler
 * Temporaries
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *)

signature TEMP =
sig
  type temp

  val reset : unit -> unit	(* resets temp numbering *)
  val new : unit -> temp	(* returns a unique new temp *)
  val name : temp -> string	(* returns the name of a temp *)
  val compare : temp * temp -> order (* comparison function *)
  val numTemps : unit -> int (* returns the number of temps created *)
  val allTemps : unit -> temp list (* returns all created *)
end

structure Temp :> TEMP =
struct
  type temp = int

  local
    val counter = ref 1
    val tempList = ref []
  in
    (* warning: calling reset() may jeopardize uniqueness of temps! *)
    fun reset () = ( counter := 1 )
    fun new () = (!counter before ( tempList := !counter :: !tempList;
                                    counter := !counter + 1 ))
    fun numTemps () = length (!tempList)
    fun allTemps () = !tempList
  end

  fun name t = "%t[" ^ Int.toString t ^ "]"

  fun compare (t1,t2) = Int.compare (t1,t2)
end
