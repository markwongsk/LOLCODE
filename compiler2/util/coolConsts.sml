(* L2 Compiler
 * Cool constants
 * Author: Andrew Audibert <aaudiber@andrew.cmu.edu>
 * Author: Mark Wong Siang Kai <msiangka@andrew.cmu.edu>
 *)

structure Consts =
struct
  val one = Tree.CONST(Ast.IntConst(Word32.fromInt 1))
  val zero = Tree.CONST(Ast.IntConst(Word32.fromInt 0))
  (* magic number beyond which we will not use the seo algorithm
   * and will just put everything on the stack
   *)
  val seoThreshold = 80
end
