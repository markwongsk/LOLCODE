(* L2 Compiler
 * Parsing
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Glueing together the pieces produced by ML-Lex and ML-Yacc
 *)

signature PARSE =
sig
  (* parse filename = ast
   * will raise ErrorMsg.Error in case of lexing or parsing error
   *)
  val parse : string -> Ast.program
end

structure Parse :> PARSE =
struct

  structure L2LrVals = L2LrValsFn (structure Token = LrParser.Token)
  structure L2Lex = L2LexFn (structure Tokens = L2LrVals.Tokens)
  structure L2Parse = Join (structure ParserData = L2LrVals.ParserData
                            structure Lex = L2Lex
                            structure LrParser = LrParser)

  (* Main parsing function *)
  fun parse filename =
      SafeIO.withOpenIn filename (fn instream =>
        let
	  val _ = ErrorMsg.reset() (* no errors, no messages so far *)
	  val _ = ParseState.setfile filename (* start at position 0 in filename *)
	  fun parseerror (s, p1, p2) = ErrorMsg.error (ParseState.ext (p1,p2)) s
	  val lexer = LrParser.Stream.streamify
			  (L2Lex.makeLexer (fn _ => TextIO.input instream))
	  (* 0 = no error correction, 15 = reasonable lookahead for correction *)
	  val (absyn, _) = L2Parse.parse(0, lexer, parseerror, ())
          val _ = if !ErrorMsg.anyErrors
		  then raise ErrorMsg.Error
		  else ()
	in
	  absyn
	end)
      handle LrParser.ParseError => raise ErrorMsg.Error (* always preceded by msg *)
           | e as IO.Io _ => ( ErrorMsg.error NONE (exnMessage e);
                               raise ErrorMsg.Error )
           (* moved here for portability between v110.59 and later versions *)
	   (* was: L2Lex.LexError => ... *)
           | e => ( ErrorMsg.error NONE "lexer error" ;
                    raise ErrorMsg.Error )

end
