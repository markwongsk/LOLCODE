(* L2 Compiler
 * Lexer
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Lexes forward compatible fragment of C0
 *
 * Update this file to lex the necessary keywords and other tokens
 * in order to make the grammar forward compatible with C0.
 *)

structure A = Ast
structure S = Symbol

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token

local
  val commentLevel = ref 0
  val commentPos = ref 0
in
  fun enterComment yypos =
      ( commentLevel := !commentLevel + 1 ;
	commentPos := yypos )

  fun exitComment () =
      ( commentLevel := !commentLevel - 1 ;
	!commentLevel = 0 )

  fun number (yyt, yyp) =
      let
        val ext = ParseState.ext (yyp, yyp + size yyt)
	val numOpt = Word32Signed.fromString yyt
                     handle Overflow =>
			    ( ErrorMsg.error ext
                                ("integral constant `" ^ yyt ^ "' too large") ;
			      NONE )
      in
	case numOpt
	  of NONE => ( ErrorMsg.error ext
                         ("cannot parse integral constant `" ^ yyt ^ "'");
		       Tokens.INTCONST (Word32Signed.ZERO, yyp, yyp + size yyt) )
	   | SOME n => Tokens.INTCONST (n,yyp,yyp + size yyt)
      end

  fun eof () =
      ( if (!commentLevel > 0)
          then (ErrorMsg.error (ParseState.ext (!commentPos,!commentPos)) "unterminated comment")
          else ();
	Tokens.EOF (0,0) )		(* bogus position information; unused *)

end

%%
%header (functor L2LexFn(structure Tokens : L2_TOKENS));
%full
%s COMMENT COMMENT_LINE;

id = [A-Za-z_][A-Za-z0-9_]*;
hexnum = 0[xX][0-9a-fA-F]+;
decnum = 0 | ([1-9][0-9]*);

ws = [\ \t\011\012\013];

%%

<INITIAL> {ws}+       => (lex ());
<INITIAL> \n          => (ParseState.newline(yypos); lex());

<INITIAL> "{"         => (Tokens.LBRACE (yypos, yypos + size yytext));
<INITIAL> "}"         => (Tokens.RBRACE (yypos, yypos + size yytext));
<INITIAL> "("         => (Tokens.LPAREN (yypos, yypos + size yytext));
<INITIAL> ")"         => (Tokens.RPAREN (yypos, yypos + size yytext));

<INITIAL> ";"         => (Tokens.SEMI (yypos, yypos + size yytext));

<INITIAL> "++"        => (Tokens.POSTINC (yypos, yypos + size yytext));
<INITIAL> "--"        => (Tokens.POSTDEC (yypos, yypos + size yytext));

<INITIAL> "="         => (Tokens.ASSIGN (yypos, yypos + size yytext));
<INITIAL> "+="        => (Tokens.PLUSEQ (yypos, yypos + size yytext));
<INITIAL> "-="        => (Tokens.MINUSEQ (yypos, yypos + size yytext));
<INITIAL> "*="        => (Tokens.STAREQ (yypos, yypos + size yytext));
<INITIAL> "/="        => (Tokens.SLASHEQ (yypos, yypos + size yytext));
<INITIAL> "%="        => (Tokens.PERCENTEQ (yypos, yypos + size yytext));
<INITIAL> "&="        => (Tokens.ANDEQ (yypos, yypos + size yytext));
<INITIAL> "^="        => (Tokens.XOREQ (yypos, yypos + size yytext));
<INITIAL> "|="        => (Tokens.OREQ (yypos, yypos + size yytext));
<INITIAL> "<<="       => (Tokens.LSHIFTEQ (yypos, yypos + size yytext));
<INITIAL> ">>="       => (Tokens.RSHIFTEQ (yypos, yypos + size yytext));

<INITIAL> "&&"        => (Tokens.AND (yypos, yypos + size yytext));
<INITIAL> "||"        => (Tokens.OR (yypos, yypos + size yytext));
<INITIAL> "<<"        => (Tokens.LSHIFT (yypos, yypos + size yytext));
<INITIAL> ">>"        => (Tokens.RSHIFT (yypos, yypos + size yytext));
<INITIAL> "<="        => (Tokens.LEQ (yypos, yypos + size yytext));
<INITIAL> ">="        => (Tokens.GEQ (yypos, yypos + size yytext));
<INITIAL> "=="        => (Tokens.EQ (yypos, yypos + size yytext));
<INITIAL> "!="        => (Tokens.NEQ (yypos, yypos + size yytext));

<INITIAL> "!"         => (Tokens.NOT (yypos, yypos + size yytext));
<INITIAL> "~"         => (Tokens.BNOT (yypos, yypos + size yytext));

<INITIAL> "+"         => (Tokens.PLUS (yypos, yypos + size yytext));
<INITIAL> "-"         => (Tokens.MINUS (yypos, yypos + size yytext));
<INITIAL> "*"         => (Tokens.STAR (yypos, yypos + size yytext));
<INITIAL> "/"         => (Tokens.SLASH (yypos, yypos + size yytext));
<INITIAL> "%"         => (Tokens.PERCENT (yypos, yypos + size yytext));
<INITIAL> "<"         => (Tokens.LT (yypos, yypos + size yytext));
<INITIAL> ">"         => (Tokens.GT (yypos, yypos + size yytext));
<INITIAL> "&"         => (Tokens.BAND (yypos, yypos + size yytext));
<INITIAL> "^"         => (Tokens.BXOR (yypos, yypos + size yytext));
<INITIAL> "|"         => (Tokens.BOR (yypos, yypos + size yytext));

<INITIAL> "?"         => (Tokens.TERN (yypos, yypos + size yytext));
<INITIAL> ":"         => (Tokens.COLON (yypos, yypos + size yytext));

<INITIAL> "return"    => (Tokens.RETURN (yypos, yypos + size yytext));
<INITIAL> "int"       => (Tokens.INT (yypos, yypos + size yytext));
<INITIAL> "struct"    => (Tokens.STRUCT (yypos, yypos + size yytext));
<INITIAL> "typedef"   => (Tokens.TYPEDEF (yypos, yypos + size yytext));
<INITIAL> "if"        => (Tokens.IF (yypos, yypos + size yytext));
<INITIAL> "else"      => (Tokens.ELSE (yypos, yypos + size yytext));
<INITIAL> "while"     => (Tokens.WHILE (yypos, yypos + size yytext));
<INITIAL> "for"       => (Tokens.FOR (yypos, yypos + size yytext));
<INITIAL> "continue"  => (Tokens.CONTINUE (yypos, yypos + size yytext));
<INITIAL> "break"     => (Tokens.BREAK (yypos, yypos + size yytext));
<INITIAL> "assert"    => (Tokens.ASSERT (yypos, yypos + size yytext));
<INITIAL> "true"      => (Tokens.TRUE (yypos, yypos + size yytext));
<INITIAL> "false"     => (Tokens.FALSE (yypos, yypos + size yytext));
<INITIAL> "NULL"      => (Tokens.NULL (yypos, yypos + size yytext));
<INITIAL> "alloc"     => (Tokens.ALLOC (yypos, yypos + size yytext));
<INITIAL> "alloc_array"  => (Tokens.ALLOC_ARRAY (yypos, yypos + size yytext));
<INITIAL> "bool"      => (Tokens.BOOL (yypos, yypos + size yytext));
<INITIAL> "void"      => (Tokens.VOID (yypos, yypos + size yytext));
<INITIAL> "char"      => (Tokens.CHAR (yypos, yypos + size yytext));
<INITIAL> "string"    => (Tokens.STRING (yypos, yypos + size yytext));

<INITIAL> {hexnum}    => (number (yytext, yypos));
<INITIAL> {decnum}    => (number (yytext, yypos));

<INITIAL> {id}        => (let
                            val id = Symbol.symbol yytext
                          in
                            Tokens.IDENT (id, yypos, yypos + size yytext)
                          end);

<INITIAL> "/*"        => (YYBEGIN COMMENT; enterComment yypos; lex());
<INITIAL> "*/"        => (ErrorMsg.error (ParseState.ext (yypos, yypos)) "unbalanced comments";
                          lex());

<INITIAL> "//"        => (YYBEGIN COMMENT_LINE; lex());
<INITIAL> .           => (ErrorMsg.error (ParseState.ext (yypos,yypos))
                              ("illegal character: \"" ^ yytext ^ "\"");
                          lex ());

<COMMENT> "/*"        => (enterComment yypos; lex());
<COMMENT> "*/"        => (if exitComment () then YYBEGIN INITIAL else (); lex());
<COMMENT> \n          => (ParseState.newline yypos; lex ());
<COMMENT> .           => (lex());

<COMMENT_LINE> \n     => (ParseState.newline yypos; YYBEGIN INITIAL; lex());
<COMMENT_LINE> .      => (lex());
