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
%header (functor L1LexFn(structure Tokens : L1_TOKENS));
%full
%s COMMENT COMMENT_LINE;

id = [A-Za-z_][A-Za-z0-9_]*;
hexnum = 0[xX][0-9a-fA-F]+;
decnum = 0 | ([1-9][0-9]*);

ws = [\ \t\011\012\013];

%%

<INITIAL> {ws}+       => (lex ());
<INITIAL> \n          => (ParseState.newline(yypos); lex());

<INITIAL> "HAI"       => (Tokens.HAI (yypos, yypos + size yytext));
<INITIAL> "KTHXBYE"   => (Tokens.KTHXBYE (yypos, yypos + size yytext));
<INITIAL> "ITZ"       => (Tokens.ITZ (yypos, yypos + size yytext));
<INITIAL> "R"         => (Tokens.R (yypos, yypos + size yytext));

<INITIAL> "IHAZA"     => (Tokens.IHAZA (yypos, yypos + size yytext));

<INITIAL> "DIFFOF"    => (Tokens.DIFFOF (yypos, yypos + size yytext));
<INITIAL> "PRODUKTOF" => (Tokens.PRODUKTOF (yypos, yypos + size yytext));
<INITIAL> "QUOSHUNTOF"=> (Tokens.QUOSHUNTOF (yypos, yypos + size yytext));
<INITIAL> "MODOF"     => (Tokens.MODOF (yypos, yypos + size yytext));

<INITIAL> "FOUNDYR"    => (Tokens.FOUNDYR (yypos, yypos + size yytext));

<INITIAL> {hexnum}    => (number (yytext, yypos));
<INITIAL> {decnum}    => (number (yytext, yypos));

<INITIAL> {id}        => (let
                            val id = Symbol.symbol yytext
                          in
                            Tokens.IDENT (id, yypos, yypos + size yytext)
                          end);

<INITIAL> .           => (ErrorMsg.error (ParseState.ext (yypos,yypos))
                              ("illegal character: \"" ^ yytext ^ "\"");
                          lex ());
