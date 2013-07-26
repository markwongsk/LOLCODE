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

<INITIAL> "HAI"       => (Tokens.HAI (yypos, yypos + size yytext));
<INITIAL> "KTHXBYE"   => (Tokens.KTHXBYE (yypos, yypos + size yytext));

<INITIAL> "HOW"       => (Tokens.HOW (yypos, yypos + size yytext));
<INITIAL> "DUZ"       => (Tokens.DUZ (yypos, yypos + size yytext));
<INITIAL> "I"         => (Tokens.I (yypos, yypos + size yytext));
<INITIAL> "MAIN"      => (Tokens.MAIN (yypos, yypos + size yytext));
<INITIAL> "IF"        => (Tokens.IF (yypos, yypos + size yytext));
<INITIAL> "U"         => (Tokens.U (yypos, yypos + size yytext));
<INITIAL> "SAY"       => (Tokens.SAY (yypos, yypos + size yytext));
<INITIAL> "SO"        => (Tokens.SO (yypos, yypos + size yytext));

<INITIAL> "IHAZA"     => (Tokens.IHAZA (yypos, yypos + size yytext));
<INITIAL> "ITZ"       => (Tokens.ITZ (yypos, yypos + size yytext));
<INITIAL> "R"         => (Tokens.R (yypos, yypos + size yytext));

<INITIAL> "O"         => (Tokens.O (yypos, yypos + size yytext));
<INITIAL> "RLY?"      => (Tokens.RLYY (yypos, yypos + size yytext));
<INITIAL> "YA"        => (Tokens.YA (yypos, yypos + size yytext));
<INITIAL> "RLY"       => (Tokens.RLY (yypos, yypos + size yytext));
<INITIAL> "NO"        => (Tokens.NO (yypos, yypos + size yytext));
<INITIAL> "WAI"       => (Tokens.WAI (yypos, yypos + size yytext));
<INITIAL> "OIC"       => (Tokens.OIC (yypos, yypos + size yytext));
<INITIAL> "SAEM"      => (Tokens.SAEM (yypos, yypos + size yytext));
<INITIAL> "DIFFRINT"  => (Tokens.DIFFRINT (yypos, yypos + size yytext));
<INITIAL> "IM"        => (Tokens.IM (yypos, yypos + size yytext));
<INITIAL> "IN"        => (Tokens.IN (yypos, yypos + size yytext));
<INITIAL> "YR"        => (Tokens.YR (yypos, yypos + size yytext));
<INITIAL> "OUTTA"     => (Tokens.OUTTA (yypos, yypos + size yytext));
<INITIAL> "TIL"       => (Tokens.TIL (yypos, yypos + size yytext));
<INITIAL> "WILE"      => (Tokens.WILE (yypos, yypos + size yytext));
<INITIAL> "GTFO"      => (Tokens.GTFO (yypos, yypos + size yytext));
<INITIAL> "UPPIN"     => (Tokens.UPPIN (yypos, yypos + size yytext));
<INITIAL> "NERFIN"    => (Tokens.NERFIN (yypos, yypos + size yytext));

<INITIAL> "SUMOF"     => (Tokens.SUMOF (yypos, yypos + size yytext));
<INITIAL> "DIFFOF"    => (Tokens.DIFFOF (yypos, yypos + size yytext));
<INITIAL> "PRODUKTOF" => (Tokens.PRODUKTOF (yypos, yypos + size yytext));
<INITIAL> "QUOSHUNTOF"=> (Tokens.QUOSHUNTOF (yypos, yypos + size yytext));
<INITIAL> "MODOF"     => (Tokens.MODOF (yypos, yypos + size yytext));
<INITIAL> "EITHER"    => (Tokens.EITHER (yypos, yypos + size yytext));
<INITIAL> "BOTH"      => (Tokens.BOTH (yypos, yypos + size yytext));
<INITIAL> "NOT"       => (Tokens.NOT (yypos, yypos + size yytext));
<INITIAL> "NEG"       => (Tokens.NEG (yypos, yypos + size yytext));
<INITIAL> "BIGGR"     => (Tokens.BIGGR (yypos, yypos + size yytext));
<INITIAL> "SMALLR"    => (Tokens.SMALLR (yypos, yypos + size yytext));
<INITIAL> "AN"        => (Tokens.AN (yypos, yypos + size yytext));

<INITIAL> "FOUNDYR"    => (Tokens.FOUNDYR (yypos, yypos + size yytext));

<INITIAL> {hexnum}    => (number (yytext, yypos));
<INITIAL> {decnum}    => (number (yytext, yypos));

<INITIAL> {id}        => (let
                            val id = Symbol.symbol yytext
                          in
                            Tokens.IDENT (id, yypos, yypos + size yytext)
                          end);
<INITIAL> "WIN"       => (Tokens.WIN (yypos, yypos + size yytext));
<INITIAL> "FAIL"      => (Tokens.FAIL (yypos, yypos + size yytext));

<INITIAL> .           => (ErrorMsg.error (ParseState.ext (yypos,yypos))
                              ("illegal character: \"" ^ yytext ^ "\"");
                          lex ());
