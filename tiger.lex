type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1 p2

val openComments = ref 0

val openString = ref false
val curStringStartPosition = ref 0
val curString = ref ""

fun eof() = 
    let val pos = hd(!linePos) 
    in
        if !openString = true then err (pos, "String was not closed") else ();
        if !openComments > 0 then err(pos, "Too many open comments") else ();
        if !openComments < 0 then err(pos, "Too many closed comments") else ();
        Tokens.EOF(pos,pos)
    end
fun isValidAscii (x) = 
    case Int.fromString(x) of
    SOME num => if num<=255 then true else false
    | NONE  => false

fun parseEscapeSequence(yytext) = 
    Char.toString (valOf(Char.fromString yytext))
    
%% 
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%s STRING COMMENT;
%%

<INITIAL> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> \012	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue()); 
<INITIAL> \013	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> \t => (linePos := yypos :: !linePos; continue());
<INITIAL> " " => (linePos := yypos :: !linePos; continue());
<INITIAL>"type" => (linePos := yypos :: !linePos; Tokens.TYPE(yypos, yypos+4));
<INITIAL>"var" => (linePos := yypos :: !linePos; Tokens.VAR(yypos,yypos+3));
<INITIAL> "function" => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL> "break" => (Tokens.BREAK(yypos,yypos+5));
<INITIAL> "of" => (Tokens.OF(yypos,yypos+2));
<INITIAL> "end" => (Tokens.END(yypos,yypos+3));
<INITIAL> "in" => (Tokens.IN(yypos,yypos+2));
<INITIAL> "nil" =>  (Tokens.NIL(yypos,yypos+3));
<INITIAL> "let" =>  (Tokens.LET(yypos,yypos+3));
<INITIAL> "do" =>  (Tokens.DO(yypos,yypos+3)) ;
<INITIAL> "of" => (Tokens.OF(yypos,yypos+2));
<INITIAL> "end" => (Tokens.END(yypos,yypos+3));
<INITIAL> "in" => (Tokens.IN(yypos,yypos+2));
<INITIAL> "nil" =>  (Tokens.NIL(yypos,yypos+3));
<INITIAL> "let" =>  (Tokens.LET(yypos,yypos+3));
<INITIAL> "do" =>  (Tokens.DO(yypos,yypos+3)) ;
<INITIAL> "to" => (Tokens.TO(yypos,yypos+2));
<INITIAL> "for" => (Tokens.FOR(yypos,yypos+3));
<INITIAL> "while" => (Tokens.WHILE(yypos,yypos+5));
<INITIAL> "else" => (Tokens.ELSE(yypos,yypos+4));
<INITIAL> "then" => (Tokens.THEN(yypos,yypos+4));
<INITIAL> "if" => (Tokens.IF(yypos,yypos+2));
<INITIAL> "array" => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL> ":=" => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL> "|" => (Tokens.OR(yypos,yypos+1));
<INITIAL> "&" => (Tokens.AND(yypos,yypos+1));
<INITIAL> ">=" => (Tokens.GE(yypos,yypos+2));
<INITIAL> ">" => (Tokens.GT(yypos,yypos+1));
<INITIAL> "<=" => (Tokens.LE(yypos,yypos+2));
<INITIAL> "<" => (Tokens.LT(yypos,yypos+1));
<INITIAL> "<>" => (Tokens.NEQ(yypos,yypos+2));
<INITIAL> "=" => (Tokens.EQ(yypos,yypos+1));
<INITIAL> "/" => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL> "*" => (Tokens.TIMES(yypos,yypos+1));
<INITIAL> "-" => (Tokens.MINUS(yypos,yypos+1));
<INITIAL> "+" => (Tokens.PLUS(yypos,yypos+1));
<INITIAL> "." => (Tokens.DOT(yypos,yypos+1));
<INITIAL> "}" => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL> "{" => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL> "]" => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL> "[" => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL> ")" => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL> "(" => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> ":" => (Tokens.COLON(yypos,yypos+1));
<INITIAL> "," => (Tokens.COMMA(yypos,yypos+1));
<INITIAL> [0-9][0-9]* => (Tokens.INT(Option.valOf(Int.fromString(yytext)), yypos, yypos + size(yytext)));
<INITIAL> ([a-z]|[A-Z])([a-z]|[A-Z]|[0-9]|_)* => (Tokens.ID(yytext,yypos,yypos + size(yytext)));
<INITIAL> "/*" => (YYBEGIN COMMENT; openComments:= !openComments+1; continue());
<INITIAL> \" => (YYBEGIN STRING; openString:= true; curStringStartPosition:= yypos; curString:= ""; continue()); 
<STRING> \\n => ( curString:= !curString^"\n";continue());
<STRING> \\t => ( curString:= !curString^"\t";continue());
<STRING> \\[0-9][0-9][0-9] => (if isValidAscii(String.extract(yytext,1,NONE)) then curString := !curString ^ Char.toString(Char.chr(Option.valOf(Int.fromString(String.extract(yytext,1,NONE))))) else err(yypos, "Illegal Ascii code in Escape Sequence"); continue());
<STRING> \\\\ => (curString:= !curString ^ "\\"; continue());
<STRING> \\\" => (curString:= !curString ^ "\""; continue());
<STRING> "\^"[@A-Z[\]^_] => (curString := !curString ^ parseEscapeSequence(yytext); continue());
<STRING> \\[ \t\n\012\013]+\\ => (continue()); 
<STRING> \" => (YYBEGIN INITIAL; openString:= false; Tokens.STRING(!curString,!curStringStartPosition,yypos+1));
<STRING> \n|\013 => (err(yypos, "Illegal New Line in String"); continue());
<STRING> \\ => (err(yypos, "Illegal Escape Sequence in String"); continue());
<STRING> . => (curString:= !curString^yytext;continue());
<COMMENT> "/*" => (openComments:= !openComments+1; continue());
<COMMENT> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT> \012	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue()); 
<COMMENT> \013	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT> "*/" => (openComments:= !openComments-1; if !openComments = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT> . => (continue());
<INITIAL> . => (err(yypos, "Illegal character"); continue());
