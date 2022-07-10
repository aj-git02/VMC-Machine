structure Tokens= Tokens
(*
datatype aux= INTEGER | BOOLEAN;
datatype AST=empty | PROG of string*AST | INT of string | BOOL of string | LT of AST*AST | LEQ of AST*AST | GT of AST*AST | GEQ of AST*AST | EQ of AST*AST | NEG of AST | NEQ of AST*AST | ITE of AST*AST*AST | WH of AST*AST | SEQ of AST*AST | AND of AST*AST | NOT of AST | OR of AST*AST | TT | FF | PLUS of AST*AST | MINUS of AST*AST | TIMES of AST*AST | DIV of AST*AST | MOD of AST*AST | SET of string*AST | DEC of AST*AST | BLK of AST*AST | stringnode of string| intnode of int | RD of string | WR of AST;
*)

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token


val linenumber = ref 1  
val colnumber = ref 2
val eof = fn () => Tokens.EOF(!linenumber, !colnumber)
val error = fn (c) => TextIO.output(TextIO.stdOut,"error" ^ c ^ "\n")

%%
%header (functor ASTLexFun(structure Tokens:AST_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
numeral = [0-9][0-9]*;
alphanum = [A-Za-z][A-Za-z0-9]*;
%%
[\n|\r\n]       => (colnumber := 1 ; linenumber := (!linenumber) + 1; lex());
";"      => ( colnumber := (!colnumber) + 1 ; Tokens.SEMICOLON(!linenumber,!colnumber));
","      => ( colnumber := (!colnumber) + 1 ; Tokens.COMMA(!linenumber,!colnumber));
":"      => ( colnumber := (!colnumber) + 1 ; Tokens.COLON(!linenumber,!colnumber));
"::"      => ( colnumber := (!colnumber) + 2 ; Tokens.DCOLON(!linenumber,!colnumber));
{ws}+    => (colnumber := (!colnumber) + size yytext ;lex());
"("      => (colnumber := (!colnumber) + 1 ;Tokens.LPAREN(!linenumber,!colnumber));
")"      => (colnumber := (!colnumber) + 1 ;Tokens.RPAREN(!linenumber,!colnumber));
"{"     => (colnumber := (!colnumber) + 1 ;Tokens.LBRACE(!linenumber,!colnumber));
"}"     => (colnumber := (!colnumber) + 1 ;Tokens.RBRACE(!linenumber,!colnumber));
"~"    => (colnumber := (!colnumber) + 1 ;Tokens.NEG(!linenumber,!colnumber));
"<"    =>(colnumber := (!colnumber) + 1 ;Tokens.LT(!linenumber,!colnumber));
"<="    =>(colnumber := (!colnumber) + 2 ;Tokens.LEQ(!linenumber,!colnumber));
"="     => (colnumber := (!colnumber) + 1 ;Tokens.EQ(!linenumber,!colnumber)); 
">"     => (colnumber := (!colnumber) + 1 ;Tokens.GT(!linenumber,!colnumber)); 
">="    => (colnumber := (!colnumber) + 2 ;Tokens.GEQ(!linenumber,!colnumber)); 
"<>"    => (colnumber := (!colnumber) + 2 ;Tokens.NEQ(!linenumber,!colnumber)); 
"!"    => (colnumber := (!colnumber) + 1 ;Tokens.NOT(!linenumber,!colnumber));
"+"      => (colnumber := (!colnumber) + 1 ;Tokens.PLUS(!linenumber,!colnumber));
"-"     => (colnumber := (!colnumber) + 1 ;Tokens.MINUS(!linenumber,!colnumber));
"*"     => (colnumber := (!colnumber) + 1 ;Tokens.TIMES(!linenumber,!colnumber));
"/"     => (colnumber := (!colnumber) + 1 ;Tokens.DIV(!linenumber,!colnumber));
"%"     => (colnumber := (!colnumber) + 1 ;Tokens.MOD(!linenumber,!colnumber));
"&&"    => (colnumber := (!colnumber) + 2 ;Tokens.AND(!linenumber,!colnumber));
"||"    => (colnumber := (!colnumber) + 2 ;Tokens.OR(!linenumber,!colnumber));
":="    => (colnumber := (!colnumber) + 2 ;Tokens.SET(!linenumber,!colnumber));
{numeral}   => (colnumber := (!colnumber) + size yytext ;Tokens.NUMERAL( valOf (Int.fromString yytext) ,!linenumber,!colnumber));
{alphanum}   => (colnumber := (!colnumber) + size yytext ;
    if yytext="if" then Tokens.IF(!linenumber,!colnumber) else 
    if yytext="then" then Tokens.THEN(!linenumber,!colnumber) else 
    if yytext="else" then  Tokens.ELSE(!linenumber,!colnumber) else 
    if yytext="endif" then  Tokens.ENDIF(!linenumber,!colnumber) else 
    if yytext="while" then  Tokens.WH(!linenumber,!colnumber) else 
    if yytext="do" then  Tokens.DO(!linenumber,!colnumber) else 
    if yytext="endwh" then  Tokens.ENDWH(!linenumber,!colnumber) else 
    if yytext="tt" then  Tokens.TT(!linenumber,!colnumber) else 
    if yytext="ff" then  Tokens.FF(!linenumber,!colnumber) else 
    if yytext="program" then Tokens.PROG(!linenumber,!colnumber) else 
    if yytext="int" then Tokens.INT(!linenumber,!colnumber) else 
    if yytext="var" then Tokens.VAR(!linenumber,!colnumber) else 
    if yytext="read" then Tokens.READ(!linenumber,!colnumber) else 
    if yytext="write" then Tokens.WRITE(!linenumber,!colnumber) else 
    if yytext="bool" then Tokens.BOOL(!linenumber,!colnumber) else
    Tokens.VARIABLE( yytext ,!linenumber,!colnumber)) ; 
.   => (error (yytext); lex());
