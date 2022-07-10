
open ASTstruc;
exception wrongtype;
exception undeclaredVariable;

val h : (string, aux) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op =) (100, undeclaredVariable)

fun typevar(v)=HashTable.lookup h (v);

fun equality(x,y)= if (x=INTEGER andalso y=INTEGER) orelse (x=BOOLEAN andalso y=BOOLEAN) then true else false;

fun typeexp(MINUS(x,y))=if typeexp(x)=INTEGER andalso typeexp(y)=INTEGER then INTEGER else raise wrongtype
 | typeexp(PLUS(x,y))=if typeexp(x)=INTEGER andalso typeexp(y)=INTEGER then INTEGER else raise wrongtype
 | typeexp(TIMES(x,y))=if typeexp(x)=INTEGER andalso typeexp(y)=INTEGER then INTEGER else raise wrongtype
 | typeexp(DIV(x,y))=if typeexp(x)=INTEGER andalso typeexp(y)=INTEGER then INTEGER else raise wrongtype
 | typeexp(MOD(x,y))=if typeexp(x)=INTEGER andalso typeexp(y)=INTEGER then INTEGER else raise wrongtype
 | typeexp(LT(x,y))=if typeexp(x)=typeexp(y) then BOOLEAN else raise wrongtype
 | typeexp(LEQ(x,y))=if equality(typeexp(x),typeexp(y)) then BOOLEAN else raise wrongtype
 | typeexp(GT(x,y))=if equality(typeexp(x),typeexp(y)) then BOOLEAN else raise wrongtype
 | typeexp(GEQ(x,y))=if equality(typeexp(x),typeexp(y)) then BOOLEAN else raise wrongtype
 | typeexp(EQ(x,y))=if equality(typeexp(x),typeexp(y)) then BOOLEAN else raise wrongtype
 | typeexp(NEQ(x,y))=if equality(typeexp(x),typeexp(y)) then BOOLEAN else raise wrongtype
 | typeexp(AND(x,y))=if typeexp(x)=BOOLEAN andalso typeexp(y)=BOOLEAN then BOOLEAN else raise wrongtype
 | typeexp(OR(x,y))=if typeexp(x)=BOOLEAN andalso typeexp(y)=BOOLEAN then BOOLEAN else raise wrongtype
 | typeexp(NOT(x))=if typeexp(x)=BOOLEAN then BOOLEAN else raise wrongtype
 | typeexp(NEG(x))=if typeexp(x)=INTEGER then INTEGER else raise wrongtype
 | typeexp(intnode(x))=INTEGER
 | typeexp(stringnode(x))=typevar(x)
 | typeexp(TT)=BOOLEAN
 | typeexp(FF)=BOOLEAN;


fun typecheck(PROG(x,y))=typecheck(y)
 | typecheck(BLK(x,y))=typecheck(y)
 | typecheck(SEQ(x,y))=typecheck(x) andalso typecheck(y)
 | typecheck(empty)=true
 | typecheck(SET(x,y))=typeexp(y)=typevar(x)
 | typecheck(ITE(x,y,z))=typeexp(x)=BOOLEAN andalso typecheck(y) andalso typecheck(z)
 | typecheck(WH(x,y))=typeexp(x)=BOOLEAN andalso typecheck(y)
 | typecheck(RD(x))=typevar(x)=INTEGER orelse typevar(x)=BOOLEAN
 | typecheck(WR(x))=typeexp(x)=INTEGER;

fun helper(x,y)=let val tp=HashTable.insert h (x,y); in x end;

fun add(x::xs,INTEGER,z)=DEC(DEC(INT(helper(x,INTEGER)),add(xs,INTEGER,empty)),z)
  | add(x::xs,BOOLEAN,z)=DEC(DEC(BOOL(helper(x,BOOLEAN)),add(xs,BOOLEAN,empty)),z)
  | add([],y,z)=empty;

fun check(x,y)=if y then x else empty;

%%

%name AST

%term
  RPAREN  | LPAREN | AND | OR | SEMICOLON | COLON | DCOLON | LBRACE | RBRACE | 
  NEG | LT | LEQ | EQ | GT | GEQ | NEQ | NOT | PLUS | MINUS | TIMES | DIV | MOD | SET |
  IF | THEN | ELSE | ENDIF | PROG | VAR | INT | BOOL | VARIABLE of string | TT | FF |
  WH | DO | ENDWH | NUMERAL of int | READ | WRITE | COMMA | EOF

%nonterm   PROGRAM of AST | BLOCK of AST | DECLARATION of AST | CMDSEQ of AST | VLIS of string list | TYPE of aux | CMD of AST | EXP of AST 
%pos int
%eop EOF
%noshift EOF

 
%left OR
%left AND
%right NOT
%left EQ 
%left NEQ
%left GT 
%left LT 
%left GEQ
%left LEQ
%left MINUS
%left PLUS
%left TIMES
%left MOD
%left DIV
%right NEG


%start PROGRAM

%verbose

%%
PROGRAM : PROG VARIABLE DCOLON BLOCK (check(PROG(VARIABLE,BLOCK),typecheck(PROG(VARIABLE,BLOCK))))
BLOCK : DECLARATION LBRACE CMDSEQ RBRACE (BLK(DECLARATION,CMDSEQ))
DECLARATION : VAR VLIS TYPE SEMICOLON DECLARATION (add(VLIS,TYPE,DECLARATION))
 | (empty)
VLIS : VARIABLE COMMA VLIS (VARIABLE::VLIS)
 | VARIABLE COLON([VARIABLE])
TYPE : INT (INTEGER)
 | BOOL (BOOLEAN)
CMDSEQ : CMD SEMICOLON CMDSEQ (SEQ(CMD,CMDSEQ))
 | (empty) 
CMD : VARIABLE SET EXP (SET(VARIABLE,EXP))
 | IF EXP THEN LBRACE CMDSEQ RBRACE ELSE LBRACE CMDSEQ RBRACE ENDIF (ITE(EXP,CMDSEQ1,CMDSEQ2))
 | WH EXP DO LBRACE CMDSEQ RBRACE ENDWH (WH(EXP,CMDSEQ))
 | READ VARIABLE (RD(VARIABLE))
 | WRITE EXP (WR(EXP))
EXP : NUMERAL  (intnode(NUMERAL))   
 | PLUS NUMERAL (PLUS(intnode(NUMERAL),empty))
 | VARIABLE (stringnode(VARIABLE))  
 | TT (TT)
 | FF (FF)
 | EXP LT EXP (LT(EXP1,EXP2))
 | EXP LEQ EXP (LEQ(EXP1,EXP2))
 | EXP GEQ EXP (GEQ(EXP1,EXP2))
 | EXP GT EXP (GT(EXP1,EXP2))
 | EXP EQ EXP (EQ(EXP1,EXP2))
 | EXP NEQ EXP (NEQ(EXP1,EXP2))
 | EXP AND EXP (AND(EXP1,EXP2))
 | EXP OR EXP (OR(EXP1,EXP2))
 | NOT EXP (NOT(EXP))
 | NEG EXP (NEG(EXP))
 | EXP PLUS EXP (PLUS(EXP1,EXP2))
 | EXP MINUS EXP (MINUS(EXP1,EXP2))
 | EXP TIMES EXP (TIMES(EXP1,EXP2)) 
 | EXP DIV EXP (DIV(EXP1,EXP2))
 | EXP MOD EXP (MOD(EXP1,EXP2))       
 | LPAREN EXP RPAREN (EXP)


         

