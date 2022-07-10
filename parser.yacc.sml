functor ASTLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : AST_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

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


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\076\000\003\000\054\000\004\000\053\000\011\000\052\000\
\\012\000\051\000\013\000\050\000\014\000\049\000\015\000\048\000\
\\016\000\047\000\018\000\046\000\019\000\045\000\020\000\044\000\
\\021\000\043\000\022\000\042\000\000\000\
\\001\000\002\000\034\000\010\000\033\000\017\000\032\000\018\000\031\000\
\\032\000\030\000\033\000\029\000\034\000\028\000\038\000\027\000\000\000\
\\001\000\003\000\054\000\004\000\053\000\011\000\052\000\012\000\051\000\
\\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\018\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\022\000\042\000\025\000\061\000\000\000\
\\001\000\003\000\054\000\004\000\053\000\011\000\052\000\012\000\051\000\
\\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\018\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\022\000\042\000\036\000\059\000\000\000\
\\001\000\005\000\024\000\000\000\
\\001\000\005\000\039\000\000\000\
\\001\000\006\000\023\000\041\000\022\000\000\000\
\\001\000\007\000\005\000\000\000\
\\001\000\008\000\009\000\000\000\
\\001\000\008\000\077\000\000\000\
\\001\000\008\000\078\000\000\000\
\\001\000\008\000\085\000\000\000\
\\001\000\009\000\025\000\000\000\
\\001\000\009\000\081\000\000\000\
\\001\000\009\000\082\000\000\000\
\\001\000\009\000\087\000\000\000\
\\001\000\023\000\037\000\000\000\
\\001\000\026\000\084\000\000\000\
\\001\000\027\000\088\000\000\000\
\\001\000\028\000\003\000\000\000\
\\001\000\030\000\021\000\031\000\020\000\000\000\
\\001\000\032\000\004\000\000\000\
\\001\000\032\000\011\000\000\000\
\\001\000\032\000\035\000\000\000\
\\001\000\037\000\083\000\000\000\
\\001\000\038\000\055\000\000\000\
\\001\000\042\000\000\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\029\000\008\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\024\000\018\000\032\000\017\000\035\000\016\000\039\000\015\000\
\\040\000\014\000\000\000\
\\100\000\003\000\054\000\004\000\053\000\011\000\052\000\012\000\051\000\
\\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\018\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\022\000\042\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\003\000\054\000\004\000\053\000\011\000\052\000\012\000\051\000\
\\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\018\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\022\000\042\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\012\000\051\000\015\000\048\000\018\000\046\000\019\000\045\000\
\\020\000\044\000\021\000\043\000\022\000\042\000\000\000\
\\111\000\018\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\022\000\042\000\000\000\
\\112\000\012\000\051\000\018\000\046\000\019\000\045\000\020\000\044\000\
\\021\000\043\000\022\000\042\000\000\000\
\\113\000\011\000\052\000\012\000\051\000\015\000\048\000\018\000\046\000\
\\019\000\045\000\020\000\044\000\021\000\043\000\022\000\042\000\000\000\
\\114\000\011\000\052\000\012\000\051\000\014\000\049\000\015\000\048\000\
\\016\000\047\000\018\000\046\000\019\000\045\000\020\000\044\000\
\\021\000\043\000\022\000\042\000\000\000\
\\115\000\011\000\052\000\012\000\051\000\014\000\049\000\015\000\048\000\
\\018\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\022\000\042\000\000\000\
\\116\000\011\000\052\000\012\000\051\000\013\000\050\000\014\000\049\000\
\\015\000\048\000\016\000\047\000\018\000\046\000\019\000\045\000\
\\020\000\044\000\021\000\043\000\022\000\042\000\000\000\
\\117\000\003\000\054\000\011\000\052\000\012\000\051\000\013\000\050\000\
\\014\000\049\000\015\000\048\000\016\000\047\000\018\000\046\000\
\\019\000\045\000\020\000\044\000\021\000\043\000\022\000\042\000\000\000\
\\118\000\011\000\052\000\012\000\051\000\013\000\050\000\014\000\049\000\
\\015\000\048\000\016\000\047\000\018\000\046\000\019\000\045\000\
\\020\000\044\000\021\000\043\000\022\000\042\000\000\000\
\\119\000\000\000\
\\120\000\020\000\044\000\021\000\043\000\022\000\042\000\000\000\
\\121\000\018\000\046\000\020\000\044\000\021\000\043\000\022\000\042\000\000\000\
\\122\000\021\000\043\000\022\000\042\000\000\000\
\\123\000\000\000\
\\124\000\021\000\043\000\000\000\
\\125\000\000\000\
\"
val actionRowNumbers =
"\019\000\021\000\007\000\030\000\
\\008\000\027\000\022\000\036\000\
\\020\000\006\000\004\000\012\000\
\\001\000\023\000\001\000\016\000\
\\001\000\005\000\034\000\033\000\
\\022\000\032\000\036\000\028\000\
\\041\000\042\000\046\000\045\000\
\\044\000\025\000\001\000\001\000\
\\001\000\040\000\003\000\001\000\
\\002\000\030\000\031\000\035\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\043\000\055\000\056\000\
\\000\000\009\000\037\000\010\000\
\\029\000\061\000\060\000\059\000\
\\058\000\057\000\052\000\049\000\
\\050\000\051\000\048\000\047\000\
\\054\000\053\000\062\000\036\000\
\\036\000\013\000\014\000\024\000\
\\017\000\039\000\011\000\036\000\
\\015\000\018\000\038\000\026\000"
val gotoT =
"\
\\001\000\087\000\000\000\
\\000\000\
\\000\000\
\\002\000\005\000\003\000\004\000\000\000\
\\000\000\
\\000\000\
\\005\000\008\000\000\000\
\\004\000\011\000\007\000\010\000\000\000\
\\006\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\024\000\000\000\
\\000\000\
\\008\000\034\000\000\000\
\\000\000\
\\008\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\038\000\000\000\
\\000\000\
\\004\000\039\000\007\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\054\000\000\000\
\\008\000\055\000\000\000\
\\008\000\056\000\000\000\
\\000\000\
\\000\000\
\\008\000\058\000\000\000\
\\000\000\
\\003\000\060\000\000\000\
\\000\000\
\\000\000\
\\008\000\061\000\000\000\
\\008\000\062\000\000\000\
\\008\000\063\000\000\000\
\\008\000\064\000\000\000\
\\008\000\065\000\000\000\
\\008\000\066\000\000\000\
\\008\000\067\000\000\000\
\\008\000\068\000\000\000\
\\008\000\069\000\000\000\
\\008\000\070\000\000\000\
\\008\000\071\000\000\000\
\\008\000\072\000\000\000\
\\008\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\077\000\007\000\010\000\000\000\
\\004\000\078\000\007\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\084\000\007\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 88
val numrules = 36
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUMERAL of unit ->  (int) | VARIABLE of unit ->  (string)
 | EXP of unit ->  (AST) | CMD of unit ->  (AST)
 | TYPE of unit ->  (aux) | VLIS of unit ->  (string list)
 | CMDSEQ of unit ->  (AST) | DECLARATION of unit ->  (AST)
 | BLOCK of unit ->  (AST) | PROGRAM of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "RPAREN"
  | (T 1) => "LPAREN"
  | (T 2) => "AND"
  | (T 3) => "OR"
  | (T 4) => "SEMICOLON"
  | (T 5) => "COLON"
  | (T 6) => "DCOLON"
  | (T 7) => "LBRACE"
  | (T 8) => "RBRACE"
  | (T 9) => "NEG"
  | (T 10) => "LT"
  | (T 11) => "LEQ"
  | (T 12) => "EQ"
  | (T 13) => "GT"
  | (T 14) => "GEQ"
  | (T 15) => "NEQ"
  | (T 16) => "NOT"
  | (T 17) => "PLUS"
  | (T 18) => "MINUS"
  | (T 19) => "TIMES"
  | (T 20) => "DIV"
  | (T 21) => "MOD"
  | (T 22) => "SET"
  | (T 23) => "IF"
  | (T 24) => "THEN"
  | (T 25) => "ELSE"
  | (T 26) => "ENDIF"
  | (T 27) => "PROG"
  | (T 28) => "VAR"
  | (T 29) => "INT"
  | (T 30) => "BOOL"
  | (T 31) => "VARIABLE"
  | (T 32) => "TT"
  | (T 33) => "FF"
  | (T 34) => "WH"
  | (T 35) => "DO"
  | (T 36) => "ENDWH"
  | (T 37) => "NUMERAL"
  | (T 38) => "READ"
  | (T 39) => "WRITE"
  | (T 40) => "COMMA"
  | (T 41) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 36) $$ (T 35) $$ (T 34)
 $$ (T 33) $$ (T 32) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.BLOCK BLOCK1, _, BLOCK1right)) :: _ :: ( _,
 ( MlyValue.VARIABLE VARIABLE1, _, _)) :: ( _, ( _, PROG1left, _)) :: 
rest671)) => let val  result = MlyValue.PROGRAM (fn _ => let val  (
VARIABLE as VARIABLE1) = VARIABLE1 ()
 val  (BLOCK as BLOCK1) = BLOCK1 ()
 in (check(PROG(VARIABLE,BLOCK),typecheck(PROG(VARIABLE,BLOCK))))
end)
 in ( LrTable.NT 0, ( result, PROG1left, BLOCK1right), rest671)
end
|  ( 1, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.CMDSEQ 
CMDSEQ1, _, _)) :: _ :: ( _, ( MlyValue.DECLARATION DECLARATION1, 
DECLARATION1left, _)) :: rest671)) => let val  result = MlyValue.BLOCK
 (fn _ => let val  (DECLARATION as DECLARATION1) = DECLARATION1 ()
 val  (CMDSEQ as CMDSEQ1) = CMDSEQ1 ()
 in (BLK(DECLARATION,CMDSEQ))
end)
 in ( LrTable.NT 1, ( result, DECLARATION1left, RBRACE1right), rest671
)
end
|  ( 2, ( ( _, ( MlyValue.DECLARATION DECLARATION1, _, 
DECLARATION1right)) :: _ :: ( _, ( MlyValue.TYPE TYPE1, _, _)) :: ( _,
 ( MlyValue.VLIS VLIS1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671))
 => let val  result = MlyValue.DECLARATION (fn _ => let val  (VLIS as 
VLIS1) = VLIS1 ()
 val  (TYPE as TYPE1) = TYPE1 ()
 val  (DECLARATION as DECLARATION1) = DECLARATION1 ()
 in (add(VLIS,TYPE,DECLARATION))
end)
 in ( LrTable.NT 2, ( result, VAR1left, DECLARATION1right), rest671)

end
|  ( 3, ( rest671)) => let val  result = MlyValue.DECLARATION (fn _ =>
 (empty))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.VLIS VLIS1, _, VLIS1right)) :: _ :: ( _, ( 
MlyValue.VARIABLE VARIABLE1, VARIABLE1left, _)) :: rest671)) => let
 val  result = MlyValue.VLIS (fn _ => let val  (VARIABLE as VARIABLE1)
 = VARIABLE1 ()
 val  (VLIS as VLIS1) = VLIS1 ()
 in (VARIABLE::VLIS)
end)
 in ( LrTable.NT 4, ( result, VARIABLE1left, VLIS1right), rest671)
end
|  ( 5, ( ( _, ( _, _, COLON1right)) :: ( _, ( MlyValue.VARIABLE 
VARIABLE1, VARIABLE1left, _)) :: rest671)) => let val  result = 
MlyValue.VLIS (fn _ => let val  (VARIABLE as VARIABLE1) = VARIABLE1 ()
 in ([VARIABLE])
end)
 in ( LrTable.NT 4, ( result, VARIABLE1left, COLON1right), rest671)

end
|  ( 6, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.TYPE (fn _ => (INTEGER))
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 7, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.TYPE (fn _ => (BOOLEAN))
 in ( LrTable.NT 5, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.CMDSEQ CMDSEQ1, _, CMDSEQ1right)) :: _ :: (
 _, ( MlyValue.CMD CMD1, CMD1left, _)) :: rest671)) => let val  result
 = MlyValue.CMDSEQ (fn _ => let val  (CMD as CMD1) = CMD1 ()
 val  (CMDSEQ as CMDSEQ1) = CMDSEQ1 ()
 in (SEQ(CMD,CMDSEQ))
end)
 in ( LrTable.NT 3, ( result, CMD1left, CMDSEQ1right), rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.CMDSEQ (fn _ => (
empty))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.VARIABLE VARIABLE1, VARIABLE1left, _)) :: rest671)) => let
 val  result = MlyValue.CMD (fn _ => let val  (VARIABLE as VARIABLE1)
 = VARIABLE1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (SET(VARIABLE,EXP))
end)
 in ( LrTable.NT 6, ( result, VARIABLE1left, EXP1right), rest671)
end
|  ( 11, ( ( _, ( _, _, ENDIF1right)) :: _ :: ( _, ( MlyValue.CMDSEQ 
CMDSEQ2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.CMDSEQ CMDSEQ1, _, _
)) :: _ :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left,
 _)) :: rest671)) => let val  result = MlyValue.CMD (fn _ => let val 
 (EXP as EXP1) = EXP1 ()
 val  CMDSEQ1 = CMDSEQ1 ()
 val  CMDSEQ2 = CMDSEQ2 ()
 in (ITE(EXP,CMDSEQ1,CMDSEQ2))
end)
 in ( LrTable.NT 6, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 12, ( ( _, ( _, _, ENDWH1right)) :: _ :: ( _, ( MlyValue.CMDSEQ 
CMDSEQ1, _, _)) :: _ :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, 
( _, WH1left, _)) :: rest671)) => let val  result = MlyValue.CMD (fn _
 => let val  (EXP as EXP1) = EXP1 ()
 val  (CMDSEQ as CMDSEQ1) = CMDSEQ1 ()
 in (WH(EXP,CMDSEQ))
end)
 in ( LrTable.NT 6, ( result, WH1left, ENDWH1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.VARIABLE VARIABLE1, _, VARIABLE1right)) :: 
( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.CMD (fn _ => let val  (VARIABLE as VARIABLE1) = VARIABLE1 ()
 in (RD(VARIABLE))
end)
 in ( LrTable.NT 6, ( result, READ1left, VARIABLE1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
WRITE1left, _)) :: rest671)) => let val  result = MlyValue.CMD (fn _
 => let val  (EXP as EXP1) = EXP1 ()
 in (WR(EXP))
end)
 in ( LrTable.NT 6, ( result, WRITE1left, EXP1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.NUMERAL NUMERAL1, NUMERAL1left, 
NUMERAL1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  (NUMERAL as NUMERAL1) = NUMERAL1 ()
 in (intnode(NUMERAL))
end)
 in ( LrTable.NT 7, ( result, NUMERAL1left, NUMERAL1right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.NUMERAL NUMERAL1, _, NUMERAL1right)) :: ( _
, ( _, PLUS1left, _)) :: rest671)) => let val  result = MlyValue.EXP
 (fn _ => let val  (NUMERAL as NUMERAL1) = NUMERAL1 ()
 in (PLUS(intnode(NUMERAL),empty))
end)
 in ( LrTable.NT 7, ( result, PLUS1left, NUMERAL1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.VARIABLE VARIABLE1, VARIABLE1left, 
VARIABLE1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  (VARIABLE as VARIABLE1) = VARIABLE1 ()
 in (stringnode(VARIABLE))
end)
 in ( LrTable.NT 7, ( result, VARIABLE1left, VARIABLE1right), rest671)

end
|  ( 18, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.EXP (fn _ => (TT))
 in ( LrTable.NT 7, ( result, TT1left, TT1right), rest671)
end
|  ( 19, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.EXP (fn _ => (FF))
 in ( LrTable.NT 7, ( result, FF1left, FF1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (LT(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (LEQ(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (GEQ(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (GT(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (EQ(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (NEQ(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AND(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (OR(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  (EXP as EXP1) = EXP1 ()
 in (NOT(EXP))
end)
 in ( LrTable.NT 7, ( result, NOT1left, EXP1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NEG1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  (EXP as EXP1) = EXP1 ()
 in (NEG(EXP))
end)
 in ( LrTable.NT 7, ( result, NEG1left, EXP1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (PLUS(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (MINUS(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (TIMES(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (DIV(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (MOD(EXP1,EXP2))
end)
 in ( LrTable.NT 7, ( result, EXP1left, EXP2right), rest671)
end
|  ( 35, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : AST_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun SET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun PROG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun VARIABLE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VARIABLE (fn () => i),p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun WH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.NUMERAL (fn () => i),p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
