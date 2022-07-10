functor ASTLexFun(structure Tokens:AST_TOKENS)=
   struct
    structure UserDeclarations =
      struct
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

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\034\037\003\003\036\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\034\033\003\003\003\032\030\003\029\028\027\026\025\024\003\023\
\\021\021\021\021\021\021\021\021\021\021\018\017\014\013\011\003\
\\003\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\003\003\003\003\003\
\\003\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\008\006\005\004\003\
\\003"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\016\015\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\020\000\000\019\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\022\022\022\022\022\022\022\022\022\022\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\031\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (34, 
"\000\000\000\000\000\000\000\000\000\035\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 67)], trans = 0},
{fin = [(N 23),(N 67)], trans = 0},
{fin = [(N 21),(N 67)], trans = 0},
{fin = [(N 1),(N 67)], trans = 6},
{fin = [(N 56)], trans = 0},
{fin = [(N 19),(N 67)], trans = 0},
{fin = [(N 65),(N 67)], trans = 9},
{fin = [(N 65)], trans = 9},
{fin = [(N 32),(N 67)], trans = 11},
{fin = [(N 35)], trans = 0},
{fin = [(N 30),(N 67)], trans = 0},
{fin = [(N 25),(N 67)], trans = 14},
{fin = [(N 38)], trans = 0},
{fin = [(N 28)], trans = 0},
{fin = [(N 3),(N 67)], trans = 0},
{fin = [(N 7),(N 67)], trans = 18},
{fin = [(N 59)], trans = 0},
{fin = [(N 10)], trans = 0},
{fin = [(N 62),(N 67)], trans = 21},
{fin = [(N 62)], trans = 21},
{fin = [(N 48),(N 67)], trans = 0},
{fin = [(N 44),(N 67)], trans = 0},
{fin = [(N 5),(N 67)], trans = 0},
{fin = [(N 42),(N 67)], trans = 0},
{fin = [(N 46),(N 67)], trans = 0},
{fin = [(N 17),(N 67)], trans = 0},
{fin = [(N 15),(N 67)], trans = 0},
{fin = [(N 67)], trans = 30},
{fin = [(N 53)], trans = 0},
{fin = [(N 50),(N 67)], trans = 0},
{fin = [(N 40),(N 67)], trans = 0},
{fin = [(N 13),(N 67)], trans = 34},
{fin = [(N 13)], trans = 34},
{fin = [(N 1),(N 67)], trans = 0},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (colnumber := 1 ; linenumber := (!linenumber) + 1; lex())
| 10 => ( colnumber := (!colnumber) + 2 ; Tokens.DCOLON(!linenumber,!colnumber))
| 13 => let val yytext=yymktext() in colnumber := (!colnumber) + size yytext ;lex() end
| 15 => (colnumber := (!colnumber) + 1 ;Tokens.LPAREN(!linenumber,!colnumber))
| 17 => (colnumber := (!colnumber) + 1 ;Tokens.RPAREN(!linenumber,!colnumber))
| 19 => (colnumber := (!colnumber) + 1 ;Tokens.LBRACE(!linenumber,!colnumber))
| 21 => (colnumber := (!colnumber) + 1 ;Tokens.RBRACE(!linenumber,!colnumber))
| 23 => (colnumber := (!colnumber) + 1 ;Tokens.NEG(!linenumber,!colnumber))
| 25 => (colnumber := (!colnumber) + 1 ;Tokens.LT(!linenumber,!colnumber))
| 28 => (colnumber := (!colnumber) + 2 ;Tokens.LEQ(!linenumber,!colnumber))
| 3 => ( colnumber := (!colnumber) + 1 ; Tokens.SEMICOLON(!linenumber,!colnumber))
| 30 => (colnumber := (!colnumber) + 1 ;Tokens.EQ(!linenumber,!colnumber))
| 32 => (colnumber := (!colnumber) + 1 ;Tokens.GT(!linenumber,!colnumber))
| 35 => (colnumber := (!colnumber) + 2 ;Tokens.GEQ(!linenumber,!colnumber))
| 38 => (colnumber := (!colnumber) + 2 ;Tokens.NEQ(!linenumber,!colnumber))
| 40 => (colnumber := (!colnumber) + 1 ;Tokens.NOT(!linenumber,!colnumber))
| 42 => (colnumber := (!colnumber) + 1 ;Tokens.PLUS(!linenumber,!colnumber))
| 44 => (colnumber := (!colnumber) + 1 ;Tokens.MINUS(!linenumber,!colnumber))
| 46 => (colnumber := (!colnumber) + 1 ;Tokens.TIMES(!linenumber,!colnumber))
| 48 => (colnumber := (!colnumber) + 1 ;Tokens.DIV(!linenumber,!colnumber))
| 5 => ( colnumber := (!colnumber) + 1 ; Tokens.COMMA(!linenumber,!colnumber))
| 50 => (colnumber := (!colnumber) + 1 ;Tokens.MOD(!linenumber,!colnumber))
| 53 => (colnumber := (!colnumber) + 2 ;Tokens.AND(!linenumber,!colnumber))
| 56 => (colnumber := (!colnumber) + 2 ;Tokens.OR(!linenumber,!colnumber))
| 59 => (colnumber := (!colnumber) + 2 ;Tokens.SET(!linenumber,!colnumber))
| 62 => let val yytext=yymktext() in colnumber := (!colnumber) + size yytext ;Tokens.NUMERAL( valOf (Int.fromString yytext) ,!linenumber,!colnumber) end
| 65 => let val yytext=yymktext() in colnumber := (!colnumber) + size yytext ;
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
    Tokens.VARIABLE( yytext ,!linenumber,!colnumber) end
| 67 => let val yytext=yymktext() in error (yytext); lex() end
| 7 => ( colnumber := (!colnumber) + 1 ; Tokens.COLON(!linenumber,!colnumber))
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
