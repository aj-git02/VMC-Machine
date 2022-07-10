exception undeclaredVariable;
val symbolTable : (string, int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op =) (100, undeclaredVariable)

datatype aux= INTEGER | BOOLEAN;
datatype AST=empty | PROG of string*AST | INT of string | BOOL of string | LT of AST*AST | LEQ of AST*AST | GT of AST*AST | GEQ of AST*AST | EQ of AST*AST | NEG of AST | NEQ of AST*AST | ITE of AST*AST*AST | WH of AST*AST | SEQ of AST*AST | AND of AST*AST | NOT of AST | OR of AST*AST | TT | FF | PLUS of AST*AST | MINUS of AST*AST | TIMES of AST*AST | DIV of AST*AST | MOD of AST*AST | SET of string*AST | DEC of AST*AST | BLK of AST*AST | stringnode of string| intnode of int | RD of string | WR of AST;

(* open ASTParser;
datatype AST=result; *)



signature STACK =
  sig
  type 'a Stack
  exception EmptyStack
  exception Error of string
  val create: unit -> 'a Stack
  val push : 'a * 'a Stack -> 'a Stack
  val pop : 'a Stack -> 'a Stack
  val top : 'a Stack -> 'a
  val empty: 'a Stack -> bool
    val poptop : 'a Stack -> ('a * 'a Stack) option
  val nth : 'a Stack * int -> 'a
  val drop : 'a Stack * int -> 'a Stack
  val depth : 'a Stack -> int
  val app : ('a -> unit) -> 'a Stack -> unit
  val map : ('a -> 'b) -> 'a Stack -> 'b Stack
  val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
  val find : ('a -> bool) -> 'a Stack -> 'a option
  val filter : ('a -> bool) -> 'a Stack -> 'a Stack
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
  val exists : ('a -> bool) -> 'a Stack -> bool
  val all : ('a -> bool) -> 'a Stack -> bool
  val list2stack : 'a list -> 'a Stack (* Convert a list into a stack *)
  val stack2list: 'a Stack -> 'a list (* Convert a stack into a list *)
  val toString: ('a -> string) -> 'a Stack -> string
end;

fun tp(x)=if x>5 then true else false;
val l=[1,2,3,6,7,8];
List.filter tp l;
List.filter;

(* > some prob *)
structure FunStack : STACK =
struct
  type 'a Stack='a list;
  exception EmptyStack;
  exception Error of string;
  fun create() = [];
  fun push(a,l)=a::l;
  fun pop([]) = raise EmptyStack
    | pop(a::l)=l;
  fun top([]) = raise EmptyStack
    | top(a::l)=a;
  fun empty([])= true
    | empty(a)=false;
  fun nth(a::l,0)=a
    | nth(a::l,x)=nth(l,x-1)
    | nth([],x)= raise Error("less then n elements");
  val poptop=List.getItem;
  val drop=List.drop;
  fun depth(l)=List.length(l);
  val app=List.app;
  val map=List.map;
  val mapPartial=List.mapPartial;
  val find=List.find;
  val filter=List.filter;
  val foldr=List.foldr;
  val foldl=List.foldl;
  val exists=List.exists;
  val all=List.all;
  fun list2stack(l)=l;
  fun stack2list(l)=l;
  fun makestring(f,[],x)=x
    |makestring(f,a::l,x)=makestring(f,l,x^f(a));
  fun toString(f) l=makestring(f,l,"");
end;

fun stacker(s,l)=if FunStack.empty(s) then l else
let val a=FunStack.top(s) in
  stacker(FunStack.pop(s),FunStack.push(a,l)) end;  

val l=FunStack.create();

datatype stacktype=Set | Ite | exp | Int of int | Str of string | bexp | itestart | setstart  | cmd  | cmdseq | And | Or | Not | plus | minus | times | Tt | Ff | Lt | Leq | Geq | Gt | Eq | Neq | Neg | Wh | Seq | DIv | Mod;

fun stringmaker(Set)="SET "
  |stringmaker(Ite)="IfThenElse "
  |stringmaker(exp)=" "
  |stringmaker(cmd)=" "
  |stringmaker(cmdseq)=" "
  |stringmaker(Str(x))=x^" "
  |stringmaker(Int(x))=Int.toString(x)^" " (*this rule is doubtful*)
  |stringmaker(bexp)=" "
  |stringmaker(And)="AND "
  |stringmaker(Or)="OR "
  |stringmaker(Not)="NOT "
  |stringmaker(plus)="+ "
  |stringmaker(minus)="- "
  |stringmaker(times)="* "
  |stringmaker(DIv)="// "
  |stringmaker(Mod)="% "
  |stringmaker(Gt)="> "
  |stringmaker(Lt)="< "
  |stringmaker(Geq)=">= "
  |stringmaker(Leq)="<= "
  |stringmaker(Eq)="= "
  |stringmaker(Neq)="<> "
  |stringmaker(Neg)="~ "
  |stringmaker(Wh)="while "
  |stringmaker(Seq)="Seq "
  |stringmaker(setstart)=" "

val l=[[1],[2]];
[2,3]::l;

fun postexp(PLUS(x,y),l)=postexp(x,postexp(y,FunStack.push(plus,l)))
  | postexp(MINUS(x,y),l)=postexp(x,postexp(y,FunStack.push(minus,l)))
  | postexp(DIV(x,y),l)=postexp(x,postexp(y,FunStack.push(DIv,l)))
  | postexp(MOD(x,y),l)=postexp(x,postexp(y,FunStack.push(Mod,l)))
  | postexp(TIMES(x,y),l)=postexp(x,postexp(y,FunStack.push(times,l)))
  | postexp(AND(x,y),l)=postexp(x,postexp(y,FunStack.push(And,l)))
  | postexp(OR(x,y),l)=postexp(x,postexp(y,FunStack.push(Or,l)))
  | postexp(NOT(x),l)=postexp(x,FunStack.push(Not,l))
  | postexp(NEG(x),l)=postexp(x,FunStack.push(Neg,l))
  | postexp(empty,l)=FunStack.push(exp,l)
  | postexp(LT(x,y),l)=postexp(x,postexp(y,FunStack.push(Lt,l)))
  | postexp(GT(x,y),l)=postexp(x,postexp(y,FunStack.push(Gt,l)))
  | postexp(EQ(x,y),l)=postexp(x,postexp(y,FunStack.push(Eq,l)))
  | postexp(GEQ(x,y),l)=postexp(x,postexp(y,FunStack.push(Geq,l)))
  | postexp(LEQ(x,y),l)=postexp(x,postexp(y,FunStack.push(Leq,l)))
  | postexp(NEQ(x,y),l)=postexp(x,postexp(y,FunStack.push(Neq,l)))
  | postexp(intnode(x),l)=FunStack.push(Int(x),FunStack.push(exp,l))
  | postexp(FF,s)=FunStack.push(Int(0),FunStack.push(exp,s))
  | postexp(TT,s)=FunStack.push(Int(1),FunStack.push(exp,s))
  | postexp(stringnode(x),s)=FunStack.push(Str(x),FunStack.push(exp,s));

(*fun postfix(intnode(x),s)= FunStack.push(Int(x),s)
  | postfix(OR(x,y),s: stacktype FunStack.Stack)= FunStack.push(exp(postexp(x,FunStack.create())),FunStack.push(exp(postexp(y,FunStack.create())),FunStack.push(Or,s))) *)
fun postfix(SEQ(x,SEQ(y:AST,z:AST)),s)=postfix(x,FunStack.push(cmd,postfix(SEQ(y,z),FunStack.push(Seq,s))))
(*  | postfix(SEQ(x,y),s)= FunStack.push(cmd(postfix(x,FunStack.create())),FunStack.push(cmd(postfix(y,FunStack.create())),FunStack.push(Seq,s))) *)
  | postfix(SEQ(x,y),s)= postfix(x,FunStack.push(cmd,s))
  |postfix(WH(x,SEQ(y,z)),s)=postexp(x,postfix(SEQ(y,z),FunStack.push(Wh,s)))
 (* |postfix(WH(x,y),s)=FunStack.push(bexp(postexp(x,FunStack.create())),FunStack.push(cmd(postfix(y,FunStack.create())),FunStack.push(Wh,s))) *)
  | postfix(ITE(x,y,z),s)=postexp(x,postfix(y,FunStack.push(itestart,postfix(z,FunStack.push(Ite,s)))))
  | postfix(SET(x,y),s)=FunStack.push(setstart,FunStack.push(Str(x),postexp(y,FunStack.push(Set,s))))
  | postfix(empty,s)=s
  | postfix(RD(x),s)=s
  | postfix(WR(x),s)=s;
  (*| postfix(stringnode(x),s)=FunStack.push(Str(x),s);*)

SEQ (SET ("A",NEG (intnode 4)),empty);
val l=postfix(SEQ (SET ("A",NEG (intnode 4)),empty),FunStack.create());
FunStack.top(l);


val maxMemSize=10;
val memory=Array.array(maxMemSize,0);
fun memstring(mem,a,s)=if a=maxMemSize then s else memstring(mem,a+1,s^", "^Int.toString(Array.sub(mem,a)));

(*Array.update(memory,0,5);*)
fun halp(Ite)=true
|halp(Wh)=true
|halp(Seq)=true
|halp(Set)=true
|halp(x)=false;

signature VMC=
sig
  val toString: stacktype FunStack.Stack* int array *stacktype FunStack.Stack -> string
  val rules : stacktype FunStack.Stack* int array *stacktype FunStack.Stack -> stacktype FunStack.Stack* int array *stacktype FunStack.Stack
end;

fun isint(Int(x))=true
  |isint(x)=false;
fun iscmd(cmd)=true
  |iscmd(x)=false;
fun isexp(exp)=true
  |isexp(x)=false;
fun isstr(Str(x))=true
  |isstr(x)=false;
fun isset(Set)=true
  |isset(x)=false;
fun iswh(Wh)=true
  |iswh(x)=false;
fun isite(Ite)=true
  |isite(x)=false;
fun isff(Int(0))=true
  |isff(x)=false;
fun isseq(Seq)=true
  |isseq(x)=false;
fun isbexp(bexp)=true
  |isbexp(x)=false;
fun isitestart(itestart)=true
|isitestart(x)=false;
fun isand(And)=true
| isand(x)=false;
fun isor(Or)=true
| isor(x)=false;
fun istimes(times)=true
  | istimes(x)=false;
fun isminus(minus)=true
| isminus(x)=false;
fun isdiv(DIv)=true
| isdiv(x)=false;
fun ismod(Mod)=true
| ismod(x)=false;
fun isplus(plus)=true
| isplus(x)=false;
fun isnot(Not)=true
| isnot(x)=false;
fun isneg(Neg)=true
| isneg(x)=false;
fun iseq(Eq)=true
| iseq(x)=false;
fun isneq(Neq)=true
| isneq(x)=false;
fun islt(Lt)=true
| islt(x)=false;
fun isgt(Gt)=true
| isgt(x)=false;
fun iscmd(cmd)=true
| iscmd(x)=false;
fun isgeq(Geq)=true
| isgeq(x)=false;
fun isleq(Leq)=true
  | isleq(x)=false;
fun iscmdseq(cmdseq)=true
  | iscmdseq(x)=false;
  fun issetstart(setstart)=true
  | issetstart(x)=false;
fun extract(Int(x))=x;
fun extractvar(Str(x))=x;
fun extractbool(Int(1))=true
  |extractbool(Int(0))=false;
fun Convert(true)=Int(1)
  |Convert(false)=Int(0);


fun cmdpopper(s)=if iscmd(FunStack.top(s)) then FunStack.pop(s) else cmdpopper(FunStack.pop(s));
fun exppopper(s)=if isexp(FunStack.top(s)) then FunStack.pop(s) else exppopper(FunStack.pop(s));
fun cmd2popper(s,0,l)=let val x=FunStack.top(s) in 
if iscmd(FunStack.top(s)) then cmd2popper(FunStack.pop(s),1,FunStack.push(FunStack.top(s),l)) else cmd2popper(FunStack.pop(s),0,FunStack.push(FunStack.top(s),l))
end
|cmd2popper(s,1,l)=if iscmd(FunStack.top(s)) then stacker(l,FunStack.pop(s)) else cmd2popper(FunStack.pop(s),1,l);
fun specialwhile(V,C)=if isexp(FunStack.top(C)) then 

structure Vmc: VMC=
struct
  fun toString(V,M,C)="( "^FunStack.toString stringmaker (V)^" ; "^memstring(memory,0,"")^" ; "^FunStack.toString stringmaker (C)^" )";
  fun rules(V,M,C)= 
  let 
    val t=if FunStack.depth(C)>=1 then FunStack.top(C) else Ff;
    val t1=if FunStack.depth(C)>=2 then FunStack.nth(C,1) else Ff;
    val s1=FunStack.filter halp (V);
    val s2=FunStack.filter halp (C);
    val a=if FunStack.depth(s2)>=1 then FunStack.top(s2) else Tt;
    val b=if FunStack.depth(s2)>=2 then FunStack.nth(s2,1) else Tt;
    val c=if FunStack.depth(s2)>=3 then FunStack.nth(s2,2) else Tt;
    val d=if FunStack.depth(s2)>=4 then FunStack.nth(s2,3) else Tt;
    val e=if FunStack.depth(V)>=1 then FunStack.top(V) else Tt;
    val f=if FunStack.depth(V)>=2 then FunStack.nth(V,1) else Tt;
    val g=if FunStack.depth(V)>=3 then FunStack.nth(V,2) else Tt;
  in
  (* if iswh(a) then
    if isff(e) then (exppopper(V),M,FunStack.pop(C)) else (FunStack.pop(FunStack.pop(FunStack.pop(V))),M,stacker(stacker(extractcmdseq(f),FunStack.create()),FunStack.push(g,FunStack.push(f,C)))) else
  if iswh(c) andalso (isbexp(a) andalso iscmdseq(b)) then
    (FunStack.push(b,FunStack.push(a,V)),M,stacker(stacker(extractbexp(a),FunStack.create()),FunStack.pop(FunStack.pop(C)))) else
   *)
  if isitestart(t) then 
    if isff(e) then (FunStack.pop(V),M,cmdpopper(C)) else
    (FunStack.pop(V),M,cmd2popper(C,0,[]))
  else 
  if iswhstart(t) then 
    if isff(e) then (FunStack.pop(V),M,cmdpopper(C)) else
    (FunStack.pop(V),M,cmd2popper(C,0,[]))
  else 
  if isseq(t) orelse isexp(t) orelse iscmd(t) orelse isite(t) then (V,M,FunStack.pop(C)) else
  if isset (t) then let val np=Array.update(memory,HashTable.lookup symbolTable (extractvar(f)),extract(e)); in (FunStack.pop(FunStack.pop(V)),M,FunStack.pop(C)) end else
    
  if isplus(t) then (FunStack.push(Int(extract(e) + extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else 
  if isminus(t) then (FunStack.push(Int(extract(f) - extract(e)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else 
  if isdiv(t) then (FunStack.push(Int(extract(f) div extract(e)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else 
  if ismod(t) then (FunStack.push(Int(extract(f) mod extract(e)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else 
  if istimes(t) then (FunStack.push(Int(extract(e) * extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isleq(t) then (FunStack.push(Convert(extract(e) >= extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isgeq(t) then (FunStack.push(Convert(extract(e) <= extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if iseq(t) then (FunStack.push(Convert(extract(e) = extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isneq(t) then (FunStack.push(Convert(extract(e) <> extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if islt(t) then (FunStack.push(Convert(extract(e) > extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isgt(t) then (FunStack.push(Convert(extract(e) < extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isand(t) then (FunStack.push(Convert(extractbool(e) andalso extractbool(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isor(t) then (FunStack.push(Convert(extractbool(e) orelse extractbool(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isnot(t) then (FunStack.push(Convert(not (extractbool(e))),FunStack.pop(V)),M,FunStack.pop(C)) else
  if isneg(t) then (FunStack.push(Int(0-extract(e)),FunStack.pop(V)),M,FunStack.pop(C)) else
  if issetstart(t) then (FunStack.push(t1,V),M,FunStack.pop(FunStack.pop(C))) else
  if isint(t) then  (FunStack.push(t,V),M,FunStack.pop(C)) else 
  if isstr(t) then (FunStack.push(Int(Array.sub(memory,HashTable.lookup symbolTable (extractvar(t)))),V),M,FunStack.pop(C)) else (V,M,FunStack.pop(C))
  end
end;

fun helpers(empty,n)=n
| helpers(DEC(DEC(x,y),z),n)=helpers(z,helpers(DEC(x,y),n))
| helpers(DEC(INT(x),z),n)=let val tp=HashTable.insert symbolTable (x,n) in
    helpers(z,n+1) end
| helpers(DEC(BOOL(x),z),n)=let val tp=HashTable.insert symbolTable (x,n) in
    helpers(z,n+1) end;

fun helper(PROG(x,y))=helper(y)
  |helper(BLK(x,y))=let 
  val npt=helpers(x,0);
in y end;

fun startexecution((V,M,C))=if FunStack.empty(C) then print("Operation succesful\n") else let val y=print("Configuration :  "^Vmc.toString(V,M,C)^"\n");
val (A,B,C)=Vmc.rules(V,M,C); in startexecution((A,B,C)) end;


fun execute(ast)=let
  val post=postfix(helper(ast),FunStack.create());
in startexecution((FunStack.create(),memory,post)) end;