exception undeclaredVariable;
val symbolTable : (string, int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op =) (100, undeclaredVariable)

open ASTstruc;

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

(* > some prob *)
structure FunStack :> STACK =
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

datatype stacktype=Set | Ite | exp of stacktype FunStack.Stack | Int of int | Str of string | bexp of stacktype FunStack.Stack | cmd of stacktype FunStack.Stack | cmdseq of stacktype FunStack.Stack | And | Or | Not | plus | minus | times | Tt | Ff | Lt | Leq | Geq | Gt | Eq | Neq | Neg | Wh | Seq | DIv | Mod;

fun stringmaker(Set)="SET "
  |stringmaker(Ite)="IfThenElse "
  |stringmaker(exp(x))="exp ( "^FunStack.toString stringmaker (x)^" ) "
  |stringmaker(cmd(x))="cmd ( "^FunStack.toString stringmaker (x)^" ) "
  |stringmaker(cmdseq(x))="cmdseq ( "^FunStack.toString stringmaker (x)^" ) "
  |stringmaker(Str(x))=x^" "
  |stringmaker(Int(x))=Int.toString(x)^" " (*this rule is doubtful*)
  |stringmaker(bexp(x))="bexp ( "^FunStack.toString stringmaker (x)^" ) "
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

fun postexp(PLUS(x,y),l)=postexp(x,postexp(y,FunStack.push(plus,l)))
  | postexp(MINUS(x,y),l)=postexp(x,postexp(y,FunStack.push(minus,l)))
  | postexp(DIV(x,y),l)=postexp(x,postexp(y,FunStack.push(DIv,l)))
  | postexp(MOD(x,y),l)=postexp(x,postexp(y,FunStack.push(Mod,l)))
  | postexp(TIMES(x,y),l)=postexp(x,postexp(y,FunStack.push(times,l)))
  | postexp(AND(x,y),l)=postexp(x,postexp(y,FunStack.push(And,l)))
  | postexp(OR(x,y),l)=postexp(x,postexp(y,FunStack.push(Or,l)))
  | postexp(NOT(x),l)=postexp(x,FunStack.push(Not,l))
  | postexp(NEG(x),l)=postexp(x,FunStack.push(Neg,l))
  | postexp(empty,l)=l
  | postexp(LT(x,y),l)=postexp(x,postexp(y,FunStack.push(Lt,l)))
  | postexp(GT(x,y),l)=postexp(x,postexp(y,FunStack.push(Gt,l)))
  | postexp(EQ(x,y),l)=postexp(x,postexp(y,FunStack.push(Eq,l)))
  | postexp(GEQ(x,y),l)=postexp(x,postexp(y,FunStack.push(Geq,l)))
  | postexp(LEQ(x,y),l)=postexp(x,postexp(y,FunStack.push(Leq,l)))
  | postexp(NEQ(x,y),l)=postexp(x,postexp(y,FunStack.push(Neq,l)))
  | postexp(intnode(x),l)=FunStack.push(Int(x),l)
  | postexp(FF,s)=FunStack.push(Int(0),s)
  | postexp(TT,s)=FunStack.push(Int(1),s)
  | postexp(stringnode(x),s)=FunStack.push(Str(x),s);


fun postfix(SEQ(x,SEQ(y:AST,z:AST)),s)= FunStack.push(cmd(postfix(x,FunStack.create())),postfix(SEQ(y,z),FunStack.push(Seq,s)))
  | postfix(SEQ(x,y),s)= FunStack.push(cmd(postfix(x,FunStack.create())),s)
  |postfix(WH(x,SEQ(y,z)),s)=FunStack.push(bexp(postexp(x,FunStack.create())),FunStack.push(cmdseq(postfix(SEQ(y,z),FunStack.create())),FunStack.push(Wh,s)))
  | postfix(ITE(x,y,z),s)=FunStack.push(bexp(postexp(x,FunStack.create())),FunStack.push(cmdseq(postfix(y,FunStack.create())),FunStack.push(cmdseq(postfix(z,FunStack.create())),FunStack.push(Ite,s))))
  | postfix(SET(x,y),s)=FunStack.push(Str(x),FunStack.push(exp(postexp(y,FunStack.create())),FunStack.push(Set,s)))
  | postfix(empty,s)=s
  | postfix(RD(x),s)=s
  | postfix(WR(x),s)=s;

(*mempry definition and its helper functions*)
val maxMemSize=10;
val memory=Array.array(maxMemSize,1234561);
(*to convert the memory to a string*)
fun memstring(mem,a,s)=if a=maxMemSize then s else memstring(mem,a+1,s^", "^Int.toString(Array.sub(mem,a)));
fun memhelper(x)=Array.update(memory,x,0);
(*to reset the memory*)
fun resetmem(mem)=Array.app memhelper mem;


signature VMC=
sig
  val toString: stacktype FunStack.Stack* int array *stacktype FunStack.Stack -> string
  val rules : stacktype FunStack.Stack* int array *stacktype FunStack.Stack -> stacktype FunStack.Stack* int array *stacktype FunStack.Stack
end;

fun isint(Int(x))=true
  |isint(x)=false;
fun iscmd(cmd(x))=true
  |iscmd(x)=false;
fun isexp(exp(x))=true
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
fun isbexp(bexp(x))=true
  |isbexp(x)=false;
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
fun iscmd(cmd(x))=true
| iscmd(x)=false;
fun isgeq(Geq)=true
| isgeq(x)=false;
fun isleq(Leq)=true
  | isleq(x)=false;
fun iscmdseq(cmdseq(x))=true
  | iscmdseq(x)=false;
fun extract(Int(x))=x;
fun extractvar(Str(x))=x;
fun extractbexp(bexp(x))=x;
fun extractbool(Int(1))=true
  |extractbool(Int(0))=false;
fun Convert(true)=Int(1)
  |Convert(false)=Int(0);
fun extractexp(exp(x))=x;
fun extractcmd(cmd(x))=x;
fun extractcmdseq(cmdseq(x))=x;
fun stacker(s,l)=if FunStack.empty(s) then l else
let val a=FunStack.top(s) in
  stacker(FunStack.pop(s),FunStack.push(a,l)) end;

structure Vmc: VMC=
struct
  fun toString(V,M,C)="( "^FunStack.toString stringmaker (V)^" ; "^memstring(memory,0,"")^" ; "^FunStack.toString stringmaker (C)^" )";
  fun rules(V,M,C)= 
  let 
    val a=if FunStack.depth(C)>=1 then FunStack.top(C) else Tt;
    val b=if FunStack.depth(C)>=2 then FunStack.nth(C,1) else Tt;
    val c=if FunStack.depth(C)>=3 then FunStack.nth(C,2) else Tt;
    val d=if FunStack.depth(C)>=4 then FunStack.nth(C,3) else Tt;
    val e=if FunStack.depth(V)>=1 then FunStack.top(V) else Tt;
    val f=if FunStack.depth(V)>=2 then FunStack.nth(V,1) else Tt;
    val g=if FunStack.depth(V)>=3 then FunStack.nth(V,2) else Tt;
  in
  if iswh(a) then
    if isff(e) then (FunStack.pop(FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else (FunStack.pop(FunStack.pop(FunStack.pop(V))),M,stacker(stacker(extractcmdseq(f),FunStack.create()),FunStack.push(g,FunStack.push(f,C)))) else
  if iswh(c) andalso (isbexp(a) andalso iscmdseq(b)) then
    (FunStack.push(b,FunStack.push(a,V)),M,stacker(stacker(extractbexp(a),FunStack.create()),FunStack.pop(FunStack.pop(C)))) else
  
  if isite(d) andalso (isbexp(a) orelse isint(a)) then
    if isbexp(a) then (V,M,stacker(stacker(extractbexp(a),FunStack.create()),FunStack.pop(C))) else
     (FunStack.push(a,V),M,FunStack.pop(C)) else
  if isite(c) then 
    if isff(e) then (FunStack.pop(V),M,stacker(stacker(extractcmdseq(b),FunStack.create()),FunStack.pop(FunStack.pop(FunStack.pop(C))))) else 
    (FunStack.pop(V),M,stacker(stacker(extractcmdseq(a),FunStack.create()),FunStack.pop(FunStack.pop(FunStack.pop(C)))))
  else 
  if isseq(c) andalso not (iscmd(a) andalso iscmd(b)) then (V,M,FunStack.push(a,FunStack.push(b,FunStack.pop(FunStack.pop(FunStack.pop(C)))))) else
  if isseq(c) then (V,M,stacker(stacker(extractcmd(a),FunStack.create()),stacker(stacker(extractcmd(b),FunStack.create()),FunStack.pop(FunStack.pop(FunStack.pop(C)))))) else
  if iscmd(a) then (V,M,stacker(stacker(extractcmd(a),FunStack.create()),FunStack.pop(C))) else
  if isset(c) andalso isexp(b) then (FunStack.push(a,V),M,stacker(stacker(extractexp(b),FunStack.create()),FunStack.pop(FunStack.pop(C)))) else 
  if isset (a) then let val np=Array.update(memory,HashTable.lookup symbolTable (extractvar(f)),extract(e)); in (FunStack.pop(FunStack.pop(V)),M,FunStack.pop(C)) end else
    
  if isplus(a) then (FunStack.push(Int(extract(e) + extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else 
  if isminus(a) then (FunStack.push(Int(extract(f) - extract(e)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else 
  if isdiv(a) then
    if extract(e)=0 then raise FunStack.Error("Division by zero") else (FunStack.push(Int(extract(f) div extract(e)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else 
  if ismod(a) then 
    if extract(e)=0 then raise FunStack.Error("Division by zero") else (FunStack.push(Int(extract(f) mod extract(e)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else 
  if istimes(a) then (FunStack.push(Int(extract(e) * extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isleq(a) then (FunStack.push(Convert(extract(e) >= extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isgeq(a) then (FunStack.push(Convert(extract(e) <= extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if iseq(a) then (FunStack.push(Convert(extract(e) = extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isneq(a) then (FunStack.push(Convert(extract(e) <> extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if islt(a) then (FunStack.push(Convert(extract(e) > extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isgt(a) then (FunStack.push(Convert(extract(e) < extract(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isand(a) then (FunStack.push(Convert(extractbool(e) andalso extractbool(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isor(a) then (FunStack.push(Convert(extractbool(e) orelse extractbool(f)),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) else
  if isnot(a) then (FunStack.push(Convert(not (extractbool(e))),FunStack.pop(V)),M,FunStack.pop(C)) else
  if isneg(a) then (FunStack.push(Int(0-extract(e)),FunStack.pop(V)),M,FunStack.pop(C)) else
    
  if isint(a) then  (FunStack.push(a,V),M,FunStack.pop(C)) else 
  if isstr(a) then
    if Array.sub(memory,HashTable.lookup symbolTable (extractvar(a)))=1234561 then raise FunStack.Error("Uninitialised vaiable") else
   (FunStack.push(Int(Array.sub(memory,HashTable.lookup symbolTable (extractvar(a)))),V),M,FunStack.pop(C)) else (V,M,FunStack.pop(C))
  end
end;

fun SymbolTableInsert(empty,n)=n
| SymbolTableInsert(DEC(DEC(x,y),z),n)=SymbolTableInsert(z,SymbolTableInsert(DEC(x,y),n))
| SymbolTableInsert(DEC(INT(x),z),n)=let val tp=HashTable.insert symbolTable (x,n) in
    SymbolTableInsert(z,n+1) end
| SymbolTableInsert(DEC(BOOL(x),z),n)=let val tp=HashTable.insert symbolTable (x,n) in
    SymbolTableInsert(z,n+1) end;

fun CommandSEQExtract(PROG(x,y))=CommandSEQExtract(y)
  |CommandSEQExtract(BLK(x,y))=let 
  val npt=SymbolTableInsert(x,0);
in y end;

fun startexecute(V,M,C)=if FunStack.empty(C) then (V,M,C) else let 
val (A,B,C)=Vmc.rules(V,M,C); in startexecute(A,B,C) end;

fun refresh()=let val y= HashTable.clear symbolTable;  in () end;

fun execute(post)=startexecute(FunStack.create(),memory,post)

fun start(x)=let
  val ast=generate(x);
  val post=postfix(CommandSEQExtract(ast),FunStack.create());
in execute(post) end;
