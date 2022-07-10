## COL226 Assignment 4 (VMC Machine)

Akarsh Jain

2020CS10318

##### How to run the files

```bash
$ ml-lex lexer.lex
$ ml-yacc parser.yacc
$ sml while_ast.sml
- start("test.txt");
```

This will the perform the computation as encoded in the AST for the program written in *test.txt* file. A series of Configurations in the form of ( V ; M ; C) will be printed.

There are 4 test files, one *testerror* file (this must give a type error) for your convenience in test folder.

##### Helper Functions

The signatures of **Stack** and **VMC** are as defined in the pdf

```ocaml
fun start(file)
given the file this calls the execute fucntion with the postfix notation

fun execute(postfix)
given the postfix notation this calls startexecute

fun startexecute(V,M,C)
starts the execution with applying each rule one by one till end state is reached

fun postfix(AST,l)
converts AST into postfix form in a iterative way

fun postexp(AST,l)
converts a expression into postfix form in a iterative way

fun stringmaker(stacktype)
is used to convert a stacktype element to string

fun startexecution((V,M,C))
this loops over untill final configuration is not obtained

fun CommandSEQExtract(AST):
thorws away PROG and BLK nodes

fun SymbolTableInsert():
inserts variable declarations in symboltable where the adress of each variable in memory is stored

```

There are other functions between VMC signature and structure, these are used for pattern matching and helpful in executing rules.

##### "stacktype" datatype definition

This is the datatype of the elements stored in the stack.

```ocaml
datatype stacktype=Set | Ite | exp of stacktype FunStack.Stack | Int of int | Str of string | bexp of stacktype FunStack.Stack | cmd of stacktype FunStack.Stack | cmdseq of stacktype FunStack.Stack | And | Or | Not | plus | minus | times | Tt | Ff | Lt | Leq | Geq | Gt | Eq | Neq | Neg | Wh | Seq | DIv | Mod;
```

##### Design Decisions

Rules of operation of the VMC machine are as given in the pdf. Some redundant ones are removed (example 3rd to 7th rules are removed since they can be handled using rules 1,2,8) and some which facilitated operation depending on the stack-datatype are added (this are used for deconstruction mainly eg from exp(x) to x).

The stack is made using stacktype elements, this helps in getting easier lookahead and hence efficient implementation of rules. This has been approved by Prof. S Arun Kumar via email.

Another file in folder named old is also provided, which does not use recursive constructors, but cannot handle while loop.

##### Sample Test Case

```
program hello231 ::
    var A, B, D : int;
    var E , F : bool;
{
	A:=3;
    B:=0;
    while A>=0 do {
        A:=A+B;
        B:=B-1;
    }
    endwh ;
    read D;
    write 5+4;
    if tt then {
        if E || F then {
            E:=ff;
        }
        else {
            F:=ff;
        }
        endif;
    }
    else{
        E:=E>F;
    }
    endif;
}
```

```
Postfix notation

cmd ( A exp ( 3  ) SET  ) cmd ( B exp ( 0  ) SET  ) cmd ( bexp ( A 0 >=  ) cmdseq ( cmd ( A exp ( A B +  ) SET  ) cmd ( B exp ( B 1 -  ) SET  ) Seq  ) while  ) cmd (  ) cmd (  ) cmd ( bexp ( 1  ) cmdseq ( cmd ( bexp ( E F OR  ) cmdseq ( cmd ( E exp ( 0  ) SET  )  ) cmdseq ( cmd ( F exp ( 0  ) SET  )  ) IfThenElse  )  ) cmdseq ( cmd ( E exp ( E F >  ) SET  )  ) IfThenElse  ) Seq Seq Seq Seq Seq
```

```
Machine output

val it = (-,[|~3,~4,1234561,1234561,1234561,1234561,1234561,1234561,
    1234561|],-)
```

##### Acknowledgements

**Everything has been done by me** 

