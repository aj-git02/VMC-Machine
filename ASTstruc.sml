structure ASTstruc=
struct
    datatype aux= INTEGER | BOOLEAN;
    datatype AST=empty | PROG of string*AST | INT of string | BOOL of string | LT of AST*AST | LEQ of AST*AST | GT of AST*AST | GEQ of AST*AST | EQ of AST*AST | NEG of AST | NEQ of AST*AST | ITE of AST*AST*AST | WH of AST*AST | SEQ of AST*AST | AND of AST*AST | NOT of AST | OR of AST*AST | TT | FF | PLUS of AST*AST | MINUS of AST*AST | TIMES of AST*AST | DIV of AST*AST | MOD of AST*AST | SET of string*AST | DEC of AST*AST | BLK of AST*AST | stringnode of string| intnode of int | RD of string | WR of AST;
end;