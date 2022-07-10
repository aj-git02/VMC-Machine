exception syntaxErr
structure ASTLrVals = ASTLrValsFun(structure Token = LrParser.Token)
structure ASTLex = ASTLexFun(structure Tokens = ASTLrVals.Tokens);
structure ASTParser =Join(structure LrParser = LrParser
    structure ParserData = ASTLrVals.ParserData
    structure Lex = ASTLex)