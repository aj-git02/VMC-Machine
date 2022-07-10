exception ASTerror
fun generate (file) =
let val instream = TextIO.openIn file;
    val f : int -> string = fn n => if TextIO.endOfStream instream then "" else TextIO.inputN (instream,n);
    val printError : string * int * int -> unit = fn (str,line,col) => print (file^"["^Int.toString line^":"^Int.toString col^"] "^str^"\n");
    val (ast,rem) = ASTParser.parse(0,(ASTParser.makeLexer (f)),printError, ())
    handle ASTParser.ParseError => raise ASTerror;
    val _ = TextIO.closeIn instream;
in ast
end;