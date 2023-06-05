(* compiler.sml *)
structure Pi =
struct
exception PiError;
fun compile (fileName) =
let 


val grab : int -> string = fn
n => if (n > size(fileName))
then ""
else substring(fileName,0,n-1);
val printError : string * int * int -> unit = fn
(msg,line,col) =>
print (fileName^"["^Int.toString line^":"
^Int.toString col^"] "^msg^"\n");
val (tree,rem) = PiParser.parse
(15,
(PiParser.makeLexer grab fileName),
printError,
fileName)
handle PiParser.ParseError => raise PiError;
(* Close the source program file *)
(* val _ = TextIO.closeIn inStream; *)
in tree
end
end;