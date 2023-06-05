structure T = Tokens 

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token

type lexarg = string
type arg = lexarg
val lin = ref 1;
val col = ref 0; (*lin and col are not really used, hence not updated*)
val eof = fn fileName => T.EOL (!lin,!col);

fun containsDecimal(x:string) =
    let
        val s = explode x;
        fun cd([]) = false
        |   cd(a::b) = if(a = #".") then true else cd(b);
    in
        cd(s)
    end;


%%
%full
%header (functor PiLexFun(structure Tokens: Pi_TOKENS));
%arg (fileName:string);

alpha = [A-Za-z];
num = [0-9.~()];
deci = [0-9.];
alphanum = [0-9a-zA-Z];
spaces = [\ \t];
%%
{spaces} => (continue());
"[" => (T.LPAREN(!lin,!col));
"]" => (T.RPAREN(!lin,!col));
{alpha}+ => (if(yytext = "decimal") then T.SHOWDECIMAL(!lin,!col) else continue());
"\n"  => (lin:= !lin + 1; continue());
";" => (T.EOL(!lin,!col));  
"/" => (T.DIV(!lin,!col));
"*" => (T.MUL(!lin,!col));
"+" => (T.ADD(!lin,!col));
"-" => (T.SUB(!lin,!col));
{num}+ => (if(containsDecimal(yytext)) then T.DECI(yytext,!lin,!col) else T.NUMBA(yytext,!lin,!col));
. => (print("unmatched character typed "^yytext^"\n"); continue());