
datatype lexresult = ID of string | LPAREN | RPAREN | DIV | MUL | ADD | SUB 
| NUMBA of string | DISPLAY | EOL

val eof = fn () => EOF
%%
%structure CalcLex
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
"\n"  => lex()
"/" => (DIV);
"(" => (LPAREN);
")" => RPAREN;
"*" => MUL;
"+" => ADD;
"-" => SUB;
{alphabets}+ => if(yytext = "display") then DISPLAY else NUMBA(yytext;
. => (print("unmatched character typed "^yytext); lex());



