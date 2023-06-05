signature Pi_TOKENS =
sig
type ('a,'b) token
type svalue
val SHOWDECIMAL:  'a * 'a -> (svalue,'a) token
val DECI: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val EOL:  'a * 'a -> (svalue,'a) token
val DISPLAY:  'a * 'a -> (svalue,'a) token
val NUMBA: (string) *  'a * 'a -> (svalue,'a) token
val SUB:  'a * 'a -> (svalue,'a) token
val ADD:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
end
signature Pi_LRVALS=
sig
structure Tokens : Pi_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
