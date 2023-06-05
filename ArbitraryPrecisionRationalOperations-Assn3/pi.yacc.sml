functor PiLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Pi_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\007\000\008\000\006\000\012\000\005\000\000\000\
\\001\000\002\000\007\000\008\000\006\000\012\000\005\000\013\000\004\000\000\000\
\\001\000\003\000\022\000\004\000\022\000\005\000\022\000\006\000\022\000\
\\007\000\022\000\010\000\022\000\011\000\022\000\000\000\
\\001\000\003\000\023\000\004\000\023\000\005\000\023\000\006\000\023\000\
\\007\000\023\000\010\000\023\000\011\000\023\000\000\000\
\\001\000\003\000\024\000\004\000\011\000\005\000\010\000\006\000\024\000\
\\007\000\024\000\010\000\024\000\011\000\024\000\000\000\
\\001\000\003\000\025\000\004\000\025\000\005\000\025\000\006\000\025\000\
\\007\000\025\000\010\000\025\000\011\000\025\000\000\000\
\\001\000\003\000\026\000\004\000\026\000\005\000\026\000\006\000\026\000\
\\007\000\026\000\010\000\026\000\011\000\026\000\000\000\
\\001\000\003\000\027\000\004\000\011\000\005\000\010\000\006\000\027\000\
\\007\000\027\000\010\000\027\000\011\000\027\000\000\000\
\\001\000\003\000\028\000\004\000\028\000\005\000\028\000\006\000\028\000\
\\007\000\028\000\010\000\028\000\011\000\028\000\000\000\
\\001\000\003\000\018\000\004\000\011\000\005\000\010\000\006\000\009\000\
\\007\000\008\000\000\000\
\\001\000\004\000\011\000\005\000\010\000\006\000\009\000\007\000\008\000\
\\010\000\020\000\011\000\020\000\000\000\
\\001\000\004\000\011\000\005\000\010\000\006\000\009\000\007\000\008\000\
\\010\000\021\000\011\000\021\000\000\000\
\\001\000\010\000\000\000\011\000\000\000\000\000\
\"
val actionRowNumbers =
"\001\000\010\000\000\000\003\000\
\\002\000\000\000\000\000\000\000\
\\000\000\000\000\011\000\009\000\
\\007\000\004\000\005\000\006\000\
\\008\000\012\000"
val gotoT =
"\
\\001\000\001\000\002\000\017\000\000\000\
\\000\000\
\\001\000\010\000\000\000\
\\000\000\
\\000\000\
\\001\000\011\000\000\000\
\\001\000\012\000\000\000\
\\001\000\013\000\000\000\
\\001\000\014\000\000\000\
\\001\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 18
val numrules = 9
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | DECI of unit ->  (string) | NUMBA of unit ->  (string)
 | ID of unit ->  (string) | START of unit ->  (Rational.rational)
 | EXP of unit ->  (Rational.rational)
end
type svalue = MlyValue.svalue
type result = Rational.rational
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 10) => true | (T 9) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "LPAREN"
  | (T 2) => "RPAREN"
  | (T 3) => "DIV"
  | (T 4) => "MUL"
  | (T 5) => "ADD"
  | (T 6) => "SUB"
  | (T 7) => "NUMBA"
  | (T 8) => "DISPLAY"
  | (T 9) => "EOL"
  | (T 10) => "EOF"
  | (T 11) => "DECI"
  | (T 12) => "SHOWDECIMAL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 12) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)
) => let val  result = MlyValue.START (fn _ => let val  (EXP as EXP1)
 = EXP1 ()
 in (
print (Rational.showRat(EXP)); 
                    print "\n";
                    EXP
)
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
SHOWDECIMAL1left, _)) :: rest671)) => let val  result = MlyValue.START
 (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (print(Rational.toDecimal(EXP)); print "\n"; EXP)
end)
 in ( LrTable.NT 1, ( result, SHOWDECIMAL1left, EXP1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.NUMBA NUMBA1, NUMBA1left, NUMBA1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (NUMBA
 as NUMBA1) = NUMBA1 ()
 in (
valOf(Rational.make_rat(BigInt.getBigInt(NUMBA), BigInt.getBigInt("1")))
)
end)
 in ( LrTable.NT 0, ( result, NUMBA1left, NUMBA1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.DECI DECI1, DECI1left, DECI1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (DECI
 as DECI1) = DECI1 ()
 in (Rational.fromDecimal(DECI))
end)
 in ( LrTable.NT 0, ( result, DECI1left, DECI1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (Rational.add(EXP1,EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (Rational.multiply(EXP1,EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (valOf(Rational.divide(EXP1,EXP2)))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (Rational.subtract(EXP1,EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Pi_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMBA (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.NUMBA (fn () => i),p1,p2))
fun DISPLAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun EOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun DECI (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.DECI (fn () => i),p1,p2))
fun SHOWDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
end
end
