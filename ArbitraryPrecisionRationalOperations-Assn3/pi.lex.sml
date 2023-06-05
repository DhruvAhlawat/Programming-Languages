functor PiLexFun(structure Tokens: Pi_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

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




      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as (fileName:string)) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if(yytext = "decimal") then T.SHOWDECIMAL(!lin,!col) else continue())
      end
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lin:= !lin + 1; continue()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.EOL(!lin,!col)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.DIV(!lin,!col)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.LPAREN(!lin,!col)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RPAREN(!lin,!col)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.MUL(!lin,!col)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.ADD(!lin,!col)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.SUB(!lin,!col)))
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if(containsDecimal(yytext)) then T.DECI(yytext,!lin,!col) else T.NUMBA(yytext,!lin,!col))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print("unmatched character typed "^yytext^"\n"); continue())
      end
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"a"
              then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"a"
              then yyAction1(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"a"
              then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"a"
              then yyAction1(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ14(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ14(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ14(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"~"
              then if inp <= #"9"
                  then yyQ14(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ14(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ14(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ14(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"~"
              then if inp <= #"9"
                  then yyQ14(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ9(strm', lastMatch)
            else if inp < #"."
              then if inp = #"("
                  then yyQ4(strm', lastMatch)
                else if inp < #"("
                  then if inp = #"\v"
                      then yyQ1(strm', lastMatch)
                    else if inp < #"\v"
                      then if inp = #"\t"
                          then yyQ2(strm', lastMatch)
                        else if inp = #"\n"
                          then yyQ3(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = #" "
                      then yyQ2(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = #"+"
                  then yyQ7(strm', lastMatch)
                else if inp < #"+"
                  then if inp = #")"
                      then yyQ5(strm', lastMatch)
                      else yyQ6(strm', lastMatch)
                else if inp = #","
                  then yyQ1(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = #"A"
              then yyQ12(strm', lastMatch)
            else if inp < #"A"
              then if inp = #":"
                  then yyQ1(strm', lastMatch)
                else if inp < #":"
                  then if inp = #"/"
                      then yyQ10(strm', lastMatch)
                      else yyQ9(strm', lastMatch)
                else if inp = #";"
                  then yyQ11(strm', lastMatch)
                  else yyQ1(strm', lastMatch)
            else if inp = #"{"
              then yyQ1(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"["
                  then yyQ1(strm', lastMatch)
                else if inp < #"["
                  then yyQ12(strm', lastMatch)
                else if inp <= #"`"
                  then yyQ1(strm', lastMatch)
                  else yyQ12(strm', lastMatch)
            else if inp = #"~"
              then yyQ9(strm', lastMatch)
              else yyQ1(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
