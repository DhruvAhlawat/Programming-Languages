signature RATIONAL =
sig
    type rational
    type bigint
    exception rat_error
    val make_rat: bigint * bigint -> rational option
    val rat: bigint -> rational option
    val neg: rational -> rational
    val inverse : rational -> rational option
    val reci: bigint -> rational option
    val multiply : rational * rational -> rational (* multiplication *)
    val divide : rational * rational -> rational option (* division *)
    val equal : rational * rational -> bool (* equality *)
    val add : rational * rational -> rational (* addition *)
    val showRat : rational -> string
    val subtract : rational * rational -> rational (* subtraction *)
    val less : rational * rational -> bool
    val fromDecimal : string -> rational
    val toDecimal : rational -> string
    val showDecimal : rational -> string



    val getFractionalParts : bigint*bigint*bigint list -> char list*int  
  

    
  
end;

signature BIGINT = 
    sig
    exception unknownCharacter
    type bigint = char list 
    val add : bigint*bigint -> bigint
    val sub : bigint*bigint -> bigint
    val getBigInt : string -> bigint
    val mult : bigint*bigint -> bigint
    exception bigintError
    val leq : bigint*bigint -> int   (*where if int = 0 then greater than, if int = 1 then less than, if int = 2 then equal*)
    val divide : bigint*bigint -> bigint*bigint (*quotient and remainder*)
    val GCD : bigint*bigint -> bigint
    val LCM : bigint*bigint -> bigint    
    val stripZeros : bigint -> bigint
end;

structure BigInt : BIGINT =
struct
exception unknownCharacter
(*Helper functions*)
    fun stripZeros([]) = []
    |   stripZeros(b::y) = if (length(y) = 0) then [b] 
    else if #"0"=b then stripZeros(y)
    else if(#"~" = b) then #"~"::stripZeros(y)
    else b::y;

    fun reverse(x,z) = if null(x) then z
    else reverse(tl(x),hd(x)::z); 
    

    val zeroVal : int = Char.ord(#"0"); 
    fun rev x = reverse(x,[]);   

    fun charToint(x) = (Char.ord(x) - zeroVal);

    fun getHeadTail(x,0) = ([],x)
    |   getHeadTail(x,n) = (*n should not be greater than length of a*)
    let
        fun ght(a,b,0) = (rev b,a)
        |   ght(a::a1,b,c) = ght(a1,a::b,c-1) 
    in
        ght(x,[],n)
    end;    

    (*MultiplyDigit is required in both multiply and divide hence it is kept separate*)
    fun multiplyDigit(x,0) = [#"0"]
        |   multiplyDigit(x,1) = rev x 
        |   multiplyDigit(x,k) = 
            let 
                val revX = (x); (*rev and stripzeros are both already applied on x, this much is assumed*)
                fun MultHelp([],b,carry) = if (carry = 0 ) then [] else [(Char.chr(carry + zeroVal))]
                |   MultHelp(a::y,b,carry) = 
                    let 
                        val dig = Char.ord(a) - zeroVal;
                        val num = (dig*b)  + carry;
                        val ones = (num mod 10);
                        val tens = ((num - ones) div 10);
                    in
                        (Char.chr(ones + zeroVal)) :: (MultHelp(y,b,tens)) (*appending character to head of the list to maintain time complexity*)
                    end
            in
                rev (MultHelp(revX,k,0))
            end;
(*<-------------End of helper functions----------------->*)

    type bigint = char list
    exception bigintError
    
    (*for leq, if int = 0 then greater than, if int = 1 then less than, if int = 2 then equal*)
fun leq([],[]) = 2
    |   leq([],y) = 1
    |   leq(x, []) = 0 (*leq*)
    |   leq(x,y) = let
            val x = implode (stripZeros( x));
            val y = implode (stripZeros( y)); (*removes the leading 0's while comparing*)
        in
            if(String.size(x) < String.size(y)) then
                1
            else if (String.size(x) > String.size(y))  then    
                0
            else
                let 
                    val compa = String.compare(x,y);
                in
                    if(compa = LESS) then 1 else if (compa = EQUAL) then 2 else 0 (*if the sizes are same then the inbuilt compare function works well*)
                end
        end;

    
    fun add([],[]) = [#"0"]
    |   add([#"~",#"0"],[#"~",#"0"]) = [#"0"]    (*there is no negative 0*)
    |   add(#"~"::a, #"~"::b) =
    let
        val az = stripZeros(a);
        val bz = stripZeros(b);
    in    
        if(az = [#"0"] andalso bz = [#"0"]) then [#"0"] else  #"~"::add(a,b)
    end
    |   add(#"~"::a,y) = sub(y,a) (*subtracts a from y if a is x negative bigint number*)
    |   add(x,#"~"::b) = sub(x,b) (*subtracts b from x if a is y negative bigint number*)
    |   add(x,y) =  
        let
        val revXList = rev (stripZeros(( x)));
        val revYList = rev (stripZeros(( y))); (*reverses both of the input strings, since integer addition is done from the right*)
        fun addHelper([],[],0) = "" (*addHelper basically takes 2 list inputs and one int input that is supposed to be the carry*)
        |   addHelper([],[],a) = String.str(Char.chr(a + zeroVal))
        |   addHelper([],a::y,car) = if(car = 0) then (implode (rev (a::y))) 
            else
            let 
                val curDig = Char.ord(a) - zeroVal + car; 
                val newCarry = if (curDig > 9) then 1 else 0; (*stores the value of the new carry to be passed*)
                val curDig = if (newCarry >= 1) then (Char.ord(a) - zeroVal + car - 10) 
                    else (Char.ord(a) - zeroVal + car); (*curDig is the value of the current digit that would come after the addition process*)
                (* now the statements and variables have been defined *)
            in
                ((addHelper([],y,newCarry)) ^ String.str(Char.chr(curDig + zeroVal)))
            end
        |   addHelper(a::y, [], car) = addHelper([], a::y, car) (*same as the above case*)
        |   addHelper(a::y,b::z, car) = (*main general case*)
            let
                val curDig = Char.ord(a) + Char.ord(b) - (2*zeroVal) + car;
                val newCarry = if (curDig > 9) then 1 else 0;
                val curDig = if (newCarry >= 1) then (Char.ord(a) + Char.ord(b) - (2*zeroVal) + car - 10) (*if digit is greater than 10 than reduce it by 10 and increase carry*)
                    else (Char.ord(a) + Char.ord(b) - (2*zeroVal) + car);
            in
                ((addHelper(y,z,newCarry)) ^ (String.str(Char.chr(curDig + zeroVal)))) (*concatenates the current digit to the output of the later calls of the funciton*)
            end;
        in
            explode (addHelper(revXList,revYList,0))
        end
    (*add is used to add 2 bigints represented in the format of char lists*)
    and sub(#"~"::a, #"~"::b) = sub(b,a) 
    |   sub(a,#"~"::b) = add(a,b) (*subtracting a negative number is the same as adding*)
    |   sub(#"~"::a, b) = #"~"::add(a,b)
    |   sub(x,y) = 
    let 
        val comp = leq(x,y);
    in
        if(comp = 2) then [#"0"] else if (comp = 1) then #"~"::sub(y,x) else 
        let 
            val revX = rev(stripZeros(x)); 
            val revY = rev(stripZeros(y)); (*the strip zeros is just a precaution, and its fine to put it everywhere
            since it will be an O(1) command if there are no leading zeroes*)

            fun SubtractHelper([],[],carry) = ""
            |   SubtractHelper(a::y,[],carry) = if(carry = 0) then (implode (rev (a::y)))
            else
                (*gotta subtract this carry value from the current digit*)
                let 
                    val curDig = Char.ord(a) - zeroVal;
                    val newCarry = if(carry <= curDig) then 0 else 1;
                    val curDig = if (newCarry = 0) then (curDig - carry) else (10 + curDig - carry);
                in
                    (SubtractHelper(y,[],newCarry)) ^ (String.str(Char.chr(curDig + zeroVal)))
                end
            |   SubtractHelper(a::y,b::z,carry) = 
                let 
                    val curDig = Char.ord(a) - Char.ord(b) - carry;
                    val newCarry = if(curDig >= 0) then 0 else 1;
                    val curDig = if (newCarry = 0) then (curDig) else (10 + curDig);
                in
                    (SubtractHelper(y,z,newCarry)) ^ (String.str(Char.chr(curDig + zeroVal)))
                end
            |   SubtractHelper([],_,_) = "Error";
            (* val possibleValue = implode(stripZeros (explode (SubtractHelper(revX,revY,0)))); *)
        in
        (stripZeros (explode (SubtractHelper(revX,revY,0)))) (*removes the leading 0's*)
        end
    end;
    (*sub ends*)

fun getBigInt("") = []
    |   getBigInt(a) = 
    let
        val ans = (explode a);
        fun isNumeric([]) = true
        |   isNumeric(x::y) = if(Char.ord(x) - Char.ord(#"0") >=0 andalso Char.ord(x) - Char.ord(#"0") <= 9) then isNumeric(y) else false;
    in 
        if(isNumeric ans orelse (hd(ans) = #"~" andalso isNumeric(tl(ans)))) then ans else raise unknownCharacter
    end
    

fun mult(([] | [#"0"] | [#"~",#"0"]),a) = [#"0"]
    |   mult(a,([] | [#"0"] | [#"~",#"0"])) = [#"0"](*anything multiplied by empty or 0 is 0*)
    |   mult(#"~"::a, #"~"::b) = mult(a,b) (*negatives cancel out*)
    |   mult((#"~"::a,b) | (a,#"~"::b)) = #"~"::mult(a,b)
    |   mult(a,b) =
    let
        val ra = rev(stripZeros(a));
        val rb = rev(stripZeros(b));
            (*important fact that x is a reversed representation*)
            fun multAllDigs(x,[]) = [#"0"]
            |   multAllDigs(x,c::d) = add(multiplyDigit(x,charToint(c)), multAllDigs(#"0"::x,d)) 
        in
            multAllDigs(ra,rb)
    end;


fun divide(a:bigint,([]| [#"0"]):bigint) = raise bigintError
    |   divide(([] | [#"0"]),a) = ([#"0"]:bigint,[#"0"]:bigint)
    |   divide(#"~"::a, #"~"::b) = divide(a,b)
    |   divide(a,#"~"::b) = divide(#"~"::a,b)
    |   divide(#"~"::a, b) = let
        val (q,rem) =  divide(a,b);
        in
            (#"~"::q,rem)
        end 
    |   divide(a,b) = 
        let 
            val a = stripZeros(a); val b = stripZeros(b);
            val alen = length(a);
            val blen = length(b); (*obviously if length of b is greater than a then we need to put all of a in b*)
            fun getQval(x,y,10) = 9(*returns highest integer digit k such that y*k <= x *) 
            (*y needs to be reversed*)
            |   getQval([],_,_) = ~1
            |   getQval(x,y,n) = 
                let 
                    val multVal = multiplyDigit(y,n); (*if x is not reversed, then we need to reverse it for using this function*)
                    val check = leq(multVal,x);
                in
                    if(check = 0) then (n-1)
                    else if(check = 1) then getQval(x,y,n+1)
                    else (n)
                end;

            fun divHelp([]:char list,y,rem,q) = (stripZeros(rev q),stripZeros rem)
            |   divHelp(x::xtail,y,rem,q) = 
            let
                val rem = rem@[x]; (*the new value of the remainder*)
                val comp = leq(rem,y);
                val revY = rev y;
            in
                if(comp = 1) then divHelp(xtail,y,rem,#"0"::q) 
                else if(comp = 2) then divHelp(xtail,y,[#"0"],#"1"::q)
                else
                let
                    val curQ = getQval(rem,revY,0);
                    val charCurQ = Char.chr(curQ + zeroVal);
                    val newRem = sub(rem,multiplyDigit(revY,curQ))                
                in
                    divHelp(xtail,y,newRem,charCurQ::q)
                end
            end
        in
            if(b = [#"0"]) then raise bigintError 
            else divHelp(a,b,[],[])
        end;

fun GCD(a:bigint,[#"0"]) = a
    |   GCD(#"~"::a, #"~"::b) = GCD(a,b)
    |   GCD(#"~"::a,b) = #"~"::GCD(a,b)
    |   GCD(a,b) = 
        let
            val (q,rem) = divide(a,b);
        in
            GCD(b,rem)
        end;

fun LCM(a:bigint,[#"0"]) = [#"0"]
    |   LCM([#"0"],a) = [#"0"]
    |   LCM(a,b) = #1 (divide(mult(a,b), GCD(a,b)));

end;

functor makeRational (bi : BIGINT): RATIONAL =
    struct
    type bigint = bi.bigint;
    exception rat_error;
    type rational = bigint*bigint;

    fun ReduceToFractionalNormalForm((#"~"::a,#"~"::b)):rational = 
        ReduceToFractionalNormalForm((a,b))
        |   ReduceToFractionalNormalForm((a,#"~"::b)) = ReduceToFractionalNormalForm((#"~"::a,b))
        |   ReduceToFractionalNormalForm((#"~"::a,b)) = let val (num,den) = ReduceToFractionalNormalForm((a,b)) in (#"~"::num,den) end
        |   ReduceToFractionalNormalForm((a,b)) = 
        let
            val curGcd:bigint = BigInt.GCD(a:bigint,b:bigint);
        in
            if(curGcd = ([#"1"]:bigint)) then (a,b) 
            else (#1 (BigInt.divide(a,curGcd)),#1 (BigInt.divide(b,curGcd)))
    end;

    fun make_rat(a:bigint,b:bigint): rational option = 
        let
            val rb = BigInt.stripZeros(b);
        in 
            if(rb = [#"0"] orelse rb = [#"~",#"0"]) then NONE else SOME(ReduceToFractionalNormalForm((a,b):rational))
    end;

    fun showRat((a,b):rational) = 
        let
            val (ra,rb) = ReduceToFractionalNormalForm((a,b))
            val ListResult = (ra@[#"/"])@(rb);
        in
            implode ListResult
    end; 

    fun rat(a:bigint) : rational option = 
        let
            val c = bi.stripZeros(a);
        in
            if(c = [#"0"]) then NONE else SOME((c,[#"1"]):rational)
    end;

    fun neg(a:rational) =
        let
            val (x,y) =  ReduceToFractionalNormalForm(a);
        in
            if(hd(x) = #"~") then (tl(x),y)
            else    (#"~"::x,y)
    end;

    fun reci(a:bigint) = make_rat([#"1"],a);

    fun inverse((x,y):rational) = make_rat(y,x); 

    
    fun divide((w,x):rational, (y,z):rational) =
        let
            val sy = bi.stripZeros(y);
        in
            if(sy = [#"0"] orelse sy = [#"~",#"0"]) then NONE 
            else 
            make_rat(bi.mult(w,z), bi.mult(x,sy))
        end; 

    fun multiply((w,x):rational, (y,z):rational) =
        let
            val SOME(kk) = make_rat(bi.mult(w,y),bi.mult(x,z));
        in
            kk
        end;

    (*Assuming that rationals are always stored in their fractional normal form*)
    fun equal((a,b):rational, (c,d):rational) = if(a = b andalso d = b) then true else false; 

    fun add((a,b):rational, (c,d):rational) = 
    let
        val gcd= bi.GCD(b,d);
        val (fac1,_) =  bi.divide(d,gcd);
        val (fac2,_) =  bi.divide(b,gcd);
        val (lcm,_) = bi.divide(bi.mult(b,d),gcd)
        val finalAns = ReduceToFractionalNormalForm(bi.add(bi.mult(a,fac1), bi.mult(c,fac2)), lcm)
        in
        finalAns
    end; 

    fun subtract(a,b) = 
        let
            val (a1,a2) = ReduceToFractionalNormalForm(a);
            val (b1,b2) = ReduceToFractionalNormalForm(b);
            fun sub((#"~"::x,xb),(#"~"::y,yb)) = add((y,yb),(#"~"::x,xb))
            |   sub((#"~"::x,xb), (y,yb)) = add((#"~"::y,yb),(#"~"::x,xb))
            |   sub((x,xb),(#"~"::y,yb)) = add((y,yb),(x,xb))
            |   sub((x,xb),(y,yb)) = add((x,xb),(#"~"::y,yb))
        in
            sub((a1,a2),(b1,b2))
    end;

    fun less((#"~"::a,a1):rational, (#"~"::b,b1):rational) = less((b,b1),(a,a1))
        |   less((a,a1):rational,(#"~"::b,b1):rational) = false 
        |   less((#"~"::a,a1):rational,(b,b1):rational) = 
            if (b = [#"0"] andalso a = [#"0"]) then false 
            else true
        |   less((a,b),(c,d)) =  
            let
                val gcd= bi.GCD(b,d)
                val (fac1,_) =  bi.divide(d,gcd)
                val (fac2,_) =  bi.divide(b,gcd)
                val check = bi.leq(bi.mult(a,fac1),bi.mult(c,fac2))
            in
                if(check = 1) then true else false
    end;

    fun parseTill([],b) = ([],[])
    |   parseTill((a::c):bigint, b:char) = if(a = b) then ([],c) else 
    let
        val (ans,left) = (parseTill(c,b))
    in
        (a::ans,left)
    end


    fun parseStringToDecimal(s:string) = 
    let
        val a = explode s;
        val sign = if(hd(a) = #"~") then (#"~") else (#"+");
        val a = if(hd(a) = #"~" orelse hd(a) = #"+") then tl(a) else a;

        val (I,a) = parseTill(a,#"."); (*I stores the integer part and a stores the stuff after the .*)
        val I = if(I = []) then [#"0"] else I;

        val (N,a) = parseTill(a,#"("); 
        val (R,a) = parseTill(a,#")"); (*now all of S,I,N,R have been parsed*)
    in
        (sign,I,N,R)
    end;

    fun getPow10(x) = 
    let
        fun Pow10(0) = [#"1"]
        |   Pow10(n) = #"0"::Pow10(n-1)
    in
        rev (Pow10(x))
    end;

    fun fromDecimal(s:string) :rational = 
    let
        val (sign,I,N,R) = parseStringToDecimal(s);
        val num = if(R = [#"0"]) then ((bi.stripZeros(I@N)):bigint) else bi.sub((I@N@R):bigint,(I@N):bigint);
        val nlen = length(N); 
        val rlen = length(R);
        val denom = if(R = [#"0"]) then getPow10(nlen) 
                    else (bi.sub(getPow10(nlen + rlen),getPow10(nlen)));
        val num = if(sign = #"~" andalso not (num = [#"0"])) then #"~"::num else num;
        val result = ReduceToFractionalNormalForm((num,denom))
    in
        result
    end;

    fun findInList([],c,n) = ~1
    |   findInList(a::b,c,n) = if(a=c) then n else findInList(b,c,n+1);


    fun getFractionalParts(rem,b:bigint,rlist) = 
        let
        val listPos = findInList(rlist,rem,1);
        in
        if (not (listPos = ~1)) then ([],listPos) 
        else
            if(bi.stripZeros(rem) = [#"0"]) then 
            ([#"0"],1)
        else
            let
                val (q,newRem) = BigInt.divide(rem@[#"0"],b);
            in  
                if (newRem = [#"0"]) then
                    (q,0)
                else if(q = [#"0"]) then 
                    let 
                        val (N,R) = getFractionalParts(newRem,b,rem::rlist);
                    in
                        (#"0"::N,R)
                    end
                else 
                    let
                        val (N,R) = getFractionalParts(newRem, b,rem::rlist);
                    in
                        (hd(q)::N,R)
                    end
                     
                    
            end
    end;
        
    fun toDecimal(([#"0"],_) | ([#"~",#"0"],_)) = ".(0)"
        |   toDecimal((a,b):rational) = 
        let 
            val (I,rem) = bi.divide(a,b); (*I is the integer part of the final form*)
            val (nr, num) = getFractionalParts(rem,b,[]) 
            val nrstring = implode nr;
            val siz = size(nrstring);
            val N = String.substring(nrstring, 0,siz - num);
            val R = if(num = 0) then "0" else String.substring(nrstring,siz-num,num);
            val R = "("^R^")";
            val I = implode I;
        in
            I^"."^N^R (*^" and nrstring is "^nrstring (* and num is "^Int.toString(num)^" and size is "^Int.toString(size(nrstring)) *) *)
    end;

    fun showDecimal(a) = (*prints and also returns the decimalNormalForm string*)
    let
        val decimalNormalForm = toDecimal(a);
        in
        (print(decimalNormalForm); decimalNormalForm ) (*this is used to print the decimal normal form*)
    end

end;

structure Rational = makeRational(BigInt);

