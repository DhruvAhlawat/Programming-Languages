(*StripZeros takes care of leading zeros at all the helper functions*)
fun stripZeros([]) = []
|   stripZeros(b::y) = if (length(y) = 0) then [b] 
else if #"0"=b then stripZeros(y)
                else b::y;

fun reverse(x,z) = if null(x) then z
else reverse(tl(x),hd(x)::z); 


val zeroVal : int = Char.ord(#"0"); 
fun rev x = reverse(x,[]);   
(* First, I need a function that can be used to add two strings as integers *)

(* adds two strings as integers, works in O(n+m) where n and m are lengths of the strings*)
fun add("","") = "0" 
|   add(x,y) = 
    let 
        val revXList = rev (stripZeros((explode x)));
        val revYList = rev (stripZeros((explode y))); (*reverses both of the input strings, since integer addition is done from the right*)
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
        addHelper(revXList,revYList,0)
    end;


(* then what I need is a comparision function that can compare 2 strings as integers *)
(*Compares both strings as integers*)
fun leq("","") = true
|   leq("",y) = true
|   leq(x, "") = false (*leq*)
|   leq(x,y) = let
        val x = implode (stripZeros(explode x));
        val y = implode (stripZeros(explode y)); (*removes the leading 0's while comparing*)
    in
        if(String.size(x) < String.size(y)) then
            true
        else if (String.size(x) > String.size(y))  then    
            false
        else
            let 
                val compa = String.compare(x,y);
            in
                (compa = LESS) orelse (compa = EQUAL) (*if the sizes are same then the inbuilt compare function works well*)
            end
    end;
(*done*)


(*Multiplier also makes things easy so here it is*)
fun multiply(x,0) = "0"
|   multiply(x,1) = x 
|   multiply(x,k) = 
    let 
        val revX = rev (stripZeros(explode x));
        fun MultHelp([],b,carry) = if (carry = 0 ) then "" else String.str(Char.chr(carry + zeroVal))
        |   MultHelp(a::y,b,carry) = 
            let 
                val dig = Char.ord(a) - zeroVal;
                val num = (dig*b)  + carry;
                val ones = (num mod 10);
                val tens = ((num - ones) div 10);
            in
                (MultHelp(y,b,tens) ^ String.str(Char.chr(ones + zeroVal)))
            end;
    in
        MultHelp(revX,k,0)
    end;


(*Also would need a subtractor, for the remainder values*)
fun subtract(x,y) = 
    let 
        val revX = rev(stripZeros(explode x)); 
        val revY = rev(stripZeros(explode y));
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
      implode(stripZeros (explode (SubtractHelper(revX,revY,0)))) (*removes the leading 0's*)
    end;

(*with this all the basic arithmetic functions have been developed and the main code can finally be started*)

fun findNextNum(div10,remainder,cur) = if (cur = 10) then 9 else
    let
        val curString = String.str(Char.chr(cur + zeroVal));
        val sumDiv = add(div10,curString);
        val v = multiply(sumDiv,cur);
    in
        if(leq(v,remainder)) then 
            findNextNum(div10,remainder,cur+1)
        else 
            cur-1
    end;  
(*Above function was used as a linear helper function to determine the next digit*)
fun  isqrtld(x) = 
    let 
        val xList = (explode (x)); 
        fun findSquareRoot(ans,divisor,[],remainder) = (ans,remainder)
        |   findSquareRoot(ans,divisor,ab,remainder) = if((length(ab) mod 2) = 0) then 
            let
                val remNew = remainder ^ (String.str(hd(ab))^(String.str(hd(tl(ab))))); (*the new remainder*)
                val div10 = (divisor^"0");
                val nextDig = findNextNum(div10,remNew,0); (*this returns an integer for the digit*)
                (* print(INt.toString(nextDig)) *)
                val div10 = add(div10,String.str(Char.chr(nextDig + zeroVal))); (*updated value of div10*)
                val remNew = subtract(remNew,multiply(div10,nextDig));
                val div10 = add(div10,String.str(Char.chr(nextDig + zeroVal))); (*added the current digit again*)
                val ansNew = (ans)^(String.str(Char.chr(nextDig + zeroVal)));
            in
                (findSquareRoot(ansNew,div10,tl(tl(ab)),remNew))
            end
        else
            let
                val remNew = remainder ^ String.str(hd(ab)); (*new value for the remainder is this*)
                val div10 = (divisor^"0");
                val nextDig = findNextNum(div10,remNew,0); (*this returns an integer for the digit*)
                val div10 = add(div10,String.str(Char.chr(nextDig + zeroVal))); (*updated value of div10*)
                val remNew = subtract(remNew,multiply(div10,nextDig));
                val div10 = add(div10,String.str(Char.chr(nextDig + zeroVal))); (*added the current digit again*)
                val ansNew = (ans)^(String.str(Char.chr(nextDig + zeroVal)));
            in
                findSquareRoot(ansNew,div10,tl((ab)),remNew) (*Calls this helper function recursively*)
            end
    in
        findSquareRoot("","",xList,"0")
    end;
