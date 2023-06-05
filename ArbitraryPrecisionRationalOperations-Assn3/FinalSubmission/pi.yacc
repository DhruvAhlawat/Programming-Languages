%%
%name Pi
%term ID of string | LPAREN | RPAREN | DIV | MUL | ADD | SUB 
| NUMBA of string | DISPLAY | EOL | EOF | DECI of string | SHOWDECIMAL

%nonterm EXP of Rational.rational | START of Rational.rational 
%pos int
%eop EOL EOF
%noshift EOF EOL
%nonassoc EOF DISPLAY 

%nodefault
%verbose
%arg (fileName) : string

%left SHOWDECIMAL
%left ADD SUB 
%left DIV MUL
%left LPAREN RPAREN EOL

%%
START : EXP (print (Rational.showRat(EXP)); 
                    print "\n";
                    EXP)
        | SHOWDECIMAL EXP (print(Rational.toDecimal(EXP)); print "\n"; EXP)
EXP : NUMBA            (valOf(Rational.make_rat(BigInt.getBigInt(NUMBA), BigInt.getBigInt("1"))))
    | DECI          (Rational.fromDecimal(DECI))
    | EXP ADD EXP    (Rational.add(EXP1,EXP2))
    | EXP MUL EXP   (Rational.multiply(EXP1,EXP2))
    | EXP DIV EXP     (valOf(Rational.divide(EXP1,EXP2)))
    | EXP SUB EXP     (Rational.subtract(EXP1,EXP2))
    | LPAREN EXP RPAREN (EXP)



