
1. showDecimal(a) is used to first print the decimal normal form of the rational a, and then assign it as a string. So incase the length is long, during the print statement the entire decimal normal form would be printed, while it finally returns it as well but SML/NJ does not print it entirely and adds a # at the end in such cases.

    this makes it different from toDecimal, which only returns the decimal normal form as string, without explicitly printing it.


# Grammar for Rational numbers
Terminals = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, . ,(,), +, ˜, eps, /}
Non-Terminals = {S, G, I, J, K, N, M, L} (where eps is the empty character)
Start-symbol = S

Rules =

1. S => I | +I | ~I
2. I => MJ | .L
3. J => MJ | eps | /ZNK | .L
4. K => MK | eps
8. Z => 0 | eps
5. N => 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
6. M => 0 | N
7. L => K(MK) | MK

### some notes about the above grammar 
1. the Z, M and N non terminals are essentially for digits, and the reason that I made 3 of them was so that in the denominator part that J can go to (/ZNK), we never have a situation where the denominator is just zeros, but it can still have leading 0s before a nonzero number. This ensures that the denominator is always non-zero.

2. depending on which state S moves to, determines whether there would be a sign used and also what it would be.

3. I can directly go to .L so we can have a number written in decimal form with the Integer part blank (taken as 0). Otherwise it can also go to MJ, which signifies a non-empty integer or a non-empty fractional form (if J eventually goes to /ZNK), or a decimal form (if J goes to .L)

4. ZNK goes to a non-zero integer number, as N is a non-zero digit while Z is either emtpy or some leading zeros.

5. L either can go to MK, in which case it would just be some digits (that are following the decimal .), or it can go to K(MK) which is one of the forms for rational numbers where the part in bracket denotes the recurring part and the part outside denotes the nonrecurring part. note that here the recurring part is always non-empty


# Grammar for rational number expressions
We will use the same grammar as above to denote rational numbers, so S will denote a rational number as it always forms a terminating sequence to a rational number.

Non-Terminals = {E, T, F, V} + {S, G, I, J, K, N, M, L}(from rational grammar) 
Terminals = {-, +, *, /} + {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, . ,(,), +, ˜, eps} 
Start symbol = E

Rules = 

1. E => E+T | E-T | T
2. T => T*F | T/F | F
3. F => (E) | S
1. S => I | +I | ~I | V
2. I => MJ | .L
3. J => MJ | eps | .L
4. K => MK | eps
8. Z => 0 | eps
5. N => 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
6. M => 0 | N
7. L => K(MK) | MK
8. V => AB
9. A => [a-zA-Z_]
10. B => AB | MB | NB | eps

(where [a-zA-Z_] denotes any alphabet or underscore. Written like this for clarity for reader since they are a lot of characters)
### some notes about the above grammar

1. Since the expression can contain variables, according to the assignment statement, I added the Non-terminal V which always leads to a valid variable name, starting with a non-numeral.

2. + and - are left associative, and both have the same precedence.

3. * and / are also left associative, and have higher precedence than + and - as they appear lower in the derivation tree.

4. bracketed expressions have the highest precedence order

5. an important note is that an expression like 4/5 may be interpreted as an integer with division operator in between another integer, OR as a rational number of the type p/q. Since the final answer will come out the same, it may be fine, but I removed this type of ambiguity by removing the form of rational number as p/q
(remove the rule allowing J to go to /ZNK). 


# How to use the rational.sml

1. either open sml by typing  
>       rational.sml

    or when sml is already opened, 
>        use "rational.sml"

2. type 
>       CM.make "pi.cm";

3. for compiling the results of calculations, there is a function Pi.compile which needs to be called, and it takes in an argument of the file name that contains the expression to be evaluated. I have added the test file for this purpose
to run it, type 
>      Pi.compile "testInput.txt";

    and the output will be printed and as well as returned as a type of Rational.rational.


### the syntax for rational expressions
The printed output is in the fractional Normal form by default, but if it is required to print it in decimal form then you should add the word decimal before the entire expression in the file.
there usual infix operators with the usual precedence relations. 

## Important note.
while the decimal format of rational in the expression may or may not be of the standard normal form with minimum non-recurring part, it is necessary that it follows the regular expression given for it in the assignment. for e.g., 9/2 should be written as 4.5(0) and not 4.5
