

def add(a,b):
    #using only recursion is allowed
    a = a[::-1]; 
    b = b[::-1]; #first we reverse both of the strings, then we send to add helper
    return addReversed(a,b,0);
def addReversed(a,b,carry):
    #now the strings a and b are in reverse order
    if(len(a) == 0 and len(b) == 0):
        if(carry == 0):
            return "";
        else:
            return chr(carry + ord('0'));
    if(len(a) == 0):
        curDig = ord(b[0]) + carry - ord('0');
        b = b[1:];
    elif(len(b) == 0):
        curDig = ord(a[0]) + carry - ord('0');
        a = a[1:];
    else:
        curDig = ord(a[0]) + ord(b[0]) + carry - 2*ord('0') ;
        a = a[1:]; b = b[1:]; #removing the first digit from both
    if(curDig <= 9):
        #then we can simply add this character
        #to our ongoing thing
        nextDigs = addReversed(a,b,0); #no carry then
        nextDigs += chr(curDig + ord('0'));
        return nextDigs;
    else:
        car = 1; #the carry can only be 1 anyway
        curDig -= 10;
        nextDigs = addReversed(a,b,car);
        nextDigs += chr(curDig + ord('0'));
        return nextDigs;

print(add("","99999"));

