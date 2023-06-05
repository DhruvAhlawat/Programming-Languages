

maxDigsStored = 0; 

def findNextNum(div10,rem,cur): #returns the next number to the one we want
    #gotta add a digit to ans10, which is ans*10
    #how we do this is basically a loop but using recursion
    val = (div10 + cur)*cur;
    if( val <= rem): 
        return findNextNum(div10,rem,cur+1);
    if(val > rem):
        return cur-1;
    

#currently ans is a string and so is div and rem
def findSquareRoot(ans,div,number:str,rem):
    if(len(number) == 0):
        return (ans,rem);
    if(len(number)%2 == 0):
        #in this case we can
        rem *= 100;
        #then we take the first two digits of number
        toAdd = (ord(number[0])-ord('0'))*10 + ord(number[1]) - ord('0');
        number = number[2:]; #removing the first 2 digits, gotta do this in O(1) using lists
        rem += toAdd;
        #now we find the best addition by a simple loo-
        #no we cannot use any loops, so what we must do is recursion again
    else:
        rem *= 10; rem += ord(number[0]) - ord('0'); number = number[1:];
    #now rem has been formed, just gotta form the div and answer components
    nextDig = findNextNum(div*10,rem,0); 
    div = (div*10) + nextDig;
    rem = (rem - (div*nextDig));
    div += nextDig;
    ans = (10*ans) + nextDig;
    return findSquareRoot(ans,div,number,rem);


#print(findSquareRoot(0,0,"1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",0));

import math;
print(math.sqrt(1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0));



