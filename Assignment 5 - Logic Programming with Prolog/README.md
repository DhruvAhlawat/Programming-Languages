#Some Design Decisions

1. in arith.P, I have allowed a unary '-' being applied on a negative number. It negates the value of the negative number so essentially it makes it positive.

for example, if arith([-2,-2]). is called, then the output will give 2 solutions
	1. -2=-2
	2. --2=--2
nothing on this was mentioned so I took this decision to allow it.

2. the output for Q4 gives the 'paddler' first in each line. In total there are 5 trips, 3 from the left to right and 2 from right to left. It also prints the one who paddled twice at the end. The predicate is called by just typing abcd. 

