main :- write('This is a sample Prolog program'),
write('This program is written in the test.pl file').

word(abalone,a,b,a,l,o,n,e).
word(abandon,a,b,a,n,d,o,n).
word(enhance,e,n,h,a,n,c,e).
word(anagram,a,n,a,g,r,a,m).
word(connect,c,o,n,n,e,c,t).
word(elegant,e,l,e,g,a,n,t).

crossword(V1,V2,V3,H1,H2,H3) :-
    word(V1,_,X12,_,X14,_,X16,_),
    word(V2,_,X22,_,X24,_,X26,_),
    word(V3,_,X32,_,X34,_,X36,_),
    word(H1,_,X12,_,X22,_,X32,_),
    word(H2,_,X14,_,X24,_,X34,_),
    word(H3,_,X16,_,X26,_,X36,_).

succ(0,1).
succ(X,Y) :- succ(X-1,Y-1).

child(martha,charlotte).
child(charlotte,caroline).   
child(caroline,laura).   
child(laura,rose).
descend(X,Y) :- child(X,Y).
descend(X,Y) :- child(X,Z), descend(Z,Y).

numeral(0).
numeral(succ(X)) :- numeral(X).

add(0, Y, Y).
add(succ(X), Y, Z) :- add(X, succ(Y), Z).

greater_than(succ(_),0). 
greater_than(succ(X),succ(Y)) :- greater_than(X,Y).

directTrain(forbach,saarbruecken).
directTrain(freyming,forbach).
directTrain(fahlquemont,stAvold).
directTrain(stAvold,forbach).
directTrain(saarbruecken,dudweiler).
directTrain(metz,fahlquemont).
directTrain(nancy,metz).

travelBetween(X,Y) :- directTrain(X,Y); 
directTrain(X,Z), travelBetween(Z,Y); travelBetween(Y,X).

connected(1,2).
connected(3,4).
connected(5,6).
connected(7,8).
connected(9,10).
connected(12,13).
connected(13,14).
connected(15,16).
connected(17,18).
connected(19,20).
connected(4,1).
connected(6,3).
connected(4,7).
connected(6,11).
connected(14,9).
connected(11,15).
connected(16,12).
connected(14,17).
connected(16,19).

path(X,Y) :- connected(X,Y); connected(X,Z), path(Z,Y).

member(X, [X|_]).
member(X, [_|T]) :- member(X,T).

