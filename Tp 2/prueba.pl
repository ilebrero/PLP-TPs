count([],X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X|T],Y,Z):- X\=Y,count(T,X,Z).