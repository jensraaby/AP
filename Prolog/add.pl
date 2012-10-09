add(X,List1,List2) :- member(X,List1), !, List1 = List2.

add(X, [Y | YS], L ) :- member(Z,YS), 
     Z \= X, 
     X \= Y, 
     member(X,L),
     member(Y,L).