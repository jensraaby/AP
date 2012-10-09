male(robert).
female(alice).

parent(robert, kenneth).
parent(robert, michael).
parent(robert, alice).

# X is father to Y
father(X, Y) :- male(X), parent(X, Y).

mother(X, Y) :- female(X), parent(X, Y).

grandfather(X, Y) :- father(X, Z), parent(Z, Y).


haveChild(X, Y) :- parent(X, Y).

# X is ancestor to Y
ancestor(X, Y) :- parent(X,Y)
ancestor(X, Y) :- parent(Z,Y), ancestor(X,Z).

# X is ancestor to Y
brother(X, Y) :- male(X), parent(Z, X), parent(Z, Y), X \= Y.
