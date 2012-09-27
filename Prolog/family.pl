male(robert).
male(hans).
male(michael).
male(kenneth).
female(alice).
female(inge).

parent(robert,kenneth).
parent(robert,michael).
parent(robert,alice).
parent(hans,robert).
parent(inge,kenneth).
parent(inge,alice).
parent(inge,michael).

father(X,Y) :- parent(X,Y), male(X).

mother(X,Y) :- parent(X,Y), female(X).

grandFather(X,Y) :- father(X,Z), parent(Z,Y).

haveChild(X,Y) :- parent(X,Z), parent(Y,Z).

ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).

brother(X,Y) :- parent(Z,X), 
    parent(Z,Y), 
    male(X), 
    male(Y),
    X \= Y.