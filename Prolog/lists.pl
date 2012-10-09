/*concat_lists([X|XS], [Y|YS],[X,L | concat_lists(XS,[Y|YS],[L|LS])]). */

concat_lists([],Z,Z).
concat_lists([X | XS], Y,[X | Z]) :-
    concat_lists(XS, Y, Z).
    
    
add(X,List1,List2) :- member(X,List1), !, List1 = List2.
add(X, [Y | YS], [X,Y | YS] ).