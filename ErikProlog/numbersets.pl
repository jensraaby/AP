nat(z).
nat(s(X)) :- nat(X).

less(z, s(_)).
less(s(X), s(Y)) :- less(X, Y), nat(X), nat(Y).

checkset([]).
checkset([X]) :- less(X, _).
checkset([Head|[First|Tail]]) :- less(Head, First), checkset([First|Tail]).

ismember(Element, Set, Value) :- ismember2(Element, Set, Value, z).
ismember(Element, [z|Set], Value) :- ismember2(Element, Set, Value, z), less(z, Element).

ismember2(_, [], no, _).
ismember2(Num, [Num|_], yes, _).
ismember2(Num, [Head|Tail], Val, Last) :- less(Head, Num), less(Last, Head), ismember2(Num, Tail, Val, Head).
ismember2(Num, [z|Tail], Val, z): less(z, Num), ismember2(Num, Tail, Val, z).

union(Left, Right, Union) :- checkset(Left), checkset(Right), union2(Left, Right, Union).

union2([], [], []).
union2([], [H|T], [H|T]).
union2([H|T], [], [H|T]).
union2([L|Left], [R|Right], [L|Tail]) :- less(L, R), union2(Left, [R|Right], Tail).
union2([L|Left], [R|Right], [R|Tail]) :- less(R, L), union2([L|Left], Right, Tail).
union2([H|Left], [H|Right], [H|Tail]) :- union2(Left, Right, Tail).

intersection(Left, Right, Intersection) :- checkset(Left), checkset(Right), intersection2(Left, Right, Intersection).

intersection2([], [], []).
intersection2([], [_|_], []).
intersection2([_|_], [], []).
intersection2([L|Left], [R|Right], Tail) :- less(L, R), intersection2(Left, [R|Right], Tail).
intersection2([L|Left], [R|Right], Tail) :- less(R, L), intersection2([L|Left], Right, Tail).
intersection2([H|Left], [H|Right], [H|Tail]) :- intersection2(Left, Right, Tail).
