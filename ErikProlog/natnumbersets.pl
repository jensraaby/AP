nat(z).
nat(s(X)) :- nat(X).

% Zero is the base case
% otherwise reduce each variable by one until the first becomes z, or stop if Y becomes z first.
less(z, s(_)).
less(s(X), s(Y)) :- less(X, Y).

% First rule = empty set is valid
% second = check a valid nat
% third  = checking ordering of initial numbers and recursing
checkset([]).
checkset([X]) :- nat(X).
checkset([First|[Second|Tail]]) :- less(First, Second), checkset([Second|Tail]).

% ismember
%ismember(Nat,Set,yes or no)
ismember(z,[z|_],yes).
ismember(X,[X],yes).
ismember(X,[_|[X|_]],yes).
/*ismember(X,[Y|Rest],yes) :- less(X,Y), ismember(X,Rest,yes). */

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
