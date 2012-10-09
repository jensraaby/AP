% Zero is the base case
% otherwise reduce each variable by one until the first becomes z, or stop if Y becomes z first.
less(z, s(_)).
less(s(X), s(Y)) :- less(X, Y).

% First rule = empty set is valid
% second = check a valid nat
% third  = checking ordering of initial numbers and recursing
checkset([]).
checkset([X]) :- less(X, _).
checkset([Head|[First|Tail]]) :- less(Head, First), checkset([First|Tail]).

% First rule = initialize call to helper function, sets "lowest checked value" to z
% second = initialize call to helper function, checking if z is less than our element, as the previous case ignores this possibility
% There are two versions of ismember: ismember2 and ismember 3
%	The difference is ismember4 is the "simple" version, which performs correctly as long as a set is provided.
%	ismember2 is the "complex" version, which will correctly return sets if there is a variable and Val is set to yes.
ismember(Element, Set, Value) :- ismember2(Element, Set, Value, z).
ismember(Element, [z|Set], Value) :- less(z, Element), ismember2(Element, Set, Value, z).

% First rule = cannot be a member of an empty set
% second = if the Num matches the Head of our set, yes
% third  = if our Num is greater than the Head of the list, peel off the head and recurse, keeping track of previous comparison
% fourth = if Num is greater than Head, Num is not an element (definition of set, in order)
% ismember2 requires more code because it WILL generate the correct sets through Y via ismember(X,Y,yes), where X is given.
ismember2(_, [], no, _).
ismember2(Num, [Num|_], yes, _).
ismember2(Num, [Head|Tail], Val, Last) :- less(Head, Num), less(Last, Head), ismember2(Num, Tail, Val, Head).
ismember2(Num, [Head|_], no, _) :- less(Num, Head).

% initialize call to helper function, ensures that the input is a Set.
ismember3(Element, Set, Value) :- ismember4(Element, Set, Value), checkset(Set).

% First rule = cannot be a member of an empty set
% second = if Num matches Head, yes
% third  = if Num is greater than Head, peel off Head and recurse!
% fourth = if Num is less than Head, we're done (definition of set)
% ismember4 requires less code because it is not expected to generate sets through Y via ismember(X,Y,yes), where X is given.
ismember4(_, [], no).
ismember4(Num, [Num|_], yes).
ismember4(Num, [Head|Tail], Val) :- less(Head, Num), ismember4(Num, Tail, Val).
ismember4(Num, [Head|_], no) :- less(Num, Head).

% initialize call to helper function, ensures that inputs are valid sets
union(Left, Right, Union) :- union2(Left, Right, Union), checkset(Left), checkset(Right).

% First rule = union of two empty sets is the empty set
% second = union of the empty set and nonempty X is X
% third  = same as second, reversed. Three cases are made so that prolog does not repeat itself
% fourth = if the Left Head is lesser, add it to the front of the Union
% fifth  = same, with Right Head
% sixth  = if the Head is the same for both lists, add it only once to the Union
union2([], [], []).
union2([], [H|T], [H|T]).
union2([H|T], [], [H|T]).
union2([L|Left], [R|Right], [L|Tail]) :- less(L, R), union2(Left, [R|Right], Tail).
union2([L|Left], [R|Right], [R|Tail]) :- less(R, L), union2([L|Left], Right, Tail).
union2([H|Left], [H|Right], [H|Tail]) :- union2(Left, Right, Tail).

% initialize call to helper function, ensure that inputs are both valid sets
intersection(Left, Right, Intersection) :- intersection2(Left, Right, Intersection), checkset(Left), checkset(Right).

% First rule = intersection of two empty sets is the empty set
% second = intersection of the empty set and nonempty X is the empty set
% third  = same as second, reversed. Three cases for empty sets so that prolog does not repeat itself
% fourth = if the Left Head is lesser, remove it and recurse on the remainder
% fifth  = same, with Right Head
% sixth  = if the Head is the same for both lists, add it only once to the intersection
intersection2([], [], []).
intersection2([], [_|_], []).
intersection2([_|_], [], []).
intersection2([L|Left], [R|Right], Tail) :- less(L, R), intersection2(Left, [R|Right], Tail).
intersection2([L|Left], [R|Right], Tail) :- less(R, L), intersection2([L|Left], Right, Tail).
intersection2([H|Left], [H|Right], [H|Tail]) :- intersection2(Left, Right, Tail).


% Below are functions only used to check validity of our claims.
% They make use of built-in predicates by prolog, but are used only to CHECK the code.
% i2n converts an int into a nat
% minimum, remove, and popmin are all part of genset
i2n(0,z) :- !.
i2n(X,s(Y)) :- Z is (X-1), i2n(Z,Y).

minimum([X],X) :- !.
minimum([H|T],H) :- minimum(T,X), H < X.
minimum([H|T],X) :- minimum(T,X), H >= X.

remove(_, [], []) :- !.
remove(X, [H|T], [H|Out]) :- \+ (X = H), !, remove(X,T,Out).
remove(X, [X|T], Out) :- !, remove(X,T,Out).

popmin(In, Out, Min) :- minimum(In,Min), remove(Min, In, Out).

% genset turns a list of ints into a set of nats. This lets us test several variations easily!
genset([],[]) :- !.
genset(In,[Nat|Set]) :- !, popmin(In, Out, Min), i2n(Min, Nat), ismember(Nat,Set,no), genset(Out,Set).

% genlist turns a list of ints into a list of nats (not necessarily a set!). This lets us error test.
genlist([],[]) :- !.
genlist([Head|Tail],[Nat|Set]) :- i2n(Head,Nat), genlist(Tail,Set).

% genintlist creates integer lists of the given length, with values between min and max! (inefficient, but it is my own)
genintlist(Min,Max,Length,List) :- length(List,Length), genints(Min,Max,List).

% returns every int value between Min and Max
between(X,_,X).
between(Min,Max,Val) :- X is (Min+1), Min<Max, between(X,Max,Val).

% generates every integer list possible
genints(_,_,[]).
genints(Min,Max,[X|List]) :- between(Min,Max,X), genints(Min,Max,List).

% generates every nats list, not necessarily sets
genlists(Min,Max,Length,List) :- genintlist(Min,Max,Length,Ints), genlist(Ints,List).

% generates every nats set. inefficient, but simple
gensets(Min,Max,Length,List) :- genintlist(Min,Max,Length,Ints), genset(Ints,List).
