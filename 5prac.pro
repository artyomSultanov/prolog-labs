% task 1
add(A, B, C) :- C is A + B.
myMap(_, [], _, []).
myMap([], _, _, []).
myMap([H1 | T1], [H2 | T2], Func, [Hr | Tr]) :- 
	Term =.. [Func, H1, H2, Hr],
	call(Term),
	myMap(T1, T2, Func, Tr).

% task 2
choosePairs(Lst1, Lst2, Func, Lst) :- 
	choosePairs_help(Lst1, Lst2, Func, X),
	(var(Lst) -> sort(X, Lst); sort(X, Res1), sort(Lst, Res2), Res1 = Res2).

choosePairs_help([], [], _, []).
choosePairs_help([H1 | T1], [H2 | T2], Func, Lst) :-
	Term =.. [Func, H1, H2],
	(call(Term) -> Hr = H1-H2, choosePairs_help(T1, T2, Func, Tr), Lst = [Hr | Tr];
		choosePairs_help(T1, T2, Func, Lst)).

% task 3
foldl(X, _, [], X) :- !.
foldl(X0, Func, [H | T], Res) :-
	Term =.. [Func, X0, H, X1],
	call(Term),
	foldl(X1, Func, T, Res).

% task 4
% Взял предикат insert из 2.4а
insert(tr(X, L, R), X, tr(X, L, R)).
insert(tr(N, L, R), X, Tree) :- X < N, L = nil, L1 = tr(X, nil, nil), Tree = tr(N, L1, R).
insert(tr(N, L, R), X, Tree) :- N < X, R = nil, R1 = tr(X, nil, nil), Tree = tr(N, L, R1).
insert(tr(N, L, R), X, tr(Nn, Nl, Nr)) :- X < N, \+ L = nil, insert(L, X, L1), tr(Nn, Nl, Nr) = tr(N, L1, R).
insert(tr(N, L, R), X, tr(Nn, Nl, Nr)) :- N < X, \+ R = nil, insert(R, X, R1), tr(Nn, Nl, Nr) = tr(N, L, R1).
treeFromList(Lst, Tree) :- nonvar(Lst), Lst = [H | T],
	foldl(tr(H, nil, nil), insert, T, Tree), !.

% task 5
addHead(_, [], []).
addHead(X, [H | T], [[X | H] | Tr]) :- addHead(X, T, Tr).

% task 6
deepSort([], []).
deepSort([H | T], R) :- sort(H, X), deepSort(T, Y), R = [X | Y].
deepFilter([], _, []).
deepFilter([H | T], N, R) :- 
	length(H, N) -> append(H, Res, R), deepFilter(T, N, Res); deepFilter(T, N, R).
deepCheck_help(_, []).
deepCheck_help(Lst, [H | T]) :- 
	member(H, Lst), deepCheck_help(Lst, T).
deepCheck(_, 0, _). 
deepCheck(Lst, N, R) :-
	deepFilter(R, N, X), X \= [], 
	deepCheck_help(Lst, X), N1 is N - 1, deepCheck(Lst, N1, R).
subsets(L, S) :- var(L), var(S), false.
subsets(Lst, Subs) :- 
	var(Lst), nonvar(Subs),
	deepFilter(Subs, 1, X), 
	length(X, N), length(Subs, N1), 
	N =\= 1, 2**N =:= N1, 
	deepCheck(X, N, Subs),
	sort(X, Lst), !.
subsets(Lst, Subs) :- 
	nonvar(Lst), subsets_help(Lst, Y, [[]]), deepSort(Y, X),
	(var(Subs) -> sort(X, Subs);
		length(Lst, N1), length(Subs, N2), 2**N1 =:= N2,
		sort(X, Res1), sort(Subs, Res2), Res1 = Res2).
subsets_help([], Subs, Subs).
subsets_help([H | T], Subs, Res) :-
	addHead(H, Res, X),
	append(X, Res, Y),
	subsets_help(T, Subs, Y).

% task 7*
thing(plate, 1, 1.5).
thing(jar, 3, 5).
thing(silverSpoon, 0.3, 15).
sack(3).
countPrice([], 0).
countPrice([H | T], Price) :-
	thing(H, _, X), countPrice(T, Sum), Price is Sum + X.

countWeight([], 0).
countWeight([H | T], Weight) :-
	thing(H, X, _), countWeight(T, Sum), Weight is Sum + X.

pack(Things, MaxPrice) :- 
	findall(X, thing(X, _, _), SetThings),
	subsets(SetThings, SubSetThings),
	pack_help(SubSetThings, [], 0, Things, MaxPrice), !.
pack_help([], GoodThings, MaxPrice, GoodThings, MaxPrice).
pack_help([H | T], Things, Price, GoodThings, MaxPrice) :-
	countPrice(H, X),
	countWeight(H, Y),
	sack(W),
	(Y =< W, X > Price -> pack_help(T, H, X, GoodThings, MaxPrice); 
		pack_help(T, Things, Price, GoodThings, MaxPrice)).