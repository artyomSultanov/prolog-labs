% task 1
toSquares(A, A, 1).
toSquares(A, B, K) :- A =\= B, A1 is abs(A - B), B1 is min(A, B), toSquares(A1, B1, K1), K is K1 + 1.

% task 2
checkDigit(N, X) :- abs(N) >= 0, abs(N) < 10, abs(N) =:= X.
checkDigit(N, X) :- 
	abs(N) >= 10,
	(X =:= abs(N) mod 10 ; N1 is abs(N) div 10, checkDigit(N1, X)).
qntDigit(A, A, X, 1) :- checkDigit(A, X).
qntDigit(A, A, X, 0) :- \+ checkDigit(A, X).
qntDigit(A, B, X, K) :- 
	A < B,
	checkDigit(A, X),
	A1 is A + 1,
	qntDigit(A1, B, X, K1),
	K is K1 + 1, !.
qntDigit(A, B, X, K) :-
	A < B,
	\+ checkDigit(A, X),
	A1 is A + 1,
	qntDigit(A1, B, X, K).

% task 3
fact(X, Y) :- var(X), var(Y), write('CHECK ARGS'), !.
fact(0, 1).
fact(X, Y) :- 
	nonvar(X),
	nonvar(Y),
	X > 0,
	X1 is X - 1,
	Y mod X =:= 0,
	Y1 is Y div X,
	fact(X1, Y1).
fact(1, Y) :- var(Y), Y = 1.
fact(X, Y) :- 
	var(Y),
	X > 0,
	X1 is X - 1,
	fact(X1, Y1),
	Y is Y1 * X, !.
fact(X, Y) :- var(X), fact_iter(X, Y, 1, 1).
fact_iter(X, Y, X, Y).
fact_iter(X, Y, XN, YN) :- 
	YN < Y,
	X1 is XN + 1,
	Y1 is YN * X1, 
	fact_iter(X, Y, X1, Y1).

% task 4 
% 4 a
insert(tr(X, L, R), X, tr(X, L, R)).
insert(tr(N, L, R), X, Tree) :-
	X < N,
	L = nil,
	L1 = tr(X, nil, nil),
	Tree = tr(N, L1, R).
insert(tr(N, L, R), X, Tree) :-
	N < X,
	R = nil,
	R1 = tr(X, nil, nil),
	Tree = tr(N, L, R1).
insert(tr(N, L, R), X, tr(Nn, Nl, Nr)) :-
	X < N,
	\+ L = nil,
	insert(L, X, L1),
	tr(Nn, Nl, Nr) = tr(N, L1, R).
insert(tr(N, L, R), X, tr(Nn, Nl, Nr)) :-
	N < X,
	\+ R = nil,
	insert(R, X, R1),
	tr(Nn, Nl, Nr) = tr(N, L, R1).

% 4 b -------------
contains(Tree, X) :- nonvar(X), containsNonvar(Tree, X).
contains(Tree, X) :- var(X), containsVar(Tree, X).
%% containsNonvar(tr(N, _, _), N) :- write('N= '),write(N),nl.
containsNonvar(tr(N, _, _), N) :- !.
containsNonvar(tr(N, L, _), X) :- X < N, containsNonvar(L, X), !.
containsNonvar(tr(_, _, R), X) :- containsNonvar(R, X), !.

containsVar(tr(N, nil, _), X) :- N = X.
containsVar(tr(_, L, _), X) :- \+ L = nil, containsVar(L, X).
containsVar(tr(N, L, R), X) :- \+ R = nil, \+ L = nil, N = X.
containsVar(tr(_, _, R), X) :- \+ R = nil, containsVar(R, X).

% 4 c
isSearchTree(tr(N, nil, nil)) :- integer(N).
isSearchTree(tr(N, L, R)) :- isSTiter(tr(N, L, R), -1.2e+10, +1.2e+10).
isSTiter(nil, _, _).
isSTiter(tr(N, L, R), Min, Max) :-
	Min < N,
	N < Max,
	isSTiter(L, Min, N),
	isSTiter(R, N, Max).

% 4 d*
searchNextToDeleted(tr(X, nil, nil), X).
searchNextToDeleted(tr(N, L, _), X) :- L \= nil -> searchNextToDeleted(L, X); N = X.
removeNextToDeleted(tr(X, nil, nil), X, nil). 
removeNextToDeleted(tr(X, nil, R), X, R). 
removeNextToDeleted(tr(X, L, nil), X, L). 
removeNextToDeleted(tr(N, L, R), X, Tree) :- N > X, Tree = tr(N, Y, R), removeNextToDeleted(L, X, Y). 

myRemove(nil, _, nil). % главный предикат
myRemove(tr(X, nil, nil), X, nil) :- !.
myRemove(tr(X, L, nil), X, Tree) :- Tree = L, !.
myRemove(tr(X, nil, R), X, Tree) :- Tree = R, !.
myRemove(tr(X, L, R), X, Tree) :- searchNextToDeleted(R, Y), write(Y), removeNextToDeleted(R, Y, NewR), write(NewR), Tree = tr(Y, L, NewR), !.
myRemove(tr(N, L, R), X, Tree) :- 
	X < N -> myRemove(L, X, Y), Tree = tr(N, Y, R); myRemove(R, X, Y), Tree = tr(N, L, Y).

% supertask