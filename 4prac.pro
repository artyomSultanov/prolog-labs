:- initialization(consult('kinship.pro')).

% task 1
pred(X, Y) :- parent(X, Y).
pred(X, Y) :- setof(C, (parent(X, C), pred(C, Y)), _).

% task 2
brother(X, Y) :- 
	male(X),
	parent(P1, X), parent(P1, Y), male(P1),
	parent(P2, X), parent(P2, Y), female(P2),
	X \= Y.

% task 3
married(X, Y) :- bagof(L, (parent(X, L), parent(Y, L)), _), X \= Y.

% task 4
husband(X, Y) :- male(X), female(Y), bagof(L, (parent(X, L), parent(Y, L)), _), X \= Y.

% task 5
brotherORsister(X, Y) :-
	parent(P1, X), parent(P1, Y), male(P1),
	parent(P2, X), parent(P2, Y), female(P2),
	X \= Y.
cousin(X, Y) :- parent(P1, X), parent(P2, Y),
	brotherORsister(P1, P2).

% task 6
num_of_children(X, N) :- 
	nonvar(N), N = 0 -> parent(M, X), male(M), \+ parent(X, _);
		bagof(C, parent(X, C), L), length(L, N).

% task 7
nepMap([]).
nepMap(Lst) :- Lst = [H | T], num_of_children(H, 0), nepMap(T). 
nephews([], Y) :- parent(A, Y), male(A), \+ brotherORsister(Y, _).
nephews([], Y) :- bagof(A, parent(Y, A), _),\+ parent(_, Y).
nephews([], Y) :- bagof(C, brotherORsister(Y, C), L), nepMap(L).
nephews(X, Y) :- 
		setof(C, (brotherORsister(Y, BS), parent(BS, C)), X).

% task 8
family([F, M | C]) :- husband(F, M), bagof(D, (parent(F, D), parent(M, D)), L), sort(L, C).
