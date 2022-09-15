% task 1
myReverse(Lst, Rev) :- var(Lst), nonvar(Rev), myReverse(Rev, Lst), !.
myReverse(Lst, Rev) :- myReverse_new(Lst, Rev, []).
myReverse_new([], Rev, Rev).
myReverse_new([H | T], Rev, X) :-
	myReverse_new(T, Rev, [H | X]).

replaceAtom(H, X) :- 
	(((H = 'A'; H = 'a') -> X = 'a');
	((H = 'B'; H = 'b') -> X = 'b');
	((H = 'C'; H = 'c') -> X = 'c')).
palindrom(Lst) :- length(Lst, X), X mod 2 =:= 0, palindrom_new_even(Lst, [], []).
palindrom(Lst) :- length(Lst, X), X mod 2 =\= 0, palindrom_new_odd(Lst, [], []).

palindrom_new_even([], Rev1, Rev2) :- myReverse(Rev1, Rev2), !.
palindrom_new_even([H | T], Rev1, Rev2) :-
	length([H | T], Len0), length(Rev1, Len1),
	replaceAtom(H, Hn),
	(Len0 > Len1 -> palindrom_new_odd(T, [Hn | Rev1], Rev2);
	Len0 =< Len1 -> palindrom_new_odd(T, Rev1, [Hn | Rev2])).

palindrom_new_odd([], Rev1, Rev2) :- myReverse(Rev1, Rev2), !.
palindrom_new_odd([H | T], Rev1, Rev2) :-
	length([H | T], Len0), length(Rev1, Len1),
	replaceAtom(H, Hn),
	(Len0 > Len1 + 1 -> palindrom_new_odd(T, [Hn | Rev1], Rev2);
	Len0 =:= Len1 + 1 -> palindrom_new_even(T, Rev1, Rev2);
	Len0 < Len1 + 1 -> palindrom_new_odd(T, Rev1, [Hn | Rev2])).

% task 2
prime(2).
prime(3).
prime(N) :- N =\= 1, N mod 2 =\= 0, prime(N, 3).
prime(N, K) :-
	\+ N mod K =:= 0, 
	(K > abs(sqrt(N)), ! ; K1 is K + 2, prime(N, K1), !). 
primeLst([], []).
primeLst([N], [N]) :- prime(N).
primeLst([H | T], [H | Tn]) :- prime(H), primeLst(T, Tn), !.
primeLst([H | T], Lst) :- \+ prime(H), primeLst(T, Lst).

% task 3
myPrefix([], []).
myPrefix([], X) :- X \= [], myPrefix([], []).
myPrefix([H | T], [H | Tn]) :- myPrefix(T, Tn).
mySublist(Sub, Res) :- (myPrefix(Sub, Res); Res = [_ | T], mySublist(Sub, T)).


% task 4
zipSort([], [], []).
zipSort([], Lst, Lst).
zipSort(Lst, [], Lst).
zipSort([H1, H2 | []], [Hs1, Hs2 | []], Lst) :- 
	H1 < H2, Hs1 < Hs2,
	((H2 < Hs1 -> Lst = [H1, H2, Hs1, Hs2]);
	(H1 < Hs1, H2 < Hs2 -> Lst = [H1, Hs1, H2, Hs2]);
	(H1 > Hs1, H2 < Hs2 -> Lst = [Hs1, H1, H2, Hs2]);
	(H1 > Hs1, H1 < Hs2, H2 > Hs2 -> Lst = [Hs1, H1, Hs2, H2]);
	(H1 > Hs2 -> Lst = [Hs1, Hs2, H1, H2])), !.
zipSort([H1 | T1], [H2 | T2], [H1 | Tn]) :-
	H1 =< H2,
	zipSort(T1, [H2 | T2], Tn).
zipSort([H1 | T1], [H2 | T2], [H2 | Tn]) :-
	H2 < H1,
	zipSort([H1 | T1], T2, Tn).

% task 5 a
numlst(0, [0]).
numlst(N, Lst) :- var(N), nonvar(Lst), numlst_first(N, Lst, 0). % Я ТУТ
numlst(N, Lst) :- var(N), var(Lst), numlst_vars(N, Lst, 0, [0]).
numlst(N, Lst) :- nonvar(N), numlst_second(N, Lst, []).
numlst_second(0, D, D) :- !.
numlst_second(N, Lst, D) :-
	N > 0, N1 is N // 10, D1 is N mod 10, numlst_second(N1, Lst, [D1 | D]).

% task 5 b
% numlst_b определён выше в задаче 5 а.
numlst_vars(N, Lst, N, Lst) :- N \= 0, numlst(N, Lst).
numlst_vars(N, Lst, K, Res) :- 
	K1 is K + 1, numlst(K, Res), numlst_vars(N, Lst, K1, _).
numlst_first(N, [], N).
numlst_first(N, [H | T], K) :-
	K1 is K*10 + H, numlst_first(N, T, K1).

% task 6*
splitSet(_, [], [], []).
splitSet(Pivot, [H | T], [H | T1], Bigger) :- Pivot >= H, splitSet(Pivot, T, T1, Bigger).
splitSet(Pivot, [H | T], Smaller, [H | T1]) :- Pivot < H, splitSet(Pivot, T, Smaller, T1).
quickSort([], []) :- !.
quickSort([X], [X]) :- !.
quickSort(Lst, Res) :- 
	Lst = [H | T],
	splitSet(H, T, Smaller, Bigger),
	quickSort(Smaller, S),
	quickSort(Bigger, B),
	append(S, [H | B], Res), !.

% task 7*
order([_]) :- !.
order(Lst) :- Lst = [H1 | T], T = [H2 | _], H1 is H2 - 1, order(T).
maxSublist(Lst, Res) :- 
	bagof(S, (mySublist(S, Lst), order(S)), Subs),
	maxSublist_help(Subs, 0, [], Res).
maxSublist_help([], _, Res, Res) :- !.
maxSublist_help(Subs, N, Max, Res) :-
	Subs = [H | T],
	(length(H, X), X > N -> maxSublist_help(T, X, H, Res);
		maxSublist_help(T, N, Max, Res)).
