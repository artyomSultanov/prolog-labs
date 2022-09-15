% task 1
fact(N, 1) :- N =< 0.
fact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.

% task 2
p1(X, Y, Z) :- Z = X + Y.
p2(X, Y, Z) :- Z is X + Y.

% task 3
even(X) :- X mod 2 =:= 0.
odd(X) :- X mod 2 =\= 0.

% task 4
fact2(0, 1).
fact2(N, 2) :- even(N), N < 0.
fact2(N, 1) :- odd(N), N < 0.
fact2(N, F) :- N >= 0, N1 is N - 2, fact2(N1, F1), F is N * F1.

% task 5
lid(N, K) :- K**2 > N, N =\= 1.
lid(N, K) :- K**2 =< N, N mod K =\= 0, K1 is K + 1, lid(N, K1).
prime(X) :- lid(X, 2).

% task 6
sirakuz(1, A0) :- A0 > 0, write(A0).
sirakuz(N, A0) :- N > 1, A0 mod 2 =:= 0, A0 > 0, A1 is A0 div 2, N1 is N - 1, write(A0), nl, sirakuz(N1, A1).
sirakuz(N, A0) :- N > 1, A0 mod 2 =\= 0, A0 > 0, A1 is 3 * A0 + 1, N1 is N - 1, write(A0), nl, sirakuz(N1, A1).

% task 7
numOfDays(M, X) :- ((M = jan; M = mar; M = may; M = jul; M = aug; M = oct; M = dec), X > 0, X =< 31);
				   ((M = apr; M = jun; M = sep; M = nov), X > 0, X =< 30);
		   		   (M = feb, X > 0, X =< 28).
nextMonth(date(M1, _), date(M2, _)) :- (M1 = jan, M2 = feb); (M1 = feb, M2 = mar);
									   (M1 = mar, M2 = apr); (M1 = apr, M2 = may);
					 				   (M1 = may, M2 = jun); (M1 = jun, M2 = jul);
					 				   (M1 = jul, M2 = aug); (M1 = aug, M2 = sep);
					 				   (M1 = sep, M2 = oct); (M1 = oct, M2 = nov);
					 				   (M1 = nov, M2 = dec); (M1 = dec, M2 = jan).
nextDate(D1, D2) :- date(M1, Day1) = D1, D2 = date(_, Day2), numOfDays(M1, Day1),
				    nextMonth(D1, D2), Day2 = 1, Tom1 = Day1 + 1, \+ numOfDays(M1, Tom1).
nextDate(D1, D2) :- date(M1, Day1) = D1, D2 = date(M2, Day2), numOfDays(M1, Day1),
					M1 = M2, Tom1 is Day1 + 1, Tom1 = Day2, numOfDays(M2, Day2).