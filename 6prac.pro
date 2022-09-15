% task 1
opposite(left, right).
opposite(right, left).

unsafe(state(_, P, A, family(Dad, Mom, S1, S2, D1, D2))) :-
	Lst = [Dad, Mom, S1, S2, D1, D2],
	opposite(P, A), memberchk(A, Lst).
unsafe(state(_, _, _, family(Dad, Mom, S1, S2, _, _))) :- opposite(Dad, Mom), (Mom = S1; Mom = S2).
unsafe(state(_, _, _, family(Dad, Mom, _, _, D1, D2))) :- opposite(Dad, Mom), (Dad = D1; Dad = D2).

% changeMove изменяет один в Lst по индексу N 
changeMove(Lst, N, Res) :- changeMove_help(Lst, N, 1, [], Res).
changeMove_help([], _, _, Lst, Res) :- reverse(Lst, Res).
changeMove_help([H | T], N, K, Lst, Res) :- 
	K = N -> opposite(H, X), K1 is K + 1, changeMove_help(T, N, K1, [X | Lst], Res); K1 is K + 1, changeMove_help(T, N, K1, [H | Lst], Res).
% trueMovesFor1 находит все возможные перемещения одного человека
trueMovesFor1(Lst, Res) :- trueMovesFor1_help(Lst, 1, [], Res).
trueMovesFor1_help(Lst, K, Res, Res) :- K1 is K - 1, length(Lst, K1), !.
trueMovesFor1_help(Lst, K, X, Res) :-
	K1 is K + 1, changeMove(Lst, K, R), trueMovesFor1_help(Lst, K1, [R | X], Res).
% trueMovesFor2 находит все возможные перемещения двух человек
trueMovesFor2(Lst, Res) :- trueMovesFor2_help(Lst, 1, 2, [], Res).
trueMovesFor2_help(Lst, _, I, Res, Res) :- I1 is I - 1, length(Lst, I1), !.
trueMovesFor2_help(Lst, K, I, X, Res) :-
	K =:= 2, K < I -> K1 is K + 1, trueMovesFor2_help(Lst, K1, I, X, Res);
	(K < I -> K1 is K + 1, changeMove(Lst, K, Y), changeMove(Y, I, Y1), trueMovesFor2_help(Lst, K1, I, [Y1 | X], Res);
		K1 = 1, I1 is I + 1, trueMovesFor2_help(Lst, K1, I1, X, Res)).

% genMove - предикат, генерирующий возможные перемещения
genMove(state(Boat, P, A, family(Dad, Mom, S1, S2, D1, D2)), state(Boat1, P1, A1, family(Dad1, Mom1, S1n, S2n, D1n, D2n))) :-
	opposite(Boat, Boat1),
	Lst1 = [P, A, Dad, Mom], Lst2 = [P1, A1, Dad1, Mom1],
	Lst3 = [S1, S2, D1, D2], Lst4 = [S1n, S2n, D1n, D2n],
	Lst1withoutA = [P, Dad, Mom], Lst2withoutA = [P1, Dad1, Mom1],
	trueMovesFor1(Lst1withoutA, X),
	%% write(X), nl, nl,
	trueMovesFor2(Lst1, Y),
	%% write(Y), nl, nl,
	trueMovesFor1(Lst3, Z),
	%% write(Z), nl, nl,
	(member(Lst2withoutA, X), Lst4 = Lst3, A = A1; 
	member(Lst2, Y), Lst4 = Lst3; 
	member(Lst2withoutA, X), member(Lst4, Z), A = A1).

dfs(TargetState, TargetState, ResultPath, ResultPath).
dfs(CurState, TargetState, CurPath, ResultPath) :-
	CurState \= TargetState,
	genMove(CurState, NewState),
	\+ unsafe(NewState),
	\+ memberchk(NewState, CurPath),
	dfs(NewState, TargetState, [NewState | CurPath], ResultPath).

% printPath - предикат, красиво выводящий результат
printPath([]).
%% printPath([H | T]) :- H = state(B, P, A, family(X1, X2, X3, X4, X5, X6)), write(B-P-A-fam-[X1-X2-X3-X4-X5-X6]), nl, printPath(T).
printPath([H  | T]) :- write(H), nl, printPath(T).
solve :-
	dfs(state(left, left, left, family(left, left, left, left, left, left)), 
 		state(right, right, right, family(right, right, right, right, right, right)),
	 	[state(left, left, left, family(left, left, left, left, left, left))],
 		ResPath),
	reverse(ResPath,ResPathRev),
	printPath(ResPathRev).

% task 2
visit(yes).
visit(no).
impl(A, B) :- call(A) -> call(B) ; true.

start :-
	visit(Andrey), visit(Dmitriy), visit(Boris), visit(Victor), visit(Grigoriy),
	impl((Andrey = yes, Dmitriy = yes), (Boris = no)),
	impl((Andrey = yes, Dmitriy = no), (Boris = yes, Victor = no)),
	(Andrey = yes, Victor = yes ; Andrey = no, Victor = no),
	impl((Dmitriy = yes), (Grigoriy = no)),
	impl((Boris = no, Victor = no), (Dmitriy = yes)),
	impl((Boris = no, Victor = yes), (Dmitriy = no, Grigoriy = yes)),
	impl((Boris = yes), (Andrey = yes)),
	write(andrey), write(':'), write(Andrey), nl,
	write(dmitriy), write(':'), write(Dmitriy), nl,
	write(boris), write(':'), write(Boris), nl,
	write(victor), write(':'), write(Victor), nl,
	write(grigoriy), write(':'), write(Grigoriy), nl.

% task 3
myZip([], _, _, _, Res) :- !, Res = [].
myZip(_, [], _, _, Res) :- !, Res = [].
myZip(_, _, [], _, Res) :- !, Res = [].
myZip([H1 | T1], [H2 | T2], [H3 | T3], Pred, [Hr | Tr]) :-
	H =.. [Pred, H1, H2], Hr =.. [Pred, H, H3], myZip(T1, T2, T3, Pred, Tr).

statement11(_-fantik-Color) :- Color \= orange.
statement12(_-murlyka-Color) :- Color \= gray.
statement21(_-druzhok-Color) :- Color \= white.
statement22(_-elisey-Color) :- Color \= gray.
statement31(misha-_-black).
statement32(maksim-murlyka-_).
statement41(pasha-elisey-_).
statement42(dima-_-white).
statement51(dima-Cat-_) :- Cat \= fantik.
statement52(_-druzhok-Color) :- Color \= gray.
statement61(dima-druzhok-_).
statement61(misha-elisey-_).
myFilter(Lst, Res) :- myFilter_help(Lst, [], Res).
myFilter_help([], Res, Res).
myFilter_help([H | T], X, Res) :- delete(T, H, Y), myFilter_help(Y, [H | X], Res). 

checkAll(Statements, Lst, Res) :- checkAll_help(Statements, Lst, 1, Res).
%% checkAll_help(_, _, _, _).
checkAll_help(Statements, Lst, K, Res) :-
	length(Statements, Len),
	K < Len -> nth(K, Statements, FalseStatement),
		delete(Statements, FalseStatement, TrueStatements),
		checkFalse(FalseStatement, Lst), checkTrue(TrueStatements, Lst),
		Res = Lst.
myAndMapFalse([], _, _).
myAndMapFalse([H | T], Pred, X) :-
	P =.. [Pred, H],
	(call(P) -> X = H; myAndMapFalse(T, Pred, X)).
checkFalse([S1, S2], Lst) :-
	myAndMapFalse(Lst, S1, X),
	myAndMapFalse(Lst, S2, Y), X \= Y.

myMapTrue([], _).
myMapTrue([H | T], [S1, S2]) :-
	St1 =.. [S1, H], call(St1),
	St2 =.. [S2, H], call(St2),
	myMapTrue(T, [S1, S2]).
checkTrue([S | T], Lst) :-
	myMapTrue(Lst, S), checkTrue(T, Lst).
findCats :- 
	Guys = [misha, maksim, pasha, dima],
	Cats = [fantik, murlyka, druzhok, elisey],
	Colors = [orange, gray, white, black],
	permutation(Cats, AllCats),
	permutation(Colors, AllColors),
	myZip(Guys, AllCats, AllColors, '-', All),
	checkAll([[statement11, statement12],
			  [statement21, statement22],
			  [statement31, statement32],
			  [statement41, statement42],
			  [statement51, statement52],
			  [statement61, statement61]], All, Answer),
	write(Answer),nl,
	printPath(Answer).

% task 4
days(Days) :- Days = [monday, tuesday, wednesday, thursday, friday, saturday, sunday].
threeDays(Today, Tomorrow, Tomorrow2) :-
	days(Days),
	length(Days, Len),
	member(X, Days),
	nth(K, Days, X),
	Today = X,
	K1 is K + 1, K2 is K + 2,
	(K1 =< Len -> nth(K1, Days, Tomorrow); K1n is K1 - Len, nth(K1n, Days, Tomorrow)),
	(K2 =< Len -> nth(K2, Days, Tomorrow2); K2n is K2 - Len, nth(K2n, Days, Tomorrow2)).
whatDay(X) :-
	threeDays(X, T1, T2),
	((X \= monday, X \= tuesday) -> (T1 \= thursday, T1 \= saturday, T1 \= sunday), (T2 = wednesday; T2 = friday);
		((T1 = thursday; T1 = saturday; T1 = sunday) -> (X = tuesday; X = monday), (T2 = wednesday; T2 = friday);
			(T2 \= wednesday, T2 \= friday) -> (X = tuesday; X = monday), (T1 \= thursday, T1 \= saturday, T1 \= sunday))).


% task 5 a
% pos сопоставляет порядок столбца в численном виде
pos(a-_, 1).
pos(b-_, 2).
pos(c-_, 3).
pos(d-_, 4).
pos(e-_, 5).
pos(f-_, 6).
pos(g-_, 7).
pos(h-_, 8).
% trueMoveChess находит позицию Pos2, достижимую из Pos1 
trueMoveChess(Pos1, Pos2) :-
	Pos1 \= Pos2,
	Pos1 = _-Y1, Pos2 = _-Y2,
	Y1 >= 1, Y1 =< 8, Y2 >= 1, Y2 =< 8,
	pos(Pos1, N1), pos(Pos2, N2),
	abs(Y1 - Y2) =< 1,
	abs(N1 - N2) =< 1.
% moveChess перебирает варианты всех перемещений, удовлетворяющих предикату trueMoveChess
moveChess(Start, End) :- 
	ChessBoard = [[a-1, a-2, a-3, a-4, a-5, a-6, a-7, a-8],
			  	  [b-1, b-2, b-3, b-4, b-5, b-6, b-7, b-8],
			  	  [c-1, c-2, c-3, c-4, c-5, c-6, c-7, c-8],
			  	  [d-1, d-2, d-3, d-4, d-5, d-6, d-7, d-8],
			  	  [e-1, e-2, e-3, e-4, e-5, e-6, e-7, e-8],
			  	  [f-1, f-2, f-3, f-4, f-5, f-6, f-7, f-8],
			  	  [g-1, g-2, g-3, g-4, g-5, g-6, g-7, g-8],
			      [h-1, h-2, h-3, h-4, h-5, h-6, h-7, h-8]],
	findall(Pos, (member(Col, ChessBoard), member(Pos, Col), trueMoveChess(Start, Pos)), End).
% incFront сопоставляет список позиций с списком списков вида [позиция, номер фронта] 
incFront([], _, []).
incFront([H | T], K, [[H, K] | T1]) :- incFront(T, K, T1).
% addLast добавляет элемент E в конец списка
addLast([], E, [E]).
addLast([H | T], E, [H | T1]) :- addLast(T, E, T1).
% deletePos удаляет позиции, которые нельзя посещать
deletePos([], _, []).
deletePos([H | T], ListOfBadPositions, Res) :-
	memberchk(H, ListOfBadPositions) -> deletePos(T, ListOfBadPositions, Res);
		Res = [H | T1], deletePos(T, ListOfBadPositions, T1). 
% makeHoles создаёт список позиций, которые нельзя посещать
makeHoles(ListOfPawns, Res) :- makeHoles_help(ListOfPawns, [], Res).
makeHoles_help([], Res, Res).
makeHoles_help([H | T], Lst, Res) :-
	H = Col-Row,
	pos(Col-Row, ColNum),
	RowNew is Row - 1,
	(ColNum < 8, ColNum > 1 -> % если между 1 и 8, то пешки бьют два поля
		ColNumLeft is ColNum - 1,
		ColNumRight is ColNum + 1,
		pos(P1-_, ColNumLeft),
		pos(P2-_, ColNumRight),
		makeHoles_help(T, [H, P1-RowNew, P2-RowNew | Lst], Res); % иначе бьют по одному
			(ColNum =:= 8 -> ColNumLeft is ColNum - 1, pos(P1-_, ColNumLeft), makeHoles_help(T, [H, P1-RowNew | Lst], Res);
				(ColNum =:= 1 -> ColNumRight is ColNum + 1, pos(P2-_, ColNumRight), makeHoles_help(T, [H, P2-RowNew | Lst], Res)))).
% bfs предикат поиска в ширину, который ищет кратчайший путь от данной позиции в целевую
bfs([[Goal, Res] | _], _ , _, [Goal, Res]). % если находим целевую позицию, то останавливаемся
bfs(Queue, Visited, ListOfBadPositions, [Goal, Res]) :-
	Queue \= [], % если список будет пуст, то закончились рассматриваемые позиции
	Queue = [[P, N] | TailQueue], P \= Goal,
	% если мы попали в посещенную ранее вершину, то пропускаем её
	(memberchk(P, Visited) -> bfs(TailQueue, Visited, ListOfBadPositions, [Goal, Res]);
		% иначе
		(moveChess(P, NewWays), % NewWays - все позиции, в которые можно попасть из текущей
		deletePos(NewWays, ListOfBadPositions, WaysWithoutPawns), % удаляем из этих позиций пробитые или занятые позиции
		deletePos(WaysWithoutPawns, Visited, TrueWays), % удаляем уже посещённые позиции
		N1 is N + 1, % для нашего нового фронта + увеличиваем его значение
		incFront(TrueWays, N1, NewFront), %   <-+ 
		addLast(Visited, P, NewVisited), % добавляем в список посещённых позиций текущую
		append(TailQueue, NewFront, NewQueue), % дополняем в нашу очередь новый фронт 
		bfs(NewQueue, NewVisited, ListOfBadPositions, [Goal, Res]))).

king_path1(ListOfPawns, NumberOfMoves) :-
	Start = a-8,
	End = h-1,
	makeHoles(ListOfPawns, ListOfBadPositions), % дополняем к пешкам позиции, которые они бьют
	bfs([[Start, 0]], [], ListOfBadPositions, [End, NumberOfMoves]).

% task 5 b
% myDelMap удаляет номер фронта у посещенных мест Visited
myDelMap([], []).
myDelMap([[H, _] | T], [H | T1]) :-
	myDelMap(T, T1).
% makePath восстанавливает путь начиная с конца
makePath(Visited, CurPos, 0, Path, KingPath) :- memberchk([X, 0], Visited), trueMoveChess(X, CurPos), [X | Path] = KingPath.
makePath(Visited, CurPos, CurVal, ListOfPawns, ListOfAttackedFields, Path, KingPath) :-
	member([P, CurVal], Visited),
	trueMoveChess(P, CurPos),
	(memberchk(P, ListOfAttackedFields), \+ memberchk(P, ListOfPawns )-> 
		moveChess(P, WaysFromAttackedFields), 
		deletePos(ListOfPawns, WaysFromAttackedFields, NewListOfPawns),
		CurVal1 is CurVal - 1,
		makePath(Visited, P, CurVal1, NewListOfPawns, ListOfAttackedFields, [P | Path], KingPath);
		CurVal1 is CurVal - 1,
		makePath(Visited, P, CurVal1, ListOfPawns, ListOfAttackedFields, [P | Path], KingPath)).
% bfs2 отличается от bfs лишь тем, что тут добавлен предикат, восстанавливающий путь,
% свободным считается уже сам путь ?KingPath и изменён вид хранения посещённых мест Visited,
% где теперь вместе с позицией лежит номер фронта
bfs2([[Goal, Res] | _], Visited, _, _, ConstListOfPawns, ConstListOfAttackedFields, Goal, KingPath) :- 
	Res1 is Res - 1,
	makePath(Visited, Goal, Res1, ConstListOfPawns, ConstListOfAttackedFields, [Goal], KingPath).
bfs2(Queue, Visited, ListOfPawns, ListOfAttackedFields, ConstListOfPawns, ConstListOfAttackedFields, Goal, KingPath) :-
	Queue \= [],
	Queue = [[P, N] | TailQueue], P \= Goal,
	(memberchk([P, _], Visited) -> bfs2(TailQueue, Visited, ListOfPawns, ListOfAttackedFields, ConstListOfPawns, ConstListOfAttackedFields, Goal, KingPath); % пропускаем посещённые поля
		(moveChess(P, NewWays), % ищем все ходы короля
		deletePos(NewWays, ListOfAttackedFields, WaysWithoutAttackedFields), % убираем из этих ходов битые пешками поля
		myDelMap(Visited, PosVisited), % убираем фронты у посещённых полей
		deletePos(WaysWithoutAttackedFields, PosVisited, TrueWays), % удаляем из небитых полей посещённые
		N1 is N + 1,
		deletePos(ListOfPawns, TrueWays, WaysWithNotAttackedPawns), % удаляем пешки, которые мы смогли побить
		attackedByPawns(WaysWithNotAttackedPawns, ListOfAttackedFieldsByNotAttackedPawns), % для небитых пешек находим поля, которые они ещё бьют
		incFront(TrueWays, N1, NewFront), % увеличиваем значение следующего фронта
		addLast(Visited, [P, N], NewVisited), % добавляем текущее поле в список посещённых
		append(TailQueue, NewFront, NewQueue), % добавляем в очередь наш новый фронт
		bfs2(NewQueue, NewVisited, WaysWithNotAttackedPawns, ListOfAttackedFieldsByNotAttackedPawns, ConstListOfPawns, ConstListOfAttackedFields, Goal, KingPath))).
king_path2(ListOfPawns, KingPath) :-
	Start = a-8,
	End = h-1,
	attackedByPawns(ListOfPawns, ListOfAttackedFields), % создаём список полей, которые бьют пешки
	bfs2([[Start, 0]], [], ListOfPawns, ListOfAttackedFields, ListOfPawns, ListOfAttackedFields, End, KingPath).
% task 5 c
attackedByPawns([], []).
attackedByPawns([H | T], Fields) :-
	pos(H, N),
	N1 is N + 1, 
	N2 is N - 1, 
	H = _-Row,
	RowNew is Row - 1,
	((N > 1, N < 8 -> 
		pos(ColNew1-_, N2),
		pos(ColNew2-_, N1), 
		H1 = ColNew1-RowNew, 
		H2 = ColNew2-RowNew, 
		attackedByPawns(T, Tn),
		Fields = [H1, H2 | Tn]);
	(N = 1 -> 
		pos(ColNew-_, N1),  
		H1 = ColNew-RowNew,
		attackedByPawns(T, Tn),
		Fields = [H1 | Tn]);
	(N = 8 -> 
		pos(ColNew-_, N2),
		H1 = ColNew-RowNew, 
		attackedByPawns(T, Tn),
		Fields = [H1 | Tn])).





% task 6