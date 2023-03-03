% Travail pratique 1 : Gomoku
% Conception d'un jeu intelligent
%
% remis par
%   Omar Akrout    (NI: 111 165 246)
%   René Chenard   (NI: 111 232 277)
%   Vincent Martel (NI: 111 105 797)
%
% dans le cadre du cours
%   IFT-2003 - Intelligence artificielle I
%   Hiver 2023

%====================================================%
%    Tests unitaires pour alphabeta_heuristic.pl.    %
%                                          		     %
%    Exécuter les tests: ?- run_tests.     		     %
%====================================================%


:- [gomoku].

:- begin_tests(alphabeta_heuristic).

test(alphabeta_heuristic_1) :-
	set_goal(5),
	Pos = [
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,v,v,v,b,v,v,v],
		[v,v,v,n,v,v,n,v,b,v,v],
		[v,v,v,v,b,n,n,n,n,b,v],
		[v,v,v,v,v,b,n,b,v,v,v],
		[v,v,v,v,v,n,b,b,v,v,v],
		[v,v,v,v,n,v,v,b,v,v,v],
		[v,v,v,b,v,v,v,v,n,v,v],
		[v,v,v,v,v,v,v,v,v,v,v]]-b-(3-7),
	Alpha = -inf,
	Beta = inf,
	
	Depth is 1,
	get_time(TimeStamp),
	TimeLimit is 1.5,
	
	Pos = Board-_-_,
	alphabeta_heuristic(Pos, Alpha, Beta, GoodPos, Val, Depth, TimeStamp, TimeLimit),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 2-6,
	Player = n,
	show_heuristic_values(Board, Player, MoveDone, IdealMove, IdealMoveScore),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = IdealMoveScore).

test(alphabeta_heuristic_2) :-
	set_goal(5),
	Pos = [
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,b,v,v,v,v,v,v],
		[v,v,v,v,n,v,n,v,v,v,v],
		[v,v,v,v,b,n,v,v,v,v,v],
		[v,v,v,v,b,b,n,v,v,v,v],
		[v,v,v,n,b,n,n,v,v,v,v],
		[v,v,v,v,b,v,v,b,v,v,v],
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,v,v,v,v,v,v,v]]-b-(8-4),
	Alpha = -inf,
	Beta = inf,
	
	Depth is 1,
	get_time(TimeStamp),
	TimeLimit is 1.5,
	
	Pos = Board-_-_,
	alphabeta_heuristic(Pos, Alpha, Beta, GoodPos, Val, Depth, TimeStamp, TimeLimit),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 9-4,
	Player = n,
	show_heuristic_values(Board, Player, MoveDone, IdealMove, IdealMoveScore),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = IdealMoveScore).

test(alphabeta_heuristic_3) :-
	set_goal(5),
	Pos = [
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,v,v,v,n,v,v,v,v],
		[v,v,v,v,v,v,b,v,v,v,v],
		[v,v,b,n,v,b,b,v,v,v,v],
		[v,v,v,n,b,v,b,v,v,v,v],
		[v,v,v,v,n,b,b,v,v,v,v],
		[v,v,n,v,n,n,n,v,v,v,v],
		[v,v,v,v,n,v,v,v,v,v,v]]-n-(9-2),
	Alpha = -inf,
	Beta = inf,
	
	Depth is 1,
	get_time(TimeStamp),
	TimeLimit is 1.5,
	
	Pos = Board-_-_,
	alphabeta_heuristic(Pos, Alpha, Beta, GoodPos, Val, Depth, TimeStamp, TimeLimit),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 9-3,
	Player = b,
	show_heuristic_values(Board, Player, MoveDone, IdealMove, IdealMoveScore),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = IdealMoveScore).
	
:- end_tests(alphabeta_heuristic).

% Affiche la position jouée et celle désirée avec leur score respectif:
show_heuristic_values(Board, Player, MoveDone, IdealMove, IdealMoveScore) :-
	make_a_move(Board, Player, MoveDone, MoveDoneBoard),
	heuristic_score(MoveDoneBoard, MoveDoneScore),
	make_a_move(Board, Player, IdealMove, IdealMoveBoard),
	heuristic_score(IdealMoveBoard, IdealMoveScore),
	coordinates_to_id(MoveDone, MD_ID),
	format('~nPosition jouée:  ~w;  Valeur heuristique: ~w~n', [MD_ID, MoveDoneScore]),
	display_gomoku_board(MoveDoneBoard),
	coordinates_to_id(IdealMove, IM_ID),
	format('Position idéale: ~w;  Valeur heuristique: ~w~n', [IM_ID, IdealMoveScore]),
	display_gomoku_board(IdealMoveBoard).
