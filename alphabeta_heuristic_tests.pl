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


:- [board].
:- [interface].
:- [agent].

:- begin_tests(alphabeta_heuristic).

test(prevent_open_four) :-
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

	profile(alphabeta_heuristic(Pos, Alpha, Beta, GoodPos, Val, Depth, TimeStamp, TimeLimit)),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 2-6,
	show_heuristic_values(Pos, GoodPos, IdealMove, IdealMoveScore),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = IdealMoveScore).

test(prevent_closed_four) :-
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

	profile(alphabeta_heuristic(Pos, Alpha, Beta, GoodPos, Val, Depth, TimeStamp, TimeLimit)),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 9-4,
	show_heuristic_values(Pos, GoodPos, IdealMove, IdealMoveScore),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = IdealMoveScore).

test(prevent_semi3_four) :-
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

	profile(alphabeta_heuristic(Pos, Alpha, Beta, GoodPos, Val, Depth, TimeStamp, TimeLimit)),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 9-3,
	show_heuristic_values(Pos, GoodPos, IdealMove, IdealMoveScore),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = IdealMoveScore).

test(prevent_semi3_three) :-
	set_goal(5),
	Pos = [
		[v,v,v,v,v,v,v,v,v,v,v],
		[v,v,v,n,v,v,v,v,v,v,v],
		[v,v,v,v,b,v,b,n,n,v,v],
		[v,v,v,v,v,n,v,b,v,v,v],
		[v,v,v,v,v,b,n,v,v,b,v],
		[v,v,v,v,b,n,n,n,n,b,v],
		[v,v,v,b,n,b,b,b,b,n,v],
		[v,v,n,b,n,b,b,b,b,n,v],
		[v,b,v,v,n,b,n,n,n,b,v],
		[v,v,v,v,b,n,n,n,v,v,v],
		[v,v,v,n,v,v,b,v,v,v,v]]-b-(2-6),
	Alpha = -inf,
	Beta = inf,
	
	Depth is 1,
	get_time(TimeStamp),
	TimeLimit is 1.5,
	
	profile(alphabeta_heuristic(Pos, Alpha, Beta, GoodPos, Val, Depth, TimeStamp, TimeLimit)),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 4-3,
	show_heuristic_values(Pos, GoodPos, IdealMove, IdealMoveScore),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = IdealMoveScore).

test(win_next_turn) :-
	set_goal(5),
	Pos = [
		[v,n,v,v,v,v,b,v,v,n,v],
		[v,v,b,n,v,n,n,v,b,v,v],
		[b,v,n,b,n,v,b,b,b,b,n],
		[v,n,n,n,b,v,b,n,n,n,v],
		[b,v,n,b,b,b,b,n,v,v,v],
		[v,b,n,n,n,b,n,n,b,v,v],
		[v,v,b,v,n,n,b,b,n,v,v],
		[v,v,v,n,b,b,n,b,v,v,v],
		[v,v,v,v,v,v,b,n,n,v,v],
		[v,v,v,v,v,v,v,v,b,v,v],
		[v,v,v,v,v,v,v,v,v,v,v]]-n-(2-10),
	Alpha = -inf,
	Beta = inf,
	
	Depth is 1,
	get_time(TimeStamp),
	TimeLimit is 1.5,
	
	profile(alphabeta_heuristic(Pos, Alpha, Beta, GoodPos, Val, Depth, TimeStamp, TimeLimit)),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 2-5,
	show_heuristic_values(Pos, GoodPos, IdealMove, IdealMoveScore),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = IdealMoveScore).
	
:- end_tests(alphabeta_heuristic).

% Affiche la position jouée et celle désirée avec leur score respectif:
show_heuristic_values(Board-_-_, MoveDoneBoard-Player-MoveDone, IdealMove, IdealMoveScore) :-
	heuristic_score(MoveDoneBoard-Player-_, MoveDoneScore),
	make_a_move(Board, Player, IdealMove, IdealMoveBoard),
	heuristic_score(IdealMoveBoard-Player-_, IdealMoveScore),
	coordinates_to_id(MoveDone, MD_ID),
	format('~nPosition jouée:  ~w;  Valeur heuristique: ~w~n', [MD_ID, MoveDoneScore]),
	display_gomoku_board(MoveDoneBoard),
	coordinates_to_id(IdealMove, IM_ID),
	format('Position idéale: ~w;  Valeur heuristique: ~w~n', [IM_ID, IdealMoveScore]),
	display_gomoku_board(IdealMoveBoard).
