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
%    Tests unitaires pour alphabeta.pl.              %
%                                                    %
%    Exécuter les tests: ?- run_tests.               %
%====================================================%


:- [board].
:- [interface].
:- [agent].

:- begin_tests(alphabeta).

test(alphabeta_tic_tac_toe_1) :-
	set_goal(3),
	Pos = [[b,b,n],
		   [v,n,v],
		   [b,n,v]]-b-(0-1),
	Alpha = -inf,
	Beta = inf,

	time(alphabeta(Pos, Alpha, Beta, GoodPos, Val)),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 1-0,
	show_static_values(Pos, GoodPos, IdealMove),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = 0).

test(alphabeta_tic_tac_toe_2) :-
	set_goal(3),
	Pos = [[b,n,b],
		   [n,v,v],
		   [n,b,v]]-n-(0-1),
	Alpha = -inf,
	Beta = inf,
	
	time(alphabeta(Pos, Alpha, Beta, GoodPos, Val)),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 2-2,
	show_static_values(Pos, GoodPos, IdealMove),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = -1).

test(alphabeta_tic_tac_toe_3) :-
	set_goal(3),
	Pos = [[b,b,n],
		   [v,n,v],
		   [b,n,v]]-b-(0-1),
	Alpha = -inf,
	Beta = inf,
	
	time(alphabeta(Pos, Alpha, Beta, GoodPos, Val)),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 1-0,
	show_static_values(Pos, GoodPos, IdealMove),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = 0).

test(alphabeta_tic_tac_toe_4) :-
	set_goal(3),
	Pos = [[b,b,n],
		   [n,v,b],
		   [v,v,n]]-b-(1-2),
	Alpha = -inf,
	Beta = inf,
	
	time(alphabeta(Pos, Alpha, Beta, GoodPos, Val)),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 2-0,
	show_static_values(Pos, GoodPos, IdealMove),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = 1).

test(alphabeta_tic_tac_toe_5) :-
	set_goal(3),
	Pos = [[v,b,b],
		   [v,n,v],
		   [n,v,v]]-b-(0-2),
	Alpha = -inf,
	Beta = inf,
	
	time(alphabeta(Pos, Alpha, Beta, GoodPos, Val)),
	GoodPos = _-_-MoveDone,
	
	IdealMove = 0-0,
	show_static_values(Pos, GoodPos, IdealMove),
	
	assertion(MoveDone = IdealMove),
	assertion(Val = 1).
	
:- end_tests(alphabeta).

% Affiche la position jouée et celle désirée avec leur score respectif:
show_static_values(Board-_-_, MoveDoneBoard-Player-MoveDone, IdealMove) :-
	static_score(MoveDoneBoard, Player, MoveDoneScore),
	make_a_move(Board, Player, IdealMove, IdealMoveBoard),
	static_score(IdealMoveBoard, Player, IdealMoveScore),
	coordinates_to_id(MoveDone, MD_ID),
	format('~nPosition jouée:  ~w;  Valeur statique: ~w~n', [MD_ID, MoveDoneScore]),
	display_gomoku_board(MoveDoneBoard),
	coordinates_to_id(IdealMove, IM_ID),
	format('Position idéale: ~w;  Valeur statique: ~w~n', [IM_ID, IdealMoveScore]),
	display_gomoku_board(IdealMoveBoard).
