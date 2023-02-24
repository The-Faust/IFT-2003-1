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

%===========================================%
%  Options:                                 %
%  -Initialiser le jeu:  ?- gomoku.         %
%  -Avec paramétrage:    ?- play.           %
%  -Tic-Tac-Toe (bonus): ?- tictactoe.      %
%                                           %
%  Pour une partie AI vs AI:                %
%  - Gomoku:             ?- gomoku_auto     %
%  - Tic-Tac-Toe:        ?- tictactoe_auto  %
%===========================================%


:- [board].
:- [interface].
:- [evaluation].
%:- [minimax].
:- [alphabeta].
:- [agent].


% Identifiants du joueur:
players_name(n, 'noir').    % Joueur noir (n).
players_name(b, 'blanc').   % Joueur blanc (b).

% Prédicat qui permet d'alterner les joeurs:
other(b, n).
other(n, b).

% Établi un tour complet et boucle jusqu'à ce que le jeu termine:
turn(Board, Player, NewBoard, Goal) :-
	introduce_turn(Player),
	(   % Vérifie s'il reste un emplacement vide:
		has_an_empty_cell(Board) ->
		(
			(   % Vérifie à qui le tour appartient:
				b_getval(players_color, Color),
				Player == Color ->
				(   % Le joueur choisi la case à jouer:
					request_next_move(Board, Move)
				)
				;
				(   % L'ordinateur choisi la case à jouer:
					agent(Board, Player, Goal, Move)
				)
			),
			make_a_move(Board, Player, Move, NewBoard)
		)
		;
		% Aucun emplacement vide, c'est un impasse:
		display_tie
	),
	conclude_turn(NewBoard, Player, Goal, NextPlayer),
	turn(NewBoard, NextPlayer, _, Goal).

% Démarre le jeu avec paramétrage:
play :-
	Firstplayer = n,
	set_gomoku_board(Board),
	request_goal(Board, Goal),
	request_players_color,
	display_gomoku_board(Board),
	turn(Board, Firstplayer, _, Goal).

% Démarre le jeu selon les paramètres typiques:
gomoku :-
	Firstplayer = n,
	N is 7,
	Goal is 5,
	create_gomoku_board(N, Board),
	request_players_color,
	display_gomoku_board(Board),
	turn(Board, Firstplayer, _, Goal).

% Partie de Gomoku AI vs AI:
gomoku_auto :-
	Firstplayer = n,
	N is 7,
	Goal is 5,
	create_gomoku_board(N, Board),
	b_setval(players_color, v),
	display_gomoku_board(Board),
	turn(Board, Firstplayer, _, Goal).

% Implémentation de Tic-Tac-Toe (bonus):
tictactoe :-
	Firstplayer = n,
	N is 3,
	Goal is 3,
	create_gomoku_board(N, Board),
	request_players_color,
	display_gomoku_board(Board),
	turn(Board, Firstplayer, _, Goal).

% Partie de Tic-Tac-Toe AI vs AI:
tictactoe_auto :-
	Firstplayer = n,
	N is 3,
	Goal is 3,
	create_gomoku_board(N, Board),
	b_setval(players_color, v),
	display_gomoku_board(Board),
	turn(Board, Firstplayer, _, Goal).
	