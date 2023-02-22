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

%======================================%
%  Options:                            %
%  -Initialiser le jeu:  ?- gomoku.    %
%  -Avec paramétrage:    ?- play.      %
%  -Tic-Tac-Toe (bonus): ?- tictactoe. %
%======================================%

:- [board].
:- [interface].
:- [evaluation].
:- [agent].

% Identifiants du joueur:
players_name(n, 'noir').    % Joueur noir (n).
players_name(b, 'blanc').   % Joueur blanc (b).

% Prédicat qui permet d'alterner les joeurs:
other(b, n).
other(n, b).

% Permet à un joueur de jouer son tour:
move(Board, Player, NewBoard, WinningScore) :-
	(   % Vérifie s'il reste un emplacement vide:
		cell_is_empty(Board, _, _) ->
		(
			(   % Vérifie à qui le tour appartient:
				b_getval(players_color, Color),
				Player == Color ->
				(   % Le joueur choisi la case à jouer:
					request_next_move(Board, Row, Col)
				)
				;
				(   % L'ordinateur choisi la case à jouer:
					agent(Board, WinningScore, Row, Col)
				)
			),
			% Met à jour le plateau de jeu:
			set_cell_content(Board, Row, Col, Player, NewBoard)
		)
		;
		(
			write('Le plateau de jeu est plein!\n'),
			write('Il s\'agit d\'un impasse!\n'),
			halt
		)
	).

% Établi un tour complet et boucle jusqu'à ce que le jeu termine:
turn(Board, Player, NewBoard, WinningScore) :-
	draw_line,
	players_name(Player, PlayersName),
	cell_to_char(Player, PlayersSymbol),
	format('Le joueur ~w (~w) joue son tour:\n', [PlayersName, PlayersSymbol]),
	move(Board, Player, NewBoard, WinningScore),
	display_gomoku_board(NewBoard),
	evaluate_score(NewBoard, Player, Score),
	format('Score du joueur ~w (~w): ~d\n', [PlayersName, PlayersSymbol, Score]),
	(
		evaluate_score(NewBoard, Player, WinningScore)  ->
		(
			draw_line,
			format('Le joueur ~w (~w) gagne!\n', [PlayersName, PlayersSymbol]),
			halt
		)
		;
		true
	),
	other(Player, NextPlayer),
	turn(NewBoard, NextPlayer, _, WinningScore).

% Démarre le jeu avec paramétrage:
play :-
	Firstplayer = n,
	set_gomoku_board(Board),
	length(Board, N),
	(
		N > 3 ->
		format(atom(Prompt), 'Choisissez l\'objectif, soit le nombre de jetons à aligner: (min: 3, max: ~d)', N),
		request_valid_integer(3, N, Prompt, WinningScore)
		;
		WinningScore is 3
	),
	request_players_color,
	display_gomoku_board(Board),
	turn(Board, Firstplayer, _, WinningScore).

% Démarre le jeu selon les paramètres typiques:
gomoku :-
	Firstplayer = n,
	N is 19,
	WinningScore is 5,
	create_gomoku_board(N, Board),
	request_players_color,
	display_gomoku_board(Board),
	turn(Board, Firstplayer, _, WinningScore).

% Implémentation de Tic-Tac-Toe (bonus):
tictactoe :-
	Firstplayer = n,
	N is 3,
	WinningScore is 3,
	create_gomoku_board(N, Board),
	request_players_color,
	display_gomoku_board(Board),
	turn(Board, Firstplayer, _, WinningScore).
