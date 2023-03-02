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
%  - Gomoku:             ?- gomoku_auto.    %
%  - Tic-Tac-Toe:        ?- tictactoe_auto. %
%===========================================%


% Chargement des modules:
:- [board].
:- [interface].
:- [agent].

% Paramètres choisis par l'utilisateur:
:- dynamic player/1. % Couleur de l'utilisateur.

% Démarre le jeu avec paramétrage:
play :-
	set_board_size(BoardSize),
	request_goal(BoardSize, Goal),
	request_players_color,
	begin_game(Firstplayer, Goal, BoardSize, Board),
	turn(Board, Firstplayer, _).

% Démarre le jeu selon les paramètres typiques:
gomoku :-
	request_players_color,
	begin_game(Firstplayer, 5, 19, Board),
	turn(Board, Firstplayer, _).

% Partie de Gomoku AI vs AI:
gomoku_auto :-
	begin_game(Firstplayer, 5, 19, Board),
	turn(Board, Firstplayer, _).

% Implémentation de Tic-Tac-Toe (bonus):
tictactoe :-
	request_players_color,
	begin_game(Firstplayer, 3, 3, Board),
	turn(Board, Firstplayer, _).

% Partie de Tic-Tac-Toe AI vs AI (bonus):
tictactoe_auto :-
	begin_game(Firstplayer, 3, 3, Board),
	turn(Board, Firstplayer, _).

% Établi la routine de début de partie:
begin_game(Firstplayer, Goal, BoardSize, Board) :-
	set_goal(Goal),
	create_gomoku_board(BoardSize, Board),
	display_gomoku_board(Board),
	Firstplayer = n,
	load_cache.

% Établi la routine de fin de partie:
end_game :-
	save_cache,
	break.

% Établi un tour complet et boucle jusqu'à ce que le jeu termine:
turn(Board, Player, NewBoard) :-
	% Suivi de la durée d'un tour:
	statistics(runtime, [Start|_]),
	% Annonce le prochain tour:
	introduce_turn(Player),
	(   % Vérifie s'il reste un emplacement vide:
		has_an_empty_cell(Board) ->
		(
			(   % Vérifie à qui le tour appartient:
				player(Player) ->
				(   % Le joueur choisi la case à jouer:
					request_next_move(Board, Move)
				)
				;
				(   % L'ordinateur choisi la case à jouer:
					agent(Board, Player, Move)
				)
			),
			make_a_move(Board, Player, Move, NewBoard)
		)
		;
		% Aucun emplacement vide, c'est un impasse:
		display_tie
	),
	% Conclu le tour:
	conclude_turn(NewBoard, Player, NextPlayer),
	% Affiche les statistiques (temps écoulé):
	statistics(runtime, [End|_]),
	Time is (End - Start)/1000,
	format('Le tour a pris: ~3f secondes.~n', [Time]),
	% Récursion jusqu'à l'atteinte d'un état final:
	turn(NewBoard, NextPlayer, _).

% Permet de sauvegarder les calculs effectués:
save_cache :-
	tell('evaluations.cache'),
	listing([memo_static_score, memo_heuristic_score]),
	told.

% Permet de charger les calculs effectués:
load_cache :-
	(
		exists_file('evaluations.cache') ->
		(
			retractall(memo_static_score(_)),
			retractall(memo_heuristic_score(_)),
			consult('evaluations.cache')
		)
		;
		true
	).
