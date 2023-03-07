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
%              Agent intelligent .          %
%===========================================%


:- [heuristic_evaluation].
:- [minimax].
:- [alphabeta].
:- [alphabeta_heuristic].

% L'agent choisit de jouer dans (Row, Col):
agent(Board, Player, Move) :-
	other(Player, LastPlayer),
	(
		% Vérifie s'il est possible de gagner sur ce tour:
%		winning_move(Board, Player, Move)
%		;
%		% Vérifie s'il est possible de perdre au prochain tour:
%		winning_move(Board, LastPlayer, Move)
%		;
		% Algorithme Minimax:
%		minimax(Board-LastPlayer-nil, _-_-Move, _).
		% Algorithme Alpha-Bêta:
%		alphabeta(Board-LastPlayer-nil, -inf, inf, _-_-Move, _)
		% Algorithme Alpha-Bêta avec heuristique (profodeur de recherche limitée):
		get_time(Time),
		alphabeta_heuristic(Board-LastPlayer-nil, -inf, inf, _-_-Move, _, 1, Time, 1.5)
	).

% Établi les transitions possibles à partir d'un état:
moves(Board-LastPlayer-_, PosList) :-
	not(game_over(Board, _)),
	% Récupère les cases non utilisées:
	get_possible_moves(Board, PossibleMoves),
	random_permutation(PossibleMoves, PossibleMovesShuffled),
	% Détermine à qui le tour appartient:
	other(Player, LastPlayer),
	% Construit la liste des transitions possibles:
	bagof(NewBoard-Player-Move,
		(
			member(Move, PossibleMovesShuffled),
			make_a_move(Board, Player, Move, NewBoard)
		), PosList).

% Évalue la valeur d'un état pour un joueur:
staticval(Board-_-_, Value) :-
	game_over(Board, Winner), !,
	(
		Winner = nil ->
		Value is 0
		;
		(
			Winner = n ->
			Value is 1
			;
			Value is -1
		)
	).

% Évalue la valeur heurisitique d'un état pour un joueur:
heuristicval(Pos, Value) :-
	heuristic_score(Pos, Value), !.

% Établi à qui appartient le tour:
min_to_move(_-n-_).   % -> au joueur blanc (il est précédé par le joueur noir).
max_to_move(_-b-_).   % -> au joueur noir (il est précédé par le joueur blanc).
max_to_move(_-nil-_). % -> au joueur noir (il est le premier à jouer).
