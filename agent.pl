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


:- [evaluation].
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
%		alphabeta(Board-LastPlayer-nil, -inf, inf, _-_-Move, _).
		% Algorithme Alpha-Bêta avec heuristique (profodeur de recherche limitée):
		get_time(Time),
		alphabeta_heuristic(Board-LastPlayer-nil, -inf, inf, NewBoard-_-Move, _, 3, Time, 1),
		heuristicval(NewBoard-_-_, Value),
		writeln(Value)
	).

% Établi les transitions possibles à partir d'un état:
moves(Board-LastPlayer-_, PosList) :-
	not(game_over(Board, _)),
	% Récupère les cases non utilisées:
	get_possible_moves(Board, PossibleMoves),
	% Détermine à qui le tour appartient:
	other(Player, LastPlayer),
	% Construit la liste des transitions possibles:
	bagof(NewBoard-Player-Move,
		(
			member(Move, PossibleMoves),
			make_a_move(Board, Player, Move, NewBoard)
		), PosList).

% Évalue la valeur d'un état pour un joueur:
staticval(Board-_-_, Value) :-
	game_over(Board, Winner),
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
heuristicval(Board-_-_, Value) :-
	heuristic_score(Board, Value).

% Établi à qui appartient le tour:
min_to_move(_-n-_).   % -> au joueur blanc (il est précédé par le joueur noir).
max_to_move(_-b-_).   % -> au joueur noir (il est précédé par le joueur blanc).
max_to_move(_-nil-_). % -> au joueur noir (il est le premier à jouer).

% Vérifie s'il est possible pour le joueur de gagner en un tour:
winning_move(Board, Player, Move) :-
	get_goal(Goal),
	cell_is_empty(Board, Move),
	set_cell_content(Board, Move, Player, NewBoard),
	evaluate_score(NewBoard, Player, Score),
	Score >= Goal.

% Permet de créer l'empreinte d'un état:
hash_pos(Board-_-_, Hash) :-
	hash_function(Board, Hash).

% Permet de créer l'empreinte d'une configuration du plateau:
hash_function(Board, Hash) :-
	flatten(Board, GridString),
	hash_function(GridString, 0, Hash).
hash_function([], Hash, Hash).
hash_function([C|Cs], Acc, Hash) :-
	char_code(C, Code),
	NewAcc is ((Acc << 5) - Acc) + Code,
	hash_function(Cs, NewAcc, Hash).
	