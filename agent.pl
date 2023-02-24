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


% L'agent choisit de jouer dans (Row, Col):
agent(Board, Player, Goal, Move) :-
	%other(Player, OtherPlayer),
	(
%		% Vérifie s'il est possible de gagner sur ce tour:
%		winning_move(Board, Player, Goal, Move)
%		;
%		% Vérifie s'il est possible de perdre au prochain tour:
%		winning_move(Board, OtherPlayer, Goal, Move)
%		;
		% Algorithme Alpha-Bêta:
		alphabeta(Board-Goal-nil, -inf, inf, _-_-Move, _)
	).

% Heuristique qui permet d'évaluer la valeur d'un état:
heuristic_value(Board, Player, Goal, Value) :-
	game_over(Board, Goal, Winner),
	(
		Winner = nil ->
		Value is 0
		;
		(
			Winner = Player ->
			Value is 1
			;
			Value is -1
		)
	).

% Établi les transitions possibles à partir d'un état:
moves(Board-Goal-LastMove, PosList) :-
	not(game_over(Board, Goal, _)),
	(
		% Récupère les cases non utilisées:
		get_possible_moves(Board, PossibleMoves),
		sort(0, @=<, PossibleMoves, Sorted),
		% Mélange l'ordre des cases pour éviter l'aspect prévisibile:
		%random_permutation(PossibleMoves, PossibleMovesShuffled),
		% Détermine à qui le tour appartient:
		(
			max_to_move(Board-_-LastMove) ->
			Player = n
			;
			Player = b
		),
		% Construit la liste des transitions possibles:
		%bagof(NewBoard-Goal-Move,
		setof(NewBoard-Goal-Move,
			(
				member(Move, Sorted), %PossibleMovesShuffled),
				make_a_move(Board, Player, Move, NewBoard)
			), PosList)
	).

% Évalue la valeur d'un état (Maximiseur = n; Minimiseur = b):
staticval(Board-Goal-_, Val) :-
	heuristic_value(Board, n, Goal, Val).

% Est-ce le tour à l'adversaire?
min_to_move(Pos) :-
	not(max_to_move(Pos)).

% Est-ce le tour à l'agent intelligent?
max_to_move(Board-_-Move) :-
	(	% L'agent intelligent vient d'être activé;
		% On détermine à qui appartient le tour:
		Move = nil,
		count_cells(Board, [N]>>(N = n), NCount),
		count_cells(Board, [B]>>(B = b), BCount),
		BCount =< NCount
	)
	;
	(	% On vérifie si c'était le joueur blanc qui a joué au dernier tour:
		get_cell_content(Board, Move, LastPlayer),
		LastPlayer = b
	).
