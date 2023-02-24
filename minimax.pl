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
%          Algorithme Minimax .        %
%======================================%

% Permet de passer du mode de maximisation à celui de minimisation:
switch(max, min).
switch(min, max).

% Permet de trouver la meilleure action pour un joueur:
minimax(Board, Player, Goal, MaxDepth, BestMove) :-
	get_possible_moves(Board, PossibleMoves),
	random_permutation(PossibleMoves, PossibleMovesShuffled),
	findall(Score-Move,
			(
				member(Move, PossibleMovesShuffled),
				minimax(Board, Player, Goal, max, MaxDepth, Move, Score)
			),
			Options),
	sort(1, @>, Options, [_-BestMove|_]).

% La profondeure maximale de recherche a été atteinte:
minimax(Board, Player, Goal, _, RemainingDepth, _, Score) :-
	RemainingDepth =< 0 ->
	(
		heuristic_value(Board, Player, Goal, Score),
		!
	).

% Un des joueur a gagné:
minimax(Board, Player, Goal, _, _, _, Score) :-
	heuristic_value(Board, Player, Goal, Score).

% Le plateau est plein, c'est un impasse:
minimax(Board, _, _, _, _, _, 0) :-
	not(has_an_empty_cell(Board)), !.

% Effectue une itération de minimax:
minimax(Board, Player, Goal, Maximizing, RemainingDepth, Move, BestScore) :-
	make_a_move(Board, Player, Move, NewBoard),
	other(Player, Opponent),
	get_possible_moves(NewBoard, PossibleMoves),
	random_permutation(PossibleMoves, PossibleMovesShuffled),
	switch(Maximizing, NegMaximizing),
	NewRemainingDepth is RemainingDepth - 1,
	findall(Score,
			(
				member(OpponentsMove, PossibleMovesShuffled),
				minimax(NewBoard, Opponent, Goal, NegMaximizing, NewRemainingDepth, OpponentsMove, Score)
			),
			Scores),
	(
		length(Scores, 0) ->
		BestScore is 0
		;
		(
			Maximizing = max ->
			(
				max_list(Scores, Max),
				BestScore is Max
			)
			;
			(
				min_list(Scores, Min),
				BestScore is Min
			)
		)
	).
