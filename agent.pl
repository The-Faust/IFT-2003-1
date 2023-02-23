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
%           Agent intelligent .        %
%======================================%

% Vérifie s'il est possible pour le joueur de gagner en un tour:
winning_move(Board, Player, Goal, Move) :-
	cell_is_empty(Board, Move),
	set_cell_content(Board, Move, Player, NewBoard),
	evaluate_score(NewBoard, Player, Goal).

make_move(Board, Player, Move, NewBoard) :-
	set_cell_content(Board, Move, Player, NewBoard).

% L'agent choisit de jouer dans (Row, Col):
agent(Board, Player, Goal, Move) :-
	other(Player, OtherPlayer),
	(
		% Vérifie s'il est possible de gagner sur ce tour:
		winning_move(Board, Player, Goal, Move)
		;
		% Vérifie s'il est possible de perdre au prochain tour:
		winning_move(Board, OtherPlayer, Goal, Move)
		;
		% Minimax:
		MaxDepth is 2,
		minimax(Board, Player, Goal, MaxDepth, Move)
	).

switch(max, min).
switch(min, max).

minimax(Board, Player, Goal, MaxDepth, BestMove) :-
	get_possible_moves(Board, PossibleMoves),
	findall(Score-Move,
			(
				member(Move, PossibleMoves),
				minimax(Board, Player, Goal, max, MaxDepth, Move, Score)
			),
			Options),
	sort(1, @>, Options, [_-BestMove|_]).
	%format('minimax(Board = ~w, Player = ~w, Goal = ~w, MaxDepth = ~w, BestMove = ~w)\n', [Board, Player, Goal, MaxDepth, BestMove]).

minimax(_, _, _, _, RemainingDepth, _, BestScore) :-
	RemainingDepth =< 0 -> BestScore is 0, !.
	%format('minimax(_, _, _, _, RemainingDepth = ~w, _, 0)\n', [RemainingDepth]), !.

minimax(Board, Player, Goal, _, _, _, BestScore) :-
	evaluate_score(Board, Winner, Goal),
	Winner = Player -> BestScore is 1; BestScore is -1.
	%format('minimax(Board = ~w, Player = ~w, Goal = ~w, _, _, _, BestScore = ~w)\n', [Board, Player, Goal, BestScore]).

minimax(Board, _, _, _, _, _, 0) :-
	count_empty_cells(Board, 0).
	%format('minimax(Board = ~w, _, _, _, _, _, 0)\n', [Board]).

minimax(Board, Player, Goal, Maximizing, RemainingDepth, Move, BestScore) :-
	make_move(Board, Player, Move, NewBoard),
	other(Player, Opponent),
	get_possible_moves(NewBoard, PossibleMoves),
	switch(Maximizing, NegMaximizing),
	NewRemainingDepth is RemainingDepth - 1,
	findall(Score,
			(
				member(OpponentsMove, PossibleMoves),
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
	%format('minimax(Board = ~w, Player = ~w, Goal = ~w, Maximizing = ~w, RemainingDepth = ~w, Move = ~w, BestScore = ~w)\n', [Board, Player, Goal, Maximizing, RemainingDepth, Move, BestScore]).
	