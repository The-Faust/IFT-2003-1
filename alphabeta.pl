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
%           Algorithme Alpha-Bêta .         %
%===========================================%


% Permet de passer du mode de maximisation à celui de minimisation:
switch(max, min).
switch(min, max).


alpha_beta(Board, Player, Goal, Maximizing, RemainingDepth, Alpha, Beta, BestMove, BestValue) :-
	RemainingDepth > 0,
	NewRemainingDepth is RemainingDepth - 1,
	get_possible_moves(Board, PossibleMoves),
	random_permutation(PossibleMoves, PossibleMovesShuffled),
	bounded_best(Board, Player, Goal, Maximizing, NewRemainingDepth, Alpha, Beta, PossibleMovesShuffled, BestMove, BestValue), !,
	format('alpha_beta(Board =~w, Player =~w, Goal =~w, Maximizing =~w, RemainingDepth =~w, Alpha =~w, Beta =~w, BestMove =~w, BestValue =~w)\n', [Board, Player, Goal, Maximizing, RemainingDepth, Alpha, Beta, BestMove, BestValue]).


alpha_beta(Board, Player, Goal, _, _, _, _, _, BestValue) :-
	heuristic_value(Board, Player, Goal, BestValue), !,
	format('alpha_beta(Board =~w, Player =~w, Goal =~w, _, _, _, _, _, BestValue =~w)\n', [Board, Player, Goal, BestValue]).
	
	
bounded_best(Board, Player, Goal, Maximizing, RemainingDepth, Alpha, Beta, [Move|PossibleMoves], BestMove, BestValue) :-
	make_a_move(Board, Player, Move, NewBoard),
	other(Player, Opponent),
	switch(Maximizing, NegMaximizing),
	alpha_beta(NewBoard, Opponent, Goal, NegMaximizing, RemainingDepth, Alpha, Beta, _, Value),
	good_enough(NewBoard, Player, Goal, Maximizing, RemainingDepth, Alpha, Beta, PossibleMoves, Move, Value, BestMove, BestValue),
	format('bounded_best(Board =~w, Player =~w, Goal =~w, Maximizing =~w, RemainingDepth =~w, Alpha =~w, Beta =~w, [Move|PossibleMoves] = [~w|~w], BestMove =~w, BestValue =~w)\n', [Board, Player, Goal, Maximizing, RemainingDepth, Alpha, Beta, Move, PossibleMoves, BestMove, BestValue]).


good_enough(_, _, _, _, _, _, _, [], BestMove, BestValue, BestMove, BestValue) :-
	!,
	format('good_enough(_, _, _, _, _, _, _, [], BestMove =~w, BestValue =~w, BestMove =~w, BestValue =~w)\n', [BestMove, BestValue, BestMove, BestValue]).


good_enough(_, _, _, min, _, Alpha, _, _, BestMove, BestValue, BestMove, BestValue) :-
	BestValue > Alpha, !,
	format('good_enough(_, _, _, min, _, Alpha =~w, _, _, BestMove =~w, BestValue =~w, BestMove =~w, BestValue =~w)\n', [Alpha, BestMove, BestValue, BestMove, BestValue]).


good_enough(_, _, _, max, _, _, Beta, _, BestMove, BestValue, BestMove, BestValue) :-
	BestValue < Beta, !,
	format('good_enough(_, _, _, max, _, _, Beta =~w, _, BestMove =~w, BestValue =~w, BestMove =~w, BestValue =~w)\n', [Beta, BestMove, BestValue, BestMove, BestValue]).


good_enough(Board, Player, Goal, Maximizing, RemainingDepth, Alpha, Beta, PossibleMoves, Move, Value, BestMove, BestValue) :-
	new_bounds(Maximizing, Alpha, Beta, Value, NewAlpha, NewBeta),
	bounded_best(Board, Player, Goal, Maximizing, RemainingDepth, NewAlpha, NewBeta, PossibleMoves, Move1, Value1),
	better_of(Maximizing, Move, Value, Move1, Value1, BestMove, BestValue),
	format('good_enough(Board =~w, Player =~w, Goal =~w, Maximizing =~w, RemainingDepth =~w, Alpha =~w, Beta =~w, PossibleMoves =~w, Move =~w, Value =~w, BestMove =~w, BestValue =~w)\n', [Board, Player, Goal, Maximizing, RemainingDepth, Alpha, Beta, PossibleMoves, Move, Value, BestMove, BestValue]).


new_bounds(min, Alpha, Beta, Value, Value, Beta) :-
	Value > Alpha, !,
	format('new_bounds(min, Alpha =~w, Beta =~w, Value =~w, Value =~w, Beta =~w)\n', [Alpha, Beta, Value, Value, Beta]).


new_bounds(max, Alpha, Beta, Value, Alpha, Value) :-
	Value < Beta, !,
	format('new_bounds(max, Alpha =~w, Beta =~w, Value =~w, Alpha =~w, Value =~w)\n', [Alpha, Beta, Value, Alpha, Value]).


new_bounds(_, Alpha, Beta, _, Alpha, Beta) :-
	format('new_bounds(_, Alpha =~w, Beta =~w, _, Alpha =~w, Beta =~w)\n', [Alpha, Beta, Alpha, Beta]).


better_of(min, Move, Value, _, Value1, Move, Value) :-
	Value > Value1, !,
	format('better_of(min, Move =~w, Value =~w, _, Value1 =~w, Move =~w, Value =~w)\n', [Move, Value, Value1, Move, Value]).


better_of(max, Move, Value, _, Value1, Move, Value) :-
	Value < Value1, !,
	format('better_of(max, Move =~w, Value =~w, _, Value1 =~w, Move =~w, Value =~w)\n', [Move, Value, Value1, Move, Value]).


better_of(_, _, _, Move1, Value1, Move1, Value1) :-
	format('better_of(_, _, _, Move1 =~w, Value1 =~w, Move1 =~w, Value1 =~w)\n', [Move1, Value1, Move1, Value1]).
