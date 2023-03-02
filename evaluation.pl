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
%            Évaluation du score.           %
%===========================================%


:- [board].

:- dynamic memo_score/3.	% Mémoïsation du score pour une configuration.
:- dynamic goal/1.  		% Nombre de pions à aligner pour gagner.

% Assigne un but (nombre de pions à aligner):
set_goal(Goal) :-
	assertz(goal(Goal)).

% Récupère le but:
get_goal(Goal) :-
	goal(Goal).

% Récupère l'alignement de jetons le plus long mémoïsé pour un joueur:
evaluate_score(Board, Player, BestScore) :-
	member(Player, [n, b]),
	memo_score(Board, Player, BestScore),
	!.

% Évalue le score d'un joueur, soit l'alignement de jetons le plus long:
evaluate_score(Board, Player, Score) :-
	member(Player, [n, b]),
	get_last_index(Board, LastIndex),
	LastIndex_1 is LastIndex - 1,
	setof(Streak,
		(R)^(
			between(0, LastIndex, R),
			check_direction(Board, Player, R-0, 0-1, 0, 0, Streak)
		),
		HorizontalStreaks),
	setof(Streak,
		(C)^(
			between(0, LastIndex, C),
			check_direction(Board, Player, 0-C, 1-0, 0, 0, Streak)
		),
		VerticalStreaks),
	setof(Streak,
		(R, C)^(
			between(0, LastIndex_1, R),
			between(0, LastIndex_1, C),
			(
				(R = 0 ; C = 0) ->
				true
				;
				(R = 0)
				;
				(C = 0)
		),
		check_direction(Board, Player, R-C, 1-1, 0, 0, Streak)
	),
	DiagonalDownStreaks),
	setof(Streak,
		(R, C)^(
			between(1, LastIndex, R),
			between(0, LastIndex_1, C),
			(
				(R = LastIndex ; C = 0) ->
				true
				;
				(R = LastIndex)
				;
				(C = 0)
			),
			check_direction(Board, Player, R-C, -1-1, 0, 0, Streak)
		),
		DiagonalUpStreaks),
	!,
	flatten([HorizontalStreaks, VerticalStreaks, DiagonalDownStreaks, DiagonalUpStreaks], Streaks),
	max_list(Streaks, Score),
	assertz(memo_score(Board, Player, Score)).

% Évalue l'alignement de jetons le plus long dans une direction donnée à partir c'une case:
check_direction(Board, Player, R-C, StepR-StepC, Streak, PreviousLongestStreak, LongestStreak) :-
	(
		get_cell_content(Board, R-C, Content), !,
		(
			Content = Player ->
			(
				CurrentStreak is Streak + 1,
				(
					CurrentStreak > PreviousLongestStreak ->
					NewLongestStreak = CurrentStreak
					;
					NewLongestStreak = PreviousLongestStreak
				)
			)
			;
			(
				CurrentStreak is 0,
				NewLongestStreak = PreviousLongestStreak
			)
		),
		NewR is R + StepR,
		NewC is C + StepC,
		check_direction(Board, Player, NewR-NewC, StepR-StepC, CurrentStreak, NewLongestStreak, LongestStreak)
	).
check_direction(_, _, _, _, _, PreviousLongestStreak, PreviousLongestStreak).

% Évalue le score heuristique:
heuristic_score(Board, TotalScore) :-
	get_goal(Goal),
	get_last_index(Board, LastIndex),
	get_horizontal_lines(Board, HorizontalLines),
	get_vertical_lines(Board, VerticalLines),
	findall(Score,
				(member(Line, HorizontalLines),
				line_score(Line, Score)),
				HorizontalLinesScores),
	findall(Score,
				(member(Line, VerticalLines),
				line_score(Line, Score)),
				VerticalLinesScores),
	M is Goal - 1,
	N is LastIndex - M,
	findall(Score,
				(between(0, N, R),
				between(0, N, C),
				(
					(R = 0 ; C = 0) ->
					true
					;
					(R = 0)
					;
					(C = 0)
				),
				get_line(Board, R-C, 1-1, [], Line),
				line_score(Line, Score)),
				DiagonalLinesDownScores),
	findall(Score,
				(between(M, LastIndex, R),
				between(0, N, C),
				(
					(R = LastIndex ; C = 0) ->
					true
					;
					(R = LastIndex)
					;
					(C = 0)
				),
				get_line(Board, R-C, -1-1, [], Line),
				line_score(Line, Score)),
				DiagonalLinesUpScores),
	!,
	flatten([HorizontalLinesScores, VerticalLinesScores, DiagonalLinesDownScores, DiagonalLinesUpScores], Scores),
	sum_list(Scores, TotalScore).

line_score(Line, Score) :-
	get_goal(Goal),
	findall(Value, (
		Goal2 is Goal + 2,
		between(1, Goal2, L),
		member(P-S, [n-1, b-(-1)]),
		phrase(fixed_open_rep(P, L, Goal2), Line),
		value(L, o, V),
		Value is S * V
	), OpenVals),
	findall(Value, (
		Goal1 is Goal + 1,
		between(2, Goal1, L),
		member(P-S, [n-1, b-(-1)]),
		phrase(fixed_closed_rep(P, L, Goal1), Line),
		value(L, c, V),
		Value is S * V
	), ClosedVals),
	findall(Value, (
		member(P-S, [n-1, b-(-1)]),
		phrase(full_rep(P, Goal), Line),
		value(Goal, f, V),
		Value is S * V
	), FullVals),
	flatten([OpenVals, ClosedVals, FullVals], Values),
	sum_list(Values, Score).

value(1, o, 0.01) :- !.
value(Goal_1, o, Value) :- get_goal(Goal), Goal_1 is Goal - 1, Value is (4**Goal_1) + 999999, !.
value(L, o, Value) :- Value is (4**L), !.
value(Goal_1, c, Value) :- get_goal(Goal), Goal_1 is Goal - 1, Value is ((4**Goal_1) + 1000000)/2, !.
value(L, c, Value) :- Value is (4**(L - 1)), !.
value(L, f, Value) :- Value is (4**L) + 1000000, !.

% Vérifie si la partie est terminée:
game_over(Board, Winner) :-
	get_goal(Goal),
	member(Winner, [n, b]),
	evaluate_score(Board, Winner, Score),
	Score >= Goal, !
	;
	not(has_an_empty_cell(Board)) -> Winner = nil.
