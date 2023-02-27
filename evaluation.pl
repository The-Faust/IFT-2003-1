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

set_goal(Goal) :-
	assertz(goal(Goal)).

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
	N is LastIndex - Goal + 1,
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
	sliding_window(Goal, Line, Scores),
	sum_list(Scores, Score).
	
sliding_window(WindowSize, Line, Scores) :-
	length(Line, Len),
	Len >= WindowSize,
	sliding_window(WindowSize, Line, 1, Len, Scores), !.
	
sliding_window(WindowSize, _, I, Len, []) :-
	I + WindowSize - 1 > Len.
sliding_window(WindowSize, Line, I, Len, [Score|Scores]) :-
	window_score(WindowSize, Line, I, Bs, Ns),
	eval_window_score(Bs-Ns, Score),
	I1 is I + 1,
	sliding_window(WindowSize, Line, I1, Len, Scores).
	
window_score(0, _, _, 0, 0).
window_score(WindowSize, Line, I, Bs, Ns) :-
	WindowSize > 0,
	N1 is WindowSize - 1,
	nth1(I, Line, X),
	I1 is I + 1,
	window_score(N1, Line, I1, Bs1, Ns1),
	(   X = b -> Bs is Bs1 + 1 ; Bs = Bs1 ),
	(   X = n -> Ns is Ns1 + 1 ; Ns = Ns1 ).
	
eval_window_score(B-0, Value) :- Value is -12**B.
eval_window_score(0-N, Value) :- Value is 12**N.
eval_window_score(_, 0).

% Vérifie si la partie est terminée:
game_over(Board, Winner) :-
	get_goal(Goal),
	member(Winner, [n, b]),
	evaluate_score(Board, Winner, Score),
	Score >= Goal, !
	;
	not(has_an_empty_cell(Board)) -> Winner = nil.
