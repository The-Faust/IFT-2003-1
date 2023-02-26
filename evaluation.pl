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

% Vérifie si la partie est terminée:
game_over(Board, Winner) :-
	get_goal(Goal),
	member(Winner, [n, b]),
	evaluate_score(Board, Winner, Score),
	Score >= Goal, !
	;
	not(has_an_empty_cell(Board)) -> Winner = nil.
