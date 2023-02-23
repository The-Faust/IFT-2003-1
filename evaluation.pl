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
%         Évaluation du score.         %
%======================================%

% Évalue le score d'un joueur, soit l'alignement de jetons le plus long:
evaluate_score(Board, Player, BestScore) :-
	setof(Score,
			(StepR, StepC)^(member((StepR, StepC), [(0, 1), (1, 0), (1, 1), (1, -1)]),
							check_direction(Board, Player, StepR-StepC, Score)),
			Scores),
	max_list(Scores, BestScore).

% Évalue le meilleur score dans la direction donnée:
check_direction(Board, Player, Direction, BestScore) :-
	setof(StreakScore,
		NextMove^(
			get_cell_content(Board, NextMove, Player),
			(
				evaluate_score_helper(Board, Player, NextMove, Direction, 1, 1, StreakScore)
			)
			;
			StreakScore is 0
		),
		Scores
	),
	max_list(Scores, BestScore).

% Fonction utilitaire pour check_direction(Board, Player, Direction, BestScore):
evaluate_score_helper(_, _, _, _, _, BestScore, BestScore).
evaluate_score_helper(Board, Player, R-C, StepR-StepC, ActualScore, PreviousBestScore, BestScore) :-
	R1 is R + StepR,
	C1 is C + StepC,
	are_valid_coordinates(Board, R1-C1),
	get_cell_content(Board, R1-C1, Cell),
	(
		Cell = Player ->
		NewScore is ActualScore + 1,
		(
			PreviousBestScore < NewScore ->
			NewBestScore is NewScore
			;
			NewBestScore is PreviousBestScore
		),
		NewBestScore is NewScore
		;
		NewScore is 0,
		NewBestScore is PreviousBestScore,
		!,
		fail
	),
	evaluate_score_helper(Board, Player, R1-C1, StepR-StepC, NewScore, NewBestScore, BestScore).
	