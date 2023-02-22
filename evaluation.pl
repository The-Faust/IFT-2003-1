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
	Directions = [[0,1], [1,0], [1,1], [1,-1]],
	findall(Score,
		(
			member(Direction, Directions),
			check_direction(Board, Player, Direction, Score)
		),
		Scores
	),
	max_list(Scores, BestScore).

% Évalue le meilleur score dans la direction donnée:
check_direction(Board, Player, Direction, BestScore) :-
	(
		nth0(0, Direction, StepR),
		nth0(1, Direction, StepC),
		findall(Score,
			(
				nth0(R, Board, Row),
				nth0(C, Row, Cell),
				(
					Cell = Player ->
					evaluate_score_helper(Board, Player, R, C, StepR, StepC, 1, 1, Score)
				;
					Score is 0
				)
			),
			Scores
		),
		max_list(Scores, BestScore)
	).

% Fonction utilitaire pour check_direction(Board, Player, Direction, BestScore):
evaluate_score_helper(Board, Player, R, C, StepR, StepC, ActualScore, PreviousBestScore, BestScore) :-
	R1 is R + StepR,
	C1 is C + StepC,
	are_valid_coordinates(Board, R1, C1) ->
	(
		get_cell_content(Board, R1, C1, Cell),
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
			NewBestScore is PreviousBestScore
		)
	),
	evaluate_score_helper(Board, Player, R1, C1, StepR, StepC, NewScore, NewBestScore, BestScore)
	;
	BestScore is PreviousBestScore.
