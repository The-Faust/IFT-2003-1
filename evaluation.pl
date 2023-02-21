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

%===================================%
%     Utilitaires d'évaluation.     %
%===================================%

% Évalue la l'alignement de jetons le plus long pour un joueur:
longest_alignment(Board, Player, LongestCount) :-
	Directions = [[0,1], [1,0], [1,1], [1,-1]],
	findall(AlignmentLength, (
		member(Direction, Directions),
		check_direction(Board, Player, Direction, AlignmentLength)
	), AlignmentLengths),
	max_list(AlignmentLengths, LongestCount).

% Évalue la l'alignement de jetons dans la direction donnée:
check_direction(Board, Player, Direction, LongestCount) :-
	findall(AlignmentLength, (
		nth0(R, Board, Row),
		nth0(C, Row, Cell),
		(
			Cell = Player ->
			check_direction_helper(Board, Player, R, C, Direction, AlignmentLength)
		)
	), AlignmentLengths),
	max_list(AlignmentLengths, LongestCount).

% Fonction utilitaire pour check_direction(Board, Player, Direction, LongestCount):
check_direction_helper(Board, Player, R, C, Direction, LongestCount) :-
	nth0(0, Direction, StepR),
	nth0(1, Direction, StepC),
	get_cell_content(Board, R, C, Cell),
	Cell = Player ->
	longest_alignment_helper(Board, Player, R, C, StepR, StepC, 1, 1, LongestCount)
	;
	LongestCount is 0.

% Fonction utilitaire pour check_direction_helper(Board, Player, R, C, Direction, LongestCount):
longest_alignment_helper(Board, Player, R, C, StepR, StepC, ActualCount, PreviousLongestCount, LongestCount) :-
	R1 is R + StepR,
	C1 is C + StepC,
	are_valid_coordinates(Board, R1, C1) ->
	(
		get_cell_content(Board, R1, C1, Cell),
		(
			Cell = Player ->
			NewCount is ActualCount + 1,
			(
				PreviousLongestCount < NewCount -> 
				NewLongestCount is NewCount
				;
				NewLongestCount is PreviousLongestCount
			),
			NewLongestCount is NewCount
			;
			NewCount is 0,
			NewLongestCount is PreviousLongestCount
		)
	),
	longest_alignment_helper(Board, Player, R1, C1, StepR, StepC, NewCount, NewLongestCount, LongestCount)
	;
	LongestCount is PreviousLongestCount.
