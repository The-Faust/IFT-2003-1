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
%     Évaluation heuristique du score.      %
%===========================================%


:- [board].
:- [static_evaluation].

:- dynamic memo_heuristic_score/3.	% Mémoïsation du score pour une configuration.

% Récupère l'alignement de jetons le plus long mémoïsé pour un joueur:
heuristic_score(Board, TotalScore) :-
	get_goal(Goal),
	hash_function(Board, Hash),
	memo_heuristic_score(Hash, Goal, TotalScore),
	!.

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
	sum_list(Scores, TotalScore),
	hash_function(Board, Hash),
	assertz(memo_heuristic_score(Hash, Goal, TotalScore)).

% Évalue le score pour une ligne donnée:
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

% Établi la valeur d'une séquence selon le type (o: ouverte, c: fermée, f: complète):
value(1, o, 0.01) :- !.
value(Goal_1, o, Value) :- get_goal(Goal), Goal_1 is Goal - 1, Value is (4**(Goal_1**2)) + 999999, !.
value(L, o, Value) :- Value is (4**(L**2)), !.
value(Goal_1, c, Value) :- get_goal(Goal), Goal_1 is Goal - 1, Value is ((4**(Goal_1**2)) + 1000000)/2, !.
value(L, c, Value) :- Value is (4**((L - 1)**2)), !.
value(L, f, Value) :- Value is (4**(L**2)) + 1000000, !.
