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
heuristic_score(Board-_-_, TotalScore) :-
	get_goal(Goal),
	hash_function(Board, Hash),
	memo_heuristic_score(Hash, Goal, TotalScore),
	!.

heuristic_score(Board-Player-_, TotalScore) :-
	get_goal(Goal),
	get_all_lines(Board, Lines),
	line_score(Lines, Player, TotalScore),
	!,
	hash_function(Board, Hash),
	assertz(memo_heuristic_score(Hash, Goal, TotalScore)).

line_score(Line, Player, Score) :-
	(
		get_goal(Goal),
		clumped(Line, RLE),
		aggregate_all(sum(PartialScore), (
			member(Playing-Sign, [n-1, b-(-1)]),
			pattern(Playing, Pattern, Type, N, Goal),
			sublist(RLE, Pattern),
			( Playing = Player -> Factor is 1 ; Factor is 10 ),
			value(Type, N, Goal, Value),
			PartialScore is Sign * Factor * Value
		), Score)
	), !.

sublist(List, Sublist) :-
	append(_, Rest, List),
	append(Sublist, _, Rest).

pattern(Playing, [v-A, Playing-N, v-B], opened, N, Goal) :- Goal #=< A + N + B, dif(A, 1), dif(B, 1).
pattern(Playing, [P-_, v-1, Playing-N, v-B], opened, N, Goal) :- Goal #=< 1 + N + B, dif(P, Playing).
pattern(Playing, [v-A, Playing-N, v-1, Q-_], opened, N, Goal) :- Goal #=< A + N + 1, dif(Q, Playing).
pattern(Playing, [P-_, v-1, Playing-N, v-1, Q-_], opened, N, Goal) :- Goal #=< N + 2, dif(P, Playing), dif(Q, Playing).

pattern(Playing, [v-A, Playing-N, Y-_], closed, N, Goal) :- other(Playing, Other), member(Y, [x, Other]), Goal #=< A + N, dif(A, 1).
pattern(Playing, [X-_, Playing-N, v-B], closed, N, Goal) :- other(Playing, Other), member(X, [x, Other]), Goal #=< N + B, dif(B, 1).
pattern(Playing, [P-_, v-1, Playing-N, Y-_], closed, N, Goal) :- other(Playing, Other), member(Y, [x, Other]), Goal #=< 1 + N, dif(P, Playing).
pattern(Playing, [X-_, Playing-N, v-1, Q-_], closed, N, Goal) :- other(Playing, Other), member(X, [x, Other]), Goal #=< N + 1, dif(Q, Playing).

pattern(Playing, [v-_, Playing-M, v-1, Playing-N, v-_], semi_opened3, S, Goal) :- S #= M + N, Goal #=< S + 1.
pattern(Playing, [P-_, Playing-M, v-1, Playing-N, Q-_], semi_opened2, S, Goal) :- S #= M + N, Goal #=< S + 1, ( dif(P, v) ; dif(Q, v) ).
pattern(Playing, [P-_, Playing-M, v-1, Playing-N, Q-_], semi_opened1, S, Goal) :- S #= M + N, Goal #=< S + 1, ( dif(P, v) , dif(Q, v) ).

value(opened, N, Goal, Value) :- Value is 20000/(10**(Goal - N)), !.
value(closed, N, Goal, Value) :- Value is 8000/(10**(Goal - N)), !.
value(semi_opened3, N, Goal, Value) :- Value is 18000/(10**(Goal - N)), !.
value(semi_opened2, N, Goal, Value) :- Value is 6000/(10**(Goal - N)), !.
value(semi_opened1, N, Goal, Value) :- Value is 4000/(10**(Goal - N)), !.
