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

%====================================================%
%         Évaluation heuristique du score.           %
%====================================================%


:- [board].
:- [static_evaluation].

:- dynamic memo_heuristic_score/3.	% Mémoïsation du score pour une configuration.

% Récupère l'alignement de jetons le plus long mémoïsé pour un joueur:
heuristic_score(Board-_-_,Score) :-
	get_goal(Goal),
	hash_function(Board, Hash),
	memo_heuristic_score(Hash, Goal, Score),
	!.

heuristic_score(Board-Player-_, Score) :-
	get_goal(Goal),
	get_all_lines(Board, Lines),
	clumped(Lines, RLE),
	sum_score(Player, RLE, Score),
	!,
	hash_function(Board, Hash),
	assertz(memo_heuristic_score(Hash, Goal, Score)).

sum_score(Player, Line, Score) :-
	sum_score(Player, Line, 0, Score).
sum_score(_, [], Score, Score) :- !.
sum_score(Player, [X|Xs], PreviousScore, Score) :-
	get_goal(Goal),
	(
		( append(Pattern, _, [X|Xs]), pattern(Playing, Pattern, Type, N, Goal) ) ->
		(
			( Playing = Player -> Factor is 1 ; Factor is 10 ),
			( Playing = n -> Sign = 1 ; Sign = -1 ),
			value(Type, N, Goal, Value),
			NewScore is PreviousScore + Sign * Factor * Value
		)
		;
		(
			NewScore is PreviousScore
		)
	),
	sum_score(Player, Xs, NewScore, Score).

is_a_player(Symbol) :- dif(Symbol, x), dif(Symbol, v), !.

pattern(Playing, [v-A, Playing-N, v-B], opened, N, Goal) :- N #< Goal, Goal #=< A + N + B, dif(A, 1), dif(B, 1), is_a_player(Playing), !.
pattern(Playing, [P-_, v-1, Playing-N, v-B], opened, N, Goal) :- N #< Goal, Goal #=< 1 + N + B, dif(P, Playing), is_a_player(Playing), !.
pattern(Playing, [v-A, Playing-N, v-1, Q-_], opened, N, Goal) :- N #< Goal, Goal #=< A + N + 1, dif(Q, Playing), is_a_player(Playing), !.
pattern(Playing, [P-_, v-1, Playing-N, v-1, Q-_], opened, N, Goal) :- N #< Goal, Goal #=< N + 2, dif(P, Playing), dif(Q, Playing), is_a_player(Playing), !.

pattern(Playing, [v-A, Playing-N, Y-_], closed, N, Goal) :- N #< Goal, other(Playing, Other), member(Y, [x, Other]), Goal #=< A + N, dif(A, 1), is_a_player(Playing), !.
pattern(Playing, [X-_, Playing-N, v-B], closed, N, Goal) :- N #< Goal, other(Playing, Other), member(X, [x, Other]), Goal #=< N + B, dif(B, 1), is_a_player(Playing), !.
pattern(Playing, [P-_, v-1, Playing-N, Y-_], closed, N, Goal) :- N #< Goal, other(Playing, Other), member(Y, [x, Other]), Goal #=< 1 + N, dif(P, Playing), is_a_player(Playing), !.
pattern(Playing, [X-_, Playing-N, v-1, Q-_], closed, N, Goal) :- N #< Goal, other(Playing, Other), member(X, [x, Other]), Goal #=< N + 1, dif(Q, Playing), is_a_player(Playing), !.

pattern(Playing, [v-_, Playing-M, v-1, Playing-N, v-_], semi_opened3, S, Goal) :- S #= M + N, Goal #=< S + 1, is_a_player(Playing), !.
pattern(Playing, [P-_, Playing-M, v-1, Playing-N, Q-_], semi_opened2, S, Goal) :- S #= M + N, Goal #=< S + 1, ( dif(P, v) ; dif(Q, v) ), is_a_player(Playing), !.
pattern(Playing, [P-_, Playing-M, v-1, Playing-N, Q-_], semi_opened1, S, Goal) :- S #= M + N, Goal #=< S + 1, ( dif(P, v) , dif(Q, v) ), is_a_player(Playing), !.

pattern(Playing, [Playing-N], win, _, Goal) :- Goal #=< N, is_a_player(Playing), !.

value(win, _, Goal, Value) :- Value is 3 * 10**(Goal - 1), !.
value(opened, N, _, Value) :- Value is 3 * 10**(N - 1), !.
value(closed, N, _, Value) :- Value is 1 * 10**(N - 1), !.
value(semi_opened3, N, _, Value) :- Value is 2 * 10**(N - 1), !.
value(semi_opened2, N, _, Value) :- Value is 8 * 10**(N - 2), !.
value(semi_opened1, N, _, Value) :- Value is 5 * 10**(N - 2), !.