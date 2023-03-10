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

% Le joueur noir maximise, le joueur blanc minimise:
players_sign(n, 1) :- !.
players_sign(b, -1) :- !.

% Avantage du prochain tour:
advantage(Playing, Player, Factor) :- dif(Playing, Player) -> Factor is 10 ; Factor is 1, !.

% Évalue la valeur heuristique d'un état:
heuristic_score(Board-Player-_, Score) :-
    get_all_lines(Board, Lines),
    clumped(Lines, RLE),
    sum_score(Player, RLE, Score), !.

% Calcule la somme du score sur une ligne:
sum_score(Player, Line, Score) :-
    sum_score(Player, x, Line, 0, Score).
sum_score(Player, PS, [S1-N1, S2-N2, S3-N3|Rest], PreviousScore, Score) :-
    (
        (
            get_goal(Goal),
            (
                ( length(Rest, L), L >= 1 ) ->
                Rest = [NS-_|_]
                ;
                NS = x
            ),
            (
                is_a_winning_row(Goal, PS, S1-N1, S2-N2, S3-N3, NS), value(win, N2, Value), Playing = S2
                ;
                is_an_opened_row(Goal, PS, S1-N1, S2-N2, S3-N3, NS), value(opened, N2, Value), Playing = S2
                ;
                is_a_closed_row(Goal, PS, S1-N1, S2-N2, S3-N3, NS), value(closed, N2, Value), Playing = S2
                ;
                is_a_semi_opened_row_3(Goal, PS, S1-N1, S2-N2, S3-N3, NS), N is N1 + N3, value(semi_opened3, N, Value), Playing = S1
                ;
                is_a_semi_opened_row_2(Goal, PS, S1-N1, S2-N2, S3-N3, NS), N is N1 + N3, value(semi_opened2, N, Value), Playing = S1
                ;
                is_a_semi_opened_row_1(Goal, PS, S1-N1, S2-N2, S3-N3, NS), N is N1 + N3, value(semi_opened1, N, Value), Playing = S1
            ),
            players_sign(Playing, Sign),
            advantage(Playing, Player, Factor),
            NewScore is PreviousScore + Sign * Factor * Value
        )
        ;
        (
            NewScore is PreviousScore
        )
    ), !,
    sum_score(Player, S1, [S2-N2, S3-N3|Rest], NewScore, Score).
sum_score(_, _, _, Score, Score) :- !.

% Reconnaissance des patrons:
is_a_winning_row(Goal, _, _, S2-N2, _, _) :- member(S2, [b, n]), Goal =< N2, !.                                                     % Rangée gagnante.

is_an_opened_row(Goal, _, v-N1, S2-N2, v-N3, _) :- member(S2, [b, n]), dif(N1, 1), dif(N3, 1), Goal =< N1 + N2 + N3, !.             % Rangée ouverte avec deux espaces vides ou plus de chaque côtés.
is_an_opened_row(Goal, PS, v-1, S2-N2, v-N3, _) :- member(S2, [b, n]), dif(PS, S2), dif(N3, 1), Goal =< 1 + N2 + N3, !.             % Rangée ouverte avec un espace vide à gauche et deux espaces vides ou plus à droite.
is_an_opened_row(Goal, _, v-N1, S2-N2, v-1, NS) :- member(S2, [b, n]), dif(N1, 1), dif(NS, S2), Goal =< N1 + N2 + 1, !.             % Rangée ouverte avec un espace vide à droite et deux espaces vides ou plus à gauche.
is_an_opened_row(Goal, PS, v-1, S2-N2, v-1, NS) :- member(S2, [b, n]), dif(PS, S2), dif(NS, S2), Goal =< N2 + 2, !.                 % Rangée ouverte avec un seul espace vide de chaque côtés.

is_a_closed_row(Goal, _, v-N1, S2-N2, S3-_, _) :- member(S2, [b, n]), dif(N1, 1), dif(S3, v), Goal =< N1 + N2, !.                   % Rangée fermée à droite avec deux espaces vides ou plus à gauche.
is_a_closed_row(Goal, _, S1-_, S2-N2, v-N3, _) :- member(S2, [b, n]), dif(S1, v), dif(N3, 1), Goal =< N2 + N3, !.                   % Rangée fermée à gauche avec deux espaces vides ou plus à droite.
is_a_closed_row(Goal, PS, v-1, S2-N2, S3-_, _) :- member(S2, [b, n]), dif(PS, S2), dif(S3, v), Goal =< 1 + N2, !.                   % Rangée fermée à droite avec un seul espace vide à gauche.
is_a_closed_row(Goal, _, S1-_, S2-N2, v-1, NS) :- member(S2, [b, n]), dif(S1, v), dif(NS, S2), Goal =< 1 + N2, !.                   % Rangée fermée à gauche avec un seul espace vide à droite.

is_a_semi_opened_row_3(Goal, v, P-N1, v-1, P-N3, v) :- member(P, [b, n]), Goal =< N1 + 1 + N3, !.                                   % Rangée semi-ouverte des deux côtés.
is_a_semi_opened_row_2(Goal, PS, P-N1, v-1, P-N3, NS) :- member(P, [b, n]), ( dif(PS, v) ; dif(NS, v) ), Goal =< N1 + 1 + N3, !.    % Rangée semi-ouverte d'un seul côté.
is_a_semi_opened_row_1(Goal, PS, P-N1, v-1, P-N3, NS) :- member(P, [b, n]), dif(PS, v), dif(NS, v), Goal =< N1 + 1 + N3, !.         % Rangée semi-ouverte au centre seulement.

% Pour évaluer la valeur d'un patron:
value(win, N, Value) :-          Value is 3 * 10**(N - 1), !.
value(opened, N, Value) :-       Value is 3 * 10**(N - 1), !.
value(closed, N, Value) :-       Value is 1 * 10**(N - 1), !.
value(semi_opened3, N, Value) :- Value is 2 * 10**(N - 1), !.
value(semi_opened2, N, Value) :- Value is 8 * 10**(N - 2), !.
value(semi_opened1, N, Value) :- Value is 5 * 10**(N - 2), !.
