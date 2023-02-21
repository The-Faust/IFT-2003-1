% Travail pratique 1 : Gomoku
% Conception d'un jeu intelligent
%
% remis par
%   Omar Akrout    (NI: 111 165 246)
%   René Chenard   (NI: 111 232 277)
% et
%   Vincent Martel (NI: 111 105 797)
%
% dans le cadre du cours
%   IFT-2003 - Intelligence artificielle I
%   Hiver 2023

%===================================%
%     Prédicats de fin de jeu.      %
%===================================%

:- use_module(library(clpfd)).  % Pour: transpose.
:- [board].

% Vérifie s'il y a un joueur a aligné suffisament de jetons:
win(Board, Player, Length) :-
    (
        row_win(Board, Player, Length)
    ;
        column_win(Board, Player, Length)
    ;
        diagonal_win(Board, Player, Length)
    )
    ->
    (
        players_name(Player, PlayersName),
        display_gomoku_board(Board),
        format('Le joueur ~w gagne!\n', [PlayersName]),
        halt
    ).

% Vérifie s'il y a un joueur a aligné suffisament de jetons horizontalement:
row_win(Board, Player, Length) :-
    findall(Player, between(1, Length, _), Sequence),
    Sequence = [_|_],
    call_nth((nth1(_, Board, Row), append(_, Zs, Row), append(Sequence, _, Zs)), 1).

% Vérifie s'il y a un joueur a aligné suffisament de jetons verticalement:
column_win(Board, Player, Length) :-
    transpose(Board, TransposedBoard),
    row_win(TransposedBoard, Player, Length).
    
% Vérifie s'il y a un joueur a aligné suffisament de jetons diagonalement:
diagonal_win(Board, Player, Length) :-
    length(Board, N),
    N_1 is N - 1,
    (
        diagonal_win_helper(Board, Player, Length, 1)
        ;
        diagonal_win_helper(Board, Player, Length, -1)
    ).

% Fonction utilitaire pour diagonal_win(Board, Player, Length):
diagonal_win_helper(Board, Player, Length, Step) :-
    nth0(R, Board, Row),
    nth0(C, Row, Cell),
    (
        Cell = Player ->
        diagonal_win_helper_helper(Board, Player, Length, R, C, Step, 1)
    ).

% Fonction utilitaire pour diagonal_win_helper(Board, Player, Length, Step):
diagonal_win_helper_helper(Board, Player, Length, R, C, Step, Count) :-
    R1 is R + Step,
    C1 is C + 1,
    get_cell_content(Board, R1, C1, Cell),
    (Cell = Player -> NewCount is Count + 1 ; NewCount is 0),
    (
        NewCount >= Length ->
        true
        ;
        (
            are_valid_coordinates(Board, R1, C1) ->
            diagonal_win_helper_helper(Board, Player, Length, R1, C1, Step, NewCount)
        )
    ).
