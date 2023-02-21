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
%     Prédicats de fin de jeu.      %
%===================================%

:- use_module(library(clpfd)).  % Pour: transpose.

% Vérifie s'il y a un joueur a aligné suffisament de jetons:
win(Board, Player, Length) :-
    (
        row_win(Board, Player, Length)
    ;
        column_win(Board, Player, Length)
%   ;
%       diagonal_win(Board, Player, Length)
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
    
% diagonal_win(Board, Player, Length) :- À implémenter!
