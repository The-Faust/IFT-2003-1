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


:- set_prolog_flag(verbose, silent).

% Chargement des modules:
:- ['./src/game_components/game_engine'].

% Démarre le jeu avec paramétrage:
play :-
    set_board_size(BoardSize),
    request_goal(BoardSize, Goal),
    request_players_color,
    begin_game(Firstplayer, Goal, BoardSize, Board),
    turn(Board, Firstplayer, _).

% Démarre le jeu selon les paramètres typiques:
gomoku(Size) :-
    request_players_color,
    begin_game(Firstplayer, 5, Size, Board),
    turn(Board, Firstplayer, _).

% Implémentation de Tic-Tac-Toe (bonus):
tictactoe :-
    request_players_color,
    begin_game(Firstplayer, 3, 3, Board),
    turn(Board, Firstplayer, _).

:- initialization welcome_screen.
