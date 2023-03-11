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
:- [src/game_components/board].
:- [src/game_components/user_interface].
:- [src/agent].

use_module(library(csv)).


% Paramètres choisis par l'utilisateur:
:- dynamic player/1. % Couleur de l'utilisateur.

% Démarre le jeu avec paramétrage:
play :-
    set_board_size(BoardSize),
    request_goal(BoardSize, Goal),
    request_players_color,
    begin_game(Firstplayer, Goal, BoardSize, Board),
    turn(Board, Firstplayer, _, [], X).

% Démarre le jeu selon les paramètres typiques:
gomoku(Size) :-
    request_players_color,
    begin_game(Firstplayer, 5, Size, Board),
    turn(Board, Firstplayer, _, [], X).

% Implémentation de Tic-Tac-Toe (bonus):
tictactoe :-
    request_players_color,
    begin_game(Firstplayer, 3, 3, Board),
    turn(Board, Firstplayer, _, [], X).


% Établi la routine de début de partie:
begin_game(Firstplayer, Goal, BoardSize, Board) :-
    set_goal(Goal),
    create_gomoku_board(BoardSize, Board),
    Firstplayer = n,
    cls,
    display_gomoku_board(Board).

% Établi la routine de fin de partie:
end_game :-
    retractall(goal(_)),
    request_continue_playing.

% Établi un tour complet et boucle jusqu'à ce que le jeu termine:
turn(Board, Player, NewBoard, Data, X) :-
    % Annonce le prochain tour:
    introduce_turn(Player, StartTime),
    (   % Vérifie s'il reste un emplacement vide:
        has_an_empty_cell(Board) ->
        (
            (   % Vérifie à qui le tour appartient:
                player(Player) ->
                (   % Le joueur choisi la case à jouer:
                    request_next_move(Board, Move)
                )
                ;
                (   % L'ordinateur choisi la case à jouer:
                    agent(Board, Player, Move)
                )
            ),
            make_a_move(Board, Player, Move, NewBoard)
        )
        ;
        % Aucun emplacement vide, c'est un impasse:
        csv_write_file('games.csv', Data),
        display_tie
    ),
    % Conclu le tour:
    conclude_turn(NewBoard-Player-Move, NextPlayer, StartTime, Data, X),
    print(X),
    % Récursion jusqu'à l'atteinte d'un état final:
    turn(NewBoard, NextPlayer, _, X, X1).

:- initialization welcome_screen.
