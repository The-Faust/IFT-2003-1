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
%                     Engin du jeu.                  %
%====================================================%


% Chargement des modules:
:- [board].
:- [user_interface].
:- [agent].
:- ['../evaluation_algorithms/static_evaluation'].
:- ['../evaluation_algorithms/heuristic_evaluation'].
:- ['../search_algorithms/minimax'].
:- ['../search_algorithms/alphabeta'].
:- ['../search_algorithms/bounded_alphabeta'].

% Paramètres choisis par l'utilisateur:
:- dynamic player/1. % Couleur de l'utilisateur.

% Prédicat qui permet d'alterner les joeurs:
other(b, n).
other(n, b).

% Établi la routine de début de partie:
begin_game(Firstplayer, Goal, BoardSize, Board) :-
    % Défini le but:
    set_goal(Goal),
    % Créer le plateau de jeu:
    create_gomoku_board(BoardSize, Board),
    % Le premier à jouer est le joueur noir:
    Firstplayer = n,
    % Efface le contenu de l'écran:
    cls,
    % Affiche le plateau de jeu:
    display_gomoku_board(Board).

% Établi la routine de fin de partie:
end_game :-
    % Réinitialise le but:
    retractall(goal(_)),
    % Demande au joueur s'il veut retourner au menu:
    request_continue_playing.

% Établi un tour complet et boucle jusqu'à ce que le jeu termine:
turn(Board, Player, NewBoard) :-
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
            % Effectue l'action du joueur:
            make_a_move(Board, Player, Move, NewBoard)
        )
        ;
        % Aucun emplacement vide, c'est un impasse:
        display_tie
    ),
    % Conclu le tour:
    conclude_turn(NewBoard-Player-Move, NextPlayer, StartTime),
    % Récursion jusqu'à l'atteinte d'un état final:
    turn(NewBoard, NextPlayer, _).
    