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
%    Tests unitaires pour board.pl.                  %
%                                                    %
%    Exécuter les tests: ?- run_tests.               %
%====================================================%

:- [src/game_components/board].
:- [src/game_components/interface].

:- begin_tests(board).

test(create_board_5x5) :-
    create_gomoku_board(5, Board),
    assertion(Board = [[v,v,v,v,v],[v,v,v,v,v],[v,v,v,v,v],[v,v,v,v,v],[v,v,v,v,v]]).

test(hashing_function_5x5) :-
    generate_boards(5, 5, 1000, Boards),
    assertion(check_collisions(Boards)).
  
test(hashing_function_10x10) :-
    generate_boards(10, 10, 1000, Boards),
    assertion(check_collisions(Boards)).

test(hashing_function_15x15) :-
    generate_boards(10, 10, 1000, Boards),
    assertion(check_collisions(Boards)).
  
:- end_tests(board).

% Génère une configuration aléatoire de plateau de jeu de taille NxM:
random_board(N, M, Board) :-
    length(Board, N),
    maplist(random_row(M), Board).

% Génère une configuration aléatoire d'une ligne de plateau de jeu de taille M:
random_row(M, Row) :-
    length(Row, M),
    maplist(random_cell, Row).

% Génère une cellule avec un contenu aléatoire:
random_cell(Cell) :-
    random(1, 4, X),
    (
      X = 2 -> Cell = n ;
      X = 3 -> Cell = b ;
      Cell = v
    ).

% Génère plusieurs configurations aléatoires uniques de plateau de jeu de taille NxM:
generate_boards(N, M, NumBoards, Boards) :-
  maplist(random_board(N, M), Boards),
  list_to_set(Boards, UniqueBoards),
  length(UniqueBoards, NumBoards), !.

% Vérifie s'il y a collision pour la fonction de hachage:
check_collisions(Boards) :-
    maplist(hash_function, Boards, Hashes),
    length(Hashes, NumHashes),
    list_to_set(Hashes, UniqueHashes),
    list_to_set(Boards, UniqueBoards),
    length(UniqueHashes, NumUniqueHashes),
    length(UniqueBoards, NumUniqueBoards),
    NumCollisions is NumUniqueBoards - NumUniqueHashes,
    format("~nNumber of boards generated: ~d~n", [NumHashes]),
    format("Number of unique boards: ~d~n", [NumUniqueBoards]),
    format("Number of unique hash values: ~d~n", [NumUniqueHashes]),
    format("Number of collisions: ~d~n", [NumCollisions]),
    NumCollisions is 0, !.
