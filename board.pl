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
%               Gomoku Board.               %
%===========================================%


:- use_module(library(clpfd)).

% Créer un plateau de jeu vierge de dimensions N×N:
create_gomoku_board(N, Board) :-
    length(Board, N),
    maplist(create_row(N), Board).

% Créer une rangée du plateau de jeu de dimensions N:
create_row(N, Row) :-
    length(Row, N),
    maplist(=(v), Row).

% Extrait le contenu d'une case du plateau:
get_cell_content(Board, Row-Col, Content) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Content).

% Met à jour une case du plateau:
set_cell_content(Board, Row-Col, Content, NewBoard) :-
    nth0(Row, Board, OldRow),
    replace(OldRow, Col, Content, NewRow),
    replace(Board, Row, NewRow, NewBoard).

% Prédicat utilitaire pour remplacer le contenu d'une case:
replace(List, Index, NewElem, NewList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, NewList, NewElem, Rest).

% Vérifie si la case est vide:
cell_is_empty(Board, Row-Col) :-
    get_cell_content(Board, Row-Col, Content),
    Content == v.

% Compte le nombre de cases respectant un prédicat donné:
count_cells(Board, Predicate, Count) :-
    flatten(Board, Flat),
    include(Predicate, Flat, FilteredCells),
    length(FilteredCells, Count).

% Compte le nombre de cases vides:
count_empty_cells(Board, Count) :-
    count_cells(Board, [C]>>(C = v), Count).

% Compte le nombre de cases occupées:
count_occupied_cells(Board, Count) :-
    count_cells(Board, [C]>>(not(C = v)), Count).

% Vérifie s'il reste un emplacement non occupé:
has_an_empty_cell(Board) :-
    flatten(Board, Flat),
    memberchk(v, Flat).

% Vérifie si les coordonnées existent sur le plateau:
are_valid_coordinates(Board, Row-Col) :-
    get_last_index(Board, LastIndex),
    between(0, LastIndex, Col),
    between(0, LastIndex, Row).

% Retourne le dernier index du plateau (débute à 0):
get_last_index(Board, LastIndex) :-
    length(Board, N),
    LastIndex is N - 1.
    
% Permet d'établir les cases où il est possible de jouer:
get_possible_moves(Board, Moves) :-
    findall(Move, cell_is_empty(Board, Move), Moves).
    
% Permet d'obtenir une case vide au hasard:
get_a_random_move(Board, Row-Col) :-
    get_last_index(Board, LastIndex),
    repeat,
    (
        random_between(0, LastIndex, Row),
        random_between(0, LastIndex, Col),
        cell_is_empty(Board, Row-Col)
    ).

% Permet d'effectuer un mouvement:
make_a_move(Board, Player, Move, NewBoard) :-
    set_cell_content(Board, Move, Player, NewBoard).

% Effectue une rotation de 90º du plateau:
rotate_board(Board, RotatedBoard) :-
    transpose(Board, TransposedBoard),
    maplist(reverse, TransposedBoard, RotatedBoard).

% Effectue une réflexion selon l'axe horizontal du plateau:
flip_horizontal(Board, FlippedBoard) :-
    reverse(Board, FlippedBoard).

% Effectue une réflexion selon l'axe vertical du plateau:
flip_vertical(Board, FlippedBoard) :-
    maplist(reverse, Board, FlippedBoard).

% Effectue une réflexion selon la première diagonale du plateau:
flip_first_diagonal(Board, FlippedBoard) :-
    transpose(Board, FlippedBoard).

% Effectue une réflexion selon la seconde diagonale du plateau:
flip_second_diagonal(Board, FlippedBoard) :-
    reverse(Board, ReversedBoard),
    transpose(ReversedBoard, TransposedBoard),
    reverse(TransposedBoard, FlippedBoard).

% Permet d'inverser le plateau de jeu (b <-> n):
invert_board(Board, InvertedBoard) :-
    maplist(invert_row, Board, InvertedBoard).

invert_row(Row, InvertedRow) :-
    maplist(invert_value, Row, InvertedRow).
    
invert_value(b, n).
invert_value(n, b).
invert_value(X, X) :- dif(X, b), dif(X, n).

% Extrait les lignes horizontales:
get_horizontal_lines(Board, HorizontalLines) :-
    Board = HorizontalLines.
    
% Extrait les lignes verticales:
get_vertical_lines(Board, VerticalLines) :-
    transpose(Board, VerticalLines).
    
% Extrait une ligne dans une direction donnée à partir d'une case:
get_line(Board, R-C, StepR-StepC, Accumulator, Line) :-
    get_cell_content(Board, R-C, Content), !,
    NewAccumulator = [Content|Accumulator],
    NewR is R + StepR,
    NewC is C + StepC,
    get_line(Board, NewR-NewC, StepR-StepC, NewAccumulator, Line).
get_line(_, _, _, Line, Line).
