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

% Vérifie s'il est possible pour le joueur de gagner en un tour:
winning_move(Board, Player, Goal, Move) :-
    cell_is_empty(Board, Move),
    set_cell_content(Board, Move, Player, NewBoard),
    evaluate_score(NewBoard, Player, Score),
    Score >= Goal.
