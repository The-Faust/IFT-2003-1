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
    create_list(N, v, Row).

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

% Créer une liste avec logueur spécifiée et une valeur par défaut:
create_list(Length, DefaultValue, List) :-
    length(List, Length),
    maplist(=(DefaultValue), List), !.

% Permet d'extraire une sous-liste à l'aide d'un index:
sublist(List, From, Count, SubList) :-
    To is From + Count - 1,
    findall(E, (between(From, To, I), nth0(I, List, E)), SubList).

% Permet de vérifier si une liste est contenue dans l'autre:
is_a_sublist(SubList, List) :-
    phrase((..., SubList), List, _), !.

% Permet de vérifier si une liste est contenue dans l'autre et d'obtenir son index:
is_a_sublist_index(SubList, List, Index) :-
    phrase((..., SubList), List, Before),
    length(Before, Index), !.

% Défini "n'importe quelle séquence" avec les points de suspension:
... --> [] | [_], ... .

% Défini une séquence du symbole S:
rep(S) --> [S] | [S], rep(S).

% Défini une séquence du symbole S et de longueur L:
rep(S, L) --> [S], {L is 1} | [S], { L_1 is L - 1 }, rep(S, L_1), !.

% Défini une séquence du symbole S ouverte (v des deux côtés):
open_rep(S) --> ..., [v], rep(S), [v], ... .

% Défini une séquence du symbole S ouverte (v des deux côtés) et de longueur L:
open_rep(S, L) --> ..., [v], rep(S, L), [v], ... .

% Défini une séquence complète:
full_rep(S, L) --> ..., rep(S, L), ... .

% Défini une séquence du symbole S fermée (v d'un seul côté):
closed_rep(S) --> { dif(S, P), dif(P, v) }, ..., [P], rep(S), [v], ... |
                  { dif(S, P), dif(P, v) }, ..., [v], rep(S), [P], ... |
                  rep(S), [v], ... | ..., [v], rep(S).

% Défini une séquence du symbole S fermée (v d'un seul côté) et de longueur L:
closed_rep(S, L) --> { dif(S, P), dif(P, v) }, ..., [P], rep(S, L), [v], ... |
                     { dif(S, P), dif(P, v) }, ..., [v], rep(S, L), [P], ... |
                     rep(S, L), [v], ... | ..., [v], rep(S, L).

% Défini une séquence du symbole S ouverte (v des deux côtés):
fixed_open_rep(S, T) --> { between(1, T, A), Temp is T - A, between(1, Temp, B), C is Temp - B },
                         ..., rep(v, A), rep(S, B), rep(v, C), ... .

% Défini une séquence du symbole S ouverte (v des deux côtés):
fixed_open_rep(S, L, T) --> { Temp is T - L, between(1, Temp, A), B is T - A - L },
                             ..., rep(v, A), rep(S, L), rep(v, B), ... .

% Défini une séquence du symbole S fermée (v d'un seul côté):
fixed_closed_rep(S, T) --> { dif(S, P), dif(P, v), between(1, T, A), B is T - A }, ..., [P], rep(S, A), rep(v, B), ... |
                           { dif(S, P), dif(P, v), between(1, T, A), B is T - A }, ..., rep(v, A), rep(S, B), [P], ... |
                           { between(1, T, A), B is T - A }, rep(S, A), rep(v, B), ... |
                           { between(1, T, A), B is T - A }, ..., rep(v, A), rep(S, B).

% Défini une séquence du symbole S fermée (v d'un seul côté):
fixed_closed_rep(S, L, T) --> { dif(S, P), dif(P, v), A is T - L }, ..., [P], rep(S, L), rep(v, A), ... |
                              { dif(S, P), dif(P, v), A is T - L }, ..., rep(v, A), rep(S, L), [P], ... |
                              { A is T - L }, rep(S, L), rep(v, A), ... |
                              { A is T - L }, ..., rep(v, A), rep(S, L).

% Permet de compter le nombre d'occurences d'une sous-liste:
occurrences(List, Sublist, Count) :-
    phrase(occurrences(Sublist, 0, Count), List).
    
occurrences("", _, _) --> !, { false }.

occurrences(Sublist, Count0, Count) -->
    Sublist, !, { Count1 is Count0 + 1 },
    occurrences(Sublist, Count1, Count).
    
occurrences(Sublist, Count0, Count) -->
    [_], !,
    occurrences(Sublist, Count0, Count).
    
occurrences(_, Count, Count) --> !.

% Permet de créer l'empreinte d'une configuration du plateau:
hash_function(Board, Hash) :-
  flatten(Board, GridString),
  hash_function(GridString, 0, Hash).
  
hash_function([], Hash, Hash).

hash_function([C|Cs], Acc, Hash) :-
  char_code(C, Code),
  NewAcc is ((Acc << 5) - Acc) + Code,
  hash_function(Cs, NewAcc, Hash).
