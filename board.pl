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
%                    Gomoku Board.                   %
%====================================================%


:- use_module(library(clpfd)). % Pour 'transpose'.

% Prédicat qui permet d'alterner les joeurs:
other(b, n).
other(n, b).

% Valeur numérique du contenu d'une case:
cell_to_num(v, 1).   	% Case vide (v).
cell_to_num(n, 2).   	% Case avec un pion noir (n).
cell_to_num(b, 3).   	% Case avec un pion blanc (b).

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

% Vérifie si toutes les cases d'une liste sont vides (contient v):
contains_only_empty_cells(List) :-
    \+ (member(Element, List), Element \= v).

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

% Extrait les lignes horizontales:
get_horizontal_lines(Board, HorizontalLines) :-
    Board = HorizontalLines.
    
% Extrait les lignes verticales:
get_vertical_lines(Board, VerticalLines) :-
    transpose(Board, VerticalLines).

% Extrait les lignes diagonales descendantes:
get_diagonal_lines_down(Board, DiagonalLinesDown) :-
    get_goal(Goal),
    get_last_index(Board, LastIndex),
    Goal_1 is Goal - 1,
    LastUsefulIndex is LastIndex - Goal_1,
    findall(Line, (
              between(0, LastUsefulIndex, R),
              between(0, LastUsefulIndex, C),
              (
                (R = 0 ; C = 0) ->
                true
                ;
                (R = 0)
                ;
                (C = 0)
              ),
              get_line(Board, R-C, 1-1, [], Line)
            ), DiagonalLinesDown).

% Extrait les lignes diagonales montantes:
get_diagonal_lines_up(Board, DiagonalLinesUp) :-
    get_goal(Goal),
    get_last_index(Board, LastIndex),
    Goal_1 is Goal - 1,
    LastUsefulIndex is LastIndex - Goal_1,
    findall(Line, (
              between(Goal_1, LastIndex, R),
              between(0, LastUsefulIndex, C),
              (
                (R = LastIndex ; C = 0) ->
                true
                ;
                (R = LastIndex)
                ;
                (C = 0)
              ),
              get_line(Board, R-C, -1-1, [], Line)
            ), DiagonalLinesUp).

% Extrait une ligne dans une direction donnée à partir d'une case:
get_line(Board, R-C, StepR-StepC, Accumulator, Line) :-
    get_cell_content(Board, R-C, Content), !,
    NewAccumulator = [Content|Accumulator],
    NewR is R + StepR,
    NewC is C + StepC,
    get_line(Board, NewR-NewC, StepR-StepC, NewAccumulator, Line).
get_line(_, _, _, Line, Line).

% Extrait toutes les lignes et filtre celles qui sont vides:
get_all_lines(Board, Lines) :-
    get_horizontal_lines(Board, HorizontalLines),
    filter_and_pad_lines(HorizontalLines, HorizontalLinesP),
    get_vertical_lines(Board, VerticalLines),
    filter_and_pad_lines(VerticalLines, VerticalLinesP),
    get_diagonal_lines_up(Board, DiagonalLinesUp),
    filter_and_pad_lines(DiagonalLinesUp, DiagonalLinesUpP),
    get_diagonal_lines_down(Board, DiagonalLinesDown),
    filter_and_pad_lines(DiagonalLinesDown, DiagonalLinesDownP),
    flatten([HorizontalLinesP, VerticalLinesP, DiagonalLinesUpP, DiagonalLinesDownP], Lines).

% Ajoute des délimiteurs, x, à une ligne:
pad_line(Line, PaddedLine) :-
    flatten([x, Line, x], PaddedLine).

% Vérifie si la ligne vaut la peine d'être traitée:
line_is_worth_treating(Line) :-
    not(contains_only_empty_cells(Line)).

% Filtre les lignes sans intérêt et ajoute des délimiteurs:
filter_and_pad_lines(Lines, TreatedLines) :-
    include(line_is_worth_treating, Lines, FilteredLines),
    concurrent_maplist(pad_line, FilteredLines, TreatedLines).

% Créer une liste avec logueur spécifiée et une valeur par défaut:
create_list(Length, DefaultValue, List) :-
    length(List, Length),
    maplist(=(DefaultValue), List), !.

% Permet de créer l'empreinte d'une configuration du plateau:
hash_function(Board, Hash) :-
    flatten(Board, GridString),
    hash_function(GridString, 0, Hash).
hash_function([], Hash, Hash).
hash_function([C|Cs], Acc, Hash) :-
    cell_to_num(C, Code),
    NewAcc is ((Acc << 2) - Acc) + Code,
    hash_function(Cs, NewAcc, Hash).
    