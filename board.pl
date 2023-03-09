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


% Prédicat qui permet d'alterner les joeurs:
other(b, n).
other(n, b).

% Créer un plateau de jeu vierge de dimensions N×N:
create_gomoku_board(N, Board) :-
    length(Board, N),
    maplist(create_row(N), Board).

% Créer une rangée du plateau de jeu de dimensions N:
create_row(N, Row) :-
    length(Row, N),
    maplist(=(v), Row), !.

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
    get_cell_content(Board, Row-Col, v).

% Vérifie s'il reste un emplacement non occupé:
has_an_empty_cell(Board) :-
    cell_is_empty(Board, _).

% Vérifie si toutes les cases d'une liste sont vides (contient v):
contains_only_empty_cells(List) :-
    \+ (member(Element, List), Element \= v).

% Vérifie si les coordonnées existent sur le plateau:
are_valid_coordinates(Board, Row-Col) :-
    get_cell_content(Board, Row-Col, _).

% Retourne le dernier index du plateau (débute à 0):
get_last_index(Board, LastIndex) :-
    length(Board, N),
    LastIndex is N - 1.
    
% Permet d'établir les cases où il est possible de jouer:
get_possible_moves(Board, Moves) :-
    findall(Move, cell_is_empty(Board, Move), Moves).

% Permet d'effectuer un mouvement:
make_a_move(Board, Player, Move, NewBoard) :-
    set_cell_content(Board, Move, Player, NewBoard).
        
% Identifie le début d'une diagonale descendante:
start_of_a_diagonal_line_down(Row-Col, Goal, LastIndex) :-
    LastUsefulIndex is LastIndex - Goal + 1,
    between(0, LastUsefulIndex, Row),
    between(0, LastUsefulIndex, Col),
    (
      (Row = 0 ; Col = 0) ->
      true
      ;
      (Row = 0)
      ;
      (Col = 0)
    ).

% Identifie le début d'une diagonale montante:
start_of_a_diagonal_line_up(Row-Col, Goal, LastIndex) :-
    Goal_1 is Goal - 1,
    LastUsefulIndex is LastIndex - Goal_1,
    between(Goal_1, LastIndex, Row),
    between(0, LastUsefulIndex, Col),
    (
      (Row = LastIndex ; Col = 0) ->
      true
      ;
      (Row = LastIndex)
      ;
      (Col = 0)
    ).

% Identifie le début d'une ligne:
start_of_an_horizontal_line(Row-Col, _, LastIndex) :-
    between(0, LastIndex, Row),
    Col = 0.

% Identifie le début d'une colonne:
start_of_a_vertical_line(Row-Col, _, LastIndex) :-
    between(0, LastIndex, Col),
    Row = 0.

% Extrait les lignes pour un itérateur d'indices et une direction donnée:
get_lines(Board, StartingIndices, Direction, [x|Line]) :-
    get_goal(Goal),
    get_last_index(Board, LastIndex),
    findall(Line, (
              call(StartingIndices, Row-Col, Goal, LastIndex),
              get_line(Board, Row-Col, Direction, [x], Line)
            ), Line).

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
    get_lines(Board, start_of_a_diagonal_line_down, 1-1, DiagonalLinesDown),
    get_lines(Board, start_of_a_diagonal_line_up, -1-1, DiagonalLinesUp),
    get_lines(Board, start_of_an_horizontal_line, 0-1, HorizontalLines),
    get_lines(Board, start_of_a_vertical_line, 1-0, VerticalLines),
    flatten([HorizontalLines, VerticalLines, DiagonalLinesUp, DiagonalLinesDown], Lines).
