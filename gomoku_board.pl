% Rédigé par René Chenard, le 19 février 2023.
%
% Pour établir un plateau de jeu: set_gomoku_board(_).

:- use_module(library(clpfd)).

% Identifiants du joueur:
players_name(n, 'noir').    % Joueur noir (n).
players_name(b, 'blanc').   % Joueur blanc (b).

% Identifiants du contenu d'une case:
cell_to_char(v, '┼').   % Case vide (v).
cell_to_char(n, '●').   % Case avec un pion noir (n).
cell_to_char(b, '◯').   % Case avec un pion blanc (b).

% Prédicat qui permet d'alterner les joeurs:
other(b, n).
other(n, b).

% Établi le plateau de jeu selon des paramètres fournis:
set_gomoku_board(Board) :-
    % Demande à l'utilisateur la taille (N), du plateau de jeu composé du grillage N×N:
    get_valid_integer(3, 26, 'Choisissez la taille du jeu: (min: 3, max: 26)', N),
    % Produit le plateau de jeu de dimensions N×N avec des cases vides (v):
    create_gomoku_board(N, Board).

% Créer un plateau de jeu vierge de dimensions N×N:
create_gomoku_board(N, Board) :-
    length(Board, N),
    maplist(create_row(N), Board).

% Créer une rangée du plateau de jeu de dimensions N:
create_row(N, Row) :-
    length(Row, N),
    maplist(=(v), Row).

% Affiche le plateau de jeu.
display_gomoku_board(Board) :-
    length(Board, N),
    write('   '),
    % Itère sur le nom des colonnes:
    forall(
        between(1, N, X),
        (
            ABC is 64 + X, % Le caractère encodé en entier (65 = A, ..., Z = 90).
            format(' ~c', [ABC])
        )
    ),
    nl,
    % Itère sur les lignes:
    forall(
        nth1(Y, Board, Row),
        (
            % Affiche le nom de la ligne:
            format('~|~t~d~2+ ', [Y]),
            % Affiche le contenu de la ligne:
            maplist([C]>>(cell_to_char(C, Char), format('─~w', [Char])), Row),
            write('─\n')
        )
    ),
    nl.

% Demande au joueur la couleur qu'il veut jouer:
get_players_color :-
    repeat,
    write('Quelle couleur voulez-vous jouer? (n: noir ●, b: blanc ◯)\nLe pion noir débute la partie.\n'),
    read_line_to_string(user_input, Input),
    string_lower(Input, Input_Lower),
    atom_string(Color, Input_Lower),
    (
        member(Color, [b, n]) -> 
        (
            b_setval(players_color, Color),
            true
        )
        ;
        write('Vous devez choisir une couleur entre b et n.\n'),
        fail
    ).

% Demande à l'utilisateur d'entrer un un nombre entre Min et Max, inclusivement:
get_valid_integer(Min, Max, Prompt, Value) :-
    repeat,
    write(Prompt),
    nl,
    read_line_to_string(user_input, Input),
    (
        number_chars(Value, Input), integer(Value), between(Min, Max, Value) -> true
        ;
        format('Vous devez choisir une valeur entre ~d et ~d.\n', [Min, Max]),
        fail
    ).

% Demande à l'utilisateur de choisir une case du plateau:
request_cell_coordinates(N, Row, Col) :-
    write('Choisissez la case où vous voulez jouer: (ex. A1)\n'),
    repeat,
    read_line_to_string(user_input, Input),
    string_upper(Input, Input_Upper),
    string_chars(Input_Upper, [ColChar|RowChars]),
    char_code(ColChar, Code),
    Col1 is Code - 64,
    number_chars(Row1, RowChars),
    (
        integer(Col1), integer(Row1), between(1, N, Col1), between(1, N, Row1) ->
        (
            Col is Col1 - 1,
            Row is Row1 - 1,
            true
        )
        ;
        write('Vous devez choisir une case valide!\n'),
        fail
    ).

% Demande à l'utilisateur de choisir une case du plateau qui est vide:
get_empty_cell(Board, Row, Col) :-
    length(Board, N),
    repeat,
    request_cell_coordinates(N, Row, Col),
    (
        cell_is_empty(Board, Row, Col) -> true
        ;
        write('Vous devez choisir une case qui est vide!\n'),
        fail
    ).

% Extrait le contenu d'une case du plateau:
get_cell_content(Board, Row, Col, Content) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Content).

% Vérifie si la case est vide:
cell_is_empty(Board, Row, Col) :-
    get_cell_content(Board, Row, Col, Content),
    Content == v.

% Met à jour une case du plateau:
update_board(Board, Row, Col, Player, NewBoard) :-
    nth0(Row, Board, OldRow),
    replace(OldRow, Col, Player, NewRow),
    replace(Board, Row, NewRow, NewBoard).

% Prédicat utilitaire pour remplacer le contenu d'une case:
replace(List, Index, NewElem, NewList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, NewList, NewElem, Rest).

% Permet à un joueur de jouer son tour:
move(Board, Player, NewBoard) :-
    (
        % Vérifie s'il reste un emplacement vide:
        cell_is_empty(Board, R, C) ->
        (
            (
                b_getval(players_color, Color),
                Player == Color ->
                (   % Le joueur choisi la case à jouer:
                    get_empty_cell(Board, Row, Col)
                )
                ;
                (   % L'ordinateur choisi la case à jouer:
                    % (Solution temporaire: La première case vide est choisie par l'ordinateur.)
                    write('L\'adversaire joue son tour:\n'),
                    Row is R,
                    Col is C
                )
            ),
            % Met à jour le plateau de jeu:
            update_board(Board, Row, Col, Player, NewBoard)
        )
        ;
        (
            write('Le plateau est plein!\n'),
            fail
        )
    ).

% Établi un tour complet et boucle jusqu'à ce que le jeu termine:
turn(Board, Player, NewBoard) :-
    display_gomoku_board(Board),
    move(Board, Player, NewBoard),
    not(win(NewBoard, Player, 3)),
    other(Player, NextPlayer),
    turn(NewBoard, NextPlayer, _).

% Démarre le jeu:
play :-
    Firstplayer = n,
    set_gomoku_board(Board),
    get_players_color,
    turn(Board, Firstplayer, _).

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
