% Rédigé par René Chenard, le 19 février 2023.
%
% Pour établir un plateau de jeu: set_gomoku_board(_).

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
    create_gomoku_board(N, Board),
    % Affiche le plateau de jeu:
    display_gomoku_board(Board).

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

% Demande à l'utilisateur d'entrer un un nombre entre Min et Max, inclusivement:
get_valid_integer(Min, Max, Prompt, Value) :-
    repeat,
    write(Prompt),
    nl,
    read_line_to_string(user_input, Input),
    (
        number_chars(Value, Input), integer(Value), between(Min, Max, Value) ->  true
        ;
        format('Vous devez choisir une valeur entre ~d et ~d.\n', [Min, Max]),
        fail
    ).

% Demande à l'utilisateur de choisir une case du plateau:
get_valid_cell(N, Row, Col) :-
    write('Choisissez la case où vous voulez jouer: (ex. A1)\n'),
    repeat,
    read_line_to_string(user_input, Input),
    string_upper(Input, Input_Upper),
    string_chars(Input_Upper, [ColChar|RowChars]),
    char_code(ColChar, Code),
    Col1 is Code - 64,
    number_chars(Row1, RowChars),
    (
        integer(Col1), integer(Row1), between(1, N, Col1), between(1, N, Row1) -> ( Col is Col1 - 1, Row is Row1 - 1, true )
        ;
        write('Vous devez choisir une case valide!\n'),
        fail
    ).

% Demande à l'utilisateur de choisir une case du plateau qui est vide:
get_empty_cell(Board, Row, Col) :-
    length(Board, N),
    repeat,
    get_valid_cell(N, Row, Col),
    (
        cell_is_empty(Board, Row, Col) -> true
        ;
        write('Vous devez choisir une case qui est vide!\n'),
        fail
    ).

% Extrait le contenu d'une case du plateau:
cell_content(Board, Row, Col, Content) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Content).

% Vérifie si la case est vide:
cell_is_empty(Board, Row, Col) :-
    cell_content(Board, Row, Col, Content),
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
                Player == n ->
                (   % Le joueur choisi la case à jouer:
                    get_empty_cell(Board, Row, Col)
                )
                ;
                (   % L'ordinateur choisi la case à jouer:
                    % (Solution temporaire: La première case vide est choisie par l'ordinateur.)
                    Row is R,
                    Col is C
                )
            ),
            % Met à jour le plateau de jeu:
            update_board(Board, Row, Col, Player, NewBoard),
            % Affiche le plateau de jeu:
            display_gomoku_board(NewBoard)
        )
        ;
        (
            write('Le plateau est plein!'),
            fail
        )
    ).

% Établi un tour complet et boucle jusqu'à ce que le jeu termine:
turn(Board, Player, NewBoard) :-
    move(Board, Player, NewBoard),
    other(Player, NextPlayer),
    turn(NewBoard, NextPlayer, _).

% Démarre le jeu:
play :-
    FirstPlayer is n,
    set_gomoku_board(Board),
    turn(Board, FirstPlayer, NewBoard).