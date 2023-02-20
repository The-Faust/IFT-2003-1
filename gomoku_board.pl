% Rédigé par René Chenard, le 19 février 2023.
%
% Pour établir un plateau de jeu: set_gomoku_board(_).

% Identifiants du contenu d'une case:
cell_to_char(v, '┼').   % Case vide (v).
cell_to_char(n, '●').   % Case avec un pion noir (n).
cell_to_char(b, '◯').   % Case avec un pion blanc (b).

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
    ).


% Demande à l'utilisateur d'entrer un un nombre entre Min et Max, inclusivement:
get_valid_integer(Min, Max, Prompt, Value) :-
    repeat,
    write(Prompt),
    nl,
    read(Value),
    (
        integer(Value), between(Min, Max, Value) ->  true
        ;
        format('Erreur: Vous devez choisir une valeur entre ~d et ~d.\n', [Min, Max]),
        fail
    ).

cell_content(Board, Row, Col, Content) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Content).
    
cell_is_empty(Board, Row, Col) :-
    cell_content(Board, Row, Col, Content),
    Content == v.
    