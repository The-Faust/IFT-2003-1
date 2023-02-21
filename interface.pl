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

%===================================%
%             Interface.            %
%===================================%

% Identifiants du contenu d'une case:
cell_to_char(v, '┼').   % Case vide (v).
cell_to_char(n, '●').   % Case avec un pion noir (n).
cell_to_char(b, '◯').   % Case avec un pion blanc (b).

% Affiche le plateau de jeu.
display_gomoku_board(Board) :-
	length(Board, N),
	write('   '),
	forall(   % Itère sur le nom des colonnes.
		between(1, N, X),
		(
			ABC is 64 + X, % Le caractère encodé en entier (65 = A, ..., Z = 90).
			format(' ~c', [ABC])
		)
	),
	nl,
	forall(   % Itère sur les lignes.
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
	
% Établi le plateau de jeu selon des paramètres fournis:
set_gomoku_board(Board) :-
	% Demande à l'utilisateur la taille (N), du plateau de jeu composé du grillage N×N:
	request_valid_integer(3, 26, 'Choisissez la taille du jeu: (min: 3, max: 26)', N),
	% Produit le plateau de jeu de dimensions N×N avec des cases vides (v):
	create_gomoku_board(N, Board).

% Demande au joueur la couleur qu'il veut jouer:
request_players_color :-
	write('Quelle couleur voulez-vous jouer? (n: noir ●, b: blanc ◯)\nLe pion noir débute la partie.\n'),
	repeat,
	read_line_to_string(user_input, Input),
	string_lower(Input, Input_Lower),
	atom_string(Color, Input_Lower),
	(
		member(Color, [b, n]) ->
		b_setval(players_color, Color),
		true
		;
		write('Vous devez choisir une couleur entre b et n.\n'),
		fail
	).

% Demande à l'utilisateur d'entrer un un nombre entre Min et Max, inclusivement:
request_valid_integer(Min, Max, Prompt, Value) :-
	write(Prompt),
	repeat,
	nl,
	read_line_to_string(user_input, Input),
	(
		catch(number_chars(Value, Input), _, false), integer(Value), between(Min, Max, Value) ->
		true
		;
		format('Vous devez choisir une valeur entre ~d et ~d.', [Min, Max]),
		fail
	).

% Demande à l'utilisateur de choisir une case du plateau:
request_cell_coordinates(Board, Row, Col) :-
	write('Choisissez la case où vous voulez jouer: (ex. A1)\n'),
	repeat,
	read_line_to_string(user_input, Input),
	(
		string_upper(Input, Input_Upper),
		string_chars(Input_Upper, [ColChar|RowChars]),
		(
			char_code(ColChar, Code),
			catch(number_chars(Row1, RowChars), _, false),
			Col is Code - 65,
			Row is Row1 - 1,
			are_valid_coordinates(Board, Row, Col) ->
			true
			;
			write('Vous devez choisir une case valide!\n'),
			fail
		)
	).

% Demande à l'utilisateur de choisir une case du plateau qui est vide:
request_next_move(Board, Row, Col) :-
	repeat,
	request_cell_coordinates(Board, Row, Col),
	(
		cell_is_empty(Board, Row, Col) ->
		true
		;
		write('Vous devez choisir une case qui est vide!\n'),
		fail
	).
