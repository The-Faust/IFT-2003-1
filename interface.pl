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
%                 Interface.                %
%===========================================%


:- [board].
:- [heuristic_evaluation].

% Identifiants du joueur:
players_name(n, 'noir').    % Joueur noir (n).
players_name(b, 'blanc').   % Joueur blanc (b).

% Identifiants du contenu d'une case:
cell_to_char(v, '┼').   	% Case vide (v).
cell_to_char(n, '●').   	% Case avec un pion noir (n).
cell_to_char(b, '◯').   	% Case avec un pion blanc (b).

% Convertit un numéro de colonne en la lettre correspondante:
column_nb_to_id(C, ID) :-
	Code is C + 65, 	% Le caractère encodé en entier (65 = A, ..., Z = 90).
	char_code(ID, Code).

% Convertit un couple en coordonnées lisibles:
coordinates_to_id(R-C, ID) :-
	column_nb_to_id(C, ColID),
	RowID is R + 1,
	format(atom(ID), '~w~w', [ColID, RowID]).

% Affiche le plateau de jeu:
display_gomoku_board(Board) :-
	get_last_index(Board, LastIndex),
	write('   '),
	forall(   % Itère sur le nom des colonnes.
		between(0, LastIndex, C),
		(
			column_nb_to_id(C, ID),
			format(' ~w', [ID])
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
set_board_size(N) :-
	% Demande à l'utilisateur la taille (N), du plateau de jeu composé du grillage N×N:
	request_valid_integer(3, 26, 'Choisissez la taille du jeu: (min: 3, max: 26)', N).

% Demande à l'utilisateur le nombre de jetons à aligner pour gagner:
request_goal(N, Goal) :-
	(
		N > 3 ->
		format(atom(Prompt), 'Choisissez l\'objectif, soit le nombre de jetons à aligner: (min: 3, max: ~d)', N),
		request_valid_integer(3, N, Prompt, Goal)
		;
		Goal is 3
	).

% Demande au joueur la couleur qu'il veut jouer:
request_players_color :-
	writeln('Quelle couleur voulez-vous jouer? (n: noir ●, b: blanc ◯)'),
	writeln('Le pion noir débute la partie.'),
	writeln('Appuyer sur la touche Entrée pour un duel IA vs IA.'),
	repeat,
	read_line_to_string(user_input, Input),
	(
		Input = "" ->
		assertz(player(nil))
		;
		string_lower(Input, Input_Lower),
		atom_string(Color, Input_Lower),
		(
			member(Color, [b, n]) ->
			assertz(player(Color)),
			true
			;
			write('Vous devez choisir une couleur entre b et n.\n'),
			fail
		)
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
request_cell_coordinates(Board, Row-Col) :-
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
			are_valid_coordinates(Board, Row-Col) ->
			true
			;
			write('Vous devez choisir une case valide!\n'),
			fail
		)
	).

% Demande à l'utilisateur de choisir une case du plateau qui est vide:
request_next_move(Board, Move) :-
	repeat,
	request_cell_coordinates(Board, Move),
	(
		cell_is_empty(Board, Move) ->
		true
		;
		write('Vous devez choisir une case qui est vide!\n'),
		fail
	).

% Affiche une ligne de séparation:
draw_line :-
	write('────────────────────────────────────────────────────────────────────\n').

% Affiche à qui le tour appartient:
introduce_turn(Player) :-
	draw_line,
	players_name(Player, PlayersName),
	cell_to_char(Player, PlayersSymbol),
	format('Le joueur ~w (~w) joue son tour:\n\n', [PlayersName, PlayersSymbol]).

% Affiche le résultat du tour qui termine:
conclude_turn(NewBoard-Player-Move, NextPlayer) :-
	other(Player, NextPlayer),
	display_gomoku_board(NewBoard),
	static_score(NewBoard, Player, StaticScore),
	heuristic_score(NewBoard-Player-_, HeuristicScore),
	players_name(Player, PlayersName),
	cell_to_char(Player, PlayersSymbol),
	coordinates_to_id(Move, MoveID),
	format('Le joueur ~w (~w) a joué la position ~w.\n\n', [PlayersName, PlayersSymbol, MoveID]),
	format('Score statique du joueur: ~d\n', [StaticScore]),
	format('Score heuristique du joueur: ~d\n\n', [HeuristicScore]),
	(
		get_goal(Goal),
		StaticScore >= Goal ->
		(
			draw_line,
			format('Le joueur ~w (~w) gagne!\n', [PlayersName, PlayersSymbol]),
			writeln(NewBoard),
			end_game
		)
		;
		true
	).

% Informe l'utilisateur qu'on a obtenu un impasse:
display_tie :-
	write('Le plateau de jeu est plein!\n'),
	write('Il s\'agit d\'un impasse!\n'),
	end_game.
	