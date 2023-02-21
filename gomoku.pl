% Travail pratique 1 : Gomoku
% Conception d'un jeu intelligent
%
% remis par
%   Omar Akrout    (NI: 111 165 246)
%   René Chenard   (NI: 111 232 277)
% et
%   Vincent Martel (NI: 111 105 797)
%
% dans le cadre du cours
%   IFT-2003 - Intelligence artificielle I
%   Hiver 2023

%===================================%
%   Initialiser le jeu: ?- play.    %
%===================================%

:- [board].
:- [interface].
:- [win_predicates].

% Identifiants du joueur:
players_name(n, 'noir').    % Joueur noir (n).
players_name(b, 'blanc').   % Joueur blanc (b).

% Prédicat qui permet d'alterner les joeurs:
other(b, n).
other(n, b).

% Permet à un joueur de jouer son tour:
move(Board, Player, NewBoard, Length) :-
	(   % Vérifie s'il reste un emplacement vide:
		cell_is_empty(Board, R, C) ->
		(
			(   % Vérifie à qui le tour appartient:
				b_getval(players_color, Color),
				Player == Color ->
				(   % Le joueur choisi la case à jouer:
					request_next_move(Board, Row, Col)
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
			set_cell_content(Board, Row, Col, Player, NewBoard)
		)
		;
		(
			write('Le plateau de jeu est plein!\n'),
			write('Il s\'agit d\'un impasse!\n'),
			halt
		)
	).

% Établi un tour complet et boucle jusqu'à ce que le jeu termine:
turn(Board, Player, NewBoard) :-
	Length = 3, % Nombre de pions devant être alignés pour gagner.
	display_gomoku_board(Board),
	move(Board, Player, NewBoard, Length),
	not(win(NewBoard, Player, 3)),
	other(Player, NextPlayer),
	turn(NewBoard, NextPlayer, _).
	
% Démarre le jeu:
play :-
	Firstplayer = n,
	set_gomoku_board(Board),
	request_players_color,
	turn(Board, Firstplayer, _).
	