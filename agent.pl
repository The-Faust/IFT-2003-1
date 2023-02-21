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
%         Agent intelligent.        %
%===================================%

:- [board].

% L'agent choisit de jouer dans (Row, Col):
agent(Board, Length, Row, Col) :-
	(	% Vérifie s'il est possible de gagner sur ce tour:
		cell_is_empty(Board, Row, Col),
		b_getval(players_color, Player),
		other(AI, Player),
		set_cell_content(Board, Row, Col, AI, NewBoard),
		win(NewBoard, AI, Length)
		;
		% Place un jeton dans un emplacement vide:
		cell_is_empty(Board, Row, Col)
	).
