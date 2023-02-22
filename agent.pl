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

%======================================%
%           Agent intelligent .        %
%======================================%

% Vérifie s'il est possible pour le joueur de gagner en un tour:
winning_move(Board, Player, WinningScore, Move) :-
	cell_is_empty(Board, Move),
	set_cell_content(Board, Move, Player, NewBoard),
	evaluate_score(NewBoard, Player, WinningScore).

% L'agent choisit de jouer dans (Row, Col):
agent(Board, Player, WinningScore, Move) :-
	other(Player, OtherPlayer),
	(	% Vérifie s'il est possible de gagner sur ce tour:
		winning_move(Board, Player, WinningScore, Move)
		;
		% Vérifie s'il est possible de perdre au prochain tour:
		winning_move(Board, OtherPlayer, WinningScore, Move)
		;
		% Choisi une case vide au hasard:
		get_a_random_move(Board, Move)
	).
