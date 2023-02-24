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
%              Agent intelligent .          %
%===========================================%

% L'agent choisit de jouer dans (Row, Col):
agent(Board, Player, Goal, Move) :-
	other(Player, OtherPlayer),
	(
%		% Vérifie s'il est possible de gagner sur ce tour:
%		winning_move(Board, Player, Goal, Move)
%		;
%		% Vérifie s'il est possible de perdre au prochain tour:
%		winning_move(Board, OtherPlayer, Goal, Move)
%		;
		% Minimax:
		MaxDepth is 5,
		alpha_beta(Board, Player, Goal, max, MaxDepth, -1000, 1000, Move, _)
		%minimax(Board, Player, Goal, MaxDepth, Move)
	).

% Heuristique qui permet d'évaluer la valeur d'un état:
heuristic_value(Board, Player, Goal, Value) :-
	evaluate_score(Board, Player, Score),
	(	% Vérifie si le joueur gagne:
		Score >= Goal ->
		Value is 1000
		;
		(	% Vérifie si l'opposant gagne:
			other(Player, Opponent),
			evaluate_score(Board, Opponent, OpponentScore),
			(
				OpponentScore >= Goal ->
				Value is -1000
				;
				% Ni le joueur, ni l'opposant ne gagnent:
				Value is Score - OpponentScore
			)
		)
	).
