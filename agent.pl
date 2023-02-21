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
	% Solution temporaire: La première case vide est choisie par l'ordinateur.
	cell_is_empty(Board, Row, Col).
