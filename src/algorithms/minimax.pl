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

%====================================================%
%                 Algorithme Minimax.                %
%====================================================%


:- module(minimax, [minimax/3]).

% Algorithme Minimax tel que suggéré dans le cours de Gauthier Picard
% intitulé "Artificial Intelligence - MINES Saint-Etienne"
% Source: https://www.emse.fr/~picard/cours/ai/minimax/minimax.pl

minimax(Pos, BestNextPos, Val) :-
    moves(Pos, NextPosList),
    best(NextPosList, BestNextPos, Val), !.

minimax(Pos, _, Val) :-
    staticval(Pos, Val).

best([Pos], Pos, Val) :-
    minimax(Pos, _, Val), !.

best([Pos1 | PosList], BestPos, BestVal) :-
    minimax(Pos1, _, Val1),
    best(PosList, Pos2, Val2),
    betterof(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterof(Pos0, Val0, _, Val1, Pos0, Val0) :-
    min_to_move(Pos0),
    Val0 > Val1, !
    ;
    max_to_move(Pos0),
    Val0 < Val1, !.

betterof(_, _, Pos1, Val1, Pos1, Val1).
