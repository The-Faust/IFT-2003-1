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
%  Algorithme Alpha-Bêta avec heuristique.  %
%===========================================%


:- module(alphabeta_heuristic, [alphabeta_heuristic/6]).

% Modification de l'algorithme Alpha-Bêta tel que suggéré dans le livre
% intitulé "Prolog programming for artificial intelligence"
% par Ivan Bratko, 1986
% Source: https://silp.iiita.ac.in/wp-content/uploads/PROLOG.pdf

% Cette version de l'algorithme limite la profondeur de recherche et
% utilise un heuristique pour évaluer la valeur d'un état.

alphabeta_heuristic(Pos, Alpha, Beta, GoodPos, Val, Depth) :-
    Depth > 0, moves(Pos, PosList), !,
    boundedbest(PosList, Alpha, Beta, GoodPos, Val, Depth);
    heuristicval(Pos, Val).

boundedbest([Pos|PosList], Alpha, Beta, GoodPos, GoodVal, Depth) :-
    Depth1 is Depth - 1,
    alphabeta_heuristic(Pos, Alpha, Beta, _, Val, Depth1),
    goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth).

goodenough([], _, _, Pos, Val, Pos, Val, _) :- !.

goodenough(_, Alpha, Beta, Pos, Val, Pos, Val, _) :-
    min_to_move(Pos), Val > Beta, !;
    max_to_move(Pos), Val < Alpha, !.

goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth) :-
    newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta),
    boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1, Depth),
    betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
    min_to_move(Pos), Val > Alpha, !.

newbounds(Alpha, Beta, Pos, Val, Alpha, Val) :-
    max_to_move(Pos), Val < Beta, !.

newbounds(Alpha, Beta, _, _, Alpha, Beta).

betterof(Pos, Val, _, Val1, Pos, Val) :-
    min_to_move(Pos), Val > Val1, !;
    max_to_move(Pos), Val < Val1, !.

betterof(_, _, Pos1, Val1, Pos1, Val1).
