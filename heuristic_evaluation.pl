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
%     Évaluation heuristique du score.      %
%===========================================%


:- [board].
:- [static_evaluation].

:- dynamic memo_heuristic_score/3.	% Mémoïsation du score pour une configuration.

% Récupère l'alignement de jetons le plus long mémoïsé pour un joueur:
heuristic_score(Board-_-_, TotalScore) :-
	get_goal(Goal),
	hash_function(Board, Hash),
	memo_heuristic_score(Hash, Goal, TotalScore),
	!.

% Évalue le score heuristique:
heuristic_score(Board-LastPlayer-_, TotalScore) :-
	get_goal(Goal),
	get_last_index(Board, LastIndex),
	get_horizontal_lines(Board, HorizontalLines),
	get_vertical_lines(Board, VerticalLines),
	M is Goal - 1,
	N is LastIndex - M,
	findall(Score, (
						member(Line, HorizontalLines),
						(
							contains_only_empty_cells(Line) ->
							Score is 0
							;
							line_score(Line, LastPlayer, Score)
						)
					), HorizontalLinesScores),
	findall(Score, (
						member(Line, VerticalLines),
						(
							contains_only_empty_cells(Line) ->
							Score is 0
							;
							line_score(Line, LastPlayer, Score)
						)
					), VerticalLinesScores),
	findall(Score, (
						between(0, N, R),
						between(0, N, C),
						(
							(R = 0 ; C = 0) ->
							true
							;
							(R = 0)
							;
							(C = 0)
						),
						get_line(Board, R-C, 1-1, [], Line),
						(
							contains_only_empty_cells(Line) ->
							Score is 0
							;
							line_score(Line, LastPlayer, Score)
						)
					), DiagonalLinesDownScores),
	findall(Score, (
						between(M, LastIndex, R),
						between(0, N, C),
						(
							(R = LastIndex ; C = 0) ->
							true
							;
							(R = LastIndex)
							;
							(C = 0)
						),
						get_line(Board, R-C, -1-1, [], Line),
						(
							contains_only_empty_cells(Line) ->
							Score is 0
							;
							line_score(Line, LastPlayer, Score)
						)
					), DiagonalLinesUpScores),
	!,
	flatten([HorizontalLinesScores, VerticalLinesScores, DiagonalLinesDownScores, DiagonalLinesUpScores], Scores),
	sum_list(Scores, TotalScore),
	hash_function(Board, Hash),
	assertz(memo_heuristic_score(Hash, Goal, TotalScore)).

% Évalue le score pour une ligne donnée:
line_score(Line, LastPlayer, Score) :-
	get_goal(Goal),
	Goal_1 is Goal - 1,
	findall(Value, (
		between(1, Goal_1, L),
		member(P-S, [n-1, b-(-1)]),
		L2 is L + 2,
		phrase(opened_row(P, L, max(Goal, L2)), Line),
		value(L, Goal, o, P, LastPlayer, V),
		Value is S * V
	), OpenedRowsValues),
	findall(Value, (
		between(2, Goal_1, L),
		member(P-S, [n-1, b-(-1)]),
		phrase(closed_row(P, L, Goal), Line),
		value(L, Goal, c, P, LastPlayer, V),
		Value is S * V
	), ClosedRowsValues),
	findall(Value, (
		member(P-S, [n-1, b-(-1)]),
		phrase(full_row(P, Goal), Line),
		value(Goal, Goal, f, P, LastPlayer, V),
		Value is S * V
	), FullRowsValues),
	findall(Value, (
		member(P-S, [n-1, b-(-1)]),
		phrase(semi_opened_row(P, Goal_1, F), Line),
		value(Goal_1, Goal, F, P, LastPlayer, V),
		Value is S * V
	), SemiOpenedRowsValues),
	!,
	flatten([OpenedRowsValues, ClosedRowsValues, FullRowsValues, SemiOpenedRowsValues], Values),
	sum_list(Values, Score). %, format('Score = ~w,~34| OpenedRowsValues = ~w, ClosedRowsValues = ~w, FullRowsValues = ~w, SemiOpenedRowsValues = ~w ~140| Line = ~w ~n', [Score, OpenedRowsValues, ClosedRowsValues, FullRowsValues, SemiOpenedRowsValues, Line]).
	
% value(NumberOfStones, Goal, PatternType, Player, NextPlayer, Value).
% 	NumberOfStones: Nombre de pions impliqués pour un joueur.
% 	Goal: Nombre de pions à aligner pour gagner.
% 	PatternType: Type d'alignment.
% 	- o: opened row   (i.e. v-n-n-n-n-v-v)
% 	- c: closed row   (i.e. b-n-n-n-n-v-v)
% 	- f: full row     (i.e. b-n-n-n-n-n-b)
% 	- s1: semi-opened with a single free space (i.e. b-n-n-n-v-n-b)
% 	- s2: semi-opened with a two free spaces   (i.e. b-n-n-n-v-n-v)
% 	- s3: semi-opened with a three free spaces (i.e. v-n-n-n-v-n-v)
% 	Player: Joueur pour qui on évalue l'alignement.
% 	NextPlayer: Prochain joueur à jouer.
% 	Value: Valeur de l'alignement.

% Valeur d'une configuration qui promet la victoire:
winning_score(1048576).
% Pénalisation sur le score d'un configuration où l'opposant peut jouer un tour et défaire la valeur de la configuration:
turn_penalty(Player, NextPlayer, Penalty) :- dif(Player, NextPlayer) -> Penalty is 0 ; Penalty is 0.5.

%    (NumberOfStones, 	  Goal, PatternType, 	    Player, 		NextPlayer, 		Value)
value(			   1,    	_, 		  	  o, 	  	 	 _, 				 _, 		 0.01) 	:- !.
%value(			Goal, 	  Goal, 		  _, 		Player, 		NextPlayer, 		Value) 	:- winning_score(WS), turn_penalty(Player, NextPlayer, Penalty), Value is WS / (4**(Penalty)), !.
%value(	      Goal_1, 	  Goal, 		  _, 	    Player, 		NextPlayer, 		Value) 	:- dif(Player, NextPlayer), Goal_1 #= Goal - 1, winning_score(WS), Value is WS**2, !.
value(			Goal, 	  Goal, 		  f, 		Player, 		NextPlayer, 		Value) 	:- winning_score(WS), turn_penalty(Player, NextPlayer, Penalty), Value is WS / (2**(Penalty)), !.
value(			   N, 	  Goal, 		  o, 	    Player, 		NextPlayer, 		Value)  :- winning_score(WS), turn_penalty(Player, NextPlayer, Penalty), Value is WS / (2**((Goal - N)**2 + Penalty)), !.
value(			   N, 	  Goal, 		 s3, 	    	Player, 		NextPlayer, 		Value)  :- winning_score(WS), turn_penalty(Player, NextPlayer, Penalty), Value is WS / (2**((Goal - N)**2 + Penalty)), !.
value(			   N, 	  Goal, 		 s2, 	    Player, 		NextPlayer, 		Value)  :- winning_score(WS), turn_penalty(Player, NextPlayer, Penalty), Value is WS / (2**((Goal - N)**2 + Penalty + 1)), !.
value(			   N, 	  Goal, 		 s1, 	    Player, 		NextPlayer, 		Value)	:- winning_score(WS), turn_penalty(Player, NextPlayer, Penalty), Value is WS / (2**((Goal - N)**2 + Penalty + 2)), !.
value(			   N, 	  Goal, 		  c, 	    	Player, 		NextPlayer, 		Value)  :- winning_score(WS), turn_penalty(Player, NextPlayer, Penalty), Value is WS / (2**((Goal - N)**2 + Penalty + 2)), !.
value(NumberOfStones, 	  Goal, PatternType, 	    Player, 		NextPlayer, 			0) 	:- writeln('Attention! Vérifier la valeur heuristique!'),
																							format('NumberOfStones = ~w, Goal = ~w, PatternType = ~w, Player = ~w, NextPlayer = ~w\n',
																									[NumberOfStones, Goal, PatternType, Player, NextPlayer]), !.
