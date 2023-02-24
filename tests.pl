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
%    Tests unitaires pour alphabeta.pl.     %
%                                           %
%    Exécuter les tests: ?- run_tests.      %
%===========================================%


:- [board].
:- [interface].
:- [evaluation].
:- [alphabeta].
:- [agent].


:- begin_tests(alphabeta).

test(alphabeta_tic_tac_toe_1) :-
	Pos = [[b,b,n],[v,n,v],[b,n,v]]-3-(0-1),
	Alpha = -inf,
	Beta = inf,
	
	alphabeta(Pos, Alpha, Beta, GoodPos, Val),
	GoodPos = _-_-Move,
	
	assertion(Move = 1-0),
	assertion(Val = 0).

test(alphabeta_tic_tac_toe_2) :-
	Pos = [[b,n,b],[n,v,v],[n,b,v]]-3-(0-1),
	Alpha = -inf,
	Beta = inf,
	
	alphabeta(Pos, Alpha, Beta, GoodPos, Val),
	GoodPos = _-_-Move,
	
	assertion(Move = 2-2),
	assertion(Val = -1).
	
:- end_tests(alphabeta).
