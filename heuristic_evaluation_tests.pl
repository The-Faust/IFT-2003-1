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
%    Tests unitaires pour heuristic_evaluation.pl.   %
%                                                    %
%    Exécuter les tests: ?- run_tests.               %
%====================================================%


:- [gomoku].

:- begin_tests(heuristic_evaluation).

test(line_score) :-
	set_goal(5),
	
	% No value:
	
	line_score([v,v,v,v,v,v,v,v,v,v], n, EmptyRowScore),
	assertion(EmptyRowScore = 0),
	
	line_score([n,v,v,v,v,v,v,v,v,v], n, ClosedNLeft1Score),
	assertion(ClosedNLeft1Score = 0),
	
	line_score([b,n,v,v,v,v,v,v,v,v], n, ClosedNLeft2Score),
	assertion(ClosedNLeft2Score = 0),
	
	line_score([v,v,v,v,v,v,v,v,v,n], n, ClosedNRight1Score),
	assertion(ClosedNRight1Score = 0),
	
	line_score([v,v,v,v,v,v,v,v,n,b], n, ClosedNRight2Score),
	assertion(ClosedNRight2Score = 0),
	
	% Opened rows:
	
	line_score([v,v,v,v,n,v,v,v,v,v], n, Opened1NScore),
	assertion(Opened1NScore = 0.01),
	
	line_score([v,v,v,v,n,n,v,v,v,v], n, Opened2NScore),
	assertion(Opened2NScore = 1448.1546878700492),
	
	line_score([v,v,v,n,n,n,v,v,v,v], n, Opened3NScore),
	assertion(Opened3NScore = 46340.950011841574),
	
	line_score([v,v,v,n,n,n,n,v,v,v], n, Opened4NScore),
	assertion(Opened4NScore = 370727.6000947326),
	
	line_score([v,v,v,v,b,v,v,v,v,v], n, Opened1BScore),
	assertion(Opened1BScore = -0.01),
	
	line_score([v,v,v,v,b,b,v,v,v,v], n, Opened2BScore),
	assertion(Opened2BScore = -2048),
	
	line_score([v,v,v,b,b,b,v,v,v,v], n, Opened3BScore),
	assertion(Opened3BScore = -65536),
	
	line_score([v,v,v,b,b,b,b,v,v,v], n, Opened4BScore),
	assertion(Opened4BScore = -524288),
	
	% Closed rows:
	
	line_score([v,v,v,b,n,n,v,v,v,v], n, Closed2NScore),
	assertion(Closed2NScore = 362.0386719675123),
	
	line_score([v,v,v,b,n,n,n,v,v,v], n, Closed3NScore),
	assertion(Closed3NScore = 11585.237502960394),
	
	line_score([v,v,v,b,n,n,n,n,v,v], n, Closed4NScore),
	assertion(Closed4NScore = 92681.90002368315),
	
	line_score([v,v,v,n,b,b,v,v,v,v], n, Closed2BScore),
	assertion(Closed2BScore = -512),
	
	line_score([v,v,v,n,b,b,b,v,v,v], n, Closed3BScore),
	assertion(Closed3BScore = -16384),
	
	line_score([v,v,v,n,b,b,b,b,v,v], n, Closed4BScore),
	assertion(Closed4BScore = -131072),
	
	% Full rows:
	
	line_score([v,v,v,n,n,n,n,n,v,v], n, Opened5NScore),
	assertion(Opened5NScore = 741455.2001894652),
	
	line_score([v,v,v,b,n,n,n,n,n,v], n, Closed5NScore),
	assertion(Closed5NScore = 741455.2001894652),
	
	line_score([v,v,v,b,b,b,b,b,v,v], n, Opened5BScore),
	assertion(Opened5BScore = -1048576),
	
	line_score([v,v,v,n,b,b,b,b,b,v], n, Closed5BScore),
	assertion(Closed5BScore = -1048576),
	
	% Semi-Opened rows:
	
	line_score([v,v,v,n,v,n,n,n,v,v], n, SemiOpened3NScore_1),
	assertion(SemiOpened3NScore_1 = 370727.6000947326),
	
	line_score([v,v,v,n,n,v,n,n,v,v], n, SemiOpened3NScore_2),
	assertion(SemiOpened3NScore_2 = 370727.6000947326),
	
	line_score([v,v,b,n,n,v,n,n,v,v], n, SemiOpened2NScore),
	assertion(SemiOpened2NScore = 185363.8000473663),
	
	line_score([v,v,b,n,n,v,n,n,b,v], n, SemiOpened1NScore),
	assertion(SemiOpened1NScore = 92681.90002368315),
	
	line_score([v,v,v,b,v,b,b,b,v,v], n, SemiOpened3BScore_1),
	assertion(SemiOpened3BScore_1 = -524288),
	
	line_score([v,v,v,b,b,v,b,b,v,v], n, SemiOpened3BScore_2),
	assertion(SemiOpened3BScore_2 = -524288),
	
	line_score([v,v,n,b,b,v,b,b,v,v], n, SemiOpened2BScore),
	assertion(SemiOpened2BScore = -262144),
	
	line_score([v,v,n,b,b,v,b,b,n,v], n, SemiOpened1BScore),
	assertion(SemiOpened1BScore = -131072)
	.

%test(evaluate_score_3x3_empty) :-
%	set_goal(3),
%	Pos = [
%		[v,v,v],
%		[v,v,v],
%		[v,v,v]]-nil-nil,
%	
%	heuristic_score(Pos, Score),
%	
%	assertion(Score = 0).
%
%test(evaluate_score_3x3_middle_n) :-
%	set_goal(3),
%	Pos = [
%		[v,v,v],
%		[v,n,v],
%		[v,v,v]]-n-(1-1),
%
%	heuristic_score(Pos, Score),
%	
%	assertion(Score = 0.04).

:- end_tests(heuristic_evaluation).
