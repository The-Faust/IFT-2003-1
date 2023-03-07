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
	writeln(''),
	set_goal(5),
	
	% Ligne vide:
	
	time(check_line_score(xvvvvvvvvvvx, n, EmptyRowScore)),
	assertion(EmptyRowScore =:= 0),
	
	% Opened rows:
	
	time(check_line_score(xvvvvnvvvvvx, n, Opened1NScore)),
	assertion(Opened1NScore =:= 2),
	
	time(check_line_score(xvvvvnnvvvvx, n, Opened2NScore)),
	assertion(Opened2NScore =:= 20),
	
	time(check_line_score(xvvvnnnvvvvx, n, Opened3NScore)),
	assertion(Opened3NScore =:= 200),
	
	time(check_line_score(xvvvnnnnvvvx, n, Opened4NScore)),
	assertion(Opened4NScore =:= 2000),
	
	time(check_line_score(xvvvvbvvvvvx, n, Opened1BScore)),
	assertion(Opened1BScore =:= -20),
	
	time(check_line_score(xvvvvbbvvvvx, n, Opened2BScore)),
	assertion(Opened2BScore =:= -200),
	
	time(check_line_score(xvvvbbbvvvvx, n, Opened3BScore)),
	assertion(Opened3BScore =:= -2000),
	
	time(check_line_score(xvvvbbbbvvvx, n, Opened4BScore)),
	assertion(Opened4BScore =:= -20000),
	
	% Closed rows:
	
	time(check_line_score(xnvvvvvvvvvx, n, ClosedNLeft1Score)),
	assertion(ClosedNLeft1Score =:= 0.8),
	
	time(check_line_score(xbnvvvvvvvvx, n, ClosedNLeft2Score)),
	assertion(ClosedNLeft2Score =:= 0.8),
	
	time(check_line_score(xvvvvvvvvvnx, n, ClosedNRight1Score)),
	assertion(ClosedNRight1Score =:= 0.8),
	
	time(check_line_score(xvvvvvvvvnbx, n, ClosedNRight2Score)),
	assertion(ClosedNRight2Score =:= 0.8),
	
	time(check_line_score(xvvvbnnvvvvx, n, Closed2NScore_1)),
	assertion(Closed2NScore_1 =:= 8),
	
	time(check_line_score(xvvvnnbvvvvx, n, Closed2NScore_2)),
	assertion(Closed2NScore_2 =:= 0),
	
	time(check_line_score(xvvvbnnnvvvx, n, Closed3NScore_1)),
	assertion(Closed3NScore_1 =:= 80),
	
	time(check_line_score(xvvvnnnbvvvx, n, Closed3NScore_2)),
	assertion(Closed3NScore_2 =:= 80),
	
	time(check_line_score(xvvvbnnnnvvx, n, Closed4NScore_1)),
	assertion(Closed4NScore_1 =:= 800),
	
	time(check_line_score(xvvvnnnnbvvx, n, Closed4NScore_2)),
	assertion(Closed4NScore_2 =:= 800),
	
	time(check_line_score(xvvvnbbvvvvx, n, Closed2BScore)),
	assertion(Closed2BScore =:= -80),
	
	time(check_line_score(xvvvnbbbvvvx, n, Closed3BScore)),
	assertion(Closed3BScore =:= -800),
	
	time(check_line_score(xvvvnbbbbvvx, n, Closed4BScore)),
	assertion(Closed4BScore =:= -8000),
	
	% Full rows:
	
	time(check_line_score(xvvvnnnnnvvx, n, Opened5NScore)),
	assertion(Opened5NScore =:= 20000),
	
	time(check_line_score(xvvvbnnnnnvx, n, Closed5NScore)),
	assertion(Closed5NScore =:= 8000),
	
	time(check_line_score(xvvvbbbbbvvx, n, Opened5BScore)),
	assertion(Opened5BScore =:= -200000),
	
	time(check_line_score(xvvvnbbbbbvx, n, Closed5BScore)),
	assertion(Closed5BScore =:= -80000),
	
	% Semi-Opened rows:
	
	time(check_line_score(xvvvnvnnnvvx, n, SemiOpened3NScore_1)),
	assertion(SemiOpened3NScore_1 =:= 1800),
	
	time(check_line_score(xvvvnnvnnvvx, n, SemiOpened3NScore_2)),
	assertion(SemiOpened3NScore_2 =:= 1800),
	
	time(check_line_score(xvvbnnvnnvvx, n, SemiOpened2NScore)),
	assertion(SemiOpened2NScore =:= 600),
	
	time(check_line_score(xvvbnnvnnbvx, n, SemiOpened1NScore)),
	assertion(SemiOpened1NScore =:= 600),
	
	time(check_line_score(xvvvbvbbbvvx, n, SemiOpened3BScore_1)),
	assertion(SemiOpened3BScore_1 =:= -18000),
	
	time(check_line_score(xvvvbbvbbvvx, n, SemiOpened3BScore_2)),
	assertion(SemiOpened3BScore_2 =:= -18000),
	
	time(check_line_score(xvvnbbvbbvvx, n, SemiOpened2BScore)),
	assertion(SemiOpened2BScore =:= -6000),
	
	time(check_line_score(xvvnbbvbbnvx, n, SemiOpened1BScore)),
	assertion(SemiOpened1BScore =:= -6000)
	.

test(line_score_misc) :-
	writeln(''),
	set_goal(5),
	
	time(check_line_score(xvvvnbbbvvvxxvvvnnnnbvvx, n, Score1)),
	assertion(Score1 =:= 0).

%test(evaluate_score_3x3_empty) :-
%	set_goal(3),
%	Pos = [
%		[vvv],
%		[vvv],
%		[vvv]]-nil-nil,
%	
%	heuristic_score(Pos, Score),
%	
%	assertion(Score = 0).
%
%test(evaluate_score_3x3_middle_n) :-
%	set_goal(3),
%	Pos = [
%		[vvv],
%		[vnv],
%		[vvv]]-n-(1-1),
%
%	heuristic_score(Pos, Score),
%	
%	assertion(Score = 0.04).

:- end_tests(heuristic_evaluation).

check_line_score(Line, Player, Score) :-
	atom_chars(Line, LineList),
	line_score(LineList, Player, Score),
	format('Line = ~w, Player = ~w, Score = ~w\n', [Line, Player, Score]).
	