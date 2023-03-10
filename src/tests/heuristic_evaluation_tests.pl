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


:- [src/game_components/heuristic_evaluation].

:- begin_tests(heuristic_evaluation).

test(line_score) :-
    writeln(''),
    set_goal(5),
    
    % ==================== Ligne vide ====================
    
    time(check_line_score(xvvvvvvvvvvx, n, EmptyRowScore)),
    assertion(EmptyRowScore =:= 0),
    
    % ================= rangées ouvertes =================
    
    time(check_line_score(xvvvvnvvvvvx, n, Opened1NScore)),
    assertion(Opened1NScore =:= 3),
    
    time(check_line_score(xvvvvnnvvvvx, n, Opened2NScore)),
    assertion(Opened2NScore =:= 30),
    
    time(check_line_score(xvvvnnnvvvvx, n, Opened3NScore)),
    assertion(Opened3NScore =:= 300),
    
    time(check_line_score(xvvvnnnnvvvx, n, Opened4NScore)),
    assertion(Opened4NScore =:= 3000),
    
    time(check_line_score(xvvvvbvvvvvx, n, Opened1BScore)),
    assertion(Opened1BScore =:= -30),
    
    time(check_line_score(xvvvvbbvvvvx, n, Opened2BScore)),
    assertion(Opened2BScore =:= -300),
    
    time(check_line_score(xvvvbbbvvvvx, n, Opened3BScore)),
    assertion(Opened3BScore =:= -3000),
    
    time(check_line_score(xvvvbbbbvvvx, n, Opened4BScore)),
    assertion(Opened4BScore =:= -30000),
    
    % ================= rangées fermées ==================
    
    time(check_line_score(xnvvvvvvvvvx, n, ClosedNLeft1Score)),
    assertion(ClosedNLeft1Score =:= 1),
    
    time(check_line_score(xbnvvvvvvvvx, n, ClosedNLeft2Score)),
    assertion(ClosedNLeft2Score =:= 1),
    
    time(check_line_score(xvvvvvvvvvnx, n, ClosedNRight1Score)),
    assertion(ClosedNRight1Score =:= 1),
    
    time(check_line_score(xvvvvvvvvnbx, n, ClosedNRight2Score)),
    assertion(ClosedNRight2Score =:= 1),
    
    time(check_line_score(xvvvbnnvvvvx, n, Closed2NScore_1)),
    assertion(Closed2NScore_1 =:= 10),
    
    time(check_line_score(xvvvnnbvvvvx, n, Closed2NScore_2)),
    assertion(Closed2NScore_2 =:= 0),
    
    time(check_line_score(xvvvbnnnvvvx, n, Closed3NScore_1)),
    assertion(Closed3NScore_1 =:= 100),
    
    time(check_line_score(xvvvnnnbvvvx, n, Closed3NScore_2)),
    assertion(Closed3NScore_2 =:= 100),
    
    time(check_line_score(xvvvbnnnnvvx, n, Closed4NScore_1)),
    assertion(Closed4NScore_1 =:= 1000),
    
    time(check_line_score(xvvvnnnnbvvx, n, Closed4NScore_2)),
    assertion(Closed4NScore_2 =:= 1000),
    
    time(check_line_score(xvvvnbbvvvvx, n, Closed2BScore)),
    assertion(Closed2BScore =:= -100),
    
    time(check_line_score(xvvvnbbbvvvx, n, Closed3BScore)),
    assertion(Closed3BScore =:= -1000),
    
    time(check_line_score(xvvvnbbbbvvx, n, Closed4BScore)),
    assertion(Closed4BScore =:= -10000),
    
    % ================ rangées complètes =================
    
    time(check_line_score(xvvvnnnnnvvx, n, Opened5NScore)),
    assertion(Opened5NScore =:= 30000),
    
    time(check_line_score(xvvvbnnnnnvx, n, Closed5NScore)),
    assertion(Closed5NScore =:= 30000),
    
    time(check_line_score(xvvvbbbbbvvx, n, Opened5BScore)),
    assertion(Opened5BScore =:= -300000),
    
    time(check_line_score(xvvvnbbbbbvx, n, Closed5BScore)),
    assertion(Closed5BScore =:= -300000),
    
    % ============== rangées semi-ouvertes ===============
    
    time(check_line_score(xvvvnvnnnvvx, n, SemiOpened3NScore_1)),
    assertion(SemiOpened3NScore_1 =:= 2000),
    
    time(check_line_score(xvvvnnvnnvvx, n, SemiOpened3NScore_2)),
    assertion(SemiOpened3NScore_2 =:= 2000),
    
    time(check_line_score(xvvbnnvnnvvx, n, SemiOpened2NScore)),
    assertion(SemiOpened2NScore =:= 800),
    
    time(check_line_score(xvvbnnvnnbvx, n, SemiOpened1NScore)),
    assertion(SemiOpened1NScore =:= 800),
    
    time(check_line_score(xvvvbvbbbvvx, n, SemiOpened3BScore_1)),
    assertion(SemiOpened3BScore_1 =:= -20000),
    
    time(check_line_score(xvvvbbvbbvvx, n, SemiOpened3BScore_2)),
    assertion(SemiOpened3BScore_2 =:= -20000),
    
    time(check_line_score(xvvnbbvbbvvx, n, SemiOpened2BScore)),
    assertion(SemiOpened2BScore =:= -8000),
    
    time(check_line_score(xvvnbbvbbnvx, n, SemiOpened1BScore)),
    assertion(SemiOpened1BScore =:= -8000).

test(line_score_misc) :-
    writeln(''),
    set_goal(5),
    
    time(check_line_score(xvvvnbbbvvvxxvvvnnnnbvvx, n, Score1)),
    assertion(Score1 =:= 0).

:- end_tests(heuristic_evaluation).

% Affiche le résultat de l'évaluation du score d'une ligne:
check_line_score(Line, Player, Score) :-
    atom_chars(Line, LineList),
    clumped(LineList, RLE),
    sum_score(Player, RLE, Score),
    format('Line = ~w, Player = ~w, Score = ~w\n', [Line, Player, Score]).
    