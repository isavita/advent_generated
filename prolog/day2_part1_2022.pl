:- initialization(main).

shape_score('X', 1). % Rock
shape_score('Y', 2). % Paper
shape_score('Z', 3). % Scissors

outcome_score('A', 'X', 3). % Draw
outcome_score('B', 'Y', 3). % Draw
outcome_score('C', 'Z', 3). % Draw
outcome_score('A', 'Y', 6). % Win
outcome_score('B', 'Z', 6). % Win
outcome_score('C', 'X', 6). % Win
outcome_score('A', 'Z', 0). % Lose
outcome_score('B', 'X', 0). % Lose
outcome_score('C', 'Y', 0). % Lose

round_score(O, Y, Score) :-
    shape_score(Y, ShapeScore),
    outcome_score(O, Y, OutcomeScore),
    Score is ShapeScore + OutcomeScore.

total_score([], 0).
total_score([H|T], TotalScore) :-
    H = (O, Y),
    round_score(O, Y, Score),
    total_score(T, RestScore),
    TotalScore is Score + RestScore.

read_input(File, Rounds) :-
    open(File, read, Stream),
    read_lines(Stream, Rounds),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [(O, Y)|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    split_string(Line, " ", "", [OStr, YStr]),
    atom_string(O, OStr),
    atom_string(Y, YStr),
    read_lines(Stream, Lines).

main :-
    read_input('input.txt', Rounds),
    total_score(Rounds, Score),
    write(Score), nl.